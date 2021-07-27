# pylint: disable=too-many-lines
import typing as ty
from abc import abstractmethod
from dataclasses import dataclass

from rflx.common import Base
from rflx.const import BUILTINS_PACKAGE
from rflx.expression import (
    FALSE,
    TRUE,
    UNDEFINED,
    Add,
    And,
    Attribute,
    Expr,
    First,
    Last,
    Name,
    Number,
    Size,
    Sub,
    ValidChecksum,
    ValueRange,
    Variable,
)
from rflx.identifier import ID
from rflx.model import (
    FINAL,
    INITIAL,
    Composite,
    Enumeration,
    Field,
    Integer,
    Link,
    Message,
    Opaque,
    Refinement,
    Scalar,
    Sequence,
    Type,
)
from rflx.pyrflx.bitstring import Bitstring
from rflx.pyrflx.error import PyRFLXError, Severity, Subsystem


class TypeValue(Base):

    _value: ty.Any = None

    def __init__(self, vtype: Type) -> None:
        self._type = vtype

    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return self._value == other._value and self._type == other._type
        return NotImplemented

    def equal_type(self, other: Type) -> bool:
        return isinstance(self._type, type(other))

    @property
    def name(self) -> str:
        return self._type.name

    @property
    def identifier(self) -> ID:
        return self._type.identifier

    @property
    def package(self) -> ID:
        return self._type.package

    @property
    def initialized(self) -> bool:
        return self._value is not None

    def _raise_initialized(self) -> None:
        if not self.initialized:
            raise PyRFLXError(f"value {self.identifier} not initialized")

    def clear(self) -> None:
        self._value = None

    @abstractmethod
    def assign(self, value: ty.Any, check: bool = True) -> None:
        raise NotImplementedError

    @abstractmethod
    def parse(self, value: ty.Union[Bitstring, bytes], check: bool = True) -> None:
        raise NotImplementedError

    @property
    @abstractmethod
    def bitstring(self) -> Bitstring:
        raise NotImplementedError

    @property
    @abstractmethod
    def size(self) -> Expr:
        raise NotImplementedError

    @property
    @abstractmethod
    def value(self) -> ty.Any:
        raise NotImplementedError

    @property
    @abstractmethod
    def accepted_type(self) -> type:
        raise NotImplementedError

    def clone(self) -> "TypeValue":
        return self.__class__(self._type)

    @classmethod
    def construct(
        cls, vtype: Type, imported: bool = False, refinements: ty.Sequence["RefinementValue"] = None
    ) -> "TypeValue":
        if isinstance(vtype, Integer):
            return IntegerValue(vtype)
        if isinstance(vtype, Enumeration):
            return EnumValue(vtype, imported)
        if isinstance(vtype, Opaque):
            return OpaqueValue(vtype)
        if isinstance(vtype, Sequence):
            return SequenceValue(vtype)
        if isinstance(vtype, Message):
            return MessageValue(vtype, refinements)
        raise PyRFLXError("cannot construct unknown type: " + type(vtype).__name__)


class ScalarValue(TypeValue):

    _type: Scalar

    def __init__(self, vtype: Scalar) -> None:
        super().__init__(vtype)

    @property
    @abstractmethod
    def expr(self) -> Expr:
        return NotImplemented

    @property
    def size(self) -> Number:
        return self._type.size


class IntegerValue(ScalarValue):

    _value: int
    _type: Integer

    def __init__(self, vtype: Integer) -> None:
        super().__init__(vtype)

    @property
    def _first(self) -> int:
        return self._type.first.value

    @property
    def _last(self) -> int:
        return self._type.last.value

    def assign(self, value: int, check: bool = True) -> None:
        if check and (
            And(*self._type.constraints("__VALUE__", check))
            .substituted(
                mapping={Variable("__VALUE__"): Number(value), Size("__VALUE__"): self._type.size}
            )
            .simplified()
            != TRUE
        ):
            raise PyRFLXError(f"value {value} not in type range {self._first} .. {self._last}")
        self._value = value

    def parse(self, value: ty.Union[Bitstring, bytes], check: bool = True) -> None:
        if isinstance(value, bytes):
            value = Bitstring.from_bytes(value)
        self.assign(int(value), check)

    @property
    def expr(self) -> Number:
        self._raise_initialized()
        return Number(self._value)

    @property
    def value(self) -> int:
        self._raise_initialized()
        return self._value

    @property
    def bitstring(self) -> Bitstring:
        self._raise_initialized()
        return Bitstring(format(self._value, f"0{self.size}b"))

    @property
    def accepted_type(self) -> type:
        return int


class EnumValue(ScalarValue):

    _value: ty.Tuple[str, Number]
    _type: Enumeration

    def __init__(self, vtype: Enumeration, imported: bool = False) -> None:
        super().__init__(vtype)
        self.__imported = imported
        self.__builtin = self._type.package == BUILTINS_PACKAGE
        self.__literals: ty.Dict[Name, Expr] = {}

        for k, v in self._type.literals.items():
            if self.__builtin or not self.__imported:
                self.__literals[Variable(k)] = v
            if not self.__builtin:
                self.__literals[Variable(self._type.package * k)] = v

    def assign(self, value: str, check: bool = True) -> None:
        prefixed_value = (
            ID(value)
            if value.startswith(str(self._type.package)) or not self.__imported or self.__builtin
            else self._type.package * value
        )
        if Variable(prefixed_value) not in self.literals:
            raise PyRFLXError(f"{value} is not a valid enum value")
        r = (
            (
                And(*self._type.constraints("__VALUE__", check, not self.__imported))
                .substituted(
                    mapping={
                        **self.literals,
                        **{Variable("__VALUE__"): self._type.literals[prefixed_value.name]},
                        **{Size("__VALUE__"): self._type.size},
                    }
                )
                .simplified()
            )
            if check
            else TRUE
        )
        assert r == TRUE
        self._value = (
            str(prefixed_value)
            if self.__imported and not self.__builtin
            else str(prefixed_value.name),
            self._type.literals[prefixed_value.name],
        )

    def parse(self, value: ty.Union[Bitstring, bytes], check: bool = True) -> None:
        if isinstance(value, bytes):
            value = Bitstring.from_bytes(value)
        value_as_number = Number(int(value))
        if value_as_number not in self.literals.values():
            if self._type.always_valid:
                self._value = f"RFLX_UNKNOWN_{self.name.upper()}", value_as_number
            else:
                raise PyRFLXError(f"Number {value_as_number.value} is not a valid enum value")
        else:
            for k, v in self.literals.items():
                if v == value_as_number:
                    assert isinstance(k, Variable)
                    assert isinstance(v, Number)
                    self._value = (
                        str(k.identifier) if self.__imported else str(k.identifier.name),
                        v,
                    )

    def clone(self) -> "TypeValue":
        return self.__class__(self._type, self.__imported)

    @property
    def numeric_value(self) -> Number:
        self._raise_initialized()
        return self._value[1]

    @property
    def value(self) -> str:
        self._raise_initialized()
        return self._value[0]

    @property
    def expr(self) -> Variable:
        self._raise_initialized()
        return Variable(self._value[0])

    @property
    def bitstring(self) -> Bitstring:
        self._raise_initialized()
        return Bitstring(format(self._value[1].value, f"0{self.size}b"))

    @property
    def accepted_type(self) -> type:
        return str

    @property
    def literals(self) -> ty.Mapping[Name, Expr]:
        return self.__literals


class CompositeValue(TypeValue):
    def __init__(self, vtype: Composite) -> None:
        self._expected_size: ty.Optional[Expr] = None
        super().__init__(vtype)

    def set_expected_size(self, expected_size: Expr) -> None:
        self._expected_size = expected_size

    def _check_size_of_assigned_value(
        self, value: ty.Union[bytes, Bitstring, ty.List[TypeValue]]
    ) -> None:
        if isinstance(value, bytes):
            size_of_value = len(value) * 8
        elif isinstance(value, Bitstring):
            size_of_value = len(value)
        else:
            bits = [element.bitstring for element in value]
            size_of_value = len(Bitstring.join(bits))

        if (
            self._expected_size is not None
            and isinstance(self._expected_size, Number)
            and size_of_value != self._expected_size.value
        ):
            raise PyRFLXError(
                f"invalid data size: input size is {len(value) * 8} "
                f"while expected input size is {self._expected_size.value}"
            )

    @property
    @abstractmethod
    def value(self) -> ty.Any:
        raise NotImplementedError


class OpaqueValue(CompositeValue):

    _value: ty.Optional[bytes]
    _nested_message: ty.Optional["MessageValue"] = None

    def __init__(self, vtype: Opaque) -> None:
        super().__init__(vtype)
        self._refinement_message: ty.Optional["MessageValue"] = None

    def assign(self, value: bytes, check: bool = True) -> None:
        self.parse(value, check)

    def parse(self, value: ty.Union[Bitstring, bytes], check: bool = True) -> None:
        if check:
            self._check_size_of_assigned_value(value)
        if self._refinement_message is not None:
            nested_msg = self._refinement_message.clone()
            try:
                nested_msg.parse(value, check)
            except PyRFLXError as e:
                e.appendleft(
                    f"Error while parsing nested message {self._refinement_message.identifier}",
                    Subsystem.PYRFLX,
                    Severity.ERROR,
                )
                raise e
            assert nested_msg.valid_message
            self._nested_message = nested_msg
            self._value = nested_msg.bytestring
        else:
            self._value = bytes(value)

    def set_refinement(self, model_of_refinement_msg: "MessageValue") -> None:
        self._refinement_message = model_of_refinement_msg

    @property
    def size(self) -> Expr:
        if self._value is None:
            return self._expected_size if self._expected_size is not None else UNDEFINED
        return Number(len(self._value) * 8)

    @property
    def nested_message(self) -> ty.Optional["MessageValue"]:
        return self._nested_message

    @property
    def value(self) -> bytes:
        self._raise_initialized()
        assert self._value is not None
        return self._value

    @property
    def bitstring(self) -> Bitstring:
        self._raise_initialized()
        assert self._value is not None
        size = self.size
        assert isinstance(size, Number)
        if size.value == 0:
            return Bitstring("")
        return Bitstring(format(int.from_bytes(self._value, "big"), f"0{size}b"))

    @property
    def accepted_type(self) -> type:
        return bytes


class SequenceValue(CompositeValue):

    _value: ty.List[TypeValue]

    def __init__(self, vtype: Sequence) -> None:
        super().__init__(vtype)
        self._element_type = vtype.element_type
        self._is_message_sequence = isinstance(self._element_type, Message)
        self._value = []

    def assign(self, value: ty.List[TypeValue], check: bool = True) -> None:
        if check:
            self._check_size_of_assigned_value(value)
        for v in value:
            if self._is_message_sequence:
                if isinstance(v, MessageValue):
                    assert isinstance(self._element_type, Message)
                    if not v.equal_type(self._element_type):
                        raise PyRFLXError(
                            f'cannot assign "{v.name}" to an sequence of '
                            f'"{self._element_type.name}"'
                        )
                    if not v.valid_message:
                        raise PyRFLXError(
                            f'cannot assign message "{v.name}" to sequence of messages: '
                            f"all messages must be valid"
                        )
                else:
                    raise PyRFLXError(
                        f"cannot assign {type(v).__name__} to an sequence of "
                        f"{type(self._element_type).__name__}"
                    )
            else:
                if isinstance(v, MessageValue) or not v.equal_type(self._element_type):
                    raise PyRFLXError(
                        f"cannot assign {type(v).__name__} to an sequence of "
                        f"{type(self._element_type).__name__}"
                    )

        self._value = value

    def parse(self, value: ty.Union[Bitstring, bytes], check: bool = True) -> None:
        self._check_size_of_assigned_value(value)
        if isinstance(value, bytes):
            value = Bitstring.from_bytes(value)
        if self._is_message_sequence:

            while len(value) != 0:
                nested_message = TypeValue.construct(self._element_type)
                assert isinstance(nested_message, MessageValue)
                try:
                    nested_message.parse(value, check)
                except PyRFLXError as e:
                    e.appendleft(
                        f"cannot parse nested messages in sequence of type "
                        f"{self._element_type.full_name}",
                        Subsystem.PYRFLX,
                        Severity.ERROR,
                    )
                    raise e
                assert nested_message.valid_message
                self._value.append(nested_message)
                value = value[len(nested_message.bitstring) :]

        elif isinstance(self._element_type, Scalar):
            value_str = str(value)
            type_size = self._element_type.size
            type_size_int = type_size.value
            new_value = []

            while len(value_str) != 0:
                nested_value = TypeValue.construct(
                    self._element_type, imported=self._element_type.package != self._type.package
                )
                nested_value.parse(Bitstring(value_str[:type_size_int]), check)
                new_value.append(nested_value)
                value_str = value_str[type_size_int:]

            self._value = new_value
        else:
            raise PyRFLXError(
                f"sequences of {self._element_type.identifier} currently not supported"
            )

    @property
    def size(self) -> Expr:
        if not self._value:
            return self._expected_size if self._expected_size is not None else UNDEFINED
        return Number(len(self.bitstring))

    @property
    def value(self) -> ty.Sequence[TypeValue]:
        self._raise_initialized()
        return self._value

    @property
    def bitstring(self) -> Bitstring:
        self._raise_initialized()
        bits = [element.bitstring for element in self._value]
        return Bitstring.join(bits)

    @property
    def accepted_type(self) -> type:
        return list

    @property
    def element_type(self) -> Type:
        return self._element_type


class MessageValue(TypeValue):
    # pylint: disable=too-many-instance-attributes
    # pylint: disable=too-many-public-methods

    _type: Message

    def __init__(
        self,
        model: Message,
        refinements: ty.Sequence["RefinementValue"] = None,
        skip_verification: bool = False,
        state: "MessageValue.State" = None,
    ) -> None:
        super().__init__(model)
        self._skip_verification = skip_verification
        self._refinements = refinements or []
        self._path: ty.List[Link] = []

        self._fields: ty.Mapping[str, MessageValue.Field] = (
            state.fields
            if state and state.fields
            else {
                f.name: self.Field(
                    TypeValue.construct(
                        self._type.types[f] if f in self._type.types else Opaque(),
                        imported=f in self._type.types
                        and self._type.types[f].package != model.package,
                    ),
                    f.name,
                )
                for f in (INITIAL,) + self._type.fields
            }
        )

        self._checksums: ty.Mapping[str, MessageValue.Checksum] = (
            state.checksums
            if state and state.checksums
            else {
                str(field_name): MessageValue.Checksum(str(field_name), parameters)
                for field_name, parameters in self._type.checksums.items()
            }
        )

        self.__type_literals: ty.Mapping[Name, Expr] = (
            state.type_literals
            if state and state.type_literals
            else {
                k: v
                for t in (
                    f.typeval.literals
                    for f in self._fields.values()
                    if isinstance(f.typeval, EnumValue)
                )
                for k, v in t.items()
            }
        )
        self.__additional_enum_literals: ty.Dict[Name, Expr] = {}
        self.__message_first_name = First("Message")
        initial = self._fields[INITIAL.name]
        initial.first = Number(0)
        initial.typeval.assign(bytes())
        self._simplified_mapping: ty.Dict[Name, Expr] = dict.fromkeys(
            [initial.name_size, initial.name_last, initial.name_first, self.__message_first_name],
            Number(0),
        )
        self.accessible_fields: ty.List[str] = []
        if self._skip_verification:
            self._last_field = INITIAL.name
        else:
            self._preset_fields(INITIAL.name)
        self.__message_last_name = Last("Message")
        self.__message_size_name = Size("Message")

    def add_refinement(self, refinement: "RefinementValue") -> None:
        self._refinements = [*(self._refinements or []), refinement]

    def clone(self) -> "MessageValue":
        return MessageValue(
            self._type,
            self._refinements,
            self._skip_verification,
            MessageValue.State(
                {
                    k: MessageValue.Field(
                        v.typeval.clone(),
                        k,
                        v.name_variable,
                        v.name_first,
                        v.name_last,
                        v.name_size,
                    )
                    for k, v in self._fields.items()
                },
                self._checksums,
                self.__type_literals,
            ),
        )

    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return self._fields == other._fields and self._type == other._type
        return NotImplemented

    def equal_type(self, other: Type) -> bool:
        return self.identifier == other.identifier

    @property
    def model(self) -> Message:
        return self._type

    @property
    def path(self) -> ty.Sequence[Link]:
        """
        The returned path is only correct, if the parse() method is used to set the field values.
        If fields are set manually using the set() method, path is empty. If the set() method is
        used to manipulate fields that have already been set by parse(), path is incorrect.
        """
        return self._path

    def inner_messages(self) -> ty.List["MessageValue"]:
        messages: ty.List["MessageValue"] = []
        for field in self._fields.values():
            typeval = field.typeval
            if isinstance(typeval, SequenceValue) and isinstance(typeval.element_type, Message):
                assert isinstance(typeval.value, list)
                messages.extend(typeval.value)
            if isinstance(typeval, OpaqueValue) and typeval.nested_message is not None:
                messages.append(typeval.nested_message)
        return messages

    def _valid_refinement_condition(self, refinement: "RefinementValue") -> bool:
        return self.__simplified(refinement.condition) == TRUE

    def _next_link(self, source_field_name: str) -> ty.Optional[Link]:
        field = Field(source_field_name)
        if field == FINAL or not self._type.structure:
            # structure is empty in case of NULL message
            # ISSUE: Componolit/RecordFlux#643
            return None
        for link in self._type.outgoing(field):
            if self.__simplified(link.condition) == TRUE:
                return link
        return None

    def _next_field(self, field_name: str, append_to_path: bool = False) -> str:
        if self._skip_verification and field_name != FINAL.name and self._fields[field_name].next:
            return self._fields[field_name].next

        next_link = self._next_link(field_name)
        if append_to_path and next_link is not None:
            self._path.append(next_link)

        if next_link is None and field_name == INITIAL.name:
            # the INITIAL field has no outgoing links in case of NULL message
            # ISSUE: Componolit/RecordFlux#643
            return FINAL.name
        return next_link.target.name if next_link is not None else ""

    def _prev_field(self, fld: str) -> str:
        if fld == INITIAL.name:
            return ""
        if self._skip_verification:
            return self._fields[fld].prev
        prev: ty.List[str] = [
            l.source.name
            for l in self._type.incoming(Field(fld))
            if self.__simplified(l.condition) == TRUE
        ]

        if len(prev) == 1:
            return prev[0]
        for field in prev:
            if field in self.accessible_fields:
                return field
        return ""

    def _get_size(self, fld: str) -> ty.Optional[Number]:
        typeval = self._fields[fld].typeval
        if isinstance(typeval, ScalarValue):
            return typeval.size
        assert isinstance(typeval, CompositeValue)
        for l in self._type.incoming(Field(fld)):
            if (
                self._fields[l.source.name].set
                and l.size != UNDEFINED
                and (self._skip_verification or self.__simplified(l.condition) == TRUE)
            ):
                size = self.__simplified(l.size)
                return size if isinstance(size, Number) else None
        return None

    def _get_first(self, fld: str) -> ty.Optional[Number]:
        for l in self._type.incoming(Field(fld)):
            if l.first != UNDEFINED and (
                self._skip_verification or self.__simplified(l.condition) == TRUE
            ):
                first = self.__simplified(l.first)
                return first if isinstance(first, Number) else None
        prv = self._prev_field(fld)
        if self._skip_verification and prv:
            first = self._fields[prv].first
            size = self._fields[prv].typeval.size
            assert isinstance(first, Number)
            assert isinstance(size, Number)
            return first + size
        if prv and UNDEFINED not in (self._fields[prv].first, self._fields[prv].typeval.size):
            first = self.__simplified(Add(self._fields[prv].first, self._fields[prv].typeval.size))
            return first if isinstance(first, Number) else None
        return None

    @property
    def accepted_type(self) -> type:
        return bytes

    @property
    def size(self) -> Number:
        return Number(len(self.bitstring))

    def assign(self, value: bytes, check: bool = True) -> None:
        raise NotImplementedError

    def parse(self, value: ty.Union[Bitstring, bytes], check: bool = True) -> None:
        assert not self._skip_verification
        self._path.clear()
        if isinstance(value, bytes):
            value = Bitstring.from_bytes(value)
        current_field_name = self._next_field(INITIAL.name, append_to_path=True)
        last_field_first_in_bitstr = current_field_first_in_bitstr = 0

        def get_current_pos_in_bitstr(field_name: str) -> int:
            # if the previous node is a virtual node i.e. has the same first as the current node
            # set the current pos in bitstring back to the first position of its predecessor
            this_first = self._fields[field_name].first
            prev_first = self._fields[self._prev_field(field_name)].first

            if not isinstance(prev_first, Number) or not isinstance(this_first, Number):
                return current_field_first_in_bitstr

            return (
                last_field_first_in_bitstr
                if prev_first.value == this_first.value
                else current_field_first_in_bitstr
            )

        def set_field_without_size(
            field_name: str, field: MessageValue.Field
        ) -> ty.Tuple[int, int]:
            last_pos_in_bitstr = current_pos_in_bitstring = get_current_pos_in_bitstr(field_name)
            assert isinstance(field.typeval, CompositeValue)
            first = self._get_first(field_name)
            assert first is not None
            field.first = first
            self._set_parsed_value(field_name, value[current_pos_in_bitstring:])
            return last_pos_in_bitstr, current_pos_in_bitstring

        def set_field_with_size(field_name: str, field_size: int) -> ty.Tuple[int, int]:
            assert isinstance(value, Bitstring)
            last_pos_in_bitstr = current_pos_in_bitstring = get_current_pos_in_bitstr(field_name)
            self._set_parsed_value(
                field_name,
                value[current_pos_in_bitstring : current_pos_in_bitstring + field_size],
            )
            current_pos_in_bitstring += field_size
            return last_pos_in_bitstr, current_pos_in_bitstring

        while current_field_name != FINAL.name:
            current_field = self._fields[current_field_name]
            size = self._get_size(current_field_name)
            if isinstance(current_field.typeval, CompositeValue) and size is None:
                (
                    last_field_first_in_bitstr,
                    current_field_first_in_bitstr,
                ) = set_field_without_size(current_field_name, current_field)

            else:
                assert size is not None
                current_field_size = size.value
                try:
                    (
                        last_field_first_in_bitstr,
                        current_field_first_in_bitstr,
                    ) = set_field_with_size(current_field_name, current_field_size)
                except IndexError:
                    raise PyRFLXError(
                        f"Bitstring representing the message is too short - "
                        f"stopped while parsing field: {current_field_name}"
                    ) from None
            current_field_name = self._next_field(current_field_name, append_to_path=True)

    def _set_unchecked(
        self, field_name: str, value: ty.Union[bytes, int, str, ty.Sequence[TypeValue]]
    ) -> None:
        field = self._fields[field_name]
        field.prev = self._last_field
        self._fields[self._last_field].next = field_name
        self._last_field = field_name
        f_first = self._get_first(field_name)
        f_size = self._get_size(field_name)
        assert isinstance(f_first, Number)
        field.first = f_first
        if isinstance(field.typeval, CompositeValue) and f_size is not None:
            field.typeval.set_expected_size(f_size)
        field.typeval.assign(value, not self._skip_verification)
        self.__update_simplified_mapping(field)
        self.accessible_fields.append(field_name)

    def _set_checked(
        self, field_name: str, value: ty.Union[bytes, int, str, ty.Sequence[TypeValue], Bitstring]
    ) -> None:
        def set_refinement(fld: MessageValue.Field, fld_name: str) -> None:
            if isinstance(fld.typeval, OpaqueValue):
                for ref in self._refinements:
                    if (
                        ref.pdu.name == self.name
                        and ref.field.name == fld_name
                        and self._valid_refinement_condition(ref)
                    ):
                        fld.typeval.set_refinement(ref.sdu)

        def check_outgoing_condition_satisfied() -> None:
            if all(
                self.__simplified(o.condition) == FALSE
                for o in self._type.outgoing(Field(field_name))
            ):
                self._fields[field_name].typeval.clear()
                raise PyRFLXError(
                    f"none of the field conditions "
                    f"{[str(o.condition) for o in self._type.outgoing(Field(field_name))]}"
                    f" for field {field_name} have been met by the assigned value: {value!s}"
                )

        if field_name in self.accessible_fields:
            field = self._fields[field_name]
            f_first = field.first
            f_size = field.typeval.size
            field_first = (
                f_first
                if self._skip_verification and isinstance(f_first, Number)
                else self._get_first(field_name)
            )
            field_size = (
                f_size
                if self._skip_verification and isinstance(f_size, Number)
                else self._get_size(field_name)
            )
            assert field_first is not None
            field.first = field_first
            if isinstance(field.typeval, CompositeValue) and field_size is not None:
                field.typeval.set_expected_size(field_size)
            set_refinement(field, field_name)
            try:
                if isinstance(value, Bitstring):
                    field.typeval.parse(value)
                    if (
                        isinstance(field.typeval, EnumValue)
                        and Variable(field.typeval.value) not in self.__type_literals
                    ):
                        self.__additional_enum_literals[
                            Variable(field.typeval.value)
                        ] = field.typeval.numeric_value
                elif isinstance(value, field.typeval.accepted_type):
                    field.typeval.assign(value)
                else:
                    raise PyRFLXError(
                        f"cannot assign different types: {field.typeval.accepted_type.__name__}"
                        f" != {type(value).__name__}"
                    )
            except PyRFLXError as e:
                e.appendleft(
                    f"cannot set value for field {field_name}",
                    Subsystem.PYRFLX,
                    Severity.ERROR,
                )
                raise e
        else:
            raise PyRFLXError(f"cannot access field {field_name}")

        self.__update_simplified_mapping()
        check_outgoing_condition_satisfied()

    def set(
        self,
        field_name: str,
        value: ty.Union[bytes, int, str, ty.Sequence[TypeValue]],
    ) -> None:
        if self._skip_verification:
            self._set_unchecked(field_name, value)
            return
        self._set_checked(field_name, value)
        self._preset_fields(field_name)
        for checksum in self._checksums.values():
            if (
                not self._fields[checksum.field_name].set or checksum.calculated
            ) and self._is_checksum_settable(checksum):
                self._fields[checksum.field_name].typeval.assign(0)
                checksum.calculated = True
                checksum_value = self._calculate_checksum(checksum)
                self._set_checked(checksum.field_name, checksum_value)

    def _set_parsed_value(
        self,
        field_name: str,
        value: ty.Union[bytes, int, str, ty.Sequence[TypeValue], Bitstring],
    ) -> None:
        self._set_checked(field_name, value)
        self._preset_fields(field_name)

    def _preset_fields(self, fld: str) -> None:
        assert not self._skip_verification
        nxt = self._next_field(fld)
        fields: ty.List[str] = []
        while nxt and nxt != FINAL.name:
            field = self._fields[nxt]
            first = self._get_first(nxt)
            size = self._get_size(nxt)
            if first is None:
                break

            if (self.__simplified(self._type.field_condition(Field(nxt))) == TRUE) and (
                self._is_valid_composite_field(nxt)
                if isinstance(self._fields[nxt].typeval, CompositeValue)
                else size is not None
            ):
                fields.append(nxt)

            if size is None:
                break

            field.first = first
            if isinstance(field.typeval, OpaqueValue):
                field.typeval.set_expected_size(size)

            if field.set and isinstance(field.typeval, OpaqueValue):
                field.first = UNDEFINED
                field.typeval.clear()
                break
            self._last_field = nxt
            nxt = self._next_field(nxt)
        try:
            self.accessible_fields = (
                self.accessible_fields[: self.accessible_fields.index(fld) + 1] + fields
            )
        except ValueError:
            self.accessible_fields = fields

    def set_checksum_function(self, checksums: ty.Dict[str, ty.Callable]) -> None:
        for checksum_field_name, checksum_function in checksums.items():
            if checksum_field_name not in self.fields:
                raise PyRFLXError(
                    f"cannot set checksum function: field {checksum_field_name} is not defined"
                )
            for field_name, checksum in self._checksums.items():
                if field_name == checksum_field_name:
                    checksum.function = checksum_function
                else:
                    raise PyRFLXError(
                        f"cannot set checksum function: field {checksum_field_name} "
                        f"has not been defined as a checksum field"
                    )

    def _is_checksum_settable(self, checksum: "MessageValue.Checksum") -> bool:
        def valid_path(value_range: ValueRange) -> bool:
            lower = value_range.lower.substituted(
                func=lambda e: self._fields[self._next_field(INITIAL.name)].name_first
                if e == self.__message_first_name
                else e
            )
            expr: ty.Dict[Expr, str] = dict.fromkeys([lower, value_range.upper])

            for e in expr:
                if isinstance(e, Sub):
                    assert isinstance(e.left, (First, Last))
                    expr[e] = str(e.left.prefix)
                elif isinstance(e, Add):
                    for t in e.terms:
                        if isinstance(t, (First, Last)):
                            expr[e] = str(t.prefix)
                else:
                    assert isinstance(e, (First, Last))
                    expr[e] = str(e.prefix)

            field = expr.get(lower)
            assert isinstance(field, str)
            upper_field_name = expr[value_range.upper]
            if upper_field_name == "Message":
                upper_field_name = "Final"
            while field != upper_field_name:
                field = self._next_field(field)
                if field == "Final" or field in self._checksums:
                    continue
                if field == "" or not self._fields[field].set:
                    break
            else:
                return True

            return False

        for expr_tuple in checksum.parameters:
            expr_tuple.evaluated_expression = self.__simplified(expr_tuple.expression)
            if (
                isinstance(expr_tuple.evaluated_expression, ValueRange)
                and isinstance(expr_tuple.expression, ValueRange)
                and (
                    not isinstance(expr_tuple.evaluated_expression.lower, Number)
                    or not isinstance(expr_tuple.evaluated_expression.upper, Number)
                    or not valid_path(expr_tuple.expression)
                )
            ):
                return False
            if (
                isinstance(expr_tuple.evaluated_expression, Variable)
                and not self._fields[expr_tuple.evaluated_expression.name].set
            ):
                return False
            if (
                isinstance(expr_tuple.evaluated_expression, Attribute)
                and not self._fields[str(expr_tuple.evaluated_expression.prefix)].set
            ):
                return False
        return True

    def update_checksums(self) -> None:
        for checksum in self._checksums.values():
            self._simplified_mapping[ValidChecksum(checksum.field_name)] = TRUE
            self._is_checksum_settable(checksum)
            checksum_value = self._calculate_checksum(checksum)
            self._fields[checksum.field_name].typeval.assign(checksum_value)

    def _calculate_checksum(
        self, checksum: "MessageValue.Checksum"
    ) -> ty.Union[bytes, int, str, ty.Sequence[TypeValue], Bitstring]:
        if not checksum.function:
            raise PyRFLXError(
                f"cannot calculate checksum for {checksum.field_name}: "
                f"no callable checksum function provided"
            )

        arguments: ty.Dict[str, ty.Union[int, ty.Tuple[int, int]]] = {}
        for expr_tuple in checksum.parameters:
            if isinstance(expr_tuple.evaluated_expression, ValueRange):
                assert isinstance(expr_tuple.evaluated_expression.lower, Number) and isinstance(
                    expr_tuple.evaluated_expression.upper, Number
                )
                arguments[str(expr_tuple.expression)] = (
                    expr_tuple.evaluated_expression.lower.value,
                    expr_tuple.evaluated_expression.upper.value,
                )
            elif isinstance(expr_tuple.evaluated_expression, Variable):
                assert (
                    expr_tuple.evaluated_expression.name in self.fields
                    and self._fields[expr_tuple.evaluated_expression.name].set
                )
                arguments[str(expr_tuple.expression)] = self._fields[
                    expr_tuple.evaluated_expression.name
                ].typeval.value
            else:
                assert isinstance(expr_tuple.evaluated_expression, Number)
                arguments[str(expr_tuple.expression)] = expr_tuple.evaluated_expression.value
        return checksum.function(self._unchecked_bytestring(), **arguments)

    def get(
        self, field_name: str
    ) -> ty.Union["MessageValue", ty.Sequence[TypeValue], int, str, bytes]:
        if field_name not in self.valid_fields:
            if field_name not in self.fields:
                raise PyRFLXError(f'"{field_name}" is not a field of this message')
            raise PyRFLXError(f'"{field_name}" is not set')
        field = self._fields[field_name]
        if isinstance(field.typeval, OpaqueValue) and field.typeval.nested_message is not None:
            return field.typeval.nested_message
        return self._fields[field_name].typeval.value

    @property
    def bitstring(self) -> Bitstring:
        bits = ""
        field = self._next_field(INITIAL.name)
        while field and field != FINAL.name:
            field_val = self._fields[field]
            if (
                not field_val.set
                or not isinstance(field_val.first, Number)
                or not field_val.first.value <= len(bits)
            ):
                break
            bits = f"{bits[: field_val.first.value]}{str(self._fields[field].typeval.bitstring)}"
            field = self._next_field(field)

        return Bitstring(bits)

    @property
    def value(self) -> ty.Any:
        raise NotImplementedError

    def _unchecked_bytestring(self) -> bytes:
        bits = str(self.bitstring)
        assert len(bits) % 8 == 0
        return b"".join(
            [int(bits[i : i + 8], 2).to_bytes(1, "big") for i in range(0, len(bits), 8)]
        )

    @property
    def bytestring(self) -> bytes:
        if not self._skip_verification and not self.valid_message:
            raise PyRFLXError(f"cannot create bytestring of invalid message: {self.identifier}")
        return self._unchecked_bytestring()

    @property
    def fields(self) -> ty.List[str]:
        return [f.name for f in self._type.fields]

    def _is_valid_composite_field(self, field: str) -> bool:
        assert isinstance(self._fields[field].typeval, CompositeValue)
        incoming = self._type.incoming(Field(field))

        for l in incoming:
            if (
                l.size != UNDEFINED
                and self._fields[l.source.name].set
                and self.__simplified(l.condition) == TRUE
            ):
                valid_edge = l
                break
        else:
            return False

        return all(
            (v.name in self._fields and self._fields[v.name].set) or v.name == "Message"
            for v in valid_edge.size.variables()
        )

    @property
    def valid_fields(self) -> ty.List[str]:
        return [
            f
            for f in self.accessible_fields
            if (
                self._fields[f].set
                and self.__simplified(self._type.field_condition(Field(f))) == TRUE
                and any(
                    self.__simplified(o.condition) == TRUE for o in self._type.outgoing(Field(f))
                )
            )
        ]

    @property
    def required_fields(self) -> ty.List[str]:
        accessible = self.accessible_fields
        valid = self.valid_fields
        return [f for f in accessible if f not in valid]

    @property
    def valid_message(self) -> bool:
        return (
            bool(self.valid_fields)
            and self._next_field(self.valid_fields[-1]) == FINAL.name
            and all(
                (self._is_checksum_settable(checksum) or self._skip_verification)
                and self._calculate_checksum(checksum) == self.get(checksum.field_name)
                for checksum in self._checksums.values()
            )
        )

    def __update_simplified_mapping(self, field: ty.Optional[Field] = None) -> None:
        if field:
            if isinstance(field.typeval, ScalarValue):
                self._simplified_mapping[field.name_variable] = field.typeval.expr
            last = field.last
            assert isinstance(last, Number)
            self._simplified_mapping[field.name_size] = field.typeval.size
            self._simplified_mapping[field.name_first] = field.first
            self._simplified_mapping[field.name_last] = last
            self._simplified_mapping[self.__message_last_name] = last
            self._simplified_mapping[self.__message_size_name] = last + Number(1)
            return

        self._simplified_mapping = {
            Size(field_type.full_name): field_type.size
            for field_type in self._type.types.values()
            if isinstance(field_type, Scalar)
        }
        self._simplified_mapping[self.__message_first_name] = Number(0)
        for v in self._fields.values():
            if isinstance(v.typeval, ScalarValue) and v.set:
                self._simplified_mapping[v.name_variable] = v.typeval.expr
            if isinstance(v.typeval, ScalarValue) or v.set:
                self._simplified_mapping[v.name_size] = v.typeval.size
            if isinstance(v.first, Number):
                self._simplified_mapping[v.name_first] = v.first
            if isinstance(v.last, Number):
                self._simplified_mapping[v.name_last] = v.last

        nxt = self._next_field(INITIAL.name)
        while nxt:
            last_field = nxt
            nxt = self._next_field(last_field)
            last = self._fields[last_field].last
            if nxt == FINAL.name or not isinstance(last, Number):
                break
            if nxt:
                continue
            if any(l.target == FINAL for l in self._type.outgoing(Field(last_field))):
                self._simplified_mapping[self.__message_last_name] = last
                self._simplified_mapping[self.__message_size_name] = last + Number(1)

        # ISSUE: Componolit/RecordFlux#422
        self._simplified_mapping.update({ValidChecksum(f): TRUE for f in self._checksums})

    def __simplified(self, expr: Expr) -> Expr:
        if expr in {TRUE, FALSE}:
            return expr

        def subst(expression: Expr) -> Expr:
            if expression in self._simplified_mapping:
                assert isinstance(expression, Name)
                return self._simplified_mapping[expression]
            if expression in self.__type_literals:
                assert isinstance(expression, Name)
                return self.__type_literals[expression]
            if expression in self.__additional_enum_literals:
                assert isinstance(expression, Name)
                return self.__additional_enum_literals[expression]
            return expression

        return expr.substituted(func=subst).substituted(func=subst).simplified()

    class Checksum:
        def __init__(self, field_name: str, parameters: ty.Sequence[Expr]):
            self.field_name = field_name
            self.function: ty.Optional[ty.Callable] = None
            self.calculated = False

            @dataclass
            class ExpressionTuple:
                expression: Expr
                evaluated_expression: Expr = UNDEFINED

            self.parameters: ty.List[ExpressionTuple] = []
            for expr in parameters:
                assert isinstance(expr, (ValueRange, Attribute, Variable))
                self.parameters.append(ExpressionTuple(expr))

    @dataclass
    class Field(Base):
        typeval: TypeValue
        __first: Expr
        __last: Expr
        name_variable: Variable
        name_first: First
        name_last: Last
        name_size: Size
        prev: str
        next: str

        def __init__(
            self,
            t: TypeValue,
            name: str = "",
            name_variable: Variable = None,
            name_first: First = None,
            name_last: Last = None,
            name_size: Size = None,
        ):
            # pylint: disable=too-many-arguments
            assert name or (name_variable and name_first and name_last and name_size)
            self.typeval = t
            self.__is_scalar = isinstance(self.typeval, ScalarValue)
            self.first: Expr = UNDEFINED
            self.name_variable = name_variable if name_variable else Variable(name)
            self.name_first = name_first if name_first else First(name)
            self.name_last = name_last if name_last else Last(name)
            self.name_size = name_size if name_size else Size(name)
            self.prev = ""
            self.next = ""

        def _last(self) -> Expr:
            if self.first == UNDEFINED:
                return UNDEFINED
            return Sub(Add(self.__first, self.typeval.size), Number(1)).simplified()

        @property
        def first(self) -> Expr:
            return self.__first

        @first.setter
        def first(self, first: Expr) -> None:
            self.__first = first
            if self.__is_scalar:
                self.__last = self._last()

        def __eq__(self, other: object) -> bool:
            if isinstance(other, MessageValue.Field):
                return (
                    self.first == other.first
                    and self.last == other.last
                    and self.typeval == other.typeval
                )
            return NotImplemented

        @property
        def set(self) -> bool:
            return (
                self.typeval.initialized
                and isinstance(self.typeval.size, Number)
                and isinstance(self.first, Number)
                and isinstance(self.last, Number)
            )

        @property
        def last(self) -> Expr:
            return self.__last if self.__is_scalar else self._last()

    @dataclass
    class State:
        fields: ty.Optional[ty.Mapping[str, "MessageValue.Field"]] = None
        checksums: ty.Optional[ty.Mapping[str, "MessageValue.Checksum"]] = None
        type_literals: ty.Optional[ty.Mapping[Name, Expr]] = None


class RefinementValue:
    def __init__(self, refinement: Refinement, sdu_message: MessageValue) -> None:
        self.package = refinement.package
        self.pdu = refinement.pdu
        self.field = refinement.field
        self.sdu = sdu_message
        self.condition = refinement.condition
