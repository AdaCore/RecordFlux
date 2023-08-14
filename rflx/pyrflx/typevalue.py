from __future__ import annotations

import typing as ty
from abc import abstractmethod
from collections import abc
from dataclasses import dataclass
from typing import Any, Optional, Protocol, Union

from rflx.common import Base
from rflx.const import BUILTINS_PACKAGE
from rflx.error import Severity, Subsystem, fatal_fail
from rflx.expression import (
    FALSE,
    TRUE,
    UNDEFINED,
    Add,
    Aggregate,
    And,
    Attribute,
    Expr,
    First,
    Last,
    Literal,
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
    BOOLEAN,
    FINAL,
    INITIAL,
    ByteOrder,
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
from rflx.pyrflx.error import PyRFLXError


class ChecksumFunction(Protocol):
    def __call__(self, message: bytes, **kwargs: object) -> int:
        ...  # pragma: no cover


ValueType = Union[
    "MessageValue",
    ty.Sequence["TypeValue"],
    ty.Tuple[str, Number],
    int,
    str,
    bytes,
]


class TypeValue(Base):
    _value: Optional[ValueType] = None

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
    def assign(self, value: Any, check: bool = True) -> None:  # type: ignore[misc]
        raise NotImplementedError

    @abstractmethod
    def parse(self, value: Union[Bitstring, bytes], check: bool = True) -> None:
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
    def value(self) -> ValueType:
        raise NotImplementedError

    @property
    @abstractmethod
    def accepted_type(self) -> type:
        raise NotImplementedError

    @abstractmethod
    def as_json(self) -> object:
        raise NotImplementedError

    def clone(self) -> TypeValue:
        return self.__class__(self._type)

    @classmethod
    def construct(
        cls,
        vtype: Type,
        imported: bool = False,
        refinements: Optional[abc.Sequence[RefinementValue]] = None,
    ) -> TypeValue:
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
            And(*self._type.constraints("__VALUE__"))
            .substituted(
                mapping={Variable("__VALUE__"): Number(value), Size("__VALUE__"): self._type.size},
            )
            .simplified()
            != TRUE
        ):
            raise PyRFLXError(f"value {value} not in type range {self._first} .. {self._last}")
        self._value = value

    def parse(self, value: Union[Bitstring, bytes], check: bool = True) -> None:
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

    def as_json(self) -> object:
        return self._value


class EnumValue(ScalarValue):
    _value: tuple[str, Number]
    _type: Enumeration

    def __init__(self, vtype: Enumeration, imported: bool = False) -> None:
        super().__init__(vtype)
        self._imported = imported
        self._builtin = self._type.package == BUILTINS_PACKAGE
        self._literals: dict[Name, Expr] = {}

        for k, v in self._type.literals.items():
            if self._builtin or not self._imported:
                self._literals[Literal(k)] = v
            if not self._builtin:
                self._literals[Literal(self._type.package * k)] = v

    def assign(self, value: str, check: bool = True) -> None:
        prefixed_value = (
            ID(value)
            if value.startswith(str(self._type.package)) or self._builtin
            else self._type.package * value
        )
        if Literal(prefixed_value) not in self.literals:
            raise PyRFLXError(f"{value} is not a valid enum value")
        r = (
            (
                And(*self._type.constraints("__VALUE__", same_package=not self._imported))
                .substituted(
                    mapping={
                        **self.literals,
                        Variable("__VALUE__"): self._type.literals[prefixed_value.name],
                        Size("__VALUE__"): self._type.size,
                    },
                )
                .simplified()
            )
            if check
            else TRUE
        )
        assert r == TRUE
        self._value = (str(prefixed_value), self._type.literals[prefixed_value.name])

    def parse(self, value: Union[Bitstring, bytes], _check: bool = True) -> None:
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
                    assert isinstance(k, Literal)
                    assert isinstance(v, Number)
                    self._value = (str(k.identifier), v)

    def clone(self) -> TypeValue:
        return self.__class__(self._type, self._imported)

    @property
    def numeric_value(self) -> Number:
        self._raise_initialized()
        return self._value[1]

    @property
    def value(self) -> str:
        self._raise_initialized()
        return self._value[0]

    @property
    def expr(self) -> Literal:
        self._raise_initialized()
        return Literal(self._value[0])

    @property
    def bitstring(self) -> Bitstring:
        self._raise_initialized()
        return Bitstring(format(self._value[1].value, f"0{self.size}b"))

    @property
    def accepted_type(self) -> type:
        return str

    @property
    def literals(self) -> abc.Mapping[Name, Expr]:
        return self._literals

    def as_json(self) -> object:
        return (self._value[0], self._value[1].value)


class CompositeValue(TypeValue):
    def __init__(self, vtype: Composite) -> None:
        self._expected_size: Optional[Expr] = None
        super().__init__(vtype)

    def set_expected_size(self, expected_size: Expr) -> None:
        self._expected_size = expected_size

    def _check_size_of_assigned_value(
        self,
        value: Union[bytes, Bitstring, abc.Sequence[TypeValue]],
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
                f"while expected input size is {self._expected_size.value}",
            )

    @property
    @abstractmethod
    def value(self) -> ValueType:
        raise NotImplementedError


class OpaqueValue(CompositeValue):
    _value: Optional[bytes]
    _nested_message: Optional[MessageValue] = None

    def __init__(self, vtype: Opaque) -> None:
        super().__init__(vtype)
        self._refinement_message: Optional[MessageValue] = None

    def assign(self, value: bytes, check: bool = True) -> None:
        self.parse(value, check)

    def parse(self, value: Union[Bitstring, bytes], check: bool = True) -> None:
        if check:
            self._check_size_of_assigned_value(value)
        if self._refinement_message is not None:
            nested_msg = self._refinement_message.clone()
            try:
                nested_msg.parse(value, check)
            except PyRFLXError as e:
                new_exception = PyRFLXError(
                    f"Error while parsing nested message {self._refinement_message.identifier}",
                )
                new_exception.extend(e)
                raise new_exception from e
            assert nested_msg.valid_message
            self._nested_message = nested_msg
            self._value = nested_msg.bytestring
        else:
            self._value = bytes(value)

    def set_refinement(self, model_of_refinement_msg: MessageValue) -> None:
        self._refinement_message = model_of_refinement_msg

    @property
    def size(self) -> Expr:
        if self._value is None:
            return self._expected_size if self._expected_size is not None else UNDEFINED
        return Number(len(self._value) * 8)

    @property
    def nested_message(self) -> Optional[MessageValue]:
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

    def as_json(self) -> Optional[bytes]:
        return self._value


class SequenceValue(CompositeValue):
    _value: list[TypeValue]

    def __init__(self, vtype: Sequence) -> None:
        super().__init__(vtype)
        self._element_type = vtype.element_type
        self._is_message_sequence = isinstance(self._element_type, Message)
        self._value = []

    def assign(self, value: list[TypeValue], check: bool = True) -> None:
        if check:
            self._check_size_of_assigned_value(value)
        for v in value:
            if self._is_message_sequence:
                if isinstance(v, MessageValue):
                    assert isinstance(self._element_type, Message)
                    if not v.equal_type(self._element_type):
                        raise PyRFLXError(
                            f'cannot assign "{v.name}" to an sequence of '
                            f'"{self._element_type.name}"',
                        )
                    if not v.valid_message:
                        raise PyRFLXError(
                            f'cannot assign message "{v.name}" to sequence of messages: '
                            f"all messages must be valid",
                        )
                else:
                    raise PyRFLXError(
                        f"cannot assign {type(v).__name__} to an sequence of "
                        f"{type(self._element_type).__name__}",
                    )
            else:
                if isinstance(v, MessageValue) or not v.equal_type(self._element_type):
                    raise PyRFLXError(
                        f"cannot assign {type(v).__name__} to an sequence of "
                        f"{type(self._element_type).__name__}",
                    )

        self._value = value

    def parse(self, value: Union[Bitstring, bytes], check: bool = True) -> None:
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
                    new_exception = PyRFLXError(
                        f"cannot parse nested messages in sequence of type "
                        f"{self._element_type.full_name}",
                    )
                    new_exception.extend(e)
                    raise new_exception from e
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
                    self._element_type,
                    imported=self._element_type.package != self._type.package,
                )
                nested_value.parse(Bitstring(value_str[:type_size_int]), check)
                new_value.append(nested_value)
                value_str = value_str[type_size_int:]

            self._value = new_value

        else:
            assert False

    @property
    def size(self) -> Expr:
        if not self._value:
            return self._expected_size if self._expected_size is not None else UNDEFINED
        return Number(len(self.bitstring))

    @property
    def value(self) -> abc.Sequence[TypeValue]:
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

    def as_json(self) -> object:
        return [f.as_json() for f in self._value]


class MessageValue(TypeValue):
    _type: Message

    def __init__(
        self,
        model: Message,
        refinements: Optional[abc.Sequence[RefinementValue]] = None,
        skip_verification: bool = False,
        parameters: Optional[abc.Mapping[Name, Expr]] = None,
        state: Optional[MessageValue.State] = None,
    ) -> None:
        super().__init__(model)
        self._skip_verification = skip_verification
        self._refinements = refinements or []
        self._path: list[Link] = []
        self._parameters = parameters or {}

        self._fields: abc.Mapping[str, MessageValue.Field] = (
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
                for f in (INITIAL, *self._type.fields)
            }
        )

        self._checksums: abc.Mapping[str, MessageValue.Checksum] = (
            state.checksums
            if state and state.checksums
            else {
                str(field_name): MessageValue.Checksum(str(field_name), parameters)
                for field_name, parameters in self._type.checksums.items()
            }
        )

        self._message_first_name = First("Message")
        initial = self._fields[INITIAL.name]
        initial.first = Number(0)
        initial.typeval.assign(b"")
        self._simplified_mapping: dict[Name, Expr] = dict.fromkeys(
            [initial.name_size, initial.name_last, initial.name_first, self._message_first_name],
            Number(0),
        )
        self.accessible_fields: list[str] = []
        if self._skip_verification:
            self._last_field = INITIAL.name
        else:
            self._preset_fields(INITIAL.name)
        self._message_last_name = Last("Message")
        self._message_size_name = Size("Message")

    def add_refinement(self, refinement: RefinementValue) -> None:
        self._refinements = [*(self._refinements or []), refinement]

    def add_parameters(self, parameters: abc.Mapping[str, Union[bool, int, str]]) -> None:
        expected = set(p.name for p in self._type.parameter_types)
        added = set(p for p in parameters)

        if expected - added:
            message = ", ".join(expected - added)
            raise PyRFLXError(f"missing parameter values: {message}")

        if added - expected:
            message = ", ".join(added - expected)
            raise PyRFLXError(f"unexpected parameter values: {message}")

        def check_type(valid: bool) -> None:
            if not valid:
                raise PyRFLXError(
                    f'message argument for "{name}" has invalid type "{type(value).__name__}"',
                )

        enum_literals = {
            str(l): str(t.package * l)
            for t in self._type.types.values()
            if isinstance(t, Enumeration) and t.package == self.package
            for l in t.literals
        }
        params: dict[Name, Expr] = {}
        expr: Expr
        for name, value in parameters.items():
            if isinstance(value, bool):
                check_type(self.model.parameter_types[Field(name)] == BOOLEAN)
                expr = TRUE if value else FALSE
            elif isinstance(value, int):
                check_type(isinstance(self.model.parameter_types[Field(name)], Integer))
                expr = Number(value)
            elif isinstance(value, str):
                check_type(
                    isinstance(self.model.parameter_types[Field(name)], Enumeration)
                    and self.model.parameter_types[Field(name)] != BOOLEAN,
                )
                if value in enum_literals:
                    expr = Literal(enum_literals[value])
                else:
                    assert value in enum_literals.values()
                    expr = Literal(value)
            else:
                raise PyRFLXError(
                    f'message argument for "{name}" has unsupported type "{type(value).__name__}"',
                )

            params[Variable(name)] = expr

        self._parameters = params
        if not self._skip_verification:
            self._preset_fields(INITIAL.name)

    def clone(self) -> MessageValue:
        return MessageValue(
            self._type,
            self._refinements,
            self._skip_verification,
            self._parameters,
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
    def path(self) -> abc.Sequence[Link]:
        """
        Return the message path for a parsed message.

        The returned path is only correct, if the parse() method is used to set the field values.
        If fields are set manually using the set() method, path is empty. If the set() method is
        used to manipulate fields that have already been set by parse(), path is incorrect.
        """
        return self._path

    def inner_messages(self) -> list[MessageValue]:
        messages: list[MessageValue] = []
        for field in self._fields.values():
            typeval = field.typeval
            if isinstance(typeval, SequenceValue) and isinstance(typeval.element_type, Message):
                assert isinstance(typeval.value, list)
                messages.extend(typeval.value)
            if isinstance(typeval, OpaqueValue) and typeval.nested_message is not None:
                messages.append(typeval.nested_message)
        return messages

    def as_json(self) -> dict[str, dict[str, object]]:
        result: dict[str, dict[str, object]] = {}
        for field_name in self.valid_fields:
            field: dict[str, object] = {}
            field_value = self.get(field_name)
            if isinstance(field_value, MessageValue):
                field["value"] = field_value.bytestring.hex()
            elif isinstance(field_value, bytes):
                field["value"] = field_value.hex()
            elif isinstance(field_value, (int, str)):
                field["value"] = field_value
            elif isinstance(field_value, list):
                field["value"] = [f.as_json() for f in field_value]
            else:
                raise NotImplementedError(f"Unsupported: {type(field_value)}")
            first = self._simplified(self._fields[field_name].first)
            assert isinstance(first, Number)
            field["first"] = first.value
            last = self._simplified(self._fields[field_name].last)
            assert isinstance(last, Number)
            field["last"] = last.value
            result[field_name] = field
        return result

    def _valid_refinement_condition(self, refinement: RefinementValue) -> bool:
        return self._simplified(refinement.condition) == TRUE

    def _next_link(self, source_field_name: str) -> Optional[Link]:
        field = Field(source_field_name)
        if field == FINAL:
            return None
        for link in self._type.outgoing(field):
            if self._simplified(link.condition) == TRUE:
                return link
        return None

    def _next_field(self, field_name: str, append_to_path: bool = False) -> str:
        if self._skip_verification and field_name != FINAL.name and self._fields[field_name].next:
            return self._fields[field_name].next

        next_link = self._next_link(field_name)
        if append_to_path and next_link is not None:
            self._path.append(next_link)

        return next_link.target.name if next_link is not None else ""

    def _prev_field(self, fld: str) -> str:
        if fld == INITIAL.name:
            return ""
        if self._skip_verification:
            return self._fields[fld].prev
        prev: list[str] = [
            l.source.name
            for l in self._type.incoming(Field(fld))
            if self._simplified(l.condition) == TRUE
        ]

        if len(prev) == 1:
            return prev[0]
        for field in prev:
            if field in self.accessible_fields:
                return field
        return ""

    def _get_size(self, fld: str) -> Optional[Number]:
        typeval = self._fields[fld].typeval
        if isinstance(typeval, ScalarValue):
            return typeval.size
        assert isinstance(typeval, CompositeValue)
        for l in self._type.incoming(Field(fld)):
            if (
                self._fields[l.source.name].set
                and l.size != UNDEFINED
                and (self._skip_verification or self._simplified(l.condition) == TRUE)
            ):
                size = self._simplified(l.size)
                return size if isinstance(size, Number) else None
        return None

    def _get_first(self, fld: str) -> Optional[Number]:
        for l in self._type.incoming(Field(fld)):
            if l.first != UNDEFINED and (
                self._skip_verification or self._simplified(l.condition) == TRUE
            ):
                first = self._simplified(l.first)
                return first if isinstance(first, Number) else None
        prv = self._prev_field(fld)
        if self._skip_verification and prv:
            first = self._fields[prv].first
            size = self._fields[prv].typeval.size
            assert isinstance(first, Number)
            assert isinstance(size, Number)
            return first + size
        if prv and UNDEFINED not in (self._fields[prv].first, self._fields[prv].typeval.size):
            first = self._simplified(Add(self._fields[prv].first, self._fields[prv].typeval.size))
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

    def parse(self, value: Union[Bitstring, bytes], _check: bool = True) -> None:
        assert not self._skip_verification
        self._path.clear()
        if isinstance(value, bytes):
            value = Bitstring.from_bytes(value)
        message_size = len(value)
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

        def set_field_without_size(field_name: str, field: MessageValue.Field) -> tuple[int, int]:
            last_pos_in_bitstr = current_pos_in_bitstring = get_current_pos_in_bitstr(field_name)
            assert isinstance(field.typeval, CompositeValue)
            first = self._get_first(field_name)
            assert first is not None
            field.first = first
            self._set_parsed_value(field_name, value[current_pos_in_bitstring:], message_size)
            return last_pos_in_bitstr, current_pos_in_bitstring

        def set_field_with_size(field_name: str, field_size: int) -> tuple[int, int]:
            assert isinstance(value, Bitstring)
            last_pos_in_bitstr = current_pos_in_bitstring = get_current_pos_in_bitstr(field_name)
            self._set_parsed_value(
                field_name,
                value[current_pos_in_bitstring : current_pos_in_bitstring + field_size],
                message_size,
            )
            current_pos_in_bitstring += field_size
            return last_pos_in_bitstr, current_pos_in_bitstring

        while current_field_name != FINAL.name:
            assert current_field_name, (
                "end of message is not reached, but next field is undefined"
                " (possibly caused by incorrect simplificiation of link condition in _next_link or"
                " check_outgoing_condition_satisfied)"
            )
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
                        f"stopped while parsing field: {current_field_name}",
                    ) from None
            current_field_name = self._next_field(current_field_name, append_to_path=True)

    def _set_unchecked(
        self,
        field_name: str,
        value: Union[bytes, int, str, abc.Sequence[TypeValue]],
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
        self._update_simplified_mapping(field=field)
        self.accessible_fields.append(field_name)

    def _set_checked(
        self,
        field_name: str,
        value: Union[bytes, int, str, abc.Sequence[TypeValue], Bitstring],
        message_size: Optional[int] = None,
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
            simplified = [
                self._simplified(o.condition) for o in self._type.outgoing(Field(field_name))
            ]
            unresolved = [o for o in simplified if o not in (FALSE, TRUE)]
            error_msg = ", ".join([str(o) for o in unresolved])
            assert (
                not unresolved
            ), f"unresolved field conditions in {self.model.name}.{field_name}: {error_msg}"
            if all(o == FALSE for o in simplified):
                self._fields[field_name].typeval.clear()
                raise PyRFLXError(
                    f"none of the field conditions "
                    f"{[str(o.condition) for o in self._type.outgoing(Field(field_name))]}"
                    f" for field {field_name} have been met by the assigned value: {value!s}",
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
                    buffer = (
                        value
                        if self._type.byte_order[Field(field_name)] == ByteOrder.HIGH_ORDER_FIRST
                        or not (isinstance(field.typeval, ScalarValue))
                        or len(value) <= 8
                        or len(value) % 8 != 0
                        else value.swap()
                    )
                    field.typeval.parse(buffer)
                elif isinstance(value, field.typeval.accepted_type):
                    field.typeval.assign(value)
                else:
                    raise PyRFLXError(  # noqa: TRY301
                        f"cannot assign different types: {field.typeval.accepted_type.__name__}"
                        f" != {type(value).__name__}",
                    )
            except PyRFLXError as e:
                new_exception = PyRFLXError(f"cannot set value for field {field_name}")
                new_exception.extend(e)
                raise new_exception from e
        else:
            raise PyRFLXError(f"cannot access field {field_name}")

        self._update_simplified_mapping(message_size)
        check_outgoing_condition_satisfied()

    def set(  # noqa: A003
        self,
        field_name: str,
        value: Union[bytes, int, str, abc.Sequence[TypeValue]],
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
        value: Union[bytes, int, str, abc.Sequence[TypeValue], Bitstring],
        message_size: int,
    ) -> None:
        self._set_checked(field_name, value, message_size)
        self._preset_fields(field_name)

    def _preset_fields(self, fld: str) -> None:
        assert not self._skip_verification
        nxt = self._next_field(fld)
        fields: list[str] = []

        while nxt and nxt != FINAL.name:
            field = self._fields[nxt]
            first = self._get_first(nxt)
            size = self._get_size(nxt)
            if first is None:
                break

            if (self._simplified(self._type.path_condition(Field(nxt))) == TRUE) and (
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

    def set_checksum_function(self, checksums: abc.Mapping[str, ChecksumFunction]) -> None:
        for checksum_field_name, checksum_function in checksums.items():
            if checksum_field_name not in self.fields:
                raise PyRFLXError(
                    f"cannot set checksum function: field {checksum_field_name} is not defined",
                )
            for field_name, checksum in self._checksums.items():
                if field_name == checksum_field_name:
                    checksum.function = checksum_function
                else:
                    raise PyRFLXError(
                        f"cannot set checksum function: field {checksum_field_name} "
                        f"has not been defined as a checksum field",
                    )

    def _is_checksum_settable(self, checksum: MessageValue.Checksum) -> bool:
        def valid_path(value_range: ValueRange) -> bool:
            lower = value_range.lower.substituted(
                func=lambda e: self._fields[self._next_field(INITIAL.name)].name_first
                if e == self._message_first_name
                else e,
            )
            expr: dict[Expr, str] = {}

            for e in [lower, value_range.upper]:
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
            expr_tuple.evaluated_expression = self._simplified(expr_tuple.expression)
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

    def _calculate_checksum(self, checksum: MessageValue.Checksum) -> int:
        if not checksum.function:
            raise PyRFLXError(
                f"cannot calculate checksum for {checksum.field_name}: "
                f"no callable checksum function provided",
            )

        arguments: dict[str, Union[str, int, bytes, tuple[int, int], list[int]]] = {}
        for expr_tuple in checksum.parameters:
            if isinstance(expr_tuple.evaluated_expression, ValueRange):
                assert isinstance(expr_tuple.evaluated_expression.lower, Number)
                assert isinstance(expr_tuple.evaluated_expression.upper, Number)
                arguments[str(expr_tuple.expression)] = (
                    expr_tuple.evaluated_expression.lower.value,
                    expr_tuple.evaluated_expression.upper.value,
                )
            elif isinstance(expr_tuple.evaluated_expression, Aggregate):
                arguments[str(expr_tuple.expression)] = [
                    v.value
                    for v in expr_tuple.evaluated_expression.elements
                    if isinstance(v, Number)
                ]
            elif isinstance(expr_tuple.evaluated_expression, Literal):
                arguments[str(expr_tuple.expression)] = str(expr_tuple.evaluated_expression)
            else:
                assert isinstance(expr_tuple.evaluated_expression, Number)
                arguments[str(expr_tuple.expression)] = expr_tuple.evaluated_expression.value
        return checksum.function(self._unchecked_bytestring(), **arguments)

    def get(self, field_name: str) -> ValueType:
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
                # https://github.com/nedbat/coveragepy/issues/772
                # A dummy statement is needed to disable the peephole optimizer, so that the break
                # statement is detected during coverage analysis.
                # CPython 3.8 and 3.9 are affected. The issue is fixed in CPython 3.10.
                dummy = 0  # noqa: F841
                break
            added_bits = str(self._fields[field].typeval.bitstring)
            added_bits_adjusted = (
                added_bits
                if self._type.byte_order[Field(field)] == ByteOrder.HIGH_ORDER_FIRST
                or not isinstance(self._fields[field].typeval, ScalarValue)
                or len(added_bits) <= 8
                or len(added_bits) % 8 != 0
                else Bitstring.swap_bitstring(added_bits)
            )
            bits = f"{bits[: field_val.first.value]}{added_bits_adjusted}"
            field = self._next_field(field)

        return Bitstring(bits)

    @property
    def value(self) -> ValueType:
        raise NotImplementedError

    def _unchecked_bytestring(self) -> bytes:
        bits = str(self.bitstring)
        assert len(bits) % 8 == 0
        return b"".join(
            [int(bits[i : i + 8], 2).to_bytes(1, "big") for i in range(0, len(bits), 8)],
        )

    @property
    def bytestring(self) -> bytes:
        if not self._skip_verification and not self.valid_message:
            raise PyRFLXError(f"cannot create bytestring of invalid message: {self.identifier}")
        return self._unchecked_bytestring()

    @property
    def fields(self) -> list[str]:
        return [f.name for f in self._type.fields]

    def _is_valid_composite_field(self, field: str) -> bool:
        assert isinstance(self._fields[field].typeval, CompositeValue)
        incoming = self._type.incoming(Field(field))

        for l in incoming:
            if (
                l.size != UNDEFINED
                and self._fields[l.source.name].set
                and self._simplified(l.condition) == TRUE
            ):
                valid_edge = l
                break
        else:
            return False

        return all(
            (v.name in self._fields and self._fields[v.name].set)
            or v in self._parameters
            or v.name == "Message"
            for v in valid_edge.size.variables()
        )

    @property
    def valid_fields(self) -> list[str]:
        return [
            f
            for f in self.accessible_fields
            if (
                self._fields[f].set
                and self._simplified(self._type.path_condition(Field(f))) == TRUE
                and any(
                    self._simplified(o.condition) == TRUE for o in self._type.outgoing(Field(f))
                )
            )
        ]

    @property
    def required_fields(self) -> list[str]:
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

    def _update_simplified_mapping(
        self,
        message_size: Optional[int] = None,
        field: Optional[Field] = None,
    ) -> None:
        if field:
            if isinstance(field.typeval, ScalarValue):
                self._simplified_mapping[field.name_variable] = field.typeval.expr
            last = field.last
            assert isinstance(last, Number)
            self._simplified_mapping[field.name_size] = field.typeval.size
            self._simplified_mapping[field.name_first] = field.first
            self._simplified_mapping[field.name_last] = last
            self._simplified_mapping[self._message_last_name] = (
                Number(message_size - 1) if message_size else last
            )
            self._simplified_mapping[self._message_size_name] = (
                Number(message_size) if message_size else last + Number(1)
            )
            return

        self._simplified_mapping = {
            Size(field_type.full_name): field_type.size
            for field_type in self._type.types.values()
            if isinstance(field_type, Scalar)
        }
        self._simplified_mapping[self._message_first_name] = Number(0)
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
                self._simplified_mapping[self._message_last_name] = (
                    Number(message_size - 1) if message_size else last
                )
                self._simplified_mapping[self._message_size_name] = (
                    Number(message_size) if message_size else last + Number(1)
                )

        # Eng/RecordFlux/RecordFlux#422
        self._simplified_mapping.update({ValidChecksum(f): TRUE for f in self._checksums})

    def _simplified(self, expr: Expr, max_iterations: int = 16) -> Expr:
        if expr in {TRUE, FALSE}:
            return expr

        def subst(expression: Expr) -> Expr:
            if expression in {TRUE, FALSE}:
                return expression
            if expression in self._simplified_mapping:
                assert isinstance(expression, Name)
                return self._simplified_mapping[expression]
            if expression in self._parameters:
                assert isinstance(expression, Name)
                return self._parameters[expression]
            if (
                isinstance(expression, Variable)
                and expression.name in self.fields
                and self._fields[expression.identifier.flat].set
            ):
                exp_value = self._fields[expression.identifier.flat].typeval.value
                if isinstance(exp_value, bytes):
                    return Aggregate(*[Number(b) for b in exp_value])
                if (
                    isinstance(exp_value, list)
                    and len(exp_value) > 0
                    and isinstance(exp_value[0], IntegerValue)
                ):
                    return Aggregate(*[Number(e.value) for e in exp_value])
                raise NotImplementedError
            return expression

        res = expr
        for _ in range(max_iterations):
            res1 = res.substituted(func=subst).simplified()
            if res == res1:
                break
            res = res1
        else:
            fatal_fail(
                f"failed to simplify complex expression `{expr}` "
                f"after `{max_iterations}` iterations, "
                f"best effort: `{res}`",
                Subsystem.PYRFLX,
                Severity.ERROR,
                expr.location,
            )

        return res

    class Checksum:
        def __init__(self, field_name: str, parameters: abc.Sequence[Expr]):
            self.field_name = field_name
            self.function: Optional[ChecksumFunction] = None
            self.calculated = False

            @dataclass
            class Expressiontuple:
                expression: Expr
                evaluated_expression: Expr = UNDEFINED

            self.parameters: list[Expressiontuple] = []
            for expr in parameters:
                assert isinstance(expr, (ValueRange, Attribute, Variable))
                self.parameters.append(Expressiontuple(expr))

    class Field(Base):
        def __init__(  # noqa: PLR0913
            self,
            type_value: TypeValue,
            name: str = "",
            name_variable: Optional[Variable] = None,
            name_first: Optional[First] = None,
            name_last: Optional[Last] = None,
            name_size: Optional[Size] = None,
        ):
            assert name or (name_variable and name_first and name_last and name_size)
            self.typeval = type_value
            self.name_variable = name_variable if name_variable else Variable(name)
            self.name_first = name_first if name_first else First(name)
            self.name_last = name_last if name_last else Last(name)
            self.name_size = name_size if name_size else Size(name)
            self.prev = ""
            self.next = ""

            self._is_scalar = isinstance(self.typeval, ScalarValue)
            self._first: Expr = UNDEFINED
            self._last: Expr = UNDEFINED

        def _calculate_last(self) -> Expr:
            if self.first == UNDEFINED:
                return UNDEFINED
            return Sub(Add(self._first, self.typeval.size), Number(1)).simplified()

        @property
        def first(self) -> Expr:
            return self._first

        @first.setter
        def first(self, first: Expr) -> None:
            self._first = first
            if self._is_scalar:
                self._last = self._calculate_last()

        def __eq__(self, other: object) -> bool:
            if isinstance(other, MessageValue.Field):
                return (
                    self.first == other.first
                    and self.last == other.last
                    and self.typeval == other.typeval
                )
            return NotImplemented

        @property
        def set(self) -> bool:  # noqa: A003
            return (
                self.typeval.initialized
                and isinstance(self.typeval.size, Number)
                and isinstance(self.first, Number)
                and isinstance(self.last, Number)
            )

        @property
        def last(self) -> Expr:
            return self._last if self._is_scalar else self._calculate_last()

    @dataclass
    class State:
        fields: Optional[abc.Mapping[str, MessageValue.Field]] = None
        checksums: Optional[abc.Mapping[str, MessageValue.Checksum]] = None


class RefinementValue:
    def __init__(self, refinement: Refinement, sdu_message: MessageValue) -> None:
        self.package = refinement.package
        self.pdu = refinement.pdu
        self.field = refinement.field
        self.sdu = sdu_message
        self.condition = refinement.condition
