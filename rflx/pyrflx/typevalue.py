from abc import ABC, abstractmethod
from typing import Any, Dict, List, Mapping, Optional, Sequence, Tuple, Union

from rflx.common import generic_repr
from rflx.expression import (
    FALSE,
    TRUE,
    UNDEFINED,
    Add,
    And,
    Expr,
    First,
    Last,
    Length,
    Name,
    Sub,
    Variable,
)
from rflx.identifier import ID
from rflx.model import (
    FINAL,
    INITIAL,
    Array,
    Composite,
    Enumeration,
    Field,
    Integer,
    Message,
    Number,
    Opaque,
    Refinement,
    Scalar,
    Type,
)
from rflx.pyrflx.bitstring import Bitstring


class NotInitializedError(Exception):
    pass


class TypeValue(ABC):

    _value: Any = None

    def __init__(self, vtype: Type) -> None:
        self._type = vtype

    def __repr__(self) -> str:
        return generic_repr(self.__class__.__name__, self.__dict__)

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
            raise NotInitializedError("value not initialized")

    def clear(self) -> None:
        self._value = None

    @abstractmethod
    def assign(self, value: Any, check: bool = True) -> None:
        raise NotImplementedError

    @abstractmethod
    def parse(self, value: Union[Bitstring, bytes]) -> None:
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
    def value(self) -> Any:
        raise NotImplementedError

    @property
    @abstractmethod
    def accepted_type(self) -> type:
        raise NotImplementedError

    @classmethod
    def construct(cls, vtype: Type, refinements: Sequence[Refinement] = None) -> "TypeValue":
        if isinstance(vtype, Integer):
            return IntegerValue(vtype)
        if isinstance(vtype, Enumeration):
            return EnumValue(vtype)
        if isinstance(vtype, Opaque):
            return OpaqueValue(vtype)
        if isinstance(vtype, Array):
            return ArrayValue(vtype)
        if isinstance(vtype, Message):
            return MessageValue(vtype, refinements)
        raise ValueError("cannot construct unknown type: " + type(vtype).__name__)


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
        size_expr = self._type.size.simplified()
        assert isinstance(size_expr, Number)
        return size_expr


class IntegerValue(ScalarValue):

    _value: int
    _type: Integer

    def __init__(self, vtype: Integer) -> None:
        super().__init__(vtype)

    @property
    def _first(self) -> int:
        first = self._type.first.simplified()
        assert isinstance(first, Number)
        return first.value

    @property
    def _last(self) -> int:
        last = self._type.last.simplified()
        assert isinstance(last, Number)
        return last.value

    def assign(self, value: int, check: bool = True) -> None:
        if (
            And(*self._type.constraints("__VALUE__", check))
            .substituted(
                mapping={Variable("__VALUE__"): Number(value), Length("__VALUE__"): self._type.size}
            )
            .simplified()
            != TRUE
        ):
            raise ValueError(f"value {value} not in type range {self._first} .. {self._last}")
        self._value = value

    def parse(self, value: Union[Bitstring, bytes]) -> None:
        if isinstance(value, bytes):
            value = Bitstring.from_bytes(value)
        self.assign(int(value))

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

    _value: Tuple[str, Number]
    _type: Enumeration

    def __init__(self, vtype: Enumeration) -> None:
        super().__init__(vtype)

    def assign(self, value: str, check: bool = True) -> None:
        if value not in self._type.literals:
            raise KeyError(f"{value} is not a valid enum value")
        r = (
            And(*self._type.constraints("__VALUE__", check))
            .substituted(
                mapping={
                    **{Variable(k): v for k, v in self._type.literals.items()},
                    **{Variable("__VALUE__"): self._type.literals[value]},
                    **{Length("__VALUE__"): self._type.size},
                }
            )
            .simplified()
        )
        assert r == TRUE
        self._value = value, self._type.literals[value]

    def parse(self, value: Union[Bitstring, bytes]) -> None:
        if isinstance(value, bytes):
            value = Bitstring.from_bytes(value)
        value_as_number = Number(int(value))
        if value_as_number not in self.literals.values():
            if self._type.always_valid:
                self._value = "UNKNOWN", value_as_number
            else:
                raise KeyError(f"Number {value_as_number.value} is not a valid enum value")
        else:
            for k, v in self.literals.items():
                if v == value_as_number:
                    assert isinstance(v, Number)
                    self._value = str(k), v

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
    def literals(self) -> Mapping[Name, Expr]:
        return {Variable(k): v for k, v in self._type.literals.items()}


class CompositeValue(TypeValue):
    def __init__(self, vtype: Composite) -> None:
        self._expected_size: Optional[Expr] = None
        super().__init__(vtype)

    def set_expected_size(self, expected_size: Expr) -> None:
        self._expected_size = expected_size

    def _check_length_of_assigned_value(
        self, value: Union[bytes, Bitstring, List[TypeValue]]
    ) -> None:
        if isinstance(value, bytes):
            length_of_value = len(value) * 8
        elif isinstance(value, Bitstring):
            length_of_value = len(value)
        else:
            bits = [element.bitstring for element in value]
            length_of_value = len(Bitstring.join(bits))

        if (
            self._expected_size is not None
            and isinstance(self._expected_size, Number)
            and length_of_value != self._expected_size.value
        ):
            raise ValueError(
                f"invalid data length: input length is {len(value) * 8} "
                f"while expected input length is {self._expected_size.value}"
            )

    @property
    @abstractmethod
    def value(self) -> Any:
        raise NotImplementedError


class OpaqueValue(CompositeValue):

    _value: bytes
    _nested_message: Optional["MessageValue"] = None

    def __init__(self, vtype: Opaque) -> None:
        super().__init__(vtype)
        self._refinement_message: Optional[Message] = None
        self._all_refinements: Sequence[Refinement] = []

    def assign(self, value: bytes, check: bool = True) -> None:
        self.parse(value)

    def parse(self, value: Union[Bitstring, bytes]) -> None:
        self._check_length_of_assigned_value(value)
        if self._refinement_message is not None:
            nested_msg = MessageValue(self._refinement_message, self._all_refinements)
            try:
                nested_msg.parse(value)
            except (IndexError, ValueError, KeyError) as e:
                raise ValueError(
                    f"Error while parsing nested message "
                    f"{self._refinement_message.identifier}: {e}"
                )
            assert nested_msg.valid_message
            self._nested_message = nested_msg
            self._value = nested_msg.bytestring
        else:
            self._value = bytes(value)

    def set_refinement(
        self, model_of_refinement_msg: Message, all_refinements: Sequence[Refinement]
    ) -> None:
        self._refinement_message = model_of_refinement_msg
        self._all_refinements = all_refinements

    @property
    def size(self) -> Expr:
        if self._value is None:
            return self._expected_size if self._expected_size is not None else UNDEFINED
        return Number(len(self._value) * 8)

    @property
    def nested_message(self) -> Optional["MessageValue"]:
        self._raise_initialized()
        return self._nested_message

    @property
    def value(self) -> bytes:
        self._raise_initialized()
        return self._value

    @property
    def bitstring(self) -> Bitstring:
        self._raise_initialized()
        return Bitstring(format(int.from_bytes(self._value, "big"), f"0{self.size}b"))

    @property
    def accepted_type(self) -> type:
        return bytes


class ArrayValue(CompositeValue):

    _value: List[TypeValue]

    def __init__(self, vtype: Array) -> None:
        super().__init__(vtype)
        self._element_type = vtype.element_type
        self._is_message_array = isinstance(self._element_type, Message)
        self._value = []

    def assign(self, value: List[TypeValue], check: bool = True) -> None:
        self._check_length_of_assigned_value(value)
        for v in value:
            if self._is_message_array:
                if isinstance(v, MessageValue):
                    assert isinstance(self._element_type, Message)
                    if not v.equal_type(self._element_type):
                        raise ValueError(
                            f'cannot assign "{v.name}" to an array of "{self._element_type.name}"'
                        )
                    if not v.valid_message:
                        raise ValueError(
                            f'cannot assign message "{v.name}" to array of messages: '
                            f"all messages must be valid"
                        )
                else:
                    raise ValueError(
                        f"cannot assign {type(v).__name__} to an array of "
                        f"{type(self._element_type).__name__}"
                    )
            else:
                if isinstance(v, MessageValue) or not v.equal_type(self._element_type):
                    raise ValueError(
                        f"cannot assign {type(v).__name__} to an array of "
                        f"{type(self._element_type).__name__}"
                    )

        self._value = value

    def parse(self, value: Union[Bitstring, bytes]) -> None:
        self._check_length_of_assigned_value(value)
        if isinstance(value, bytes):
            value = Bitstring.from_bytes(value)
        if self._is_message_array:

            while len(value) != 0:
                nested_message = TypeValue.construct(self._element_type)
                assert isinstance(nested_message, MessageValue)
                try:
                    nested_message.parse(value)
                except (IndexError, ValueError, KeyError) as e:
                    raise ValueError(
                        f"cannot parse nested messages in array of type "
                        f"{self._element_type.full_name}: {e}"
                    )
                assert nested_message.valid_message
                self._value.append(nested_message)
                value = value[len(nested_message.bitstring) :]

        elif isinstance(self._element_type, Scalar):
            value_str = str(value)
            type_size = self._element_type.size
            assert isinstance(type_size, Number)
            type_size_int = type_size.value
            new_value = []

            while len(value_str) != 0:
                nested_value = TypeValue.construct(self._element_type)
                nested_value.parse(Bitstring(value_str[:type_size_int]))
                new_value.append(nested_value)
                value_str = value_str[type_size_int:]

            self._value = new_value
        else:
            raise NotImplementedError(f"Arrays of {self._element_type} currently not supported")

    @property
    def size(self) -> Expr:
        if not self._value:
            return self._expected_size if self._expected_size is not None else UNDEFINED
        return Number(len(self.bitstring))

    @property
    def value(self) -> Sequence[TypeValue]:
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


class MessageValue(TypeValue):

    _type: Message

    def __init__(self, model: Message, refinements: Sequence[Refinement] = None) -> None:
        super().__init__(model)
        self._refinements = refinements or []
        self._fields: Dict[str, MessageValue.Field] = {
            f.name: self.Field(TypeValue.construct(self._type.types[f])) for f in self._type.fields
        }
        self.__type_literals: Mapping[Name, Expr] = {}
        self._last_field: str = self._next_field(INITIAL.name)
        for t in [
            f.typeval.literals for f in self._fields.values() if isinstance(f.typeval, EnumValue)
        ]:
            self.__type_literals = {**self.__type_literals, **t}
        initial = self.Field(OpaqueValue(Opaque()))
        initial.first = Number(0)
        initial.typeval.assign(bytes())
        self._fields[INITIAL.name] = initial
        self._preset_fields(INITIAL.name)

    def __copy__(self) -> "MessageValue":
        return MessageValue(self._type, self._refinements)

    def __repr__(self) -> str:
        return generic_repr(self.__class__.__name__, self.__dict__)

    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return self._fields == other._fields and self._type == other._type
        return NotImplemented

    def equal_type(self, other: Type) -> bool:
        return self.identifier == other.identifier

    def _valid_refinement_condition(self, refinement: Refinement) -> bool:
        return self.__simplified(refinement.condition) == TRUE

    def _next_field(self, fld: str) -> str:
        if fld == FINAL.name:
            return ""
        if fld == INITIAL.name:
            links = self._type.outgoing(INITIAL)
            if not links:
                return FINAL.name
            return links[0].target.name

        for l in self._type.outgoing(Field(fld)):
            if self.__simplified(l.condition) == TRUE:
                return l.target.name
        return ""

    def _prev_field(self, fld: str) -> str:
        if fld == INITIAL.name:
            return ""
        for l in self._type.incoming(Field(fld)):
            if self.__simplified(l.condition) == TRUE:
                return l.source.name
        return ""

    def _get_length_unchecked(self, fld: str) -> Expr:
        for l in self._type.incoming(Field(fld)):
            if self.__simplified(l.condition) == TRUE and l.length != UNDEFINED:
                return self.__simplified(l.length)

        typeval = self._fields[fld].typeval
        if isinstance(typeval, ScalarValue):
            return typeval.size
        return UNDEFINED

    def _has_length(self, fld: str) -> bool:
        return isinstance(self._get_length_unchecked(fld), Number)

    def _get_length(self, fld: str) -> Number:
        length = self._get_length_unchecked(fld)
        assert isinstance(length, Number)
        return length

    def _get_first_unchecked(self, fld: str) -> Expr:
        for l in self._type.incoming(Field(fld)):
            if self.__simplified(l.condition) == TRUE and l.first != UNDEFINED:
                return self.__simplified(l.first)
        prv = self._prev_field(fld)
        if prv:
            return self.__simplified(Add(self._fields[prv].first, self._fields[prv].typeval.size))
        return UNDEFINED

    def _has_first(self, fld: str) -> bool:
        return isinstance(self._get_first_unchecked(fld), Number)

    def _get_first(self, fld: str) -> Number:
        first = self._get_first_unchecked(fld)
        assert isinstance(first, Number)
        return first

    @property
    def accepted_type(self) -> type:
        return bytes

    @property
    def size(self) -> Number:
        return Number(len(self.bitstring))

    def assign(self, value: bytes, check: bool = True) -> None:
        raise NotImplementedError

    def parse(self, value: Union[Bitstring, bytes]) -> None:
        if isinstance(value, bytes):
            value = Bitstring.from_bytes(value)
        current_field_name = self._next_field(INITIAL.name)
        last_field_first_in_bitstr = current_field_first_in_bitstr = 0
        current_field_length = 0

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

        def set_field_without_length(field_name: str, field: MessageValue.Field) -> Tuple[int, int]:
            last_pos_in_bitstr = current_pos_in_bitstring = get_current_pos_in_bitstr(field_name)
            assert isinstance(field.typeval, OpaqueValue)
            field.first = self._get_first(field_name)
            self.set(field_name, value[current_pos_in_bitstring:])
            return last_pos_in_bitstr, current_pos_in_bitstring

        def set_field_with_length(field_name: str, field_length: int) -> Tuple[int, int]:
            assert isinstance(value, Bitstring)
            last_pos_in_bitstr = current_pos_in_bitstring = get_current_pos_in_bitstr(field_name)
            if field_length < 8 or field_length % 8 == 0:
                self.set(
                    field_name,
                    value[current_pos_in_bitstring : current_pos_in_bitstring + field_length],
                )
                current_pos_in_bitstring += field_length
            else:
                bytes_used_for_field = field_length // 8 + 1
                first_pos = current_pos_in_bitstring
                field_bits = Bitstring()

                for _ in range(bytes_used_for_field - 1):
                    field_bits += value[current_pos_in_bitstring : current_pos_in_bitstring + 8]
                    current_pos_in_bitstring += 8

                k = field_length // bytes_used_for_field + 1
                field_bits += value[current_pos_in_bitstring + 8 - k : first_pos + field_length]
                current_pos_in_bitstring = first_pos + field_length
                self.set(field_name, field_bits)
            return last_pos_in_bitstr, current_pos_in_bitstring

        while current_field_name != FINAL.name:
            current_field = self._fields[current_field_name]
            if isinstance(current_field.typeval, OpaqueValue) and not self._has_length(
                current_field_name
            ):
                (
                    last_field_first_in_bitstr,
                    current_field_first_in_bitstr,
                ) = set_field_without_length(current_field_name, current_field)

            else:
                assert self._has_length(current_field_name)
                current_field_length = self._get_length(current_field_name).value
                try:
                    (
                        last_field_first_in_bitstr,
                        current_field_first_in_bitstr,
                    ) = set_field_with_length(current_field_name, current_field_length)
                except IndexError:
                    raise IndexError(
                        f"Bitstring representing the message is too short - "
                        f"stopped while parsing field: {current_field_name}"
                    )
            current_field_name = self._next_field(current_field_name)

    def set(
        self, field_name: str, value: Union[bytes, int, str, Sequence[TypeValue], Bitstring]
    ) -> None:
        def set_refinement(fld: MessageValue.Field, fld_name: str) -> None:
            if isinstance(fld.typeval, OpaqueValue):
                for ref in self._refinements:
                    if (
                        ref.pdu.name == self.name
                        and ref.field.name == fld_name
                        and self._valid_refinement_condition(ref)
                    ):
                        fld.typeval.set_refinement(ref.sdu, self._refinements)

        if field_name in self.accessible_fields:
            field = self._fields[field_name]
            field.first = self._get_first(field_name)
            if isinstance(field.typeval, CompositeValue) and self._has_length(field_name):
                field.typeval.set_expected_size(self._get_length(field_name))
            set_refinement(field, field_name)
            try:
                if isinstance(value, Bitstring):
                    field.typeval.parse(value)
                elif isinstance(value, field.typeval.accepted_type):
                    field.typeval.assign(value)
                else:
                    raise TypeError(
                        f"cannot assign different types: {field.typeval.accepted_type.__name__}"
                        f" != {type(value).__name__}"
                    )
            except (ValueError, KeyError, TypeError) as e:
                raise ValueError(f"Error while setting value for field {field_name}: {e}")
        else:
            raise KeyError(f"cannot access field {field_name}")

        if all(
            [
                self.__simplified(o.condition) == FALSE
                for o in self._type.outgoing(Field(field_name))
            ]
        ):
            self._fields[field_name].typeval.clear()
            if isinstance(value, bytes):
                value_repr = "x" + value.hex()
            else:
                value_repr = str(value)

            raise ValueError(
                f"none of the field conditions "
                f"{[str(o.condition) for o in self._type.outgoing(Field(field_name))]}"
                f" for field {field_name} have been met by the assigned value: {value_repr}"
            )

        self._preset_fields(field_name)

    def _preset_fields(self, fld: str) -> None:
        nxt = self._next_field(fld)
        while nxt and nxt != FINAL.name:
            field = self._fields[nxt]
            if not self._has_first(nxt) or not self._has_length(nxt):
                break

            field.first = self._get_first(nxt)
            if isinstance(field.typeval, OpaqueValue):
                field.typeval.set_expected_size(self._get_length(nxt))

            if field.set and isinstance(field.typeval, OpaqueValue):
                field.first = UNDEFINED
                field.typeval.clear()
                break

            self._last_field = nxt
            nxt = self._next_field(nxt)

    def get(self, field_name: str) -> Union["MessageValue", Sequence[TypeValue], int, str, bytes]:
        if field_name not in self.valid_fields:
            raise ValueError(f"field {field_name} not valid")
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
            bits = bits[: field_val.first.value] + str(self._fields[field].typeval.bitstring)
            field = self._next_field(field)

        return Bitstring(bits)

    @property
    def value(self) -> Any:
        raise NotImplementedError

    @property
    def bytestring(self) -> bytes:
        bits = str(self.bitstring)
        if len(bits) < 8:
            bits = bits.ljust(8, "0")

        return b"".join(
            [int(bits[i : i + 8], 2).to_bytes(1, "big") for i in range(0, len(bits), 8)]
        )

    @property
    def fields(self) -> List[str]:
        return [f.name for f in self._type.fields]

    @property
    def accessible_fields(self) -> List[str]:
        nxt = self._next_field(INITIAL.name)
        fields: List[str] = []
        while nxt and nxt != FINAL.name:

            if (
                self.__simplified(self._type.field_condition(Field(nxt))) != TRUE
                or not self._has_first(nxt)
                or (
                    not self._has_length(nxt)
                    if not isinstance(self._fields[nxt].typeval, OpaqueValue)
                    else not self._is_valid_opaque_field(nxt)
                )
            ):
                break

            fields.append(nxt)
            nxt = self._next_field(nxt)
        return fields

    def _is_valid_opaque_field(self, field: str) -> bool:
        if self._get_length_unchecked(field) == UNDEFINED:
            return False

        for edge in self._type.incoming(Field(field)):
            if self.__simplified(edge.condition) == TRUE:
                valid_edge = edge
                break
        else:
            return True

        return all(
            [
                (v.name in self._fields and self._fields[v.name].set) or v.name == "Message"
                for v in valid_edge.length.variables()
            ]
        )

    @property
    def valid_fields(self) -> List[str]:
        return [
            f
            for f in self.accessible_fields
            if (
                self._fields[f].set
                and self.__simplified(self._type.field_condition(Field(f))) == TRUE
                and any(
                    [self.__simplified(i.condition) == TRUE for i in self._type.incoming(Field(f))]
                )
                and any(
                    [self.__simplified(o.condition) == TRUE for o in self._type.outgoing(Field(f))]
                )
            )
        ]

    @property
    def required_fields(self) -> List[str]:
        accessible = self.accessible_fields
        valid = self.valid_fields
        return [f for f in accessible if f not in valid]

    @property
    def valid_message(self) -> bool:
        return bool(self.valid_fields) and self._next_field(self.valid_fields[-1]) == FINAL.name

    def __simplified(self, expr: Expr) -> Expr:
        field_values: Mapping[Name, Expr] = {
            **{
                Variable(k): v.typeval.expr
                for k, v in self._fields.items()
                if isinstance(v.typeval, ScalarValue) and v.set
            },
            **{Length(k): v.typeval.size for k, v in self._fields.items() if v.set},
            **{First(k): v.first for k, v in self._fields.items() if v.set},
            **{Last(k): v.last for k, v in self._fields.items() if v.set},
        }

        mapping = {**field_values, **self.__type_literals}

        return expr.substituted(mapping=mapping).substituted(mapping=mapping).simplified()

    class Field:
        def __init__(self, t: TypeValue):
            self.typeval = t
            self.first: Expr = UNDEFINED

        def __eq__(self, other: object) -> bool:
            if isinstance(other, MessageValue.Field):
                return (
                    self.first == other.first
                    and self.last == other.last
                    and self.typeval == other.typeval
                )
            return NotImplemented

        def __repr__(self) -> str:
            return generic_repr(self.__class__.__name__, self.__dict__)

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
            return Sub(Add(self.first, self.typeval.size), Number(1)).simplified()
