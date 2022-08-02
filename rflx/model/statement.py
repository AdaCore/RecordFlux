from abc import abstractmethod
from typing import Callable, Dict, List, Sequence

import rflx.typing_ as rty
from rflx.common import Base
from rflx.error import Location, RecordFluxError, Severity, Subsystem
from rflx.expression import Expr, Variable
from rflx.identifier import ID, StrID


class Statement(Base):
    def __init__(
        self, identifier: StrID, type_: rty.Type = rty.Undefined(), location: Location = None
    ):
        self.identifier = ID(identifier)
        self.type_ = type_
        self.location = location

    @abstractmethod
    def check_type(
        self, statement_type: rty.Type, typify_variable: Callable[[Expr], Expr]
    ) -> RecordFluxError:
        """Set the types of variables, and check the types of the statement and expressions."""
        raise NotImplementedError

    @abstractmethod
    def variables(self) -> Sequence[Variable]:
        """Return all referenced variables in the statement."""
        raise NotImplementedError


class Assignment(Statement):
    def __init__(
        self,
        identifier: StrID,
        expression: Expr,
        type_: rty.Type = rty.Undefined(),
        location: Location = None,
    ) -> None:
        super().__init__(identifier, type_, location)
        self.expression = expression

    def variables(self) -> Sequence[Variable]:
        return [Variable(self.identifier), *self.expression.variables()]


class VariableAssignment(Assignment):
    def __str__(self) -> str:
        return f"{self.identifier} := {self.expression}"

    def check_type(
        self, statement_type: rty.Type, typify_variable: Callable[[Expr], Expr]
    ) -> RecordFluxError:
        self.type_ = statement_type
        self.expression = self.expression.substituted(typify_variable)
        return rty.check_type_instance(
            statement_type, rty.Any, self.location, f'variable "{self.identifier}"'
        ) + self.expression.check_type(statement_type)


class MessageFieldAssignment(Assignment):
    def __init__(
        self,
        message: StrID,
        field: StrID,
        expression: Expr,
        type_: rty.Type = rty.Undefined(),
        location: Location = None,
    ) -> None:
        super().__init__(message, expression, type_, location)
        self.message = ID(message)
        self.field = ID(field)

    def __str__(self) -> str:
        return f"{self.message}.{self.field} := {self.expression}"

    def check_type(
        self, statement_type: rty.Type, typify_variable: Callable[[Expr], Expr]
    ) -> RecordFluxError:
        error = RecordFluxError()
        field_type: rty.Type = rty.Undefined()
        if isinstance(statement_type, rty.Message):
            if self.field in statement_type.fields:
                field_type = statement_type.types[self.field]
            elif self.field in statement_type.parameters:
                error.extend(
                    [
                        (
                            f'message parameter "{self.field}" cannot be set using an assignment',
                            Subsystem.MODEL,
                            Severity.ERROR,
                            self.field.location,
                        ),
                        (
                            "use a Reset statement to change the message parameters",
                            Subsystem.MODEL,
                            Severity.INFO,
                            self.field.location,
                        ),
                    ],
                )
            else:
                error.extend(
                    [
                        (
                            f'invalid message field "{self.field}"',
                            Subsystem.MODEL,
                            Severity.ERROR,
                            self.field.location,
                        ),
                    ],
                )
        self.type_ = statement_type
        self.expression = self.expression.substituted(typify_variable)
        return (
            error
            + rty.check_type_instance(
                statement_type, rty.Message, self.location, f'variable "{self.identifier}"'
            )
            + self.expression.check_type(field_type)
        )


class AttributeStatement(Statement):
    def __init__(
        self,
        identifier: StrID,
        attribute: str,
        parameters: List[Expr],
        type_: rty.Type = rty.Undefined(),
        location: Location = None,
    ) -> None:
        super().__init__(identifier, type_, location)
        self.attribute = attribute
        self.parameters = parameters

    def __str__(self) -> str:
        parameters = ", ".join([str(p) for p in self.parameters])
        return f"{self.identifier}'{self.attribute}" + (f" ({parameters})" if parameters else "")

    def check_type(
        self, statement_type: rty.Type, typify_variable: Callable[[Expr], Expr]
    ) -> RecordFluxError:
        raise NotImplementedError

    def variables(self) -> Sequence[Variable]:
        return [Variable(self.identifier), *[e for p in self.parameters for e in p.variables()]]


class ListAttributeStatement(AttributeStatement):
    def __init__(
        self,
        identifier: StrID,
        parameter: Expr,
        type_: rty.Type = rty.Undefined(),
        location: Location = None,
    ) -> None:
        super().__init__(identifier, self.__class__.__name__, [parameter], type_, location)


class Append(ListAttributeStatement):
    def check_type(
        self, statement_type: rty.Type, typify_variable: Callable[[Expr], Expr]
    ) -> RecordFluxError:
        self.type_ = statement_type
        self.parameter = self.parameter.substituted(typify_variable)
        error = rty.check_type_instance(
            statement_type, rty.Sequence, self.location, f'variable "{self.identifier}"'
        )
        if isinstance(statement_type, rty.Sequence):
            error += self.parameter.check_type(statement_type.element)
            if isinstance(statement_type.element, rty.Message) and isinstance(
                self.parameter, Variable
            ):
                error.extend(
                    [
                        (
                            "appending independently created message not supported",
                            Subsystem.MODEL,
                            Severity.ERROR,
                            self.parameter.location,
                        ),
                        (
                            "message aggregate should be used instead",
                            Subsystem.MODEL,
                            Severity.INFO,
                            self.parameter.location,
                        ),
                    ],
                )
        return error

    @property
    def parameter(self) -> Expr:
        return self.parameters[0]

    @parameter.setter
    def parameter(self, value: Expr) -> None:
        assert isinstance(self.parameters, list)
        self.parameters[0] = value


class Extend(ListAttributeStatement):
    def check_type(
        self, statement_type: rty.Type, typify_variable: Callable[[Expr], Expr]
    ) -> RecordFluxError:
        self.type_ = statement_type
        self.parameters[0] = self.parameters[0].substituted(typify_variable)
        return rty.check_type_instance(
            statement_type, rty.Sequence, self.location, f'variable "{self.identifier}"'
        ) + self.parameters[0].check_type(statement_type)


class Reset(AttributeStatement):
    def __init__(
        self,
        identifier: StrID,
        associations: Dict[ID, Expr] = None,
        type_: rty.Type = rty.Undefined(),
        location: Location = None,
    ) -> None:
        super().__init__(identifier, self.__class__.__name__, [], type_, location)
        self.associations = associations or {}

    def check_type(
        self, statement_type: rty.Type, typify_variable: Callable[[Expr], Expr]
    ) -> RecordFluxError:
        self.type_ = statement_type
        self.associations = {
            i: e.substituted(typify_variable) for i, e in self.associations.items()
        }
        return rty.check_type_instance(
            statement_type,
            (rty.Sequence, rty.Message),
            self.location,
            f'variable "{self.identifier}"',
        )

    def variables(self) -> Sequence[Variable]:
        return [
            Variable(self.identifier),
            *[v for e in self.associations.values() for v in e.variables()],
        ]


class ChannelAttributeStatement(AttributeStatement):
    def __init__(
        self,
        identifier: StrID,
        parameter: Expr,
        type_: rty.Type = rty.Undefined(),
        location: Location = None,
    ) -> None:
        super().__init__(identifier, self.__class__.__name__, [parameter], type_, location)

    def check_type(
        self, statement_type: rty.Type, typify_variable: Callable[[Expr], Expr]
    ) -> RecordFluxError:
        self.type_ = statement_type
        self.parameters = [self.parameter.substituted(typify_variable)]
        return rty.check_type(
            statement_type,
            self._expected_channel_type(),
            self.location,
            f'channel "{self.identifier}"',
        ) + self.parameter.check_type_instance(rty.Message)

    @property
    def parameter(self) -> Expr:
        return self.parameters[0]

    @abstractmethod
    def _expected_channel_type(self) -> rty.Channel:
        raise NotImplementedError


class Read(ChannelAttributeStatement):
    def _expected_channel_type(self) -> rty.Channel:
        return rty.Channel(readable=True, writable=False)


class Write(ChannelAttributeStatement):
    def _expected_channel_type(self) -> rty.Channel:
        return rty.Channel(readable=False, writable=True)
