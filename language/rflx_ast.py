from typing import ClassVar

from langkit.dsl import ASTNode, Field, abstract


@abstract
class RFLXNode(ASTNode):  # type: ignore[misc]
    """Root node class for the RecordFlux language."""


@abstract
class AbstractID(RFLXNode):
    """Base class for identifiers."""


class UnqualifiedID(AbstractID):
    """Simple, unqualified identifiers, i.e. identifiers without a package part (e.g. "Foo")."""

    token_node = True


class ID(AbstractID):
    """Qualified identifiers which may optionally have a package part (e.g. "Pkg::Foo", "Foo")."""

    package = Field(type=UnqualifiedID)
    name = Field(type=UnqualifiedID)


class Parameter(RFLXNode):
    identifier = Field(type=UnqualifiedID)
    type_identifier = Field(type=ID)


class Parameters(RFLXNode):
    parameters = Field(type=Parameter.list)


@abstract
class Declaration(RFLXNode):
    """Base class for declarations (types, refinements, sessions)."""


@abstract
class TypeDef(RFLXNode):
    """Base class for type definitions (integers, messages, type derivations, sequences, enums)."""


class TypeDecl(Declaration):
    """Type declaration (type Foo is ...)."""

    identifier = Field(type=UnqualifiedID)
    parameters = Field(type=Parameters)
    definition = Field(type=TypeDef)


@abstract
class Expr(RFLXNode):
    """Base class for expressions."""


class Op(RFLXNode):
    """Operators for binary expressions."""

    enum_node = True
    alternatives: ClassVar[list[str]] = [
        "pow",
        "mul",
        "div",
        "add",
        "sub",
        "mod",
        "eq",
        "neq",
        "le",
        "lt",
        "gt",
        "ge",
        "and",
        "or",
        "in",
        "notin",
    ]


class Negation(Expr):
    data = Field(type=Expr)


class BinOp(Expr):
    """Binary operation."""

    left = Field(type=Expr)
    op = Field(type=Op)
    right = Field(type=Expr)


class ParenExpression(Expr):
    """Parenthesized expression."""

    data = Field(type=Expr)


class MessageAggregateAssociation(RFLXNode):
    identifier = Field(type=UnqualifiedID)
    expression = Field(type=Expr)


@abstract
class BaseAggregate(RFLXNode):
    """Base class for message aggregates."""


class NullMessageAggregate(BaseAggregate):
    pass


class MessageAggregateAssociations(BaseAggregate):
    associations = Field(type=MessageAggregateAssociation.list)


class MessageAggregate(Expr):
    identifier = Field(type=ID)
    values = Field(type=BaseAggregate)


class RefinementDecl(Declaration):
    """Refinement declaration (for Message use (Field => Inner_Type))."""

    pdu = Field(type=ID)
    field = Field(type=UnqualifiedID)
    sdu = Field(type=ID)
    condition = Field(type=Expr)


@abstract
class FormalDecl(RFLXNode):
    """Base class for generic formal session declarations."""


@abstract
class LocalDecl(RFLXNode):
    """Base class for session or state local declarations."""


class VariableDecl(LocalDecl):
    """Session variable declaration."""

    identifier = Field(type=UnqualifiedID)
    type_identifier = Field(type=ID)
    initializer = Field(type=Expr)


class RenamingDecl(LocalDecl):
    """Session renaming declaration."""

    identifier = Field(type=UnqualifiedID)
    type_identifier = Field(type=ID)
    expression = Field(type=Expr)


@abstract
class Statement(RFLXNode):
    """Base class for statements."""


class Assignment(Statement):
    """Assignment of expression to unqualified identifier."""

    identifier = Field(type=UnqualifiedID)
    expression = Field(type=Expr)


class MessageFieldAssignment(Statement):
    """Assignment of expression to message field."""

    message = Field(type=UnqualifiedID)
    field = Field(type=UnqualifiedID)
    expression = Field(type=Expr)


class AttrStmt(RFLXNode):
    """Attribute statement kind."""

    enum_node = True
    alternatives: ClassVar[list[str]] = [
        "append",
        "extend",
        "read",
        "write",
    ]


class AttributeStatement(Statement):
    """Attribute statement."""

    identifier = Field(type=UnqualifiedID)
    attr = Field(type=AttrStmt)
    expression = Field(type=Expr)


class Reset(Statement):
    """Reset statement."""

    identifier = Field(type=UnqualifiedID)
    associations = Field(type=MessageAggregateAssociation.list)


@abstract
class SequenceLiteral(Expr):
    """Base class for sequence literals (strings, sequence aggregates)."""


class StringLiteral(SequenceLiteral):
    """Double-quoted string literal."""

    token_node = True


class NumericLiteral(Expr):
    token_node = True


class SequenceAggregate(SequenceLiteral):
    """List of literal sequence values."""

    values = Field(type=NumericLiteral.list)


class Concatenation(SequenceLiteral):
    """Concatenation of aggregates or string literals."""

    left = Field(type=SequenceLiteral)
    right = Field(type=SequenceLiteral)


class Description(RFLXNode):
    """String description of an entity."""

    content = Field(type=StringLiteral)


class Transition(RFLXNode):
    """Unconditional session state transition."""

    target = Field(type=UnqualifiedID)
    description = Field(type=Description)


class ConditionalTransition(Transition):
    """Conditional session state transition."""

    condition = Field(type=Expr)


class StateBody(RFLXNode):
    """Body of a session state."""

    declarations = Field(type=LocalDecl.list)
    actions = Field(type=Statement.list)
    conditional_transitions = Field(type=ConditionalTransition.list)
    final_transition = Field(type=Transition)
    exception_transition = Field(type=Transition)
    end_identifier = Field(type=UnqualifiedID)


class State(RFLXNode):
    """Session state."""

    identifier = Field(type=UnqualifiedID)
    description = Field(type=Description)
    body = Field(type=StateBody)


class SessionDecl(Declaration):
    parameters = Field(type=FormalDecl.list)
    identifier = Field(type=UnqualifiedID)
    declarations = Field(type=LocalDecl.list)
    states = Field(type=State.list)
    end_identifier = Field(type=UnqualifiedID)


class Package(RFLXNode):
    identifier = Field(type=UnqualifiedID)
    declarations = Field(type=Declaration.list)
    end_identifier = Field(type=UnqualifiedID)


@abstract
class IntegerTypeDef(TypeDef):
    """Base class for all integer type definitions."""


class Aspect(RFLXNode):
    identifier = Field(type=UnqualifiedID)
    value = Field(type=Expr)


class RangeTypeDef(IntegerTypeDef):
    first = Field(type=Expr)
    last = Field(type=Expr)
    size = Field(type=Aspect)


class ModularTypeDef(IntegerTypeDef):
    mod = Field(type=Expr)


@abstract
class AbstractMessageTypeDef(TypeDef):
    """Base class for message type definitions."""


class NullMessageTypeDef(AbstractMessageTypeDef):
    pass


class TypeDerivationDef(TypeDef):
    base = Field(type=ID)


class SequenceTypeDef(TypeDef):
    element_type = Field(type=ID)


@abstract
class EnumerationDef(TypeDef):
    """Base class for enumeration definitions."""


class PositionalEnumerationDef(EnumerationDef):
    elements = Field(type=UnqualifiedID.list)


class ElementValueAssoc(RFLXNode):
    """Element/value association."""

    identifier = Field(type=UnqualifiedID)
    literal = Field(type=NumericLiteral)


class NamedEnumerationDef(EnumerationDef):
    elements = Field(type=ElementValueAssoc.list)


class EnumerationTypeDef(TypeDef):
    elements = Field(type=EnumerationDef)
    aspects = Field(type=Aspect.list)


class Then(RFLXNode):
    """Link to field."""

    target = Field(type=UnqualifiedID)
    aspects = Field(type=Aspect.list)
    condition = Field(type=Expr)


class TypeArgument(RFLXNode):
    identifier = Field(type=UnqualifiedID)
    expression = Field(type=Expr)


class NullMessageField(RFLXNode):
    then = Field(type=Then)


class MessageField(RFLXNode):
    identifier = Field(type=UnqualifiedID)
    type_identifier = Field(type=ID)
    type_arguments = Field(type=TypeArgument.list)
    aspects = Field(type=Aspect.list)
    condition = Field(type=Expr)
    thens = Field(type=Then.list)


class MessageFields(RFLXNode):
    initial_field = Field(type=NullMessageField)
    fields = Field(type=MessageField.list)


@abstract
class BaseChecksumVal(RFLXNode):
    """Base class for checksum values."""


class ChecksumVal(BaseChecksumVal):
    """Single checksum value."""

    data = Field(type=Expr)


class ChecksumValueRange(BaseChecksumVal):
    """Checksum value range."""

    first = Field(type=Expr)
    last = Field(type=Expr)


class ChecksumAssoc(RFLXNode):
    """Association between checksum field and list of covered fields."""

    identifier = Field(type=UnqualifiedID)
    covered_fields = Field(type=BaseChecksumVal.list)


@abstract
class MessageAspect(RFLXNode):
    """Base class for message aspects."""


class ChecksumAspect(MessageAspect):
    associations = Field(type=ChecksumAssoc.list)


class ByteOrderType(RFLXNode):
    enum_node = True
    alternatives: ClassVar[list[str]] = [
        "highorderfirst",
        "loworderfirst",
    ]


class ByteOrderAspect(MessageAspect):
    byte_order = Field(type=ByteOrderType)


class MessageTypeDef(AbstractMessageTypeDef):
    message_fields = Field(type=MessageFields)
    aspects = Field(type=MessageAspect.list)


class Variable(Expr):
    identifier = Field(type=ID)


class Attr(RFLXNode):
    """Attribute kind."""

    enum_node = True
    alternatives: ClassVar[list[str]] = [
        "first",
        "size",
        "last",
        "valid_checksum",
        "has_data",
        "head",
        "opaque",
        "present",
        "valid",
    ]


class Attribute(Expr):
    expression = Field(type=Expr)
    kind = Field(type=Attr)


class ContextItem(Expr):
    """Import statement (with Package)."""

    item = Field(type=UnqualifiedID)


class Specification(RFLXNode):
    """RecordFlux specification."""

    context_clause = Field(type=ContextItem.list)
    package_declaration = Field(type=Package)


class FormalFunctionDecl(FormalDecl):
    identifier = Field(type=UnqualifiedID)
    parameters = Field(type=Parameters)
    return_type_identifier = Field(type=ID)


@abstract
class ChannelAttribute(RFLXNode):
    """Base class for channel attributes."""


class Readable(ChannelAttribute):
    """Channel attribute (channel can be read)."""


class Writable(ChannelAttribute):
    """Channel attribute (channel can be written)."""


class FormalChannelDecl(FormalDecl):
    identifier = Field(type=UnqualifiedID)
    parameters = Field(type=ChannelAttribute.list)


class Quantifier(RFLXNode):
    """Quantifier kind."""

    enum_node = True
    alternatives: ClassVar[list[str]] = [
        "all",
        "some",
    ]


class QuantifiedExpression(Expr):
    operation = Field(type=Quantifier)
    parameter_identifier = Field(type=UnqualifiedID)
    iterable = Field(type=Expr)
    predicate = Field(type=Expr)


class Comprehension(Expr):
    iterator = Field(type=UnqualifiedID)
    sequence = Field(type=Expr)
    condition = Field(type=Expr)
    selector = Field(type=Expr)


class Call(Expr):
    identifier = Field(type=UnqualifiedID)
    arguments = Field(type=Expr.list)


class Conversion(Expr):
    target_identifier = Field(type=ID)
    argument = Field(type=Expr)


class TermAssoc(RFLXNode):
    identifier = Field(type=UnqualifiedID)
    expression = Field(type=Expr)


class Binding(Expr):
    expression = Field(type=Expr)
    bindings = Field(type=TermAssoc.list)


class Select(Expr):
    expression = Field(type=Expr)
    selector = Field(type=UnqualifiedID)


class Choice(Expr):
    selectors = Field(type=RFLXNode.list)
    expression = Field(type=Expr)


class CaseExpression(Expr):
    expression = Field(type=Expr)
    choices = Field(type=Choice.list)
