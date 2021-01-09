from langkit.dsl import ASTNode, Field, abstract


@abstract
class RFLXNode(ASTNode):
    """
    Root node class for the RecordFlux DSL language
    """


@abstract
class AbstractID(RFLXNode):
    pass


class NullID(AbstractID):
    """
    "null" identifier
    """


class UnqualifiedID(AbstractID):
    """
    Simple, unqualified identifiers, i.e. identifiers without a package part (e.g. "Foo")
    """

    token_node = True


class ID(AbstractID):
    """
    Qualified identifiers which may optionally have a package part (e.g. "Pkg::Foo", "Foo")
    """

    package = Field(type=UnqualifiedID)
    name = Field(type=UnqualifiedID)


@abstract
class Declaration(RFLXNode):
    """
    Base class for declarations (types, refinements, sessions)
    """


@abstract
class TypeDef(RFLXNode):
    """
    Base class for type definitions (integers, abstract messages, type derivations, arrays,
    enumerations)
    """


class TypeDecl(Declaration):
    """
    Type specification (type Foo is ...)
    """

    identifier = Field(type=UnqualifiedID)
    definition = Field(type=TypeDef)


@abstract
class Expr(RFLXNode):
    """
    Base class for expressions
    """


class Op(RFLXNode):
    """
    Operators for expressions
    """

    enum_node = True
    alternatives = [
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
    """
    Negation
    """

    data = Field(type=Expr)


class BinOp(Expr):
    """
    Binary operation
    """

    left = Field(type=Expr)
    op = Field(type=Op)
    right = Field(type=Expr)


class ParenExpression(Expr):
    """
    Parenthesized expression
    """

    data = Field(type=Expr)


class RefinementSpec(Declaration):
    """
    Refinement specification (for Message use (Field => Inner_Type))
    """

    pdu = Field(type=ID)
    field = Field(type=UnqualifiedID)
    sdu = Field(type=ID)
    condition = Field(type=Expr)


@abstract
class FormalDecl(RFLXNode):
    """
    Base class for generic formal session declarations
    """


class FormalPrivateTypeDecl(FormalDecl):
    """
    Formal private session type declaration
    """

    identifier = Field(type=UnqualifiedID)


class SessionAspects(RFLXNode):
    """
    Session aspects (Initial, Final)
    """

    initial = Field(type=UnqualifiedID)
    final = Field(type=UnqualifiedID)


@abstract
class LocalDecl(RFLXNode):
    """
    Base class for session or state local declarations
    """


class VariableDecl(LocalDecl):
    """
    Session variable declaration
    """

    identifier = Field(type=UnqualifiedID)
    type_identifier = Field(type=ID)
    initializer = Field(type=Expr)


class RenamingDecl(LocalDecl):
    """
    Session renaming declaration
    """

    identifier = Field(type=UnqualifiedID)
    type_identifier = Field(type=ID)
    expression = Field(type=Expr)


@abstract
class BaseStateBody(RFLXNode):
    """
    Base class for session state body
    """


class NullStateBody(BaseStateBody):
    """
    Null session state body
    """


@abstract
class Statement(RFLXNode):
    """
    Base class for statements
    """


class Assignment(Statement):
    """
    Assignment of expression to unqualified identifier
    """

    identifier = Field(type=UnqualifiedID)
    expression = Field(type=Expr)


class ListAttr(RFLXNode):
    """
    List attribute kind
    """

    enum_node = True
    alternatives = [
        "Append",
        "Extend",
        "Read",
        "Write",
    ]


class ListAttribute(Statement):
    """
    List attribute statement
    """

    identifier = Field(type=UnqualifiedID)
    attr = Field(type=ListAttr)
    expression = Field(type=Expr)


class Reset(Statement):
    """
    List reset statement
    """

    identifier = Field(type=UnqualifiedID)


@abstract
class ArrayLiteral(Expr):
    """
    Base class for array literals (strings, array aggregates)
    """


class StringLiteral(ArrayLiteral):
    """
    Double-quoted string literal
    """

    token_node = True


class NumericLiteral(Expr):
    """
    Numeric literal
    """

    token_node = True


class ArrayAggregate(ArrayLiteral):
    """
    List of literal array values
    """

    values = Field(type=NumericLiteral.list)


class Concatenation(ArrayLiteral):
    """
    Concatenation of aggragates or string literals
    """

    left = Field(type=ArrayLiteral)
    right = Field(type=ArrayLiteral)


class Description(RFLXNode):
    """
    String description of an entity
    """

    content = Field(type=StringLiteral)


class Transition(RFLXNode):
    """
    Unconditional session state transition
    """

    target = Field(type=UnqualifiedID)
    description = Field(type=Description)


class ConditionalTransition(Transition):
    """
    Conditional session state transition
    """

    condition = Field(type=Expr)


class StateBody(BaseStateBody):
    """
    Body of a session state
    """

    declarations = Field(type=LocalDecl.list)
    actions = Field(type=Statement.list)
    conditional_transitions = Field(type=ConditionalTransition.list)
    final_transition = Field(type=Transition)
    end_identifier = Field(type=UnqualifiedID)


class State(RFLXNode):
    """
    Session state
    """

    identifier = Field(type=UnqualifiedID)
    description = Field(type=Description)
    body = Field(type=BaseStateBody)


class SessionSpec(Declaration):
    """
    Session specification
    """

    parameters = Field(type=FormalDecl.list)
    identifier = Field(type=UnqualifiedID)
    aspects = Field(type=SessionAspects)
    declarations = Field(type=LocalDecl.list)
    states = Field(type=State.list)
    end_identifier = Field(type=UnqualifiedID)


class PackageSpec(RFLXNode):
    """
    RecordFlux package
    """

    identifier = Field(type=UnqualifiedID)
    declarations = Field(type=Declaration.list)
    end_identifier = Field(type=UnqualifiedID)


@abstract
class IntegerTypeDef(TypeDef):
    """
    Base class for all integer type definitions
    """


class Aspect(RFLXNode):
    """
    Aspect
    """

    identifier = Field(type=UnqualifiedID)
    value = Field(type=Expr)


class RangeTypeDef(IntegerTypeDef):
    """
    Range type definition
    """

    lower = Field(type=Expr)
    upper = Field(type=Expr)
    size = Field(type=Aspect)


class ModularTypeDef(IntegerTypeDef):
    """
    Modular type definition
    """

    mod = Field(type=Expr)


@abstract
class AbstractMessageTypeDef(TypeDef):
    """
    Base class for message types
    """


class NullMessageTypeDef(AbstractMessageTypeDef):
    """
    Null message type
    """


class TypeDerivationDef(TypeDef):
    """
    Type derivation definition
    """

    base = Field(type=ID)


class ArrayTypeDef(TypeDef):
    """
    Array type definition
    """

    element_type = Field(type=ID)


@abstract
class EnumerationDef(TypeDef):
    """
    Base class for enumeration
    """


class PositionalEnumerationDef(EnumerationDef):
    """
    Positional enumeration
    """

    elements = Field(type=UnqualifiedID.list)


class ElementValueAssoc(RFLXNode):
    """
    Element/value association
    """

    identifier = Field(type=UnqualifiedID)
    literal = Field(type=NumericLiteral)


class NamedEnumerationDef(EnumerationDef):
    """
    Named enumeration
    """

    elements = Field(type=ElementValueAssoc.list)


class EnumerationTypeDef(TypeDef):
    """
    Enumeration type definition
    """

    elements = Field(type=EnumerationDef)
    aspects = Field(type=Aspect.list)


class Then(RFLXNode):
    """
    Link to field
    """

    target = Field(type=AbstractID)
    aspects = Field(type=Aspect.list)
    condition = Field(type=Expr)


class NullComponent(RFLXNode):
    """
    Null message component
    """

    then = Field(type=Then)


class Component(RFLXNode):
    """
    Message component
    """

    identifier = Field(type=UnqualifiedID)
    type_identifier = Field(type=ID)
    aspects = Field(type=Aspect.list)
    condition = Field(type=Expr)
    thens = Field(type=Then.list)


class Components(RFLXNode):
    """
    Message components
    """

    initial_component = Field(type=NullComponent)
    components = Field(type=Component.list)


@abstract
class BaseChecksumVal(RFLXNode):
    """
    Base class for checksum values
    """


class ChecksumVal(BaseChecksumVal):
    """
    Single checksum value
    """

    data = Field(type=Expr)


class ChecksumValueRange(BaseChecksumVal):
    """
    Checksum value range
    """

    lower = Field(type=Expr)
    upper = Field(type=Expr)


class ChecksumAssoc(RFLXNode):
    """
    Association between checksum field and list of covered fields
    """

    identifier = Field(type=UnqualifiedID)
    covered_fields = Field(type=BaseChecksumVal.list)


class ChecksumAspect(RFLXNode):
    """
    Checksum aspect
    """

    associations = Field(type=ChecksumAssoc.list)


class MessageTypeDef(AbstractMessageTypeDef):
    """
    Message type definition
    """

    components = Field(type=Components)
    checksums = Field(type=ChecksumAspect)


class UnqualifiedVariable(Expr):
    """
    Unqualified variable
    """

    identifier = Field(type=UnqualifiedID)


class Variable(Expr):
    """
    Variable
    """

    identifier = Field(type=ID)


class Attr(RFLXNode):
    """
    Attribute
    """

    enum_node = True
    alternatives = [
        "First",
        "Size",
        "Last",
        "Valid_Checksum",
        "Head",
        "Opaque",
        "Present",
        "Valid",
    ]


class Attribute(Expr):
    """
    Attribute
    """

    expression = Field(type=Expr)
    kind = Field(type=Attr)


class ContextItem(Expr):
    """
    Import statement (with Package)
    """

    item = Field(type=UnqualifiedID)


class Specification(RFLXNode):
    """
    RecordFlux specification
    """

    context_clause = Field(type=ContextItem.list)
    package_declaration = Field(type=PackageSpec)


class Parameter(RFLXNode):
    """
    Parameter
    """

    identifier = Field(type=UnqualifiedID)
    type_identifier = Field(type=ID)


class Parameters(RFLXNode):
    """
    Parameter list
    """

    parameters = Field(type=Parameter.list)


class FormalFunctionDecl(FormalDecl):
    """
    Formal function declaration
    """

    identifier = Field(type=UnqualifiedID)
    parameters = Field(type=Parameters)
    return_type_identifier = Field(type=ID)


@abstract
class ChannelAttribute(RFLXNode):
    """
    Base class for channel attributes
    """


class Readable(ChannelAttribute):
    """
    Channel attribute (channel can be read)
    """


class Writable(ChannelAttribute):
    """
    Channel attribute (channel can be written)
    """


class FormalChannelDecl(FormalDecl):
    """
    Channel declaration
    """

    identifier = Field(type=UnqualifiedID)
    parameters = Field(type=ChannelAttribute.list)


class Quantifier(RFLXNode):
    """
    Quantifier kind
    """

    enum_node = True
    alternatives = [
        "all",
        "some",
    ]


class QuantifiedExpression(Expr):
    """
    Quantified expression
    """

    operation = Field(type=Quantifier)
    parameter_identifier = Field(type=UnqualifiedID)
    iterable = Field(type=Expr)
    predicate = Field(type=Expr)


class Comprehension(Expr):
    """
    List comprehension
    """

    iterator = Field(type=UnqualifiedID)
    array = Field(type=Expr)
    selector = Field(type=Expr)
    condition = Field(type=Expr)


class Call(Expr):
    """
    Function call
    """

    identifier = Field(type=UnqualifiedID)
    arguments = Field(type=Expr.list)


class Conversion(Expr):
    """
    Type conversion
    """

    target_identifier = Field(type=ID)
    argument = Field(type=Expr)


class MessageComponent(RFLXNode):
    """
    Message component association
    """

    identifier = Field(type=UnqualifiedID)
    expression = Field(type=Expr)


@abstract
class BaseComponents(RFLXNode):
    """
    Base class for message components
    """


class NullComponents(BaseComponents):
    """
    Null message components
    """


class MessageComponents(BaseComponents):
    """
    Message components
    """

    components = Field(type=MessageComponent.list)


class MessageAggregate(Expr):
    """
    Message aggregate
    """

    identifier = Field(type=ID)
    values = Field(type=BaseComponents)


class TermAssoc(RFLXNode):
    """
    Term association
    """

    identifier = Field(type=UnqualifiedID)
    expression = Field(type=Expr)


class Binding(Expr):
    """
    Variable binding
    """

    expression = Field(type=Expr)
    bindings = Field(type=TermAssoc.list)


class Select(Expr):
    """
    Selector
    """

    expression = Field(type=Expr)
    selector = Field(type=UnqualifiedID)
