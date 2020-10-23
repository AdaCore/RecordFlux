from langkit.dsl import ASTNode, Field, abstract  # type: ignore


@abstract
class RFLXNode(ASTNode):
    pass


class NullID(RFLXNode):
    pass


class UnqualifiedID(RFLXNode):
    token_node = True


class ID(RFLXNode):
    package = Field()
    name = Field()


class PackageDeclarationNode(RFLXNode):
    name_start = Field()
    content = Field()
    name_end = Field()


@abstract
class TypeDef(RFLXNode):
    pass


@abstract
class IntegerTypeDef(TypeDef):
    pass


class RangeTypeDef(IntegerTypeDef):
    lower = Field()
    upper = Field()
    size = Field()


class ModularTypeDef(IntegerTypeDef):
    mod = Field()


@abstract
class AbstractMessageTypeDef(TypeDef):
    pass


class NullMessageTypeDef(AbstractMessageTypeDef):
    pass


class TypeDerivationDef(TypeDef):
    type_name = Field()


class ArrayTypeDef(TypeDef):
    type_name = Field()


@abstract
class Enumeration(TypeDef):
    pass


class NamedEnumeration(Enumeration):
    elements = Field()


class PositionalEnumeration(Enumeration):
    elements = Field()


class EnumerationTypeDef(TypeDef):
    elements = Field()
    aspects = Field()


class ElementValueAssoc(TypeDef):
    name = Field()
    literal = Field()


class MessageTypeDef(AbstractMessageTypeDef):
    components = Field()
    checksums = Field()


class Type(RFLXNode):
    identifier = Field()
    type_definition = Field(type=TypeDef)


class Refinement(RFLXNode):
    pdu = Field()
    field = Field()
    sdu = Field()
    condition = Field()


class NumericLiteral(RFLXNode):
    token_node = True


@abstract
class Aspect(RFLXNode):
    pass


class MathematicalAspect(Aspect):
    name = Field()
    value = Field()


class BooleanAspect(Aspect):
    name = Field()
    value = Field()


class Then(RFLXNode):
    name = Field()
    aspects = Field()
    condition = Field()


class If(RFLXNode):
    condition = Field()


class NullComponent(RFLXNode):
    then = Field()


class Component(RFLXNode):
    name = Field()
    type_name = Field()
    thens = Field()


class Components(RFLXNode):
    null_component = Field()
    components = Field()


class ValueRange(RFLXNode):
    lower = Field()
    upper = Field()


class ChecksumAssoc(RFLXNode):
    name = Field()
    covered_fields = Field()


class ChecksumAspect(RFLXNode):
    associations = Field()


class Variable(RFLXNode):
    name = Field()


class Op(RFLXNode):
    enum_node = True
    alternatives = [
        "pow",
        "mul",
        "div",
        "add",
        "sub",
        "eq",
        "neq",
        "le",
        "lt",
        "gt",
        "ge",
        "and",
        "or",
    ]


class BinOp(RFLXNode):
    left = Field()
    op = Field(type=Op)
    right = Field()


class ParenExpression(RFLXNode):
    expr = Field()


class BooleanExpression(RFLXNode):
    expr = Field()


class MathematicalExpression(RFLXNode):
    expr = Field()


@abstract
class Attribute(RFLXNode):
    pass


class FirstAttribute(Attribute):
    variable = Field()


class SizeAttribute(Attribute):
    variable = Field()


class LastAttribute(Attribute):
    variable = Field()


class Specification(RFLXNode):
    context_clause = Field()
    package_declaration = Field()
