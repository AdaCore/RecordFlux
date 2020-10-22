from langkit.dsl import ASTNode, Field, abstract  # type: ignore


@abstract
class RFLXNode(ASTNode):
    pass


class ID(RFLXNode):
    token_node = True


class PackageDeclarationNode(RFLXNode):
    name_start = Field()
    content = Field()
    name_end = Field()


@abstract
class IntegerTypeDef(RFLXNode):
    pass


class RangeTypeDef(IntegerTypeDef):
    lower = Field()
    upper = Field()
    size = Field()


class ModularTypeDef(IntegerTypeDef):
    mod = Field()


class Type(RFLXNode):
    identifier = Field()
    type_definition = Field(type=IntegerTypeDef)


class BasedLiteral(RFLXNode):
    # base = Field()
    # value = Field()
    pass


class NumericLiteral(RFLXNode):
    value = Field()


class SizeAspect(RFLXNode):
    size = Field()
