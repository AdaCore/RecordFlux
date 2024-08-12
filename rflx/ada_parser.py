from __future__ import annotations

from functools import reduce
from typing import Literal

import lark.grammar

from rflx import ada
from rflx.identifier import ID

ADA_GRAMMAR = lark.Lark(
    r"""
        # 2.3 (2/2)
        identifier:                 /[a-zA-Z][a-zA-Z0-9_]*/

        # 2.4 (2)
        numeric_literal:            decimal_literal

        # 2.4.1 (2)
        decimal_literal:            numeral

        # 2.4.1 (3)
        numeral:                    /\d(_?\d)*/

        # 2.6 (2)
        string_literal:             /"(""|[^"])*"/

        # 2.8 (2)
        pragma:                     "pragma" identifier \
                                        optional_pragma_arguments \
                                        ";"

        optional_pragma_arguments:  pragma_arguments?

        pragma_arguments:           "(" \
                                        pragma_argument_association \
                                        ( "," pragma_argument_association )* \
                                    ")"

        # 2.8 (3/3)
        pragma_argument_association: \
                                    expression

        # 3.1 (3/3)
        basic_declaration: \
                                    type_declaration \
                                  | object_declaration \
                                  | subprogram_declaration \
                                  | unified_function_declaration \
                                  | pragma

        # 3.1 (4)
        defining_identifier:        identifier

        # 3.2.1 (2)
        type_declaration:           combined_type_declaration

        # 3.2.1 (3/3)
        # Type declarations must be combined, as the parser does not backtrack.
        # For full_type_declaration see combined_type_declaration.

        # 3.2.1 (4/2)
        type_definition: \
                                    integer_type_definition
                                  | array_type_definition
                                  | access_type_definition
                                  | derived_type_definition
                                  | private_type_definition

        # 3.2.2 (3/2)
        subtype_indication:         subtype_mark optional_constraint

        optional_constraint:        constraint?

        # 3.2.2 (4)
        subtype_mark:               name

        # 3.2.2 (5)
        constraint:                  scalar_constraint

        # 3.2.2 (6)
        scalar_constraint: \
                                    range_constraint

        # 3.3.1 (2/3)
        object_declaration: \
                                    defining_identifier_list ":" constant_opt subtype_indication \
                                        object_declaration_expr \
                                    ";"

        object_declaration_expr:    (":=" expression)?
        !constant_opt:              "constant"?

        # 3.3.1 (3)
        defining_identifier_list: \
                                    defining_identifier ("," defining_identifier)*

        # 3.4 (2/2)
        derived_type_definition: \
                                    "new" subtype_indication

        # 3.5 (2)
        range_constraint:           "range" range

        # 3.5 (3)
        range: \
                                    simple_expression ".." simple_expression

        # 3.5.1 (2)
        integer_type_definition:    signed_integer_type_definition | modular_type_definition

        # 3.5.1 (3)
        signed_integer_type_definition: "range" expression ".." expression

        # 3.5.1 (4)
        modular_type_definition:    "mod" expression

        # 3.6 (2)
        array_type_definition: \
                                    unconstrained_array_definition

        # 3.6 (3)
        unconstrained_array_definition: \
                                    "array" "(" index_subtype_definition \
                                        ( "," index_subtype_definition )* ")" \
                                            "of" component_definition

        # 3.6 (4)
        index_subtype_definition:   subtype_mark "range" "<>"

        # 3.6 (7/2)
        component_definition: \
                                    subtype_indication

        # 3.7 (2/2)
        discriminant_part:          known_discriminant_part

        optional_discriminant_part: discriminant_part?

        # 3.7 (4)
        known_discriminant_part: \
                                    "(" discriminant_specification \
                                        ( ";" discriminant_specification )* ")"

        # 3.7 (5/2)
        discriminant_specification: \
                                    defining_identifier_list ":" subtype_mark \
                                        optional_default_expression

        # 3.7 (6)
        default_expression:         expression

        optional_default_expression: \
                                    (":=" default_expression)?

        # 3.8.1 (4)
        discrete_choice_list:       discrete_choice ( "|" discrete_choice )*

        # 3.8.1 (5/3)
        discrete_choice:            choice_expression | others

        # 3.10 (2/2)
        access_type_definition: \
                                    access_to_object_definition

        # 3.10 (3)
        access_to_object_definition: \
                                    "access" subtype_indication

        # 3.11 (2)
        declarative_part:           declarative_item*

        # 3.11 (3)
        declarative_item: \
                                    basic_declarative_item | body

        # 3.11 (4/1)
        basic_declarative_item: \
                                    basic_declaration | use_clause

        # 3.11 (5)
        body:                       proper_body

        # 3.11 (5)
        proper_body: \
                                    subprogram_body

        # 4.1 (2/3)
        name:                       direct_name
                                  | indexed_component
                                  | selected_component | attribute_reference
                                  | qualified_expression

        # 4.1 (3)
        direct_name:                identifier

        # 4.1 (4)
        prefix:                     name

        # 4.1.1 (2)
        indexed_component:          prefix indexed_component_expressions

        indexed_component_expressions: \
                                    "(" expression ( "," expression )* ")"

        # 4.1.3 (2)
        selected_component:         prefix "." selector_name

        # 4.1.3 (3)
        selector_name:              identifier

        # 4.1.4 (2)
        attribute_reference:        prefix "'" attribute_designator

        # 4.1.4 (3/2)
        attribute_designator: \
                                    identifier ( "(" expression ")" )?

        # 4.3 (2)
        aggregate:                  array_aggregate

        # 4.3.3 (2)
        array_aggregate: \
                                    positional_array_aggregate

        # 4.3.3 (3/2)
        positional_array_aggregate: \
                                    "(" expression "," expression ("," expression)* ")"

        # 4.4 (2)
        expression:                 relation (relation_operator relation)*

        !relation_operator:         "and" | "and then" | "or" | "or else"

        # 4.4 (2.1/3)
        choice_expression: \
                                    choice_relation (relation_operator choice_relation)?

        # 4.4 (2.2/3)
        choice_relation: \
                                    simple_expression (relational_operator simple_expression)?

        # 4.4 (3/3)
        relation: \
                                    simple_expression (relational_operator simple_expression)?
                                  | simple_expression in_operator membership_choice_list

        !in_operator:               "in"

        # 4.4 (3.1/3)
        membership_choice_list:     membership_choice

        # 4.4 (3.2/3)
        membership_choice:          range

        # 4.4 (4)
        simple_expression:          term (binary_adding_operator term)*

        # 4.4 (5)
        term:                       factor (multiplying_operator factor)*

        # 4.4 (6)
        factor:                     primary ("**" primary)? | negated_primary

        negated_primary:            "not" primary

        # 4.4 (7/3)
        primary: \
                                    true | false
                                  | numeric_literal | string_literal | aggregate
                                  | function_call
                                  | name | "(" expression ")"
                                  | conditional_expression

        true:                       "True"
        false:                      "False"

        # 4.5 (3)
        !relational_operator:       "=" | "/=" | "<" | "<=" | ">" | ">="

        # 4.5 (4)
        !binary_adding_operator:    "+" | "-" | "&"

        # 4.5 (4)
        !multiplying_operator:      "*" | "/" | "mod" | "rem"

        # 4.5.7 (2/3)
        conditional_expression:     if_expression | case_expression

        # 4.5.7 (3/3)
        if_expression: \
                                    "if" condition "then" expression \
                                    if_expr_elsifs \
                                    if_expr_else

        if_expr_elsifs:             if_expr_elsif*
        if_expr_elsif:              ("elsif" condition "then" expression)
        if_expr_else:               ("else" expression)?

        # 4.5.7 (4/3)
        condition:                  expression

        # 4.5.7 (5/3)
        case_expression: \
                                    "case" expression "is" \
                                    case_expr_alternatives

        case_expr_alternatives: case_expression_alternative ( "," case_expression_alternative )*

        # 4.5.7 (6/3)
        case_expression_alternative: \
                                    "when" discrete_choice_list "=>" \
                                    expression

        # 4.7 (2)
        qualified_expression: \
                                    subtype_mark "'(" expression ")"

        # 5.1 (2/3)
        sequence_of_statements:     statement statement*

        # 5.1 (3)
        statement: \
                                    simple_statement | compound_statement

        # 5.1 (4/2)
        simple_statement:           pragma_statement
                                  | simple_return_statement

        # 5.1 (4/2)
        compound_statement: \
                                    if_statement

        # 5.3 (2)
        if_statement: \
                                    "if" condition "then" \
                                        sequence_of_statements \
                                    optional_elsifs \
                                    optional_else \
                                    "end" "if" ";"

        optional_elsifs:            elsif*
        elsif:                      "elsif" condition "then" sequence_of_statements
        optional_else:              elsebranch?
        elsebranch:                 "else" sequence_of_statements

        # 6.1 (2/3)
        # Function specifications is special-cased in unified_function_declaration
        subprogram_declaration: \
                                    subprogram_specification \
                                    optional_aspect_specification ";"

        # 6.1 (4/2)
        subprogram_specification: \
                                    procedure_specification

        # 6.1 (4.1/2)
        procedure_specification:    "procedure" defining_program_unit_name parameter_profile

        # 6.1 (4.2/2)
        function_specification:     "function" defining_designator parameter_and_result_profile

        # 6.1 (5)
        designator:                 name

        # 6.1 (6)
        defining_designator:        defining_program_unit_name

        # 6.1 (7)
        defining_program_unit_name: (identifier ".")* defining_identifier

        # 6.1 (12)
        parameter_profile:          parameter_profile_formal_part

        parameter_profile_formal_part: formal_part?

        # 6.1 (13/2)
        parameter_and_result_profile: \
                                    parameter_and_result_profile_formal_part "return" subtype_mark

        parameter_and_result_profile_formal_part: formal_part?

        # 6.1 (14)
        formal_part: \
                                    "(" parameter_specification (";" parameter_specification)* ")"

        # 6.1 (15/3)
        parameter_specification: \
                                    defining_identifier_list ":" mode subtype_mark \
                                        optional_default_expression

        # 6.1 (16)
        !mode:                       "in"? | "in" "out" | "out"

        # 6.3 (2/3)
        # function subprogram_body is special-cased in unified_function_declaration.
        subprogram_body: \
                                    procedure_specification \
                                        optional_aspect_specification "is" \
                                        declarative_part \
                                    "begin" \
                                        handled_sequence_of_statements \
                                    "end" designator ";"

        # 6.4 (3)
        function_call: \
                                    prefix actual_parameter_part

        # 6.4 (4)
        actual_parameter_part: \
                                    "(" parameter_association ("," parameter_association)* ")"

        # 6.4 (5)
        parameter_association: \
                                    (selector_name "=>")? explicit_actual_parameter

        # 6.4 (6)
        explicit_actual_parameter:  expression

        # 6.5 (2/2)
        simple_return_statement:    "return" expression? ";"

        # 6.8 (2/3)
        # expression_function_declaration is special-cased in unified_function_declaration.

        # 7.1 (2)
        package_declaration:        package_specification ";"

        # 7.1 (3/3)
        package_specification:      "package" defining_program_unit_name \
                                    optional_aspect_specification "is" \
                                    ( \
                                        package_specification_part \
                                      | generic_package_instantiation_part \
                                    )

        package_specification_part: package_spec_declarations \
                                        optional_package_spec_private_declarations \
                                    "end" defining_program_unit_name

        package_spec_declarations:  basic_declarative_item*

        optional_package_spec_private_declarations: \
                                    ( "private" basic_declarative_item* )?

        # 7.2 (2/3)
        package_body: \
                                    "package" "body" defining_program_unit_name \
                                    optional_aspect_specification "is" \
                                    declarative_part \
                                    "end" defining_program_unit_name ";"

        # 8.4 (2)
        use_clause:                 use_package_clause | use_type_clause

        # 8.4 (3)
        use_package_clause:         "use" name ("," name)* ";"

        # 8.4 (4/3)
        use_type_clause:            "use" "all"? "type" subtype_mark ("," subtype_mark)* ";"

        # 10.1.1 (3)
        compilation_unit: \
                                    context_clause library_item

        # 10.1.1 (4)
        library_item:               "private"? library_unit_declaration
                                  | library_unit_body

        # 10.1.1 (5)
        library_unit_declaration:   \
                                    package_declaration
                                  | generic_declaration

        # 10.1.1 (7)
        library_unit_body:          package_body

        # 10.1.1 (8)

        # 10.1.2 (2)
        context_clause:             context_item*

        # 10.1.2 (3)
        context_item:               with_clause | use_clause | pragma

        # 10.1.2 (4/2)
        with_clause:                nonlimited_with_clause

        # 10.1.2 (4.2/2)
        nonlimited_with_clause:     "with" name ("," name)* ";"


        # 11.2 (2)
        handled_sequence_of_statements: \
                                    sequence_of_statements

        # 12.1 (2)
        generic_declaration:        generic_package_declaration

        # 12.1 (4)
        generic_package_declaration: \
                                    generic_formal_part package_specification ";"

        # 12.1 (5)
        generic_formal_part:        "generic" generic_formal_parameter_declaration*

        # 12.1 (6)
        generic_formal_parameter_declaration: \
                                    formal_type_declaration \
                                  | formal_subprogram_declaration

        # 12.3 (2/3)
        generic_package_instantiation_part: \
                                        "new" name optional_generic_actual_part

        optional_generic_actual_part:   generic_actual_part?

        # 12.3 (3)
        generic_actual_part: \
                                    "(" generic_association ("," generic_association)* ")"

        # 12.3 (4)
        generic_association: \
                                    explicit_generic_actual_parameter

        # 12.3 (5)
        explicit_generic_actual_parameter: name

        # 12.5 (2/3)
        formal_type_declaration: \
                                    formal_complete_type_declaration

        # 12.5 (2.1/3)
        formal_complete_type_declaration: \
                                    "type" defining_identifier optional_discriminant_part "is" \
                                        formal_type_definition \
                                        optional_aspect_specification ";"

        # 12.5 (3/2)
        formal_type_definition: \
                                    formal_private_type_definition

        # 12.5.1 (2)
        !formal_private_type_definition: "private"

        # 12.6 (2/2)
        formal_subprogram_declaration: formal_concrete_subprogram_declaration

        # 12.6 (2.1/3)
        formal_concrete_subprogram_declaration: \
                                    "with" formal_concrete_subprogram_specification ";"

        # Due to reduce/reduce conflict we cannot use subprogram_specification as stated in the
        # LRM (cf. unified_function_declaration).
        formal_concrete_subprogram_specification: \
                                    procedure_specification | function_specification

        # 13.1.1 (2/3)
        aspect_specification:       "with"  aspect_part ( "," aspect_part )*

        optional_aspect_specification: aspect_specification?

        # 13.1.1 (3/3)
        aspect_mark:                identifier

        # SPARK rules

        # SPARK RM 6.1.3
        contract_case_list:         "(" contract_case ( "," contract_case )* ")"

        contract_case:              condition "=>" expression
                                  | others "=>" expression

        # SPARK RM 6.1.4
        global_specification:       moded_global
                                  | global_list
                                  | null_global_specification

        moded_global:               "(" moded_global_list ( "," moded_global_list )* ")"

        moded_global_list:          mode_selector "=>" global_list

        global_list:                global_item
                                  | "(" global_item ( "," global_item )* ")"

        !mode_selector:             "Input" | "Output" | "In_Out"

        global_item:                name

        null_global_specification:  null

        # SPARK RM 6.1.5
        dependency_relation:        "(" dependency_clause ( "," dependency_clause )* ")"

        dependency_clause:          output_list "=>" input_list

        output_list:                name

        input_list:                 input
                                  | "(" input ( "," input )* ")"
                                  | null

        input:                      name
        !null:                      "null"

        # Custom rules
        pragma_statement:           "pragma" identifier \
                                        optional_pragma_arguments \
                                        ";"

        file:                       compilation_unit* /\0/

        unified_function_declaration: \
                                    function_specification \
                                        ( function_declaration_part \
                                        | function_body_part  \
                                        | expression_function_part ) \
                                    ";"

        function_declaration_part: optional_aspect_specification

        function_body_part:         optional_aspect_specification \
                                    "is" \
                                        declarative_part \
                                    "begin" \
                                        handled_sequence_of_statements \
                                    "end" designator

        expression_function_part:   /is\s*\(/ expression ")" optional_aspect_specification

        combined_type_declaration: \
                                    "type" defining_identifier optional_discriminant_part "is" \
                                        type_definition optional_aspect_specification ";"

        !private_type_definition:   "private"

        aspect_part:                aspect_always_terminates
                                  | aspect_annotate
                                  | aspect_contract_cases
                                  | aspect_convention
                                  | aspect_default_initial_condition
                                  | aspect_depends
                                  | aspect_ghost
                                  | aspect_global
                                  | aspect_import
                                  | aspect_post
                                  | aspect_pre
                                  | aspect_spark_mode

        aspect_always_terminates:   "Always_Terminates" ( "=>" expression )?

        aspect_annotate:            "Annotate" "=>" expression

        aspect_contract_cases:      "Contract_Cases" "=>" contract_case_list

        aspect_convention:          "Convention" "=>" expression

        aspect_default_initial_condition: \
                                    "Default_Initial_Condition" "=>" expression

        aspect_depends:             "Depends" "=>" dependency_relation

        !aspect_ghost:              "Ghost"

        aspect_global:              "Global" "=>" global_specification

        aspect_import:              "Import"

        aspect_post:                "Post"  "=>" expression

        aspect_pre:                 "Pre"  "=>" expression

        aspect_spark_mode:          "SPARK_Mode" ( "=>" expression )?

        !others:                    "others"

        # Skip whitespace
        %import common.WS
        %ignore WS

        # Skip comments
        %ignore /--.*/
    """,
    start="file",
    parser="lalr",
)


class FunctionPart:
    def declaration(self, specification: ada.FunctionSpecification) -> ada.Declaration:
        raise NotImplementedError


class FunctionDeclPart(FunctionPart):
    def __init__(self, aspects: list[ada.Aspect] | None):
        self.aspects = aspects

    def declaration(self, specification: ada.FunctionSpecification) -> ada.Declaration:
        return ada.SubprogramDeclaration(specification=specification, aspects=self.aspects)


class FunctionBodyPart(FunctionPart):
    def __init__(
        self,
        aspects: list[ada.Aspect] | None,
        declarations: list[ada.Declaration],
        statements: list[ada.Statement],
    ):
        self.aspects = aspects
        self.declarations = declarations
        self.statements = statements

    def declaration(self, specification: ada.FunctionSpecification) -> ada.Declaration:
        return ada.SubprogramBody(
            specification=specification,
            declarations=self.declarations,
            statements=self.statements,
            aspects=self.aspects,
        )


class PackagePart:
    pass


class PackageSpecificationPart(PackagePart):
    def __init__(
        self,
        declarations: list[ada.Declaration],
        private_declarations: list[ada.Declaration] | None,
        identifier: ID,
    ):
        self.declarations = declarations
        self.private_declarations = private_declarations
        self.identifier = identifier


class PackageInstantiationPart(PackagePart):
    def __init__(
        self,
        name: ID,
        associations: list[ID] | None,
    ):
        self.name = name
        self.associations = associations


class ExpressionFunctionPart(FunctionPart):
    def __init__(self, expression: ada.Expr, aspects: list[ada.Aspect] | None):
        self.expression = expression
        self.aspects = aspects

    def declaration(self, specification: ada.FunctionSpecification) -> ada.Declaration:
        return ada.ExpressionFunctionDeclaration(
            specification=specification,
            expression=self.expression,
            aspects=self.aspects,
        )


class Constraint:
    pass


class ScalarConstraint(Constraint):
    pass


class RangeConstraint(ScalarConstraint):
    def __init__(self, first: ada.Expr, last: ada.Expr):
        self.first = first
        self.last = last


class TreeToAda(lark.Transformer[lark.lexer.Token, ada.PackageUnit]):

    def identifier(self, data: list[lark.lexer.Token]) -> ID:
        return ID(data[0])

    def numeric_literal(self, data: list[ada.Number]) -> ada.Number:
        return data[0]

    def decimal_literal(self, data: list[ada.Number]) -> ada.Number:
        return data[0]

    def numeral(self, data: list[lark.Token]) -> ada.Number:
        assert isinstance(data[0].value, str)
        return ada.Number(int(data[0].value))

    def string_literal(self, data: list[lark.lexer.Token]) -> ada.String:
        return ada.String(data[0][1:-1])

    def pragma(self, data: tuple[ID, list[ada.Expr]]) -> ada.Pragma:
        return ada.Pragma(identifier=data[0], parameters=data[1])

    def optional_pragma_arguments(self, data: list[list[ada.Expr]]) -> list[ada.Expr]:
        if len(data) == 0:
            return []
        return data[0]

    def pragma_arguments(self, data: list[ada.Expr]) -> list[ada.Expr]:
        return data

    def pragma_argument_association(self, data: list[ada.Expr]) -> ada.Expr:
        return data[0]

    def basic_declaration(self, data: list[ada.Declaration]) -> ada.Declaration:
        return data[0]

    def defining_identifier(self, data: list[ID]) -> ID:
        return data[0]

    def type_declaration(self, data: list[ada.Declaration]) -> ada.Declaration:
        return data[0]

    def type_definition(self, data: list[ada.Declaration]) -> ada.Declaration:
        return data[0]

    def subtype_indication(
        self,
        data: tuple[ID, Constraint | None],
    ) -> tuple[ID, Constraint | None]:
        return data

    def optional_constraint(self, data: list[Constraint]) -> Constraint | None:
        if len(data) == 0:
            return None
        return data[0]

    def subtype_mark(self, data: list[ID]) -> ID:
        return data[0]

    def constraint(self, data: tuple[ScalarConstraint]) -> Constraint:
        return data[0]

    def scalar_constraint(self, data: tuple[RangeConstraint]) -> ScalarConstraint:
        return data[0]

    def object_declaration(
        self,
        data: tuple[list[ID], bool, tuple[ID, Constraint | None], ada.Expr | None],
    ) -> ada.ObjectDeclaration:
        assert data[2][1] is None
        return ada.ObjectDeclaration(
            identifiers=data[0],
            type_identifier=data[2][0],
            expression=data[3],
            constant=data[1],
        )

    def object_declaration_expr(self, data: list[ada.Expr]) -> ada.Expr | None:
        if not data:
            return None
        return data[0]

    def constant_opt(self, data: list[lark.Token]) -> bool:
        return len(data) > 0 and data[0] == "constant"

    def defining_identifier_list(self, data: list[ID]) -> list[ID]:
        return data

    def derived_type_definition(
        self,
        data: tuple[tuple[ID, Constraint | None]],
    ) -> ada.DerivedType:
        identifier, constraint = data[0]
        if constraint is None:
            return ada.PlainDerivedType(
                identifier="__INVALID__",
                type_identifier=identifier,
            )
        if isinstance(constraint, RangeConstraint):
            return ada.DerivedRangeType(
                identifier="__INVALID__",
                type_identifier=identifier,
                first=constraint.first,
                last=constraint.last,
            )
        assert False, data

    def range_constraint(self, data: tuple[ada.ValueRange]) -> RangeConstraint:
        return RangeConstraint(first=data[0].lower, last=data[0].upper)

    def range(self, data: tuple[ada.Number, ada.Number]) -> ada.ValueRange:
        return ada.ValueRange(lower=data[0], upper=data[1])

    def integer_type_definition(self, data: list[ada.Declaration]) -> ada.Declaration:
        return data[0]

    def signed_integer_type_definition(
        self,
        data: tuple[ada.Number, ada.Number],
    ) -> ada.Declaration:
        return ada.RangeType(identifier="__INVALID__", first=data[0], last=data[1])

    def modular_type_definition(self, data: list[ada.Expr]) -> ada.Declaration:
        return ada.ModularType(identifier="__INVALID__", modulus=data[0])

    def array_type_definition(self, data: tuple[ada.UnconstrainedArrayType]) -> ada.ArrayType:
        return data[0]

    def unconstrained_array_definition(self, data: tuple[ID, ID]) -> ada.UnconstrainedArrayType:
        return ada.UnconstrainedArrayType(
            identifier="__INVALID__",
            index_type=data[0],
            component_identifier=data[1],
        )

    def index_subtype_definition(self, data: list[ID]) -> ID:
        return data[0]

    def component_definition(self, data: tuple[tuple[ID, Constraint | None]]) -> ID:
        identifier, constraint = data[0]
        assert constraint is None
        return identifier

    def discriminant_part(self, data: tuple[list[ada.Discriminant]]) -> list[ada.Discriminant]:
        return data[0]

    def optional_discriminant_part(
        self,
        data: list[list[ada.Discriminant]],
    ) -> list[ada.Discriminant] | None:
        if len(data) == 0:
            return None
        return data[0]

    def known_discriminant_part(self, data: list[ada.Discriminant]) -> list[ada.Discriminant]:
        return data

    def discriminant_specification(
        self,
        data: tuple[list[ID], ID, ada.Expr | None],
    ) -> ada.Discriminant:
        identifiers, type_identifier, default = data
        return ada.Discriminant(
            identifiers=identifiers,
            type_identifier=type_identifier,
            default=default,
        )

    def default_expression(self, data: list[ada.Expr]) -> ada.Expr:
        return data[0]

    def optional_default_expression(self, data: list[ada.Expr]) -> ada.Expr | None:
        if len(data) == 0:
            return None
        return data[0]

    def discrete_choice_list(self, data: list[ada.Expr]) -> list[ada.Expr]:
        return data

    def discrete_choice(self, data: list[ada.Expr]) -> ada.Expr:
        return data[0]

    def access_type_definition(self, data: tuple[ID]) -> ada.AccessType:
        return ada.AccessType(identifier="__INVALID__", object_identifier=data[0])

    def access_to_object_definition(self, data: tuple[tuple[ID, Constraint | None]]) -> ID:
        identifier, constraint = data[0]
        assert constraint is None
        return identifier

    def declarative_part(self, data: list[ada.Declaration]) -> list[ada.Declaration]:
        return data

    def declarative_item(
        self,
        data: list[ada.Declaration | ada.SubprogramBody],
    ) -> ada.Declaration | ada.SubprogramBody:
        return data[0]

    def basic_declarative_item(self, data: list[ada.Declaration]) -> ada.Declaration:
        return data[0]

    def body(self, data: list[ada.SubprogramBody]) -> ada.SubprogramBody:
        return data[0]

    def proper_body(self, data: list[ada.SubprogramBody]) -> ada.SubprogramBody:
        return data[0]

    def name(self, data: list[ID | ada.Attribute]) -> ID | ada.Attribute:
        return data[0]

    def direct_name(self, data: list[ID]) -> ID:
        return data[0]

    def prefix(self, data: list[ID]) -> ID:
        return data[0]

    def indexed_component(self, data: tuple[ID, list[ada.Expr]]) -> ada.Indexed:
        return ada.Indexed(ada.Variable(data[0]), *data[1])

    def indexed_component_expressions(self, data: list[ada.Expr]) -> list[ada.Expr]:
        return data

    def selected_component(self, data: tuple[ID, ID]) -> ID:
        return data[0] * data[1]

    def selector_name(self, data: list[ID]) -> ID:
        return data[0]

    def attribute_reference(self, data: tuple[ID, str]) -> ada.Attribute:
        attributes: dict[str, type[ada.Attribute]] = {
            "Size": ada.Size,
            "Length": ada.Length,
            "First": ada.First,
            "Last": ada.Last,
            "LoopEntry": ada.LoopEntry,
            "Range": ada.Range,
            "Old": ada.Old,
            "Result": ada.Result,
            "Constrained": ada.Constrained,
            "Valid": ada.Valid,
            "Access": ada.Access,
            "Initialized": ada.Initialized,
            "Image": ada.Image,
            "Class": ada.Class,
            "UnrestrictedAccess": ada.UnrestrictedAccess,
        }
        return attributes[data[1]](data[0])

    def attribute_designator(self, data: list[ID]) -> str:
        return data[0].ada_str

    def aggregate(self, data: list[list[ada.Expr]]) -> ada.Aggregate:
        return ada.Aggregate(*data[0])

    def array_aggregate(self, data: list[list[ada.Expr]]) -> list[ada.Expr]:
        return data[0]

    def positional_array_aggregate(self, data: list[ada.Expr]) -> list[ada.Expr]:
        return data

    def expression(self, data: list[ada.Expr | str]) -> ada.Expr:

        operators: dict[str, type[ada.BoolAssExpr]] = {
            "and": ada.And,
            "and then": ada.AndThen,
            "or": ada.Or,
            "or else": ada.OrElse,
        }

        assert isinstance(data[-1], ada.Expr)

        if len(data) == 1:
            return data[-1]

        assert len(data) > 2
        assert isinstance(data[-2], str)

        left = self.expression(data[:-2])
        operation = operators[data[-2]]

        terms = left.terms if isinstance(left, operation) else [left]
        return operation(*terms, data[-1])

    def relation_operator(self, data: list[lark.Token]) -> str:
        assert isinstance(data[0].value, str)
        return data[0].value

    def choice_expression(self, data: list[ada.Expr | str]) -> ada.Expr:
        if len(data) == 1:
            assert isinstance(data[0], ada.Expr)
            return data[0]
        raise NotImplementedError

    def choice_relation(self, data: list[ada.Expr | str]) -> ada.Expr:
        if len(data) == 1:
            assert isinstance(data[0], ada.Expr)
            return data[0]
        raise NotImplementedError

    def relation(self, data: list[ada.Expr | str]) -> ada.Expr:

        operators: dict[str, type[ada.Relation]] = {
            "=": ada.Equal,
            "/=": ada.NotEqual,
            "<": ada.Less,
            "<=": ada.LessEqual,
            ">": ada.Greater,
            ">=": ada.GreaterEqual,
            "in": ada.In,
        }

        if len(data) == 1:
            assert isinstance(data[0], ada.Expr)
            return data[0]

        left, operator, right = data
        assert isinstance(left, ada.Expr)
        assert isinstance(operator, str)
        assert isinstance(right, ada.Expr)
        return operators[operator](left, right)

    def in_operator(self, data: list[lark.Token]) -> str:
        assert isinstance(data[0].value, str)
        return data[0].value

    def membership_choice_list(self, data: list[ada.Expr]) -> ada.Expr:
        return data[0]

    def membership_choice(self, data: list[ada.ValueRange]) -> ada.Expr:
        return data[0]

    def simple_expression(self, data: list[ada.Expr | str]) -> ada.Expr:

        assert isinstance(data[-1], ada.Expr), data

        if len(data) == 1:
            return data[-1]

        assert len(data) > 2, data
        assert isinstance(data[-2], str)

        left = self.simple_expression(data[:-2])

        if data[-2] == "-":
            return ada.Sub(left, data[-1])

        if data[-2] == "+":
            terms = left.terms if isinstance(left, ada.Add) else [left]
            return ada.Add(*terms, data[-1])

        if data[-2] == "&":
            terms = left.terms if isinstance(left, ada.Concatenation) else [left]
            return ada.Concatenation(*terms, data[-1])

        raise NotImplementedError(f"simple expression with operator {data[:-2]}")

    def term(self, data: list[ada.Expr | str]) -> ada.Expr:

        assert isinstance(data[-1], ada.Expr), data

        if len(data) == 1:
            return data[-1]

        assert len(data) > 2, data
        assert isinstance(data[-2], str)

        left = self.term(data[:-2])

        if data[-2] == "/":
            return ada.Div(left, data[-1])

        if data[-2] == "mod":
            return ada.Mod(left, data[-1])

        if data[-2] == "rem":
            return ada.Rem(left, data[-1])

        if data[-2] == "*":
            terms = left.terms if isinstance(left, ada.Mul) else [left]
            return ada.Mul(*terms, data[-1])

        raise NotImplementedError(f"term with operator {data[:-2]}")

    def factor(self, data: list[ada.Expr]) -> ada.Expr:
        if len(data) == 1:
            return data[0]
        return ada.Pow(data[0], data[1])

    def negated_primary(self, data: list[ada.Expr]) -> ada.Expr:
        return ada.Not(data[0])

    def primary(self, data: list[ada.Expr | ID]) -> ada.Expr:
        # TODO(senier): How exactly do we distinguish ID and Variable?
        if isinstance(data[0], ID):
            return ada.Variable(data[0])
        return data[0]

    def true(self, _: list[lark.Token]) -> ada.Expr:
        return ada.TRUE

    def false(self, _: list[lark.Token]) -> ada.Expr:
        return ada.FALSE

    def relational_operator(self, data: list[lark.Token]) -> str:
        assert isinstance(data[0].value, str)
        return data[0].value

    def binary_adding_operator(self, data: list[lark.Token]) -> str:
        assert isinstance(data[0].value, str)
        return data[0].value

    def multiplying_operator(self, data: list[lark.Token]) -> str:
        assert isinstance(data[0].value, str)
        return data[0].value

    def conditional_expression(self, data: list[ada.Expr]) -> ada.Expr:
        return data[0]

    def if_expression(
        self,
        data: tuple[
            ada.Expr,
            ada.Expr,
            list[tuple[ada.Expr, ada.Expr]] | None,
            ada.Expr | None,
        ],
    ) -> ada.IfExpr:
        condition, expression, elif_expressions, else_expression = data
        condition_expressions = [(condition, expression)]
        if elif_expressions:
            condition_expressions.extend(elif_expressions)
        return ada.IfExpr(
            condition_expressions=condition_expressions,
            else_expression=else_expression,
        )

    def if_expr_elsifs(
        self,
        data: list[tuple[ada.Expr, ada.Expr]],
    ) -> list[tuple[ada.Expr, ada.Expr]]:
        return data

    def if_expr_elsif(
        self,
        data: tuple[ada.Expr, ada.Expr],
    ) -> tuple[ada.Expr, ada.Expr]:
        return data

    def if_expr_else(self, data: list[ada.Expr]) -> ada.Expr | None:
        if not data:
            return None
        return data[0]

    def condition(self, data: list[ada.Expr]) -> ada.Expr:
        return data[0]

    def case_expression(
        self,
        data: tuple[ada.Expr, list[tuple[ada.Expr, ada.Expr]]],
    ) -> ada.CaseExpr:
        return ada.CaseExpr(control_expression=data[0], case_expressions=data[1])

    def case_expr_alternatives(
        self,
        data: list[tuple[list[ada.Expr], ada.Expr]],
    ) -> list[tuple[ada.Expr, ada.Expr]]:
        return [(c, e[1]) for e in data for c in e[0]]

    def case_expression_alternative(
        self,
        data: tuple[list[ada.Expr], ada.Expr],
    ) -> tuple[list[ada.Expr], ada.Expr]:
        return data

    def qualified_expression(self, data: tuple[ID, ada.Expr]) -> ada.Expr:
        return ada.QualifiedExpr(type_identifier=data[0], expression=data[1])

    def sequence_of_statements(self, data: list[ada.Statement]) -> list[ada.Statement]:
        return data

    def statement(self, data: list[ada.Statement]) -> ada.Statement:
        return data[0]

    def simple_statement(self, data: list[ada.Statement]) -> ada.Statement:
        return data[0]

    def compound_statement(self, data: list[ada.Statement]) -> ada.Statement:
        return data[0]

    def if_statement(
        self,
        data: tuple[
            ada.Expr,
            list[ada.Statement],
            list[tuple[ada.Expr, list[ada.Statement]]] | None,
            list[ada.Statement] | None,
        ],
    ) -> ada.IfStatement:
        condition_statements = [(data[0], data[1])]
        if data[2]:
            condition_statements.extend(data[2])
        return ada.IfStatement(condition_statements=condition_statements, else_statements=data[3])

    def optional_elsifs(
        self,
        data: list[tuple[ada.Expr, list[ada.Statement]]],
    ) -> list[tuple[ada.Expr, list[ada.Statement]]] | None:
        if not data:
            return None
        return data

    def elsif(
        self,
        data: tuple[ada.Expr, list[ada.Statement]],
    ) -> tuple[ada.Expr, list[ada.Statement]]:
        return data

    def optional_else(
        self,
        data: list[list[ada.Statement]],
    ) -> list[ada.Statement] | None:
        if not data:
            return None
        return data[0]

    def elsebranch(
        self,
        data: tuple[list[ada.Statement]],
    ) -> list[ada.Statement]:
        return data[0]

    def subprogram_declaration(
        self,
        data: tuple[ada.SubprogramSpecification, list[ada.Aspect]],
    ) -> ada.SubprogramDeclaration:
        specification, aspects = data
        return ada.SubprogramDeclaration(specification=specification, aspects=aspects)

    def subprogram_specification(
        self,
        data: list[ada.SubprogramSpecification],
    ) -> ada.SubprogramSpecification:
        return data[0]

    def procedure_specification(
        self,
        data: tuple[ID, list[ada.Parameter] | None],
    ) -> ada.ProcedureSpecification:
        identifier, parameters = data
        return ada.ProcedureSpecification(
            identifier=identifier,
            parameters=parameters,
        )

    def function_specification(
        self,
        data: tuple[ID, tuple[ID, list[ada.Parameter] | None]],
    ) -> ada.FunctionSpecification:
        identifier, (return_type, parameters) = data
        return ada.FunctionSpecification(
            identifier=identifier,
            return_type=return_type,
            parameters=parameters,
        )

    def designator(self, data: list[ID]) -> ID:
        return reduce(lambda l, r: l * r, data)

    def defining_designator(self, data: list[ID]) -> ID:
        return data[0]

    def defining_program_unit_name(self, data: list[ID]) -> ID:
        return reduce(lambda l, r: l * r, data)

    def parameter_profile(
        self,
        data: list[list[ada.Parameter] | None],
    ) -> list[ada.Parameter] | None:
        return data[0]

    def parameter_profile_formal_part(
        self,
        data: list[list[ada.Parameter]],
    ) -> list[ada.Parameter] | None:
        if not data:
            return None
        return data[0]

    def parameter_and_result_profile(
        self,
        data: tuple[list[ada.Parameter] | None, ID],
    ) -> tuple[ID, list[ada.Parameter] | None]:
        return data[1], data[0]

    def parameter_and_result_profile_formal_part(
        self,
        data: list[list[ada.Parameter]],
    ) -> list[ada.Parameter] | None:
        if not data:
            return None
        return data[0]

    def formal_part(self, data: list[ada.Parameter]) -> list[ada.Parameter]:
        return data

    def parameter_specification(
        self,
        data: tuple[list[ID], str, ID, ada.Expr | None],
    ) -> ada.Parameter:
        identifiers, mode, type_identifier, default = data

        if mode == "in out":
            return ada.InOutParameter(
                identifiers=identifiers,
                type_identifier=type_identifier,
                default=default,
            )

        if mode == "out":
            return ada.OutParameter(
                identifiers=identifiers,
                type_identifier=type_identifier,
                default=default,
            )

        if mode == "in":
            return ada.Parameter(
                identifiers=identifiers,
                type_identifier=type_identifier,
                default=default,
            )

        assert False, f"Unexpected mode {mode}"

    def mode(self, data: list[lark.Token]) -> str:
        if not data:
            return "in"
        if "in" in data:
            if "out" in data:
                return "in out"
            return "in"
        assert data == ["out"]
        return "out"

    def subprogram_body(
        self,
        data: tuple[
            ada.ProcedureSpecification,
            list[ada.Aspect] | None,
            list[ada.Declaration],
            list[ada.Statement],
        ],
    ) -> ada.SubprogramBody:
        return ada.SubprogramBody(
            specification=data[0],
            declarations=data[2],
            statements=data[3],
            aspects=data[1],
        )

    def function_call(
        self,
        data: tuple[ID, tuple[list[ada.Expr] | None, dict[ID, ada.Expr] | None]],
    ) -> ada.Call:
        identifier, (arguments, named_arguments) = data
        return ada.Call(identifier=identifier, arguments=arguments, named_arguments=named_arguments)

    def actual_parameter_part(
        self,
        data: list[tuple[ID | None, ada.Expr]],
    ) -> tuple[list[ada.Expr] | None, list[tuple[ID, ada.Expr]] | None]:
        arguments = [e for i, e in data if not i]
        named_arguments = [(i, e) for i, e in data if i]
        return arguments or None, named_arguments or None

    def parameter_association(
        self,
        data: list[tuple[ID | None, ada.Expr]],
    ) -> tuple[ID | None, ada.Expr]:
        return data[0]

    def explicit_actual_parameter(self, data: list[ada.Expr]) -> tuple[ID | None, ada.Expr]:
        return None, data[0]

    def simple_return_statement(self, data: list[ada.Expr]) -> ada.ReturnStatement:
        return ada.ReturnStatement(data[0] if data else None)

    def package_declaration(
        self,
        data: list[ada.PackageDeclaration],
    ) -> tuple[None, ada.PackageDeclaration]:
        return None, data[0]

    def package_specification(
        self,
        data: tuple[ID, list[ada.Aspect] | None, PackagePart],
    ) -> ada.PackageDeclaration:
        identifier, aspects, part = data
        if isinstance(part, PackageInstantiationPart):
            assert aspects is None
            return ada.GenericPackageInstantiation(
                identifier=identifier,
                generic_package=part.name,
                associations=part.associations,
            )

        if isinstance(part, PackageSpecificationPart):
            assert identifier == part.identifier
            return ada.PackageDeclaration(
                identifier=identifier,
                declarations=part.declarations,
                private_declarations=part.private_declarations,
                aspects=aspects,
            )

        raise NotImplementedError(type(part))

    def package_specification_part(
        self,
        data: tuple[list[ada.Declaration], list[ada.Declaration] | None, ID],
    ) -> PackageSpecificationPart:
        return PackageSpecificationPart(
            declarations=data[0],
            private_declarations=data[1],
            identifier=data[2],
        )

    def package_spec_declarations(self, data: list[ada.Declaration]) -> list[ada.Declaration]:
        return data

    def optional_package_spec_private_declarations(
        self,
        data: list[ada.Declaration],
    ) -> list[ada.Declaration] | None:
        if not data:
            return None
        return data

    def use_clause(
        self,
        data: list[ada.UsePackageClause | ada.UseTypeClause],
    ) -> ada.UsePackageClause | ada.UseTypeClause:
        return data[0]

    def use_package_clause(self, data: list[ID]) -> ada.UsePackageClause:
        assert len(data) == 1
        return ada.UsePackageClause(identifier=data[0])

    def use_type_clause(self, data: list[ID]) -> ada.UseTypeClause:
        assert len(data) == 1
        return ada.UseTypeClause(identifier=data[0])

    def compilation_unit(
        self,
        data: tuple[
            list[ada.ContextItem],
            tuple[
                ada.PackageDeclaration | ada.PackageBody,
                list[ada.SubprogramDeclaration | ada.TypeDeclaration] | None,
            ],
        ],
    ) -> tuple[
        list[ada.ContextItem],
        ada.PackageDeclaration | ada.PackageBody,
        list[ada.SubprogramDeclaration | ada.TypeDeclaration] | None,
    ]:
        return data[0], data[1][0], data[1][1]

    def library_item(
        self,
        data: list[
            tuple[
                list[ada.SubprogramDeclaration | ada.TypeDeclaration] | None,
                ada.PackageDeclaration | ada.PackageBody,
            ]
        ],
    ) -> tuple[
        ada.PackageDeclaration | ada.PackageBody,
        list[ada.SubprogramDeclaration | ada.TypeDeclaration] | None,
    ]:
        return data[0][1], data[0][0]

    def library_unit_declaration(
        self,
        data: tuple[
            tuple[
                list[ada.SubprogramDeclaration | ada.TypeDeclaration] | None,
                ada.PackageDeclaration,
            ]
        ],
    ) -> tuple[
        list[ada.SubprogramDeclaration | ada.TypeDeclaration] | None,
        ada.PackageDeclaration,
    ]:
        return data[0]

    def library_unit_body(
        self,
        data: list[ada.PackageBody],
    ) -> tuple[None, ada.PackageBody]:
        return None, data[0]

    def context_clause(self, data: list[ada.ContextItem]) -> list[ada.ContextItem]:
        return data

    def context_item(self, data: list[ada.ContextItem]) -> ada.ContextItem:
        return data[0]

    def with_clause(self, data: list[ada.WithClause]) -> ada.WithClause:
        return data[0]

    def nonlimited_with_clause(self, data: list[ID]) -> ada.WithClause:
        return ada.WithClause(data[0])

    def handled_sequence_of_statements(
        self,
        data: list[list[ada.Statement]],
    ) -> list[ada.Statement]:
        return data[0]

    def generic_declaration(
        self,
        data: tuple[
            tuple[list[ada.SubprogramDeclaration | ada.TypeDeclaration], ada.PackageDeclaration]
        ],
    ) -> tuple[
        list[ada.SubprogramDeclaration | ada.TypeDeclaration] | None,
        ada.PackageDeclaration,
    ]:
        return data[0]

    def generic_package_declaration(
        self,
        data: tuple[
            list[ada.SubprogramDeclaration | ada.TypeDeclaration] | None,
            ada.PackageDeclaration,
        ],
    ) -> tuple[
        list[ada.SubprogramDeclaration | ada.TypeDeclaration] | None,
        ada.PackageDeclaration,
    ]:
        return data

    def generic_formal_part(
        self,
        data: list[tuple[ada.SubprogramDeclaration | ada.TypeDeclaration]],
    ) -> list[ada.SubprogramDeclaration | ada.TypeDeclaration] | None:
        if not data:
            return []
        return [d[0] for d in data]

    def generic_formal_parameter_declaration(
        self,
        data: list[ada.SubprogramDeclaration | ada.TypeDeclaration],
    ) -> list[ada.SubprogramDeclaration | ada.TypeDeclaration]:
        return data

    def package_body(
        self,
        data: tuple[ID, list[ada.Aspect] | None, list[ada.Declaration] | None, ID],
    ) -> ada.PackageBody:
        identifier, aspects, declarations, end_identifier = data
        assert identifier == end_identifier

        return ada.PackageBody(
            identifier=identifier,
            declarations=declarations,
            statements=None,
            aspects=aspects,
        )

    def generic_package_instantiation_part(
        self,
        data: tuple[ID, list[ID] | None],
    ) -> PackageInstantiationPart:
        return PackageInstantiationPart(name=data[0], associations=data[1])

    def optional_generic_actual_part(self, data: list[list[ID]]) -> list[ID] | None:
        if len(data) == 0:
            return None
        return data[0]

    def generic_actual_part(self, data: list[ID]) -> list[ID]:
        return data

    def generic_association(self, data: list[ID]) -> ID:
        return data[0]

    def explicit_generic_actual_parameter(self, data: list[ID]) -> ID:
        return data[0]

    def formal_type_declaration(self, data: list[ada.TypeDeclaration]) -> ada.TypeDeclaration:
        return data[0]

    def formal_complete_type_declaration(
        self,
        data: tuple[ID, list[ada.Discriminant] | None, Literal["private"], list[ada.Aspect] | None],
    ) -> ada.PrivateType:
        identifier, discriminants, _, aspects = data
        return ada.PrivateType(identifier=identifier, aspects=aspects, discriminants=discriminants)

    def formal_type_definition(self, data: tuple[Literal["private"]]) -> Literal["private"]:
        return data[0]

    def formal_private_type_definition(self, data: tuple[lark.Token]) -> Literal["private"]:
        assert data[0] == "private"
        return "private"

    def formal_subprogram_declaration(
        self,
        data: tuple[ada.SubprogramDeclaration],
    ) -> ada.SubprogramDeclaration:
        return data[0]

    def formal_concrete_subprogram_declaration(
        self,
        data: tuple[ada.SubprogramSpecification],
    ) -> ada.SubprogramDeclaration:
        return ada.SubprogramDeclaration(specification=data[0])

    def formal_concrete_subprogram_specification(
        self,
        data: tuple[ada.SubprogramSpecification],
    ) -> ada.SubprogramSpecification:
        return data[0]

    def aspect_specification(self, data: list[ada.Aspect]) -> list[ada.Aspect]:
        return data

    def aspect_mark(self, data: list[ID]) -> ID:
        return data[0]

    def contract_case_list(self, data: list[list[ada.Expr]]) -> ada.ContractCases:
        return ada.ContractCases(*[(c, e) for c, e in data])  # noqa: C416

    def contract_case(self, data: list[ada.Expr]) -> list[ada.Expr]:
        return data

    def global_specification(self, data: tuple[ada.Global]) -> ada.Global:
        return data[0]

    def moded_global(
        self,
        data: list[tuple[Literal["Input", "Output", "In_Out"], list[ID]]],
    ) -> ada.Global:
        inputs: list[ID] | None = None
        outputs: list[ID] | None = None
        in_outs: list[ID] | None = None

        for mode, ids in data:
            if mode == "Input":
                assert inputs is None
                inputs = ids
            elif mode == "Output":
                assert outputs is None
                outputs = ids
            elif mode == "In_Out":
                assert in_outs is None
                in_outs = ids
            else:
                assert False, mode

        return ada.Global(inputs=inputs, outputs=outputs, in_outs=in_outs)

    def moded_global_list(
        self,
        data: tuple[Literal["Input", "Output", "In_Out"], list[ID]],
    ) -> tuple[Literal["Input", "Output", "In_Out"], list[ID]]:
        return data

    def global_list(self, data: list[ID]) -> list[ID]:
        return data

    def mode_selector(self, data: tuple[lark.Token]) -> str:
        assert isinstance(data[0].value, str)
        assert data[0].value in ("Input", "Output", "In_Out")
        return data[0].value

    def global_item(self, data: tuple[ID]) -> ID:
        return data[0]

    def null_global_specification(self, data: tuple[str]) -> ada.Global:
        assert data[0] == "null"
        return ada.Global()

    def dependency_relation(self, data: list[tuple[ID, list[ID]]]) -> ada.Depends:
        return ada.Depends(dict(data))

    def dependency_clause(self, data: tuple[list[ID], list[ID]]) -> tuple[list[ID], list[ID]]:
        return data

    def output_list(self, data: list[ID | ada.Attribute]) -> ID:
        assert isinstance(data[0], ID)
        return data[0]

    def input_list(self, data: list[ID]) -> list[ID]:
        return data

    def input(self, data: tuple[ID | ada.Attribute]) -> ID:
        assert isinstance(data[0], ID)
        return data[0]

    def null(self, data: tuple[lark.Token]) -> Literal["null"]:
        assert data[0] == "null"
        return "null"

    def pragma_statement(self, data: tuple[ID, list[ada.Expr]]) -> ada.Statement:
        return ada.PragmaStatement(identifier=data[0], parameters=data[1])

    def optional_aspect_specification(
        self,
        data: list[list[ada.Aspect]],
    ) -> list[ada.Aspect] | None:
        if len(data) == 0:
            return None
        return data[0]

    def unified_function_declaration(
        self,
        data: tuple[ada.FunctionSpecification, FunctionPart],
    ) -> ada.Declaration:
        return data[1].declaration(data[0])

    def function_declaration_part(self, data: list[list[ada.Aspect] | None]) -> FunctionDeclPart:
        return FunctionDeclPart(aspects=data[0])

    def function_body_part(
        self,
        data: tuple[list[ada.Aspect] | None, list[ada.Declaration], list[ada.Statement]],
    ) -> FunctionBodyPart:
        return FunctionBodyPart(declarations=data[1], statements=data[2], aspects=data[0])

    def expression_function_part(
        self,
        data: tuple[lark.Token, ada.Expr, list[ada.Aspect] | None],
    ) -> ExpressionFunctionPart:
        return ExpressionFunctionPart(expression=data[1], aspects=data[2])

    def combined_type_declaration(
        self,
        data: tuple[
            ID,
            list[ada.Discriminant] | None,
            ada.AccessType
            | ada.DerivedRangeType
            | ada.DerivedRecordType
            | ada.ModularType
            | ada.PlainDerivedType
            | ada.PrivateType
            | ada.RangeType
            | ada.UnconstrainedArrayType,
            list[ada.Aspect],
        ],
    ) -> ada.TypeDeclaration:
        identifier, discriminants, definition, aspects = data
        assert definition.identifier == ID("__INVALID__")
        if isinstance(definition, ada.ModularType):
            return ada.ModularType(
                identifier=identifier,
                modulus=definition.modulus,
                aspects=aspects,
            )
        if isinstance(definition, ada.RangeType):
            return ada.RangeType(
                identifier=identifier,
                first=definition.first,
                last=definition.last,
                aspects=aspects,
            )
        if isinstance(definition, ada.DerivedRecordType):
            return ada.DerivedRecordType(
                identifier=identifier,
                type_identifier=definition.type_identifier,
            )
        if isinstance(definition, ada.DerivedRangeType):
            return ada.DerivedRangeType(
                identifier=identifier,
                type_identifier=definition.type_identifier,
                first=definition.first,
                last=definition.last,
            )
        if isinstance(definition, ada.PlainDerivedType):
            return ada.PlainDerivedType(
                identifier=identifier,
                type_identifier=definition.type_identifier,
            )
        if isinstance(definition, ada.UnconstrainedArrayType):
            return ada.UnconstrainedArrayType(
                identifier=identifier,
                index_type=definition.index_type,
                component_identifier=definition.component_identifier,
            )
        if isinstance(definition, ada.AccessType):
            return ada.AccessType(
                identifier=identifier,
                object_identifier=definition.object_identifier,
            )
        if isinstance(definition, ada.PrivateType):
            return ada.PrivateType(
                identifier=identifier,
                discriminants=discriminants,
                aspects=aspects,
            )

        assert False, data

    def private_type_definition(self, data: list[lark.Token]) -> ada.PrivateType:
        assert data[0] == "private"
        return ada.PrivateType("__INVALID__")

    def aspect_part(self, data: tuple[ada.Aspect]) -> ada.Aspect:
        return data[0]

    def aspect_always_terminates(self, data: tuple[ada.Expr]) -> ada.AlwaysTerminates:
        return ada.AlwaysTerminates(expression=data[0] if data else None)

    def aspect_annotate(self, data: tuple[ada.Aggregate]) -> ada.Annotate:
        return ada.Annotate(
            *[e.name for e in data[0].elements if isinstance(e, ada.Variable)],
        )

    def aspect_contract_cases(self, data: tuple[ada.ContractCases]) -> ada.ContractCases:
        return data[0]

    def aspect_convention(self, data: tuple[ada.Variable]) -> ada.Convention:
        kind = {"Intrinsic": ada.ConventionKind.Intrinsic}
        return ada.Convention(convention=kind[data[0].name])

    def aspect_default_initial_condition(
        self,
        data: tuple[ada.Expr],
    ) -> ada.DefaultInitialCondition:
        return ada.DefaultInitialCondition(data[0])

    def aspect_depends(self, data: tuple[ada.Depends]) -> ada.Depends:
        return data[0]

    def aspect_ghost(self, data: tuple[lark.Token]) -> ada.Ghost:
        assert data[0] == "Ghost"
        return ada.Ghost()

    def aspect_global(self, data: tuple[ada.Global]) -> ada.Global:
        return data[0]

    def aspect_import(self, _: list[None]) -> ada.Import:
        return ada.Import()

    def aspect_post(self, data: tuple[ada.Expr]) -> ada.Postcondition:
        return ada.Postcondition(data[0])

    def aspect_pre(self, data: tuple[ada.Expr]) -> ada.Precondition:
        return ada.Precondition(data[0])

    def aspect_spark_mode(self, data: list[ada.Variable]) -> ada.SparkMode:
        if not data:
            return ada.SparkMode()
        return ada.SparkMode(off=data[0].name == "off")

    def others(self, data: list[lark.Token]) -> ada.Variable:
        assert data[0] == "others"
        return ada.Variable("others")

    def file(
        self,
        data: list[
            tuple[
                list[ada.ContextItem],
                ada.PackageDeclaration | ada.PackageBody,
                list[ada.SubprogramDeclaration | ada.TypeDeclaration] | None,
            ]
        ],
    ) -> ada.PackageUnit:
        data_len = len(data[:-1])
        assert data_len in [1, 2], data

        declaration_context, declaration, formal_parameters = data[0]
        assert isinstance(declaration, ada.PackageDeclaration), declaration

        if data_len == 1:
            body_context: list[ada.ContextItem] = []
            body = ada.PackageBody(identifier=declaration.identifier)
        elif data_len == 2:
            body_context, tmp, _ = data[1]
            assert isinstance(tmp, ada.PackageBody)
            body = tmp

        return ada.PackageUnit(
            declaration_context=declaration_context,
            declaration=declaration,
            body_context=body_context,
            body=body,
            formal_parameters=formal_parameters,
        )


def parse(text: str) -> ada.PackageUnit:
    return TreeToAda().transform(ADA_GRAMMAR.parse(f"{text}\0"))
