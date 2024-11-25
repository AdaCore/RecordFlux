from __future__ import annotations

from dataclasses import dataclass
from functools import reduce
from pathlib import Path
from typing import Literal, TypeVar

import lark.grammar
from lark.exceptions import VisitError

from rflx import ada
from rflx.common import assert_never
from rflx.error import fail
from rflx.identifier import ID
from rflx.rapidflux import Location


class ParseError(Exception):
    pass


# This grammar supports the *subset* of Ada 2012 used by the RecordFlux generator and in templates.
ADA_GRAMMAR = lark.Lark(
    r"""
        # 2.3 (2/2)
        identifier:                 /[a-zA-Z][a-zA-Z0-9_]*/

        # 2.4 (2)
        ?numeric_literal:           decimal_literal

        # 2.4.1 (2)
        ?decimal_literal:           numeral

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
        ?pragma_argument_association: \
                                    expression

        # 3.1 (3/3)
        ?basic_declaration: \
                                    type_declaration | subtype_declaration \
                                  | object_declaration \
                                  | unified_subprogram_declaration \
                                  | pragma

        # 3.1 (4)
        ?defining_identifier:       identifier

        # 3.2.1 (2)
        ?type_declaration:          combined_type_declaration

        # 3.2.1 (3/3)
        # Type declarations must be combined, as the parser does not backtrack.
        # For full_type_declaration see combined_type_declaration.

        # 3.2.1 (4/2)
        # Contrary to the LRM, we need to include the "is" of the type declaration as part of
        # the type definition as we must treat "is (" as a single token inside
        # enumeration_type_definition to keep the parser LR(1).
        ?type_definition: \
                                    enumeration_type_definition | ("is" (integer_type_definition
                                  | array_type_definition
                                  | record_type_definition | access_type_definition
                                  | derived_type_definition
                                  | private_type_definition))

        # 3.2.2 (2/3)
        subtype_declaration: \
                                    "subtype" defining_identifier "is" subtype_indication \
                                        optional_aspect_specification ";"

        # 3.2.2 (3/2)
        subtype_indication:         subtype_mark optional_constraint

        optional_constraint:        constraint?

        # 3.2.2 (4)
        ?subtype_mark:              name

        # 3.2.2 (5)
        ?constraint:                scalar_constraint

        # 3.2.2 (6)
        ?scalar_constraint: \
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
                                    "new" subtype_indication optional_record_extension_part

        # 3.5 (2)
        ?range_constraint:          "range" (range | box)

        # 3.5 (3)
        range: \
                                    simple_expression ".." simple_expression

        # 3.5.1 (2)
        enumeration_type_definition: \
                                     is_lpar enumeration_literal_specification \
                                        ( "," enumeration_literal_specification )* ")"

        # 3.5.1 (3)
        ?enumeration_literal_specification: \
                                    defining_identifier

        # 3.5.4 (2)
        ?integer_type_definition:   signed_integer_type_definition | modular_type_definition

        # 3.5.4 (3)
        signed_integer_type_definition: \
                                    "range" expression ".." expression

        # 3.5.4 (4)
        modular_type_definition:    "mod" expression

        # 3.6 (2)
        ?array_type_definition: \
                                    combined_array_definition

        # 3.6 (3)
        # unconstrained_array_definition must be combined with constrained_array
        # definition to keep parser LR(1) (see combinded_array_definition).

        # 3.6 (4)
        # index_subtype_definition is combined with discrete_subtype_definition
        # (cf. combined_subtype_definition)

        # 3.6 (7/2)
        component_definition: \
                                    subtype_indication

        # 3.6.1 (3)
        discrete_range:             range

        # 3.7 (2/2)
        ?discriminant_part:         known_discriminant_part

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
        ?default_expression:        expression

        optional_default_expression: \
                                    (":=" default_expression)?

        # 3.8 (2)
        ?record_type_definition:    record_definition

        # 3.8 (3)
        record_definition: \
                                    "record" \
                                        component_list \
                                    "end" "record"
                                  | "null" "record"

        # 3.8 (4)
        component_list: \
                                    component_item component_item*

        # 3.8 (5/1)
        ?component_item:            component_declaration


        # 3.8 (6/3)
        component_declaration: \
                                    defining_identifier_list ":" component_definition \
                                            optional_default_expression \
                                        ";"

        # 3.8.1 (4)
        discrete_choice_list:       discrete_choice ( "|" discrete_choice )*

        # 3.8.1 (5/3)
        ?discrete_choice:           choice_expression | others

        # 3.9.1 (2)
        ?record_extension_part:     "with" record_definition

        optional_record_extension_part: \
                                    record_extension_part?

        # 3.10 (2/2)
        access_type_definition: \
                                    access_to_object_definition

        # 3.10 (3)
        access_to_object_definition: \
                                    "access" subtype_indication

        # 3.10 (5.1/2)
        !null_exclusion:            "not" "null"

        optional_null_exclusion:    null_exclusion?

        # 3.11 (2)
        declarative_part:           declarative_item*

        # 3.11 (3)
        ?declarative_item: \
                                    basic_declarative_item

        # 3.11 (4/1)
        ?basic_declarative_item: \
                                    basic_declaration | use_clause

        # 4.1 (2/3)
        ?name:                      direct_name
                                  | function_call | slice
                                  | selected_component | attribute_reference
                                  | qualified_expression

        # 4.1 (3)
        ?direct_name:               identifier

        # 4.1 (4)
        ?prefix:                    name

        # 4.1.2 (2)
        slice:                      prefix "(" discrete_range ")"

        # 4.1.3 (2)
        selected_component:         prefix "." selector_name

        # 4.1.3 (3)
        ?selector_name:             identifier

        # 4.1.4 (2)
        attribute_reference:        prefix "'" attribute_designator

        # 4.1.4 (3/2)
        attribute_designator: \
                                    identifier optional_argument

        optional_argument:          ( "(" expression ")" )?

        # 4.3 (2)
        ?aggregate:                 array_aggregate

        # 4.3.3 (2)
        array_aggregate: \
                                    "(" array_component_association \
                                        ( "," array_component_association )* ")"

        # 4.3.3 (3/2)
        # positional_array_aggregate: Unified in array_aggregate to keep parser LR(1)

        # 4.3.3 (4)
        # named_array_aggregate: Unified in array_aggregate to keep parser LR(1)

        # 4.3.3 (5/2)
        array_component_association: \
                                    (name "=>")? expression

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
        ?membership_choice_list:    membership_choice

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
        ?conditional_expression:    if_expression | case_expression

        # 4.5.7 (3/3)
        if_expression: \
                                    "if" condition "then" expression \
                                    if_expr_elsifs \
                                    optional_if_expr_else

        if_expr_elsifs:             if_expr_elsif*
        if_expr_elsif:              ("elsif" condition "then" expression)
        optional_if_expr_else:      ("else" expression)?

        # 4.5.7 (4/3)
        ?condition:                 expression

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
        ?statement: \
                                    simple_statement | compound_statement

        # 5.1 (4/2)
        ?simple_statement:          pragma_statement
                                  | unified_assignment_procedure_call_statement
                                  | simple_return_statement

        # 5.1 (4/2)
        ?compound_statement: \
                                    if_statement | case_statement
                                  | loop_statement | block_statement

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
        ?elsebranch:                "else" sequence_of_statements

        # 5.4 (2/3)
        case_statement: \
                                    "case" expression "is" \
                                        case_statement_alternatives \
                                    "end" "case" ";"

        case_statement_alternatives: \
                                    case_statement_alternative \
                                    case_statement_alternative*

        # 5.4 (3)
        case_statement_alternative: \
                                    "when" discrete_choice_list "=>" \
                                        sequence_of_statements


        # 5.5 (2)
        loop_statement: \
                                    iteration_scheme "loop" \
                                        sequence_of_statements \
                                    "end" "loop" ";"

        ?iteration_scheme:          for_iteration_scheme

        for_iteration_scheme:       "for" defining_identifier "in" optional_reverse \
                                        expression ".." expression

        !optional_reverse:          "reverse"?

        # 5.6 (2)
        block_statement:            "declare" \
                                        declarative_part \
                                    "begin" \
                                        handled_sequence_of_statements \
                                    "end" ";"

        # 6.1 (6)
        ?defining_designator:       defining_program_unit_name | defining_operator_symbol

        # 6.1 (7)
        defining_program_unit_name: (identifier ".")* defining_identifier

        # 6.1 (9)
        operator_symbol:            /"(""|[^"])*"/

        # 6.1 (11)
        ?defining_operator_symbol:  operator_symbol

        # 6.1 (14)
        formal_part: \
                                    "(" parameter_specification (";" parameter_specification)* ")"

        optional_formal_part:       formal_part?

        # 6.1 (15/3)
        parameter_specification: \
                                    defining_identifier_list ":" mode subtype_mark \
                                        optional_default_expression

        # 6.1 (16)
        !mode:                       "in"? | "in" "out" | "out"

        # 6.4 (3)
        function_call: \
                                    prefix actual_parameter_part

        # 6.4 (4)
        actual_parameter_part: \
                                    "(" parameter_association ("," parameter_association)* ")"

        # 6.4 (5)
        ?parameter_association: \
                                    (selector_name "=>")? explicit_actual_parameter

        # 6.4 (6)
        explicit_actual_parameter:  expression

        # 6.5 (2/2)
        simple_return_statement:    "return" expression? ";"

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

        # 8.3.1 (2/2)
        !overriding_indicator:       "not"? "overriding"

        optional_overriding_indicator: \
                                    overriding_indicator?

        # 8.4 (2)
        ?use_clause:                use_package_clause | use_type_clause

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
        ?library_unit_declaration:   \
                                    package_declaration
                                  | generic_declaration

        # 10.1.1 (7)
        library_unit_body:          package_body

        # 10.1.1 (8)

        # 10.1.2 (2)
        context_clause:             context_item*

        # 10.1.2 (3)
        ?context_item:              with_clause | use_clause | pragma

        # 10.1.2 (4/2)
        ?with_clause:               nonlimited_with_clause

        # 10.1.2 (4.2/2)
        nonlimited_with_clause:     "with" name ("," name)* ";"


        # 11.2 (2)
        ?handled_sequence_of_statements: \
                                    sequence_of_statements

        # 12.1 (2)
        ?generic_declaration:       generic_package_declaration

        # 12.1 (4)
        generic_package_declaration: \
                                    generic_formal_part package_specification ";"

        # 12.1 (5)
        generic_formal_part:        "generic" generic_formal_parameter_declaration*

        # 12.1 (6)
        generic_formal_parameter_declaration: \
                                    formal_object_declaration \
                                  | formal_type_declaration \
                                  | "with" ( formal_subprogram_declaration
                                           | formal_package_declaration )

        # 12.3 (2/3)
        generic_package_instantiation_part: \
                                        "new" name optional_generic_actual_part

        optional_generic_actual_part:   generic_actual_part?

        # 12.3 (3)
        generic_actual_part: \
                                    "(" generic_association ("," generic_association)* ")"

        # 12.3 (4)
        generic_association: \
                                    ( name "=>" )? \
                                        explicit_generic_actual_parameter

        # 12.3 (5)
        ?explicit_generic_actual_parameter: \
                                    expression

        # 12.4 (2/3)
        formal_object_declaration: \
                                    defining_identifier_list ":" subtype_mark \
                                        optional_default_expression \
                                        optional_aspect_specification ";"

        # 12.5 (2/3)
        ?formal_type_declaration: \
                                    formal_complete_type_declaration

        # 12.5 (2.1/3)
        # As we need to treat "is (" as a token (cf. type_definition), we also need to treat
        # formal_discrete_type_definition specially here.
        formal_complete_type_declaration: \
                                    "type" defining_identifier optional_discriminant_part ("is" \
                                        formal_type_definition | formal_discrete_type_definition)\
                                        optional_aspect_specification ";"

        # 12.5 (3/2)
        ?formal_type_definition: \
                                    formal_private_type_definition
                                  | formal_signed_integer_type_definition
                                  | formal_array_type_definition
                                  | formal_access_type_definition

        # 12.5.1 (2)
        !formal_private_type_definition: \
                                    "private"

        # 12.5.2 (2)
        formal_discrete_type_definition: \
                                    is_lpar "<>" ")"

        # 12.5.2 (3)
        formal_signed_integer_type_definition: \
                                    "range" "<>"

        # 12.5.3 (2)
        ?formal_array_type_definition: \
                                    array_type_definition

        # 12.5.4 (2)
        ?formal_access_type_definition: \
                                    access_type_definition

        # 12.6 (2/2)
        ?formal_subprogram_declaration: \
                                    formal_concrete_subprogram_declaration

        # 12.6 (2.1/3)
        formal_concrete_subprogram_declaration: \
                                    unified_subprogram_declaration

        # 12.7 (2/3)
        formal_package_declaration: \
                                    "package" defining_identifier "is" "new" name \
                                        formal_package_actual_part ";"

        # 12.7 (3/2)
        formal_package_actual_part: \
                                    "(" formal_package_association \
                                        ( "," formal_package_association )* ")"

        formal_package_association: (name "=>")? (formal_package_association_box | name)

        !formal_package_association_box: "<>"

        # 13.1.1 (2/3)
        aspect_specification:       "with"  aspect_part ( "," aspect_part )*

        optional_aspect_specification: aspect_specification?

        # SPARK rules

        # SPARK RM 6.1.3
        contract_case_list:         "(" contract_case ( "," contract_case )* ")"

        contract_case:              condition "=>" expression
                                  | others "=>" expression

        # SPARK RM 6.1.4
        ?global_specification:      moded_global
                                  | global_list
                                  | null_global_specification

        moded_global:               "(" moded_global_list ( "," moded_global_list )* ")"

        moded_global_list:          mode_selector "=>" global_list

        global_list:                global_item
                                  | "(" global_item ( "," global_item )* ")"

        !mode_selector:             "Input" | "Output" | "In_Out"

        ?global_item:               name

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

        unified_subprogram_declaration: \
                                    optional_overriding_indicator \
                                        (unified_function_declaration
                                        | unified_procedure_declaration)

        unified_function_declaration: \
                                    "function" defining_designator \
                                        ( \
                                            unified_generic_function_instantiation
                                          | unified_function_declaration_part \
                                        )

        unified_generic_function_instantiation: \
                                    "is" "new" name \
                                        optional_generic_actual_part \
                                            optional_aspect_specification ";"

        unified_function_declaration_part: \
                                    optional_formal_part "return" optional_null_exclusion \
                                        subtype_mark optional_unified_function_part ";"

        optional_unified_function_part: \
                                    (unified_with | unified_function_is | unified_renames)?

        unified_with:               "with" unified_aspect_parts optional_unified_body
        unified_aspect_parts:      aspect_part ( "," aspect_part )*

        optional_unified_body:      ("is" unified_body)?
        unified_body:               declarative_part \
                                    "begin" \
                                        handled_sequence_of_statements \
                                    "end" defining_designator

        # As we need to treat "is (" as a token (cf. type_definition), we also need to treat
        # unified_expression_function specially here.
        ?unified_function_is:       unified_expression_function
                                    | "is" ( unified_body | unified_abstract | unified_separate )

        unified_expression_function: is_lpar expression ")" optional_aspect_specification

        unified_abstract:           "abstract" optional_aspect_specification
        unified_separate:           "separate" optional_aspect_specification
        unified_renames:            "renames" name optional_aspect_specification

        unified_procedure_declaration: \
                                    "procedure" defining_program_unit_name optional_formal_part \
                                        optional_unified_procedure_declaration_part ";"

        optional_unified_procedure_declaration_part: \
                                    (unified_renames | unified_with | unified_procedure_is)?

        ?unified_procedure_is: \
                                    "is" \
                                    ( \
                                        unified_body
                                      | unified_generic_procedure_instantiation
                                      | unified_separate
                                      | unified_abstract \
                                    )

        unified_generic_procedure_instantiation: \
                                    "new" name \
                                        optional_generic_actual_part optional_aspect_specification

        combined_type_declaration: \
                                    "type" defining_identifier optional_discriminant_part \
                                        type_definition optional_aspect_specification ";"

        !private_type_definition:   "private"

        ?aspect_part:               aspect_always_terminates
                                  | aspect_annotate
                                  | aspect_contract_cases
                                  | aspect_convention
                                  | aspect_default_initial_condition
                                  | aspect_depends
                                  | aspect_dynamic_predicate
                                  | aspect_ghost
                                  | aspect_global
                                  | aspect_import
                                  | aspect_post
                                  | aspect_pre
                                  | aspect_size
                                  | aspect_spark_mode

        aspect_always_terminates:   "Always_Terminates" ( "=>" expression )?

        aspect_annotate:            "Annotate" "=>" expression

        ?aspect_contract_cases:     "Contract_Cases" "=>" contract_case_list

        aspect_convention:          "Convention" "=>" expression

        aspect_default_initial_condition: \
                                    "Default_Initial_Condition" "=>" expression

        ?aspect_depends:            "Depends" "=>" dependency_relation

        aspect_dynamic_predicate:   "Dynamic_Predicate" "=>" expression

        !aspect_ghost:              "Ghost"

        ?aspect_global:             "Global" "=>" global_specification

        aspect_import:              "Import"

        aspect_post:                "Post" "=>" expression

        aspect_pre:                 "Pre" "=>" expression

        aspect_size:                "Size" "=>" expression

        aspect_spark_mode:          "SPARK_Mode" ( "=>" on_off )?

        !on_off:                    "On" | "Off"

        !others:                    "others"

        is_lpar:                     /is\s*\(/

        combined_array_definition: \
                                    "array" "(" combined_subtype_definition \
                                        ( "," combined_subtype_definition )* ")" \
                                            "of" component_definition

        ?combined_subtype_definition: \
                                    subtype_indication

        !box:                        "<>"

        unified_assignment_procedure_call_statement: \
                                    name (":=" expression)? ";"


        # Skip whitespace
        %import common.WS
        %ignore WS

        # Skip comments
        %ignore /--.*/
    """,
    start="file",
    parser="lalr",
    propagate_positions=True,
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
        associations: list[tuple[ID | None, ada.Expr]] | None,
    ):
        self.name = name
        self.associations = associations


class RangeConstraint:
    def __init__(self, first: ada.Expr, last: ada.Expr):
        self.first = first
        self.last = last

    @property
    def to_value_range(self) -> ada.ValueRange:
        return ada.ValueRange(lower=self.first, upper=self.last)


class SignedIntegerConstraint:
    pass


@dataclass
class RenamingDeclaration:
    identifier: ID


@dataclass
class Aspects:
    aspects: list[ada.Aspect] | None = None


@dataclass
class UnifiedFunctionDeclaration:
    designator: ID
    part: UnifiedGenericFunctionInstantiationPart | UnifiedFunctionDeclarationPart


@dataclass
class UnifiedGenericFunctionInstantiationPart:
    name: ID
    actuals: list[tuple[ID | None, ada.Expr]] | None
    aspects: list[ada.Aspect] | None


@dataclass
class UnifiedFunctionDeclarationPart:
    formal_part: list[ada.Parameter] | None
    null_exclusion: bool
    return_type: ID
    function_part: (
        UnifiedWith
        | UnifiedBody
        | UnifiedExpressionFunction
        | UnifiedAbstract
        | UnifiedSeparate
        | UnifiedRenames
        | None
    )


@dataclass
class UnifiedProcedureDeclaration:
    name: ID
    parameters: list[ada.Parameter] | None
    part: (
        UnifiedRenames
        | UnifiedWith
        | UnifiedBody
        | UnifiedGenericProcedureInstantiation
        | UnifiedSeparate
        | UnifiedAbstract
        | None
    )


@dataclass
class UnifiedGenericProcedureInstantiation:
    name: ID
    actual_part: list[tuple[ID | None, ada.Expr]] | None
    aspects: list[ada.Aspect] | None


@dataclass
class UnifiedBody:
    declarations: list[ada.Declaration]
    statements: list[ada.Statement]
    name: ID


@dataclass
class UnifiedExpressionFunction:
    expression: ada.Expr
    aspects: list[ada.Aspect] | None


@dataclass
class UnifiedWith:
    aspect_parts: list[ada.Aspect]
    body: UnifiedBody | None


@dataclass
class UnifiedAbstract:
    aspects: list[ada.Aspect] | None


@dataclass
class UnifiedSeparate:
    aspects: list[ada.Aspect] | None


@dataclass
class UnifiedRenames:
    name: ID
    aspects: list[ada.Aspect] | None


T = TypeVar("T")


class TreeToAda(lark.Transformer[lark.lexer.Token, ada.PackageUnit]):
    def __default__(self, token: lark.Token, _: None, _meta: lark.tree.Meta) -> None:
        raise ParseError(f'missing handler for rule "{token}"')

    def identifier(self, data: list[lark.lexer.Token]) -> ID:
        return ID(data[0])

    def numeral(self, data: list[lark.Token]) -> ada.Number:
        assert isinstance(data[0].value, str)
        return ada.Number(int(data[0].value))

    def string_literal(self, data: list[lark.lexer.Token]) -> ada.String:
        return ada.String(data[0][1:-1])

    def pragma(self, data: tuple[ID, list[ada.Expr] | None]) -> ada.Pragma:
        return ada.Pragma(identifier=data[0], parameters=data[1])

    def optional_pragma_arguments(self, data: list[list[ada.Expr]]) -> list[ada.Expr] | None:
        return self._optional(data)

    def pragma_arguments(self, data: list[ada.Expr]) -> list[ada.Expr]:
        return data

    def subtype_declaration(
        self,
        data: tuple[
            ID,
            tuple[ID, RangeConstraint | SignedIntegerConstraint | None],
            list[ada.Aspect] | None,
        ],
    ) -> ada.Subtype:
        identifier, (base_identifier, constraint), aspects = data
        if constraint is None:
            return ada.Subtype(
                identifier=identifier,
                base_identifier=base_identifier,
                aspects=aspects,
            )
        if isinstance(constraint, RangeConstraint):
            return ada.RangeSubtype(
                identifier=identifier,
                base_identifier=base_identifier,
                first=constraint.first,
                last=constraint.last,
            )
        if isinstance(constraint, SignedIntegerConstraint):
            raise ParseError("signed integer constraint invalid for subtypes")
        assert_never(constraint)

    def subtype_indication(
        self,
        data: tuple[ID, RangeConstraint | SignedIntegerConstraint | None],
    ) -> tuple[ID, RangeConstraint | SignedIntegerConstraint | None]:
        return data

    def optional_constraint(
        self,
        data: list[RangeConstraint | SignedIntegerConstraint],
    ) -> RangeConstraint | SignedIntegerConstraint | None:
        return self._optional(data)

    def object_declaration(
        self,
        data: tuple[
            list[ID],
            bool,
            tuple[ID, RangeConstraint | SignedIntegerConstraint | None],
            ada.Expr | None,
        ],
    ) -> ada.ObjectDeclaration:
        if data[2][1] is not None:
            raise ParseError("invalid constraint in object declaration")
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
        data: tuple[
            tuple[ID, RangeConstraint | SignedIntegerConstraint | None],
            ada.RecordType | None,
        ],
    ) -> ada.DerivedType:
        (identifier, constraint), extension = data
        if extension:
            return ada.DerivedRecordType(
                identifier="__INVALID__",
                type_identifier=identifier,
                record_extension=extension.components,
            )
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
        if isinstance(constraint, SignedIntegerConstraint):
            raise ParseError("signed integer constraint invalid for derived types")
        assert_never(constraint)

    def range(self, data: tuple[ada.Number, ada.Number]) -> RangeConstraint:
        return RangeConstraint(first=data[0], last=data[1])

    def enumeration_type_definition(self, data: list[ID]) -> ada.EnumerationType:
        return ada.EnumerationType("__INVALID__", literals={i: None for i in data[1:]})

    def signed_integer_type_definition(
        self,
        data: tuple[ada.Number, ada.Number],
    ) -> ada.Declaration:
        return ada.SignedIntegerType(identifier="__INVALID__", first=data[0], last=data[1])

    def modular_type_definition(self, data: list[ada.Expr]) -> ada.Declaration:
        return ada.ModularType(identifier="__INVALID__", modulus=data[0])

    def component_definition(
        self,
        data: tuple[tuple[ID, RangeConstraint | SignedIntegerConstraint | None]],
    ) -> ID:
        identifier, constraint = data[0]
        if constraint is not None:
            raise ParseError("invalid constraint in component definition")
        return identifier

    def discrete_range(self, data: tuple[RangeConstraint]) -> ada.ValueRange:
        return data[0].to_value_range

    def optional_discriminant_part(
        self,
        data: list[list[ada.Discriminant]],
    ) -> list[ada.Discriminant] | None:
        return self._optional(data)

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

    def optional_default_expression(self, data: list[ada.Expr]) -> ada.Expr | None:
        return self._optional(data)

    def record_definition(self, data: list[list[ada.Component]]) -> ada.RecordType:
        if not data:
            return ada.RecordType("__INVALID__", components=[])
        return ada.RecordType(identifier="__INVALID__", components=data[0])

    def component_list(self, data: list[ada.Component]) -> list[ada.Component]:
        return data

    def component_declaration(self, data: tuple[list[ID], ID, ada.Expr]) -> ada.Component:
        identifier_list, type_identifier, default = data
        if len(identifier_list) != 1:
            raise ParseError("identifier lists unsupported for record component declaration")
        return ada.Component(
            identifier=identifier_list[0],
            type_identifier=type_identifier,
            default=default,
        )

    def discrete_choice_list(self, data: list[ada.Expr]) -> list[ada.Expr]:
        return data

    def optional_record_extension_part(self, data: list[ada.RecordType]) -> ada.RecordType | None:
        return self._optional(data)

    def access_type_definition(self, data: tuple[ID]) -> ada.AccessType:
        return ada.AccessType(identifier="__INVALID__", object_identifier=data[0])

    def access_to_object_definition(
        self,
        data: tuple[tuple[ID, RangeConstraint | SignedIntegerConstraint | None]],
    ) -> ID:
        identifier, constraint = data[0]
        if constraint is not None:
            raise ParseError("invalid constraint in access to object definition")
        return identifier

    def null_exclusion(self, data: list[lark.Token]) -> bool:
        assert len(data) > 0
        return True

    def optional_null_exclusion(self, data: list[bool]) -> bool:
        if len(data) == 0:
            return False
        return True

    def declarative_part(self, data: list[ada.Declaration]) -> list[ada.Declaration]:
        return data

    def slice(self, data: tuple[ID, ada.ValueRange]) -> ada.Slice:
        name, value_range = data
        return ada.Slice(ada.Variable(name), value_range.lower, value_range.upper)

    def selected_component(self, data: tuple[ID, ID]) -> ID:
        return data[0] * data[1]

    def attribute_reference(self, data: tuple[ID, tuple[ID, ada.Expr | None]]) -> ada.Attribute:
        prefix, (identifier, expression) = data
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
        attribute_expressions: dict[str, type[ada.AttributeExpr]] = {
            "Pos": ada.Pos,
            "Succ": ada.Succ,
            "Val": ada.Val,
        }

        name = identifier.parts[0]
        if expression is not None:
            return attribute_expressions[name](prefix, expression)
        return attributes[name](prefix)

    def attribute_designator(self, data: tuple[ID, ada.Expr | None]) -> tuple[ID, ada.Expr | None]:
        return data[0], data[1]

    def optional_argument(self, data: list[ada.Expr]) -> ada.Expr | None:
        return self._optional(data)

    def array_aggregate(
        self,
        data: list[tuple[ID | None, ada.Expr]],
    ) -> ada.Aggregate | ada.NamedAggregate:
        named_arguments = [(n, e) for n, e in data if n is not None]
        positional_arguments = [e for n, e in data if n is None]
        if named_arguments and positional_arguments:
            raise ParseError("invalid mixed positional and named aggregate")
        if named_arguments:
            return ada.NamedAggregate(*named_arguments)
        if positional_arguments:
            return ada.Aggregate(*positional_arguments)
        assert False

    def array_component_association(self, data: list[ID | ada.Expr]) -> tuple[ID | None, ada.Expr]:
        if len(data) == 1:
            assert isinstance(data[0], ada.Expr)
            return None, data[0]
        assert len(data) == 2
        assert isinstance(data[0], ID)
        assert isinstance(data[1], ada.Expr)
        return data[0], data[1]

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
        assert len(data) == 1
        assert isinstance(data[0], ada.Expr)
        return data[0]

    def choice_relation(self, data: list[ada.Expr | str]) -> ada.Expr:
        assert len(data) == 1
        assert isinstance(data[0], ada.Expr)
        return data[0]

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

    def membership_choice(self, data: tuple[RangeConstraint]) -> ada.Expr:
        return data[0].to_value_range

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
        data: list[list[ada.Expr]],
    ) -> list[tuple[ada.Expr, ada.Expr]]:
        return [(c, e) for c, e in data]  # noqa: C416

    def if_expr_elsif(
        self,
        data: tuple[ada.Expr, ada.Expr],
    ) -> tuple[ada.Expr, ada.Expr]:
        return data

    def optional_if_expr_else(self, data: list[ada.Expr]) -> ada.Expr | None:
        return self._optional(data)

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
            condition_statements.extend((c, e) for c, e in data[2])
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
        return self._optional(data)

    def case_statement(
        self,
        data: tuple[ada.Expr, list[tuple[list[ada.Expr], list[ada.Statement]]]],
    ) -> ada.CaseStatement:
        control_expression, alternatives = data
        case_statements = [(choice[0], statements) for choice, statements in alternatives]
        return ada.CaseStatement(
            control_expression=control_expression,
            case_statements=case_statements,
        )

    def case_statement_alternatives(
        self,
        data: list[tuple[list[ada.Expr], list[ada.Statement]]],
    ) -> list[tuple[list[ada.Expr], list[ada.Statement]]]:
        return data

    def case_statement_alternative(
        self,
        data: tuple[list[ada.Expr], list[ada.Statement]],
    ) -> tuple[list[ada.Expr], list[ada.Statement]]:
        return data[0], data[1]

    def loop_statement(
        self,
        data: tuple[tuple[ID, bool, ada.Expr, ada.Expr], list[ada.Statement]],
    ) -> ada.ForLoop:
        (identifier, reverse, lower, upper), statements = data
        return ada.ForIn(
            identifier=identifier,
            iterator=ada.ValueRange(lower=lower, upper=upper),
            statements=statements,
            reverse=reverse,
        )

    def for_iteration_scheme(
        self,
        data: tuple[ID, bool, ada.Expr, ada.Expr],
    ) -> tuple[ID, bool, ada.Expr, ada.Expr]:
        return data[0], data[1], data[2], data[3]

    def optional_reverse(self, data: list[lark.Token]) -> bool:
        if len(data) == 0:
            return False
        return True

    def block_statement(
        self,
        data: tuple[list[ada.Declaration], list[ada.Statement]],
    ) -> ada.Declare:
        return ada.Declare(declarations=data[0], statements=data[1])

    def defining_program_unit_name(self, data: list[ID]) -> ID:
        return reduce(lambda l, r: l * r, data)

    def operator_symbol(self, data: tuple[lark.Token]) -> ID:
        return ID(data[0].value)

    def formal_part(self, data: list[ada.Parameter]) -> list[ada.Parameter]:
        return data

    def optional_formal_part(self, data: list[list[ada.Parameter]]) -> list[ada.Parameter] | None:
        return self._optional(data)

    def parameter_specification(
        self,
        data: tuple[list[ID], Literal["", "in", "out", "in out"], ID, ada.Expr | None],
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
            return ada.InParameter(
                identifiers=identifiers,
                type_identifier=type_identifier,
                default=default,
            )

        if mode == "":
            return ada.Parameter(
                identifiers=identifiers,
                type_identifier=type_identifier,
                default=default,
            )

        assert_never(mode)

    def mode(self, data: list[lark.Token]) -> Literal["", "in", "out", "in out"]:
        if not data:
            return ""
        if "in" in data:
            if "out" in data:
                return "in out"
            return "in"
        assert data == ["out"]
        return "out"

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
        data: tuple[
            ID,
            list[ada.Aspect] | None,
            PackageSpecificationPart | PackageInstantiationPart,
        ],
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
            if identifier != part.identifier:
                raise ParseError("inconsistent package identifiers")
            return ada.PackageDeclaration(
                identifier=identifier,
                declarations=part.declarations,
                private_declarations=part.private_declarations,
                aspects=aspects,
            )

        assert_never(part)

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

    def use_package_clause(self, data: list[ID]) -> ada.UsePackageClause:
        if len(data) != 1:
            raise ParseError("multiple packages in one use-clause not implemented")
        return ada.UsePackageClause(identifier=data[0])

    def use_type_clause(self, data: list[ID]) -> ada.UseTypeClause:
        return ada.UseTypeClause(*data)

    def compilation_unit(
        self,
        data: tuple[
            list[ada.ContextItem],
            tuple[
                ada.PackageDeclaration | ada.PackageBody,
                list[ada.FormalSubprogramDeclaration | ada.TypeDeclaration] | None,
            ],
        ],
    ) -> tuple[
        list[ada.ContextItem],
        ada.PackageDeclaration | ada.PackageBody,
        list[ada.FormalSubprogramDeclaration | ada.TypeDeclaration] | None,
    ]:
        return data[0], data[1][0], data[1][1]

    def library_item(
        self,
        data: list[
            tuple[
                list[ada.FormalSubprogramDeclaration | ada.TypeDeclaration] | None,
                ada.PackageDeclaration | ada.PackageBody,
            ]
        ],
    ) -> tuple[
        ada.PackageDeclaration | ada.PackageBody,
        list[ada.FormalSubprogramDeclaration | ada.TypeDeclaration] | None,
    ]:
        return data[0][1], data[0][0]

    def library_unit_body(
        self,
        data: list[ada.PackageBody],
    ) -> tuple[None, ada.PackageBody]:
        return None, data[0]

    def context_clause(self, data: list[ada.ContextItem]) -> list[ada.ContextItem]:
        return data

    def nonlimited_with_clause(self, data: list[ID]) -> ada.WithClause:
        return ada.WithClause(data[0])

    def generic_package_declaration(
        self,
        data: tuple[
            list[ada.FormalSubprogramDeclaration | ada.TypeDeclaration] | None,
            ada.PackageDeclaration,
        ],
    ) -> tuple[
        list[ada.FormalSubprogramDeclaration | ada.TypeDeclaration] | None,
        ada.PackageDeclaration,
    ]:
        return data

    def generic_formal_part(
        self,
        data: list[tuple[ada.FormalSubprogramDeclaration | ada.TypeDeclaration]],
    ) -> list[ada.FormalSubprogramDeclaration | ada.TypeDeclaration] | None:
        if not data:
            return []
        return [d[0] for d in data]

    def generic_formal_parameter_declaration(
        self,
        data: list[ada.FormalSubprogramDeclaration | ada.TypeDeclaration | ada.ObjectDeclaration],
    ) -> list[ada.FormalSubprogramDeclaration | ada.TypeDeclaration | ada.ObjectDeclaration]:
        return data

    def package_body(
        self,
        data: tuple[ID, list[ada.Aspect] | None, list[ada.Declaration] | None, ID],
    ) -> ada.PackageBody:
        identifier, aspects, declarations, end_identifier = data
        if identifier != end_identifier:
            raise ParseError("inconsistent package identifiers")

        return ada.PackageBody(
            identifier=identifier,
            declarations=declarations,
            statements=None,
            aspects=aspects,
        )

    def overriding_indicator(self, data: list[lark.Token]) -> bool | None:
        return len(data) == 1

    def optional_overriding_indicator(self, data: list[bool | None]) -> bool | None:
        return self._optional(data)

    def generic_package_instantiation_part(
        self,
        data: tuple[ID, list[tuple[ID | None, ada.Expr]] | None],
    ) -> PackageInstantiationPart:
        return PackageInstantiationPart(name=data[0], associations=data[1])

    def optional_generic_actual_part(
        self,
        data: list[list[tuple[ID | None, ada.Expr]]],
    ) -> list[tuple[ID | None, ada.Expr]] | None:
        return self._optional(data)

    def generic_actual_part(
        self,
        data: list[tuple[ID | None, ada.Expr]],
    ) -> list[tuple[ID | None, ada.Expr]]:
        return data

    def generic_association(
        self,
        data: list[ID | ada.Expr],
    ) -> tuple[ID | None, ada.Expr]:
        if len(data) == 1:
            assert isinstance(data[0], ada.Expr)
            return None, data[0]
        assert len(data) == 2
        assert isinstance(data[0], ID)
        assert isinstance(data[1], ada.Expr)
        return data[0], data[1]

    def formal_object_declaration(
        self,
        data: tuple[list[ID], ID, ada.Expr | None, list[ada.Aspect] | None],
    ) -> ada.ObjectDeclaration:
        identifiers, type_identifier, default_expression, aspects = data
        return ada.ObjectDeclaration(
            identifiers=identifiers,
            type_identifier=type_identifier,
            expression=default_expression,
            aspects=aspects,
        )

    def formal_complete_type_declaration(
        self,
        data: tuple[
            ID,
            list[ada.Discriminant] | None,
            ada.PrivateType
            | ada.DiscreteType
            | ada.FormalSignedIntegerType
            | ada.UnconstrainedArrayType
            | ada.ArrayType
            | ada.AccessType,
            list[ada.Aspect] | None,
        ],
    ) -> (
        ada.PrivateType
        | ada.DiscreteType
        | ada.FormalSignedIntegerType
        | ada.UnconstrainedArrayType
        | ada.ArrayType
        | ada.AccessType
    ):
        identifier, discriminants, declaration, aspects = data
        assert declaration.identifier == ID("__INVALID__")
        if isinstance(declaration, ada.PrivateType):
            return ada.PrivateType(
                identifier=identifier,
                discriminants=discriminants,
                aspects=aspects,
            )
        if isinstance(declaration, ada.DiscreteType):
            return ada.DiscreteType(
                identifier=identifier,
                discriminants=discriminants,
                aspects=aspects,
            )
        if isinstance(declaration, ada.FormalSignedIntegerType):
            return ada.FormalSignedIntegerType(
                identifier=identifier,
                discriminants=discriminants,
                aspects=aspects,
            )
        if isinstance(declaration, ada.UnconstrainedArrayType):
            return ada.UnconstrainedArrayType(
                identifier=identifier,
                index_type=declaration.index_type,
                component_identifier=declaration.component_identifier,
            )
        if isinstance(declaration, ada.ArrayType):
            return ada.ArrayType(
                identifier=identifier,
                index_type=declaration.index_type,
                component_identifier=declaration.component_identifier,
            )
        if isinstance(declaration, ada.AccessType):
            return ada.AccessType(
                identifier=identifier,
                object_identifier=declaration.object_identifier,
            )

        assert_never(declaration)

    def formal_private_type_definition(self, _: None) -> ada.PrivateType:
        return ada.PrivateType("__INVALID__")

    def formal_discrete_type_definition(self, _: None) -> ada.DiscreteType:
        return ada.DiscreteType("__INVALID__")

    def formal_signed_integer_type_definition(self, _: None) -> ada.FormalSignedIntegerType:
        return ada.FormalSignedIntegerType("__INVALID__")

    def formal_concrete_subprogram_declaration(
        self,
        data: tuple[ada.Declaration],
    ) -> ada.FormalSubprogramDeclaration:
        assert isinstance(data[0], ada.SubprogramDeclaration)
        assert data[0].formal_parameters is None
        return ada.FormalSubprogramDeclaration(
            specification=data[0].specification,
            aspects=data[0].aspects,
        )

    def formal_package_declaration(
        self,
        data: tuple[ID, ID, list[tuple[ID | None, ID | None]]],
    ) -> ada.FormalPackageDeclaration:
        identifier, generic_identifier, associations = data
        return ada.FormalPackageDeclaration(
            identifier=identifier,
            generic_identifier=generic_identifier,
            associations=associations,
        )

    def formal_package_actual_part(
        self,
        data: list[tuple[ID | None, ID | None] | None],
    ) -> list[tuple[ID | None, ID | None] | None] | None:
        if data == [(None, None)]:
            return None
        return data

    def formal_package_association(
        self,
        data: list[ID | None],
    ) -> tuple[ID | None, ID | None] | None:
        if len(data) == 2:
            return data[0], data[1]
        return None, data[0]

    def formal_package_association_box(self, _: tuple[lark.Token]) -> None:
        return None

    def aspect_specification(self, data: list[ada.Aspect]) -> list[ada.Aspect]:
        return data

    def contract_case_list(self, data: list[list[ada.Expr]]) -> ada.ContractCases:
        return ada.ContractCases(*[(c, e) for c, e in data])  # noqa: C416

    def contract_case(self, data: list[ada.Expr]) -> list[ada.Expr]:
        return data

    def moded_global(
        self,
        data: list[tuple[Literal["Input", "Output", "In_Out"], list[ID]]],
    ) -> ada.Global:
        inputs: list[ID] | None = None
        outputs: list[ID] | None = None
        in_outs: list[ID] | None = None

        for mode, ids in data:
            if mode == "Input":
                if inputs is not None:
                    raise ParseError("duplicate Input")
                inputs = ids
            elif mode == "Output":
                if outputs is not None:
                    raise ParseError("duplicate Output")
                outputs = ids
            elif mode == "In_Out":
                if in_outs is not None:
                    raise ParseError("duplicate In_Out")
                in_outs = ids
            else:
                assert_never(mode)

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
        return self._optional(data)

    def combined_type_declaration(
        self,
        data: tuple[
            ID,
            list[ada.Discriminant] | None,
            ada.AccessType
            | ada.DerivedRangeType
            | ada.DerivedRecordType
            | ada.EnumerationType
            | ada.ModularType
            | ada.PlainDerivedType
            | ada.PrivateType
            | ada.SignedIntegerType
            | ada.RecordType
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
        if isinstance(definition, ada.SignedIntegerType):
            return ada.SignedIntegerType(
                identifier=identifier,
                first=definition.first,
                last=definition.last,
                aspects=aspects,
            )
        if isinstance(definition, ada.DerivedRecordType):
            return ada.DerivedRecordType(
                identifier=identifier,
                type_identifier=definition.type_identifier,
                record_extension=definition.record_extension,
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
        if isinstance(definition, ada.EnumerationType):
            size_aspect = [a for a in aspects if isinstance(a, ada.SizeAspect)] if aspects else None
            if size_aspect and len(size_aspect) > 1:
                raise ParseError("multiple size aspects")

            return ada.EnumerationType(
                identifier=identifier,
                literals=definition.literals,
                size=size_aspect[0].expression if size_aspect else None,
            )
        if isinstance(definition, ada.RecordType):
            return ada.RecordType(
                identifier=identifier,
                components=definition.components,
                discriminants=discriminants,
                variant_part=definition.variant_part,
                aspects=aspects,
                abstract=definition.abstract,
                tagged=definition.tagged,
            )

        assert_never(definition)

    def private_type_definition(self, data: list[lark.Token]) -> ada.PrivateType:
        assert data[0] == "private"
        return ada.PrivateType("__INVALID__")

    def aspect_always_terminates(self, data: tuple[ada.Expr]) -> ada.AlwaysTerminates:
        return ada.AlwaysTerminates(expression=data[0] if data else None)

    def aspect_annotate(self, data: tuple[ada.Aggregate]) -> ada.Annotate:
        return ada.Annotate(
            *[e.name for e in data[0].elements if isinstance(e, ada.Variable)],
        )

    def aspect_convention(self, data: tuple[ada.Variable]) -> ada.Convention:
        kind = {"Intrinsic": ada.ConventionKind.Intrinsic}
        return ada.Convention(convention=kind[data[0].name])

    def aspect_default_initial_condition(
        self,
        data: tuple[ada.Expr],
    ) -> ada.DefaultInitialCondition:
        return ada.DefaultInitialCondition(data[0])

    def aspect_dynamic_predicate(self, data: tuple[ada.Expr]) -> ada.DynamicPredicate:
        return ada.DynamicPredicate(data[0])

    def aspect_ghost(self, data: tuple[lark.Token]) -> ada.Ghost:
        assert data[0] == "Ghost"
        return ada.Ghost()

    def aspect_import(self, _: list[None]) -> ada.Import:
        return ada.Import()

    def aspect_post(self, data: tuple[ada.Expr]) -> ada.Postcondition:
        return ada.Postcondition(data[0])

    def aspect_pre(self, data: tuple[ada.Expr]) -> ada.Precondition:
        return ada.Precondition(data[0])

    def aspect_size(self, data: tuple[ada.Expr]) -> ada.SizeAspect:
        return ada.SizeAspect(data[0])

    def aspect_spark_mode(self, data: list[bool]) -> ada.SparkMode:
        if not data:
            return ada.SparkMode()
        return ada.SparkMode(off=not data[0])

    def on_off(self, data: tuple[lark.Token]) -> bool:
        if data[0].value == "On":
            return True
        return False

    def others(self, data: list[lark.Token]) -> ada.Variable:
        assert data[0] == "others"
        return ada.Variable("others")

    def is_lpar(self, _: tuple[lark.Token]) -> None:
        return None

    def combined_array_definition(
        self,
        data: tuple[tuple[ID, RangeConstraint | SignedIntegerConstraint | None], ID],
    ) -> ada.UnconstrainedArrayType | ada.ArrayType:
        (index_type, constraint), component_identifier = data
        if type(constraint) == SignedIntegerConstraint:
            return ada.UnconstrainedArrayType(
                identifier="__INVALID__",
                index_type=index_type,
                component_identifier=component_identifier,
            )
        if constraint is not None:
            raise ParseError("discrete array subtypes not implemented")
        return ada.ArrayType(
            identifier="__INVALID__",
            index_type=index_type,
            component_identifier=component_identifier,
        )

    def box(self, _: tuple[lark.Token]) -> SignedIntegerConstraint:
        return SignedIntegerConstraint()

    def unified_assignment_procedure_call_statement(
        self,
        data: list[ID | ada.Expr],
    ) -> ada.CallStatement | ada.Assignment:
        if len(data) == 1:
            if isinstance(data[0], ID):
                return ada.CallStatement(identifier=data[0])
            call = data[0]
            assert isinstance(call, ada.Call), type(call)
            return ada.CallStatement(
                identifier=call.identifier,
                arguments=call.arguments,
                named_arguments=call.named_arguments,
            )
        assert len(data) == 2
        assert isinstance(data[1], ada.Expr)
        return ada.Assignment(data[0], data[1])

    def file(
        self,
        data: list[
            tuple[
                list[ada.ContextItem],
                ada.PackageDeclaration | ada.PackageBody,
                list[
                    ada.FormalSubprogramDeclaration
                    | ada.TypeDeclaration
                    | ada.FormalPackageDeclaration
                ]
                | None,
            ]
        ],
    ) -> ada.PackageUnit:
        data_len = len(data[:-1])
        if data_len not in [1, 2]:
            raise ParseError(f"expected exactly two units, got {data_len}")

        declaration_context, declaration, formal_parameters = data[0]
        if not isinstance(declaration, ada.PackageDeclaration):
            raise ParseError("expected package declaration")

        if data_len == 1:
            body_context: list[ada.ContextItem] = []
            body = ada.PackageBody(identifier=declaration.identifier)
        else:
            assert data_len == 2
            body_context, tmp, _ = data[1]
            if not isinstance(tmp, ada.PackageBody):
                raise ParseError("expected package body as second unit")
            body = tmp

        return ada.PackageUnit(
            declaration_context=declaration_context,
            declaration=declaration,
            body_context=body_context,
            body=body,
            formal_parameters=formal_parameters,
        )

    def unified_subprogram_declaration(
        self,
        data: tuple[bool | None, UnifiedFunctionDeclaration | UnifiedProcedureDeclaration],
    ) -> ada.Declaration:
        overriding, declaration = data
        if isinstance(declaration, UnifiedFunctionDeclaration):
            return self._unified_function_declaration(declaration, overriding)
        if isinstance(declaration, UnifiedProcedureDeclaration):
            return self._unified_procedure_declaration(declaration, overriding)
        assert_never(declaration)

    def unified_function_declaration(
        self,
        data: tuple[ID, UnifiedGenericFunctionInstantiationPart | UnifiedFunctionDeclarationPart],
    ) -> UnifiedFunctionDeclaration:
        return UnifiedFunctionDeclaration(data[0], data[1])

    def unified_generic_function_instantiation(
        self,
        data: tuple[ID, list[tuple[ID | None, ada.Expr]] | None, list[ada.Aspect] | None],
    ) -> UnifiedGenericFunctionInstantiationPart:
        name, actuals, aspects = data
        return UnifiedGenericFunctionInstantiationPart(name=name, actuals=actuals, aspects=aspects)

    def unified_function_declaration_part(
        self,
        data: tuple[
            list[ada.Parameter] | None,
            bool,
            ID,
            UnifiedWith
            | UnifiedBody
            | UnifiedExpressionFunction
            | UnifiedAbstract
            | UnifiedSeparate
            | UnifiedRenames
            | None,
        ],
    ) -> UnifiedFunctionDeclarationPart:
        formal_part, null_exclusion, return_type, function_part = data
        return UnifiedFunctionDeclarationPart(
            formal_part=formal_part,
            null_exclusion=null_exclusion,
            return_type=return_type,
            function_part=function_part,
        )

    def optional_unified_function_part(
        self,
        data: list[
            UnifiedWith
            | UnifiedBody
            | UnifiedExpressionFunction
            | UnifiedAbstract
            | UnifiedSeparate
            | UnifiedRenames
        ],
    ) -> (
        UnifiedWith
        | UnifiedBody
        | UnifiedExpressionFunction
        | UnifiedAbstract
        | UnifiedSeparate
        | UnifiedRenames
        | None
    ):
        return self._optional(data)

    def unified_with(self, data: tuple[list[ada.Aspect], UnifiedBody | None]) -> UnifiedWith:
        return UnifiedWith(aspect_parts=data[0], body=data[1])

    def unified_aspect_parts(self, data: list[ada.Aspect]) -> list[ada.Aspect]:
        return data

    def optional_unified_body(self, data: list[UnifiedBody]) -> UnifiedBody | None:
        return self._optional(data)

    def unified_body(
        self,
        data: tuple[list[ada.Declaration], list[ada.Statement], ID],
    ) -> UnifiedBody:
        return UnifiedBody(declarations=data[0], statements=data[1], name=data[2])

    def unified_expression_function(
        self,
        data: tuple[None, ada.Expr, list[ada.Aspect] | None],
    ) -> UnifiedExpressionFunction:
        return UnifiedExpressionFunction(expression=data[1], aspects=data[2])

    def unified_abstract(self, data: list[list[ada.Aspect] | None]) -> UnifiedAbstract:
        return UnifiedAbstract(aspects=data[0])

    def unified_separate(self, data: list[list[ada.Aspect] | None]) -> UnifiedSeparate:
        return UnifiedSeparate(aspects=data[0])

    def unified_renames(self, data: tuple[ID, list[ada.Aspect] | None]) -> UnifiedRenames:
        return UnifiedRenames(name=data[0], aspects=data[1])

    def unified_procedure_declaration(
        self,
        data: tuple[
            ID,
            list[ada.Parameter] | None,
            UnifiedRenames
            | UnifiedWith
            | UnifiedBody
            | UnifiedGenericProcedureInstantiation
            | UnifiedSeparate
            | UnifiedAbstract
            | None,
        ],
    ) -> UnifiedProcedureDeclaration:
        name, parameters, part = data
        return UnifiedProcedureDeclaration(name, parameters, part)

    def optional_unified_procedure_declaration_part(
        self,
        data: list[
            UnifiedRenames
            | UnifiedWith
            | UnifiedBody
            | UnifiedGenericProcedureInstantiation
            | UnifiedSeparate
            | UnifiedAbstract
        ],
    ) -> (
        UnifiedRenames
        | UnifiedWith
        | UnifiedBody
        | UnifiedGenericProcedureInstantiation
        | UnifiedSeparate
        | UnifiedAbstract
        | None
    ):
        return self._optional(data)

    def unified_generic_procedure_instantiation(
        self,
        data: tuple[
            ID,
            list[tuple[ID | None, ada.Expr]] | None,
            list[ada.Aspect] | None,
        ],
    ) -> UnifiedGenericProcedureInstantiation:
        return UnifiedGenericProcedureInstantiation(data[0], data[1], data[2])

    @staticmethod
    def _optional(data: list[T]) -> T | None:
        if len(data) == 0:
            return None
        assert len(data) == 1
        return data[0]

    def _unified_function_declaration(
        self,
        declaration: UnifiedFunctionDeclaration,
        overriding: bool | None,
    ) -> ada.Declaration:
        if isinstance(declaration.part, UnifiedGenericFunctionInstantiationPart):
            return ada.GenericFunctionInstantiation(
                identifier=declaration.designator,
                generic_name=declaration.part.name,
                associations=declaration.part.actuals,
                overriding=overriding,
                aspects=declaration.part.aspects,
            )
        if isinstance(declaration.part, UnifiedFunctionDeclarationPart):
            function_specification = ada.FunctionSpecification(
                identifier=declaration.designator,
                return_type=declaration.part.return_type,
                parameters=declaration.part.formal_part,
                overriding=overriding,
                not_null=declaration.part.null_exclusion,
            )
            if declaration.part.function_part is None:
                return ada.SubprogramDeclaration(
                    specification=function_specification,
                )
            if isinstance(declaration.part.function_part, UnifiedWith):
                if declaration.part.function_part.body is None:
                    return ada.SubprogramDeclaration(
                        specification=function_specification,
                        aspects=declaration.part.function_part.aspect_parts,
                    )
                return ada.SubprogramBody(
                    specification=function_specification,
                    declarations=declaration.part.function_part.body.declarations,
                    statements=declaration.part.function_part.body.statements,
                    aspects=declaration.part.function_part.aspect_parts,
                )
            if isinstance(declaration.part.function_part, UnifiedBody):
                assert function_specification.identifier == declaration.part.function_part.name
                return ada.SubprogramBody(
                    specification=function_specification,
                    declarations=declaration.part.function_part.declarations,
                    statements=declaration.part.function_part.statements,
                )
            if isinstance(declaration.part.function_part, UnifiedExpressionFunction):
                return ada.ExpressionFunctionDeclaration(
                    specification=function_specification,
                    expression=declaration.part.function_part.expression,
                    aspects=declaration.part.function_part.aspects,
                )
            if isinstance(declaration.part.function_part, UnifiedAbstract):
                return ada.AbstractSubprogramDeclaration(
                    specification=function_specification,
                    aspects=declaration.part.function_part.aspects,
                )
            if isinstance(declaration.part.function_part, UnifiedSeparate):
                return ada.SeparateSubprogramDeclaration(
                    specification=function_specification,
                    aspects=declaration.part.function_part.aspects,
                )
            if isinstance(declaration.part.function_part, UnifiedRenames):
                return ada.SubprogramRenamingDeclaration(
                    specification=function_specification,
                    subprogram_identifier=declaration.part.function_part.name,
                    aspects=declaration.part.function_part.aspects,
                )
            assert_never(declaration.part.function_part)
        assert_never(declaration.part)

    def _unified_procedure_declaration(
        self,
        declaration: UnifiedProcedureDeclaration,
        overriding: bool | None,
    ) -> ada.Declaration:
        procedure_specification = ada.ProcedureSpecification(
            identifier=declaration.name,
            parameters=declaration.parameters,
            overriding=overriding,
        )
        if declaration.part is None:
            return ada.SubprogramDeclaration(
                specification=procedure_specification,
            )
        if isinstance(declaration.part, UnifiedRenames):
            return ada.SubprogramRenamingDeclaration(
                specification=procedure_specification,
                subprogram_identifier=declaration.part.name,
                aspects=declaration.part.aspects,
            )
        if isinstance(declaration.part, UnifiedWith):
            if declaration.part.body is None:
                return ada.SubprogramDeclaration(
                    specification=procedure_specification,
                    aspects=declaration.part.aspect_parts,
                )
            return ada.SubprogramBody(
                specification=procedure_specification,
                declarations=declaration.part.body.declarations,
                statements=declaration.part.body.statements,
                aspects=declaration.part.aspect_parts,
            )
        if isinstance(declaration.part, UnifiedBody):
            if procedure_specification.identifier != declaration.part.name:
                raise ParseError("inconsistent identifier")
            return ada.SubprogramBody(
                specification=procedure_specification,
                declarations=declaration.part.declarations,
                statements=declaration.part.statements,
            )
        if isinstance(declaration.part, UnifiedGenericProcedureInstantiation):
            return ada.GenericProcedureInstantiation(
                identifier=declaration.name,
                generic_name=declaration.part.name,
                associations=declaration.part.actual_part,
                overriding=overriding,
                aspects=declaration.part.aspects,
            )
        if isinstance(declaration.part, UnifiedSeparate):
            return ada.SeparateSubprogramDeclaration(
                specification=procedure_specification,
                aspects=declaration.part.aspects,
            )
        if isinstance(declaration.part, UnifiedAbstract):
            return ada.AbstractSubprogramDeclaration(
                specification=procedure_specification,
                aspects=declaration.part.aspects,
            )
        assert_never(declaration.part)


def parse(text: str, source: Path | None = None) -> ada.PackageUnit:
    try:
        return TreeToAda().transform(ADA_GRAMMAR.parse(f"{text}\0"))
    except VisitError as e:
        assert isinstance(e.orig_exc, ParseError)  # noqa: PT017
        assert isinstance(e.obj, lark.Tree)  # noqa: PT017
        fail(
            str(e.orig_exc),
            location=Location(
                start=(e.obj.meta.line, e.obj.meta.column),
                end=(e.obj.meta.end_line, e.obj.meta.end_column),
                source=source,
            ),
        )


def parse_file(spec: Path) -> ada.PackageUnit:
    data = [spec.read_text()]
    body = spec.with_suffix(".adb")
    if body.exists():
        data.append(body.read_text())
    return parse(text="\n".join(data), source=spec)
