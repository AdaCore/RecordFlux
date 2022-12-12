from pathlib import Path

import pytest

from rflx.identifier import ID
from tests import utils
from tests.const import MAIN
from tests.data.fixtures.integration import (
    DEFINITE_MESSAGE_WITH_BUILTIN_TYPE_SPEC,
    DEFINITE_PARAMETERIZED_MESSAGE_SPEC,
    PARAMETERIZED_MESSAGE_SPEC,
)
from tests.utils import FEATURES, assert_provable_code, create_complement, create_model, get_config


@pytest.mark.verification
@pytest.mark.parametrize("feature", [f.name for f in FEATURES])
def test_provability(feature: str, tmp_path: Path) -> None:
    config = get_config(feature)
    if config.prove is None:
        pytest.skip()
    model, integration = create_model(feature)
    units = []
    if model.sessions:
        assert len(model.sessions) == 1
        assert model.sessions[0].identifier == ID("Test::Session")
        units = ["main", "lib", "rflx-test-session"]
        create_complement(config, feature, tmp_path)
    assert_provable_code(model, integration, tmp_path, main=MAIN, units=[*units, *config.prove])


@pytest.mark.verification
def test_definite_message_with_builtin_type_provability(tmp_path: Path) -> None:
    utils.assert_provable_code_string(
        DEFINITE_MESSAGE_WITH_BUILTIN_TYPE_SPEC, tmp_path, units=["rflx-test-message"]
    )


@pytest.mark.verification
def test_parameterized_message_provability(tmp_path: Path) -> None:
    utils.assert_provable_code_string(
        PARAMETERIZED_MESSAGE_SPEC, tmp_path, units=["rflx-test-message"]
    )


@pytest.mark.verification
def test_definite_parameterized_message_provability(tmp_path: Path) -> None:
    utils.assert_provable_code_string(
        DEFINITE_PARAMETERIZED_MESSAGE_SPEC, tmp_path, units=["rflx-test-message"]
    )


@pytest.mark.verification
def test_message_field_conditions_provability(tmp_path: Path) -> None:
    spec = """\
      package Test is
         type Byte is range 0 .. 2 ** 8 - 1 with Size => 8;
         type Length_16 is range 0 .. 2 ** 16 - 1 with Size => 16;

         type My_Seq is sequence of Byte;

         type Repr is
            message
               Count : Byte;
               Length : Length_16;
               Hash : My_Seq
                  with Size => 32
                  then Structs
                     with Size => 8 * Length - 16 - (Hash'Last - Count'First + 1)
                     if 8 * Length >= 16 + (Hash'Last - Count'First + 1);
               Structs : My_Seq;
            end message
               with Byte_Order => Low_Order_First;
      end Test;
    """
    utils.assert_provable_code_string(spec, tmp_path, units=["rflx-test-repr"])


@pytest.mark.verification
def test_parameterized_message_set_scalar(tmp_path: Path) -> None:
    spec = """\
      package Test is

         type Length_16 is range 0 .. 2 ** 16 - 1 with Size => 16;

         type Signature_Length is range 0 .. 512 with Size => 16;

         type Measurements_Response (Signature_Length : Signature_Length;
                                     Has_Signature    : Boolean) is
            message
               Opaque_Length : Length_16;
               Opaque_Data : Opaque
                  with Size => 8 * Opaque_Length
                  then Signature
                     if Has_Signature = True
                  then null
                     if Has_Signature = False;
               Signature : Opaque
                  with Size => 8 * Signature_Length;
            end message
               with Byte_Order => Low_Order_First;

      end Test;
    """
    utils.assert_provable_code_string(spec, tmp_path, units=["rflx-test-measurements_response"])


@pytest.mark.verification
def test_message_large_number_of_fields(tmp_path: Path) -> None:
    spec = """\
      package Test is
         type Byte is range 0 .. 2 ** 8 - 1 with Size => 8;

         type Repr is
            message
               Field_1 : Byte;
               Field_2 : Byte;
               Field_3 : Byte;
               Field_4 : Byte;
               Field_5 : Byte;
               Field_6 : Byte;
               Field_7 : Byte;
               Field_8 : Byte;
               Field_9 : Byte;
               Field_10 : Byte;
               Field_11 : Byte;
               Field_12 : Byte;
               Field_13 : Byte;
               Field_14 : Byte;
               Field_15 : Byte;
               Field_16 : Byte;
               Field_17 : Byte;
               Field_18 : Byte;
               Field_19 : Byte;
               Field_20 : Byte;
               Field_21 : Byte;
               Field_22 : Byte;
               Field_23 : Byte;
               Field_24 : Byte;
            end message;
      end Test;
   """
    utils.assert_provable_code_string(spec, tmp_path, units=["rflx-test-repr"])
