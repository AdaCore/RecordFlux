pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
pragma SPARK_Mode;
with RFLX.RFLX_Scalar_Sequence;
pragma Warnings (Off, "unit ""*RFLX_Types"" is not referenced");
with RFLX.RFLX_Types;
pragma Warnings (On, "unit ""*RFLX_Types"" is not referenced");

package RFLX.Universal.Option_Types is new RFLX.RFLX_Scalar_Sequence (RFLX.Universal.Option_Type, 8, RFLX.Universal.Valid_Option_Type, RFLX.Universal.To_Actual, RFLX.Universal.To_U64);