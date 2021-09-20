pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
pragma SPARK_Mode;
with RFLX.RFLX_Scalar_Sequence;
with RFLX.Universal;
pragma Warnings (Off, "unit ""*RFLX_Types"" is not referenced");
with RFLX.RFLX_Types;
pragma Warnings (On, "unit ""*RFLX_Types"" is not referenced");

package RFLX.Universal.Values is new RFLX.RFLX_Scalar_Sequence (RFLX.Universal.Value, RFLX.Universal.Value, RFLX.Universal.Valid, RFLX.Universal.To_Actual, RFLX.Universal.To_Base);
