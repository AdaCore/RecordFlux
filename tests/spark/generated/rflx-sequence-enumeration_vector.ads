pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
pragma SPARK_Mode;
with RFLX.RFLX_Scalar_Sequence;
pragma Warnings (Off, "unit ""*RFLX_Types"" is not referenced");
with RFLX.RFLX_Types;
pragma Warnings (On, "unit ""*RFLX_Types"" is not referenced");

package RFLX.Sequence.Enumeration_Vector is new RFLX.RFLX_Scalar_Sequence (RFLX.Sequence.Enumeration, 8, RFLX.Sequence.Valid_Enumeration, RFLX.Sequence.To_Actual, RFLX.Sequence.To_U64);
