pragma Style_Checks ("N3aAbCdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
pragma SPARK_Mode;
with RFLX.RFLX_Scalar_Sequence;
pragma Warnings (Off, "unit ""*RFLX_Types"" is not referenced");
with RFLX.RFLX_Types;
pragma Warnings (On, "unit ""*RFLX_Types"" is not referenced");

package RFLX.TLV.Tags is new RFLX.RFLX_Scalar_Sequence (RFLX.TLV.Tag, 8, RFLX.TLV.Valid_Tag, RFLX.TLV.To_Actual, RFLX.TLV.To_Base_Integer);
