pragma SPARK_Mode;
with RFLX.RFLX_Types;
with RFLX.In_TLV.Generic_Contains;
with RFLX.TLV.Message;

package RFLX.In_TLV.Contains is new RFLX.In_TLV.Generic_Contains (RFLX.RFLX_Types, RFLX.TLV.Message);
