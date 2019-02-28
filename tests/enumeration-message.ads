package Enumeration.Message
  with SPARK_Mode
is

   function Is_Contained (Buffer : Types.Bytes) return Boolean
     with
       Ghost,
       Import;

   procedure Label (Buffer : Types.Bytes)
     with
       Ghost,
       Post => Is_Contained (Buffer);

   function Valid_Priority_0 (Buffer : Types.Bytes) return Boolean is
      (((Buffer'Length >= 1 and then Buffer'First <= (Types.Index_Type'Last / 2)) and then Valid_Priority (Buffer (Buffer'First .. Buffer'First), 5)))
     with
       Pre => Is_Contained (Buffer);

   function Get_Priority_0 (Buffer : Types.Bytes) return Priority is
      (Convert_To_Priority (Buffer (Buffer'First .. Buffer'First), 5))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Priority_0 (Buffer));

   function Valid_Priority (Buffer : Types.Bytes) return Boolean is
      (Valid_Priority_0 (Buffer))
     with
       Pre => Is_Contained (Buffer);

   function Get_Priority (Buffer : Types.Bytes) return Priority is
      ((if Valid_Priority_0 (Buffer) then Get_Priority_0 (Buffer) else Unreachable_Priority))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Priority (Buffer));

   function Is_Valid (Buffer : Types.Bytes) return Boolean is
      (Valid_Priority_0 (Buffer))
     with
       Pre => Is_Contained (Buffer);

   function Message_Length (Buffer : Types.Bytes) return Types.Length_Type is
      ((if Valid_Priority_0 (Buffer) then 0 else Unreachable_Types_Length_Type))
     with
       Pre => (Is_Contained (Buffer) and then Is_Valid (Buffer));

end Enumeration.Message;
