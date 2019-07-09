generic
package Expression.Generic_Message with
  SPARK_Mode
is

   function Is_Contained (Buffer : Types.Bytes) return Boolean with
     Ghost,
     Import;

   procedure Label (Buffer : Types.Bytes) with
     Ghost,
     Post => Is_Contained (Buffer);

   function Valid_Payload_0 (Buffer : Types.Bytes) return Boolean is
     ((Buffer'Length >= 2 and then Buffer'First <= (Types.Index_Type'Last / 2)))
    with
     Pre => Is_Contained (Buffer);

   function Get_Payload_0_First (Buffer : Types.Bytes) return Types.Index_Type is
     (Buffer'First)
    with
     Pre => (Is_Contained (Buffer) and then Valid_Payload_0 (Buffer));

   function Get_Payload_0_Last (Buffer : Types.Bytes) return Types.Index_Type is
     ((Buffer'First + 1))
    with
     Pre => (Is_Contained (Buffer) and then Valid_Payload_0 (Buffer));

   function Valid_Payload (Buffer : Types.Bytes) return Boolean is
     ((Valid_Payload_0 (Buffer) and then Buffer (Buffer'First .. (Buffer'First + 1)) = (1, 2)))
    with
     Pre => Is_Contained (Buffer);

   function Get_Payload_First (Buffer : Types.Bytes) return Types.Index_Type is
     ((if Valid_Payload_0 (Buffer) then Get_Payload_0_First (Buffer) else Unreachable_Types_Index_Type))
    with
     Pre => (Is_Contained (Buffer) and then Valid_Payload (Buffer));

   function Get_Payload_Last (Buffer : Types.Bytes) return Types.Index_Type is
     ((if Valid_Payload_0 (Buffer) then Get_Payload_0_Last (Buffer) else Unreachable_Types_Index_Type))
    with
     Pre => (Is_Contained (Buffer) and then Valid_Payload (Buffer));

   procedure Get_Payload (Buffer : Types.Bytes; First : out Types.Index_Type; Last : out Types.Index_Type) with
     Pre => (Is_Contained (Buffer) and then Valid_Payload (Buffer)),
     Post => (First = Get_Payload_First (Buffer) and then Last = Get_Payload_Last (Buffer));

   function Is_Valid (Buffer : Types.Bytes) return Boolean is
     ((Valid_Payload_0 (Buffer) and then Buffer (Buffer'First .. (Buffer'First + 1)) = (1, 2)))
    with
     Pre => Is_Contained (Buffer);

   function Message_Length (Buffer : Types.Bytes) return Types.Length_Type is
     ((if (Valid_Payload_0 (Buffer) and then Buffer (Buffer'First .. (Buffer'First + 1)) = (1, 2)) then 2 else Unreachable_Types_Length_Type))
    with
     Pre => (Is_Contained (Buffer) and then Is_Valid (Buffer));

end Expression.Generic_Message;
