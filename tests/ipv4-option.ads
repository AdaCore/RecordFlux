package IPv4.Option
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

   function Valid_Copied_0 (Buffer : Types.Bytes) return Boolean is
      (((Buffer'Length >= 1 and then Buffer'First <= (Types.Index_Type'Last / 2)) and then Valid_Flag_Type (Buffer (Buffer'First .. Buffer'First), 7)))
     with
       Pre => Is_Contained (Buffer);

   function Get_Copied_0 (Buffer : Types.Bytes) return Flag_Type is
      (Convert_To_Flag_Type (Buffer (Buffer'First .. Buffer'First), 7))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Copied_0 (Buffer));

   function Valid_Copied (Buffer : Types.Bytes) return Boolean is
      (Valid_Copied_0 (Buffer))
     with
       Pre => Is_Contained (Buffer);

   function Get_Copied (Buffer : Types.Bytes) return Flag_Type is
      ((if Valid_Copied_0 (Buffer) then Get_Copied_0 (Buffer) else Unreachable_Flag_Type))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Copied (Buffer));

   function Valid_Option_Class_00 (Buffer : Types.Bytes) return Boolean is
      ((Valid_Copied_0 (Buffer) and then ((Buffer'Length >= 1 and then Buffer'First <= (Types.Index_Type'Last / 2)) and then Valid_Option_Class_Type (Buffer (Buffer'First .. Buffer'First), 5))))
     with
       Pre => Is_Contained (Buffer);

   function Get_Option_Class_00 (Buffer : Types.Bytes) return Option_Class_Type is
      (Convert_To_Option_Class_Type (Buffer (Buffer'First .. Buffer'First), 5))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Option_Class_00 (Buffer));

   function Valid_Option_Class (Buffer : Types.Bytes) return Boolean is
      (Valid_Option_Class_00 (Buffer))
     with
       Pre => Is_Contained (Buffer);

   function Get_Option_Class (Buffer : Types.Bytes) return Option_Class_Type is
      ((if Valid_Option_Class_00 (Buffer) then Get_Option_Class_00 (Buffer) else Unreachable_Option_Class_Type))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Option_Class (Buffer));

   function Valid_Option_Number_000 (Buffer : Types.Bytes) return Boolean is
      ((Valid_Option_Class_00 (Buffer) and then (Buffer'Length >= 1 and then Buffer'First <= (Types.Index_Type'Last / 2))))
     with
       Pre => Is_Contained (Buffer);

   function Get_Option_Number_000 (Buffer : Types.Bytes) return Option_Number_Type is
      (Convert_To_Option_Number_Type (Buffer (Buffer'First .. Buffer'First), 0))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Option_Number_000 (Buffer));

   function Valid_Option_Number (Buffer : Types.Bytes) return Boolean is
      ((Valid_Option_Number_000 (Buffer) and then (Get_Option_Number_000 (Buffer) > 1 or (Get_Option_Class_00 (Buffer) = Control and then Get_Option_Number_000 (Buffer) = 1))))
     with
       Pre => Is_Contained (Buffer);

   function Get_Option_Number (Buffer : Types.Bytes) return Option_Number_Type is
      ((if Valid_Option_Number_000 (Buffer) then Get_Option_Number_000 (Buffer) else Unreachable_Option_Number_Type))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Option_Number (Buffer));

   function Valid_Option_Length_0001 (Buffer : Types.Bytes) return Boolean is
      ((Valid_Option_Number_000 (Buffer) and then (((Buffer'Length >= 2 and then Buffer'First <= (Types.Index_Type'Last / 2)) and then Get_Option_Number_000 (Buffer) > 1) and then Convert_To_Option_Length_Type_Base (Buffer ((Buffer'First + 1) .. (Buffer'First + 1)), 0) >= 2)))
     with
       Pre => Is_Contained (Buffer);

   function Get_Option_Length_0001 (Buffer : Types.Bytes) return Option_Length_Type is
      (Convert_To_Option_Length_Type_Base (Buffer ((Buffer'First + 1) .. (Buffer'First + 1)), 0))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Option_Length_0001 (Buffer));

   function Valid_Option_Length (Buffer : Types.Bytes) return Boolean is
      ((Valid_Option_Length_0001 (Buffer) and then (((((Get_Option_Class_00 (Buffer) = Control and then Get_Option_Number_000 (Buffer) = 2) and then Get_Option_Length_0001 (Buffer) = 11) or ((Get_Option_Class_00 (Buffer) = Control and then Get_Option_Number_000 (Buffer) = 8) and then Get_Option_Length_0001 (Buffer) = 4)) or (Get_Option_Class_00 (Buffer) = Control and then ((Get_Option_Number_000 (Buffer) = 3 or Get_Option_Number_000 (Buffer) = 7) or Get_Option_Number_000 (Buffer) = 9))) or (Get_Option_Class_00 (Buffer) = Debugging_And_Measurement and then Get_Option_Number_000 (Buffer) = 4))))
     with
       Pre => Is_Contained (Buffer);

   function Get_Option_Length (Buffer : Types.Bytes) return Option_Length_Type is
      ((if Valid_Option_Length_0001 (Buffer) then Get_Option_Length_0001 (Buffer) else Unreachable_Option_Length_Type))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Option_Length (Buffer));

   function Valid_Option_Data_00010 (Buffer : Types.Bytes) return Boolean is
      ((Valid_Option_Length_0001 (Buffer) and then ((Buffer'Length >= Types.Length_Type (Get_Option_Length_0001 (Buffer)) and then Buffer'First <= (Types.Index_Type'Last / 2)) and then (((((Get_Option_Class_00 (Buffer) = Control and then Get_Option_Number_000 (Buffer) = 2) and then Get_Option_Length_0001 (Buffer) = 11) or ((Get_Option_Class_00 (Buffer) = Control and then Get_Option_Number_000 (Buffer) = 8) and then Get_Option_Length_0001 (Buffer) = 4)) or (Get_Option_Class_00 (Buffer) = Control and then ((Get_Option_Number_000 (Buffer) = 3 or Get_Option_Number_000 (Buffer) = 7) or Get_Option_Number_000 (Buffer) = 9))) or (Get_Option_Class_00 (Buffer) = Debugging_And_Measurement and then Get_Option_Number_000 (Buffer) = 4)))))
     with
       Pre => Is_Contained (Buffer);

   function Get_Option_Data_00010_First (Buffer : Types.Bytes) return Types.Index_Type is
      ((Buffer'First + 2))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Option_Data_00010 (Buffer));

   function Get_Option_Data_00010_Last (Buffer : Types.Bytes) return Types.Index_Type is
      ((Buffer'First + Types.Length_Type (Get_Option_Length_0001 (Buffer)) + (-1)))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Option_Data_00010 (Buffer));

   function Valid_Option_Data (Buffer : Types.Bytes) return Boolean is
      (Valid_Option_Data_00010 (Buffer))
     with
       Pre => Is_Contained (Buffer);

   function Get_Option_Data_First (Buffer : Types.Bytes) return Types.Index_Type is
      ((if Valid_Option_Data_00010 (Buffer) then Get_Option_Data_00010_First (Buffer) else Unreachable_Types_Index_Type))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Option_Data (Buffer));

   function Get_Option_Data_Last (Buffer : Types.Bytes) return Types.Index_Type is
      ((if Valid_Option_Data_00010 (Buffer) then Get_Option_Data_00010_Last (Buffer) else Unreachable_Types_Index_Type))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Option_Data (Buffer));

   procedure Get_Option_Data (Buffer : Types.Bytes; First : out Types.Index_Type; Last : out Types.Index_Type)
     with
       Pre => (Is_Contained (Buffer) and then Valid_Option_Data (Buffer)),
       Post => (First = Get_Option_Data_First (Buffer) and then Last = Get_Option_Data_Last (Buffer));

   function Is_Valid (Buffer : Types.Bytes) return Boolean is
      (((Valid_Option_Number_000 (Buffer) and then (Get_Option_Class_00 (Buffer) = Control and then Get_Option_Number_000 (Buffer) = 1)) or Valid_Option_Data_00010 (Buffer)))
     with
       Pre => Is_Contained (Buffer);

   function Message_Length (Buffer : Types.Bytes) return Types.Length_Type is
      ((if (Valid_Option_Number_000 (Buffer) and then (Get_Option_Class_00 (Buffer) = Control and then Get_Option_Number_000 (Buffer) = 1)) then 1 elsif Valid_Option_Data_00010 (Buffer) then Types.Length_Type (Get_Option_Length_0001 (Buffer)) else Unreachable_Types_Length_Type))
     with
       Pre => (Is_Contained (Buffer) and then Is_Valid (Buffer));

end IPv4.Option;
