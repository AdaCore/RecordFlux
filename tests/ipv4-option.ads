package IPv4.Option
  with SPARK_Mode
is

   function Is_Contained (Buffer : Bytes) return Boolean
     with
       Ghost,
       Import;

   procedure Initialize (Buffer : Bytes)
     with
       Post => Is_Contained (Buffer);

   function Valid_Copied_0 (Buffer : Bytes) return Boolean is
      (((Buffer'Length >= 1 and then Buffer'First <= (Natural'Last / 2)) and then Valid_Flag_Type (Buffer (Buffer'First .. Buffer'First), 7)))
     with
       Pre => Is_Contained (Buffer);

   function Copied_0 (Buffer : Bytes) return Flag_Type is
      (Convert_To_Flag_Type (Buffer (Buffer'First .. Buffer'First), 7))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Copied_0 (Buffer));

   function Valid_Copied (Buffer : Bytes) return Boolean is
      (Valid_Copied_0 (Buffer))
     with
       Pre => Is_Contained (Buffer);

   function Copied (Buffer : Bytes) return Flag_Type is
      ((if Valid_Copied_0 (Buffer) then Copied_0 (Buffer) else Unreachable_Flag_Type))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Copied (Buffer));

   function Valid_Option_Class_00 (Buffer : Bytes) return Boolean is
      ((Valid_Copied_0 (Buffer) and then ((Buffer'Length >= 1 and then Buffer'First <= (Natural'Last / 2)) and then Valid_Option_Class_Type (Buffer (Buffer'First .. Buffer'First), 5))))
     with
       Pre => Is_Contained (Buffer);

   function Option_Class_00 (Buffer : Bytes) return Option_Class_Type is
      (Convert_To_Option_Class_Type (Buffer (Buffer'First .. Buffer'First), 5))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Option_Class_00 (Buffer));

   function Valid_Option_Class (Buffer : Bytes) return Boolean is
      (Valid_Option_Class_00 (Buffer))
     with
       Pre => Is_Contained (Buffer);

   function Option_Class (Buffer : Bytes) return Option_Class_Type is
      ((if Valid_Option_Class_00 (Buffer) then Option_Class_00 (Buffer) else Unreachable_Option_Class_Type))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Option_Class (Buffer));

   function Valid_Option_Number_000 (Buffer : Bytes) return Boolean is
      ((Valid_Option_Class_00 (Buffer) and then (Buffer'Length >= 1 and then Buffer'First <= (Natural'Last / 2))))
     with
       Pre => Is_Contained (Buffer);

   function Option_Number_000 (Buffer : Bytes) return Option_Number_Type is
      (Convert_To_Option_Number_Type (Buffer (Buffer'First .. Buffer'First), 0))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Option_Number_000 (Buffer));

   function Valid_Option_Number (Buffer : Bytes) return Boolean is
      ((Valid_Option_Number_000 (Buffer) and then (Option_Number_000 (Buffer) > 1 or (Option_Class_00 (Buffer) = Control and then Option_Number_000 (Buffer) = 1))))
     with
       Pre => Is_Contained (Buffer);

   function Option_Number (Buffer : Bytes) return Option_Number_Type is
      ((if Valid_Option_Number_000 (Buffer) then Option_Number_000 (Buffer) else Unreachable_Option_Number_Type))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Option_Number (Buffer));

   function Valid_Option_Length_0001 (Buffer : Bytes) return Boolean is
      ((Valid_Option_Number_000 (Buffer) and then ((Buffer'Length >= 2 and then Buffer'First <= (Natural'Last / 2)) and then Option_Number_000 (Buffer) > 1)))
     with
       Pre => Is_Contained (Buffer);

   function Option_Length_0001 (Buffer : Bytes) return Option_Length_Type is
      (Convert_To_Option_Length_Type (Buffer ((Buffer'First + 1) .. (Buffer'First + 1)), 0))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Option_Length_0001 (Buffer));

   function Valid_Option_Length (Buffer : Bytes) return Boolean is
      ((Valid_Option_Length_0001 (Buffer) and then (((((Option_Class_00 (Buffer) = Control and then Option_Number_000 (Buffer) = 2) and then Option_Length_0001 (Buffer) = 11) or ((Option_Class_00 (Buffer) = Control and then Option_Number_000 (Buffer) = 8) and then Option_Length_0001 (Buffer) = 4)) or (Option_Class_00 (Buffer) = Control and then ((Option_Number_000 (Buffer) = 3 or Option_Number_000 (Buffer) = 7) or Option_Number_000 (Buffer) = 9))) or (Option_Class_00 (Buffer) = Debugging_And_Measurement and then Option_Number_000 (Buffer) = 4))))
     with
       Pre => Is_Contained (Buffer);

   function Option_Length (Buffer : Bytes) return Option_Length_Type is
      ((if Valid_Option_Length_0001 (Buffer) then Option_Length_0001 (Buffer) else Unreachable_Option_Length_Type))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Option_Length (Buffer));

   function Valid_Option_Data_00010 (Buffer : Bytes) return Boolean is
      ((Valid_Option_Length_0001 (Buffer) and then ((Buffer'Length >= Natural (Option_Length_0001 (Buffer)) and then Buffer'First <= (Natural'Last / 2)) and then (((((Option_Class_00 (Buffer) = Control and then Option_Number_000 (Buffer) = 2) and then Option_Length_0001 (Buffer) = 11) or ((Option_Class_00 (Buffer) = Control and then Option_Number_000 (Buffer) = 8) and then Option_Length_0001 (Buffer) = 4)) or (Option_Class_00 (Buffer) = Control and then ((Option_Number_000 (Buffer) = 3 or Option_Number_000 (Buffer) = 7) or Option_Number_000 (Buffer) = 9))) or (Option_Class_00 (Buffer) = Debugging_And_Measurement and then Option_Number_000 (Buffer) = 4)))))
     with
       Pre => Is_Contained (Buffer);

   function Option_Data_00010_First (Buffer : Bytes) return Natural is
      ((Buffer'First + 2))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Option_Data_00010 (Buffer));

   function Option_Data_00010_Last (Buffer : Bytes) return Natural is
      ((Buffer'First + Natural (Option_Length_0001 (Buffer)) + (-1)))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Option_Data_00010 (Buffer));

   function Valid_Option_Data (Buffer : Bytes) return Boolean is
      (Valid_Option_Data_00010 (Buffer))
     with
       Pre => Is_Contained (Buffer);

   function Option_Data_First (Buffer : Bytes) return Natural is
      ((if Valid_Option_Data_00010 (Buffer) then Option_Data_00010_First (Buffer) else Unreachable_Natural))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Option_Data (Buffer));

   function Option_Data_Last (Buffer : Bytes) return Natural is
      ((if Valid_Option_Data_00010 (Buffer) then Option_Data_00010_Last (Buffer) else Unreachable_Natural))
     with
       Pre => (Is_Contained (Buffer) and then Valid_Option_Data (Buffer));

   procedure Option_Data (Buffer : Bytes; First : out Natural; Last : out Natural)
     with
       Pre => (Is_Contained (Buffer) and then Valid_Option_Data (Buffer)),
       Post => (First = Option_Data_First (Buffer) and then Last = Option_Data_Last (Buffer));

   function Is_Valid (Buffer : Bytes) return Boolean is
      (((Valid_Option_Number_000 (Buffer) and then (Option_Class_00 (Buffer) = Control and then Option_Number_000 (Buffer) = 1)) or Valid_Option_Data_00010 (Buffer)))
     with
       Pre => Is_Contained (Buffer);

   function Message_Length (Buffer : Bytes) return Natural is
      ((if (Valid_Option_Number_000 (Buffer) and then (Option_Class_00 (Buffer) = Control and then Option_Number_000 (Buffer) = 1)) then 1 elsif Valid_Option_Data_00010 (Buffer) then Natural (Option_Length_0001 (Buffer)) else Unreachable_Natural))
     with
       Pre => (Is_Contained (Buffer) and then Is_Valid (Buffer));

end IPv4.Option;
