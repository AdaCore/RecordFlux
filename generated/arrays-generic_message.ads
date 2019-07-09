with Scalar_Sequence;

generic
   with package Modular_Vector is new Scalar_Sequence (<>);
   with package Range_Vector is new Scalar_Sequence (<>);
   with package Enumeration_Vector is new Scalar_Sequence (<>);
   with package AV_Enumeration_Vector is new Scalar_Sequence (<>);
package Arrays.Generic_Message with
  SPARK_Mode
is

   function Is_Contained (Buffer : Types.Bytes) return Boolean with
     Ghost,
     Import;

   procedure Label (Buffer : Types.Bytes) with
     Ghost,
     Post => Is_Contained (Buffer);

   function Valid_Length_0 (Buffer : Types.Bytes) return Boolean is
     ((Buffer'Length >= 1 and then Buffer'First <= (Types.Index_Type'Last / 2)))
    with
     Pre => Is_Contained (Buffer);

   function Get_Length_0 (Buffer : Types.Bytes) return Length_Type is
     (Convert_To_Length_Type (Buffer (Buffer'First .. Buffer'First), 0))
    with
     Pre => (Is_Contained (Buffer) and then Valid_Length_0 (Buffer));

   function Valid_Length (Buffer : Types.Bytes) return Boolean is
     (Valid_Length_0 (Buffer))
    with
     Pre => Is_Contained (Buffer);

   function Get_Length (Buffer : Types.Bytes) return Length_Type is
     ((if Valid_Length_0 (Buffer) then Get_Length_0 (Buffer) else Unreachable_Length_Type))
    with
     Pre => (Is_Contained (Buffer) and then Valid_Length (Buffer));

   function Valid_Modular_Vector_00 (Buffer : Types.Bytes) return Boolean is
     ((Valid_Length_0 (Buffer) and then (Buffer'Length >= (Types.Length_Type (Get_Length_0 (Buffer)) + 1) and then Buffer'First <= (Types.Index_Type'Last / 2))))
    with
     Pre => Is_Contained (Buffer);

   function Get_Modular_Vector_00_First (Buffer : Types.Bytes) return Types.Index_Type is
     ((Buffer'First + 1))
    with
     Pre => (Is_Contained (Buffer) and then Valid_Modular_Vector_00 (Buffer));

   function Get_Modular_Vector_00_Last (Buffer : Types.Bytes) return Types.Index_Type is
     ((Types.Length_Type (Get_Length_0 (Buffer)) + Buffer'First))
    with
     Pre => (Is_Contained (Buffer) and then Valid_Modular_Vector_00 (Buffer));

   function Valid_Modular_Vector (Buffer : Types.Bytes) return Boolean is
     (Valid_Modular_Vector_00 (Buffer))
    with
     Pre => Is_Contained (Buffer);

   function Get_Modular_Vector_First (Buffer : Types.Bytes) return Types.Index_Type is
     ((if Valid_Modular_Vector_00 (Buffer) then Get_Modular_Vector_00_First (Buffer) else Unreachable_Types_Index_Type))
    with
     Pre => (Is_Contained (Buffer) and then Valid_Modular_Vector (Buffer));

   function Get_Modular_Vector_Last (Buffer : Types.Bytes) return Types.Index_Type is
     ((if Valid_Modular_Vector_00 (Buffer) then Get_Modular_Vector_00_Last (Buffer) else Unreachable_Types_Index_Type))
    with
     Pre => (Is_Contained (Buffer) and then Valid_Modular_Vector (Buffer));

   procedure Get_Modular_Vector (Buffer : Types.Bytes; First : out Types.Index_Type; Last : out Types.Index_Type) with
     Pre => (Is_Contained (Buffer) and then Valid_Modular_Vector (Buffer)),
     Post => ((First = Get_Modular_Vector_First (Buffer) and then Last = Get_Modular_Vector_Last (Buffer)) and then Modular_Vector.Is_Contained (Buffer (First .. Last)));

   function Valid_Range_Vector_000 (Buffer : Types.Bytes) return Boolean is
     ((Valid_Modular_Vector_00 (Buffer) and then (Buffer'Length >= (Types.Length_Type (Get_Length_0 (Buffer)) + 3) and then Buffer'First <= (Types.Index_Type'Last / 2))))
    with
     Pre => Is_Contained (Buffer);

   function Get_Range_Vector_000_First (Buffer : Types.Bytes) return Types.Index_Type is
     ((Types.Length_Type (Get_Length_0 (Buffer)) + Buffer'First + 1))
    with
     Pre => (Is_Contained (Buffer) and then Valid_Range_Vector_000 (Buffer));

   function Get_Range_Vector_000_Last (Buffer : Types.Bytes) return Types.Index_Type is
     ((Types.Length_Type (Get_Length_0 (Buffer)) + Buffer'First + 2))
    with
     Pre => (Is_Contained (Buffer) and then Valid_Range_Vector_000 (Buffer));

   function Valid_Range_Vector (Buffer : Types.Bytes) return Boolean is
     (Valid_Range_Vector_000 (Buffer))
    with
     Pre => Is_Contained (Buffer);

   function Get_Range_Vector_First (Buffer : Types.Bytes) return Types.Index_Type is
     ((if Valid_Range_Vector_000 (Buffer) then Get_Range_Vector_000_First (Buffer) else Unreachable_Types_Index_Type))
    with
     Pre => (Is_Contained (Buffer) and then Valid_Range_Vector (Buffer));

   function Get_Range_Vector_Last (Buffer : Types.Bytes) return Types.Index_Type is
     ((if Valid_Range_Vector_000 (Buffer) then Get_Range_Vector_000_Last (Buffer) else Unreachable_Types_Index_Type))
    with
     Pre => (Is_Contained (Buffer) and then Valid_Range_Vector (Buffer));

   procedure Get_Range_Vector (Buffer : Types.Bytes; First : out Types.Index_Type; Last : out Types.Index_Type) with
     Pre => (Is_Contained (Buffer) and then Valid_Range_Vector (Buffer)),
     Post => ((First = Get_Range_Vector_First (Buffer) and then Last = Get_Range_Vector_Last (Buffer)) and then Range_Vector.Is_Contained (Buffer (First .. Last)));

   function Valid_Enumeration_Vector_0000 (Buffer : Types.Bytes) return Boolean is
     ((Valid_Range_Vector_000 (Buffer) and then (Buffer'Length >= (Types.Length_Type (Get_Length_0 (Buffer)) + 5) and then Buffer'First <= (Types.Index_Type'Last / 2))))
    with
     Pre => Is_Contained (Buffer);

   function Get_Enumeration_Vector_0000_First (Buffer : Types.Bytes) return Types.Index_Type is
     ((Types.Length_Type (Get_Length_0 (Buffer)) + Buffer'First + 3))
    with
     Pre => (Is_Contained (Buffer) and then Valid_Enumeration_Vector_0000 (Buffer));

   function Get_Enumeration_Vector_0000_Last (Buffer : Types.Bytes) return Types.Index_Type is
     ((Types.Length_Type (Get_Length_0 (Buffer)) + Buffer'First + 4))
    with
     Pre => (Is_Contained (Buffer) and then Valid_Enumeration_Vector_0000 (Buffer));

   function Valid_Enumeration_Vector (Buffer : Types.Bytes) return Boolean is
     (Valid_Enumeration_Vector_0000 (Buffer))
    with
     Pre => Is_Contained (Buffer);

   function Get_Enumeration_Vector_First (Buffer : Types.Bytes) return Types.Index_Type is
     ((if Valid_Enumeration_Vector_0000 (Buffer) then Get_Enumeration_Vector_0000_First (Buffer) else Unreachable_Types_Index_Type))
    with
     Pre => (Is_Contained (Buffer) and then Valid_Enumeration_Vector (Buffer));

   function Get_Enumeration_Vector_Last (Buffer : Types.Bytes) return Types.Index_Type is
     ((if Valid_Enumeration_Vector_0000 (Buffer) then Get_Enumeration_Vector_0000_Last (Buffer) else Unreachable_Types_Index_Type))
    with
     Pre => (Is_Contained (Buffer) and then Valid_Enumeration_Vector (Buffer));

   procedure Get_Enumeration_Vector (Buffer : Types.Bytes; First : out Types.Index_Type; Last : out Types.Index_Type) with
     Pre => (Is_Contained (Buffer) and then Valid_Enumeration_Vector (Buffer)),
     Post => ((First = Get_Enumeration_Vector_First (Buffer) and then Last = Get_Enumeration_Vector_Last (Buffer)) and then Enumeration_Vector.Is_Contained (Buffer (First .. Last)));

   function Valid_AV_Enumeration_Vector_00000 (Buffer : Types.Bytes) return Boolean is
     ((Valid_Enumeration_Vector_0000 (Buffer) and then (Buffer'Length >= (Types.Length_Type (Get_Length_0 (Buffer)) + 7) and then Buffer'First <= (Types.Index_Type'Last / 2))))
    with
     Pre => Is_Contained (Buffer);

   function Get_AV_Enumeration_Vector_00000_First (Buffer : Types.Bytes) return Types.Index_Type is
     ((Types.Length_Type (Get_Length_0 (Buffer)) + Buffer'First + 5))
    with
     Pre => (Is_Contained (Buffer) and then Valid_AV_Enumeration_Vector_00000 (Buffer));

   function Get_AV_Enumeration_Vector_00000_Last (Buffer : Types.Bytes) return Types.Index_Type is
     ((Types.Length_Type (Get_Length_0 (Buffer)) + Buffer'First + 6))
    with
     Pre => (Is_Contained (Buffer) and then Valid_AV_Enumeration_Vector_00000 (Buffer));

   function Valid_AV_Enumeration_Vector (Buffer : Types.Bytes) return Boolean is
     (Valid_AV_Enumeration_Vector_00000 (Buffer))
    with
     Pre => Is_Contained (Buffer);

   function Get_AV_Enumeration_Vector_First (Buffer : Types.Bytes) return Types.Index_Type is
     ((if Valid_AV_Enumeration_Vector_00000 (Buffer) then Get_AV_Enumeration_Vector_00000_First (Buffer) else Unreachable_Types_Index_Type))
    with
     Pre => (Is_Contained (Buffer) and then Valid_AV_Enumeration_Vector (Buffer));

   function Get_AV_Enumeration_Vector_Last (Buffer : Types.Bytes) return Types.Index_Type is
     ((if Valid_AV_Enumeration_Vector_00000 (Buffer) then Get_AV_Enumeration_Vector_00000_Last (Buffer) else Unreachable_Types_Index_Type))
    with
     Pre => (Is_Contained (Buffer) and then Valid_AV_Enumeration_Vector (Buffer));

   procedure Get_AV_Enumeration_Vector (Buffer : Types.Bytes; First : out Types.Index_Type; Last : out Types.Index_Type) with
     Pre => (Is_Contained (Buffer) and then Valid_AV_Enumeration_Vector (Buffer)),
     Post => ((First = Get_AV_Enumeration_Vector_First (Buffer) and then Last = Get_AV_Enumeration_Vector_Last (Buffer)) and then AV_Enumeration_Vector.Is_Contained (Buffer (First .. Last)));

   function Is_Valid (Buffer : Types.Bytes) return Boolean is
     (Valid_AV_Enumeration_Vector_00000 (Buffer))
    with
     Pre => Is_Contained (Buffer);

   function Message_Length (Buffer : Types.Bytes) return Types.Length_Type is
     ((if Valid_AV_Enumeration_Vector_00000 (Buffer) then (Types.Length_Type (Get_Length_0 (Buffer)) + 7) else Unreachable_Types_Length_Type))
    with
     Pre => (Is_Contained (Buffer) and then Is_Valid (Buffer));

end Arrays.Generic_Message;
