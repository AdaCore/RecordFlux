with Ada.Text_IO;

package body SPARK.Text_IO with
    SPARK_Mode => Off
is

    procedure Put (To : String) is
    begin
        Ada.Text_IO.Put (To);
    end Put;

    procedure Put_Line (To : String) is
    begin
        Ada.Text_IO.Put_Line (To);
    end Put_Line;

    procedure New_Line is
    begin
        Ada.Text_IO.New_Line;
    end New_Line;

end SPARK.Text_IO;
