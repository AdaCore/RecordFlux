with Ada.Command_Line;
with Ada.Text_IO;
with SPARK.File_IO;
with RFLX.RFLX_Builtin_Types;
with RFLX.MiniPNG.File;
with RFLX.MiniPNG.CD_Block;
with RFLX.MiniPNG.CD_Blocks;

procedure Q1
is
   procedure Process_Comment (Data : RFLX.RFLX_Builtin_Types.Bytes)
   is
   begin
      Ada.Text_IO.Put_Line ("Comment:");
      for B of Data loop
         Ada.Text_IO.Put (Character'Val (RFLX.RFLX_Builtin_Types.Byte'Pos (B)));
      end loop;
      Ada.Text_IO.New_Line;
   end Process_Comment;
   procedure Get_Comment is new RFLX.MiniPNG.CD_Block.Get_Content (Process_Comment);
   File    : RFLX.RFLX_Builtin_Types.Bytes_Ptr;
   Context : RFLX.MiniPNG.File.Context      := RFLX.MiniPNG.File.Create;
   Blocks  : RFLX.MiniPNG.CD_Blocks.Context := RFLX.MiniPNG.CD_Blocks.Create;
   Block   : RFLX.MiniPNG.CD_Block.Context  := RFLX.MiniPNG.CD_Block.Create;
   Height  : RFLX.MiniPNG.Image_Size;
   Width   : RFLX.MiniPNG.Image_Size;
   Pixel   : RFLX.MiniPNG.Pixel_Type;
begin
   if Ada.Command_Line.Argument_Count /= 1 then
      Ada.Text_IO.Put_Line ("Usage: "
                            & Ada.Command_Line.Command_Name
                            & " FILE");
      Ada.Command_Line.Set_Exit_Status (1);
      return;
   end if;
   File := SPARK.File_IO.Read_File_Ptr (Ada.Command_Line.Argument (1));
   RFLX.MiniPNG.File.Initialize (Context, File);
   RFLX.MiniPNG.File.Verify_Message (Context);
   if not RFLX.MiniPNG.File.Structural_Valid_Message (Context) then
      Ada.Text_IO.Put_Line ("Invalid file.");
      Ada.Command_Line.Set_Exit_Status (1);
      return;
   end if;
   Height := RFLX.MiniPNG.File.Get_Image_Height (Context);
   Width  := RFLX.MiniPNG.File.Get_Image_Width (Context);
   Pixel  := RFLX.MiniPNG.File.Get_Pixel (Context);
   Ada.Text_IO.Put_Line ("Width: " & Width'Img);
   Ada.Text_IO.Put_Line ("Height: " & Height'Img);
   Ada.Text_IO.Put_Line ("Pixel type: " & Pixel'Img);
   if not RFLX.MiniPNG.File.Present (Context, RFLX.MiniPNG.File.F_Blocks) then
      return;
   end if;
   RFLX.MiniPNG.File.Switch_To_Blocks (Context, Blocks);
   while RFLX.MiniPNG.CD_Blocks.Valid_Element (Blocks) loop
      RFLX.MiniPNG.CD_Blocks.Switch (Blocks, Block);
      RFLX.MiniPNG.CD_Block.Verify_Message (Block);
      if RFLX.MiniPNG.CD_Block.Structural_Valid_Message (Block) then
         case RFLX.MiniPNG.CD_Block.Get_Tag (Block) is
            when RFLX.MiniPNG.Comment =>
               Get_Comment (Block);
            when RFLX.MiniPNG.Data =>
               null;
         end case;
      else
         Ada.Text_IO.Put_Line ("Invalid block.");
      end if;
      RFLX.MiniPNG.CD_Blocks.Update (Blocks, Block);
   end loop;
end Q1;
