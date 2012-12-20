with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Command_Line;                  use Ada.Command_Line;

with ENa.Ansi.Picture;                  use ENa.Ansi.Picture;

procedure Test_Picture is

  Pic : array (1 .. Argument_Count) of Picture_Type;
  F   : File_Type;

begin
  for I in 1 .. Argument_Count loop
    Put_Line("Reading file """ & Argument(I) & """");
    Get(Argument(I), Pic(I));
    Put(Pic(I), I*2, I*2, Width => 1);

    Create(F, Out_File, Argument(I)(1 .. Argument(I)'Last - 4) & ".apa");
    Put(F, Pic(I));
    Reset(F, In_File);
    Get(F, Pic(I));
    Put(Pic(I), 50+I*2, 20+I*2, Width => 1);
    Close(F);
  end loop;
end Test_Picture;
