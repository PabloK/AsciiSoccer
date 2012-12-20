with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_Io; use Ada.Integer_Text_Io;

procedure Tecken is

   Ch: Character := Character'First;
begin
   for I in Character'Range loop

      Put(Ch);
      Ch := Character'Succ(Ch);
      Put("    ");
      Put(I);
      New_Line;
   end loop;


end Tecken;

