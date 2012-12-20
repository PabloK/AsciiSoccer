with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Strings.Fixed;   use Ada.Strings.Fixed;
with Ada.Strings;         use Ada.Strings;

package Ena.Ansi.Picture.Capa is

   procedure Compress(Namn : in String);
   procedure Decompress(Namn : in String);

end Ena.Ansi.Picture.Capa;
