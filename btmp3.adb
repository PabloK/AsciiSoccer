with Ada.Text_IO;
use Ada.Text_IO;

package body Btmp3 is

   function System(Cmd:String) return Integer;
   pragma Import(C,System,"system");

   procedure Play(Mp3:String) is
      R:Integer;
   begin
      R:=System("/student/bin/mpg123 -Z " & Mp3 & " 2>/dev/null &");
   end Play;

   procedure Stop is
      R:Integer;
   begin
      R:=System("pkill -KILL mpg123 2>/dev/null");
   end Stop;

end Btmp3;
