with ada.integer_text_IO;  use ada.integer_text_io;
with ada.text_io;          use ada.text_io;
with Tja.Window.Elementary;
with Tja.Window.text;
with Tja.Window.Graphic;
use  Tja.Window.Elementary;
use  Tja.Window.text;
use  Tja.Window.Graphic;
with Typer;
use Typer;

procedure Print_Handelse(Handelse : in Natural;
                         Goal     : in Natural) is

begin
   if Handelse = 1 then
      Goto_XY(117, 13);
      Set_Background_Colour(Black);
      Set_Foreground_Colour(Cyan);
      Put(Goal, Width =>0);
   else
      Goto_XY(131, 13);
      Set_Background_Colour(Black);
      Set_Foreground_Colour(Cyan);
      Put(Goal,Width => 0);
   end if;
end Print_Handelse;

