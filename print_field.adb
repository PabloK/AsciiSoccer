with ada.integer_text_IO;
use ada.integer_text_io;
with ada.text_io;
use ada.text_io;
with Tja.Window.Elementary;
with Tja.Window.text;
with Tja.Window.Graphic;
use  Tja.Window.Elementary;
use  Tja.Window.text;
use  Tja.Window.Graphic;
with Typer;
use Typer;

procedure print_field is

begin

   clear_window;


----planen fylls
   Set_Background_Colour(red);
   Put_Line("                                                                                                    ");
   for I in 1..28 loop
      if not(I > 11 and I < 18) then
         Set_Background_Colour(red); Put(" ");

      else
         Set_Background_Colour(White); Put(" ");
      end if;

    Set_Background_Colour(green);
    Put("                                                                                                  ");

    if not(I > 11 and I < 18) then
        Set_Background_Colour(red); Put_line(" ");

    else
      Set_Background_Colour(White); Put_line(" ");
      end if;
   end loop;

   Set_Background_Colour(red);
   Put_Line("                                                                                                    ");


   for I in 1..20 loop
      Goto_XY(110, (I+4));
      if I = 1 or I = 20 then
         Set_Background_Colour(cyan);
         Put("                           ");
      else
        Set_Background_Colour(cyan);
        Put(" ");
        Set_Background_Colour(black);
        Put("                         ");
        Set_Background_Colour(cyan);
        Put(" ");
      end if;

   end loop;

      Goto_XY(117, 13);
      Set_Background_Colour(Black);
      Set_Foreground_Colour(Cyan);
      Put(0, Width =>0);
      Goto_XY(131, 13);
      Set_Background_Colour(Black);
      Set_Foreground_Colour(Cyan);
      Put(0,Width => 0);

end print_field;
