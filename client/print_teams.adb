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
with Put_Flaggor;

procedure Print_teams(Lagnamn1, lagnamn2  : in String;
                      Lagnr1, Lagnr2      : in Natural;
                      L1, L2              : in integer) is

begin


   Put_Flaggor(Lagnr1,113);
   Put_Flaggor(Lagnr2,126);


      Goto_XY(113, 11);
      Set_Background_Colour(Black);
      Set_Foreground_Colour(Cyan);

      Put(Lagnamn1);

      Goto_XY(124, 11);
      Set_Background_Colour(Black);
      Set_Foreground_Colour(Cyan);
      Put('-');



      Goto_XY(126, 11);
      Set_Background_Colour(Black);
      Set_Foreground_Colour(Cyan);
      for I in 1..10 loop
         Put(Lagnamn2(I));
      end loop;



end Print_teams;

