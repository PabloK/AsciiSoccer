with Tja.Window.Elementary;
with Tja.Window.text;
with Tja.Window.Graphic;
use  Tja.Window.Elementary;
use  Tja.Window.text;
use  Tja.Window.Graphic;
with Typer;
use Typer;
with Ada.Text_Io;
use Ada.Text_Io;


procedure put_flaggor(Lagnr : in Natural;
                     X: in Natural) is

begin

   Goto_XY(X, 7);

   case Lagnr is
      when 1 =>



         Set_Background_Colour(Blue);
         Put(' ');
         Set_Background_Colour(yellow);
         Put(' ');
         Set_Background_Colour(Blue);
         Put("  ");

         Goto_XY(X, 8);

         Set_Background_Colour(Yellow);
         Put("    ");
         Goto_XY(X, 9);

         Set_Background_Colour(Blue);
         Put(' ');
         Set_Background_Colour(yellow);
         Put(' ');
         Set_Background_Colour(Blue);
         Put("  ");

      when 2 =>
         Set_Background_Colour(green);
         Put("  ");
         Set_Background_Colour(yellow);
         Put(' ');
         Set_Background_Colour(green);
         Put("  ");

         Goto_XY(X, 8);

         Set_Background_Colour(green);
         Put(' ');
          Set_Background_Colour(yellow);
          Put(' ');
          Set_Background_Colour(Blue);
          Put(' ');
          Set_Background_Colour(yellow);
          Put(' ');
          Set_Background_Colour(green);
          Put(' ');

         Goto_XY(X, 9);
          Set_Background_Colour(green);
         Put("  ");
         Set_Background_Colour(yellow);
         Put(' ');
         Set_Background_Colour(green);
         Put("  ");

      when 3 =>

         Set_Background_Colour(white);
         Put("  ");
         Set_Background_Colour(red);
         Put(" ");
         Set_Background_Colour(white);
         Put("   ");

         Goto_XY(X, 8);

         Set_Background_Colour(red);
         Put("      ");
         Goto_XY(X, 9);

         Set_Background_Colour(white);
         Put("  ");
         Set_Background_Colour(red);
         Put(" ");
         Set_Background_Colour(white);
         Put("   ");

      when 4 =>

         Set_Background_Colour(green);
         Put("  ");
         Set_Background_Colour(white);
         Put("  ");
         Set_Background_Colour(red);
         Put("  ");

         Goto_XY(X, 8);
          Set_Background_Colour(green);
         Put("  ");
         Set_Background_Colour(white);
         Put("  ");
         Set_Background_Colour(red);
         Put("  ");

         Goto_XY(X, 9);
          Set_Background_Colour(green);
         Put("  ");
         Set_Background_Colour(white);
         Put("  ");
         Set_Background_Colour(red);
         Put("  ");

          when 5 =>

         Set_Background_Colour(Red);
         Put(' ');
         Set_Background_Colour(White);
         Put(' ');
         Set_Background_Colour(red);
         Put("  ");

         Goto_XY(X, 8);

         Set_Background_Colour(White);
         Put("    ");
         Goto_XY(X, 9);

         Set_Background_Colour(red);
         Put(' ');
         Set_Background_Colour(white);
         Put(' ');
         Set_Background_Colour(red);
         Put("  ");

      when others =>
         null;
   end case;

end Put_Flaggor;

