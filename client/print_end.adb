with ada.integer_text_IO;
use ada.integer_text_io;
with ada.text_io;
use ada.text_io;
with Tja.Window.Elementary;
use  Tja.Window.Elementary;
with Tja.Window.text;
with Tja.Window.Graphic;
use  Tja.Window.text;
use  Tja.Window.Graphic;
with Typer;
use Typer;



procedure Print_End(H       : in Natural;
                    Lag     : in Planinfo_Type;
                    Lagnamn : in Lagnamnslista_Typ;
                    AS      : in Natural;
                    G1,G2   : in Natural;
                    Lag1, Lag2: in Natural) is
  begin
  clear_window;
  Set_Background_Colour(Cyan);

  for I in 1..50 loop
     Goto_XY(4, (I));
     Put(" ");
  end loop;

  for I in 1..100 loop
     Goto_XY(I, 4);
     Put(" ");
  end loop;

  for I in 1..50 loop
     Goto_XY(100, (I));
     Put(" ");
  end loop;

  for I in 1..100 loop
     Goto_XY(I, 50);
     Put(" ");
  end loop;


   GoTo_XY(10,10);
   Set_Background_Colour(Black);
   Set_Foreground_Colour(cyan);

   Put("Matchen Är Över");
   GoTo_XY(10,12);
   Put("Vinnare är: ");

   if H=1 then
      for I in 1..20 loop
         if ((Lagnamn(Lag1).Namn(I)) = ' ') then
            null;
         else
            Put(Lagnamn(Lag1).Namn(I));
         end if;
      end loop;

      Put(" med ");
      Put((G1 + 1), Width =>1);
      Put(" mål");
   else
      for I in 1..20 loop
         if ((Lagnamn(Lag2).Namn(I)) = ' ') then
            null;
         else
            Put(Lagnamn(Lag2).Namn(I));
         end if;
      end loop;

      Put(" med ");
      Put((G2 + 1), Width =>1);
      Put(" mål");
   end if;
      GoTo_XY(10,14);
   Put("Och Looosers är: ");

   if H=1 then
      for I in 1..20 loop
         if ((Lagnamn(Lag2).Namn(I)) = ' ') then
            Put(Lagnamn(Lag2).Namn(I));
         end if;
      end loop;

      Put(" med ");
      Put((G2), Width =>1);
      Put(" mål");
   else
      for I in 1..20 loop
         if ((Lagnamn(Lag1).Namn(I)) = ' ') then
            null;
         else
            Put(Lagnamn(Lag1).Namn(I));
         end if;
      end loop;

      Put(" med ");
      Put((G1), Width =>1);
      Put(" mål");
   end if;


  end Print_End;

