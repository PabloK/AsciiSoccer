with Ada.Text_Io; use Ada.Text_Io;
with Ada.Integer_Text_IO;
with Spelarrorelse;
with Tja.Window.Elementary;
with Tja.Window.text;
with Tja.Window.Graphic;
use Ada.Integer_Text_Io,  Tja.Window.Elementary,  Tja.Window.text;
use  Tja.Window.Graphic;
with Rita;
with Print_Field;
with Sudda;
with Typer;
use Typer;



procedure K_Ritare(Spelare, G_spelare: Planinfo_Type;
                 Antal_Spelare: natural;
                 BC1,BC2,PC1,PC2: Colour_type) is


   BC: Colour_Type:= Green;
   PC: Colour_Type:= White;
   J: Integer;

begin

   Sudda(G_Spelare(0).X,G_Spelare(0).Y);

   Rita(Spelare(0).X, Spelare(0).Y, Spelare(0).Tecken,BC,PC);

   for I in 1..(Antal_Spelare/2) loop

         J:=I*2;
         Sudda(G_Spelare(J-1).X,G_Spelare(J-1).Y);
         Rita(Spelare(J-1).X, Spelare(J-1).Y, Spelare(J-1).Tecken,BC1,PC1);
         Sudda(G_Spelare(J).X,G_Spelare(J).Y);
         Rita(Spelare(J).X, Spelare(J).Y, Spelare(J).Tecken,BC2,PC2);

   end loop;

end K_Ritare;

