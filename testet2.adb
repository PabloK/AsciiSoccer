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



procedure Testet(Spelare, G_spelare: Planinfo_Type;
		 Antal_Spelare: Integer;
		 BC1,BC2,PC1,PC2: Colour_type) is
   

   BC: Colour_Type:= Green;
   PC: Colour_Type:= White;
   

begin
   
  
   Print_Field;
   

   Sudda(G_Spelare(0).X,G_Spelare(0).Y);

   Rita(Spealare(0).X, Spelare(0).Y, Spelare(0).Tecken,BC,PC);
   
   for I in 1..Antal_Spelare loop
      Sudda(G_Spelare(I).X,G_Spelare(I).Y);
      if Spelare(I).Lagnr = 1 then
	 Rita(Spelare(I).X, Spelare(I).Y, Spelare(I).Tecken,BC1,PC1);
      elsif Spelare(I).Lagnr = 2 then
	 Rita(Spelare(I).X, Spelare(I).Y, Spelare(I).Tecken,BC2,PC2);
      end if;
   end loop;

end Testet;

