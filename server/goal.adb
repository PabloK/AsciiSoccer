----------------------------
--funktionen goal kräver att det finns globala typer spelplan_X och _Y
--den tar in bollens koordinater i ordningen X,Y och returnerar 0 om det
--inte blev mål, 1 om det blev mål till vänster och 2 om det blev mål
--till höger. Målet är skrivet till höjden i (X-led/2) +-3.
----------------------------

with Typer; use Typer;
with ada.integer_text_IO;
use ada.integer_text_io;
with ada.text_io;
use ada.text_io;

Function Goal(X,Y: in Integer) return Integer is

   ----------------------------------
   --Yg1 och 2 är gränserna för var målen är i y-koordinater.
   --Spelplan-y kan inte vara ett udda tal
   -----------------------------------
   Yg1: integer:=(Spelplan_Y'last/2)-3;
   Yg2: integer:=(Spelplan_Y'Last/2)+3;


   ---------------------------------
   --typen innehåller alla y-koordinater för målet
   ----------------------------------
   subtype Ymal is Integer range Yg1..Yg2;

   --B används för att se om Y är inom målets storlek
   B:Boolean;

begin

   ----------------------------------
   --Kollar om bollens koordinater överensstämmer med något av målens alla
   --koordiinater och returnerar en integer
   ------------------------------
   B:= Y in Ymal'Range;

   if B and X =1 then
      return 2;
   elsif B and X= Spelplan_X'Last then
      return 1;
   end if;

return 0;


end Goal;
