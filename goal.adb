----------------------------
--funktionen goal kr�ver att det finns globala typer spelplan_X och _Y
--den tar in bollens koordinater i ordningen X,Y och returnerar 0 om det
--inte blev m�l, 1 om det blev m�l till v�nster och 2 om det blev m�l
--till h�ger. M�let �r skrivet till h�jden i (X-led/2) +-3.
----------------------------

with Typer; use Typer;
with ada.integer_text_IO;
use ada.integer_text_io;
with ada.text_io;
use ada.text_io;

Function Goal(X,Y: in Integer) return Integer is

   ----------------------------------
   --Yg1 och 2 �r gr�nserna f�r var m�len �r i y-koordinater.
   --Spelplan-y kan inte vara ett udda tal
   -----------------------------------
   Yg1: integer:=(Spelplan_Y'last/2)-3;
   Yg2: integer:=(Spelplan_Y'Last/2)+3;


   ---------------------------------
   --typen inneh�ller alla y-koordinater f�r m�let
   ----------------------------------
   subtype Ymal is Integer range Yg1..Yg2;

   --B anv�nds f�r att se om Y �r inom m�lets storlek
   B:Boolean;

begin

   ----------------------------------
   --Kollar om bollens koordinater �verensst�mmer med n�got av m�lens alla
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
