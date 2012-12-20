--------------------------------------
--Tar in en character som motsvarar siffrorna 1..9
--och byter koordinaterna X,Y till de nya koordinaterna
--------------------------------------

with ada.text_io;
use ada.text_io;
with ada.integer_text_io;
use ada.integer_text_io;

procedure move(X,Y: in out Integer;
	int: in integer) is
begin
   case int is
      when 1 =>
         X:= X-1;
         Y:= Y+1;

      when 2 =>
         Y:= Y+1;

      when 3 =>
         X:= X+1;
         Y:= Y+1;

      when 4 =>
         X:= X-1;

      when 6 =>
         X:= X+1;

      when 7 =>
         X:= X-1;
         Y:= Y-1;

      when 8 =>
         Y:= Y-1;

      when 9 =>
         X:= X+1;
         Y:= Y-1;

      when others =>
         null;

   end case;

end Move;



