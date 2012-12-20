
with TJa.Window.Graphic;
use Tja.Window.Graphic;
with TJa.Window.Elementary;
use Tja.Window.Elementary;
with TJa.Window.Text;
use Tja.Window.Text;
with ada.text_io;
use ada.text_io;


procedure Rita(X, Y: in Natural;
               Z: in Character;
               BC, PC: in Colour_Type) is


begin


   Goto_XY(X,Y);
   Set_Background_Colour(BC);
   Set_Foreground_Colour(PC);
   Put(Z);



end Rita;
