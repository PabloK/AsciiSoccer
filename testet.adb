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



procedure Testet is

  subtype Spelplan_X is Integer range 1..20;

  subtype Spelplan_Y is Integer range 1..20;

  X,Y,BX,BY: Integer;
  BC,PC: Colour_Type;


begin

   X:=4;
   Y:=4;
   BX:=5;
   BY:=5;
   BC:=Blue;
   PC:=Red;
   Print_Field;

   Rita(X,Y,'s',BC,PC);
   Rita(BX,BY,'b',BC,PC);
   delay(1.0);
   Sudda(X,Y);
   Sudda(Bx,By);
   Spelarrorelse(X,Y,BX,BY,3);
   Rita(X,Y,'s',BC,PC);
   Rita(BX,BY,'b',BC,PC);
   delay(1.0);
 Sudda(X,Y);
   Sudda(Bx,By);
   Spelarrorelse(X,Y,BX,BY,1);

 Rita(X,Y,'s',BC,PC);
 Rita(BX,BY,'b',BC,PC);
 delay(1.0);
   Sudda(X,Y);
   Sudda(Bx,By);
   Spelarrorelse(X,Y,BX,BY,6);
   Rita(X,Y,'s',BC,PC);
   Rita(BX,BY,'b',BC,PC);
   delay(1.0);
   Sudda(X,Y);
   Sudda(Bx,By);
   Spelarrorelse(X,Y,BX,BY,6);
   Rita(X,Y,'s',BC,PC);
   Rita(BX,BY,'b',BC,PC);
   delay(1.0);
   Sudda(X,Y);
   Sudda(Bx,By);
   Spelarrorelse(X,Y,BX,BY,6);
   Rita(X,Y,'s',BC,PC);
   Rita(BX,BY,'b',BC,PC);
    delay(1.0);
   Sudda(X,Y);
   Sudda(Bx,By);
   Spelarrorelse(X,Y,BX,BY,6);
   Rita(X,Y,'s',BC,PC);
   Rita(BX,BY,'b',BC,PC);
end Testet;
