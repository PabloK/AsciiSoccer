with Tja.Keyboard, Tja.Keyboard.keys, Tja.Window.Elementary, Tja.Window.graphic;use Tja.Keyboard, Tja.Keyboard.keys, Tja.Window.Elementary, Tja.Window.graphic;
with Ada.Text_IO;
use Ada.Text_IO;
with Tja.Window.Text;
use Tja.Window.Text;

procedure Ovning is

   subtype Spelplan is Integer Range 1..52;

   procedure Sudda(X,Y : in Natural) is



   begin

      Goto_XY(X,Y);
      Put(" ");

   end sudda;

   procedure move (XC,YC : in Integer;
                     X,Y : in out Spelplan) is
      D : Integer;
   begin


      Sudda(X,Y);
      D := X+XC;
      if D in Spelplan'First+1..Spelplan'Last-1  then
         X:= X+XC;
      end if;
      D := Y+YC;

      if D in  Spelplan'First+1..Spelplan'Last-1  then
         Y:= Y+YC;
      end if;
      Goto_XY(X,Y);
      Put('X');

   end Move;


   Key_Pressed : Key_Type;
   Key_B : Boolean := False;
   X,Y : Spelplan := 20;


begin
 Set_Default_Colours(White,Green);
Reset_Colours;
    Set_Echo_Mode(Off);
   Put(Ascii.Esc & "[?25l");
  Clear_Window;
   for I in Spelplan'Range loop
      Goto_XY(I,1);
      Put('-');
      Goto_XY(I,Spelplan'Last);
      Put('-');
      Goto_XY(1,I);
      Put('|');
       Goto_XY(Spelplan'Last,I);
       Put('|');
   end loop;



   Set_Graphical_Mode(On);
 Set_Default_Colours(White,Green);
Reset_Colours;
   Goto_XY(X,Y);

   loop
      begin
      Get_Immediate(Key_Pressed,Key_B);

      if Key_B and Is_Character(Key_pressed) then

         case To_Character(Key_Pressed) is

            when '1' =>

               Move(-1,1,X,Y);

            when '2' =>

               Move(0,1,X,Y);

            when '3' =>

              Move(1,1,X,Y);

            when '4' =>

               Move(-1,0,X,Y);

            when '6' =>

               Move(1,0,X,Y);

            when '7' =>

               Move(-1,-1,X,Y);

            when '8' =>

               Move(0,-1,X,Y);

            when '9' =>

              Move(1,-1,X,Y);

            when 'x' =>
               exit;

            when others =>

               null;

         end case;

      end if;

   exception

   when others =>
      null;
   end;
   end loop;
   Put(Ascii.Esc & "[?25h");
   Set_Graphical_Mode(Off);
   Set_Default_Colours(Black,White);
     Reset_Colours;

end Ovning;
