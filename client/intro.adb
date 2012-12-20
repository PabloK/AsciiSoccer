with Tja.Keyboard, Tja.Window.Elementary, Tja.Window.Text, Ada.Text_Io, Ada.Integer_Text_IO;
use Tja.Keyboard, Tja.Window.Elementary, Tja.Window.Text, Ada.Text_Io, Ada.Integer_Text_IO;

procedure Intro is

   type Img_Type is array(1..23) of String (1..30);

   Img : Img_Type;

   procedure Animate(X,Y          : in Integer;
                     Shadow       : in Boolean;
                     Color,Scolor : in Color_Type;
                     Img          : in Img_Type)is

      procedure Put_s(Str : in String;
                      Background,Text : in Color_type)is

      begin

         for I in Str'Range loop

            if Str(I) /= ' ' then

               Set_Background_Colour(Text);
               Put(' ');

            else

               Set_Background_Colour(Background);
               Put(' ');

            end if;

         end loop;


      end Put_s;

   begin

      for I in  1..Img'Last loop

         for J in 1..I loop
            if Shadow  then
               Goto_XY(X+3,Y+Img'Last-2-I+J);
               Set_Foreground_colour(SColor);
                Put_s(Img(J),Black,SColor);
            end if;
         Goto_XY(X,Y+Img'Last-I+J);
         Set_Foreground_Colour(Color);
         Put_s(Img(J),Black,Color);
         end loop;
         delay(0.04);
      end loop;

   end Animate;

begin


Img := ("  AAAA    AAAA   AAAA  AA   AA",
        " AAAAAA  AA  AA AA  AA AAA AAA",
        "AAA  AAA AA     AA  AA  AA AA ",
        "AA    AA AA     AA  AA        ",
        "AA    AA AAAAA  AA      AA  AA",
        "AA    AA  AAAAA AA  AA AAA AAA",
        "AAAAAAAA     AA AA  AA AAA AAA",
        "AAAAAAAA     AA AA  AA AAA AAA",
        "AAA  AAA AA  AA AA  AA AAA AAA",
        "AA    AA  AAAA   AAAA  AAA AAA",
        "                              ",
        "  XXXX  XXX   XXX  XXX        ",
        " XX    XX XX XX XX   XX       ",
        "XX    XX  XXXX  XX  XXXXX     ",
        "XXXXX XX  XXXX  XX   XX       ",
        "XX     XX XX XX XX   XX       ",
        "XX      XXX   XXX     XXX     ",
        "XX  BBB    BBB   BBB  BBB     ",
        "XX  B  B  BB  B   BB   BB     ",
        "X   BBB   BBBBB   BB   BB     ",
        "    B  B  BB  B   BB   BB B  B",
        "    BBB           BB   BBB  BB",
        "                 BBBBBBBBBBBB ");

Set_Default_Colours(White,Black);
Reset_Colours;
Clear_Window;
Animate(65,10,True,Cyan,Blue,Img);
New_Line;

delay(0.5);



end Intro;
