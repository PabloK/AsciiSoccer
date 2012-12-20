with Ada.Text_IO;                       use Ada.Text_IO;

with TJa.Window.Elementary;             use TJa.Window.Elementary;
with TJa.Window.Text;                   use TJa.Window.Text;
with TJa.Window.Graphic;                use TJa.Window.Graphic;

procedure Test_TJa is

  Message : constant String := " Hello world ";
  X_Start : constant Integer := 10;
  Y_Start : constant Integer := 2;

begin
  Reset_Colours;  -- Standard colours is supposed to be black on white ...
  Clear_Window;

  -- Draw a rectangle on screen ...
  Set_Graphical_Mode(On);

  Goto_XY(X_Start, Y_Start);
  Put(Upper_Left_Corner);
  Put(Horisontal_Line, Times => Message'Length);
  Put(Upper_Right_Corner);

  Goto_XY(X_Start, Y_Start + 1);
  Put(Vertical_Line);
  Goto_XY(X_Start + Message'Length + 1, Y_Start + 1);
  Put(Vertical_Line);

  Goto_XY(X_Start, Y_Start + 2);
  Put(Lower_Left_Corner);
  Put(Horisontal_Line, Times => Message'Length);
  Put(Lower_Right_Corner);

  Set_Graphical_Mode(Off);

  -- Prints the message inside the rectangle ...
  Goto_XY(X_Start + 1, Y_Start + 1);
  Set_Background_Colour(Blue);
  Set_Foreground_Colour(White);
  Set_Bold_Mode(On);
  Put(Message);
  Reset_Colours;
  Reset_Text_Modes;  -- Resets boold mode ...

  Goto_XY(1, Y_Start + 4);
end Test_TJa;
