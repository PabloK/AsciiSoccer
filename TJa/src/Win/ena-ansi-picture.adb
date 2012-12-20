--*-------------------------------------------------------------------------*--
--*                                                                         *--
--*                        ERIK NILSSON ADA LIBRARY                         *--
--*                                                                         *--
--*                    E N A . A N S I _ P I C T U R E                      *--
--*                                                                         *--
--*                                B o d y                                  *--
--*                                                                         *--
--*                       Erik A. Nilsson, 2009-02-05                       *--
--*                                                                         *--
--*  See specification for details.                                         *--
--*-------------------------------------------------------------------------*--



package body Ena.Ansi.Picture is
   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

   procedure Get (Filename : in     String;
                  Item     :    out Picture_type) is




      package Char_IO is
         new Ada.Sequential_Io(Character);

      use Char_IO;

      F         : Char_IO.File_Type;
      Outfile   : Ada.Text_IO.File_Type;
      char      : Character;
      rows      : Natural := 0;
      Pixel     : Rgb;
      Color     : String(1..2);
      Prev_Color: String(1..2) := "XX";

   begin
      Open(F, In_File, filename);


     for I in 1 .. 15 loop
        read(F, Char);  -- First part of header ...
     end loop;

     Item.Xdim := 0;
     for I in 1 .. 4 loop
        read(F, Char);
        Item.xdim := 256*Item.xdim + Character'Pos(Char);
     end loop;

     Item.Ydim := 0;
     for I in 1 .. 4 loop
        read(F, Char);
        Item.ydim := 256*Item.ydim + Character'Pos(Char);
     end loop;
     for I in 24 .. 54 loop
        read(F, Char);  -- Last part of header ...
     end loop;


     for Y in 1..Item.ydim loop
        for X in 1..Item.xdim loop

           for Col in 1..3 loop
              read(F, Char);
              pixel(Col) := Character'pos(char); -- pixel recieves rgb values
           end loop;
           Color := Rgb_To_Ansi(Pixel);

           if Color = Prev_Color then
              Item.Data := Item.Data & "--";
           else
              Item.data := Item.data & Color;
              Prev_Color := Color;
           end if;


        end loop;
        for I in 1 .. (4 - ((Item.Xdim * 3) mod 4)) mod 4 loop
           read(F, Char);

        end loop;

     end loop;
     Close(F);
   end Get;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------


   procedure Put (Outfile  : in     Ada.Text_IO.File_type;
                  Item     : in     Picture_Type) is

   begin
      Put_line(Outfile, "apa1");
      put(OutFile, Item.xdim);
      put(OutFile, Item.ydim);
      New_Line(Outfile);
      Put_Line(Outfile, Item.Data);
   end put;

  -----------------------------------------------------------------------------
   ----------------------------------------------------------------------------

     procedure Get (File : in     Ada.Text_IO.File_type;
                    Item :    out Picture_type) is


     begin
        Skip_Line(File);
        Get(File, Item.Xdim);
        Get(File, Item.Ydim);
        Skip_Line(File);
        Item.Data := Get_Line(File);

     end Get;

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- Note : The data in the .apa files is stored NOT from the pictures upper
-- lefthand corner to the lower righthand corner. Rather it is stored from the
-- lower lefthand corner to the upper righthand corner. This means that it is
-- stored as an upp-down flipped image. This is why Put draws the image from
-- Top to bottom. After the picture is drawn, put returns cursor to the lower
-- righthand corner.

     procedure Put(Item     : in     picture_type;
                   X,Y      : in     Natural := 1;
                   width    : in     Natural := 2) is

        procedure Csi(Item : in    String)  is
        begin
           Put(Ascii.Esc);
           Put("[");
           Put(Item);
        end Csi;

        procedure Change_Colors(Fore, Back : in Integer) is

           Tmp_F : Integer := Fore;
           Tmp_B : Integer := Back;

        begin

           if Tmp_F < 8 then
              Tmp_F := 30 + Tmp_F;
           else
              Tmp_F := 90 + Tmp_F - 8;
           end if;

           if Tmp_B < 8 then
              Tmp_B := 40 + Tmp_B;
           else
              Tmp_B := 100 + Tmp_B - 8;
           end if;

           Csi(Integer'Image(Tmp_F)(2..Integer'Image(Tmp_F)'Last) & ";" &
               Integer'Image(Tmp_B)(2..Integer'Image(Tmp_B)'Last) & "m");

        end Change_Colors;


        function To_Dec(Hex : in Character) return integer is
           To_dec_Error : exception;
        begin
           if Hex in '0'..'9' then
              return Character'Pos(Hex) - 48;
           elsif Hex in 'A'..'F' then
              return Character'Pos(Hex) - 55;
           else
              raise To_dec_Error;
           end if;
        end To_Dec;


        procedure Graphical_Mode(On : in    Boolean) is
        begin
           if On then
              Put(Ascii.ESC);
              Put("(0");
              Put(Ascii.ESC);
              Put(")0");
           else -- off
              Put(Ascii.ESC);
              Put("(B");
              Put(Ascii.ESC);
              Put(")B");
           end if;
           Flush;
        end Graphical_Mode;

        Fore : Character;
        Back : Character;
        Row  : Natural := 1;

     begin
        Graphical_Mode(True);

        Csi(Integer'Image(Y + Item.Ydim)(2..Integer'Image(Y + Item.Ydim)'last)
            & ";" &     -- go to lower left corner of picture
            Integer'Image(X)(2..Integer'Image(X)'Last) & "H");

        for I in  1..Item.Xdim*Item.Ydim loop

           fore := Element(Item.Data, 2*I-1);
           Back := Element(Item.Data, 2*I);

           if Fore /= '-' then
              Change_colors(To_Dec(Fore), To_Dec(Back));
           end if;
           for I in 1..Width loop
              Put('a');
           end loop;

           if ((I) mod Item.Xdim) = 0 then -- jump to line above
                Csi(Integer'Image(Y + Item.Ydim - Row)(2..Integer'Image(Y + Item.Ydim - Row)'last)
                    & ";" &
                    Integer'Image(X)(2..Integer'Image(X)'Last) & "H");

                Row := Row + 1;
           end if;
        end loop;
        Graphical_Mode(False);
        Csi("97m");

        Csi(Integer'Image(Y + Item.Ydim)(2..Integer'Image(Y + Item.Ydim)'last)
            & ";" &     -- go to lower right corner of picture
            Integer'Image(X+Item.Xdim+1)(2..Integer'Image(X+item.Xdim+1)'Last) & "H");
     end Put;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------


end Ena.Ansi.Picture;

