package body Meny is

   -----------------------------------------------------------
   -- Returns the string_length empty chars in the end removed
   -----------------------------------------------------------
   function String_Length(Str : in String)return natural is

      Spaces : Natural := 0;

   begin

      for I in reverse Str'Range loop


         if Str(I) /= ' ' then
            return Str'Last - Spaces;
         end if;

         Spaces := Spaces + 1;

      end loop;

      return 1;

   end String_Length;


   ------------------------------------
   -- Draws and handels selected box
   ------------------------------------
   function Select_Box(Box    : in  Select_Type;
                       Colour : in  Colour_Type )return Integer is

   Key : Key_Type;
   Selected : Integer := 1;

   begin

   Set_Default_Colours(Cyan,Black);
   Goto_XY(Box.X,Box.Y);
   Set_Background_Colour(Colour);
   Put(Box.Caption(1..String_Length(Box.Caption)));
   Reset_Colours;
   Goto_XY(Box.X+Box.Caption'Length,Box.Y);
   Put (Box.List(Selected));

   loop

         Get_Immediate(Key);
         if To_String(Key) = "a" then

            if Selected > 1 then
               Selected := Selected - 1;
            end if;

            Goto_XY(Box.X+Box.Caption'Length,Box.Y);
            Put (Box.List(Selected));

         elsif To_String(Key) = "d" then

            if Selected < Box.List'Last then
               Selected := Selected + 1;
            end if;

            Goto_XY(Box.X+Box.Caption'Length,Box.Y);
            Put (Box.List(Selected));

         elsif Is_Return(Key) then

            exit;

         end if;

   end loop;
   Goto_XY(Box.X,Box.Y);
   Put(Box.Caption(1..String_Length(Box.Caption)));
   return Selected;

end Select_box;
------------------------------------------------
function Init_Meny(Meny           : in Meny_Type;
                   Colour,Scolour : in Colour_Type;
                   End_Message    : in string) return Meny_Result_Type Is

   Meny_res        : Meny_Result_Type := (others => 1);
   Key             : Key_Type;
   J               : Integer := 1;
   First_Time      : Boolean := True;

begin

   ---------------------------------------------
   -- Draws Menue
   ---------------------------------------------

   for I in Meny_Type'Range loop

   Goto_XY(Meny(I).X,Meny(I).Y);
   Put(Meny(I).Caption);
   Goto_XY(Meny(I).X + Meny(I).Caption'Length,Meny(I).Y);
   Put (Meny(I).List(1));

   end loop;
   Goto_XY(Meny(Meny'Last).X,Meny(Meny'Last).Y+2);
   Put(End_Message);
   ---------------------------------------------
   -- Selector
   ---------------------------------------------
   loop

      Set_Default_Colours(Cyan,Black);

         if To_String(Key) = "w" or First_Time Then

            if J <= Meny'Last then
            Reset_Colours;
            Goto_XY(Meny(J).X,Meny(J).Y);
            Put(Meny(J).Caption);
            Goto_XY(Meny(Meny'Last).X,Meny(Meny'Last).Y+2);
            Put(End_Message);
            else
            Reset_Colours;
            Goto_XY(Meny(J-1).X,Meny(J-1).Y);
            Put(Meny(J-1).Caption);
            Goto_XY(Meny(Meny'Last).X,Meny(Meny'Last).Y+2);
            Put(End_Message);
            end if;

            if J > 1 then
               J := J - 1;
            end if;

            Goto_XY(Meny(J).X,Meny(J).Y);
            Set_Background_Colour(Colour);
            Put(Meny(J).Caption(1..String_Length(Meny(J).Caption)));
            Reset_Colours;
            First_Time := False;

         elsif To_String(Key) = "s" then

            if J = Meny_Res'Last then

               Reset_Colours;
               Goto_XY(Meny(J).X,Meny(J).Y);
               Put(Meny(J).Caption(1..String_Length(Meny(J).Caption)));
               Goto_XY(Meny(Meny'Last).X,Meny(Meny'Last).Y+2);
               Set_Background_colour(Colour);
               Put(End_Message);
               Reset_Colours;
               J := J + 1;

            elsif J < Meny'Last then

            Reset_Colours;
            Goto_XY(Meny(J).X,Meny(J).Y);
            Put(Meny(J).Caption(1..String_Length(Meny(J).Caption)));
            Goto_XY(Meny(Meny'Last).X,Meny(Meny'Last).Y+2);
            Put(End_Message);

            if J < Meny'Last then
               J := J + 1;
            end if;

            Goto_XY(Meny(J).X,Meny(J).Y);
            Set_Background_Colour(Colour);
            Put(Meny(J).Caption(1..String_Length(Meny(J).Caption)));
            Reset_Colours;
            end if;



         elsif Is_Return(Key) or To_String(Key) = "d" then
            if J > Meny'Last and not(Is_Right_Arrow(Key)) then

               exit;

            elsif J <= Meny'Last then

               Meny_Res(J) := Select_Box(Meny(J),Scolour);
               Goto_XY(Meny(J).X,Meny(J).Y);
               Set_Background_Colour(Colour);
               Put(Meny(J).Caption(1..String_Length(Meny(J).Caption)));
               Reset_Colours;

            end if;


         end if;

         Get_Immediate(Key);

   end loop;

   return Meny_res;

end Init_Meny;

end Meny;
