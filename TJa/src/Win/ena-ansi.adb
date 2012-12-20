--*-------------------------------------------------------------------------*--
--*                                                                         *--
--*                        ERIK NILSSON ADA LIBRARY                         *--
--*                                                                         *--
--*                            E N A . A N S I                              *--
--*                                                                         *--
--*                                B o d y                                  *--
--*                                                                         *--
--*                       Erik A. Nilsson, 2009-02-05                       *--
--*                                                                         *--
--*  See specification for details.                                         *--
--*-------------------------------------------------------------------------*--

package body Ena.Ansi is

   procedure Csi(Item : in    String)  is
   begin
      Put(Ascii.Esc);
      Put("[");
      Put(Item);
   end Csi;

   -------------------------------------------------------------------------

   function RGB_To_Ansi(Pixel : in RGB) return string is



         function To_Hex(N : in Integer) return Character is
            S : String(1..16) := "0123456789ABCDEF";
         begin
            return S(N);
         end To_Hex;


         Result   : String(1..2) := "00"; -- A4 means light green foreground on dark blue background.
         Dist     : Natural;
         Min_Dist : Natural := Pixel(1)**2 + Pixel(2)**2 + Pixel(3)**2;
         Mixed_RGB: RGB;

      begin

         for J in Basic_Colors_Rgb'Range loop
            for I in Basic_Colors_Rgb'Range loop
               Mixed_RGB := ((Basic_Colors_Rgb(I)(1) + Basic_Colors_Rgb(J)(1))/2,   -- mixes two basic colors
                             (Basic_Colors_Rgb(I)(2) + Basic_Colors_Rgb(J)(2))/2,   -- for a new color
                             (Basic_Colors_Rgb(I)(3) + Basic_Colors_Rgb(J)(3))/2);  -- as an RGB-value.

               Mixed_RGB := (Mixed_RGB(1) - Pixel(3),  -- The vector from the pixel
                             Mixed_RGB(2) - Pixel(2),  -- and the mixed color.
                             Mixed_RGB(3) - Pixel(1));

               Dist :=  Mixed_RGB(1)**2 + Mixed_RGB(2)**2 + Mixed_RGB(3)**2;
               -- the distance between the new color and
               -- the pixel;

               if Dist < Min_Dist then
                  Min_Dist := Dist;
                  Result := (To_Hex(I), To_Hex(J));
               end if;

            end loop;
         end loop;

         return Result;
      end RGB_To_Ansi;
      -------------------------------------------------------------------------


end Ena.Ansi;
