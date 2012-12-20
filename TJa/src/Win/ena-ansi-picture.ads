--*-------------------------------------------------------------------------*--
--*                                                                         *--
--*                        ERIK NILSSON ADA LIBRARY                         *--
--*                                                                         *--
--*                    E N A . A N S I _ P I C T U R E                      *--
--*                                                                         *--
--*                                S p e c                                  *--
--*                                                                         *--
--*                       Erik A. Nilsson, 2009-02-05                       *--
--*                                                                         *--
--* This package was inteded for personal use only. There may be some flaws *--
--* in the code, usage is at own risk. There is no guarrantee that the      *--
--* contents will work properly with your terminal. Most terminals seem not *--
--* to support the 16 ANSI colors, most only support 8. Package was tested  *--
--* with "gnome-terminal", a variant dtterm found in Java Desktop for       *--
--* Solaris.                                                                *--
--*                                                                         *--
--*-------------------------------------------------------------------------*--
--*                                                                         *--
--* Description:                                                            *--
--*                                                                         *--
--* The main purpose of the package is to draw so called "ANSIart" pictures *--
--* in the shell window. There is a new data type for storing such pictures *--
--* and modules for writing them to disk and to the screen. This package    *--
--* permits bitmat images (BMP) to be converted to this new format. The 16  *--
--* ANSI colors mentioned above can be mixed, using a special graphical     *--
--* character, to get a full set of 256 colors. There are many duplicates   *--
--* however and the actual color depth may be lower.                        *--
--*                                                                         *--
--*-------------------------------------------------------------------------*--

-- Ada standard libraries.
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;     use Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Integer_Text_IO;               use Ada.Integer_Text_IO;
with Ada.Sequential_IO;

-------------------------------------------------------------------------------
package Ena.Ansi.Picture is

   --*-------------------------------------------------------------------------
   --* "Picture_Type" is the structure in which the pictures are represented.
   --* It contains information about the dimensions of the picture and the data
   --* itself. The data is stored dynamically, so no constraints need to be set
   --* when declaring a "Picture_Type"
   --*-------------------------------------------------------------------------

   type Picture_Type is private;

   --*-------------------------------------------------------------------------
   --* The following procedure parses a BMP file and stores it as a
   --* Picture_Type. NOTE: The adaption from RGB-valued pixels to the 256 ANSI
   --* colors is a brute force calculation, hence may take some time.
   --* I found that converting a 120 x 120 pixel BMP-file took about 6 seconds.
   --* Once a picture has been converted, however, it can be stored as an
   --* ANSI-Picture-Adaptation, see below.
   --*-------------------------------------------------------------------------

   procedure Get (Filename : in     String;
                  Item     :    out Picture_type);

   --*-------------------------------------------------------------------------
   --* The following procedure writes a Picture_Type to disk as an
   --* ANSI-Picture-Adaptation file (APA-file). The procedure requires an
   --* Ada.Text_IO.File_Type properly prepared. See the code examples at the
   --* end of this specification.
   --*-------------------------------------------------------------------------

   procedure Put (Outfile  : in     Ada.Text_IO.File_type;
                  Item     : in     Picture_Type);

   --*-------------------------------------------------------------------------
   --* The following procedure reads a Picture_Type from an APA-file. This get
   --* Is considerably faster than its BMP counterpart above since the colors
   --* are already converted. This procedure also requires an
   --* Ada.Text_IO.File_type properly prepared for use.
   --*-------------------------------------------------------------------------

   procedure Get (File     : in     Ada.Text_IO.File_type;
                  Item     :    out Picture_type);

   --*-------------------------------------------------------------------------
   --* The following procedure writes a Picture_Type to the screen. The X and Y
   --* parameters determine the location on the screen where (1, 1) is at the
   --* upper left corner of the terminal window. The "Width" parameter sets
   --* the number of spaces to be used for every pixel in the picture. Since
   --* every space in the terminal is almost twice as high as it is wide, the
   --* width 2 usually gives the best ratio.
   --*-------------------------------------------------------------------------

   procedure Put(Item     : in     picture_type;
                 X,Y      : in     Natural := 1;
                 width    : in     Natural := 2);

private

   type Picture_Type is
      record
         Xdim : Natural;
         Ydim : Natural;
         Data : Unbounded_String;
      end record;

end Ena.Ansi.Picture;

--*-------------------------------------------------------------------------*--
--*                                                                         *--
--*                               Examples                                  *--
--*                                                                         *--
--* Both examples uses Ada.Text_IO.
--* The following example illustrates converting a BMP-file to an APA-file.
--
--    procedure Test_Ena_Ansi_Picture is
--
--       Temp : Picture_Type;
--       F    : Ada.Text_IO.File_Type;
--
--    begin
--       Get("nejdetskadu.bmp", Temp);
--       Ada.Text_IO.Create(F, Out_File, "nejdetskadu.apa");
--       Put(F, Temp);
--       Ada.Text_IO.Close(F);
--    end Test_Ena_Ansi_Picture;
--*                                                                         *--
--*-------------------------------------------------------------------------*--
--*                                                                         *--
--* The following example illustrates reading an APA-file from disk and     *--
--* writing it to the terminal window.
--
--    procedure Test_Ena_Ansi_Picture2 is
--
--       Temp : Picture_Type;
--       F    : Ada.Text_IO.File_Type;
--
--    begin
--       Ada.Text_IO.Open(F, In_File, "nejdetskadu.apa");
--       Get(F, Temp);
--       Ada.Text_IO.Close(F);
--       Put(Temp, Width => 2, X => 20, Y => 10);
--    end Test_Ena_Ansi_Picture2;
--*                                                                         *--
--*-------------------------------------------------------------------------*--


