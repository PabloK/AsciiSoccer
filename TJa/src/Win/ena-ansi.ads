--*-------------------------------------------------------------------------*--
--*                                                                         *--
--*                        ERIK NILSSON ADA LIBRARY                         *--
--*                                                                         *--
--*                            E N A . A N S I                              *--
--*                                                                         *--
--*                                S p e c                                  *--
--*                                                                         *--
--*                       Erik A. Nilsson, 2009-02-05                       *--
--*                                                                         *--
--*-------------------------------------------------------------------------*--

-- Ada standard libraries.
with Ada.Text_IO;                       use Ada.Text_IO;

package Ena.Ansi is




   type RGB is private;

private

   --*-------------------------------------------------------------------------
   --* Csi is a procedure which premits ansi escape sequences to be written in
   --* the terminal window. "CSI" stands for "Control Sequence Initiator".
   --*-------------------------------------------------------------------------

   procedure Csi(Item : in    String);

   function RGB_To_Ansi(Pixel : in RGB) return String;

   type RGB is
     array (1..3) of integer;

    Basic_Colors_RGB : constant array (1..16) of RGB :=
           ((0,       0,      0),  -- Dark black,     value 0,
            (220,     0,      0),  -- Dark red,       value 1,
            (0 ,      220,    0),  -- Dark green,     value 2,
            (210,     210,    20), -- Dark yellow,    value 3,
            (0,       40,     225),-- Dark blue,      value 4,
            (210,     0,      210),-- Dark magenta,   value 5,
            (0,       210,    210),-- Dark cyan,      value 6,
            (210,     210,    210),-- Dark white,     value 7,
            (170,     170,    170),-- Light Black,    value 8,
            (255,     0,      0),  -- Light Red,      value 9,
            (0,       255,    0),  -- Light Green,    value A,
            (255,     255,    0),  -- Light Yellow,   value B,
            (0,       120,    255),-- Light Blue,     value C,
            (255,     0,      255),-- Light Magenta,  value D,
            (0,       255,    255),-- Light Cyan,     value E,
            (255,     255,    255));--Light White,    value F,

end Ena.ansi;
