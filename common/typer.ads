with tja.sockets;
use  tja.sockets;
with Tja.Window.Elementary;
with Tja.Window.text;
with Tja.Window.Graphic;
use  Tja.Window.Elementary,  Tja.Window.text;
use  Tja.Window.Graphic;


package Typer is


   -----------------------
   -- Spelar och bollpost
   -----------------------
   -- TODO fix names of variables
   type Spelar_Post is record
      X, Y, Lagnr, Spelar_Nr : Integer := 0;
      Namn        : String(1..20) := (others => ' ');
      Tecken : Character;
      Lagnamn: Natural;
      NameLength : Natural;
   end record;
   ---------------------------
   -- Global array av posterna
   ---------------------------
   -- TODO fix names of variables
   type Planinfo_Type is array(0..8) of Spelar_Post;


   type Player_Soc_Arr_type is array(1..8) of Socket_Type;

        subtype Spelplan_X is Integer range 1..100;
        subtype Spelplan_Y is Integer range 1..30;


  type Lag_typ is record
     Namn          : String(1..20);
     Length        : Integer;
     B_Farg, S_Farg: Colour_Type;

  end record;

  type Lagnamnslista_typ is array (1..5) of Lag_typ;


 end Typer;
