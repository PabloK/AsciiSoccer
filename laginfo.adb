with Typer; use Typer;
with Tja.Window.Elementary;
with Tja.Window.text;
with Tja.Window.Graphic;

use  Tja.Window.Elementary,  Tja.Window.text;
use  Tja.Window.Graphic;

function Laginfo return Lagnamnslista_Typ is

    Lagnamnslista: Lagnamnslista_Typ;

begin

   Lagnamnslista(1).Namn:="Sverige             ";
   Lagnamnslista(1).Length := 7;
   Lagnamnslista(1).B_Farg:=Blue;
   Lagnamnslista(1).S_Farg:=Yellow;

   Lagnamnslista(2).Namn:="Braslien            ";
   Lagnamnslista(2).Length := 8;
   Lagnamnslista(2).B_Farg:=Yellow;
   Lagnamnslista(2).S_Farg:=Green;

   Lagnamnslista(3).Namn:="England             ";
   Lagnamnslista(3).Length := 7;
   Lagnamnslista(3).B_Farg:=White;
   Lagnamnslista(3).S_Farg:=Red;

   Lagnamnslista(4).Namn:="Italien             ";
   Lagnamnslista(4).Length := 7;
   Lagnamnslista(4).B_Farg:=Blue;
   Lagnamnslista(4).S_Farg:=White;

   Lagnamnslista(5).Namn:="Danmark             ";
   Lagnamnslista(5).Length := 7;
   Lagnamnslista(5).B_Farg:=Red;
   Lagnamnslista(5).S_Farg:=White;

-- TODO create more countries

   return Lagnamnslista;

end Laginfo;
