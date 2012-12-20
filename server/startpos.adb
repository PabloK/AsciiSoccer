--Först Planinfo Antal Spelare

with Ada.Integer_Text_Io;
use Ada.Integer_Text_Io;
with Typer;
use Typer;

procedure Startpos(Planinfo: in out Planinfo_Type;
                   Antal: in Natural) is

begin

   --bollen
   PlanInfo(0).X:=50;
   PlanInfo(0).Y:=15;

   --spelarna
   --två stycken----
   PlanInfo(1).X:=25;
   PlanInfo(1).Y:=8;

   PlanInfo(2).X:=75;
   PlanInfo(2).Y:=8;
   --fyra stycken----
   if Antal>=4 then
   PlanInfo(3).X:=25;
   PlanInfo(3).Y:=22;

   PlanInfo(4).X:=75;
   PlanInfo(4).Y:=22;
   end if;
   --sex stycken-----
   if Antal>=6 then
      PlanInfo(5).X:=25;
      PlanInfo(5).Y:=12;

      PlanInfo(6).X:=75;
      PlanInfo(6).Y:=12;
   end if;
   --åtta stycken----
   if Antal>=8 then
      PlanInfo(7).X:=25;
      PlanInfo(7).Y:=18;

      PlanInfo(8).X:=75;
      PlanInfo(8).Y:=18;
   end if;
end Startpos;
