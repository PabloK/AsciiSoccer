-------------------------
--Denna procedur flyttar kollar om bollen och spelaren skall flyttas
-------------------------
with ada.integer_text_IO;
use ada.integer_text_io;
with ada.text_io;
use ada.text_io;
with move;
with Startpos;
with Typer;
with Goal;
use Typer;




procedure skotta(Plan_Info: in out Planinfo_Type;
                Riktning: in Integer;
                Antal_Spelare: in Integer;
                Handelse: out Natural;
                Valid: out Boolean) is


   Kopia: Planinfo_Type:= Plan_Info;

begin
Valid:=True;

   Move(Plan_Info(0).X,Plan_Info(0).Y,Riktning);
   -------

      for I in Planinfo_Type'First+1..Antal_Spelare loop
         if Plan_Info(0).X=Plan_Info(I).X and Plan_Info(0).Y=Plan_Info(I).Y then
            Plan_Info:=Kopia;
            Valid:=False;
         end if;
      end loop;





   --------------------------------------------
   --nu kollas det om bollen är för nära "mål"kanten
   if Plan_Info(0).X=Spelplan_X'First+1 and Plan_Info(0).Y in Spelplan_Y'First+1..Spelplan_Y'Last-1 and  Plan_Info(0).Y Not in 13..18 then
      Plan_Info:=Kopia;
   elsif Plan_Info(0).Y=Spelplan_Y'First+1 or Plan_Info(0).Y=Spelplan_Y'Last-1 then
      Plan_Info:=Kopia;
       Valid:=False;
   end if;

   if  Plan_Info(0).X=Spelplan_X'Last-1 and Plan_Info(0).Y in  Spelplan_Y'First+1..Spelplan_Y'Last-1 and Plan_Info(0).Y not in 13..18 then
      Plan_Info:=Kopia;
       Valid:=False;
   elsif Plan_Info(0).Y=Spelplan_Y'First+1 or Plan_Info(0).Y=Spelplan_Y'Last-1 then
      Plan_Info:=Kopia;
       Valid:=False;

   end if;

   --kollar om det är mål, om det är det flyttas bollen till mitten och alla spelare flyttas till startpositionerna...
   Handelse:= Goal(Plan_Info(0).X,Plan_Info(0).Y);
   if Handelse/=0 then
      Startpos(Plan_Info,Antal_Spelare);
   end if;

   --detta förhindrar att bollen fastnar i stolpen...[fulkod]
   if Plan_Info(0).X= Spelplan_X'First or Plan_Info(0).X= Spelplan_X'Last then
      Plan_Info:=Kopia;
       Valid:=False;
   end if;





   --kollar om spelaren är utanför planen
   for Count in  Planinfo_Type'First+1..Antal_Spelare loop
      if Plan_Info(Count).X=Spelplan_X'First or Plan_Info(Count).X=Spelplan_X'Last or Plan_Info(Count).Y=Spelplan_y'First or Plan_Info(Count).Y=Spelplan_Y'Last then
         Plan_Info:=Kopia;
          Valid:=False;
      end if;
   end loop;





end skotta;
-------------
--fråga petter om du inte fattar, han orkar inte skriva beskrivning just nu
--------------
