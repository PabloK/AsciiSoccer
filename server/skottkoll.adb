with ada.integer_text_IO;
use ada.integer_text_io;
with ada.text_io;
use ada.text_io;
with Typer;
use Typer;

procedure skottkoll(Plan_Info: in Planinfo_Type;
                    Spelarnummer: in Integer;
                    Riktning: out Integer;
                    Valid: out Boolean) is

   Xdiff,Ydiff: Integer;


begin

   Valid:=False;

 Xdiff:=Plan_Info(0).X-Plan_Info(Spelarnummer).X;
 Ydiff:=Plan_Info(0).Y-Plan_Info(Spelarnummer).Y;


  if Xdiff=1 then
     case Ydiff is
        when -1 =>
           Riktning:=9;
           Valid:=True;
        when 0 =>
           Riktning:=6;
             Valid:=True;
        when 1 =>
           Riktning:=3;
             Valid:=True;
        when others =>
           null;
     end case;
       end if;

     if Xdiff=0 then
     case Ydiff is
        when -1 =>
           Riktning:=8;
           Valid:=True;
        when 1 =>
           Riktning:=2;
             Valid:=True;
        when others =>
           null;
     end case;
     end if;

     if Xdiff=-1 then
     case Ydiff is
        when -1 =>
           Riktning:=7;
           Valid:=True;
        when 0 =>
           Riktning:=4;
             Valid:=True;
        when 1 =>
           Riktning:=1;
             Valid:=True;
        when others =>
           null;
     end case;
     end if;


     end Skottkoll;
