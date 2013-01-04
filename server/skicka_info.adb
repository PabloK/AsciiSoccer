with Typer; 
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Tja.Sockets;

use Typer; 
use Tja.Sockets;
use Ada.Text_IO;
use Ada.Integer_Text_IO;

procedure Skicka_Info(Socket                 : in Socket_Type;
                      Planinfo               : in Planinfo_Type;
                      Antal_Spelare          : in Integer)is

begin

   Put(Antal_Spelare);
   Put(Planinfo(1).LagNr);
   Put(Planinfo(2).LagNr);

   Put_line(Socket,Antal_Spelare);
   Put_line(Socket,Planinfo(1).LagNr);
   Put_line(Socket,Planinfo(2).LagNr);

   for I in 1..Antal_Spelare loop

      Put(Planinfo(I).Namn(1..Planinfo(I).NameLength));
      New_line;
      Put(Planinfo(I).Tecken);
      New_line;
      Put(Planinfo(I).X);
      New_line;
      Put(Planinfo(I).Y);
      New_line;

      Put_Line(Socket,Planinfo(I).Namn(1..Planinfo(I).NameLength));
      Put_Line(Socket,Planinfo(I).Tecken);
      Put_line(Socket,Planinfo(I).X);
      Put_line(Socket,Planinfo(I).Y);

   end loop;

end Skicka_Info;

