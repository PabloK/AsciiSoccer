with Typer; use Typer;
with Tja.Sockets; use Tja.Sockets;

procedure Skicka_Info(Socket                 : in Socket_Type;
                      Planinfo               : in Planinfo_Type;
                      Antal_Spelare          : in Integer)is

begin

   Put_line(Socket,Antal_Spelare);
   Put_line(Socket,Planinfo(1).LagNr);
   Put_line(Socket,Planinfo(2).LagNr);
   for I in 0..Antal_Spelare loop

      Put_Line(Socket,Planinfo(I).Namn);
      Put_Line(Socket,Planinfo(I).Tecken);
      Put_line(Socket,Planinfo(I).X);
      Put_line(Socket,Planinfo(I).Y);

   end loop;


end Skicka_Info;

