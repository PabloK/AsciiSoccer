with Typer; use Typer;
with Tja.Sockets; use Tja.Sockets;
with Ada.Text_IO; use Ada.Text_IO;

procedure Get_Plan(    Socket        : in Socket_Type;
                       Planinfo      : out Planinfo_Type;
                       Antal_Spelare : in Natural;
                       Handelse      : out Natural) is
begin


for I in 0..Antal_Spelare loop

   Get(Socket,Planinfo(I).X);
   Skip_Line(Socket);
   Get(Socket,Planinfo(I).Y);
   Skip_Line(Socket);

end loop;

Get(Socket,Handelse);
Skip_Line(Socket);

end Get_Plan;
