with Typer; use Typer;
with Tja.Sockets; use Tja.Sockets;

procedure Get_Planinfo(Socket        : in Socket_Type;
                       Planinfo      : out Planinfo_Type;
                       Antal_Spelare : in natural) is

   N : Natural;
begin


for I in 0..Antal_Spelare loop

   Get_line(Socket,Planinfo(I).Namn, N);
   Skip_Line(Socket);
   Get(Socket,Planinfo(I).Tecken);
   Skip_Line(Socket);
   Get(Socket,Planinfo(I).X);
   Skip_Line(Socket);
   Get(Socket,Planinfo(I).Y);
   Skip_Line(Socket);

end loop;

end Get_Planinfo;

