with Tja.Sockets, Ada.Text_IO, Ada.Integer_Text_IO;
with Typer; use Typer;
use Tja.Sockets, Ada.Text_IO, Ada.Integer_Text_IO;


procedure uppdatera_klienter(Player_Soc_Arr : in out Player_Soc_Arr_Type;
                             Antal_Spelare  : in Natural;
                             Plan_Info      : in Planinfo_Type;
                             Handelse       : in Natural) is

begin

  for I in 1..Antal_Spelare loop
    Put_line(Player_Soc_Arr(I),"start");
    Put_line(Player_Soc_Arr(I),"update_gameboard");

    for J in 0..Antal_Spelare loop
      Put_Line(Player_Soc_Arr(I), Plan_Info(J).X);
      Put_Line(Player_Soc_Arr(I), Plan_Info(J).Y);
    end loop;

    Put_Line(Player_Soc_Arr(I), Handelse);
    Put_line(Player_Soc_Arr(I),"end");

    if Handelse = 3 or Handelse = 4 then
      Close(Player_Soc_Arr(I));
    end if;
  end loop;

end uppdatera_klienter;
