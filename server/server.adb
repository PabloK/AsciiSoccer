with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Command_Line;

with Tja.Sockets;
with Tja.Window.Elementary;
with Tja.Window.Text;

with Spelarrorelse;
with Typer;
with Goal;
with Uppdatera_Klienter;
with Skicka_Info;
with Meny;
with Startpos;
with Skotta;
with Skottkoll;

use Ada.Text_IO;
use Ada.Integer_Text_IO;
use Ada.Command_Line;

use Tja.Sockets;
use Tja.Window.Elementary;
use Tja.Window.text;

use Typer;

procedure Server is

  -------------------------------------------------
  -- Gameboard data type
  ------------------------------------------------
  protected type Task_Com_Type is

    procedure Write_Player (Spelarnummer, Riktning : in Integer); -- X, Y Change in these dimensions
    procedure Read_Arr (Plan_Info_ut : out PlanInfo_Type;
			Handelse_ut  : out Natural);
    function Handelser return Integer;
    procedure Add_Data (Namn                  : in String;
			Spelarnummer, Lagnamn : in Integer;
			Teck                  : in character);
    procedure Im_Ready;
    function Ready return Boolean;
    function is_game_finnished return Boolean;
    procedure Set_Num_Of_Players (N : in Natural);
    procedure Lagval (Lag    : in Integer;
		      Player : in out integer);
    procedure Shoot_Is_Valid (Spelar_Nr : in Integer;
			      Valid     : out Boolean;
			      SkottR    : out Integer );
    procedure  Shoot (Riktning : in Integer;
		      Valid    : out Boolean);

  private
    Plan_Info      : Planinfo_Type;
    Antal_Spelare  : Natural := 0;
    Num_Of_Players : Natural := 0;
    Antal_Ready    : Natural := 0;
    Handelse       : Integer;
    Goal_1, Goal_2 : Natural := 0;
    Game_Over      : Boolean := False;

  end Task_Com_Type;

  protected body Task_Com_Type is

    function is_game_finnished return Boolean is
    begin
      return Game_Over;
    end is_game_finnished;

    procedure Write_Player (Spelarnummer, Riktning : in Integer) is

    begin
      if (not (is_game_finnished)) then

	Spelarrorelse (Spelarnummer, Plan_Info, Riktning, Antal_Spelare, Handelse);
	if Handelse = 1 then
	  Goal_1 := Goal_1 + 1;
	  New_Line;
	  Put ("Lag 1 gjorde mÂl!");
	elsif  Handelse = 2 then
	  Goal_2 := Goal_2 + 1;
	  New_Line;
	  Put ("Lag 2 gjorde mÂl!");
	end if;

	if Goal_2 > 2 then

	  Handelse := 4;
	  Game_Over := True;
	  --TODO end game in favour of team 2

	elsif Goal_1 > 2 then

	  Handelse := 3;
	  Game_Over := True;
	  New_Line;
	  --TODO end game in favour of team

	end if;

      end if;

    end Write_Player;


    procedure Read_Arr (Plan_Info_ut : out PlanInfo_Type;
			Handelse_ut  : out Natural) is

    begin
      Plan_Info_ut :=  Plan_Info;
      Handelse_Ut := Handelse;
      Handelse := 0;

    end Read_Arr;

    function Handelser return Integer is
    begin
      return Handelse;
    end Handelser;

    procedure Add_Data (Namn                  : in String;
			Spelarnummer, Lagnamn : in Integer;
			Teck                  : in character ) is
    begin
      Plan_Info (Spelarnummer).Namn := Namn;
      Plan_Info (Spelarnummer).Spelar_Nr := Spelarnummer;
      Plan_Info (Spelarnummer).Tecken := Teck;
      Num_Of_players := Num_Of_players + 1;
    end Add_Data;

    function Ready return Boolean is

    begin
      return Antal_Ready = Antal_Spelare;
    end Ready;

    Procedure Im_Ready is

    begin

      Antal_Ready := Antal_Ready + 1;
      if Ready then
	Startpos (Plan_Info, Antal_Spelare);
      end if;

    end Im_Ready;

    procedure Set_Num_Of_Players (N : in natural) is

    begin
      Antal_Spelare := N;
    end Set_Num_Of_Players;

    procedure Lagval (Lag    : in Integer;
		      Player : in out Integer) is

    begin

      loop

	Plan_Info (Player).Lagnr := Lag;
	Player := Player + 2;
	exit when Player > Antal_Spelare;

      end loop;


    end Lagval;

    procedure Shoot_Is_Valid (Spelar_Nr  : in Integer;
			      Valid      : out Boolean;
			      SkottR     : out Integer ) is

    begin

      SkottKoll (Plan_Info, Spelar_Nr, SkottR, Valid);

    end Shoot_Is_Valid;


    procedure Shoot (Riktning : in Integer;
		     Valid    : out Boolean) is

    begin
      Skotta (Plan_Info, Riktning, Antal_Spelare, Handelse, Valid);
    end Shoot;

  end Task_Com_Type;


  Task_Com : Task_Com_Type;

  --------------------------------------------
  -- Skott och pass tasken
  -------------------------------------------
  task Shoot is
    entry Skott (Riktning : in Integer);
    entry Pass (Riktning : in Integer);

  end Shoot;

  task body Shoot is

    Val : Boolean;
  begin

    loop
      select
	accept Skott (Riktning : in Integer) do
	  Val := True;
	  for I in 1 .. 5 loop
	    if Val then
	      Task_Com.Shoot (Riktning, Val);
	      delay 0.15;
	    end if;
	  end loop;
	end skott;
      or
	accept Pass (Riktning : in Integer) do
	  Val := True;
	  for I in 1 .. 3 loop
	    if Val then
	      Task_Com.Shoot (Riktning, Val);
	      delay 0.2;
	    end if;
	  end loop;
	end pass;
      end select;
    end loop;
  end Shoot;

  task type Com_In_Task is
    entry Start (Tempsoc        : in Socket_Type;
		 Temp_Spelar_Nr : in natural);
  end Com_In_Task;

  task body Com_In_Task is

    Socket           : Socket_Type;
    Int, Lagnr        : Integer := 0;
    Spelar_Nr, L     : Natural;
    Name             : String (1 .. 20);
    Ch, Riktning      : Character;
    S                : String (1 .. 1);
    Valid            : Boolean;
    SkottR           : Integer;

  begin

    select
      accept Start (Tempsoc   : in Socket_Type;
		    Temp_Spelar_Nr : in Natural) do
	Socket := Tempsoc;
	Spelar_Nr := Temp_Spelar_Nr;

      end Start;

    end select;

    if Spelar_Nr = 1 then
      Get (Socket, Int); --Land
      Skip_Line (Socket);
      Lagnr := 1;
      Task_Com.Lagval (Int, lagnr);
    elsif  Spelar_Nr = 2 then
      Get (Socket, Int); --Land
      Skip_Line (Socket);
      Lagnr := 2;
      Task_Com.Lagval (Int, lagnr);

    elsif Spelar_Nr mod 2 = 0 then
      Lagnr := 1;
    else
      Lagnr := 2;
    end if;
    Get_Line (Socket, Name, L);

    if L >= Name'Last then
      Skip_Line (Socket);
    end if;

    Get (Socket, Ch);
    Skip_Line (Socket);

    Task_Com.Add_Data (Name, Spelar_Nr, Lagnr, Ch);
    Task_Com.Im_Ready;

    loop

      Get (Socket, Riktning);
      S (1) := Riktning;
      Skip_Line (Socket);

      if Riktning = 'q' then
	--Task_Com.Quit(Spelar_Nr);
	--close(socket);
	exit;
      end if;
      if Riktning = 's' then
	Task_Com.Shoot_Is_Valid (Spelar_Nr, Valid, SkottR);
	if Valid then
	  Shoot.Skott (SkottR);
	end if;

      elsif Riktning = 'd' then
	Task_Com.Shoot_Is_Valid (Spelar_Nr, Valid, SkottR);
	if Valid then
	  Shoot.Pass (SkottR);
	end if;

      else
	Task_Com.Write_Player (Spelar_Nr, Integer'value (S));
      end if;

    end loop;

  end Com_In_Task;

  ------------------------------------------------
  -- Task to control changes in the game board
  ------------------------------------------------
  task Com_Out_Task is
    entry Add (tempsoc       : in Socket_Type;
	       tempspelarnr  : in Natural);
    entry Init;
  end Com_Out_Task;

  task body Com_Out_Task is

    Player_Soc_Arr           : Player_Soc_Arr_Type;
    Antal_Spelare, Handelse  : Natural;
    Plan_Info                : Planinfo_Type;
    Matchstart               : Boolean := false;

  begin

    loop
      select

	accept Add (Tempsoc : in Socket_Type;
	     Tempspelarnr : in Natural)do

	  Player_Soc_Arr (Tempspelarnr) := Tempsoc;          -- Add player to the socket array
	  Antal_Spelare  := Antal_Spelare + 1;

	end Add;

      or

	-- Startar spelet
	accept Init do

	  Put ("Game Started!");
	  Task_Com.Read_Arr (Plan_Info, Handelse);
	  for I in 1 .. Antal_Spelare loop
	    Skicka_Info (Player_Soc_Arr (I), Plan_Info, Antal_spelare);
	  end loop;
	  Matchstart := True;

	end Init;

      or
	delay (0.04);
	if Matchstart then
	  Task_Com.Read_Arr (Plan_Info, Handelse);
	  Uppdatera_Klienter (Player_Soc_Arr, Antal_Spelare, Plan_Info, Handelse);
	end if;

      end select;
    end loop;

  end Com_Out_Task;

  -- Meny


  package Game_Meny_package is
    new Meny (4, 13, 2);
  use Game_Meny_package;
  Game_Meny : Meny_Type := ((65, 38, "Antalspelare:", (" 2           ", " 4           ", " 6           ", " 8           ")),
			    (65, 40, "Portnummer:  ", (" 4000        ", " 4500        ", " 5000        ", " 5500        ")));



  -----------------------------------------------------

  type In_Type is Access Com_in_Task;              -- Pointer to the Task that handles information from players
  type Player_Task_Type is array (1 .. 8) of In_Type; -- Keeps track of the player this task belongs to

  Meny_Res           : Meny_Result_Type;
  Lyssnare           : Listener_Type;
  Socket             : Socket_Type;
  Valt_Antal_Spelare : natural;
  P_In_Arr           : Player_Task_Type;

begin

  --Meny
  Meny_Res := Init_Meny (Game_Meny, Blue, Yellow, "Klar");
  Valt_Antal_Spelare := Meny_Res (1) * 2;


  Task_Com.Set_Num_Of_Players (Valt_Antal_Spelare); -- Adds number of players to the game array.

  New_Line;
  Goto_XY (65, 44);
  Set_Colours (Cyan, Black);
  Put_Line ("V√§ntar p√• Klienter..");

  -- Startar kommunikation
  Initiate (Lyssnare, (Meny_Res (2) * 500 + 3500));


  for I in 1 .. Valt_Antal_Spelare loop

    Wait_For_Connection (Lyssnare, Socket);
    Goto_XY (65, 46 + 2 * I);
    Put_Line ("Ny spelare angame_overen");
    if I = 1 or I = 2 then
      Put_Line (Socket, 1);
    else
      Put_line (Socket, 0);
    end if;

    P_In_Arr (I) := new Com_In_task;
    P_In_Arr (I).Start (Socket, Natural (I));
    Com_Out_Task.Add (Socket, I);

  end loop;

  loop
    exit when Task_Com.Ready;
  end loop;

  Com_Out_Task.Init;

  loop
    exit when Task_Com.is_game_finnished;
    delay (1.0);
  end loop;

end Server;
