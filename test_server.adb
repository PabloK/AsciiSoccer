with Tja.Sockets, Ada.Text_IO, Ada.Integer_Text_IO, Spelarrorelse, Typer, Goal, Uppdatera_Klienter, Ada.Command_Line, Skicka_Info,Intro,Meny,Tja.Window.Elementary,Tja.Window.Text, Startpos, Skotta, Skottkoll;
use Tja.Sockets, Ada.Text_IO, Ada.Integer_Text_IO, Typer, Ada.Command_Line,Tja.Window.Elementary,Tja.Window.text;

procedure Server is

   -------------------------------------------------
   -- Våran superskyddade datatyp!
   ------------------------------------------------
   protected type Task_Com_Type is

      procedure Write_Player(Spelarnummer, Riktning : in Integer); -- X, Y är förändringen i leden
      procedure Read_Arr(Plan_Info_ut : out PlanInfo_Type;
                         Handelse_ut  : out Natural);
      function Handelser return Integer;
      procedure Add_Data(Namn: in String;
                         Spelarnummer, Lagnamn: in Integer;
                         Teck : in character);
      procedure Im_Ready;
      function Ready return Boolean;
      function endgame return Boolean;
      procedure Num_Of_Players(N : in Natural);
      procedure Lagval(Lag   : in Integer;
                       Player: in out integer);
      procedure Shoot_Is_Valid(Spelar_Nr : in Integer;
                               Valid  : out Boolean;
                               SkottR : out Integer );
      procedure  Shoot(Riktning : in Integer;
                       Valid    : out Boolean);

     private
      Plan_Info     : Planinfo_Type;
      Antal_Spelare : Natural := 0;
      Num_Of_Players: Natural := 0;
      Antal_Ready   : Natural := 0;
      Handelse      : Integer;
      Goal_1, Goal_2: Natural := 0;
      Slut          : Boolean := False;

   end Task_Com_Type;

   ---------------------------------------
   -- Nu börjar all godiskod
   --------------------------------------

   protected body Task_Com_Type is

      function endgame return Boolean is
      begin
         return slut;
      end endgame;

      procedure Write_Player(Spelarnummer, Riktning : in Integer) is

      begin
         if (not(slut)) then

         Spelarrorelse(Spelarnummer, Plan_Info,Riktning,Antal_Spelare, Handelse);
         if Handelse = 1 then
            Goal_1 := Goal_1 + 1;
            New_Line;
            Put("Lag 1 gjorde mål!");
         elsif  Handelse = 2 then
            Goal_2 := Goal_2 + 1;
            New_Line;
            Put("Lag 2 gjorde mål!");
         end if;

         if Goal_2 > 2 then

            Handelse := 4;
            Slut := True;
            New_Line;
            Put("Lag 2 vann!");

         elsif Goal_1 > 2 then

            Handelse := 3;
            Slut := True;
            New_Line;
            Put("Lag 1 vann!");

         end if;

         end if;

      end Write_Player;


       procedure Read_Arr(Plan_Info_ut : out PlanInfo_Type;
                         Handelse_ut  : out Natural) is

      begin
         Plan_Info_ut :=  Plan_Info;
         Handelse_Ut := Handelse;
         Handelse:= 0;

      end Read_Arr;

      function Handelser return Integer is
      begin
         return Handelse;
      end Handelser;

      procedure Add_Data(Namn: in String;
                         Spelarnummer, Lagnamn: in Integer;
                         Teck : in character ) is
      begin
         Plan_Info(Spelarnummer).Namn:=Namn;
         Plan_Info(Spelarnummer).Spelar_Nr:=Spelarnummer;
         Plan_Info(Spelarnummer).Tecken := Teck;
         Num_Of_players := Num_Of_players + 1;
      end Add_Data;

      function Ready return Boolean is

      begin
         return Antal_Ready = Antal_Spelare;
      end Ready;

      Procedure Im_Ready is

      begin

         Antal_Ready := Antal_Ready+1;
         Put(Antal_Ready);
         if Antal_Ready = Antal_spelare then
            Startpos(Plan_Info, Antal_Spelare);
         end if;

      end Im_Ready;

      procedure Num_Of_Players(N: in natural) is

      begin
         Antal_Spelare := N;
      end Num_Of_Players;

      procedure Lagval(Lag : in Integer;
                       Player : in out Integer) is

      begin

      loop

         Plan_Info(Player).Lagnr := Lag;
         Player := Player + 2;
         exit when Player > Antal_Spelare;

      end loop;


      end Lagval;

      procedure Shoot_Is_Valid(Spelar_Nr  : in Integer;
                               Valid  : out Boolean;
                               SkottR : out Integer ) is

      begin

         SkottKoll(Plan_Info,Spelar_Nr,SkottR, Valid);

      end Shoot_Is_Valid;


      -------
      -- Specialare för skott
      -------
      procedure Shoot(Riktning : in Integer;
                      Valid : out Boolean) is

      begin
         Skotta(Plan_Info, Riktning, Antal_Spelare, Handelse, Valid);
      end Shoot;

      end Task_Com_Type;


      Task_Com : Task_Com_Type;

   --------------------------------------------
   -- Skott och pass tasken
   -------------------------------------------
   task Shoot is
      entry Skott(Riktning : in Integer);
      entry Pass(Riktning : in Integer);

   end Shoot;

   task body Shoot is

      Val : Boolean;
   begin

      loop
         select
            accept Skott(Riktning : in Integer) do
               Val := True;
               for I in 1..5 loop
                  if Val then
                     Task_Com.Shoot(Riktning, Val);
                     delay 0.15;
                  end if;

               end loop;

            end skott;

         or

            accept Pass(Riktning : in Integer) do
               Val := True;
               for I in 1..3 loop
                  if Val then
                      Task_Com.Shoot(Riktning, Val);
                     delay 0.2;
                  end if;

               end loop;

            end pass;

         end select;

      end loop;

      end Shoot;

   ---------------------------------------------
   -- Task type för in kommunikation
   ---------------------------------------------
   task type Com_In_Task is
      entry Start(Tempsoc    : in Socket_Type;
                  Temp_Spelar_Nr : in natural);
   end Com_In_Task;

   task body Com_In_Task is

      Socket           : Socket_Type;
      Int,Lagnr        : Integer := 0;
      Spelar_Nr, L     : Natural;
      Name             : String(1..20);
      Ch,Riktning      : Character;
      S                : String(1..1);
      Valid            : Boolean;
      SkottR           : Integer;

   begin

      select
         accept Start(Tempsoc   : in Socket_Type;
                      Temp_Spelar_Nr : in Natural) do
            Socket := Tempsoc;
            Spelar_Nr := Temp_Spelar_Nr;

         end Start;

      end select;

      if Spelar_Nr = 1 then
         Get(Socket, Int); --Land
         Skip_Line(Socket);
         Lagnr := 1;
         Task_Com.Lagval(Int, lagnr);
      elsif  Spelar_Nr = 2 then
         Get(Socket, Int); --Land
         Skip_Line(Socket);
         Lagnr := 2;
         Task_Com.Lagval(Int, lagnr);

      elsif Spelar_Nr mod 2 = 0 then
          Lagnr := 1;
      else
          Lagnr := 2;
      end if;
      Get_Line(Socket, Name, L);

      if L >= Name'Last then
           Skip_Line(Socket);
      end if;

      Get(Socket,Ch);
      Skip_Line(Socket);

            Task_Com.Add_Data(Name, Spelar_Nr, Lagnr, Ch);
            Task_Com.Im_Ready;

      loop

         Get(Socket, Riktning);
         S(1) := Riktning;
         Skip_Line(Socket);

         if Riktning = 'q' then
            --Task_Com.Quit(Spelar_Nr);
            --close(socket);
            exit;
         if Riktning = 's' then
            Task_Com.Shoot_Is_Valid(Spelar_Nr,Valid,SkottR);
            if Valid then
               Shoot.Skott(SkottR);
            end if;

         elsif Riktning = 'd' then
            Task_Com.Shoot_Is_Valid(Spelar_Nr,Valid, SkottR);
            if Valid then
               Shoot.Pass(SkottR);
            end if;

         else
            Task_Com.Write_Player(Spelar_Nr,Integer'value(S));
         end if;

      end loop;

   end Com_In_Task;

   ------------------------------------------------
   -- Tasken som kollar ändring å skickar ut
   ------------------------------------------------
   task Com_Out_Task is
      entry Add(tempsoc       : in Socket_Type;
                tempspelarnr : in Natural);
      entry Init;
   end Com_Out_Task;

   task body Com_Out_Task is



      Player_Soc_Arr : Player_Soc_Arr_Type;
      Antal_Spelare, Handelse  : Natural;
      Plan_Info      : Planinfo_Type;
      Matchstart     : Boolean := false;

   begin

      loop
         select

            accept Add(Tempsoc : in Socket_Type;
                    Tempspelarnr : in Natural)do

            Player_Soc_Arr(Tempspelarnr) := Tempsoc;          -- Lägger till en spelarsocket i listan för ut kommunikation
            Antal_Spelare  := Tempspelare;                   -- Uppdaterar antal spelare

            end Add;

         or

            accept Init do

            Task_Com.Read_Arr(Plan_Info, Handelse);
            for I in 1..Antal_Spelare loop
               Skicka_Info(Player_Soc_Arr(I), Plan_Info, Antal_spelare);    --skickar start info till spelare på soc I
            end loop;
            Matchstart := True;

         end Init;

        or
         delay (0.04);
         if Matchstart then
            Task_Com.Read_Arr(Plan_Info, Handelse, Antal_spelare);
            Uppdatera_Klienter(Player_Soc_Arr,Antal_Spelare,Plan_Info, Handelse);
         end if;

      end select;
      end loop;

   end Com_Out_Task;

   -- Meny


   package Game_Meny_package is
      new Meny(4,13,2);
   use Game_Meny_package;
   Game_Meny : Meny_Type := ((65,38,"Antalspelare:",(" 2           "," 4           "," 6           "," 8           ")),
                             (65,40,"Portnummer:  ",(" 4000        "," 4500        "," 5000        "," 5500        ")));



   -----------------------------------------------------

   type In_Type is Access Com_in_Task;              -- Pekare till tasken som tar in information från klienterna
   type Player_Task_Type is array(1..8) of In_Type; -- Håller reda på vilken spelare tasken till hör

   Meny_Res          : Meny_Result_Type;
   Lyssnare          : Listener_Type;
   Socket            : Socket_Type;
   Max_Antal_Spelare : natural;
   P_In_Arr          : Player_Task_Type;
   I                 : Integer := 0;

begin

   --Intro
   Intro;

   --Meny
   Meny_Res := Init_Meny(Game_Meny,Blue,Cyan,"Klar");
   Max_Antal_Spelare := Meny_Res(1) * 2;


   Task_Com.Num_Of_Players(Max_Antal_Spelare); -- LÄgger in antal spelare i Spel arrayen

   New_Line;
   Goto_XY(65,44);
   Set_Colours(Cyan,Black);
   Put_Line("Väntar på Klienter..");

   -- Startar kommunikation
   Initiate(Lyssnare,(Meny_Res(2)*500+3500));


   loop
      Wait_For_Connection(Lyssnare,Socket);
      I = I + 1;
      Goto_XY(65,46+2*I);
      Put_Line("Ny spelare ansluten");
      if I = 1 or I = 2 then
         Put_Line(Socket, 1);
      else
         Put_line(Socket, 0);
      end if;
      Task_Com.Update(I, );
      if I < 8 then
       P_In_Arr(I) := new Com_In_task;
       P_In_Arr(I).Start(Socket, Natural(I));
       Com_Out_Task.Start(Socket);
      else
	 null;
      end if;
      Task_Com.Update()
   end loop;
   loop
      exit when Task_Com.Ready;
   end loop;
   Com_Out_Task.Init;

   loop
      exit when Task_Com.endgame;
      delay(1.0);
   end loop;

   New_Line;
   Put("Nu är matchen slut");
   delay(9.0);
end Server;


