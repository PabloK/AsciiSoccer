with Tja.Sockets;         use Tja.Sockets;
with Ada.Command_Line;    use Ada.Command_Line;
with Ada.Exceptions;      use Ada.Exceptions;
with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Typer;               use Typer;
with Tja.Window.Graphic;  use Tja.Window.Graphic;
with Tja.Keyboard;        use Tja.Keyboard;
with K_Ritare;
with Laginfo;
with Get_Planinfo;
with Get_Plan;
with Print_Field;
with Tja.Window.Elementary;
with Tja.Window.text;
use  Tja.Window.Elementary,  Tja.Window.text;
with Ada.Text_IO; use Ada.Text_IO;
with Btmp3;       use Btmp3;
with Intro;
with Print_Handelse, Print_Teams,Meny;
with Print_End;
procedure Klient is

    protected type End_check is


       procedure Write_End;
       function Read_End return Boolean;

     private

      Slut  : Boolean := False;

   end End_check;

   protected body End_check is
     ----------------------
      procedure Write_End is

      begin
         Slut := True;
      end Write_End;
     ---------------------
      function Read_End return Boolean is

      begin
         return Slut;
      end Read_End;
      -------------------
   end End_check;

   End_Ch : End_Check;

   Antal_Spelare: Natural;

   task type Talk is
      entry start(So            : in Socket_Type;
                 Plan_Tmp       : in Planinfo_Type;
                 Antal_Spelare  : in Natural;
                 BC1,BC2,PC1,PC2: in Colour_Type;
                 Lag_Info       : in Lagnamnslista_Typ;
                 Lag1, Lag2     : in Natural);
   end Talk;

   task body talk is



      Socket           : Socket_Type;
      Plan, Ghost_Plan : Planinfo_Type;
      Handelse         : Natural;
      BC_1, BC_2, PC_1, PC_2 : Colour_Type;
      Goal_1, Goal_2   : Natural := 0;
      Team_Info        : Lagnamnslista_Typ;
      L1, L2           : Natural;
      Slut             : Boolean := False;
   begin
      select
         accept start(So             : in Socket_Type;
                      Plan_Tmp       : in Planinfo_Type;
                      Antal_Spelare  : in Natural;
                      BC1,BC2,PC1,PC2: in Colour_Type;
                      Lag_Info       : in Lagnamnslista_Typ;
                      Lag1, Lag2     : in Natural) do

            Socket:= So;
            Plan := Plan_Tmp;
            Ghost_Plan := Plan_tmp;
            Print_Field;
            Goto_XY(1,35);
            BC_1 := BC1;
            BC_2 := BC2;
            PC_1 := PC1;
            PC_2 := PC2;
            K_ritare(Plan, Ghost_Plan, Antal_Spelare, BC1,BC2,PC1,PC2);
            Print_teams(Lag_Info(Lag1).namn,Lag_Info(Lag2).Namn,Lag1,Lag2, Lag_Info(Lag1).Length,Lag_Info(Lag2).Length);
            Team_Info := Lag_Info;
            L1 := Lag1;
            L2 := Lag2;

         end start;
      end select;

      loop

        if (not(Slut)) then
           Get_plan(Socket, Plan, Antal_Spelare, Handelse);
        end if;

        --k_ritare får in nuvarande plan, gamla planen antal spelare samt färger för lagen

        if Handelse = 1 then
           Goal_1 := Goal_1 + 1;
           Print_Handelse(Handelse, Goal_1);
        elsif Handelse = 2 then
           Goal_2 := Goal_2 + 1;
           Print_Handelse(Handelse, Goal_2);
        elsif Handelse = 3 then
           print_end(1,Plan, Team_info,Antal_spelare,Goal_1,Goal_2, L1, L2);
           Slut := True;
           End_Ch.Write_End;
           Close(Socket);
        elsif Handelse = 4 then
           print_end(2,Plan, Team_info,Antal_spelare,Goal_1,Goal_2, L1, L2);
           Slut := True;
           End_Ch.Write_End;
           Close(Socket);
        end if;
        K_ritare(Plan, Ghost_Plan, Antal_Spelare, BC_1,BC_2,PC_1,PC_2);
        Ghost_Plan := Plan;

      end loop;

   end Talk;



   package Game_Meny_package is
      new Meny(5,13,2);
   use Game_Meny_package;
   Game_Meny : Meny_Type := ((65,38,"             ",("             ","             ","             ","             ","             ")),
                             (65,40,"Välj lag:    ",(" Sverige     "," Brasilien   "," England     "," Italien     "," Dannmark    ")));


   Meny_Res        : Meny_Result_Type;
   S               : Socket_Type;
   Str             : String(1..20);
   T               : Talk;
   Int             : Integer;
   Riktning        : Key_Type;
   Ch              : Character;
   Plan            : Planinfo_Type;
   Lag_1,Lag_2, N  : Natural;
   L_Info          : Lagnamnslista_Typ;
   BC1,BC2,PC1,PC2: Colour_Type;

begin
   Play("W1.mp3");
   Intro;
   L_Info := Laginfo; --variabeln l_info tilldelas lite info om lagen som skall spela och ha roligt tillsammans med olika fina färger



   Initiate(S);
   Connect(S, Argument(1), Natural'Value(Argument(2)));
   Get(S,Int);
   Skip_Line(S);

   -- Spelar info
   if Int = 1 then

   --Meny
      Meny_Res := Init_Meny(Game_Meny,Blue,Cyan,"Klar");
      New_Line;
      Set_Colours(CYan,Black);
   Goto_XY(65,44);
   Put("Skriv in ditt namn: ");
   Get_line(Str, N);
   if N = 20 then
      Skip_Line;
   end if;
   Goto_XY(65,46);
   Put("Välj dne bokstav du vill vara: ");
   Get(Ch);
   Put_line(S,Meny_Res(2));
   Put_Line(S,Str(1..N));
   Put_line(S,Ch);

   else
   Goto_XY(65,44);
   Put("Skriv in ditt namn: ");
   Get_line(Str, N);
   if N = 20 then
      Skip_Line;
   end if;
   Goto_XY(65,46);
   Put("Nu din char: ");
   Get(Ch);
   Put_Line(S,Str(1..N));
   Put_line(S,Ch);

end if;
-- Hämta från servern
Goto_XY(65, 48);
Put_Line("Väntar på Server");
Set_Echo_Mode(Off);
Get(S,Antal_Spelare);
   Put("Antal spelare: ");
   Put(Antal_Spelare);
Skip_Line(S);
Get(S,Lag_1);
   Put("Lag 1: ");
   Put(Lag_1);
Skip_Line(S);
Get(S,Lag_2);
   Put("Lag 1: ");
   Put(Lag_1);
Skip_Line(S);
Get_Planinfo(S,Plan,Antal_spelare);

-- Initierar färger

    BC1:=L_Info(Lag_1).B_Farg;
    BC2:=L_Info(Lag_2).B_Farg;
    PC1:=L_Info(Lag_1).S_Farg;
    PC2:=L_Info(Lag_2).S_Farg;
    Stop;
    -----------------------
    -- Loading
    -----------------------
    Clear_window;
    Goto_XY(45, 30);
    Put("LOADING");
    delay 0.5;
    Put(".");
    delay 0.5;
    Put(".");
    delay 0.5;
    Put(".");
    ------------------------
    Goto_XY(45, 30);
    Put("LOADING   ");
    ------------------------
    Goto_XY(45, 30);
    Put("LOADING");
    delay 0.5;
    Put(".");
     delay 0.5;
    Put(".");
    delay 0.5;
    Put(".");
    delay 0.5;
    ------------------------
    ------------------------

    -- Startar task
    Play("9.mp3");
    delay 3.0;
    T.start(S, Plan, Antal_Spelare, BC1,BC2,PC1,PC2, L_Info, Lag_1, Lag_2);
    Put(AscII.Esc&"[?25l");

    -------------------------------------
    loop
       ---------------------
       exit when End_Ch.Read_End;   -- Kollar om matchen är slut
       --------------------
       Get_immediate(Riktning);
       if Is_Character(Riktning)then

          Ch := To_Character(Riktning);

          if Ch in '1'..'9' and Ch /= '5'  then

             Put_line(S, Ch);

          elsif Ch='S' or Ch='s' then
             Put_line(S, 's');

          elsif Ch='D' or Ch='d' then
             Put_line(S, 'd');

          elsif  Ch = 'X' or Ch ='x' then
             exit;
          end if;
       end if;
    end loop;
    -------------------------------------
    abort T;
    Stop;

end Klient;
