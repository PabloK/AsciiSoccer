-------------------------------------------------------------------------------
--|                                                                         |--
--|                       Torbjörn Jonsson Ada library                      |--
--|                                                                         |--
--|                          T J A . S O C K E T S                          |--
--|                                                                         |--
--|                           Body implementation                           |--
--|                              Version  1.00                              |--
--|                                                                         |--
--|                              (C) Copyright                              |--
--|                   Torbjörn Jonsson,  TorJo@Ida.LiU.se                   |--
--|                                                                         |--
-------------------------------------------------------------------------------
--|                                                                         |--
--| Change log:                                                             |--
--|                                                                         |--
--|   2005-01-29  Version 1.00 is ok.                                       |--
--|               Created and documented by Torbjörn Jonsson.               |--
--|                                                                         |--
-------------------------------------------------------------------------------
--|                                                                         |--
--| Implementation details:                                                 |--
--|                                                                         |--
-------------------------------------------------------------------------------

-- Ada standard libraries.
--  with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Characters.Latin_1;            use Ada.Characters.Latin_1;
with Ada.Strings;                       use Ada.Strings;
with Ada.Strings.Fixed;                 use Ada.Strings.Fixed;
--  with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;

-- External libraries.
--  with Gnat.OS_Lib;                       use Gnat.OS_Lib;
--  with Gnat.Directory_Operations;         use Gnat.Directory_Operations;
--  with Gnat.Regexp;                       use Gnat.Regexp;

-- Internal libraries.
--  with TJa.Lists.Checked.Double_Linked.Unsorted_List.Checked_Data;

-------------------------------------------------------------------------------
package body TJa.Sockets is

  -----------------------------------------------------------------------------
  --| Initiate
  -----------------------------------------------------------------------------

  procedure Initiate(Listener         : in out Listener_Type;
                     Port_Number      : in     Natural;
                     Max_Queue_Length : in     Positive := 15) is

    use Gnat.Sockets;

  begin
    if not Listener.Initiated then
      Listener.Address.Addr := Addresses(Get_Host_By_Name(Host_Name), 1);
      Listener.Address.Port := Port_Type(Port_Number);

      Create_Socket(Listener.Socket);

      --  Allow reuse of local addresses.
      Set_Socket_Option(Listener.Socket, Socket_Level, (Reuse_Address, True));

      Bind_Socket(Listener.Socket, Listener.Address);
      Listen_Socket(Listener.Socket, Max_Queue_Length);

      Listener.Initiated := True;
    end if;
  end Initiate;

  -----------------------------------------------------------------------------
  --| Wait_For_Connection
  -----------------------------------------------------------------------------

  procedure Wait_For_Connection(Listener : in     Listener_Type;
                                Socket   :    out Socket_Type) is

    use Gnat.Sockets;

  begin
    if not Listener.Initiated then
      null;
      --NYI: ... raise Socket_Error;
    else
      Accept_Socket(Listener.Socket, Socket.Socket, Socket.Address);

      Socket.Channel := Stream(Socket.Socket);
      Socket.Initiated := True;
      Socket.Open := True;
    end if;
  end Wait_For_Connection;

  -----------------------------------------------------------------------------
  --| Close
  -----------------------------------------------------------------------------

  procedure Close(Listener : in out Listener_Type) is

    use Gnat.Sockets;

  begin
    Shutdown_Socket(Listener.Socket);
    Close_Socket(Listener.Socket);
    Listener.Initiated := False;
  end Close;

  -----------------------------------------------------------------------------
  --| Initiate
  -----------------------------------------------------------------------------

  procedure Initiate(Socket : in out Socket_Type) is

    use Gnat.Sockets;

  begin
    if not Socket.Initiated then
      Create_Socket(Socket.Socket);
      Set_Socket_Option(Socket.Socket, Socket_Level, (Reuse_Address, True));
      Socket.Initiated := True;
    end if;
  end Initiate;

  -----------------------------------------------------------------------------
  --| Connect
  -----------------------------------------------------------------------------

  procedure Connect(Socket      : in out Socket_Type;
                    Host_Name   : in     String;
                    Port_Number : in     Natural) is

    use Gnat.Sockets;

  begin
    if not Socket.Initiated then
      null;
      -- NYI: ... raise ...;
    elsif Socket.Open then
      null;
      -- NYI: ... raise ...;
    else
      Socket.Address.Addr := Addresses(Get_Host_By_Name(Host_Name), 1);
      Socket.Address.Port := Port_Type(Port_Number);

      Connect_Socket(Socket.Socket, Socket.Address);
      Socket.Channel := Stream(Socket.Socket);

      Socket.Open := True;
    end if;
  end Connect;

  -----------------------------------------------------------------------------
  --| Close
  -----------------------------------------------------------------------------

  procedure Close(Socket : in out Socket_Type) is

    use Gnat.Sockets;

  begin
    Shutdown_Socket(Socket.Socket);
    Close_Socket(Socket.Socket);
    Socket.Initiated := False;
    Socket.Open := False;
  end Close;

  -----------------------------------------------------------------------------
  --| Internal_Put
  -----------------------------------------------------------------------------

  procedure Internal_Put(Socket        : in Socket_Type;
                         Item          : in String;
                         New_Line_Mark : in Boolean := False) is

  begin
    for I in Item'Range loop
      Character'Output(Socket.Channel, Item(I));
    end loop;
    if New_Line_Mark then
      Character'Output(Socket.Channel, LF);  -- Latin_1
    end if;
  end Internal_Put;

  -----------------------------------------------------------------------------
  --| Internal_Get
  -----------------------------------------------------------------------------

  Last_Input_Char : Character := ' ';
  Has_Input_Char  : Boolean   := False;

  -----------------------------------------------------------------------------

  procedure Internal_Get(Socket        : in     Socket_Type;
                         Item          :    out Character;
                         New_Line_Mark :    out Boolean) is

  begin
    if Has_Input_Char then
      if Last_Input_Char = LF then  -- Latin_1
        New_Line_Mark := True;
        Item := ' ';
      else
        New_Line_Mark := False;
        Item := Last_Input_Char;
      end if;
      Has_Input_Char := False;
    else
      Last_Input_Char := Character'Input(Socket.Channel);
      if Last_Input_Char = LF then  -- Latin_1
        New_Line_Mark := True;
        Item := ' ';
      else
        New_Line_Mark := False;
        Item := Last_Input_Char;
      end if;
    end if;
  end Internal_Get;

  -----------------------------------------------------------------------------

  procedure Internal_Look_Ahead(Socket        : in     Socket_Type;
                                Item          :    out Character;
                                New_Line_Mark :    out Boolean) is

  begin
    if Has_Input_Char then
      if Last_Input_Char = LF then  -- Latin_1
        New_Line_Mark := True;
        Item := ' ';
      else
        New_Line_Mark := False;
        Item := Last_Input_Char;
      end if;
    else
      Last_Input_Char := Character'Input(Socket.Channel);
      if Last_Input_Char = LF then  -- Latin_1
        New_Line_Mark := True;
        Item := ' ';
      else
        New_Line_Mark := False;
        Item := Last_Input_Char;
      end if;
      Has_Input_Char := True;
    end if;
  end Internal_Look_Ahead;

  -----------------------------------------------------------------------------

  procedure Internal_Ignore_White_Spaces(Socket : in Socket_Type) is

  begin
--      Put_Line("DEBUG: Internal_Ignore_White_Spaces");
    if Has_Input_Char then
--        Put_Line("DEBUG: Internal_Ignore_White_Spaces: Has buffered character");
      if Last_Input_Char = LF or else Last_Input_Char = ' ' then  -- Latin_1
--          Put_Line("DEBUG: Internal_Ignore_White_Spaces: It was a white one");
        Has_Input_Char := False;
      end if;
    end if;
--      Put_Line("DEBUG: Internal_Ignore_White_Spaces: No (more) buffered whites");

    while not Has_Input_Char loop
      Last_Input_Char := Character'Input(Socket.Channel);
      if Last_Input_Char /= LF and then Last_Input_Char /= ' ' then  -- Latin_1
        Has_Input_Char := True;
--          Put_Line("DEBUG: Internal_Ignore_White_Spaces: Found 'non white' =" &
--                   Character'Image(Last_Input_Char));
--        else
--          Put_Line("DEBUG: Internal_Ignore_White_Spaces: Found white one");
      end if;
    end loop;
  end Internal_Ignore_White_Spaces;

  -----------------------------------------------------------------------------

  procedure Internal_Try_To_Get(Socket         : in     Socket_Type;
                                Legal_Chars    : in     String;
                                Item           : in out String;
                                Pos            : in out Positive;
                                New_Line_Mark  :    out Boolean;
                                Character_Read :    out Boolean) is

    Ch : Character;

  begin
--      Put_Line("DEBUG: Internal_Try_To_Get: Before look ahead");
    Internal_Look_Ahead(Socket, Ch, New_Line_Mark);
    Character_Read := False;
    if not New_Line_Mark then
--        Put_Line("DEBUG: Internal_Try_To_Get: Before 'for'");
      for I in Legal_Chars'Range loop
        if Ch = Legal_Chars(I) then
--            Put_Line("DEBUG: Internal_Try_To_Get: Found " & Legal_Chars(I .. I));
          Internal_Get(Socket, Item(Pos), New_Line_Mark);
          Character_Read := True;
          Pos := Pos + 1;
          exit;
        end if;
      end loop;
    end if;
  end Internal_Try_To_Get;

  -----------------------------------------------------------------------------
  --| Put
  -----------------------------------------------------------------------------

  procedure Put(Socket : in Socket_Type;
                Item   : in String) is

  begin
    Internal_Put(Socket, Item);
  end Put;

  -----------------------------------------------------------------------------
  procedure Put(Socket : in Socket_Type;
                Item   : in Character) is

    Str : String(1 .. 1) := (1 => Item);

  begin
    Internal_Put(Socket, Str);
  end Put;

  -----------------------------------------------------------------------------
  procedure Put(Socket : in Socket_Type;
                Item   : in Integer;
                Width  : in Field       := Default_Width;
                Base   : in Number_Base := Default_Base) is

    Str : String(1 .. Integer'Max(Width, Field'Last));
    Len : Natural;

  begin
    Put(Str, Item, Base);
--      Put_Line("DEBUG: Integer: |" & Str & "|.");
    Len := Integer'Max(Width, Trim(Str, Both)'Length);
    Internal_Put(Socket, Str(Str'Last - Len + 1 .. Str'Last));
  end Put;

  -----------------------------------------------------------------------------
  procedure Put(Socket : in Socket_Type;
                Item   : in Float;
                Fore   : in Field := Default_Fore;
                Aft    : in Field := Default_Aft;
                Exp    : in Field := Default_Exp) is

    Str : String(1 .. 3 * Field'Last + 2);
    Len : Natural;

  begin
--      Put_Line("DEBUG: Float in.");
    Put(Str, Item, Aft, Exp);
--      Put_Line("DEBUG: Float: |" & Str & "|.");
    Len := Trim(Str, Both)'Length;
    if Exp = 0 then
      Len := Integer'Max(Len, Fore + Aft + 1);
    elsif Exp > 0 then
      Len := Integer'Max(Len, Fore + Aft + Exp + 2);
    end if;
    Internal_Put(Socket, Str(Str'Last - Len + 1 .. Str'Last));
  end Put;

  -----------------------------------------------------------------------------
  --| Put_Line
  -----------------------------------------------------------------------------

  procedure Put_Line(Socket : in Socket_Type;
                     Item   : in String) is

  begin
    Put(Socket, Item);
    New_Line(Socket);
  end Put_Line;

  -----------------------------------------------------------------------------
  procedure Put_Line(Socket : in Socket_Type;
                     Item   : in Character) is

  begin
    Put(Socket, Item);
    New_Line(Socket);
  end Put_Line;

  -----------------------------------------------------------------------------
  procedure Put_Line(Socket : in Socket_Type;
                     Item   : in Integer;
                     Width  : in Field       := Default_Width;
                     Base   : in Number_Base := Default_Base) is

  begin
    Put(Socket, Item, Width, Base);
    New_Line(Socket);
  end Put_Line;

  -----------------------------------------------------------------------------
  procedure Put_Line(Socket : in Socket_Type;
                     Item   : in Float;
                     Fore   : in Field := Default_Fore;
                     Aft    : in Field := Default_Aft;
                     Exp    : in Field := Default_Exp) is

  begin
    Put(Socket, Item, Fore, Aft, Exp);
    New_Line(Socket);
  end Put_Line;

  -----------------------------------------------------------------------------
  --| New_Line
  -----------------------------------------------------------------------------

  procedure New_Line(Socket  : in Socket_Type;
                     Spacing : in Positive := 1) is

  begin
    for I in 1 .. Spacing loop
      Internal_Put(Socket, "", New_Line_Mark => True);
    end loop;
  end New_Line;

  -----------------------------------------------------------------------------
  --| Get
  -----------------------------------------------------------------------------

  procedure Get(Socket : in     Socket_Type;
                Item   :    out String) is

  begin
    for I in Item'Range loop
      Get(Socket, Item(I));
    end loop;
  end Get;

  -----------------------------------------------------------------------------
  procedure Get(Socket : in     Socket_Type;
                Item   :    out Character) is

    Eoln : Boolean;

  begin
    loop
      Internal_Get(Socket, Item, Eoln);
      exit when not Eoln;
    end loop;
  end Get;

  -----------------------------------------------------------------------------
  procedure Get(Socket : in     Socket_Type;
                Item   :    out Integer;
                Width  : in     Natural := 0) is

    Curr_Width : Natural;

  begin
    if Width = 0 then
      Curr_Width := Field'Last;
    else
      Curr_Width := Width;
    end if;
--      Put_Line("DEBUG: Get(Integer): Width = " & Integer'Image(Curr_Width));

    declare
      Str          : String(1 .. Integer'Min(Curr_Width, Field'Last));
      Pos          : Integer := 1;
      Base         : Natural;
      Base_Mark    : String(1 .. 1);
      Legal_Digits : String(1 .. 32) := "00112233445566778899AaBbCcDdEeFf";
      Eoln         : Boolean;
      Ok           : Boolean;

    begin
--        Put_Line("DEBUG: Get(Integer): Ignore white spaces");
      Internal_Ignore_White_Spaces(Socket);
      Str := (others => ' ');

--        Put_Line("DEBUG: Get(Integer): Try to get sign");
      Internal_Try_To_Get(Socket, "+-", Str, Pos, Eoln, Ok);

--        Put_Line("DEBUG: Get(Integer): Try to get digits");
      Ok := True;
      while Ok and then (not Eoln) and then (Pos <= Str'Last) loop
        Internal_Try_To_Get(Socket, Legal_Digits(1 .. 20), Str, Pos, Eoln, Ok);
      end loop;

      if (not Eoln) and then (Pos <= Str'Last - 2) then
--          Put_Line("DEBUG: Get(Integer): Try to get '#'");
        -- Possible that it is an explicit "base" input.
        Base := abs Integer'Value(Str(1 .. Pos - 1));
        Internal_Try_To_Get(Socket, "#:", Str, Pos, Eoln, Ok);
        if Ok and then (Base in 2 .. 16) then
--            Put_Line("DEBUG: Get(Integer): Try to get 'base digits'");
          Base_Mark := Str(Pos - 1 .. Pos - 1);
          while Ok and then (not Eoln) and then (Pos < Str'Last) loop
            Internal_Try_To_Get(Socket, Legal_Digits(1 .. Base * 2),
                                Str, Pos, Eoln, Ok);
          end loop;
--            Put_Line("DEBUG: Get(Integer): Try to get '#'");
          Internal_Try_To_Get(Socket, Base_Mark, Str, Pos, Eoln, Ok);
        end if;
      end if;

--        Put_Line("DEBUG: Get(Integer): Convert string to integer");
--        Put_Line("DEBUG: Get(Integer): Pos = " & Integer'Image(Pos));
      Item := Integer'Value(Str(1 .. Pos - 1));
    end;
--      Put_Line("DEBUG: Get(Integer): Done");
  end Get;

  -----------------------------------------------------------------------------
  procedure Get(Socket : in     Socket_Type;
                Item   :    out Float) is

    Str          : String(1 .. 3 * Field'Last + 2);
    Pos          : Positive := 1;
    Eoln         : Boolean;
    Ok           : Boolean;

  begin
    Internal_Ignore_White_Spaces(Socket);
    Str := (others => ' ');

    Internal_Try_To_Get(Socket, "+-", Str, Pos, Eoln, Ok);
    Ok := True;
    while Ok and then (not Eoln) and then (Pos <= Str'Last) loop
      Internal_Try_To_Get(Socket, "0123456789", Str, Pos, Eoln, Ok);
    end loop;

    if (not Eoln) and then (Pos < Str'Last) then
      Internal_Try_To_Get(Socket, ".", Str, Pos, Eoln, Ok);
      while Ok and then (not Eoln) and then (Pos <= Str'Last) loop
        Internal_Try_To_Get(Socket, "0123456789", Str, Pos, Eoln, Ok);
      end loop;

      if (not Eoln) and then (Pos < Str'Last) then
        Internal_Try_To_Get(Socket, "Ee", Str, Pos, Eoln, Ok);
        if (not Eoln) and then (Pos < Str'Last) then
          Internal_Try_To_Get(Socket, "+-", Str, Pos, Eoln, Ok);
          while Ok and then (not Eoln) and then (Pos <= Str'Last) loop
            Internal_Try_To_Get(Socket, "0123456789", Str, Pos, Eoln, Ok);
          end loop;
        end if;
      end if;
    end if;

    Item := Float'Value(Str(1 .. Pos - 1));
  end Get;

  -----------------------------------------------------------------------------
  --| Get_Line
  -----------------------------------------------------------------------------

  procedure Get_Line(Socket : in     Socket_Type;
                     Item   : in out String;
                     Last   :    out Natural) is

    Ch   : Character;
    Eoln : Boolean;

  begin
    Last := Item'First - 1;
    for I in Item'Range loop
      Internal_Get(Socket, Ch, Eoln);
      if Eoln then
        exit;
      else
        Item(I) := Ch;
        Last := I;
      end if;
    end loop;
  end Get_Line;

  -----------------------------------------------------------------------------
  --| Skip_Line
  -----------------------------------------------------------------------------

  procedure Skip_Line(Socket  : in Socket_Type;
                      Spacing : in Positive := 1) is

    Ch   : Character;
    Eoln : Boolean;

  begin
    for I in 1 .. Spacing loop
      loop
        Internal_Get(Socket, Ch, Eoln);
        exit when Eoln;
      end loop;
    end loop;
  end Skip_Line;

  -----------------------------------------------------------------------------

end TJa.Sockets;
