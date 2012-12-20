-------------------------------------------------------------------------------
--|                                                                         |--
--|                       Torbjörn Jonsson Ada library                      |--
--|                                                                         |--
--|                          T J A . S O C K E T S                          |--
--|                                                                         |--
--|                              Specification                              |--
--|                              Version  1.00                              |--
--|                                                                         |--
--|                              (C) Copyright                              |--
--|                   Torbjörn Jonsson,  TorJo@Ida.LiU.se                   |--
--|                                                                         |--
-------------------------------------------------------------------------------
--|                                                                         |--
--| Versions:                                                               |--
--|                                                                         |--
--|   2005-01-29  Version 1.00 is ok.                                       |--
--|               Created and documented by Torbjörn Jonsson.               |--
--|                                                                         |--
-------------------------------------------------------------------------------
--|                                                                         |--
--| Description:                                                            |--
--|                                                                         |--
--|   This library is hides the problems with normal sockets handling.      |--
--|                                                                         |--
-------------------------------------------------------------------------------

-- Ada standard libraries.
with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Float_Text_IO;                 use Ada.Float_Text_IO;
with Ada.Integer_Text_IO;               use Ada.Integer_Text_IO;
--  with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;

-- External libraries.
with Gnat.Sockets;

-- Internal libraries.
--  with TJa.Text;                          use TJa.Text;
--  with TJa.Lists.Checked.Double_Linked.Unsorted_List.Checked_Data;

-------------------------------------------------------------------------------
package TJa.Sockets is

  -----------------------------------------------------------------------------
  --| "Listener_Type" is the type used for socket connections. A variable of
  --| this type is used to wait for connections from other programs.
  -----------------------------------------------------------------------------

  type Listener_Type is private;
   
  -----------------------------------------------------------------------------
  --| "Socket_Type" is the type used for socket communication. A variable of
  --| this type is used for communication with another program.
  -----------------------------------------------------------------------------

  type Socket_Type is private;

  -----------------------------------------------------------------------------
  --| "Initiate" is the first thing to do with a "Listener". It does the
  --| initiation of the socket which includes to start listen at the given port
  --| "Port_Number" at the local host. "Max_Queue_Length" is the number of
  --| connections which can be queued.
  -----------------------------------------------------------------------------
  
  procedure Initiate(Listener         : in out Listener_Type;
		     Port_Number      : in     Natural;
		     Max_Queue_Length : in     Positive := 15);

  -----------------------------------------------------------------------------
  --| "Wait_For_Connection" waits for a connection from another host at the
  --| "Listener". When another host is connected (connection is established)
  --| the "Socket" can be used for the communication and the "Listener" can be
  --| used for the next connection.
  --|
  --| The "Socket" can be used for both reading and writing operations when the
  --| connection is established.
  -----------------------------------------------------------------------------

  procedure Wait_For_Connection(Listener : in     Listener_Type;
				Socket   :    out Socket_Type);

  -----------------------------------------------------------------------------
  --| "Close" is used to close a "Listener". The "Listener" can't receive any
  --| more connections after it has been closed. A "Listener" must be closed
  --| before the program ends.
  -----------------------------------------------------------------------------

  procedure Close(Listener : in out Listener_Type);
  
  -----------------------------------------------------------------------------
  --| "Initiate" is the first thing to do with a "Socket" which shall be used
  --| to connect to a host. It does the initiation of the socket.
  -----------------------------------------------------------------------------
  
  procedure Initiate(Socket : in out Socket_Type);

  -----------------------------------------------------------------------------
  --| "Connect" tries to connect to another host named "Host_Name" at the port
  --| number "Port_Number".
  -----------------------------------------------------------------------------

  procedure Connect(Socket      : in out Socket_Type;
		    Host_Name   : in     String;
		    Port_Number : in     Natural);

  -----------------------------------------------------------------------------
  --| "Close" is used to close a "Socket". A "Socket" loses the connection when
  --| it is closed. A "Socket" must be closed before the program ends.
  --|
  --| When a "Socket" is closed the other host receives an exception when it
  --| tries to do something with the connection.
  -----------------------------------------------------------------------------

  procedure Close(Socket : in out Socket_Type);

  -----------------------------------------------------------------------------
  --| "Put" writes the "Item" to the "Socket".
  -----------------------------------------------------------------------------

  procedure Put(Socket : in Socket_Type;
		Item   : in String);
  
  procedure Put(Socket : in Socket_Type;
		Item   : in Character);
  
  procedure Put(Socket : in Socket_Type;
		Item   : in Integer;
		Width  : in Field       := Default_Width;
		Base   : in Number_Base := Default_Base);
  
  procedure Put(Socket : in Socket_Type;
		Item   : in Float;
	        Fore   : in Field := Default_Fore;
	        Aft    : in Field := Default_Aft;
		Exp    : in Field := Default_Exp);

  -----------------------------------------------------------------------------
  --| "Put_Line" writes the "Item" and an end of line mark to the "Socket".
  -----------------------------------------------------------------------------

  procedure Put_Line(Socket : in Socket_Type;
		     Item   : in String);
  
  procedure Put_Line(Socket : in Socket_Type;
		     Item   : in Character);
  
  procedure Put_Line(Socket : in Socket_Type;
		     Item   : in Integer;
		     Width  : in Field       := Default_Width;
		     Base   : in Number_Base := Default_Base);
  
  procedure Put_Line(Socket : in Socket_Type;
		     Item   : in Float;
		     Fore   : in Field := Default_Fore;
		     Aft    : in Field := Default_Aft;
		     Exp    : in Field := Default_Exp);

  -----------------------------------------------------------------------------
  --| "New_Line" writes "Spacing" number of end of line marks to "Socket".
  -----------------------------------------------------------------------------

  procedure New_Line(Socket  : in Socket_Type;
		     Spacing : in Positive := 1);
  
  -----------------------------------------------------------------------------
  --| "Get" reads the "Item" from the "Socket". It leaves the rest of the row
  --| in "Socket".
  -----------------------------------------------------------------------------

  procedure Get(Socket : in     Socket_Type;
		Item   :    out String);

  procedure Get(Socket : in     Socket_Type;
		Item   :    out Character);
  
  procedure Get(Socket : in     Socket_Type;
		Item   :    out Integer;
	        Width  : in     Natural := 0);
  
  procedure Get(Socket : in     Socket_Type;
		Item   :    out Float);

  -----------------------------------------------------------------------------
  --| "Get_Line" read a string (the "Item") from the "Socket". If the length
  --| of the input string is less than the length of "Item" the end of line
  --| mark is read from "Socket". If the input is longer than "Item" the rest
  --| of the input is left in the "Socket".
  --|
  --| The "Last" parameter returns the index in "Item" where the last character
  --| is inserted. If first thing in "Socket" is end of line mark then "Last"
  --| is set to one less than the first index in "Item".
  --|
  --| If "Last" is less then the last index in "Item" the positions in "Item"
  --| where index is bigger than "Last" are unchanged.
  -----------------------------------------------------------------------------

  procedure Get_Line(Socket : in     Socket_Type;
		     Item   : in out String;
		     Last   :    out Natural);

  -----------------------------------------------------------------------------
  --| "Skip_Line" deletes "Spacing" number of lines from "Socket". A line
  --| includes the end of line mark.
  -----------------------------------------------------------------------------

  procedure Skip_Line(Socket  : in Socket_Type;
		      Spacing : in Positive := 1);
  
  -----------------------------------------------------------------------------
  --| Special exceptions for "Sockets"-library.
  -----------------------------------------------------------------------------

  Sockets_Internal_Error : exception;  -- An internal error has occured.

  -----------------------------------------------------------------------------

private

  -----------------------------------------------------------------------------

  type Listener_Type is
     record
       Address   : Gnat.Sockets.Sock_Addr_Type;
       Socket    : Gnat.Sockets.Socket_Type;
       Initiated : Boolean := False;
     end record;

  -----------------------------------------------------------------------------
  
  type Socket_Type is
     record
       Address   : Gnat.Sockets.Sock_Addr_Type;
       Socket    : Gnat.Sockets.Socket_Type;
       Channel   : Gnat.Sockets.Stream_Access;
       Initiated : Boolean := False;
       Open      : Boolean := False;
     end record;

  -----------------------------------------------------------------------------

end TJa.Sockets;
