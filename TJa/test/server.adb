with Ada.Command_Line;    use Ada.Command_Line;
with Ada.Exceptions;      use Ada.Exceptions;
with Ada.Text_IO;         use Ada.Text_IO;
with TJa.Sockets;         use TJa.Sockets;


procedure Server is


   --Servern beh�ver en Listener_type f�r att ta emot inkommande anslutningar
   Lyssnare : Listener_Type;
   
   --Socket_type anv�nds f�r att kunna kommunicera med en klient
   Socket : Socket_Type;
   
   Text      : String(1..100); --Anv�nds f�r att ta emot text
   Textlangd : Natural;        --Kommer inneh�lla l�ngden p� denna text
   Antal_E   : Natural;        --Ska ber�knas till antalet 'E' i Text
   

begin
   
   --Denna rutin kontrollerar att programmet startas med en parameter.
   --Annars kastas ett fel.
   --Argumentet skall vara portnummret, programmet kan t.ex. startas med:
   --> server 3400
   if Argument_Count /= 1 then
      Raise_Exception(Constraint_Error'Identity,
                      "Usage: " & Command_Name & " port");
   end if;

   --Initierar lyssnaren p� en port
   Initiate(Lyssnare,Natural'Value(Argument(1)));
   
   --V�ntar tills en anslutning bildas, kr�vs att en klient k�r connect
   Wait_For_Connection(Lyssnare,Socket);
     
   --Nu har en anslutning skapats och vi kan d� b�rja en loop eller n�got
   Put_Line("Klient ansluten...");
   
   loop
      
      --V�ntar p� en str�ng fr�n klienten
      Get_Line(Socket,Text,Textlangd);
      
      --Letar r�tt p� antalet 'E' i denna text
      Antal_E:=0;
      for I in 1..Textlangd loop
	 if Text(I)='E' then
	    Antal_E := Antal_E + 1;
	 end if;
      end loop;
      
      --Skickar resultatet tillbaka
      Put_Line(Socket,Antal_E);
      
   end loop;
   
   --Lite felhantering       
exception
   
   when Constraint_Error =>
      Put_Line("Du matade inte in en paramter inneh�llande portnummer");
   when others => --kanske end_error eller socket_error, det betyder att
		  --klienten st�ngt sin socket. D� skall den st�ngas �ven
		  --h�r.
      Put_Line("Nu dog klienten");
      Close(Socket);
      
      
end Server;
