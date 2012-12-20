with Ada.Command_Line;    use Ada.Command_Line;
with Ada.Exceptions;      use Ada.Exceptions;
with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with TJa.Sockets;         use TJa.Sockets;

procedure Klient is

   --Socket_type anv�nds f�r att kunna kommunicera med en server
   Socket : Socket_Type;

   Text      : String(1..100); --Anv�nds f�r att ta emot text fr�n anv�ndaren
   Textlangd : Natural;        --Kommer inneh�lla l�ngden p� denna text
   Resultat  : Natural;        --Resultatet fr�n servern


begin
   --Denna rutin kontrollerar att programmet startas med tv� parametrar.
   --Annars kastas ett fel.
   --Argumentet skall vara serverns adress och portnummer, t.ex.:
   --> klient localhost 3400
   if Argument_Count /= 2 then
      Raise_Exception(Constraint_Error'Identity,
                      "Usage: " & Command_Name & " remotehost remoteport");
   end if;

   -- Initierar en socket, detta kr�vs f�r att kunna ansluta denna till
   -- servern.
   Initiate(Socket);

   -- Ansluter till servern
   Connect(Socket, Argument(1), Positive'Value(Argument(2)));


   loop



   end loop;

   --Innan programmet avslutar st�ngs socketen, detta genererar ett exception
   --hos servern, pss kommer denna klient f� ett exception n�r servern avslutas
   Close(Socket);



end Klient;
