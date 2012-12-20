-- Paket för att spela Mp3
-- (c) 2006 Ted Steen och Bengt Werstén
--
package Btmp3 is
   --Play spelar upp en låt, 'Mp3' är sökvägen till mp3.filen
   procedure Play(Mp3:String);
   --Stop avslutar en låt, dock ej definerat vilken :P
   procedure Stop;

end Btmp3;
