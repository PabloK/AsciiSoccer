gnatmake -gnat95 server/server.adb  -aITJa/src/Win -aIcommon/ -o server.exe; 
gnatmake -gnat95 client/klient.adb  -aITJa/src/Win -aIcommon/ -o client.exe; 
rm *.o *.ali;
