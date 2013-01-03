gnatmake -gnat95 server/server.adb  -aLTJa/lib/Win -aITJa/src/Win -aOTJa/lib/Win -aIcommon/; 
gnatmake -gnat95 client/klient.adb  -aLTJa/lib/Win -aITJa/src/Win -aOTJa/lib/Win -aIcommon/; 
rm *.o *.ali;