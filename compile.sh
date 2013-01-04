gnatmake -gnat95 server/server.adb  -aITJa/src/Win -aIcommon/ -o server.exe; 
rm *.o *.ali;
./server.exe
