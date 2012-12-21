gnatmake server/server.adb -aI"TJa/src/Win/" -aI"server/" -aI"common/"
gnatmake client/klient.adb -aI"TJa/src/Win/" -aI"client/" -aI"common/"
rm *.o *.ali