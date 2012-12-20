#AsciiSoccer

##Download gnat here
![http://libre.adacore.com/download/](http://libre.adacore.com/download/)

##Compileing
```shell
cd AsciiSoccer/TJa/lib/Win
gnat compile -gnat95 ../../src/Win/*.adb
cd /AsciiSoccer
gnat make -gnat95 -aLTJa/lib/Win -aITJa/src/Win -aOTJa/lib/Win AsciiSoccer/server.adb
```
