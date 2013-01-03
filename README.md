#AsciiSoccer
Multiplayer ASCII-art soccer game for cygwin.

##Download gnat here
http://libre.adacore.com/download/

=======
##Download Cygwin
A link to cygwin http://cygwin.com/install.html installation page.

##Download Gnat
Here is a link to http://libre.adacore.com/download/ where gnat can be downloaded.

##Compiling
```shell
cd AsciiSoccer/TJa/lib/Win
gnat compile -gnat95 ../../src/Win/*.adb
cd /AsciiSoccer
gnat make -gnat95 -aLTJa/lib/Win -aITJa/src/Win -aOTJa/lib/Win AsciiSoccer/server.adb
```
