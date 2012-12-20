=AsciiSoccer=

==Download gnat here==
http://libre.adacore.com/download/

==Compileing==
cd to AsciiSoccer/TJa/lib/Win
gnat compile -gnat95 ../../src/Win/*.adb
cd to /AsciiSoccer
gnat make -gnat95 -aLTJa/lib/Win -aITJa/src/Win -aOTJa/lib/Win AsciiSoccer/server.adb

