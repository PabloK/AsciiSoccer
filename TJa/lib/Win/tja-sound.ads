package TJa.Sound is

  procedure Play_Melody(Item : in String);

  -- Beskrivning av Play_Melody:
  -- ===========================
  --
  -- F�r att skapa en melodi beh�ver man skapa en str�ng med toner.
  -- Dessa toner byggs upp med hj�lp av nedanst�ende delar.
  --
  -- Ett par exempel p� en str�ngar som motsvarar n�gra olika saker:
  --
  -- Bj�rnen sover:
  --   "O4L2FFFAGGGbHAAGGl+l+FAAAAo+l+Cl+bHGGGGl+bHl+AFFFAGGGbHAAGGl+l+F"
  --
  -- Blinka lilla stj�rna (version 1):
  --   "L+DDAAHHl+AGG#F#FEEl+DAAGG#F#Fl+EAAGG#F#Fl+EDDAAHHl+AGG#F#FEEl+D"
  --
  -- Blinka lilla stj�rna (version 2):
  --   "L3.Dl+D.Al+A.Bl+Bl+A.Gl+G.#Fl+#F.El+El+D.Al+A.Gl+G.#Fl+#Fl+E" &
  --   ".Al+A.Gl+G.#Fl+#Fl+E.Dl+D.Al+A.Bl+Bl+A.Gl+G.#Fl+#F.El+El+D"
  --
  -- Olika noter:
  -- ------------
  --    "A"     = ton A (enligt gamla systemet)
  --    "B"     = ton H (enligt gamla systemet)
  --    "C"     = ton C (enligt gamla systemet)
  --    "D"     = ton D (enligt gamla systemet)
  --    "E"     = ton E (enligt gamla systemet)
  --    "F"     = ton F (enligt gamla systemet)
  --    "G"     = ton G (enligt gamla systemet)
  --    "H"     = ton H (gamla systemet)
  --
  --    F�r att komma �t de halvtoner som inte finns med h�r m�ste man
  --    anv�nda prefixen beskrivna nedan.
  --
  -- Paus:
  -- -----
  --    " "     = Paus
  --
  --
  -- Oktavbyte:
  -- ----------
  --    "Ox"    = Byt till oktav x (g�ller tills ny "Ox" ges)
  --              Oktav 4 inneh�ller A med frekvensen 440 hz.
  --              x=+ el. x=- ger en oktav upp resp. ned.
  --
  -- Tonl�ngdsbyte:
  -- --------------
  --    "Lx"    = Byte av tonl�ngd, d�r x motsvaras av 0=helnot,
  --              1=halvnot, ... (g�ller tills ny "Lx" ges).
  --              standardtonl�ngd �r x=2 (fj�rdedelsnoter).
  --              En helnot spelas i 2.0 sekunder.
  --              x=+ el. x=- ger en l�ngre resp. kortare tonl�ngd.
  --
  -- Prefix till en not:
  -- -------------------
  --    "#"     = Tonh�jning ett halvt tonsteg
  --    "b"     = Tons�nkning ett halvt tonsteg
  --    "ox"    = Som "Ox", men bara f�r denna not
  --    "lx"    = Som "Lx", men bara f�r denna not
  --    "."     = Punkterad not (d.v.s. 1.5 * tonl�ngden)
  --
  --    De av dessa som anv�nds m�ste ligga i f�ljande ordning:
  --            [ox][lx][.][#][b]

  -----------------------------------------------------------------------------
  procedure Play_File(Filename : in String);

  -- Beskrivning av Play_File:
  -- =========================
  --
  -- Om man har s.k. ".au"-filer med ljud kan man spela upp dessa med
  -- hj�lp av denna procedur. Andra format s�som ".wav", ".mp3" eller
  -- s� g�r inte att f� ut n�got vettigt av. Det blir bara en massa
  -- brus.

  -----------------------------------------------------------------------------
  Audio_Device_Error : exception;  -- F�s om man inte hittar n�gon "ljudport".
  Note_Error         : exception;
  Note_Prefix_Error  : exception;
  Octave_Error       : exception;
  Tone_Length_Error  : exception;

end TJa.Sound;
