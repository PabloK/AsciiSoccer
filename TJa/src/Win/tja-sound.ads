package TJa.Sound is

  procedure Play_Melody(Item : in String);

  -- Beskrivning av Play_Melody:
  -- ===========================
  --
  -- För att skapa en melodi behöver man skapa en sträng med toner.
  -- Dessa toner byggs upp med hjälp av nedanstående delar.
  --
  -- Ett par exempel på en strängar som motsvarar några olika saker:
  --
  -- Björnen sover:
  --   "O4L2FFFAGGGbHAAGGl+l+FAAAAo+l+Cl+bHGGGGl+bHl+AFFFAGGGbHAAGGl+l+F"
  --
  -- Blinka lilla stjärna (version 1):
  --   "L+DDAAHHl+AGG#F#FEEl+DAAGG#F#Fl+EAAGG#F#Fl+EDDAAHHl+AGG#F#FEEl+D"
  --
  -- Blinka lilla stjärna (version 2):
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
  --    För att komma åt de halvtoner som inte finns med här måste man
  --    använda prefixen beskrivna nedan.
  --
  -- Paus:
  -- -----
  --    " "     = Paus
  --
  --
  -- Oktavbyte:
  -- ----------
  --    "Ox"    = Byt till oktav x (gäller tills ny "Ox" ges)
  --              Oktav 4 innehåller A med frekvensen 440 hz.
  --              x=+ el. x=- ger en oktav upp resp. ned.
  --
  -- Tonlängdsbyte:
  -- --------------
  --    "Lx"    = Byte av tonlängd, där x motsvaras av 0=helnot,
  --              1=halvnot, ... (gäller tills ny "Lx" ges).
  --              standardtonlängd är x=2 (fjärdedelsnoter).
  --              En helnot spelas i 2.0 sekunder.
  --              x=+ el. x=- ger en längre resp. kortare tonlängd.
  --
  -- Prefix till en not:
  -- -------------------
  --    "#"     = Tonhöjning ett halvt tonsteg
  --    "b"     = Tonsänkning ett halvt tonsteg
  --    "ox"    = Som "Ox", men bara för denna not
  --    "lx"    = Som "Lx", men bara för denna not
  --    "."     = Punkterad not (d.v.s. 1.5 * tonlängden)
  --
  --    De av dessa som används måste ligga i följande ordning:
  --            [ox][lx][.][#][b]

  -----------------------------------------------------------------------------
  procedure Play_File(Filename : in String);

  -- Beskrivning av Play_File:
  -- =========================
  --
  -- Om man har s.k. ".au"-filer med ljud kan man spela upp dessa med
  -- hjälp av denna procedur. Andra format såsom ".wav", ".mp3" eller
  -- så går inte att få ut något vettigt av. Det blir bara en massa
  -- brus.

  -----------------------------------------------------------------------------
  Audio_Device_Error : exception;  -- Fås om man inte hittar någon "ljudport".
  Note_Error         : exception;
  Note_Prefix_Error  : exception;
  Octave_Error       : exception;
  Tone_Length_Error  : exception;

end TJa.Sound;
