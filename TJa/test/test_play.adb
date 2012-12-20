with Ada.Command_Line;                  use Ada.Command_Line;

with TJa.Sound;                         use TJa.Sound;

procedure Test_Play is

begin
  if Argument_Count > 0 then
    -- Någon har startat programmet med en  eller flera ljudfil(er)
    -- som argument:
    --   test_play splat.au
    --   test_play splat.au rooster.au
    -- Vi spelar upp dem en efter en.
    for I in 1 .. Argument_Count loop
      Play_File(Argument(I));
      Play_Melody("  ");
    end loop;
    return;
  end if;

  -- C-dur (skala från oktav 2 och uppåt) ...
  Play_Melody("O2CDEFGAHO+CDEFGAHO+CDEFGAHO+CDEFGAHO+CDEFGAHO+C");

  -- Björnen sover ...
  Play_Melody("O4L2FFFAGGGbHAAGGl+l+FAAAAo+l+Cl+bHGGGGl+bHl+AFFFAGGGbHAAGGl+l+F");

  -- Amazing grace ...
  Play_Melody("L+O4Cl+FL-AFL+l+AGl+FDl+CCl+Fl-Al-Fl+AGO+L+.CCO-L-" &
              "Ao+.CL-Ao+CAL+l+FC.DL-FFDL+l+CCl+Fl-Al-Fl+AGL+.FF");

  -- Bamse-låten
  Play_Melody("L+O4DGAo+EL-o+Do+CHAL+GDEGDHL-AG#FGL+l+A" &
              "DGAo+EL-o+Do+CHAL+GDEGDHL-AG#FAl+l+G" &
              "HHHHHHHHAAAAAAAAGGGGGG#FGAG#FEDL+l+D" &
              "DGAo+EL-o+Do+CHAL+GDEGDHL-AG#FAl+l+G");

  -- Var nöjd med allt som livet ger ...
  Play_Melody("O4CDF l+A#GAl-G.Fl- l-FGFGFGl-F.Dl- l-CFCFAo+Do+CbHAl+l+G" &
              " O+CDC l+DCDl-CO-.Al- l-FGFGFo+DFFl- l-GAo+CAo+CAFDCl+l+.F");

  -- Gånglåt från Äppelbo ...
  for I in 1 .. 2 loop
    Play_Melody("O4L+DGl-Gl-DGHo+l+.D" &
                "L-HO+Cl+DDo-Hl+DDE" &
                "l+CCO-Hl+AAHO+l+CCDl+EDCo-" &
                "HCO-HAl+G#FGAHA#Fl+DE#Fl+l+.G");
  end loop;
  for I in 1 .. 2 loop
    Play_Melody("HO+DL+GGGl-#Fl-E#F#F#Fl-Gl-#F" &
                "EEL-EG#FEl+ED#Cl+DCo-H" &
                "l+CCO-Hl+AGA" &
                "Ho+CHAl+G#FGAHA#Fl+DE#Fl+l+.G");
  end loop;

  -- Spela upp filen som innehåller ljudet av en kvinna som skrattar till.
  Play_File("laugh.au");
end Test_Play;
