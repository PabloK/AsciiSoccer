with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Strings;                       use Ada.Strings;
with Ada.Strings.Fixed;                 use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Numerics;                      use Ada.Numerics;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Sequential_IO;

package body TJa.Sound is

  -----------------------------------------------------------------------------
  -- Float -> Integer conversion: Get the nearest integer value (use rounding).
  -----------------------------------------------------------------------------
  function To_Integer(Item : in Float) return Integer is

  begin
    return Integer(Float'Rounding(Item));
  end To_Integer;

  -----------------------------------------------------------------------------
  -- Integer <-> Character conversions.
  -----------------------------------------------------------------------------
  function Ascii_To_Character(Item : in Integer) return Character is

  begin
    return Character'Val(Item);
  end Ascii_To_Character;
  -----------------------------------------------------------------------------
  function Character_To_Ascii(Item : in Character) return Integer is

  begin
    return Character'Pos(Item);
  end Character_To_Ascii;

  -----------------------------------------------------------------------------
  -- '0'..'9' <--> 0..9
  -----------------------------------------------------------------------------
  function To_Integer(Item : in Character) return Integer is

  begin
    return Character'Pos(Item) - Character'Pos('0');
  end To_Integer;

  -----------------------------------------------------------------------------
  -- Create special notes.
  -----------------------------------------------------------------------------
  type Note_Type is (Pause, A, Bess, H, C, Ciss, D, Diss, E, F, Fiss, G, Giss);

  type Tone_Type is
    record
      Note   : Note_Type;
      Octave : Integer;
      Length : Integer;
      Dotted : Boolean;
    end record;

  -----------------------------------------------------------------------------
  function To_Tone_Type(Item        : in     String;
                        Curr_Octave : in     Natural;
                        Curr_Length : in     Natural) return Tone_Type is

    Tone : Tone_Type;
    X    : Boolean := False;

  begin
    case Item(Item'Last) is
      when ' ' => Tone.Note := Pause;
      when 'A' => Tone.Note := A;
      when 'B' => Tone.Note := H;
      when 'C' => Tone.Note := C;
      when 'D' => Tone.Note := D;
      when 'E' => Tone.Note := E;
      when 'F' => Tone.Note := F;
      when 'G' => Tone.Note := G;
      when 'H' => Tone.Note := H;
      when others => raise Note_Error;
    end case;

    Tone.Octave := Curr_Octave;
    Tone.Length := Curr_Length;
    Tone.Dotted := False;

    for I in Item'First .. Item'Last - 1 loop
      case Item(I) is
        when 'b' =>
          if Tone.Note = Pause then
            raise Note_Prefix_Error;
          end if;
          Tone.Note := Note_Type'Pred(Tone.Note);
          if Tone.Note = Pause then
            Tone.Note := Note_Type'Last;
          end if;
        when '#' =>
          if Tone.Note = Pause then
            raise Note_Prefix_Error;
          end if;
          Tone.Note := Note_Type'Succ(Tone.Note);
        when '.' =>
          Tone.Dotted := True;
        when 'o' =>
          X := True;
          case Item(I + 1) is
            when '+'        => Tone.Octave := Tone.Octave + 1;
            when '-'        => Tone.Octave := Tone.Octave - 1;
            when '0' .. '7' => Tone.Octave := To_Integer(Item(I + 1));
            when others     => raise Note_Prefix_Error;
          end case;
        when 'l' =>
          X := True;
          case Item(I + 1) is
            when '-'        => Tone.Length := Tone.Length + 1;
            when '+'        => Tone.Length := Tone.Length - 1;
            when '0' .. '7' => Tone.Length := To_Integer(Item(I + 1));
            when others     => raise Note_Prefix_Error;
          end case;
        when others =>
          if not X or else not (Item(I) in '0' .. '7' or else
				Item(I) = '+' or else Item(I) = '-') then
            raise Note_Error;
          end if;
          X := False;
      end case;
    end loop;

    return Tone;
  end To_Tone_Type;

  -----------------------------------------------------------------------------
  function To_Seconds(Item : in Tone_Type) return Float is

    Seconds : Float := 2.0;  -- Seconds for a "helnot" ...
    Factor  : Float := 1.0;

  begin
    if Item.Dotted then
      Factor := 1.5;
    end if;
    return Seconds * Factor / Float(2 ** Item.Length);
  end To_Seconds;

  -----------------------------------------------------------------------------
  -- Constants meassured in tests for output device.
  -----------------------------------------------------------------------------
  Meassured_No_Of_Characters  : constant Positive := 200_000;
  Meassured_Time_In_Seconds   : constant Float    := 25.0;

  -----------------------------------------------------------------------------
  -- Constants calculated from meassured constants.
  -----------------------------------------------------------------------------
  Time_For_One_Character      : constant Float :=
    Meassured_Time_In_Seconds / Float(Meassured_No_Of_Characters);

  No_Of_Characters_Per_Second : constant Float := 1.0 / Time_For_One_Character;

  -----------------------------------------------------------------------------
  -- Create the string to send to the audio device corresponding to the
  -- frequency and the time.
  --     Frequency - frequency given in hertz (Hz).
  --     Seconds   - time given in seconds (s).
  -----------------------------------------------------------------------------
  function Create_Sound(Frequency : in Float;
                        Seconds   : in Float;
                        Volume    : in Float := 50.0) return String is

    Vol        : Float := Float'Max(0.0, Float'Min(100.0, Volume));
    No_Chars   : Natural  := To_Integer(Seconds / Time_For_One_Character);
    Output_Str : String(1 .. No_Chars) := (others => Ascii_To_Character(0));
  
  begin
    if Frequency > 0.0 then
      for I in Output_Str'Range loop
        Output_Str(I) :=
	  Ascii_To_Character(Integer(Vol / 2.0 *
				     (1.0 + Sin(Float(I - 1) * 2.0 * Pi *
						Time_For_One_Character *
						Frequency))));
      end loop;

       for J in Output_Str'Range loop
         if Character'Pos(Output_Str(J)) < 5 then
          for I in reverse Output_Str'Range loop
            if Character'Pos(Output_Str(I)) < 5 then
              return Output_Str(J .. I);
            end if;
          end loop;
         end if;
       end loop;
    end if;

    return Output_Str;
  end Create_Sound;
  
  -----------------------------------------------------------------------------
  function Create_Pause(Seconds : in Float) return String is

  begin
    return Create_Sound(0.0, Seconds);
  end Create_Pause;
  
  -----------------------------------------------------------------------------
  Frequency_List : array (Note_Type) of Float :=
                     (Pause => 0.0,
		      C     => 2093.00, Ciss => 2217.46, D     => 2349.32,
		      Diss  => 2489.02, E    => 2637.02, F     => 2793.83,
		      Fiss  => 2959.96, G    => 3135.96, Giss  => 3322.44,
		      A     => 3520.00, Bess => 3729.31, H     => 3951.07);

  function Create_Sound(Note    : in Note_Type;
                        Octave  : in Integer;
                        Seconds : in Float;
                        Volume  : in Float) return String is

  begin
    if Note = Pause then
      return Create_Pause(Seconds);
    end if;
    return Create_Sound(Frequency_List(Note) * 2.0 ** (Octave - 7),
			Seconds, Volume);
  end Create_Sound;

  -----------------------------------------------------------------------------
  function Create_Sound(Tone : in Tone_Type) return String is

  begin
    return (Create_Sound(Tone.Note, Tone.Octave, To_Seconds(Tone), 50.0));
  end Create_Sound;

  -----------------------------------------------------------------------------
  -- Device handling ...
  -----------------------------------------------------------------------------
  Device_String_Length : constant Positive := 1000;

  subtype String_Device_String_Length is String(1 .. Device_String_Length);

  package Char_IO is
    new Ada.Sequential_IO(String_Device_String_Length);
  use Char_IO;

  -----------------------------------------------------------------------------
  procedure Open(Audio_Device : in out Char_IO.File_Type) is

    Found_Audio_Device : Boolean := False;
    All_Devices        : array (1 .. 2) of String(1 .. 30) :=
                           (("/tmp/SUNWut/dev/utaudio/      "),
			    ("/dev/sound/                   "));

  begin
    for J in All_Devices'Range loop
      for I in 0 .. 100 loop
        begin
          Open(Audio_Device, Out_File,
               Trim(All_Devices(J), Both) & Trim(Integer'Image(I), Both));
          Found_Audio_Device := True;
--            Put_Line("Audiodevice = " &
--                     Trim(All_Devices(J), Both) & Trim(Integer'Image(I), Both));
          exit;
        exception
           when others =>
             null;
        end;
      end loop;
      exit when Found_Audio_Device;
    end loop;

    if not Found_Audio_Device then
      Put_Line("Audiodevice not found. Aborting ...");
      raise Audio_Device_Error;
    end if;
  end Open;

  -----------------------------------------------------------------------------
  procedure Internal_Play(Audio : in Char_IO.File_Type;
                          Item  : in String) is

    S : String(1 .. Device_String_Length);

  begin
    S := (others => Ascii_To_Character(0));
    S(1 .. Item'Length) := Item;
    Write(Audio, S);
  end Internal_Play;

  -----------------------------------------------------------------------------
  procedure Play(Audio : in Char_IO.File_Type;
                 Item  : in String) is

  begin
    if Item'Length <= Device_String_Length then
      Internal_Play(Audio, Item);
    else
      Internal_Play(Audio, Item(Item'First ..
                                Item'First + Device_String_Length - 1));
      Play(Audio, Item(Item'First + Device_String_Length .. Item'Last));
    end if;
  end Play;

  -----------------------------------------------------------------------------
  type Melody_Type is
    array (Positive range <>) of Tone_Type;

  Null_Melody : constant Melody_Type(1 .. 1) := (1 => (Pause, 0, 7, False));

  -----------------------------------------------------------------------------
  function "&"(Left  : in Tone_Type;
               Right : in Melody_Type) return Melody_Type is

    Res : Melody_Type(1 .. Right'Length + 1);

  begin
    Res(1) := Left;
    Res(2 .. Res'Last) := Right;
    return Res;
  end "&";

  -----------------------------------------------------------------------------
  function To_Melody_Type(Item         : in String;
                          Start_Octave : in Natural := 4;
                          Start_Length : in Natural := 3) return Melody_Type is

    Octave : Natural  := Start_Octave;
    Length : Natural  := Start_Length;
    Start  : Positive := Item'First;

  begin
    if Item'Length > 0 then
      for I in Item'Range loop
        if Item(I) = 'O' then
          case Item(I + 1) is
            when '+'        => Octave := Octave + 1;
            when '-'        => Octave := Octave - 1;
            when '0' .. '7' => Octave := To_Integer(Item(I + 1));
            when others     => raise Octave_Error;
          end case;
          Start := Start + 2;
        elsif Item(I) = 'L' then
          case Item(I + 1) is
            when '-'        => Length := Length + 1;
            when '+'        => Length := Length - 1;
            when '0' .. '7' => Length := To_Integer(Item(I + 1));
            when others     => raise Tone_Length_Error;
          end case;
          Start := Start + 2;
        elsif Item(I) in 'A' .. 'H' or else Item(I) = ' ' then
          if I = Item'Last then
            return To_Tone_Type(Item(Start .. I), Octave, Length) & Null_Melody;
          else
            return (To_Tone_Type(Item(Start .. I), Octave, Length) &
                    To_Melody_Type(Item(I + 1 .. Item'Last), Octave, Length));
          end if;
        end if;
      end loop;
    end if;
    return Null_Melody;
  end To_Melody_Type;

  -----------------------------------------------------------------------------
  function To_Sound(Item : in Melody_Type) return String is

    U_Str : Unbounded_String := Null_Unbounded_String;

  begin
    for Tone in Item'Range loop
      Append(U_Str, Create_Sound(Item(Tone)));
    end loop;
    return To_String(U_Str);
  end To_Sound;

  -----------------------------------------------------------------------------
  function To_Sound(Item : in String) return String is

  begin
    return To_Sound(To_Melody_Type(Item));
  end To_Sound;

  -----------------------------------------------------------------------------
  procedure Play_Melody(Item : in String) is

    Audio_Device : Char_IO.File_Type;

  begin
    Open(Audio_Device);
    Play(Audio_Device, To_Sound(Item));
    Close(Audio_Device);

  exception
     when Audio_Device_Error =>
       raise;
     when others =>
       Close(Audio_Device);
       raise;
  end Play_Melody;

  -----------------------------------------------------------------------------
  procedure Play_File(Filename : in String) is

    Audio_Device : Char_IO.File_Type;
    F            : Ada.Text_IO.File_Type;
    Item         : String(1 .. Device_String_Length);
    Len          : Integer;
    
  begin
    Open(F, In_File, Filename);
    Open(Audio_Device);
    while not End_Of_File(F) loop
      Get_Line(F, Item, Len);
      Play(Audio_Device, Item(1 .. Len));
    end loop;
    Close(Audio_Device);
    Close(F);

  exception
     when Audio_Device_Error =>
       raise;
     when Ada.Text_IO.Name_Error =>
       raise;
     when others =>
       Close(Audio_Device);
       Close(F);
       raise;
  end Play_File;

  -----------------------------------------------------------------------------

end TJa.Sound;
