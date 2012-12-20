package body Ena.Ansi.Picture.Capa is


   procedure Compress(Namn : in String) is

      Source,Target : File_Type;
      Cnt : Integer := 0;
      C : Character;

   begin

      Put_Line(Namn);

      Open(Source, In_File, Namn & ".apa");
      Create(Target, Out_File, Namn & ".capa");

      Put_Line(Target,"Capa, the ultimate compression, by GOD");
      for I in 1..2 loop
         while not End_Of_Line(Source) loop
            Get(Source,C);
            Put(Target,C);
         end loop;
         Skip_Line(Source);
         New_Line(Target);
      end loop;

      while not End_Of_File(Source) loop
         Cnt := 0;
         Get(Source, C);
         while C = '-' and not End_Of_File(Source) loop
               Cnt := Cnt + 1;
               Get(Source,C);
         end loop;

         if Cnt > 0 and End_Of_File(Source) then
            Put(Target, "G" & Trim(Integer'Image(Cnt+1),Both) & "H");
         elsif Cnt > 0 then
            Put(Target, "G" & Trim(Integer'Image(Cnt),Both) & "H");
         end if;

         if End_Of_File(Source) then
            exit;
         end if;

         Put(Target,C);
         Get(Source,C);
         Put(Target,C);
      end loop;



      Close(Source);
      Close(Target);

   end Compress;

   procedure Decompress(Namn : in String) is

      Source,Target : File_Type;
      Cnt : Integer := 0;
      C : Character;

   begin

      Open(Source, In_File, Namn & ".capa");
      Create(Target, Out_File, Namn & ".apa");

      Skip_Line(Source);
      for I in 1..2 loop
         while not End_Of_Line(Source) loop
            Get(Source,C);
            Put(Target,C);
         end loop;
         Skip_Line(Source);
         New_Line(Target);
      end loop;


      while not End_Of_File(Source) loop
         Get(Source, C);

         if C = 'G' then
            Get(Source, Cnt);
            for I in 1..Cnt loop
               Put(Target, '-');
            end loop;
            Get(Source, C);
         else
            Put(Target, C);
            Get(Source, C);
            Put(Target, C);
         end if;
      end loop;

      New_Line(Target);

      Close(Source);
      Close(Target);

   end Decompress;


end Ena.Ansi.Picture.Capa;
