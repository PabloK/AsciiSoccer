-------------------------------------------------------------------------------
--|                                                                         |--
--|                       Torbjörn Jonsson Ada library                      |--
--|                                                                         |--
--|                          T J A . F I L E _ I O                          |--
--|                                                                         |--
--|                           Body implementation                           |--
--|                              Version  3.00                              |--
--|                                                                         |--
--|                              (C) Copyright                              |--
--|                   Torbjörn Jonsson,  TorJo@Ida.LiU.se                   |--
--|                                                                         |--
-------------------------------------------------------------------------------
--|                                                                         |--
--| Change log:                                                             |--
--|                                                                         |--
--|   2000-12-11  Version 3.00 is ok.                                       |--
--|               Created and documented by Torbjörn Jonsson.               |--
--|                                                                         |--
-------------------------------------------------------------------------------
--|                                                                         |--
--| Implementation details:                                                 |--
--|                                                                         |--
-------------------------------------------------------------------------------

-- Ada standard libraries.
with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;

-- External libraries.
with Gnat.OS_Lib;                       use Gnat.OS_Lib;
with Gnat.Directory_Operations;         use Gnat.Directory_Operations;
with Gnat.Regexp;                       use Gnat.Regexp;

-- Internal libraries.
with TJa.Lists.Checked.Double_Linked.Unsorted_List.Checked_Data;

-------------------------------------------------------------------------------
package body TJa.File_IO is

  --***************************************************************************
  --| Declarations of local types, constants and methods.
  --***************************************************************************

  -- Used for "Copy" and "Move".
  function To_String_Access(S : in String) return Gnat.OS_Lib.String_Access;
  function To_String_Access(S : in Unbounded_String)
           return Gnat.OS_Lib.String_Access;


  --***************************************************************************
  --| Definitions of public methods.
  --***************************************************************************

  -----------------------------------------------------------------------------
  --| File_Exists
  -----------------------------------------------------------------------------

  function File_Exists(Name : in String;
                       Path : in String := "")
           return Boolean is

  begin
    if Path = "" then
      return Is_Regular_File(Name);
    elsif Path(Path'Last) = Directory_Separator then
      return Is_Regular_File(Path & Name);
    end if;
    return Is_Regular_File(To_String(To_Unbounded_String(Path) &
                                     Directory_Separator & Name));
  end File_Exists;
  -----------------------------------------------------------------------------
  function File_Exists(Name : in String;
                       Path : in Unbounded_String)
           return Boolean is

  begin
    return File_Exists(To_String(path), Name);
  end File_Exists;
  -----------------------------------------------------------------------------
  function File_Exists(Name : in Unbounded_String;
                       Path : in String := "")
           return Boolean is

  begin
    return File_Exists(To_String(Name), Path);
  end File_Exists;
  -----------------------------------------------------------------------------
  function File_Exists(Name : in Unbounded_String;
                       Path : in Unbounded_String)
           return Boolean is

  begin
    return File_Exists(To_String(Name), To_String(Path));
  end File_Exists;

  -----------------------------------------------------------------------------
  --| Directory_Exists
  -----------------------------------------------------------------------------

  function Directory_Exists(Name : in String;
                            Path : in String := "")
           return Boolean is

  begin
    if Path = "" then
      return Is_Directory(Name);
    elsif Path(Path'Last) = Directory_Separator then
      return Is_Regular_File(Path & Name);
    end if;
    return Is_Directory(To_String(To_Unbounded_String(Path) &
                                  Directory_Separator & Name));
  end Directory_Exists;
  -----------------------------------------------------------------------------
  function Directory_Exists(Name : in String;
                            Path : in Unbounded_String)
           return Boolean is

  begin
    return Directory_Exists(To_String(path), Name);
  end Directory_Exists;
  -----------------------------------------------------------------------------
  function Directory_Exists(Name : in Unbounded_String;
                            Path : in String := "")
           return Boolean is

  begin
    return Directory_Exists(To_String(Name), Path);
  end Directory_Exists;
  -----------------------------------------------------------------------------
  function Directory_Exists(Name : in Unbounded_String;
                            Path : in Unbounded_String)
           return Boolean is

  begin
    return Directory_Exists(To_String(Name), To_String(Path));
  end Directory_Exists;

  -----------------------------------------------------------------------------
  --| Create
  -----------------------------------------------------------------------------

  procedure Create(Directory_Name : in String) is

    Args : Argument_List(1..1);
    Ok   : Boolean;

  begin
    if Directory_Name = "" then
      raise File_IO_Create_Error;
    end if;

    -- Make argument list for system command "mkdir" (Unix).
    Args := (1 => To_String_Access(Directory_Name));

    -- Copy file.
    Spawn("/usr/bin/mkdir", Args, Ok);
    if not Ok then
      raise File_IO_Create_Error;
    end if;
  end Create;
  -----------------------------------------------------------------------------
  procedure Create(Directory_Name : in Unbounded_String) is

  begin
    Create(To_String(Directory_Name));
  end Create;

  -----------------------------------------------------------------------------
  --| Copy
  -----------------------------------------------------------------------------

  procedure Copy(From : in String;
                 To   : in String) is

    Args : Argument_List(1..2);
    Ok   : Boolean;

  begin
    if not File_Exists(From, "") then
      raise File_IO_Name_Error;
    end if;

    -- Make argument list for system command "cp" (Unix).
    Args := (1 => To_String_Access(From),
             2 => To_String_Access(To));

    -- Copy file.
    Spawn("/usr/bin/cp", Args, Ok);
    if not Ok then
      raise File_IO_Internal_Error;
    end if;
  end Copy;
  -----------------------------------------------------------------------------
  procedure Copy(From : in String;
                 To   : in Unbounded_String) is

  begin
    Copy(From, To_String(To));
  end Copy;
  -----------------------------------------------------------------------------
  procedure Copy(From : in Unbounded_String;
                 To   : in String) is

  begin
    Copy(To_String(From), To);
  end Copy;
  -----------------------------------------------------------------------------
  procedure Copy(From : in Unbounded_String;
                 To   : in Unbounded_String) is

  begin
    Copy(To_String(From), To_String(To));
  end Copy;

  -----------------------------------------------------------------------------
  --| Move
  -----------------------------------------------------------------------------

  procedure Move(From : in String;
                 To   : in String) is

    Args : Argument_List(1..2);
    Ok   : Boolean;

  begin
    if not File_Exists(From, "") then
      raise File_IO_Name_Error;
    end if;

    -- Make argument list for system command "mv" (Unix).
    Args := (1 => To_String_Access(From),
             2 => To_String_Access(To));

    -- Move file.
    Spawn("/usr/bin/mv", Args, Ok);
    if not Ok then
      raise File_IO_Internal_Error;
    end if;
  end Move;
  -----------------------------------------------------------------------------
  procedure Move(From : in String;
                 To   : in Unbounded_String) is

  begin
    Move(From, To_String(To));
  end Move;
  -----------------------------------------------------------------------------
  procedure Move(From : in Unbounded_String;
                 To   : in String) is

  begin
    Move(To_String(From), To);
  end Move;
  -----------------------------------------------------------------------------
  procedure Move(From : in Unbounded_String;
                 To   : in Unbounded_String) is

  begin
    Move(To_String(From), To_String(To));
  end Move;

  -----------------------------------------------------------------------------
  --| Copy_Between_Systems
  -----------------------------------------------------------------------------

  procedure Copy_Between_Systems(From : in String;
                                 To   : in String) is

    Args : Argument_List(1..2);
    Ok   : Boolean;

  begin
    -- Make argument list for system command "scp" (Unix).
    Args := (1 => To_String_Access(From),
             2 => To_String_Access(To));

    -- Copy file.
    Spawn("/usr/bin/scp", Args, Ok);
    if not Ok then
      raise File_IO_Name_Error;
    end if;
  end Copy_Between_Systems;
  -----------------------------------------------------------------------------
  procedure Copy_Between_Systems(From : in String;
                                 To   : in Unbounded_String) is

  begin
    Copy_Between_Systems(From, To_String(To));
  end Copy_Between_Systems;
  -----------------------------------------------------------------------------
  procedure Copy_Between_Systems(From : in Unbounded_String;
                                 To   : in String) is

  begin
    Copy_Between_Systems(To_String(From), To);
  end Copy_Between_Systems;
  -----------------------------------------------------------------------------
  procedure Copy_Between_Systems(From : in Unbounded_String;
                                 To   : in Unbounded_String) is

  begin
    Copy_Between_Systems(To_String(From), To_String(To));
  end Copy_Between_Systems;

  -----------------------------------------------------------------------------
  --| Is_File
  -----------------------------------------------------------------------------

  function Is_File(Name : in String)
           return Boolean is

  begin
    return Gnat.OS_Lib.Is_Regular_File(Name);
  end Is_File;
  -----------------------------------------------------------------------------
  function Is_File(Name : in Unbounded_String)
           return Boolean is

  begin
    return Is_File(To_String(Name));
  end Is_File;

  -----------------------------------------------------------------------------
  --| Is_Directory
  -----------------------------------------------------------------------------

  function Is_Directory(Name : in String)
           return Boolean is

  begin
    return Gnat.OS_Lib.Is_Directory(Name);
  end Is_Directory;
  -----------------------------------------------------------------------------
  function Is_Directory(Name : in Unbounded_String)
           return Boolean is

  begin
    return Is_Directory(To_String(Name));
  end Is_Directory;

  -----------------------------------------------------------------------------
  --| Make_Directory_List
  --|
  --| Procedure definitions.
  -----------------------------------------------------------------------------

  procedure Make_Directory_List(Directory_List :    out Directory_List_Type;
                                Directory_Name : in     String := "";
                                Reg_Exp        : in     String := "*") is

  begin
    Directory_List := Make_Directory_List(Directory_Name => Directory_Name,
                                          Reg_Exp        => Reg_Exp);
  end Make_Directory_List;
  -----------------------------------------------------------------------------
  procedure Make_Directory_List(Directory_List :    out Directory_List_Type;
                                Directory_Name : in     String;
                                Reg_Exp        : in     Unbounded_String) is

  begin
    Make_Directory_List(Directory_Name => Directory_Name,
                        Reg_Exp        => To_String(Reg_Exp),
                        Directory_List => Directory_List);
  end Make_Directory_List;
  -----------------------------------------------------------------------------
  procedure Make_Directory_List(Directory_List :    out Directory_List_Type;
                                Directory_Name : in     Unbounded_String;
                                Reg_Exp        : in     String) is

  begin
    Make_Directory_List(Directory_Name => To_String(Directory_Name),
                        Reg_Exp        => Reg_Exp,
                        Directory_List => Directory_List);
  end Make_Directory_List;
  -----------------------------------------------------------------------------
  procedure Make_Directory_List(Directory_List :    out Directory_List_Type;
                                Directory_Name : in     Unbounded_String :=
                                  Null_Unbounded_String;
                                Reg_Exp        : in     Unbounded_String :=
                                  To_Unbounded_String("*")) is

  begin
    Make_Directory_List(Directory_Name => To_String(Directory_Name),
                        Reg_Exp        => To_String(Reg_Exp),
                        Directory_List => Directory_List);
  end Make_Directory_List;

  -----------------------------------------------------------------------------
  --| Make_Directory_List
  --|
  --| Function definitions.
  -----------------------------------------------------------------------------

  function Make_Directory_List(Directory_Name : in String := "";
                               Reg_Exp        : in String := "*")
           return Directory_List_Type is

    -- 'Glob => True' makes "normal" directory regexp.
    Filename_Reg_Exp : Regexp := Compile(Reg_Exp, Glob => True);

    -- The list which will be returned.
    Dir_List : Directory_List_Type := Null_Directory_List;

    Directory : Dir_Type;
    Filename  : String(1..256);  -- Max 256 characters in a filname. :-(
    Last      : Natural;

  begin
    if ((Directory_Name /= "") and then
        not Directory_Exists(Directory_Name, "")) then
      raise File_IO_Name_Error;
    end if;

    if Directory_Name = "" then
      Open(Directory, ".");
    else
      Open(Directory, Directory_Name);
    end if;

    loop
      Read(Directory, Filename, Last);
      exit when Last = 0;

      -- If filename longer than 255 characters we take this as an error.
      if Last = 256 then
        raise File_IO_To_Long_Name_Error;
      end if;

      if Match(Filename(1..Last), Filename_Reg_Exp) then
        Insert_Sorted(Dir_List, To_Unbounded_String(Filename(1..Last)));
      end if;
    end loop;

    Close(Directory);

    return Dir_List;
  end Make_Directory_List;
  -----------------------------------------------------------------------------
  function Make_Directory_List(Directory_Name : in String;
                               Reg_Exp        : in Unbounded_String)
           return Directory_List_Type is

  begin
    return Make_Directory_List(Directory_Name => Directory_Name,
                               Reg_Exp        => To_String(Reg_Exp));
  end Make_Directory_List;
  -----------------------------------------------------------------------------
  function Make_Directory_List(Directory_Name : in Unbounded_String;
                               Reg_Exp        : in String)
           return Directory_List_Type is

  begin
    return Make_Directory_List(Directory_Name => To_String(Directory_Name),
                               Reg_Exp        => Reg_Exp);
  end Make_Directory_List;
  -----------------------------------------------------------------------------
  function Make_Directory_List(Directory_Name : in Unbounded_String :=
                                 Null_Unbounded_String;
                               Reg_Exp        : in Unbounded_String :=
                                 To_Unbounded_String("*"))
           return Directory_List_Type is

  begin
    return Make_Directory_List(Directory_Name => To_String(Directory_Name),
                               Reg_Exp        => To_String(Reg_Exp));
  end Make_Directory_List;

  -----------------------------------------------------------------------------
  --| Empty
  -----------------------------------------------------------------------------

  function Empty(Directory_List : in Directory_List_Type)
           return Boolean is

  begin
    return (No_Of_Files(Directory_List) = 0);
  end Empty;

  -----------------------------------------------------------------------------
  --| No_Of_Files
  -----------------------------------------------------------------------------

  function No_Of_Files(Directory_List : in Directory_List_Type)
           return Natural is

  begin
    return Length(Directory_List);
  end No_Of_Files;

  -----------------------------------------------------------------------------
  --| Get_Filename
  -----------------------------------------------------------------------------

  function Get_Filename(Directory_List : in Directory_List_Type;
                        Index          : in Positive)
           return String is

  begin
    if Index > No_Of_Files(Directory_List) then
        raise File_IO_Index_Error;
    end if;

    return To_String(Element(Directory_List, Index));
  end Get_Filename;

  -----------------------------------------------------------------------------
  -- To_Text_Type, To_Directory_List_Type
  -----------------------------------------------------------------------------

--    function To_Text_Type(Item : in Directory_List_Type)
--             return Text_Type is

--      Temp : Text_Type := Null_Text;

--    begin
--      for I in 1..No_Of_Files(Item) loop
--        Temp := Temp & Element(Item, I);
--      end loop;

--      return Temp;
--    end To_Text_Type;

--    --===========================================================================
--    function To_Directory_List_Type(Item : in Text_Type)
--             return Directory_List_Type is

--      Temp : Directory_List_Type := Null_Directory_List;

--    begin
--      for I in 1..No_Of_Rows(Item) loop
--        Temp := Temp & Row(Item, I);
--      end loop;

--      return Temp;
--    end To_Directory_List_Type;

  -----------------------------------------------------------------------------
  --| Relation operators: "=", "<", ">", "<=", ">="
  -----------------------------------------------------------------------------

--    function "="(Left, Right : in Directory_List_Type) return Boolean is

--    begin
--      return List_Type(Left) = List_Type(Right);
--    end "=";
--    -----------------------------------------------------------------------------
--    function "<"(Left, Right : in Directory_List_Type) return Boolean is

--    begin
--      return List_Type(Left) < List_Type(Right);
--    end "<";
--    -----------------------------------------------------------------------------
--    function ">"(Left, Right : in Directory_List_Type) return Boolean is

--    begin
--      return (Right < Left);
--    end ">";
--    -----------------------------------------------------------------------------
--    function "<="(Left, Right : in Directory_List_Type) return Boolean is

--    begin
--      return not (Right < Left);
--    end "<=";
--    -----------------------------------------------------------------------------
--    function ">="(Left, Right : in Directory_List_Type) return Boolean is

--    begin
--      return not (Left < Right);
--    end ">=";


  --***************************************************************************
  --| Definitions of local methods.
  --***************************************************************************

  -----------------------------------------------------------------------------
  --| To_String_Access
  --|
  --| We need this function(s) to convert from 'String' / 'Unbounded_String' to
  --| 'Gnat.OS_Lib.String_Access' for use in the methods "Copy" and "Move".
  -----------------------------------------------------------------------------

  function To_String_Access(S : in String)
           return Gnat.OS_Lib.String_Access is

    Sp : Gnat.OS_Lib.String_Access;

  begin
    Sp := new String(S'Range);
    Sp.All := S;

    return Sp;
  end To_String_Access;
  -----------------------------------------------------------------------------
  function To_String_Access(S : in Unbounded_String)
           return Gnat.OS_Lib.String_Access is

  begin
    return To_String_Access(To_String(S));
  end To_String_Access;

  -----------------------------------------------------------------------------
  --| Get_Key / Put  (needed for instansiation of TJa.Lists...)
  -----------------------------------------------------------------------------

  function Get_Key(Data : in Unbounded_String) return Unbounded_String is

  begin
    return Data;
  end Get_Key;

  -----------------------------------------------------------------------------
--    procedure Put(File : in File_Type;
--                  Item : in Unbounded_String) is

--    begin
--      Put(File, To_String(Item));
--    end Put;

  --***************************************************************************
  --| Definition of initiation part.
  --***************************************************************************

begin
  -- Nothing have to be done.
  null;
end TJa.File_IO;
