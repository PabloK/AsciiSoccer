-------------------------------------------------------------------------------
--|                                                                         |--
--|                       Torbjörn Jonsson Ada library                      |--
--|                                                                         |--
--|                          T J A . F I L E _ I O                          |--
--|                                                                         |--
--|                              Specification                              |--
--|                              Version  3.00                              |--
--|                                                                         |--
--|                              (C) Copyright                              |--
--|                   Torbjörn Jonsson,  TorJo@Ida.LiU.se                   |--
--|                                                                         |--
-------------------------------------------------------------------------------
--|                                                                         |--
--| Versions:                                                               |--
--|                                                                         |--
--|   2000-12-11  Version 3.00 is ok.                                       |--
--|               Created and documented by Torbjörn Jonsson.               |--
--|                                                                         |--
-------------------------------------------------------------------------------
--|                                                                         |--
--| Description:                                                            |--
--|                                                                         |--
--|   This library is a complement to the Ada standard libraries for        |--
--|   handling files.                                                       |--
--|                                                                         |--
-------------------------------------------------------------------------------

-- Ada standard libraries.
with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;

-- External libraries.

-- Internal libraries.
--  with TJa.Text;                          use TJa.Text;
with TJa.Lists.Checked.Double_Linked.Unsorted_List.Checked_Data;

-------------------------------------------------------------------------------
package TJa.File_IO is

  -----------------------------------------------------------------------------
  --| "Directory_List_Type" is the type for a directory listing. The value
  --| "Null_Directory_List" are used to identify that the directory list is
  --| "empty".
  -----------------------------------------------------------------------------

  type Directory_List_Type is private;

  Null_Directory_List : constant Directory_List_Type;

  -----------------------------------------------------------------------------
  --| "File_Exists" checks if a file named 'Name' exist in current directory or
  --| in the directory specified by the 'Path'. It's ok to insert a full path
  --| into the filename 'Name'.
  --|
  --| "Directory_Exists" does the same thing as "File_Exists" but for a
  --| directory.
  -----------------------------------------------------------------------------

  function File_Exists(Name : in String;
                       Path : in String := "") return Boolean;
  function File_Exists(Name : in String;
                       Path : in Unbounded_String) return Boolean;
  function File_Exists(Name : in Unbounded_String;
                       Path : in String := "") return Boolean;
  function File_Exists(Name : in Unbounded_String;
                       Path : in Unbounded_String) return Boolean;

  function Directory_Exists(Name : in String;
                            Path : in String := "") return Boolean;
  function Directory_Exists(Name : in String;
                            Path : in Unbounded_String) return Boolean;
  function Directory_Exists(Name : in Unbounded_String;
                            Path : in String := "") return Boolean;
  function Directory_Exists(Name : in Unbounded_String;
                            Path : in Unbounded_String) return Boolean;

  -----------------------------------------------------------------------------
  --| "Create" creates a new directory (including path). If the directory
  --| cant be created (not right permission or something like that) the
  --| exception 'File_IO_Create_Error' will be raised.
  -----------------------------------------------------------------------------

  procedure Create(Directory_Name : in String);
  procedure Create(Directory_Name : in Unbounded_String);

  -----------------------------------------------------------------------------
  --| "Copy" makes a copy of file with filename "From" to a file named "To".
  --| "Move" moves the file with filename "From" to the new filename "To".
  --|
  --| If file named "To" already exists it will be deleted before copy or move
  --| is done.
  --|
  --| If "From" doesn't exist 'File_IO_Name_Error' is rasied.
  --| If action not done properly 'File_IO_Internal_Error' is raised.
  -----------------------------------------------------------------------------

  procedure Copy(From : in String;
                 To   : in String);
  procedure Copy(From : in String;
                 To   : in Unbounded_String);
  procedure Copy(From : in Unbounded_String;
                 To   : in String);
  procedure Copy(From : in Unbounded_String;
                 To   : in Unbounded_String);

  procedure Move(From : in String;
                 To   : in String);
  procedure Move(From : in String;
                 To   : in Unbounded_String);
  procedure Move(From : in Unbounded_String;
                 To   : in String);
  procedure Move(From : in Unbounded_String;
                 To   : in Unbounded_String);

  -----------------------------------------------------------------------------
  --| "Copy_Between_Systems" makes a copy of file with filename "From" to a
  --| file named "To" ("From" and "To" including hostnames and everything).
  --|
  --| If file named "To" already exists it will be deleted before copy or move
  --| is done.
  --|
  --| If "From" doesn't exist or action not done properly 'File_IO_Name_Error'
  --| is rasied.
  -----------------------------------------------------------------------------

  procedure Copy_Between_Systems(From : in String;
                                 To   : in String);
  procedure Copy_Between_Systems(From : in String;
                                 To   : in Unbounded_String);
  procedure Copy_Between_Systems(From : in Unbounded_String;
                                 To   : in String);
  procedure Copy_Between_Systems(From : in Unbounded_String;
                                 To   : in Unbounded_String);

  -----------------------------------------------------------------------------
  --| "File" returns 'True' if "Name" is a regular file.
  --| "Directory" returns 'True' if "Name" is a directory.
  --|
  --| "Name" must include the full path to the file or directory if it's not in
  --| current directory.
  --|
  --| If "Name" doesn't exist 'File_IO_Name_Error' is rasied.
  -----------------------------------------------------------------------------

  function Is_File(Name : in String) return Boolean;
  function Is_File(Name : in Unbounded_String) return Boolean;

  function Is_Directory(Name : in String) return Boolean;
  function Is_Directory(Name : in Unbounded_String) return Boolean;

  -----------------------------------------------------------------------------
  --| "Make_Directory_List" returns the list of filenames and directories in
  --| current directory or directory named "Directory_Name". The filnames
  --| doesn't include full path.
  --|
  --| If 'Reg_Exp' is used the list only includes files matching that regular
  --| expression.
  --|
  --| If "Directory_Name" doesn't exist 'File_IO_Name_Error' is rasied.
  -----------------------------------------------------------------------------

  procedure Make_Directory_List(Directory_List :    out Directory_List_Type;
                                Directory_Name : in     String := "";
                                Reg_Exp        : in     String := "*");
  procedure Make_Directory_List(Directory_List :    out Directory_List_Type;
                                Directory_Name : in     String;
                                Reg_Exp        : in     Unbounded_String);
  procedure Make_Directory_List(Directory_List :    out Directory_List_Type;
                                Directory_Name : in     Unbounded_String;
                                Reg_Exp        : in     String);
  procedure Make_Directory_List(Directory_List :    out Directory_List_Type;
                                Directory_Name : in     Unbounded_String :=
                                  Null_Unbounded_String;
                                Reg_Exp        : in     Unbounded_String :=
                                  To_Unbounded_String("*"));

  function Make_Directory_List(Directory_Name : in String := "";
                               Reg_Exp        : in String := "*")
           return Directory_List_Type;
  function Make_Directory_List(Directory_Name : in String;
                               Reg_Exp        : in Unbounded_String)
           return Directory_List_Type;
  function Make_Directory_List(Directory_Name : in Unbounded_String;
                               Reg_Exp        : in String)
           return Directory_List_Type;
  function Make_Directory_List(Directory_Name : in Unbounded_String :=
                                 Null_Unbounded_String;
                               Reg_Exp        : in Unbounded_String :=
                                 To_Unbounded_String("*"))
           return Directory_List_Type;

  -----------------------------------------------------------------------------
  --| "Empty" equals 'True' if there are no elements in directory list.
  -----------------------------------------------------------------------------

  function Empty(Directory_List : in Directory_List_Type) return Boolean;

  -----------------------------------------------------------------------------
  --| "No_Of_Files" returns the number of files in a directory list.
  -----------------------------------------------------------------------------

  function No_Of_Files(Directory_List : in Directory_List_Type) return Natural;

  -----------------------------------------------------------------------------
  --| "Get_Filename" returns filename number 'Index' from directory list.
  -----------------------------------------------------------------------------

  function Get_Filename(Directory_List : in Directory_List_Type;
                        Index          : in Positive) return String;

  -----------------------------------------------------------------------------
  --| Conversion between "Directory_List_Type" and "Text_Type".
  -----------------------------------------------------------------------------

--    function To_Text_Type(Item : in Directory_List_Type) return Text_Type;
--    function To_Directory_List_Type(Item : in Text_Type)
--             return Directory_List_Type;

  -----------------------------------------------------------------------------
  --| Operators for comparing directory lists ("/=" implicit from "=" in Ada).
  --|
  --| "=" returns 'True' only if all data in the lists are identical.
  --|
  --| If lists are not equal and all data are equal to the point that one of
  --| the lists ends the shortest list is less than the other.
  --|
  --| The result 'True'/'False' from the comparision of the first differing
  --| datas in the lists are the result for the comparision of the lists.
  -----------------------------------------------------------------------------

--    function "="(Left, Right : in Directory_List_Type) return Boolean;
--    function "<"(Left, Right : in Directory_List_Type) return Boolean;
--    function ">"(Left, Right : in Directory_List_Type) return Boolean;
--    function "<="(Left, Right : in Directory_List_Type) return Boolean;
--    function ">="(Left, Right : in Directory_List_Type) return Boolean;

  -----------------------------------------------------------------------------
  --| Special exceptions for "File_IO"-library.
  -----------------------------------------------------------------------------

  File_IO_Internal_Error     : exception;  -- An internal error has occured.
  File_IO_Path_Error         : exception;  -- Path is incorrect in some way.
  File_IO_Create_Error       : exception;  -- Not possible to create directory.
  File_IO_To_Long_Name_Error : exception;  -- Filename to long.
  File_IO_Index_Error        : exception;  -- File number out of bounds.
  File_IO_Name_Error         : exception;  -- File- or directory name doesn't
                                           -- exist.

  -----------------------------------------------------------------------------

private

  -----------------------------------------------------------------------------
  -- We use the list package for the directory list. For instansiation we need
  -- the function "Get_Key" which returns the key value from the item.
  -----------------------------------------------------------------------------

  function Get_Key(Data : in Unbounded_String) return Unbounded_String;

--    procedure Put(File : in File_Type;
--                  Item : in Unbounded_String);

  -----------------------------------------------------------------------------
  package Directory_Lists is
    new TJa.Lists.Checked.Double_Linked.Unsorted_List.Checked_Data
    (Data_Type => Unbounded_String,
     Key_Type  => Unbounded_String);
  use Directory_Lists;

  -----------------------------------------------------------------------------
  -- The directory list type.
  -----------------------------------------------------------------------------

  type Directory_List_Type is new Directory_Lists.List_Type;

  -----------------------------------------------------------------------------
  --| The constant that represents "empty" directory list.
  -----------------------------------------------------------------------------

  Null_Directory_list : constant Directory_List_Type :=
    Directory_List_Type(Directory_Lists.Null_List);

  -----------------------------------------------------------------------------

end TJa.File_IO;
