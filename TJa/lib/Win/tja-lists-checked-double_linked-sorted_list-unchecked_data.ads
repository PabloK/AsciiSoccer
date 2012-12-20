-------------------------------------------------------------------------------
--|                                                                         |--
--|                       Torbj�rn Jonsson Ada library                      |--
--|                                                                         |--
--|      T J A . L I S T S . C H E C K E D . D O U B L E _ L I N K E D      |--
--|          . S O R T E D _ L I S T . U N C H E C K E D _ D A T A          |--
--|                                                                         |--
--|                              Specification                              |--
--|                              Version  3.00                              |--
--|                                                                         |--
--|                              (C) Copyright                              |--
--|                   Torbj�rn Jonsson,  TorJo@Ida.LiU.se                   |--
--|                                                                         |--
-------------------------------------------------------------------------------
--|                                                                         |--
--| Versions:                                                               |--
--|                                                                         |--
--|   2000-12-01  Version 3.00 is ok.                                       |--
--|               Created and documented by Torbj�rn Jonsson.               |--
--|                                                                         |--
-------------------------------------------------------------------------------
--|                                                                         |--
--| Description:                                                            |--
--|                                                                         |--
--|   This package includes type and methods for a sorted double linked     |--
--|   list where the data inserted is the copied data (the calling program  |--
--|   must send a 'Copy'-function to handle the copying of data). This list |--
--|   package should be used if the data in list is pointers to dynamic     |--
--|   memory and the data should not be accessible from calling program.    |--
--|                                                                         |--
-------------------------------------------------------------------------------

-- Ada standard libraries.
with Ada.Finalization;                  use Ada.Finalization;

-- External libraries.

-- Internal libraries.
with TJa.Lists.Internal.Double_Linked;
with TJa.Misc;                          use TJa.Misc;

-------------------------------------------------------------------------------
generic

  -- List consists of "Data_Type".
  type Data_Type is private;

  -- We need a key type if we want to search after data or sort lists.
  type Key_Type is private;

  -- We need a function which copies the data (needed for dynamic data).
  with function Copy(Data : in Data_Type) return Data_Type is <>;

  -- We need a procedure which deletes the data (needed for dynamic data).
  with procedure Delete(Data : in out Data_Type) is <>;

  -- We need a function for extracting the key from a data.
  with function Get_Key(Data : in Data_Type) return Key_Type is <>;

  -- We need some relation operators for the key type.
  with function "="(Left, Right : in Key_Type) return Boolean is <>;
  with function "<"(Left, Right : in Key_Type) return Boolean is <>;

package TJa.Lists.Checked.Double_Linked.Sorted_List.Unchecked_Data is

  -----------------------------------------------------------------------------
  --| "List_Type" is the type for a list. The value "Null_List" is used to
  --| identify that the list is "empty".
  -----------------------------------------------------------------------------

  type List_Type is private;

  Null_List : constant List_Type;

  -----------------------------------------------------------------------------
  --| "Empty" equals 'True' if there are no elements in list.
  -----------------------------------------------------------------------------

  function Empty(List : in List_Type) return Boolean;

  pragma Inline(Empty);

  -----------------------------------------------------------------------------
  --| "Length" returns number of elements in list.
  -----------------------------------------------------------------------------

  function Length(List : in List_Type) return Natural;

  pragma Inline(Length);

  -----------------------------------------------------------------------------
  --| "Delete" is used for deleting some or all elements from the list.
  --|
  --| If the list is the only parameter the whole list is deleted.
  --|
  --| If the second parameter is Index this is the index in the list which
  --| shall be deleted.
  --|
  --| The other case deletes all the elements from "index" 'Low' to 'High'
  --| (including 'Low' and 'High'). If 'High = 0' then all list from 'Low' to
  --| the end is deleted.
  --|
  --| If the functions are used a copy of the list is made and the items are
  --| deleted from the copy and returned.
  --|
  --| "List_Constraint_Error" is raised if illegal "indexes" are given.
  -----------------------------------------------------------------------------

  procedure Delete(List : in out List_Type);
  procedure Delete(List  : in out List_Type;
                   Index : in     Positive);
  procedure Delete(List : in out List_Type;
                   Low  : in     Positive;
                   High : in     Natural);

  function Delete(List  : in List_Type;
                  Index : in Positive) return List_Type;
  function Delete(List : in List_Type;
                  Low  : in Positive;
                  High : in Natural) return List_Type;

  pragma Inline(Delete);

  -----------------------------------------------------------------------------
  --| "Element" returns the data at 'Index' from list. The list is unchanged.
  --|
  --| "List_Constraint_Error" is raised if 'Index > Length(List)'.
  -----------------------------------------------------------------------------

  function Element(List  : in List_Type;
                   Index : in Positive) return Data_Type;

  pragma Inline(Element);

  -----------------------------------------------------------------------------
  --| "Copy" returns a copy of list. The list is unchanged.
  -----------------------------------------------------------------------------

  function Copy(List : in List_Type) return List_Type;

  pragma Inline(Copy);

  -----------------------------------------------------------------------------
  --| "Slice" returns a slice of list. The list is unchanged. A slice is all
  --| the elements from "index" 'Low' to 'High' (including 'Low' and 'High').
  --| If 'High = 0' then all list from 'Low' to the end is returned.
  --|
  --| "List_Constraint_Error" is raised if 'Low > Length(List)' or if
  --| 'High > Length(List)'.
  --|
  --| "List_Data_Error" is raised if 'Low > High' and 'High /= 0'.
  -----------------------------------------------------------------------------

  function Slice(List : in List_Type;
                 Low  : in Positive;
                 High : in Natural) return List_Type;

  pragma Inline(Slice);

  -----------------------------------------------------------------------------
  --| "Insert" is used to insert a new item sorted into list.
  --|
  --| If the functions are used a copy of the list is made and the new item is
  --| inserted in the copy and returned.
  --|
  --| "List_Data_Error" is raised if 'Empty(New_Item)' (in the cases when
  --| 'New_Item' is an 'List_Type'.
  -----------------------------------------------------------------------------

  procedure Insert(List     : in out List_Type;
                   New_Item : in     List_Type);
  procedure Insert(List     : in out List_Type;
                   New_Item : in     Data_Type);

  function Insert(List     : in List_Type;
                  New_Item : in List_Type) return List_Type;
  function Insert(List     : in List_Type;
                  New_Item : in Data_Type) return List_Type;

  pragma Inline(Insert);

  -----------------------------------------------------------------------------
  --| "Member" searches the list for an element and returns 'True' if it was
  --| found, otherwise 'False'.
  -----------------------------------------------------------------------------

  function Member(List : in List_Type;
                  Key  : in Key_Type) return Boolean;

  pragma Inline(Member);

  -----------------------------------------------------------------------------
  --| "Find" searches the list for an element with a specific key and returns
  --| the data if it was found, otherwise 'List_Constraint_Error' is raised.
  -----------------------------------------------------------------------------

  function Find(List : in List_Type;
                Key  : in Key_Type) return Data_Type;

  pragma Inline(Find);

  -----------------------------------------------------------------------------
  --| "Index" searches the index in list where the data has a specific key. If
  --| the data isn't found "Index" returns 0.
  -----------------------------------------------------------------------------

  function Index(List : in List_Type;
                 Key  : in Key_Type) return Natural;

  pragma Inline(Index);

  -----------------------------------------------------------------------------
  --| Operators for comparing lists ("/=" implicit from "=" in Ada).
  --|
  --| "=" returns 'True' only if all data in the lists are identical.
  --|
  --| If lists are not equal and all data are equal to the point that one of
  --| the lists ends the shortest list is less than the other.
  --|
  --| The result 'True'/'False' from the comparision of the first differing
  --| data in the lists are the result for the comparision of the lists.
  -----------------------------------------------------------------------------

  function "="(Left, Right : in List_Type) return Boolean;
  function "<"(Left, Right : in List_Type) return Boolean;
  function ">"(Left, Right : in List_Type) return Boolean;
  function "<="(Left, Right : in List_Type) return Boolean;
  function ">="(Left, Right : in List_Type) return Boolean;

  pragma Inline("=");
  pragma Inline("<");
  pragma Inline(">");
  pragma Inline("<=");
  pragma Inline(">=");

  -----------------------------------------------------------------------------
  --| "Compare" returns:
  --|   - 'Less' if first list is less than the second.
  --|   - 'Equal' if both lists are equal.
  --|   - 'Greater' if first list is greater than the second.
  -----------------------------------------------------------------------------

  function Compare(Left, Right : in List_Type) return Relation_Type;

  pragma Inline(Compare);

  -----------------------------------------------------------------------------
  --| "Subprogram access types" and "Iterators". Used for manipulating or just
  --| accessing some or all of the data in list.
  --|
  --| "Iterate" are the methods for accessing all data in list (one at the
  --| time) and the subprogram that uses (the "Action" parameter) the data must
  --| be of the "subprogram access type". Use "Procedure_Name'Access" to get
  --| the "subprogram access type" in the calling program.
  -----------------------------------------------------------------------------

  type Action_Procedure_In_Mode is
    access procedure(Data : in Data_Type);

  -----------------------------------------------------------------------------
  procedure Iterate(List   : in List_Type;
                    Action : in Action_Procedure_In_Mode);

  pragma Inline(Iterate);

  -----------------------------------------------------------------------------

private

  -----------------------------------------------------------------------------
  --| Internal representation of "List_Type". We use the internal library
  --| "TJa.Lists.Internal".
  -----------------------------------------------------------------------------

  -- Needed for instansiation, but not for this package.
  type Action_Procedure_In_Out_Mode is
    access procedure(Data : in out Data_Type);

  type Action_Function is
    access function(Data : in Data_Type) return Data_Type;

  -----------------------------------------------------------------------------
  package Internal_List is
    new TJa.Lists.Internal.Double_Linked
      (Data_Type                    => Data_Type,
       Action_Procedure_In_Mode     => Action_Procedure_In_Mode,
       Action_Procedure_In_Out_Mode => Action_Procedure_In_Out_Mode,
       Action_Function              => Action_Function,
       Copy                         => Copy,
       Delete                       => Delete);

  -----------------------------------------------------------------------------
  type List_Type is
    new Controlled with record
      List : Internal_List.List_Type;
    end record;

  -----------------------------------------------------------------------------
  --| The "List_Type" is a safe list type. If an list object isn't deleted
  --| before it's destroyed the finalization library is used to do that.
  -----------------------------------------------------------------------------

  procedure Initialize(List : in out List_Type);
  procedure Adjust(List : in out List_Type);
  procedure Finalize(List : in out List_Type);

  -----------------------------------------------------------------------------
  --| The constant that represents "empty" list.
  -----------------------------------------------------------------------------

  Null_List : constant List_Type := (Controlled with
                                     List => Internal_List.Null_List);

  -----------------------------------------------------------------------------

end TJa.Lists.Checked.Double_Linked.Sorted_List.Unchecked_Data;