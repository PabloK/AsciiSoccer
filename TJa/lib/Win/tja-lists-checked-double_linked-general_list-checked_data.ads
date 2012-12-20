-------------------------------------------------------------------------------
--|                                                                         |--
--|                       Torbjörn Jonsson Ada library                      |--
--|                                                                         |--
--|      T J A . L I S T S . C H E C K E D . D O U B L E _ L I N K E D      |--
--|           . G E N E R A L _ L I S T . C H E C K E D _ D A T A           |--
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
--|   2000-11-26  Version 3.00 is ok.                                       |--
--|               Created and documented by Torbjörn Jonsson.               |--
--|                                                                         |--
-------------------------------------------------------------------------------
--|                                                                         |--
--| Description:                                                            |--
--|                                                                         |--
--|   This package includes type and methods for a general double linked    |--
--|   list.                                                                 |--
--|                                                                         |--
-------------------------------------------------------------------------------

-- Ada standard libraries.
with Ada.Finalization;                  use Ada.Finalization;

-- External libraries.

-- Internal libraries.
with TJa.Lists.Internal.Double_Linked;

-------------------------------------------------------------------------------
generic

  -- List consists of "Data_Type".
  type Data_Type is private;

package TJa.Lists.Checked.Double_Linked.General_List.Checked_Data is

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
  --| "Insert" is used to insert a new item into list at a specific position.
  --| If the 'After = 0' the new item is inserted first in list. 'After = 0'
  --| as default (which means that we always inserts elements first if the
  --| 'After'-parameter is ignored in the call.
  --|
  --| If the functions are used a copy of the list is made and the new item is
  --| inserted in the copy and returned.
  --|
  --| "List_Constraint_Error" is raised if 'After > Length(List)'.
  -----------------------------------------------------------------------------

  procedure Insert(List     : in out List_Type;
                   New_Item : in     List_Type;
                   After    : in     Natural := 0);
  procedure Insert(List     : in out List_Type;
                   New_Item : in     Data_Type;
                   After    : in     Natural := 0);

  function Insert(List     : in List_Type;
                  New_Item : in List_Type;
                  After    : in Natural := 0) return List_Type;
  function Insert(List     : in List_Type;
                  New_Item : in Data_Type;
                  After    : in Natural := 0) return List_Type;

  pragma Inline(Insert);

  -----------------------------------------------------------------------------
  --| "Append" is used for inserting new items in the end of a list.
  -----------------------------------------------------------------------------

  procedure Append(List     : in out List_Type;
                   New_Item : in     List_Type);
  procedure Append(List     : in out List_Type;
                   New_Item : in     Data_Type);

  pragma Inline(Append);

  -----------------------------------------------------------------------------
  --| "&" is used for concatenating lists or a list and a data.
  -----------------------------------------------------------------------------

  function "&"(Left, Right : in List_Type) return List_Type;
  function "&"(Left  : in List_Type;
               Right : in Data_Type) return List_Type;
  function "&"(Left  : in Data_Type;
               Right : in List_Type) return List_Type;

  pragma Inline("&");

  -----------------------------------------------------------------------------
  --| "Replace" deletes the element(s) in list referenced by the "index(es)"
  --| and inserts the new item(s) at that location.
  --|
  --| If the functions are used a copy of the list is made and the operation is
  --| done to the copy and then returned.
  --|
  --| "List_Constraint_Error" is raised if illegal "indexes" are given.
  -----------------------------------------------------------------------------

  procedure Replace(List     : in out List_Type;
                    Index    : in     Positive;
                    New_Item : in     List_Type);
  procedure Replace(List     : in out List_Type;
                    Index    : in     Positive;
                    New_Item : in     Data_Type);
  procedure Replace(List     : in out List_Type;
                    Low      : in     Positive;
                    High     : in     Natural;
                    New_Item : in     List_Type);
  procedure Replace(List     : in out List_Type;
                    Low      : in     Positive;
                    High     : in     Natural;
                    New_Item : in     Data_Type);

  function Replace(List     : in List_Type;
                   Index    : in Positive;
                   New_Item : in List_Type) return List_Type;
  function Replace(List     : in List_Type;
                   Index    : in Positive;
                   New_Item : in Data_Type) return List_Type;
  function Replace(List     : in List_Type;
                   Low      : in Positive;
                   High     : in Natural;
                   New_Item : in List_Type) return List_Type;
  function Replace(List     : in List_Type;
                   Low      : in Positive;
                   High     : in Natural;
                   New_Item : in Data_Type) return List_Type;

  pragma Inline(Replace);

  -----------------------------------------------------------------------------
  --| "Turn" reverses the list.
  --|
  --| If the function are used a copy of the list is made and the operation is
  --| done to the copy and then returned.
  -----------------------------------------------------------------------------

  procedure Turn(List : in out List_Type);

  function Turn(List : in List_Type) return List_Type;

  pragma Inline(Turn);

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

  type Action_Procedure_In_Out_Mode is
    access procedure(Data : in out Data_Type);

  type Action_Function is
    access function(Data : in Data_Type) return Data_Type;

  -----------------------------------------------------------------------------

  procedure Iterate(List   : in List_Type;
                    Action : in Action_Procedure_In_Mode);
  procedure Iterate(List   : in List_Type;
                    Action : in Action_Procedure_In_Out_Mode);

  procedure Iterate(List   : in List_Type;
                    Action : in Action_Function);

  pragma Inline(Iterate);

  -----------------------------------------------------------------------------

private

  -----------------------------------------------------------------------------
  --| Internal representation of "List_Type". We use the internal library
  --| "TJa.Lists.Internal".
  -----------------------------------------------------------------------------

  -- These "Dummy" methods does nothing but is needed for instansiation of the
  -- generic package 'TJa.Lists.Internal.Double_Linked'.
  function Copy_Dummy(Item : in Data_Type) return Data_Type;
  procedure Delete_Dummy(Item : in out Data_Type);

  pragma Inline(Copy_Dummy);
  pragma Inline(Delete_Dummy);

  -----------------------------------------------------------------------------
  package Internal_List is
    new TJa.Lists.Internal.Double_Linked
      (Data_Type                    => Data_Type,
       Action_Procedure_In_Mode     => Action_Procedure_In_Mode,
       Action_Procedure_In_Out_Mode => Action_Procedure_In_Out_Mode,
       Action_Function              => Action_Function,
       Copy                         => Copy_Dummy,
       Delete                       => Delete_Dummy);

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

end TJa.Lists.Checked.Double_Linked.General_List.Checked_Data;
