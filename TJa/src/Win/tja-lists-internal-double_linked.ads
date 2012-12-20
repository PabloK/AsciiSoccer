-------------------------------------------------------------------------------
--|                                                                         |--
--|                       Torbj�rn Jonsson Ada library                      |--
--|                                                                         |--
--|     T J A . L I S T S . I N T E R N A L . D O U B L E _ L I N K E D     |--
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
--|   2000-11-24  Version 3.00 is ok.                                       |--
--|               Created and documented by Torbj�rn Jonsson.               |--
--|                                                                         |--
-------------------------------------------------------------------------------
--|                                                                         |--
--| Description:                                                            |--
--|                                                                         |--
--|   This package is an internal list package which should not be used   � |--
--|   from other than TJa-library packages.                                 |--
--|                                                                         |--
--| Preconditions:                                                          |--
--|                                                                         |--
--|   This package assumes that the calling methods are "intelligent", that |--
--|   they don't call a method which can do something wrong with the list   |--
--|   (e.g. "Insert(List, Index, New_Item)" should not be called with a     |--
--|   sorted list).                                                         |--
--|                                                                         |--
-------------------------------------------------------------------------------

-- Ada standard libraries.
with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

-- External libraries.

-- Internal libraries.
with TJa.Misc;                          use TJa.Misc;

-------------------------------------------------------------------------------
generic

  -- List consists of "Data_Type".
  type Data_Type is private;

  -- We need these types for the 'Iterate'-procedures.
  type Action_Procedure_In_Mode is
    access procedure(Data : in Data_Type); -- is private;

  type Action_Procedure_In_Out_Mode is
    access procedure(Data : in out Data_Type); -- is private;

  type Action_Function is
    access function(Data : in Data_Type) return Data_Type; -- is private;

  -- We need a function which copies the data (needed for dynamic data).
  with function Copy(Data : in Data_Type) return Data_Type is <>;

  -- We need a procedure which deletes the data (needed for dynamic data).
  with procedure Delete(Data : in out Data_Type) is <>;

package TJa.Lists.Internal.Double_Linked is

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
  procedure Delete(List : in out List_Type;
                   Low  : in     Positive;
                   High : in     Natural);

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

  procedure Iterate(List   : in List_Type;
                    Action : in Action_Procedure_In_Mode);
  procedure Iterate(List   : in List_Type;
                    Action : in Action_Procedure_In_Out_Mode);

  procedure Iterate(List   : in List_Type;
                    Action : in Action_Function);

  pragma Inline(Iterate);

  --===========================================================================
  --| With_Key
  --===========================================================================

  generic

    -- We need a key type if we want to search after data or sort lists.
    type Key_Type is private;

    -- We need a function for extracting the key from a data.
    with function Get_Key(Data : in Data_Type) return Key_Type is <>;

    -- We need some relation operators for the key type.
    with function "="(Left, Right : in Key_Type) return Boolean is <>;
    with function "<"(Left, Right : in Key_Type) return Boolean is <>;

  package With_Key is

    ---------------------------------------------------------------------------
    --| "Insert_Sorted" is used to insert a new item sorted into list. It
    --| assumes that the list is already sorted.
    --|
    --| If the functions are used a copy of the list is made and the new item
    --| is inserted in the copy and returned.
    --|
    --| "List_Data_Error" is raised if 'Empty(New_Item)' (in the cases when
    --| 'New_Item' is an 'List_Type'.
    ---------------------------------------------------------------------------

    procedure Insert_Sorted(List     : in out List_Type;
                            New_Item : in     List_Type);
    procedure Insert_Sorted(List     : in out List_Type;
                            New_Item : in     Data_Type);

    function Insert_Sorted(List     : in List_Type;
                           New_Item : in List_Type) return List_Type;
    function Insert_Sorted(List     : in List_Type;
                           New_Item : in Data_Type) return List_Type;

    pragma Inline(Insert_Sorted);

    ---------------------------------------------------------------------------
    --| "Sort" makes the list sorted.
    --|
    --| If the function are used a copy of the list is made and the operation
    --| is done to the copy and then returned.
    ---------------------------------------------------------------------------

    procedure Sort(List : in out List_Type);

    function Sort(List : in List_Type) return List_Type;

    pragma Inline(Sort);

    ---------------------------------------------------------------------------
    --| "Sorted" checks if the list is sorted.
    ---------------------------------------------------------------------------

    function Sorted(List : in List_Type) return Boolean;

    pragma Inline(Sorted);

    ---------------------------------------------------------------------------
    --| "Member" searches the list for an element and returns 'True' if it was
    --| found, otherwise 'False'.
    ---------------------------------------------------------------------------

    function Member(List : in List_Type;
                    Key  : in Key_Type) return Boolean;

    pragma Inline(Member);

    ---------------------------------------------------------------------------
    --| "Member_Sorted" deos the same as "Member" but for a sorted list.
    ---------------------------------------------------------------------------

    function Member_Sorted(List : in List_Type;
                           Key  : in Key_Type) return Boolean;

    pragma Inline(Member_Sorted);

    ---------------------------------------------------------------------------
    --| "Find" searches the list for an element with a specific key and returns
    --| the data if it was found, otherwise 'List_Constraint_Error' is raised.
    ---------------------------------------------------------------------------

    function Find(List : in List_Type;
                  Key  : in Key_Type) return Data_Type;

    pragma Inline(Find);

    ---------------------------------------------------------------------------
    --| "Find_Sorted" does the same as "Find" but for a sorted list.
    ---------------------------------------------------------------------------

    function Find_Sorted(List : in List_Type;
                         Key  : in Key_Type) return Data_Type;

    pragma Inline(Find_Sorted);

    ---------------------------------------------------------------------------
    --| "Index" searches the index in list where the data has a specific key.
    --| If the data isn't found "Index" returns 0.
    ---------------------------------------------------------------------------

    function Index(List : in List_Type;
                   Key  : in Key_Type) return Natural;

    pragma Inline(Index);

    ---------------------------------------------------------------------------
    --| "Index_Sorted" does the same as "Index" but for a sorted list.
    ---------------------------------------------------------------------------

    function Index_Sorted(List : in List_Type;
                          Key  : in Key_Type) return Natural;

    pragma Inline(Index_Sorted);

    ---------------------------------------------------------------------------
    --| Operators for comparing lists ("/=" implicit from "=" in Ada).
    --|
    --| "=" returns 'True' only if all data in the lists are identical.
    --|
    --| If lists are not equal and all data are equal to the point that one of
    --| the lists ends the shortest list is less than the other.
    --|
    --| The result 'True'/'False' from the comparision of the first differing
    --| data in the lists are the result for the comparision of the lists.
    ---------------------------------------------------------------------------

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

    ---------------------------------------------------------------------------
    --| "Compare" returns:
    --|   - 'Less' if first list is less than the second.
    --|   - 'Equal' if both lists are equal.
    --|   - 'Greater' if first list is greater than the second.
    ---------------------------------------------------------------------------

    function Compare(Left, Right : in List_Type) return Relation_Type;

    pragma Inline(Compare);

    ---------------------------------------------------------------------------

  end With_Key;

  -----------------------------------------------------------------------------

private

  -----------------------------------------------------------------------------
  --| Internal representation of "List_Type" needs som type of nodes and we use
  --| "Node_Type" to contain users data ("Data_Type").
  -----------------------------------------------------------------------------

  type Node_Type;

  type Node_List_Type is
    access Node_Type;

  type Node_Type is
    record
      Data     : Data_Type;
      Previous : Node_List_Type := null;
      Next     : Node_List_Type := null;
    end record;

  type List_Type is
    record
      No_Of_Nodes      : Natural        := 0;
      Node_List        : Node_List_Type := null;
      Last_Node        : Node_List_Type := null;
      Current_Node     : Node_List_Type := null;
      Current_Position : Natural        := 0;
    end record;

  -----------------------------------------------------------------------------
  --| The constant that represents "empty" list.
  -----------------------------------------------------------------------------

  Null_List : constant List_Type := (No_Of_Nodes      => 0,
                                     Node_List        => null,
                                     Last_Node        => null,
                                     Current_Node     => null,
                                     Current_Position => 0);

  -----------------------------------------------------------------------------

end TJa.Lists.Internal.Double_Linked;
