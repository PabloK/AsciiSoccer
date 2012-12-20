-------------------------------------------------------------------------------
--|                                                                         |--
--|                       Torbjörn Jonsson Ada library                      |--
--|                                                                         |--
--|     T J A . L I S T S . I N T E R N A L . D O U B L E _ L I N K E D     |--
--|                                                                         |--
--|                           Body implementation                           |--
--|                              Version  4.00                              |--
--|                                                                         |--
--|                              (C) Copyright                              |--
--|                   Torbjörn Jonsson,  TorJo@Ida.LiU.se                   |--
--|                                                                         |--
-------------------------------------------------------------------------------
--|                                                                         |--
--| Change log:                                                             |--
--|                                                                         |--
--|   2005-02-07  Version 4.00 is ok.                                       |--
--|                                                                         |--
-------------------------------------------------------------------------------
--|                                                                         |--
--| Implementation details:                                                 |--
--|                                                                         |--
-------------------------------------------------------------------------------

package body TJa.Lists.Internal.Double_Linked is

  --***************************************************************************
  --| Declarations of local types, constants and methods.
  --***************************************************************************

  function Local_Empty(List : in Node_List_Type) return Boolean;

  pragma Inline(Local_Empty);

  -----------------------------------------------------------------------------

  function Local_Last_Node(Node_List : in Node_List_Type)
           return Node_List_Type;

  pragma Inline(Local_Last_Node);

  -----------------------------------------------------------------------------

  function Local_New_Node(Data : in Data_Type) return Node_List_Type;

  pragma Inline(Local_New_Node);

  -----------------------------------------------------------------------------

  function Local_Copy(Node_List : in Node_List_Type) return Node_List_Type;

  pragma Inline(Local_Copy);

  -----------------------------------------------------------------------------

  procedure Local_Turn(Node_List : in out Node_List_Type;
                       Last_Node :    out Node_List_Type);

  pragma Inline(Local_Turn);

  -----------------------------------------------------------------------------

  procedure Local_Free_Node is
    new Ada.Unchecked_Deallocation(Object => Node_Type,
                                   Name   => Node_List_Type);

  -----------------------------------------------------------------------------

  procedure Local_Find_Position(List : in out List_Type;
                                Pos  : in     Positive);

  function Local_Find_Position(List : in List_Type;
                               Pos  : in Positive) return Node_List_Type;

  pragma Inline(Local_Find_Position);

  -----------------------------------------------------------------------------

  procedure Local_Forward(List  : in out List_Type;
                          Count : in     Positive := 1);

  function Local_Forward(List  : in List_Type;
                         Count : in Positive := 1) return Node_List_Type;

  pragma Inline(Local_Forward);

  -----------------------------------------------------------------------------

  procedure Local_Backward(List  : in out List_Type;
                           Count : in     Positive := 1);

  function Local_Backward(List  : in List_Type;
                          Count : in Positive := 1) return Node_List_Type;

  pragma Inline(Local_Backward);

  -----------------------------------------------------------------------------

  procedure Local_Delete_Current_Node(List  : in out List_Type;
                                      Count : in     Positive := 1);

  pragma Inline(Local_Delete_Current_Node);


  --***************************************************************************
  --| Definitions of public methods.
  --***************************************************************************

  -----------------------------------------------------------------------------
  --| Empty
  -----------------------------------------------------------------------------

  function Empty(List : in List_Type) return Boolean is

  begin
    return (List.No_Of_Nodes = 0);
  end Empty;

  -----------------------------------------------------------------------------
  --| Length
  -----------------------------------------------------------------------------

  function Length(List : in List_Type) return Natural is

  begin
    return List.No_Of_Nodes;
  end Length;

  -----------------------------------------------------------------------------
  --| Delete
  -----------------------------------------------------------------------------

  procedure Delete(List : in out List_Type) is

  begin
    if not Empty(List) then
      List.Current_Node := List.Node_List;
      List.Current_Position := 1;
      Local_Delete_Current_Node(List, Count => List.No_Of_Nodes);
    end if;
  end Delete;
  -----------------------------------------------------------------------------
  procedure Delete(List : in out List_Type;
                   Low  : in     Positive;
                   High : in     Natural) is

    My_High : Positive;

  begin
    if (Low > List.No_Of_Nodes) or else (High > List.No_Of_Nodes) then
      raise List_Constraint_Error;
    end if;

    if High = 0 then
      My_High := List.No_Of_Nodes;
    else
      if Low > High then
        raise List_Data_Error;
      end if;
      My_High := High;
    end if;

    -- 'List.Current_Position = 3' means that 'Current_Element' point at node
    -- number 3 in list. 'My_High = 2' means that the nodes with number Low..2
    -- shall be deleted.

    Local_Find_Position(List, Low);
    Local_Delete_Current_Node(List, Count => My_High - Low + 1);
  end Delete;

  --===========================================================================
  -- NYI: ... This is not an effective way to do this, but ...
  function Delete(List : in List_Type;
                  Low  : in Positive;
                  High : in Natural) return List_Type is

    New_List : List_Type := Copy(List);

  begin
    Delete(New_List, Low, High);
    return New_List;
  end Delete;

  -----------------------------------------------------------------------------
  --| Element
  -----------------------------------------------------------------------------

  function Element(List  : in List_Type;
                   Index : in Positive) return Data_Type is

  begin
    if Index > List.No_Of_Nodes then
      raise List_Constraint_Error;
    end if;

    return Copy(Local_Find_Position(List, Index).Data);
  end Element;

  -----------------------------------------------------------------------------
  --| Copy
  -----------------------------------------------------------------------------

  function Copy(List : in List_Type) return List_Type is

    New_List : List_Type := (No_Of_Nodes      => List.No_Of_Nodes,
                             Node_List        => Local_Copy(List.Node_List),
                             Last_Node        => null,
                             Current_Node     => null,
                             Current_Position => List.Current_Position);

  begin
    New_List.Current_Node := New_List.Node_List;
    New_List.Last_Node := New_List.Node_List;

    if List.No_Of_Nodes /= 0 then
      for I in 2 .. List.No_Of_Nodes loop
        New_List.Last_Node := New_List.Last_Node.Next;
      end loop;
      for I in 2 .. List.Current_Position loop
        New_List.Current_Node := New_List.Current_Node.Next;
      end loop;
    end if;

    return New_List;
  end Copy;

  -----------------------------------------------------------------------------
  --| Slice
  -----------------------------------------------------------------------------

  function Slice(List : in List_Type;
                 Low  : in Positive;
                 High : in Natural) return List_Type is

    New_List : List_Type;
    My_High  : Positive;

  begin
    if (Low > List.No_Of_Nodes) or else (High > List.No_Of_Nodes) then
      raise List_Constraint_Error;
    end if;

    if High = 0 then
      My_High := List.No_Of_Nodes;
    else
      if Low > High then
        raise List_Data_Error;
      end if;
      My_High := High;
    end if;

    New_List := Copy(List);

    if My_High < List.No_Of_Nodes then
      Delete(New_List, My_High + 1, 0);
    end if;
    if Low > 1 then
      Delete(New_List, 1, Low);
    end if;

    return New_List;
  end Slice;

  -----------------------------------------------------------------------------
  --| Insert
  -----------------------------------------------------------------------------

  procedure Insert(List     : in out List_Type;
                   New_Item : in     List_Type;
                   After    : in     Natural := 0) is

    New_List : List_Type;

  begin
    if List.No_Of_Nodes < After then
      raise List_Constraint_Error;
    end if;

    if Empty(New_Item) then
      return;
    end if;

    if Empty(List) then
      List := Copy(New_Item);
      return;
    end if;

    for I in 1 .. Length(List) loop
      -- NYI: Insert each element in list ...
      null;
    end loop;
  end Insert;
  -----------------------------------------------------------------------------
  procedure Insert(List     : in out List_Type;
                   New_Item : in     Data_Type;
                   After    : in     Natural := 0) is

    New_Node : Node_List_Type := Local_New_Node(New_Item);

  begin
--      Put_Line("DEBUG: List: Insert: Start ...");
--      Put_Line("DEBUG: List: Insert: No of nodes = " &
--               Integer'Image(List.No_Of_Nodes));
--      Put_Line("DEBUG: List: Insert: After = " & Integer'Image(After));
    if List.No_Of_Nodes < After then
      raise List_Constraint_Error;
    end if;

    if List.No_Of_Nodes = 0 then
      -- List is empty. First data in list.
--        Put_Line("DEBUG: List: Insert: First(1) ...");
      List.Node_List := New_Node;
      List.Current_Node := New_Node;
      List.Last_Node := New_Node;
      List.No_Of_Nodes := 1;
      List.Current_Position := 1;
    elsif After = 0 then
      -- List has at least one node before. Insert first in list.
--        Put_Line("DEBUG: List: Insert: First(2) ...");
      List.Node_List.Previous := New_Node;
      New_Node.Next := List.Node_List;
      List.Node_List := New_Node;

      if List.No_Of_Nodes = 0 then
        List.Last_Node := New_Node;
      end if;
      List.No_Of_Nodes := List.No_Of_Nodes + 1;

      List.Current_Node := New_Node;
      List.Current_Position := 1;
    elsif After = List.No_Of_Nodes then
      -- List has at least one node before. Insert after last node in list.
--        Put_Line("DEBUG: List: Insert: Last ...");
      List.Last_Node.Next := New_Node;
      New_Node.Previous := List.Last_Node;
      List.Last_Node := New_Node;

      List.No_Of_Nodes := List.No_Of_Nodes + 1;

      List.Current_Node := New_Node;
      List.Current_Position := List.No_Of_Nodes;
    else
      -- List has at least one node before. Insert after some node inside list.
--        Put_Line("DEBUG: List: Insert: Current position = " &
--                 Integer'Image(List.Current_Position));
      Local_Find_Position(List, After);
--        Put_Line("DEBUG: List: Insert: Current position = " &
--                 Integer'Image(List.Current_Position));
      List.Current_Node.Next.Previous := New_Node;
      New_Node.Next := List.Current_Node.Next;

      List.Current_Node.Next := New_Node;
      New_Node.Previous := List.Current_Node;

      List.Current_Node := List.Current_Node.Next;
      List.Current_Position := List.Current_Position + 1;

      List.No_Of_Nodes := List.No_Of_Nodes + 1;
    end if;
  end Insert;

  --===========================================================================
  function Insert(List     : in List_Type;
                  New_Item : in List_Type;
                  After    : in Natural := 0) return List_Type is

    New_List : List_Type;

  begin
    if List.No_Of_Nodes < After then
      raise List_Constraint_Error;
    end if;

    New_List := Copy(List);

    if Empty(New_Item) then
      return New_List;
    end if;

    Insert(New_List, New_Item, After);
    return New_List;
  end Insert;
  -----------------------------------------------------------------------------
  function Insert(List     : in List_Type;
                  New_Item : in Data_Type;
                  After    : in Natural := 0) return List_Type is

    New_List : List_Type;

  begin
    if List.No_Of_Nodes < After then
      raise List_Constraint_Error;
    end if;

    New_List := Copy(List);

    Insert(New_List, New_Item, After);
    return New_List;
  end Insert;

  -----------------------------------------------------------------------------
  --| Append
  -----------------------------------------------------------------------------

  procedure Append(List     : in out List_Type;
                   New_Item : in     List_Type) is

  begin
    Insert(List, New_Item, After => List.No_Of_Nodes);
  end Append;
  -----------------------------------------------------------------------------
  procedure Append(List     : in out List_Type;
                   New_Item : in     Data_Type) is

  begin
    Insert(List, New_Item, After => List.No_Of_Nodes);
  end Append;

  -----------------------------------------------------------------------------
  --| "&"
  -----------------------------------------------------------------------------

  function "&"(Left, Right : in List_Type) return List_Type is

    New_List : List_Type := Copy(Left);

  begin
    Append(New_List, Right);
    return New_List;
  end "&";
  -----------------------------------------------------------------------------
  function "&"(Left  : in List_Type;
               Right : in Data_Type) return List_Type is

    New_List : List_Type := Copy(Left);

  begin
    Append(New_List, Right);
    return New_List;
  end "&";
  -----------------------------------------------------------------------------
  function "&"(Left  : in Data_Type;
               Right : in List_Type) return List_Type is

    New_List : List_Type := (No_Of_Nodes      => 1,
                             Node_List        => Local_New_Node(Left),
                             Last_Node        => null,
                             Current_Node     => null,
                             Current_Position => 0);

  begin
    New_List.Last_Node := New_List.Node_List;
    New_List.Current_Node := New_List.Node_List;
    Append(New_List, Right);
    return New_List;
  end "&";

  -----------------------------------------------------------------------------
  --| Replace
  -----------------------------------------------------------------------------

  procedure Replace(List     : in out List_Type;
                    Index    : in     Positive;
                    New_Item : in     List_Type) is

  begin
    Replace(List, Index, Index, New_Item);
  end Replace;
  -----------------------------------------------------------------------------
  procedure Replace(List     : in out List_Type;
                    Index    : in     Positive;
                    New_Item : in     Data_Type) is

  begin
    Replace(List, Index, Index, New_Item);
  end Replace;
  -----------------------------------------------------------------------------
  procedure Replace(List     : in out List_Type;
                    Low      : in     Positive;
                    High     : in     Natural;
                    New_Item : in     List_Type) is

    My_High : Positive;

  begin
    if (List.No_Of_Nodes < Low) or else (List.No_Of_Nodes < High) then
      raise List_Constraint_Error;
    end if;

    if High = 0 then
      My_High := List.No_Of_Nodes;
    else
      if Low > High then
        raise List_Data_Error;
      end if;
      My_High := High;
    end if;

    Delete(List, Low, My_High);
    Insert(List, New_Item, After => Low - 1);
  end Replace;
  -----------------------------------------------------------------------------
  procedure Replace(List     : in out List_Type;
                    Low      : in     Positive;
                    High     : in     Natural;
                    New_Item : in     Data_Type) is

    My_High : Positive;

  begin
    if (List.No_Of_Nodes < Low) or else (List.No_Of_Nodes < High) then
      raise List_Constraint_Error;
    end if;

    if High = 0 then
      My_High := List.No_Of_Nodes;
    else
      if Low > High then
        raise List_Data_Error;
      end if;
      My_High := High;
    end if;

    Delete(List, Low, My_High);
    Insert(List, New_Item, After => Low - 1);
  end Replace;

  --===========================================================================
  function Replace(List     : in List_Type;
                   Index    : in Positive;
                   New_Item : in List_Type) return List_Type is

  begin
    return Replace(List, Index, Index, New_Item);
  end Replace;
  -----------------------------------------------------------------------------
  function Replace(List     : in List_Type;
                   Index    : in Positive;
                   New_Item : in Data_Type) return List_Type is

  begin
    return Replace(List, Index, Index, New_Item);
  end Replace;
  -----------------------------------------------------------------------------
  -- NYI: ... This is not an effective way to do this, but ...
  function Replace(List     : in List_Type;
                   Low      : in Positive;
                   High     : in Natural;
                   New_Item : in List_Type) return List_Type is

    New_List : List_Type := Copy(List);

  begin
    Replace(New_List, Low, High, New_Item);
    return New_List;
  exception
     when others =>
       Delete(New_List);
       raise;
  end Replace;
  -----------------------------------------------------------------------------
  -- NYI: ... This is not an effective way to do this, but ...
  function Replace(List     : in List_Type;
                   Low      : in Positive;
                   High     : in Natural;
                   New_Item : in Data_Type) return List_Type is

    New_List : List_Type := Copy(List);

  begin
    Replace(New_List, Low, High, New_Item);
    return New_List;
  exception
     when others =>
       Delete(New_List);
       raise;
  end Replace;

  -----------------------------------------------------------------------------
  --| Turn
  -----------------------------------------------------------------------------

  procedure Turn(List : in out List_Type) is

    Last_Node : Node_List_Type;

  begin
    if not Empty(List) then
      Local_Turn(List.Node_List, Last_Node);
      List.Last_Node := Last_Node;
      List.Current_Position := (List.No_Of_Nodes - List.Current_Position + 1);
    end if;
  end Turn;

  --===========================================================================
  function Turn(List : in List_Type) return List_Type is

    New_List : List_Type := Copy(List);

  begin
    Turn(New_List);
    return New_List;
  end Turn;

  -----------------------------------------------------------------------------
  --| Iterate
  -----------------------------------------------------------------------------

  procedure Iterate(List   : in List_Type;
                    Action : in Action_Procedure_In_Mode) is

    Node_List : Node_List_Type := List.Node_List;

  begin
    while not Local_Empty(Node_List) loop
      Action(Node_List.Data);
      Node_List := Node_List.Next;
    end loop;
  end Iterate;
  -----------------------------------------------------------------------------
  procedure Iterate(List   : in List_Type;
                    Action : in Action_Procedure_In_Out_Mode) is

    Node_List : Node_List_Type := List.Node_List;

  begin
    while not Local_Empty(Node_List) loop
      Action(Node_List.Data);
      Node_List := Node_List.Next;
    end loop;
  end Iterate;

  --===========================================================================
  procedure Iterate(List   : in List_Type;
                    Action : in Action_Function) is

    Node_List : Node_List_Type := List.Node_List;

  begin
    while not Local_Empty(Node_List) loop
      Node_List.Data := Action(Node_List.Data);
      Node_List := Node_List.Next;
    end loop;
  end Iterate;

  -----------------------------------------------------------------------------
  --| With_Key
  -----------------------------------------------------------------------------

  package body With_Key is

    --*************************************************************************
    --| Declarations of local types, constants and methods.
    --*************************************************************************

    function Local_Find_Index(List : in List_Type;
                              Key  : in Key_Type) return Natural;

    pragma Inline(Local_Find_Index);

    ---------------------------------------------------------------------------

    function Local_Find_Index_Sorted(List : in List_Type;
                                     Key  : in Key_Type) return Natural;

    pragma Inline(Local_Find_Index_Sorted);

    ---------------------------------------------------------------------------

    function Local_Sorted(Node_List : in Node_List_Type) return Boolean;

    pragma Inline(Local_Sorted);

    ---------------------------------------------------------------------------

    function Local_Compare(Left, Right : in Node_List_Type)
             return Relation_Type;

    pragma Inline(Local_Compare);


    --*************************************************************************
    --| Definitions of public methods.
    --*************************************************************************

    ---------------------------------------------------------------------------
    --| Insert_Sorted
    ---------------------------------------------------------------------------

    procedure Insert_Sorted(List     : in out List_Type;
                            New_Item : in     List_Type) is

      Temp_New_Item : Node_List_Type;

    begin
      if Empty(New_Item) then
        return;
      end if;

      -- Insert all new items but the first in the list.
      Temp_New_Item := New_Item.Node_List.Next;
      while not Local_Empty(Temp_New_Item) loop
        Insert_Sorted(List, Temp_New_Item.Data);
        Temp_New_Item := Temp_New_Item.Next;
      end loop;

      -- Insert first new item in the list.
      Insert_Sorted(List, New_Item.Node_List.Data);
    end Insert_Sorted;
    ---------------------------------------------------------------------------
    procedure Insert_Sorted(List     : in out List_Type;
                            New_Item : in     Data_Type) is

    begin
--        Put_Line("DEBUG: List: Insert_Sorted: Start ...");
--        Put_Line("DEBUG: List: Insert_Sorted: No of nodes = " &
--                 Integer'Image(List.No_Of_Nodes));
--        Put_Line("DEBUG: List: Insert_Sorted: Current position = " &
--                 Integer'Image(List.Current_Position));
      -- First?
      if (Empty(List) or else
          (Get_Key(New_Item) < Get_Key(List.Node_List.Data))) then
        Insert(List, New_Item, After => 0);
        return;
      end if;

      -- Last?
      if Get_Key(List.Last_Node.Data) < Get_Key(New_Item) then
        Insert(List, New_Item, After => List.No_Of_Nodes);
        return;
      end if;

      -- The new data shall be placed somewhere inside list.
--        Put_Line("DEBUG: List: Insert_Sorted: Search backward(1) ...");
      while Get_Key(New_Item) < Get_Key(List.Current_Node.Data) loop
--          Put_Line("DEBUG: List: Insert_Sorted: Search backward(2) ...");
        Local_Backward(List);
--          if List.Current_Node = null then
--            Put_Line("DEBUG: List: Insert_Sorted: Current_Node = null");
--          end if;
      end loop;
--        Put_Line("DEBUG: List: Insert_Sorted: No of nodes = " &
--                 Integer'Image(List.No_Of_Nodes));
--        Put_Line("DEBUG: List: Insert_Sorted: Current position = " &
--                 Integer'Image(List.Current_Position));
      -- Now the data in the list in less then (or equal to) the new one
--        Put_Line("DEBUG: List: Insert_Sorted: Search forward(1) ...");
      while Get_Key(List.Current_Node.Data) < Get_Key(New_Item) loop
--          Put_Line("DEBUG: List: Insert_Sorted: Search forward(2) ...");
        Local_Forward(List);
      end loop;
--        Put_Line("DEBUG: List: Insert_Sorted: No of nodes = " &
--                 Integer'Image(List.No_Of_Nodes));
--        Put_Line("DEBUG: List: Insert_Sorted: Current position = " &
--                 Integer'Image(List.Current_Position));

      -- Now the data in the list is greater then (or equal to) the new one
      Insert(List, New_Item, After => List.Current_Position - 1);
--        Put_Line("DEBUG: List: Insert_Sorted: No of nodes = " &
--                 Integer'Image(List.No_Of_Nodes));
--        Put_Line("DEBUG: List: Insert_Sorted: Current position = " &
--                 Integer'Image(List.Current_Position));
    end Insert_Sorted;

    --=========================================================================
    function Insert_Sorted(List     : in List_Type;
                           New_Item : in List_Type) return List_Type is

      New_List : List_Type := Copy(List);

    begin
      Insert_Sorted(New_List, New_Item);
      return New_List;
    end Insert_Sorted;
    ---------------------------------------------------------------------------
    function Insert_Sorted(List     : in List_Type;
                           New_Item : in Data_Type) return List_Type is

      New_List : List_Type := Copy(List);

    begin
      Insert_Sorted(New_List, New_Item);
      return New_List;
    end Insert_Sorted;

    ---------------------------------------------------------------------------
    --| Sort
    ---------------------------------------------------------------------------

    procedure Sort(List : in out List_Type) is

      New_List : List_Type;

    begin
      if Empty(List) then
        return;
      end if;

      -- NYI: Not very efficient, but ...
      Insert_Sorted(New_List, List);

      Delete(List);
      List := New_List;
      List.Current_Node := List.Node_List;
      List.Current_Position := 0;
    end Sort;

    --=========================================================================
    function Sort(List : in List_Type) return List_Type is

      New_List : List_Type := Copy(List);

    begin
      Sort(New_List);
      return New_List;
    end Sort;

    ---------------------------------------------------------------------------
    --| Sorted
    ---------------------------------------------------------------------------

    function Sorted(List : in List_Type) return Boolean is

    begin
      return Local_Sorted(List.Node_List);
    end Sorted;

    ---------------------------------------------------------------------------
    --| Member
    ---------------------------------------------------------------------------

    function Member(List : in List_Type;
                    Key  : in Key_Type) return Boolean is

      Index : Natural := Local_Find_Index(List, Key);

    begin
      return Index /= 0;
    end Member;


    ---------------------------------------------------------------------------
    --| Member_Sorted
    ---------------------------------------------------------------------------

    function Member_Sorted(List : in List_Type;
                           Key  : in Key_Type) return Boolean is

      Index : Natural := Local_Find_Index_Sorted(List, Key);

    begin
      return Index /= 0;
    end Member_Sorted;

    ---------------------------------------------------------------------------
    --| Find
    ---------------------------------------------------------------------------

    function Find(List : in List_Type;
                  Key  : in Key_Type) return Data_Type is


      Index : Natural := Local_Find_Index(List, Key);

    begin
      if Index = 0 then
        raise List_Constraint_Error;
      end if;

      return Copy(Local_Find_Position(List, Index).Data);
    end Find;

    ---------------------------------------------------------------------------
    --| Find_Sorted
    ---------------------------------------------------------------------------

    function Find_Sorted(List : in List_Type;
                         Key  : in Key_Type) return Data_Type is

      Index : Natural := Local_Find_Index_Sorted(List, Key);

    begin
      if Index = 0 then
        raise List_Constraint_Error;
      end if;

      return Copy(Local_Find_Position(List, Index).Data);
    end Find_Sorted;

    ---------------------------------------------------------------------------
    --| Index
    ---------------------------------------------------------------------------

    function Index(List : in List_Type;
                   Key  : in Key_Type) return Natural is

    begin
      return Local_Find_Index(List, Key);
    end Index;


    ---------------------------------------------------------------------------
    --| Index_Sorted
    ---------------------------------------------------------------------------

    function Index_Sorted(List : in List_Type;
                          Key  : in Key_Type) return Natural is

    begin
      return Local_Find_Index_Sorted(List, Key);
    end Index_Sorted;

    ---------------------------------------------------------------------------
    --| Relation operators: "=", "<", ">", "<=", ">="
    ---------------------------------------------------------------------------

    function "="(Left, Right : in List_Type) return Boolean is

    begin
      if Length(Left) /= Length(Right) then
        return False;
      end if;

      return Compare(Left, Right) = Equal;
    end "=";
    ---------------------------------------------------------------------------
    function "<"(Left, Right : in List_Type) return Boolean is

    begin
      return Compare(Left, Right) = Less;
    end "<";
    ---------------------------------------------------------------------------
    function ">"(Left, Right : in List_Type) return Boolean is

    begin
      return (Right < Left);
    end ">";
    ---------------------------------------------------------------------------
    function "<="(Left, Right : in List_Type) return Boolean is

    begin
      return not (Right < Left);
    end "<=";
    ---------------------------------------------------------------------------
    function ">="(Left, Right : in List_Type) return Boolean is

    begin
      return not (Left < Right);
    end ">=";

    ---------------------------------------------------------------------------
    --| Compare
    ---------------------------------------------------------------------------

    function Compare(Left, Right : in List_Type) return Relation_Type is

    begin
      return Local_Compare(Left.Node_List, Right.Node_List);
    end Compare;


    --*************************************************************************
    --| Definitions of local methods.
    --*************************************************************************

    ---------------------------------------------------------------------------
    --| Local_Find_Index[_Sorted]
    ---------------------------------------------------------------------------

    function Local_Find_Index(List : in List_Type;
                              Key  : in Key_Type) return Natural is

      Index     : Natural        := 0;
      Node_List : Node_List_Type := List.Node_List;

    begin
      while not Local_Empty(Node_List) loop
        Index := Index + 1;
        if Get_Key(Node_List.Data) = Key then
          return Index;
        end if;
        Node_List := Node_List.Next;
      end loop;

      return 0;
    end Local_Find_Index;

    ---------------------------------------------------------------------------
    -- NYI: Can be more efficient ...

    function Local_Find_Index_Sorted(List : in List_Type;
                                     Key  : in Key_Type) return Natural is

      Index     : Natural        := 0;
      Node_List : Node_List_Type := List.Node_List;

    begin
      while not Local_Empty(Node_List) loop
        Index := Index + 1;
        if Get_Key(Node_List.Data) = Key then
          return Index;
        elsif Key < Get_Key(Node_List.Data) then
          return 0;
        end if;
        Node_List := Node_List.Next;
      end loop;

      return 0;
    end Local_Find_Index_Sorted;

    ---------------------------------------------------------------------------
    --| Local_Sorted
    ---------------------------------------------------------------------------

    function Local_Sorted(Node_List : in Node_List_Type) return Boolean is

      First_Node  : Node_List_Type;
      Second_Node : Node_List_Type;

    begin
      if Local_Empty(Node_List) then
        return True;
      end if;

      First_Node := Node_List;
      Second_Node := First_Node.Next;
      while not Local_Empty(Second_Node) loop
        if Get_Key(Second_Node.Data) < Get_Key(First_Node.Data) then
          return False;
        end if;
        First_Node := First_Node.Next;
        Second_Node := First_Node.Next;
      end loop;

      -- It must be sorted if we come to this point.
      return True;
    end Local_Sorted;

    ---------------------------------------------------------------------------
    --| Local_Compare
    ---------------------------------------------------------------------------

    function Local_Compare(Left, Right : in Node_List_Type)
             return Relation_Type is

      Left_List  : Node_List_Type := Left;
      Right_List : Node_List_Type := Right;

    begin
      -- Search after the first difference.
      while not Local_Empty(Left_List) loop
        if Local_Empty(Right_List) then
          -- 'Right' is shorter than 'Left'.
          return Greater;
        end if;

        -- Check if the list elements are different.
        if Get_Key(Left_List.Data) < Get_Key(Right_List.Data) then
          return Less;
        elsif Get_Key(Right_List.Data) < Get_Key(Left_List.Data) then
          return Greater;
        end if;

        -- All data are equal up to this point. Go to next element.
        Left_List := Left_List.Next;
        Right_List := Right_List.Next;
      end loop;

      if not Local_Empty(Right_List) then
        -- 'Left' is shorter than 'Right'.
        return Less;
      end if;

      -- The lists are equal.
      return Equal;
    end Local_Compare;


    --*************************************************************************
    --| Definition of initiation part.
    --*************************************************************************

  begin
    -- Nothing have to be done.
    null;
  end With_Key;


  --***************************************************************************
  --| Definitions of local methods.
  --***************************************************************************

  -----------------------------------------------------------------------------
  --| Local_Find_Position
  -----------------------------------------------------------------------------

  procedure Local_Find_Position(List : in out List_Type;
                                Pos  : in     Positive) is

    Node_List : Node_List_Type := List.Node_List;

  begin
    if Pos > List.No_Of_Nodes then
      raise Internal_Error;
    end if;

    for I in 2 .. Pos loop
      Node_List := Node_List.Next;
    end loop;
    List.Current_Position := Pos;
    List.Current_Node := Node_List;

--      if List.Current_Position > Pos then
--        Local_Backward(List, Count => List.Current_Position - Pos);
--      elsif List.Current_Position < Pos then
--        Local_Forward(List, Count => Pos - List.Current_Position);
--      end if;
  end Local_Find_Position;

  -----------------------------------------------------------------------------

  function Local_Find_Position(List : in List_Type;
                               Pos  : in Positive) return Node_List_Type is

    Node_List : Node_List_Type := List.Node_List;

  begin
    if Pos > List.No_Of_Nodes then
      raise Internal_Error;
    end if;

    for I in 2 .. Pos loop
      Node_List := Node_List.Next;
    end loop;
    return Node_List;

--      if List.Current_Position > Pos then
--        return Local_Backward(List, Count => List.Current_Position - Pos);
--      elsif List.Current_Position < Pos then
--        return Local_Forward(List, Count => Pos - List.Current_Position);
--      end if;
--      return List.Current_Node;
  end Local_Find_Position;

  -----------------------------------------------------------------------------
  --| Local_Forward
  -----------------------------------------------------------------------------

  procedure Local_Forward(List  : in out List_Type;
                          Count : in     Positive := 1) is

  begin
--      Put_Line("DEBUG: List: Local_Forward: Start ...");
    if Count > List.No_Of_Nodes - List.Current_Position then
      raise Internal_Error;
    end if;

--      Put_Line("DEBUG: List: Local_Forward: Step forward(1) ...");
    for I in 1 .. Count loop
--        Put_Line("DEBUG: List: Local_Forward: Step forward(2) ...");
      List.Current_Node := List.Current_Node.Next;
    end loop;
--      Put_Line("DEBUG: List: Local_Forward: Fix current position ...");
    List.Current_Position := List.Current_Position + Count;
--      Put_Line("DEBUG: List: Local_Forward: End ...");
  end Local_Forward;

  -----------------------------------------------------------------------------

  function Local_Forward(List  : in List_Type;
                         Count : in Positive := 1) return Node_List_Type is

    Temp : Node_List_Type;

  begin
    if Count > List.No_Of_Nodes - List.Current_Position then
      raise Internal_Error;
    end if;

    Temp := List.Current_Node;
    for I in 1 .. Count loop
      Temp := Temp.Next;
    end loop;
    return Temp;
  end Local_Forward;

  -----------------------------------------------------------------------------
  --| Local_Backward
  -----------------------------------------------------------------------------

  procedure Local_Backward(List  : in out List_Type;
                           Count : in     Positive := 1) is

  begin
--      Put_Line("DEBUG: List: Local_Backward: Start ...");
    if Count >= List.Current_Position then
      raise Internal_Error;
    end if;

--      Put_Line("DEBUG: List: Local_Backward: Step backward(1) ...");
    for I in 1 .. Count loop
--        Put_Line("DEBUG: List: Local_Backward: Step backward(2) ...");
      List.Current_Node := List.Current_Node.Previous;
    end loop;
--      Put_Line("DEBUG: List: Local_Backward: Current position = " &
--               Integer'Image(List.Current_Position));
--      Put_Line("DEBUG: List: Local_Backward: Fix current position ...");
    List.Current_Position := List.Current_Position - Count;
--      Put_Line("DEBUG: List: Local_Backward: Current position = " &
--               Integer'Image(List.Current_Position));
--      Put_Line("DEBUG: List: Local_Backward: End ...");
  end Local_Backward;

  -----------------------------------------------------------------------------

  function Local_Backward(List  : in List_Type;
                          Count : in Positive := 1) return Node_List_Type is

    Temp : Node_List_Type;

  begin
    if Count > List.No_Of_Nodes - List.Current_Position then
      raise Internal_Error;
    end if;

    Temp := List.Current_Node;
    for I in 1 .. Count loop
      Temp := Temp.Previous;
    end loop;
    return Temp;
  end Local_Backward;

  -----------------------------------------------------------------------------
  --| Local_Delete_Current_Node
  -----------------------------------------------------------------------------

  procedure Local_Delete_Current_Node(List  : in out List_Type;
                                      Count : in     Positive := 1) is

    C_E  : Node_List_Type;
    Temp : Node_List_Type;

  begin
    if ((List.No_Of_Nodes = 0) or else
        (Count > List.No_Of_Nodes - List.Current_Position + 1)) then
--        Put_Line("DEBUG: Local_Delete_Current_Node: Try to delete to much ...");
      raise Internal_Error;
    end if;

    if List.Current_Position = 1 then
      -- In the beginning of the list ...
--        Put_Line("DEBUG: Local_Delete_Current_Node: In the beginning ...");
      C_E := List.Current_Node;
      for I in 1 .. Count loop
        Temp := C_E;
        C_E := C_E.Next;
--          Put_Line("DEBUG: Local_Delete_Current_Node: '.Next' ok ...");
        if C_E /= null then
          C_E.Previous := null;
        end if;
        Delete(Temp.Data);
--          Put_Line("DEBUG: Local_Delete_Current_Node: Ok to delete data ...");
        Local_Free_Node(Temp);
      end loop;
      List.No_Of_Nodes := List.No_Of_Nodes - Count;
      List.Node_List := C_E;
      List.Current_Node := C_E;
      if List.No_Of_Nodes = 0 then
        List.Last_Node := null;
        List.Current_Position := 0;
      end if;
    else
      -- Not in the beginning of the list ...
--        Put_Line("DEBUG: Local_Delete_Current_Node: Not in the beginning ...");
      C_E := List.Current_Node;
      List.Current_Node := List.Current_Node.Previous; -- Set to node before.
      for I in 1 .. Count loop
        Temp := C_E;
        C_E := C_E.Next;
        if C_E /= null then
          C_E.Previous := Temp.Previous;
        end if;
        Delete(Temp.Data);
        Local_Free_Node(Temp);
      end loop;
      List.Current_Node.Next := C_E;
      List.No_Of_Nodes := List.No_Of_Nodes - Count;
      if C_E = null then
        if List.Current_Position > 0 then
          List.Current_Position := List.Current_Position - 1;
        end if;
        List.Last_Node := List.Current_Node;
      else
        List.Current_Node := C_E;
      end if;
    end if;
  exception
     when others =>
       Put_Line("DEBUG: Local_Delete_Current_Node: Some internal error ...");
  end Local_Delete_Current_Node;

  -----------------------------------------------------------------------------
  --| Local_Empty
  -----------------------------------------------------------------------------
  function Local_Empty(List : in Node_List_Type) return Boolean is

  begin
    return (List = null);
  end Local_Empty;

  -----------------------------------------------------------------------------
  --| Local_New_Node
  -----------------------------------------------------------------------------

  function Local_New_Node(Data : in Data_Type) return Node_List_Type is

  begin
    return new Node_Type'(Data => Copy(Data), Previous => null, Next => null);
  end Local_New_Node;

  -----------------------------------------------------------------------------
  --| Local_Last_Node
  -----------------------------------------------------------------------------
  -- NYI: ... This is not an effective way to do this, but ...

  function Local_Last_Node(Node_List : in Node_List_Type)
           return Node_List_Type is

    Temp_Node_List : Node_List_Type;

  begin
    if Local_Empty(Node_List) then
      return null;
    end if;

    Temp_Node_List := Node_List;
    while not Local_Empty(Temp_Node_List.Next) loop
      Temp_Node_List := Temp_Node_List.Next;
    end loop;

    return Temp_Node_List;
  end Local_Last_Node;

  -----------------------------------------------------------------------------
  --| Local_Copy
  -----------------------------------------------------------------------------

  function Local_Copy(Node_List : in Node_List_Type) return Node_List_Type is

    Temp_Node_List     : Node_List_Type;
    New_Node_List      : Node_List_Type := null;
    Temp_New_Node_List : Node_List_Type;

  begin
    if not Local_Empty(Node_List) then
      -- Copy the first element.
      New_Node_List := Local_New_Node(Node_List.Data);
      Temp_New_Node_List := New_Node_List;
      -- Copy rest of list.
      Temp_Node_List := Node_List;
      while not Local_Empty(Temp_Node_List.Next) loop
        Temp_Node_List := Temp_Node_List.Next;
        Temp_New_Node_List.Next := Local_New_Node(Temp_Node_List.Data);
        Temp_New_Node_List.Next.Previous := Temp_New_Node_List;
        Temp_New_Node_List := Temp_New_Node_List.Next;
      end loop;
    end if;
    -- Return the copy.
    return New_Node_List;
  end Local_Copy;

  -----------------------------------------------------------------------------
  --| Local_Turn
  -----------------------------------------------------------------------------

  procedure Local_Turn(Node_List : in out Node_List_Type;
                       Last_Node :    out Node_List_Type) is

    New_Node_List : Node_List_Type := null;
    Node          : Node_List_Type;

  begin
    Last_Node := Node_List;
    while not Local_Empty(Node_List) loop
      -- Get first node.
      Node := Node_List;
      Node_List := Node_List.Next;
      -- Node.Next := null; -- Not needed. It will be assigned again below.
      if not Local_Empty(Node_List) then
        Node_List.Previous := null;
      end if;

      -- Insert node before the rest in new list.
      Node.Next := New_Node_List;
      if not Local_Empty(New_Node_List) then
        New_Node_List.Previous := Node;
      end if;
      New_Node_List := Node;
    end loop;
    Node_List := New_Node_List;
  end Local_Turn;


  --***************************************************************************
  --| Definition of initiation part.
  --***************************************************************************

begin
  -- Nothing have to be done.
  null;
end TJa.Lists.Internal.Double_Linked;
