-------------------------------------------------------------------------------
--|                                                                         |--
--|                       Torbjörn Jonsson Ada library                      |--
--|                                                                         |--
--|     T J A . L I S T S . I N T E R N A L . S I N G L E _ L I N K E D     |--
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
--|   2000-11-24  Version 3.00 is ok.                                       |--
--|                                                                         |--
-------------------------------------------------------------------------------
--|                                                                         |--
--| Implementation details:                                                 |--
--|                                                                         |--
-------------------------------------------------------------------------------

package body TJa.Lists.Internal.Single_Linked is

  --***************************************************************************
  --| Declarations of local types, constants and methods.
  --***************************************************************************

  function Local_New_Node(Data : in Data_Type) return Node_List_Type;

  pragma Inline(Local_New_Node);

  -----------------------------------------------------------------------------
  procedure Local_Delete(Node_List : in out Node_List_Type);
  procedure Local_Delete(Node_List : in out Node_List_Type;
                         Low       : in     Positive;
                         High      : in     Natural);
  function Local_Delete(Node_List : in Node_List_Type;
                        Low       : in Positive;
                        High      : in Natural) return Node_List_Type;

  pragma Inline(Local_Delete);

  -----------------------------------------------------------------------------
  function Local_Element(Node_List : in Node_List_Type;
                         Index     : in Positive) return Data_Type;

  pragma Inline(Local_Element);

  -----------------------------------------------------------------------------
  function Local_Copy(Node_List : in Node_List_Type) return Node_List_Type;

  pragma Inline(Local_Copy);

  -----------------------------------------------------------------------------
  function Local_Slice(Node_List : in Node_List_Type;
                       Low       : in Positive;
                       High      : in Natural) return Node_List_Type;

  pragma Inline(Local_Slice);

  -----------------------------------------------------------------------------
  procedure Local_Insert(Node_List : in out Node_List_Type;
                         New_Item  : in     Node_List_Type;
                         After     : in     Natural);

  pragma Inline(Local_Insert);

  -----------------------------------------------------------------------------
  procedure Local_Replace(Node_List : in out Node_List_Type;
                          Low       : in     Positive;
                          High      : in     Natural;
                          New_Item  : in     Node_List_Type);

  pragma Inline(Local_Replace);

  -----------------------------------------------------------------------------
  procedure Local_Turn(Node_List : in out Node_List_Type);

  pragma Inline(Local_Turn);


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
    Local_Delete(List.Node_List);
    List.No_Of_Nodes := 0;
    List.Current_Element := null;
    List.Current_Position := 0;
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

    if High > 0 then
      if Low > High then
        raise List_Data_Error;
      end if;

      My_High := High;
      List.No_Of_Nodes := List.No_Of_Nodes - (High + 1 - Low);
    else
      My_High := List.No_Of_Nodes;
      List.No_Of_Nodes := Low - 1;
    end if;

    -- 'List.Current_Position = 2' means that 'Current_Element' point at node
    -- number 3 in list. 'My_High = 2' means that the nodes with number Low..2
    -- shall be deleted. If 'List.Current_Position < Low'

    if (List.Current_Position + 1) < Low then
      -- Element(s) after current element shall be deleted. Nothing has to be
      -- done to the current position.
      Local_Delete(List.Current_Element,
                   (Low - List.Current_Position),
                   (My_High - List.Current_Position));
    else
      Local_Delete(List.Node_List, Low, My_High);

      if List.Current_Position >= My_High then
        -- Element(s) before current element is deleted. Just calculate new
        -- position.
        List.Current_Position := List.Current_Position - (My_High + 1 - Low);
      else
        -- Current element is deleted! Set current element to be the same as
        -- the node list.
        List.Current_Element := List.Node_List;
        List.Current_Position := 0;
      end if;
    end if;
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

    if List.Current_Position < Index then
      return Local_Element(List.Current_Element,
                           Index - List.Current_Position);
    end if;
    return Local_Element(List.Node_List, Index);
  end Element;

  -----------------------------------------------------------------------------
  --| Copy
  -----------------------------------------------------------------------------

  function Copy(List : in List_Type) return List_Type is

    New_List : List_Type := (No_Of_Nodes      => List.No_Of_Nodes,
                             Node_List        => Local_Copy(List.Node_List),
                             Current_Element  => null,
                             Current_Position => 0);
  begin
    New_List.Current_Element := New_List.Node_List;
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

    if High > 0 then
      if Low > High then
        raise List_Data_Error;
      end if;

      My_High := High;
    else
      My_High := List.No_Of_Nodes;
    end if;

    if List.Current_Position < Low then
      New_List := (No_Of_Nodes => My_High + 1 - Low,
                   Node_List   => Local_Slice(List.Current_Element,
                                              Low - List.Current_Position,
                                              My_High - List.Current_Position),
                   Current_Element  => null,
                   Current_Position => 0);
    else
      New_List := (No_Of_Nodes => My_High + 1 - Low,
                   Node_List   => Local_Slice(List.Node_List, Low, My_High),
                   Current_Element  => null,
                   Current_Position => 0);
    end if;
    New_List.Current_Element := New_List.Node_List;

    return New_List;
  end Slice;

  -----------------------------------------------------------------------------
  --| Insert
  -----------------------------------------------------------------------------

  procedure Insert(List     : in out List_Type;
                   New_Item : in     List_Type;
                   After    : in     Natural := 0) is

    New_Node_List : Node_List_Type;

  begin
    if List.No_Of_Nodes < After then
      raise List_Constraint_Error;
    end if;

    if Empty(New_Item) then
      return;
    end if;

    New_Node_List := Local_Copy(New_Item.Node_List);

    if List.Current_Position < After then
      Local_Insert(List.Current_Element, New_Node_List,
                   (After - List.Current_Position));
    else
      Local_Insert(List.Node_List, New_Node_List, After);
    end if;
    List.No_Of_Nodes := List.No_Of_Nodes + New_Item.No_Of_Nodes;
    List.Current_Element := New_Node_List;
    List.Current_Position := After;
  end Insert;
  -----------------------------------------------------------------------------
  procedure Insert(List     : in out List_Type;
                   New_Item : in     Data_Type;
                   After    : in     Natural := 0) is

    New_Node_List : Node_List_Type;

  begin
    if List.No_Of_Nodes < After then
      raise List_Constraint_Error;
    end if;

    New_Node_List := Local_New_Node(New_Item);

    if List.Current_Position < After then
      Local_Insert(List.Current_Element, New_Node_List,
                   (After - List.Current_Position));
    else
      Local_Insert(List.Node_List, New_Node_List, After);
    end if;
    List.No_Of_Nodes := List.No_Of_Nodes + 1;
    List.Current_Element := New_Node_List;
    List.Current_Position := After;
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
    Append(New_List, Copy(Right));
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
                             Current_Element  => null,
                             Current_Position => 0);

  begin
    New_List.Current_Element := New_List.Node_List;
    Append(New_List, Copy(Right));
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

    New_Node_List : Node_List_Type;
    My_High       : Positive;

  begin
    if (List.No_Of_Nodes < Low) or else (List.No_Of_Nodes < High) then
      raise List_Constraint_Error;
    end if;

    if High > 0 then
      if Low > High then
        raise List_Data_Error;
      end if;

      My_High := High;
    else
      My_High := List.No_Of_Nodes;
    end if;

    New_Node_List := Local_Copy(New_Item.Node_List);

    if (List.Current_Position + 1) < Low then
      Local_Replace(List.Current_Element, (Low - List.Current_Position),
                    (My_High - List.Current_Position), New_Node_List);
    else
      Local_Replace(List.Node_List, Low, My_High, New_Node_List);
    end if;
    List.No_Of_Nodes := (List.No_Of_Nodes + New_Item.No_Of_Nodes -
                         (My_High + 1 - Low));
    List.Current_Element := New_Node_List;
    List.Current_Position := Low - 1;
  end Replace;
  -----------------------------------------------------------------------------
  procedure Replace(List     : in out List_Type;
                    Low      : in     Positive;
                    High     : in     Natural;
                    New_Item : in     Data_Type) is

    New_Node_List : Node_List_Type;
    My_High       : Positive;

  begin
    if (Low > List.No_Of_Nodes) or else (High > List.No_Of_Nodes) then
      raise List_Constraint_Error;
    end if;

    if High > 0 then
      if Low > High then
        raise List_Data_Error;
      end if;

      My_High := High;
    else
      My_High := List.No_Of_Nodes;
    end if;

    New_Node_List := Local_New_Node(New_Item);

    if (List.Current_Position + 1) < Low then
      Local_Replace(List.Current_Element, (Low - List.Current_Position),
                    (My_High - List.Current_Position), New_Node_List);
    else
      Local_Replace(List.Node_List, Low, My_High, New_Node_List);
    end if;
    -- Next assignment should be:
    --   List.No_Of_Nodes := (List.No_Of_Nodes + 1 - (My_High + 1 - Low));
    -- but the '+ 1' and '- 1' are removed => more effectivity.
    List.No_Of_Nodes := (List.No_Of_Nodes - (My_High - Low));
    List.Current_Element := New_Node_List;
    List.Current_Position := Low - 1;
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

  begin
    if not Empty(List) then
      Local_Turn(List.Node_List);
      List.Current_Position := (List.No_Of_Nodes - 1 - List.Current_Position);
    end if;
  end Turn;

  --===========================================================================
  function Turn(List : in List_Type) return List_Type is

    New_List : List_Type := Copy(List);

  begin
    if not Empty(List) then
      Local_Turn(New_List.Node_List);
      New_List.Current_Position := (New_List.No_Of_Nodes - 1 -
                                    New_List.Current_Position);
    end if;

    return New_List;
  end Turn;

  -----------------------------------------------------------------------------
  --| Iterate
  -----------------------------------------------------------------------------

  procedure Iterate(List   : in List_Type;
                    Action : in Action_Procedure_In_Mode) is

    Node_List : Node_List_Type := List.Node_List;

  begin
    while Node_List /= null loop
      Action(Node_List.Data);
      Node_List := Node_List.Next;
    end loop;
  end Iterate;
  -----------------------------------------------------------------------------
  procedure Iterate(List   : in List_Type;
                    Action : in Action_Procedure_In_Out_Mode) is

    Node_List : Node_List_Type := List.Node_List;

  begin
    while Node_List /= null loop
      Action(Node_List.Data);
      Node_List := Node_List.Next;
    end loop;
  end Iterate;

  --===========================================================================
  procedure Iterate(List   : in List_Type;
                    Action : in Action_Function) is

    Node_List : Node_List_Type := List.Node_List;

  begin
    while Node_List /= null loop
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

    procedure Local_Insert_Sorted(Node_List        : in out Node_List_Type;
                                  New_Item         : in     Node_List_Type;
                                  Current_Position : in out Natural);

    pragma Inline(Local_Insert_Sorted);

    ---------------------------------------------------------------------------
    procedure Local_Sort(Node_List : in out Node_List_Type);

    pragma Inline(Local_Sort);

    ---------------------------------------------------------------------------
    function Local_Sorted(Node_List : in Node_List_Type) return Boolean;

    pragma Inline(Local_Sorted);

    ---------------------------------------------------------------------------
    function Local_Find(Node_List       : in Node_List_Type;
                        Current_Element : in Node_List_Type;
                        Key             : in Key_Type) return Node_List_Type;

    pragma Inline(Local_Find);

    ---------------------------------------------------------------------------
    function Local_Index(Node_List           : in     Node_List_Type;
                         Current_Element     : in     Node_List_Type;
                         Key                 : in     Key_Type) return Natural;

    pragma Inline(Local_Index);

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

      New_Node_List : Node_List_Type;

    begin
      if Empty(New_Item) then
        return;
      end if;

      New_Node_List := Local_Copy(New_Item.Node_List);

      if ((List.Current_Element /= null) and then
          (Get_Key(List.Current_Element.Data) <
           Get_Key(New_Item.Node_List.Data))) then
        -- Use current position for higher efficiency.
        Local_Insert_Sorted(List.Current_Element, New_Node_List,
                            List.Current_Position);
      else
        List.Current_Position := 0;
        Local_Insert_Sorted(List.Node_List, New_Node_List,
                            List.Current_Position);
      end if;
      List.No_Of_Nodes := List.No_Of_Nodes + New_Item.No_Of_Nodes;
      List.Current_Element := New_Node_List;
    end Insert_Sorted;
    ---------------------------------------------------------------------------
    procedure Insert_Sorted(List     : in out List_Type;
                            New_Item : in     Data_Type) is

      New_Node_List : Node_List_Type := Local_New_Node(New_Item);

    begin
      if ((List.Current_Element /= null) and then
          (Get_Key(List.Current_Element.Data) < Get_Key(New_Item))) then
        -- Use current position for higher efficiency.
        Local_Insert_Sorted(List.Current_Element, New_Node_List,
                            List.Current_Position);
      else
        List.Current_Position := 0;
        Local_Insert_Sorted(List.Node_List, New_Node_List,
                            List.Current_Position);
      end if;
      List.No_Of_Nodes := List.No_Of_Nodes + 1;
      List.Current_Element := New_Node_List;
    end Insert_Sorted;

    --=========================================================================
    -- NYI: ... This is not an effective way to do this, but ...
    function Insert_Sorted(List     : in List_Type;
                           New_Item : in List_Type) return List_Type is

      New_List      : List_Type := Copy(List);
      New_Node_List : Node_List_Type;

    begin
      if Empty(New_Item) then
        return New_List;
      end if;

      New_Node_List := Local_Copy(New_Item.Node_List);

      if ((New_List.Current_Element /= null) and then
          (Get_Key(New_List.Current_Element.Data) <
           Get_Key(New_Item.Node_List.Data))) then
        -- Use current position for higher efficiency.
        Local_Insert_Sorted(New_List.Current_Element, New_Node_List,
                            New_List.Current_Position);
      else
        New_List.Current_Position := 0;
        Local_Insert_Sorted(New_List.Node_List, New_Node_List,
                            New_List.Current_Position);
      end if;
      New_List.No_Of_Nodes := New_List.No_Of_Nodes + New_Item.No_Of_Nodes;
      New_List.Current_Element := New_Node_List;

      return New_List;
    end Insert_Sorted;
    ---------------------------------------------------------------------------
    -- NYI: ... This is not an effective way to do this, but ...
    function Insert_Sorted(List     : in List_Type;
                           New_Item : in Data_Type) return List_Type is

      New_List      : List_Type := Copy(List);
      New_Node_List : Node_List_Type;

    begin
      New_Node_List := Local_New_Node(New_Item);

      if ((New_List.Current_Element /= null) and then
          (Get_Key(New_List.Current_Element.Data) < Get_Key(New_Item))) then
        -- Use current position for higher efficiency.
        Local_Insert_Sorted(New_List.Current_Element, New_Node_List,
                            New_List.Current_Position);
      else
        New_List.Current_Position := 0;
        Local_Insert_Sorted(New_List.Node_List, New_Node_List,
                            New_List.Current_Position);
      end if;
      New_List.No_Of_Nodes := New_List.No_Of_Nodes + 1;
      New_List.Current_Element := New_Node_List;

      return New_List;
    end Insert_Sorted;

    ---------------------------------------------------------------------------
    --| Sort
    ---------------------------------------------------------------------------

    procedure Sort(List : in out List_Type) is

    begin
      Local_Sort(List.Node_List);
      List.Current_Element := List.Node_List;
      List.Current_Position := 0;
    end Sort;

    --=========================================================================
    function Sort(List : in List_Type) return List_Type is

      New_List : List_Type := Copy(List);

    begin
      Local_Sort(New_List.Node_List);
      New_List.Current_Element := New_List.Node_List;
      New_List.Current_Position := 0;
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

    begin
      return (Local_Find(List.Node_List, null, Key) /= null);
    end Member;


    ---------------------------------------------------------------------------
    --| Member_Sorted
    ---------------------------------------------------------------------------

    function Member_Sorted(List : in List_Type;
                           Key  : in Key_Type) return Boolean is

      Temp_Current_Element : Node_List_Type;

    begin
      if ((List.Current_Element /= null) and then
          not (Key < Get_Key(List.Current_Element.Data))) then
        Temp_Current_Element := Local_Find(List.Current_Element, null, Key);
      else
        Temp_Current_Element := Local_Find(List.Node_List,
                                           List.Current_Element, Key);
      end if;

      return (Temp_Current_Element /= null);
    end Member_Sorted;

    ---------------------------------------------------------------------------
    --| Find
    ---------------------------------------------------------------------------

    function Find(List : in List_Type;
                  Key  : in Key_Type) return Data_Type is

      Temp_Current_Element : Node_List_Type;

    begin
      Temp_Current_Element := Local_Find(List.Node_List, null, Key);

      if Temp_Current_Element = null then
        raise List_Constraint_Error;
      end if;

      return Copy(Temp_Current_Element.Data);
    end Find;


    ---------------------------------------------------------------------------
    --| Find_Sorted
    ---------------------------------------------------------------------------

    function Find_Sorted(List : in List_Type;
                         Key  : in Key_Type) return Data_Type is

      Temp_Current_Element : Node_List_Type;

    begin
      if ((List.Current_Element /= null) and then
          not (Key < Get_Key(List.Current_Element.Data))) then
        Temp_Current_Element := Local_Find(List.Current_Element, null, Key);
      else
        Temp_Current_Element := Local_Find(List.Node_List,
                                           List.Current_Element, Key);
      end if;

      if Temp_Current_Element = null then
        raise List_Constraint_Error;
      end if;

      return Copy(Temp_Current_Element.Data);
    end Find_Sorted;

    ---------------------------------------------------------------------------
    --| Index
    ---------------------------------------------------------------------------

    function Index(List : in List_Type;
                   Key  : in Key_Type) return Natural is

    begin
      return Local_Index(List.Node_List, null, Key);
    end Index;


    ---------------------------------------------------------------------------
    --| Index_Sorted
    ---------------------------------------------------------------------------

    function Index_Sorted(List : in List_Type;
                          Key  : in Key_Type) return Natural is

    begin
      if ((List.Current_Element /= null) and then
          not (Key < Get_Key(List.Current_Element.Data))) then
        return Local_Index(List.Node_List, null, Key);
      end if;
      return Local_Index(List.Node_List, List.Current_Element, Key);
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
    --| Local_Insert_Sorted
    ---------------------------------------------------------------------------
    -- NYI: ... This is not an effective way to do this, but ...

    procedure Local_Insert_Sorted(Node_List        : in out Node_List_Type;
                                  New_Item         : in     Node_List_Type;
                                  Current_Position : in out Natural) is

    begin
      if (New_Item /= null) then
        if (Node_List = null) then
          Local_Insert(Node_List, New_Item, After => 0);
        elsif not (Get_Key(Node_List.Data) < Get_Key(New_Item.Data)) then
          declare
            Local_Current_Position : Natural        := Current_Position;
            Next_New_Item          : Node_List_Type := New_Item.Next;
          begin
            New_Item.Next := null;
            Local_Insert(Node_List, New_Item, After => 0);
            Current_Position := Current_Position + 1;
            Local_Insert_Sorted(Node_List.Next, Next_New_Item,
                                Current_Position);
            Current_Position := Local_Current_Position;
          end;
        else
          Current_Position := Current_Position + 1;
          Local_Insert_Sorted(Node_List.Next, New_Item, Current_Position);
        end if;
      end if;
    end Local_Insert_Sorted;

    ---------------------------------------------------------------------------
    --| Local_Sort
    ---------------------------------------------------------------------------

    procedure Local_Sort(Node_List : in out Node_List_Type) is

      New_Node_List  : Node_List_Type := null;
      Node           : Node_List_Type;
      Dummy_Position : Natural := 0;

    begin
      while Node_List /= null loop
        Node := Node_List;
        Node_List := Node_List.Next;
        Node.Next := null;
        Local_Insert_Sorted(New_Node_List, Node, Dummy_Position);
      end loop;
      Node_List := New_Node_List;
    end Local_Sort;

    ---------------------------------------------------------------------------
    --| Local_Sorted
    ---------------------------------------------------------------------------

    function Local_Sorted(Node_List : in Node_List_Type) return Boolean is

      First_Node  : Node_List_Type;
      Second_Node : Node_List_Type;

    begin
      if Node_List = null then
        return True;
      end if;

      First_Node := Node_List;
      Second_Node := First_Node.Next;
      while Second_Node /= null loop
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
    --| Local_Find
    ---------------------------------------------------------------------------

    function Local_Find(Node_List       : in Node_List_Type;
                        Current_Element : in Node_List_Type;
                        Key             : in Key_Type) return Node_List_Type is

      Temp_Node_List : Node_List_Type := Node_List;

    begin
      while ((Temp_Node_List /= null) and then
             (Temp_Node_List /= Current_Element)) loop
        exit when (Get_Key(Temp_Node_List.Data) = Key);
        Temp_Node_List := Temp_Node_List.Next;
      end loop;
      return Temp_Node_List;
    end Local_Find;

    ---------------------------------------------------------------------------
    --| Local_Index
    ---------------------------------------------------------------------------

    function Local_Index(Node_List           : in     Node_List_Type;
                         Current_Element     : in     Node_List_Type;
                         Key                 : in     Key_Type)
             return Natural is

      Temp_Node_List : Node_List_Type := Node_List;
      Temp_Index     : Natural := 0;

    begin
      while ((Temp_Node_List /= null) and then
             (Temp_Node_List /= Current_Element)) loop
        Temp_Index := Temp_Index + 1;
        exit when (Get_Key(Temp_Node_List.Data) = Key);
        Temp_Node_List := Temp_Node_List.Next;
      end loop;

      if Temp_Node_List /= null then
        return Temp_Index;
      end if;
      return 0;
    end Local_Index;

    ---------------------------------------------------------------------------
    --| Local_Compare
    ---------------------------------------------------------------------------

    function Local_Compare(Left, Right : in Node_List_Type)
             return Relation_Type is

      Left_List  : Node_List_Type := Left;
      Right_List : Node_List_Type := Right;

    begin
      -- Search after the first difference.
      while Left_List /= null loop
        if Right_List = null then
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

      if Right_List /= null then
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
  --| Local_New_Node
  -----------------------------------------------------------------------------

  function Local_New_Node(Data : in Data_Type) return Node_List_Type is

  begin
    return new Node_Type'(Data => Copy(Data), Next => null);
  end Local_New_Node;

  -----------------------------------------------------------------------------
  --| Local_Delete_First
  --|
  --| Uses "Free" instansiated from "Ada.Unchecked_Deallocation".
  -----------------------------------------------------------------------------

  procedure Free is
    new Ada.Unchecked_Deallocation(Object => Node_Type,
                                   Name   => Node_List_Type);

  -----------------------------------------------------------------------------
  procedure Local_Delete_First(Node_List : in out Node_List_Type) is

    Node : Node_List_Type;

  begin
    Node := Node_List;
    Node_List := Node_List.Next;
    Delete(Node.Data);
    Free(Node);
  end Local_Delete_First;

  pragma Inline(Local_Delete_First);

  -----------------------------------------------------------------------------
  --| Delete
  -----------------------------------------------------------------------------

  procedure Local_Delete(Node_List : in out Node_List_Type) is

  begin
    while Node_List /= null loop
      Local_Delete_First(Node_List);
    end loop;
  end Local_Delete;
  -----------------------------------------------------------------------------
  procedure Local_Delete(Node_List : in out Node_List_Type;
                         Low       : in     Positive;
                         High      : in     Natural) is

    Temp_Node_List : Node_List_Type := Node_List;

  begin
    if Low = 1 then
      -- Delete a part of of list.
      for I in Low..High loop
        Local_Delete_First(Node_List);
      end loop;
    else
      -- Find first element to delete.
      Temp_Node_List := Node_List;
      for I in 3..Low loop
        Temp_Node_List := Temp_Node_List.Next;
      end loop;
      -- Delete a part of rest of list.
      for I in Low..High loop
        Local_Delete_First(Temp_Node_List.Next);
      end loop;
    end if;
  end Local_Delete;

  --===========================================================================
  -- NYI: ... This is not an effective way to do this, but ...
  function Local_Delete(Node_List : in Node_List_Type;
                        Low       : in Positive;
                        High      : in Natural) return Node_List_Type is

    New_Node_List : Node_List_Type := Local_Copy(Node_List);

  begin
    Local_Delete(New_Node_List, Low, High);
    return New_Node_List;
  end Local_Delete;

  -----------------------------------------------------------------------------
  --| Local_Element
  -----------------------------------------------------------------------------

  function Local_Element(Node_List : in Node_List_Type;
                         Index     : in Positive) return Data_Type is

    Temp_Node_List : Node_List_Type := Node_List;

  begin
    for I in 2..Index loop
      Temp_Node_List := Temp_Node_List.Next;
    end loop;
    return Copy(Temp_Node_List.Data);
  end Local_Element;

  -----------------------------------------------------------------------------
  --| Local_Copy
  -----------------------------------------------------------------------------

  function Local_Copy(Node_List : in Node_List_Type) return Node_List_Type is

    Temp_Node_List     : Node_List_Type;
    New_Node_List      : Node_List_Type := null;
    Temp_New_Node_List : Node_List_Type;

  begin
    if Node_List /= null then
      -- Copy the first element.
      Temp_Node_List := Node_List;
      New_Node_List := Local_New_Node(Temp_Node_List.Data);
      Temp_New_Node_List := New_Node_List;
      -- Copy rest of list.
      while Temp_Node_List.Next /= null loop
        Temp_Node_List := Temp_Node_List.Next;
        Temp_New_Node_List.Next := Local_New_Node(Temp_Node_List.Data);
        Temp_New_Node_List := Temp_New_Node_List.Next;
      end loop;
    end if;
    -- Return the slice.
    return New_Node_List;
  end Local_Copy;

  -----------------------------------------------------------------------------
  --| Local_Slice
  -----------------------------------------------------------------------------

  function Local_Slice(Node_List : in Node_List_Type;
                       Low       : in Positive;
                       High      : in Natural) return Node_List_Type is

    Temp_Node_List     : Node_List_Type;
    New_Node_List      : Node_List_Type := null;
    Temp_New_Node_List : Node_List_Type;

  begin
    -- Find first element to copy.
    Temp_Node_List := Node_List;
    for I in 2..Low loop
      Temp_Node_List := Temp_Node_List.Next;
    end loop;
    -- Copy the first element of slice.
    New_Node_List := Local_New_Node(Temp_Node_List.Data);
    Temp_New_Node_List := New_Node_List;
    -- Copy rest of slice.
    for I in (Low + 1)..High loop
      Temp_Node_List := Temp_Node_List.Next;
      Temp_New_Node_List.Next := Local_New_Node(Temp_Node_List.Data);
      Temp_New_Node_List := Temp_New_Node_List.Next;
    end loop;
    -- Return the slice.
    return New_Node_List;
  end Local_Slice;

  -----------------------------------------------------------------------------
  --| Local_Insert
  -----------------------------------------------------------------------------

  procedure Local_Insert(Node_List : in out Node_List_Type;
                         New_Item  : in     Node_List_Type;
                         After     : in     Natural) is

    Temp_List     : Node_List_Type;
    New_Temp_List : Node_List_Type;

  begin
    if After = 0 then
      -- Find last element in new list.
      New_Temp_List := New_Item;
      while New_Temp_List.Next /= null loop
        New_Temp_List := New_Temp_List.Next;
      end loop;
      -- Insert list after new list.
      New_Temp_List.Next := Node_List;
      -- Set list to be the whole list.
      Node_List := New_Item;
    else
      -- Find right place to insert the new list.
      Temp_List := Node_List;
      for I in 2..After loop
        Temp_List := Temp_List.Next;
      end loop;
      -- Find last element in new list.
      New_Temp_List := New_Item;
      while New_Temp_List.Next /= null loop
        New_Temp_List := New_Temp_List.Next;
      end loop;
      -- Insert rest of list after new list.
      New_Temp_List.Next := Temp_List.Next;
      -- Insert new list at right place in list.
      Temp_List.Next := New_Item;
    end if;
  end Local_Insert;

  -----------------------------------------------------------------------------
  --| Local_Replace
  -----------------------------------------------------------------------------

  procedure Local_Replace(Node_List : in out Node_List_Type;
                          Low       : in     Positive;
                          High      : in     Natural;
                          New_Item  : in     Node_List_Type) is

    Temp_Node_List : Node_List_Type;

  begin
    if Low = 1 then
      -- Delete a part of of list.
      for I in Low..High loop
        Local_Delete_First(Node_List);
      end loop;
      -- Insert the new list.
      Local_Insert(Node_List, New_Item, After => 0);
    else
      -- Find first element to replace (delete)
      Temp_Node_List := Node_List;
      for I in 3..Low loop
        Temp_Node_List := Temp_Node_List.Next;
      end loop;
      -- Delete a part of rest of list.
      Local_Delete(Temp_Node_List, 2, (High + 2 - Low));
      -- Insert the new list.
      Local_Insert(Temp_Node_List, New_Item, After => 1);
    end if;
  end Local_Replace;

  -----------------------------------------------------------------------------
  --| Local_Turn
  -----------------------------------------------------------------------------

  procedure Local_Turn(Node_List : in out Node_List_Type) is

    New_Node_List : Node_List_Type := null;
    Node          : Node_List_Type;

  begin
    while Node_List /= null loop
      Node := Node_List;
      Node_List := Node_List.Next;
      Node.Next := null;
      Local_Insert(New_Node_List, Node, After => 0);
    end loop;
    Node_List := New_Node_List;
  end Local_Turn;


  --***************************************************************************
  --| Definition of initiation part.
  --***************************************************************************

begin
  -- Nothing have to be done.
  null;
end TJa.Lists.Internal.Single_Linked;
