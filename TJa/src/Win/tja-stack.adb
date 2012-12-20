-------------------------------------------------------------------------------
--|                                                                         |--
--|                       Torbjörn Jonsson Ada library                      |--
--|                                                                         |--
--|                            T J A . S T A C K                            |--
--|                                                                         |--
--|                           Body implementation                           |--
--|                              Version  2.00                              |--
--|                                                                         |--
--|                           (C) Copyright, 2000                           |--
--|                   Torbjörn Jonsson,  TorJo@Ida.LiU.se                   |--
--|                                                                         |--
-------------------------------------------------------------------------------
--|                                                                         |--
--| Change log:                                                             |--
--|                                                                         |--
--|   2000-02-22  Version 2.00 is ok.                                       |--
--|               Created and documented by Torbjörn Jonsson.               |--
--|                                                                         |--
-------------------------------------------------------------------------------
--|                                                                         |--
--| Implementation details:                                                 |--
--|                                                                         |--
--|   The stack is implemented as a simple single linked list with no       |--
--|   special check for memory error.                                       |--
--|                                                                         |--
-------------------------------------------------------------------------------

-- Ada standard libraries.
with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Finalization;                  use Ada.Finalization;
with Ada.Unchecked_Deallocation;

-- External libraries.

-- Internal libraries.

-------------------------------------------------------------------------------
package body TJa.Stack is

  --***************************************************************************
  --| Declarations of local types, constants and methods.
  --***************************************************************************

  -----------------------------------------------------------------------------
  --| "Node_Type" is used to contain users data (generic "Data_Type").
  -----------------------------------------------------------------------------

  type Node_Type is
       record
         Data : Data_Type;
         Next : Node_Access_Type := null;
       end record;

  -----------------------------------------------------------------------------
  --| These methods is used for internal "Node_List".
  -----------------------------------------------------------------------------

  function New_Node(Data : in Data_Type)
           return Node_Access_Type;

  function Copy(List : in Node_Access_Type)
           return Node_Access_Type;

  procedure Delete(List : in out Node_Access_Type);


  --***************************************************************************
  --| Definitions of public methods.
  --***************************************************************************

  -----------------------------------------------------------------------------
  --| Empty
  -----------------------------------------------------------------------------

  function Empty(Stack : in Stack_Type)
           return Boolean is

  begin
    return (Size(Stack) = 0);
  end Empty;

  -----------------------------------------------------------------------------
  --| Size
  -----------------------------------------------------------------------------

  function Size(Stack : in Stack_Type)
           return Natural is

  begin
    return Stack.No_Of_Nodes;
  end Size;

  -----------------------------------------------------------------------------
  --| Push
  -----------------------------------------------------------------------------

  procedure Push(Stack : in out Stack_Type;
                 Item  : in     Data_Type) is

    Temp : Node_Access_Type := Stack.Node_List;

  begin
    Stack.Node_List := New_Node(Item);
    Stack.Node_List.Next := Temp;
    Stack.No_Of_Nodes := Stack.No_Of_Nodes + 1;
  end Push;

  -----------------------------------------------------------------------------
  --| Pop
  -----------------------------------------------------------------------------

  procedure Pop(Stack : in out Stack_Type;
                Item  :    out Data_Type) is

  begin
    Item := Top(Stack);
    Pop(Stack);
  end Pop;
  -----------------------------------------------------------------------------
  procedure Pop(Stack : in out Stack_Type) is

    Temp : Node_Access_Type := Stack.Node_List;

  begin
    if Empty(Stack) then
      raise Empty_Stack_Error;
    else
      Stack.Node_List := Stack.Node_List.Next;
      Stack.No_Of_Nodes := Stack.No_Of_Nodes - 1;
      Temp.Next := null;
      Delete(Temp);
    end if;
  end Pop;

  -----------------------------------------------------------------------------
  --| Top
  -----------------------------------------------------------------------------

  procedure Top(Stack : in     Stack_Type;
                Item  :    out Data_Type) is

  begin
    Item := Top(Stack);
  end Top;

  --===========================================================================
  function Top(Stack : in Stack_Type)
           return Data_Type is

  begin
    if Empty(Stack) then
      raise Empty_Stack_Error;
    end if;

    return Stack.Node_List.Data;
  end Top;

  -----------------------------------------------------------------------------
  --| Delete
  -----------------------------------------------------------------------------

  procedure Delete(Stack : in out Stack_Type) is

  begin
    Delete(Stack.Node_List);
  end Delete;

  -----------------------------------------------------------------------------
  --| The methods for automatic "Initiation", "Adjustment" and "Finalization".
  -----------------------------------------------------------------------------

  procedure Initialize(Stack : in out Stack_Type) is

  begin
    null;
  end Initialize;
  -----------------------------------------------------------------------------
  procedure Adjust(Stack : in out Stack_Type) is

    Node_List : Node_Access_Type := Copy(Stack.Node_List);

  begin
    Stack.Node_List := Node_List;
  end Adjust;
  -----------------------------------------------------------------------------
  procedure Finalize(Stack : in out Stack_Type) is

  begin
    Delete(Stack);
  end Finalize;


  --***************************************************************************
  --| Definitions of local methods.
  --***************************************************************************

  -----------------------------------------------------------------------------
  --| New_Node
  -----------------------------------------------------------------------------

  function New_Node(Data : in Data_Type)
           return Node_Access_Type is

    Node : Node_Access_Type := new Node_Type;

  begin
    Node.Data := Data;
    return Node;
  end New_Node;

  -----------------------------------------------------------------------------
  --| Copy
  -----------------------------------------------------------------------------

  function Copy(List : in Node_Access_Type)
           return Node_Access_Type is

    New_List : Node_Access_Type := null;

  begin
    if List /= null then
      New_List := New_Node(List.Data);
      New_List.Next := Copy(List.Next);
    end if;

    return New_List;
  end Copy;

  -----------------------------------------------------------------------------
  --| Delete
  --|
  --| Uses "Free" instansiated from "Ada.Unchecked_Deallocation".
  -----------------------------------------------------------------------------

  procedure Free is
    new Ada.Unchecked_Deallocation(Object => Node_Type,
                                   Name   => Node_Access_Type);
  -----------------------------------------------------------------------------
  procedure Delete(List : in out Node_Access_Type) is

  begin
    if List /= null then
      Delete(List.Next);
      Free(List);
    end if;
  end Delete;

  -----------------------------------------------------------------------------

end TJa.Stack;
