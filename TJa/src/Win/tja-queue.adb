-------------------------------------------------------------------------------
--|                                                                         |--
--|                       Torbjörn Jonsson Ada library                      |--
--|                                                                         |--
--|                            T J A . Q U E U E                            |--
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
package body TJa.Queue is

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
         Prev : Node_Access_Type := null;
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

  function Empty(Queue : in Queue_Type)
           return Boolean is

  begin
    return (Length(Queue) = 0);
  end Empty;

  -----------------------------------------------------------------------------
  --| Length
  -----------------------------------------------------------------------------

  function Length(Queue : in Queue_Type)
           return Natural is

  begin
    return Queue.No_Of_Nodes;
  end Length;

  -----------------------------------------------------------------------------
  --| Enqueue
  -----------------------------------------------------------------------------

  procedure Enqueue(Queue : in out Queue_Type;
                    Item  : in     Data_Type) is

    Temp : Node_Access_Type := New_Node(Item);

  begin
    Temp.Prev := Queue.Last_Node;
    if Queue.No_Of_Nodes = 0 then
      Queue.Node_List := Temp;
    else
      Queue.Last_Node.Next := Temp;
    end if;
    Queue.Last_Node := Temp;

    Queue.No_Of_Nodes := Queue.No_Of_Nodes + 1;
  end Enqueue;

  -----------------------------------------------------------------------------
  --| Dequeue
  -----------------------------------------------------------------------------

  procedure Dequeue(Queue : in out Queue_Type;
                    Item  :    out Data_Type) is

  begin
    Item := Front(Queue);
    Dequeue(Queue);
  end Dequeue;
  -----------------------------------------------------------------------------
  procedure Dequeue(Queue : in out Queue_Type) is

    Temp : Node_Access_Type := Queue.Node_List;

  begin
    if Empty(Queue) then
      raise Empty_Queue_Error;
    else
      Queue.Node_List := Queue.Node_List.Next;
      if Queue.No_Of_Nodes = 1 then
        Queue.Last_Node := null;
      end if;

      Queue.No_Of_Nodes := Queue.No_Of_Nodes - 1;

      Temp.Next := null;
      Delete(Temp);
    end if;
  end Dequeue;

  -----------------------------------------------------------------------------
  --| Front
  -----------------------------------------------------------------------------

  procedure Front(Queue : in     Queue_Type;
                  Item  :    out Data_Type) is

  begin
    Item := Front(Queue);
  end Front;

  --===========================================================================
  function Front(Queue : in Queue_Type)
           return Data_Type is

  begin
    if Empty(Queue) then
      raise Empty_Queue_Error;
    end if;

    return Queue.Node_List.Data;
  end Front;

  -----------------------------------------------------------------------------
  --| Delete
  -----------------------------------------------------------------------------

  procedure Delete(Queue : in out Queue_Type) is

  begin
    Delete(Queue.Node_List);
  end Delete;

  -----------------------------------------------------------------------------
  --| The methods for automatic "Initiation", "Adjustment" and "Finalization".
  -----------------------------------------------------------------------------

  procedure Initialize(Queue : in out Queue_Type) is

  begin
    null;
  end Initialize;
  -----------------------------------------------------------------------------
  procedure Adjust(Queue : in out Queue_Type) is

    Node_List : Node_Access_Type := Copy(Queue.Node_List);

  begin
    Queue.Node_List := Node_List;
  end Adjust;
  -----------------------------------------------------------------------------
  procedure Finalize(Queue : in out Queue_Type) is

  begin
    Delete(Queue);
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
      if New_List.Next /= null then
        New_List.Next.Prev := New_List;
      end if;
      New_List.Prev := null;
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

end TJa.Queue;
