-------------------------------------------------------------------------------
--|                                                                         |--
--|                       Torbjörn Jonsson Ada library                      |--
--|                                                                         |--
--|                            T J A . Q U E U E                            |--
--|                                                                         |--
--|                              Specification                              |--
--|                              Version  2.00                              |--
--|                                                                         |--
--|                           (C) Copyright, 2000                           |--
--|                   Torbjörn Jonsson,  TorJo@Ida.LiU.se                   |--
--|                                                                         |--
-------------------------------------------------------------------------------
--|                                                                         |--
--| Versions:                                                               |--
--|                                                                         |--
--|   2000-02-22  Version 2.00 is ok.                                       |--
--|               Created and documented by Torbjörn Jonsson.               |--
--|                                                                         |--
-------------------------------------------------------------------------------
--|                                                                         |--
--| Description:                                                            |--
--|                                                                         |--
--|   This is a queue package which can be used to queue "all" different    |--
--|   types of elements.                                                    |--
--|                                                                         |--
-------------------------------------------------------------------------------

-- Ada standard libraries.
with Ada.Finalization;                  use Ada.Finalization;

-- External libraries.

-- Internal libraries.

-------------------------------------------------------------------------------
generic

  -- "Data_Type" represents the data inserted in the queue.
  type Data_Type is private;

package TJa.Queue is

  -----------------------------------------------------------------------------
  --| "Queue_Type" is the type for a general queue. The value "Null_Queue" are
  --| used to identify that the queue is "empty".
  -----------------------------------------------------------------------------

  type Queue_Type is private;

  Null_Queue : constant Queue_Type;

  -----------------------------------------------------------------------------
  --| "Empty" equals 'True' if there are no elements in queue.
  -----------------------------------------------------------------------------

  function Empty(Queue : in Queue_Type) return Boolean;

  -----------------------------------------------------------------------------
  --| "Length" returns number of elements in queue.
  -----------------------------------------------------------------------------

  function Length(Queue : in Queue_Type) return Natural;

  -----------------------------------------------------------------------------
  --| "Enqueue" is used to insert a new item to the front of the queue.
  -----------------------------------------------------------------------------

  procedure Enqueue(Queue : in out Queue_Type;
                    Item  : in     Data_Type);

  -----------------------------------------------------------------------------
  --| "Dequeue" is used to remove and return an item from the front of the
  --| queue. If "Dequeue" only have one parameter it just removes the front
  --| element without returning it.
  -----------------------------------------------------------------------------

  procedure Dequeue(Queue : in out Queue_Type);
  procedure Dequeue(Queue : in out Queue_Type;
                    Item  :    out Data_Type);

  -----------------------------------------------------------------------------
  --| "Front" is used to copy the front element from queue. The queue is
  --| unchanged.
  -----------------------------------------------------------------------------

  procedure Front(Queue : in     Queue_Type;
                  Item  :    out Data_Type);

  function Front(Queue : in Queue_Type) return Data_Type;

  -----------------------------------------------------------------------------
  --| "Delete" is used to remove all items from queue.
  -----------------------------------------------------------------------------

  procedure Delete(Queue : in out Queue_Type);

  -----------------------------------------------------------------------------
  --| Special exception for queue package.
  -----------------------------------------------------------------------------

  Empty_Queue_Error : exception;  -- Tried to dequeue/front from empty queue.

  -----------------------------------------------------------------------------

private

  -----------------------------------------------------------------------------
  --| Internal representation of queue needs som type of nodes and we use
  --| pointers to these nodes.
  -----------------------------------------------------------------------------

  type Node_Type;
  type Node_Access_Type is access Node_Type;

  type Queue_Type is
    new Controlled with
       record
         No_Of_Nodes : Natural := 0;
         Node_List   : Node_Access_Type := null;
         Last_Node   : Node_Access_Type := null;
       end record;

  -----------------------------------------------------------------------------
  --| The "Queue_Type" is a safe queue type. If an queue object isn't deleted
  --| before it's destroyed the finalization library is used to do that.
  -----------------------------------------------------------------------------

  procedure Initialize(Queue : in out Queue_Type);
  procedure Adjust(Queue : in out Queue_Type);
  procedure Finalize(Queue : in out Queue_Type);

  -----------------------------------------------------------------------------
  --| The constant that represents "empty" queue.
  -----------------------------------------------------------------------------

  Null_Queue : constant Queue_Type := (Controlled with
                                       No_Of_Nodes => 0,
                                       Node_List   => null,
                                       Last_Node   => null);

  -----------------------------------------------------------------------------

end TJa.Queue;
