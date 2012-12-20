-------------------------------------------------------------------------------
--|                                                                         |--
--|                       Torbjörn Jonsson Ada library                      |--
--|                                                                         |--
--|                            T J A . S T A C K                            |--
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
--|   This is a stack package which can be used to stack "all" different    |--
--|   types of elements.                                                    |--
--|                                                                         |--
-------------------------------------------------------------------------------

-- Ada standard libraries.
with Ada.Finalization;                  use Ada.Finalization;

-- External libraries.

-- Internal libraries.

-------------------------------------------------------------------------------
generic

  -- "Data_Type" represents the data inserted in the stack.
  type Data_Type is private;

package TJa.Stack is

  -----------------------------------------------------------------------------
  --| "Stack_Type" is the type for a general stack. The value "Null_Stack" are
  --| used to identify that the stack is "empty".
  -----------------------------------------------------------------------------

  type Stack_Type is private;

  Null_Stack : constant Stack_Type;

  -----------------------------------------------------------------------------
  --| "Empty" equals 'True' if there are no elements in stack.
  -----------------------------------------------------------------------------

  function Empty(Stack : in Stack_Type) return Boolean;

  -----------------------------------------------------------------------------
  --| "Size" returns number of elements in stack.
  -----------------------------------------------------------------------------

  function Size(Stack : in Stack_Type) return Natural;

  -----------------------------------------------------------------------------
  --| "Push" is used to insert a new item to the top of the stack.
  -----------------------------------------------------------------------------

  procedure Push(Stack : in out Stack_Type;
                 Item  : in     Data_Type);

  -----------------------------------------------------------------------------
  --| "Pop" is used to remove and return an item from the top of the stack. If
  --| "Pop" only have one parameter it just removes the top element without
  --| returning it.
  -----------------------------------------------------------------------------

  procedure Pop(Stack : in out Stack_Type);
  procedure Pop(Stack : in out Stack_Type;
                Item  :    out Data_Type);

  -----------------------------------------------------------------------------
  --| "Top" is used to copy the top element from stack. The stack is unchanged.
  -----------------------------------------------------------------------------

  procedure Top(Stack : in     Stack_Type;
                Item  :    out Data_Type);

  function Top(Stack : in Stack_Type) return Data_Type;

  -----------------------------------------------------------------------------
  --| "Delete" is used to remove all items from stack.
  -----------------------------------------------------------------------------

  procedure Delete(Stack : in out Stack_Type);

  -----------------------------------------------------------------------------
  --| Special exception for stack package.
  -----------------------------------------------------------------------------

  Empty_Stack_Error : exception;  -- Tried to pop/top from empty stack.

  -----------------------------------------------------------------------------

private

  -----------------------------------------------------------------------------
  --| Internal representation of stack needs som type of nodes and we use
  --| pointers to these nodes.
  -----------------------------------------------------------------------------

  type Node_Type;
  type Node_Access_Type is access Node_Type;

  type Stack_Type is
    new Controlled with
       record
         No_Of_Nodes : Natural := 0;
         Node_List   : Node_Access_Type := null;
       end record;

  -----------------------------------------------------------------------------
  --| The "Stack_Type" is a safe stack type. If an stack object isn't deleted
  --| before it's destroyed the finalization library is used to do that.
  -----------------------------------------------------------------------------

  procedure Initialize(Stack : in out Stack_Type);
  procedure Adjust(Stack : in out Stack_Type);
  procedure Finalize(Stack : in out Stack_Type);

  -----------------------------------------------------------------------------
  --| The constant that represents "empty" stack.
  -----------------------------------------------------------------------------

  Null_Stack : constant Stack_Type := (Controlled with
                                       No_Of_Nodes => 0,
                                       Node_List   => null);

  -----------------------------------------------------------------------------

end TJa.Stack;
