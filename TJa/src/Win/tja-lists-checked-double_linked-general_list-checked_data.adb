-------------------------------------------------------------------------------
--|                                                                         |--
--|                       Torbjörn Jonsson Ada library                      |--
--|                                                                         |--
--|      T J A . L I S T S . C H E C K E D . D O U B L E _ L I N K E D      |--
--|           . G E N E R A L _ L I S T . C H E C K E D _ D A T A           |--
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
--|   2000-11-26  Version 3.00 is ok.                                       |--
--|                                                                         |--
-------------------------------------------------------------------------------
--|                                                                         |--
--| Implementation details:                                                 |--
--|                                                                         |--
-------------------------------------------------------------------------------

package body TJa.Lists.Checked.Double_Linked.General_List.Checked_Data is

  --***************************************************************************
  --| Declarations of local types, constants and methods.
  --***************************************************************************

  package I_L renames Internal_List;


  --***************************************************************************
  --| Definitions of public methods.
  --***************************************************************************

  -----------------------------------------------------------------------------
  --| Empty
  -----------------------------------------------------------------------------

  function Empty(List : in List_Type) return Boolean is

  begin
    return I_L.Empty(I_L.List_Type(List.List));
  end Empty;

  -----------------------------------------------------------------------------
  --| Length
  -----------------------------------------------------------------------------

  function Length(List : in List_Type) return Natural is

  begin
    return I_L.Length(I_L.List_Type(List.List));
  end Length;

  -----------------------------------------------------------------------------
  --| Delete
  -----------------------------------------------------------------------------

  procedure Delete(List : in out List_Type) is

  begin
    I_L.Delete(I_L.List_Type(List.List));
  end Delete;
  -----------------------------------------------------------------------------
  procedure Delete(List  : in out List_Type;
                   Index : in     Positive) is

  begin
    I_L.Delete(I_L.List_Type(List.List), Index, Index);
  end Delete;
  -----------------------------------------------------------------------------
  procedure Delete(List : in out List_Type;
                   Low  : in     Positive;
                   High : in     Natural) is

  begin
    I_L.Delete(I_L.List_Type(List.List), Low, High);
  end Delete;

  --===========================================================================
  function Delete(List  : in List_Type;
                  Index : in Positive) return List_Type is

  begin
    return (Controlled with
            List => I_L.Delete(I_L.List_Type(List.List), Index, Index));
  end Delete;
  -----------------------------------------------------------------------------
  function Delete(List : in List_Type;
                  Low  : in Positive;
                  High : in Natural) return List_Type is

  begin
    return (Controlled with
            List => I_L.Delete(I_L.List_Type(List.List), Low, High));
  end Delete;

  -----------------------------------------------------------------------------
  --| Element
  -----------------------------------------------------------------------------

  function Element(List  : in List_Type;
                   Index : in Positive) return Data_Type is

  begin
    return I_L.Element(I_L.List_Type(List.List), Index);
  end Element;

  -----------------------------------------------------------------------------
  --| Copy
  -----------------------------------------------------------------------------

  function Copy(List : in List_Type) return List_Type is

  begin
    return (Controlled with
            List => I_L.Copy(I_L.List_Type(List.List)));
  end Copy;

  -----------------------------------------------------------------------------
  --| Slice
  -----------------------------------------------------------------------------

  function Slice(List : in List_Type;
                 Low  : in Positive;
                 High : in Natural) return List_Type is

  begin
    return (Controlled with
            List => I_L.Slice(I_L.List_Type(List.List), Low, High));
  end Slice;

  -----------------------------------------------------------------------------
  --| Insert
  -----------------------------------------------------------------------------

  procedure Insert(List     : in out List_Type;
                   New_Item : in     List_Type;
                   After    : in     Natural := 0) is

  begin
    I_L.Insert(I_L.List_Type(List.List), I_L.List_Type(New_Item.List), After);
  end Insert;
  -----------------------------------------------------------------------------
  procedure Insert(List     : in out List_Type;
                   New_Item : in     Data_Type;
                   After    : in     Natural := 0) is

  begin
    I_L.Insert(I_L.List_Type(List.List), New_Item, After);
  end Insert;

  --===========================================================================
  function Insert(List     : in List_Type;
                  New_Item : in List_Type;
                  After    : in Natural := 0) return List_Type is

  begin
    return (Controlled with
            List => I_L.Insert(I_L.List_Type(List.List),
                               I_L.List_Type(New_Item.List), After));
  end Insert;
  -----------------------------------------------------------------------------
  function Insert(List     : in List_Type;
                  New_Item : in Data_Type;
                  After    : in Natural := 0) return List_Type is

  begin
    return (Controlled with
            List => I_L.Insert(I_L.List_Type(List.List), New_Item, After));
  end Insert;

  -----------------------------------------------------------------------------
  --| Append
  -----------------------------------------------------------------------------

  procedure Append(List     : in out List_Type;
                   New_Item : in     List_Type) is

  begin
    I_L.Append(I_L.List_Type(List.List), I_L.List_Type(New_Item.List));
  end Append;
  -----------------------------------------------------------------------------
  procedure Append(List     : in out List_Type;
                   New_Item : in     Data_Type) is

  begin
    I_L.Append(I_L.List_Type(List.List), New_Item);
  end Append;

  -----------------------------------------------------------------------------
  --| "&"
  -----------------------------------------------------------------------------

  function "&"(Left, Right : in List_Type) return List_Type is

  begin
    return (Controlled with
            List => I_L."&"(I_L.List_Type(Left.List),
                            I_L.List_Type(Right.List)));
  end "&";
  -----------------------------------------------------------------------------
  function "&"(Left  : in List_Type;
               Right : in Data_Type) return List_Type is

  begin
    return (Controlled with
            List => I_L."&"(I_L.List_Type(Left.List), Right));
  end "&";
  -----------------------------------------------------------------------------
  function "&"(Left  : in Data_Type;
               Right : in List_Type) return List_Type is

  begin
    return (Controlled with
            List => I_L."&"(Left, I_L.List_Type(Right.List)));
  end "&";

  -----------------------------------------------------------------------------
  --| Replace
  -----------------------------------------------------------------------------

  procedure Replace(List     : in out List_Type;
                    Index    : in     Positive;
                    New_Item : in     List_Type) is

  begin
    I_L.Replace(I_L.List_Type(List.List), Index, I_L.List_Type(New_Item.List));
  end Replace;
  -----------------------------------------------------------------------------
  procedure Replace(List     : in out List_Type;
                    Index    : in     Positive;
                    New_Item : in     Data_Type) is

  begin
    I_L.Replace(I_L.List_Type(List.List), Index, New_Item);
  end Replace;
  -----------------------------------------------------------------------------
  procedure Replace(List     : in out List_Type;
                    Low      : in     Positive;
                    High     : in     Natural;
                    New_Item : in     List_Type) is

  begin
    I_L.Replace(I_L.List_Type(List.List), Low, High,
                I_L.List_Type(New_Item.List));
  end Replace;
  -----------------------------------------------------------------------------
  procedure Replace(List     : in out List_Type;
                    Low      : in     Positive;
                    High     : in     Natural;
                    New_Item : in     Data_Type) is

  begin
    I_L.Replace(I_L.List_Type(List.List), Low, High, New_Item);
  end Replace;

  --===========================================================================
  function Replace(List     : in List_Type;
                   Index    : in Positive;
                   New_Item : in List_Type) return List_Type is

  begin
    return (Controlled with
            List => I_L.Replace(I_L.List_Type(List.List), Index,
                                I_L.List_Type(New_Item.List)));
  end Replace;
  -----------------------------------------------------------------------------
  function Replace(List     : in List_Type;
                   Index    : in Positive;
                   New_Item : in Data_Type) return List_Type is

  begin
    return (Controlled with
            List => I_L.Replace(I_L.List_Type(List.List), Index, New_Item));
  end Replace;
  -----------------------------------------------------------------------------
  function Replace(List     : in List_Type;
                   Low      : in Positive;
                   High     : in Natural;
                   New_Item : in List_Type) return List_Type is

  begin
    return (Controlled with
            List => I_L.Replace(I_L.List_Type(List.List), Low, High,
                                I_L.List_Type(New_Item.List)));
  end Replace;
  -----------------------------------------------------------------------------
  function Replace(List     : in List_Type;
                   Low      : in Positive;
                   High     : in Natural;
                   New_Item : in Data_Type) return List_Type is

  begin
    return (Controlled with
            List => I_L.Replace(I_L.List_Type(List.List), Low, High,
                                New_Item));
  end Replace;

  -----------------------------------------------------------------------------
  --| "Turn" reverses the list.
  -----------------------------------------------------------------------------

  procedure Turn(List : in out List_Type) is

  begin
    I_L.Turn(I_L.List_Type(List.List));
  end Turn;

  --===========================================================================
  function Turn(List : in List_Type) return List_Type is

  begin
    return (Controlled with
            List => I_L.Turn(I_L.List_Type(List.List)));
  end Turn;

  -----------------------------------------------------------------------------
  --| Iterate
  -----------------------------------------------------------------------------

  procedure Iterate(List   : in List_Type;
                    Action : in Action_Procedure_In_Mode) is

  begin
    I_L.Iterate(I_L.List_Type(List.List), Action);
  end Iterate;
  -----------------------------------------------------------------------------
  procedure Iterate(List   : in List_Type;
                    Action : in Action_Procedure_In_Out_Mode) is

  begin
    I_L.Iterate(I_L.List_Type(List.List), Action);
  end Iterate;

  --===========================================================================
  procedure Iterate(List   : in List_Type;
                    Action : in Action_Function) is

  begin
    I_L.Iterate(I_L.List_Type(List.List), Action);
  end Iterate;


  --**************************************************************************
  --| Definitions of local methods.
  --***************************************************************************

  -----------------------------------------------------------------------------
  --| Copy_Dummy, Delete_Dummy
  -----------------------------------------------------------------------------

  function Copy_Dummy(Item : in Data_Type) return Data_Type is

  begin
    return Item;
  end Copy_Dummy;
  -----------------------------------------------------------------------------
  procedure Delete_Dummy(Item : in out Data_Type) is

  begin
    null;
  end Delete_Dummy;

  -----------------------------------------------------------------------------
  --| The methods for automatic "Initiation", "Adjustment" and "Finalization".
  -----------------------------------------------------------------------------

  procedure Initialize(List : in out List_Type) is

  begin
    null;
  end Initialize;
  -----------------------------------------------------------------------------
  procedure Adjust(List : in out List_Type) is

  begin
    List.List := I_L.Copy(I_L.List_Type(List.List));
  end Adjust;
  -----------------------------------------------------------------------------
  procedure Finalize(List : in out List_Type) is

  begin
    I_L.Delete(I_L.List_Type(List.List));
  end Finalize;


  --***************************************************************************
  --| Definition of initiation part.
  --***************************************************************************

begin
  -- Nothing have to be done.
  null;
end TJa.Lists.Checked.Double_Linked.General_List.Checked_Data;
