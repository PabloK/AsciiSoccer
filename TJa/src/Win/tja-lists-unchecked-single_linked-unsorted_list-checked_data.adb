-------------------------------------------------------------------------------
--|                                                                         |--
--|                       Torbjörn Jonsson Ada library                      |--
--|                                                                         |--
--|    T J A . L I S T S . U N C H E C K E D . S I N G L E _ L I N K E D    |--
--|          . U N S O R T E D _ L I S T . C H E C K E D _ D A T A          |--
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

package body TJa.Lists.Unchecked.Single_Linked.Unsorted_List.Checked_Data is

  --***************************************************************************
  --| Declarations of local types, constants and methods.
  --***************************************************************************

  -- Needs the internal package with methods depending on keys.

  package Internal_List_With_Key is
    new Internal_List.With_Key(Key_Type => Key_Type);

  -----------------------------------------------------------------------------
  package I_L renames Internal_List;
  package I_L_W_K renames Internal_List_With_Key;


  --***************************************************************************
  --| Definitions of public methods.
  --***************************************************************************

  -----------------------------------------------------------------------------
  --| Empty
  -----------------------------------------------------------------------------

  function Empty(List : in List_Type) return Boolean is

  begin
    return I_L.Empty(I_L.List_Type(List));
  end Empty;

  -----------------------------------------------------------------------------
  --| Length
  -----------------------------------------------------------------------------

  function Length(List : in List_Type) return Natural is

  begin
    return I_L.Length(I_L.List_Type(List));
  end Length;

  -----------------------------------------------------------------------------
  --| Delete
  -----------------------------------------------------------------------------

  procedure Delete(List : in out List_Type) is

  begin
    I_L.Delete(I_L.List_Type(List));
  end Delete;
  -----------------------------------------------------------------------------
  procedure Delete(List  : in out List_Type;
                   Index : in     Positive) is

  begin
    I_L.Delete(I_L.List_Type(List), Index, Index);
  end Delete;
  -----------------------------------------------------------------------------
  procedure Delete(List : in out List_Type;
                   Low  : in     Positive;
                   High : in     Natural) is

  begin
    I_L.Delete(I_L.List_Type(List), Low, High);
  end Delete;

  --===========================================================================
  function Delete(List  : in List_Type;
                  Index : in Positive) return List_Type is

  begin
    return List_Type(I_L.Delete(I_L.List_Type(List), Index, Index));
  end Delete;
  -----------------------------------------------------------------------------
  function Delete(List : in List_Type;
                  Low  : in Positive;
                  High : in Natural) return List_Type is

  begin
    return List_Type(I_L.Delete(I_L.List_Type(List), Low, High));
  end Delete;

  -----------------------------------------------------------------------------
  --| Element
  -----------------------------------------------------------------------------

  function Element(List  : in List_Type;
                   Index : in Positive) return Data_Type is

  begin
    return I_L.Element(I_L.List_Type(List), Index);
  end Element;

  -----------------------------------------------------------------------------
  --| Copy
  -----------------------------------------------------------------------------

  function Copy(List : in List_Type) return List_Type is

  begin
    return List_Type(I_L.Copy(I_L.List_Type(List)));
  end Copy;

  -----------------------------------------------------------------------------
  --| Slice
  -----------------------------------------------------------------------------

  function Slice(List : in List_Type;
                 Low  : in Positive;
                 High : in Natural) return List_Type is

  begin
    return List_Type(I_L.Slice(I_L.List_Type(List), Low, High));
  end Slice;

  -----------------------------------------------------------------------------
  --| Insert
  -----------------------------------------------------------------------------

  procedure Insert(List     : in out List_Type;
                   New_Item : in     List_Type;
                   After    : in     Natural := 0) is

  begin
    I_L.Insert(I_L.List_Type(List), I_L.List_Type(New_Item), After);
  end Insert;
  -----------------------------------------------------------------------------
  procedure Insert(List     : in out List_Type;
                   New_Item : in     Data_Type;
                   After    : in     Natural := 0) is

  begin
    I_L.Insert(I_L.List_Type(List), New_Item, After);
  end Insert;

  --===========================================================================
  function Insert(List     : in List_Type;
                  New_Item : in List_Type;
                  After    : in Natural := 0) return List_Type is

  begin
    return List_Type(I_L.Insert(I_L.List_Type(List), I_L.List_Type(New_Item),
                                After));
  end Insert;
  -----------------------------------------------------------------------------
  function Insert(List     : in List_Type;
                  New_Item : in Data_Type;
                  After    : in Natural := 0) return List_Type is

  begin
    return List_Type(I_L.Insert(I_L.List_Type(List), New_Item, After));
  end Insert;

  -----------------------------------------------------------------------------
  --| Insert_Sorted
  -----------------------------------------------------------------------------

  procedure Insert_Sorted(List     : in out List_Type;
                          New_Item : in     List_Type) is

  begin
    I_L_W_K.Insert_Sorted(I_L.List_Type(List), I_L.List_Type(New_Item));
  end Insert_Sorted;
  -----------------------------------------------------------------------------
  procedure Insert_Sorted(List     : in out List_Type;
                          New_Item : in     Data_Type) is

  begin
    I_L_W_K.Insert_Sorted(I_L.List_Type(List), New_Item);
  end Insert_Sorted;

  --===========================================================================
  function Insert_Sorted(List     : in List_Type;
                         New_Item : in List_Type) return List_Type is

  begin
    return List_Type(I_L_W_K.Insert_Sorted(I_L.List_Type(List),
                                           I_L.List_Type(New_Item)));
  end Insert_Sorted;
  -----------------------------------------------------------------------------
  function Insert_Sorted(List     : in List_Type;
                         New_Item : in Data_Type) return List_Type is

  begin
    return List_Type(I_L_W_K.Insert_Sorted(I_L.List_Type(List), New_Item));
  end Insert_Sorted;

  -----------------------------------------------------------------------------
  --| Append
  -----------------------------------------------------------------------------

  procedure Append(List     : in out List_Type;
                   New_Item : in     List_Type) is

  begin
    I_L.Append(I_L.List_Type(List), I_L.List_Type(New_Item));
  end Append;
  -----------------------------------------------------------------------------
  procedure Append(List     : in out List_Type;
                   New_Item : in     Data_Type) is

  begin
    I_L.Append(I_L.List_Type(List), New_Item);
  end Append;

  -----------------------------------------------------------------------------
  --| "&"
  -----------------------------------------------------------------------------

  function "&"(Left, Right : in List_Type) return List_Type is

  begin
    return List_Type(I_L."&"(I_L.List_Type(Left), I_L.List_Type(Right)));
  end "&";
  -----------------------------------------------------------------------------
  function "&"(Left  : in List_Type;
               Right : in Data_Type) return List_Type is

  begin
    return List_Type(I_L."&"(I_L.List_Type(Left), Right));
  end "&";
  -----------------------------------------------------------------------------
  function "&"(Left  : in Data_Type;
               Right : in List_Type) return List_Type is

  begin
    return List_Type(I_L."&"(Left, I_L.List_Type(Right)));
  end "&";

  -----------------------------------------------------------------------------
  --| Replace
  -----------------------------------------------------------------------------

  procedure Replace(List     : in out List_Type;
                    Index    : in     Positive;
                    New_Item : in     List_Type) is

  begin
    I_L.Replace(I_L.List_Type(List), Index, I_L.List_Type(New_Item));
  end Replace;
  -----------------------------------------------------------------------------
  procedure Replace(List     : in out List_Type;
                    Index    : in     Positive;
                    New_Item : in     Data_Type) is

  begin
    I_L.Replace(I_L.List_Type(List), Index, New_Item);
  end Replace;
  -----------------------------------------------------------------------------
  procedure Replace(List     : in out List_Type;
                    Low      : in     Positive;
                    High     : in     Natural;
                    New_Item : in     List_Type) is

  begin
    I_L.Replace(I_L.List_Type(List), Low, High, I_L.List_Type(New_Item));
  end Replace;
  -----------------------------------------------------------------------------
  procedure Replace(List     : in out List_Type;
                    Low      : in     Positive;
                    High     : in     Natural;
                    New_Item : in     Data_Type) is

  begin
    I_L.Replace(I_L.List_Type(List), Low, High, New_Item);
  end Replace;

  --===========================================================================
  function Replace(List     : in List_Type;
                   Index    : in Positive;
                   New_Item : in List_Type) return List_Type is

  begin
    return List_Type(I_L.Replace(I_L.List_Type(List), Index,
                                 I_L.List_Type(New_Item)));
  end Replace;
  -----------------------------------------------------------------------------
  function Replace(List     : in List_Type;
                   Index    : in Positive;
                   New_Item : in Data_Type) return List_Type is

  begin
    return List_Type(I_L.Replace(I_L.List_Type(List), Index, New_Item));
  end Replace;
  -----------------------------------------------------------------------------
  function Replace(List     : in List_Type;
                   Low      : in Positive;
                   High     : in Natural;
                   New_Item : in List_Type) return List_Type is

  begin
    return List_Type(I_L.Replace(I_L.List_Type(List), Low, High,
                                 I_L.List_Type(New_Item)));
  end Replace;
  -----------------------------------------------------------------------------
  function Replace(List     : in List_Type;
                   Low      : in Positive;
                   High     : in Natural;
                   New_Item : in Data_Type) return List_Type is

  begin
    return List_Type(I_L.Replace(I_L.List_Type(List), Low, High, New_Item));
  end Replace;

  -----------------------------------------------------------------------------
  --| "Turn" reverses the list.
  -----------------------------------------------------------------------------

  procedure Turn(List : in out List_Type) is

  begin
    I_L.Turn(I_L.List_Type(List));
  end Turn;

  --===========================================================================
  function Turn(List : in List_Type) return List_Type is

  begin
    return List_Type(I_L.Turn(I_L.List_Type(List)));
  end Turn;

  -----------------------------------------------------------------------------
  --| "Sort" reverses the list.
  -----------------------------------------------------------------------------

  procedure Sort(List : in out List_Type) is

  begin
    I_L_W_K.Sort(I_L.List_Type(List));
  end Sort;

  --===========================================================================
  function Sort(List : in List_Type) return List_Type is

  begin
    return List_Type(I_L_W_K.Sort(I_L.List_Type(List)));
  end Sort;

  -----------------------------------------------------------------------------
  --| "Sorted" checks if the list is sorted.
  -----------------------------------------------------------------------------

  function Sorted(List : in List_Type) return Boolean is

  begin
    return I_L_W_K.Sorted(I_L.List_Type(List));
  end Sorted;

  -----------------------------------------------------------------------------
  --| Member
  -----------------------------------------------------------------------------

  function Member(List : in List_Type;
                  Key  : in Key_Type) return Boolean is

  begin
    return I_L_W_K.Member(I_L.List_Type(List), Key);
  end Member;

  -----------------------------------------------------------------------------
  --| Member_Sorted
  -----------------------------------------------------------------------------

  function Member_Sorted(List : in List_Type;
                         Key  : in Key_Type) return Boolean is

  begin
    return I_L_W_K.Member_Sorted(I_L.List_Type(List), Key);
  end Member_Sorted;

  -----------------------------------------------------------------------------
  --| Find
  -----------------------------------------------------------------------------

  function Find(List : in List_Type;
                Key  : in Key_Type) return Data_Type is

  begin
    return I_L_W_K.Find(I_L.List_Type(List), Key);
  end Find;

  -----------------------------------------------------------------------------
  --| Find_Sorted
  -----------------------------------------------------------------------------

  function Find_Sorted(List : in List_Type;
                       Key  : in Key_Type) return Data_Type is

  begin
    return I_L_W_K.Find_Sorted(I_L.List_Type(List), Key);
  end Find_Sorted;

  -----------------------------------------------------------------------------
  --| Index
  -----------------------------------------------------------------------------

  function Index(List : in List_Type;
                 Key  : in Key_Type) return Natural is

  begin
    return I_L_W_K.Index(I_L.List_Type(List), Key);
  end Index;

  -----------------------------------------------------------------------------
  --| Index_Sorted
  -----------------------------------------------------------------------------

  function Index_Sorted(List : in List_Type;
                        Key  : in Key_Type) return Natural is

  begin
    return I_L_W_K.Index_Sorted(I_L.List_Type(List), Key);
  end Index_Sorted;

  -----------------------------------------------------------------------------
  --| Relation operators: "=", "<", ">", "<=", ">="
  -----------------------------------------------------------------------------

  function "="(Left, Right : in List_Type) return Boolean is

  begin
    return I_L_W_K."="(I_L.List_Type(Left), I_L.List_Type(Right));
  end "=";
  -----------------------------------------------------------------------------
  function "<"(Left, Right : in List_Type) return Boolean is

  begin
    return I_L_W_K."<"(I_L.List_Type(Left), I_L.List_Type(Right));
  end "<";
  -----------------------------------------------------------------------------
  function ">"(Left, Right : in List_Type) return Boolean is

  begin
    return I_L_W_K.">"(I_L.List_Type(Left), I_L.List_Type(Right));
  end ">";
  -----------------------------------------------------------------------------
  function "<="(Left, Right : in List_Type) return Boolean is

  begin
    return I_L_W_K."<="(I_L.List_Type(Left), I_L.List_Type(Right));
  end "<=";
  -----------------------------------------------------------------------------
  function ">="(Left, Right : in List_Type) return Boolean is

  begin
    return I_L_W_K.">="(I_L.List_Type(Left), I_L.List_Type(Right));
  end ">=";

  -----------------------------------------------------------------------------
  --| Compare
  -----------------------------------------------------------------------------

  function Compare(Left, Right : in List_Type) return Relation_Type is

  begin
    return I_L_W_K.Compare(I_L.List_Type(Left), I_L.List_Type(Right));
  end Compare;

  -----------------------------------------------------------------------------
  --| Iterate
  -----------------------------------------------------------------------------

  procedure Iterate(List   : in List_Type;
                    Action : in Action_Procedure_In_Mode) is

  begin
    I_L.Iterate(I_L.List_Type(List), Action);
  end Iterate;
  -----------------------------------------------------------------------------
  procedure Iterate(List   : in List_Type;
                    Action : in Action_Procedure_In_Out_Mode) is

  begin
    I_L.Iterate(I_L.List_Type(List), Action);
  end Iterate;

  --===========================================================================
  procedure Iterate(List   : in List_Type;
                    Action : in Action_Function) is

  begin
    I_L.Iterate(I_L.List_Type(List), Action);
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


  --***************************************************************************
  --| Definition of initiation part.
  --***************************************************************************

begin
  -- Nothing have to be done.
  null;
end TJa.Lists.Unchecked.Single_Linked.Unsorted_List.Checked_Data;
