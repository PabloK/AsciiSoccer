-------------------------------------------------------------------------------
--|                                                                         |--
--|                       Torbjörn Jonsson Ada library                      |--
--|                                                                         |--
--|    T J A . L I S T S . U N C H E C K E D . S I N G L E _ L I N K E D    |--
--|          . S O R T E D _ L I S T . U N C H E C K E D _ D A T A          |--
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

package body TJa.Lists.Unchecked.Single_Linked.Sorted_List.Unchecked_Data is

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
                   New_Item : in     List_Type) is

  begin
    I_L_W_K.Insert_Sorted(I_L.List_Type(List), I_L.List_Type(New_Item));
  end Insert;
  -----------------------------------------------------------------------------
  procedure Insert(List     : in out List_Type;
                   New_Item : in     Data_Type) is

  begin
    I_L_W_K.Insert_Sorted(I_L.List_Type(List), New_Item);
  end Insert;

  --===========================================================================
  function Insert(List     : in List_Type;
                  New_Item : in List_Type) return List_Type is

  begin
    return List_Type(I_L_W_K.Insert_Sorted(I_L.List_Type(List),
                                           I_L.List_Type(New_Item)));
  end Insert;
  -----------------------------------------------------------------------------
  function Insert(List     : in List_Type;
                  New_Item : in Data_Type) return List_Type is

  begin
    return List_Type(I_L_W_K.Insert_Sorted(I_L.List_Type(List), New_Item));
  end Insert;

  -----------------------------------------------------------------------------
  --| Member
  -----------------------------------------------------------------------------

  function Member(List : in List_Type;
                  Key  : in Key_Type) return Boolean is

  begin
    return I_L_W_K.Member_Sorted(I_L.List_Type(List), Key);
  end Member;

  -----------------------------------------------------------------------------
  --| Find
  -----------------------------------------------------------------------------

  function Find(List : in List_Type;
                Key  : in Key_Type) return Data_Type is

  begin
    return I_L_W_K.Find_Sorted(I_L.List_Type(List), Key);
  end Find;

  -----------------------------------------------------------------------------
  --| Index
  -----------------------------------------------------------------------------

  function Index(List : in List_Type;
                 Key  : in Key_Type) return Natural is

  begin
    return I_L_W_K.Index_Sorted(I_L.List_Type(List), Key);
  end Index;

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


  --**************************************************************************
  --| Definitions of local methods.
  --***************************************************************************


  --***************************************************************************
  --| Definition of initiation part.
  --***************************************************************************

begin
  -- Nothing have to be done.
  null;
end TJa.Lists.Unchecked.Single_Linked.Sorted_List.Unchecked_Data;
