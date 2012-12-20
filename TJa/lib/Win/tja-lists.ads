-------------------------------------------------------------------------------
--|                                                                         |--
--|                       Torbjörn Jonsson Ada library                      |--
--|                                                                         |--
--|                            T J A . L I S T S                            |--
--|                                                                         |--
--|                              Specification                              |--
--|                              Version  3.00                              |--
--|                                                                         |--
--|                              (C) Copyright                              |--
--|                   Torbjörn Jonsson,  TorJo@Ida.LiU.se                   |--
--|                                                                         |--
-------------------------------------------------------------------------------
--|                                                                         |--
--| Versions:                                                               |--
--|                                                                         |--
--|   2000-11-24  Version 3.00 is ok.                                       |--
--|               Created and documented by Torbjörn Jonsson.               |--
--|                                                                         |--
-------------------------------------------------------------------------------
--|                                                                         |--
--| Description:                                                            |--
--|                                                                         |--
--|   This package is the top level for list data structures in the TJa-    |--
--|   library.                                                              |--
--|                                                                         |--
-------------------------------------------------------------------------------

-- Ada standard libraries.

-- External libraries.

-- Internal libraries.

-------------------------------------------------------------------------------
package TJa.Lists is

  -----------------------------------------------------------------------------
  --| Global exceptions for all lists in TJa-library.
  -----------------------------------------------------------------------------

  List_Internal_Error   : exception;  -- Internal error. "Shouldn't" be raised.
  List_Data_Error       : exception;  -- Data doesn't exist.
  List_Constraint_Error : exception;  -- Index out of bounds.

  -----------------------------------------------------------------------------

end TJa.Lists;
