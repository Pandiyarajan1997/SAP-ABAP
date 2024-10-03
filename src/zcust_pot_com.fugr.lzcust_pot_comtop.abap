FUNCTION-POOL ZCUST_POT_COM              MESSAGE-ID SV.

* INCLUDE LZCUST_POT_COMD...                 " Local class definition
  INCLUDE LSVIMDAT                                . "general data decl.
  INCLUDE LZCUST_POT_COMT00                       . "view rel. data dcl.

TYPES : BEGIN OF ty_where_clause,
          line TYPE char72,
        END OF ty_where_clause.
