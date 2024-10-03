*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSTO_MHC........................................*
DATA:  BEGIN OF STATUS_ZSTO_MHC                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSTO_MHC                      .
CONTROLS: TCTRL_ZSTO_MHC
            TYPE TABLEVIEW USING SCREEN '1111'.
*.........table declarations:.................................*
TABLES: *ZSTO_MHC                      .
TABLES: ZSTO_MHC                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
