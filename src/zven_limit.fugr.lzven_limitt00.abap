*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZVEN_DEBIT_TAB..................................*
DATA:  BEGIN OF STATUS_ZVEN_DEBIT_TAB                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZVEN_DEBIT_TAB                .
CONTROLS: TCTRL_ZVEN_DEBIT_TAB
            TYPE TABLEVIEW USING SCREEN '5000'.
*.........table declarations:.................................*
TABLES: *ZVEN_DEBIT_TAB                .
TABLES: ZVEN_DEBIT_TAB                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
