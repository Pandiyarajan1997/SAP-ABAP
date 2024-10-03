*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZVEN_PAY_WHATSAP................................*
DATA:  BEGIN OF STATUS_ZVEN_PAY_WHATSAP              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZVEN_PAY_WHATSAP              .
CONTROLS: TCTRL_ZVEN_PAY_WHATSAP
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZVEN_PAY_WHATSAP              .
TABLES: ZVEN_PAY_WHATSAP               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
