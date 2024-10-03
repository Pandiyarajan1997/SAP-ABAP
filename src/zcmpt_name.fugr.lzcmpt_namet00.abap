*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZCMPT_NAME......................................*
DATA:  BEGIN OF STATUS_ZCMPT_NAME                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCMPT_NAME                    .
CONTROLS: TCTRL_ZCMPT_NAME
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZCMPT_NAME                    .
TABLES: ZCMPT_NAME                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
