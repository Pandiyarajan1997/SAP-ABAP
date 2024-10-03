*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZHRP_POS_MAIN...................................*
DATA:  BEGIN OF STATUS_ZHRP_POS_MAIN                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZHRP_POS_MAIN                 .
CONTROLS: TCTRL_ZHRP_POS_MAIN
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZHRP_POS_MAIN                 .
TABLES: ZHRP_POS_MAIN                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
