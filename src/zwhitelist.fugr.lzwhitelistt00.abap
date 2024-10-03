*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZWHI_LIST_CUST..................................*
DATA:  BEGIN OF STATUS_ZWHI_LIST_CUST                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWHI_LIST_CUST                .
CONTROLS: TCTRL_ZWHI_LIST_CUST
            TYPE TABLEVIEW USING SCREEN '3333'.
*.........table declarations:.................................*
TABLES: *ZWHI_LIST_CUST                .
TABLES: ZWHI_LIST_CUST                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
