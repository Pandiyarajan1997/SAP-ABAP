*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZCOR2_MATCHNG...................................*
DATA:  BEGIN OF STATUS_ZCOR2_MATCHNG                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCOR2_MATCHNG                 .
CONTROLS: TCTRL_ZCOR2_MATCHNG
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZCOR2_MATCHNG                 .
TABLES: ZCOR2_MATCHNG                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
