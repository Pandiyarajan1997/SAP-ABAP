*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZDMS_DIS_LIVE...................................*
DATA:  BEGIN OF STATUS_ZDMS_DIS_LIVE                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZDMS_DIS_LIVE                 .
CONTROLS: TCTRL_ZDMS_DIS_LIVE
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZDMS_DIS_LIVE                 .
TABLES: ZDMS_DIS_LIVE                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
