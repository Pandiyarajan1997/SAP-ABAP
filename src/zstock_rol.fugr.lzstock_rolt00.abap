*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZWH_BRN.........................................*
DATA:  BEGIN OF STATUS_ZWH_BRN                       .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWH_BRN                       .
CONTROLS: TCTRL_ZWH_BRN
            TYPE TABLEVIEW USING SCREEN '9191'.
*.........table declarations:.................................*
TABLES: *ZWH_BRN                       .
TABLES: ZWH_BRN                        .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
