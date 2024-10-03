*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZDN_CAL.........................................*
DATA:  BEGIN OF STATUS_ZDN_CAL                       .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZDN_CAL                       .
CONTROLS: TCTRL_ZDN_CAL
            TYPE TABLEVIEW USING SCREEN '1223'.
*.........table declarations:.................................*
TABLES: *ZDN_CAL                       .
TABLES: ZDN_CAL                        .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
