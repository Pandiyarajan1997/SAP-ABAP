*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMAIL_ID........................................*
DATA:  BEGIN OF STATUS_ZMAIL_ID                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMAIL_ID                      .
CONTROLS: TCTRL_ZMAIL_ID
            TYPE TABLEVIEW USING SCREEN '2191'.
*.........table declarations:.................................*
TABLES: *ZMAIL_ID                      .
TABLES: ZMAIL_ID                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
