*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSD_AUTOMAIL_CFA................................*
DATA:  BEGIN OF STATUS_ZSD_AUTOMAIL_CFA              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSD_AUTOMAIL_CFA              .
CONTROLS: TCTRL_ZSD_AUTOMAIL_CFA
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSD_AUTOMAIL_CFA              .
TABLES: ZSD_AUTOMAIL_CFA               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
