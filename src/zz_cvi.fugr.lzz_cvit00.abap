*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSD_CUS_TON.....................................*
DATA:  BEGIN OF STATUS_ZSD_CUS_TON                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSD_CUS_TON                   .
CONTROLS: TCTRL_ZSD_CUS_TON
            TYPE TABLEVIEW USING SCREEN '0002'.
*.........table declarations:.................................*
TABLES: *ZSD_CUS_TON                   .
TABLES: ZSD_CUS_TON                    .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
