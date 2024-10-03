*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSALES_PRO......................................*
DATA:  BEGIN OF STATUS_ZSALES_PRO                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSALES_PRO                    .
CONTROLS: TCTRL_ZSALES_PRO
            TYPE TABLEVIEW USING SCREEN '9988'.
*.........table declarations:.................................*
TABLES: *ZSALES_PRO                    .
TABLES: ZSALES_PRO                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
