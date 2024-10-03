*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZTRAVEL_ELIG....................................*
DATA:  BEGIN OF STATUS_ZTRAVEL_ELIG                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTRAVEL_ELIG                  .
CONTROLS: TCTRL_ZTRAVEL_ELIG
            TYPE TABLEVIEW USING SCREEN '9000'.
*.........table declarations:.................................*
TABLES: *ZTRAVEL_ELIG                  .
TABLES: ZTRAVEL_ELIG                   .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
