*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZPALLET_YEAR....................................*
DATA:  BEGIN OF STATUS_ZPALLET_YEAR                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPALLET_YEAR                  .
CONTROLS: TCTRL_ZPALLET_YEAR
            TYPE TABLEVIEW USING SCREEN '1111'.
*.........table declarations:.................................*
TABLES: *ZPALLET_YEAR                  .
TABLES: ZPALLET_YEAR                   .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
