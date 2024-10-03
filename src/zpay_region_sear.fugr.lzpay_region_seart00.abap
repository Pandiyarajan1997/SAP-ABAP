*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZPAYTERM_REGION.................................*
DATA:  BEGIN OF STATUS_ZPAYTERM_REGION               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPAYTERM_REGION               .
CONTROLS: TCTRL_ZPAYTERM_REGION
            TYPE TABLEVIEW USING SCREEN '1111'.
*.........table declarations:.................................*
TABLES: *ZPAYTERM_REGION               .
TABLES: ZPAYTERM_REGION                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
