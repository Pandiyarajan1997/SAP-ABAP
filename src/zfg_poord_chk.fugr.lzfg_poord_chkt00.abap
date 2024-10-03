*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZPOORD_CHK_SKIP.................................*
DATA:  BEGIN OF STATUS_ZPOORD_CHK_SKIP               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPOORD_CHK_SKIP               .
CONTROLS: TCTRL_ZPOORD_CHK_SKIP
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZPOORD_CHK_SKIP               .
TABLES: ZPOORD_CHK_SKIP                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
