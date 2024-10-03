*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZGST_FISCYR_CHK.................................*
DATA:  BEGIN OF STATUS_ZGST_FISCYR_CHK               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZGST_FISCYR_CHK               .
CONTROLS: TCTRL_ZGST_FISCYR_CHK
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZGST_FISCYR_CHK               .
TABLES: ZGST_FISCYR_CHK                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
