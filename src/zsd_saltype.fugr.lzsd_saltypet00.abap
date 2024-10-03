*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSD_SALTYP......................................*
DATA:  BEGIN OF STATUS_ZSD_SALTYP                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSD_SALTYP                    .
CONTROLS: TCTRL_ZSD_SALTYP
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSD_SALTYP                    .
TABLES: ZSD_SALTYP                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
