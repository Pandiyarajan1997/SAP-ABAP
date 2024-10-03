*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSD_INV_NR_OBJ..................................*
DATA:  BEGIN OF STATUS_ZSD_INV_NR_OBJ                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSD_INV_NR_OBJ                .
CONTROLS: TCTRL_ZSD_INV_NR_OBJ
            TYPE TABLEVIEW USING SCREEN '9000'.
*.........table declarations:.................................*
TABLES: *ZSD_INV_NR_OBJ                .
TABLES: ZSD_INV_NR_OBJ                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
