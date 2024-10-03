*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZCUSTMAT_ROL....................................*
DATA:  BEGIN OF STATUS_ZCUSTMAT_ROL                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCUSTMAT_ROL                  .
CONTROLS: TCTRL_ZCUSTMAT_ROL
            TYPE TABLEVIEW USING SCREEN '3191'.
*.........table declarations:.................................*
TABLES: *ZCUSTMAT_ROL                  .
TABLES: ZCUSTMAT_ROL                   .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
