*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZCUS_EXCL_STMNT.................................*
DATA:  BEGIN OF STATUS_ZCUS_EXCL_STMNT               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCUS_EXCL_STMNT               .
CONTROLS: TCTRL_ZCUS_EXCL_STMNT
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZCUS_EXCL_STMNT               .
TABLES: ZCUS_EXCL_STMNT                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
