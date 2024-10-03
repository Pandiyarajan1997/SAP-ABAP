*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMM_MAT_COVERAGE................................*
DATA:  BEGIN OF STATUS_ZMM_MAT_COVERAGE              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMM_MAT_COVERAGE              .
CONTROLS: TCTRL_ZMM_MAT_COVERAGE
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZMM_MAT_COVERAGE              .
TABLES: ZMM_MAT_COVERAGE               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
