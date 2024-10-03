*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMM_MAT_BLOCK...................................*
DATA:  BEGIN OF STATUS_ZMM_MAT_BLOCK                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMM_MAT_BLOCK                 .
CONTROLS: TCTRL_ZMM_MAT_BLOCK
            TYPE TABLEVIEW USING SCREEN '2000'.
*.........table declarations:.................................*
TABLES: *ZMM_MAT_BLOCK                 .
TABLES: ZMM_MAT_BLOCK                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
