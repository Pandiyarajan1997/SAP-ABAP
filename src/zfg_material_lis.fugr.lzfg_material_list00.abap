*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMM_MAT_LIST....................................*
DATA:  BEGIN OF STATUS_ZMM_MAT_LIST                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMM_MAT_LIST                  .
CONTROLS: TCTRL_ZMM_MAT_LIST
            TYPE TABLEVIEW USING SCREEN '0002'.
*.........table declarations:.................................*
TABLES: *ZMM_MAT_LIST                  .
TABLES: ZMM_MAT_LIST                   .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
