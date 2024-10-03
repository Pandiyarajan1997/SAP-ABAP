*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMM_USERID......................................*
DATA:  BEGIN OF STATUS_ZMM_USERID                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMM_USERID                    .
CONTROLS: TCTRL_ZMM_USERID
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZMM_USERID                    .
TABLES: ZMM_USERID                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
