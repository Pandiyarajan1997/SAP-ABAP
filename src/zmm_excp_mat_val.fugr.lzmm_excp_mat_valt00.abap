*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMM_EXCP_MAT_VAL................................*
DATA:  BEGIN OF STATUS_ZMM_EXCP_MAT_VAL              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMM_EXCP_MAT_VAL              .
CONTROLS: TCTRL_ZMM_EXCP_MAT_VAL
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZMM_EXCP_MAT_VAL              .
TABLES: ZMM_EXCP_MAT_VAL               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
