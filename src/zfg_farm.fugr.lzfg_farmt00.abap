*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFARM_GLACC_MAST................................*
DATA:  BEGIN OF STATUS_ZFARM_GLACC_MAST              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFARM_GLACC_MAST              .
CONTROLS: TCTRL_ZFARM_GLACC_MAST
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFARM_GLACC_MAST              .
TABLES: ZFARM_GLACC_MAST               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
