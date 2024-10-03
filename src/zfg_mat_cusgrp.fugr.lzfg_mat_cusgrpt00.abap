*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMM_MAT_CUSGRP..................................*
DATA:  BEGIN OF STATUS_ZMM_MAT_CUSGRP                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMM_MAT_CUSGRP                .
CONTROLS: TCTRL_ZMM_MAT_CUSGRP
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZMM_MAT_CUSGRP                .
TABLES: ZMM_MAT_CUSGRP                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
