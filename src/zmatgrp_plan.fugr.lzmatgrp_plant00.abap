*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMATGRP_PLAN....................................*
DATA:  BEGIN OF STATUS_ZMATGRP_PLAN                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMATGRP_PLAN                  .
CONTROLS: TCTRL_ZMATGRP_PLAN
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZMATGRP_PLAN                  .
TABLES: ZMATGRP_PLAN                   .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
