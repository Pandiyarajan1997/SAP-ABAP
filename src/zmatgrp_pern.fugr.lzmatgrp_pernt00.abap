*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMATGRP_PERN....................................*
DATA:  BEGIN OF STATUS_ZMATGRP_PERN                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMATGRP_PERN                  .
CONTROLS: TCTRL_ZMATGRP_PERN
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZMATGRP_PERN                  .
TABLES: ZMATGRP_PERN                   .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
