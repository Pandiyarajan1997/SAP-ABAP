*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMATGRP_PERC....................................*
DATA:  BEGIN OF STATUS_ZMATGRP_PERC                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMATGRP_PERC                  .
CONTROLS: TCTRL_ZMATGRP_PERC
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: ZVEN_CREDEMAIL..................................*
DATA:  BEGIN OF STATUS_ZVEN_CREDEMAIL                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZVEN_CREDEMAIL                .
CONTROLS: TCTRL_ZVEN_CREDEMAIL
            TYPE TABLEVIEW USING SCREEN '0002'.
*.........table declarations:.................................*
TABLES: *ZMATGRP_PERC                  .
TABLES: *ZVEN_CREDEMAIL                .
TABLES: ZMATGRP_PERC                   .
TABLES: ZVEN_CREDEMAIL                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
