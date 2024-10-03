*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZHR_AUTH_TOKEN..................................*
DATA:  BEGIN OF STATUS_ZHR_AUTH_TOKEN                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZHR_AUTH_TOKEN                .
CONTROLS: TCTRL_ZHR_AUTH_TOKEN
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZHR_AUTH_TOKEN                .
TABLES: ZHR_AUTH_TOKEN                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
