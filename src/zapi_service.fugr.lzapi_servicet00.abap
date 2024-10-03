*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZAPI_SERVICE....................................*
DATA:  BEGIN OF STATUS_ZAPI_SERVICE                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZAPI_SERVICE                  .
CONTROLS: TCTRL_ZAPI_SERVICE
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZAPI_SERVICE                  .
TABLES: ZAPI_SERVICE                   .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
