*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: YEINV_URL.......................................*
DATA:  BEGIN OF STATUS_YEINV_URL                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_YEINV_URL                     .
CONTROLS: TCTRL_YEINV_URL
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: YZEINV_URL......................................*
DATA:  BEGIN OF STATUS_YZEINV_URL                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_YZEINV_URL                    .
CONTROLS: TCTRL_YZEINV_URL
            TYPE TABLEVIEW USING SCREEN '1111'.
*.........table declarations:.................................*
TABLES: *YEINV_URL                     .
TABLES: *YZEINV_URL                    .
TABLES: YEINV_URL                      .
TABLES: YZEINV_URL                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
