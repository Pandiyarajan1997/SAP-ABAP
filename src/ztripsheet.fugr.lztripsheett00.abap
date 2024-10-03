*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLOG1...........................................*
DATA:  BEGIN OF STATUS_ZLOG1                         .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLOG1                         .
CONTROLS: TCTRL_ZLOG1
            TYPE TABLEVIEW USING SCREEN '1111'.
*...processing: ZLOG2...........................................*
DATA:  BEGIN OF STATUS_ZLOG2                         .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLOG2                         .
CONTROLS: TCTRL_ZLOG2
            TYPE TABLEVIEW USING SCREEN '4444'.
*.........table declarations:.................................*
TABLES: *ZLOG1                         .
TABLES: *ZLOG2                         .
TABLES: ZLOG1                          .
TABLES: ZLOG2                          .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
