*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZCUST_ATTRIBUTE.................................*
DATA:  BEGIN OF STATUS_ZCUST_ATTRIBUTE               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCUST_ATTRIBUTE               .
CONTROLS: TCTRL_ZCUST_ATTRIBUTE
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZCUST_ATTRIBUTE               .
TABLES: ZCUST_ATTRIBUTE                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
