*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZCUST_PROCS_STP.................................*
DATA:  BEGIN OF STATUS_ZCUST_PROCS_STP               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCUST_PROCS_STP               .
CONTROLS: TCTRL_ZCUST_PROCS_STP
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZCUST_PROCS_STP               .
TABLES: ZCUST_PROCS_STP                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
