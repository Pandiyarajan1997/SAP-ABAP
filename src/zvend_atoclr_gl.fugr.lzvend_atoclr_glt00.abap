*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZVEND_ATOCLR_GL.................................*
DATA:  BEGIN OF STATUS_ZVEND_ATOCLR_GL               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZVEND_ATOCLR_GL               .
CONTROLS: TCTRL_ZVEND_ATOCLR_GL
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZVEND_ATOCLR_GL               .
TABLES: ZVEND_ATOCLR_GL                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
