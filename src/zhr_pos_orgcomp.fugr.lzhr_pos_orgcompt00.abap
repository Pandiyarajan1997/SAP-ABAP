*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZHR_POS_ORGCOMP.................................*
DATA:  BEGIN OF STATUS_ZHR_POS_ORGCOMP               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZHR_POS_ORGCOMP               .
CONTROLS: TCTRL_ZHR_POS_ORGCOMP
            TYPE TABLEVIEW USING SCREEN '9000'.
*.........table declarations:.................................*
TABLES: *ZHR_POS_ORGCOMP               .
TABLES: ZHR_POS_ORGCOMP                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
