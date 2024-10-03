*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZHR_BP_SET_PERNR................................*
DATA:  BEGIN OF STATUS_ZHR_BP_SET_PERNR              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZHR_BP_SET_PERNR              .
CONTROLS: TCTRL_ZHR_BP_SET_PERNR
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZHR_BP_SET_PERNR              .
TABLES: ZHR_BP_SET_PERNR               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
