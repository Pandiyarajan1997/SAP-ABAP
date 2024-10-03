*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSM_PRD.........................................*
DATA:  BEGIN OF STATUS_ZSM_PRD                       .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSM_PRD                       .
CONTROLS: TCTRL_ZSM_PRD
            TYPE TABLEVIEW USING SCREEN '1212'.
*.........table declarations:.................................*
TABLES: *ZSM_PRD                       .
TABLES: ZSM_PRD                        .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
