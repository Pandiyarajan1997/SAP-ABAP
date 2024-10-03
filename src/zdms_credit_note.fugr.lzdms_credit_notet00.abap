*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZDMS_CREDIT_NOTE................................*
DATA:  BEGIN OF STATUS_ZDMS_CREDIT_NOTE              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZDMS_CREDIT_NOTE              .
CONTROLS: TCTRL_ZDMS_CREDIT_NOTE
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZDMS_CREDIT_NOTE              .
TABLES: ZDMS_CREDIT_NOTE               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
