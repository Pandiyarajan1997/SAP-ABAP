*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZDMS_DZ_REMOVAL.................................*
DATA:  BEGIN OF STATUS_ZDMS_DZ_REMOVAL               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZDMS_DZ_REMOVAL               .
CONTROLS: TCTRL_ZDMS_DZ_REMOVAL
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZDMS_DZ_REMOVAL               .
TABLES: ZDMS_DZ_REMOVAL                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
