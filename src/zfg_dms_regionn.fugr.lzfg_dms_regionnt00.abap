*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZDMS_REGIO_MAP..................................*
DATA:  BEGIN OF STATUS_ZDMS_REGIO_MAP                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZDMS_REGIO_MAP                .
CONTROLS: TCTRL_ZDMS_REGIO_MAP
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZDMS_REGIO_MAP                .
TABLES: ZDMS_REGIO_MAP                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
