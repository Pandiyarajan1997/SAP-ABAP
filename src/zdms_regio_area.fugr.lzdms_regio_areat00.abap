*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZDMS_REGIO_AREA.................................*
DATA:  BEGIN OF STATUS_ZDMS_REGIO_AREA               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZDMS_REGIO_AREA               .
CONTROLS: TCTRL_ZDMS_REGIO_AREA
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZDMS_REGIO_AREA               .
TABLES: ZDMS_REGIO_AREA                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
