*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZDMS_DGDR_GLACC.................................*
DATA:  BEGIN OF STATUS_ZDMS_DGDR_GLACC               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZDMS_DGDR_GLACC               .
CONTROLS: TCTRL_ZDMS_DGDR_GLACC
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZDMS_DGDR_GLACC               .
TABLES: ZDMS_DGDR_GLACC                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
