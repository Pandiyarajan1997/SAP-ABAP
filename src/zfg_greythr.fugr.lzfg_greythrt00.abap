*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZGREYTHR_TAB....................................*
DATA:  BEGIN OF STATUS_ZGREYTHR_TAB                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZGREYTHR_TAB                  .
CONTROLS: TCTRL_ZGREYTHR_TAB
            TYPE TABLEVIEW USING SCREEN '0100'.
*...processing: ZHR_GREYTHR_LOV.................................*
DATA:  BEGIN OF STATUS_ZHR_GREYTHR_LOV               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZHR_GREYTHR_LOV               .
CONTROLS: TCTRL_ZHR_GREYTHR_LOV
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: ZHR_GREYTHR_LOVF................................*
DATA:  BEGIN OF STATUS_ZHR_GREYTHR_LOVF              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZHR_GREYTHR_LOVF              .
CONTROLS: TCTRL_ZHR_GREYTHR_LOVF
            TYPE TABLEVIEW USING SCREEN '0002'.
*...processing: ZHR_HIRING_EMAIL................................*
DATA:  BEGIN OF STATUS_ZHR_HIRING_EMAIL              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZHR_HIRING_EMAIL              .
CONTROLS: TCTRL_ZHR_HIRING_EMAIL
            TYPE TABLEVIEW USING SCREEN '0003'.
*.........table declarations:.................................*
TABLES: *ZGREYTHR_TAB                  .
TABLES: *ZHR_GREYTHR_LOV               .
TABLES: *ZHR_GREYTHR_LOVF              .
TABLES: *ZHR_HIRING_EMAIL              .
TABLES: ZGREYTHR_TAB                   .
TABLES: ZHR_GREYTHR_LOV                .
TABLES: ZHR_GREYTHR_LOVF               .
TABLES: ZHR_HIRING_EMAIL               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
