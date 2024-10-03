*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZDMS_DISTB_GL...................................*
DATA:  BEGIN OF STATUS_ZDMS_DISTB_GL                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZDMS_DISTB_GL                 .
CONTROLS: TCTRL_ZDMS_DISTB_GL
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZDMS_DISTB_GL                 .
TABLES: ZDMS_DISTB_GL                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
