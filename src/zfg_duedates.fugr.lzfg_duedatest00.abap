*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFI_DUE_DATES...................................*
DATA:  BEGIN OF STATUS_ZFI_DUE_DATES                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFI_DUE_DATES                 .
CONTROLS: TCTRL_ZFI_DUE_DATES
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFI_DUE_DATES                 .
TABLES: ZFI_DUE_DATES                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
