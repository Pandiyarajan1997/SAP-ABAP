*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFI_CF_PARAMS...................................*
DATA:  BEGIN OF STATUS_ZFI_CF_PARAMS                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFI_CF_PARAMS                 .
CONTROLS: TCTRL_ZFI_CF_PARAMS
            TYPE TABLEVIEW USING SCREEN '9000'.
*.........table declarations:.................................*
TABLES: *ZFI_CF_PARAMS                 .
TABLES: ZFI_CF_PARAMS                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
