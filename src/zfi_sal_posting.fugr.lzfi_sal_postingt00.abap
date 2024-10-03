*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFI_SAL_POSTING.................................*
DATA:  BEGIN OF STATUS_ZFI_SAL_POSTING               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFI_SAL_POSTING               .
CONTROLS: TCTRL_ZFI_SAL_POSTING
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFI_SAL_POSTING               .
TABLES: ZFI_SAL_POSTING                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
