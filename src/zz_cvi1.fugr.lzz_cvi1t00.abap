*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFI_CUSTTYPE....................................*
DATA:  BEGIN OF STATUS_ZFI_CUSTTYPE                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFI_CUSTTYPE                  .
CONTROLS: TCTRL_ZFI_CUSTTYPE
            TYPE TABLEVIEW USING SCREEN '0002'.
*.........table declarations:.................................*
TABLES: *ZFI_CUSTTYPE                  .
TABLES: ZFI_CUSTTYPE                   .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
