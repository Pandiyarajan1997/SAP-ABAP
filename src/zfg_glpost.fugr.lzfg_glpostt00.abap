*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFI_GLACC_MASTER................................*
DATA:  BEGIN OF STATUS_ZFI_GLACC_MASTER              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFI_GLACC_MASTER              .
CONTROLS: TCTRL_ZFI_GLACC_MASTER
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFI_GLACC_MASTER              .
TABLES: ZFI_GLACC_MASTER               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
