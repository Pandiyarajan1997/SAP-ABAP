*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFI_GST_GLACC...................................*
DATA:  BEGIN OF STATUS_ZFI_GST_GLACC                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFI_GST_GLACC                 .
CONTROLS: TCTRL_ZFI_GST_GLACC
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFI_GST_GLACC                 .
TABLES: ZFI_GST_GLACC                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
