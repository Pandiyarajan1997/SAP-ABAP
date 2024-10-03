*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFI_TRAVELD_GL..................................*
DATA:  BEGIN OF STATUS_ZFI_TRAVELD_GL                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFI_TRAVELD_GL                .
CONTROLS: TCTRL_ZFI_TRAVELD_GL
            TYPE TABLEVIEW USING SCREEN '9000'.
*.........table declarations:.................................*
TABLES: *ZFI_TRAVELD_GL                .
TABLES: ZFI_TRAVELD_GL                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
