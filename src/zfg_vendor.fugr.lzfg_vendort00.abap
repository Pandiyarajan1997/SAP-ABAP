*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZTA_EMP_PYMTBLK.................................*
DATA:  BEGIN OF STATUS_ZTA_EMP_PYMTBLK               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTA_EMP_PYMTBLK               .
CONTROLS: TCTRL_ZTA_EMP_PYMTBLK
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZTA_EMP_PYMTBLK               .
TABLES: ZTA_EMP_PYMTBLK                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
