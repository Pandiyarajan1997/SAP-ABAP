*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSD_PF_OLD_NEW..................................*
DATA:  BEGIN OF STATUS_ZSD_PF_OLD_NEW                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSD_PF_OLD_NEW                .
CONTROLS: TCTRL_ZSD_PF_OLD_NEW
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSD_PF_OLD_NEW                .
TABLES: ZSD_PF_OLD_NEW                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
