*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZPO_TO_SO_CONFIG................................*
DATA:  BEGIN OF STATUS_ZPO_TO_SO_CONFIG              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPO_TO_SO_CONFIG              .
CONTROLS: TCTRL_ZPO_TO_SO_CONFIG
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZPO_TO_SO_CONFIG              .
TABLES: ZPO_TO_SO_CONFIG               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
