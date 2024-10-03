*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZPO_REMARKS.....................................*
DATA:  BEGIN OF STATUS_ZPO_REMARKS                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPO_REMARKS                   .
CONTROLS: TCTRL_ZPO_REMARKS
            TYPE TABLEVIEW USING SCREEN '0002'.
*...processing: ZSD_COGS_BAPI...................................*
DATA:  BEGIN OF STATUS_ZSD_COGS_BAPI                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSD_COGS_BAPI                 .
CONTROLS: TCTRL_ZSD_COGS_BAPI
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZPO_REMARKS                   .
TABLES: *ZSD_COGS_BAPI                 .
TABLES: ZPO_REMARKS                    .
TABLES: ZSD_COGS_BAPI                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
