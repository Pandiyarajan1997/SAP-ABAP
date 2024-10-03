*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSD_RE_STATU_DMS................................*
DATA:  BEGIN OF STATUS_ZSD_RE_STATU_DMS              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSD_RE_STATU_DMS              .
CONTROLS: TCTRL_ZSD_RE_STATU_DMS
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSD_RE_STATU_DMS              .
TABLES: ZSD_RE_STATU_DMS               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
