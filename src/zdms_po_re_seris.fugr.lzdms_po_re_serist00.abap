*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZDMS_PO_RE_SERIS................................*
DATA:  BEGIN OF STATUS_ZDMS_PO_RE_SERIS              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZDMS_PO_RE_SERIS              .
CONTROLS: TCTRL_ZDMS_PO_RE_SERIS
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZDMS_PO_RE_SERIS              .
TABLES: ZDMS_PO_RE_SERIS               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
