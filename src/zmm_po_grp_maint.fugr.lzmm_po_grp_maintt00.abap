*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMM_PO_GRP_MAINT................................*
DATA:  BEGIN OF STATUS_ZMM_PO_GRP_MAINT              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMM_PO_GRP_MAINT              .
CONTROLS: TCTRL_ZMM_PO_GRP_MAINT
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZMM_PO_GRP_MAINT              .
TABLES: ZMM_PO_GRP_MAINT               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
