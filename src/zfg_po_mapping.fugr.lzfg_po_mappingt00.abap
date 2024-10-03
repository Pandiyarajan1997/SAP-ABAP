*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMM_PO_MAPPING..................................*
DATA:  BEGIN OF STATUS_ZMM_PO_MAPPING                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMM_PO_MAPPING                .
CONTROLS: TCTRL_ZMM_PO_MAPPING
            TYPE TABLEVIEW USING SCREEN '0002'.
*.........table declarations:.................................*
TABLES: *ZMM_PO_MAPPING                .
TABLES: ZMM_PO_MAPPING                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
