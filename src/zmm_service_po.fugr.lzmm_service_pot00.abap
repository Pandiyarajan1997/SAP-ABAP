*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMM_PO_PLANT_CC.................................*
DATA:  BEGIN OF STATUS_ZMM_PO_PLANT_CC               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMM_PO_PLANT_CC               .
CONTROLS: TCTRL_ZMM_PO_PLANT_CC
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: ZMM_PO_VEN_TAX..................................*
DATA:  BEGIN OF STATUS_ZMM_PO_VEN_TAX                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMM_PO_VEN_TAX                .
CONTROLS: TCTRL_ZMM_PO_VEN_TAX
            TYPE TABLEVIEW USING SCREEN '0002'.
*.........table declarations:.................................*
TABLES: *ZMM_PO_PLANT_CC               .
TABLES: *ZMM_PO_VEN_TAX                .
TABLES: ZMM_PO_PLANT_CC                .
TABLES: ZMM_PO_VEN_TAX                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
