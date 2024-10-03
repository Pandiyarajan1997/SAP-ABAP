*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMM_PLANT_SUPP..................................*
DATA:  BEGIN OF STATUS_ZMM_PLANT_SUPP                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMM_PLANT_SUPP                .
CONTROLS: TCTRL_ZMM_PLANT_SUPP
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZMM_PLANT_SUPP                .
TABLES: ZMM_PLANT_SUPP                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
