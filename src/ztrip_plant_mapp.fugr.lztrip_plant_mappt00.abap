*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZTRIP_PLANT.....................................*
DATA:  BEGIN OF STATUS_ZTRIP_PLANT                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTRIP_PLANT                   .
CONTROLS: TCTRL_ZTRIP_PLANT
            TYPE TABLEVIEW USING SCREEN '3333'.
*.........table declarations:.................................*
TABLES: *ZTRIP_PLANT                   .
TABLES: ZTRIP_PLANT                    .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
