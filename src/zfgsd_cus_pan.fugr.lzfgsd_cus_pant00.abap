*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSD_CUS_PAN.....................................*
DATA:  BEGIN OF STATUS_ZSD_CUS_PAN                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSD_CUS_PAN                   .
CONTROLS: TCTRL_ZSD_CUS_PAN
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSD_CUS_PAN                   .
TABLES: ZSD_CUS_PAN                    .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
