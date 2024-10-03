*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSD_GST_REG.....................................*
DATA:  BEGIN OF STATUS_ZSD_GST_REG                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSD_GST_REG                   .
CONTROLS: TCTRL_ZSD_GST_REG
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSD_GST_REG                   .
TABLES: ZSD_GST_REG                    .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
