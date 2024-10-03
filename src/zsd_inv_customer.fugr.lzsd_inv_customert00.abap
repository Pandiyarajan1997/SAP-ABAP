*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSD_INV_CUSTOMER................................*
DATA:  BEGIN OF STATUS_ZSD_INV_CUSTOMER              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSD_INV_CUSTOMER              .
CONTROLS: TCTRL_ZSD_INV_CUSTOMER
            TYPE TABLEVIEW USING SCREEN '9000'.
*.........table declarations:.................................*
TABLES: *ZSD_INV_CUSTOMER              .
TABLES: ZSD_INV_CUSTOMER               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
