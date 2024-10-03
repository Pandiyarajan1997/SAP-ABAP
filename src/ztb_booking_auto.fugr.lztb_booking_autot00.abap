*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZTB_BOOKING_AUTO................................*
DATA:  BEGIN OF STATUS_ZTB_BOOKING_AUTO              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTB_BOOKING_AUTO              .
CONTROLS: TCTRL_ZTB_BOOKING_AUTO
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZTB_BOOKING_AUTO              .
TABLES: ZTB_BOOKING_AUTO               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
