*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSALES_TABLE....................................*
DATA:  BEGIN OF STATUS_ZSALES_TABLE                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSALES_TABLE                  .
CONTROLS: TCTRL_ZSALES_TABLE
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZSALES_TABLE                  .
TABLES: ZSALES_TABLE                   .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
