*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZWATBASE_TABLE..................................*
DATA:  BEGIN OF STATUS_ZWATBASE_TABLE                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWATBASE_TABLE                .
CONTROLS: TCTRL_ZWATBASE_TABLE
            TYPE TABLEVIEW USING SCREEN '9713'.
*.........table declarations:.................................*
TABLES: *ZWATBASE_TABLE                .
TABLES: ZWATBASE_TABLE                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
