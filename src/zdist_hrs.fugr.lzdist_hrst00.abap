*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMIS_CUST_ST....................................*
DATA:  BEGIN OF STATUS_ZMIS_CUST_ST                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMIS_CUST_ST                  .
CONTROLS: TCTRL_ZMIS_CUST_ST
            TYPE TABLEVIEW USING SCREEN '0101'.
*...processing: ZSKU_TABLE......................................*
DATA:  BEGIN OF STATUS_ZSKU_TABLE                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSKU_TABLE                    .
CONTROLS: TCTRL_ZSKU_TABLE
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZMIS_CUST_ST                  .
TABLES: *ZSKU_TABLE                    .
TABLES: ZMIS_CUST_ST                   .
TABLES: ZSKU_TABLE                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
