*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZCUST_ACC_TYPE..................................*
DATA:  BEGIN OF STATUS_ZCUST_ACC_TYPE                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCUST_ACC_TYPE                .
CONTROLS: TCTRL_ZCUST_ACC_TYPE
            TYPE TABLEVIEW USING SCREEN '6666'.
*...processing: ZCUST_CONDITION.................................*
DATA:  BEGIN OF STATUS_ZCUST_CONDITION               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCUST_CONDITION               .
CONTROLS: TCTRL_ZCUST_CONDITION
            TYPE TABLEVIEW USING SCREEN '3333'.
*.........table declarations:.................................*
TABLES: *ZCUST_ACC_TYPE                .
TABLES: *ZCUST_CONDITION               .
TABLES: ZCUST_ACC_TYPE                 .
TABLES: ZCUST_CONDITION                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
