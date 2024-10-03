*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZPRD_GROUP_PPL..................................*
DATA:  BEGIN OF STATUS_ZPRD_GROUP_PPL                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPRD_GROUP_PPL                .
CONTROLS: TCTRL_ZPRD_GROUP_PPL
            TYPE TABLEVIEW USING SCREEN '3333'.
*.........table declarations:.................................*
TABLES: *ZPRD_GROUP_PPL                .
TABLES: ZPRD_GROUP_PPL                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
