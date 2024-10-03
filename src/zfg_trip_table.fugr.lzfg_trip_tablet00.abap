*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFILLING........................................*
DATA:  BEGIN OF STATUS_ZFILLING                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFILLING                      .
CONTROLS: TCTRL_ZFILLING
            TYPE TABLEVIEW USING SCREEN '1111'.
*...processing: ZFREIGHT_CITY...................................*
DATA:  BEGIN OF STATUS_ZFREIGHT_CITY                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFREIGHT_CITY                 .
CONTROLS: TCTRL_ZFREIGHT_CITY
            TYPE TABLEVIEW USING SCREEN '4444'.
*...processing: ZFREIGHT_FLOAD..................................*
DATA:  BEGIN OF STATUS_ZFREIGHT_FLOAD                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFREIGHT_FLOAD                .
CONTROLS: TCTRL_ZFREIGHT_FLOAD
            TYPE TABLEVIEW USING SCREEN '6666'.
*...processing: ZFREIGHT_HLOAD..................................*
DATA:  BEGIN OF STATUS_ZFREIGHT_HLOAD                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFREIGHT_HLOAD                .
CONTROLS: TCTRL_ZFREIGHT_HLOAD
            TYPE TABLEVIEW USING SCREEN '5555'.
*...processing: ZFREIGHT_TYPE...................................*
DATA:  BEGIN OF STATUS_ZFREIGHT_TYPE                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFREIGHT_TYPE                 .
CONTROLS: TCTRL_ZFREIGHT_TYPE
            TYPE TABLEVIEW USING SCREEN '3333'.
*...processing: ZTRIP_APPROVAL..................................*
DATA:  BEGIN OF STATUS_ZTRIP_APPROVAL                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTRIP_APPROVAL                .
CONTROLS: TCTRL_ZTRIP_APPROVAL
            TYPE TABLEVIEW USING SCREEN '7777'.
*...processing: ZTRUCK..........................................*
DATA:  BEGIN OF STATUS_ZTRUCK                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTRUCK                        .
CONTROLS: TCTRL_ZTRUCK
            TYPE TABLEVIEW USING SCREEN '9999'.
*.........table declarations:.................................*
TABLES: *ZFILLING                      .
TABLES: *ZFREIGHT_CITY                 .
TABLES: *ZFREIGHT_FLOAD                .
TABLES: *ZFREIGHT_HLOAD                .
TABLES: *ZFREIGHT_TYPE                 .
TABLES: *ZTRIP_APPROVAL                .
TABLES: *ZTRUCK                        .
TABLES: ZFILLING                       .
TABLES: ZFREIGHT_CITY                  .
TABLES: ZFREIGHT_FLOAD                 .
TABLES: ZFREIGHT_HLOAD                 .
TABLES: ZFREIGHT_TYPE                  .
TABLES: ZTRIP_APPROVAL                 .
TABLES: ZTRUCK                         .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
