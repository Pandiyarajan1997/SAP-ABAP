*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZHR_ASSET_STATUS................................*
DATA:  BEGIN OF STATUS_ZHR_ASSET_STATUS              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZHR_ASSET_STATUS              .
CONTROLS: TCTRL_ZHR_ASSET_STATUS
            TYPE TABLEVIEW USING SCREEN '0002'.
*...processing: ZHR_ASSET_TYPE..................................*
DATA:  BEGIN OF STATUS_ZHR_ASSET_TYPE                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZHR_ASSET_TYPE                .
CONTROLS: TCTRL_ZHR_ASSET_TYPE
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZHR_ASSET_STATUS              .
TABLES: *ZHR_ASSET_TYPE                .
TABLES: ZHR_ASSET_STATUS               .
TABLES: ZHR_ASSET_TYPE                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
