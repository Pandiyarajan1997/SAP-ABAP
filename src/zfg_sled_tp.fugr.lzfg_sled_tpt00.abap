*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZV_TP_LOG_HEAD..................................*
TABLES: ZV_TP_LOG_HEAD, *ZV_TP_LOG_HEAD. "view work areas
CONTROLS: TCTRL_ZV_TP_LOG_HEAD
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZV_TP_LOG_HEAD. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZV_TP_LOG_HEAD.
* Table for entries selected to show on screen
DATA: BEGIN OF ZV_TP_LOG_HEAD_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZV_TP_LOG_HEAD.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZV_TP_LOG_HEAD_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZV_TP_LOG_HEAD_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZV_TP_LOG_HEAD.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZV_TP_LOG_HEAD_TOTAL.

*...processing: ZV_TP_LOG_ITEM..................................*
TABLES: ZV_TP_LOG_ITEM, *ZV_TP_LOG_ITEM. "view work areas
CONTROLS: TCTRL_ZV_TP_LOG_ITEM
TYPE TABLEVIEW USING SCREEN '0002'.
DATA: BEGIN OF STATUS_ZV_TP_LOG_ITEM. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZV_TP_LOG_ITEM.
* Table for entries selected to show on screen
DATA: BEGIN OF ZV_TP_LOG_ITEM_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZV_TP_LOG_ITEM.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZV_TP_LOG_ITEM_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZV_TP_LOG_ITEM_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZV_TP_LOG_ITEM.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZV_TP_LOG_ITEM_TOTAL.

*.........table declarations:.................................*
TABLES: ZMM_SLED_343_LOG               .
TABLES: ZMM_TR_POST_HEAD               .
