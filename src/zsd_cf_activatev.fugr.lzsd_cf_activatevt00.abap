*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSD_CF_ACTIVATEV................................*
TABLES: ZSD_CF_ACTIVATEV, *ZSD_CF_ACTIVATEV. "view work areas
CONTROLS: TCTRL_ZSD_CF_ACTIVATEV
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZSD_CF_ACTIVATEV. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZSD_CF_ACTIVATEV.
* Table for entries selected to show on screen
DATA: BEGIN OF ZSD_CF_ACTIVATEV_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZSD_CF_ACTIVATEV.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSD_CF_ACTIVATEV_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZSD_CF_ACTIVATEV_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZSD_CF_ACTIVATEV.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSD_CF_ACTIVATEV_TOTAL.

*.........table declarations:.................................*
TABLES: KNA1                           .
TABLES: ZSD_CF_ACTIVATE                .
