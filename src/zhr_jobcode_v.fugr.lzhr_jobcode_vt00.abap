*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZHR_JOBCODE_V...................................*
TABLES: ZHR_JOBCODE_V, *ZHR_JOBCODE_V. "view work areas
CONTROLS: TCTRL_ZHR_JOBCODE_V
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZHR_JOBCODE_V. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZHR_JOBCODE_V.
* Table for entries selected to show on screen
DATA: BEGIN OF ZHR_JOBCODE_V_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZHR_JOBCODE_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZHR_JOBCODE_V_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZHR_JOBCODE_V_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZHR_JOBCODE_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZHR_JOBCODE_V_TOTAL.

*.........table declarations:.................................*
TABLES: HRP1000                        .
TABLES: ZHR_JOBCODE                    .
