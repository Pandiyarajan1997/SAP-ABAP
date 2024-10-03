*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZCUS_CF_CUMM....................................*
DATA:  BEGIN OF STATUS_ZCUS_CF_CUMM                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCUS_CF_CUMM                  .
CONTROLS: TCTRL_ZCUS_CF_CUMM
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: ZV_CUST_COMMS...................................*
TABLES: ZV_CUST_COMMS, *ZV_CUST_COMMS. "view work areas
CONTROLS: TCTRL_ZV_CUST_COMMS
TYPE TABLEVIEW USING SCREEN '0002'.
DATA: BEGIN OF STATUS_ZV_CUST_COMMS. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZV_CUST_COMMS.
* Table for entries selected to show on screen
DATA: BEGIN OF ZV_CUST_COMMS_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZV_CUST_COMMS.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZV_CUST_COMMS_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZV_CUST_COMMS_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZV_CUST_COMMS.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZV_CUST_COMMS_TOTAL.

*.........table declarations:.................................*
TABLES: *ZCUS_CF_CUMM                  .
TABLES: ZCUS_CF_CUMM                   .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
