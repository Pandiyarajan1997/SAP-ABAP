*&---------------------------------------------------------------------*
*& Report  ZTRIP_ST
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ZTRIP_ST.

TYPE-POOLS:SLIS.



TABLES:ZTRIP_ST.

DATA:WA_ZTRIP_SHEET1 TYPE ZTRIP_ST,
      IT_ZTRIP_SHEET1 TYPE TABLE OF ZTRIP_ST.

DATA:WA_FCAT TYPE SLIS_FIELDCAT_ALV,
      IT_FCAT TYPE SLIS_T_FIELDCAT_ALV,
      WA_LAYOUT TYPE SLIS_LAYOUT_ALV,
      WA_SORT TYPE SLIS_SORTINFO_ALV,
      IT_SORT TYPE SLIS_T_SORTINFO_ALV.


DATA : LV_UNIQ1 TYPE ZTRIP_ST-UNIQ1  .


SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-000.

" SELECT-OPTIONS:S_UNIQ1 FOR ZTRIP_ST-UNIQ1.
SELECT-OPTIONS:S_UNIQ1 FOR LV_UNIQ1   MATCHCODE OBJECT ZUNIQ  .
" SEARCH : S_UNIQ1 FOR ZTRIP_ST-UNIQ1 .
SELECTION-SCREEN END OF BLOCK B1 .


SELECT * FROM  ZTRIP_ST
         INTO TABLE IT_ZTRIP_SHEET1
         WHERE UNIQ1 IN S_UNIQ1 .


SORT IT_ZTRIP_SHEET1 BY UNIQ1.




WA_FCAT-TABNAME = 'IT_ZTRIP_SHEET1'.
WA_FCAT-FIELDNAME = 'UNIQ1'.
WA_FCAT-SELTEXT_M = 'Trip Doc.Number'.
APPEND WA_FCAT TO IT_FCAT.
CLEAR WA_FCAT.



WA_FCAT-TABNAME = 'IT_ZTRIP_SHEET1'.
WA_FCAT-FIELDNAME = 'INVOICENO'.
WA_FCAT-SELTEXT_M = 'Invoice Number'.
WA_FCAT-NO_ZERO = 'X'.
APPEND WA_FCAT TO IT_FCAT.
CLEAR WA_FCAT.



WA_FCAT-TABNAME = 'IT_ZTRIP_SHEET1'.
WA_FCAT-FIELDNAME = 'INVOICEDATE'.
WA_FCAT-SELTEXT_M = 'Billing Date'.
APPEND WA_FCAT TO IT_FCAT.
CLEAR WA_FCAT.



WA_FCAT-TABNAME = 'IT_ZTRIP_SHEET1'.
WA_FCAT-FIELDNAME = 'QUANTITY'.
WA_FCAT-SELTEXT_M = 'Quantity'.
APPEND WA_FCAT TO IT_FCAT.
CLEAR WA_FCAT.


WA_FCAT-TABNAME = 'IT_ZTRIP_SHEET1'.
WA_FCAT-FIELDNAME = 'GROSSVOLUMEPERINVOICE'.
WA_FCAT-SELTEXT_M = 'Gross Weight'.
APPEND WA_FCAT TO IT_FCAT.
CLEAR WA_FCAT.



WA_FCAT-TABNAME = 'IT_ZTRIP_SHEET1'.
WA_FCAT-FIELDNAME = 'NETVOLUMEPERINVOICE'.
WA_FCAT-SELTEXT_M = 'Volume'.
APPEND WA_FCAT TO IT_FCAT.
CLEAR WA_FCAT.



WA_FCAT-TABNAME = 'IT_ZTRIP_SHEET1'.
WA_FCAT-FIELDNAME = 'SKUDEALERNAME'.
WA_FCAT-SELTEXT_M = 'Sales Branch'.
APPEND WA_FCAT TO IT_FCAT.
CLEAR WA_FCAT.


*WA_FCAT-TABNAME = 'IT_ZTRIP_SHEET1'.
*WA_FCAT-FIELDNAME = 'CUSTOMER'.
*WA_FCAT-SELTEXT_M = 'Customer Number'.
*APPEND WA_FCAT TO IT_FCAT.
*CLEAR WA_FCAT.


WA_FCAT-TABNAME = 'IT_ZTRIP_SHEET1'.
WA_FCAT-FIELDNAME = 'NAME'.
WA_FCAT-SELTEXT_M = 'Customer Name'.
APPEND WA_FCAT TO IT_FCAT.
CLEAR WA_FCAT.


WA_FCAT-TABNAME = 'IT_ZTRIP_SHEET1'.
WA_FCAT-FIELDNAME = 'CITY'.
WA_FCAT-SELTEXT_M = 'City'.
APPEND WA_FCAT TO IT_FCAT.
CLEAR WA_FCAT.


WA_FCAT-TABNAME = 'IT_ZTRIP_SHEET1'.
WA_FCAT-FIELDNAME = 'CARTONS'.
WA_FCAT-SELTEXT_M = 'LR Number'.
APPEND WA_FCAT TO IT_FCAT.
CLEAR WA_FCAT.




WA_FCAT-TABNAME = 'IT_ZTRIP_SHEET1'.
WA_FCAT-FIELDNAME = 'TRANSPORTERNAMELRNO'.
WA_FCAT-SELTEXT_M = 'Transporter Name/LR No'.

APPEND WA_FCAT TO IT_FCAT.
CLEAR WA_FCAT.



WA_FCAT-TABNAME = 'IT_ZTRIP_SHEET1'.
WA_FCAT-FIELDNAME = 'VEHICLENODRIVERNAME'.
WA_FCAT-SELTEXT_M = 'Vechile Number'.
APPEND WA_FCAT TO IT_FCAT.
CLEAR WA_FCAT.


WA_FCAT-TABNAME = 'IT_ZTRIP_SHEET1'.
WA_FCAT-FIELDNAME = 'TDATE'.
WA_FCAT-SELTEXT_M = 'Trip Date'.
APPEND WA_FCAT TO IT_FCAT.
CLEAR WA_FCAT.

WA_FCAT-TABNAME = 'IT_ZTRIP_SHEET1'.
WA_FCAT-FIELDNAME = 'TIME'.
WA_FCAT-SELTEXT_M = 'Trip Time'.
APPEND WA_FCAT TO IT_FCAT.
CLEAR WA_FCAT.



WA_LAYOUT-ZEBRA = 'X'.
WA_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.


WA_SORT-FIELDNAME = 'UNIQ1'.
APPEND WA_SORT TO IT_SORT.


CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
 EXPORTING
*   I_INTERFACE_CHECK                 = ' '
*   I_BYPASSING_BUFFER                = ' '
*   I_BUFFER_ACTIVE                   = ' '
   I_CALLBACK_PROGRAM                = SY-REPID
*   I_CALLBACK_PF_STATUS_SET          = ' '
*   I_CALLBACK_USER_COMMAND           = ' '
   I_CALLBACK_TOP_OF_PAGE            = 'TOP_OF_PAGE'
*   I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*   I_CALLBACK_HTML_END_OF_LIST       = ' '
*   I_STRUCTURE_NAME                  =
*   I_BACKGROUND_ID                   = ' '
*   I_GRID_TITLE                      = 'TRIP SHEET DETAILS'
*   I_GRID_SETTINGS                   =
   IS_LAYOUT                         = WA_LAYOUT
   IT_FIELDCAT                       = IT_FCAT
*   IT_EXCLUDING                      =
*   IT_SPECIAL_GROUPS                 =
   IT_SORT                           = IT_SORT
*   IT_FILTER                         =
*   IS_SEL_HIDE                       =
*   I_DEFAULT                         = 'X'
*   I_SAVE                            = ' '
*   IS_VARIANT                        =
*   IT_EVENTS                         =
*   IT_EVENT_EXIT                     =
*   IS_PRINT                          =
*   IS_REPREP_ID                      =
*   I_SCREEN_START_COLUMN             = 0
*   I_SCREEN_START_LINE               = 0
*   I_SCREEN_END_COLUMN               = 0
*   I_SCREEN_END_LINE                 = 0
*   I_HTML_HEIGHT_TOP                 = 0
*   I_HTML_HEIGHT_END                 = 0
*   IT_ALV_GRAPHICS                   =
*   IT_HYPERLINK                      =
*   IT_ADD_FIELDCAT                   =
*   IT_EXCEPT_QINFO                   =
*   IR_SALV_FULLSCREEN_ADAPTER        =
* IMPORTING
*   E_EXIT_CAUSED_BY_CALLER           =
*   ES_EXIT_CAUSED_BY_USER            =
  TABLES
    T_OUTTAB                          = IT_ZTRIP_SHEET1
* EXCEPTIONS
*   PROGRAM_ERROR                     = 1
*   OTHERS                            = 2
          .
IF SY-SUBRC <> 0.
* Implement suitable error handling here
ENDIF.





*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*


*   *----------------------------------------------------------------------*
FORM TOP_OF_PAGE.

  DATA:WA_HEADER TYPE SLIS_LISTHEADER,
        IT_HEADER TYPE SLIS_T_LISTHEADER.

  WA_HEADER-TYP = 'H'.
  WA_HEADER-INFO  = 'Tripsheet Details'.
  APPEND WA_HEADER TO IT_HEADER.
  CLEAR WA_HEADER.


*
*  WA_HEADER-TYP = 'S'.
*  CONCATENATE 'Report Run Date:'
*                 SY-DATUM+6(2) '-'
*                 SY-DATUM+4(2) '-'
*                 SY-DATUM+0(4) INTO WA_HEADER-INFO.
*  APPEND WA_HEADER TO IT_HEADER.
*  CLEAR WA_HEADER.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = IT_HEADER
      I_LOGO             = 'ZLOGO'
*     I_END_OF_LIST_GRID =
*     I_ALV_FORM         =
    .

ENDFORM.                    "TOP_OF_PAGE
