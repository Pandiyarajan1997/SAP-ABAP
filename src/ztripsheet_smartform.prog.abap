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
      WA_LAYOUT TYPE SLIS_LAYOUT_ALV.


DATA : LV_UNIQ1 TYPE ZTRIP_ST-UNIQ1  .


DATA:FORMNAME TYPE TDSFNAME VALUE 'ZTRIP_SHEET'.

DATA:FM_NAME TYPE RS38L_FNAM.


SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-000.

" SELECT-OPTIONS:S_UNIQ1 FOR ZTRIP_ST-UNIQ1.
*SELECT-OPTIONS:S_UNIQ1 FOR LV_UNIQ1   MATCHCODE OBJECT ZUNIQ  .

PARAMETERS:S_UNIQ1 TYPE ZTRIP_ST-UNIQ1 OBLIGATORY MATCHCODE OBJECT ZUNIQ .


" SEARCH : S_UNIQ1 FOR ZTRIP_ST-UNIQ1 .
SELECTION-SCREEN END OF BLOCK B1 .


*SELECT * FROM  ZTRIP_ST
*         INTO TABLE IT_ZTRIP_SHEET1
*         WHERE UNIQ1 = S_UNIQ1 .








*SORT IT_ZTRIP_SHEET1 BY UNIQ1.


*
*
*WA_FCAT-TABNAME = 'IT_ZTRIP_SHEET1'.
*WA_FCAT-FIELDNAME = 'UNIQ1'.
*WA_FCAT-SELTEXT_M = 'Trip Doc.Number'.
*APPEND WA_FCAT TO IT_FCAT.
*CLEAR WA_FCAT.
*
*
*
*WA_FCAT-TABNAME = 'IT_ZTRIP_SHEET1'.
*WA_FCAT-FIELDNAME = 'INVOICENO'.
*WA_FCAT-SELTEXT_M = 'Invoice Number'.
*APPEND WA_FCAT TO IT_FCAT.
*CLEAR WA_FCAT.
*
*
*
*WA_FCAT-TABNAME = 'IT_ZTRIP_SHEET1'.
*WA_FCAT-FIELDNAME = 'INVOICEDATE'.
*WA_FCAT-SELTEXT_M = 'Billing Date'.
*APPEND WA_FCAT TO IT_FCAT.
*CLEAR WA_FCAT.
*
*
*WA_FCAT-TABNAME = 'IT_ZTRIP_SHEET1'.
*WA_FCAT-FIELDNAME = 'SKUDEALERNAME'.
*WA_FCAT-SELTEXT_M = 'Sales Office'.
*APPEND WA_FCAT TO IT_FCAT.
*CLEAR WA_FCAT.
*
*
*
*WA_FCAT-TABNAME = 'IT_ZTRIP_SHEET1'.
*WA_FCAT-FIELDNAME = 'QUANTITY'.
*WA_FCAT-SELTEXT_M = 'Quantity'.
*APPEND WA_FCAT TO IT_FCAT.
*CLEAR WA_FCAT.
*
*
*
*WA_FCAT-TABNAME = 'IT_ZTRIP_SHEET1'.
*WA_FCAT-FIELDNAME = 'CARTONS'.
*WA_FCAT-SELTEXT_M = 'No of cartons/Pails/Drums'.
*APPEND WA_FCAT TO IT_FCAT.
*CLEAR WA_FCAT.
*
*
*
*
*WA_FCAT-TABNAME = 'IT_ZTRIP_SHEET1'.
*WA_FCAT-FIELDNAME = 'NETVOLUMEPERINVOICE'.
*WA_FCAT-SELTEXT_M = 'Volume'.
*APPEND WA_FCAT TO IT_FCAT.
*CLEAR WA_FCAT.
*
*
*
*
*WA_FCAT-TABNAME = 'IT_ZTRIP_SHEET1'.
*WA_FCAT-FIELDNAME = 'GROSSVOLUMEPERINVOICE'.
*WA_FCAT-SELTEXT_M = 'Gross Weight'.
*APPEND WA_FCAT TO IT_FCAT.
*CLEAR WA_FCAT.
*
*
*
*WA_FCAT-TABNAME = 'IT_ZTRIP_SHEET1'.
*WA_FCAT-FIELDNAME = 'TRANSPORTERNAMELRNO'.
*WA_FCAT-SELTEXT_M = 'Transporter Name/LR No'.
*
*APPEND WA_FCAT TO IT_FCAT.
*CLEAR WA_FCAT.
*
*
*
*WA_FCAT-TABNAME = 'IT_ZTRIP_SHEET1'.
*WA_FCAT-FIELDNAME = 'VEHICLENODRIVERNAME'.
*WA_FCAT-SELTEXT_M = 'Vechile Number'.
*APPEND WA_FCAT TO IT_FCAT.
*CLEAR WA_FCAT.
*
*
*WA_FCAT-TABNAME = 'IT_ZTRIP_SHEET1'.
*WA_FCAT-FIELDNAME = 'TDATE'.
*WA_FCAT-SELTEXT_M = 'Trip Date'.
*APPEND WA_FCAT TO IT_FCAT.
*CLEAR WA_FCAT.
*
*WA_FCAT-TABNAME = 'IT_ZTRIP_SHEET1'.
*WA_FCAT-FIELDNAME = 'TIME'.
*WA_FCAT-SELTEXT_M = 'Trip Time'.
*APPEND WA_FCAT TO IT_FCAT.
*CLEAR WA_FCAT.
*
*
*
*WA_LAYOUT-ZEBRA = 'X'.
*WA_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.


*
*CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
* EXPORTING
**   I_INTERFACE_CHECK                 = ' '
**   I_BYPASSING_BUFFER                = ' '
**   I_BUFFER_ACTIVE                   = ' '
*   I_CALLBACK_PROGRAM                = SY-REPID
**   I_CALLBACK_PF_STATUS_SET          = ' '
**   I_CALLBACK_USER_COMMAND           = ' '
**   I_CALLBACK_TOP_OF_PAGE            = ' '
**   I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
**   I_CALLBACK_HTML_END_OF_LIST       = ' '
**   I_STRUCTURE_NAME                  =
**   I_BACKGROUND_ID                   = ' '
*   I_GRID_TITLE                      = 'TRIP SHEET DETAILS'
**   I_GRID_SETTINGS                   =
*   IS_LAYOUT                         = WA_LAYOUT
*   IT_FIELDCAT                       = IT_FCAT
**   IT_EXCLUDING                      =
**   IT_SPECIAL_GROUPS                 =
**   IT_SORT                           =
**   IT_FILTER                         =
**   IS_SEL_HIDE                       =
**   I_DEFAULT                         = 'X'
**   I_SAVE                            = ' '
**   IS_VARIANT                        =
**   IT_EVENTS                         =
**   IT_EVENT_EXIT                     =
**   IS_PRINT                          =
**   IS_REPREP_ID                      =
**   I_SCREEN_START_COLUMN             = 0
**   I_SCREEN_START_LINE               = 0
**   I_SCREEN_END_COLUMN               = 0
**   I_SCREEN_END_LINE                 = 0
**   I_HTML_HEIGHT_TOP                 = 0
**   I_HTML_HEIGHT_END                 = 0
**   IT_ALV_GRAPHICS                   =
**   IT_HYPERLINK                      =
**   IT_ADD_FIELDCAT                   =
**   IT_EXCEPT_QINFO                   =
**   IR_SALV_FULLSCREEN_ADAPTER        =
** IMPORTING
**   E_EXIT_CAUSED_BY_CALLER           =
**   ES_EXIT_CAUSED_BY_USER            =
*  TABLES
*    T_OUTTAB                          = IT_ZTRIP_SHEET1
** EXCEPTIONS
**   PROGRAM_ERROR                     = 1
**   OTHERS                            = 2
*          .
*IF SY-SUBRC <> 0.
** Implement suitable error handling here
*ENDIF.






CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
  EXPORTING
    FORMNAME           = FORMNAME
*   VARIANT            = ' '
*   DIRECT_CALL        = ' '
  IMPORTING
    FM_NAME            = FM_NAME
  EXCEPTIONS
    NO_FORM            = 1
    NO_FUNCTION_MODULE = 2
    OTHERS             = 3.
IF SY-SUBRC <> 0.
* Implement suitable error handling here
ENDIF.




CALL FUNCTION FM_NAME"'/1BCDWB/SF00000255'
  EXPORTING
*   ARCHIVE_INDEX              =
*   ARCHIVE_INDEX_TAB          =
*   ARCHIVE_PARAMETERS         =
*   CONTROL_PARAMETERS         =
*   MAIL_APPL_OBJ              =
*   MAIL_RECIPIENT             =
*   MAIL_SENDER                =
*   OUTPUT_OPTIONS             =
*   USER_SETTINGS              = 'X'
    S_UNIQ1                    = S_UNIQ1
* IMPORTING
*   DOCUMENT_OUTPUT_INFO       =
*   JOB_OUTPUT_INFO            =
*   JOB_OUTPUT_OPTIONS         =
* EXCEPTIONS
*   FORMATTING_ERROR           = 1
*   INTERNAL_ERROR             = 2
*   SEND_ERROR                 = 3
*   USER_CANCELED              = 4
*   OTHERS                     = 5
          .
IF SY-SUBRC <> 0.
* Implement suitable error handling here
ENDIF.
