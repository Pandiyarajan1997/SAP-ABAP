report ZVK11_BDC_PGM
NO STANDARD PAGE HEADING LINE-SIZE 255.
TYPES TRUXS_T_TEXT_DATA(4096) TYPE C OCCURS 0.

TYPE-POOLS : SLIS .

TABLES: T100.
TYPE-POOLS: TRUXS.

"INCLUDE BDCRECX1.

TYPES: BEGIN OF GS_VK11,
           KSCHL TYPE RV13A-KSCHL,
           WERKS TYPE KOMG-WERKS,
           "MATKL TYPE KOMG-MATKL,
           MATKL(10) TYPE C, " TYPE KOMG-MATKL,
           "KBETR TYPE KONP-KBETR,
           KBETR(10) TYPE C,
           KONWA TYPE KONP-KONWA,
           DATAB(10) TYPE C,
           DATBI(10) TYPE C,
           "DATAB TYPE RV13A-DATAB,
           "DATBI TYPE RV13A-DATBI,
 END OF GS_VK11.

 DATA: GT_VK11 TYPE  TABLE OF GS_VK11,
       WA_VK11 TYPE  GS_VK11.

DATA: GT_RAW TYPE TRUXS_T_TEXT_DATA.

DATA: GT_BDCDATA LIKE BDCDATA    OCCURS 0 WITH HEADER LINE.

DATA: I_BDCMSG  LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE,
       BDCMSGCOLL LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE ,
        EXP_CMNMSG_05 TYPE C ,
         C_DISMODE   TYPE CHAR1 VALUE 'N'.

DATA : RAW TYPE TABLE OF ALSMEX_TABLINE ,
       WRAW TYPE  ALSMEX_TABLINE ,
       WRA TYPE  ALSMEX_TABLINE .

DATA:   MESSTAB LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE.
DATA:   WA_MESSAGE LIKE BDCMSGCOLL.

TYPES : BEGIN OF GS_MSG,
        STATUS(10) TYPE C,
        MESSAGE(50) TYPE C,
        FIELD(132)  TYPE C,
        END OF GS_MSG.


DATA : IT_MSG TYPE TABLE OF GS_MSG,
       WA_MSG TYPE GS_MSG.


DATA : FCAT TYPE SLIS_T_FIELDCAT_ALV,
       W_FCAT TYPE SLIS_FIELDCAT_ALV.

DATA: S_LAYOUT TYPE SLIS_LAYOUT_ALV.

DATA : LV_FILE TYPE RLGRAP-FILENAME.


*  PARAMETERS: C_MODE   LIKE CTU_PARAMS-DISMODE DEFAULT 'S'.
*
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.


SELECTION-SCREEN BEGIN OF BLOCK A1 WITH FRAME TITLE TEXT-000.

PARAMETERS:P_FILE TYPE RLGRAP-FILENAME OBLIGATORY,
           P_MODE TYPE CTU_PARAMS-DISMODE DEFAULT 'A',
           P_UPDATE TYPE CTU_PARAMS-UPDMODE DEFAULT 'A' NO-DISPLAY.

SELECTION-SCREEN END OF BLOCK A1.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.

*BREAK-POINT .

  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      PROGRAM_NAME  = SYST-CPROG
      DYNPRO_NUMBER = SYST-DYNNR
      FIELD_NAME    = 'P_FILE'
    IMPORTING
      FILE_NAME     = P_FILE.

START-OF-SELECTION.


LV_FILE = P_FILE.
  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
*    I_FIELD_SEPERATOR          = 'x'
    I_LINE_HEADER              = 'X'
      I_TAB_RAW_DATA             =  GT_RAW
      I_FILENAME                 = LV_FILE
    TABLES
      I_TAB_CONVERTED_DATA       =  GT_VK11[]
 EXCEPTIONS
   CONVERSION_FAILED          = 1
  OTHERS                     = 2
            .

  IF SY-SUBRC <> 0.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.

"perform open_group.

LOOP AT GT_VK11 INTO WA_VK11.

perform bdc_dynpro      using 'SAPMV13A' '0100'.
perform bdc_field       using 'BDC_CURSOR'
                              'RV13A-KSCHL'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
perform bdc_field       using 'RV13A-KSCHL'
                              WA_VK11-KSCHL. "'ZSCH'.
perform bdc_dynpro      using 'SAPMV13A' '1359'.
perform bdc_field       using 'BDC_CURSOR'
                              'KOMG-MATKL(01)'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
perform bdc_field       using 'KOMG-WERKS'
                              WA_VK11-WERKS. " '1000'.
perform bdc_field       using 'KOMG-MATKL(01)'
                              WA_VK11-MATKL. "'002'.
perform bdc_field       using 'KONP-KBETR(01)'
                              WA_VK11-KBETR. "'              20'.
perform bdc_field       using 'KONP-KONWA(01)'
                              WA_VK11-KONWA. "'inr'.
perform bdc_dynpro      using 'SAPMV13A' '1359'.
perform bdc_field       using 'BDC_CURSOR'
                              'RV13A-DATBI(01)'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
perform bdc_field       using 'RV13A-DATAB(01)'
                              WA_VK11-DATAB. "'01.09.2020'.
perform bdc_field       using 'RV13A-DATBI(01)'
                              WA_VK11-DATBI. "'10.10.2020'.
perform bdc_dynpro      using 'SAPMV13A' '1359'.
perform bdc_field       using 'BDC_CURSOR'
                              'RV13A-DATBI(01)'.
perform bdc_field       using 'BDC_OKCODE'
                              '=SICH'.
"perform bdc_transaction using 'VK11'.

"perform close_group.

CALL TRANSACTION 'VK11' USING GT_BDCDATA
                            MODE P_MODE
                            UPDATE 's' MESSAGES INTO I_BDCMSG.

  "PERFORM CLOSE_GROUP.

  REFRESH GT_BDCDATA.

  READ TABLE BDCMSGCOLL INTO WA_MSG WITH KEY MSGTYP = 'E'.


    IF SY-SUBRC = 0.

    WA_MSG-STATUS = 'Error'.
    WA_MSG-FIELD  = MESSTAB-FLDNAME.
    CONCATENATE WA_VK11-KSCHL 'Ended with Error' INTO WA_MSG-MESSAGE SEPARATED BY SPACE.

    ELSE.

    WA_MSG-STATUS = 'Success'.

    CONCATENATE WA_VK11-KSCHL 'Saved Successfully' INTO WA_MSG-MESSAGE SEPARATED BY SPACE.

    ENDIF.


    APPEND WA_MSG TO IT_MSG.
    CLEAR : WA_MSG , MESSTAB.
    REFRESH : MESSTAB[].


  ENDLOOP.

  CLEAR : W_FCAT .
  W_FCAT-FIELDNAME = 'STATUS' .
  W_FCAT-COL_POS = 1 .
  W_FCAT-SELTEXT_L = 'Status' .
  W_FCAT-OUTPUTLEN = 10 .
  W_FCAT-TABNAME = 'IT_FINAL'.
  APPEND W_FCAT TO FCAT.

  CLEAR : W_FCAT .
  W_FCAT-FIELDNAME = 'MESSAGE' .
  W_FCAT-COL_POS = 2 .
  W_FCAT-SELTEXT_L = 'Message' .
  W_FCAT-TABNAME = 'IT_FINAL'.
W_FCAT-OUTPUTLEN = 50 .
  APPEND W_FCAT TO FCAT.

  CLEAR : W_FCAT .
  W_FCAT-FIELDNAME = 'FIELD' .
  W_FCAT-COL_POS = 3 .
  W_FCAT-SELTEXT_L = 'Field Name' .
  W_FCAT-TABNAME = 'IT_FINAL'.
W_FCAT-OUTPUTLEN = 50 .
  APPEND W_FCAT TO FCAT.
  S_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.

 CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
   EXPORTING
*   I_INTERFACE_CHECK                 = ' '
*   I_BYPASSING_BUFFER                = ' '
*   I_BUFFER_ACTIVE                   = ' '
*   I_CALLBACK_PROGRAM                = ' '
*   I_CALLBACK_PF_STATUS_SET          = ' '
*   I_CALLBACK_USER_COMMAND           = ' '
*   I_CALLBACK_TOP_OF_PAGE            = ' '
*   I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*   I_CALLBACK_HTML_END_OF_LIST       = ' '
*   I_STRUCTURE_NAME                  =
*   I_BACKGROUND_ID                   = ' '
*   I_GRID_TITLE                      =
*   I_GRID_SETTINGS                   =
    IS_LAYOUT                         = S_LAYOUT
    IT_FIELDCAT                       = FCAT
*   IT_EXCLUDING                      =
*   IT_SPECIAL_GROUPS                 =
*   IT_SORT                           =
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
      T_OUTTAB                          = IT_MSG
* EXCEPTIONS
*   PROGRAM_ERROR                     = 1
*   OTHERS                            = 2
            .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


FORM BDC_DYNPRO USING PROGRAM DYNPRO.
  CLEAR GT_BDCDATA.
  GT_BDCDATA-PROGRAM  = PROGRAM.
  GT_BDCDATA-DYNPRO   = DYNPRO.
  GT_BDCDATA-DYNBEGIN = 'X'.
  APPEND  GT_BDCDATA.
ENDFORM.                    "BDC_DYNPRO

"----------------------------------------------------------------------*
      "  Insert field                                                  *
"----------------------------------------------------------------------*
FORM BDC_FIELD USING FNAM FVAL.

  CLEAR GT_BDCDATA.
  GT_BDCDATA-FNAM = FNAM.
  GT_BDCDATA-FVAL = FVAL.
  APPEND  GT_BDCDATA.

ENDFORM.                    "BDC_FIELD
