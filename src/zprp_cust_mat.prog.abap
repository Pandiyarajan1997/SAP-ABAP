report ZPRP_CUST_MAT
       no standard page heading line-size 255.

TYPES TRUXS_T_TEXT_DATA(4096) TYPE C OCCURS 0.

TYPE-POOLS : SLIS .

TABLES: T100.
TYPE-POOLS: TRUXS.

TYPES : BEGIN OF GS_VK11 ,
         KSCHL TYPE RV13A-KSCHL ,
         VKORG TYPE KOMG-VKORG ,
         VTWEG TYPE KOMG-VTWEG ,
         KUNNR TYPE KOMG-KUNNR ,
         MATNR1 TYPE KOMG-MATNR ,
         KBETR1(11) TYPE C ,
         DATAB(10) TYPE C,
         DATBI(10) TYPE C ,

       END OF GS_VK11 .

 DATA : GT_VK11 TYPE TABLE OF GS_VK11 ,
        WA_VK11 TYPE GS_VK11 .

DATA: IT_RAW TYPE TRUXS_T_TEXT_DATA.

DATA:   IT_BDCDATA LIKE BDCDATA    OCCURS 0 WITH HEADER LINE.

DATA: I_BDCMSG  LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE,
       BDCMSGCOLL LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE ,
        EXP_CMNMSG_05 TYPE C ,
         C_DISMODE   TYPE CHAR1 VALUE 'N'.

DATA : RAW TYPE TABLE OF ALSMEX_TABLINE ,
       WRAW TYPE  ALSMEX_TABLINE ,
       WRA TYPE  ALSMEX_TABLINE .

DATA:   MESSTAB LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE.
DATA:   WA_MESSAGE LIKE BDCMSGCOLL.


TYPES : BEGIN OF TY_FINAL,
        STATUS(10) TYPE C,
        MESSAGE(50) TYPE C,
        FIELD(132)  TYPE C,
        END OF TY_FINAL.


DATA : IT_FINAL TYPE TABLE OF TY_FINAL,
       WA_FINAL TYPE TY_FINAL.


DATA : FCAT TYPE SLIS_T_FIELDCAT_ALV,
       W_FCAT TYPE SLIS_FIELDCAT_ALV.

DATA: S_LAYOUT TYPE SLIS_LAYOUT_ALV.

DATA : LV_FILE TYPE RLGRAP-FILENAME.

*PARAMETERS: P_file LIKE RLGRAP-FILENAME OBLIGATORY.
  PARAMETERS P_FILE TYPE IBIPPARMS-PATH OBLIGATORY.
  PARAMETERS: C_MODE   LIKE CTU_PARAMS-DISMODE DEFAULT 'S'.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.

  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      PROGRAM_NAME  = SYST-CPROG
      DYNPRO_NUMBER = SYST-DYNNR
      FIELD_NAME    = 'P_FILE'
    IMPORTING
      FILE_NAME     = P_FILE.

*  T_FILE = P_FILE.

START-OF-SELECTION.

LV_FILE = P_FILE.
  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
*    I_FIELD_SEPERATOR          = 'x'
    I_LINE_HEADER              = 'X'
      I_TAB_RAW_DATA             =  IT_RAW
      I_FILENAME                 = LV_FILE
    TABLES
      I_TAB_CONVERTED_DATA       =  GT_VK11[]   "#EC CI_FLDEXT_OK[2215424]  added by SPLABAP during code remedation
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

"include bdcrecx1.


"perform open_group.

LOOP AT GT_VK11 INTO WA_VK11 .

perform bdc_dynpro      using 'SAPMV13A' '0100'.
perform bdc_field       using 'BDC_CURSOR'
                              'RV13A-KSCHL'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
perform bdc_field       using 'RV13A-KSCHL'
                              WA_VK11-KSCHL . "'ZPRP'.
perform bdc_dynpro      using 'SAPLV14A' '0100'.
perform bdc_field       using 'BDC_CURSOR'
                              'RV130-SELKZ(01)'.
perform bdc_field       using 'BDC_OKCODE'
                              '=WEIT'.
perform bdc_dynpro      using 'SAPMV13A' '1005'.
perform bdc_field       using 'BDC_CURSOR'
                              WA_VK11-KUNNR . "'KOMG-KUNNR'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
perform bdc_field       using 'KOMG-VKORG'
                              WA_VK11-VKORG. "'1000'.
perform bdc_field       using 'KOMG-VTWEG'
                              WA_VK11-VTWEG . "'20'.
perform bdc_field       using 'KOMG-KUNNR'
                              WA_VK11-KUNNR . "'10000196'.
perform bdc_dynpro      using 'SAPMV13A' '1005'.
perform bdc_field       using 'BDC_CURSOR'
                              WA_VK11-KUNNR . "'KOMG-KUNNR'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
perform bdc_field       using 'KOMG-VKORG'
                              WA_VK11-VKORG . "'1000'.
perform bdc_field       using 'KOMG-VTWEG'
                              WA_VK11-VTWEG . "'20'.
perform bdc_field       using 'KOMG-KUNNR'
                              WA_VK11-KUNNR . "'10000302'.
perform bdc_dynpro      using 'SAPMV13A' '1005'.
perform bdc_field       using 'BDC_CURSOR'
                              'KOMG-MATNR(01)'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
perform bdc_field       using 'KOMG-VKORG'
                              WA_VK11-VKORG . "'1000'.
perform bdc_field       using 'KOMG-VTWEG'
                              WA_VK11-VTWEG . "'20'.
perform bdc_field       using 'KOMG-KUNNR'
                              WA_VK11-KUNNR .  "'10000302'.
perform bdc_field       using 'KOMG-MATNR(1)'
                              WA_VK11-MATNR1  . "'NCWFU001M2'.
perform bdc_dynpro      using 'SAPMV13A' '1005'.
perform bdc_field       using 'BDC_CURSOR'
                              WA_VK11-KBETR1 . "'KONP-KBETR(01)'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
perform bdc_field       using 'KONP-KBETR(1)'
                              WA_VK11-KBETR1 . " '               5'.
perform bdc_field       using 'RV13A-DATAB(1)'
                              WA_VK11-DATAB . " '
perform bdc_field       using 'RV13A-DATBI(1)'
                              WA_VK11-DATBI . " '
perform bdc_dynpro      using 'SAPMV13A' '1005'.
perform bdc_field       using 'BDC_CURSOR'
                              'KOMG-MATNR(01)'.
perform bdc_field       using 'BDC_OKCODE'
                              '=SICH'.

CALL TRANSACTION 'VK11' USING IT_BDCDATA
                            MODE C_MODE
                            UPDATE 's' MESSAGES INTO I_BDCMSG.

REFRESH IT_BDCDATA.

READ TABLE BDCMSGCOLL INTO WA_MESSAGE WITH KEY MSGTYP = 'E'.

"perform bdc_transaction using 'VK11'.

"perform close_group.

  IF SY-SUBRC = 0.

    WA_FINAL-STATUS = 'Error'.
    WA_FINAL-FIELD  = MESSTAB-FLDNAME.
    CONCATENATE WA_VK11-KSCHL 'Ended with Error' INTO WA_FINAL-MESSAGE SEPARATED BY SPACE.

    ELSE.

    WA_FINAL-STATUS = 'Success'.

    CONCATENATE WA_VK11-KSCHL 'Saved Successfully' INTO WA_FINAL-MESSAGE SEPARATED BY SPACE.

    ENDIF.


    APPEND WA_FINAL TO IT_FINAL.
    CLEAR : WA_FINAL , MESSTAB.
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
      T_OUTTAB                          = IT_FINAL
* EXCEPTIONS
*   PROGRAM_ERROR                     = 1
*   OTHERS                            = 2
            .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  BDC_DYNPRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0269   text
*      -->P_0270   text
*----------------------------------------------------------------------*
FORM BDC_DYNPRO  USING    PROGRAM DYNPRO.
  CLEAR IT_BDCDATA.
  IT_BDCDATA-PROGRAM  = PROGRAM.
  IT_BDCDATA-DYNPRO   = DYNPRO.
  IT_BDCDATA-DYNBEGIN = 'X'.
  APPEND  IT_BDCDATA.
ENDFORM.                    " BDC_DYNPRO

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM BDC_FIELD USING FNAM FVAL.

  CLEAR IT_BDCDATA.
  IT_BDCDATA-FNAM = FNAM.
  IT_BDCDATA-FVAL = FVAL.
  APPEND  IT_BDCDATA.

ENDFORM.                    "BDC_FIELD
