*&---------------------------------------------------------------------*
*& Report  ZVK11_BDC2
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ZVK11_BDC2
    NO STANDARD PAGE HEADING LINE-SIZE 255..






*include bdcrecx1.

TYPE-POOLS TRUXS.

TYPE-POOLS SLIS.


TYPES:BEGIN OF STR,
      KSCHL  TYPE  RV13A-KSCHL,
      "SELKZ  TYPE  RV130-SELKZ,
      SELKZ1 TYPE RV130-SELKZ,
      REGIO  TYPE  KOMG-REGIO,
      KDGRP  TYPE  KOMG-KDGRP,
      MATNR  TYPE  KOMG-MATNR,
      KONWA  TYPE  KONP-KONWA,
      KMEIN  TYPE  KONP-KMEIN,
      DATAB(10)  TYPE  C,
      DATBI(10)  TYPE C,
      KSTBM1(15) TYPE C,
      KSTBM2(15) TYPE C,
      KSTBM3(15) TYPE C,
      KSTBM4(15) TYPE C,
      KSTBM5(15) TYPE C,
      KONMS  TYPE  RV13A-KONMS,
      KBETR1(11) TYPE C,
      KBETR2(11) TYPE C,
      KBETR3(11) TYPE C,
      KBETR4(11) TYPE C,
      KBETR5(11) TYPE C,
     " DATAB1(10) TYPE C,
      "DATBI1(10) TYPE C,
  END OF STR.


DATA:   MESSTAB LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE.
DATA:   WA_MESSAGE LIKE BDCMSGCOLL.


DATA: I_BDCMSG  LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE,
       BDCMSGCOLL LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE ,
        EXP_CMNMSG_05 TYPE C ,
         C_DISMODE   TYPE CHAR1 VALUE 'N'.

DATA : RAW TYPE TABLE OF ALSMEX_TABLINE ,
       WRAW TYPE  ALSMEX_TABLINE ,
       WRA TYPE  ALSMEX_TABLINE .

DATA:WA TYPE STR,
      IT TYPE TABLE OF STR.

DATA: LV_RAW TYPE TRUXS_T_TEXT_DATA,
      LV_FILE TYPE RLGRAP-FILENAME,
      IT_MSG TYPE TABLE OF BDCMSGCOLL.

DATA:   BDCDATA LIKE BDCDATA    OCCURS 0 WITH HEADER LINE.

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







SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME TITLE TEXT-000.
PARAMETERS:P_FILE TYPE RLGRAP-FILENAME,
           P_MODE TYPE CTU_PARAMS-DISMODE DEFAULT 'A',
           P_UPDATE TYPE CTU_PARAMS-UPDMODE DEFAULT 'A' NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK BLK1.



AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.

* CALL FUNCTION 'F4_FILENAME'
*  EXPORTING
*    PROGRAM_NAME        = SYST-CPROG
*    DYNPRO_NUMBER       = SYST-DYNNR
*    FIELD_NAME          = 'P_FILE'
*  IMPORTING
*    FILE_NAME           = P_FILE
*           .
  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
     EXPORTING
       PROGRAM_NAME        = SYST-REPID
       DYNPRO_NUMBER       = SYST-DYNNR
       FIELD_NAME          = 'P_FILE '
*     STATIC              = ' '
*     MASK                = ' '
      CHANGING
        FILE_NAME           = P_FILE
*   EXCEPTIONS
*     MASK_TOO_LONG       = 1
*     OTHERS              = 2
              .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.



  LV_FILE = P_FILE.

START-OF-SELECTION.
  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
*   I_FIELD_SEPERATOR          =
  I_LINE_HEADER              = 'X'
      I_TAB_RAW_DATA             = LV_RAW
      I_FILENAME                 = LV_FILE
    TABLES
      I_TAB_CONVERTED_DATA       = IT "#EC CI_FLDEXT_OK[2215424]
      "Added by SPLABAP during code remediation
* EXCEPTIONS
*   CONVERSION_FAILED          = 1
*   OTHERS                     = 2
            .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


*start-of-sele  ction.

*perform open_group.
*
*perform bdc_dynpro      using 'SAPMV13A' '0100'.
*perform bdc_field       using 'BDC_CURSOR'
*                              'RV13A-KSCHL'.
*perform bdc_field       using 'BDC_OKCODE'
*                              '/00'.
*perform bdc_field       using 'RV13A-KSCHL'
*                              'Y005'.
*perform bdc_dynpro      using 'SAPLV14A' '0100'.
*perform bdc_field       using 'BDC_CURSOR'
*                              'RV130-SELKZ(03)'.
*perform bdc_field       using 'BDC_OKCODE'
*                              '=WEIT'.
*perform bdc_field       using 'RV130-SELKZ(01)'
*                              ''.
*perform bdc_field       using 'RV130-SELKZ(03)'
*                              'X'.
*perform bdc_dynpro      using 'SAPMV13A' '1826'.
*perform bdc_field       using 'BDC_CURSOR'
*                              'KONP-KMEIN(01)'.
*perform bdc_field       using 'BDC_OKCODE'
*                              '/00'.
*perform bdc_field       using 'KOMG-REGIO'
*                              'KA'.
*perform bdc_field       using 'KOMG-KDGRP'
*                              '02'.
*perform bdc_field       using 'KOMG-MATNR(01)'
*                              'NCTHT022A1'.
*perform bdc_field       using 'KONP-KONWA(01)'
*                              'inr'.
*perform bdc_field       using 'KONP-KMEIN(01)'
*                              'L'.
*perform bdc_dynpro      using 'SAPMV13A' '1826'.
*perform bdc_field       using 'BDC_CURSOR'
*                              'KONP-KBETR(01)'.
*perform bdc_field       using 'BDC_OKCODE'
*                              '=PSTF'.
*perform bdc_dynpro      using 'SAPMV13A' '0303'.
*perform bdc_field       using 'BDC_CURSOR'
*                              'KONM-KBETR(05)'.
*perform bdc_field       using 'BDC_OKCODE'
*                              '/00'.
*perform bdc_field       using 'RV13A-DATAB'
*                              '25.07.2015'.
*perform bdc_field       using 'RV13A-DATBI'
*                              '31.12.9999'.
*perform bdc_field       using 'KONM-KSTBM(01)'
*                              '                 10'.
*perform bdc_field       using 'KONM-KSTBM(02)'
*                              '                 20'.
*perform bdc_field       using 'KONM-KSTBM(03)'
*                              '                 30'.
*perform bdc_field       using 'KONM-KSTBM(04)'
*                              '                 40'.
*perform bdc_field       using 'KONM-KSTBM(05)'
*                              '                 50'.
*perform bdc_field       using 'RV13A-KONMS(01)'
*                              'L'.
*perform bdc_field       using 'KONM-KBETR(01)'
*                              '               2'.
*perform bdc_field       using 'KONM-KBETR(02)'
*                              '               3'.
*perform bdc_field       using 'KONM-KBETR(03)'
*                              '               4'.
*perform bdc_field       using 'KONM-KBETR(04)'
*                              '               5'.
*perform bdc_field       using 'KONM-KBETR(05)'
*                              '               6'.
*perform bdc_dynpro      using 'SAPMV13A' '0303'.
*perform bdc_field       using 'BDC_CURSOR'
*                              'KONM-KSTBM(01)'.
*perform bdc_field       using 'BDC_OKCODE'
*                              '=BACK'.
*perform bdc_field       using 'RV13A-DATAB'
*                              '25.07.2015'.
*perform bdc_field       using 'RV13A-DATBI'
*                              '31.12.9999'.
*perform bdc_dynpro      using 'SAPMV13A' '1826'.
*perform bdc_field       using 'BDC_CURSOR'
*                              'KONP-KBETR(01)'.
*perform bdc_field       using 'BDC_OKCODE'
*                              '=SICH'.
*perform bdc_transaction using 'VK11'.

*perform close_group.




  " BREAK-POINT.

  LOOP AT IT INTO WA.




    PERFORM BDC_DYNPRO      USING 'SAPMV13A' '0100'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'RV13A-KSCHL'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM BDC_FIELD       USING 'RV13A-KSCHL'
                                  WA-KSCHL."'Y005'.
    PERFORM BDC_DYNPRO      USING 'SAPLV14A' '0100'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'RV130-SELKZ(03)'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=WEIT'.
*     PERFORM BDC_FIELD       USING 'RV130-SELKZ(01)'
*                                   WA-SELKZ."''.
    PERFORM BDC_FIELD       USING 'RV130-SELKZ(03)'
                                  WA-SELKZ1."'X'.
    PERFORM BDC_DYNPRO      USING 'SAPMV13A' '1826'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'KONP-KMEIN(01)'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM BDC_FIELD       USING 'KOMG-REGIO'
                                  WA-REGIO."'KA'.
    PERFORM BDC_FIELD       USING 'KOMG-KDGRP'
                                  WA-KDGRP."'02'.
    PERFORM BDC_FIELD       USING 'KOMG-MATNR(01)'
                                  WA-MATNR."' NCTHT022A1'.
    PERFORM BDC_FIELD       USING 'KONP-KONWA(01)'
                                  WA-KONWA."'inr'.
    PERFORM BDC_FIELD       USING 'KONP-KMEIN(01)'
                                  WA-KMEIN."'L'.
    PERFORM BDC_DYNPRO      USING 'SAPMV13A' '1826'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'KONP-KBETR(01)'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=PSTF'.
    PERFORM BDC_DYNPRO      USING 'SAPMV13A' '0303'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'KONM-KBETR(05)'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM BDC_FIELD       USING 'RV13A-DATAB'
                                  WA-DATAB."'25.07.2015'.
    PERFORM BDC_FIELD       USING 'RV13A-DATBI'
                                  WA-DATBI."'31.12.9999'.
    PERFORM BDC_FIELD       USING 'KONM-KSTBM(01)'
                                  WA-KSTBM1."'                 10'.
    PERFORM BDC_FIELD       USING 'KONM-KSTBM(02)'
                                  WA-KSTBM2."'                 20'.
    PERFORM BDC_FIELD       USING 'KONM-KSTBM(03)'
                                  WA-KSTBM3."'                 30'.
    PERFORM BDC_FIELD       USING 'KONM-KSTBM(04)'
                                  WA-KSTBM4."'                 40'.
    PERFORM BDC_FIELD       USING 'KONM-KSTBM(05)'
                                  WA-KSTBM5."'                 50'.
    PERFORM BDC_FIELD       USING 'RV13A-KONMS(01)'
                                  WA-KONMS."'L'.
    PERFORM BDC_FIELD       USING 'KONM-KBETR(01)'
                                  WA-KBETR1."'               2'.
    PERFORM BDC_FIELD       USING 'KONM-KBETR(02)'
                                  WA-KBETR2."'               3'.
    PERFORM BDC_FIELD       USING 'KONM-KBETR(03)'
                                  WA-KBETR3."'               4'.
    PERFORM BDC_FIELD       USING 'KONM-KBETR(04)'
                                  WA-KBETR4."'               5'.
    PERFORM BDC_FIELD       USING 'KONM-KBETR(05)'
                                  WA-KBETR5."'               6'.
    PERFORM BDC_DYNPRO      USING 'SAPMV13A' '0303'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'KONM-KSTBM(01)'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=BACK'.
    PERFORM BDC_FIELD       USING 'RV13A-DATAB'
                                  WA-DATAB."'25.07.2015'.
    PERFORM BDC_FIELD       USING 'RV13A-DATBI'
                                  WA-DATBI."'31.12.9999'.
    PERFORM BDC_DYNPRO      USING 'SAPMV13A' '1826'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'KONP-KBETR(01)'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=SICH'.
*    PERFORM BDC_TRANSACTION USING 'VK11'.

*    CALL TRANSACTION 'VK11' USING BDCDATA
*                            MODE MODE
*                            UPDATE UPDATE MESSAGES INTO IT_MSG.


    CALL TRANSACTION 'VK11' USING BDCDATA MODE P_MODE UPDATE P_UPDATE MESSAGES INTO  IT_MSG.
    REFRESH BDCDATA.



    READ TABLE BDCMSGCOLL INTO WA_MESSAGE WITH KEY MSGTYP = 'E'.


    IF SY-SUBRC = 0.

      WA_FINAL-STATUS = 'Error'.
      WA_FINAL-FIELD  = MESSTAB-FLDNAME.
      CONCATENATE WA-KSCHL 'Ended with Error' INTO WA_FINAL-MESSAGE SEPARATED BY SPACE.

    ELSE.

      WA_FINAL-STATUS = 'Success'.

      CONCATENATE WA-KSCHL 'Saved Successfully' INTO WA_FINAL-MESSAGE SEPARATED BY SPACE.

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





*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
FORM BDC_DYNPRO USING PROGRAM DYNPRO.
  CLEAR BDCDATA.
  BDCDATA-PROGRAM  = PROGRAM.
  BDCDATA-DYNPRO   = DYNPRO.
  BDCDATA-DYNBEGIN = 'X'.
  APPEND BDCDATA.
ENDFORM.                    "BDC_DYNPRO

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM BDC_FIELD USING FNAM FVAL.
*  IF FVAL <> NODATA.
  CLEAR BDCDATA.
  BDCDATA-FNAM = FNAM.
  BDCDATA-FVAL = FVAL.
  APPEND BDCDATA.
*  ENDIF.
ENDFORM.                    "BDC_FIELD
