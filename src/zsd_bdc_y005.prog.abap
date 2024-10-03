" DEVELOPED BY RAM ON 25/07/2015


REPORT ZSD_BDC_Y005
       NO STANDARD PAGE HEADING LINE-SIZE 255.

TYPES TRUXS_T_TEXT_DATA(4096) TYPE C OCCURS 0.

TYPE-POOLS : SLIS .

" INCLUDE BDCRECX1.

TYPES : BEGIN OF TY_EXCEL ,
    KSCHL(004) TYPE C,
    SELKZ(001) TYPE C,
*        SELKZ_01_002(001),
** data element: SELKZ_LIST
*        SELKZ_02_003(001),
    VKBUR(004) TYPE C,
    KDGRP(002) TYPE C,
    MATNR(018) TYPE C,
    KONWA(005) TYPE C,
    KMEIN(003) TYPE C,
    DATAB(010) TYPE C,
    DATBI(010) TYPE C,
   " DATAB1(010) TYPE C,
    "DATBI1(010) TYPE C,
    KSTBM(006) TYPE C,
    KSTBM1(006) TYPE C,
    KSTBM2(006) TYPE C,
    KSTBM3(006) TYPE C,
    KSTBM4(006) TYPE C,  " KSTBM4(019) TYPE C,
    KONMS(003) TYPE C,
    KBETR(016) TYPE C,
    KBETR1(016) TYPE C,
    KBETR2(016) TYPE C,
    KBETR3(016) TYPE C,
    KBETR4(016) TYPE C,
    "DATAB2(010) TYPE C,
    "DATBI2(010) TYPE C,
      END OF TY_EXCEL .


TYPES : BEGIN OF TY_FINAL,
      STATUS(10) TYPE C,
      MESSAGE(50) TYPE C,
      FIELD(132)  TYPE C,
      END OF TY_FINAL.

DATA : IT_EXCEL TYPE TABLE OF TY_EXCEL,
       WA_EXCEL TYPE TY_EXCEL.

DATA:   BDCDATA LIKE BDCDATA    OCCURS 0 WITH HEADER LINE,
        BDCMSGCOLL LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE ,
        EXP_CMNMSG_05 TYPE C ,
         C_DISMODE   TYPE CHAR1 VALUE 'N'.

DATA : RAW TYPE TABLE OF ALSMEX_TABLINE ,
       WRAW TYPE  ALSMEX_TABLINE ,
       WRA TYPE  ALSMEX_TABLINE .

DATA: IT_RAW TYPE TRUXS_T_TEXT_DATA.

DATA:   MESSTAB LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE.
DATA:   WA_MESSAGE LIKE BDCMSGCOLL.

DATA : IT_FINAL TYPE TABLE OF TY_FINAL,
       WA_FINAL TYPE TY_FINAL.

DATA : FCAT TYPE SLIS_T_FIELDCAT_ALV,
       W_FCAT TYPE SLIS_FIELDCAT_ALV.

DATA: S_LAYOUT TYPE SLIS_LAYOUT_ALV.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001 .


PARAMETER FNAME TYPE RLGRAP-FILENAME.
PARAMETERS: P_DISMOD LIKE CTU_PARAMS-DISMODE DEFAULT 'A'.

SELECTION-SCREEN END OF BLOCK B1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR FNAME.

  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      PROGRAM_NAME  = SYST-CPROG
      DYNPRO_NUMBER = SYST-DYNNR
      FIELD_NAME    = 'FNAME'
    IMPORTING
      FILE_NAME     = FNAME.

START-OF-SELECTION.

  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
      I_FIELD_SEPERATOR    = 'X'
      I_LINE_HEADER        = 'X'
      I_TAB_RAW_DATA       = IT_RAW
      I_FILENAME           = FNAME
    TABLES
      I_TAB_CONVERTED_DATA = IT_EXCEL[]
    "  I_TAB_CONVERTED_DATA = IT_RAW
    EXCEPTIONS
      CONVERSION_FAILED    = 1
      OTHERS               = 2.
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.


  "  PERFORM OPEN_GROUP.

  LOOP AT IT_EXCEL INTO WA_EXCEL.


    PERFORM BDC_DYNPRO      USING 'SAPMV13A' '0100'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'RV13A-KSCHL'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM BDC_FIELD       USING 'RV13A-KSCHL'
                                   WA_EXCEL-KSCHL.    " 'Y005'.
    PERFORM BDC_DYNPRO      USING 'SAPLV14A' '0100'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'RV130-SELKZ(02)'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=WEIT'.
    PERFORM BDC_FIELD       USING 'RV130-SELKZ(01)'
                                  ''.
    PERFORM BDC_FIELD       USING 'RV130-SELKZ(02)'
                                  WA_EXCEL-SELKZ .                      "'X'.
    PERFORM BDC_DYNPRO      USING 'SAPMV13A' '1825'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'KONP-KBETR(01)'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=PSTF'.
    PERFORM BDC_FIELD       USING 'KOMG-VKBUR'
                                  WA_EXCEL-VKBUR    .         "'1101'.
    PERFORM BDC_FIELD       USING 'KOMG-KDGRP'
                                  WA_EXCEL-KDGRP .              "'02'.
    PERFORM BDC_FIELD       USING 'KOMG-MATNR(01)'
                                   WA_EXCEL-MATNR .                     " 'NCTHT022A1'.
    PERFORM BDC_FIELD       USING 'KONP-KONWA(01)'
                                   WA_EXCEL-KONWA .                               "'inr'.
    PERFORM BDC_FIELD       USING 'KONP-KMEIN(01)'
                                   WA_EXCEL-KMEIN .                             "  'l'.
    PERFORM BDC_FIELD       USING 'RV13A-DATAB(01)'
                                  WA_EXCEL-DATAB      .                              "  '01.07.2015'.
    PERFORM BDC_FIELD       USING 'RV13A-DATBI(01)'
                                  WA_EXCEL-DATBI      .                                  "'30.07.2015'.
    PERFORM BDC_DYNPRO      USING 'SAPMV13A' '0303'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'KONM-KBETR(05)'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM BDC_FIELD       USING 'RV13A-DATAB'
                                  WA_EXCEL-DATAB .                             "                  '01.07.2015'.
    PERFORM BDC_FIELD       USING 'RV13A-DATBI'
                                  WA_EXCEL-DATBI .                                "'30.07.2015'.
    PERFORM BDC_FIELD       USING 'KONM-KSTBM(01)'
                                  WA_EXCEL-KSTBM.                                                        " '                 10'.
    PERFORM BDC_FIELD       USING 'KONM-KSTBM(02)'
                                  WA_EXCEL-KSTBM1.                                "   '                 20'.
    PERFORM BDC_FIELD       USING 'KONM-KSTBM(03)'
                                   WA_EXCEL-KSTBM2 .                                         "'                 30'.
    PERFORM BDC_FIELD       USING 'KONM-KSTBM(04)'
                                  WA_EXCEL-KSTBM3.                                            "'                 40'.
    PERFORM BDC_FIELD       USING 'KONM-KSTBM(05)'
                                  WA_EXCEL-KSTBM4 .                                  "'                 50'.
    PERFORM BDC_FIELD       USING 'RV13A-KONMS(01)'
                                  WA_EXCEL-KONMS.                                                        "'l'.
    PERFORM BDC_FIELD       USING 'KONM-KBETR(01)'
                                  WA_EXCEL-KBETR.                                            "'               2'.
    PERFORM BDC_FIELD       USING 'KONM-KBETR(02)'
                                  WA_EXCEL-KBETR1.                                            "'               3'.
    PERFORM BDC_FIELD       USING 'KONM-KBETR(03)'
                                  WA_EXCEL-KBETR2.                    "               4'.
    PERFORM BDC_FIELD       USING 'KONM-KBETR(04)'
                                  WA_EXCEL-KBETR3.                                            "   '               5'.
    PERFORM BDC_FIELD       USING 'KONM-KBETR(05)'
                                  WA_EXCEL-KBETR4 .                                      "'               6'.
    PERFORM BDC_DYNPRO      USING 'SAPMV13A' '0303'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'KONM-KSTBM(01)'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=BACK'.
    PERFORM BDC_FIELD       USING 'RV13A-DATAB'
                                  WA_EXCEL-DATAB.                                  "'01.07.2015'.
    PERFORM BDC_FIELD       USING 'RV13A-DATBI'
                                  WA_EXCEL-DATBI.                                    "'30.07.2015'.
    PERFORM BDC_DYNPRO      USING 'SAPMV13A' '1825'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'KOMG-MATNR(01)'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=SICH'.
    "  PERFORM BDC_TRANSACTION USING 'VK11'.

    CALL TRANSACTION 'VK11' USING BDCDATA
                                MODE   P_DISMOD
                                UPDATE 'S'
                                MESSAGES INTO MESSTAB.


    REFRESH: BDCDATA.

    READ TABLE MESSTAB INTO WA_MESSAGE WITH KEY MSGTYP = 'E'.

    IF SY-SUBRC = 0.

      WA_FINAL-STATUS = 'Error'.
      WA_FINAL-FIELD  = MESSTAB-FLDNAME.
      CONCATENATE WA_EXCEL-KSCHL 'Ended with Error' INTO WA_FINAL-MESSAGE SEPARATED BY SPACE.

    ELSE.

      WA_FINAL-STATUS = 'Success'.

      CONCATENATE WA_EXCEL-KSCHL 'Saved Successfully' INTO WA_FINAL-MESSAGE SEPARATED BY SPACE.

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
  W_FCAT-COL_POS = 2 .
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
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*       WITH SY-MSGV1 SY-MSGV2 .
* Implement suitable error handling here
  ENDIF.




  " PERFORM CLOSE_GROUP.




  """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*&---------------------------------------------------------------------*
*&      Form  bdc_dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PROGRAM    text
*      -->DYNPRO     text
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
  CLEAR BDCDATA.
  BDCDATA-FNAM = FNAM.
  BDCDATA-FVAL = FVAL.
  APPEND BDCDATA.
ENDFORM.                    "BDC_FIELD
