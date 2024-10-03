REPORT ZPRP_SO_MAT
       NO STANDARD PAGE HEADING LINE-SIZE 255.

*include bdcrecx1.


TYPES TRUXS_T_TEXT_DATA(4096) TYPE C OCCURS 0.

TYPE-POOLS : SLIS .

TABLES: T100.
TYPE-POOLS: TRUXS.


TYPES : BEGIN OF GS_PRP,
        KSCHL	 TYPE	KSCHA,
        VKBUR TYPE VKBUR,
*         KDGRP TYPE KDGRP,
        MATNR1 TYPE KOMG-MATNR ,
        KBETR1(11) TYPE C ,
        DATAB TYPE KODATAB,
        DATBI TYPE KODATBI,
        END OF GS_PRP.


DATA : GT_PRP TYPE STANDARD TABLE OF GS_PRP,
       WA_PRP TYPE GS_PRP.

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


DATA LV_DATE TYPE C LENGTH 10.
DATA LV_DATE1 TYPE C LENGTH 10.


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
*     I_FIELD_SEPERATOR    = 'x'
      I_LINE_HEADER        = 'X'
      I_TAB_RAW_DATA       = IT_RAW
      I_FILENAME           = LV_FILE
    TABLES
      I_TAB_CONVERTED_DATA = GT_PRP "#EC CI_FLDEXT_OK[2215424] added by SPLABAP during code remedation
    EXCEPTIONS
      CONVERSION_FAILED    = 1
      OTHERS               = 2.

  IF SY-SUBRC <> 0.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.


*  PERFORM OPEN_GROUP.

  LOOP AT GT_PRP INTO WA_PRP.

    CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
      EXPORTING
        DATE_INTERNAL            = WA_PRP-DATAB
      IMPORTING
        DATE_EXTERNAL            = LV_DATE
      EXCEPTIONS
        DATE_INTERNAL_IS_INVALID = 1
        OTHERS                   = 2.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.


    CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
      EXPORTING
        DATE_INTERNAL            = WA_PRP-DATBI
      IMPORTING
        DATE_EXTERNAL            = LV_DATE1
      EXCEPTIONS
        DATE_INTERNAL_IS_INVALID = 1
        OTHERS                   = 2.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.






    PERFORM BDC_DYNPRO      USING 'SAPMV13A' '0100'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'RV13A-KSCHL'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM BDC_FIELD       USING 'RV13A-KSCHL'  WA_PRP-KSCHL.
*                                'ZPRP'.
    PERFORM BDC_DYNPRO      USING 'SAPLV14A' '0100'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'RV130-SELKZ(02)'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=WEIT'.
    PERFORM BDC_FIELD       USING 'RV130- SELKZ(01)'
                                  ''.
    PERFORM BDC_FIELD       USING 'RV130-SELKZ(02)'
                                  'X'.
    PERFORM BDC_DYNPRO      USING 'SAPMV13A' '1837'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'KONP-KBETR(01)'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM BDC_FIELD       USING 'KOMG-VKBUR' WA_PRP-VKBUR.
*                                '1102'.
    PERFORM BDC_FIELD       USING 'KOMG-MATNR(01)' WA_PRP-MATNR1.
*                                'NCWFU001M2'.

    PERFORM BDC_FIELD       USING 'RV13A-DATAB(01)' LV_DATE.   " WA_PRP-DATAB.
    PERFORM BDC_FIELD       USING 'RV13A-DATBI(01)'  LV_DATE1 . "WA_PRP-DATBI.

    PERFORM BDC_DYNPRO      USING 'SAPMV13A' '1837'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'KONP-KBETR(01)'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM BDC_FIELD       USING 'KONP-KBETR(01)' WA_PRP-KBETR1.
*                             '               3'.

    PERFORM BDC_DYNPRO      USING 'SAPMV13A' '1837'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'KOMG-MATNR(01)'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=SICH'.
*  PERFORM BDC_TRANSACTION USING 'VK11'.

*  PERFORM CLOSE_GROUP.



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
      CONCATENATE WA_PRP-KSCHL 'Ended with Error' INTO WA_FINAL-MESSAGE SEPARATED BY SPACE.

    ELSE.

      WA_FINAL-STATUS = 'Success'.

      CONCATENATE WA_PRP-KSCHL 'Saved Successfully' INTO WA_FINAL-MESSAGE SEPARATED BY SPACE.

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
  CLEAR IT_BDCDATA.
  IT_BDCDATA-PROGRAM  = PROGRAM.
  IT_BDCDATA-DYNPRO   = DYNPRO.
  IT_BDCDATA-DYNBEGIN = 'X'.
  APPEND IT_BDCDATA.
ENDFORM.                    "BDC_DYNPRO

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM BDC_FIELD USING FNAM FVAL.
  IF FVAL <> SPACE.
    CLEAR IT_BDCDATA.
    IT_BDCDATA-FNAM = FNAM.
    IT_BDCDATA-FVAL = FVAL.
    APPEND IT_BDCDATA.
  ENDIF.
ENDFORM.                    "BDC_FIELD
