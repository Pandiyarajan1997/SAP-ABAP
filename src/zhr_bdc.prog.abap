report ZHR_BDC
       no standard page heading line-size 255.

TYPES TRUXS_T_TEXT_DATA(4096) TYPE C OCCURS 0.

TYPE-POOLS : SLIS .

TABLES: T100.
TYPE-POOLS: TRUXS.
TYPES:BEGIN OF ty_infotype,
pernr(08) type c,
  DATAB(10) TYPE C,
  ICNUM(20) type c,
  DATAB1(10) TYPE C,
  DATAB2(10) TYPE C,

  city(20) type C,
  country(20) type C,



  END OF ty_infotype.


  DATA:IT_infotype TYPE TABLE OF ty_infotype,
        wa_infotype TYPE ty_infotype.

DATA: IT_RAW TYPE TRUXS_T_TEXT_DATA.
DATA: KBETR1  TYPE  KONP-KBETR.
*DATA : IT_BDCDATA TYPE TABLE OF BDCDATA,
*       WA_BDCDATA TYPE BDCDATA.
DATA:   IT_BDCDATA LIKE BDCDATA    OCCURS 0 WITH HEADER LINE.
*     wa_bdcdata like line of it_bdcdata.

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
          FIELD1(132)  TYPE C,
          FIELD2(132)  TYPE C,
          FIELD3(132)  TYPE C,
        END OF TY_FINAL.


DATA : IT_FINAL TYPE TABLE OF TY_FINAL,
       WA_FINAL TYPE TY_FINAL.


DATA : FCAT TYPE SLIS_T_FIELDCAT_ALV,
       W_FCAT TYPE SLIS_FIELDCAT_ALV.

DATA: S_LAYOUT TYPE SLIS_LAYOUT_ALV.

DATA : LV_FILE TYPE RLGRAP-FILENAME.

*PARAMETERS: P_file LIKE RLGRAP-FILENAME OBLIGATORY.
  PARAMETERS P_FILE TYPE IBIPPARMS-PATH OBLIGATORY.
  PARAMETERS: C_MODE   LIKE CTU_PARAMS-DISMODE DEFAULT 'A'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.


*BREAK-POINT .

  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      PROGRAM_NAME  = SYST-CPROG
      DYNPRO_NUMBER = SYST-DYNNR
      FIELD_NAME    = 'P_FILE'
    IMPORTING
      FILE_NAME     = P_FILE.

* T_FILE = P_FILE.


START-OF-SELECTION.


LV_FILE = P_FILE.
  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
*    I_FIELD_SEPERATOR          = 'x'
    I_LINE_HEADER              = 'X'
      I_TAB_RAW_DATA             =  IT_RAW
      I_FILENAME                 = LV_FILE
    TABLES
      I_TAB_CONVERTED_DATA       =  IT_infotype[]
 EXCEPTIONS
   CONVERSION_FAILED          = 1
  OTHERS                     = 2
            .

    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

LOOP AT IT_infotype INTO wa_infotype.

*passport details
perform bdc_dynpro      using 'SAPMP50A' '1000'.
perform bdc_field       using 'BDC_OKCODE'
                              '=INS'.
perform bdc_field       using 'RP50G-PERNR'
                              wa_infotype-pernr . "'700309'.
perform bdc_field       using 'BDC_CURSOR'
                              'RP50G-ENDDA'.
perform bdc_field       using 'RP50G-TIMR6'
                              'X'.
perform bdc_field       using 'RP50G-BEGDA'
                             '01.01.2015'.
perform bdc_field       using 'RP50G-ENDDA'
                              '31.12.9999'.
perform bdc_field       using 'RP50G-CHOIC'
                              '185'.
perform bdc_field       using 'RP50G-SUBTY'
                              '09'.
perform bdc_dynpro      using 'MP018500' '2000'.
perform bdc_field       using 'BDC_CURSOR'
                              'P0185-ISSPL'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
perform bdc_field       using 'P0185-BEGDA'
                            wa_infotype-datab . "  '01.01.2015'.
perform bdc_field       using 'P0185-ENDDA'
                              '31.12.9999'.
perform bdc_field       using 'P0185-ICNUM'
                             wa_infotype-ICNUM . " 'tyhjm12345'.
perform bdc_field       using 'P0185-FPDAT'
                             wa_infotype-datab1 . " '01.01.2015'.
perform bdc_field       using 'P0185-EXPID'
                            wa_infotype-datab2 . " '21.12.2020'.
perform bdc_field       using 'P0185-ISSPL'
                             wa_infotype-city . " 'chennai'.
perform bdc_field       using 'P0185-IDCOT'
                             wa_infotype-country . " 'IN'.
perform bdc_dynpro      using 'MP018500' '2000'.
perform bdc_field       using 'BDC_CURSOR'
                              'P0185-BEGDA'.
perform bdc_field       using 'BDC_OKCODE'
                              '=UPD'.
perform bdc_field       using 'P0185-BEGDA'
                            wa_infotype-datab . "  '01.01.2015'.
perform bdc_field       using 'P0185-ENDDA'
                              '31.12.9999'.
perform bdc_field       using 'P0185-ICNUM'
                              wa_infotype-ICNUM . " 'TYHJM12345'.
perform bdc_field       using 'P0185-FPDAT'
                             wa_infotype-datab1 . " '01.01.2015'.
perform bdc_field       using 'P0185-EXPID'
                              wa_infotype-datab2 . " '21.12.2020'.
perform bdc_field       using 'P0185-ISSPL'
                             wa_infotype-city . " 'chennai'.
perform bdc_field       using 'P0185-IDCOT'
                              wa_infotype-country . " 'IN'.







CALL TRANSACTION 'PA30' USING IT_BDCDATA MODE C_MODE UPDATE 'S' MESSAGES INTO I_BDCMSG.

IF NOT I_BDCMSG[] IS INITIAL.
*    PERFORM FORMAT_MESSAGE.
  READ TABLE I_BDCMSG INTO WA_MESSAGE .
  PERFORM ERROR.
ENDIF.


REFRESH IT_BDCDATA.

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

  CLEAR : W_FCAT .
  W_FCAT-FIELDNAME = 'FIELD1' .
  W_FCAT-COL_POS = 3 .
  W_FCAT-SELTEXT_L = 'Field Name' .
  W_FCAT-TABNAME = 'IT_FINAL'.
W_FCAT-OUTPUTLEN = 50 .
  APPEND W_FCAT TO FCAT.

*  CLEAR : W_FCAT .
*  W_FCAT-FIELDNAME = 'FIELD2' .
*  W_FCAT-COL_POS = 3 .
*  W_FCAT-SELTEXT_L = 'Field Name' .
*  W_FCAT-TABNAME = 'IT_FINAL'.
*W_FCAT-OUTPUTLEN = 50 .
*  APPEND W_FCAT TO FCAT.
*
*  CLEAR : W_FCAT .
*  W_FCAT-FIELDNAME = 'FIELD3' .
*  W_FCAT-COL_POS = 3 .
*  W_FCAT-SELTEXT_L = 'Field Name' .
*  W_FCAT-TABNAME = 'IT_FINAL'.
*W_FCAT-OUTPUTLEN = 50 .
*  APPEND W_FCAT TO FCAT.

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





FORM BDC_DYNPRO USING PROGRAM DYNPRO.
  CLEAR IT_BDCDATA.
*  wa_BDCDATA-PROGRAM  = PROGRAM.
*  wa_BDCDATA-DYNPRO   = DYNPRO.
*  wa_BDCDATA-DYNBEGIN = 'X'.
   IT_BDCDATA-PROGRAM  = PROGRAM.
 IT_BDCDATA-DYNPRO   = DYNPRO.
  IT_BDCDATA-DYNBEGIN = 'X'.
  APPEND IT_BDCDATA.
*  APPEND wa_BDCDATA to IT_BDCDATA.
ENDFORM.                    "BDC_DYNPRO

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM BDC_FIELD USING FNAM FVAL.

  CLEAR IT_BDCDATA.
*  wa_BDCDATA-FNAM = FNAM.
*  wa_BDCDATA-FVAL = FVAL.
*  APPEND wa_BDCDATA to IT_BDCDATA.
IT_BDCDATA-FNAM = FNAM.
IT_BDCDATA-FVAL = FVAL.
  APPEND  IT_BDCDATA.

ENDFORM.

FORM ERROR.
  DATA: L_MSG(100).
  LOOP AT I_BDCMSG.
  CALL FUNCTION 'FORMAT_MESSAGE'
         EXPORTING
              ID        = I_BDCMSG-MSGID
              LANG      = SY-LANGU
              NO        = I_BDCMSG-MSGNR
              V1        = I_BDCMSG-MSGV1
              V2        = I_BDCMSG-MSGV2
              V3        = I_BDCMSG-MSGV3
              V4        = I_BDCMSG-MSGV4
         IMPORTING
              MSG       = L_MSG
         EXCEPTIONS
              NOT_FOUND = 1
              OTHERS    = 2.


  IF I_BDCMSG-MSGTYP  = 'E'.
*    LOOP AT I_BDCMSG to wa_message.
*
*
*WA_FINAL-STATUS = 'Success'.

WA_FINAL-STATUS = I_BDCMSG-MSGTYP.
    WA_FINAL-FIELD  = MESSTAB-FLDNAME.
    CONCATENATE wa_infotype-pernr 'Ended with Error' INTO WA_FINAL-MESSAGE SEPARATED BY SPACE.
    WA_FINAL-FIELD = I_BDCMSG-MSGV1.
   WA_FINAL-FIELD1 = I_BDCMSG-MSGV2.
*    wa_final-field2 = I_BDCMSG-MSGV3.
*    wa_final-field3 = I_BDCMSG-MSGV4.

*ENDLOOP.
    ELSE.


WA_FINAL-STATUS =  I_BDCMSG-MSGTYP.
    CONCATENATE wa_infotype-pernr 'Saved Successfully' INTO WA_FINAL-MESSAGE SEPARATED BY SPACE.
WA_FINAL-FIELD = I_BDCMSG-MSGV1.
WA_FINAL-FIELD1 = I_BDCMSG-MSGV2.
    ENDIF.

ENDLOOP.
ENDFORM.
