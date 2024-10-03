report ZWATER_BDC
       no standard page heading line-size 255.

TYPE-POOLS:TRUXS.

TYPE-POOLS:SLIS.


TYPES: BEGIN OF STR_ZWATBASE_TABLE,
        SL_NO  TYPE ZWATBASE_TABLE-SL_NO,
        ALTERNATIVE TYPE ZWATBASE_TABLE-ALTERNATIVE,
        MATNR TYPE ZWATBASE_TABLE-MATNR,
        STAGE TYPE ZWATBASE_TABLE-STAGE,
        MATNR_ALT(30) TYPE C,
        MATNR_PRO(300) TYPE C,
      END OF STR_ZWATBASE_TABLE.


 TYPES:BEGIN OF STR1,
     STATUS(10) TYPE C,
      MESSAGE(50) TYPE C,
      FIELD(132) TYPE C,
  END OF STR1.

  DATA:WA_ZWA_BASE_TABLE TYPE STR_ZWATBASE_TABLE,
      IT_ZWA_BASE_TABLE TYPE TABLE OF STR_ZWATBASE_TABLE.

  DATA:WA_FCAT TYPE SLIS_FIELDCAT_ALV,
      IT_FCAT TYPE SLIS_T_FIELDCAT_ALV,
      WA_LAYOUT TYPE SLIS_LAYOUT_ALV.

DATA:LV_FILE TYPE RLGRAP-FILENAME.


DATA:I_TAB_RAW_DATA TYPE TRUXS_T_TEXT_DATA,
      I_FILENAME TYPE RLGRAP-FILENAME.

DATA:BDCDATA LIKE BDCDATA OCCURS 0 WITH HEADER LINE.

DATA:IT_MSGTAB LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE,
      WA_MSGTAB LIKE BDCMSGCOLL.


DATA:WA_STR1 TYPE STR1,
      IT_STR1 TYPE TABLE OF STR1.


SELECTION-SCREEN BEGIN OF BLOCK A1 WITH FRAME TITLE TEXT-000.

PARAMETERS:P_FILE TYPE RLGRAP-FILENAME.
PARAMETERS:P_MODE TYPE CTU_PARAMS-DISMODE.
PARAMETERS:P_UPDATE TYPE CTU_PARAMS-UPDMODE.

SELECTION-SCREEN END OF BLOCK A1.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.

  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
  EXPORTING
    PROGRAM_NAME        = SYST-REPID
    DYNPRO_NUMBER       = SYST-DYNNR
    FIELD_NAME          = 'P_FILE'
*   STATIC              = ' '
*   MASK                = ' '
*   FILEOPERATION       = 'R'
*   PATH                =
   CHANGING
     FILE_NAME           = P_FILE
*   LOCATION_FLAG       = 'P'
* EXCEPTIONS
*   MASK_TOO_LONG       = 1
*   OTHERS              = 2
           .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.


  LV_FILE = P_FILE.

START-OF-SELECTION.


START-OF-SELECTION.

  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
*   I_FIELD_SEPERATOR          =
   I_LINE_HEADER              = 'X'
      I_TAB_RAW_DATA             =  I_TAB_RAW_DATA
      I_FILENAME                 = LV_FILE
    TABLES
      I_TAB_CONVERTED_DATA       = IT_ZWA_BASE_TABLE"#EC CI_FLDEXT_OK[2215424]
      "Added by SPLABAP during code remediation
* EXCEPTIONS
*   CONVERSION_FAILED          = 1
*   OTHERS                     = 2
            .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.



"include bdcrecx1.

 LOOP AT IT_ZWA_BASE_TABLE INTO WA_ZWA_BASE_TABLE .

"perform open_group.

perform bdc_dynpro      using 'SAPLZWATER_BASE' '2222'.
perform bdc_field       using 'BDC_CURSOR'
                              'VIM_POSITION_INFO'.
perform bdc_field       using 'BDC_OKCODE'
                              '=NEWL'.
perform bdc_dynpro      using 'SAPLZWATER_BASE' '2222'.
perform bdc_field       using 'BDC_CURSOR'
                              'ZWATBASE_TABLE-MATNR_PRO(01)'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
perform bdc_field       using 'ZWATBASE_TABLE-SL_NO(01)'
                               WA_ZWA_BASE_TABLE-SL_NO . "'0003'.

perform bdc_field       using 'ZWATBASE_TABLE-STLAL(01)'
                              WA_ZWA_BASE_TABLE-ALTERNATIVE .  " '20000059'.

perform bdc_field       using 'ZWATBASE_TABLE-MATNR(01)'
                              WA_ZWA_BASE_TABLE-MATNR .  " '20000059'.
perform bdc_field       using 'ZWATBASE_TABLE-STAGE(01)'
                               WA_ZWA_BASE_TABLE-STAGE. "'TEST6'.
perform bdc_field       using 'ZWATBASE_TABLE-MATNR_ALT(01)'
                               WA_ZWA_BASE_TABLE-MATNR_ALT. "'TEST5'.
perform bdc_field       using 'ZWATBASE_TABLE-MATNR_PRO(01)'
                               WA_ZWA_BASE_TABLE-MATNR_PRO. "'TEST6'.
perform bdc_dynpro      using 'SAPLZWATER_BASE' '2222'.
perform bdc_field       using 'BDC_CURSOR'
                              'ZWATBASE_TABLE-MATNR_PRO(02)'.
perform bdc_field       using 'BDC_OKCODE'
                              '=SAVE'.
"perform bdc_field       using 'ZWATBASE_TABLE-SL_NO(02)'
 "                             '2'.
"perform bdc_field       using 'ZWATBASE_TABLE-MATNR(02)'
 "                             'ENAFA501'.
"perform bdc_field       using 'ZWATBASE_TABLE-STAGE(02)'
 "                             '1'.
"perform bdc_field       using 'ZWATBASE_TABLE-MATNR_ALT(02)'
 "                             '10000029'.
"perform bdc_field       using 'ZWATBASE_TABLE-MATNR_PRO(02)'
 "                             'TESTING1'.
perform bdc_dynpro      using 'SAPLZWATER_BASE' '2222'.
perform bdc_field       using 'BDC_CURSOR'
                              'ZWATBASE_TABLE-SL_NO(03)'.
perform bdc_field       using 'BDC_OKCODE'
                              '=BACK'.
perform bdc_dynpro      using 'SAPLZWATER_BASE' '2222'.
perform bdc_field       using 'BDC_CURSOR'
                              'ZWATBASE_TABLE-SL_NO(01)'.
perform bdc_field       using 'BDC_OKCODE'
                              '=BACK'.
" perform bdc_transaction using 'ZWAT_BASE'.

 CALL TRANSACTION 'ZWAT_BASE' USING BDCDATA MODE P_MODE UPDATE P_UPDATE MESSAGES INTO IT_MSGTAB.
    CLEAR BDCDATA.
    REFRESH BDCDATA[].

    READ TABLE IT_MSGTAB INTO WA_MSGTAB WITH KEY MSGTYP = 'E'.

    IF SY-SUBRC = 0.

      WA_STR1-STATUS = 'Error'.
      WA_STR1-FIELD = WA_MSGTAB-FLDNAME.
*      CONCATENATE WA_KSCHL 'Ended With Error' INTO WA_STR1-MESSAGE SEPARATED BY SPACE.

    ELSE.

      WA_STR1-STATUS = 'Sucess'.

*      CONCATENATE WA-KSCHL 'Saved Sucessfully' INTO WA_STR1-MESSAGE SEPARATED BY SPACE.

    ENDIF.

    APPEND WA_STR1 TO IT_STR1.

    CLEAR:WA_STR1,WA_MSGTAB.
    REFRESH IT_MSGTAB[].

  ENDLOOP.




  CLEAR : WA_FCAT .
  WA_FCAT-FIELDNAME = 'STATUS' .
  WA_FCAT-COL_POS = 1 .
  WA_FCAT-SELTEXT_L = 'Status' .
  WA_FCAT-OUTPUTLEN = 10 .
  WA_FCAT-TABNAME = 'IT_FINAL'.
  APPEND WA_FCAT TO IT_FCAT.



  CLEAR : WA_FCAT .
  WA_FCAT-FIELDNAME = 'MESSAGE' .
  WA_FCAT-COL_POS = 2 .
  WA_FCAT-SELTEXT_L = 'Message' .
  WA_FCAT-OUTPUTLEN = 50 .
  WA_FCAT-TABNAME = 'IT_FINAL'.
  APPEND WA_FCAT TO IT_FCAT.


  CLEAR : WA_FCAT .
  WA_FCAT-FIELDNAME = 'FIELD' .
  WA_FCAT-COL_POS = 3 .
  WA_FCAT-SELTEXT_L = 'Field Name' .
  WA_FCAT-OUTPUTLEN = 50 .
  WA_FCAT-TABNAME = 'IT_FINAL'.
  APPEND WA_FCAT TO IT_FCAT.


  WA_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.



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
     IS_LAYOUT                         = WA_LAYOUT
     IT_FIELDCAT                       = IT_FCAT
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
      T_OUTTAB                          = IT_STR1
* EXCEPTIONS
*   PROGRAM_ERROR                     = 1
*   OTHERS                            = 2
            .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.

" perform close_group.
*&---------------------------------------------------------------------*
*&      Form  BDC_DYNPRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0216   text
*      -->P_0217   text
*----------------------------------------------------------------------*
FORM BDC_DYNPRO  USING    PROGRAM DYNPRO.
  CLEAR BDCDATA.
  BDCDATA-PROGRAM  = PROGRAM.
  BDCDATA-DYNPRO   = DYNPRO.
  BDCDATA-DYNBEGIN = 'X'.
  APPEND BDCDATA.

ENDFORM.                    " BDC_DYNPRO
*&---------------------------------------------------------------------*
*&      Form  BDC_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0331   text
*      -->P_0332   text
*----------------------------------------------------------------------*
FORM BDC_FIELD  USING    FNAM FVAL.
*  IF FVAL <> NODATA.
  CLEAR BDCDATA.
  BDCDATA-FNAM = FNAM.
  BDCDATA-FVAL = FVAL.
  APPEND BDCDATA.
*  ENDIF.
ENDFORM.                    " BDC_FIELD
