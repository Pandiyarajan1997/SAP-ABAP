*&---------------------------------------------------------------------*
*& Report  ZPRV_SO_MAT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ZPRV_SO_MAT


*REPORT ZPRV_SO_MAT
       NO STANDARD PAGE HEADING LINE-SIZE 255.

*INCLUDE BDCRECX1.


TYPE-POOLS TRUXS.

TYPE-POOLS SLIS.



TYPES:BEGIN OF STR,
      KSCHL TYPE RV13A-KSCHL,
    "  SELKZ TYPE RV130-SELKZ,
      SELKZ1 TYPE RV130-SELKZ,
      VKBUR TYPE KOMG-VKBUR,
      MATNR TYPE KOMG-MATNR,
      KONWA TYPE KONP-KONWA,
      KMEIN TYPE KONP-KMEIN,
      DATAB(10) TYPE C,
      DATBI(10) TYPE C,
      KSTBM(15) TYPE C,
      KSTBM1(15) TYPE C,
      KSTBM2(15) TYPE C,
      KSTBM3(15) TYPE C,
      KSTBM4(15) TYPE C,
      KONMS TYPE RV13A-KONMS,
      KBETR(11) TYPE C,
      KBETR1(11) TYPE C,
      KBETR2(11) TYPE C,
      KBETR3(11) TYPE C,
      KBETR4(11) TYPE C,
      END OF STR.




TYPES:BEGIN OF STR1,
      STATUS(10) TYPE C,
      MESSAGE(50) TYPE C,
      FIELD(132) TYPE C,
  END OF STR1.


DATA:WA_STR1 TYPE STR1,
      IT_STR1 TYPE TABLE OF STR1.


DATA:WA_FCAT TYPE SLIS_FIELDCAT_ALV,
      IT_FCAT TYPE SLIS_T_FIELDCAT_ALV,
      WA_LAYOUT TYPE SLIS_LAYOUT_ALV.



DATA:WA TYPE STR,
      IT TYPE TABLE OF STR,
     LV_FILE TYPE RLGRAP-FILENAME.


DATA:LV_RAW TYPE TRUXS_T_TEXT_DATA.

DATA:BDCDATA LIKE BDCDATA OCCURS 0 WITH HEADER LINE.

DATA:IT_MSGTAB LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE,
      WA_MSGTAB LIKE BDCMSGCOLL.








SELECTION-SCREEN BEGIN OF BLOCK A1 WITH FRAME TITLE TEXT-000.

PARAMETERS:P_FILE TYPE RLGRAP-FILENAME OBLIGATORY,
           P_MODE TYPE CTU_PARAMS-DISMODE DEFAULT 'A',
           P_UPDATE TYPE CTU_PARAMS-UPDMODE DEFAULT 'A' NO-DISPLAY.

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

START-OF-SELECTION.

  LV_FILE = P_FILE.

  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
*   I_FIELD_SEPERATOR          = 'X'
   I_LINE_HEADER              = 'X'
      I_TAB_RAW_DATA             = LV_RAW
      I_FILENAME                 = LV_FILE
    TABLES
      I_TAB_CONVERTED_DATA       = IT  "#EC CI_FLDEXT_OK[2215424] added by SPLABAP during code remedation
* EXCEPTIONS
*   CONVERSION_FAILED          = 1
*   OTHERS                     = 2
            .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.




  LOOP AT IT INTO WA.


*start-of-selection.
*
*perform open_group.

    PERFORM BDC_DYNPRO      USING 'SAPMV13A' '0100'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'RV13A-KSCHL'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM BDC_FIELD       USING 'RV13A-KSCHL'
                                  WA-KSCHL."'Z005'.
    PERFORM BDC_DYNPRO      USING 'SAPLV14A' '0100'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'RV130-SELKZ(02)'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=WEIT'.
*perform bdc_field       using 'RV130-SELKZ(01)'
*                              ''.
    PERFORM BDC_FIELD       USING 'RV130-SELKZ(02)'
                                  WA-SELKZ1."'X'.
    PERFORM BDC_DYNPRO      USING 'SAPMV13A' '1837'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'KOMG-MATNR(01)'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=PSTF'.
    PERFORM BDC_FIELD       USING 'KOMG-VKBUR'
                                  WA-VKBUR."'6001'.
    PERFORM BDC_FIELD       USING 'KOMG-MATNR(01)'
                                  WA-MATNR."'ENAFA511E3'.
    PERFORM BDC_FIELD       USING 'KONP-KONWA(01)'
                                  WA-KONWA."'INR'.
    PERFORM BDC_FIELD       USING 'KONP-KMEIN(01)'
                                  WA-KMEIN."'L'.
    PERFORM BDC_FIELD       USING 'RV13A-DATAB(01)'
                                  WA-DATAB."'01.07.2015'.
    PERFORM BDC_FIELD       USING 'RV13A-DATBI(01)'
                                  WA-DATBI."'01.07.2015'.
    PERFORM BDC_DYNPRO      USING 'SAPMV13A' '0303'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'KONM-KBETR(05)'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=BACK'.
    PERFORM BDC_FIELD       USING 'RV13A-DATAB'
                                  WA-DATAB."'01.07.2015'.
    PERFORM BDC_FIELD       USING 'RV13A-DATBI'
                                  WA-DATBI."'01.07.2015'.
    PERFORM BDC_FIELD       USING 'KONM-KSTBM(01)'
                                  WA-KSTBM."'                 10'.
    PERFORM BDC_FIELD       USING 'KONM-KSTBM(02)'
                                  WA-KSTBM1."'                 20'.
    PERFORM BDC_FIELD       USING 'KONM-KSTBM(03)'
                                  WA-KSTBM2."'                 30'.
    PERFORM BDC_FIELD       USING 'KONM-KSTBM(04)'
                                  WA-KSTBM3."'                 40'.
    PERFORM BDC_FIELD       USING 'KONM-KSTBM(05)'
                                  WA-KSTBM4."'                 50'.
    PERFORM BDC_FIELD       USING 'RV13A-KONMS(01)'
                                  WA-KONMS."'L'.
    PERFORM BDC_FIELD       USING 'KONM-KBETR(01)'
                                  WA-KBETR."'               2'.
    PERFORM BDC_FIELD       USING 'KONM-KBETR(02)'
                                  WA-KBETR1."'               4'.
    PERFORM BDC_FIELD       USING 'KONM-KBETR(03)'
                                  WA-KBETR2."'               6'.
    PERFORM BDC_FIELD       USING 'KONM-KBETR(04)'
                                  WA-KBETR3."'               8'.
    PERFORM BDC_FIELD       USING 'KONM-KBETR(05)'
                                  WA-KBETR4."'              10'.
    PERFORM BDC_DYNPRO      USING 'SAPMV13A' '1837'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'KOMG-MATNR(01)'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=SICH'.
*perform bdc_transaction using 'VK11'.
*
*perform close_group.


    CALL TRANSACTION 'VK11' USING BDCDATA MODE P_MODE UPDATE P_UPDATE MESSAGES INTO IT_MSGTAB.
    CLEAR BDCDATA.
    REFRESH BDCDATA[].

    READ TABLE IT_MSGTAB INTO WA_MSGTAB WITH KEY MSGTYP = 'E'.

    IF SY-SUBRC = 0.

      WA_STR1-STATUS = 'Error'.
      WA_STR1-FIELD  = WA_MSGTAB-FLDNAME.
      CONCATENATE WA-KSCHL 'Ended with Error' INTO WA_STR1-MESSAGE SEPARATED BY SPACE.

    ELSE.


      WA_STR1-STATUS = 'Sucess'.

      CONCATENATE WA-KSCHL 'Saved Sucessfully' INTO WA_STR1-MESSAGE SEPARATED BY SPACE.

    ENDIF.

    APPEND WA_STR1 TO IT_STR1.

    CLEAR:WA_STR1,WA_MSGTAB.





    REFRESH IT_MSGTAB[].
*    REFRESH IT_STR1[].




  ENDLOOP.
*
*  REFRESH IT[].
*  REFRESH BDCDATA[].





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
*  REFRESH BDCDATA[].
  BDCDATA-FNAM = FNAM.
  BDCDATA-FVAL = FVAL.
  APPEND BDCDATA.
*  ENDIF.

ENDFORM.                    "BDC_FIELD
