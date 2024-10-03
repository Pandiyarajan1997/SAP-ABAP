REPORT ZCUSTMAT_ROL
       NO STANDARD PAGE HEADING LINE-SIZE 255.

*include bdcrecx1.

TYPE-POOLS:TRUXS.

TYPE-POOLS:SLIS.

TYPES:BEGIN OF TY_ZCUSTMAT_ROL,
  KUNNR TYPE ZCUSTMAT_ROL-KUNNR,
  MATNR TYPE ZCUSTMAT_ROL-MATNR,
  NEW_VAL TYPE CHAR18,
  NEW_DAT TYPE ZCUSTMAT_ROL-NEW_DAT,
  NEW_CRE TYPE ZCUSTMAT_ROL-NEW_CRE,
  END OF TY_ZCUSTMAT_ROL.

DATA: IT_ZCUSTMAT_ROL TYPE TABLE OF TY_ZCUSTMAT_ROL,
      WA_ZCUSTMAT_ROL TYPE TY_ZCUSTMAT_ROL.

DATA: IT_READ_TABLE TYPE TABLE OF ZCUSTMAT_ROL,
      WA_READ_TABLE TYPE ZCUSTMAT_ROL.

DATA: OLD_VAL TYPE CHAR18,"ZCUSTMAT_ROL-OLD_DAT,
      OLD_DAT TYPE ZCUSTMAT_ROL-OLD_DAT,
      OLD_CRE TYPE ZCUSTMAT_ROL-OLD_CRE.

TYPES:BEGIN OF STR1,
   STATUS(10) TYPE C,
   KUNNR TYPE KNA1-KUNNR,
   MATNR TYPE MARC-MATNR,
   MINBE TYPE CHAR18,
   MESSAGE(50) TYPE C,
   FIELD(132) TYPE C,
END OF STR1.

DATA: WA_STR1 TYPE STR1,
      IT_STR1 TYPE TABLE OF STR1.


DATA: WA_FCAT TYPE SLIS_FIELDCAT_ALV,
      IT_FCAT TYPE SLIS_T_FIELDCAT_ALV,
      WA_LAYOUT TYPE SLIS_LAYOUT_ALV.

DATA: LV_FILE TYPE RLGRAP-FILENAME.

DATA:I_TAB_RAW_DATA TYPE TRUXS_T_TEXT_DATA,
      I_FILENAME TYPE RLGRAP-FILENAME.

DATA: IT_MSGTAB LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE,
      WA_MSGTAB LIKE BDCMSGCOLL.

DATA:BDCDATA LIKE BDCDATA OCCURS 0 WITH HEADER LINE.

DATA: QUANTITY TYPE MARC-MINBE.

DATA: USR_ID TYPE USR01-BNAME.

DATA: CUR_DAT TYPE SY-DATUM,
      DATUM TYPE CHAR10,
      OLD_DATUM TYPE CHAR10.

DATA: MSG TYPE CHAR50.

TYPES: BEGIN OF TY_SH,
  S TYPE CTU_PARAMS-UPDMODE,
  END OF TY_SH.

  DATA: IT_SH TYPE TABLE OF TY_SH,
        WA_SH TYPE TY_SH.



SELECTION-SCREEN BEGIN OF BLOCK A1 WITH FRAME TITLE TEXT-000.

PARAMETERS:P_FILE TYPE RLGRAP-FILENAME.
PARAMETERS:P_MODE TYPE CTU_PARAMS-DISMODE.
PARAMETERS:P_UPDATE TYPE CTU_PARAMS-UPDMODE.

SELECTION-SCREEN END OF BLOCK A1.

IF P_UPDATE = 'L'.

  MESSAGE 'Please Select Update Type Synchronous OR Asynchronous'  type 'I' DISPLAY LIKE 'E' .
  SUBMIT ZCUSTMAT_ROL VIA SELECTION-SCREEN .

ENDIF.


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

  SELECT * FROM ZCUSTMAT_ROL INTO TABLE IT_READ_TABLE.

  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
      EXPORTING
*   I_FIELD_SEPERATOR          =
    I_LINE_HEADER              = 'X'
    I_TAB_RAW_DATA             =  I_TAB_RAW_DATA
    I_FILENAME                 =  LV_FILE
     TABLES
    I_TAB_CONVERTED_DATA       =  IT_ZCUSTMAT_ROL"#EC CI_FLDEXT_OK[2215424] "Added by SPLABAP during code remediation
* EXCEPTIONS
*   CONVERSION_FAILED          = 1
*   OTHERS                     = 2
              .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.

*  PERFORM OPEN_GROUP.

  USR_ID = SY-UNAME.
  CUR_DAT = SY-DATUM.

  CONCATENATE CUR_DAT+6(2) '.' CUR_DAT+4(2) '.' CUR_DAT(4) INTO DATUM .

  LOOP AT IT_ZCUSTMAT_ROL INTO WA_ZCUSTMAT_ROL.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = WA_ZCUSTMAT_ROL-KUNNR
      IMPORTING
        OUTPUT = WA_ZCUSTMAT_ROL-KUNNR.

**    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        INPUT  = WA_ZCUSTMAT_ROL-MATNR
      IMPORTING
        OUTPUT = WA_ZCUSTMAT_ROL-MATNR.

    READ TABLE IT_READ_TABLE INTO WA_READ_TABLE WITH KEY KUNNR = WA_ZCUSTMAT_ROL-KUNNR MATNR = WA_ZCUSTMAT_ROL-MATNR .
    IF WA_READ_TABLE IS NOT INITIAL.
      OLD_VAL = WA_READ_TABLE-NEW_VAL.
      OLD_DAT = WA_READ_TABLE-NEW_DAT.
      OLD_CRE = WA_READ_TABLE-NEW_CRE.

      CONCATENATE OLD_DAT+6(2) '.' OLD_DAT+4(2) '.' OLD_DAT(4) INTO OLD_DATUM.

      DELETE FROM ZCUSTMAT_ROL WHERE KUNNR = WA_READ_TABLE-KUNNR AND MATNR = WA_READ_TABLE-MATNR.

      SHIFT WA_ZCUSTMAT_ROL-KUNNR LEFT DELETING LEADING SPACE.
      SHIFT WA_ZCUSTMAT_ROL-MATNR LEFT DELETING LEADING SPACE.

      PERFORM BDC_DYNPRO      USING 'SAPLZCUSTMAT_ROL' '3191'.
      PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                    'ZCUSTMAT_ROL-NEW_VAL(01)'.
      PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                    '=NEWL'.
      PERFORM BDC_DYNPRO      USING 'SAPLZCUSTMAT_ROL' '3191'.
      PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                    'ZCUSTMAT_ROL-NEW_CRE(01)'.
      PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                    '=SAVE'.
      PERFORM BDC_FIELD       USING 'ZCUSTMAT_ROL-KUNNR(01)'
                                     WA_ZCUSTMAT_ROL-KUNNR. "'100001'.
      PERFORM BDC_FIELD       USING 'ZCUSTMAT_ROL-MATNR(01)'
                                     WA_ZCUSTMAT_ROL-MATNR.   "'10000000'.
      PERFORM BDC_FIELD       USING 'ZCUSTMAT_ROL-NEW_VAL(01)'
                                     WA_ZCUSTMAT_ROL-NEW_VAL. "'100.050'.
      PERFORM BDC_FIELD       USING 'ZCUSTMAT_ROL-NEW_DAT(01)'
                                     DATUM. " '21.08.2019'.
      PERFORM BDC_FIELD       USING 'ZCUSTMAT_ROL-NEW_CRE(01)'
                                     USR_ID .               " 'prasad'.
      PERFORM BDC_FIELD       USING 'ZCUSTMAT_ROL-OLD_VAL(01)'
                                     OLD_VAL. " '100.200'.
      PERFORM BDC_FIELD       USING 'ZCUSTMAT_ROL-OLD_DAT(01)'
                                     OLD_DATUM." '10.12.2018'.
      PERFORM BDC_FIELD       USING 'ZCUSTMAT_ROL-OLD_CRE(01)'
                                     OLD_CRE.               "'prasad'.
      PERFORM BDC_DYNPRO      USING 'SAPLZCUSTMAT_ROL' '3191'.
      PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                    '/EABR'.
      PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                    'ZCUSTMAT_ROL-KUNNR(02)'.
      PERFORM BDC_DYNPRO      USING 'SAPLZCUSTMAT_ROL' '3191'.
      PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                    '/EABR'.
      PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                    'ZCUSTMAT_ROL-NEW_CRE(01)'.

    ELSE.

      SHIFT WA_ZCUSTMAT_ROL-KUNNR LEFT DELETING LEADING SPACE.
      SHIFT WA_ZCUSTMAT_ROL-MATNR LEFT DELETING LEADING SPACE.

      PERFORM BDC_DYNPRO      USING 'SAPLZCUSTMAT_ROL' '3191'.
      PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                    'ZCUSTMAT_ROL-NEW_VAL(01)'.
      PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                    '=NEWL'.
      PERFORM BDC_DYNPRO      USING 'SAPLZCUSTMAT_ROL' '3191'.
      PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                    'ZCUSTMAT_ROL-NEW_CRE(01)'.
      PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                    '=SAVE'.
      PERFORM BDC_FIELD       USING 'ZCUSTMAT_ROL-KUNNR(01)'
                                     WA_ZCUSTMAT_ROL-KUNNR. "'100001'.
      PERFORM BDC_FIELD       USING 'ZCUSTMAT_ROL-MATNR(01)'
                                     WA_ZCUSTMAT_ROL-MATNR.   "'10000000'.
      PERFORM BDC_FIELD       USING 'ZCUSTMAT_ROL-NEW_VAL(01)'
                                     WA_ZCUSTMAT_ROL-NEW_VAL.   "'10'.
      PERFORM BDC_FIELD       USING 'ZCUSTMAT_ROL-NEW_DAT(01)'
                                      DATUM. " '21.08.1991'.
      PERFORM BDC_FIELD       USING 'ZCUSTMAT_ROL-NEW_CRE(01)'
                                      USR_ID . " 'prasad'.
      PERFORM BDC_DYNPRO      USING 'SAPLZCUSTMAT_ROL' '3191'.
      PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                    '/EABR'.
      PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                    'ZCUSTMAT_ROL-KUNNR(02)'.
      PERFORM BDC_DYNPRO      USING 'SAPLZCUSTMAT_ROL' '3191'.
      PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                    '/EABR'.
      PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                    'ZCUSTMAT_ROL-NEW_CRE(01)'.

    ENDIF.
    CLEAR: WA_READ_TABLE.

    CALL TRANSACTION 'ZSKU_ROL' USING BDCDATA MODE P_MODE UPDATE P_UPDATE  MESSAGES INTO IT_MSGTAB.
    CLEAR BDCDATA.
    REFRESH BDCDATA[].

    READ TABLE IT_MSGTAB INTO WA_MSGTAB WITH KEY MSGTYP = 'E'.

*~~~~~~~~~~~~~~~~~~~ Status Modification PR@$@TH On 22/01/19
    IF SY-SUBRC = 0.

      SHIFT WA_ZCUSTMAT_ROL-KUNNR LEFT DELETING LEADING '0' .
      SHIFT WA_ZCUSTMAT_ROL-MATNR LEFT DELETING LEADING '0' .
      WA_STR1-STATUS = 'Error'.
      WA_STR1-FIELD = WA_MSGTAB-FLDNAME.
      WA_STR1-KUNNR = WA_ZCUSTMAT_ROL-KUNNR.
      WA_STR1-MATNR = WA_ZCUSTMAT_ROL-MATNR.
      WA_STR1-MINBE = QUANTITY.

    ELSE.

      SHIFT WA_ZCUSTMAT_ROL-KUNNR LEFT DELETING LEADING '0' .
      SHIFT WA_ZCUSTMAT_ROL-MATNR LEFT DELETING LEADING '0' .
      WA_STR1-STATUS = 'Sucess'.
      WA_STR1-FIELD = WA_MSGTAB-FLDNAME.
      WA_STR1-KUNNR = WA_ZCUSTMAT_ROL-KUNNR.
      WA_STR1-MATNR = WA_ZCUSTMAT_ROL-MATNR.
      WA_STR1-MINBE = QUANTITY.

    ENDIF.

    APPEND WA_STR1 TO IT_STR1.

    CLEAR:WA_STR1,WA_MSGTAB,QUANTITY.
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
  WA_FCAT-FIELDNAME = 'KUNNR' .
  WA_FCAT-COL_POS = 2 .
  WA_FCAT-SELTEXT_L = 'Customer Number' .
  WA_FCAT-OUTPUTLEN = 10 .
  WA_FCAT-TABNAME = 'IT_FINAL'.
  APPEND WA_FCAT TO IT_FCAT.

  CLEAR : WA_FCAT .
  WA_FCAT-FIELDNAME = 'MATNR' .
  WA_FCAT-COL_POS = 3 .
  WA_FCAT-SELTEXT_L = 'Material Number' .
  WA_FCAT-OUTPUTLEN = 18 .
  WA_FCAT-TABNAME = 'IT_FINAL'.
  APPEND WA_FCAT TO IT_FCAT.

  CLEAR : WA_FCAT .
  WA_FCAT-FIELDNAME = 'MESSAGE' .
  WA_FCAT-COL_POS = 4 .
  WA_FCAT-SELTEXT_L = 'Message' .
  WA_FCAT-OUTPUTLEN = 50 .
  WA_FCAT-TABNAME = 'IT_FINAL'.
  APPEND WA_FCAT TO IT_FCAT.

  CLEAR : WA_FCAT .
  WA_FCAT-FIELDNAME = 'FIELD' .
  WA_FCAT-COL_POS = 5 .
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
  BDCDATA-FNAM = FNAM.
  BDCDATA-FVAL = FVAL.
  APPEND BDCDATA.
*  ENDIF.
ENDFORM.                    "BDC_FIELD
