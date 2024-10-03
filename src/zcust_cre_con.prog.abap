*&---------------------------------------------------------------------*
*& Report  ZCUST_CRE_CON
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ZCUST_CRE_CON.

TABLES: ZCUST_CRE_CON.

TYPES:BEGIN OF TY_CER,
  BUKRS TYPE ZCUST_CRE_CON-BUKRS,
  KUNNR TYPE ZCUST_CRE_CON-KUNNR,
  NAME1 TYPE ZCUST_CRE_CON-NAME1,
  KLIMK TYPE ZCUST_CRE_CON-KLIMK,
  KLIMK_CUR TYPE ZCUST_CRE_CON-KLIMK_CUR,
  FBL5N_BAL TYPE ZCUST_CRE_CON-FBL5N_BAL,
  "AVAIL_BAL TYPE ZCUST_CRE_CON-AVAIL_BAL,
  END OF TY_CER.

  DATA: IT TYPE TABLE OF TY_CER,
        WA TYPE TY_CER.

  TYPES:BEGIN OF TY_CER1,
  BUKRS TYPE ZCUST_CRE_CON-BUKRS,
  KUNNR TYPE ZCUST_CRE_CON-KUNNR,
  NAME1 TYPE ZCUST_CRE_CON-NAME1,
  KLIMK TYPE I,"ZCUST_CRE_CON-KLIMK,
  KLIMK_CUR TYPE ZCUST_CRE_CON-KLIMK_CUR,
  FBL5N_BAL TYPE ZCUST_CRE_CON-FBL5N_BAL,
  "AVAIL_BAL TYPE ZCUST_CRE_CON-AVAIL_BAL,
  END OF TY_CER1.

DATA: IT_FINAL TYPE TABLE OF TY_CER1,
      WA_FINAL TYPE TY_CER1,
      IT_FINAL1 TYPE TABLE OF TY_CER1,
      WA_FINAL1 TYPE TY_CER1.

TYPES:BEGIN OF TY_CER_TEM,
  BUKRS TYPE ZCUST_CRE_CON-BUKRS,
  KUNNR TYPE ZCUST_CRE_CON-KUNNR,
  NAME1 TYPE ZCUST_CRE_CON-NAME1,
  KLIMK TYPE ZCUST_CRE_CON-KLIMK,
  KLIMK_CUR TYPE ZCUST_CRE_CON-KLIMK_CUR,
  FBL5N_BAL TYPE ZCUST_CRE_CON-FBL5N_BAL,
  "AVAIL_BAL TYPE ZCUST_CRE_CON-AVAIL_BAL,
  END OF TY_CER_TEM.

DATA: IT_TEM TYPE TABLE OF TY_CER_TEM,
      WA_TEM TYPE TY_CER_TEM.

DATA: IT_TEM1 TYPE TABLE OF ZCUST_CRE_CON,
      WA_TEM1 TYPE ZCUST_CRE_CON.

TYPES:BEGIN OF TY_KNB1,
  KUNNR TYPE KNB1-KUNNR,
  BUKRS TYPE KNB1-BUKRS,
  LOEVM TYPE KNB1-LOEVM,
  END OF TY_KNB1.

DATA: IT_KNB1  TYPE TABLE OF TY_KNB1,
      WA_KNB1 TYPE TY_KNB1.

TYPES:BEGIN OF TY_KNA1,
  KUNNR TYPE KNA1-KUNNR,
  NAME1 TYPE KNA1-NAME1,
  END OF TY_KNA1.

DATA: IT_KNA1 TYPE TABLE OF TY_KNA1,
      WA_KNA1 TYPE TY_KNA1.

DATA : WA_LAYOUT TYPE SLIS_LAYOUT_ALV.
DATA : IT_HEADER TYPE SLIS_T_LISTHEADER,
       WA_HEADER TYPE SLIS_LISTHEADER.

DATA : IT_FCL TYPE TABLE OF SLIS_FIELDCAT_ALV,
       WA_FCL TYPE SLIS_FIELDCAT_ALV.

DATA: POS TYPE I  VALUE 0 .

DATA: ID_RETURN	TYPE BAPIRETURN,
      IT_FBL5N  TYPE STANDARD TABLE OF BAPI3007_2,
      WA_FBL5N  LIKE LINE OF IT_FBL5N.

DATA: FBL5N_AMT TYPE NETWR.
DATA: KEY TYPE SY-UCOMM .

SELECTION-SCREEN:BEGIN OF BLOCK CCC WITH FRAME TITLE TEXT-001.
PARAMETERS: P_BUKRS LIKE ZCUST_CRE_CON-BUKRS.
SELECT-OPTIONS : S_KUNNR FOR ZCUST_CRE_CON-KUNNR.
SELECTION-SCREEN:END OF BLOCK CCC.

START-OF-SELECTION.
  PERFORM GETDATA.
  PERFORM FETCHDATA.
  PERFORM FIELDCAT.
  PERFORM DISPLAY.
END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  GETDATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GETDATA .

  IF P_BUKRS IS NOT INITIAL OR S_KUNNR IS NOT INITIAL.
    SELECT  BUKRS
            KUNNR
            NAME1
            KLIMK
            KLIMK_CUR
            FBL5N_BAL
           "AVAIL_BAL
           FROM ZCUST_CRE_CON INTO TABLE IT WHERE BUKRS EQ P_BUKRS AND KUNNR IN S_KUNNR.

    SELECT KUNNR BUKRS LOEVM FROM KNB1 INTO TABLE IT_KNB1 WHERE KUNNR IN S_KUNNR AND BUKRS EQ P_BUKRS AND LOEVM <> 'X'  .

  ELSE.
    SELECT  BUKRS
            KUNNR
            NAME1
            KLIMK
            KLIMK_CUR
            FBL5N_BAL
            "AVAIL_BAL
       FROM ZCUST_CRE_CON INTO TABLE IT ." WHERE BUKRS EQ P_BUKRS AND KUNNR IN S_KUNNR.
    SELECT KUNNR BUKRS LOEVM FROM KNB1 INTO TABLE IT_KNB1 WHERE LOEVM <> 'X'  .

  ENDIF.

  SELECT KUNNR NAME1 FROM KNA1 INTO TABLE IT_KNA1 FOR ALL ENTRIES IN IT_KNB1 WHERE KUNNR EQ IT_KNB1-KUNNR.

ENDFORM.                    " GETDATA

*&---------------------------------------------------------------------*
*&      Form  FETCHDATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FETCHDATA .

   LOOP AT IT_KNB1 INTO WA_KNB1.
    READ TABLE IT INTO WA WITH KEY BUKRS = WA_KNB1-BUKRS KUNNR = WA_KNB1-KUNNR .
    READ TABLE IT_KNA1 INTO WA_KNA1 WITH KEY KUNNR = WA_KNB1-KUNNR .
    WA_FINAL-KUNNR = WA_KNB1-KUNNR.
    WA_FINAL-BUKRS = WA_KNB1-BUKRS.
    WA_FINAL-NAME1 = WA_KNA1-NAME1.
*    IF WA IS NOT INITIAL.
    "WA_FINAL-KKBER = WA-KKBER.
    WA_FINAL-KLIMK = WA-KLIMK."#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
    WA_FINAL-KLIMK_CUR = WA-KLIMK_CUR.
*      WA_FINAL-FBL5N_BAL = WA-FBL5N_BAL.
    CLEAR: IT_FBL5N , WA_FBL5N .
    CALL FUNCTION 'BAPI_AR_ACC_GETOPENITEMS'"#EC CI_USAGE_OK[2628704]"Added by SPLABAP during code remediation
      EXPORTING
        COMPANYCODE = WA_FINAL-BUKRS
        CUSTOMER    = WA_FINAL-KUNNR
        KEYDATE     = SY-DATUM
*       NOTEDITEMS  = ' '
*       SECINDEX    = ' '
      IMPORTING
        RETURN      = ID_RETURN
      TABLES
        LINEITEMS   = IT_FBL5N.
    LOOP AT IT_FBL5N INTO WA_FBL5N.
      IF WA_FBL5N-DB_CR_IND = 'S'.
        FBL5N_AMT = FBL5N_AMT + WA_FBL5N-LC_AMOUNT .
      ELSEIF WA_FBL5N-DB_CR_IND = 'H'.
        FBL5N_AMT = FBL5N_AMT - WA_FBL5N-LC_AMOUNT .
      ENDIF.
    ENDLOOP.
    CLEAR: WA-FBL5N_BAL.
    WA_FINAL-FBL5N_BAL = FBL5N_AMT .
    CLEAR: FBL5N_AMT .
*    ENDIF.
*    IF WA_FINAL-KLIMK <> 0 ." AND WA_FINAL-FBL5N_BAL <> 0.
*      WA_FINAL-AVAIL_BAL = WA_FINAL-KLIMK - WA_FINAL-FBL5N_BAL.
*    ELSE.
*      WA_FINAL-AVAIL_BAL = 0 .
*    ENDIF.
*    SHIFT WA_FINAL-KUNNR LEFT DELETING LEADING '0' .
    APPEND: WA_FINAL TO IT_FINAL .
    CLEAR: WA_FINAL , WA_KNA1 , WA .
  ENDLOOP.
* APPEND LINES OF IT_FINAL TO IT_FINAL1.

ENDFORM.                    " FETCHDATA

*&---------------------------------------------------------------------*
*&      Form  FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FIELDCAT .

  WA_FCL-COL_POS = POS .
  WA_FCL-FIELDNAME = 'BUKRS'.
  WA_FCL-TABNAME = WA_FINAL."#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
  WA_FCL-SELTEXT_M = 'Company Code'.
  APPEND: WA_FCL TO IT_FCL.
  CLEAR: WA_FCL.

  POS = POS + 1 .
    WA_FCL-COL_POS = POS .
  WA_FCL-FIELDNAME = 'KUNNR'.
  WA_FCL-TABNAME = WA_FINAL."#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
  WA_FCL-SELTEXT_M = 'Customer Code'.
  WA_FCL-NO_ZERO = 'X'.
  APPEND: WA_FCL TO IT_FCL.
  CLEAR: WA_FCL.

  POS = POS + 1 .
  WA_FCL-COL_POS = POS .
  WA_FCL-FIELDNAME = 'NAME1'.
  WA_FCL-TABNAME = WA_FINAL."#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
  WA_FCL-SELTEXT_M = 'Customer Name'.
  APPEND: WA_FCL TO IT_FCL.
  CLEAR: WA_FCL.

*  POS = POS + 1 .
*  WA_FCL-COL_POS = POS .
*  WA_FCL-FIELDNAME = 'KKBER'.
*  WA_FCL-TABNAME = WA_FINAL.
*  WA_FCL-SELTEXT_M = 'Contorl Area'.
*  APPEND: WA_FCL TO IT_FCL.
*  CLEAR: WA_FCL.

  POS = POS + 1 .
  WA_FCL-COL_POS = POS .
  WA_FCL-FIELDNAME = 'KLIMK'.
  WA_FCL-TABNAME = WA_FINAL."#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
  WA_FCL-SELTEXT_M = 'Credit Limit'.
  WA_FCL-EDIT = 'X' .
  APPEND: WA_FCL TO IT_FCL.
  CLEAR: WA_FCL.

  POS = POS + 1 .
  WA_FCL-COL_POS = POS .
  WA_FCL-FIELDNAME = 'KLIMK_CUR'.
  WA_FCL-TABNAME = WA_FINAL."#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
  WA_FCL-SELTEXT_M = 'Current Limit'.
  APPEND: WA_FCL TO IT_FCL.
  CLEAR: WA_FCL.

  POS = POS + 1 .
  WA_FCL-COL_POS = POS .
  WA_FCL-FIELDNAME = 'FBL5N_BAL'.
  WA_FCL-TABNAME = WA_FINAL."#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
  WA_FCL-SELTEXT_M = 'FBL5N Balance'.
  APPEND: WA_FCL TO IT_FCL.
  CLEAR: WA_FCL.

*  POS = POS + 1 .
*  WA_FCL-COL_POS = POS .
*  WA_FCL-FIELDNAME = 'AVAIL_BAL'.
*  WA_FCL-TABNAME = WA_FINAL.
*  WA_FCL-SELTEXT_M = 'Available Balance'.
*  APPEND: WA_FCL TO IT_FCL.
*  CLEAR: WA_FCL.

ENDFORM.                    " FIELDCAT

*&---------------------------------------------------------------------*
*&      Form  DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY .

  WA_HEADER-TYP = 'H'.
  WA_HEADER-INFO = 'Customer Credit Control'.
  APPEND: WA_HEADER TO IT_HEADER.
  CLEAR: WA_HEADER.

  WA_HEADER-INFO = 'S'.
  WA_HEADER-KEY = ''.
  WA_HEADER-TYP = ''.
  APPEND: WA_HEADER TO IT_HEADER.
  CLEAR: WA_HEADER.

  WA_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  WA_LAYOUT-ZEBRA = 'X'.
  WA_LAYOUT-EXPAND_ALL = 'X'.


   CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
   EXPORTING
*     I_INTERFACE_CHECK                 = ' '
*     I_BYPASSING_BUFFER                = ' '
*     I_BUFFER_ACTIVE                   = ' '
      I_CALLBACK_PROGRAM                = SY-REPID
*     I_CALLBACK_PF_STATUS_SET          = 'PF_STATUS'
      I_CALLBACK_USER_COMMAND           = 'USER_CMD '
      I_CALLBACK_TOP_OF_PAGE            = 'TOP_OF_PAGE'
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME                  =
*     I_BACKGROUND_ID                   = ' '
*     I_GRID_TITLE                      =
*     I_GRID_SETTINGS                   =
      IS_LAYOUT                         = WA_LAYOUT
      IT_FIELDCAT                       = IT_FCL
*     IT_EXCLUDING                      =
*     IT_SPECIAL_GROUPS                 =
*     IT_SORT                           =
*     IT_FILTER                         =
*     IS_SEL_HIDE                       =
*     I_DEFAULT                         = 'X'
*     I_SAVE                            = ' '
*     IS_VARIANT                        =
*     IT_EVENTS                         =
*     IT_EVENT_EXIT                     =
*     IS_PRINT                          =
*     IS_REPREP_ID                      =
*     I_SCREEN_START_COLUMN             = 0
*     I_SCREEN_START_LINE               = 0
*     I_SCREEN_END_COLUMN               = 0
*     I_SCREEN_END_LINE                 = 0
*     I_HTML_HEIGHT_TOP                 = 0
*     I_HTML_HEIGHT_END                 = 0
*     IT_ALV_GRAPHICS                   =
*     IT_HYPERLINK                      =
*     IT_ADD_FIELDCAT                   =
*     IT_EXCEPT_QINFO                   =
*     IR_SALV_FULLSCREEN_ADAPTER        =
*   IMPORTING
*     E_EXIT_CAUSED_BY_CALLER           =
*     ES_EXIT_CAUSED_BY_USER            =
    TABLES
        T_OUTTAB                          = IT_FINAL
*   EXCEPTIONS
*     PROGRAM_ERROR                     = 1
*     OTHERS                            = 2
            .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.                    " DISPLAY

*&---------------------------------------------------------------------*
*&      Form  ALV_USER_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM USER_CMD USING R_UCOMM LIKE SY-UCOMM RS_SELFIELD TYPE SLIS_SELFIELD.

  CASE SY-UCOMM.
    WHEN '&DATA_SAVE'.
      LOOP AT IT_FINAL INTO WA_FINAL.
        READ TABLE IT_FINAL1 INTO WA_FINAL1 WITH KEY BUKRS = WA_FINAL-BUKRS KUNNR = WA_FINAL-KUNNR .
        READ TABLE IT INTO WA WITH KEY KUNNR = WA_FINAL-KUNNR BUKRS = WA_FINAL-BUKRS .
        IF WA IS NOT INITIAL AND WA-KLIMK <> WA_FINAL-KLIMK."#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
          MOVE-CORRESPONDING WA_FINAL TO WA_TEM.
          MOVE: WA_FINAL-KLIMK TO WA_TEM-KLIMK.
          "WA_TEM-AVAIL_BAL = WA_TEM-KLIMK - WA_TEM-FBL5N_BAL .
          UPDATE ZCUST_CRE_CON SET KLIMK = WA_TEM-KLIMK FBL5N_BAL = WA_TEM-FBL5N_BAL WHERE BUKRS = WA_TEM-BUKRS AND KUNNR = WA_TEM-KUNNR.
          CLEAR: WA_TEM.
        ELSE.
          MOVE-CORRESPONDING WA_FINAL TO WA_TEM1.
          "WA_TEM1-AVAIL_BAL = WA_TEM1-KLIMK - WA_TEM1-FBL5N_BAL.
          INSERT INTO ZCUST_CRE_CON VALUES WA_TEM1.
          CLEAR:WA_TEM1.
        ENDIF.
        COMMIT WORK.
      ENDLOOP.
      MESSAGE 'Record Saved Sucessfully' TYPE 'I'.
  ENDCASE.

ENDFORM.                    "ALV_USER_COMMAND

*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM TOP_OF_PAGE.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY       = IT_HEADER
*     I_LOGO                   =
*     I_END_OF_LIST_GRID       =
*     I_ALV_FORM               =
            .

ENDFORM.                    "TOP_OF_PAGE
