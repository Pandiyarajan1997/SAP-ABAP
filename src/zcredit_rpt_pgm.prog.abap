*&---------------------------------------------------------------------*
*& Report  ZCREDIT_RPT_PGM
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ZCREDIT_RPT_PGM.

TYPES : BEGIN OF GS_ZADV_INT_PAY,
        "MANDT TYPE ZADV_INT_PAY-MANDT,
        BUKRS TYPE ZADV_INT_PAY-BUKRS,
        KUNNR TYPE ZADV_INT_PAY-KUNNR,
        INT_DATE TYPE ZADV_INT_PAY-INT_DATE,
        BELNR TYPE ZADV_INT_PAY-BELNR,
        GJAHR TYPE ZADV_INT_PAY-GJAHR,
        DMBTR TYPE ZADV_INT_PAY-DMBTR,
        INT_PER TYPE ZADV_INT_PAY-INT_PER,
        INT_AMT TYPE ZADV_INT_PAY-INT_AMT,
        CNDOC TYPE ZADV_INT_PAY-CNDOC,
        CNDATE TYPE ZADV_INT_PAY-CNDATE,
        CNAMT TYPE ZADV_INT_PAY-CNAMT,
      END OF GS_ZADV_INT_PAY.

DATA : GT_ZADV_INT_PAY TYPE TABLE OF GS_ZADV_INT_PAY,
       WA_ZADV_INT_PAY TYPE GS_ZADV_INT_PAY.

DATA : GT_fINAL TYPE TABLE OF GS_ZADV_INT_PAY,
       WA_fINAL TYPE GS_ZADV_INT_PAY.

DATA : GT_FCAT TYPE SLIS_T_FIELDCAT_ALV,
       IT_SORT TYPE SLIS_T_SORTINFO_ALV,
       WA_FCAT TYPE SLIS_FIELDCAT_ALV,
       WA_SORT TYPE SLIS_SORTINFO_ALV.


DATA : LV_BUKRS TYPE ZADV_INT_PAY-BUKRS ,
       LV_KUNNR TYPE ZADV_INT_PAY-KUNNR,
       LV_CNDOC TYPE ZADV_INT_PAY-CNDOC,
       LV_CNDAT TYPE ZADV_INT_PAY-CNDATE.

DATA : LAYOUT TYPE SLIS_LAYOUT_ALV.

SELECTION-SCREEN : BEGIN OF BLOCK C1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS  : SO_BUKRS FOR LV_BUKRS OBLIGATORY NO INTERVALS NO-EXTENSION ,
                    SO_KUNNR FOR LV_KUNNR ,
                    SO_CNDOC FOR LV_CNDOC ,
                    SO_CNDAT FOR LV_CNDAT.

SELECTION-SCREEN : END OF BLOCK C1.

START-OF-SELECTION.
  PERFORM GET_DATA.
  PERFORM READ_DATA.

END-OF-SELECTION.
  PERFORM FIELD_CATLOG.
  PERFORM ALV_DISPLAY.
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA .

  SELECT
        BUKRS
        KUNNR
        INT_DATE
        BELNR
        GJAHR
        DMBTR
        INT_PER
        INT_AMT
        CNDOC
        CNDATE
        CNAMT FROM ZADV_INT_PAY INTO TABLE GT_ZADV_INT_PAY WHERE BUKRS IN SO_BUKRS AND KUNNR IN SO_KUNNR AND CNDOC IN SO_CNDOC AND CNDATE IN SO_CNDAT .

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_DATA .

  LOOP AT GT_ZADV_INT_PAY INTO WA_ZADV_INT_PAY .

    MOVE-CORRESPONDING WA_ZADV_INT_PAY TO WA_FINAL.
    APPEND WA_FINAL TO GT_FINAL.
    CLEAR : WA_FINAL , WA_ZADV_INT_PAY.

  ENDLOOP.

ENDFORM.                    " READ_DATA
*&---------------------------------------------------------------------*
*&      Form  FIELD_CATLOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FIELD_CATLOG .

  PERFORM ALV_LAYOUT USING 1 'Company Code' 'BUKRS' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 2 'Customer' 'KUNNR' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 3 'Interest Date' 'INT_DATE' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 4 'Document Number' 'BELNR' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 5 'Fiscal Year' 'GJAHR' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 6 'Amount in LC' 'DMBTR' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 7 'Interest Percentage' 'INT_PER' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 8 'Interest Amount' 'INT_AMT' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 9 'Credit Note Document' 'CNDOC' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 10 'Credit Note Date' 'CNDATE' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 11 'Credit Note Amount' 'CNAMT' 'GT_FINAL' ''.

ENDFORM.                    " FIELD_CATLOG
*&---------------------------------------------------------------------*
*&      Form  ALV_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ALV_DISPLAY .

  LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  LAYOUT-ZEBRA = 'X'.


  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
   EXPORTING
*     I_INTERFACE_CHECK              = ' '
*     I_BYPASSING_BUFFER             =
*     I_BUFFER_ACTIVE                = ' '
     I_CALLBACK_PROGRAM             = SY-REPID
*     I_CALLBACK_PF_STATUS_SET       = ' '
*     I_CALLBACK_USER_COMMAND        = ' '
       I_CALLBACK_TOP_OF_PAGE            = 'ALV_CATALOG_HEADER'
*     I_STRUCTURE_NAME               =
       IS_LAYOUT                     = LAYOUT
         IT_FIELDCAT                    = GT_FCAT[]
*     IT_EXCLUDING                   =
*     IT_SPECIAL_GROUPS              =
       IT_SORT                        = IT_SORT
*     IT_FILTER                      =
*     IS_SEL_HIDE                    =
*         I_DEFAULT                      = 'X'
*         I_SAVE                         = ' '
*     IS_VARIANT                     =
*     IT_EVENTS                      =
*     IT_EVENT_EXIT                  =
*     IS_PRINT                       =
*     IS_REPREP_ID                   =
*     I_SCREEN_START_COLUMN          = 0
*     I_SCREEN_START_LINE            = 0
*     I_SCREEN_END_COLUMN            = 0
*     I_SCREEN_END_LINE              = 0
*     IR_SALV_LIST_ADAPTER           =
*     IT_EXCEPT_QINFO                =
*     I_SUPPRESS_EMPTY_DATA          = ABAP_FALSE
*   IMPORTING
*     E_EXIT_CAUSED_BY_CALLER        =
*     ES_EXIT_CAUSED_BY_USER         =
        TABLES
          T_OUTTAB                       = GT_FINAL[]
*   EXCEPTIONS
*     PROGRAM_ERROR                  = 1
*     OTHERS                         = 2
                .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.


ENDFORM.                    " ALV_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  ALV_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0140   text
*      -->P_0141   text
*      -->P_0142   text
*      -->P_0143   text
*----------------------------------------------------------------------*
FORM ALV_LAYOUT USING P1 P2 P3 P4 P5.
  CLEAR WA_FCAT.
  WA_FCAT-COL_POS = P1.
  WA_FCAT-SELTEXT_L = P2.
  WA_FCAT-FIELDNAME = P3.
  WA_FCAT-TABNAME = P4.
  WA_FCAT-DO_SUM = P5.
  APPEND WA_FCAT TO GT_FCAT.
ENDFORM.                    " ALV_LAYOUT


*---------------------------------------------------
*&      Form  ALV_CATALOG_HEADER
*&-------------------------------------------------------------------
*       text
*--------------------------------------------------------------------
*  -->  p1        text
*  <--  p2        text
*--------------------------------------------------------------------
FORM ALV_CATALOG_HEADER.
  DATA : LIT_HEADER TYPE  SLIS_T_LISTHEADER,
       LS_LINE TYPE SLIS_LISTHEADER.
  CLEAR LS_LINE.
  LS_LINE-TYP  = 'H'.
  LS_LINE-KEY = ' '.
  LS_LINE-INFO = 'Auto Credit Note Details Report' .
  APPEND LS_LINE TO LIT_HEADER.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = LIT_HEADER
      I_LOGO             = 'ZLOGO'.
*  CALL FUNCTION 'CONVERSION_EXIT_SDATE_OUTPUT'
*    EXPORTING
*      INPUT  = LV_BUDAT_MKPF
*    IMPORTING
*      OUTPUT = LV_BUDAT_MKPF.

**  CALL FUNCTION 'CONVERSION_EXIT_SDATE_OUTPUT'
**
**  EXPORTING
**    INPUT         = LV_BEDAT.

*      I_LOGO             = ' '.

ENDFORM.                    "ALV_CATALOG_HEADER
