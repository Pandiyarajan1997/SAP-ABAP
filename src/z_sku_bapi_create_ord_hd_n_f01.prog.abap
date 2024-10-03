*----------------------------------------------------------------------*
***INCLUDE ZZ_TEST_BAPI_CREATE_01_HEADF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  HEADER_PARTNER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM HEADER_PARTNER .
*  * Partner data

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = W2_FINAL-SP_PAR
    IMPORTING
      OUTPUT = W2_FINAL-SP_PAR.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = W2_FINAL-SH_PAR
    IMPORTING
      OUTPUT = W2_FINAL-SH_PAR.

* Sold to
  PARTNER-PARTN_ROLE = 'AG'.
  PARTNER-PARTN_NUMB = W2_FINAL-SP_PAR.
  APPEND PARTNER.


* Sales document type
  HEADER-DOC_TYPE = 'YBBR' .
  HEADERX-DOC_TYPE = 'X'.

* Sales organization
  HEADER-SALES_ORG = W2_FINAL-SA_ORG.
  HEADERX-SALES_ORG = 'X'.

* Distribution channel
  HEADER-DISTR_CHAN  = W2_FINAL-C_CHE.
  HEADERX-DISTR_CHAN = 'X'.

* Division
  HEADER-DIVISION = W2_FINAL-C_SPART.
  HEADERX-DIVISION = 'X'.


*   Sales Office
  HEADER-SALES_OFF = W2_FINAL-VKBUR.
  HEADERX-SALES_OFF = 'X'.

*   Sales Office
  HEADER-SALES_OFF = W2_FINAL-VKBUR.
  HEADERX-SALES_OFF = 'X'.

 " Payment Terms

   HEADER-PMNTTRMS = W2_FINAL-ZTERM.
   HEADERX-PMNTTRMS = 'X'.

*   HEADERX-PMNTTRMS = W2_FINAL-ZTERM.
*   HEADERX-PMNTTRMS = 'X'.


*  PURCH_DATE
  HEADER-PURCH_DATE = SY-DATUM . " W_YVBAK-ERDAT.
  HEADERX-PURCH_DATE = 'X'.

  HEADER-PURCH_NO_C = 'Automated Order'.
  HEADERX-PURCH_NO_C = 'X'.

*  HEADER-ERNAM = 'Systems'.
*  HEADERX-ERNAM = 'X'.

  HEADERX-UPDATEFLAG = 'I'.


ENDFORM.                    " HEADER_PARTNER
*&---------------------------------------------------------------------*
*&      Form  LINEITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LINEITEM .


* ITEM DATA
  ITEMX-UPDATEFLAG = 'I'.


* Material
  ITEM-MATERIAL_LONG = W1_FINAL-MATNR.
  ITEMX-MATERIAL_LONG = 'X'.

  ITEM-DIVISION = W1_FINAL-M_SPART.
  ITEMX-DIVISION = 'X'.

 " Plant
  ITEM-PLANT    = W1_FINAL-VKBUR.
  ITEMX-PLANT   = 'X'.

  APPEND ITEM.
  APPEND ITEMX.

  " Fill schedule lines
  LT_SCHEDULES_IN-ITM_NUMBER = W1_FINAL-IT_NO .
  LT_SCHEDULES_IN-SCHED_LINE = W1_FINAL-IT_NO.
  LT_SCHEDULES_IN-REQ_QTY    = W1_FINAL-AFT_OPEN .
  APPEND LT_SCHEDULES_IN.

  ITEM_NO = W1_FINAL-IT_NO.

  APPEND CONDITION.
  APPEND CONDITIONX.


ENDFORM.                    " LINEITEM
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_SALEORDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPLOAD_SALEORDER .

*  CALL FUNCTION 'BAPI_SALESORDER_CREATEFROMDAT2'"#EC CI_USAGE_OK[2438131]
*  "Added by SPLABAP during code remediation
*    EXPORTING
**     SALESDOCUMENTIN      =
*      ORDER_HEADER_IN      = HEADER
*      ORDER_HEADER_INX     = HEADERX
*    IMPORTING
*      SALESDOCUMENT        = V_VBELN
*    TABLES
*      RETURN               = RETURN
*      ORDER_ITEMS_IN       = ITEM
*      ORDER_ITEMS_INX      = ITEMX
*      ORDER_PARTNERS       = PARTNER
*      ORDER_SCHEDULES_IN   = LT_SCHEDULES_IN
*      ORDER_SCHEDULES_INX  = LT_SCHEDULES_INX
*      ORDER_CONDITIONS_IN  = CONDITION
*      ORDER_CONDITIONS_INX = CONDITIONX.
*
** Check the return table.
*  LOOP AT RETURN WHERE TYPE = 'E' OR TYPE = 'A'.
*    PERFORM UPDATEZEETABLES_elog.
*   " EXIT.
*  ENDLOOP.
*
*  IF SY-SUBRC <> 0.
*    commit work and wait.
*    WRITE: / 'Document ', V_VBELN, ' created'.
* ENDIF.

ENDFORM.                    " UPLOAD_SALEORDER

*&---------------------------------------------------------------------*
*&      Form  UPDATEZEETABLES_ELOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATEZEETABLES_ELOG .
  data : URETURN type table of   BAPIRET2 WITH HEADER LINE.
  data : ELOG type table of YMARD_ELOG WITH HEADER LINE.
  refresh URETURN.
  refresh ELOG.
  append return to URETURN.


  LOOP AT URETURN.
    MOVE-CORRESPONDING URETURN to ELOG.
            ELOG-mandt = sy-mandt.
            ELOG-YMBELN = W_COUNT.
            ELOG-ERDAT  = SY-DATUM.
            ELOG-ERZET  = SY-UZEIT.
            ELOG-YNUMBER = URETURN-NUMBER.
            ELOG-YPARAMETER = URETURN-PARAMETER.
            ELOG-YROW = URETURN-ROW.
            append ELOG.
  CLEAR URETURN.
  ENDLOOP.
*
  if ELOG[] is not INITIAL.
  modify YMARD_ELOG FROM TABLE ELOG.
  endif.
ENDFORM.                    " UPDATEZEETABLES_ELOG
