*&---------------------------------------------------------------------*
*& Report  ZSALES_OLDORDER_CLOSE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ZSALES_OLDORDER_CLOSE.
TABLES: VBAK, YSALES_CLO_ELOG.  "VBUP,

DATA: DATE_1 TYPE D.
DATA: START_DATE_1 TYPE D.
DATA: RETURN LIKE BAPIRET2    OCCURS 0 WITH HEADER LINE.
DATA MTEXT TYPE STRING.
DATA : I_COUNT(10) TYPE N .

DATA : W_COUNT(10) TYPE N .
TYPES : BEGIN OF GS_VBAK,
          VBELN TYPE VBAK-VBELN,
       END OF GS_VBAK.

DATA :  IT_VBAK TYPE TABLE OF GS_VBAK,
        WA_VBAK TYPE GS_VBAK.

TYPES : BEGIN OF GS_VBUP,
          VBELN TYPE VBUP-VBELN,
          POSNR TYPE VBUP-POSNR,
          GBSTA TYPE VBUP-GBSTA,
       END OF GS_VBUP.

DATA :  IT_VBUP TYPE TABLE OF GS_VBUP,
        WA_VBUP TYPE GS_VBUP.

DATA: T_ITEM LIKE BAPISDITM OCCURS 0 WITH HEADER LINE.
DATA: T_ITEMX LIKE BAPISDITMX OCCURS 0 WITH HEADER LINE.
DATA: T_RETURN LIKE BAPIRET2 OCCURS 0 WITH HEADER LINE.
DATA: BAPISDH1X LIKE BAPISDH1X.
DATA: T_VBAP LIKE VBAP OCCURS 0 WITH HEADER LINE.





DATE_1 = SY-DATUM.
DATE_1 = DATE_1 - 90.
START_DATE_1 = DATE_1 - 730.
PERFORM CLOSE_ORDERS.

*&---------------------------------------------------------------------*
*&      Form  Close_orders
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM CLOSE_ORDERS.
  SELECT VBELN FROM VBAKUK INTO TABLE IT_VBAK WHERE BESTK <> ' '
    AND ( AUART = 'YBBR' OR AUART = 'YBDP' )
    AND GBSTK <> 'C'
    AND ERDAT BETWEEN START_DATE_1 AND DATE_1 . "#EC CI_USAGE_OK[2198647] "Added by SPLABAP during code remediation ".  <= DATE_1 .
  SELECT VBELN POSNR GBSTA FROM VBAP INTO TABLE IT_VBUP FOR ALL ENTRIES IN IT_VBAK WHERE VBELN = IT_VBAK-VBELN.  "VBUP

*  LOOP AT IT_VBAK INTO WA_VBAK.
*
*    CALL FUNCTION 'SD_WF_ORDER_REJECT'
*      EXPORTING
*        REASON_FOR_REJECTION = '12'
*        SALES_ORDER_DOC_NO   = WA_VBAK-VBELN.  "  SD_WF_ORDER_REJECT
*
*    IF SY-SUBRC = 0.
*       WRITE: / 'Sales Document ', WA_VBAK-VBELN, ' REJECT Status Set'.
*    ELSE.
*      WRITE: / 'Sales Document ', WA_VBAK-VBELN, 'Not REJECT Status Set'.
*    ENDIF.
*
*COMMIT WORK.
  LOOP AT IT_VBAK INTO WA_VBAK.

    READ TABLE IT_VBUP INTO WA_VBUP WITH KEY VBELN = WA_VBAK-VBELN.


    BAPISDH1X-UPDATEFLAG = 'U'.

    LOOP AT IT_VBUP INTO WA_VBUP WHERE VBELN = WA_VBAK-VBELN.

      IF WA_VBUP-GBSTA <> 'C'.
        T_ITEM-ITM_NUMBER = WA_VBUP-POSNR.
        T_ITEM-REASON_REJ = '12'.
        APPEND T_ITEM.

        T_ITEMX-ITM_NUMBER = WA_VBUP-POSNR.
        T_ITEMX-UPDATEFLAG = 'U'.
        T_ITEMX-REASON_REJ = 'X'.
        APPEND T_ITEMX.
      ENDIF.

    ENDLOOP.
    CALL FUNCTION 'BAPI_SALESORDER_CHANGE'  "#EC CI_USAGE_OK[2438131] "Added by SPLABAP during code remediation
      EXPORTING
        SALESDOCUMENT    = WA_VBAK-VBELN
        ORDER_HEADER_INX = BAPISDH1X
      TABLES
        RETURN           = T_RETURN
        ORDER_ITEM_IN    = T_ITEM
        ORDER_ITEM_INX   = T_ITEMX.

    LOOP AT T_RETURN WHERE TYPE = 'E' OR TYPE = 'A'.
      PERFORM UPDATEZEETABLES_ELOG.
*      EXIT.
    ENDLOOP.

** Check for error messages.
*    IF SY-SUBRC = 0.
*      WRITE: / 'Sales order not updated',WA_VBAK-VBELN,T_RETURN-MESSAGE.
*
*    ELSE.
*
** Successfully updated
*      WRITE: / 'Sales order updated',WA_VBAK-VBELN.
*    ENDIF.

    COMMIT WORK.
    CLEAR:
*       Variables:
            WA_VBAK,WA_VBUP,
*       Structures:
            BAPISDH1X,
            T_RETURN,
            T_ITEM,
            T_ITEMX.

*     Refreshing internal tables:
    REFRESH:
      T_RETURN,
      T_ITEM,
      T_ITEMX.

  ENDLOOP.



ENDFORM.                    "Close_orders

*&---------------------------------------------------------------------*
*&      Form  UPDATEZEETABLES_ELOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATEZEETABLES_ELOG .
  DATA : URETURN TYPE TABLE OF   BAPIRET2 WITH HEADER LINE.
  DATA : ELOG TYPE TABLE OF YSALES_CLO_ELOG WITH HEADER LINE.
  REFRESH URETURN.
  REFRESH ELOG.
  APPEND T_RETURN TO URETURN.


  LOOP AT URETURN.
    MOVE-CORRESPONDING URETURN TO ELOG.
    ELOG-MANDT = SY-MANDT.
    ELOG-YMBELN = W_COUNT.
    ELOG-ERDAT  = SY-DATUM.
    ELOG-ERZET  = SY-UZEIT.
    ELOG-YNUMBER = URETURN-NUMBER.
    ELOG-YPARAMETER = URETURN-PARAMETER.
    ELOG-YROW = URETURN-ROW.
    APPEND ELOG.
    CLEAR URETURN.
  ENDLOOP.
*
  IF ELOG[] IS NOT INITIAL.
    MODIFY YSALES_CLO_ELOG FROM TABLE ELOG.
  ENDIF.
ENDFORM.                    " UPDATEZEETABLES_ELOG
