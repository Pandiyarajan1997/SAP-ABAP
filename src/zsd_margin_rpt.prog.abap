*&---------------------------------------------------------------------*
*& Report  ZSD_MARGIN_RPT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ZSD_MARGIN_RPT.


TABLES : ZSD_MARGIN,T001W,T171T.

TYPES:BEGIN OF TY_T171T,
  SPRAS TYPE T171T-SPRAS,
  BZIRK TYPE T171T-BZIRK,
  BZTXT TYPE T171T-BZTXT,
  END OF TY_T171T.

DATA:IT_T171T TYPE TABLE OF TY_T171T,
      WA_T171T TYPE TY_T171T.

TYPES:BEGIN OF TY_T001W,
   WERKS TYPE T001W-WERKS,
   NAME1 TYPE T001W-NAME1 ,
  END OF TY_T001W.

DATA: IT_T001W TYPE TABLE OF TY_T001W,
      WA_T001W TYPE  TY_T001W.

TYPES : BEGIN OF GS_VBRP,
        VBELN TYPE VBRP-VBELN,
        POSNR TYPE VBRP-POSNR,
        FKIMG TYPE VBRP-FKIMG,
        BRGEW TYPE VBRP-BRGEW,
        MATNR TYPE VBRP-MATNR,
        WERKS TYPE VBRP-WERKS,
        VKBUR TYPE VBRP-VKBUR,
        KDGRP_AUFT TYPE VBRP-KDGRP_AUFT,
      END OF GS_VBRP.

DATA : gt_vbrp TYPE TABLE OF gs_vbrp,
       wa_vbrp TYPE gs_vbrp.

 TYPES : BEGIN OF GS_T151T,
              MANDT TYPE T151T-MANDT,
              SPRAS TYPE T151T-SPRAS,
              KDGRP TYPE T151T-KDGRP,
              KTEXT TYPE T151T-KTEXT,
            END OF GS_T151T.

DATA : GT_T151T TYPE TABLE OF GS_T151T,
       WA_T151T TYPE GS_T151T.

TYPES: BEGIN OF TY_ZSD_MARGIN,
        VBELN TYPE ZSD_MARGIN-VBELN,
        POSNR TYPE ZSD_MARGIN-POSNR,
        FKART TYPE ZSD_MARGIN-FKART,
        FKDAT TYPE ZSD_MARGIN-FKDAT,
        KUNNR TYPE ZSD_MARGIN-KUNNR,
        NAME1 TYPE ZSD_MARGIN-NAME1,
        MATNR TYPE ZSD_MARGIN-MATNR,
        MAKTX TYPE ZSD_MARGIN-MAKTX,
        WERKS TYPE ZSD_MARGIN-WERKS,
        BZIRK TYPE ZSD_MARGIN-BZIRK,
        VKBUR TYPE ZSD_MARGIN-VKBUR,
        KBETR TYPE ZSD_MARGIN-KBETR,
        FKIMG TYPE ZSD_MARGIN-FKIMG,
        QTY_LIT TYPE ZSD_MARGIN-QTY_LIT,
        SAL_VAL TYPE ZSD_MARGIN-SAL_VAL,
        TAX_GST TYPE ZSD_MARGIN-TAX_GST,
        INC_TAX TYPE ZSD_MARGIN-INC_TAX,
        MAT_COST TYPE ZSD_MARGIN-MAT_COST,
        TURN_REB TYPE ZSD_MARGIN-TURN_REB,
        FRI_COST TYPE ZSD_MARGIN-FRI_COST,
        REB_BILL TYPE ZSD_MARGIN-REB_BILL,
        CASH_DIS TYPE ZSD_MARGIN-CASH_DIS,
        SKU_COMM TYPE ZSD_MARGIN-SKU_COMM,
        OPEX_BILL TYPE ZSD_MARGIN-OPEX_BILL,
        CORP_BILL TYPE ZSD_MARGIN-CORP_BILL,
        SCHEME TYPE ZSD_MARGIN-SCHEME,
        PROD_CON TYPE ZSD_MARGIN-PROD_CON,
        PROD_PER TYPE ZSD_MARGIN-PROD_PER,
      END OF TY_ZSD_MARGIN.

DATA:IT_ZSD_MARGIN TYPE TABLE OF TY_ZSD_MARGIN,
     WA_ZSD_MARGIN TYPE TY_ZSD_MARGIN.

TYPES: BEGIN OF TY_FINAL,
        VBELN TYPE ZSD_MARGIN-VBELN,
        POSNR TYPE ZSD_MARGIN-POSNR,
        FKART TYPE ZSD_MARGIN-FKART,
        FKDAT TYPE ZSD_MARGIN-FKDAT,
        KUNNR TYPE ZSD_MARGIN-KUNNR,
        NAME1 TYPE ZSD_MARGIN-NAME1,
        MATNR TYPE ZSD_MARGIN-MATNR,
        MAKTX TYPE ZSD_MARGIN-MAKTX,
        WERKS TYPE ZSD_MARGIN-WERKS,
        BZIRK TYPE ZSD_MARGIN-BZIRK,
        VKBUR TYPE ZSD_MARGIN-VKBUR,
        KBETR TYPE ZSD_MARGIN-KBETR,
        FKIMG TYPE ZSD_MARGIN-FKIMG,
        QTY_LIT TYPE ZSD_MARGIN-QTY_LIT,
        SAL_VAL TYPE ZSD_MARGIN-SAL_VAL,
        TAX_GST TYPE ZSD_MARGIN-TAX_GST,
        INC_TAX TYPE ZSD_MARGIN-INC_TAX,
        MAT_COST TYPE ZSD_MARGIN-MAT_COST,
        TURN_REB TYPE ZSD_MARGIN-TURN_REB,
        FRI_COST TYPE ZSD_MARGIN-FRI_COST,
        REB_BILL TYPE ZSD_MARGIN-REB_BILL,
        CASH_DIS TYPE ZSD_MARGIN-CASH_DIS,
        SKU_COMM TYPE ZSD_MARGIN-SKU_COMM,
        OPEX_BILL TYPE ZSD_MARGIN-OPEX_BILL,
        CORP_BILL TYPE ZSD_MARGIN-CORP_BILL,
        SCHEME TYPE ZSD_MARGIN-SCHEME,
        PROD_CON TYPE ZSD_MARGIN-PROD_CON,
        PROD_PER TYPE ZSD_MARGIN-PROD_PER,

*        SPRAS_T171T TYPE T171T-SPRAS,
*        BZIRKT_171T TYPE T171T-BZIRK,
        BZTXT TYPE T171T-BZTXT,
        KTEXT TYPE T151T-KTEXT,
*        WERKS_T001W TYPE T001W-WERKS,
        NAME1_T001W TYPE T001W-NAME1 ,
       END OF TY_FINAL.

DATA: IT_FINAL TYPE TABLE OF TY_FINAL,
      WA_FINAL TYPE TY_FINAL.


SELECTION-SCREEN:BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001 .
*PARAMETERS:S_FKART TYPE ZSD_MARGIN-FKART.
SELECT-OPTIONS: S_FKART FOR ZSD_MARGIN-FKART,
                S_WERKS FOR ZSD_MARGIN-WERKS,
                S_KUNNR FOR ZSD_MARGIN-KUNNR,
                S_MATNR FOR ZSD_MARGIN-MATNR,
                S_FKDAT FOR ZSD_MARGIN-FKDAT.

SELECTION-SCREEN: END OF BLOCK B1.

START-OF-SELECTION.
  PERFORM GET_DATA.
  PERFORM PUT_DATA.

END-OF-SELECTION.

  DATA: IT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,
        WA_FIELDCAT TYPE SLIS_FIELDCAT_ALV,
        WA_LAYOUT TYPE SLIS_LAYOUT_ALV,
        IT_REPID TYPE SY-REPID VALUE SY-REPID .


  PERFORM FIELDCAT.
  PERFORM LAYOUT.
  PERFORM DISPLAY.

*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA .

  SELECT VBELN POSNR FKART FKDAT KUNNR NAME1 MATNR MAKTX WERKS BZIRK VKBUR  KBETR FKIMG QTY_LIT SAL_VAL TAX_GST INC_TAX MAT_COST TURN_REB FRI_COST
    REB_BILL CASH_DIS SKU_COMM OPEX_BILL CORP_BILL SCHEME PROD_CON PROD_PER FROM ZSD_MARGIN
      INTO TABLE IT_ZSD_MARGIN
*    WHERE FKART = S_FKART
*    AND   WERKS IN S_WERKS
*    AND   KUNNR IN S_KUNNR
*    AND   MATNR IN S_MATNR
*    AND   FKDAT IN S_FKDAT.
    WHERE  FKART IN S_FKART
    AND   FKDAT IN S_FKDAT
    AND   KUNNR IN S_KUNNR
    AND   MATNR IN S_MATNR
    AND   WERKS IN S_WERKS.

  IF IT_ZSD_MARGIN IS NOT INITIAL.
    SELECT WERKS NAME1 FROM T001W
      INTO TABLE IT_T001W
      FOR ALL ENTRIES IN IT_ZSD_MARGIN
      WHERE WERKS = IT_ZSD_MARGIN-WERKS.

    SELECT SPRAS BZIRK BZTXT FROM T171T
      INTO TABLE IT_T171T
       FOR ALL ENTRIES IN IT_ZSD_MARGIN
    WHERE SPRAS = 'EN'
     AND  BZIRK = IT_ZSD_MARGIN-BZIRK.


    SELECT VBELN "Billing Document
      POSNR "Item No.
      FKIMG "Actual Invoiced Quantity
      BRGEW "Gross Weight
      MATNR "Material Number
      WERKS "Plant
      VKBUR "Sales Office
      KDGRP_AUFT " Customer sales group
     FROM VBRP
INTO TABLE GT_VBRP
FOR ALL ENTRIES IN IT_ZSD_MARGIN
WHERE VBELN = IT_ZSD_MARGIN-VBELN .

    SELECT
            MANDT
            SPRAS
            KDGRP
            KTEXT
              FROM T151T INTO TABLE GT_T151T WHERE SPRAS = 'EN' .

  ENDIF.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  PUT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PUT_DATA .


  LOOP AT IT_ZSD_MARGIN INTO WA_ZSD_MARGIN.

    WA_FINAL-VBELN = WA_ZSD_MARGIN-VBELN.
    WA_FINAL-FKART = WA_ZSD_MARGIN-FKART.
    WA_FINAL-FKDAT = WA_ZSD_MARGIN-FKDAT.
    WA_FINAL-KUNNR = WA_ZSD_MARGIN-KUNNR.
    WA_FINAL-NAME1 = WA_ZSD_MARGIN-NAME1.
    WA_FINAL-MATNR = WA_ZSD_MARGIN-MATNR.
    WA_FINAL-MAKTX = WA_ZSD_MARGIN-MAKTX.
    WA_FINAL-WERKS = WA_ZSD_MARGIN-WERKS.
*    WA_FINAL-BZIRK = WA_ZSD_MARGIN-BZIRK.
    WA_FINAL-VKBUR = WA_ZSD_MARGIN-VKBUR.
    WA_FINAL-KBETR = WA_ZSD_MARGIN-KBETR.
    WA_FINAL-FKIMG = WA_ZSD_MARGIN-FKIMG.
    WA_FINAL-QTY_LIT = WA_ZSD_MARGIN-QTY_LIT.
    WA_FINAL-SAL_VAL = WA_ZSD_MARGIN-SAL_VAL.
    WA_FINAL-TAX_GST = WA_ZSD_MARGIN-TAX_GST.
    WA_FINAL-INC_TAX = WA_ZSD_MARGIN-INC_TAX.
    WA_FINAL-MAT_COST = WA_ZSD_MARGIN-MAT_COST.
    WA_FINAL-TURN_REB = WA_ZSD_MARGIN-TURN_REB.
    WA_FINAL-FRI_COST = WA_ZSD_MARGIN-FRI_COST.
    WA_FINAL-REB_BILL = WA_ZSD_MARGIN-REB_BILL.
    WA_FINAL-CASH_DIS = WA_ZSD_MARGIN-CASH_DIS.
    WA_FINAL-SKU_COMM = WA_ZSD_MARGIN-SKU_COMM.
    WA_FINAL-OPEX_BILL = WA_ZSD_MARGIN-OPEX_BILL.
    WA_FINAL-CORP_BILL = WA_ZSD_MARGIN-CORP_BILL.
    WA_FINAL-SCHEME = WA_ZSD_MARGIN-SCHEME.
    WA_FINAL-PROD_CON = WA_ZSD_MARGIN-PROD_CON.
    WA_FINAL-PROD_PER = WA_ZSD_MARGIN-PROD_PER.


    READ TABLE IT_T001W INTO WA_T001W WITH KEY WERKS = WA_ZSD_MARGIN-WERKS.

    IF SY-SUBRC = 0.
      WA_FINAL-NAME1_T001W = WA_T001W-NAME1.

    ENDIF.

    READ TABLE IT_T171T INTO WA_T171T WITH KEY SPRAS = 'EN'
                                                BZIRK = WA_ZSD_MARGIN-BZIRK.

    IF  SY-SUBRC = 0.
      WA_FINAL-BZTXT = WA_T171T-BZTXT.

    ENDIF.

    READ TABLE GT_VBRP INTO WA_VBRP WITH KEY VBELN = WA_ZSD_MARGIN-VBELN .
      IF SY-SUBRC = 0 .
    READ TABLE GT_T151T INTO WA_T151T WITH KEY SPRAS = 'EN' KDGRP = WA_VBRP-KDGRP_AUFT . " KDGRP = WA_ZSD_MARGIN-
    IF  SY-SUBRC = 0.
         WA_FINAL-KTEXT = WA_T151T-KTEXT .
    ENDIF.
    ENDIF.

    APPEND WA_FINAL TO IT_FINAL.
    CLEAR WA_FINAL.
    CLEAR:  WA_VBRP,WA_T151T.
  ENDLOOP.

ENDFORM.                    " PUT_DATA
*&---------------------------------------------------------------------*
*&      Form  FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FIELDCAT .

  WA_FIELDCAT-FIELDNAME = 'VBELN'.
  WA_FIELDCAT-TABNAME = 'IT_FINAL'.
  WA_FIELDCAT-SELTEXT_S = 'Document'.
  WA_FIELDCAT-SELTEXT_L = 'Document'.
  WA_FIELDCAT-COL_POS = 1.
  WA_FIELDCAT-OUTPUTLEN = '15'.

  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME = 'FKART'.
  WA_FIELDCAT-TABNAME = 'IT_FINAL'.
  WA_FIELDCAT-SELTEXT_S = 'BillT'.
  WA_FIELDCAT-SELTEXT_L = 'Billing Type'.
  WA_FIELDCAT-COL_POS = 2.
  WA_FIELDCAT-OUTPUTLEN = '5'.

  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR WA_FIELDCAT.
  WA_FIELDCAT-FIELDNAME = 'FKDAT'.
  WA_FIELDCAT-TABNAME = 'IT_FINAL'.
  WA_FIELDCAT-SELTEXT_S = 'Billing Date'.
  WA_FIELDCAT-SELTEXT_L = 'Billing Date'.
  WA_FIELDCAT-COL_POS = 3.
  WA_FIELDCAT-OUTPUTLEN = '12'.

  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME = 'KTEXT'.
  WA_FIELDCAT-TABNAME = 'IT_FINAL'.
  WA_FIELDCAT-SELTEXT_S = 'Customer Group Desc'.
  WA_FIELDCAT-SELTEXT_L = 'Customer Group Desc'.
  WA_FIELDCAT-COL_POS = 4.
  WA_FIELDCAT-OUTPUTLEN = '12'.

  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME = 'KUNNR'.
  WA_FIELDCAT-TABNAME = 'IT_FINAL'.
  WA_FIELDCAT-SELTEXT_S = 'Customer'.
  WA_FIELDCAT-SELTEXT_L = 'Customer'.
  WA_FIELDCAT-COL_POS = 5.
  WA_FIELDCAT-OUTPUTLEN = '12'.

  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME = 'NAME1'.
  WA_FIELDCAT-TABNAME = 'IT_FINAL'.
  WA_FIELDCAT-SELTEXT_S = 'Name'.
  WA_FIELDCAT-SELTEXT_L = 'Name'.
  WA_FIELDCAT-COL_POS = 6.
  WA_FIELDCAT-OUTPUTLEN = '10'.

  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR WA_FIELDCAT.


  WA_FIELDCAT-FIELDNAME = 'MATNR'.
  WA_FIELDCAT-TABNAME = 'IT_FINAL'.
  WA_FIELDCAT-SELTEXT_S = 'Material'.
  WA_FIELDCAT-SELTEXT_L = 'Material'.
  WA_FIELDCAT-COL_POS = 7.
  WA_FIELDCAT-OUTPUTLEN = '10'.

  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME = 'MAKTX'.
  WA_FIELDCAT-TABNAME = 'IT_FINAL'.
  WA_FIELDCAT-SELTEXT_S = 'Material Description'.
  WA_FIELDCAT-SELTEXT_L = 'Material Description'.
  WA_FIELDCAT-COL_POS = 8.
  WA_FIELDCAT-OUTPUTLEN = '20'.

  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME = 'WERKS'.
  WA_FIELDCAT-TABNAME = 'IT_FINAL'.
  WA_FIELDCAT-SELTEXT_S = 'Plant'.
  WA_FIELDCAT-SELTEXT_L = 'Plant'.
  WA_FIELDCAT-COL_POS = 9.
  WA_FIELDCAT-OUTPUTLEN = '5'.

  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME = 'NAME1_T001W'.
  WA_FIELDCAT-TABNAME = 'IT_FINAL'.
  WA_FIELDCAT-SELTEXT_S = 'Name'.
  WA_FIELDCAT-SELTEXT_L = 'Name'.
  WA_FIELDCAT-COL_POS = 10.
  WA_FIELDCAT-OUTPUTLEN = '20'.

  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR WA_FIELDCAT.

*  WA_FIELDCAT-FIELDNAME = 'BZIRK'.
*  WA_FIELDCAT-TABNAME = 'IT_FINAL'.
*  WA_FIELDCAT-SELTEXT_S = 'District'.
*  WA_FIELDCAT-SELTEXT_L = 'Sales District'.
*  WA_FIELDCAT-COL_POS = 9.
*  WA_FIELDCAT-OUTPUTLEN = '7'.
*
*  APPEND WA_FIELDCAT TO IT_FIELDCAT.
*  CLEAR WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME = 'BZTXT'.
  WA_FIELDCAT-TABNAME = 'IT_FINAL'.
  WA_FIELDCAT-SELTEXT_S = 'Region Name'.
  WA_FIELDCAT-SELTEXT_L = 'Region Name'.
  WA_FIELDCAT-COL_POS = 11.
  WA_FIELDCAT-OUTPUTLEN = '15'.

  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME = 'VKBUR'.
  WA_FIELDCAT-TABNAME = 'IT_FINAL'.
  WA_FIELDCAT-SELTEXT_S = 'Office'.
  WA_FIELDCAT-SELTEXT_L = 'Sales Office'.
  WA_FIELDCAT-COL_POS = 12.
  WA_FIELDCAT-OUTPUTLEN = '5'.

  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME = 'KBETR'.
  WA_FIELDCAT-TABNAME = 'IT_FINAL'.
  WA_FIELDCAT-SELTEXT_S = 'Selling Price'.
  WA_FIELDCAT-SELTEXT_L = 'Selling Price'.
  WA_FIELDCAT-COL_POS = 13.
  WA_FIELDCAT-OUTPUTLEN = '10'.

  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME = 'FKIMG'.
  WA_FIELDCAT-TABNAME = 'IT_FINAL'.
  WA_FIELDCAT-SELTEXT_S = 'Billed Qty'.
  WA_FIELDCAT-SELTEXT_L = 'Billed Qty'.
  WA_FIELDCAT-COL_POS = 14.
  WA_FIELDCAT-OUTPUTLEN = '10'.

  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME = 'QTY_LIT'.
  WA_FIELDCAT-TABNAME = 'IT_FINAL'.
  WA_FIELDCAT-SELTEXT_S = 'Billing Qt'.
  WA_FIELDCAT-SELTEXT_L = 'Billing Qty in Litre'.
  WA_FIELDCAT-COL_POS = 15.
  WA_FIELDCAT-OUTPUTLEN = '10'.

  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME = 'SAL_VAL'.
  WA_FIELDCAT-TABNAME = 'IT_FINAL'.
  WA_FIELDCAT-SELTEXT_S = 'Sales Value'.
  WA_FIELDCAT-SELTEXT_L = 'Sales Value'.
  WA_FIELDCAT-COL_POS = 16.
  WA_FIELDCAT-OUTPUTLEN = '10'.

  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME = 'TAX_GST'.
  WA_FIELDCAT-TABNAME = 'IT_FINAL'.
  WA_FIELDCAT-SELTEXT_S = 'TAX( GST)'.
  WA_FIELDCAT-SELTEXT_L = 'TAX( GST)'.
  WA_FIELDCAT-COL_POS = 17.
  WA_FIELDCAT-OUTPUTLEN = '10'.

  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME = 'INC_TAX'.
  WA_FIELDCAT-TABNAME = 'IT_FINAL'.
  WA_FIELDCAT-SELTEXT_S = 'Inc Tax'.
  WA_FIELDCAT-SELTEXT_L = 'Including With Tax'.
  WA_FIELDCAT-COL_POS = 18.
  WA_FIELDCAT-OUTPUTLEN = '10'.

  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME = 'MAT_COST'.
  WA_FIELDCAT-TABNAME = 'IT_FINAL'.
  WA_FIELDCAT-SELTEXT_S = 'Mat Cost'.
  WA_FIELDCAT-SELTEXT_L = 'Material Cost'.
  WA_FIELDCAT-COL_POS = 19.
  WA_FIELDCAT-OUTPUTLEN = '10'.

  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME = 'TURN_REB'.
  WA_FIELDCAT-TABNAME = 'IT_FINAL'.
  WA_FIELDCAT-SELTEXT_S = 'Turnover R'.
  WA_FIELDCAT-SELTEXT_L = 'Turnover Rebate Disc'.
  WA_FIELDCAT-COL_POS = 20.
  WA_FIELDCAT-OUTPUTLEN = '10'.

  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR WA_FIELDCAT.


  WA_FIELDCAT-FIELDNAME = 'FRI_COST'.
  WA_FIELDCAT-TABNAME = 'IT_FINAL'.
  WA_FIELDCAT-SELTEXT_S = 'Frt cost'.
  WA_FIELDCAT-SELTEXT_L = 'Freight cost'.
  WA_FIELDCAT-COL_POS = 21.
  WA_FIELDCAT-OUTPUTLEN = '8'.

  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME = 'REB_BILL'.
  WA_FIELDCAT-TABNAME = 'IT_FINAL'.
  WA_FIELDCAT-SELTEXT_S = 'Reb Bill'.
  WA_FIELDCAT-SELTEXT_L = 'Rebate in Bill'.
  WA_FIELDCAT-COL_POS = 22.
  WA_FIELDCAT-OUTPUTLEN = '8'.

  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME = 'CASH_DIS'.
  WA_FIELDCAT-TABNAME = 'IT_FINAL'.
  WA_FIELDCAT-SELTEXT_S = 'Cash Dis'.
  WA_FIELDCAT-SELTEXT_L = 'Cash Discount'.
  WA_FIELDCAT-COL_POS = 23.
  WA_FIELDCAT-OUTPUTLEN = '10'.

  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME = 'SKU_COMM'.
  WA_FIELDCAT-TABNAME = 'IT_FINAL'.
  WA_FIELDCAT-SELTEXT_S = 'SKU Comm'.
  WA_FIELDCAT-SELTEXT_L = 'SKU commission'.
  WA_FIELDCAT-COL_POS = 24.
  WA_FIELDCAT-OUTPUTLEN = '10'.

  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME = 'OPEX_BILL'.
  WA_FIELDCAT-TABNAME = 'IT_FINAL'.
  WA_FIELDCAT-SELTEXT_S = 'Opex Marg'.
  WA_FIELDCAT-SELTEXT_L = 'Opex Margin'.
  WA_FIELDCAT-COL_POS = 25.
  WA_FIELDCAT-OUTPUTLEN = '10'.

  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME = 'CORP_BILL'.
  WA_FIELDCAT-TABNAME = 'IT_FINAL'.
  WA_FIELDCAT-SELTEXT_S = 'Corp Marg'.
  WA_FIELDCAT-SELTEXT_L = 'Corporate Margin'.
  WA_FIELDCAT-COL_POS = 26.
  WA_FIELDCAT-OUTPUTLEN = '10'.

  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME = 'SCHEME'.
  WA_FIELDCAT-TABNAME = 'IT_FINAL'.
  WA_FIELDCAT-SELTEXT_S = 'Scheme Val'.
  WA_FIELDCAT-SELTEXT_L = 'Scheme Value in CN'.
  WA_FIELDCAT-COL_POS = 27.
  WA_FIELDCAT-OUTPUTLEN = '10'.

  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME = 'PROD_CON'.
  WA_FIELDCAT-TABNAME = 'IT_FINAL'.
  WA_FIELDCAT-SELTEXT_S = 'Product Co'.
  WA_FIELDCAT-SELTEXT_L = 'Product Contribution'.
  WA_FIELDCAT-COL_POS = 28.
  WA_FIELDCAT-OUTPUTLEN = '9'.

  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME = 'PROD_PER'.
  WA_FIELDCAT-TABNAME = 'IT_FINAL'.
  WA_FIELDCAT-SELTEXT_S = 'Pro Per'.
  WA_FIELDCAT-SELTEXT_L = 'Product Percentage'.
  WA_FIELDCAT-COL_POS = 29.
  WA_FIELDCAT-OUTPUTLEN = '8'.

  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR WA_FIELDCAT.




ENDFORM.                    " FIELDCAT

*&---------------------------------------------------------------------*
*&      Form  LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LAYOUT .
  WA_LAYOUT-ZEBRA ='X'.
ENDFORM.                    " LAYOUT
*&---------------------------------------------------------------------*
*&      Form  DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY .
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
   EXPORTING
*   I_INTERFACE_CHECK                 = ' '
*   I_BYPASSING_BUFFER                = ' '
*   I_BUFFER_ACTIVE                   = ' '
     I_CALLBACK_PROGRAM                = IT_REPID
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
     IT_FIELDCAT                       = IT_FIELDCAT
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
* Implement suitable error handling here
  ENDIF.

ENDFORM.                    " DISPLAY
