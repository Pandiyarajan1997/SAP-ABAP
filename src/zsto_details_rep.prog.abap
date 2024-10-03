*&---------------------------------------------------------------------*
*& Report  ZSTO_DETAILS_REP
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ZSTO_DETAILS_REP.

TYPES : BEGIN OF GS_EKPO,
        MANDT TYPE EKPO-MANDT,
        EBELN TYPE EKPO-EBELN,
        EBELP TYPE EKPO-EBELP,
        MATNR TYPE EKPO-MATNR,
        BUKRS TYPE EKPO-BUKRS,
        WERKS TYPE EKPO-WERKS,
        KTMNG TYPE EKPO-KTMNG,
        MENGE TYPE EKPO-MENGE,
        MEINS TYPE EKPO-MEINS,
      END OF GS_EKPO.

 DATA : GT_EKPO TYPE TABLE OF GS_EKPO,
        WA_EKPO TYPE GS_EKPO.

 TYPES : BEGIN OF GS_EKKO ,
          EBELN TYPE EKKO-EBELN,
          BSART TYPE EKKO-BSART,
         END OF GS_EKKO.
  DATA : GT_EKKO TYPE TABLE OF GS_EKKO,
         WA_EKKO TYPE GS_EKKO.

TYPES : BEGIN OF GS_EKBE,
        MANDT TYPE EKBE-MANDT,
        EBELN TYPE EKBE-EBELN,
        EBELP TYPE EKBE-EBELP,
        ZEKKN TYPE EKBE-ZEKKN,
        VGABE TYPE EKBE-VGABE,
        GJAHR TYPE EKBE-GJAHR,
        BELNR TYPE EKBE-BELNR,
        BUZEI TYPE EKBE-BUZEI,
        BEWTP TYPE EKBE-BEWTP,
        BUDAT TYPE EKBE-BUDAT,
        MENGE TYPE EKBE-MENGE,
        DMBTR TYPE EKBE-DMBTR,
        XBLNR TYPE EKBE-XBLNR,
        MATNR TYPE EKBE-MATNR,
        WERKS TYPE EKBE-WERKS,
        CHARG TYPE EKBE-CHARG,
        BLDAT TYPE EKBE-BLDAT,
      END OF GS_EKBE.

DATA : GT_EKBE TYPE TABLE OF GS_EKBE,
        WA_EKBE TYPE GS_EKBE.

TYPES : BEGIN OF GS_MKPF,
        MANDT TYPE MKPF-MANDT,
        MBLNR TYPE MKPF-MBLNR,
        MJAHR TYPE MKPF-MJAHR,
        BLART TYPE MKPF-BLART,
        BLDAT TYPE MKPF-BLDAT,
        BUDAT TYPE MKPF-BUDAT,
      END OF GS_MKPF.

DATA : GT_MKPF TYPE TABLE OF GS_MKPF,
          WA_MKPF TYPE GS_MKPF.

TYPES : BEGIN OF GS_MSEG,
        MANDT TYPE MSEG-MANDT,
        MBLNR TYPE MSEG-MBLNR,
        MJAHR TYPE MSEG-MJAHR,
        ZEILE TYPE MSEG-ZEILE,
        BWART TYPE MSEG-BWART,
        MATNR TYPE MSEG-MATNR,
        WERKS TYPE MSEG-WERKS,
        EBELN TYPE MSEG-EBELN,
        EBELP TYPE MSEG-EBELP,
        ZEKKN TYPE MSEG-ZEKKN,
      END OF GS_MSEG.

DATA : GT_MSEG TYPE TABLE OF GS_MSEG,
        WA_MSEG TYPE GS_MSEG.

TYPES : BEGIN OF GS_T001W,
          WERKS TYPE T001W-WERKS,
          NAME1 TYPE T001W-NAME1,
       END OF GS_T001W.

DATA : GT_T001W TYPE TABLE OF GS_T001W,
        WA_T001W TYPE GS_T001W.

TYPES : BEGIN OF GS_MAKT ,
         MATNR TYPE MAKT-MATNR,
         MAKTX TYPE MAKT-MAKTX,
       END OF GS_MAKT.

DATA : GT_MAKT TYPE TABLE OF GS_MAKT,
       WA_MAKT TYPE GS_MAKT.

TYPES : BEGIN OF GS_MARA,
          MATNR TYPE MARA-MATNR,
          VOLUM TYPE MARA-VOLUM,
          SPART TYPE MARA-SPART,
        END OF GS_MARA .

DATA : GT_MARA TYPE TABLE OF GS_MARA,
       WA_MARA TYPE GS_MARA.

*TYPES : BEGIN OF GS_ZTRIP_ST,
*        MANDT TYPE ZTRIP_ST-MANDT,
*        INVOICENO TYPE ZTRIP_ST-INVOICENO,
*        INVOICEDATE TYPE ZTRIP_ST-INVOICEDATE,
*        SKUDEALERNAME TYPE ZTRIP_ST-SKUDEALERNAME,
*        TRANSPORTERNAMELRNO TYPE ZTRIP_ST-TRANSPORTERNAMELRNO,
*        VEHICLENODRIVERNAME TYPE ZTRIP_ST-VEHICLENODRIVERNAME,
*        UNIQ1 TYPE ZTRIP_ST-UNIQ1,
*        TDATE TYPE ZTRIP_ST-TDATE,
*        TIME TYPE ZTRIP_ST-TIME,
*        CUSTOMER TYPE ZTRIP_ST-CUSTOMER,
*        NAME TYPE ZTRIP_ST-NAME,
*        CITY TYPE ZTRIP_ST-CITY,
*        BUKRS TYPE ZTRIP_ST-BUKRS,
*      END OF GS_ZTRIP_ST.
*
*DATA : GT_ZTRIP_ST TYPE TABLE OF GS_ZTRIP_ST,
*       WA_ZTRIP_ST TYPE GS_ZTRIP_ST.

TYPES : BEGIN OF GS_ZFREIGHT_HEADER,
        MANDT TYPE ZFREIGHT_HEADER-MANDT,
        TRIP_NO TYPE ZFREIGHT_HEADER-TRIP_NO,
        STATUS TYPE ZFREIGHT_HEADER-STATUS,
        VENDOR_CODE TYPE ZFREIGHT_HEADER-VENDOR_CODE,
        VENDOR_NAME TYPE ZFREIGHT_HEADER-VENDOR_NAME,
        FROM_CODE TYPE ZFREIGHT_HEADER-FROM_CODE,
        FROM_LOC TYPE ZFREIGHT_HEADER-FROM_LOC,
        TO_LOC TYPE ZFREIGHT_HEADER-TO_LOC,
        VECHILE_NUMBER TYPE ZFREIGHT_HEADER-VECHILE_NUMBER,
        CRDATE TYPE ZFREIGHT_HEADER-CRDATE,
        CRTIME TYPE ZFREIGHT_HEADER-CRTIME,
      END OF GS_ZFREIGHT_HEADER.

 DATA : GT_ZFREIGHT_HEADER TYPE TABLE OF GS_ZFREIGHT_HEADER,
        WA_ZFREIGHT_HEADER TYPE GS_ZFREIGHT_HEADER.

 TYPES : BEGIN OF GS_ZFREIGHT_ITEM,
              MANDT TYPE ZFREIGHT_ITEM-MANDT,
              TRIP_NO TYPE ZFREIGHT_ITEM-TRIP_NO,
              INVOICE_NO TYPE ZFREIGHT_ITEM-INVOICE_NO,
              COMPANY_CODE TYPE ZFREIGHT_ITEM-COMPANY_CODE,
              LR_NO TYPE ZFREIGHT_ITEM-LR_NO,
            END OF GS_ZFREIGHT_ITEM.

DATA : GT_ZFREIGHT_ITEM TYPE TABLE OF GS_ZFREIGHT_ITEM,
       WA_ZFREIGHT_ITEM TYPE GS_ZFREIGHT_ITEM.

TYPES : BEGIN OF GS_FINAL,
        EBELN TYPE EKBE-EBELN,
        EBELP TYPE EKBE-EBELP,
        ZEKKN TYPE EKBE-ZEKKN,
        VGABE TYPE EKBE-VGABE,
        GJAHR TYPE EKBE-GJAHR,
        BELNR TYPE EKBE-BELNR,
        BUZEI TYPE EKBE-BUZEI,
        BEWTP TYPE EKBE-BEWTP,
        BUDAT TYPE EKBE-BUDAT,
        MENGE TYPE EKBE-MENGE,
        DMBTR TYPE EKBE-DMBTR,
        XBLNR TYPE EKBE-XBLNR,
        MATNR TYPE EKBE-MATNR,
        WERKS TYPE EKBE-WERKS,
        CHARG TYPE EKBE-CHARG,
        BLDAT TYPE EKBE-BLDAT,
        MATNR1 TYPE EKPO-MATNR,
        BUKRS TYPE EKPO-BUKRS,
        WERKS1 TYPE EKPO-WERKS,
        KTMNG TYPE EKPO-KTMNG,
        MENGE1 TYPE EKPO-MENGE,
        MEINS TYPE EKPO-MEINS,
        NAME1 TYPE T001W-NAME1,
        NAME2 TYPE T001W-NAME1,
        MAKTX TYPE MAKT-MAKTX,
        VOLUM TYPE MARA-VOLUM,
        SPART TYPE MARA-SPART,
        TOT_VOL TYPE MARA-VOLUM,
        PO_HIS TYPE CHAR20,
        TRIP_NO TYPE ZFREIGHT_HEADER-TRIP_NO ,
        VENDOR_CODE TYPE ZFREIGHT_HEADER-VENDOR_CODE,
        VENDOR_NAME TYPE ZFREIGHT_HEADER-VENDOR_NAME,
        TO_LOC TYPE ZFREIGHT_HEADER-TO_LOC,
        VECHILE_NUMBER TYPE ZFREIGHT_HEADER-VECHILE_NUMBER,
        CRDATE TYPE ZFREIGHT_HEADER-CRDATE,
        CRTIME TYPE ZFREIGHT_HEADER-CRTIME,
        LR_NO TYPE ZFREIGHT_ITEM-LR_NO,
    END OF GS_FINAL.

DATA : GT_FINAL TYPE TABLE OF GS_FINAL,
       WA_FINAL TYPE GS_FINAL.

DATA : LV_REPLA TYPE EKPO-WERKS,
       LV_DATE TYPE EKBE-BUDAT,
       LV_MATNR TYPE EKBE-MATNR,
       LV_WERKS TYPE EKBE-WERKS.

DATA : GT_FCAT TYPE SLIS_T_FIELDCAT_ALV,
       IT_SORT TYPE SLIS_T_SORTINFO_ALV,
       WA_FCAT TYPE SLIS_FIELDCAT_ALV,
       WA_SORT TYPE SLIS_SORTINFO_ALV.

DATA : LAYOUT TYPE SLIS_LAYOUT_ALV.

SELECTION-SCREEN : BEGIN OF BLOCK C1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS  :  SO_DATE FOR  LV_DATE obligatory,
                     SO_WERKS FOR LV_WERKS,
                     SO_MATNR FOR LV_MATNR,
                     SO_REPLA FOR LV_REPLA.
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
"BREAK-POINT.
SELECT
        MANDT
        EBELN
        EBELP
        ZEKKN
        VGABE
        GJAHR
        BELNR
        BUZEI
        BEWTP
        BUDAT
        MENGE
        DMBTR
        XBLNR
        MATNR
        WERKS
        CHARG
        BLDAT
            FROM EKBE INTO TABLE GT_EKBE WHERE BUDAT IN SO_DATE AND WERKS IN SO_WERKS AND MATNR IN SO_MATNR and ( BEWTP = 'U' OR BEWTP = 'E' ) AND ( VGABE EQ '6' OR VGABE EQ '1' ) .

 if gt_ekbe is not initial.

SELECT
        MANDT
        EBELN
        EBELP
        MATNR
        BUKRS
        WERKS
        KTMNG
        MENGE
        MEINS FROM EKPO INTO TABLE GT_EKPO FOR ALL ENTRIES IN GT_EKBE WHERE EBELN = GT_EKBE-EBELN AND EBELP = GT_EKBE-EBELP AND WERKS IN SO_REPLA . " AND XOBLR = 'X'.

  SELECT
    EBELN
    BSART
      FROM EKKO INTO TABLE GT_EKKO FOR ALL ENTRIES IN GT_EKBE WHERE EBELN = GT_EKBE-EBELN AND ( BSART EQ 'UB' OR BSART EQ 'ZUB' OR BSART EQ 'ZSTO' ) .

   SELECT
        MANDT
        MBLNR
        MJAHR
        ZEILE
        BWART
        MATNR
        WERKS
        EBELN
        EBELP
        ZEKKN
          FROM MSEG INTO TABLE GT_MSEG FOR ALL ENTRIES IN GT_EKBE WHERE  MBLNR = GT_EKBE-BELNR AND MJAHR = GT_EKBE-GJAHR
                             AND WERKS IN SO_REPLA .
"AND ZEILE = GT_EKBE-BUZEI
    " EBELN = GT_EKBE-EBELN AND EBELP = GT_EKBE-EBELP AND ZEKKN = GT_EKBE-ZEKKN

  SELECT
        MANDT
        MBLNR
        MJAHR
        BLART
        BLDAT
        BUDAT
          FROM MKPF INTO TABLE GT_MKPF FOR ALL ENTRIES IN GT_MSEG WHERE MBLNR = GT_MSEG-MBLNR AND MJAHR = GT_MSEG-MJAHR .

    SELECT
      WERKS
      NAME1 FROM T001W INTO TABLE GT_T001W ."FOR ALL ENTRIES IN GT_EKBE WHERE WERKS = GT_EKBE-WERKS.

   SELECT
      MATNR
      MAKTX FROM MAKT INTO TABLE GT_MAKT FOR ALL ENTRIES IN GT_EKBE WHERE MATNR = GT_EKBE-MATNR .

   SELECT
      MATNR
      VOLUM
      SPART FROM MARA INTO TABLE GT_MARA FOR ALL ENTRIES IN GT_EKBE WHERE MATNR = GT_EKBE-MATNR .

    SELECT
          MANDT
          TRIP_NO
          INVOICE_NO
          COMPANY_CODE
          LR_NO
            FROM ZFREIGHT_ITEM INTO TABLE GT_ZFREIGHT_ITEM FOR ALL ENTRIES IN GT_MSEG WHERE INVOICE_NO = GT_MSEG-MBLNR .

      SELECT
        MANDT
        TRIP_NO
        STATUS
        VENDOR_CODE
        VENDOR_NAME
        FROM_CODE
        FROM_LOC
        TO_LOC
        VECHILE_NUMBER
        CRDATE
        CRTIME
          FROM ZFREIGHT_HEADER INTO TABLE GT_ZFREIGHT_HEADER FOR ALL ENTRIES IN GT_ZFREIGHT_ITEM WHERE TRIP_NO = GT_ZFREIGHT_ITEM-TRIP_NO.

 endif.

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

LOOP AT GT_EKBE INTO WA_EKBE .
          WA_FINAL-BELNR = WA_EKBE-BELNR.
          WA_FINAL-BEWTP = WA_EKBE-BEWTP.
          WA_FINAL-BUDAT = WA_EKBE-BUDAT.
          WA_FINAL-XBLNR = WA_EKBE-XBLNR.
          WA_FINAL-MATNR = WA_EKBE-MATNR .
          WA_FINAL-WERKS = WA_EKBE-WERKS .
          WA_FINAL-CHARG = WA_EKBE-CHARG .
          WA_FINAL-BLDAT = WA_EKBE-BLDAT.
          WA_FINAL-EBELN = WA_EKBE-EBELN.
          WA_FINAL-MENGE = WA_EKBE-MENGE.
          WA_FINAL-DMBTR = WA_EKBE-DMBTR.
          WA_FINAL-EBELP  = WA_EKBE-EBELP.
        IF WA_EKBE-BEWTP EQ 'U'.
           WA_FINAL-PO_HIS = 'Goods issue' .
         ENDIF.
        IF WA_EKBE-BEWTP EQ 'E'.
           WA_FINAL-PO_HIS = 'Goods receipt'.
         ENDIF.

  READ TABLE GT_EKPO INTO WA_EKPO WITH KEY EBELN = WA_FINAL-EBELN EBELP = WA_FINAL-EBELP.
         WA_FINAL-WERKS1 = WA_EKPO-WERKS .
  READ TABLE GT_T001W INTO WA_T001W WITH KEY WERKS = WA_FINAL-WERKS1.
       IF SY-SUBRC EQ 0 .
          WA_FINAL-NAME2 = WA_T001W-NAME1.
       ENDIF.
  READ TABLE GT_T001W INTO WA_T001W WITH KEY WERKS = WA_FINAL-WERKS.
     IF SY-SUBRC EQ 0 .
          WA_FINAL-NAME1 = WA_T001W-NAME1.
     ENDIF.
  READ TABLE GT_MAKT INTO WA_MAKT WITH KEY MATNR = WA_FINAL-MATNR.
    IF SY-SUBRC EQ 0 .
      WA_FINAL-MAKTX = WA_MAKT-MAKTX.
    ENDIF.

READ TABLE GT_MARA INTO WA_MARA WITH KEY MATNR = WA_FINAL-MATNR.
   IF SY-SUBRC EQ 0 .
      WA_FINAL-SPART = WA_MARA-SPART.
      WA_FINAL-TOT_VOL = WA_FINAL-MENGE * WA_MARA-VOLUM .
   ENDIF.

READ TABLE GT_ZFREIGHT_ITEM INTO WA_ZFREIGHT_ITEM WITH KEY INVOICE_NO = WA_EKBE-BELNR .
   IF SY-SUBRC EQ 0 .
     WA_FINAL-LR_NO = WA_ZFREIGHT_ITEM-LR_NO.
    READ TABLE GT_ZFREIGHT_HEADER INTO WA_ZFREIGHT_HEADER WITH KEY TRIP_NO = WA_ZFREIGHT_ITEM-TRIP_NO .
    IF SY-SUBRC EQ 0.
    WA_FINAL-TRIP_NO = WA_ZFREIGHT_HEADER-TRIP_NO .
    WA_FINAL-VENDOR_CODE =  WA_ZFREIGHT_HEADER-VENDOR_CODE .
    WA_FINAL-VENDOR_NAME = WA_ZFREIGHT_HEADER-VENDOR_NAME.
    WA_FINAL-TO_LOC = WA_ZFREIGHT_HEADER-TO_LOC .
    WA_FINAL-VECHILE_NUMBER = WA_ZFREIGHT_HEADER-VECHILE_NUMBER.
    WA_FINAL-CRDATE = WA_ZFREIGHT_HEADER-CRDATE .
    WA_FINAL-CRTIME = WA_ZFREIGHT_HEADER-CRTIME .
  ENDIF.
ENDIF.
SHIFT WA_FINAL-MATNR LEFT DELETING LEADING '0'.
READ TABLE GT_MSEG INTO WA_MSEG WITH KEY EBELN = WA_EKBE-EBELN EBELP = WA_EKBE-EBELP .
READ TABLE GT_EKKO INTO WA_EKKO WITH KEY EBELN = WA_EKBE-EBELN .
IF WA_MSEG IS NOT INITIAL AND WA_EKKO IS NOT INITIAL.
  APPEND WA_FINAL TO GT_FINAL.
  CLEAR: WA_MSEG , WA_EKKO.
ENDIF.
CLEAR WA_FINAL.
ENDLOOP.

"Commented by ram on 4/9
* LOOP AT GT_EKPO INTO WA_EKPO.
*
*   WA_FINAL-WERKS1 = WA_EKPO-WERKS .
*   "MOVE-CORRESPONDING WA_EKPO TO WA_FINAL .
*
* READ TABLE GT_T001W INTO WA_T001W WITH KEY WERKS = WA_FINAL-WERKS1.
*  IF SY-SUBRC EQ 0 .
*    WA_FINAL-NAME2 = WA_T001W-NAME1.
*  ENDIF.
*
*LOOP AT GT_EKBE INTO WA_EKBE WHERE EBELN = WA_EKPO-EBELN AND EBELP = WA_EKPO-EBELP .
*          WA_FINAL-BELNR = WA_EKBE-BELNR.
*          WA_FINAL-BEWTP = WA_EKBE-BEWTP.
*          WA_FINAL-BUDAT = WA_EKBE-BUDAT.
*          WA_FINAL-XBLNR = WA_EKBE-XBLNR.
*          WA_FINAL-MATNR = WA_EKBE-MATNR .
*          WA_FINAL-WERKS = WA_EKBE-WERKS .
*          WA_FINAL-CHARG = WA_EKBE-CHARG .
*          WA_FINAL-BLDAT = WA_EKBE-BLDAT.
*          WA_FINAL-EBELN = WA_EKBE-EBELN.
*          WA_FINAL-MENGE = WA_EKBE-MENGE.
*          WA_FINAL-DMBTR = WA_EKBE-DMBTR.
*          IF WA_EKBE-BEWTP EQ 'U'.
*            WA_FINAL-PO_HIS = 'Goods issue' .
*          ENDIF.
*           IF WA_EKBE-BEWTP EQ 'E'.
*            WA_FINAL-PO_HIS = 'Goods receipt'.
*          ENDIF.
* READ TABLE GT_T001W INTO WA_T001W WITH KEY WERKS = WA_FINAL-WERKS.
*  IF SY-SUBRC EQ 0 .
*    WA_FINAL-NAME1 = WA_T001W-NAME1.
*  ENDIF.
*
* READ TABLE GT_MAKT INTO WA_MAKT WITH KEY MATNR = WA_FINAL-MATNR.
*    IF SY-SUBRC EQ 0 .
*      WA_FINAL-MAKTX = WA_MAKT-MAKTX.
*    ENDIF.
*
*READ TABLE GT_MARA INTO WA_MARA WITH KEY MATNR = WA_FINAL-MATNR.
*   IF SY-SUBRC EQ 0 .
*      WA_FINAL-SPART = WA_MARA-SPART.
*      WA_FINAL-TOT_VOL = WA_FINAL-MENGE * WA_MARA-VOLUM .
*   ENDIF.
*
*READ TABLE GT_ZFREIGHT_ITEM INTO WA_ZFREIGHT_ITEM WITH KEY INVOICE_NO = WA_EKBE-BELNR .
*   IF SY-SUBRC EQ 0 .
*     WA_FINAL-LR_NO = WA_ZFREIGHT_ITEM-LR_NO.
*    READ TABLE GT_ZFREIGHT_HEADER INTO WA_ZFREIGHT_HEADER WITH KEY TRIP_NO = WA_ZFREIGHT_ITEM-TRIP_NO .
*    IF SY-SUBRC EQ 0.
*    WA_FINAL-TRIP_NO = WA_ZFREIGHT_HEADER-TRIP_NO .
*    WA_FINAL-VENDOR_CODE =  WA_ZFREIGHT_HEADER-VENDOR_CODE .
*    WA_FINAL-VENDOR_NAME = WA_ZFREIGHT_HEADER-VENDOR_NAME.
*    WA_FINAL-TO_LOC = WA_ZFREIGHT_HEADER-TO_LOC .
*    WA_FINAL-VECHILE_NUMBER = WA_ZFREIGHT_HEADER-VECHILE_NUMBER.
*    WA_FINAL-CRDATE = WA_ZFREIGHT_HEADER-CRDATE .
*    WA_FINAL-CRTIME = WA_ZFREIGHT_HEADER-CRTIME .
*  ENDIF.
*ENDIF.
*
**LOOP AT GT_MSEG INTO WA_MSEG WHERE MBLNR = WA_EKBE-BELNR AND MJAHR = WA_EKBE-GJAHR AND ZEILE = WA_EKBE-BUZEI AND EBELN = WA_EKBE-EBELN AND EBELP = WA_EKBE-EBELP
**                                                                                                                 AND ZEKKN = WA_EKBE-ZEKKN .
**
**LOOP AT GT_MKPF INTO WA_MKPF WHERE MBLNR = WA_MSEG-MBLNR AND MJAHR = WA_MSEG-MJAHR .
**
**ENDLOOP.
**
** ENDLOOP.
*
*ENDLOOP.
*
*SHIFT WA_FINAL-MATNR LEFT DELETING LEADING '0'.
*APPEND WA_FINAL TO GT_FINAL.
*CLEAR WA_FINAL.
*ENDLOOP.
"commented ended by ram on 4/9
"READ TABLE GT_MSEG INTO WA_MSEG WITH KEY MBLNR = WA_EKBE-BELNR  EBELN = WA_EKPO-EBELN EBELP = WA_EKPO-EBELP .
*  READ TABLE GT_EKBE INTO WA_EKBE WITH KEY EBELN = WA_EKPO-EBELN EBELP = WA_EKPO-EBELP .
*  IF SY-SUBRC EQ 0 .
*  ENDIF.

*LOOP AT GT_EKBE INTO WA_EKBE.
*  MOVE-CORRESPONDING WA_EKBE TO WA_FINAL.
*
*READ TABLE GT_EKPO INTO WA_EKPO WITH KEY EBELN = WA_FINAL-EBELN EBELP = WA_FINAL-EBELP .
*  IF SY-SUBRC EQ 0.
*     WA_FINAL-WERKS1 = WA_EKPO-WERKS.
*  ENDIF.
*
*READ TABLE GT_T001W INTO WA_T001W WITH KEY WERKS = WA_FINAL-WERKS.
*  IF SY-SUBRC EQ 0 .
*    WA_FINAL-NAME1 = WA_T001W-NAME1.
*  ENDIF.
*
* READ TABLE GT_T001W INTO WA_T001W WITH KEY WERKS = WA_EKPO-WERKS.
*  IF SY-SUBRC EQ 0 .
*    WA_FINAL-NAME2 = WA_T001W-NAME1.
*  ENDIF.
*
* READ TABLE GT_MAKT INTO WA_MAKT WITH KEY MATNR = WA_FINAL-MATNR.
*  IF SY-SUBRC EQ 0 .
*    WA_FINAL-MAKTX = WA_MAKT-MAKTX.
*  ENDIF.
*
* APPEND WA_FINAL TO GT_FINAL.
*  CLEAR WA_FINAL.
*ENDLOOP.
*
*DELETE GT_FINAL WHERE WERKS = ' ' .

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

  PERFORM ALV_LAYOUT USING 12 'Sup.Plant' 'NAME1' 'GT_FINAL' ''.
 " PERFORM ALV_LAYOUT USING 1 'Sup.Plant' 'WERKS' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 2 'Posting Date' 'BUDAT' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 3 'Material' 'MATNR' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 4 'Material Number' 'MAKTX' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 5 'Doc.Date' 'BLDAT' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 6 'Batch' 'CHARG' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 7 'Reference' 'XBLNR' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 8 'Puchase Order History' 'PO_HIS' 'GT_FINAL' ''.
  "PERFORM ALV_LAYOUT USING 9 'Rec Plant' 'WERKS1' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 10 'Rec Plant' 'NAME2' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 11 'Mat.Doc' 'BELNR' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 13 'Division' 'SPART' 'GT_FINAL' ''.

  PERFORM ALV_LAYOUT USING 14 'Puchase Order' 'EBELN' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 15 'Qty' 'MENGE' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 16 'Volume' 'TOT_VOL' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 17 'Price' 'DMBTR' 'GT_FINAL' ''.

  PERFORM ALV_LAYOUT USING 18 'Trip Number' 'TRIP_NO' 'GT_FINAL' ''.

  PERFORM ALV_LAYOUT USING 19 'Trip Date' 'CRDATE' 'GT_FINAL' ''.

  PERFORM ALV_LAYOUT USING 20 'Trip Time' 'CRTIME' 'GT_FINAL' ''.

 PERFORM ALV_LAYOUT USING 21 'Vechile No' 'VECHILE_NUMBER' 'GT_FINAL' ''.

 PERFORM ALV_LAYOUT USING 22 'Lr No' 'LR_NO' 'GT_FINAL' ''.

 " PERFORM ALV_LAYOUT USING 19 'V' 'VENDOR_CODE' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 23 'Transporter Name' 'VENDOR_NAME' 'GT_FINAL' ''.

  PERFORM ALV_LAYOUT USING 24 'To City' 'TO_LOC' 'GT_FINAL' ''.


ENDFORM.                    " FIELD_CATLOG

*&---------------------------------------------------------------------*
*&      Form  ALV_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0338   text
*      -->P_0339   text
*      -->P_0340   text
*      -->P_0341   text
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

FORM ALV_CATALOG_HEADER.
  DATA : LIT_HEADER TYPE  SLIS_T_LISTHEADER,
       LS_LINE TYPE SLIS_LISTHEADER.
  CLEAR LS_LINE.
  LS_LINE-TYP  = 'H'.
  LS_LINE-KEY = ' '.
  LS_LINE-INFO = 'Stock Transfer Order Details' .
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
