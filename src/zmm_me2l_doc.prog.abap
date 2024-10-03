*&---------------------------------------------------------------------*
*& Report ZMM_ME2L_DOC
*&*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&Functional                   : Mr.Govindarajan M   & Mr.David        *
*& Developer                   : Mr.Ramachandaran                      *
*& Company                     : Sheenlac Paints Pvt Ltd               *
*& Verified By                 :                                       *
*& Title                       : Purchasing Document Per Vendor        *
*& Report Name                 : ZMM_ME2L_DOC                          *
*& Development Id              : kpabap                                *
*& Related Information         : Purchasing Document Per Vendor        *
*&---------------------------------------------------------------------*

REPORT ZMM_ME2L_DOC.

*TABLES  : EKKO,EKPO,T163,EINA,T134T,LFA1.

TYPES: BEGIN OF ES_EKKO,                                                                           " Table 1
           EBELN TYPE EKKO-EBELN,                                 " P.O NUMBER
           BUKRS TYPE EKKO-BUKRS,                                 " COMPANY CODE
           BSTYP TYPE EKKO-BSTYP,                                 " P.O DOCUMENT CATGORY
           BSART TYPE EKKO-BSART,                                 " P.O DOCUMENT TYPE
           LOEKZ TYPE EKKO-LOEKZ,                                 " P.O DEL INDICATOR
           AEDAT TYPE EKKO-AEDAT,                                 " P.O DATE
           PINCR TYPE EKKO-PINCR,                                 " ITEM NUMBER
           LIFNR TYPE EKKO-LIFNR,                                 " VENDOR CODE
           ZTERM TYPE EKKO-ZTERM,                                 " TERMS OF PAYMENT
           EKORG TYPE EKKO-EKORG,                                 " PURCHASE ORGANIZATION
           EKGRP TYPE EKKO-EKGRP,                                 " PURCHASE GROUP
           BEDAT TYPE EKKO-BEDAT,                                 "DOCUMENT DATE
           FRGKE TYPE EKKO-FRGKE,                                 " P.O RELEASE INDICATOR
           FRGZU TYPE EKKO-FRGZU,                                 " P.O RELEASE STATUS
       END OF ES_EKKO.

DATA: GT_EKKO TYPE TABLE OF ES_EKKO,
     WA_EKKO TYPE ES_EKKO.

TYPES: BEGIN OF ES_EKPO,                                                                           " Table 2
           EBELN TYPE EKPO-EBELN,                                 " PURCHASING ORDER
           EBELP TYPE EKPO-EBELP,                                 " Item Number
           LOEKZ TYPE EKPO-LOEKZ,                                                                    "ADDED BY RAM ON 29/09/2014
           TXZ01 TYPE EKPO-TXZ01,                                 " MATERIAL DESCRIPTION
           MATNR TYPE EKPO-MATNR,                                 " MATERIAL CODE
           BUKRS TYPE EKPO-BUKRS,                                 " COMPANY CODE
           WERKS TYPE EKPO-WERKS,                                 " PLANT
           LGORT TYPE EKPO-LGORT,                                 " STORAGE LOCATION
           MATKL TYPE EKPO-MATKL,                                 " MATERIAL GROUP
           KTMNG TYPE EKPO-KTMNG,                                 " TARGET QUANTITY
           MENGE TYPE EKPO-MENGE,                                 " P.O QUANTITY
           MEINS TYPE EKPO-MEINS,                                 " PURCHASE ORDER UNIT OF MEASURE
           NETPR TYPE EKPO-NETPR,                                 " UNIT PRICE
           NETWR TYPE EKPO-NETWR,                                 " P.O VALUE
           WEPOS TYPE EKPO-WEPOS,                                  "GOODS RECEIPT
           MTART TYPE EKPO-MTART,                                 " MATERIAL TYPE
       END OF ES_EKPO.

DATA: GT_EKPO TYPE TABLE OF ES_EKPO,
     WA_EKPO TYPE ES_EKPO.

TYPES: BEGIN OF ES_EINA,                                                                       " Table 3
          MATNR TYPE EINA-MATNR,                                 " MATERIAL CODE
          LMEIN TYPE EINA-LMEIN,                                 " Base Unit Of Measure
       END OF ES_EINA.

DATA: GT_EINA TYPE TABLE OF ES_EINA,
     WA_EINA TYPE ES_EINA.

TYPES: BEGIN OF ES_LFA1,                                                                      " TABLE 4
           LIFNR TYPE LFA1-LIFNR,                                " VENDOR CODE
           NAME1 TYPE LFA1-NAME1,                               " VENDOR NAME
       END OF ES_LFA1.

DATA: GT_LFA1 TYPE TABLE OF ES_LFA1,
      WA_LFA1 TYPE ES_LFA1.

TYPES: BEGIN OF ES_T134T,                                                                     " TABLE 5
          MTART TYPE T134T-MTART,                                 " MATERIAL TYPE
          MTBEZ TYPE T134T-MTBEZ,                                 " MATERIAL TYPE DESCRIPTION
      END OF ES_T134T.

DATA: GT_T134T TYPE TABLE OF ES_T134T,
      WA_T134T TYPE ES_T134T.

TYPES: BEGIN OF ES_T163,                                                                     "TABLE 6
         PSTYP TYPE T163-PSTYP,                                 "ITEM CATAGORY
         WEPOS TYPE T163-WEPOS,                                  "GOODS RECEIPT
       END OF ES_T163.

DATA: GT_T163 TYPE TABLE OF ES_T163,
      WA_T163 TYPE ES_T163.

TYPES: BEGIN OF ES_MARA,                                                                     "TABLE 7
        MATNR TYPE MARA-MATNR,                   " MATERIAL
        MTART TYPE MARA-MTART,
        VOLUM TYPE MARA-VOLUM,                   " VOLUME
        VOLEH TYPE MARA-VOLEH,                   "VOLUM UNIT
        SPART TYPE MARA-SPART,                   " DIVISION
        END OF ES_MARA.

DATA: GT_MARA TYPE TABLE OF ES_MARA,
      WA_MARA TYPE ES_MARA.

TYPES:BEGIN OF ES_T001W,                                                                   " TABLE 8
       WERKS TYPE T001W-WERKS,
       PNAME TYPE T001W-NAME1,                    " PLANT NAME
      END OF ES_T001W.

DATA: GT_T001W TYPE TABLE OF ES_T001W,
      WA_T001W TYPE ES_T001W.

TYPES:BEGIN OF ES_EKBE,                                                                      " TABLE 9
       EBELN TYPE EKBE-EBELN,
       EBELP TYPE EKBE-EBELP,
       MATNR TYPE EKBE-MATNR,
       MENGE TYPE EKBE-MENGE,
       LFBNR TYPE EKBE-LFBNR,
       BEWTP TYPE EKBE-BEWTP,                  "MODIFIED ON 15/09/2014 BY RAM
       BWART TYPE EKBE-BWART,
       SHKZG TYPE EKBE-SHKZG,
      END OF ES_EKBE.

DATA: GT_EKBE TYPE TABLE OF ES_EKBE,
      WA_EKBE TYPE ES_EKBE.

TYPES:BEGIN OF ES_RSEG,                                                                     " TABLE 10
       EBELN TYPE RSEG-EBELN,
       IMENGE TYPE RSEG-MENGE,
       LFBNR TYPE RSEG-LFBNR,
      END OF ES_RSEG.

DATA: GT_RSEG TYPE TABLE OF ES_RSEG,
      WA_RSEG TYPE ES_RSEG.

TYPES:BEGIN OF ES_MARD,                                                                      " TABLE 10
       MATNR TYPE MARD-MATNR,
       WERKS TYPE MARD-WERKS,
       LABST TYPE MARD-LABST,
      END OF ES_MARD.

DATA: GT_MARD TYPE TABLE OF ES_MARD,
      WA_MARD TYPE ES_MARD.

TYPES:BEGIN OF ES_EKET,                                                                     " TABLE 11
         EBELN TYPE EKET-EBELN,
         EBELP TYPE EKET-EBELP,
       WEMNG TYPE EKET-WEMNG,
      END OF ES_EKET.

DATA: GT_EKET TYPE TABLE OF ES_EKET,
      WA_EKET TYPE ES_EKET.

*TYPES:BEGIN OF ES_MARC,                                                                     " TABLE 12
*       MATNR TYPE MARC-MATNR,
*       TRAME TYPE MARC-TRAME,
*      END OF ES_MARC.
*
*DATA: GT_MARC TYPE TABLE OF ES_MARC,
*      WA_MARC TYPE ES_MARC.

TYPES : BEGIN OF GS_FINAL,                                                                             " FINAL
               EBELN TYPE EKKO-EBELN,                                 " P.O NUMBER
               BUKRS TYPE EKKO-BUKRS,                                 " COMPANY CODE
               BSTYP TYPE EKKO-BSTYP,                                 " P.O DOCUMENT CATGORY
               BSART TYPE EKKO-BSART,                                 " P.O DOCUMENT TYPE
               LOEKZ TYPE EKKO-LOEKZ,                                 " P.O DEL INDICATOR
               AEDAT TYPE EKKO-AEDAT,                                 " P.O DATE
               PINCR TYPE EKKO-PINCR,                                 " ITEM NUMBER
               LIFNR TYPE EKKO-LIFNR,                                 " VENDOR CODE
               ZTERM TYPE EKKO-ZTERM,                                 " TERMS OF PAYMENT
               EKORG TYPE EKKO-EKORG,                                 " PURCHASE ORGANIZATION
               EKGRP TYPE EKKO-EKGRP,                                 " PURCHASE GROUP
               BEDAT TYPE EKKO-BEDAT,                                 " DOCUMENT DATE
               FRGKE TYPE EKKO-FRGKE,                                 " P.O RELEASE INDICATOR
               FRGZU TYPE EKKO-FRGZU,                                 " P.O RELEASE STATUS
               EBELP TYPE EKPO-EBELP,                                 " Item Number
               TXZ01 TYPE EKPO-TXZ01,                                 " MATERIAL DESCRIPTION
               MATNR TYPE EKPO-MATNR,                                 " MATERIAL CODE
               WERKS TYPE EKPO-WERKS,                                 " PLANT
               LGORT TYPE EKPO-LGORT,                                 " STORAGE LOCATION
               MATKL TYPE EKPO-MATKL,                                 " MATERIAL GROUP
               KTMNG TYPE EKPO-KTMNG,                                 " TARGET QUANTITY
               MENGE TYPE EKPO-MENGE,                                 " P.O QUANTITY
               MEINS TYPE EKPO-MEINS,                                 " PURCHASE ORDER UNIT OF MEASURE
               NETPR TYPE EKPO-NETPR,                                 " UNIT PRICE
               NETWR TYPE EKPO-NETWR,                                 " P.O VALUE
               WEPOS TYPE EKPO-WEPOS,                                 "GOODS RECEIPT
               MTART TYPE EKPO-MTART,                                 " MATERIAL TYPE
               LMEIN TYPE EINA-LMEIN,                                 " Base Unit Of Measure
               NAME1 TYPE LFA1-NAME1,                                 " VENDOR NAME
               MTBEZ TYPE T134T-MTBEZ,                                " MATERIAL TYPE DESCRIPTION
               PSTYP TYPE T163-PSTYP,                                 "ITEM CATAGORY
               VOLUM TYPE MARA-VOLUM,                                 " VOLUME
               VOLEH TYPE MARA-VOLEH,                                 "VOLUM UNIT
               SPART TYPE MARA-SPART,                                 " DIVISION
               PNAME TYPE T001W-NAME1,
               RMENGE TYPE EKBE-MENGE,
               LFBNR TYPE EKBE-LFBNR,
               BEWTP TYPE EKBE-BEWTP,                  "MODIFIED ON 15/09/2014 BY RAM
               IMENGE TYPE RSEG-MENGE,
               MATNR1 TYPE MARD-MATNR,
               LABST TYPE MARD-LABST,
               WEMNG TYPE EKET-WEMNG,                               " DELIVERED QUANTITY
               " TRAME TYPE MARC-TRAME,
               QTY_LTR TYPE P DECIMALS 3,
               BQTY TYPE P DECIMALS 3,                            " To Be Delivered Quantity
               POQTY_LTR TYPE P DECIMALS 3,                       " Delivered Quantity
               INV_QTY  TYPE P DECIMALS 3,                        " To Be INVOICE
               DQTY_LTR  TYPE P DECIMALS 3,                       " To Be Delivered Quantity IN LITER
               INVQTY_LTR  TYPE P DECIMALS 3,                     " To Be INVOICE IN LITER
               RQTY_LTR  TYPE P DECIMALS 3,                       " To Be INVOICE IN LITER
               REC_PR  TYPE P DECIMALS 3,                         " To Be INVOICE IN LITER
               DEL_PR  TYPE P DECIMALS 3,                         " To Be INVOICE IN LITER
               TSTQTY_LTR TYPE P DECIMALS 3,
               TST_PR  TYPE P DECIMALS 3,
       END OF GS_FINAL.

DATA : GT_FINAL TYPE TABLE OF GS_FINAL,
         WA_FINAL TYPE GS_FINAL.

DATA : GT_FCAT TYPE SLIS_T_FIELDCAT_ALV,
       IT_SORT TYPE SLIS_T_SORTINFO_ALV,
       WA_FCAT TYPE SLIS_FIELDCAT_ALV,
       WA_SORT TYPE SLIS_SORTINFO_ALV.

DATA :   LV_NAME1 TYPE LFA1-NAME1,
         LV_EKORG TYPE EKKO-EKORG,
         LV_BUKRS TYPE EKKO-BUKRS,
         LV_WERKS TYPE EKPO-WERKS,
         LV_SPART TYPE MARA-SPART,
         LV_EKGRP TYPE EKKO-EKGRP,
         LV_BSART TYPE EKKO-BSART,
         LV_BSTYP TYPE EKKO-BSTYP,
         LV_LOEKZ TYPE EKKO-LOEKZ,
         LV_TXZ01 TYPE EKPO-TXZ01,
         LV_MATKL TYPE EKPO-MATKL,
         LV_BEDAT TYPE EKKO-BEDAT,
         LV_EBELN TYPE EKKO-EBELN,
         LV_LIFNR TYPE EKKO-LIFNR.


DATA : LAYOUT TYPE SLIS_LAYOUT_ALV.

SELECTION-SCREEN : BEGIN OF BLOCK C1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS  : SO_BUKRS FOR LV_BUKRS,
                  SO_LIFNR FOR LV_LIFNR,
                  SO_EBELN FOR LV_EBELN,
                  SO_BEDAT FOR LV_BEDAT,
                  SO_EKORG FOR LV_EKORG,
                  SO_BSART FOR LV_BSART,
                  SO_EKGRP FOR LV_EKGRP,
                  SO_WERKS FOR LV_WERKS,
                  SO_SPART FOR LV_SPART,
                  SO_BSTYP FOR LV_BSTYP,
                  SO_LOEKZ FOR LV_LOEKZ,
                  SO_TXZ01 FOR LV_TXZ01,
                  SO_MATKL FOR LV_MATKL,
                  SO_NAME1 FOR LV_NAME1 NO-DISPLAY.
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
FORM GET_DATA .

  SELECT
      EBELN
      BUKRS
      BSTYP
      BSART
      LOEKZ
      AEDAT
      PINCR
      LIFNR
      ZTERM
      EKORG
      EKGRP
      BEDAT
      FRGKE
      FRGZU
       FROM EKKO INTO TABLE GT_EKKO WHERE BUKRS IN SO_BUKRS
     AND EKORG IN SO_EKORG AND BSART IN SO_BSART AND EBELN IN SO_EBELN
     AND LIFNR IN SO_LIFNR AND EKGRP IN SO_EKGRP AND BSART IN SO_BSART AND BSTYP IN SO_BSTYP AND BEDAT IN SO_BEDAT AND BSART <> 'ZSR' . " MODIFIED BY RAM ON 22/09/2014

  IF GT_EKKO[] IS NOT INITIAL.
    SELECT
       EBELN
       EBELP
       LOEKZ    "ADDED BY RAM ON 29/09/2014
       TXZ01
       MATNR
       BUKRS
       WERKS
       LGORT
       MATKL
       KTMNG
       MENGE
       MEINS
       NETPR
       NETWR
       WEPOS
       MTART
          FROM EKPO INTO TABLE GT_EKPO FOR ALL ENTRIES IN GT_EKKO
    WHERE EBELN  = GT_EKKO-EBELN  AND WERKS IN SO_WERKS AND MATKL IN SO_MATKL AND TXZ01 IN SO_TXZ01 AND LOEKZ <> 'L'.

    SELECT
   LIFNR
   NAME1
     FROM LFA1 INTO TABLE GT_LFA1 FOR ALL ENTRIES IN GT_EKKO
WHERE LIFNR  = GT_EKKO-LIFNR .

  ENDIF.

  SELECT
    MATNR
    LMEIN
       FROM EINA INTO TABLE GT_EINA FOR ALL ENTRIES IN GT_EKPO
 WHERE MATNR  = GT_EKPO-MATNR.

* SELECT
*    MATNR
*    TRAME
*       FROM MARC INTO TABLE GT_MARC FOR ALL ENTRIES IN GT_MARA
* WHERE MATNR  = GT_MARA-MATNR.

  SELECT
    MTART
    MTBEZ
      FROM T134T INTO TABLE GT_T134T FOR ALL ENTRIES IN GT_EKPO
WHERE MTART  = GT_EKPO-MTART.

  SELECT
    PSTYP
    WEPOS
      FROM T163 INTO TABLE GT_T163 FOR ALL ENTRIES IN GT_EKPO
WHERE  WEPOS = GT_EKPO-WEPOS.

  SELECT
 MATNR
 MTART
 VOLUM
 VOLEH
 SPART
    FROM MARA INTO TABLE GT_MARA FOR ALL ENTRIES IN GT_EKPO
WHERE  MATNR = GT_EKPO-MATNR AND SPART IN SO_SPART .

  SELECT
  WERKS
  NAME1
    FROM T001W INTO TABLE GT_T001W FOR ALL ENTRIES IN GT_EKPO WHERE WERKS = GT_EKPO-WERKS.

  IF GT_EKPO[] IS NOT INITIAL.
    SELECT
      EBELN
      EBELP
       WEMNG
       FROM EKET INTO TABLE GT_EKET FOR ALL ENTRIES IN GT_EKPO WHERE EBELN = GT_EKPO-EBELN .

    SELECT
      EBELN
      EBELP
      MATNR
      MENGE
      LFBNR
      BEWTP
      BWART
      SHKZG
       FROM EKBE INTO TABLE GT_EKBE FOR ALL ENTRIES IN GT_EKPO WHERE EBELN = GT_EKPO-EBELN  .

    SELECT
      EBELN
      MENGE
      LFBNR
       FROM RSEG INTO TABLE GT_RSEG FOR ALL ENTRIES IN GT_EKBE WHERE EBELN = GT_EKBE-EBELN AND LFBNR = GT_EKBE-LFBNR.

    SELECT
        MATNR
        WERKS
        LABST
         FROM MARD INTO TABLE GT_MARD FOR ALL ENTRIES IN GT_EKPO WHERE WERKS  = '1100'  AND MATNR = GT_EKPO-MATNR .

  ENDIF.

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

  LOOP AT GT_EKPO INTO WA_EKPO.
    MOVE-CORRESPONDING WA_EKPO TO WA_FINAL.
    READ TABLE GT_EKKO INTO WA_EKKO WITH KEY  EBELN  =  WA_EKPO-EBELN  .
    WA_FINAL-BUKRS = WA_EKKO-BUKRS.
    WA_FINAL-PINCR = WA_EKKO-PINCR.
    WA_FINAL-EKORG = WA_EKKO-EKORG.
    WA_FINAL-EKGRP = WA_EKKO-EKGRP.
    WA_FINAL-BSART = WA_EKKO-BSART.
    WA_FINAL-BSTYP = WA_EKKO-BSTYP.
  "  WA_FINAL-LOEKZ = WA_EKKO-LOEKZ.
*             WA_FINAL-EBELN = WA_EKKO-EBELN.
    WA_FINAL-AEDAT = WA_EKKO-AEDAT.
    WA_FINAL-ZTERM = WA_EKKO-ZTERM.
    WA_FINAL-FRGKE = WA_EKKO-FRGKE.
    WA_FINAL-FRGZU = WA_EKKO-FRGZU.
    WA_FINAL-LIFNR = WA_EKKO-LIFNR.
    WA_FINAL-BEDAT = WA_EKKO-BEDAT.
    READ TABLE GT_EINA INTO WA_EINA WITH KEY  MATNR  =  WA_EKPO-MATNR .
    WA_FINAL-LMEIN = WA_EINA-LMEIN.
    READ TABLE GT_T134T INTO WA_T134T WITH KEY  MTART  =  WA_EKPO-MTART .
    WA_FINAL-MTBEZ = WA_T134T-MTBEZ.
    READ TABLE GT_T163 INTO WA_T163 WITH KEY  WEPOS =  WA_EKPO-WEPOS .
    WA_FINAL-PSTYP = WA_T163-PSTYP.
    READ TABLE GT_T001W INTO WA_T001W WITH KEY  WERKS =  WA_EKPO-WERKS .
    WA_FINAL-PNAME = WA_T001W-PNAME.
    LOOP AT  GT_EKET INTO WA_EKET WHERE  EBELN =  WA_EKPO-EBELN  AND EBELP = WA_EKPO-EBELP.
      WA_FINAL-WEMNG = WA_FINAL-WEMNG + WA_EKET-WEMNG.
    ENDLOOP.
    LOOP AT  GT_EKBE INTO WA_EKBE WHERE EBELN =  WA_EKPO-EBELN AND EBELP = WA_EKPO-EBELP AND BEWTP = 'Q' .
      IF SY-SUBRC = 0.
        WA_FINAL-IMENGE  =  WA_FINAL-IMENGE  + WA_EKBE-MENGE.
      ENDIF.
    ENDLOOP.
    IF WA_MARA-MTART = 'FERT' OR WA_MARA-MTART = 'HAWA' .
      WA_FINAL-POQTY_LTR = WA_MARA-VOLUM * WA_FINAL-RMENGE.
    ENDIF.
    READ TABLE GT_MARD INTO WA_MARD WITH KEY MATNR = WA_EKPO-MATNR WERKS = '1100' .
    WA_FINAL-LABST = WA_MARD-LABST.
    READ TABLE GT_MARA INTO WA_MARA WITH KEY  MATNR =  WA_EKPO-MATNR .
    WA_FINAL-SPART = WA_MARA-SPART.
    WA_FINAL-VOLUM = WA_MARA-VOLUM.
    WA_FINAL-VOLEH = WA_MARA-VOLEH.
    WA_FINAL-QTY_LTR = WA_EKPO-MENGE.
    IF WA_MARA-MTART = 'FERT' OR WA_MARA-MTART = 'HAWA' .
      WA_FINAL-QTY_LTR = WA_MARA-VOLUM * WA_EKPO-MENGE.
    ENDIF.
*       READ TABLE GT_MARC INTO WA_MARC WITH KEY  MATNR =  WA_MARA-MATNR .              " ADDED BY RAM ON 24/09/2014
*              WA_FINAL-TRAME = WA_MARC-TRAME.
    SHIFT WA_FINAL-MATNR LEFT DELETING LEADING '0'.
    APPEND WA_FINAL TO GT_FINAL.
    CLEAR WA_FINAL.
  ENDLOOP.
  LOOP AT GT_FINAL INTO WA_FINAL.
    IF WA_FINAL-LIFNR IS NOT INITIAL.
      READ TABLE GT_LFA1 INTO WA_LFA1 WITH KEY  LIFNR  =  WA_FINAL-LIFNR .
      WA_FINAL-NAME1 = WA_LFA1-NAME1.
      WA_FINAL-RMENGE = WA_FINAL-MENGE - WA_FINAL-WEMNG .
      MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING NAME1 RMENGE  .
      CLEAR WA_FINAL.
    ENDIF.
  ENDLOOP.
  LOOP AT GT_FINAL INTO WA_FINAL.
    WA_FINAL-REC_PR = WA_FINAL-WEMNG * WA_FINAL-NETPR .
    WA_FINAL-BQTY  = WA_FINAL-MENGE - WA_FINAL-WEMNG.
    WA_FINAL-DQTY_LTR = WA_FINAL-VOLUM * WA_FINAL-RMENGE.
    WA_FINAL-DEL_PR =  WA_FINAL-BQTY * WA_FINAL-NETPR.
    " WA_FINAL-TST_PR =  WA_FINAL-TRAME * WA_FINAL-NETPR.
    MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING REC_PR BQTY DQTY_LTR DEL_PR .
    CLEAR WA_FINAL.
  ENDLOOP.
  LOOP AT GT_FINAL INTO WA_FINAL.
    WA_FINAL-RQTY_LTR = WA_FINAL-VOLUM * WA_FINAL-WEMNG.  " CHANGED BY RAM 18/09/2014
    WA_FINAL-DQTY_LTR = WA_FINAL-VOLUM * WA_FINAL-BQTY.
    WA_FINAL-INV_QTY =  WA_FINAL-BQTY - WA_FINAL-IMENGE  .
    WA_FINAL-INVQTY_LTR = WA_FINAL-INV_QTY * WA_FINAL-VOLUM.
    " WA_FINAL-TSTQTY_LTR = WA_FINAL-VOLUM * WA_FINAL-TRAME.
    MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING INV_QTY RQTY_LTR INVQTY_LTR DQTY_LTR .
    CLEAR WA_FINAL.
  ENDLOOP .
*SORT GT_FINAL BY NAME1 EBELN.
  IF  SO_SPART IS NOT INITIAL.
    DELETE GT_FINAL WHERE SPART EQ ' '. "Hided By Govind on 20.08.2014
  ENDIF.
ENDFORM.                    " READ_DATA

*&---------------------------------------------------------------------*
*&      Form  FIELD_CATLOG
*&---------------------------------------------------------------------*
FORM FIELD_CATLOG .

  PERFORM ALV_LAYOUT USING 1 'Vendor Name' 'NAME1' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 2 'Plant' 'WERKS' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 3 'Plant Name' 'PNAME' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 4 'Division' 'SPART' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 5 'Vendor Code' 'LIFNR' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 6 'Purch Document' 'EBELN' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 7 'Item' 'EBELP' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 8 'Type' 'BSART' 'GT_FINAL' ''.

  PERFORM ALV_LAYOUT USING 10 'Category' 'BSTYP' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 11 'Purch Group' 'EKGRP' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 12 'Purchase Org' 'EKORG' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 13 'Date' 'BEDAT' 'GT_FINAL' ''.

  PERFORM ALV_LAYOUT USING 15 'Material' 'MATNR' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 16 'Short Text' 'TXZ01' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 17 'Material Group' 'MATKL' 'GT_FINAL' ''.
 " PERFORM ALV_LAYOUT USING 18 'Deletion Ind' 'LOEKZ' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 19 'Release Ind' 'FRGKE' 'GT_FINAL' ''.

  PERFORM ALV_LAYOUT USING 22 'Relase Status' 'FRGZU' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 23 'SLoc' 'LGORT' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 24 'WHC.Stock Qty' 'LABST' 'GT_FINAL' 'X'.
  PERFORM ALV_LAYOUT USING 25 'P.O Qty In Cases' 'MENGE' 'GT_FINAL' 'X'.
  PERFORM ALV_LAYOUT USING 26 'P.O Qty In Ltrs' 'QTY_LTR' 'GT_FINAL' 'X'.
  PERFORM ALV_LAYOUT USING 27 'Unit Price' 'NETPR' 'GT_FINAL' 'X'.

  PERFORM ALV_LAYOUT USING 29 'Net Price' 'NETWR' 'GT_FINAL' 'X'.
  PERFORM ALV_LAYOUT USING 30 'Order Unit' 'MEINS' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 31 'Terms Of Payment' 'ZTERM' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 32 'Material Type' 'MTART' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 33 'Volume' 'VOLUM' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 34 'Volume Unit' 'VOLEH' 'GT_FINAL' ''.

  PERFORM ALV_LAYOUT USING 37 'Target Quantity' 'KTMNG' 'GT_FINAL' 'X'.
  PERFORM ALV_LAYOUT USING 38 'Delivered Qty In Cases' 'WEMNG' 'GT_FINAL' 'X'.
  PERFORM ALV_LAYOUT USING 39 'Delivered Qty In Liter' 'RQTY_LTR' 'GT_FINAL' 'X'.
  PERFORM ALV_LAYOUT USING 40 'Delivered Price' 'REC_PR' 'GT_FINAL' 'X'.

  "  PERFORM ALV_LAYOUT USING 41 'Transit Qty In Cases' 'TRAME' 'GT_FINAL' 'X'.
  " PERFORM ALV_LAYOUT USING 42 'Transit Qty In Ltr' 'TSTQTY_LTR' 'GT_FINAL' 'X'.
  "  PERFORM ALV_LAYOUT USING 43 'Transit Price' 'TST_PR' 'GT_FINAL' 'X'.

  PERFORM ALV_LAYOUT USING 46 'To Be Del Qty' 'BQTY' 'GT_FINAL' 'X'.
  PERFORM ALV_LAYOUT USING 47 'To Be Del Qty in Ltr' 'DQTY_LTR' 'GT_FINAL' 'X'.

  PERFORM ALV_LAYOUT USING  48 'To Be Del Price' 'DEL_PR' 'GT_FINAL' 'X'.

  PERFORM ALV_LAYOUT USING 49 'Invoice Done' 'IMENGE' 'GT_FINAL' 'X'.

  PERFORM ALV_LAYOUT USING 52 'To Be Invoice' 'INV_QTY' 'GT_FINAL' 'X'.
  PERFORM ALV_LAYOUT USING 53 'To Be Invoice in Ltr' 'INVQTY_LTR' 'GT_FINAL' 'X'.

  WA_SORT-FIELDNAME = 'LIFNR'.
  WA_SORT-UP = 'X'.
  APPEND WA_SORT TO IT_SORT.
  CLEAR WA_SORT.

ENDFORM.                    " FIELD_CATLOG

*&---------------------------------------------------------------------*
*&      Form  ALV_LAYOUT
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
FORM ALV_DISPLAY .

   LAYOUT-COLWIDTH_OPTIMIZE = 'X'.                                  "Added by S.Savariar as on 20/10/2014.
   LAYOUT-ZEBRA = 'X'.


  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
   EXPORTING
*     I_INTERFACE_CHECK              = ' '
*     I_BYPASSING_BUFFER             =
*     I_BUFFER_ACTIVE                = ' '
     I_CALLBACK_PROGRAM             = SY-REPID
*     I_CALLBACK_PF_STATUS_SET       = ' '
*     I_CALLBACK_USER_COMMAND        = ' '
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
