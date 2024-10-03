

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

REPORT ZMM_ME2L_DOC1.

*TABLES  : EKKO,EKPO,T163,EINA,T134T,LFA1.

TYPES: BEGIN OF ES_EKKO,                                                                           " Table 1
           EBELN TYPE EKKO-EBELN,                                 " P.O NUMBER
           BUKRS TYPE EKKO-BUKRS,                                 " COMPANY CODE
           BSTYP TYPE EKKO-BSTYP,                                 " P.O DOCUMENT CATGORY
           VERKF TYPE EKKO-VERKF,                                 " Customer Refrence   " Added By Govind On 30-04-2015
           UNSEZ TYPE EKKO-UNSEZ,                                 " Sales Requirment Plan
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
           RESWK TYPE EKKO-RESWK,                                  "SUPPLING PLANT


           KNUMV TYPE EKKO-KNUMV,                          "ADD BY MANI ON 03.10.15

       END OF ES_EKKO.

DATA: GT_EKKO TYPE TABLE OF ES_EKKO,
     WA_EKKO TYPE ES_EKKO.

TYPES: BEGIN OF ES_EKPO,                                                                           " Table 2
           EBELN TYPE EKPO-EBELN,                                 " PURCHASING ORDER
           EBELP TYPE EKPO-EBELP,                                 " Item Number
           LOEKZ TYPE EKPO-LOEKZ,                                                                    "ADDED BY RAM ON 29/09/2014
           AEDAT TYPE EKPO-AEDAT,
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
           ELIKZ TYPE EKPO-ELIKZ,
           WEPOS TYPE EKPO-WEPOS,                                  "GOODS RECEIPT
           MTART TYPE EKPO-MTART,                                 " MATERIAL TYPE
           PRDAT TYPE EKPO-PRDAT,                                   " PRICE DATE
           BSTYP TYPE EKPO-BSTYP,


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

DATA: GT_T001W1 TYPE TABLE OF ES_T001W,
      WA_T001W1 TYPE ES_T001W.

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

DATA: GT_MARD1 TYPE TABLE OF ES_MARD,
      WA_MARD1 TYPE ES_MARD.

DATA: GT_MARD2 TYPE TABLE OF ES_MARD,
      WA_MARD2 TYPE ES_MARD.

TYPES:BEGIN OF ES_EKET,
                                                                    " TABLE 11
         EBELN TYPE EKET-EBELN,
         EBELP TYPE EKET-EBELP,
         EINDT TYPE EKET-EINDT,             " Added by savariar as on 30/04/2015.
         WEMNG TYPE EKET-WEMNG,

      END OF ES_EKET.

DATA: GT_EKET TYPE TABLE OF ES_EKET,
      WA_EKET TYPE ES_EKET.

TYPES:BEGIN OF ES_MSEG,
         MBLNR TYPE MSEG-MBLNR,                                                                     " ADDED BY RAM ON 27/11/2014
         EBELN TYPE MSEG-EBELN,
         EBELP TYPE MSEG-EBELP,
         MATNR TYPE MSEG-MATNR,
         ZEILE TYPE MSEG-ZEILE,
         BWART TYPE MSEG-BWART,
         XAUTO TYPE MSEG-XAUTO,
         WERKS TYPE MSEG-WERKS,
         TMENGE TYPE MSEG-MENGE,
         BUDAT_MKPF TYPE MSEG-BUDAT_MKPF,
         SMBLN TYPE MSEG-SMBLN,
      END OF ES_MSEG.

DATA: GT_MSEG TYPE TABLE OF ES_MSEG,
      WA_MSEG TYPE ES_MSEG.

TYPES:BEGIN OF ES_MSEG1,
         MBLNR TYPE MSEG-MBLNR,                                                                     " ADDED BY RAM ON 27/11/2014
         EBELN TYPE MSEG-EBELN,
         EBELP TYPE MSEG-EBELP,
         MATNR TYPE MSEG-MATNR,
         ZEILE TYPE MSEG-ZEILE,
         BWART TYPE MSEG-BWART,
         XAUTO TYPE MSEG-XAUTO,
         WERKS TYPE MSEG-WERKS,
         TMENGE TYPE MSEG-MENGE,
         BUDAT_MKPF TYPE MSEG-BUDAT_MKPF,
      END OF ES_MSEG1.

DATA: GT_MSEG1 TYPE TABLE OF ES_MSEG,
      WA_MSEG1 TYPE ES_MSEG.

DATA: GT_MSEG2 TYPE TABLE OF ES_MSEG,
      WA_MSEG2 TYPE ES_MSEG.

TYPES:BEGIN OF ES_MVKE,                                                                     "ADDE BY RAM ON 28/11/2014
       MATNR TYPE MVKE-MATNR,
       MVGR1 TYPE MVKE-MVGR1,
      END OF ES_MVKE.

DATA: GT_MVKE TYPE TABLE OF ES_MVKE,
      WA_MVKE TYPE ES_MVKE.

TYPES:BEGIN OF ES_TVM1T,                                                                     "ADDE BY RAM ON 28/11/2014
       MVGR1 TYPE TVM1T-MVGR1,
       BEZEI TYPE TVM1T-BEZEI,
     END OF ES_TVM1T.

DATA: GT_TVM1T TYPE TABLE OF ES_TVM1T,
      WA_TVM1T TYPE ES_TVM1T.

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
               VERKF TYPE EKKO-VERKF,                                 " Customer Refrence  " Added By Govind On 30-04-2015
               UNSEZ TYPE EKKO-UNSEZ,                                 " Sales Requirment Plan
               BSART TYPE EKKO-BSART,                                 " P.O DOCUMENT TYPE
               LOEKZ TYPE EKKO-LOEKZ,                                 " P.O DEL INDICATOR
               AEDAT TYPE EKKO-AEDAT,                                 " P.O DATE
               PINCR TYPE EKKO-PINCR,                                 " ITEM interval
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
               RESWK TYPE EKKO-RESWK,
               RMENGE TYPE EKBE-MENGE,
               LFBNR TYPE EKBE-LFBNR,
               BEWTP TYPE EKBE-BEWTP,                             " MODIFIED ON 15/09/2014 BY RAM
               IMENGE TYPE RSEG-MENGE,
               MATNR1 TYPE MARD-MATNR,
               LABST TYPE MARD-LABST,
               LABST1 TYPE MARD-LABST,
               LABST2 TYPE MARD-LABST,
               WEMNG TYPE EKET-WEMNG,                               " DELIVERED QUANTITY
               EINDT TYPE EKET-EINDT,
               MBLNR TYPE MSEG-MBLNR,                               " Added by savariar as on 04/02/2015
               SMBLN TYPE MSEG-SMBLN,                               " Added by savariar as on 04/02/2015

               TMENGE TYPE MSEG-MENGE,                                                      "ADDED BY RAM ON 27/11/2014
               TMENGE1 TYPE MSEG-MENGE,
               TMENGE2 TYPE MSEG-MENGE,
               TMENGE3 TYPE MSEG-MENGE,
               TMENGE4 TYPE MSEG-MENGE,
               ZEILE TYPE MSEG-ZEILE,
               XAUTO TYPE MSEG-XAUTO,
               BUDAT_MKPF TYPE SY-DATUM,
               MVGR1 TYPE MVKE-MVGR1,
               BEZEI TYPE TVM1T-BEZEI,
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
               TQTY_LTR TYPE P DECIMALS 3,
               TR_PR TYPE P DECIMALS 3,
               LABSTT TYPE P DECIMALS 3,                 " ADDED BY RAM ON 14/11/2014
               LABST1T TYPE P DECIMALS 3,                 " ADDED BY RAM ON 14/11/2014
               LABST2T TYPE P DECIMALS 3,                 " ADDED BY RAM ON 14/11/2014
               TR_DAYS TYPE I,

               COUNT1 TYPE I,
               COUNT2 TYPE I,
               COUNT TYPE I,


               KNUMV TYPE EKKO-KNUMV,

               KPOSN TYPE KONV-KPOSN,
               KBETR TYPE KONV-KBETR,
               KWERT TYPE KONV-KWERT,



       END OF GS_FINAL.



TYPES:BEGIN OF STR_KONV,                    "ADD BY MANI 03.10.15
      KNUMV TYPE KONV-KNUMV,
      KPOSN TYPE KONV-KPOSN,
      KSCHL TYPE KONV-KSCHL,
      KBETR TYPE KONV-KBETR,
      KWERT TYPE KONV-KWERT,

  END OF STR_KONV.


DATA:WA_KONV TYPE STR_KONV,                         "ADD BY MANI 03.10.15
      IT_KONV TYPE TABLE OF STR_KONV.

DATA:LV_TABIX TYPE SY-TABIX.                          "ADD BY MANI 03.10.15




DATA : GT_FINAL TYPE TABLE OF GS_FINAL,
         WA_FINAL TYPE GS_FINAL.

DATA : GT_FCAT TYPE SLIS_T_FIELDCAT_ALV,
       IT_SORT TYPE SLIS_T_SORTINFO_ALV,
       WA_FCAT TYPE SLIS_FIELDCAT_ALV,
       WA_SORT TYPE SLIS_SORTINFO_ALV.

DATA: LV_DATE TYPE SY-DATUM.

DATA: LV_TRDATE TYPE I .

DATA: LV_DATE1 TYPE SY-DATUM.

DATA: LV_TRDATE1 TYPE I .

DATA: LV_DATE2 TYPE SY-DATUM.

DATA: LV_TRDATE2 TYPE I .

DATA :   LV_NAME1 TYPE LFA1-NAME1,
         LV_EKORG TYPE EKKO-EKORG,
         LV_BUKRS TYPE EKKO-BUKRS,
         LV_WERKS TYPE EKPO-WERKS,
         LV_SPART TYPE MARA-SPART,
         LV_EKGRP TYPE EKKO-EKGRP,
         LV_BSART TYPE EKKO-BSART,
         LV_BSTYP TYPE EKKO-BSTYP,
         LV_LOEKZ TYPE EKKO-LOEKZ,
         LV_MATNR TYPE MARA-MATNR,
         LV_MATKL TYPE EKPO-MATKL,
         LV_BEDAT TYPE EKKO-BEDAT,
         LV_EBELN TYPE EKKO-EBELN,
         LV_LIFNR TYPE EKKO-LIFNR,
         LV_MTART TYPE EKPO-MTART.


DATA : LAYOUT TYPE SLIS_LAYOUT_ALV.

SELECTION-SCREEN : BEGIN OF BLOCK C1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS  : SO_BUKRS FOR LV_BUKRS OBLIGATORY NO INTERVALS NO-EXTENSION ,
                  SO_LIFNR FOR LV_LIFNR NO-DISPLAY,
                  SO_EBELN FOR LV_EBELN,
                  SO_BEDAT FOR LV_BEDAT OBLIGATORY,
                  SO_EKORG FOR LV_EKORG NO-DISPLAY,
                  SO_BSART FOR LV_BSART,
                  SO_EKGRP FOR LV_EKGRP NO-DISPLAY,
                  SO_WERKS FOR LV_WERKS OBLIGATORY,
                  SO_SPART FOR LV_SPART,
                  SO_BSTYP FOR LV_BSTYP,
                  SO_LOEKZ FOR LV_LOEKZ,
                  SO_MATNR FOR LV_MATNR,
                  SO_MATKL FOR LV_MATKL,
                  SO_NAME1 FOR LV_NAME1 NO-DISPLAY,
                  SO_MTART FOR LV_MTART.
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

  " MODIFIED BY RAM ON 22/09/2014

  "IF GT_MARA[] IS NOT INITIAL.
  SELECT
     EBELN
     EBELP
     LOEKZ    "ADDED BY RAM ON 29/09/2014
     AEDAT
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
     ELIKZ
     WEPOS
     MTART
     PRDAT
     BSTYP
       " FROM EKPO INTO TABLE GT_EKPO FOR ALL ENTRIES IN GT_MARA
    FROM EKPO INTO TABLE GT_EKPO
  WHERE  BUKRS IN SO_BUKRS AND  BSTYP IN SO_BSTYP
    AND PRDAT IN SO_BEDAT
    "AND AEDAT IN SO_BEDAT
          AND EBELN IN SO_EBELN AND WERKS IN SO_WERKS AND MTART IN SO_MTART AND MATKL IN SO_MATKL  AND LOEKZ <> 'L' .  " COMMEND BY RAM ON 27/3 OR ELIKZ <> 'X' ) ."CHANGED ON 27/11/2014

  SELECT
      MATNR
      MTART
      VOLUM
      VOLEH
      SPART
FROM MARA INTO TABLE GT_MARA FOR ALL ENTRIES IN GT_EKPO
WHERE MATNR = GT_EKPO-MATNR AND MATNR IN SO_MATNR AND SPART IN SO_SPART .


  "ENDIF.

  IF GT_EKPO[] IS NOT INITIAL.
    SELECT
         EBELN
         BUKRS
         BSTYP
         VERKF
         UNSEZ
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
         RESWK
         KNUMV     "ADD BY MANI ON 03.10.15

            FROM EKKO INTO TABLE GT_EKKO FOR ALL ENTRIES IN GT_EKPO  WHERE  EBELN = GT_EKPO-EBELN
        AND EKORG IN SO_EKORG AND BSART IN SO_BSART
        AND LIFNR IN SO_LIFNR AND EKGRP IN SO_EKGRP AND BSART IN SO_BSART AND BSTYP IN SO_BSTYP AND BEDAT
       IN SO_BEDAT AND ( BSART <> 'ZSR'  OR BSART <> 'ZIM' OR BSART <> 'ZSC' OR BSART <> 'ZCA' ).




    SELECT  KNUMV                         "ADD BY MANI ON 03.10.15
            KPOSN
            KSCHL
            KBETR
            KWERT FROM PRCD_ELEMENTS " KONV Replaced With PRCD_ELEMENTS Added by <IT-CAR Tool> during Code Remediation - tool
            INTO TABLE IT_KONV
            FOR ALL ENTRIES IN GT_EKKO  WHERE  KNUMV = GT_EKKO-KNUMV.





  ENDIF.
  SORT GT_EKKO BY EBELN.
  DELETE ADJACENT DUPLICATES FROM GT_EKKO COMPARING EBELN.

  SELECT
  LIFNR
  NAME1
   FROM LFA1 INTO TABLE GT_LFA1 FOR ALL ENTRIES IN GT_EKKO
  WHERE LIFNR  = GT_EKKO-LIFNR .



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


*  SELECT
* MATNR
* MTART
* VOLUM
* VOLEH
* SPART
*    FROM MARA INTO TABLE GT_MARA FOR ALL ENTRIES IN GT_EKPO
*WHERE MATNR = GT_EKPO-MATNR AND  SPART IN SO_SPART .

  SELECT
  WERKS
  NAME1
    FROM T001W INTO TABLE GT_T001W FOR ALL ENTRIES IN GT_EKPO WHERE WERKS = GT_EKPO-WERKS.

  IF GT_EKPO[] IS NOT INITIAL.
    SELECT
      EBELN
      EBELP
      EINDT
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
         FROM MARD INTO TABLE GT_MARD FOR ALL ENTRIES IN GT_EKPO WHERE  WERKS  = '1100' AND MATNR = GT_EKPO-MATNR .

    SELECT
    MATNR
    WERKS
    LABST
     FROM MARD INTO TABLE GT_MARD1 FOR ALL ENTRIES IN GT_EKPO WHERE  WERKS  = '2100' AND MATNR = GT_EKPO-MATNR .

  SELECT
    MATNR
    WERKS
    LABST
     FROM MARD INTO TABLE GT_MARD2 FOR ALL ENTRIES IN GT_EKPO WHERE  WERKS  = '4100' AND MATNR = GT_EKPO-MATNR .

    SELECT
         MATNR
        MVGR1
          FROM MVKE INTO TABLE GT_MVKE FOR ALL ENTRIES IN GT_FINAL WHERE MATNR = GT_FINAL-MATNR . "ADDED BY RAM ON 28/11/2014

    SELECT
         MVGR1
         BEZEI
          FROM TVM1T INTO TABLE GT_TVM1T FOR ALL ENTRIES IN GT_MVKE WHERE MVGR1 = GT_MVKE-MVGR1 . "ADDED BY RAM ON 28/11/2014

    IF GT_EKKO[] IS NOT INITIAL.
      SELECT
         MBLNR
          EBELN
          EBELP
          MATNR
          ZEILE
          BWART
          XAUTO
          WERKS
          MENGE
          BUDAT_MKPF
          SMBLN    " Added by savariar as on 04/02/2015
          FROM MSEG INTO TABLE GT_MSEG FOR ALL ENTRIES IN GT_EKKO  WHERE EBELN = GT_EKKO-EBELN AND EBELN IN SO_EBELN   AND XAUTO <> 'X' AND BWART IN ('351','641', '643')  .   "ADDED BY RAM ON 27/11/2014

      SELECT
           MBLNR
           EBELN
           EBELP
           MATNR
           ZEILE
           BWART
           XAUTO
           WERKS
           MENGE
           BUDAT_MKPF
           SMBLN   " Added by savariar as on 04/02/2015
           FROM MSEG INTO TABLE GT_MSEG1  FOR ALL ENTRIES IN GT_EKKO  WHERE EBELN = GT_EKKO-EBELN AND  EBELN IN SO_EBELN   AND (  BWART = '101' OR BWART = '102' )  .

      SELECT   MBLNR
               EBELN
               EBELP
               MATNR
               ZEILE
               BWART
               XAUTO
               WERKS
               MENGE
               BUDAT_MKPF
               SMBLN   " Added by savariar as on 04/02/2015
               FROM MSEG INTO TABLE GT_MSEG2  FOR ALL ENTRIES IN GT_EKKO  WHERE EBELN = GT_EKKO-EBELN AND  EBELN IN SO_EBELN AND XAUTO <> 'X'  AND BWART IN ('352','642', '644' ) .
    ENDIF.

    SELECT
   WERKS
   NAME1
      FROM T001W INTO TABLE GT_T001W1 FOR ALL ENTRIES IN GT_EKKO WHERE WERKS = GT_EKKO-RESWK.







  ENDIF.




  SORT GT_EKKO BY EBELN.                       "ADD BY MANI ON 03.10.15
  SORT GT_EKPO BY EBELN EBELP.
  SORT IT_KONV BY KNUMV.




  DELETE GT_MSEG WHERE EBELN = ' '.
  DELETE GT_MSEG1 WHERE EBELN = ' '.
  DELETE GT_MSEG2 WHERE EBELN = ' '.
  DELETE GT_MVKE WHERE MVGR1 = ' ' .
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

  LV_DATE1 = SY-DATUM .

  LV_DATE2 = SY-DATUM .

  LOOP AT GT_EKPO INTO WA_EKPO.
    MOVE-CORRESPONDING WA_EKPO TO WA_FINAL.
    READ TABLE GT_EKKO INTO WA_EKKO WITH KEY  EBELN  =  WA_EKPO-EBELN  .
    WA_FINAL-BUKRS = WA_EKKO-BUKRS.
    WA_FINAL-PINCR = WA_EKKO-PINCR.
    WA_FINAL-EKORG = WA_EKKO-EKORG.
    WA_FINAL-EKGRP = WA_EKKO-EKGRP.
    WA_FINAL-VERKF = WA_EKKO-VERKF.
    WA_FINAL-UNSEZ = WA_EKKO-UNSEZ.
    WA_FINAL-BSART = WA_EKKO-BSART.
    WA_FINAL-BSTYP = WA_EKKO-BSTYP.

    WA_FINAL-AEDAT = WA_EKKO-AEDAT.
    WA_FINAL-ZTERM = WA_EKKO-ZTERM.
    WA_FINAL-FRGKE = WA_EKKO-FRGKE.
    WA_FINAL-FRGZU = WA_EKKO-FRGZU.
    WA_FINAL-LIFNR = WA_EKKO-LIFNR.
    WA_FINAL-BEDAT = WA_EKKO-BEDAT.
    WA_FINAL-RESWK = WA_EKKO-RESWK.



    WA_FINAL-KNUMV = WA_EKKO-KNUMV.
    READ TABLE GT_EINA INTO WA_EINA WITH KEY  MATNR  =  WA_EKPO-MATNR .
    WA_FINAL-LMEIN = WA_EINA-LMEIN.
    READ TABLE GT_T134T INTO WA_T134T WITH KEY  MTART  =  WA_EKPO-MTART .
    WA_FINAL-MTBEZ = WA_T134T-MTBEZ.
    READ TABLE GT_T163 INTO WA_T163 WITH KEY  WEPOS =  WA_EKPO-WEPOS .
    WA_FINAL-PSTYP = WA_T163-PSTYP.
    READ TABLE GT_T001W INTO WA_T001W WITH KEY  WERKS =  WA_EKPO-WERKS .
    WA_FINAL-PNAME = WA_T001W-PNAME.
    LOOP AT  GT_EKET INTO WA_EKET WHERE  EBELN =  WA_EKPO-EBELN  AND EBELP = WA_EKPO-EBELP.

      WA_FINAL-EINDT = WA_EKET-EINDT.
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

    READ TABLE GT_MARD INTO WA_MARD WITH KEY MATNR = WA_EKPO-MATNR WERKS = '1100'  .
    IF SY-SUBRC = 0.
      WA_FINAL-LABST = WA_MARD-LABST.
    ENDIF.
    READ TABLE GT_MARD1 INTO WA_MARD1 WITH KEY MATNR = WA_EKPO-MATNR WERKS = '2100'  .
    IF SY-SUBRC = 0.
      WA_FINAL-LABST1 = WA_MARD1-LABST.
    ENDIF.

   READ TABLE GT_MARD2 INTO WA_MARD2 WITH KEY MATNR = WA_EKPO-MATNR WERKS = '4100'  .
    IF SY-SUBRC = 0.
      WA_FINAL-LABST2 = WA_MARD2-LABST.
    ENDIF.

*    LOOP AT GT_MARD INTO WA_MARD WHERE MATNR = WA_EKPO-MATNR AND ( WERKS = '1100' OR WERKS = '2100') .
*       WA_FINAL-LABST = WA_FINAL-LABST + WA_MARD-LABST.
*       ENDLOOP.


    READ TABLE GT_MARA INTO WA_MARA WITH KEY  MATNR =  WA_EKPO-MATNR .
    WA_FINAL-SPART = WA_MARA-SPART.
    WA_FINAL-VOLUM = WA_MARA-VOLUM.
    WA_FINAL-VOLEH = WA_MARA-VOLEH.
    WA_FINAL-QTY_LTR = WA_EKPO-MENGE.
    IF WA_MARA-MTART = 'FERT' OR WA_MARA-MTART = 'HAWA' .
      WA_FINAL-QTY_LTR = WA_MARA-VOLUM * WA_EKPO-MENGE.
    ENDIF.



    LOOP AT GT_MSEG INTO WA_MSEG WHERE EBELN = WA_FINAL-EBELN  AND MATNR = WA_FINAL-MATNR AND EBELP = WA_FINAL-EBELP AND XAUTO NE 'X' AND ( BWART = '351' OR BWART = '641' OR BWART = '643' )."ADDED BY RAM ON 27/11/2014

      WA_FINAL-MBLNR  = WA_MSEG-MBLNR .   " Added by savariar as on 04/02/2015
      WA_FINAL-TMENGE1 = WA_FINAL-TMENGE1 + WA_MSEG-TMENGE.
      WA_FINAL-ZEILE   = WA_MSEG-ZEILE.
      WA_FINAL-EBELP   = WA_MSEG-EBELP.  " Added by savariar as on 04/02/2015


      """"""""""Start"""" Added by savariar as on 04/02/2015


      READ TABLE GT_MSEG2 INTO WA_MSEG2 WITH KEY SMBLN = WA_FINAL-MBLNR  EBELN = WA_FINAL-EBELN MATNR = WA_FINAL-MATNR EBELP = WA_FINAL-EBELP .
      IF SY-SUBRC = 0.
        DELETE GT_MSEG WHERE SMBLN = WA_FINAL-MBLNR.
        CLEAR : WA_MSEG.
      ENDIF.

      ON CHANGE OF WA_MSEG-EBELP."#EC CI_SORTED  "Added by SPLABAP during code remediation

        IF WA_MSEG-BUDAT_MKPF NE 0 .

          WA_FINAL-BUDAT_MKPF = WA_MSEG-BUDAT_MKPF .

          CALL FUNCTION 'HR_99S_INTERVAL_BETWEEN_DATES'
            EXPORTING
              BEGDA = LV_DATE1
              ENDDA = WA_FINAL-BUDAT_MKPF
            IMPORTING
              DAYS  = LV_TRDATE1.

          WA_FINAL-COUNT1 =  LV_TRDATE1.

        ENDIF.

        """"""""""
        """"""""""End"""" Added by savariar as on 04/02/2015
      ENDON.


    ENDLOOP.

    LOOP AT GT_MSEG1 INTO WA_MSEG1 WHERE EBELN = WA_FINAL-EBELN AND  MATNR = WA_FINAL-MATNR  AND EBELP = WA_FINAL-EBELP  AND   BWART = '101'    ."ADDED BY RAM ON 27/11/2014
      WA_FINAL-TMENGE2 =  WA_FINAL-TMENGE2 + WA_MSEG1-TMENGE.
      WA_FINAL-ZEILE   = WA_MSEG1-ZEILE.
*      WA_FINAL-BUDAT_MKPF = WA_MSEG1-BUDAT_MKPF .
    ENDLOOP.

    LOOP AT GT_MSEG1 INTO WA_MSEG1 WHERE EBELN = WA_FINAL-EBELN AND  MATNR = WA_FINAL-MATNR  AND EBELP = WA_FINAL-EBELP  AND  BWART = '102'   ."ADDED BY RAM ON 27/11/2014
      WA_FINAL-TMENGE3 =  WA_FINAL-TMENGE3 + WA_MSEG1-TMENGE.
      WA_FINAL-ZEILE   = WA_MSEG1-ZEILE.
*      WA_FINAL-BUDAT_MKPF = WA_MSEG1-BUDAT_MKPF .
    ENDLOOP.

    LOOP AT GT_MSEG2 INTO WA_MSEG2 WHERE EBELN = WA_FINAL-EBELN AND  MATNR = WA_FINAL-MATNR  AND EBELP = WA_FINAL-EBELP  AND (  BWART = '352' OR   BWART = '642' OR BWART = '644' ) AND  XAUTO NE 'X' ."ADDED BY RAM ON 27/11/2014
      WA_FINAL-TMENGE4 =  WA_FINAL-TMENGE4 + WA_MSEG2-TMENGE .
      WA_FINAL-ZEILE   = WA_MSEG2-ZEILE.
*      WA_FINAL-BUDAT_MKPF = WA_MSEG2-BUDAT_MKPF .
*
*      CALL FUNCTION 'HR_99S_INTERVAL_BETWEEN_DATES'
*        EXPORTING
*          BEGDA = LV_DATE2
*          ENDDA = WA_FINAL-BUDAT_MKPF
*        IMPORTING
*          DAYS  = LV_TRDATE2.
*
*      WA_FINAL-COUNT2 =  LV_TRDATE2.

    ENDLOOP.


    SHIFT WA_FINAL-MATNR LEFT DELETING LEADING '0'.
    APPEND WA_FINAL TO GT_FINAL.
    CLEAR WA_FINAL.




  ENDLOOP.


  LOOP AT GT_FINAL INTO WA_FINAL.


    IF WA_FINAL-LIFNR IS NOT INITIAL.
      READ TABLE GT_LFA1 INTO WA_LFA1 WITH KEY  LIFNR  =  WA_FINAL-LIFNR .
      WA_FINAL-NAME1 = WA_LFA1-NAME1.
      WA_FINAL-RMENGE = WA_FINAL-MENGE - WA_FINAL-WEMNG .
      MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING  NAME1 RMENGE  .
      CLEAR WA_FINAL.
    ENDIF.
  ENDLOOP.

  LOOP AT GT_FINAL INTO WA_FINAL.
    READ TABLE GT_MVKE INTO WA_MVKE WITH KEY  MATNR =  WA_FINAL-MATNR . "ADDED BY RAM 28/11/2014
    IF SY-SUBRC = 0.
      WA_FINAL-MVGR1 = WA_MVKE-MVGR1.
    ENDIF.
    MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING  MVGR1 .
    CLEAR WA_FINAL.

  ENDLOOP.

  LOOP AT GT_FINAL INTO WA_FINAL.
    READ TABLE GT_TVM1T INTO WA_TVM1T WITH KEY  MVGR1 =  WA_FINAL-MVGR1 .
    IF SY-SUBRC = 0.
      WA_FINAL-BEZEI = WA_TVM1T-BEZEI.
    ENDIF.
    WA_FINAL-REC_PR = WA_FINAL-WEMNG * WA_FINAL-NETPR .


    WA_FINAL-TMENGE =    ( WA_FINAL-TMENGE1  - WA_FINAL-TMENGE4 ) - ( WA_FINAL-TMENGE2 - WA_FINAL-TMENGE3 )   .

    IF WA_FINAL-TMENGE NE 0 .

      READ TABLE GT_MSEG INTO WA_MSEG WITH KEY EBELN = WA_FINAL-EBELN EBELP = WA_FINAL-EBELP TMENGE = WA_FINAL-TMENGE .
      IF SY-SUBRC = 0.

        WA_FINAL-BUDAT_MKPF = WA_MSEG-BUDAT_MKPF .


      ENDIF.


    ENDIF.

    IF WA_FINAL-TMENGE NE 0 ."AND WA_FINAL-TMENGE1 EQ WA_FINAL-TMENGE4.     """"""""""Start"""" Added by savariar as on 04/02/2015


*    WA_FINAL-COUNT = ( WA_FINAL-COUNT1 - WA_FINAL-COUNT2 ).

      WA_FINAL-COUNT =  WA_FINAL-COUNT1.

    ENDIF.

    WA_FINAL-BQTY  = WA_FINAL-MENGE - WA_FINAL-WEMNG.
    WA_FINAL-DQTY_LTR = WA_FINAL-VOLUM * WA_FINAL-RMENGE.
    WA_FINAL-DEL_PR =  WA_FINAL-BQTY * WA_FINAL-NETPR.

    MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING SPART REC_PR BQTY DQTY_LTR DEL_PR TMENGE COUNT BEZEI .
    CLEAR WA_FINAL.
  ENDLOOP.


  " START CHANGED BY RAM ON 28/1/2015
  LOOP AT GT_FINAL INTO WA_FINAL.
    IF WA_FINAL-BSART EQ 'ZNB' . "OR WA_FINAL-BSART EQ 'ZSTO' .
      WA_FINAL-TMENGE = 0 .
      MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING TMENGE .
      CLEAR WA_FINAL .
    ENDIF.
  ENDLOOP.


  LV_DATE = SY-DATUM .


  LOOP AT GT_FINAL INTO WA_FINAL.

    IF WA_FINAL-TMENGE NE 0 AND WA_FINAL-BUDAT_MKPF NE 0.

      "   LOOP AT GT_MSEG INTO WA_MSEG WHERE BUDAT_MKPF = WA_FINAL-BUDAT_MKPF AND ZEILE = WA_FINAL-ZEILE AND EBELN = WA_FINAL-EBELN .
      READ TABLE GT_MSEG INTO WA_MSEG WITH KEY  EBELN = WA_FINAL-EBELN  ZEILE = WA_FINAL-ZEILE BUDAT_MKPF = WA_FINAL-BUDAT_MKPF .

      CALL FUNCTION 'HR_99S_INTERVAL_BETWEEN_DATES'
        EXPORTING
          BEGDA = LV_DATE
          ENDDA = WA_FINAL-BUDAT_MKPF
        IMPORTING
          DAYS  = LV_TRDATE.
      WA_FINAL-TR_DAYS = LV_TRDATE.

      MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING TR_DAYS.

      CLEAR WA_FINAL .

    ENDIF.

  ENDLOOP.
*BREAK-POINT .
  "END CHANGED BY RAM ON 28/1/2015



  LOOP AT GT_FINAL INTO WA_FINAL.
    WA_FINAL-RQTY_LTR = WA_FINAL-VOLUM * WA_FINAL-WEMNG.  " CHANGED BY RAM 18/09/2014
    WA_FINAL-DQTY_LTR = WA_FINAL-VOLUM * WA_FINAL-BQTY.
    WA_FINAL-TQTY_LTR = WA_FINAL-VOLUM * WA_FINAL-TMENGE.  "ADDED BY RAM ON 27/11/2014
    WA_FINAL-LABSTT = WA_FINAL-LABST * WA_FINAL-VOLUM . " ADDED BY RAM ON 14/11/2014
    WA_FINAL-LABST1T = WA_FINAL-LABST1 * WA_FINAL-VOLUM . " ADDED BY RAM ON 14/11/2014
    WA_FINAL-LABST2T = WA_FINAL-LABST2 * WA_FINAL-VOLUM . " ADDED BY RAM ON 14/11/2014
    WA_FINAL-INV_QTY =  WA_FINAL-BQTY - WA_FINAL-IMENGE  .
    WA_FINAL-INVQTY_LTR = WA_FINAL-INV_QTY * WA_FINAL-VOLUM.



    MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING INV_QTY RQTY_LTR INVQTY_LTR DQTY_LTR LABSTT TQTY_LTR LABST1T LABST2T.
    CLEAR WA_FINAL.
  ENDLOOP .
  SORT GT_FINAL BY NAME1 EBELN.
  IF  SO_SPART IS NOT INITIAL.
    DELETE GT_FINAL WHERE SPART EQ ' '. "Hided By Govind on 20.08.2014
  ENDIF.
  LOOP AT GT_FINAL INTO WA_FINAL.
    IF WA_FINAL-LIFNR IS INITIAL.                 "ADDED BY RAM ON 29/11/2014
      READ TABLE GT_T001W1 INTO WA_T001W1 WITH KEY WERKS = WA_FINAL-RESWK .
      WA_FINAL-LIFNR = WA_T001W1-WERKS.
      WA_FINAL-NAME1 = WA_T001W1-PNAME.

      MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING LIFNR NAME1.
    ENDIF.
  ENDLOOP.





  "  ADDED BY MANI 03.10.15

  LOOP AT IT_KONV INTO WA_KONV WHERE KSCHL EQ 'P101' OR KSCHL EQ 'PI01'.
    READ TABLE GT_FINAL INTO WA_FINAL WITH KEY KNUMV = WA_KONV-KNUMV EBELP = WA_KONV-KPOSN .
    IF SY-SUBRC = 0.
      CLEAR:LV_TABIX.
      LV_TABIX = SY-TABIX.
      LOOP AT GT_FINAL INTO WA_FINAL FROM LV_TABIX.
        IF WA_FINAL-KNUMV <> WA_KONV-KNUMV OR WA_FINAL-EBELP <> WA_KONV-KPOSN.
          EXIT.
        ELSE.
*          WA_FINAL-KPOSN = WA_KONV-KPOSN.

          WA_FINAL-KBETR = WA_KONV-KBETR.
          WA_FINAL-KWERT = WA_KONV-KWERT.
*          APPEND WA_FINAL TO GT_FINAL.
          MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING KBETR KWERT.
          CLEAR:WA_FINAL.
        ENDIF.
      ENDLOOP.
    ENDIF.
    CLEAR:WA_KONV.
*    CLEAR WA_FINAL.
  ENDLOOP.


  "  ADDED BY MANI 03.10.15


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
  PERFORM ALV_LAYOUT USING 8 'Customer Ref.' 'VERKF' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 9 'Sales Requirment plan.' 'UNSEZ' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 10 'Type' 'BSART' 'GT_FINAL' ''.

  " PERFORM ALV_LAYOUT USING 10 'Category' 'BSTYP' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 11 'Purch Group' 'EKGRP' 'GT_FINAL' ''.
  " PERFORM ALV_LAYOUT USING 12 'Purchase Org' 'EKORG' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 13 'Document Date' 'BEDAT' 'GT_FINAL' ''.

  PERFORM ALV_LAYOUT USING 14 'Delivered Date' 'EINDT' 'GT_FINAL' ''.               "Added by savariar as 0n 30/04/2015

  PERFORM ALV_LAYOUT USING 15 'Material' 'MATNR' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 16 'Short Text' 'TXZ01' 'GT_FINAL' ''.
  " PERFORM ALV_LAYOUT USING 17 'Material Group' 'MVGR1' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 18 'Material Group Des' 'BEZEI' 'GT_FINAL' ''.
  " PERFORM ALV_LAYOUT USING 18 'Deletion Ind' 'LOEKZ' 'GT_FINAL' ''.  "ADDED BY RAM ON 25/11/2014
  " PERFORM ALV_LAYOUT USING 19 'Release Ind' 'FRGKE' 'GT_FINAL' ''.

  " PERFORM ALV_LAYOUT USING 22 'Relase Status' 'FRGZU' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 23 'SLoc' 'LGORT' 'GT_FINAL' ''.

  IF SO_BUKRS-LOW ='1000' OR SO_BUKRS-LOW = '4000' .
    PERFORM ALV_LAYOUT USING 24 'WHC.Stock Qty In Cases(1100)' 'LABST' 'GT_FINAL' 'X'.
    PERFORM ALV_LAYOUT USING 25 'WHC.Stock Qty In Ltrs(1100)' 'LABSTT' 'GT_FINAL' 'X'.   "Adde By Ram on 14/11/2015

    PERFORM ALV_LAYOUT USING 26 'WHC.Stock Qty In Cases(4100)' 'LABST2' 'GT_FINAL' 'X'.
    PERFORM ALV_LAYOUT USING 27 'WHC.Stock Qty In Ltrs(4100)' 'LABST2T' 'GT_FINAL' 'X'.   "Adde By Ram on 14/11/2015

  ENDIF.

  IF SO_BUKRS-LOW ='2000' .
    PERFORM ALV_LAYOUT USING 24 'WHC.Stock Qty In Cases(2100)' 'LABST1' 'GT_FINAL' 'X'.
    PERFORM ALV_LAYOUT USING 25 'WHC.Stock Qty In Ltrs(2100)' 'LABST1T' 'GT_FINAL' 'X'.   "Adde By Ram on 14/11/2015
  ENDIF.


  PERFORM ALV_LAYOUT USING 28 'P.O Qty In Cases' 'MENGE' 'GT_FINAL' 'X'.
  PERFORM ALV_LAYOUT USING 29 'P.O Qty In Ltrs' 'QTY_LTR' 'GT_FINAL' 'X'.
*  PERFORM ALV_LAYOUT USING 28 'Unit Price' 'NETPR' 'GT_FINAL' 'X'.           Modified BY MANI 03.10.2015
*
*  PERFORM ALV_LAYOUT USING 29 'Net Price' 'NETWR' 'GT_FINAL' 'X'.            Modifief BY MANI 03.10.2015
  PERFORM ALV_LAYOUT USING 32 'Order Unit' 'MEINS' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 33 'Terms Of Payment' 'ZTERM' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 34 'Material Type' 'MTART' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 35 'Volume' 'VOLUM' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 36 'Volume Unit' 'VOLEH' 'GT_FINAL' ''.

  PERFORM ALV_LAYOUT USING 37 'Transit in Qty In cases' 'TMENGE' 'GT_FINAL' 'X'.
  PERFORM ALV_LAYOUT USING 38 'Transit in Qty In Liter' 'TQTY_LTR' 'GT_FINAL' 'X'.
*  PERFORM ALV_LAYOUT USING 39 'Transit Days' 'TR_DAYS' 'GT_FINAL' 'X'.

  PERFORM ALV_LAYOUT USING 39 'Transit Days' 'COUNT' 'GT_FINAL' ''.   """"""""""Start"""" Added by savariar as on 04/02/2015

  PERFORM ALV_LAYOUT USING 40 'Delivered Qty In Cases' 'WEMNG' 'GT_FINAL' 'X'.
  PERFORM ALV_LAYOUT USING 41 'Delivered Qty In Liter' 'RQTY_LTR' 'GT_FINAL' 'X'.
  PERFORM ALV_LAYOUT USING 42 'Delivered Price' 'REC_PR' 'GT_FINAL' 'X'.

  "  PERFORM ALV_LAYOUT USING 41 'Transit Qty In Cases' 'TRAME' 'GT_FINAL' 'X'.
  " PERFORM ALV_LAYOUT USING 42 'Transit Qty In Ltr' 'TSTQTY_LTR' 'GT_FINAL' 'X'.
  "  PERFORM ALV_LAYOUT USING 43 'Transit Price' 'TST_PR' 'GT_FINAL' 'X'.

  PERFORM ALV_LAYOUT USING 46 'To Be Del Qty' 'BQTY' 'GT_FINAL' 'X'.
  PERFORM ALV_LAYOUT USING 47 'To Be Del Qty in Ltr' 'DQTY_LTR' 'GT_FINAL' 'X'.

  PERFORM ALV_LAYOUT USING  48 'To Be Del Price' 'DEL_PR' 'GT_FINAL' 'X'.

*  PERFORM ALV_LAYOUT USING  49 'COUNT' 'COUNT' 'GT_FINAL' ''.

*  PERFORM ALV_LAYOUT USING 50 'BUDAT_MKPF' 'BUDAT_MKPF' 'GT_FINAL' 'X'.

  "PERFORM ALV_LAYOUT USING 52 'To Be Invoice' 'INV_QTY' 'GT_FINAL' 'X'.
  "PERFORM ALV_LAYOUT USING 53 'To Be Invoice in Ltr' 'INVQTY_LTR' 'GT_FINAL' 'X'.

  " PERFORM ALV_LAYOUT USING 54 'Transit in Qty' 'TMENGE' 'GT_FINAL' 'X'.   "ADDED BY RAM ON 27/11/2014
  "PERFORM ALV_LAYOUT USING 55 'TYPE' 'MTART' 'GT_FINAL' ''.



  IF WA_FINAL-NETPR <> 0 AND WA_FINAL-NETWR <> 0.                                           "  ADDED BY MANI 03.10.15

    PERFORM ALV_LAYOUT USING 30 'Unit Price' 'NETPR' 'GT_FINAL' 'X'.
    PERFORM ALV_LAYOUT USING 31 'Net Price' 'NETWR' 'GT_FINAL' 'X'.
  ENDIF.

  IF WA_FINAL-NETPR = 0 AND WA_FINAL-NETWR = 0.                                                "  ADDED BY MANI 03.10.15

    PERFORM ALV_LAYOUT USING  30 'Unit Price' 'KBETR' 'GT_FINAL' 'X'.
    PERFORM ALV_LAYOUT USING  31 'Net Price' 'KWERT' 'GT_FINAL' 'X'.
  ENDIF.

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
  LS_LINE-INFO = 'Purchasing Document Per Vendor' .
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
