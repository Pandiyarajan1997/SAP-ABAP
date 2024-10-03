*&---------------------------------------------------------------------*
*& Report  ZMM_VENDOR_TRANSINFO
*&
*&---------------------------------------------------------------------*
*&Functional                   : Mr.Govindarajan M                        *
*& Developer                   : Mr.Govindarajan M                        *
*& Created On                  : 02 JAN 2013                              *
*& Title                       : Vendor Transaction Performance           *
*& Company                     : Sheenlac Paints Pvt Ltd                  *
*& Report Name                 : ZMM_VENDOR_TRANSINFO                     *
*& Development Id              : kpabap                                   *
*& Related Information         : Delay in Supply By The Vendor            *
*&---------------------------------------------------------------------   *

REPORT ZMM_VENDOR_TRANSINFO.

*TABLES : EKKO ,EKPO ,LFA1 ,MAKT ,MARC , EKBE ,EKET .
TYPE-POOLS : SLIS .
*INCLUDE .
*******************************************************************************************************************************************
********************                     STRUCTURE DECLARATION                                        ************************************
*******************************************************************************************************************************************
TYPES : BEGIN OF GS_EKKO  ,
        EBELN TYPE EBELN  ,                             "PURCHASING DOCUMENT NUMBER
        LIFNR TYPE ELIFN  ,                             "VENDOR ACCOUNT NUMBER
        BSTYP TYPE EBSTYP ,                             "PURCHASING DOCUMENT CATEGORY
        BSART TYPE ESART  ,                             "PURCHASING DOCUMENT TYPE
        LOEKZ TYPE ELOEK  ,                             "DELETION INDICATOR IN PURCHASING DOCUMENT
        AEDAT TYPE PAEDT  ,                             "Purchasing Document Item Change Date
        END OF GS_EKKO .

TYPES : BEGIN OF GS_EKPO ,
        EBELN TYPE EBELN ,                              "PURCHASING DOCUMENT NUMBER
        EBELP TYPE EBELP ,                              "ITEM NUMBER OF PURCHASING DOCUMENT
        MATNR TYPE MATNR ,                              "MATERIAL NUMBER
        WERKS TYPE EWERK ,                              " PLANT
        MENGE TYPE BSTMG ,                              "Purchase Order Quantity
        AEDAT TYPE PAEDT ,                              "Purchasing Document Item Change Date
        END OF GS_EKPO .

TYPES : BEGIN OF GS_LFA1 ,
        LIFNR TYPE LIFNR ,                              "ACCOUNT NUMBER OF VENDOR OR CREDITOR
        NAME1 TYPE NAME1_GP ,                               "NAME1
        END OF GS_LFA1 .

TYPES : BEGIN OF GS_MAKT ,
        MATNR TYPE MATNR ,                              "MATERIAL NUMBER
        MAKTX TYPE MAKTX ,                              "MATERIAL DESCRIPTION (SHORT TEXT)
        END OF GS_MAKT .

TYPES : BEGIN OF GS_MARC ,
        MATNR TYPE MATNR ,                              "MATERIAL NUMBER
        PLIFZ TYPE PLIFZ ,                              "PLANNED DELIVERY TIME IN DAYS
        END OF GS_MARC .

TYPES : BEGIN OF GS_EKET ,
        EBELN TYPE EBELN ,                              "PURCHASING DOCUMENT NUMBER
        EBELP TYPE EBELP ,                              "ITEM NUMBER OF PURCHASING DOCUMENT
*        SH_MENGE TYPE ETMEN ,                           "SCHEDULED QUANTITY
*        WEMNG TYPE WEEMG ,                              "Quantity of Goods Received
        EINDT TYPE EINDT ,                              "Item Delivery Date
        END OF GS_EKET .

TYPES : BEGIN OF GS_EKBE ,
        EBELN TYPE EBELN ,                              "PURCHASING DOCUMENT NUMBER
        EBELP TYPE EBELP ,                              "ITEM NUMBER OF PURCHASING DOCUMENT
        BELNR TYPE MBLNR ,                              "NUMBER OF MATERIAL DOCUMENT
        BUDAT TYPE BUDAT ,                              "POSTING DATE IN THE DOCUMENT
        BUZEI TYPE MBLPO ,                              "ITEM IN MATERIAL DOCUMENT
        BEWTP TYPE BEWTP ,                              "PURCHASE ORDER HISTORY CATEGORY
        BWART TYPE BWART ,                              "MOVEMENT TYPE (INVENTORY MANAGEMENT)
        MENGE	TYPE EKBE-MENGE,                          " Quantity
        LFBNR	TYPE LFBNR,                               " Document No. of a Reference Document
        END OF GS_EKBE .

TYPES: BEGIN OF GS_MSEG,
       MBLNR TYPE MSEG-MBLNR,                 " Number of Material Document
       ZEILE TYPE MSEG-ZEILE,                 " Item in Material Document
       MATNR TYPE MSEG-MATNR,                 " Material Number
       BWART TYPE MSEG-BWART,                 " Movement Type (Inventory Management)
       LFBNR TYPE MSEG-LFBNR,                 " Document No. of a Reference Document
       SMBLN TYPE MSEG-MBLNR,                 " Number of Material Document
       EBELN TYPE MSEG-EBELN,
       END OF GS_MSEG.

*TYPES : BEGIN OF GS_RBKP,



DATA: GT_MSEG TYPE TABLE OF GS_MSEG,
      WA_MSEG TYPE GS_MSEG.
*
*DATA : BEGIN OF GS_RBKP,
*      BELNR TYPE RBKP-BELNR,
*      GJAHR TYPE RBKP-GJAHR,
*      BLART TYPE RBKP-BLART,
*      BLDAT TYPE RBKP-BLDAT,
*      BUDAT TYPE RBKP-BUDAT,
*      END OF GS_RBKP.
*
*
*DATA : GT_RBKP TYPE TABLE OF GS_RBKP,
*       WA_RBKP TYPE GS_RBKP.


TYPES : BEGIN OF GS_RSEG,
  BELNR TYPE RSEG-BELNR,        "ACCOUNTING DOCUMENT NUMBER
  GJAHR TYPE RSEG-GJAHR,          "FISCAL YEAR
  EBELN TYPE RSEG-EBELN,          "PURCHASE DOCUMENT NUMBER
  EBELP TYPE RSEG-EBELP,          "ITEM NUMBER OF PURCHASING DOCUMENT
  MATNR TYPE RSEG-MATNR,          "MATERIAL NUMBER
  WERKS TYPE RSEG-WERKS,          "PLANT CODE
  BUKRS TYPE RSEG-BUKRS,          "COMPANY CODE
  MENGE TYPE RSEG-MENGE,          "QUANTITY
  MEINS TYPE RSEG-MEINS,          "BASE UNIT OF MEASURE
  LIFNR TYPE RSEG-LIFNR,          "VENDOR ACCOUNT NUMBER
  WRBTR TYPE RSEG-WRBTR,          "AMOUNT IN DOCUMENT CURRENCY
  END OF GS_RSEG.

DATA : GT_RSEG TYPE TABLE OF GS_RSEG,
       WA_RSEG TYPE GS_RSEG.

TYPES :BEGIN OF GS_HEADER ,
       LIFNR TYPE EKKO-LIFNR ,
       NAME1 TYPE LFA1-NAME1 ,
*       ZAVG_DEL TYPE P,                                "Average Delay
       EXPAND,
       END OF GS_HEADER .

TYPES : BEGIN OF GS_ITEM ,
        LIFNR TYPE EKKO-LIFNR ,
        NAME1 TYPE LFA1-NAME1 ,
        EBELN TYPE EKKO-EBELN ,
        AEDAT TYPE EKPO-AEDAT ,
        MAKTX  TYPE MAKT-MAKTX  ,
        LEDTM TYPE MARC-PLIFZ ,        " lEAD TIME
        MENGE TYPE EKPO-MENGE ,
        BELNR TYPE EKBE-BELNR ,
        BUDAT TYPE EKBE-BUDAT ,
        QTYREC TYPE EKBE-LSMNG,
        ZPER_PRO TYPE I,                                "Percentage-Procurement
        ZDAY_DEL TYPE I,
        ZDEL_PER TYPE P ,                               " Delayed-Percent
        RBELNR TYPE RSEG-BELNR,        "ACCOUNTING DOCUMENT NUMBER
        GJAHR TYPE RSEG-GJAHR,        "FISCAL YEAR
        RMENGE TYPE RSEG-MENGE,        "QUANTITY
        RMEINS TYPE RSEG-MEINS,        "BASE UNIT OF MEASURE
        WRBTR TYPE RSEG-WRBTR,        "AMOUNT IN DOCUMENT CURRENCY
        END OF GS_ITEM .

*******************************************************************************************************************************************
********************                     INTERNALTABLE & WORK AREA DECLARATION                        ************************************
*******************************************************************************************************************************************
DATA : GT_EKKO TYPE TABLE OF GS_EKKO ,
       WA_EKKO TYPE GS_EKKO ,
       GT_EKPO  TYPE TABLE OF GS_EKPO  ,
       WA_EKPO  TYPE GS_EKPO  ,
       GT_LFA1  TYPE TABLE OF GS_LFA1  ,
       WA_LFA1  TYPE GS_LFA1  ,
       GT_MAKT TYPE TABLE OF GS_MAKT ,
       WA_MAKT TYPE GS_MAKT ,
       GT_MARC TYPE TABLE OF GS_MARC ,
       WA_MARC TYPE GS_MARC ,
       GT_EKET TYPE TABLE OF GS_EKET ,
       WA_EKET TYPE GS_EKET ,
       GT_EKBE TYPE TABLE OF GS_EKBE ,
       WA_EKBE TYPE GS_EKBE ,
       WA_EKBE1 TYPE GS_EKBE ,
       GT_HEADER TYPE TABLE OF GS_HEADER ,
       WA_HEADER TYPE GS_HEADER ,
       GT_ITEM TYPE TABLE OF GS_ITEM,
       WA_ITEM TYPE GS_ITEM .

DATA : WA_FIELDCATALOGUE TYPE SLIS_FIELDCAT_ALV ,
       LT_FIELDCATALOGUE TYPE SLIS_T_FIELDCAT_ALV ,
       IT_LAYOUT TYPE SLIS_LAYOUT_ALV,
       GT_EVENTS TYPE SLIS_T_EVENT,
       WA_EVENTS TYPE SLIS_ALV_EVENT,
       KEY TYPE SLIS_KEYINFO_ALV.

DATA : LAYOUT TYPE SLIS_LAYOUT_ALV.

DATA : LS_VARIANT TYPE DISVARIANT.

LS_VARIANT-REPORT = SY-REPID.

DATA :  ZAVG_DEL TYPE P,
        LD_COUNT TYPE P,
        ZDAY_DEL TYPE VTBBEWE-ATAGE, "I,
        GD_AVG TYPE P ,
        LD_SUM_PRO TYPE P ,
        ZLEAD_DAY TYPE MARC-PLIFZ .

DATA: OR_AEDAT TYPE EKPO-AEDAT,
      OR_LIFNR TYPE EKKO-LIFNR,
      OR_BSART TYPE T161-BSART,
      OR_EKORG TYPE T024E-EKORG,
      OR_WERKS TYPE EKPO-WERKS,
      L_LIFNR TYPE EKKO-LIFNR,
      L_BSART TYPE T161-BSART,
      L_EKORG TYPE T024E-EKORG.

DATA: DELDATE TYPE SY-DATUM,
      T_LSMNG TYPE EKBE-LSMNG,
      B_LSMNG TYPE EKBE-LSMNG,
      LV_DATE TYPE SY-DATUM.

*******************************************************************************************************************************************
********************                     SELECTION-SCREEN                                             ************************************
*******************************************************************************************************************************************

*SELECTION-SCREEN: BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
*
*PARAMETERS: RB_LEAD RADIOBUTTON GROUP G1 DEFAULT 'X',
*            RB_DEL RADIOBUTTON GROUP G1.
*
*SELECTION-SCREEN: END OF BLOCK B2.

SELECTION-SCREEN : BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001 .

SELECT-OPTIONS  : SO_WERKS FOR  OR_WERKS OBLIGATORY.

SELECT-OPTIONS : SO_EKORG FOR OR_EKORG,
                 SO_BSART FOR OR_BSART,
                 SO_LIFNR FOR OR_LIFNR.

SELECTION-SCREEN : END OF BLOCK B1 .

SELECTION-SCREEN: BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-003.

SELECT-OPTIONS: SO_AEDAT FOR OR_AEDAT.

SELECTION-SCREEN: END OF BLOCK B3.

*******************************************************************************************************************************************
********************                     AT SELECTION-SCREEN                                          ************************************
*******************************************************************************************************************************************
AT SELECTION-SCREEN .

  DATA : L_WERKS TYPE WERKS.
*  IF SO_WERKS IS NOT INITIAL .
*    SELECT SINGLE WERKS  FROM EKPO INTO L_WERKS WHERE WERKS = SO_WERKS .
*    IF SY-SUBRC <> 0.
*      MESSAGE 'Enter Valid Plant' TYPE 'E'.
**      MESSAGE E001(YMSG).
*    ENDIF.
*  ENDIF .
  IF SO_EKORG IS NOT INITIAL.
    SELECT SINGLE EKORG FROM T024E INTO L_EKORG WHERE EKORG IN SO_EKORG .
    IF SY-SUBRC NE 0.
      MESSAGE 'Enter valid Purchasing Organization ' TYPE 'E'.
    ENDIF.
  ENDIF.
  IF SO_BSART IS NOT INITIAL.
    SELECT SINGLE BSART FROM T161 INTO L_BSART WHERE BSART IN SO_BSART .
    IF SY-SUBRC NE 0 .
      MESSAGE 'Enter valid Purchasing Document Type' TYPE 'E'.
    ENDIF .
  ENDIF.
  IF SO_LIFNR IS NOT INITIAL .
    SELECT SINGLE LIFNR FROM LFA1 INTO L_LIFNR WHERE LIFNR IN SO_LIFNR .
    IF SY-SUBRC NE 0.
      MESSAGE 'Enter Valid Vendor Code' TYPE 'E'.
    ENDIF.
  ENDIF.

*******************************************************************************************************************************************
********************                     START-OF-SELECTION                                           ************************************
*******************************************************************************************************************************************
START-OF-SELECTION .
  PERFORM SELECT_DATA .
  PERFORM PROCESS_DATA .
  PERFORM FIELDCATALOGUE .

  PERFORM GRID_DISPLAY.
*  PERFORM BUILD_LAYOUT .
*  PERFORM BUILD_KEY .
*  PERFORM DISPLAY_DATA .



*&---------------------------------------------------------------------*
*&      FORM  SELECT_DATA
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM SELECT_DATA .

  SELECT
    EBELN
    LIFNR
    BSTYP
    BSART
    LOEKZ
    AEDAT
    FROM EKKO INTO CORRESPONDING FIELDS OF TABLE GT_EKKO
    WHERE LIFNR IN SO_LIFNR AND
          AEDAT IN SO_AEDAT AND
          BSTYP = 'F' AND
          EKORG IN SO_EKORG AND
          BSART IN SO_BSART.

  IF GT_EKKO[] IS NOT INITIAL .

    SELECT EBELN
           EBELP
           MATNR
           WERKS
           MENGE
           AEDAT
           FROM EKPO INTO CORRESPONDING FIELDS OF TABLE GT_EKPO FOR ALL ENTRIES IN GT_EKKO
           WHERE EBELN = GT_EKKO-EBELN AND
                 WERKS IN SO_WERKS AND
                 LOEKZ = ' '.

    IF GT_EKPO[] IS NOT INITIAL.

      SELECT MATNR
             MAKTX
             FROM MAKT INTO TABLE GT_MAKT FOR ALL ENTRIES IN GT_EKPO
             WHERE MATNR = GT_EKPO-MATNR .

      SELECT MATNR
             PLIFZ
             FROM MARC INTO TABLE GT_MARC FOR ALL ENTRIES IN GT_EKPO
             WHERE MATNR = GT_EKPO-MATNR.
    ENDIF.

    SELECT LIFNR
           NAME1
           FROM LFA1 INTO CORRESPONDING FIELDS OF TABLE GT_LFA1 FOR ALL ENTRIES IN GT_EKKO
           WHERE LIFNR = GT_EKKO-LIFNR.

    SELECT EBELN
           EBELP
           EINDT
           FROM EKET INTO TABLE GT_EKET FOR ALL ENTRIES IN GT_EKKO
           WHERE EBELN = GT_EKKO-EBELN .

    SELECT EBELN
           EBELP
           BELNR
           BUDAT
           BUZEI
           BEWTP
           BWART
           MENGE
           LFBNR
           FROM EKBE INTO CORRESPONDING FIELDS OF TABLE GT_EKBE FOR ALL ENTRIES IN GT_EKKO
           WHERE EBELN = GT_EKKO-EBELN AND
                 BEWTP = 'E' AND
                 BWART IN ('105', '101' , '106', '122').

    IF GT_EKBE[] IS NOT INITIAL.

      SELECT MBLNR
             ZEILE
             MATNR
             BWART
             LFBNR
             SMBLN
              EBELN FROM MSEG INTO CORRESPONDING FIELDS OF TABLE GT_MSEG FOR ALL ENTRIES IN GT_EKBE
             WHERE MBLNR = GT_EKBE-BELNR AND BWART = GT_EKBE-BWART.
*                   BWART = '106'.
    ENDIF.

    IF GT_EKBE[] IS NOT INITIAL.

*      SELECT BELNR
*      GJAHR
*      BLART
*      BLDAT
*      BUDAT FROM RBKP INTO CORRESPONDING FIELDS OF TABLE GT_RBKP FOR ALL ENTRIES IN  .
      SELECT BELNR
           GJAHR
          EBELN
          EBELP
          MATNR
          WERKS
          BUKRS
          MENGE
          MEINS
          LIFNR
          WRBTR
           FROM RSEG INTO TABLE GT_RSEG FOR ALL ENTRIES IN GT_MSEG WHERE EBELN = GT_MSEG-EBELN.
    ENDIF.
  ENDIF .
*
*  LOOP AT GT_MSEG INTO WA_MSEG WHERE BWART = '106'.
*    DELETE GT_EKBE WHERE BELNR = WA_MSEG-SMBLN.
*    DELETE GT_EKBE WHERE BELNR = WA_MSEG-MBLNR.
*  ENDLOOP.

ENDFORM.                    " SELECT_DATA



*&---------------------------------------------------------------------*
*&      FORM  PROCESS_DATA
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM PROCESS_DATA .

  LOOP AT GT_EKKO INTO WA_EKKO .

    LOOP AT GT_EKPO INTO WA_EKPO WHERE EBELN = WA_EKKO-EBELN.

      READ TABLE GT_MAKT INTO WA_MAKT WITH KEY MATNR = WA_EKPO-MATNR.
      IF SY-SUBRC = 0.
        WA_ITEM-MAKTX = WA_MAKT-MAKTX .
      ENDIF.

*      IF RB_LEAD = 'X'.
      READ TABLE GT_MARC INTO WA_MARC WITH KEY MATNR = WA_EKPO-MATNR .
      IF SY-SUBRC = 0.
        WA_ITEM-LEDTM = WA_MARC-PLIFZ.
        DELDATE = WA_EKKO-AEDAT + WA_ITEM-LEDTM.
      ENDIF.
*      ELSEIF RB_DEL = 'X'.
*        LOOP AT GT_EKET INTO WA_EKET WHERE EBELN = WA_EKKO-EBELN AND EBELP = WA_EKPO-EBELP.
*          IF LV_DATE IS INITIAL.
*            LV_DATE = WA_EKET-EINDT.
*          ELSEIF LV_DATE < WA_EKET-EINDT.
*            LV_DATE = WA_EKET-EINDT.
*          ENDIF.
*        ENDLOOP.
*        WA_ITEM-LEDTM = LV_DATE - WA_EKKO-AEDAT.
*        DELDATE = WA_EKKO-AEDAT + WA_ITEM-LEDTM.
*      ENDIF.

      READ TABLE GT_LFA1 INTO WA_LFA1 WITH KEY LIFNR = WA_EKKO-LIFNR.
      IF SY-SUBRC = 0.
        WA_ITEM-NAME1 = WA_LFA1-NAME1 .
        WA_ITEM-LIFNR = WA_LFA1-LIFNR .
      ENDIF.

      WA_ITEM-EBELN = WA_EKPO-EBELN .
      WA_ITEM-AEDAT = WA_EKKO-AEDAT .
      WA_ITEM-MENGE = WA_EKPO-MENGE .

      LOOP AT GT_EKBE INTO WA_EKBE WHERE EBELN = WA_EKPO-EBELN AND EBELP = WA_EKPO-EBELP ."AND BWART = '105'.

        WA_ITEM-BELNR = WA_EKBE-BELNR .
        WA_ITEM-BUDAT = WA_EKBE-BUDAT .

        READ TABLE GT_EKBE INTO WA_EKBE1 WITH KEY LFBNR = WA_EKBE-LFBNR BWART = '122'.
        IF SY-SUBRC = 0.
          WA_ITEM-QTYREC = WA_EKBE-MENGE - WA_EKBE1-MENGE.
        ELSE.
          WA_ITEM-QTYREC = WA_EKBE-MENGE .
        ENDIF.

        WA_ITEM-ZDAY_DEL = WA_EKKO-AEDAT - WA_EKBE-BUDAT.

        READ TABLE GT_RSEG INTO WA_RSEG WITH KEY EBELN = WA_EKBE-EBELN.
        WA_ITEM-BELNR = WA_RSEG-BELNR.
        WA_ITEM-BUDAT = WA_EKBE-BUDAT.
        WA_ITEM-WRBTR = WA_RSEG-WRBTR.
        WA_ITEM-RMENGE = WA_RSEG-MENGE.
*        MOVE-CORRESPONDING WA_RSEG TO WA_ITEM.
*        IF WA_ITEM-ZDAY_DEL < 0.
*          WA_ITEM-ZDAY_DEL = 0.
*        ENDIF.

*        IF WA_ITEM-LEDTM NE 0.
*          WA_ITEM-ZDEL_PER = ( WA_ITEM-ZDAY_DEL / WA_ITEM-LEDTM ) * 100.
*        ELSE.
*          IF WA_ITEM-ZDAY_DEL = 0.
*            WA_ITEM-ZDEL_PER = 0.
*          ELSE.
*            WA_ITEM-ZDEL_PER = 100.
*          ENDIF.
*        ENDIF.

*        WA_ITEM-ZPER_PRO = 100 - WA_ITEM-ZDEL_PER.

        T_LSMNG = T_LSMNG + WA_EKBE-MENGE.

        APPEND WA_ITEM TO GT_ITEM.
        CLEAR: WA_EKBE.
      ENDLOOP.

*      IF WA_ITEM-MENGE > T_LSMNG.
*        IF SY-DATUM >= DELDATE.
*          WA_ITEM-ZDAY_DEL = SY-DATUM - DELDATE.
*        ELSE.
*          WA_ITEM-ZDAY_DEL = 0.
*        ENDIF.

*        IF WA_ITEM-LEDTM NE 0.
*          WA_ITEM-ZDEL_PER = ( WA_ITEM-ZDAY_DEL / WA_ITEM-LEDTM ) * 100.
*        ELSE.
*          IF WA_ITEM-ZDAY_DEL = 0.
*            WA_ITEM-ZDEL_PER = 0.
*          ELSE.
*            WA_ITEM-ZDEL_PER = 100.
*          ENDIF.
*        ENDIF.
*
*        WA_ITEM-ZPER_PRO = 100 - WA_ITEM-ZDEL_PER.

      CLEAR: WA_ITEM-BELNR, WA_ITEM-QTYREC, WA_ITEM-BUDAT.
      APPEND WA_ITEM TO GT_ITEM.
*      ENDIF.

      CLEAR: WA_ITEM, WA_MARC, WA_MAKT, WA_EKET,DELDATE,T_LSMNG, LV_DATE.
    ENDLOOP.

*    READ TABLE GT_LFA1 INTO WA_LFA1 WITH KEY LIFNR = WA_EKKO-LIFNR.
*    IF SY-SUBRC = 0.
*      WA_HEADER-LIFNR = WA_LFA1-LIFNR .
*      WA_HEADER-NAME1 = WA_LFA1-NAME1 .
*    ENDIF

*    APPEND WA_HEADER TO GT_HEADER.
*    CLEAR: WA_HEADER, WA_LFA1.
  ENDLOOP.

*  SORT GT_HEADER BY LIFNR.
  SORT GT_ITEM BY LIFNR EBELN.

*  DELETE ADJACENT DUPLICATES FROM GT_HEADER COMPARING LIFNR.

ENDFORM.                    " PROCESS_DATA

*&---------------------------------------------------------------------*
*&      FORM  FIELDCATALOGUE
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM FIELDCATALOGUE .

*  PERFORM ALV_LAYOUT USING 1   'VENDOR NUMBER'           'LIFNR'    'GT_HEADER' .
*  PERFORM ALV_LAYOUT USING 2   ' NAME'                   'NAME1'    'GT_HEADER' .
**  PERFORM ALV_LAYOUT USING 3   'AVERAGE'                 'ZAVG_DEL' 'GT_HEADER' .

  PERFORM ALV_LAYOUT USING 1   'VENDOR NUMBER'           'LIFNR'    'GT_ITEM' .
  PERFORM ALV_LAYOUT USING 2   ' NAME'                   'NAME1'    'GT_ITEM' .
  PERFORM ALV_LAYOUT USING 3   'PO.Number'               'EBELN'     'GT_ITEM' .                "PURCHASING DOCUMENT NUMBER
  PERFORM ALV_LAYOUT USING 4   ' Date '                  'AEDAT'     'GT_ITEM' .                "DATE ON WHICH RECORD WAS CREATED
  PERFORM ALV_LAYOUT USING 5   ' Material Description'   'MAKTX'     'GT_ITEM' .
  PERFORM ALV_LAYOUT USING 6   ' Lead Time'              'LEDTM'     'GT_ITEM' .                "PLANNED DELIVERY TIME IN DAYS
  PERFORM ALV_LAYOUT USING 7   ' Ord. Qty'               'MENGE'     'GT_ITEM' .
  PERFORM ALV_LAYOUT USING 8   ' GRN. No'                'BELNR'     'GT_ITEM' .                "NUMBER OF MATERIAL DOCUMENT
  PERFORM ALV_LAYOUT USING 9   ' GRN. Date'              'BUDAT'     'GT_ITEM' .                "POSTING DATE IN THE DOCUMENT
  PERFORM ALV_LAYOUT USING 10   ' Received Qty '          'QTYREC'    'GT_ITEM' .                "Quantity of Goods Received
*  PERFORM ALV_LAYOUT USING 11   ' Percentage-Procurement' 'ZPER_PRO'  'GT_ITEM' .
  PERFORM ALV_LAYOUT USING 12  ' Days Delayed'           'ZDAY_DEL'  'GT_ITEM' .
*  PERFORM ALV_LAYOUT USING 13  ' Delayed-Percent'        'ZDEL_PER'  'GT_ITEM' .
  PERFORM ALV_LAYOUT USING 14   'Bill. No' 'BELNR'  'GT_ITEM' .
  PERFORM ALV_LAYOUT USING 15  ' Bill. Date'        'BUDAT'  'GT_ITEM' .
   PERFORM ALV_LAYOUT USING 16  'Bill.Qty'        'RMENGE'  'GT_ITEM' .
  PERFORM ALV_LAYOUT USING 17  'Amount'           'WRBTR'  'GT_ITEM' .



ENDFORM.                    " FIELDCATALOGUEs


*&---------------------------------------------------------------------*
*&      FORM  ALV_LAYOUT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM ALV_LAYOUT USING P1 P2 P3 P4.

  WA_FIELDCATALOGUE-COL_POS    = P1.
  WA_FIELDCATALOGUE-SELTEXT_L  = P2.
  WA_FIELDCATALOGUE-FIELDNAME  = P3.
  WA_FIELDCATALOGUE-TABNAME    = P4.
*  WA_FIELDCATALOGUE-fieldname  = 'RADIO'.

  APPEND WA_FIELDCATALOGUE TO LT_FIELDCATALOGUE .
  CLEAR WA_FIELDCATALOGUE .
ENDFORM.                    " ALV_LAYOUT

*&---------------------------------------------------------------------*
*&      FORM  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM DISPLAY_DATA .


  CALL FUNCTION 'REUSE_ALV_HIERSEQ_LIST_DISPLAY'
    EXPORTING
*   I_INTERFACE_CHECK              = ' '
*   I_CALLBACK_PROGRAM             =
*   I_CALLBACK_PF_STATUS_SET       = ' '
*   I_CALLBACK_USER_COMMAND        = ' '
     IS_LAYOUT                      = IT_LAYOUT
     IT_FIELDCAT                    = LT_FIELDCATALOGUE
*   IT_EXCLUDING                   =
*   IT_SPECIAL_GROUPS              =
*   IT_SORT                        =
*   IT_FILTER                      =
*   IS_SEL_HIDE                    =
*   I_SCREEN_START_COLUMN          = 0
*   I_SCREEN_START_LINE            = 0
*   I_SCREEN_END_COLUMN            = 0
*   I_SCREEN_END_LINE              = 0
     I_DEFAULT                      = 'X'
     I_SAVE                         = 'X '
     IS_VARIANT                     = LS_VARIANT
*   IT_EVENTS                      =
*   IT_EVENT_EXIT                  =
      I_TABNAME_HEADER               = 'GT_HEADER'
      I_TABNAME_ITEM                 = 'GT_ITEM'
*   I_STRUCTURE_NAME_HEADER        =
*   I_STRUCTURE_NAME_ITEM          =
      IS_KEYINFO                     = KEY
*   IS_PRINT                       =
*   IS_REPREP_ID                   =
*   I_BYPASSING_BUFFER             =
*   I_BUFFER_ACTIVE                =
*   IR_SALV_HIERSEQ_ADAPTER        =
*   IT_EXCEPT_QINFO                =
*   I_SUPPRESS_EMPTY_DATA          = ABAP_FALSE
* IMPORTING
*   E_EXIT_CAUSED_BY_CALLER        =
*   ES_EXIT_CAUSED_BY_USER         =
    TABLES
      T_OUTTAB_HEADER                = GT_HEADER[]
      T_OUTTAB_ITEM                  = GT_ITEM[]
* EXCEPTIONS
*   PROGRAM_ERROR                  = 1
*   OTHERS                         = 2
            .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " DISPLAY_DATA



*&---------------------------------------------------------------------*
*&      FORM  BUILD_KEY
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM BUILD_KEY .
*KEY INFOMATION FOR THE HEADER AND ITEM TABLE

  KEY-HEADER01 = 'LIFNR'.
  KEY-ITEM01 = 'LIFNR'.
ENDFORM.                    " BUILD_KEY


*&---------------------------------------------------------------------*
*&      FORM  BUILD_LAYOUT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM BUILD_LAYOUT .

*TO EXPAND THE HEADER TABLE FOR ITEM DETAILS




  IT_LAYOUT-EXPAND_FIELDNAME = 'EXPAND'.

  IT_LAYOUT-WINDOW_TITLEBAR = 'VENDOR DELIVERY PERFORMANCE'.
  IT_LAYOUT-LIGHTS_TABNAME = 'GT_ITEM'.
  IT_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
ENDFORM.                    " BUILD_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  GRID_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GRID_DISPLAY .

   LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  LAYOUT-ZEBRA = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
   EXPORTING
*   I_INTERFACE_CHECK                 = ' '
*   I_BYPASSING_BUFFER                = ' '
*   I_BUFFER_ACTIVE                   = ' '
     I_CALLBACK_PROGRAM                = SY-CPROG
*   I_CALLBACK_PF_STATUS_SET          = ' '
*   I_CALLBACK_USER_COMMAND           = ' '
*   I_CALLBACK_TOP_OF_PAGE            = ' '
*   I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*   I_CALLBACK_HTML_END_OF_LIST       = ' '
*   I_STRUCTURE_NAME                  =
*   I_BACKGROUND_ID                   = ' '
*   I_GRID_TITLE                      =
*   I_GRID_SETTINGS                   =
    IS_LAYOUT                         = LAYOUT
     IT_FIELDCAT                       = LT_FIELDCATALOGUE
*   IT_EXCLUDING                      =
*   IT_SPECIAL_GROUPS                 =
*   IT_SORT                           =
*   IT_FILTER                         =
*   IS_SEL_HIDE                       =
     I_DEFAULT                         = 'X'
     I_SAVE                            = 'X'
     IS_VARIANT                        = LS_VARIANT
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
      T_OUTTAB                          = GT_ITEM[]
* EXCEPTIONS
*   PROGRAM_ERROR                     = 1
*   OTHERS                            = 2
            .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " GRID_DISPLAY
