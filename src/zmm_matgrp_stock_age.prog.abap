*&---------------------------------------------------------------------*
*& Report  ZRT_MAT_STOCK_AGE
*&
*&---------------------------------------------------------------------*
*&Functional                   : Mr.Govindarajan M                     *
*& Developer                   : Mr.Govindarajan M                     *
*& Created On                  : 12 Aug 2014                           *
*& Title                       : Material Stock Details                *
*& Report Name                 : ZRT_MAT_STOCK_AGE                       *
*& Development Id              : kpabap                                *
*& Solman call No              :                                       *
*& Transport Request           :                                       *
*& Related Information         : Display The Stock Details By          *
*                                Material Group                        *
*&---------------------------------------------------------------------*

REPORT  ZMM_MATGRP_STOCK_AGE.

TYPE-POOLS: SLIS.

*&---------------------------------------------------------------------*
*&  Structure & Internal Table Decleration
*&---------------------------------------------------------------------*

TYPES: BEGIN OF GS_MCHB,
       MATNR TYPE MCHB-MATNR,              " Material Code
       WERKS TYPE MCHB-WERKS,              " Valuation Area / Plant
       CHARG TYPE MCHB-CHARG,              " Batch
       ERSDA TYPE MCHB-ERSDA,              " Created On
       CLABS TYPE MCHB-CLABS,               " Stock Qty
       END OF GS_MCHB.

DATA: GT_MCHB TYPE TABLE OF GS_MCHB,
      WA_MCHB TYPE GS_MCHB.

TYPES : BEGIN OF GS_MBEW,                 "Changes on 03/09/2014
        MATNR TYPE MBEW-MATNR,            "Material
        BWKEY TYPE MBEW-BWKEY,            "Valuation Area / Plant
        VPRSV TYPE MBEW-VPRSV,            " Price Control
        VERPR TYPE MBEW-VERPR,            "Moving Price
        STPRS TYPE MBEW-STPRS,            "Standard price
        END OF GS_MBEW.

DATA: GT_MBEW TYPE TABLE OF GS_MBEW,
      WA_MBEW TYPE GS_MBEW.


TYPES: BEGIN OF GS_MARA,
       MATNR TYPE MARA-MATNR,              " Material Code
       MATKL TYPE MARA-MATKL,              " Material Group
       MEINS TYPE MARA-MEINS,              " UOM
       SPART TYPE MARA-SPART,              " Division
       VOLUM TYPE MARA-VOLUM,               " Volume
       END OF GS_MARA.

DATA: GT_MARA TYPE TABLE OF GS_MARA,
      WA_MARA TYPE GS_MARA.

TYPES :BEGIN OF GS_MVKE,
       MATNR TYPE MVKE-MATNR,
       MVGR1 TYPE MVKE-MVGR1,
       END OF GS_MVKE.

DATA : GT_MVKE TYPE TABLE OF GS_MVKE,
       WA_MVKE TYPE GS_MVKE.

TYPES : BEGIN OF GS_TVM1T,
        SPRAS TYPE TVM1T-SPRAS,
        MVGR1 TYPE TVM1T-MVGR1,
        BEZEI TYPE TVM1T-BEZEI,
        END OF GS_TVM1T.

DATA : GT_TVM1T TYPE TABLE OF GS_TVM1T,
       WA_TVM1T TYPE GS_TVM1T.

TYPES: BEGIN OF GS_MAKT,
       MATNR TYPE MAKT-MATNR,              " Material Number
       MAKTX TYPE MAKT-MAKTX,              " Material Description
       END OF GS_MAKT.

DATA: GT_MAKT TYPE TABLE OF GS_MAKT,
      WA_MAKT TYPE GS_MAKT.


TYPES : BEGIN OF GS_T001W,
        WERKS TYPE T001W-WERKS,
        NAME1 TYPE T001W-NAME1,
        END OF GS_T001W.

DATA : GT_T001W TYPE TABLE OF GS_T001W,
       WA_T001W TYPE GS_T001W.

TYPES: BEGIN OF GS_FINAL,
       MVGR1 TYPE MVKE-MVGR1,             "
       BEZEI  TYPE TVM1T-BEZEI,        "
       MATNR TYPE MCHB-MATNR,              " Material Number
       WERKS TYPE MCHB-WERKS,              " Plant
       SPART TYPE MARA-SPART,              " Division
       NAME1 TYPE T001W-NAME1,             " Plant Descriprtion
       MAKTX TYPE MAKT-MAKTX,              " Material Description
       CHARG TYPE MCHB-CHARG,              " Batch No
       ERSDA TYPE MCHB-ERSDA,              " Posting Date / Crated On
       CLABS TYPE P DECIMALS 2,              " Stock Qty
       VOLUM TYPE MARA-VOLUM,              " Volume
       VCLABS TYPE MCHB-CLABS,             " Stock In Ltrs

*       VERPR TYPE MBEW-VERPR,              "Moving Price
*       PRICE TYPE MBEW-STPRS,              "Standard price 03/09/2014
       VPRSV TYPE MBEW-VPRSV,              " Price Control

       ZAGE1 TYPE P DECIMALS 2,             " A1 0-30 Days
       VZAGE1 TYPE P DECIMALS 2,            " Stock In Ltrs
       ZAGE2 TYPE P DECIMALS 2,             " A2 31-60 Days
       VZAGE2 TYPE P DECIMALS 2,            " Stock In Ltrs
       ZAGE3 TYPE P DECIMALS 2,             " A3 61-90 Days
       VZAGE3 TYPE P DECIMALS 2,            " Stock In Ltrs
       ZAGE4 TYPE P DECIMALS 2,             " A3 91-180 Days
       VZAGE4 TYPE P DECIMALS 2,            " Stock In Ltrs
       ZAGE5 TYPE P DECIMALS 2,             " A5 Above 180 Days
       VZAGE5 TYPE P DECIMALS 2,            " Stock In Ltrs

       PRICE TYPE P DECIMALS 2,       " STOCKVALUE

       STOCKVALUE TYPE P DECIMALS 2,       " STOCKVALUE
       STOCKVALUE1 TYPE P DECIMALS 2,       " STOCKVALUE
       STOCKVALUE2 TYPE P DECIMALS 2,       " STOCKVALUE
       STOCKVALUE3 TYPE P DECIMALS 2,       " STOCKVALUE
       STOCKVALUE4 TYPE P DECIMALS 2,       " STOCKVALUE
       STOCKVALUE5 TYPE P DECIMALS 2,       " STOCKVALUE
       STOCKVALUE6 TYPE P DECIMALS 2,       " STOCKVALUE


       STOCKCASE1 TYPE P DECIMALS 2,       " STOCKVALUE
       STOCKCASE2 TYPE P DECIMALS 2,       " STOCKVALUE
       STOCKCASE3 TYPE P DECIMALS 2,       " STOCKVALUE
       STOCKCASE4 TYPE P DECIMALS 2,       " STOCKVALUE
       STOCKCASE5 TYPE P DECIMALS 2,       " STOCKVALUE
       STOCKCASE6 TYPE P DECIMALS 2,       " STOCKVALUE


       END OF GS_FINAL.

DATA: GT_FINAL TYPE TABLE OF GS_FINAL,
      WA_FINAL TYPE GS_FINAL.



DATA: LV_YEAR TYPE BKPF-GJAHR,
      LV_MONTH TYPE BKPF-MONAT,
      LV_SPART TYPE MARA-SPART,
      T_DAYS TYPE I.

*&---------------------------------------------------------------------*
*&  ALV Structure & Internal Table
*&---------------------------------------------------------------------*

DATA: GT_FCAT TYPE SLIS_T_FIELDCAT_ALV,
      WA_FCAT TYPE SLIS_FIELDCAT_ALV,
      IT_LAYOUT TYPE SLIS_LAYOUT_ALV,
      GT_EVENTS TYPE SLIS_T_EVENT,
      WA_EVENTS TYPE SLIS_ALV_EVENT,
      KEY TYPE SLIS_KEYINFO_ALV,
       IT_SORT TYPE SLIS_T_SORTINFO_ALV,
      WA_SORT LIKE LINE OF IT_SORT.


DATA : LAYOUT TYPE SLIS_LAYOUT_ALV.

DATA: LS_VARIANT TYPE DISVARIANT.
LS_VARIANT-REPORT = SY-REPID.

DATA : LV_WERKS TYPE T001W-WERKS,
       LV_MATNR  TYPE MARA-MATNR,
       LV_MVGR1  TYPE MVKE-MVGR1.

*&---------------------------------------------------------------------*
*&  Selection Screen Fields
*&---------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-101.
SELECT-OPTIONS : SO_WERKS FOR LV_WERKS ,
                 SO_MVGR1 FOR LV_MVGR1,
                 SO_MATNR FOR LV_MATNR,
                 SO_SPART FOR LV_SPART.
PARAMETERS : P_DATE TYPE SY-DATUM DEFAULT SY-DATUM.
SELECTION-SCREEN: END OF BLOCK B1.


*&---------------------------------------------------------------------*
*&  Main Logic
*&---------------------------------------------------------------------*


SELECT
  MATNR
  MATKL
  MEINS
  SPART
  VOLUM FROM MARA INTO TABLE GT_MARA
  WHERE  MATNR IN SO_MATNR AND SPART IN SO_SPART.




SELECT
  MATNR
  MAKTX FROM MAKT INTO TABLE GT_MAKT FOR ALL ENTRIES IN GT_MARA
  WHERE MATNR = GT_MARA-MATNR.

SELECT
 MATNR
   WERKS
   CHARG
   ERSDA
   CLABS FROM MCHB INTO TABLE GT_MCHB FOR ALL ENTRIES IN GT_MARA
  WHERE MATNR = GT_MARA-MATNR AND WERKS IN SO_WERKS  .

IF GT_MCHB[] IS NOT INITIAL.

  SELECT
       MATNR
       BWKEY
       VPRSV
       VERPR
       STPRS FROM MBEW INTO TABLE GT_MBEW FOR ALL ENTRIES IN GT_MCHB
      WHERE MATNR = GT_MCHB-MATNR  AND BWKEY = GT_MCHB-WERKS AND ( VPRSV = 'S' OR VPRSV = 'V' ).

  IF GT_MBEW[] IS NOT INITIAL.

    SELECT WERKS NAME1 FROM T001W INTO TABLE GT_T001W
         FOR ALL ENTRIES IN GT_MCHB WHERE WERKS = GT_MCHB-WERKS.
  ENDIF.



  SELECT MATNR
         MVGR1
    FROM MVKE
    INTO TABLE GT_MVKE FOR ALL ENTRIES IN GT_MCHB WHERE MATNR = GT_MCHB-MATNR .
ENDIF.

IF GT_MVKE[] IS NOT INITIAL.
SELECT SPRAS
  MVGR1
   BEZEI
  FROM TVM1T INTO TABLE GT_TVM1T  FOR ALL ENTRIES IN GT_MVKE
  WHERE MVGR1 = GT_MVKE-MVGR1 AND SPRAS = 'E' AND  MVGR1 IN SO_MVGR1.
ENDIF.





LOOP AT  GT_MCHB INTO WA_MCHB .
WA_FINAL-MATNR = WA_MCHB-MATNR.
WA_FINAL-WERKS = WA_MCHB-WERKS.
WA_FINAL-CHARG = WA_MCHB-CHARG.
WA_FINAL-ERSDA = WA_MCHB-ERSDA.
WA_FINAL-CLABS = WA_MCHB-CLABS.

READ TABLE GT_MARA INTO WA_MARA WITH KEY MATNR = WA_MCHB-MATNR.
WA_FINAL-SPART = WA_MARA-SPART.
WA_FINAL-VOLUM = WA_MARA-VOLUM.
READ TABLE GT_MAKT INTO WA_MAKT WITH KEY MATNR = WA_MCHB-MATNR.

WA_FINAL-MAKTX = WA_MAKT-MAKTX.

READ TABLE GT_T001W INTO WA_T001W WITH KEY WERKS = WA_MCHB-WERKS.
WA_FINAL-NAME1 = WA_T001W-NAME1.

  READ TABLE GT_MVKE INTO WA_MVKE WITH KEY  MATNR = WA_MCHB-MATNR.
  WA_FINAL-MVGR1 = WA_MVKE-MVGR1.

READ TABLE GT_MBEW INTO WA_MBEW WITH KEY MATNR = WA_MCHB-MATNR BWKEY = WA_MCHB-WERKS .
IF SY-SUBRC = 0.

  WA_FINAL-VPRSV = WA_MBEW-VPRSV.

  IF WA_MBEW-VPRSV = 'S'.
    WA_FINAL-PRICE = WA_MBEW-STPRS.
  ELSEIF WA_MBEW-VPRSV = 'V'.
    WA_FINAL-PRICE = WA_MBEW-VERPR.

  ENDIF.
ENDIF.
CALL FUNCTION 'HR_99S_INTERVAL_BETWEEN_DATES'
  EXPORTING
    BEGDA = WA_MCHB-ERSDA
    ENDDA = P_DATE
  IMPORTING
    DAYS  = T_DAYS.

IF T_DAYS <= 30.
  WA_FINAL-ZAGE1 = WA_MCHB-CLABS.
ELSEIF T_DAYS BETWEEN 31 AND 60.
  WA_FINAL-ZAGE2 = WA_MCHB-CLABS.
ELSEIF T_DAYS BETWEEN 61 AND 90.
  WA_FINAL-ZAGE3 = WA_MCHB-CLABS.
ELSEIF T_DAYS BETWEEN 91 AND 180.
  WA_FINAL-ZAGE4 = WA_MCHB-CLABS.
ELSEIF T_DAYS > 180.
  WA_FINAL-ZAGE5 = WA_MCHB-CLABS.
ENDIF.

SHIFT WA_FINAL-MATNR LEFT DELETING LEADING '0'.
APPEND WA_FINAL TO GT_FINAL.
CLEAR: WA_FINAL.

DELETE GT_FINAL WHERE CLABS EQ 0.

ENDLOOP.

LOOP AT GT_FINAL INTO WA_FINAL.
WA_FINAL-VCLABS = WA_FINAL-CLABS *  WA_FINAL-VOLUM.
WA_FINAL-VZAGE1 =  WA_FINAL-ZAGE1 * WA_FINAL-VOLUM.
WA_FINAL-VZAGE2 =  WA_FINAL-ZAGE2 * WA_FINAL-VOLUM.
WA_FINAL-VZAGE3 =  WA_FINAL-ZAGE3 * WA_FINAL-VOLUM.
WA_FINAL-VZAGE4 =  WA_FINAL-ZAGE4 * WA_FINAL-VOLUM.
WA_FINAL-VZAGE5 =  WA_FINAL-ZAGE5 * WA_FINAL-VOLUM.

WA_FINAL-STOCKVALUE = WA_FINAL-CLABS * WA_FINAL-PRICE.

IF WA_FINAL-VOLUM NE 0.
  WA_FINAL-STOCKVALUE1 = WA_FINAL-PRICE / WA_FINAL-VOLUM.
ENDIF.

WA_FINAL-STOCKVALUE2 = WA_FINAL-STOCKVALUE1 * WA_FINAL-VZAGE1 .
WA_FINAL-STOCKVALUE3 = WA_FINAL-STOCKVALUE1 * WA_FINAL-VZAGE2 .
WA_FINAL-STOCKVALUE4 = WA_FINAL-STOCKVALUE1 * WA_FINAL-VZAGE3 .
WA_FINAL-STOCKVALUE5 = WA_FINAL-STOCKVALUE1 * WA_FINAL-VZAGE4 .
WA_FINAL-STOCKVALUE6 = WA_FINAL-STOCKVALUE1 * WA_FINAL-VZAGE5 .

WA_FINAL-STOCKCASE1 = WA_FINAL-PRICE * WA_FINAL-ZAGE1 .
WA_FINAL-STOCKCASE2 = WA_FINAL-PRICE * WA_FINAL-ZAGE2 .
WA_FINAL-STOCKCASE3 = WA_FINAL-PRICE * WA_FINAL-ZAGE3 .
WA_FINAL-STOCKCASE4 = WA_FINAL-PRICE * WA_FINAL-ZAGE4 .
WA_FINAL-STOCKCASE5 = WA_FINAL-PRICE * WA_FINAL-ZAGE5 .

READ TABLE GT_TVM1T INTO WA_TVM1T WITH KEY  MVGR1  = WA_FINAL-MVGR1.
IF SY-SUBRC = 0.
  WA_FINAL-BEZEI  = WA_TVM1T-BEZEI .
ENDIF.
MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING  VCLABS VZAGE1 VZAGE2
VZAGE3 VZAGE4 VZAGE5 STOCKVALUE STOCKVALUE1 STOCKVALUE2 STOCKVALUE3
STOCKVALUE4 STOCKVALUE5 STOCKVALUE6 STOCKCASE1 STOCKCASE2 STOCKCASE3 STOCKCASE4 STOCKCASE5 BEZEI  .
CLEAR WA_FINAL.



ENDLOOP.

IF SO_MVGR1-LOW IS NOT INITIAL.
DELETE GT_FINAL WHERE MVGR1 EQ ' '.
ENDIF.

*&---------------------------------------------------------------------*
*&  ALV Layout
*&---------------------------------------------------------------------*
PERFORM ALV_LAYOUT USING 1 'Material Group' 'MVGR1' 'GT_FINAL' ' ' '' ''.
PERFORM ALV_LAYOUT USING 1 'Material Group Des.' 'BEZEI' 'GT_FINAL' ' ' '' ''.
PERFORM ALV_LAYOUT USING 3 'Plant Code' 'WERKS' 'GT_FINAL' ' ' '' ''.
PERFORM ALV_LAYOUT USING 5 'Plant Name' 'NAME1' 'GT_FINAL' ' ' '' ''.
PERFORM ALV_LAYOUT USING 6 'Division' 'SPART' 'GT_FINAL' ' ' '' ''.
PERFORM ALV_LAYOUT USING 8 'Material Code' 'MATNR' 'GT_FINAL' ' ' '' ''.
PERFORM ALV_LAYOUT USING 9 'Material Description' 'MAKTX' 'GT_FINAL' ' ' '' ''.
PERFORM ALV_LAYOUT USING 11 'Batch' 'CHARG' 'GT_FINAL' ' ' '' ''.
PERFORM ALV_LAYOUT USING 13 'Created On' 'ERSDA' 'GT_FINAL' '' '' ''.
PERFORM ALV_LAYOUT USING 16 'Volume' 'VOLUM' 'GT_FINAL' '' 'X' ''.
PERFORM ALV_LAYOUT USING 17  'Stock Qty' 'CLABS' 'GT_FINAL' 'X' 'X' ''.
PERFORM ALV_LAYOUT USING 18  'Price Ind.' 'VPRSV' 'GT_FINAL' '' 'X' ''.
PERFORM ALV_LAYOUT USING 20  'Unit Price' 'PRICE' 'GT_FINAL' '' 'X' ''.
PERFORM ALV_LAYOUT USING 21  'Stock Value' 'STOCKVALUE' 'GT_FINAL' 'X' 'X' ''.

PERFORM ALV_LAYOUT USING 22  'Stock Qty Ltrs' 'VCLABS' 'GT_FINAL' 'X' 'X' ''.
PERFORM ALV_LAYOUT USING 23  'Price in Ltrs' 'STOCKVALUE1' 'GT_FINAL' '' 'X' ''.

PERFORM ALV_LAYOUT USING 26 '< 30 Days(Case)' 'ZAGE1' 'GT_FINAL' 'X' 'X' ''.
PERFORM ALV_LAYOUT USING 27 '< 30 Days Stock Value' 'STOCKCASE1' 'GT_FINAL' 'X' 'X' 'X'.

PERFORM ALV_LAYOUT USING 29 '< 30 Stock Qty Ltrs' 'VZAGE1' 'GT_FINAL' 'X' 'X' ''.
PERFORM ALV_LAYOUT USING 30 '< 30 Stock Qty Value' 'STOCKVALUE2' 'GT_FINAL' 'X' 'X' ''.

PERFORM ALV_LAYOUT USING 31 '31-60 Days(Case)' 'ZAGE2' 'GT_FINAL' 'X' 'X' ''.
PERFORM ALV_LAYOUT USING 32 '31-60 Days Stock Value' 'STOCKCASE2' 'GT_FINAL' 'X' 'X' 'X'.

PERFORM ALV_LAYOUT USING 35 '31-60 Stock Qty Ltrs' 'VZAGE2' 'GT_FINAL' 'X' 'X' ''.
PERFORM ALV_LAYOUT USING 36 '31-60 Stock Qty Value' 'STOCKVALUE3' 'GT_FINAL' 'X' 'X' ''.

PERFORM ALV_LAYOUT USING 37 '61-90 Days(Case)' 'ZAGE3' 'GT_FINAL' 'X' 'X' ''.
PERFORM ALV_LAYOUT USING 39 '61-90 Days Stock Value' 'STOCKCASE3' 'GT_FINAL' 'X' 'X' 'X'.

PERFORM ALV_LAYOUT USING 40 '61-90Stock Qty Ltrs' 'VZAGE3' 'GT_FINAL' 'X' 'X' ''.
PERFORM ALV_LAYOUT USING 41 '61-90 Stock Qty Value' 'STOCKVALUE4' 'GT_FINAL' 'X' 'X' ''.

PERFORM ALV_LAYOUT USING 42 '91-180 Days(Case)' 'ZAGE4' 'GT_FINAL' 'X' 'X' ''.
PERFORM ALV_LAYOUT USING 43 '91-180 Days Stock Value' 'STOCKCASE4' 'GT_FINAL' 'X' 'X' 'X'.

PERFORM ALV_LAYOUT USING 44 '91-180 Stock Qty Ltrs' 'VZAGE4' 'GT_FINAL' 'X' 'X' ''.
PERFORM ALV_LAYOUT USING 45 '91-180 Stock Qty Value' 'STOCKVALUE5' 'GT_FINAL' 'X' 'X' ''.

PERFORM ALV_LAYOUT USING 46 'Above 180 Days(Case)' 'ZAGE5' 'GT_FINAL' 'X' 'X' ''.
PERFORM ALV_LAYOUT USING 48 'Above 180 Days Stock Value' 'STOCKCASE5' 'GT_FINAL' 'X' 'X' 'X'.

PERFORM ALV_LAYOUT USING 49 '>180 Stock Qty Ltrs' 'VZAGE5' 'GT_FINAL' 'X' 'X' ''.
PERFORM ALV_LAYOUT USING 50 '>180 Stock Qty Value' 'STOCKVALUE6' 'GT_FINAL' 'X' 'X' ''.


WA_SORT-FIELDNAME = 'MVGR1'.
WA_SORT-SUBTOT = 'X'.
WA_SORT-UP = 'X'.
*  WA_SORT-GROUP = 'X'.
APPEND WA_SORT TO IT_SORT.
CLEAR WA_SORT.
*
WA_SORT-FIELDNAME = 'BEZEI'.
*WA_SORT-SUBTOT = 'X'.
WA_SORT-UP = 'X'.
*  WA_SORT-GROUP = 'X'.
APPEND WA_SORT TO IT_SORT.
CLEAR WA_SORT.

WA_SORT-FIELDNAME = 'WERKS'.
*WA_SORT-SUBTOT = 'X'.
WA_SORT-UP = 'X'.
*  WA_SORT-GROUP = 'X'.
APPEND WA_SORT TO IT_SORT.
CLEAR WA_SORT.

*&---------------------------------------------------------------------*
*&   ALV Hierarchical Display
*&---------------------------------------------------------------------*


PERFORM ALV_GRID_DISPLAY.

*&---------------------------------------------------------------------*
*&      Form  ALV_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P1  text
*      -->P2  text
*      -->P3  text
*      -->P4  text
*      -->P5  text
*----------------------------------------------------------------------*
FORM ALV_LAYOUT  USING    P1 P2 P3 P4 P5 P6 P7.
  CLEAR WA_FCAT.
  WA_FCAT-COL_POS = P1.
  WA_FCAT-SELTEXT_L = P2.
  WA_FCAT-FIELDNAME = P3.
  WA_FCAT-TABNAME = P4.
  WA_FCAT-DO_SUM = P5.
  WA_FCAT-NO_ZERO = P6.
  WA_FCAT-NO_OUT = P7.

  APPEND WA_FCAT TO GT_FCAT.

ENDFORM.                    " ALV_LAYOUT


*&---------------------------------------------------------------------*
*&      Form  ALV_GRID_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ALV_GRID_DISPLAY .


   LAYOUT-COLWIDTH_OPTIMIZE = 'X'.                                  "Added by S.Savariar as on 20/10/2014.
   LAYOUT-ZEBRA = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
   EXPORTING
*     I_INTERFACE_CHECK                 = ' '
*     I_BYPASSING_BUFFER                = ' '
*     I_BUFFER_ACTIVE                   = ' '
     I_CALLBACK_PROGRAM                = SY-REPID
*     I_CALLBACK_PF_STATUS_SET          = ' '
*     I_CALLBACK_USER_COMMAND           = ' '
     I_CALLBACK_TOP_OF_PAGE            = 'ALV_CATALOG_HEADER'
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME                  =
*     I_BACKGROUND_ID                   = ' '
*     I_GRID_TITLE                      =
*     I_GRID_SETTINGS                   =
     IS_LAYOUT                          = LAYOUT
     IT_FIELDCAT                       = GT_FCAT[]
*     IT_EXCLUDING                      =
*     IT_SPECIAL_GROUPS                 =
     IT_SORT                           = IT_SORT[]
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
      T_OUTTAB                          = GT_FINAL[]
*   EXCEPTIONS
*     PROGRAM_ERROR                     = 1
*     OTHERS                            = 2
            .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.


ENDFORM.                    " ALV_GRID_DISPLAY


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

  DATA : RV_WERKS(100) TYPE C,
         RV_SPART(100) TYPE C,
         LV_BEDAT(50) TYPE C.
*         LV_BEDAT1 TYPE SY-DATUM.

  CLEAR : RV_SPART,
          RV_WERKS.
  IF SO_WERKS-HIGH IS NOT INITIAL.
    CONCATENATE 'Plant Code :' SO_WERKS-LOW 'To' SO_WERKS-HIGH INTO RV_WERKS SEPARATED BY SPACE.
  ELSE.
    CONCATENATE 'Plant Code:' SO_WERKS-LOW INTO RV_WERKS SEPARATED BY SPACE.
  ENDIF.

  IF SO_WERKS-HIGH IS NOT INITIAL.
    CONCATENATE 'Division Code :' SO_SPART-LOW 'To' SO_SPART-HIGH INTO RV_SPART SEPARATED BY SPACE.
  ELSE.
    CONCATENATE 'Division Code :'  SO_SPART-LOW INTO RV_SPART SEPARATED BY SPACE.
  ENDIF.


  CLEAR LS_LINE.
  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY = ' '.
  LS_LINE-INFO = RV_WERKS.
  APPEND LS_LINE TO LIT_HEADER.

  CLEAR LS_LINE.
  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY = ' '.
  LS_LINE-INFO = RV_SPART.
  APPEND LS_LINE TO LIT_HEADER.

  CLEAR LS_LINE.
  LS_LINE-TYP  = 'H'.
  LS_LINE-KEY = ' '.
  LS_LINE-INFO = 'Material Stock Age Wise Report' .
  APPEND LS_LINE TO LIT_HEADER.


*  CLEAR LS_LINE.

*CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
*  EXPORTING
**    IT_LIST_COMMENTARY       = LIT_HEADER
**   I_LOGO                   = 'ZLOGO' .
*   I_END_OF_LIST_GRID       =
*   I_ALV_FORM               =
*          .
*
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = LIT_HEADER
      I_LOGO             = 'ZLOGO'.

  CALL FUNCTION 'CONVERSION_EXIT_SDATE_OUTPUT'
    EXPORTING
      INPUT  = LV_BEDAT
    IMPORTING
      OUTPUT = LV_BEDAT.

**  CALL FUNCTION 'CONVERSION_EXIT_SDATE_OUTPUT'
**
**  EXPORTING
**    INPUT         = LV_BEDAT.

*      I_LOGO             = ' '.


ENDFORM.                    "ALV_CATALOG_HEADER
