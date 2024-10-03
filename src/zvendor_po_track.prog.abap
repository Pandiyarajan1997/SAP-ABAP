

*&---------------------------------------------------------------------*
*& Report  ZVENDOR_PO_TRACK
*&
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&Functional                   : Mr.Govindarajan M                     *
*& Developer                   : Mr.Savariar.S                         *
*& Created On                  : 06/08/2014                            *
*& Company                     : Sheenlac Paints Pvt Ltd               *
*& Verified By                 : Mr.Govindarajan M                     *
*& Title                       : Vendor PO Tracking Document           *
*& Report Name                 : ZVENDOR_PO_TRACK                      *
*& Development Id              : kpabap                                *
*& Related Information         : Vendor Purchase Order Tracking Report *
*&---------------------------------------------------------------------*



REPORT ZVENDOR_PO_TRACK.


TYPES : BEGIN OF GS_EKKO ,

        EBELN TYPE EBELN ,                 "Purchasing Document Number
        BSART TYPE BSART,                  "Document Type
        BSTYP TYPE EBSTYP ,                "Purchasing Document Category
        LIFNR TYPE LIFNR,                  "Vendor
        ZTERM TYPE DZTERM,                  "Payment terms
        BEDAT TYPE BEDAT,                  " Po Date
        END OF GS_EKKO .

TYPES : BEGIN OF GS_EKPO,

        EBELN TYPE EBELN,                 "Purchasing Doc. EBELN EPBELN EPBELP
        EBELP TYPE EBELP,                 "Item EBELP
        MATNR TYPE MATNR,                 "Material
        MENGE TYPE MENGE_D,               "Quantity
        NETWR TYPE NETWR,                 "Net Value
        WERKS TYPE WERKS,                 "Plant
        MTART TYPE MTART,                 "Material Type
        END OF GS_EKPO.

TYPES : BEGIN OF GS_LFA1,

         LIFNR TYPE LFA1-LIFNR,           "Vendor.
         NAME1 TYPE LFA1-NAME1,            "Name

 END OF GS_LFA1.

TYPES : BEGIN OF GS_MAKT ,
        MATNR TYPE MATNR ,                  "-Material Number
        MAKTX TYPE MAKTX ,                  "Material Description (Short Text)
        END OF GS_MAKT .

TYPES : BEGIN OF GS_T134T,

        SPRAS TYPE SPRAS,
        MTART TYPE MTART,                   "Material Type
        MTBEZ TYPE MTBEZ,                   "Mat.type descr.

        END OF GS_T134T.


TYPES : BEGIN OF GS_EKBE,

       EBELN TYPE EBELN,                 "Purchasing Doc. EBELN
       EBELP TYPE EBELP,                 "Item EBELP
       MJAHR TYPE GJAHR,                 "Mat.Doc.Year
       BELNR TYPE MBLNR,                 "Material Doc.

       BUZEI TYPE BUZEI,                 "M.ITEM

       BEWTP TYPE BEWTP,                 "Categry
       BWART TYPE BWART,                 "Type
       BUDAT TYPE BUDAT,                 " GR DATE
       MENGE TYPE MENGE_D,               "Quantity
       DMBTR TYPE DMBTR,                 "GR VALUE
       MATNR TYPE MATNR,                 "Material
       WERKS TYPE WERKS_D,               "Plant

       END OF GS_EKBE.


TYPES : BEGIN OF GS_BSAK,

        LIFNR TYPE BSAK-LIFNR,               "Vendor
        UMSKZ TYPE BSAK-UMSKZ,               "Spl Indicator
        AUGDT TYPE BSAK-AUGDT,               "Clearing Date
        GJAHR TYPE BSAK-GJAHR,              "Year
        BELNR TYPE BSAK-BELNR,               "Clearing Doc.

        BUZEI TYPE BSAK-BUZEI,               "Line Item

        BUDAT TYPE BSAK-BUDAT,               "Posting Date
        DMBTR TYPE BSAK-DMBTR,               "Amount in LC.
        ZFBDT TYPE BSAK-ZFBDT,              "Baseline Date
        AUGBL TYPE BSAK-AUGBL,              "CLR.ITEM


  END OF GS_BSAK.


TYPES : BEGIN OF GS_BKPF ,

        BELNR TYPE BKPF-BELNR,
        GJAHR TYPE BKPF-GJAHR,
        USNAM TYPE BKPF-USNAM,

 END OF GS_BKPF.


  TYPES : BEGIN OF GS_BKPF1 ,

        BELNR TYPE BKPF-BELNR,
        GJAHR TYPE BKPF-GJAHR,
        USNAM TYPE BKPF-USNAM,
        PPNAM TYPE BKPF-PPNAM,

 END OF GS_BKPF1.


  TYPES : BEGIN OF GS_V_USERNAME,

         BNAME TYPE V_USERNAME-BNAME,
         NAME_LAST TYPE V_USERNAME-NAME_LAST,

         END OF GS_V_USERNAME.



TYPES : BEGIN OF GS_FINAL ,

      EBELN TYPE EKKO-EBELN ,              "Purchasing Document Number
      BSART TYPE EKKO-BSART,               "Document Type
      BSTYP TYPE EKKO-BSTYP ,              "Purchasing Document Category
      LIFNR TYPE EKKO-LIFNR,               "Vendor
      ZTERM TYPE EKKO-ZTERM,               "Payment terms
      BEDAT TYPE EKKO-BEDAT,               "Po Date

      EBELP TYPE EKPO-EBELP,               "Item EBELP
      MATNR TYPE EKPO-MATNR,               "Material
      MENGE TYPE EKPO-MENGE,               "Quantity
      NETWR TYPE EKPO-NETWR,               "Net Value
      MTART TYPE EKPO-MTART,               "Material Type
      WERKS TYPE EKPO-WERKS,               "Plant

      NAME1 TYPE LFA1-NAME1,               "Vendor Name
      MAKTX TYPE MAKT-MAKTX ,              " Material Description

      SPRAS TYPE T134T-SPRAS,              "Material Type
      MTBEZ TYPE T134T-MTBEZ,              "Mat.type descr.

      EKEBELN TYPE EKBE-EBELN,             "Purchasing Doc. EBELN
      EKEBELP TYPE EKBE-EBELP,             "Item EBELP
      EKBELNR TYPE EKBE-BELNR,             " Gr No.
      EKBEWTP TYPE EKBE-BEWTP,             "Categery
      EKBWART TYPE EKBE-BWART,             "Type
      EKBUDAT TYPE EKBE-BUDAT,             "Gr Date
      EKMENGE TYPE EKBE-MENGE,             "Gr Qty
      EKDMBTR TYPE EKBE-DMBTR,             "Gr Value
      EKMATNR TYPE EKBE-MATNR,             "Material

      BILLBELNR TYPE EKBE-BELNR,           "Bill No.
      BILLBUDAT TYPE EKBE-BUDAT,           "Bill Date
      BILLMENGE TYPE EKBE-MENGE,           "Bill Qty
      BILLDMBTR TYPE EKBE-DMBTR,           "Bill Value

      BAUGDT TYPE BSAK-AUGDT,              "Clearing Date
      BUMSKZ TYPE BSAK-UMSKZ,              "Spl Indicator
      BBELNR TYPE BSAK-AUGBL,              "Document No
      BBUDAT TYPE BSAK-BUDAT,              "Posting Date
      BDMBTR TYPE BSAK-DMBTR,              "Amount in LC.
      BZFBDT TYPE BSAK-ZFBDT,              "Baseline Date

      SPLDATE TYPE BSAK-AUGDT,             "Spl Date
      SPLACCNO TYPE BSAK-BELNR,            "Spl Acc no
      SPLAMT   TYPE BSAK-DMBTR,            "Spl Amt

       D_DAY TYPE I,                       " Arrier Days


      GJAHR TYPE BKPF-GJAHR,            " YEAR
      USNAM TYPE BKPF-USNAM,            " USERNAME

      USNAM1 TYPE BKPF-USNAM,
      PPNAM TYPE BKPF-PPNAM,

      BNAME TYPE V_USERNAME-BNAME,
      ENAME TYPE V_USERNAME-NAME_LAST,
      CNAME TYPE V_USERNAME-NAME_LAST,
      PNAME TYPE V_USERNAME-NAME_LAST,


   END OF GS_FINAL .



DATA  :GT_EKKO TYPE TABLE OF GS_EKKO,
       WA_EKKO TYPE GS_EKKO,

       GT_EKPO TYPE TABLE OF GS_EKPO,
       WA_EKPO TYPE GS_EKPO,

       GT_LFA1 TYPE TABLE OF GS_LFA1,
       WA_LFA1 TYPE GS_LFA1,

       GT_MAKT TYPE TABLE OF GS_MAKT,
       WA_MAKT TYPE  GS_MAKT,

       GT_T134T TYPE TABLE OF GS_T134T,
       WA_T134T TYPE  GS_T134T,

       GT_EKBE TYPE TABLE OF GS_EKBE,
       WA_EKBE TYPE GS_EKBE,

       GT_BSAK TYPE TABLE OF GS_BSAK,
       WA_BSAK TYPE  GS_BSAK,

       GT_BKPF TYPE TABLE OF GS_BKPF,
       WA_BKPF TYPE  GS_BKPF,

       GT_BKPF1 TYPE TABLE OF GS_BKPF1,
       WA_BKPF1 TYPE  GS_BKPF1,

       GT_V_USERNAME TYPE TABLE OF GS_V_USERNAME,
       WA_V_USERNAME TYPE GS_V_USERNAME,

       GT_V_USERNAME1 TYPE TABLE OF GS_V_USERNAME,
       WA_V_USERNAME1 TYPE GS_V_USERNAME,

       GT_FINAL TYPE TABLE OF GS_FINAL,
       WA_FINAL TYPE GS_FINAL.

DATA : GT_FCAT TYPE SLIS_T_FIELDCAT_ALV,
       WA_FCAT TYPE SLIS_FIELDCAT_ALV.

DATA : LAYOUT TYPE SLIS_LAYOUT_ALV.

DATA: OR_WERKS TYPE T001W-WERKS,             "Plant
      OR_EBELN TYPE EKKO-EBELN,              "Po Nmuber
      OR_LIFNR TYPE EKKO-LIFNR,              "Vendor Code
      OR_BSART TYPE EKKO-BSART,              "Document Type
      OR_BEDAT TYPE EKKO-BEDAT.              "Po Date


DATA: L_WERKS TYPE T001W-WERKS,              "Plant
      L_EBELN TYPE EKKO-EBELN,               "Po Nmuber
      L_LIFNR TYPE EKKO-LIFNR,               "Vendor Code
      L_BSART TYPE EKKO-BSART,               "Document Type
      L_BEDAT TYPE EKKO-BEDAT.               "Po Date

CONSTANTS : CATE1 TYPE CHAR10  VALUE 'E'.
CONSTANTS : CATE2 TYPE CHAR10  VALUE 'Q'.

SELECTION-SCREEN : BEGIN OF BLOCK S1 WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS : SO_WERKS FOR OR_WERKS," OBLIGATORY,
                 SO_EBELN FOR OR_EBELN,
                 SO_LIFNR FOR OR_LIFNR,
                 SO_BSART FOR OR_BSART.


SELECTION-SCREEN : END OF BLOCK S1.

SELECTION-SCREEN : BEGIN OF BLOCK S2 WITH FRAME TITLE TEXT-002 .

SELECT-OPTIONS : SO_BEDAT FOR OR_BEDAT.

SELECTION-SCREEN : END OF BLOCK S2 .

AT SELECTION-SCREEN .
  IF SO_WERKS IS NOT INITIAL .
    SELECT SINGLE WERKS FROM T001W INTO L_WERKS WHERE WERKS IN SO_WERKS .
    IF SY-SUBRC <> 0.
      MESSAGE 'Enter Valid Plant' TYPE 'E'.
*    MESSAGE E001(YMSG) .
    ENDIF.
  ENDIF.


  IF SO_LIFNR IS NOT INITIAL .
    SELECT SINGLE LIFNR FROM LFA1 INTO L_LIFNR WHERE LIFNR IN SO_LIFNR .
    IF SY-SUBRC NE 0.
      MESSAGE 'Enter Valid Vendor Code' TYPE 'E'.
    ENDIF.
  ENDIF.
  IF SO_EBELN IS NOT INITIAL .
    SELECT SINGLE EBELN FROM EKKO INTO L_EBELN WHERE EBELN IN SO_EBELN .
    IF SY-SUBRC NE 0.
      MESSAGE 'Enter Valid Purchase Order No.' TYPE 'E' .
    ENDIF.
  ENDIF.

  IF SO_BEDAT IS NOT INITIAL .
    SELECT SINGLE BEDAT FROM EKKO INTO L_BEDAT WHERE BEDAT IN SO_BEDAT.
    IF SY-SUBRC NE 0.
      MESSAGE 'Enter Valid Date.' TYPE 'E' .
    ENDIF.
  ENDIF.


START-OF-SELECTION.

  PERFORM GET_DATA.
  PERFORM FIELD_CATLOG .

  DATA : GT_SORT TYPE SLIS_T_SORTINFO_ALV,
         WA_SORT TYPE SLIS_SORTINFO_ALV.

END-OF-SELECTION.


  PERFORM ALV_DISPLAY.


*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM GET_DATA .

  SELECT EBELN
        BSART
        BSTYP
        LIFNR
        ZTERM
        BEDAT
     FROM
       EKKO INTO TABLE GT_EKKO WHERE EBELN IN SO_EBELN AND BSART IN SO_BSART
    AND LIFNR IN SO_LIFNR  AND BEDAT IN SO_BEDAT."AND BSART = 'ZNB'

  SELECT EBELN
        EBELP
        MATNR
        MENGE
        NETWR
        WERKS
        MTART
  FROM
     EKPO INTO TABLE GT_EKPO FOR ALL ENTRIES IN GT_EKKO WHERE EBELN = GT_EKKO-EBELN AND EBELN IN SO_EBELN AND WERKS IN SO_WERKS. "AND BWART = '101' AND  WERKS IN SO_WERKS.

  SELECT
     LIFNR
     NAME1
 FROM
    LFA1 INTO TABLE GT_LFA1 FOR ALL ENTRIES IN GT_EKKO WHERE LIFNR = GT_EKKO-LIFNR.


  SELECT MATNR
          MAKTX
   FROM
      MAKT INTO TABLE GT_MAKT FOR ALL ENTRIES IN GT_EKPO WHERE MATNR = GT_EKPO-MATNR.

  SELECT SPRAS
         MTART
         MTBEZ
   FROM
      T134T INTO TABLE GT_T134T FOR ALL ENTRIES IN GT_EKPO WHERE MTART = GT_EKPO-MTART AND SPRAS = 'E'.



  SELECT EBELN
       EBELP
       GJAHR
       BELNR
       BUZEI
       BEWTP "Q and E
       BWART
       BUDAT
       MENGE
       DMBTR
       MATNR
       WERKS
     FROM
        EKBE INTO TABLE GT_EKBE FOR ALL ENTRIES IN GT_EKPO WHERE EBELN = GT_EKPO-EBELN AND EBELP = GT_EKPO-EBELP. "AND BWART = '101' AND BEWTP = 'E' .


  SELECT LIFNR
        UMSKZ
        AUGDT
        GJAHR
        BELNR
        BUZEI
        BUDAT
        DMBTR
        ZFBDT
        AUGBL
   FROM
        BSAK INTO TABLE GT_BSAK FOR ALL ENTRIES IN GT_EKBE WHERE BELNR = GT_EKBE-BELNR and BUZEI = GT_EKBE-BUZEI." AND GJAHR = GT_EKBE-GJAHR.
*
*  SELECT LIFNR
*        GJAHR
*        BELNR
*        BUDAT
*        DMBTR
*        WRBTR
*        EBELN
*        EBELP
*   FROM
*        BSIK INTO TABLE GT_BSIK FOR ALL ENTRIES IN GT_BSAK WHERE BELNR = GT_BSAK-BELNR AND LIFNR = GT_BSAK-LIFNR AND GJAHR = GT_BSAK-GJAHR.
*

    SELECT
    BELNR
    GJAHR
    USNAM
        FROM
      BKPF INTO TABLE GT_BKPF FOR ALL ENTRIES IN GT_EKBE WHERE BELNR = GT_EKBE-BELNR ."AND GJAHR = GT_EKBE-MJAHR  .

  SELECT
    BELNR
    GJAHR
    USNAM
    PPNAM
     FROM
      BKPF INTO TABLE GT_BKPF1 FOR ALL ENTRIES IN GT_BSAK WHERE BELNR = GT_BSAK-AUGBL. "AND GJAHR = GT_BSAK-GJAHR  .


SELECT
  BNAME
  NAME_LAST
  FROM
  V_USERNAME INTO TABLE GT_V_USERNAME FOR ALL ENTRIES IN GT_BKPF WHERE BNAME = GT_BKPF-USNAM.


SELECT
  BNAME
  NAME_LAST
  FROM
  V_USERNAME INTO TABLE GT_V_USERNAME1 FOR ALL ENTRIES IN GT_BKPF1 WHERE BNAME = GT_BKPF1-USNAM.




  LOOP AT GT_EKPO INTO WA_EKPO .

    WA_FINAL-MATNR = WA_EKPO-MATNR.
    WA_FINAL-WERKS = WA_EKPO-WERKS.
    WA_FINAL-MENGE = WA_FINAL-MENGE + WA_EKPO-MENGE.
    WA_FINAL-NETWR = WA_FINAL-NETWR + WA_EKPO-NETWR.

    WA_FINAL-MTART = WA_EKPO-MTART.


    LOOP AT GT_EKKO INTO WA_EKKO WHERE  EBELN  = WA_EKPO-EBELN.
*    READ TABLE GT_EKKO INTO WA_EKKO WITH  KEY EBELN  = WA_EKPO-EBELN.
      MOVE-CORRESPONDING WA_EKKO TO WA_FINAL.

      READ TABLE GT_LFA1 INTO WA_LFA1 WITH  KEY LIFNR  = WA_EKKO-LIFNR.
      WA_FINAL-NAME1 = WA_LFA1-NAME1.

      READ TABLE GT_T134T INTO WA_T134T WITH KEY MTART = WA_EKPO-MTART.
      WA_FINAL-MTBEZ = WA_T134T-MTBEZ.

      LOOP AT GT_EKBE INTO WA_EKBE WHERE EBELN = WA_EKPO-EBELN AND EBELP = WA_EKPO-EBELP." BWART = '101' BEWTP = 'E'.
        IF WA_EKBE-BWART = '101'.

          WA_FINAL-EKEBELN = WA_EKBE-EBELN.
          WA_FINAL-EKEBELP = WA_EKBE-EBELP.
          WA_FINAL-EKBELNR = WA_EKBE-BELNR.
          WA_FINAL-EKBUDAT = WA_EKBE-BUDAT.
          WA_FINAL-EKMENGE = WA_EKBE-MENGE.
          WA_FINAL-EKDMBTR = WA_EKBE-DMBTR.

        ENDIF.

        IF WA_EKBE-BEWTP = 'Q'.

          WA_FINAL-BILLBELNR = WA_EKBE-BELNR.
          WA_FINAL-BILLBUDAT = WA_EKBE-BUDAT.
          WA_FINAL-BILLMENGE = WA_EKBE-MENGE.
          WA_FINAL-BILLDMBTR = WA_EKBE-DMBTR.
        ENDIF.




     ENDLOOP.  "change on 09/10/2014



      LOOP AT GT_BSAK INTO WA_BSAK WHERE BELNR = WA_EKBE-BELNR AND BUZEI = WA_EKBE-BUZEI .

        IF SY-SUBRC = 0.

          WA_FINAL-BAUGDT = WA_BSAK-AUGDT.
          WA_FINAL-BBELNR = WA_BSAK-AUGBL.
          WA_FINAL-BBUDAT =   WA_BSAK-BUDAT.
          WA_FINAL-BDMBTR =   WA_BSAK-DMBTR.
          WA_FINAL-BZFBDT =   WA_BSAK-ZFBDT.

        IF WA_BSAK-UMSKZ = 'A'.

          WA_FINAL-SPLDATE  = WA_BSAK-AUGDT.
          WA_FINAL-SPLACCNO = WA_BSAK-BELNR.
          WA_FINAL-SPLAMT =   WA_BSAK-DMBTR.
        ENDIF.

  ENDIF.



  ENDLOOP.



   APPEND WA_FINAL TO GT_FINAL.
    CLEAR WA_FINAL.

      ENDLOOP.



  ENDLOOP.


  LOOP AT GT_FINAL INTO WA_FINAL.

    READ TABLE GT_MAKT INTO WA_MAKT WITH KEY MATNR = WA_FINAL-MATNR.

    WA_FINAL-MAKTX = WA_MAKT-MAKTX.
    WA_FINAL-D_DAY = WA_FINAL-BAUGDT - WA_FINAL-BZFBDT.

    READ TABLE GT_BKPF INTO WA_BKPF WITH KEY BELNR = WA_FINAL-BILLBELNR.

     IF SY-SUBRC = 0.
     WA_FINAL-USNAM = WA_BKPF-USNAM.
     ENDIF.

     READ TABLE GT_BKPF1 INTO WA_BKPF1 WITH KEY BELNR = WA_FINAL-BBELNR. "GJAHR = WA_BSAK-GJAHR .

      IF SY-SUBRC = 0.
      WA_FINAL-USNAM1 = WA_BKPF1-USNAM.
      WA_FINAL-PPNAM = WA_BKPF1-PPNAM.
      ENDIF.


    MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING MAKTX D_DAY USNAM USNAM1 PPNAM .

    CLEAR WA_FINAL .
  ENDLOOP.



LOOP AT GT_FINAL INTO WA_FINAl.



     READ TABLE GT_V_USERNAME INTO WA_V_USERNAME WITH KEY BNAME = WA_FINAL-USNAM.
     IF SY-SUBRC = 0.
     WA_FINAL-ENAME = WA_V_USERNAME-NAME_LAST.
     ENDIF.

    READ TABLE GT_V_USERNAME1 INTO WA_V_USERNAME1 WITH KEY BNAME = WA_FINAL-PPNAM.
    IF SY-SUBRC = 0.
    WA_FINAL-PNAME = WA_V_USERNAME1-NAME_LAST.
    ENDIF.

    READ TABLE GT_V_USERNAME1 INTO WA_V_USERNAME1 WITH KEY BNAME = WA_FINAL-USNAM1.
    IF SY-SUBRC = 0.
    WA_FINAL-CNAME = WA_V_USERNAME1-NAME_LAST.
   ENDIF.


   MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING ENAME CNAME PNAME.
   CLEAR WA_FINAL.
ENDLOOP.

ENDFORM.                    "GET_DATA

*&---------------------------------------------------------------------*
*&      Form  FIELD_CATLOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FIELD_CATLOG .

  PERFORM ALV_LAYOUT USING 1 'Plant' 'WERKS' 'GT_FINAL' '' '' 'X' '' ''.
  PERFORM ALV_LAYOUT USING 3 'Vendor Name' 'NAME1' 'GT_FINAL' '' '' 'X' '' ''."k
  PERFORM ALV_LAYOUT USING 5 'Payment Terms' 'ZTERM' 'GT_FINAL' '' '' 'X' '' ''."k
  PERFORM ALV_LAYOUT USING 7 'Purchasing Doc' 'EBELN' 'GT_FINAL' '' '' 'X' '' ''."k
  PERFORM ALV_LAYOUT USING 9 'PO Date' 'BEDAT' 'GT_FINAL' '' '' 'X' '' ''."k
*  PERFORM ALV_LAYOUT USING 11 'Mat.Code' 'MATNR' 'GT_FINAL' '' '' 'X' ''.
  PERFORM ALV_LAYOUT USING 13 'Mat.Description' 'MAKTX' 'GT_FINAL' '' '' 'X' '' ''."k
*  PERFORM ALV_LAYOUT USING 15 'Mat.Type' 'MTART' 'GT_FINAL' '' '' 'X' ''.
  PERFORM ALV_LAYOUT USING 17 'Mat.Type' 'MTBEZ' 'GT_FINAL' '' '' 'X' '' ''."k
  PERFORM ALV_LAYOUT USING 19 'PO Quantity' 'MENGE' 'GT_FINAL' '' '' '' '' 'X'."k
  PERFORM ALV_LAYOUT USING 21 'PO Value' 'NETWR' 'GT_FINAL' '' '' '' '' 'X'."k

  PERFORM ALV_LAYOUT USING 23 'Gr.No' 'EKBELNR' 'GT_FINAL' '' '' '' '' ''.
  PERFORM ALV_LAYOUT USING 25 'Gr.Date' 'EKBUDAT' 'GT_FINAL' '' '' '' '' ''.
  PERFORM ALV_LAYOUT USING 27 'Gr.Qty' 'EKMENGE' 'GT_FINAL' 'X' '' '' '' 'X'.
  PERFORM ALV_LAYOUT USING 29 'Gr. Value' 'EKDMBTR' 'GT_FINAL' 'X' '' '' '' 'X'.

  PERFORM ALV_LAYOUT USING 31 'Bill No' 'BILLBELNR' 'GT_FINAL' '' '' '' '' ''.
  PERFORM ALV_LAYOUT USING 33 'Bill Date' 'BILLBUDAT' 'GT_FINAL' '' '' '' '' ''.
  PERFORM ALV_LAYOUT USING 35 'Bill Qty' 'BILLMENGE' 'GT_FINAL' 'X' '' '' '' 'X'.
  PERFORM ALV_LAYOUT USING 37 'Bill Value' 'BILLDMBTR' 'GT_FINAL' 'X' '' '' '' 'X'.

  PERFORM ALV_LAYOUT USING 39 'Clr Acc.Doc.No' 'BBELNR' 'GT_FINAL' '' '' '' '' ''.
  PERFORM ALV_LAYOUT USING 41 'Clr Date' 'BAUGDT' 'GT_FINAL' '' '' '' '' ''.
  PERFORM ALV_LAYOUT USING 43 'Clr Amt' 'BDMBTR' 'GT_FINAL' '' '' '' '' 'X'.


  PERFORM ALV_LAYOUT USING 44 'Entered By' 'USNAM' 'GT_FINAL' '' '' ''  '' ''.

  PERFORM ALV_LAYOUT USING 45 'Entered User' 'ENAME' 'GT_FINAL' '' '' ''  '' ''.

  PERFORM ALV_LAYOUT USING 46 'Parked By' 'PPNAM' 'GT_FINAL' '' '' ''  '' ''.
  PERFORM ALV_LAYOUT USING 47 'Parked User' 'PNAME' 'GT_FINAL' '' '' ''  '' ''.

  PERFORM ALV_LAYOUT USING 48 'Cleared By' 'USNAM1' 'GT_FINAL' '' '' ''  '' ''.

  PERFORM ALV_LAYOUT USING 49 'Cleared User' 'CNAME' 'GT_FINAL' '' '' ''  '' ''.


  PERFORM ALV_LAYOUT USING 50 'Arriar Days' 'D_DAY' 'GT_FINAL' '' '' '' '' ''.
*
*  PERFORM ALV_LAYOUT USING 47 'Ad.Pay.Date' 'SPLDATE' 'GT_FINAL' '' '' '' ''.
*  PERFORM ALV_LAYOUT USING 49 'Ad.Pay.No' 'SPLACCNO' 'GT_FINAL' '' '' '' ''.
*  PERFORM ALV_LAYOUT USING 51 'Ad.Pay.Amt' 'SPLAMT' 'GT_FINAL' '' '' '' ''.



  WA_SORT-FIELDNAME = 'NAME1'.
  WA_SORT-TABNAME = 'GT_FINAL'.
  WA_SORT-UP = 'X'.
  WA_SORT-SUBTOT = 'X'.
  APPEND WA_SORT TO GT_SORT.
  CLEAR WA_SORT.

 WA_SORT-FIELDNAME = 'EBELN'.
  WA_SORT-TABNAME = 'GT_FINAL'.
  WA_SORT-UP = 'X'.
  APPEND WA_SORT TO GT_SORT.
  CLEAR WA_SORT.

  WA_SORT-FIELDNAME = 'BEDAT'.
  WA_SORT-TABNAME = 'GT_FINAL'.
  WA_SORT-UP = 'X'.
  APPEND WA_SORT TO GT_SORT.
  CLEAR WA_SORT.

*
*  WA_SORT-FIELDNAME = 'BBELNR'.
*  WA_SORT-TABNAME = 'GT_FINAL'.
*  WA_SORT-UP = 'X'.
*  APPEND WA_SORT TO GT_SORT.
*  CLEAR WA_SORT.
*
*
*  WA_SORT-FIELDNAME = 'BAUGDT'.
*  WA_SORT-TABNAME = 'GT_FINAL'.
*  WA_SORT-UP = 'X'.
*  APPEND WA_SORT TO GT_SORT.
*  CLEAR WA_SORT.
*
*  WA_SORT-FIELDNAME = 'BDMBTR'.
*  WA_SORT-TABNAME = 'GT_FINAL'.
*  WA_SORT-UP = 'X'.
*  APPEND WA_SORT TO GT_SORT.
*  CLEAR WA_SORT.



ENDFORM.                    "FIELD_CATLOG




*&---------------------------------------------------------------------*
*&      Form  ALV_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P1         text
*      -->P2         text
*      -->P3         text
*      -->P4         text
*      -->P5         text
*      -->P6         text
*      -->P7         text
*      -->P8         text

*----------------------------------------------------------------------*
FORM ALV_LAYOUT  USING    P1 P2 P3 P4 P5 P6 P7 P8 P9.
  CLEAR WA_FCAT.
  WA_FCAT-COL_POS = P1.
  WA_FCAT-SELTEXT_L = P2.
  WA_FCAT-FIELDNAME = P3.
  WA_FCAT-TABNAME = P4.
  WA_FCAT-DO_SUM = P5.
  WA_FCAT-OUTPUTLEN = P6.
  WA_FCAT-KEY = P7.
  WA_FCAT-NO_OUT = P8.
  WA_FCAT-NO_ZERO = P9.
  APPEND WA_FCAT TO GT_FCAT.

*
*  WA_SORT-FIELDNAME = 'EBELN'.
*  WA_SORT-SUBTOT = 'X'.
**  WA_SORT-UP = 'X'.
*  APPEND WA_SORT TO GT_SORT.
*
*
*  WA_SORT-FIELDNAME = 'BEDAT'.
*  WA_SORT-SUBTOT = 'X'.
*  WA_SORT-UP = 'X'.
**  WA_SORT-GROUP = 'X'.
*  APPEND WA_SORT TO GT_SORT.
*
*
*  WA_SORT-FIELDNAME = 'NAME1'.
*  WA_SORT-SUBTOT = 'X'.
*  WA_SORT-UP = 'X'.
**  WA_SORT-GROUP = 'X'.
*  APPEND WA_SORT TO GT_SORT.
*
*
*  WA_SORT-FIELDNAME = 'ZTERM'.
*  WA_SORT-SUBTOT = 'X'.
*  WA_SORT-UP = 'X'.
**  WA_SORT-GROUP = 'X'.
*  APPEND WA_SORT TO GT_SORT.
*
*
*  WA_SORT-FIELDNAME = 'MAKTX'.
*  WA_SORT-SUBTOT = 'X'.
*  WA_SORT-UP = 'X'.
**  WA_SORT-GROUP = 'X'.
*  APPEND WA_SORT TO GT_SORT.
*
*
*  WA_SORT-FIELDNAME = 'MTBEZ'.
*  WA_SORT-SUBTOT = 'X'.
*  WA_SORT-UP = 'X'.
**  WA_SORT-GROUP = 'X'.
*  APPEND WA_SORT TO GT_SORT.
*
*
*  WA_SORT-FIELDNAME = 'BDMBTR'.
*  WA_SORT-SUBTOT = 'X'.
*  WA_SORT-UP = 'X'.
**  WA_SORT-GROUP = 'X'.
*  APPEND WA_SORT TO GT_SORT.
*
*
*  WA_SORT-FIELDNAME = 'MENGE'.
*  WA_SORT-SUBTOT = 'X'.
*  WA_SORT-UP = 'X'.
**  WA_SORT-GROUP = 'X'.
*  APPEND WA_SORT TO GT_SORT.
*
*  WA_SORT-FIELDNAME = 'NETWR'.
*  WA_SORT-SUBTOT = 'X'.
*  WA_SORT-UP = 'X'.
**  WA_SORT-GROUP = 'X'.
*  APPEND WA_SORT TO GT_SORT.


*
*  WA_SORT-FIELDNAME = 'EKEBELN'.
*  WA_SORT-SUBTOT = 'X'.
*  WA_SORT-UP = 'X'.
**  WA_SORT-GROUP = 'X'.
*  APPEND WA_SORT TO GT_SORT.
*
*
*
*  WA_SORT-FIELDNAME = 'EKEBELP'.
*  WA_SORT-SUBTOT = 'X'.
*  WA_SORT-UP = 'X'.
**  WA_SORT-GROUP = 'X'.
*  APPEND WA_SORT TO GT_SORT.


*
*
*  WA_SORT-FIELDNAME = 'EKBUDAT'.
*  WA_SORT-SUBTOT = 'X'.
*  WA_SORT-UP = 'X'.
**  WA_SORT-GROUP = 'X'.
*  APPEND WA_SORT TO GT_SORT.
*
*
*
*  WA_SORT-FIELDNAME = 'EKMENGE'.
*  WA_SORT-SUBTOT = 'X'.
*  WA_SORT-UP = 'X'.
**  WA_SORT-GROUP = 'X'.
*  APPEND WA_SORT TO GT_SORT.
*
*
*
*  WA_SORT-FIELDNAME = 'EKDMBTR'.
*  WA_SORT-SUBTOT = 'X'.
*  WA_SORT-UP = 'X'.
**  WA_SORT-GROUP = 'X'.
*  APPEND WA_SORT TO GT_SORT.





ENDFORM.                    "ALV_LAYOUT



*&---------------------------------------------------------------------*
*&      Form  ALV_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ALV_DISPLAY .


 LAYOUT-COLWIDTH_OPTIMIZE = 'X'.                                  "Changed by S.Savariar as on 20/10/2014.
 LAYOUT-ZEBRA = 'X'.


DELETE GT_FINAL WHERE MENGE = 0.


*DELETE GT_FINAL WHERE BBELNR IS INITIAL.


*DELETE ADJACENT DUPLICATES FROM GT_FINAL COMPARING BAUGDT BBELNR BBUDAT BDMBTR BZFBDT.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
   EXPORTING
*   I_INTERFACE_CHECK                 = ' '
*   I_BYPASSING_BUFFER                = ' '
*   I_BUFFER_ACTIVE                   = ' '
    I_CALLBACK_PROGRAM                = SY-REPID
*   I_CALLBACK_PF_STATUS_SET          = ' '
*   I_CALLBACK_USER_COMMAND           = ' '
    I_CALLBACK_TOP_OF_PAGE            = 'ALV_CATALOG_HEADER'
*   I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*   I_CALLBACK_HTML_END_OF_LIST       = ' '
*   I_STRUCTURE_NAME                  =
*   I_BACKGROUND_ID                   = ' '
*   I_GRID_TITLE                      =
*   I_GRID_SETTINGS                   =
*   I_CALLBACK_HTML_TOP_OF_PAGE       = ''
   IS_LAYOUT                          = LAYOUT
    IT_FIELDCAT                      = GT_FCAT[]
*   IT_EXCLUDING                      =
*   IT_SPECIAL_GROUPS                 =
    IT_SORT                           = GT_SORT[]
*   IT_FILTER                         =
*   IS_SEL_HIDE                       =
    I_DEFAULT                         = 'X'
    I_SAVE                            = 'A'
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
*   IMPORTING
*   E_EXIT_CAUSED_BY_CALLER           =
*   ES_EXIT_CAUSED_BY_USER            =
    TABLES
      T_OUTTAB                          = GT_FINAL[]
* EXCEPTIONS
*   PROGRAM_ERROR                     = 1
*   OTHERS                            = 2
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

  DATA : LV_LIFNR(100) TYPE C,
         LV_WERKS(100) TYPE C,
         LV_BEDAT(50) TYPE C.
*         LV_BEDAT1 TYPE SY-DATUM.

  CLEAR : LV_LIFNR,
          LV_WERKS.
  IF SO_LIFNR-HIGH IS NOT INITIAL.
    CONCATENATE 'Vendor No :' SO_LIFNR-LOW 'To' SO_LIFNR-HIGH INTO LV_LIFNR SEPARATED BY SPACE.
  ELSE.
    CONCATENATE 'Vendor No :' SO_LIFNR-LOW INTO LV_LIFNR SEPARATED BY SPACE.
  ENDIF.

  IF SO_WERKS-HIGH IS NOT INITIAL.
    CONCATENATE 'Plant Code :' SO_WERKS-LOW 'To' SO_WERKS-HIGH INTO LV_WERKS SEPARATED BY SPACE.
  ELSE.
    CONCATENATE 'Plant Code :' SO_WERKS-LOW INTO LV_WERKS SEPARATED BY SPACE.
  ENDIF.


  CLEAR LS_LINE.
  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY = ' '.
  LS_LINE-INFO = LV_LIFNR.
  APPEND LS_LINE TO LIT_HEADER.

  CLEAR LS_LINE.
  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY = ' '.
  LS_LINE-INFO = LV_WERKS.
  APPEND LS_LINE TO LIT_HEADER.

  CLEAR LS_LINE.
  LS_LINE-TYP  = 'H'.
  LS_LINE-KEY = ' '.
  LS_LINE-INFO = 'VENDOR PAYMENT DUE DETAILS' .
  APPEND LS_LINE TO LIT_HEADER.

  CLEAR LS_LINE.
  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY = ' '.
  LS_LINE-INFO = LV_BEDAT.
  APPEND LS_LINE TO LIT_HEADER.

*  CLEAR LS_LINE.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = LIT_HEADER.

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
