

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



REPORT ZVENDOR_PAY_DETAILS.


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
       BEWTP TYPE BEWTP,                 "Categry
       BWART TYPE BWART,                 "Type
       BUDAT TYPE BUDAT,                 " GR DATE
       MENGE TYPE MENGE_D,               "Quantity
       DMBTR TYPE DMBTR,                 "GR VALUE
       MATNR TYPE MATNR,                 "Material
       WERKS TYPE WERKS_D,               "Plant

       END OF GS_EKBE.

TYPES : BEGIN OF GS_MSEG,

        MBLNR TYPE MSEG-MBLNR,            "Material Doc."MBLNR SMBLN
        MJAHR TYPE MSEG-MJAHR,            "Year
        BWART TYPE MSEG-BWART,            "Type
        MATNR TYPE MSEG-MATNR,            "Material
        WERKS TYPE MSEG-WERKS,            "Plant
        LIFNR TYPE MSEG-LIFNR,            "Vendor
        DMBTR TYPE MSEG-DMBTR,            "Amount in LC
        MENGE TYPE MSEG-MENGE,            "Quantity
        EBELN TYPE MSEG-EBELN,            "Purchase Order
        LFBNR TYPE MSEG-LFBNR,            "Reference No
        EBELP TYPE MSEG-EBELP,            "ITEM NUMBER OF PURCHASING DOCUMENT
*        BUDAT_MKPF type mseg-BUDAT,       "Posting Date
        BUDAT_MKPF TYPE MSEG-BUDAT_MKPF,

        SMBLN TYPE MSEG-MBLNR,            "Material Doc.No.
        CHARG TYPE MSEG-CHARG,            " Batch No
        SHKZG TYPE MSEG-SHKZG,

      END OF GS_MSEG.


TYPES : BEGIN OF GS_RBKP,                    "Added by savariar s

        BELNR TYPE RBKP-BELNR,
        LIFNR TYPE RBKP-LIFNR,

      END OF GS_RBKP.



TYPES : BEGIN OF GS_RSEG,

      BELNR TYPE RSEG-BELNR,          "Document Number BELNR
      GJAHR TYPE RSEG-GJAHR,          "Fiscal Year
      EBELN TYPE RSEG-EBELN,          "Purchasing Doc.
      EBELP TYPE RSEG-EBELP,          "Item
      MATNR TYPE RSEG-MATNR,          "Material
      BUKRS TYPE RSEG-BUKRS,          "Company Code
      WERKS TYPE RSEG-WERKS,          "Plant
      MENGE TYPE RSEG-MENGE,          "Quantity
      MEINS TYPE RSEG-MEINS,          "Base Unit
      LIFNR TYPE RSEG-LIFNR,          "Vendor
      WRBTR TYPE RSEG-WRBTR,          "Amount
      LFBNR TYPE RSEG-LFBNR,          "Reference Doc.

      END OF GS_RSEG.



TYPES : BEGIN OF GS_BSAK,

        LIFNR TYPE BSAK-LIFNR,               "Vendor
        UMSKZ TYPE BSAK-UMSKZ,               "Spl Indicator
        AUGDT TYPE BSAK-AUGDT,               "Clearing Date
        GJAHR TYPE BSAK-GJAHR,              "Year
        BELNR TYPE BSAK-BELNR,               "Clearing Doc.
        BUDAT TYPE BSAK-BUDAT,               "Posting Date
        DMBTR TYPE BSAK-DMBTR,               "Amount in LC.
        ZFBDT TYPE BSAK-ZFBDT,              "Baseline Date
        ZBD1T TYPE BSAK-ZBD1T,             "Days1
        AUGBL TYPE BSAK-AUGBL,
  END OF GS_BSAK.

TYPES : BEGIN OF GS_BKPF ,

        BELNR TYPE BKPF-BELNR,
        GJAHR TYPE BKPF-GJAHR,            " YEAR
        USNAM TYPE BKPF-USNAM,            " USERNAME

 END OF GS_BKPF.

 TYPES : BEGIN OF GS_BKPF1 ,

        BELNR TYPE BKPF-BELNR,
        GJAHR TYPE BKPF-GJAHR,            " YEAR
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

*      EKEBELN TYPE EKBE-EBELN,             "Purchasing Doc. EBELN
*      EKEBELP TYPE EKBE-EBELP,             "Item EBELP
*      EKBELNR TYPE EKBE-BELNR,             " Gr No.
*      EKBEWTP TYPE EKBE-BEWTP,             "Categery
*      EKBWART TYPE EKBE-BWART,             "Type
*      EKBUDAT TYPE EKBE-BUDAT,             "Gr Date
*      EKMENGE TYPE EKBE-MENGE,             "Gr Qty
*      EKDMBTR TYPE EKBE-DMBTR,             "Gr Value
*      EKMATNR TYPE EKBE-MATNR,             "Material

*      RBELNR TYPE EKBE-BELNR,           "Bill No.
*      BILLBUDAT TYPE EKBE-BUDAT,           "Bill Date
*      RMENGE TYPE EKBE-MENGE,           "Bill Qty
*      RDMBTR TYPE EKBE-DMBTR,           "Bill Value
*



      EKEBELN TYPE MSEG-EBELN,             "Purchasing Doc. EBELN
      EKEBELP TYPE MSEG-EBELP,             "Item EBELP
      EKBELNR TYPE MSEG-MBLNR,             " Gr No.
      EKBEWTP TYPE MSEG-BWART,             "Categery
      EKBUDAT TYPE MSEG-BUDAT_MKPF,             "Gr Date
      EKMENGE TYPE MSEG-MENGE,             "Gr Qty
      EKDMBTR TYPE MSEG-DMBTR,             "Gr Value
      EKMATNR TYPE MSEG-MATNR,             "Material"MBLNR
      EKSMBLN TYPE MSEG-SMBLN,


      MKEBELN TYPE MSEG-EBELN,             "Purchasing Doc. EBELN
      MKEBELP TYPE MSEG-EBELP,             "Item EBELP
      MKBELNR TYPE MSEG-MBLNR,             " Gr No.
*      MKBEWTP TYPE MSEG-BEWTP,             "Categery
      MKBWART TYPE MSEG-BWART,             "Type
      MKBUDAT TYPE MSEG-BUDAT_MKPF,             "Gr Date
      MKMENGE TYPE MSEG-MENGE,             "Gr Qty
      MKDMBTR TYPE MSEG-DMBTR,             "Gr Value
      MKMATNR TYPE MSEG-MATNR,             "Material
      BWART   TYPE MSEG-BWART,              " Move Type
      CHARG  TYPE MSEG-CHARG,              " Batch No
      SHKZG TYPE MSEG-SHKZG,               " Deb/Cr Ind.
      LFBNR TYPE MSEG-LFBNR,          "Reference Doc.
      MSMBLN TYPE MSEG-MBLNR,            "Material Doc.No.
      RBELNR TYPE RSEG-BELNR,           "Bill No.
*      BILLBUDAT TYPE RSEG-BUDAT,           "Bill Date
      RMENGE TYPE RSEG-MENGE,           "Bill Qty
      RDMBTR TYPE RSEG-WRBTR,           "Bill Value

      RBBELNR TYPE RBKP-BELNR,

      BAUGDT TYPE BSAK-AUGDT,              "Clearing Date
      BUMSKZ TYPE BSAK-UMSKZ,              "Spl Indicator
      BBELNR TYPE BSAK-BELNR,              "Document No
      BBUDAT TYPE BSAK-BUDAT,              "Posting Date
      BDMBTR TYPE BSAK-DMBTR,              "Amount in LC.
      BZFBDT TYPE BSAK-ZFBDT,              "Baseline Date
      ZBD1T TYPE  BSAK-ZBD1T,              "Day1

      GJAHR TYPE BKPF-GJAHR,            " YEAR
      USNAM TYPE BKPF-USNAM,            " USERNAME

       USNAM1 TYPE BKPF-USNAM,
       PPNAM TYPE BKPF-PPNAM,

      SPLDATE TYPE BSAK-AUGDT,             "Spl Date
      SPLACCNO TYPE BSAK-BELNR,            "Spl Acc no
      SPLAMT   TYPE BSAK-DMBTR,            "Spl Amt

       D_DAY TYPE I,                       " Arrier Days


       BNAME TYPE V_USERNAME-BNAME,
       ENAME TYPE V_USERNAME-NAME_LAST,
       CNAME TYPE V_USERNAME-NAME_LAST,
       PNAME TYPE V_USERNAME-NAME_LAST,

*       ENTUSR TYPE V_USERNAME-NAME_LAST,


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

       GT_MSEG TYPE TABLE OF GS_MSEG,
       WA_MSEG TYPE  GS_MSEG,

       GT_RSEG TYPE TABLE OF GS_RSEG,
       WA_RSEG TYPE  GS_RSEG,

       GT_BSAK TYPE TABLE OF GS_BSAK,
       WA_BSAK TYPE  GS_BSAK,

       GT_BKPF TYPE TABLE OF GS_BKPF,
       WA_BKPF TYPE  GS_BKPF,

       GT_BKPF1 TYPE TABLE OF GS_BKPF1,
       WA_BKPF1 TYPE  GS_BKPF1,

       GT_RBKP TYPE TABLE OF GS_RBKP,
       WA_RBKP TYPE GS_RBKP,

       GT_V_USERNAME TYPE TABLE OF GS_V_USERNAME,
       WA_V_USERNAME TYPE GS_V_USERNAME,

      GT_V_USERNAME1 TYPE TABLE OF GS_V_USERNAME,
       WA_V_USERNAME1 TYPE GS_V_USERNAME,

       GT_FINAL TYPE TABLE OF GS_FINAL,
       WA_FINAL TYPE GS_FINAL,
       W1_FINAL TYPE   GS_FINAL.

DATA : GT_FCAT TYPE SLIS_T_FIELDCAT_ALV,
       WA_FCAT TYPE SLIS_FIELDCAT_ALV.

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
       EKKO INTO TABLE GT_EKKO WHERE EBELN IN SO_EBELN AND ( BSART = 'ZNB' OR BSART = 'ZLP' OR BSART = 'ZIM' OR BSART = 'ZSR' OR BSART = 'ZSC' OR BSART = 'ZCA')
     AND BSART IN SO_BSART
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
       BEWTP "Q and E
       BWART
       BUDAT
       MENGE
       DMBTR
       MATNR
       WERKS
     FROM
        EKBE INTO TABLE GT_EKBE FOR ALL ENTRIES IN GT_EKPO WHERE EBELN = GT_EKPO-EBELN AND EBELP = GT_EKPO-EBELP ."AND BWART = '101' AND BEWTP = 'E' .

SORT GT_EKBE BY EBELN EBELP BELNR. " Added by <IT-CAR Tool> during Code Remediation
  DELETE ADJACENT DUPLICATES FROM GT_EKBE  COMPARING EBELN EBELP BELNR.

  SELECT MBLNR
          MJAHR
          BWART
          MATNR
          WERKS
          LIFNR
          DMBTR
          MENGE
          EBELN
          LFBNR
          EBELP
          BUDAT_MKPF
          SMBLN
          CHARG
          SHKZG

   FROM
           MSEG INTO TABLE GT_MSEG FOR ALL ENTRIES IN GT_EKBE WHERE EBELN = GT_EKBE-EBELN ."AND SMBLN = MBLNR.  "AND BWART = '101' EBELN  = GT_EKBE-EBELN AND EBELP = GT_EKBE-EBELP ."AND .

  SELECT BELNR
         GJAHR
         EBELN
         EBELP
         MATNR
         BUKRS
         WERKS
         MENGE
         MEINS
         LIFNR
         WRBTR
         LFBNR
    FROM
        RSEG INTO TABLE GT_RSEG FOR ALL ENTRIES IN GT_MSEG WHERE EBELN  = GT_MSEG-EBELN AND EBELP = GT_MSEG-EBELP ."AND BELNR = GT_MSEG-MBLNR.


SELECT
   BELNR
   LIFNR
   FROM RBKP INTO TABLE GT_RBKP FOR ALL ENTRIES IN GT_RSEG WHERE BELNR = GT_RSEG-BELNR.

SORT GT_RBKP BY BELNR. " Added by <IT-CAR Tool> during Code Remediation
DELETE ADJACENT DUPLICATES FROM GT_RBKP COMPARING  BELNR.



  SELECT LIFNR
        UMSKZ
        AUGDT
        GJAHR
        BELNR
        BUDAT
        DMBTR
        ZFBDT
        ZBD1T
    AUGBL
   FROM
        BSAK INTO TABLE GT_BSAK FOR ALL ENTRIES IN GT_RSEG WHERE BELNR = GT_RSEG-BELNR. " AND LIFNR = GT_RSEG-LIFNR."AND GJAHR = GT_EKBE-GJAHR.

  SELECT
    BELNR
    GJAHR
    USNAM
        FROM
      BKPF INTO TABLE GT_BKPF FOR ALL ENTRIES IN GT_RSEG WHERE BELNR = GT_RSEG-BELNR AND GJAHR = GT_RSEG-GJAHR  .

  SELECT
    BELNR
    GJAHR
    USNAM
    PPNAM
     FROM
      BKPF INTO TABLE GT_BKPF1 FOR ALL ENTRIES IN GT_BSAK WHERE BELNR = GT_BSAK-AUGBL AND GJAHR = GT_BSAK-GJAHR  .

SORT GT_BSAK BY BELNR BUDAT DMBTR. " Added by <IT-CAR Tool> during Code Remediation
  DELETE ADJACENT DUPLICATES FROM GT_BSAK  COMPARING BELNR BUDAT DMBTR.

SORT GT_RBKP BY BELNR. " Added by <IT-CAR Tool> during Code Remediation
DELETE ADJACENT DUPLICATES FROM GT_RBKP COMPARING BELNR.

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

*SORT GT_EKBE BY EBELN.

  SORT GT_MSEG BY LFBNR BWART MBLNR  .



  LOOP AT GT_MSEG INTO WA_MSEG . "AND SMBLN <> ''."AND SMBLN NE WA_EKBE-BELNR." AND BELNR <> WA_MSEG-SMBLN .



*
    WA_FINAL-EKEBELN = WA_MSEG-EBELN.
    WA_FINAL-EKEBELP = WA_MSEG-EBELP.
    WA_FINAL-EKBELNR = WA_MSEG-MBLNR.
    WA_FINAL-EKBUDAT = WA_MSEG-BUDAT_MKPF.
    WA_FINAL-EKMENGE = WA_MSEG-MENGE.
    WA_FINAL-EKDMBTR = WA_MSEG-DMBTR.
    WA_FINAL-SHKZG   = WA_MSEG-SHKZG.
    WA_FINAL-CHARG  = WA_MSEG-CHARG.
    WA_FINAL-BWART   = WA_MSEG-BWART.
    WA_FINAL-LFBNR   = WA_MSEG-LFBNR.
*    MKEBELN TYPE MSEG-EBELN,             "Purchasing Doc. EBELN
*      MKEBELP TYPE MSEG-EBELP,             "Item EBELP
*      MKBELNR TYPE MSEG-MBLNR,             " Gr No.
**      MKBEWTP TYPE MSEG-BEWTP,             "Categery
*      MKBWART TYPE MSEG-BWART,             "Type
*      MKBUDAT TYPE MSEG-BUDAT_MKPF,             "Gr Date
*      MKMENGE TYPE MSEG-MENGE,             "Gr Qty
*      MKDMBTR TYPE MSEG-DMBTR,             "Gr Value
*      MKMATNR TYPE MSEG-MATNR,             "Material


    READ TABLE GT_EKPO INTO WA_EKPO  WITH KEY EBELN  = WA_MSEG-EBELN  EBELP =  WA_MSEG-EBELP BINARY SEARCH. "#EC CI_SORTED " Added by <IT-CAR Tool> during Code Remediation

*    SORT GT_FINAL BY MENGE NETWR MTART.

*    IF SY-SUBRC = 0.

* CLEAR WA_FINAL.
    WA_FINAL-MATNR = WA_EKPO-MATNR.
    WA_FINAL-WERKS = WA_EKPO-WERKS.
*    WA_FINAL-MENGE = WA_FINAL-MENGE.
*    WA_FINAL-NETWR = WA_FINAL-NETWR.

    WA_FINAL-MENGE = WA_FINAL-MENGE + WA_EKPO-MENGE.
    WA_FINAL-NETWR = WA_FINAL-NETWR + WA_EKPO-NETWR.
    WA_FINAL-MTART = WA_EKPO-MTART.

*    ENDIF.

*     ENDIF.
 READ TABLE GT_EKKO INTO WA_EKKO WITH KEY EBELN  = WA_MSEG-EBELN BINARY SEARCH."#EC CI_SORTED " Added by <IT-CAR Tool> during Code Remediation
*    READ TABLE GT_EKKO INTO WA_EKKO WITH  KEY EBELN  = WA_EKPO-EBELN.
    MOVE-CORRESPONDING WA_EKKO TO WA_FINAL.


    READ TABLE GT_LFA1 INTO WA_LFA1 WITH  KEY LIFNR  = WA_EKKO-LIFNR BINARY SEARCH. "#EC CI_SORTED " Added by <IT-CAR Tool> during Code Remediation
    WA_FINAL-NAME1 = WA_LFA1-NAME1.

    READ TABLE GT_T134T INTO WA_T134T WITH KEY MTART = WA_EKPO-MTART BINARY SEARCH. "#EC CI_SORTED " Added by <IT-CAR Tool> during Code Remediation
    WA_FINAL-MTBEZ = WA_T134T-MTBEZ.



    READ TABLE GT_RSEG INTO WA_RSEG WITH KEY LFBNR = WA_MSEG-MBLNR EBELN = WA_MSEG-EBELN EBELP = WA_MSEG-EBELP BINARY SEARCH . "#EC CI_SORTED " Added by <IT-CAR Tool> during Code Remediation

    IF SY-SUBRC = 0.

      WA_FINAL-RBELNR = WA_RSEG-BELNR.
*          WA_FINAL-BILLBUDAT = WA_RSEG-BUDAT.
      WA_FINAL-RMENGE = WA_RSEG-MENGE.
      WA_FINAL-RDMBTR = WA_RSEG-WRBTR.

    ENDIF.



READ TABLE GT_RBKP INTO WA_RBKP WITH KEY BELNR = WA_RSEG-BELNR.

   IF SY-SUBRC = 0.
   WA_FINAL-RBBELNR = WA_RBKP-BELNR.
   ENDIF.





        READ TABLE GT_BKPF INTO WA_BKPF WITH KEY BELNR = WA_RSEG-BELNR GJAHR = WA_RSEG-GJAHR.

        WA_FINAL-GJAHR = WA_BKPF-GJAHR.
        WA_FINAL-USNAM = WA_BKPF-USNAM.

        READ TABLE GT_BKPF1 INTO WA_BKPF1 WITH KEY BELNR = WA_BSAK-AUGBL GJAHR = WA_BSAK-GJAHR BINARY SEARCH. "#EC CI_SORTED " Added by <IT-CAR Tool> during Code Remediation

        WA_FINAL-USNAM1 = WA_BKPF1-USNAM.
        WA_FINAL-PPNAM = WA_BKPF1-PPNAM.




    APPEND WA_FINAL TO GT_FINAL.
    MOVE-CORRESPONDING WA_FINAL TO W1_FINAL.
    CLEAR WA_FINAL.
  ENDLOOP.

SORT GT_FINAL By SHKZG.

LOOP AT GT_FINAL INTO WA_FINAL.

      READ TABLE GT_BSAK INTO WA_BSAK WITH KEY BELNR = WA_FINAL-RBELNR .

        WA_FINAL-BAUGDT = WA_BSAK-AUGDT.
        WA_FINAL-BBELNR = WA_BSAK-BELNR.
        WA_FINAL-BBUDAT = WA_BSAK-BUDAT.



        WA_FINAL-BZFBDT = WA_BSAK-ZFBDT.
        WA_FINAL-ZBD1T = WA_BSAK-ZBD1T.
*AT FIRST .
WA_FINAL-BDMBTR = WA_FINAL-BDMBTR .
      MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING BAUGDT BBELNR BBUDAT  BDMBTR BZFBDT ZBD1T .
      CLEAR WA_FINAL.
*ENDAT.
      ENDLOOP.

  LOOP AT GT_FINAL INTO WA_FINAL.

   IF WA_FINAL-SHKZG = 'S' .
      WA_FINAL-EKMENGE =  WA_FINAL-EKMENGE.
    ELSEIF  WA_FINAL-SHKZG = 'H'  .
      WA_FINAL-EKMENGE =  WA_FINAL-EKMENGE -  WA_FINAL-EKMENGE.
    ENDIF.

   READ TABLE GT_V_USERNAME INTO WA_V_USERNAME WITH KEY BNAME = WA_FINAL-USNAM.

    WA_FINAL-ENAME = WA_V_USERNAME-NAME_LAST.




     READ TABLE GT_V_USERNAME INTO WA_V_USERNAME WITH KEY BNAME = WA_FINAL-PPNAM.

     WA_FINAL-PNAME = WA_V_USERNAME-NAME_LAST.


    MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING EKMENGE ENAME   .
    CLEAR WA_FINAL.

*    ENDLOOP.




*ENDIF.
  ENDLOOP.


  SORT GT_FINAL BY MENGE NETWR.
*    DELETE ADJACENT DUPLICATES FROM GT_FINAL COMPARING MENGE NETWR.
*SORT  GT_FINAL BY MENGE NETWR.


*  SORT GT_FINAL BY MENGE NETWR.

  LOOP AT GT_FINAL INTO WA_FINAL.

    READ TABLE GT_MAKT INTO WA_MAKT WITH KEY MATNR = WA_FINAL-MATNR BINARY SEARCH. "#EC CI_SORTED " Added by <IT-CAR Tool> during Code Remediation
    WA_FINAL-MAKTX = WA_MAKT-MAKTX.

    WA_FINAL-D_DAY = ( WA_FINAL-BAUGDT - WA_FINAL-BZFBDT ) - WA_FINAL-ZBD1T.

*  AT END OF EBELN.
*      MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING MENGE NETWR
*            WHERE EBELN = WA_FINAL-EBELN.
*    ENDAT.
*    AT END OF matnr.
*    IF WA_FINAL-MENGE = WA_FINAL-MENGE.
*      DELETE GT_FINAL-MENGE FROM GT_FINAL.
**      SORT GT_FINAL BY MENGE.
*
*    ENDIF.

    MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING MAKTX D_DAY.
    CLEAR WA_FINAL.
  ENDLOOP.

SORT GT_FINAL BY CHARG.
*DELETE ADJACENT DUPLICATES FROM GT_FINAl COMPARING CHARG.

DELETE GT_FINAL WHERE EKMENGE = 0.


LOOP AT     GT_FINAL INTO WA_FINAl.


  READ TABLE GT_V_USERNAME1 INTO WA_V_USERNAME1 WITH KEY BNAME = WA_FINAL-PPNAM.
    IF SY-SUBRC = 0.
    WA_FINAL-PNAME = WA_V_USERNAME1-NAME_LAST.
   ENDIF.

    READ TABLE GT_V_USERNAME1 INTO WA_V_USERNAME1 WITH KEY BNAME = WA_FINAL-USNAM1.
    IF SY-SUBRC = 0.
    WA_FINAL-CNAME = WA_V_USERNAME1-NAME_LAST.
   ENDIF.


   MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING CNAME PNAME.
ENDLOOP.
ENDFORM.                    "GET_DATA

*&---------------------------------------------------------------------*
*&      Form  FIELD_CATLOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FIELD_CATLOG .

  PERFORM ALV_LAYOUT USING 1 'Plant' 'WERKS' 'GT_FINAL' '' '' 'X' '' ''.
  PERFORM ALV_LAYOUT USING 3 'Vendor Code' 'LIFNR' 'GT_FINAL' '' '' 'X' '' ''.
  PERFORM ALV_LAYOUT USING 5 'Vendor Name' 'NAME1' 'GT_FINAL' '' '' 'X' '' ''.
  PERFORM ALV_LAYOUT USING 7 'Payment Terms' 'ZTERM' 'GT_FINAL' '' '' 'X' '' ''.
  PERFORM ALV_LAYOUT USING 9 'PO Number' 'EBELN' 'GT_FINAL' '' '' 'X' '' ''.
  PERFORM ALV_LAYOUT USING 11 'PO Date' 'BEDAT' 'GT_FINAL' '' '' 'X' '' ''.
  PERFORM ALV_LAYOUT USING 13 'Mat.Description' 'MAKTX' 'GT_FINAL' '' '' 'X' '' ''.
  PERFORM ALV_LAYOUT USING 17 'Mat.Type' 'MTBEZ' 'GT_FINAL' '' '' 'X' '' ''.
  PERFORM ALV_LAYOUT USING 19 'PO Quantity' 'MENGE' 'GT_FINAL' 'X' '' '' '' 'X'.
  PERFORM ALV_LAYOUT USING 21 'PO Value' 'NETWR' 'GT_FINAL' 'X' '' '' '' 'X'.

  PERFORM ALV_LAYOUT USING 23 'GR.Number' 'EKBELNR' 'GT_FINAL' '' '' '' '' 'X'.
  PERFORM ALV_LAYOUT USING 25 'GR.Date' 'EKBUDAT' 'GT_FINAL' '' '' '' '' 'X'.
  PERFORM ALV_LAYOUT USING 27 'GR.Quantity' 'EKMENGE' 'GT_FINAL' 'X' '' '' '' 'X'.
  PERFORM ALV_LAYOUT USING 29 'GR. Value' 'EKDMBTR' 'GT_FINAL' 'X' '' '' '' 'X'.
*  PERFORM ALV_LAYOUT USING 30 'Db/Cr Ind' 'SHKZG' 'GT_FINAL' '' '' '' '' ''.
*  PERFORM ALV_LAYOUT USING 32 'Batch No' 'CHARG' 'GT_FINAL' '' '' '' '' ''.
*  PERFORM ALV_LAYOUT USING 33 'Move Type' 'BWART' 'GT_FINAL' '' '' '' '' ''.
  PERFORM ALV_LAYOUT USING 34 'Bill Number' 'RBELNR' 'GT_FINAL' '' '' '' '' 'X'.
*  PERFORM ALV_LAYOUT USING 33 'Bill Date' 'BILLBUDAT' 'GT_FINAL' '' '' '' '' ''.
  PERFORM ALV_LAYOUT USING 35 'Bill Quantity' 'RMENGE' 'GT_FINAL' 'X' '' '' '' 'X'.
  PERFORM ALV_LAYOUT USING 36 'Bill Value' 'RDMBTR' 'GT_FINAL' 'X' '' '' '' 'X'.

  PERFORM ALV_LAYOUT USING 37 'Clr Acc.Doc.No' 'BBELNR' 'GT_FINAL' '' '' '' '' 'X'."BBELNR BAUGDT BDMBTR D_DAY
  PERFORM ALV_LAYOUT USING 41 'Clr Date' 'BAUGDT' 'GT_FINAL' '' '' '' '' 'X'.
  PERFORM ALV_LAYOUT USING 43 'Clr Amt' 'BDMBTR' 'GT_FINAL' 'X' '' '' '' 'X'.


  PERFORM ALV_LAYOUT USING 44 'Entered By' 'USNAM' 'GT_FINAL' '' '' '' '' 'X'.

  PERFORM ALV_LAYOUT USING 45 'Entered User' 'ENAME' 'GT_FINAL' '' '' '' '' ''.

  PERFORM ALV_LAYOUT USING 46 'Parked By' 'PPNAM' 'GT_FINAL' '' '' '' '' 'X'.
  PERFORM ALV_LAYOUT USING 47 'Parked User' 'PNAME' 'GT_FINAL' '' '' '' '' 'X'.

  PERFORM ALV_LAYOUT USING 48 'Cleared By' 'USNAM1' 'GT_FINAL' '' '' '' '' 'X'.

  PERFORM ALV_LAYOUT USING 49 'Cleared User' 'CNAME' 'GT_FINAL' '' '' '' '' 'X'.


  PERFORM ALV_LAYOUT USING 50 'Arriar Days' 'D_DAY' 'GT_FINAL' '' '' '' '' 'X'.
**
*  PERFORM ALV_LAYOUT USING 47 'Ad.Pay.Date' 'SPLDATE' 'GT_FINAL' '' '' '' '' 'X'.
  PERFORM ALV_LAYOUT USING 51 'Ad.Pay.No' 'SPLACCNO' 'GT_FINAL' '' '' '' '' 'X'.
  PERFORM ALV_LAYOUT USING 52 'Ad.Pay.Amt' 'SPLAMT' 'GT_FINAL' '' '' '' '' 'X'.




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

WA_SORT-FIELDNAME = 'BDMBTR'.
  WA_SORT-TABNAME = 'GT_FINAL'.
  WA_SORT-UP  = 'X'.
  WA_SORT-GROUP = 'X'.
  APPEND WA_SORT TO GT_SORT.
  CLEAR WA_SORT.

   WA_SORT-FIELDNAME = 'BBELNR'.
  WA_SORT-TABNAME = 'GT_FINAL'.
  WA_SORT-UP = 'X'.
  APPEND WA_SORT TO GT_SORT.
  CLEAR WA_SORT.

   WA_SORT-FIELDNAME = 'BAUGDT'.
  WA_SORT-TABNAME = 'GT_FINAL'.
  WA_SORT-UP = 'X'.
  APPEND WA_SORT TO GT_SORT.
  CLEAR WA_SORT.





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
   WA_FCAT-NO_ZERO     = P9.
  APPEND WA_FCAT TO GT_FCAT.


ENDFORM.                    "ALV_LAYOUT



*&---------------------------------------------------------------------*
*&      Form  ALV_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ALV_DISPLAY .



  DELETE GT_FINAL WHERE MENGE = 0.


*  DELETE ADJACENT DUPLICATES FROM GT_FINAL COMPARING WERKS EBELN BEDAT NAME1 MENGE NETWR EKBELNR EKBUDAT EKMENGE EKDMBTR RBELNR RMENGE RDMBTR BBELNR BAUGDT BDMBTR D_DAY.

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
*   IS_LAYOUT                         =
  IT_FIELDCAT                      = GT_FCAT[]
*   IT_EXCLUDING                      =
*   IT_SPECIAL_GROUPS                 =
  IT_SORT                           = GT_SORT
*   IT_FILTER                         =
*   IS_SEL_HIDE                       =
*  I_DEFAULT                         = 'X'
*  I_SAVE                            = 'A'
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
  LS_LINE-INFO = 'Vendor Purchase Process Details' .
  APPEND LS_LINE TO LIT_HEADER.

  CLEAR LS_LINE.
  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY = ' '.
  LS_LINE-INFO = LV_BEDAT.
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
