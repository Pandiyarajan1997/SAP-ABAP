*&---------------------------------------------------------------------*
*& Report  ZPO_TRACK_REPORTS
*&
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&Functional                   : Mr.Govindarajan M                     *
*& Developer                   : Mr.Savariar.S                        *
*& Created On                  :                                       *
*& Company                     : Sheenlac Paints Pvt Ltd               *
*& Verified By                 :                                       *
*& Title                       : PO Tracking Document                  *
*& Report Name                 : ZPO_TRACK_REPORTS                     *
*& Development Id              : kpabap                                *
*& Related Information         : Purchase Order Tracking Report        *
*&---------------------------------------------------------------------*


REPORT ZPO_TRACK_REPORTS.

TABLES :EKBE,EKKO,EKET,LFA1,MAKT,MSEG,MKPF.

TYPES : BEGIN OF GS_EKKO ,

        EBELN TYPE EBELN ,                 "Purchasing Document Number
        BSART TYPE BSART,                  "Document Type
        BSTYP TYPE EBSTYP ,                "Purchasing Document Category
        LIFNR TYPE LIFNR,                  "Vendor
        BEDAT TYPE BEDAT,                  " Po Date
        END OF GS_EKKO .


TYPES : BEGIN OF GS_EKPO,

        EBELN TYPE EBELN,                 "Purchasing Doc. EBELN EPBELN EPBELP
        EBELP TYPE EBELP,                 "Item EBELP
        MATNR TYPE MATNR,                 "Material
        MENGE TYPE MENGE_D,               "Quantity
        WERKS TYPE WERKS,                 "Plant
        END OF GS_EKPO.

TYPES : BEGIN OF GS_EKET,

        EBELN TYPE EBELN ,                 "Purchase Order Number
        EBELP TYPE EBELP ,                 "Item Number of Purchasing Document
        EINDT TYPE EINDT,
        ETENR TYPE ETENR,                  "Schudle line
        MENGE TYPE ETMEN ,                 " PO Qty
        WEMNG TYPE WEMNG,                  "DELIVERY QTY

      END OF GS_EKET.


TYPES : BEGIN OF GS_EKBE,

        EBELN TYPE EBELN,                 "Purchasing Doc. EBELN
        EBELP TYPE EBELP,                 "Item EBELP
*        VGABE type VGABE,                 "Line Item
        BELNR TYPE MBLNR,                 "Material Doc.
        BWART TYPE BWART,
        MENGE TYPE MENGE_D,               "Quantity
        MATNR TYPE MATNR,                  "

        END OF GS_EKBE.


TYPES : BEGIN OF GS_LFA1 ,
        LIFNR TYPE LIFNR ,                 "-Account Number of Vendor or Creditor
        NAME1 TYPE NAME1 ,                 "NAME1
        END OF GS_LFA1 .

TYPES : BEGIN OF GS_MAKT ,
        MATNR TYPE MATNR ,                  "-Material Number
        MAKTX TYPE MAKTX ,                  "Material Description (Short Text)
        END OF GS_MAKT .


TYPES: BEGIN OF GS_MKPF,

       MBLNR TYPE MBLNR ,                    " Number of Material Document GR No
       BUDAT TYPE BUDAT,                     " Posting Date
       END OF GS_MKPF.

TYPES : BEGIN OF GS_FINAL ,

        EBELN TYPE EKKO-EBELN ,              "Purchasing Document Number
        BSART TYPE EKKO-BSART,               "Document Type
        BSTYP TYPE EKKO-BSTYP ,              "Purchasing Document Category
        LIFNR TYPE EKKO-LIFNR,               "Vendor
        BEDAT TYPE EKKO-BEDAT,               "Po Date

        EBELP TYPE EKPO-EBELP,               "Item EBELP
        MATNR TYPE EKPO-MATNR,               "Material
        MENGE TYPE EKPO-MENGE,               "Quantity
        WERKS TYPE EKPO-WERKS,               "Plant

        EKEBELN TYPE EKBE-EBELN,                 "Purchasing Doc. EBELN
        EKEBELP TYPE EKBE-EBELP,                 "Item EBELP
        EKBELNR TYPE EKBE-BELNR,                 "Material Doc.
        EKBWART TYPE EKBE-BWART,
        EKMENGE TYPE EKBE-MENGE,               "Quantity
        EKMATNR TYPE EKBE-MATNR,

        ETEBELN TYPE EKET-EBELN,               "Sch No EEBELP
        ETBELP TYPE EKET-EBELP,
        EINDT TYPE EKET-EINDT,               "Sch Date
        ETENR TYPE EKET-ETENR,               "Schudle line
        ETMENGE TYPE EKET-MENGE,              "Sch Qty
        ETWEMNG TYPE EKET-WEMNG,             "DELIVERY QTY
        MAKTX TYPE MAKT-MAKTX ,              " Material Description
        NAME1 TYPE LFA1-NAME1,               "Vendor Name
        BUDAT TYPE MKPF-BUDAT,               " MIGO Date
        BALQTY TYPE P DECIMALS 3,            " Balanace Qty
        D_DAY TYPE I,                        " Delay Days
        STAT(30) TYPE C,                     " Satus

        END OF GS_FINAL .

DATA  :GT_EKKO TYPE TABLE OF GS_EKKO,
       WA_EKKO TYPE GS_EKKO,

       GT_EKPO TYPE TABLE OF GS_EKPO,
       WA_EKPO TYPE GS_EKPO,

       GT_EKET TYPE TABLE OF GS_EKET,
       WA_EKET TYPE GS_EKET,

       GT_EKBE TYPE TABLE OF GS_EKBE,
       WA_EKBE TYPE GS_EKBE,
       GT_LFA1 TYPE TABLE OF GS_LFA1,
       WA_LFA1 TYPE GS_LFA1,

       GT_MAKT TYPE TABLE OF GS_MAKT,
       WA_MAKT TYPE  GS_MAKT,

       GT_MKPF TYPE TABLE OF GS_MKPF,
       WA_MKPF TYPE  GS_MKPF,

       GT_FINAL TYPE TABLE OF GS_FINAL,
       WA_FINAL TYPE GS_FINAL.

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


SELECTION-SCREEN : BEGIN OF BLOCK S1 WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS : SO_WERKS FOR OR_WERKS," OBLIGATORY,
                 SO_EBELN FOR OR_EBELN,
                 SO_LIFNR FOR OR_LIFNR.
*                 SO_BSART FOR OR_BSART.


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

  SELECT
        EBELN
        BSART
        BSTYP
        LIFNR
        BEDAT
     FROM
       EKKO INTO TABLE GT_EKKO WHERE EBELN IN SO_EBELN AND ( BSART = 'ZLP') "AND BSART IN SO_BSART
    AND LIFNR IN SO_LIFNR AND ( BSTYP = 'F' OR BSTYP = 'L' ) AND BEDAT IN SO_BEDAT.

  SELECT EBELN
        EBELP
        MATNR
        MENGE
        WERKS
  FROM
     EKPO INTO TABLE GT_EKPO FOR ALL ENTRIES IN GT_EKKO WHERE EBELN = GT_EKKO-EBELN AND EBELN IN SO_EBELN AND WERKS IN SO_WERKS. "AND BWART = '101' AND  WERKS IN SO_WERKS.
   DELETE GT_EKPO WHERE MENGE = 0.

  SELECT EBELN
        EBELP
        EINDT
        ETENR
        MENGE
        WEMNG
    FROM
       EKET INTO TABLE GT_EKET FOR ALL ENTRIES IN GT_EKPO WHERE EBELN = GT_EKPO-EBELN AND EBELP = GT_EKPO-EBELP AND EBELN IN SO_EBELN. "AND EBELP = GT_EKKO-EPBELP.


  SELECT EBELN
        EBELP
        BELNR
        BWART
        MENGE
        MATNR
     FROM
        EKBE INTO TABLE GT_EKBE FOR ALL ENTRIES IN GT_EKET WHERE EBELN = GT_EKET-EBELN AND EBELP = GT_EKET-EBELP AND EBELN IN SO_EBELN."AND MENGE = GT_EKET-WEMNG." AND EBELP = GT_EKET-EBELP AND BWART = '101' .

  SELECT MBLNR
         BUDAT
        FROM MKPF INTO TABLE GT_MKPF FOR ALL ENTRIES IN GT_EKBE WHERE MBLNR = GT_EKBE-BELNR.

  SELECT MATNR
         MAKTX
      FROM
     MAKT INTO TABLE GT_MAKT FOR ALL ENTRIES IN GT_EKPO WHERE MATNR = GT_EKPO-MATNR.

  SELECT
     LIFNR
     NAME1
 FROM
    LFA1 INTO TABLE GT_LFA1 FOR ALL ENTRIES IN GT_EKKO WHERE LIFNR = GT_EKKO-LIFNR.




  LOOP AT GT_EKPO INTO WA_EKPO.
    MOVE-CORRESPONDING WA_EKPO TO WA_FINAL.

    READ TABLE GT_EKKO INTO WA_EKKO WITH KEY EBELN = WA_EKPO-EBELN.

    WA_FINAL-LIFNR = WA_EKKO-LIFNR.
    WA_FINAL-BEDAT = WA_EKKO-BEDAT.

    READ TABLE GT_MAKT INTO WA_MAKT WITH KEY MATNR = WA_EKPO-MATNR.
    WA_FINAL-MAKTX = WA_MAKT-MAKTX.

    READ TABLE GT_EKET INTO WA_EKET WITH KEY EBELN = WA_EKPO-EBELN EBELP = WA_EKPO-EBELP.

    IF SY-SUBRC = 0.

      WA_FINAL-ETEBELN = WA_EKET-EBELN.
      WA_FINAL-ETBELP = WA_EKET-EBELP.
      WA_FINAL-EINDT = WA_EKET-EINDT.
      WA_FINAL-ETENR = WA_EKET-ETENR.
      WA_FINAL-ETMENGE = WA_EKET-MENGE.
      WA_FINAL-ETWEMNG = WA_EKET-WEMNG.

      READ TABLE GT_EKBE INTO WA_EKBE WITH KEY EBELN = WA_EKET-EBELN EBELP = WA_EKET-EBELP BWART = '101'.

      IF SY-SUBRC = 0.
        WA_FINAL-EKEBELN = WA_EKBE-EBELN.
        WA_FINAL-EKEBELP = WA_EKBE-EBELP.
        WA_FINAL-EKBELNR = WA_EKBE-BELNR.
        WA_FINAL-EKMENGE = WA_EKBE-MENGE.

        READ TABLE GT_MKPF INTO WA_MKPF WITH KEY MBLNR = WA_EKBE-BELNR.
        IF SY-SUBRC = 0.
          WA_FINAL-BUDAT = WA_MKPF-BUDAT.


          APPEND WA_FINAL TO GT_FINAL.
        ENDIF.
      ENDIF.
    ENDIF.

    APPEND WA_FINAL TO GT_FINAL.
    CLEAR WA_FINAL.

  ENDLOOP.


*  SORT GT_FINAL BY EBELN NAME1 EEBELN SMBLNR.  EMENGE MERFMG

  LOOP AT GT_FINAL INTO WA_FINAL .

    READ TABLE GT_LFA1 INTO WA_LFA1 WITH KEY LIFNR = WA_FINAL-LIFNR.
    WA_FINAL-NAME1 = WA_LFA1-NAME1.

    WA_FINAL-BALQTY = WA_FINAL-ETMENGE - WA_FINAL-EKMENGE.

    IF WA_FINAL-EKBELNR <> 0.
      WA_FINAL-D_DAY = WA_FINAL-EINDT - WA_FINAL-BUDAT.
    ENDIF.

    IF WA_FINAL-ETMENGE = WA_FINAL-EKMENGE.
      WA_FINAL-STAT = 'Fully Received'.
    ELSEIF WA_FINAL-EKMENGE = 0.
      WA_FINAL-STAT = 'Delayed'.
    ELSEIF WA_FINAL-ETMENGE > WA_FINAL-EKMENGE.
      WA_FINAL-STAT = 'Partily Received'.
    ELSEIF WA_FINAL-EINDT > SY-DATUM.
      WA_FINAL-STAT = 'To Be Received'.
    ELSEIF WA_FINAL-ETMENGE < WA_FINAL-EKMENGE.
      WA_FINAL-STAT = 'Excess Recived'.
    ENDIF.
    MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING NAME1 BALQTY D_DAY STAT.

    CLEAR WA_FINAL.

  ENDLOOP.


ENDFORM.                    "GET_DATA


*&---------------------------------------------------------------------*
*&      Form  FIELD_CATLOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FIELD_CATLOG .

  REFRESH GT_SORT.
  CLEAR WA_SORT.

  PERFORM ALV_LAYOUT USING 1 'Plant' 'WERKS' 'GT_FINAL' '' '' 'X' ''.
  PERFORM ALV_LAYOUT USING 2 'Vendor Code' 'LIFNR' 'GT_FINAL' '' '' 'X' ''.
  PERFORM ALV_LAYOUT USING 3 'Vendor Name' 'NAME1' 'GT_FINAL' '' '' 'X' ''.
  PERFORM ALV_LAYOUT USING 4 'Po Number' 'EBELN' 'GT_FINAL' '' '' 'X' ''.
  PERFORM ALV_LAYOUT USING 5 'Item No' 'EBELP' 'GT_FINAL' '' '' 'X' ''.
  PERFORM ALV_LAYOUT USING 7 'Mat.Description' 'MAKTX' 'GT_FINAL' '' '' 'X' ''.
  PERFORM ALV_LAYOUT USING 11 'PO Quantity' 'MENGE' 'GT_FINAL' 'X' '' 'X' ''.
  PERFORM ALV_LAYOUT USING 13 'PO Date' 'BEDAT' 'GT_FINAL' '' '' 'X' ''.
  PERFORM ALV_LAYOUT USING 15 'Sch. No' 'ETEBELN' 'GT_FINAL' '' '' '' ''.
**  PERFORM ALV_LAYOUT USING 16 'Sch. Line' 'ETENR' 'GT_FINAL' '' '' '' ''.
*  PERFORM ALV_LAYOUT USING 17 'Sch. Item' 'ETBELP' 'GT_FINAL' '' '' '' ''.
  PERFORM ALV_LAYOUT USING 19 'Sch. Date' 'EINDT' 'GT_FINAL' '' '' '' ''.
  PERFORM ALV_LAYOUT USING 21 'Sch. Qty' 'ETMENGE' 'GT_FINAL' 'X' '' '' ''.

  PERFORM ALV_LAYOUT USING 23 'GR.No' 'EKBELNR' 'GT_FINAL' '' '' '' ''.
  PERFORM ALV_LAYOUT USING 25 'GR.Qty' 'EKMENGE' 'GT_FINAL' 'X' '' '' ''.

  PERFORM ALV_LAYOUT USING 26 'GR.Date' 'BUDAT' 'GT_FINAL' '' '' '' ''.
  PERFORM ALV_LAYOUT USING 27 'Balance Qty' 'BALQTY' 'GT_FINAL' 'X' '' '' ''.
  PERFORM ALV_LAYOUT USING 28 'Delay Days' 'D_DAY' 'GT_FINAL' '' '' '' ''.
  PERFORM ALV_LAYOUT USING 29 'Status' 'STAT' 'GT_FINAL' '' '' '' ''.





*  WA_SORT-SPOS = 2.
  WA_SORT-FIELDNAME = 'NAME1'.
  WA_SORT-TABNAME = 'GT_FINAL'.
  WA_SORT-UP = 'X'.
  WA_SORT-SUBTOT = 'X'.
  APPEND WA_SORT TO GT_SORT.
  CLEAR WA_SORT.



ENDFORM.                    " FIELD_CATLOG



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
FORM ALV_LAYOUT  USING    P1 P2 P3 P4 P5 P6 P7 P8.
  CLEAR WA_FCAT.
  WA_FCAT-COL_POS = P1.
  WA_FCAT-SELTEXT_L = P2.
  WA_FCAT-FIELDNAME = P3.
  WA_FCAT-TABNAME = P4.
  WA_FCAT-DO_SUM = P5.
  WA_FCAT-OUTPUTLEN = P6.
  WA_FCAT-KEY = P7.
  WA_FCAT-NO_OUT = P8.
  APPEND WA_FCAT TO GT_FCAT.


*  WA_SORT-FIELDNAME = 'EBELN'.
*  WA_SORT-SUBTOT = 'X'.
*  WA_SORT-UP = 'X'.
*  APPEND WA_SORT TO GT_SORT.

*  WA_SORT-FIELDNAME = 'NAME1'.
*  WA_SORT-SUBTOT = ''.
*  WA_SORT-UP = 'X'.
*  APPEND WA_SORT TO GT_SORT.
*
**
**  WA_SORT-FIELDNAME = 'MATNR'.
**  WA_SORT-SUBTOT = ''.
**  WA_SORT-UP = 'X'.
**  APPEND WA_SORT TO GT_SORT.
*
*  WA_SORT-FIELDNAME = 'MAKTX'.
*  WA_SORT-SUBTOT = ''.
*  WA_SORT-UP = ''.
*  APPEND WA_SORT TO GT_SORT.
*
*
*  WA_SORT-FIELDNAME = 'EEBELN'.
*  WA_SORT-SUBTOT = ''.
*  WA_SORT-UP = 'X'.
*  APPEND WA_SORT TO GT_SORT.
*
*  WA_SORT-FIELDNAME = 'SMBLNR'.
*  WA_SORT-SUBTOT = ''.
*  WA_SORT-UP = 'X'.
*  APPEND WA_SORT TO GT_SORT.


ENDFORM.                    "ALV_LAYOUT



*&---------------------------------------------------------------------*
*&      Form  ALV_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ALV_DISPLAY .

*   DELETE GT_FINAL WHERE MENGE = 0.

SORT GT_FINAL BY WERKS LIFNR NAME1 EBELN EBELP MAKTX MENGE BEDAT ETEBELN ETBELP EINDT ETMENGE EKBELNR EKMENGE. " Added by <IT-CAR Tool> during Code Remediation
  DELETE ADJACENT DUPLICATES FROM GT_FINAL COMPARING WERKS LIFNR NAME1 EBELN EBELP MAKTX MENGE BEDAT ETEBELN ETBELP EINDT ETMENGE EKBELNR EKMENGE.


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
  LS_LINE-INFO = 'Schedule PO Tracking Report' .
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
