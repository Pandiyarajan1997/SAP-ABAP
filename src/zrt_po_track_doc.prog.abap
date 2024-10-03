*&---------------------------------------------------------------------*
*& Report  ZRT_PO_TRACK_DOC
*&
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&Functional                   : Mr.Govindarajan M                     *
*& Developer                   : Mr.Govindarajan M                     *
*& Created On                  :                                       *
*& Company                     : Sheenlac Paints Pvt Ltd               *
*& Verified By                 :                                       *
*& Title                       : PO Tracking Document                  *
*& Report Name                 : ZRT_PO_TRACK_DOC                      *
*& Development Id              : kpabap                                *
*& Related Information         : Purchase Order Tracking Report        *
*&---------------------------------------------------------------------*

REPORT ZRT_PO_TRACK_DOC.

TYPE-POOLS : SLIS .

*****************************************************************************************************************************
****************                           STRUCTURE  DECLERATION                *********************************************
*****************************************************************************************************************************
TYPES : BEGIN OF GS_EKKO ,
        EBELN TYPE EBELN ,                                  "Purchasing Document Number
        EKORG TYPE EKORG ,                                  "Purchasing Organization
        EKGRP TYPE BKGRP ,                                  "Purchasing Group
        BSART TYPE ESART ,                                  "Purchasing Document Type
        BSTYP TYPE EBSTYP ,                                 "Purchasing Document Category
        LOEKZ TYPE ELOEK ,                                  "Deletion Indicator in Purchasing Document
        AEDAT TYPE ERDAT ,                                  "Date on Which Record Was Created
        FRGZU TYPE FRGZU ,                                  "Release status
        REVNO TYPE REVNO ,                                  "Version number in Purchasing-PO AMENDMENT NO
        LIFNR TYPE ELIFN ,                                  "Vendor Account Number
        BEDAT TYPE BEDAT,                                   " Po Date
        END OF GS_EKKO .

TYPES : BEGIN OF GS_EKPO ,
        EBELN TYPE BSTNR ,                                  " Purchase Order Number
        EBELP TYPE EBELP ,                                  "Item Number of Purchasing Document
        WERKS TYPE EKPO-WERKS,                              " Plant
        LOEKZ TYPE ELOEK ,                                  "Deletion Indicator in Purchasing Document---------------------------
        MATNR TYPE MATNR ,                                  "Material Number
        MENGE TYPE EKPO-MENGE ,                             " PO Qty
        END OF GS_EKPO .

TYPES : BEGIN OF GS_EKET,
        EBELN TYPE EBELN ,                                  " Purchase Order Number
        EBELP TYPE EBELP ,                                  "Item Number of Purchasing Document
        EINDT TYPE EINDT,
        MENGE TYPE ETMEN ,                             " PO Qty
      END OF GS_EKET.




TYPES : BEGIN OF GS_LFA1 ,
        LIFNR TYPE LIFNR ,                                  "-Account Number of Vendor or Creditor
        NAME1 TYPE NAME1 ,                                  "NAME1
        END OF GS_LFA1 .

TYPES : BEGIN OF GS_MAKT ,
        MATNR TYPE MATNR ,                                  "-Material Number
        MAKTX TYPE MAKTX ,                                  "Material Description (Short Text)
        END OF GS_MAKT .

TYPES : BEGIN OF GS_MSEG ,
        MBLNR TYPE MBLNR ,                                  " Number of Material Document-------------------------------- GR No
        ZEILE TYPE MSEG-ZEILE,                              " Item in Material Document
        EBELN TYPE BSTNR ,                                  " Purchase Order Number
        EBELP TYPE EBELP ,                                  " Item Number of Purchasing Document
        MENGE TYPE MENGE_D ,                                  " Qty
        ERFMG TYPE ERFMG,                                     " Qty
        BWART TYPE BWART ,                                  " Movement Type
        LFBNR TYPE LFBNR ,                                  " Document No. of a Reference Document
        LIFNR TYPE RSEG-LIFNR ,                             " Vendor Code
        QINSPST TYPE MSEG-QINSPST,                          " Status of Goods Receipt Inspection
        MATNR TYPE MATNR,                                  " Material No
        WERKS TYPE WERKS_D,                                     " Plant
        END OF GS_MSEG .

TYPES: BEGIN OF GS_MKPF,
       MBLNR TYPE MBLNR ,                                  " Number of Material Document-------------------------------- GR No
       BUDAT TYPE BUDAT,                                   " Posting Date
       END OF GS_MKPF.






TYPES : BEGIN OF GS_FINAL ,
        EBELN TYPE EKKO-EBELN ,                             " Purchasing Document Number
        AEDAT TYPE EKKO-AEDAT,                               " Po Created Date
        BEDAT TYPE EKKO-BEDAT,                              " Po Document dATE
        EBELP TYPE EKPO-EBELP ,                             " Item Number of Purchasing Document
        NAME1 TYPE LFA1-NAME1 ,                             " Vendor Name
        MATNR TYPE EKPO-MATNR ,                             " Material Code
        MAKTX TYPE MAKT-MAKTX ,                             " Material Description
        MENGE TYPE EKPO-MENGE ,                             " PO Qty
        FRGZU TYPE EKKO-FRGZU ,                             " PO Rel Status
        SEBELN TYPE EKET-EBELN,                             "Sch No
        SEBELP TYPE EKET-EBELP,                             "Sch Item
        SEINDT TYPE EKET-EINDT,                           " Sch Date
        SMENGE TYPE EKET-MENGE,                             " Sch Qty
        PORDAT TYPE EREV-FGDAT ,                            " PO Rel Date
        MIGODT TYPE MKPF-BUDAT ,                            " MIGO Date
        MIGONO TYPE MSEG-BELNR ,                            " MIGO NO.
        MIGOQT TYPE MSEG-MENGE ,                            " MIGO Qty
        ERFMG TYPE MSEG-ERFMG,                             " PO Qty
        BAL_QTY TYPE P DECIMALS 3,                         " Balanace Qty
        D_DAY TYPE I,                                      " Delay Days
        STAT(30) TYPE C,                                   " Satus
        LINE_COLOR TYPE C,
        END OF GS_FINAL .

*****************************************************************************************************************************
****************                      INTERBAL TABLE & W0RK AREA DECLERATION     *********************************************
*****************************************************************************************************************************
DATA  :GT_EKKO TYPE TABLE OF GS_EKKO ,
       WA_EKKO TYPE GS_EKKO ,
       GT_EKPO TYPE TABLE OF GS_EKPO ,
       WA_EKPO TYPE GS_EKPO ,
        GT_EKET TYPE TABLE OF GS_EKET ,
       WA_EKET TYPE GS_EKET ,

       GT_LFA1 TYPE TABLE OF GS_LFA1 ,
       WA_LFA1 TYPE GS_LFA1 ,
       GT_MAKT TYPE TABLE OF GS_MAKT ,
       WA_MAKT TYPE  GS_MAKT ,
       GT_MSEG TYPE TABLE OF GS_MSEG ,
       WA_MSEG TYPE GS_MSEG ,


       GT_FINAL TYPE TABLE OF GS_FINAL ,
       WA_FINAL TYPE GS_FINAL,
       GT_MKPF TYPE TABLE OF GS_MKPF,
       WA_MKPF TYPE GS_MKPF.




DATA : WERKS TYPE T001W-WERKS ,
       EKORG TYPE T024E-EKORG ,
       EKGRP TYPE T024-EKGRP .

DATA : ZPAY_DUE_DAT TYPE  SY-DATUM ,                        "Net Due Date For Payment
       ZPAY_DAYS TYPE I ,                                   "Payment Days
       ZOVEDUE_DAYS TYPE I .                                "OVERDUE DAYS

DATA : WA_FIELDCATALOGUE TYPE SLIS_FIELDCAT_ALV ,
       LT_FIELDCATALOGUE TYPE SLIS_T_FIELDCAT_ALV .

DATA: LS_VARIANT TYPE DISVARIANT.
LS_VARIANT-REPORT = SY-REPID.

DATA : GT_SORT TYPE SLIS_T_SORTINFO_ALV,
      WA_SORT TYPE SLIS_SORTINFO_ALV.

DATA: POITEM TYPE EKPO-EBELP,
      D_BELNR(50) TYPE C ,
      D_BUDAT(50) TYPE C .

DATA: LV_TABIX TYPE I.

DATA: OR_WERKS TYPE T001W-WERKS,
      OR_EKORG TYPE EKKO-EKORG,
      OR_EKGRP TYPE EKKO-EKGRP,
      OR_LIFNR TYPE EKKO-LIFNR,
      OR_EBELN TYPE EKKO-EBELN,
      OR_AEDAT TYPE EKPO-AEDAT,
      OR_BSART TYPE T161-BSART.

DATA : L_WERKS TYPE T001W-WERKS ,
       L_EKORG TYPE T024E-EKORG ,
       L_EKGRP TYPE T024-EKGRP ,
       L_LIFNR TYPE LFA1-LIFNR ,
       L_EBELN TYPE EKKO-EBELN ,
       L_BSART TYPE T161-BSART .


DATA :   T_DAYS TYPE I.

*****************************************************************************************************************************
****************                      SELECTION SCREEN                           *********************************************
*****************************************************************************************************************************
SELECTION-SCREEN : BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001 .
SELECT-OPTIONS : SO_WERKS FOR OR_WERKS,
                 SO_EKORG FOR OR_EKORG ,
                 SO_EKGRP FOR OR_EKGRP  NO-DISPLAY ,
                 SO_LIFNR FOR OR_LIFNR ,
                 SO_EBELN FOR OR_EBELN ,
                 SO_BSART FOR OR_BSART.

SELECTION-SCREEN : END OF BLOCK B1 .

SELECTION-SCREEN : BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002 .

SELECT-OPTIONS : SO_AEDAT FOR OR_AEDAT.

SELECTION-SCREEN : END OF BLOCK B2 .

*****************************************************************************************************************************
****************                      AT SELECTION-SCREEN                        *********************************************
*****************************************************************************************************************************
AT SELECTION-SCREEN .
  IF SO_WERKS IS NOT INITIAL .
    SELECT SINGLE WERKS FROM T001W INTO L_WERKS WHERE WERKS IN SO_WERKS .
    IF SY-SUBRC <> 0.
      MESSAGE 'Enter Valid Plant' TYPE 'E'.
*    MESSAGE E001(YMSG) .
    ENDIF.
  ENDIF.
  IF SO_EKORG IS NOT INITIAL.
    SELECT SINGLE EKORG FROM T024E INTO L_EKORG WHERE EKORG IN SO_EKORG .
    IF SY-SUBRC NE 0.
      MESSAGE 'Enter valid Purchasing Organization ' TYPE 'E'.
    ENDIF.
  ENDIF.
  IF SO_EKGRP IS NOT INITIAL.
    SELECT SINGLE EKGRP FROM T024 INTO L_EKGRP WHERE EKGRP IN SO_EKGRP .
    IF SY-SUBRC NE 0.
      MESSAGE 'Enter Valid Purchase Group' TYPE 'E'.
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
  IF SO_BSART IS NOT INITIAL.
    SELECT SINGLE BSART FROM T161 INTO L_BSART WHERE BSART IN SO_BSART .
    IF SY-SUBRC NE 0 .
      MESSAGE 'Enter valid Purchasing Document Type' TYPE 'E'.
    ENDIF .
  ENDIF.

*****************************************************************************************************************************
****************                     START-OF-SELECTION                          *********************************************
*****************************************************************************************************************************
START-OF-SELECTION .
  PERFORM SELECT_DATA .
  PERFORM PROCESS_DATA .
  PERFORM FIELDCATALOGUE .
  PERFORM DISPLAY_DATA .


*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECT_DATA .

  SELECT EBELN
         EKORG
         EKGRP
         BSART
         BSTYP
         LOEKZ
         AEDAT
         FRGZU
         REVNO
         LIFNR
    BEDAT FROM EKKO INTO CORRESPONDING FIELDS OF TABLE GT_EKKO
         WHERE LIFNR IN SO_LIFNR AND
               EBELN IN SO_EBELN AND
               EKORG IN SO_EKORG AND
               EKGRP IN SO_EKGRP AND
               ( BSTYP = 'F' OR BSTYP = 'L'  ) AND
               BSART IN SO_BSART AND
               AEDAT IN SO_AEDAT.

  IF NOT GT_EKKO[] IS INITIAL .

    SELECT EBELN
           EBELP
           WERKS
           LOEKZ
           MATNR
           MENGE FROM EKPO INTO CORRESPONDING FIELDS OF TABLE GT_EKPO FOR ALL ENTRIES IN GT_EKKO
           WHERE EBELN = GT_EKKO-EBELN AND
                 WERKS IN SO_WERKS AND EBELN IN SO_EBELN.


    IF NOT GT_EKPO[] IS INITIAL .
      SELECT EBELN
         EBELP
        EINDT
        MENGE FROM EKET INTO TABLE GT_EKET FOR ALL ENTRIES IN GT_EKPO WHERE EBELN = GT_EKPO-EBELN AND EBELP = GT_EKPO-EBELP .
    ENDIF.

    IF NOT GT_EKPO[] IS INITIAL .
      SELECT MATNR MAKTX FROM MAKT INTO TABLE GT_MAKT FOR ALL ENTRIES IN GT_EKPO WHERE MATNR = GT_EKPO-MATNR .
    ENDIF.

    SELECT MBLNR
           ZEILE
           EBELN
           EBELP
           MENGE
           ERFMG
           BWART
           LFBNR
           LIFNR
           QINSPST
           MATNR
           WERKS FROM MSEG INTO CORRESPONDING FIELDS OF TABLE GT_MSEG FOR ALL ENTRIES IN GT_EKPO
           WHERE EBELN = GT_EKPO-EBELN  AND
                                        BWART = '101'.

  ENDIF.

  IF GT_MSEG[] IS NOT INITIAL.
    SELECT MBLNR
       BUDAT
      FROM MKPF INTO TABLE GT_MKPF FOR ALL ENTRIES IN GT_MSEG WHERE MBLNR = GT_MSEG-MBLNR.
  ENDIF.


  SELECT LIFNR NAME1 FROM LFA1 INTO TABLE GT_LFA1 FOR ALL ENTRIES IN GT_EKKO WHERE LIFNR = GT_EKKO-LIFNR .





ENDFORM.                    " SELECT_DATA


*&---------------------------------------------------------------------*
*&      Form  PROCESS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PROCESS_DATA .
*DATA: LD_COLOR(3) TYPE C.
  LOOP AT GT_EKPO INTO WA_EKPO.
    READ TABLE GT_EKKO INTO WA_EKKO WITH KEY EBELN = WA_EKPO-EBELN.
    IF SY-SUBRC = 0.
      WA_FINAL-EBELN = WA_EKPO-EBELN.
      WA_FINAL-AEDAT = WA_EKKO-AEDAT.
      WA_FINAL-BEDAT = WA_EKKO-BEDAT.
      WA_FINAL-EBELP = WA_EKPO-EBELP.
      WA_FINAL-MENGE = WA_EKPO-MENGE.
    ENDIF.
    READ TABLE GT_LFA1 INTO WA_LFA1 WITH KEY LIFNR = WA_EKKO-LIFNR.
    IF SY-SUBRC = 0.
      WA_FINAL-NAME1 = WA_LFA1-NAME1.
    ENDIF.

    READ TABLE GT_EKET INTO WA_EKET WITH KEY EBELN = WA_EKPO-EBELN EBELP = WA_EKPO-EBELP.
    WA_FINAL-SEBELN = WA_EKET-EBELN.
    WA_FINAL-SEBELP = WA_EKET-EBELP.
    WA_FINAL-SEINDT = WA_EKET-EINDT.
    WA_FINAL-SMENGE = WA_EKET-MENGE.

    READ TABLE GT_MAKT INTO WA_MAKT WITH KEY MATNR = WA_EKPO-MATNR.
    IF SY-SUBRC = 0.
      WA_FINAL-MATNR = WA_MAKT-MATNR.
      WA_FINAL-MAKTX = WA_MAKT-MAKTX.
    ENDIF.

    WA_FINAL-MENGE = WA_EKPO-MENGE.
    WA_FINAL-FRGZU = WA_EKKO-FRGZU.

    READ TABLE GT_MSEG INTO WA_MSEG WITH KEY  EBELN = WA_EKPO-EBELN  EBELP = WA_EKPO-EBELP.
    IF SY-SUBRC = 0.
      WA_FINAL-MIGONO = WA_MSEG-MBLNR.
      WA_FINAL-MIGOQT =  WA_MSEG-MENGE.
      WA_FINAL-ERFMG =  WA_MSEG-ERFMG.


      READ TABLE GT_MKPF INTO WA_MKPF WITH KEY MBLNR = WA_MSEG-MBLNR.

      WA_FINAL-MIGODT = WA_MKPF-BUDAT.

    ENDIF.

* LD_COLOR = LD_COLOR + 1.
* IF LD_COLOR = 2.
*      LD_COLOR = 1.
*    ENDIF.
*     CONCATENATE 'C' LD_COLOR '10' INTO WA_FINAL-LINE_COLOR.

    APPEND WA_FINAL TO GT_FINAL.

    CLEAR WA_FINAL.
  ENDLOOP.


  LOOP AT GT_FINAL INTO WA_FINAL.
    IF WA_FINAL-SMENGE = WA_FINAL-ERFMG.
      WA_FINAL-D_DAY = WA_FINAL-SEINDT - WA_FINAL-MIGODT.
    ELSEIF WA_FINAL-ERFMG = 0 OR WA_FINAL-SMENGE > WA_FINAL-ERFMG.
      WA_FINAL-D_DAY = WA_FINAL-SEINDT - SY-DATUM.
    ENDIF.

    WA_FINAL-BAL_QTY = WA_FINAL-SMENGE - WA_FINAL-ERFMG.

    IF WA_FINAL-SMENGE = WA_FINAL-ERFMG.
      WA_FINAL-STAT = 'Fully Received'.
    ELSEIF WA_FINAL-ERFMG = 0.
      WA_FINAL-STAT = 'Delayed'.
    ELSEIF WA_FINAL-SMENGE > WA_FINAL-ERFMG.
      WA_FINAL-STAT = 'Partily Received'.
    ELSEIF WA_FINAL-SEINDT > SY-DATUM.
      WA_FINAL-STAT = 'To Be Received'.
    ENDIF.

    MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING D_DAY STAT BAL_QTY.
    CLEAR WA_FINAL.
  ENDLOOP.

ENDFORM.                    " PROCESS_DATA


*&---------------------------------------------------------------------*
*&      Form  FIELDCATALOGUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FIELDCATALOGUE .
  PERFORM ALV_LAYOUT USING 1    'PO Number' 'EBELN' 'GT_FINAL' '' 'X' .
  PERFORM ALV_LAYOUT USING 2    'PO Date' 'BEDAT' 'GT_FINAL' ''  'X'.
  PERFORM ALV_LAYOUT USING 3    'Vendor Name' 'NAME1' 'GT_FINAL' ''  'X'.
  PERFORM ALV_LAYOUT USING 4    'Material Number' 'MATNR' 'GT_FINAL' '' 'X'.
  PERFORM ALV_LAYOUT USING 5    'Material Description' 'MAKTX' 'GT_FINAL' '' 'X'.
  PERFORM ALV_LAYOUT USING 6    'PO Qty' 'MENGE' 'GT_FINAL' '' 'X'.
  PERFORM ALV_LAYOUT USING 7    'Schudle Date' 'SEINDT' 'GT_FINAL' '' ''.
*  PERFORM ALV_LAYOUT USING 8   'Schudle No ' 'SEBELN' 'GT_FINAL' '' .
  PERFORM ALV_LAYOUT USING 10 'Schudle item' 'SEBELP' 'GT_FINAL' 'X' ''.
  PERFORM ALV_LAYOUT USING 12   'Schudle Qty' 'SMENGE' 'GT_FINAL' '' ''.
  PERFORM ALV_LAYOUT USING 20   'GR No.' 'MIGONO' 'GT_FINAL' '' ''.
  PERFORM ALV_LAYOUT USING 21   'GR Date.' 'MIGODT' 'GT_FINAL' '' ''.
  PERFORM ALV_LAYOUT USING 22   'GR Qty.' 'MIGOQT' 'GT_FINAL' 'X' ''.
  PERFORM ALV_LAYOUT USING 23   'GR Qty.' 'ERFMG' 'GT_FINAL' '' ''.
  PERFORM ALV_LAYOUT USING 24  'Balance Qty' 'BAL_QTY' 'GT_FINAL' '' ''.
  PERFORM ALV_LAYOUT USING 25   'Delay Days.' 'D_DAY' 'GT_FINAL' '' ''.
  PERFORM ALV_LAYOUT USING 26   'Status.' 'STAT' 'GT_FINAL' '' ''.


ENDFORM.                    " FIELDCATALOGUE


*&---------------------------------------------------------------------*
*&      Form  ALV_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0890   text
*      -->P_0891   text
*      -->P_0892   text
*----------------------------------------------------------------------*
FORM ALV_LAYOUT  USING   P1 P2 P3 P4 P5 P6.

  WA_FIELDCATALOGUE-COL_POS = P1.
  WA_FIELDCATALOGUE-SELTEXT_L  = P2.
  WA_FIELDCATALOGUE-FIELDNAME = P3.
  WA_FIELDCATALOGUE-TABNAME = P4.
  WA_FIELDCATALOGUE-NO_OUT  = P5.
  WA_FIELDCATALOGUE-KEY = P6.
  APPEND WA_FIELDCATALOGUE TO LT_FIELDCATALOGUE .
  CLEAR WA_FIELDCATALOGUE .


  WA_SORT-FIELDNAME = 'EBELN'.
  WA_SORT-SUBTOT = 'X'.
  WA_SORT-UP = 'X'.
  APPEND WA_SORT TO GT_SORT.

  WA_SORT-FIELDNAME = 'MIGONO'.
  WA_SORT-SUBTOT = 'X'.
  WA_SORT-UP = 'X'.
  APPEND WA_SORT TO GT_SORT.
ENDFORM.                    " ALV_LAYOUT


*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_DATA .
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
   EXPORTING
*   I_INTERFACE_CHECK                 = ' '
*   I_BYPASSING_BUFFER                = ' '
*   I_BUFFER_ACTIVE                   = ' '
*   I_CALLBACK_PROGRAM                = ' '
*   I_CALLBACK_PF_STATUS_SET          = ' '
*   I_CALLBACK_USER_COMMAND           = ' '
*   I_CALLBACK_TOP_OF_PAGE            = ' '
*   I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*   I_CALLBACK_HTML_END_OF_LIST       = ' '
*   I_STRUCTURE_NAME                  =
*   I_BACKGROUND_ID                   = ' '
*   I_GRID_TITLE                      =
*   I_GRID_SETTINGS                   =
*   IS_LAYOUT                         =
     IT_FIELDCAT                       = LT_FIELDCATALOGUE
*   IT_EXCLUDING                      =
*   IT_SPECIAL_GROUPS                 =
   IT_SORT                           = GT_SORT
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
      T_OUTTAB                          = GT_FINAL
* EXCEPTIONS
*   PROGRAM_ERROR                     = 1
*   OTHERS                            = 2
            .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.                    " DISPLAY_DATA
