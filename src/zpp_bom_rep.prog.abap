*&---------------------------------------------------------------------*
*& Report ZPP_BOM_REP
*&*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&Functional                   : Mr.Ramachandaran          *
*& Developer                   : Mr.Ramachandaran                      *
*& Company                     : Sheenlac Paints Pvt Ltd               *
*& Verified By                 :                                       *
*& Title                       : Bill of Material Report       *
*& Report Name                 : ZPP_BOM_REP                          *
*& Development Id              : kpabap                                *
*& Related Information         : Bom Report      *
*&---------------------------------------------------------------------*

REPORT ZPP_BOM_REP.

TYPES : BEGIN OF ES_MARA,
        MATNR TYPE MARA-MATNR,
        ERSDA TYPE MARA-ERSDA,
        ERNAM TYPE MARA-ERNAM,
        MTART TYPE MARA-MTART,
        MATKL TYPE MARA-MATKL,
        BEGRU TYPE MARA-BEGRU,
      END OF ES_MARA.

TYPES : BEGIN OF ES_MAKT,
         MATNR TYPE MAKT-MATNR,
         MAKTX TYPE MAKT-MAKTX,
       END OF ES_MAKT.

TYPES : BEGIN OF ES_MAST,
        MATNR TYPE MAST-MATNR,
        WERKS TYPE MAST-WERKS,
        STLAN TYPE MAST-STLAN,
        STLNR TYPE MAST-STLNR,
        STLAL TYPE MAST-STLAL,
        LOSVN TYPE MAST-LOSVN,
        LOSBS TYPE MAST-LOSBS,
        ANDAT TYPE MAST-ANDAT,
        ANNAM TYPE MAST-ANNAM,
        AEDAT TYPE MAST-AEDAT,
        AENAM TYPE MAST-AENAM,
        CSLTY TYPE MAST-CSLTY,
      END OF ES_MAST.

TYPES : BEGIN OF ES_STAS,
             MANDT TYPE STAS-MANDT,
             STLTY TYPE STAS-STLTY,
             STLNR TYPE STAS-STLNR,
             STLAL TYPE STAS-STLAL,
             STLKN TYPE STAS-STLKN,
             STASZ TYPE STAS-STASZ,
             LKENZ TYPE STAS-LKENZ,
             STVKN TYPE STAS-STVKN,
           END OF ES_STAS.

TYPES : BEGIN OF ES_STKO,
        STLTY TYPE STKO-STLTY,
        STLNR TYPE STKO-STLNR,
        STLAL TYPE STKO-STLAL,
        STKOZ TYPE STKO-STKOZ,
        BMEIN TYPE STKO-BMEIN,
        BMENG TYPE STKO-BMENG,
        STLST TYPE STKO-STLST,
      END OF ES_STKO.

TYPES : BEGIN OF ES_STPO,
      STLTY TYPE STPO-STLTY,
      STLNR TYPE STPO-STLNR,
      STLKN TYPE STPO-STLKN,
      STPOZ TYPE STPO-STPOZ,
      DATUV TYPE STPO-DATUV,
      LKENZ TYPE STPO-LKENZ,
      ANDAT TYPE STPO-ANDAT,
      ANNAM TYPE STPO-ANNAM,
      AEDAT TYPE STPO-AEDAT,
      AENAM TYPE STPO-AENAM,
      IDNRK TYPE STPO-IDNRK,
      PSWRK TYPE STPO-PSWRK,
      POSTP TYPE STPO-POSTP,
      POSNR TYPE STPO-POSNR,
      MEINS TYPE STPO-MEINS,
      MENGE TYPE STPO-MENGE,
      FMENG TYPE STPO-FMENG,
      AUSCH TYPE STPO-AUSCH,
      AVOAU TYPE STPO-AVOAU,
      MATKL TYPE STPO-MATKL,
      DOKAR TYPE STPO-DOKAR,
      DOKNR TYPE STPO-DOKNR,
     END OF ES_STPO.

DATA: GT_MARA TYPE TABLE OF ES_MARA,
      WA_MARA TYPE ES_MARA.

DATA : GT_MAKT TYPE TABLE OF ES_MAKT,
       WA_MAKT TYPE ES_MAKT.

DATA : GT1_MAKT TYPE TABLE OF ES_MAKT,
       WA1_MAKT TYPE ES_MAKT.

DATA: GT_MAST TYPE TABLE OF ES_MAST,
     WA_MAST TYPE ES_MAST.

DATA : GT_STAS TYPE TABLE OF ES_STAS,
       WA_STAS TYPE ES_STAS.

DATA: GT_STKO TYPE TABLE OF ES_STKO,
     WA_STKO TYPE ES_STKO.

DATA: GT_STPO TYPE TABLE OF ES_STPO,
     WA_STPO TYPE ES_STPO.

TYPES : BEGIN OF GS_FINAL,
        MATNR TYPE MARA-MATNR,
        ERSDA TYPE MARA-ERSDA,
        ERNAM TYPE MARA-ERNAM,
        MTART TYPE MARA-MTART,
        "MATKL TYPE MARA-MATKL,
        BEGRU TYPE MARA-BEGRU,
        MAKTX TYPE MAKT-MAKTX,
        "MATNR TYPE MAST-MATNR,
        WERKS TYPE MAST-WERKS,
        STLAN TYPE MAST-STLAN,
        STLNR TYPE MAST-STLNR,
        STLAL TYPE MAST-STLAL,
        LOSVN TYPE MAST-LOSVN,
        LOSBS TYPE MAST-LOSBS,
        ANDAT TYPE MAST-ANDAT,
        ANNAM TYPE MAST-ANNAM,
        AEDAT TYPE MAST-AEDAT,
        AENAM TYPE MAST-AENAM,
        CSLTY TYPE MAST-CSLTY,
        MANDT TYPE STAS-MANDT,
        "STLTY TYPE STAS-STLTY,
        "STLNR TYPE STAS-STLNR,
        "STLAL TYPE STAS-STLAL,
        "STLKN TYPE STAS-STLKN,
        STASZ TYPE STAS-STASZ,
        "LKENZ TYPE STAS-LKENZ,
 "        STVKN TYPE STAS-STVKN,
        STLTY TYPE STKO-STLTY,
        "STLNR TYPE STKO-STLNR,
        "STLAL TYPE STKO-STLAL,
        STKOZ TYPE STKO-STKOZ,
        BMEIN TYPE STKO-BMEIN,
        BMENG TYPE STKO-BMENG,
        STLST TYPE STKO-STLST,
        "STLTY TYPE STPO-STLTY,
        "STLNR TYPE STPO-STLNR,
        STLKN TYPE STPO-STLKN,
        STPOZ TYPE STPO-STPOZ,
        DATUV TYPE STPO-DATUV,
        LKENZ TYPE STPO-LKENZ,
        "ANDAT TYPE STPO-ANDAT,
        "ANNAM TYPE STPO-ANNAM,
        "AEDAT TYPE STPO-AEDAT,
        "AENAM TYPE STPO-AENAM,
        IDNRK TYPE STPO-IDNRK,
        PSWRK TYPE STPO-PSWRK,
        POSTP TYPE STPO-POSTP,
        POSNR TYPE STPO-POSNR,
        MEINS TYPE STPO-MEINS,
        MENGE TYPE STPO-MENGE,
        FMENG TYPE STPO-FMENG,
        AUSCH TYPE STPO-AUSCH,
        AVOAU TYPE STPO-AVOAU,
        MATKL TYPE STPO-MATKL,
        DOKAR TYPE STPO-DOKAR,
        DOKNR TYPE STPO-DOKNR,
        FLAG(1) TYPE C,
        DIDNRK TYPE MAKT-MAKTX,
       END OF GS_FINAL.

DATA :   GT_FINAL TYPE TABLE OF GS_FINAL,
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
         LV_MTART TYPE EKPO-MTART,
         LV_ANDAT TYPE MAST-ANDAT.

DATA : LAYOUT TYPE SLIS_LAYOUT_ALV.

SELECTION-SCREEN : BEGIN OF BLOCK C1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS  : SO_WERKS FOR LV_WERKS OBLIGATORY,
                  SO_MATNR FOR LV_MATNR,
                  SO_MTART FOR LV_MTART,
                  SO_ANDAT FOR LV_ANDAT.

SELECTION-SCREEN : END OF BLOCK C1.

START-OF-SELECTION.
  PERFORM GET_DATA.
  PERFORM READ_DATA.
  PERFORM AUTHCHECK_OVERVIEW.

END-OF-SELECTION.
  PERFORM FIELD_CATLOG.
  PERFORM ALV_DISPLAY.

*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
FORM GET_DATA .

  SELECT
          MATNR
          WERKS
          STLAN
          STLNR
          STLAL
          LOSVN
          LOSBS
          ANDAT
          ANNAM
          AEDAT
          AENAM
          CSLTY FROM MAST INTO TABLE GT_MAST WHERE MATNR IN SO_MATNR AND WERKS IN SO_WERKS AND ANDAT IN SO_ANDAT .
 IF GT_MAST IS NOT INITIAL.
    SELECT
         MATNR
         ERSDA
         ERNAM
         MTART
         MATKL
         BEGRU
           FROM MARA INTO TABLE GT_MARA FOR ALL ENTRIES IN GT_MAST WHERE MATNR = GT_MAST-MATNR AND MATNR IN SO_MATNR AND MTART IN SO_MTART.
  SELECT
    MATNR
    MAKTX
       FROM MAKT INTO TABLE GT_MAKT FOR ALL ENTRIES IN GT_MARA WHERE MATNR = GT_MARA-MATNR.

  SELECT
    STLTY
    STLNR
    STLAL
    STKOZ
    BMEIN
    BMENG
    STLST FROM STKO INTO TABLE GT_STKO FOR ALL ENTRIES IN GT_MAST WHERE STLNR = GT_MAST-STLNR AND STLST = 1 .

  SELECT
     MANDT
     STLTY
     STLNR
     STLAL
     STLKN
     STASZ
     LKENZ
     STVKN FROM STAS INTO TABLE GT_STAS FOR ALL ENTRIES IN GT_STKO WHERE STLNR = GT_STKO-STLNR AND STLAL = GT_STKO-STLAL.

  SELECT
      STLTY
      STLNR
      STLKN
      STPOZ
      DATUV
      LKENZ
      ANDAT
      ANNAM
      AEDAT
      AENAM
      IDNRK
      PSWRK
      POSTP
      POSNR
      MEINS
      MENGE
      FMENG
      AUSCH
      AVOAU
      MATKL
      DOKAR
      DOKNR FROM STPO INTO TABLE GT_STPO FOR ALL ENTRIES IN GT_STAS WHERE STLNR = GT_STAS-STLNR AND STLKN = GT_STAS-STLKN.

  SELECT
    MATNR
    MAKTX
       FROM MAKT INTO TABLE GT1_MAKT FOR ALL ENTRIES IN GT_STPO WHERE MATNR = GT_STPO-IDNRK.

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

*  LOOP AT GT_MAST INTO WA_MAST.
*    MOVE-CORRESPONDING WA_MAST TO WA_FINAL.
*    READ TABLE GT_STKO INTO WA_STKO WITH KEY STLNR = WA_FINAL-STLNR .
*    IF SY-SUBRC EQ 0 .
*    WA_FINAL-BMEIN = WA_STKO-BMEIN .
*    WA_FINAL-BMENG = WA_STKO-BMENG.
*    WA_FINAL-STLST = WA_STKO-STLST.
*    ENDIF.
*    READ TABLE GT_STPO INTO WA_STPO WITH KEY STLNR = WA_FINAL-STLNR STLKN = WA_MAST-STLAN. .
*    WA_FINAL-STLTY = WA_STPO-STLTY.
*    WA_FINAL-STPOZ = WA_STPO-STPOZ.
*    WA_FINAL-DATUV = WA_STPO-DATUV.
*    WA_FINAL-LKENZ = WA_STPO-LKENZ.
*    WA_FINAL-MEINS = WA_STPO-MEINS.
*    WA_FINAL-MENGE = WA_STPO-MENGE.
*    WA_FINAL-IDNRK = WA_STPO-IDNRK.
*    WA_FINAL-STLKN = WA_STPO-STLKN.
*    WA_FINAL-POSTP = WA_STPO-POSTP.
*    WA_FINAL-POSNR = WA_STPO-POSNR.
*    "WA_FINAL-CMEINS = WA_STPO-MEINS.
*    WA_FINAL-FMENG = WA_STPO-FMENG.
*    WA_FINAL-AUSCH = WA_STPO-AUSCH.
*    WA_FINAL-AVOAU = WA_STPO-AVOAU.
*    WA_FINAL-MATKL = WA_STPO-MATKL.
*    "WA_FINAL-STKOZ = WA_STPO-STKOZ.
*    READ TABLE GT_MARA INTO WA_MARA WITH KEY  MATNR  =  WA_MAST-MATNR  .
*    WA_FINAL-MTART = WA_MARA-MTART.
*    WA_FINAL-BEGRU = WA_MARA-BEGRU .
*    READ TABLE GT_MAKT INTO WA_MAKT WITH KEY MATNR = WA_MARA-MATNR.
*    WA_FINAL-MAKTX = WA_MAKT-MAKTX.
*    SHIFT WA_FINAL-MATNR LEFT DELETING LEADING '0'.
*    SHIFT WA_FINAL-IDNRK LEFT DELETING LEADING '0'.
*    "SHIFT WA_FINAL-IDNRK LEFT DELETING LEADING '0'.
*    APPEND WA_FINAL TO GT_FINAL.
*    CLEAR WA_FINAL.
*  ENDLOOP.

  LOOP AT GT_STPO INTO WA_STPO.
    WA_FINAL-STLNR = WA_STPO-STLNR.
    WA_FINAL-STLTY = WA_STPO-STLTY.
    WA_FINAL-STPOZ = WA_STPO-STPOZ.
    WA_FINAL-DATUV = WA_STPO-DATUV.
    WA_FINAL-LKENZ = WA_STPO-LKENZ.
    WA_FINAL-MEINS = WA_STPO-MEINS.
    WA_FINAL-MENGE = WA_STPO-MENGE.
    WA_FINAL-IDNRK = WA_STPO-IDNRK.
    WA_FINAL-STLKN = WA_STPO-STLKN.
    WA_FINAL-POSTP = WA_STPO-POSTP.
    WA_FINAL-POSNR = WA_STPO-POSNR.
    "WA_FINAL-CMEINS = WA_STPO-MEINS.
    WA_FINAL-FMENG = WA_STPO-FMENG.
    WA_FINAL-AUSCH = WA_STPO-AUSCH.
    WA_FINAL-AVOAU = WA_STPO-AVOAU.
    WA_FINAL-MATKL = WA_STPO-MATKL.
    shift wa_final-IDNRK LEFT DELETING LEADING '0' .
    "WA_FINAL-STKOZ = WA_STPO-STKOZ.
    READ TABLE GT_STKO INTO WA_STKO WITH KEY STLNR = WA_FINAL-STLNR .
    IF SY-SUBRC EQ 0 .
      WA_FINAL-BMEIN = WA_STKO-BMEIN .
      WA_FINAL-BMENG = WA_STKO-BMENG.
      WA_FINAL-STLST = WA_STKO-STLST.
    ENDIF.
    READ TABLE GT_MAST INTO WA_MAST WITH KEY STLNR = WA_FINAL-STLNR stlal = wa_stko-STLAL .
    IF SY-SUBRC EQ 0 .
      WA_FINAL-MATNR = WA_MAST-MATNR.
      WA_FINAL-WERKS = WA_MAST-WERKS.
      WA_FINAL-STLAN = WA_MAST-STLAN.
      WA_FINAL-STLAL = WA_MAST-STLAL.
      WA_FINAL-ANDAT = WA_MAST-ANDAT.
    ENDIF.
    READ TABLE GT_MAKT INTO WA_MAKT WITH KEY MATNR = WA_FINAL-MATNR.
    IF SY-SUBRC EQ 0 .
      WA_FINAL-MAKTX = WA_MAKT-MAKTX.
    ENDIF.
    READ TABLE GT_MARA INTO WA_MARA WITH KEY MATNR  = WA_MAST-MATNR  .
    IF SY-SUBRC EQ 0 .
      WA_FINAL-MTART = WA_MARA-MTART.
      WA_FINAL-BEGRU = WA_MARA-BEGRU .
    ENDIF.

   READ TABLE GT1_MAKT INTO WA1_MAKT WITH KEY MATNR = WA_STPO-IDNRK.
    IF SY-SUBRC EQ 0 .
      WA_FINAL-DIDNRK = WA1_MAKT-MAKTX.
    ENDIF.

    APPEND WA_FINAL TO GT_FINAL.
    CLEAR : WA_FINAL, wa_stko, wa_mara, wa_makt, wa_mast, wa1_makt.
  ENDLOOP.

SORT GT_FINAL BY STLNR .

DELETE GT_FINAL WHERE MTART NOT IN SO_MTART .

ENDFORM.                    " READ_DATA

*&---------------------------------------------------------------------*
*&      Form  FIELD_CATLOG
*&---------------------------------------------------------------------*
FORM FIELD_CATLOG .

  PERFORM ALV_LAYOUT USING 1 'Material' 'MATNR' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 2 'Material Description' 'MAKTX' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 3 'Plant' 'WERKS' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 4 'BOM Usage' 'STLAN' 'GT_FINAL' ''.
  "PERFORM ALV_LAYOUT USING 5 'BOM' 'STLNR' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 6 'Alt BOM' 'STLAL' 'GT_FINAL' ''.
  "PERFORM ALV_LAYOUT USING 7 'BOM Category' 'STLTY' 'GT_FINAL' ''.
  " PERFORM ALV_LAYOUT USING 8 'Node' 'STLKN' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 10 'Valid From' 'DATUV' 'GT_FINAL' ''.
  " PERFORM ALV_LAYOUT USING 12 'Deltion Ind' 'LKENZ' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 14 'Base unit' 'BMEIN' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 16 'Base quantity' 'BMENG' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 17 'Created on' 'ANDAT' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 18 'Component' 'IDNRK' 'GT_FINAL' ''.

  PERFORM ALV_LAYOUT USING 19 'Component Desc' 'DIDNRK' 'GT_FINAL' ''.
  "PERFORM ALV_LAYOUT USING 19 'Item Category' 'POSTP' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 20 'Item' 'POSNR' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 22 'Unit' 'MEINS' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 24 'Quantity' 'MENGE' 'GT_FINAL' ''.
  "PERFORM ALV_LAYOUT USING 26 'Component Scrap' 'AUSCH' 'GT_FINAL' ''.
  "PERFORM ALV_LAYOUT USING 28 'Operation scrap' 'AVOAU' 'GT_FINAL' ''.
  "PERFORM ALV_LAYOUT USING 32 'Material Group' 'MATKL' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 36 'Material Type' 'MTART' 'GT_FINAL' ''.
  "PERFORM ALV_LAYOUT USING 6 'Purch Document' 'EBELN' 'GT_FINAL' ''.

  WA_SORT-FIELDNAME = 'MATNR'.
  WA_SORT-UP = 'X'.
  APPEND WA_SORT TO IT_SORT.
  CLEAR WA_SORT.

  WA_SORT-FIELDNAME = 'POSNR'.
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
  LS_LINE-INFO = 'BOM Report' .
  APPEND LS_LINE TO LIT_HEADER.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = LIT_HEADER
      I_LOGO             = 'ZLOGO'.

ENDFORM.                    "ALV_CATALOG_HEADER
*&---------------------------------------------------------------------*
*&      Form  AUTHCHECK_OVERVIEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM AUTHCHECK_OVERVIEW .

*LOOP AT GT_FINAL INTO WA_FINAL.
*  AUTHORITY-CHECK OBJECT 'ZMTART'
*                           ID 'MTART' FIELD WA_FINAL-MTART. " ID 'ZBEGRU' FIELD WA_FINAL-BEGRU.
*  IF SY-SUBRC NE 0.
*           WA_FINAL-FLAG = 'X' .
*           MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING FLAG.
*           MESSAGE 'NO AUTHORIZATION FOR CERTAIN RECORDS' TYPE 'S'.
*  ENDIF.
*  CLEAR : WA_FINAL.
*ENDLOOP.
*LOOP AT GT_FINAL INTO WA_FINAL.
*AUTHORITY-CHECK OBJECT 'ZBEGRU'
*         ID 'BEGRU' FIELD WA_FINAL-BEGRU.
*IF SY-SUBRC <> 0.
** Implement a suitable exception handling here
*ENDIF.
*CLEAR WA_fINAL.
*ENDLOOP.


*AUTHORITY-CHECK OBJECT 'ZBEGRU'
*         ID 'BEGRU' FIELD '__________'.
*IF SY-SUBRC <> 0.
** Implement a suitable exception handling here
*ENDIF.


*  LOOP AT IT_FINOVR INTO WA_FINOVR.
*    AUTHORITY-CHECK OBJECT 'ZINVOICE'
*    ID 'ZVKBUR' FIELD WA_FINOVR-VKBUR
*    ID 'ZSPART' FIELD WA_FINOVR-SPART
*   " ID 'ZSPART' DUMMY
*    ID 'ACTVT' FIELD '03'.
*    IF SY-SUBRC NE 0.
*      WA_FINOVR-FLAG = 'x' .
*      MODIFY IT_FINOVR FROM WA_FINOVR TRANSPORTING FLAG.
*      MESSAGE 'NO AUTHORIZATION FOR CERTAIN RECORDS' TYPE 'S'.
*    ENDIF.
*    CLEAR: WA_FINOVR.
*  ENDLOOP.


*DELETE GT_FINAL WHERE MATNR = WA_FINAL-MATNR .
*    ID 'ACTVT' FIELD '03'

*LOOP AT GT_FINAL INTO WA_FINAL.
*CALL FUNCTION 'BEGRU_MAR_AUTHORITY_CHECK'
*  EXPORTING
*    AKTYP              = 3
**   FLG_DUMMY          =
*    BEGRU              = WA_FINAL-BEGRU
*     EXCEPTIONS
*   NO_AUTHORITY       = 1 .
**   OTHERS             = 2
*          .
**IF SY-SUBRC <> 0.
*** Implement suitable error handling here
**ENDIF.
*
*IF SY-SUBRC NE 0 .
*  DELETE GT_FINAL WHERE MATNR = WA_FINAL-MATNR .
*  ENDIF.
*
*ENDLOOP.

  LOOP AT GT_FINAL INTO WA_FINAL.
    AUTHORITY-CHECK OBJECT 'ZBEGRU'
             ID 'BEGRU' FIELD WA_FINAL-BEGRU .
    IF SY-SUBRC <> 0.
      WA_FINAL-FLAG = 'X' .
      MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING FLAG.
* Implement a suitable exception handling here
    ENDIF.
    CLEAR WA_FINAL.
  ENDLOOP.
  DELETE GT_FINAL WHERE FLAG = 'X' .
ENDFORM.                    " AUTHCHECK_OVERVIEW
