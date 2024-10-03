*&---------------------------------------------------------------------*
*& Report  ZRM_PM_PRD_REP
*&*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&Functional                   : Mr.Umapathy        *
*& Developer                   : Mr.Ramachandaran                      *
*& Company                     : Sheenlac Paints Pvt Ltd               *
*& Verified By                 : Mr.Govindarajan                                      *
*& Title                       : RM / PM PROCUREMENT DETAILS       *
*& Report Name                 : ZRM_PM_PRD_REP                          *
*& Development Id              : kpabap                                *
*& Related Information         : RM / PM PROCUREMENT DETAILS(PRODUCTION)       *
*&---------------------------------------------------------------------*
REPORT ZRM_PM_PRD_REP.

TYPES: BEGIN OF ES_RESB,                                                                           " Table 1
           WERKS TYPE RESB-WERKS,
           RSNUM TYPE RESB-RSNUM,
           AUFNR TYPE RESB-AUFNR,
           PLNUM TYPE RESB-PLNUM,
           XLOEK TYPE RESB-XLOEK,
           KZEAR TYPE RESB-KZEAR,
           BDTER TYPE RESB-BDTER,
           BAUGR TYPE RESB-BAUGR,
           MATNR TYPE RESB-MATNR,
           BWART TYPE RESB-BWART,
           BDMNG TYPE RESB-BDMNG,
           LGORT TYPE RESB-LGORT,
           MEINS TYPE RESB-MEINS,
           ENMNG TYPE RESB-ENMNG,
       END OF ES_RESB.

DATA: GT_RESB TYPE TABLE OF ES_RESB,
     WA_RESB TYPE ES_RESB.

TYPES: BEGIN OF ES_MARA,
        MATNR TYPE MARA-MATNR,
        MATKL TYPE MARA-MATKL,
        MTART TYPE MARA-MTART,
      END OF ES_MARA.

DATA: GT_MARA TYPE TABLE OF ES_MARA,
      WA_MARA TYPE ES_MARA.

TYPES:BEGIN OF ES_MARD,                                                                      " TABLE 3
       MATNR TYPE MARD-MATNR,
       WERKS TYPE MARD-WERKS,
       LGORT TYPE MARD-LGORT,
       LABST TYPE MARD-LABST,
       INSME TYPE MARD-INSME,
     END OF ES_MARD.

DATA: GT_MARD TYPE TABLE OF ES_MARD,
      WA_MARD TYPE ES_MARD.

TYPES:BEGIN OF ES_MARC,
      MATNR TYPE MARC-MATNR,
      WERKS TYPE MARD-WERKS,
      TRAME TYPE MARC-TRAME,
     END OF ES_MARC.

DATA: GT_MARC TYPE TABLE OF ES_MARC,
      WA_MARC TYPE ES_MARC.

TYPES:BEGIN OF ES_MAKT,
      MATNR TYPE MAKT-MATNR,
      MAKTX TYPE MAKT-MAKTX,
     END OF ES_MAKT.

DATA: GT_MAKT TYPE TABLE OF ES_MAKT,
      WA_MAKT TYPE ES_MAKT.

DATA: GT_MAKT1 TYPE TABLE OF ES_MAKT,
      WA_MAKT1 TYPE ES_MAKT.

TYPES:BEGIN OF ES_T023T,
      MATKL TYPE T023T-MATKL,
      WGBEZ TYPE T023T-WGBEZ,
     END OF ES_T023T.

DATA: GT_T023T TYPE TABLE OF ES_T023T,
      WA_T023T TYPE ES_T023T.

TYPES : BEGIN OF GS_FINAL,
           WERKS TYPE RESB-WERKS,
           RSNUM TYPE RESB-RSNUM,
           AUFNR TYPE RESB-AUFNR,
           PLNUM TYPE RESB-PLNUM,
           XLOEK TYPE RESB-XLOEK,
           KZEAR TYPE RESB-KZEAR,
           BDTER TYPE RESB-BDTER,
           BAUGR TYPE RESB-BAUGR,
           MATNR TYPE RESB-MATNR,
           BWART TYPE RESB-BWART,
           BDMNG TYPE RESB-BDMNG,
           LGORT TYPE RESB-LGORT,
           MEINS TYPE RESB-MEINS,
           ENMNG TYPE RESB-ENMNG,
           MTART TYPE MARA-MTART,
           MATKL TYPE MARA-MATKL,
           LABST TYPE MARD-LABST,
           INSME TYPE MARD-INSME,
           MAKTX TYPE MAKT-MAKTX,
           WGBEZ TYPE T023T-WGBEZ,
           TRAME TYPE MARC-TRAME,
           MAKTX1 TYPE MAKT-MAKTX,             "finished goods Desc
           MIS_PAT TYPE P DECIMALS 3,
          V_COUNT TYPE I, " Added By Govind On 10.12.2014
          V_COUNT1 TYPE I,

          TOT_QTY TYPE P DECIMALS 3,

 END OF GS_FINAL.

DATA : GT_FINAL TYPE TABLE OF GS_FINAL,
        WA_FINAL TYPE GS_FINAL.

DATA : GT_FCAT TYPE SLIS_T_FIELDCAT_ALV,
      IT_SORT TYPE SLIS_T_SORTINFO_ALV,
      WA_FCAT TYPE SLIS_FIELDCAT_ALV,
      WA_SORT TYPE SLIS_SORTINFO_ALV.

DATA :  LV_AUFNR TYPE RESB-AUFNR,
        LV_PLNUM TYPE RESB-PLNUM,
        LV_BDTER TYPE RESB-BDTER,
        LV_WERKS TYPE RESB-WERKS,
        LV_MATNR TYPE RESB-MATNR,
        LV_MTART TYPE MARA-MTART.

DATA : LAYOUT TYPE SLIS_LAYOUT_ALV.

DATA :  LV_SUM TYPE RESB-BDMNG.

DATA : V_COUNT TYPE I VALUE 0.

DATA: LV_COUNT TYPE SY-TABIX. " Added By Govind On 10.12.2014

DATA: LV_COUNT1 TYPE SY-TABIX. " Added By Govind On 10.12.2014

SELECTION-SCREEN : BEGIN OF BLOCK C1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS  : SO_AUFNR FOR LV_AUFNR ,
                  SO_PLNUM FOR LV_PLNUM ,
                  SO_BDTER FOR LV_BDTER OBLIGATORY,
                  SO_WERKS FOR LV_WERKS ,
                  SO_MATNR FOR LV_MATNR ,
                  SO_MTART FOR LV_MTART NO-DISPLAY.
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
  SELECT
        WERKS
        RSNUM
        AUFNR
        PLNUM
        XLOEK
        KZEAR
        BDTER
        BAUGR
        MATNR
        BWART
        BDMNG
        LGORT
        MEINS
        ENMNG
    FROM RESB INTO TABLE GT_RESB WHERE AUFNR IN SO_AUFNR AND PLNUM IN SO_PLNUM AND BDTER IN SO_BDTER AND WERKS IN SO_WERKS AND MATNR IN SO_MATNR AND XLOEK <> 'X' AND KZEAR <> 'X' .
  " BREAK-POINT.

  SORT GT_RESB BY WERKS . " RSNUM AUFNR PLNUM BDTER.

  IF GT_RESB[] IS NOT INITIAL.

    SELECT
       MATNR
       MATKL
       MTART
    FROM MARA INTO TABLE GT_MARA FOR ALL ENTRIES IN GT_RESB WHERE MATNR = GT_RESB-MATNR AND MTART IN SO_MTART .

    "IF GT_MARA[] IS NOT INITIAL.
    SELECT
   MATKL
   WGBEZ
    FROM T023T INTO TABLE GT_T023T FOR ALL ENTRIES IN GT_MARA WHERE MATKL = GT_MARA-MATKL .

    SELECT
      MATNR
      WERKS
      LGORT
      LABST
      INSME
   FROM MARD INTO TABLE GT_MARD FOR ALL ENTRIES IN GT_RESB WHERE MATNR = GT_RESB-MATNR AND WERKS = GT_RESB-WERKS .

    SELECT
      MATNR
      WERKS
      TRAME
   FROM MARC INTO TABLE GT_MARC FOR ALL ENTRIES IN GT_RESB WHERE MATNR = GT_RESB-MATNR AND WERKS = GT_RESB-WERKS .

    SELECT
      MATNR
      MAKTX
       FROM MAKT INTO TABLE GT_MAKT FOR ALL ENTRIES IN GT_RESB WHERE MATNR = GT_RESB-MATNR .

    SELECT
    MATNR
    MAKTX
     FROM MAKT INTO TABLE GT_MAKT1 FOR ALL ENTRIES IN GT_RESB WHERE MATNR = GT_RESB-BAUGR .
    " ENDIF.

  ENDIF.
  DELETE GT_RESB WHERE AUFNR = ' ' AND PLNUM = ' '.

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
  LOOP AT GT_RESB INTO WA_RESB.
    MOVE-CORRESPONDING WA_RESB TO WA_FINAL.

    READ TABLE GT_MARA INTO WA_MARA WITH KEY  MATNR  =    WA_RESB-MATNR  .
    WA_FINAL-MTART = WA_MARA-MTART.

    READ TABLE GT_T023T INTO WA_T023T WITH KEY  MATKL  =  WA_MARA-MATKL  .
    WA_FINAL-WGBEZ = WA_T023T-WGBEZ.

    READ TABLE GT_MAKT1 INTO WA_MAKT1 WITH KEY  MATNR  =  WA_RESB-BAUGR  .
    WA_FINAL-MAKTX1 = WA_MAKT1-MAKTX.

    READ TABLE GT_MAKT INTO WA_MAKT WITH KEY  MATNR  =  WA_RESB-MATNR  .
    WA_FINAL-MAKTX = WA_MAKT-MAKTX.

*    READ TABLE GT_MARC INTO WA_MARC WITH KEY  MATNR  =  WA_RESB-MATNR  .
*    WA_FINAL-TRAME = WA_MARC-TRAME.

    APPEND WA_FINAL TO GT_FINAL.
    CLEAR WA_FINAL.
  ENDLOOP.

  LOOP AT GT_FINAL INTO WA_FINAL.
    SHIFT WA_FINAL-RSNUM LEFT DELETING LEADING '0'.
    SHIFT WA_FINAL-AUFNR LEFT DELETING LEADING '0'.
    SHIFT WA_FINAL-PLNUM LEFT DELETING LEADING '0'.
    SHIFT WA_FINAL-BAUGR LEFT DELETING LEADING '0'.
    SHIFT WA_FINAL-MATNR LEFT DELETING LEADING '0'.
    SHIFT WA_FINAL-BWART LEFT DELETING LEADING '0'.
    MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING  RSNUM AUFNR PLNUM BAUGR MATNR BWART.
    CLEAR WA_FINAL.
  ENDLOOP.
  DELETE GT_FINAL WHERE MTART EQ ' '.
  " BREAK-POINT .
  SORT GT_FINAL BY WERKS MATNR .
  LOOP AT GT_FINAL INTO WA_FINAL.

*********** " Added By Govind On 10.12.2014 ********************
    "   WA_FINAL-MIS_PAT = WA_FINAL-BDMNG - ( WA_FINAL-ENMNG + WA_FINAL-LABST + WA_FINAL-INSME + WA_FINAL-TRAME ) .
    LV_COUNT = LV_COUNT  + 1. " Added By Govind On 10.12.2014
    ON CHANGE OF WA_FINAL-MATNR.
      WA_FINAL-V_COUNT = LV_COUNT.
      MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING  V_COUNT  .
      CLEAR WA_FINAL.
    ENDON .
  ENDLOOP.
  SORT GT_FINAL BY WERKS MATNR.
  LOOP AT  GT_MARD INTO WA_MARD.
    SHIFT WA_MARD-MATNR LEFT DELETING LEADING '0'.
    MODIFY GT_MARD FROM WA_MARD TRANSPORTING MATNR.
    CLEAR WA_MARD.
  ENDLOOP.
  SORT GT_FINAL BY  WERKS MATNR LABST DESCENDING.

  LOOP AT GT_FINAL INTO WA_FINAL.
    LOOP AT  GT_MARD INTO WA_MARD WHERE  MATNR  =  WA_FINAL-MATNR  AND WERKS = WA_FINAL-WERKS .
      SHIFT WA_MARD-MATNR LEFT DELETING LEADING '0'.
      WA_FINAL-LABST = WA_FINAL-LABST + WA_MARD-LABST.
      WA_FINAL-INSME = WA_FINAL-INSME + WA_MARD-INSME.
      IF WA_FINAL-V_COUNT GT 0.
        MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING LABST INSME  .
      ENDIF.
    ENDLOOP.
    CLEAR WA_FINAL .
  ENDLOOP.
*  SORT GT_FINAL BY WERKS MATNR  BDTER.
  LOOP AT  GT_MARC INTO WA_MARC.
    SHIFT WA_MARC-MATNR LEFT DELETING LEADING '0'.
    MODIFY GT_MARC FROM WA_MARC TRANSPORTING MATNR.
    CLEAR WA_MARC.
  ENDLOOP.
  LOOP AT GT_FINAL INTO WA_FINAL.
    LOOP AT  GT_MARC INTO WA_MARC WHERE  MATNR  =  WA_FINAL-MATNR  AND WERKS = WA_FINAL-WERKS .
      SHIFT WA_MARC-MATNR LEFT DELETING LEADING '0'.
      WA_FINAL-TRAME = WA_FINAL-TRAME + WA_MARC-TRAME.
      IF WA_FINAL-V_COUNT GT 0.
        MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING TRAME  .
      ENDIF.
    ENDLOOP.
    CLEAR WA_FINAL .
  ENDLOOP.
  LOOP AT GT_FINAL INTO WA_FINAL.
    WA_FINAL-MIS_PAT = WA_FINAL-BDMNG - ( WA_FINAL-ENMNG + WA_FINAL-LABST + WA_FINAL-INSME + WA_FINAL-TRAME ) .
    MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING  MIS_PAT .
    CLEAR WA_FINAL.
  ENDLOOP.

  LOOP AT GT_FINAL INTO WA_FINAL.
    ON CHANGE OF WA_FINAL-MATNR OR WA_FINAL-WERKS .
      CLEAR LV_SUM.
    ENDON.
    IF WA_FINAL-MATNR NE ''.
      IF WA_FINAL-BDMNG NE 0.
        LV_SUM = LV_SUM + WA_FINAL-BDMNG - ( WA_FINAL-ENMNG + WA_FINAL-LABST + WA_FINAL-INSME + WA_FINAL-TRAME ).
        WA_FINAL-TOT_QTY = LV_SUM.
        MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING TOT_QTY.
        CLEAR WA_FINAL.
      ENDIF.
    ENDIF.
  ENDLOOP.
*   SORT GT_FINAL BY LABST  WERKS MATNR    DESCENDING .

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

  PERFORM ALV_LAYOUT USING 1 'Plant' 'WERKS' 'GT_FINAL' '' '' 'X'.
  PERFORM ALV_LAYOUT USING 3 'Reservation' 'RSNUM' 'GT_FINAL' '' '' 'X'.
  PERFORM ALV_LAYOUT USING 5 'Process Order' 'AUFNR' 'GT_FINAL' '' '' 'X'.
  PERFORM ALV_LAYOUT USING 7 'Planned Order' 'PLNUM' 'GT_FINAL' '' '' 'X'.

  PERFORM ALV_LAYOUT USING 11 'FG Code' 'BAUGR' 'GT_FINAL' '' '' 'X'.
  PERFORM ALV_LAYOUT USING 12 'Material Description' 'MAKTX1' 'GT_FINAL' '' '' 'X'.
  PERFORM ALV_LAYOUT USING 13 'Req.Date' 'BDTER' 'GT_FINAL' '' '' 'X'.
  " PERFORM ALV_LAYOUT USING 13 'Material Grp Desc' 'BEZEI' 'GT_FINAL' '' ''.
  PERFORM ALV_LAYOUT USING 14 'Material Code' 'MATNR' 'GT_FINAL' '' '' 'X'.
  PERFORM ALV_LAYOUT USING 15 'Material Description' 'MAKTX' 'GT_FINAL' '' '' ''.
  PERFORM ALV_LAYOUT USING 16 'Material Group' 'WGBEZ' 'GT_FINAL' '' '' ''.
  PERFORM ALV_LAYOUT USING 17 'Material Type' 'MTART' 'GT_FINAL' '' '' ''.
  PERFORM ALV_LAYOUT USING 18 'Req Qty' 'BDMNG' 'GT_FINAL' 'X' '' ''.
  PERFORM ALV_LAYOUT USING 19 'UOM' 'MEINS' 'GT_FINAL' '' '' ''.
  PERFORM ALV_LAYOUT USING 20 'Qty Withdrawn' 'ENMNG' 'GT_FINAL' 'X' 'X' ''.
  PERFORM ALV_LAYOUT USING 22 'Closing Stock' 'LABST' 'GT_FINAL' 'X' 'X' ''.
  PERFORM ALV_LAYOUT USING 24 'Quality' 'INSME' 'GT_FINAL' 'X' 'X' ''.
  PERFORM ALV_LAYOUT USING 27 'Stock In Transit' 'TRAME' 'GT_FINAL' 'X' 'X' ''.
  " PERFORM ALV_LAYOUT USING 28 'Missing part' 'MIS_PAT' 'GT_FINAL' 'X' '' ''.

  PERFORM ALV_LAYOUT USING 29 'Missing part' 'TOT_QTY' 'GT_FINAL' '' '' ''.

  " PERFORM ALV_LAYOUT USING 29 'Count' 'V_COUNT' 'GT_FINAL' 'X' '' ''.

  WA_SORT-FIELDNAME = 'WERKS'.
  WA_SORT-UP = 'X'.
  APPEND WA_SORT TO IT_SORT.
  CLEAR WA_SORT.
*
*  WA_SORT-FIELDNAME = 'RSNUM'.
*  WA_SORT-UP = 'X'.
*  APPEND WA_SORT TO IT_SORT.
*  CLEAR WA_SORT.

*  WA_SORT-FIELDNAME = 'AUFNR'.
*  WA_SORT-UP = 'X'.
*  APPEND WA_SORT TO IT_SORT.
*  CLEAR WA_SORT.

*  WA_SORT-FIELDNAME = 'PLNUM'.
*  WA_SORT-UP = 'X'.
*  APPEND WA_SORT TO IT_SORT.
*  CLEAR WA_SORT.

*  WA_SORT-FIELDNAME = 'BAUGR'.
*  WA_SORT-UP = 'X'.
*  APPEND WA_SORT TO IT_SORT.
*  CLEAR WA_SORT.

  WA_SORT-FIELDNAME = 'MATNR'.
  WA_SORT-UP = 'X'.
   WA_SORT-SUBTOT = 'X'.
  APPEND WA_SORT TO IT_SORT.
  CLEAR WA_SORT.

*    WA_SORT-FIELDNAME = 'BDTER'.
*  WA_SORT-UP = 'X'.
*  APPEND WA_SORT TO IT_SORT.
*  CLEAR WA_SORT.

ENDFORM.                    " FIELD_CATLOG


*&---------------------------------------------------------------------*
*&      Form  ALV_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
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
  LS_LINE-INFO = 'RM / PM PROCUREMENT DETAILS' .
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

*&---------------------------------------------------------------------*
*&      Form  ALV_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0481   text
*----------------------------------------------------------------------*
FORM ALV_LAYOUT  USING P1 P2 P3 P4 P5 P6 P7.
  CLEAR WA_FCAT.
  WA_FCAT-COL_POS = P1.
  WA_FCAT-SELTEXT_L = P2.
  WA_FCAT-FIELDNAME = P3.
  WA_FCAT-TABNAME = P4.
  WA_FCAT-DO_SUM = P5.
  WA_FCAT-NO_ZERO = P6.
  WA_FCAT-KEY = P7.
  APPEND WA_FCAT TO GT_FCAT.

ENDFORM.                    " ALV_LAYOUT
