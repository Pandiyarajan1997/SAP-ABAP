*&---------------------------------------------------------------------*
*& Report  ZRM_PM_PUR_REP
*&*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&Functional                   : Mr.Umapathy        *
*& Developer                   : Mr.Ramachandaran                      *
*& Company                     : Sheenlac Paints Pvt Ltd               *
*& Verified By                 : Mr.Govindarajan                                      *
*& Title                       : RM / PM PROCUREMENT DETAILS         *
*& Report Name                 : ZRM_PM_PUR_REP                         *
*& Development Id              : kpabap                                *
*& Related Information         : RM / PM PROCUREMENT DETAILS (PURCHASE)        *
*&---------------------------------------------------------------------*

REPORT ZRM_PM_PUR_REP.

TYPES: BEGIN OF ES_RESB,
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

TYPES:BEGIN OF ES_MARD,
       MATNR TYPE MARD-MATNR,
       WERKS TYPE MARD-WERKS,
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
           ENMNG TYPE P DECIMALS 3 ,"RESB-ENMNG,
           MTART TYPE MARA-MTART,
           MATKL TYPE MARA-MATKL,
           LABST TYPE P DECIMALS 3 ," MARD-LABST,
           INSME TYPE P DECIMALS 3 , "MARD-INSME,
           TRAME TYPE P DECIMALS 3 , "MARC-TRAME,
           MAKTX TYPE MAKT-MAKTX,
           MAKTX1 TYPE MAKT-MAKTX,   " Finished goods Desc
           WGBEZ TYPE T023T-WGBEZ,
           REQ_QTY  TYPE P DECIMALS 3,
           CLO_STK  TYPE P DECIMALS 3,
           TOT_REQ TYPE P DECIMALS 3,
           TOT_REQ1 TYPE P DECIMALS 3,
           TOT_REQ2 TYPE P DECIMALS 3,
           PER_REQ TYPE P DECIMALS 1,
           PER_REQ1  TYPE P DECIMALS 3 ,
           CELLCOLOR TYPE LVC_T_SCOL,
           V_COUNT TYPE I,
           DAY TYPE I,
 END OF GS_FINAL.

DATA : GT_FINAL TYPE TABLE OF GS_FINAL,
        WA_FINAL TYPE GS_FINAL.

DATA : GT_FCAT TYPE SLIS_T_FIELDCAT_ALV,
      IT_SORT TYPE SLIS_T_SORTINFO_ALV,
      WA_FCAT TYPE SLIS_FIELDCAT_ALV,
      WA_SORT TYPE SLIS_SORTINFO_ALV.

DATA :  LV_BDTER TYPE RESB-BDTER,
        LV_WERKS TYPE RESB-WERKS,
        LV_MATNR TYPE RESB-MATNR,
        LV_MTART TYPE MARA-MTART,
        LV_MATKL TYPE T023T-MATKL,
 GD_LAYOUT    TYPE SLIS_LAYOUT_ALV.

DATA : LAYOUT TYPE SLIS_LAYOUT_ALV.

DATA : V_COUNT TYPE I VALUE 0.
DATA : LV_SUM TYPE RESB-BDMNG.
DATA : LV_SUM1 TYPE RESB-BDMNG.

DATA : LV_DAYS TYPE I.

DATA  : DA_MAX LIKE RESB-BDTER .


DATA: LV_COUNT TYPE SY-TABIX.

DATA: LV_COUNT1 TYPE SY-TABIX.

SELECTION-SCREEN : BEGIN OF BLOCK C1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS  : SO_BDTER FOR LV_BDTER OBLIGATORY,
                  SO_WERKS FOR LV_WERKS ,
                  SO_MATNR FOR LV_MATNR ,
                  SO_MTART FOR LV_MTART NO-DISPLAY,
                  SO_MATKL FOR LV_MATKL .

PARAMETERS : P_DATE TYPE SY-DATUM DEFAULT SY-DATUM NO-DISPLAY.

SELECTION-SCREEN : END OF BLOCK C1.

START-OF-SELECTION.
  PERFORM GET_DATA.
  PERFORM READ_DATA.

  PERFORM BUILD_LAYOUT .

END-OF-SELECTION.
  PERFORM FIELD_CATLOG.
  PERFORM SET_CELL_COLOURS .  "ADDED BY RAM ON 4/12/2014
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
         FROM RESB INTO TABLE GT_RESB WHERE BDTER IN SO_BDTER AND WERKS IN SO_WERKS AND MATNR IN SO_MATNR AND XLOEK <> 'X' AND KZEAR <> 'X' . " AND ( RSNUM NE 'X' OR AUFNR NE 'X' ).
 " BREAK-POINT .
  SORT GT_RESB BY WERKS .

  DELETE GT_RESB WHERE AUFNR = ' ' AND PLNUM = ' ' .

  IF GT_RESB[] IS NOT INITIAL.
    SELECT
       MATNR
       MATKL
       MTART
    FROM MARA INTO TABLE GT_MARA FOR ALL ENTRIES IN GT_RESB WHERE MATNR = GT_RESB-MATNR AND MTART IN SO_MTART .

    SELECT
      MATKL
      WGBEZ
    FROM T023T INTO TABLE GT_T023T FOR ALL ENTRIES IN GT_MARA WHERE MATKL = GT_MARA-MATKL AND WGBEZ IN SO_MATKL .

    SELECT
      MATNR
      WERKS
      LABST
      INSME
   FROM MARD INTO TABLE GT_MARD FOR ALL ENTRIES IN GT_RESB WHERE MATNR = GT_RESB-MATNR .

    SELECT
       MATNR
       WERKS
       TRAME
    FROM MARC INTO TABLE GT_MARC FOR ALL ENTRIES IN GT_RESB WHERE MATNR = GT_RESB-MATNR .

    SELECT
      MATNR
      MAKTX
       FROM MAKT INTO TABLE GT_MAKT FOR ALL ENTRIES IN GT_RESB WHERE MATNR = GT_RESB-MATNR .

    SELECT
    MATNR
    MAKTX
     FROM MAKT INTO TABLE GT_MAKT1 FOR ALL ENTRIES IN GT_RESB WHERE MATNR = GT_RESB-MATNR .

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
  LOOP AT GT_RESB INTO WA_RESB.
    MOVE-CORRESPONDING WA_RESB TO WA_FINAL.

    READ TABLE GT_MARA INTO WA_MARA WITH KEY  MATNR  =  WA_RESB-MATNR  .
    WA_FINAL-MTART = WA_MARA-MTART.

    READ TABLE GT_T023T INTO WA_T023T WITH KEY  MATKL  =  WA_MARA-MATKL  .
    WA_FINAL-WGBEZ = WA_T023T-WGBEZ.

    READ TABLE GT_MAKT INTO WA_MAKT WITH KEY  MATNR  =  WA_RESB-MATNR  .
    WA_FINAL-MAKTX = WA_MAKT-MAKTX.

   " SELECT SINGLE MAX( BDTER ) INTO DA_MAX FROM RESB WHERE MATNR = WA_RESB-MATNR .



     CALL FUNCTION 'HR_99S_INTERVAL_BETWEEN_DATES'
          EXPORTING
        BEGDA = WA_RESB-BDTER
    "       BEGDA = DA_MAX
            ENDDA = P_DATE
          IMPORTING
            DAYS  = LV_DAYS.
      IF LV_DAYS NE 0.
          WA_FINAL-DAY = LV_DAYS.
      ENDIF.
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
  SORT GT_FINAL BY WERKS MATNR .

  LOOP AT GT_FINAL INTO WA_FINAL.
    " ********** " Added By Govind On 10.12.2014 ********************
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
    WA_FINAL-REQ_QTY = WA_FINAL-BDMNG - WA_FINAL-ENMNG.
    WA_FINAL-CLO_STK = WA_FINAL-LABST + WA_FINAL-INSME.
        MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING  REQ_QTY CLO_STK .
    CLEAR WA_FINAL.
  ENDLOOP.

  LOOP AT GT_FINAL INTO WA_FINAL. " ADDED ON 20/12/2014
    WA_FINAL-TOT_REQ = WA_FINAL-REQ_QTY - WA_FINAL-CLO_STK .
    MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING TOT_REQ.
    CLEAR : WA_FINAL.
  ENDLOOP.

SORT GT_FINAL BY WERKS MATNR CLO_STK  DESCENDING .

LOOP AT GT_FINAL INTO WA_FINAL.
ON CHANGE OF WA_FINAL-MATNR OR WA_FINAL-WERKS.
CLEAR LV_SUM.
ENDON.
 IF WA_FINAL-MATNR NE ''.
  IF WA_FINAL-REQ_QTY NE 0.
     LV_SUM = LV_SUM -  WA_FINAL-CLO_STK + WA_FINAL-REQ_QTY .
     WA_FINAL-TOT_REQ1 = LV_SUM.
      MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING TOT_REQ1.
       CLEAR WA_FINAL.
    ENDIF.
ENDIF.
 ENDLOOP.

 LOOP AT GT_FINAL INTO WA_FINAL.
      IF WA_FINAL-REQ_QTY NE 0.
        WA_FINAL-PER_REQ = WA_FINAL-TOT_REQ1 / WA_FINAL-REQ_QTY * 100.
        MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING PER_REQ.
        CLEAR WA_FINAL.
    ENDIF.
 ENDLOOP.

LOOP AT GT_FINAL INTO WA_FINAL.
  ON CHANGE OF WA_FINAL-MATNR OR WA_FINAL-WERKS.
    CLEAR LV_SUM1.
  ENDON.
 IF WA_FINAL-MATNR NE ''.
   IF WA_FINAL-TOT_REQ1 NE 0 AND WA_FINAL-TOT_REQ1 > 0.
     LV_SUM1 = WA_FINAL-TOT_REQ1 -  LV_SUM1.
     WA_FINAL-TOT_REQ2 = LV_SUM1.
     LV_SUM1 =  WA_FINAL-TOT_REQ1 .
      MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING TOT_REQ2.
      CLEAR : WA_FINAL.
    ENDIF.
ENDIF.
ENDLOOP.

 LOOP AT GT_FINAL INTO WA_FINAL.
   IF WA_FINAL-REQ_QTY NE 0.
      WA_FINAL-PER_REQ = WA_FINAL-TOT_REQ2 / WA_FINAL-REQ_QTY * 100.
      MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING PER_REQ.
      CLEAR WA_FINAL.
   ENDIF.
ENDLOOP.

LOOP AT GT_FINAL INTO WA_FINAL.
  IF WA_FINAL-PER_REQ NE 0.
   IF WA_FINAL-PER_REQ < 0 .
         WA_FINAL-PER_REQ = 0 .
         MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING PER_REQ.
         CLEAR WA_FINAL.
      ENDIF .
   ENDIF.
 ENDLOOP.

 LOOP AT GT_FINAL INTO WA_FINAL .
   IF WA_FINAL-PER_REQ LE 0 OR WA_FINAL-PER_REQ EQ 0.
         WA_FINAL-DAY = 0.
   MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING DAY .
   CLEAR WA_FINAL .
        ENDIF.
ENDLOOP .

*LOOP AT GT_FINAL INTO WA_FINAL .
* AT END OF MATNR .
*   WA_FINAL-DAY = LV_DAYS .
* ENDAT.
*    MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING DAY .
*    CLEAR WA_FINAL .
*ENDLOOP .

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
  PERFORM ALV_LAYOUT USING 1 'Plant' 'WERKS' 'GT_FINAL' '' ''.
  PERFORM ALV_LAYOUT USING 4 'Material Type' 'MTART' 'GT_FINAL' '' ''.
  PERFORM ALV_LAYOUT USING 7 'Material Code' 'MATNR' 'GT_FINAL' '' ''.
  PERFORM ALV_LAYOUT USING 10 'Material Description' 'MAKTX' 'GT_FINAL' '' ''.
  PERFORM ALV_LAYOUT USING 12 'Material Group' 'WGBEZ' 'GT_FINAL' '' ''.
  PERFORM ALV_LAYOUT USING 13 'Req.Date' 'BDTER' 'GT_FINAL' '' ''.
  PERFORM ALV_LAYOUT USING 16 'Req Qty' 'REQ_QTY' 'GT_FINAL' 'X' 'X'.
  PERFORM ALV_LAYOUT USING 17 'UOM' 'MEINS' 'GT_FINAL' '' '' .
  PERFORM ALV_LAYOUT USING 19 'Closing Stock' 'CLO_STK' 'GT_FINAL' 'X' 'X'.
  PERFORM ALV_LAYOUT USING 20 'Transit' 'TRAME' 'GT_FINAL' 'X' 'X'.
  PERFORM ALV_LAYOUT USING 22 'Final Req.Qty' 'TOT_REQ1' 'GT_FINAL' '' ''.
  PERFORM ALV_LAYOUT USING 23 '% Req.Qty' 'PER_REQ' 'GT_FINAL' '' ''.
  PERFORM ALV_LAYOUT USING 25 'No of Delay Days' 'DAY' 'GT_FINAL' '' 'X' .

  WA_SORT-FIELDNAME = 'WERKS'.
  WA_SORT-UP = 'X'.
  APPEND WA_SORT TO IT_SORT.
  CLEAR WA_SORT.

  WA_SORT-FIELDNAME = 'MTART'.
  WA_SORT-UP = 'X'.
  APPEND WA_SORT TO IT_SORT.
  CLEAR WA_SORT.

  WA_SORT-FIELDNAME = 'MATNR'.
  WA_SORT-UP = 'X'.
  WA_SORT-SUBTOT = 'X'.
  APPEND WA_SORT TO IT_SORT.
  CLEAR WA_SORT.

  WA_SORT-FIELDNAME = 'MAKTX'.
  WA_SORT-UP = 'X'.
  APPEND WA_SORT TO IT_SORT.
  CLEAR WA_SORT.

  WA_SORT-FIELDNAME = 'WGBEZ'.
  WA_SORT-UP = 'X'.
  APPEND WA_SORT TO IT_SORT.
  CLEAR WA_SORT.

ENDFORM.                    " FIELD_CATLOG

*&---------------------------------------------------------------------*
*&      Form  BUILD_LAYOUT
*&---------------------------------------------------------------------*
*       Build layout for ALV grid report
*----------------------------------------------------------------------*
FORM BUILD_LAYOUT.
  GD_LAYOUT-NO_INPUT          = 'X'.
  GD_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  "  GD_LAYOUT-TOTALS_TEXT       = 'Totals'(201).
  GD_LAYOUT-COLTAB_FIELDNAME = 'CELLCOLOR'.  "CTAB_FNAME
ENDFORM.                    " BUILD_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  ALV_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ALV_DISPLAY .
*  LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
*  LAYOUT-ZEBRA = 'X'.
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
       IS_LAYOUT                     = GD_LAYOUT
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
*&---------------------------------------------------------------------*
*&      Form  ALV_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0747   text
*      -->P_0748   text
*      -->P_0749   text
*      -->P_0750   text
*----------------------------------------------------------------------*
FORM ALV_LAYOUT USING P1 P2 P3 P4 P5 P6.
  CLEAR WA_FCAT.
  WA_FCAT-COL_POS = P1.
  WA_FCAT-SELTEXT_L = P2.
  WA_FCAT-FIELDNAME = P3.
  WA_FCAT-TABNAME = P4.
  WA_FCAT-DO_SUM = P5.
  WA_FCAT-NO_ZERO = P6.
  APPEND WA_FCAT TO GT_FCAT.
ENDFORM.                    " ALV_LAYOUT

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

" SET_CELL_COLOURS
*&---------------------------------------------------------------------*
*&      Form  SET_CELL_COLOURS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_CELL_COLOURS .
  DATA: WA_CELLCOLOR TYPE LVC_S_SCOL.
  DATA: LD_INDEX TYPE SY-TABIX.

  LOOP AT GT_FINAL INTO WA_FINAL.
    LD_INDEX = SY-TABIX.

    IF WA_FINAL-PER_REQ <= 0 .
      WA_CELLCOLOR-FNAME = 'PER_REQ'.
      WA_CELLCOLOR-COLOR-COL = 5.  "color code 1-7, if outside rage defaults to 7
      WA_CELLCOLOR-COLOR-INT = '1'.  "1 = Intensified on, 0 = Intensified off
      WA_CELLCOLOR-COLOR-INV = '1'.  "1 = text colour, 0 = background colour
      APPEND WA_CELLCOLOR TO WA_FINAL-CELLCOLOR.
      MODIFY GT_FINAL FROM WA_FINAL INDEX LD_INDEX TRANSPORTING CELLCOLOR.
      CLEAR :WA_FINAL , WA_CELLCOLOR.
    ENDIF.

    IF WA_FINAL-PER_REQ GT 0 . " AND WA_FINAL-PER_REQ GT 50 .
      WA_CELLCOLOR-FNAME = 'PER_REQ'.
      WA_CELLCOLOR-COLOR-COL = 6.  "color code 1-7, if outside rage defaults to 7
      WA_CELLCOLOR-COLOR-INT = '1'.  "1 = Intensified on, 0 = Intensified off
      WA_CELLCOLOR-COLOR-INV = '1'.  "1 = text colour, 0 = background colour
      APPEND WA_CELLCOLOR TO WA_FINAL-CELLCOLOR.
      MODIFY GT_FINAL FROM WA_FINAL INDEX LD_INDEX TRANSPORTING CELLCOLOR.
      CLEAR :WA_FINAL , WA_CELLCOLOR.
    ENDIF.

  ENDLOOP.
ENDFORM.                    " SET_CELL_COLOURS
