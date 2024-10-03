

*&---------------------------------------------------------------------*
*& Report ZCOST_RUN_REPORT
*&*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&Functional                   : Mr.Ramachandran        *
*& Developer                   : Mr.Ramachandaran                      *
*& Company                     : Sheenlac Paints Pvt Ltd               *
*& Verified By                 :                                       *
*& Title                       : Costing Run Report                    *
*& Report Name                 : ZCOST_RUN REPORT                           *
*& Development Id              : kpabap                                *
*& Related Information         : Manufacturing Details       *
*&---------------------------------------------------------------------*

REPORT ZCOST_RUN_REPORT.

TABLES  : KEKO,CKIS,MARA .


TYPES : BEGIN OF ES_KEKO,
        KALNR TYPE KEKO-KALNR,
        KALKA TYPE KEKO-KALKA,
        KADKY TYPE KEKO-KADKY,
        TVERS TYPE KEKO-TVERS,
        MATNR TYPE KEKO-MATNR,
        WERKS TYPE KEKO-WERKS,
        KADAT TYPE KEKO-KADAT,
        BIDAT TYPE KEKO-BIDAT,
        POPER TYPE KEKO-POPER,
        LOSGR TYPE KEKO-LOSGR,
        MEINS TYPE KEKO-MEINS,
        FEH_STA TYPE KEKO-FEH_STA,
        BDATJ TYPE KEKO-BDATJ,
     END OF ES_KEKO.

DATA: GT_KEKO TYPE TABLE OF ES_KEKO,
     WA_KEKO TYPE ES_KEKO,
     LV_NOS TYPE I.


TYPES : BEGIN OF ES_CKIS,
        BZOBJ TYPE CKIS-BZOBJ,
        KALNR TYPE CKIS-KALNR,
        KALKA TYPE CKIS-KALKA,
        KADKY TYPE CKIS-KADKY,
        TVERS TYPE CKIS-TVERS,
        KKZMA TYPE CKIS-KKZMA,
        POSNR TYPE CKIS-POSNR,
        TYPPS TYPE CKIS-TYPPS,
        KSTAR TYPE CKIS-KSTAR,
        BUKRS TYPE CKIS-BUKRS,
        WERKS TYPE CKIS-WERKS,
        MATNR TYPE CKIS-MATNR,
        WERTN TYPE CKIS-WERTN,
        MKURS TYPE CKIS-MKURS,
        FWEHT TYPE CKIS-FWEHT,
        MENGE TYPE CKIS-MENGE,
        MEEHT TYPE CKIS-MEEHT,
        PREIS5 TYPE CKIS-PREIS5,
        LSTAR TYPE CKIS-LSTAR,
        ARBID TYPE CKIS-ARBID,
        KOSTL TYPE CKIS-KOSTL,
      END OF ES_CKIS.

DATA: GT_CKIS TYPE TABLE OF ES_CKIS,
     WA_CKIS TYPE ES_CKIS.

TYPES : BEGIN OF ES_MARA,
        MATNR TYPE MARA-MATNR,
        MTART TYPE MARA-MTART,
        BISMT TYPE MARA-BISMT,
        MEINS TYPE MARA-MEINS,
      END OF ES_MARA.

DATA: GT_MARA TYPE TABLE OF ES_MARA,
     WA_MARA TYPE ES_MARA.

TYPES : BEGIN OF GS_FINAL,
        KALNR TYPE KEKO-KALNR,
        KALKA TYPE KEKO-KALKA,
        KADKY TYPE KEKO-KADKY,
        TVERS TYPE KEKO-TVERS,
        MATNR TYPE KEKO-MATNR,
        WERKS TYPE KEKO-WERKS,
        KADAT TYPE KEKO-KADAT,
        BIDAT TYPE KEKO-BIDAT,
        POPER TYPE KEKO-POPER,
        LOSGR TYPE KEKO-LOSGR,
        MEINS TYPE KEKO-MEINS,
        BDATJ TYPE KEKO-BDATJ,
        FEH_STA TYPE KEKO-FEH_STA,
        BZOBJ TYPE CKIS-BZOBJ,
        KALNR1 TYPE CKIS-KALNR,
        KALKA1 TYPE CKIS-KALKA,
        KADKY1 TYPE CKIS-KADKY,
        TVERS1 TYPE CKIS-TVERS,
        KKZMA TYPE CKIS-KKZMA,
        POSNR TYPE CKIS-POSNR,
        TYPPS TYPE CKIS-TYPPS,
        KSTAR TYPE CKIS-KSTAR,
        BUKRS TYPE CKIS-BUKRS,
        WERKS1 TYPE CKIS-WERKS,
        MATNR1 TYPE CKIS-MATNR,
        WERTN TYPE CKIS-WERTN,
        MKURS TYPE CKIS-MKURS,
        FWEHT TYPE CKIS-FWEHT,
        MENGE TYPE CKIS-MENGE,
        MEEHT TYPE CKIS-MEEHT,
        PREIS5 TYPE CKIS-PREIS5,
        LSTAR TYPE CKIS-LSTAR,
        ARBID TYPE CKIS-ARBID,
        KOSTL TYPE CKIS-KOSTL,
        MATNR2 TYPE MARA-MATNR,
        MTART TYPE MARA-MTART,
        BISMT TYPE MARA-BISMT,
        MEINS1 TYPE MARA-MEINS,

        TOT_MCH TYPE CKIS-WERTN,
        TOT_LAB TYPE CKIS-WERTN,
        TOT_POW TYPE CKIS-WERTN,
        TOT_RM TYPE CKIS-WERTN,
        TOT_PM TYPE CKIS-WERTN,
        TOT_SFG TYPE CKIS-WERTN,
        TOT_TRA TYPE CKIS-WERTN,
        TOT_OVER TYPE CKIS-WERTN,
        TOT_STA TYPE CKIS-WERTN,
        TOT_FG TYPE CKIS-WERTN,
        TOT_FGBH TYPE CKIS-WERTN,
       END OF GS_FINAL.

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

DATA :   LV_MATNR TYPE KEKO-MATNR,
*      LV_KALNR TYPE KEKO-KALNR,
         LV_WERKS TYPE KEKO-WERKS,
         LV_KADKY TYPE KEKO-KADKY .

DATA : LAYOUT TYPE SLIS_LAYOUT_ALV.

SELECTION-SCREEN : BEGIN OF BLOCK C1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS  : SO_MATNR FOR LV_MATNR ,
                  SO_WERKS FOR LV_WERKS ,
                  SO_KADKY FOR LV_KADKY .
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

  SELECT
 	      KALNR
        KALKA
        KADKY
        TVERS
        MATNR
        WERKS
        KADAT
        BIDAT
        POPER
        LOSGR
        MEINS
        FEH_STA
        BDATJ
        FROM KEKO INTO TABLE GT_KEKO
        WHERE  MATNR IN SO_MATNR AND  WERKS IN SO_WERKS
        AND KADKY IN SO_KADKY AND ( FEH_STA EQ 'FR' OR FEH_STA EQ 'FF' )  .

  IF GT_KEKO IS NOT INITIAL.

    SELECT

      BZOBJ
      KALNR
      KALKA
      KADKY
      TVERS
      KKZMA
      POSNR
      TYPPS
      KSTAR
      BUKRS
      WERKS
      MATNR
      WERTN
      MKURS
      FWEHT
      MENGE
      MEEHT
      PREIS5
      LSTAR
      ARBID
      KOSTL
FROM CKIS INTO TABLE GT_CKIS FOR ALL ENTRIES IN GT_KEKO
WHERE KALNR = GT_KEKO-KALNR AND WERKS = GT_KEKO-WERKS AND KADKY = GT_KEKO-KADKY  .

  ENDIF.

  IF GT_CKIS IS NOT INITIAL.


    SELECT
    MATNR
    MTART
    BISMT
    MEINS
    FROM MARA INTO TABLE GT_MARA FOR ALL ENTRIES IN GT_CKIS
    WHERE MATNR = GT_CKIS-MATNR .

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

*  BREAK-POINT.
  LOOP AT GT_KEKO INTO WA_KEKO.


*    AT NEW KALNR.
*
*    IF WA_KEKO IS NOT INITIAL.
*
*
    WA_FINAL-KALNR = WA_KEKO-KALNR.
    WA_FINAL-KALKA = WA_KEKO-KALKA.
    WA_FINAL-KADKY = WA_KEKO-KADKY.
    WA_FINAL-TVERS = WA_KEKO-TVERS.
    WA_FINAL-MATNR = WA_KEKO-MATNR.
    WA_FINAL-WERKS = WA_KEKO-WERKS.
    WA_FINAL-KADAT = WA_KEKO-KADAT.
    WA_FINAL-BIDAT = WA_KEKO-BIDAT.
    WA_FINAL-POPER = WA_KEKO-POPER.
    WA_FINAL-LOSGR = WA_KEKO-LOSGR.
    WA_FINAL-MEINS = WA_KEKO-MEINS.
    WA_FINAL-BDATJ = WA_KEKO-BDATJ.
    WA_FINAL-FEH_STA = WA_KEKO-FEH_STA.
*    ENDIF.
*
*
*
*    MOVE-CORRESPONDING WA_KEKO TO WA_FINAL.

    LOOP AT GT_CKIS INTO WA_CKIS WHERE KALNR = WA_KEKO-KALNR AND KADKY = WA_KEKO-KADKY.


      WA_FINAL-BZOBJ = WA_CKIS-BZOBJ.
      WA_FINAL-KALNR = WA_CKIS-KALNR.
      WA_FINAL-KALKA = WA_CKIS-KALKA.
      "   WA_FINAL-KADKY = WA_CKIS-KADKY.
      WA_FINAL-TVERS = WA_CKIS-TVERS.
      WA_FINAL-KKZMA = WA_CKIS-KKZMA.
      WA_FINAL-POSNR = WA_CKIS-POSNR.
      WA_FINAL-TYPPS = WA_CKIS-TYPPS.


      WA_FINAL-KSTAR = WA_CKIS-KSTAR.
      WA_FINAL-BUKRS = WA_CKIS-BUKRS.
      WA_FINAL-WERKS = WA_CKIS-WERKS.
      WA_FINAL-MATNR1 = WA_CKIS-MATNR.
      WA_FINAL-WERTN  = WA_CKIS-WERTN.
      WA_FINAL-MKURS = WA_CKIS-MKURS.
      WA_FINAL-FWEHT = WA_CKIS-FWEHT.
      WA_FINAL-MENGE = WA_CKIS-MENGE.
      WA_FINAL-ARBID = WA_CKIS-ARBID.
      WA_FINAL-KOSTL = WA_CKIS-KOSTL.

*        READ TABLE GT_CKIS INTO WA_CKIS WITH KEY KALNR = WA_KEKO-KALNR WERKS = WA_KEKO-WERKS LSTAR = 'MACH'.
*        IF SY-SUBRC EQ '0'.
*          WA_FINAL-TOT_MCH = WA_CKIS-WERTN.
*        ENDIF.
*        READ TABLE GT_CKIS INTO WA_CKIS WITH KEY KALNR = WA_KEKO-KALNR WERKS = WA_KEKO-WERKS LSTAR = 'LABR'.
*        IF SY-SUBRC EQ '0'.
*          WA_FINAL-TOT_LAB = WA_CKIS-WERTN.
*        ENDIF.
*        READ TABLE GT_CKIS INTO WA_CKIS WITH KEY KALNR = WA_KEKO-KALNR WERKS = WA_KEKO-WERKS LSTAR = 'POWR'.
*        IF SY-SUBRC EQ '0'.
*          WA_FINAL-TOT_POW = WA_CKIS-WERTN.
*        ENDIF.

      IF WA_CKIS-TYPPS EQ 'G'.
        WA_FINAL-TOT_OVER = WA_CKIS-WERTN.
      ENDIF.
      IF WA_CKIS-LSTAR EQ 'MACH'.
        WA_FINAL-TOT_MCH = WA_CKIS-WERTN.
      ELSEIF WA_CKIS-LSTAR EQ 'LABR'.
        WA_FINAL-TOT_LAB = WA_CKIS-WERTN.
      ELSEIF WA_CKIS-LSTAR EQ 'POWR'.
        WA_FINAL-TOT_POW = WA_CKIS-WERTN.
      ENDIF.

      READ TABLE GT_MARA INTO WA_MARA WITH KEY MATNR = WA_CKIS-MATNR MTART = 'ROH'.
      IF SY-SUBRC EQ '0'.
        WA_FINAL-TOT_RM = WA_FINAL-TOT_RM + WA_CKIS-WERTN.
      ENDIF.
      READ TABLE GT_MARA INTO WA_MARA WITH KEY MATNR = WA_CKIS-MATNR MTART = 'VERP'.
      IF SY-SUBRC EQ '0'.
        WA_FINAL-TOT_PM = WA_FINAL-TOT_PM + WA_CKIS-WERTN.
      ENDIF.
      READ TABLE GT_MARA INTO WA_MARA WITH KEY MATNR = WA_CKIS-MATNR MTART = 'HALB'.
      IF SY-SUBRC EQ '0'.
        WA_FINAL-TOT_SFG =  WA_FINAL-TOT_SFG + WA_CKIS-WERTN.
      ENDIF.
      READ TABLE GT_MARA INTO WA_MARA WITH KEY MATNR = WA_CKIS-MATNR MTART = 'HAWA'.
      IF SY-SUBRC EQ '0'.
        WA_FINAL-TOT_TRA = WA_FINAL-TOT_TRA + WA_CKIS-WERTN.
      ENDIF.
      READ TABLE GT_MARA INTO WA_MARA WITH KEY MATNR = WA_CKIS-MATNR MTART = 'ZSTR'.
      IF SY-SUBRC EQ '0'.
        WA_FINAL-TOT_STA = WA_FINAL-TOT_STA + WA_CKIS-WERTN.
      ENDIF.

*      IF SY-SUBRC EQ '0' AND WA_MARA-MTART EQ 'ROH'.
*        WA_FINAL-TOT_RM = WA_CKIS-WERTN.
*      ENDIF.
*      IF SY-SUBRC EQ '0' AND WA_MARA-MTART EQ 'VERP'.
*        WA_FINAL-TOT_PM = WA_CKIS-WERTN.
*       ENDIF.
*      IF SY-SUBRC EQ '0' AND WA_MARA-MTART EQ 'HALB'.
*        WA_FINAL-TOT_SFG = WA_CKIS-WERTN.
*      ENDIF.
*      IF SY-SUBRC EQ '0' AND WA_MARA-MTART EQ 'HAWA'.
*        WA_FINAL-TOT_TRA = WA_CKIS-WERTN.
*      ENDIF.


      WA_FINAL-TOT_FG = WA_FINAL-TOT_OVER + WA_FINAL-TOT_MCH + WA_FINAL-TOT_LAB + WA_FINAL-TOT_POW + WA_FINAL-TOT_RM + WA_FINAL-TOT_PM + WA_FINAL-TOT_SFG + WA_FINAL-TOT_TRA  + WA_FINAL-TOT_STA .
      WA_FINAL-TOT_FGBH = WA_FINAL-TOT_FG /  WA_FINAL-LOSGR.
    ENDLOOP.

    APPEND WA_FINAL TO GT_FINAL.
    CLEAR WA_FINAL.

    " ENDAT.
  ENDLOOP.




* BREAK-POINT.

*  LOOP AT GT_FINAL INTO WA_FINAL .
*
*    WA_FINAL-TOT_FG = WA_FINAL-TOT_OVER + WA_FINAL-TOT_MCH + WA_FINAL-TOT_LAB + WA_FINAL-TOT_POW + WA_FINAL-TOT_RM + WA_FINAL-TOT_PM + WA_FINAL-TOT_SFG + WA_FINAL-TOT_TRA  + WA_FINAL-TOT_STA .
*
*  ENDLOOP.





*
*
*  LOOP AT GT_FINAL INTO WA_FINAL .
*    CASE WA_FINAL-MTART .
*      WHEN 'ROH'.
*        WA_FINAL-TOT_RM = WA_FINAL-WERTN .
*      WHEN 'VERP'.
*        WA_FINAL-TOT_PM = WA_FINAL-WERTN .
*      WHEN 'HALB'.
*        WA_FINAL-TOT_SFG = WA_FINAL-WERTN .
*      WHEN 'HAWA'.
*        WA_FINAL-TOT_TRA = WA_FINAL-WERTN .
*    ENDCASE.
*    IF SY-SUBRC EQ LV_NOS.
*      MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING TOT_MCH TOT_LAB TOT_SFG TOT_POW.
*      CLEAR WA_FINAL.
*    ENDIF.
*  ENDLOOP.
*
*  "DESCRIBE TABLE GT_FINAL LINES LV_NOS.
*
*
*  LOOP AT GT_FINAL INTO WA_FINAL .
*    IF WA_FINAL-LSTAR EQ 'MACH' .
*      WA_FINAL-TOT_MCH = WA_FINAL-WERTN .
*    ENDIF.
*    IF WA_FINAL-LSTAR EQ 'LABR' .
*      WA_FINAL-TOT_LAB = WA_FINAL-WERTN .
*    ENDIF.
*    IF WA_FINAL-LSTAR EQ 'POWR' .
*      WA_FINAL-TOT_POW = WA_FINAL-WERTN .
*    ENDIF.
*    IF SY-SUBRC EQ LV_NOS.
*      MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING TOT_MCH TOT_LAB TOT_POW.
*      CLEAR WA_FINAL.
*    ENDIF.
*
*
*
*
*
*  ENDLOOP.
*




ENDFORM.                    " READ_DATA

*&---------------------------------------------------------------------*
*&      Form  FIELD_CATLOG
*&---------------------------------------------------------------------*
FORM FIELD_CATLOG .

  PERFORM ALV_LAYOUT USING 1 'Plant' 'WERKS' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 2 'Material' 'MATNR' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 3 'Cost.Est.No' 'KALNR' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 4  'Costing Status' 'FEH_STA' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 5 'Costing Type' 'KALKA' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 6 'Costing Date' 'KADKY' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 7 'Version' 'TVERS' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 8 'Valid From' 'KADAT' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 9 'Valid to' 'BIDAT' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 10 'Period' 'POPER' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 11 'Year' 'BDATJ' 'GT_FINAL' ''.

  PERFORM ALV_LAYOUT USING 12 'RM' 'TOT_RM' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 13 'PM' 'TOT_PM' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 14 'SFG' 'TOT_SFG' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 15 'Trading' 'TOT_TRA' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 16 'Stationary' 'TOT_STA' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 17 'Machine' 'TOT_MCH' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 18 'Labour' 'TOT_LAB' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 19 'Power' 'TOT_POW' 'GT_FINAL' ''.

  PERFORM ALV_LAYOUT USING 20 'Overhead Cost' 'TOT_OVER' 'GT_FINAL' ''.
*  PERFORM ALV_LAYOUT USING 20 'Additive Cost' 'KADKY' 'GT_FINAL' ''.

  PERFORM ALV_LAYOUT USING 21 'FG' 'TOT_FG' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 22 'Costing Lot Size' 'LOSGR' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 23 'VUM' 'MEINS' 'GT_FINAL' ''.

  PERFORM ALV_LAYOUT USING 24 'FG in 1' 'TOT_FGBH' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 25 'Costing Lot Size' 'TVERS' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 26 'VUM1' 'MEINS' 'GT_FINAL' ''.

*  PERFORM ALV_LAYOUT USING 26 'Object ID' 'ARBID' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 27 'Cost Center' 'KOSTL' 'GT_FINAL' ''.


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
  LS_LINE-INFO = 'Costing Run Report' .
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
