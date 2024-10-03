*&---------------------------------------------------------------------*
*& Report  ZPP_LTP_INFO
*&
*&---------------------------------------------------------------------*
*& Functional                   : Mr.Govindarajan M   & Mr.Umapathy       *
*& Developer                   : Mr.Ramachandaran M                        *
*& Company                     : Sheenlac Paints Pvt Ltd                     *
*& Verified By                 : Mr.Govindarajan M                             *
*& Title                       : RM PM Collective Requirment at LTP             *
*& Report Name                 : ZPP_LTP                                       *
*& Development Id              : kpabap                                      *
*& Related Information         : PM Collective Requirment at LTP           *
*&---------------------------------------------------------------------  *

REPORT ZPP_LTP_INFO.

TYPES: BEGIN OF ES_S094,                                                                           " Table 1
           WERKS TYPE S094-WERKS,
           MNG02 TYPE S094-MNG02,
           MZUGA TYPE S094-MZUGA,
           BASME TYPE S094-BASME,
           DISMM TYPE S094-DISMM,
           DISPO TYPE S094-DISPO,
           SPMON TYPE S094-SPMON,
           MATNR TYPE S094-MATNR,
         END OF ES_S094.

DATA: GT_S094 TYPE TABLE OF ES_S094,
     WA_S094 TYPE ES_S094.

TYPES: BEGIN OF ES_MARD,                                                                           " Table 2
           LABST TYPE MARD-LABST,
           INSME TYPE MARD-INSME,
           SPEME TYPE MARD-SPEME,
           MATNR TYPE MARD-MATNR,
           WERKS TYPE MARD-WERKS,
         END OF ES_MARD.

DATA: GT_MARD TYPE TABLE OF ES_MARD,
     WA_MARD TYPE ES_MARD.

TYPES: BEGIN OF ES_MAKT,                                                                           " Table 3
           MAKTX TYPE MAKT-MAKTX,
           MATNR TYPE MAKT-MATNR,
        END OF ES_MAKT.

DATA: GT_MAKT TYPE TABLE OF ES_MAKT,
     WA_MAKT TYPE ES_MAKT.

TYPES: BEGIN OF ES_PLPT,                                                                          " Table 4
           PLSCN TYPE PLPT-PLSCN,
           PLWRK TYPE PLPT-PLWRK,
       END OF ES_PLPT.

DATA: GT_PLPT TYPE TABLE OF ES_PLPT,
     WA_PLPT TYPE ES_PLPT.

TYPES: BEGIN OF ES_PLPB,                                                                          " Table 5
           VERSB TYPE PLPB-VERSB,
           PLSCN TYPE PLPB-PLSCN,
       END OF ES_PLPB.

DATA: GT_PLPB TYPE TABLE OF ES_PLPB,
     WA_PLPB TYPE ES_PLPB.

TYPES: BEGIN OF ES_MARC,                                                                          " Table 6
           TRAME TYPE MARC-TRAME,
           MATNR TYPE MARC-MATNR,
           WERKS TYPE MARC-WERKS,
       END OF ES_MARC.

DATA: GT_MARC TYPE TABLE OF ES_MARC,
     WA_MARC TYPE ES_MARC.

TYPES : BEGIN OF ES_MARA,                                                                         " Table 7
           MEINS TYPE MARA-MEINS,
           MTART TYPE MARA-MTART,
           MATNR TYPE MARA-MATNR,
       END OF ES_MARA.

DATA: GT_MARA TYPE TABLE OF ES_MARA,
     WA_MARA TYPE ES_MARA.

TYPES : BEGIN OF GS_FINAL,                                                                        " FINAL
           WERKS TYPE S094-WERKS,
           MNG02 TYPE S094-MNG02,
           MZUGA TYPE S094-MZUGA,
           BASME TYPE S094-BASME,
           DISMM TYPE S094-DISMM,
           DISPO TYPE S094-DISPO,
           SPMON TYPE S094-SPMON,
           MATNR TYPE S094-MATNR,
           LABST TYPE P DECIMALS 2 ,
           INSME TYPE P DECIMALS 2,
           SPEME TYPE P DECIMALS 2,
           MAKTX TYPE MAKT-MAKTX,
           PLSCN TYPE PLPT-PLSCN,
           PLWRK TYPE PLPT-PLWRK,
           VERSB TYPE PLPB-VERSB,
           TRAME TYPE MARC-TRAME,
           MEINS TYPE MARA-MEINS,
           MTART TYPE MARA-MTART,
         END OF GS_FINAL.
DATA : GT_FCAT TYPE SLIS_T_FIELDCAT_ALV,
       IT_SORT TYPE SLIS_T_SORTINFO_ALV,
       WA_FCAT TYPE SLIS_FIELDCAT_ALV,
       WA_SORT TYPE SLIS_SORTINFO_ALV.

DATA : GT_FINAL TYPE TABLE OF GS_FINAL,
         WA_FINAL TYPE GS_FINAL.

DATA : LAYOUT TYPE SLIS_LAYOUT_ALV.

DATA :   LV_WERKS TYPE S094-WERKS,
         LV_MATNR TYPE S094-MATNR,
         LV_PLSCN TYPE PLPT-PLSCN,
         LV_MTART TYPE MARA-MTART.

SELECTION-SCREEN : BEGIN OF BLOCK C1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS  : SO_WERKS FOR LV_WERKS OBLIGATORY,
                  SO_MATNR FOR LV_MATNR,
                  SO_PLSCN FOR LV_PLSCN NO INTERVALS NO-EXTENSION OBLIGATORY,
                  SO_MTART FOR LV_MTART.
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

    SELECT   PLSCN
             PLWRK
    "   FROM PLPT INTO TABLE GT_PLPT FOR ALL ENTRIES IN  GT_PLPB WHERE  PLSCN = GT_PLPB-PLSCN AND PLWRK IN SO_WERKS .
     FROM PLPT INTO TABLE GT_PLPT WHERE PLSCN IN SO_PLSCN AND PLWRK IN SO_WERKS .
  IF GT_PLPT[] IS NOT INITIAL .
  SELECT    WERKS
            MNG02
            MZUGA
            BASME
            DISMM
            DISPO
            SPMON
            MATNR
         FROM S094 INTO TABLE GT_S094 WHERE WERKS IN SO_WERKS AND MATNR IN SO_MATNR .
 ENDIF .
    SELECT
           LABST
           INSME
           SPEME
           MATNR
           WERKS
        FROM MARD INTO TABLE GT_MARD FOR ALL ENTRIES IN GT_S094 WHERE MATNR  = GT_S094-MATNR AND WERKS IN  SO_WERKS .
SELECT
          TRAME
          MATNR
          WERKS
      FROM MARC INTO TABLE GT_MARC FOR ALL ENTRIES IN GT_MARA WHERE MATNR = GT_MARA-MATNR AND WERKS IN  SO_WERKS .

    SELECT
       MAKTX
       MATNR
   FROM MAKT INTO TABLE GT_MAKT FOR ALL ENTRIES IN GT_MARD WHERE MATNR = GT_MARD-MATNR .

    SELECT
            VERSB
            PLSCN
        FROM PLPB INTO TABLE GT_PLPB FOR ALL ENTRIES IN GT_PLPT WHERE PLSCN = GT_PLPT-PLSCN  AND PLSCN IN SO_PLSCN .

  IF GT_S094[] IS NOT INITIAL.
    SELECT
            MEINS
            MTART
            MATNR
          FROM MARA INTO TABLE GT_MARA FOR ALL ENTRIES IN GT_S094 WHERE MATNR  = GT_S094-MATNR AND MTART IN SO_MTART .
  ENDIF.
SORT GT_MARA BY MATNR MTART. " Added by <IT-CAR Tool> during Code Remediation
  DELETE ADJACENT DUPLICATES FROM GT_MARA COMPARING MATNR MTART.

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
  DELETE GT_S094 WHERE MNG02 EQ '0' .
  DELETE GT_MARC WHERE TRAME EQ '0' .

  LOOP AT GT_S094 INTO WA_S094 .
    MOVE-CORRESPONDING WA_S094 TO WA_FINAL.

    LOOP AT  GT_MARD INTO WA_MARD WHERE  MATNR =  WA_S094-MATNR AND WERKS = WA_S094-WERKS.
           WA_FINAL-LABST = WA_FINAL-LABST + WA_MARD-LABST.
           WA_FINAL-INSME = WA_FINAL-INSME + WA_MARD-INSME.
           WA_FINAL-SPEME = WA_FINAL-SPEME + WA_MARD-SPEME.
    ENDLOOP.
    READ TABLE GT_MAKT INTO WA_MAKT WITH KEY  MATNR  =  WA_MARD-MATNR .
           WA_FINAL-MAKTX = WA_MAKT-MAKTX.
    READ TABLE GT_PLPT INTO WA_PLPT WITH  KEY PLWRK = WA_S094-WERKS .
           WA_FINAL-PLSCN = WA_PLPT-PLSCN.
    READ TABLE GT_PLPB INTO WA_PLPB WITH KEY  PLSCN  =  WA_PLPT-PLSCN .
           WA_FINAL-VERSB = WA_PLPB-VERSB.
    LOOP AT GT_MARC INTO WA_MARC WHERE MATNR =  WA_S094-MATNR AND WERKS = WA_S094-WERKS .
          WA_FINAL-TRAME = WA_FINAL-TRAME + WA_MARC-TRAME.
      ENDLOOP.
    READ TABLE GT_MARA INTO WA_MARA WITH KEY MATNR = WA_S094-MATNR .
    IF SY-SUBRC = 0.
          WA_FINAL-MEINS = WA_MARA-MEINS.
          WA_FINAL-MTART = WA_MARA-MTART.
    ENDIF.
   APPEND WA_FINAL TO GT_FINAL.
    CLEAR WA_FINAL.
  ENDLOOP.
 " BREAK-POINT.
  LOOP AT GT_FINAL INTO WA_FINAL.
    SHIFT WA_FINAL-MATNR LEFT DELETING LEADING '0'.
    MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING PLSCN MATNR MATNR.
    CLEAR WA_FINAL.
  ENDLOOP.

IF SO_PLSCN IS NOT INITIAL.
  DELETE GT_FINAL WHERE PLSCN = 0.
  ENDIF.

  IF SO_MTART IS NOT INITIAL.
    DELETE GT_FINAL WHERE MTART = ''.
  ENDIF.
ENDFORM.                    "READ_DATA

*&---------------------------------------------------------------------*
*&      Form  FIELD_CATLOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FIELD_CATLOG .

  PERFORM ALV_LAYOUT USING 1 'Month' 'SPMON' 'GT_FINAL' '' ''.
  PERFORM ALV_LAYOUT USING 2 'Plant' 'WERKS' 'GT_FINAL' '' ''.
  PERFORM ALV_LAYOUT USING 3 'Material' 'MATNR' 'GT_FINAL' '' ''.
  PERFORM ALV_LAYOUT USING 4 'Material Des' 'MAKTX' 'GT_FINAL' '' ''.
  PERFORM ALV_LAYOUT USING 5 'MRP Type' 'DISMM' 'GT_FINAL' '' ''.
  PERFORM ALV_LAYOUT USING 6 'MRP Controller' 'DISPO' 'GT_FINAL' '' ''.

  PERFORM ALV_LAYOUT USING 7 'Planning Scenario' 'PLSCN' 'GT_FINAL' '' ''.
  PERFORM ALV_LAYOUT USING 8 'Version' 'VERSB' 'GT_FINAL' '' ''.
  PERFORM ALV_LAYOUT USING 9 'Material Type' 'MTART' 'GT_FINAL' '' ''.

  PERFORM ALV_LAYOUT USING 10 'Requirements' 'MNG02' 'GT_FINAL' 'X' ''.
  PERFORM ALV_LAYOUT USING 11 'GdsRecepit(MRP)' 'MZUGA' 'GT_FINAL' 'X' ''.
  PERFORM ALV_LAYOUT USING 12 'Base Unit' 'BASME' 'GT_FINAL' '' ''.

  PERFORM ALV_LAYOUT USING 14 'Unrestricted' 'LABST' 'GT_FINAL' 'X' 'X'.
  PERFORM ALV_LAYOUT USING 15 'In Qual.Insp' 'INSME' 'GT_FINAL' 'X' 'X'.
  PERFORM ALV_LAYOUT USING 16 'Blocked' 'SPEME' 'GT_FINAL' 'X' 'X'.

  PERFORM ALV_LAYOUT USING 19 'Stock In Transit' 'TRAME' 'GT_FINAL' 'X' 'X'.
  PERFORM ALV_LAYOUT USING 20 'Base Unit' 'MEINS' 'GT_FINAL' '' ''.

ENDFORM.                    " FIELD_CATLOG
*&---------------------------------------------------------------------*
*&      Form  ALV_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0568   text
*      -->P_0569   text
*      -->P_0570   text
*      -->P_0571   text
*----------------------------------------------------------------------*
FORM ALV_LAYOUT   USING P1 P2 P3 P4 P5 P6.
  CLEAR WA_FCAT.
  WA_FCAT-COL_POS = P1.
  WA_FCAT-SELTEXT_L = P2.
  WA_FCAT-FIELDNAME = P3.
  WA_FCAT-TABNAME = P4.
  WA_FCAT-DO_SUM = P5.
  WA_FCAT-NO_ZERO = P6.
  APPEND WA_FCAT TO GT_FCAT.

ENDFORM.                    " ALV_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  ALV_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ALV_DISPLAY .
           LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
           LAYOUT-ZEBRA = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
   EXPORTING
*                I_INTERFACE_CHECK                 = ' '
*                I_BYPASSING_BUFFER                = ' '
*                I_BUFFER_ACTIVE                   = ' '
                 I_CALLBACK_PROGRAM                = SY-REPID
*                I_CALLBACK_PF_STATUS_SET          = ' '
*                I_CALLBACK_USER_COMMAND           = ' '
                 I_CALLBACK_TOP_OF_PAGE            = 'ALV_CATALOG_HEADER'
*                I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*                I_CALLBACK_HTML_END_OF_LIST       = ' '
*                I_STRUCTURE_NAME                  =
*                I_BACKGROUND_ID                   = ' '
*                I_GRID_TITLE                      =
*                I_GRID_SETTINGS                   =
                 IS_LAYOUT                         = LAYOUT
                 IT_FIELDCAT                       = GT_FCAT[]
*                IT_EXCLUDING                      =
*                IT_SPECIAL_GROUPS                 =
*                IT_SORT                           =
*                IT_FILTER                         =
*                IS_SEL_HIDE                       =
                 I_DEFAULT                         = 'X'
                 I_SAVE                            = 'X'
*                IS_VARIANT                        =
*                IT_EVENTS                         =
*                IT_EVENT_EXIT                     =
*                IS_PRINT                          =
*                IS_REPREP_ID                      =
*                I_SCREEN_START_COLUMN             = 0
*                I_SCREEN_START_LINE               = 0
*                I_SCREEN_END_COLUMN               = 0
*                I_SCREEN_END_LINE                 = 0
*                I_HTML_HEIGHT_TOP                 = 0
*                I_HTML_HEIGHT_END                 = 0
*                IT_ALV_GRAPHICS                   =
*                IT_HYPERLINK                      =
*                IT_ADD_FIELDCAT                   =
*                IT_EXCEPT_QINFO                   =
*                IR_SALV_FULLSCREEN_ADAPTER        =
*              IMPORTING
*                E_EXIT_CAUSED_BY_CALLER           =
*                ES_EXIT_CAUSED_BY_USER            =
    TABLES
      T_OUTTAB                          = GT_FINAL[]
*              EXCEPTIONS
*                PROGRAM_ERROR                     = 1
*                OTHERS                            = 2
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

  DATA :
         LV_WERKS(100) TYPE C.

  CLEAR : LV_WERKS.

   IF SO_WERKS-HIGH IS NOT INITIAL.
    CONCATENATE 'Plant Code :' SO_WERKS-LOW 'To' SO_WERKS-HIGH INTO LV_WERKS SEPARATED BY SPACE.
  ELSE.
    CONCATENATE 'Plant Code :' SO_WERKS-LOW INTO LV_WERKS SEPARATED BY SPACE.
  ENDIF.

  CLEAR LS_LINE.
  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY = ' '.
  LS_LINE-INFO = LV_WERKS.
  APPEND LS_LINE TO LIT_HEADER.

  CLEAR LS_LINE.
  LS_LINE-TYP  = 'H'.
  LS_LINE-KEY = ' '.
  LS_LINE-INFO = 'RM PM Collective Requirment At LTP' .
  APPEND LS_LINE TO LIT_HEADER.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = LIT_HEADER
      I_LOGO             = 'ZLOGO'.

**  CALL FUNCTION 'CONVERSION_EXIT_SDATE_OUTPUT'
**
**  EXPORTING
**    INPUT         = LV_BEDAT.

*      I_LOGO             = ' '.

ENDFORM.                    "ALV_CATALOG_HEADER
