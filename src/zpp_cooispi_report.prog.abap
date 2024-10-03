
*&---------------------------------------------------------------------*
*& Report  ZPP_COOISPI_REPORT
*&
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&Functional                   : Mr.Govindarajan M                     *
*& Developer                   : Mr.Ramachandaran                      *
*& Modified by                : Mr.Govindarajan M                      *
*& Modified  On                  :    13/08/2014                                   *
*& Company                     : Sheenlac Paints Pvt Ltd               *
*& Verified By                 :                                       *
*& Title                       : Process Order Information Report      *
*& Report Name                 : ZPP_COOISPI_REPORT                    *
*& Development Id              : kpabap                                *
*& Related Information         : Process Order Information System      *
*&---------------------------------------------------------------------*

REPORT ZPP_COOISPI_REPORT.

TABLES  : AFKO,AFPO.

TYPES: BEGIN OF ES_AFKO,                                                                           " Table 1
        AUFNR TYPE AFKO-AUFNR,                                 " ORDER
        PLNTY TYPE AFKO-PLNTY,                                 " Order Type
        PLNBEZ TYPE AFKO-PLNBEZ,                               " MATERIAL
        GAMNG TYPE AFKO-GAMNG,                                 " TARGER QTY
        GMEIN TYPE AFKO-GMEIN,                                 " Target Qty in Unit
        IGMNG TYPE AFKO-IGMNG,                                 " CONFIRMED QTY
        DISPO TYPE AFKO-DISPO,                                 " MRP CONTROLLER
        FEVOR TYPE AFKO-FEVOR,                                 " PRO SUPERVISOR
        STLST TYPE AFKO-STLST,                                 " BOM STATUS
        STLNR TYPE AFKO-STLNR,                                 " BOM
        SBMNG TYPE AFKO-SBMNG,                                 " BASE QTY
        GSTRP TYPE AFKO-GSTRP,                                 " BASIC START DATE
        GLTRP TYPE AFKO-GLTRP,                                 " BASIC FINISH DATE
        GLTRI TYPE AFKO-GLTRI,                                 " ACTUAL FINISH
        PVERW TYPE AFKO-PVERW,                                 " USAGE
        PLNNR TYPE AFKO-PLNNR,                                 " GROUP
       END OF ES_AFKO.

 DATA: GT_AFKO TYPE TABLE OF ES_AFKO,
      WA_AFKO TYPE ES_AFKO.

TYPES: BEGIN OF EPS_AFPO,
         DAUAT TYPE AFPO-DAUAT,                                 " ORDER TYPE                                                                         "Table 2
         AUFNR TYPE AFPO-AUFNR,                                 " ORDER
         PSAMG TYPE AFPO-PSAMG,                                 " SCRAP QTY
         MATNR TYPE AFPO-MATNR,                                 " Material  NO
         PWERK TYPE AFPO-PWERK,                                 " PLANNING PLANT
         CHARG TYPE AFPO-CHARG,                                 " BATCH
         XLOEK TYPE AFPO-XLOEK,                                 " DELETION FLAG
       END OF EPS_AFPO.

   DATA: GT_AFPO TYPE TABLE OF EPS_AFPO,
      WA_AFPO TYPE EPS_AFPO.

TYPES: BEGIN OF ES_AFRU,
        ERSDA TYPE AFRU-ERSDA,                                 " ENDER ON
        ERNAM TYPE AFRU-ERNAM,                                 " ENDER BY
        BUDAT TYPE AFRU-BUDAT,                                 " POSTING DATE
        WABLNR TYPE AFRU-WABLNR,                               " MATERIAL DOC
        LAEDA TYPE AFRU-LAEDA,                                 " CHANGED ON
        AENAM TYPE AFRU-AENAM,                                 " CHANGED BY
        AUFNR TYPE AFRU-AUFNR,                                 " ORDER
        EXERZ TYPE AFRU-EXERZ,                                 " CREATED TIME
       END OF ES_AFRU.

 DATA: GT_AFRU TYPE TABLE OF ES_AFRU,
      WA_AFRU TYPE ES_AFRU.

 TYPES: BEGIN OF ES_MAKT,                                                                           " Table 4
        MATNR TYPE MAKT-MATNR,                               " ORDER
        MAKTX TYPE MAKT-MAKTX,                               " MATERIAL
         END OF ES_MAKT.

 DATA: ET_MAKT TYPE TABLE OF ES_MAKT,
      WA_MAKT TYPE ES_MAKT.

TYPES: BEGIN OF ES_MARA,                                                                           " Table 5
        MATNR TYPE MARA-MATNR,                               " MATERIAL
        MTART TYPE MARA-MTART,                                " Mat.Type
        VOLUM TYPE MARA-VOLUM,                               " VOLUME
        VOLEH TYPE MARA-VOLEH,                                " Volume Unit
       END OF ES_MARA.

 DATA: GT_MARA TYPE TABLE OF ES_MARA,
      WA_MARA TYPE ES_MARA.

TYPES : BEGIN OF GS_MARC ,
        MATNR TYPE MARC-MATNR,
        WERKS TYPE MARC-WERKS,
       END OF GS_MARC.

 DATA : GT_MARC TYPE TABLE OF GS_MARC,
         WA_MARC TYPE GS_MARC.

TYPES : BEGIN OF GS_MKPF ,
        MBLNR TYPE MKPF-MBLNR,
        BUDAT TYPE MKPF-BUDAT,
       END OF GS_MKPF.

 DATA : GT_MKPF TYPE TABLE OF GS_MKPF,
         WA_MKPF TYPE GS_MKPF.

TYPES : BEGIN OF GS_MSEG ,
        EBELN TYPE MSEG-EBELN,
        MBLNR TYPE MSEG-MBLNR,
        MATNR TYPE MSEG-MATNR,
        AUFNR TYPE MSEG-AUFNR,
        MENGE TYPE MSEG-MENGE,
        BUDAT_MKPF TYPE MSEG-BUDAT_MKPF,
        BWART TYPE MSEG-BWART,
        CHARG TYPE MSEG-CHARG,
        WERKS TYPE MSEG-WERKS,
       END OF GS_MSEG.

 DATA : GT_MSEG TYPE TABLE OF GS_MSEG,
         WA_MSEG TYPE GS_MSEG.

TYPES : BEGIN OF GS_FINAL,                                                                             " FINAL
         AUFNR TYPE MSEG-AUFNR,             " ORDER
         PLNTY TYPE AFKO-PLNTY,                                 " Order Type
   "      PLNBEZ TYPE AFKO-PLNBEZ,                               " MATERIAL
         GAMNG TYPE AFKO-GAMNG,                                 " TARGER QTY
         GMEIN TYPE AFKO-GMEIN,                                 " Target Qty in Unit
         IGMNG TYPE AFKO-IGMNG,                                 " CONFIRMED QTY
*         CGMEIN TYPE AFKO-GMEIN,                                 " Target Qty in Unit
         DISPO TYPE AFKO-DISPO,                                 " MRP CONTROLLER
         FEVOR TYPE AFKO-FEVOR,                                 " PRO SUPERVISOR
         STLST TYPE AFKO-STLST,                                 " BOM STATUS
         STLNR TYPE AFKO-STLNR,                                 " BOM
         SBMNG TYPE AFKO-SBMNG,                                 " BASE QTY
         GSTRP TYPE AFKO-GSTRP,                                 " BASIC START DATE
         GLTRP TYPE AFKO-GLTRP,                                 " BASIC FINISH DATE
         GLTRI TYPE AFKO-GLTRI,                                 " ACTUAL FINISH
         PVERW TYPE AFKO-PVERW,                                 " USAGE
         PLNNR TYPE AFKO-PLNNR,                                 " GROUP
        " CHARG TYPE AFPO-CHARG,                                 " BATCH
         XLOEK TYPE AFPO-XLOEK,                                 " DELETION FLAG
         DAUAT TYPE AFPO-DAUAT,                                 " ORDER TYPE
         PSAMG TYPE AFPO-PSAMG,                                 " SCRAP QTY
         PWERK TYPE AFPO-PWERK,                                 " PLANNING PLANT
         BUDAT TYPE AFRU-BUDAT,                                 " POSTING DATE
         WABLNR TYPE AFRU-WABLNR,                               " MATERIAL DOC
         ERSDA TYPE AFRU-ERSDA,                                 " ENDER ON
         ERNAM TYPE AFRU-ERNAM,                                 " ENDER BY
         LAEDA TYPE AFRU-LAEDA,                                 " CHANGED ON
         AENAM TYPE AFRU-AENAM,                                 " CHANGED BY
         EXERZ TYPE AFRU-EXERZ,                                 " CREATED TIME
         MAKTX TYPE MAKT-MAKTX,                                 " MATERIAL
         MTART TYPE MARA-MTART,                                   " MAT. TYPE
         VOLUM TYPE MARA-VOLUM,                               " VOLUME
         VOLEH TYPE MARA-VOLEH,                               " VOLUME UNIT
         EBELN TYPE MSEG-EBELN,
         MATNR TYPE MSEG-MATNR,
         MENGE TYPE MSEG-MENGE,
         BUDAT_MKPF TYPE MSEG-BUDAT_MKPF,
         BWART TYPE MSEG-BWART,
         CHARG TYPE MSEG-CHARG,
         WERKS TYPE MSEG-WERKS,
         TQTY_LTR TYPE P DECIMALS 3,
         CQTY_LTR TYPE P DECIMALS 3,
         SQTY_LTR TYPE P DECIMALS 3,
       END OF GS_FINAL.

DATA : GT_FINAL TYPE TABLE OF GS_FINAL,
         WA_FINAL TYPE GS_FINAL.

DATA : GT_FCAT TYPE SLIS_T_FIELDCAT_ALV,
       WA_FCAT TYPE SLIS_FIELDCAT_ALV.

DATA :   LV_AUFNR TYPE AFKO-AUFNR,
         LV_PLNTY TYPE AFKO-PLNTY,
         LV_BUDAT_MKPF TYPE MSEG-BUDAT_MKPF,
         LV_MATNR TYPE AFPO-MATNR,
         LV_PWERK TYPE AFPO-PWERK,"
         LV_DAUAT TYPE AFPO-DAUAT,
         LV_DISPO TYPE AFKO-DISPO,
         LV_FEVOR TYPE AFKO-FEVOR,
         LV_GSTRP TYPE AFKO-GSTRP,
         LV_GLTRP TYPE AFKO-GLTRP,
         LV_MTART TYPE MARA-MTART.

DATA : LAYOUT TYPE SLIS_LAYOUT_ALV.


 SELECTION-SCREEN : BEGIN OF BLOCK C1 WITH FRAME TITLE TEXT-001.

   SELECT-OPTIONS  : SO_AUFNR FOR LV_AUFNR ,
                     SO_PLNTY FOR LV_PLNTY No-DISPLAY,
                     SO_B_MK FOR LV_BUDAT_MKPF ,
                     SO_MTART FOR LV_MTART,
                     SO_MATNR FOR LV_MATNR,"OBLIGATORY
                     SO_PWERK FOR LV_PWERK ,
                     SO_DAUAT FOR LV_DAUAT No-DISPLAY,
                     SO_DISPO FOR LV_DISPO No-DISPLAY,
                     SO_FEVOR FOR LV_FEVOR No-DISPLAY .

   SELECTION-SCREEN : END OF BLOCK C1.

   SELECTION-SCREEN : BEGIN OF BLOCK C2 WITH FRAME TITLE TEXT-002.

   SELECT-OPTIONS  : SO_GSTRP FOR LV_GSTRP No-DISPLAY,
                     SO_GLTRP FOR LV_GLTRP No-DISPLAY.

   SELECTION-SCREEN : END OF BLOCK C2.

START-OF-SELECTION.
  PERFORM GET_DATA.
  PERFORM FIELD_CATLOG .

END-OF-SELECTION.
  PERFORM ALV_DISPLAY.
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*

FORM GET_DATA .

  SELECT
        MATNR
        MTART
        VOLUM
        VOLEH
            FROM MARA INTO TABLE GT_MARA   WHERE MATNR IN SO_MATNR AND MTART IN SO_MTART   .

  SELECT
        EBELN
        MBLNR
        MATNR
        AUFNR
        MENGE
        BUDAT_MKPF
        BWART
        CHARG
        WERKS
            FROM MSEG INTO TABLE GT_MSEG FOR ALL ENTRIES IN GT_MARA WHERE MATNR = GT_MARA-MATNR AND AUFNR IN SO_AUFNR AND BUDAT_MKPF IN SO_B_MK AND MATNR IN SO_MATNR  AND BWART ='101' AND WERKS IN SO_PWERK .

 SELECT
         AUFNR
         PLNTY
         PLNBEZ
         GAMNG
         GMEIN
         IGMNG
         DISPO
         FEVOR
         STLST
         STLNR
         SBMNG
         GSTRP
         GLTRP
         GLTRI
         PVERW
         PLNNR
      FROM AFKO INTO TABLE GT_AFKO FOR ALL ENTRIES IN GT_MARA
   WHERE PLNBEZ  = GT_MARA-MATNR AND   DISPO IN SO_DISPO AND FEVOR IN SO_FEVOR
    AND PLNTY IN SO_PLNTY. "AND GSTRP  IN SO_GSTRP AND GLTRP IN SO_GLTRP .

*IF GT_AFKO[] IS NOT INITIAL. " MODIFIED BY RAM ON 22/09/2014

       SELECT " Changed By Goivnd On 18.08.2014
         DAUAT
         AUFNR
         PSAMG
         MATNR
         PWERK
         CHARG
         XLOEK
      FROM AFPO INTO TABLE GT_AFPO FOR ALL ENTRIES IN GT_AFKO   WHERE AUFNR = GT_AFKO-AUFNR AND  MATNR = GT_AFKO-PLNBEZ
         AND PWERK IN SO_PWERK AND DAUAT IN SO_DAUAT  AND   AUFNR IN SO_AUFNR AND   MATNR IN SO_MATNR    .

   SELECT
        MBLNR
        BUDAT
     FROM MKPF INTO TABLE GT_MKPF FOR ALL ENTRIES IN GT_AFRU WHERE MBLNR = GT_AFRU-WABLNR .

   SELECT
        ERSDA
        ERNAM
        BUDAT
        WABLNR
        LAEDA
        AENAM
        AUFNR
        EXERZ
     FROM AFRU INTO TABLE GT_AFRU FOR ALL ENTRIES IN GT_MSEG WHERE  AUFNR = GT_MSEG-AUFNR  .

   SELECT
        MATNR
        MAKTX
     FROM MAKT INTO TABLE ET_MAKT FOR ALL ENTRIES IN GT_MSEG WHERE  MATNR  =  GT_MSEG-MATNR.

 LOOP AT GT_MSEG INTO WA_MSEG.
      MOVE-CORRESPONDING WA_MSEG TO WA_FINAL.
        READ TABLE GT_AFKO INTO WA_AFKO WITH KEY  AUFNR  =  WA_MSEG-AUFNR  .
      MOVE-CORRESPONDING WA_AFKO TO WA_FINAL.  " Changed By Goivnd On 18.08.2014
        READ TABLE GT_AFRU INTO WA_AFRU WITH KEY  AUFNR  =  WA_MSEG-AUFNR .
           IF SY-SUBRC = 0.
                  WA_FINAL-BUDAT = WA_AFRU-BUDAT.
                  WA_FINAL-ERSDA = WA_AFRU-ERSDA.
                  WA_FINAL-ERNAM = WA_AFRU-ERNAM.
                  WA_FINAL-LAEDA = WA_AFRU-LAEDA.
                  WA_FINAL-AENAM = WA_AFRU-AENAM.
           ENDIF.
       READ TABLE ET_MAKT INTO WA_MAKT WITH KEY  MATNR  =  WA_MSEG-MATNR .
                  WA_FINAL-MAKTX = WA_MAKT-MAKTX.
       READ TABLE GT_MARA INTO WA_MARA WITH KEY  MATNR  =  WA_MSEG-MATNR .
                  WA_FINAL-VOLUM = WA_MARA-VOLUM.
                  WA_FINAL-VOLEH = WA_MARA-VOLEH.
                  WA_FINAL-MTART = WA_MARA-MTART.
        APPEND WA_FINAL TO GT_FINAL.
  CLEAR WA_FINAL.
 ENDLOOP.

LOOP AT GT_FINAL INTO WA_FINAL.
                  WA_FINAL-TQTY_LTR = WA_FINAL-VOLUM * WA_FINAL-GAMNG.
                  WA_FINAL-CQTY_LTR = WA_FINAL-VOLUM * WA_FINAL-MENGE.
                  WA_FINAL-SQTY_LTR = WA_FINAL-VOLUM * WA_FINAL-PSAMG.

SHIFT WA_FINAL-AUFNR LEFT DELETING LEADING '0'.
SHIFT WA_FINAL-MATNR LEFT DELETING LEADING '0'.
MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING AUFNR MATNR TQTY_LTR CQTY_LTR SQTY_LTR.
CLEAR WA_FINAL.
ENDLOOP.
    DELETE GT_FINAL WHERE AUFNR = ''.
LOOP AT GT_FINAL INTO WA_FINAL.
   IF WA_FINAL-EBELN NE ''.                               " CHANGED BY RAM ON 23/09/2014
    WA_FINAL-AUFNR = ' ' .
        ENDIF .
MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING AUFNR.
CLEAR WA_FINAL.
ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FIELD_CATLOG
*&---------------------------------------------------------------------*

FORM FIELD_CATLOG .

   PERFORM ALV_LAYOUT USING 1 'Order' 'AUFNR' 'GT_FINAL' ''.
   PERFORM ALV_LAYOUT USING 2 'Posting Date' 'BUDAT_MKPF' 'GT_FINAL' ''.
   PERFORM ALV_LAYOUT USING 3 'Material' 'MATNR' 'GT_FINAL' ''.
   PERFORM ALV_LAYOUT USING 4 'Material Description' 'MAKTX' 'GT_FINAL' ''.
   PERFORM ALV_LAYOUT USING 5 'Batch' 'CHARG' 'GT_FINAL' ''.
   PERFORM ALV_LAYOUT USING 6 'Material Type' 'MTART' 'GT_FINAL' ''.
  " PERFORM ALV_LAYOUT USING 3 'Movement Type' 'BWART' 'GT_FINAL' ''.
 "  PERFORM ALV_LAYOUT USING 7 'Order Type' 'PLNTY' 'GT_FINAL' ''.
   PERFORM ALV_LAYOUT USING 8 'MRP Controller' 'DISPO' 'GT_FINAL' ''.
   PERFORM ALV_LAYOUT USING 9 'Pro Supervisor' 'FEVOR' 'GT_FINAL' ''.
  " PERFORM ALV_LAYOUT USING 7 'Posting Date' 'BUDAT' 'GT_FINAL' ''.
   PERFORM ALV_LAYOUT USING 10 'GROUP' 'PLNNR' 'GT_FINAL' ''.
   PERFORM ALV_LAYOUT USING 11 'Planning Plant' 'WERKS' 'GT_FINAL' ''.

   PERFORM ALV_LAYOUT USING 13 'Volume' 'VOLUM' 'GT_FINAL' ''.
   PERFORM ALV_LAYOUT USING 15 'Volume Unit' 'VOLEH' 'GT_FINAL' ''.
   PERFORM ALV_LAYOUT USING 16 'Target Quantity' 'GAMNG' 'GT_FINAL' ''.
   PERFORM ALV_LAYOUT USING 17 'Target Qty. Unit' 'GMEIN' 'GT_FINAL' ''.
   PERFORM ALV_LAYOUT USING 18 'Target Qty in Ltr' 'TQTY_LTR' 'GT_FINAL' ''.

   PERFORM ALV_LAYOUT USING 19 'Confirmed Quantity' 'MENGE' 'GT_FINAL' ''. " RAMA
   PERFORM ALV_LAYOUT USING 21 'Confirmed Qty Unit' 'GMEIN' 'GT_FINAL' ''.
   PERFORM ALV_LAYOUT USING 23 'Confirmed Qty in Ltr' 'CQTY_LTR' 'GT_FINAL' ''.
   PERFORM ALV_LAYOUT USING 24 'Scrap Quantity' 'PSAMG' 'GT_FINAL' ''.
   PERFORM ALV_LAYOUT USING 25 'Scrap Qty in Ltr' 'SQTY_LTR' 'GT_FINAL' ''.
   PERFORM ALV_LAYOUT USING 26 'Entered On' 'ERSDA' 'GT_FINAL' ''.

   PERFORM ALV_LAYOUT USING 28 'Entered By' 'ERNAM' 'GT_FINAL' ''.
   PERFORM ALV_LAYOUT USING 29 'Created Time' 'EXERZ' 'GT_FINAL' ''.
  " PERFORM ALV_LAYOUT USING 30 'Changed On' 'LAEDA' 'GT_FINAL' ''.
    " PERFORM ALV_LAYOUT USING 31 'Changed By' 'AENAM' 'GT_FINAL' ''.
   PERFORM ALV_LAYOUT USING 32 'Bom Status' ' STLST' 'GT_FINAL' ''.
   PERFORM ALV_LAYOUT USING 33 'Bom' 'STLNR' 'GT_FINAL' ''.
   PERFORM ALV_LAYOUT USING 33 'Base Qty' 'SBMNG' 'GT_FINAL' ''.
   PERFORM ALV_LAYOUT USING 35 'Basic Start Date' 'GSTRP' 'GT_FINAL' ''.
   PERFORM ALV_LAYOUT USING 36 'Basic Finish Date' 'GLTRP' 'GT_FINAL' ''.
   PERFORM ALV_LAYOUT USING 37 'Actual Finish Date' 'GLTRI' 'GT_FINAL' ''.
   PERFORM ALV_LAYOUT USING 39 'USAGE' 'PVERW' 'GT_FINAL' ''.

  PERFORM ALV_LAYOUT USING 40 'Purchase Ord.No' 'EBELN' 'GT_FINAL' ''.

ENDFORM.                                        " FIELD_CATLOG

*&---------------------------------------------------------------------*
*&      Form  ALV_LAYOUT
*&---------------------------------------------------------------------*
FORM ALV_LAYOUT  USING   P1 P2 P3 P4 P5.
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
     IS_LAYOUT                       = LAYOUT
       IT_FIELDCAT                    = GT_FCAT[]
*     IT_EXCLUDING                   =
*     IT_SPECIAL_GROUPS              =
*     IT_SORT                        =
*     IT_FILTER                      =
*     IS_SEL_HIDE                    =
       I_DEFAULT                      = 'X'
       I_SAVE                         = ' '
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
ENDFORM.

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
         LV_PWERK(100) TYPE C,
          LV_BUDAT_MKPF(100) TYPE C.

  CLEAR : LV_PWERK,
          LV_BUDAT_MKPF.

   IF SO_PWERK-HIGH IS NOT INITIAL.
    CONCATENATE 'Plant Code :' SO_PWERK-LOW 'To' SO_PWERK-HIGH INTO LV_PWERK SEPARATED BY SPACE.
  ELSE.
    CONCATENATE 'Plant Code :' SO_PWERK-LOW INTO LV_PWERK SEPARATED BY SPACE.
  ENDIF.

  IF SO_B_MK-HIGH IS NOT INITIAL.
    CONCATENATE 'Posting Date :' SO_B_MK-LOW 'To' SO_B_MK-HIGH INTO LV_BUDAT_MKPF SEPARATED BY SPACE.
  ELSE.
    CONCATENATE 'Posting Date  :' SO_B_MK-LOW INTO LV_BUDAT_MKPF SEPARATED BY SPACE.
  ENDIF.

  CLEAR LS_LINE.
  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY = ' '.
  LS_LINE-INFO = LV_PWERK.
  APPEND LS_LINE TO LIT_HEADER.

  CLEAR LS_LINE.
  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY = ' '.
  LS_LINE-INFO = LV_BUDAT_MKPF.
  APPEND LS_LINE TO LIT_HEADER.

  CLEAR LS_LINE.
  LS_LINE-TYP  = 'H'.
  LS_LINE-KEY = ' '.
  LS_LINE-INFO = 'Process Order Information System' .
  APPEND LS_LINE TO LIT_HEADER.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = LIT_HEADER
      I_LOGO             = 'ZLOGO'.

  CALL FUNCTION 'CONVERSION_EXIT_SDATE_OUTPUT'
    EXPORTING
      INPUT  = LV_BUDAT_MKPF
    IMPORTING
      OUTPUT = LV_BUDAT_MKPF.

**  CALL FUNCTION 'CONVERSION_EXIT_SDATE_OUTPUT'
**
**  EXPORTING
**    INPUT         = LV_BEDAT.

*      I_LOGO             = ' '.

ENDFORM.                    "ALV_CATALOG_HEADER
