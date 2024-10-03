
*&---------------------------------------------------------------------*
*&  Report  ZEXC_RECPT_REP
*&
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&Functional                   : Mr.Govindarajan M                     *
*& Developer                   : Mr.Savariar.S                         *
*& Created On                  : 24/10/2014                            *
*& Company                     : Sheenlac Paints Pvt Ltd               *
*& Verified By                 : Mr.Govindarajan M                     *
*& Title                       : Exices Register Report Document           *
*& Report Name                 : ZEXC_RECPT_REP                     *
*& Development Id              : kpabap                                *
*& Related Information         : Vendor Purchase Order Tracking Report *
*&---------------------------------------------------------------------*

REPORT ZPP_DEMAND1 NO STANDARD PAGE HEADING MESSAGE-ID ZMSG.

DATA : LV_MSG(200).

TYPES : BEGIN OF GS_PBIM,
        MATNR TYPE PBIM-MATNR,              "Material
        WERKS TYPE PBIM-WERKS,              "Plant
        BEDAE TYPE PBIM-BEDAE,              "Reqmts type
        VERSB TYPE PBIM-VERSB,              "Version
        PBDNR TYPE PBIM-PBDNR,              "Reqmts Plan
        BDZEI TYPE PBIM-BDZEI,              "Reqmts pointer
        VERVS TYPE PBIM-VERVS,              "Active
        END OF GS_PBIM.

DATA : GT_PBIM TYPE TABLE OF GS_PBIM ,
       WA_PBIM TYPE GS_PBIM.

TYPES : BEGIN OF GS_PBED ,
        BDZEI TYPE PBED-BDZEI,                "Reqmts pointer
        PDATU TYPE PBED-PDATU,                "Finish date
        AENAM TYPE PBED-AENAM,                "Changed by
        LAEDA TYPE PBED-LAEDA,                "Last Change
        MEINS TYPE PBED-MEINS,                "Base Unit
        PLNMG TYPE P DECIMALS 2,              "PBED-PLNMG, "Planned qty

       END OF GS_PBED .

DATA : GT_PBED  TYPE TABLE OF GS_PBED ,
       WA_PBED  TYPE GS_PBED .

TYPES : BEGIN OF GS_T001W,
        WERKS TYPE T001W-WERKS,
        NAME1 TYPE T001W-NAME1,
        ORT01 TYPE T001W-ORT01,
        END OF GS_T001W.

DATA : GT_T001W TYPE TABLE OF GS_T001W,
       WA_T001W TYPE GS_T001W.

TYPES: BEGIN OF GS_MARA,
       MATNR TYPE MARA-MATNR,              " Material Code
       MTART TYPE MARA-MTART,              "CHANGES ON 20/09/2014
       MATKL TYPE MARA-MATKL,              " Material Group
       MEINS TYPE MARA-MEINS,              " UOM
       SPART TYPE MARA-SPART,              " Division
       VOLUM TYPE P DECIMALS 2,             "MARA-VOLUM,               " Volume
       END OF GS_MARA.

DATA: GT_MARA TYPE TABLE OF GS_MARA,
      WA_MARA TYPE GS_MARA.

TYPES: BEGIN OF GS_MAKT,
       MATNR TYPE MAKT-MATNR,              " Material Number
       MAKTX TYPE MAKT-MAKTX,              " Material Description
       END OF GS_MAKT.

DATA: GT_MAKT TYPE TABLE OF GS_MAKT,
      WA_MAKT TYPE GS_MAKT.


TYPES : BEGIN OF GS_MARCSP,
        MATNR TYPE MARC-MATNR,
        WERKS TYPE MARC-WERKS,
        SOBSL TYPE MARC-SOBSL,
      END OF GS_MARCSP.

DATA : GT_MARCSP TYPE TABLE OF GS_MARCSP,
       WA_MARCSP TYPE GS_MARCSP.

TYPES: BEGIN OF ES_MARD,
      LABST TYPE P DECIMALS 2,         "Unrestricted
      INSME TYPE P DECIMALS 2,         "In Qual. Insp.
      SPEME TYPE P DECIMALS 2,         "Blocked
      MATNR TYPE MARD-MATNR,
      WERKS TYPE MARD-WERKS,
      LGORT TYPE MARD-LGORT,
      END OF ES_MARD.

DATA: GT_MARD TYPE TABLE OF ES_MARD,
      WA_MARD TYPE ES_MARD.

TYPES: BEGIN OF ES_MARC,
      TRAME TYPE P DECIMALS 2,     " Stk in Transit
      EISBE TYPE MARC-EISBE,       " Safty Stock
      MATNR TYPE MARC-MATNR,       " Material
      WERKS TYPE MARC-WERKS,       " Plant
      END OF ES_MARC.

DATA: GT_MARC TYPE TABLE OF ES_MARC,
      WA_MARC TYPE ES_MARC.

TYPES: BEGIN OF ES_MARD1,
      LABST TYPE P DECIMALS 2,       "Unrestricted
      INSME TYPE P DECIMALS 2,       "In Qual. Insp.
      SPEME TYPE P DECIMALS 2,       "Blocked
      MATNR TYPE MARD-MATNR,
      WERKS TYPE MARD-WERKS,
      LGORT TYPE MARD-LGORT,
      END OF ES_MARD1.

DATA: GT_MARD1 TYPE TABLE OF ES_MARD1,
      WA_MARD1 TYPE ES_MARD1.

TYPES: BEGIN OF ES_MARC1,
      TRAME TYPE P DECIMALS 2,  " Stk in Transit
      EISBE TYPE MARC-EISBE,    " Safty Stock
      MATNR TYPE MARC-MATNR,    " Material
      WERKS TYPE MARC-WERKS,    " Plant
      END OF ES_MARC1.

DATA: GT_MARC1 TYPE TABLE OF ES_MARC1,
      WA_MARC1 TYPE ES_MARC1.


TYPES : BEGIN OF GS_FINAL,

        MATNR TYPE PBIM-MATNR,              "Material
        MATNR1 TYPE PBIM-MATNR,
        WERKS TYPE PBIM-WERKS,              "Plant
        WERKS1 TYPE PBIM-WERKS,             "Plant
        BEDAE TYPE PBIM-BEDAE,              "Reqmts type
        VERSB TYPE PBIM-VERSB,              "Version
        PBDNR TYPE PBIM-PBDNR,              "Reqmts Plan
        VERVS TYPE PBIM-VERVS,
        BDZEI TYPE PBIM-BDZEI,              "Reqmts pointer
        PDATU TYPE PBED-PDATU,               "Finish date
        AENAM TYPE PBED-AENAM,               "Changed by
        LAEDA TYPE PBED-LAEDA,               "Last Change
        MEINS TYPE PBED-MEINS,               "Base Unit
        FINALQTY TYPE P DECIMALS 2,
        PER_QTY TYPE KONV-KBETR,
        PLNMG TYPE P DECIMALS 2,             "Planned qty
        TOTALAMT TYPE P DECIMALS 2,
        TOTALAMT1 TYPE P DECIMALS 2,
        TOTALAMT2 TYPE P DECIMALS 2,
        TOTALAMT3 TYPE P DECIMALS 2,
        TOT_QTY1 TYPE P DECIMALS 2,
        FIN_TOTQTY TYPE P DECIMALS 2,
        FIN_TOTQTY1 TYPE P DECIMALS 2,
        FINALQTY1 TYPE P DECIMALS 2,
        MTART TYPE MARA-MTART,
        VOLUM TYPE P DECIMALS 2,              " Volume
        MAKTX TYPE MAKT-MAKTX,                " Material Description
        FLAG(1) TYPE C,
        NAME1 TYPE T001W-ORT01,
        NAME2 TYPE T001W-NAME1,
        LABST TYPE P DECIMALS 2,             "Unrestricted
        INSME TYPE P DECIMALS 2,
        LGORT TYPE MARD-LGORT,               "Storgae Location
        TRAME TYPE P DECIMALS 2,             "Transit
        SOBSL TYPE MARC-SOBSL,               "FOR SP KEY
        CUMATNR TYPE MARD-MATNR,
        LABST1 TYPE P DECIMALS 2,            "Unrestricted
        INSME1 TYPE P DECIMALS 2,
        TRAME1 TYPE P DECIMALS 2,            "Transit
        EISBE TYPE P DECIMALS 2,             "Safty Stock
        V_MATCOUNT TYPE I,
        V_WERKCOUNT TYPE I,
        V_STKCUMCOUNT TYPE I,
        CELLCOLOR TYPE LVC_T_SCOL,
        DAY TYPE I,

    END OF GS_FINAL.

DATA : GT_FINAL TYPE TABLE OF GS_FINAL,
       WA_FINAL TYPE GS_FINAL.

DATA : GT_FCAT TYPE SLIS_T_FIELDCAT_ALV,
       WA_FCAT TYPE SLIS_FIELDCAT_ALV.

DATA : LAYOUT TYPE SLIS_LAYOUT_ALV.

DATA : GT_SORT TYPE SLIS_T_SORTINFO_ALV,
       WA_SORT TYPE SLIS_SORTINFO_ALV.

DATA: L_MATNR TYPE PBIM-MATNR,
      L_WERKS TYPE PBIM-WERKS,
      L_VERSB TYPE PBIM-VERSB,   "VERSION
      L_PBDNR TYPE PBIM-PBDNR,   "PLANE NO
      L_AENAM TYPE PBED-AENAM,   "USER
      L_LAEDA TYPE PBED-LAEDA,   "lAST cHNAGE
      L_MTART TYPE MARA-MTART.   "MATERIAL TYPE

DATA : TOT_QTY TYPE P DECIMALS 2.
DATA : TOT_QTY1 TYPE P DECIMALS 2.
DATA : TOT_QTY2 TYPE P DECIMALS 2.
DATA : TOT_QTY3 TYPE P DECIMALS 2.
DATA : FIN_TOTQTY TYPE P DECIMALS 2.
DATA : FIN_TOTQTY1 TYPE P DECIMALS 2.
DATA : LV_DAYS TYPE I.
DATA: LV_WERKCOUNT TYPE I VALUE 0.
DATA: LV_MATCOUNT TYPE I VALUE 0.
DATA: LV_STKCUMCOUNT TYPE I VALUE 0.

DATA : LV_MATNR TYPE PBIM-MATNR,
       LV_WERKS TYPE PBIM-WERKS,
       LV_VERSB TYPE PBIM-VERSB,   "VERSION
       LV_PBDNR TYPE PBIM-PBDNR,   "PLANE NO
       LV_AENAM TYPE PBED-AENAM,   "USER
       LV_LAEDA TYPE PBED-LAEDA,   "lAST cHNAGE
       LV_MTART TYPE MARA-MTART.   "MATERIAL TYPE

SELECTION-SCREEN : BEGIN OF BLOCK B WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS:     SO_MATNR FOR LV_MATNR,
                    SO_WERKS FOR LV_WERKS OBLIGATORY,
                    SO_VERSB FOR LV_VERSB,   "VERSION
                    SO_PBDNR FOR LV_PBDNR,   "PLANE NO
                    SO_AENAM FOR LV_AENAM,    "USER
                    SO_LAEDA FOR LV_LAEDA,    "LAST CHNAGE
                    SO_MTART FOR LV_MTART.   "MATERIAL TYPE

PARAMETERS : P_DATE TYPE SY-DATUM DEFAULT SY-DATUM NO-DISPLAY.

SELECTION-SCREEN:END OF BLOCK B.

AT SELECTION-SCREEN .

  IF SO_WERKS IS NOT INITIAL .
    SELECT SINGLE WERKS FROM T001W INTO L_WERKS WHERE WERKS IN SO_WERKS .
    IF SY-SUBRC <> 0.
*      MESSAGE 'Enter Valid Plant' TYPE 'E' WITH SO_WERKS.
      CONCATENATE SO_WERKS-LOW 'is' 'not a valid plant'  INTO LV_MSG SEPARATED BY SPACE .
      MESSAGE LV_MSG TYPE 'E'.
    ENDIF.
  ENDIF.

  IF SO_MTART IS NOT INITIAL .
    SELECT SINGLE MTART FROM MARA INTO L_MTART WHERE MTART  IN SO_MTART.
    IF SY-SUBRC NE 0.
      MESSAGE 'Enter Valid Material Type.' TYPE 'E' .
    ENDIF.
  ENDIF.

START-OF-SELECTION.

  PERFORM GET_DATA.
  PERFORM AUTHCHECK_FORPLANTWISE.
  PERFORM FIELDCATLOG.
  PERFORM BUILD_LAYOUT .
  PERFORM SET_CELL_COLOURS .
  PERFORM ALV_DISPLAY.

END-OF-SELECTION.


*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM GET_DATA.

  SELECT
        MATNR
        WERKS
        BEDAE
        VERSB
        PBDNR
        BDZEI
        VERVS
    FROM
    PBIM INTO TABLE GT_PBIM  WHERE MATNR IN SO_MATNR AND WERKS IN SO_WERKS AND VERSB IN SO_VERSB AND PBDNR IN SO_PBDNR.

  IF GT_PBIM[] IS NOT INITIAL.

    SELECT
        BDZEI
        PDATU
        AENAM
        LAEDA
        MEINS
        PLNMG
      FROM
           PBED INTO TABLE GT_PBED FOR ALL ENTRIES IN GT_PBIM WHERE BDZEI = GT_PBIM-BDZEI AND AENAM IN SO_AENAM AND LAEDA IN SO_LAEDA .

*          PBED INTO TABLE GT_PBED FOR ALL ENTRIES IN GT_PBIM WHERE BDZEI = GT_PBIM-BDZEI AND AENAM IN SO_AENAM AND LAEDA IN SO_LAEDA .
*       PBED INTO TABLE GT_PBED WHERE AENAM IN SO_AENAM AND LAEDA IN SO_LAEDA .
  ENDIF.


  IF GT_PBED[] IS NOT INITIAL.
*
*  IF GT_PBIM[] IS NOT INITIAL.
**    SELECT
**        BDZEI
**        PDATU
**        AENAM
**        LAEDA
**        MEINS
**        PLNMG
**      FROM
***          PBED INTO TABLE GT_PBED FOR ALL ENTRIES IN GT_PBIM WHERE BDZEI = GT_PBIM-BDZEI AND AENAM IN SO_AENAM AND LAEDA IN SO_LAEDA .
**
**       PBED INTO TABLE GT_PBED WHERE AENAM IN SO_AENAM AND LAEDA IN SO_LAEDA .
**  ENDIF.
**
**  IF GT_PBED[] IS NOT INITIAL.

    SELECT
      WERKS
      NAME1
      ORT01
      FROM
      T001W INTO TABLE GT_T001W FOR ALL ENTRIES IN GT_PBIM WHERE WERKS = GT_PBIM-WERKS.

  ENDIF.

  SELECT
         MATNR
         MTART
         MATKL
         MEINS
         SPART
         VOLUM
   FROM MARA INTO TABLE GT_MARA FOR ALL ENTRIES IN GT_PBIM WHERE MATNR = GT_PBIM-MATNR AND MTART IN SO_MTART.

  SELECT
         MATNR
         MAKTX
   FROM MAKT INTO TABLE GT_MAKT FOR ALL ENTRIES IN GT_MARA WHERE MATNR = GT_MARA-MATNR.

  IF GT_PBIM[] IS NOT INITIAL.
    SELECT
         MATNR
         WERKS
         SOBSL
      FROM
       MARC INTO TABLE GT_MARCSP FOR ALL ENTRIES IN GT_PBIM WHERE MATNR = GT_PBIM-MATNR AND WERKS = '1100'.

    " For Stock in all Plant wise

    SELECT
            LABST
            INSME
            SPEME
            MATNR
            WERKS
            LGORT
         FROM MARD INTO TABLE GT_MARD FOR ALL ENTRIES IN GT_PBIM WHERE MATNR  = GT_PBIM-MATNR AND WERKS = GT_PBIM-WERKS AND WERKS IN  SO_WERKS ."AND LGORT = '0006' AND LGORT = '0007' AND LGORT = '0009' .

    " For Transit in all Plant wise


    SELECT
          TRAME
          EISBE
          MATNR
          WERKS
      FROM MARC INTO TABLE GT_MARC FOR ALL ENTRIES IN GT_PBIM WHERE MATNR = GT_PBIM-MATNR AND WERKS = GT_PBIM-WERKS AND WERKS IN  SO_WERKS .


    SELECT
            LABST
            INSME
            SPEME
            MATNR
            WERKS
            LGORT
         FROM MARD INTO TABLE GT_MARD1 FOR ALL ENTRIES IN GT_PBIM WHERE MATNR  = GT_PBIM-MATNR AND WERKS = '1151'.


    SELECT
             TRAME
             EISBE
             MATNR
             WERKS
         FROM MARC INTO TABLE GT_MARC1 FOR ALL ENTRIES IN GT_PBIM WHERE MATNR = GT_PBIM-MATNR AND WERKS ='1151'.

  ENDIF.


  IF SO_WERKS NE 'IEQ1003'.

    LOOP AT GT_PBED INTO WA_PBED ."WHERE BDZEI = WA_PBIM-BDZEI .
*    READ TABLE GT_PBED INTO WA_PBED. "WITH KEY BDZEI = WA_PBIM-BDZEI.

      WA_FINAL-PDATU = WA_PBED-PDATU.
      WA_FINAL-AENAM = WA_PBED-AENAM.
      WA_FINAL-LAEDA = WA_PBED-LAEDA.
      WA_FINAL-MEINS = WA_PBED-MEINS.
      WA_FINAL-PLNMG = WA_PBED-PLNMG.
      WA_FINAL-BDZEI = WA_PBED-BDZEI.

      LOOP AT GT_PBIM INTO WA_PBIM WHERE BDZEI = WA_PBED-BDZEI .
*      IF SY-SUBRC = 0.                                         "Hidded by savariar as on 29/04/2015
        WA_FINAL-MATNR = WA_PBIM-MATNR.
*        WA_FINAL-MATNR1 = WA_PBIM-MATNR.
        WA_FINAL-WERKS = WA_PBIM-WERKS.
        WA_FINAL-WERKS1 = WA_PBIM-WERKS.
        WA_FINAL-BEDAE = WA_PBIM-BEDAE.
        WA_FINAL-VERVS = WA_PBIM-VERVS.
        WA_FINAL-VERSB = WA_PBIM-VERSB.
        WA_FINAL-PBDNR = WA_PBIM-PBDNR.
*    WA_FINAL-BDZEI = WA_PBIM-BDZEI.
*      ENDIF.

        READ TABLE GT_T001W INTO WA_T001W WITH KEY  WERKS = WA_PBIM-WERKS.

        IF SY-SUBRC = 0.
          WA_FINAL-NAME1 = WA_T001W-ORT01.
          TRANSLATE WA_FINAL-NAME1  TO UPPER CASE.
        ENDIF.

        READ TABLE GT_MARA INTO WA_MARA WITH KEY MATNR = WA_PBIM-MATNR.

        IF SY-SUBRC = 0.
          WA_FINAL-MTART = WA_MARA-MTART.
          WA_FINAL-VOLUM = WA_MARA-VOLUM.
        ENDIF.

        READ TABLE GT_MAKT INTO WA_MAKT WITH KEY MATNR = WA_MARA-MATNR.

        IF SY-SUBRC = 0.
          WA_FINAL-MAKTX = WA_MAKT-MAKTX.
        ENDIF.

        READ TABLE GT_MARCSP INTO WA_MARCSP WITH KEY MATNR = WA_FINAL-MATNR.
        IF SY-SUBRC = 0.
          WA_FINAL-SOBSL = WA_MARCSP-SOBSL.
        ENDIF.

        LOOP AT GT_MARD INTO WA_MARD WHERE MATNR  = WA_PBIM-MATNR AND WERKS = WA_PBIM-WERKS.

          WA_FINAL-LABST = WA_FINAL-LABST + WA_MARD-LABST + WA_MARD-INSME.
*      WA_FINAL-INSME = WA_FINAL-INSME + WA_MARD-INSME.

        ENDLOOP.

        READ TABLE GT_MARC INTO WA_MARC WITH KEY MATNR = WA_PBIM-MATNR WERKS = WA_PBIM-WERKS.
        WA_FINAL-TRAME = WA_MARC-TRAME.

        APPEND WA_FINAL TO GT_FINAL.
        CLEAR WA_FINAL.
*ENDLOOP.

        DELETE GT_FINAL WHERE PLNMG EQ 0.

      ENDLOOP.

    ENDLOOP.

    LOOP AT GT_FINAL INTO WA_FINAL.

      LOOP AT GT_MARD1 INTO WA_MARD1 WHERE MATNR = WA_FINAL-MATNR AND WERKS = '1151'.
        IF SY-SUBRC = 0.
          WA_FINAL-CUMATNR = WA_MARD1-MATNR.
          WA_FINAL-LABST1 = WA_FINAL-LABST1 + WA_MARD1-LABST + WA_MARD1-INSME.
        ENDIF.
      ENDLOOP.

      READ TABLE GT_MARC1 INTO WA_MARC1 WITH KEY MATNR = WA_FINAL-MATNR  WERKS = '1151' .

      IF SY-SUBRC = 0.
        WA_FINAL-CUMATNR = WA_MARC1-MATNR.
        WA_FINAL-TRAME1 = WA_MARC1-TRAME.
*      WA_FINAL-EISBE = WA_MARC1-EISBE.
      ENDIF.

      MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING LABST1 TRAME1 CUMATNR.

      CLEAR WA_FINAL.

    ENDLOOP.


    SORT GT_FINAL BY WERKS MATNR.

    LOOP AT GT_FINAL INTO WA_FINAL .
      LV_WERKCOUNT = LV_WERKCOUNT  + 1.
      LV_MATCOUNT = LV_MATCOUNT  + 1.
      AT NEW WERKS .
        WA_FINAL-V_WERKCOUNT = LV_WERKCOUNT.
        MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING V_WERKCOUNT.
      ENDAT.
      AT NEW MATNR .
        WA_FINAL-V_MATCOUNT = LV_MATCOUNT.
        MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING V_MATCOUNT.
      ENDAT.
      CLEAR WA_FINAL.
    ENDLOOP.


    LOOP AT GT_FINAL INTO WA_FINAL.
      IF WA_FINAL-V_MATCOUNT = 0 AND WA_FINAL-V_WERKCOUNT = 0.
        WA_FINAL-LABST = ' '.
        WA_FINAL-TRAME = ' '.

        MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING LABST TRAME .
      ENDIF.
      CLEAR WA_FINAL.
    ENDLOOP.


    LOOP AT GT_FINAL INTO WA_FINAL.

      TOT_QTY  =  WA_FINAL-PLNMG * WA_FINAL-VOLUM .
      MOVE  TOT_QTY TO WA_FINAL-TOTALAMT  .

      MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING TOTALAMT .
      CLEAR WA_FINAL.

    ENDLOOP.

    IF SO_MTART IS NOT INITIAL.
      DELETE GT_FINAL WHERE MTART EQ ' '.
    ENDIF.


    LOOP AT GT_FINAL INTO WA_FINAL.

      IF SY-SUBRC = 0.

        IF WA_FINAL-WERKS NE 1151 AND WA_FINAL-WERKS NE 1152 AND WA_FINAL-WERKS NE 1153 AND WA_FINAL-WERKS NE 1154 AND WA_FINAL-WERKS NE 1155 AND
          WA_FINAL-WERKS NE 1156 AND WA_FINAL-WERKS NE 1157 AND WA_FINAL-WERKS NE 1158.

          WA_FINAL-LABST1 = ' '.
          WA_FINAL-TRAME1 = ' '.
          WA_FINAL-CUMATNR  = ' '.
          MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING LABST1 TRAME1 CUMATNR.

        ENDIF.
        CLEAR WA_FINAL.
      ENDIF.
    ENDLOOP.


*********************************1151****Starting**************

    SORT GT_FINAL ASCENDING BY CUMATNR.

    LOOP AT GT_FINAL INTO WA_FINAL .
      IF WA_FINAL-CUMATNR IS NOT INITIAL.
        LV_STKCUMCOUNT = LV_STKCUMCOUNT  + 1.
*       AT END OF STKCUW.
        ON CHANGE OF WA_FINAL-CUMATNR.
          WA_FINAL-V_STKCUMCOUNT = LV_STKCUMCOUNT.
          MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING V_STKCUMCOUNT.
          CLEAR WA_FINAL.
*      ENDAT.
        ENDON.
      ENDIF.
    ENDLOOP.


    LOOP AT GT_FINAL INTO WA_FINAL.
      IF WA_FINAL-V_STKCUMCOUNT = 0 .
        WA_FINAL-LABST1 = ' '.
        WA_FINAL-TRAME1 = ' '.
        MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING LABST1 TRAME1.
      ENDIF.
      CLEAR WA_FINAL.
    ENDLOOP.


    LOOP AT GT_FINAL INTO WA_FINAL.

*      TOT_QTY1 = WA_FINAL-LABST + WA_FINAL-TRAME + WA_FINAL-LABST1 + WA_FINAL-TRAME1.
*      MOVE TOT_QTY1 TO WA_FINAL-TOTALAMT1  .
*      TOT_QTY2 = WA_FINAL-TOTALAMT1 / 1000 .
*      MOVE TOT_QTY2 TO WA_FINAL-TOTALAMT2  .
*
*      MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING TOTALAMT1 TOTALAMT2 .
*      CLEAR WA_FINAL.

      WA_FINAL-TOTALAMT2 = WA_FINAL-LABST + WA_FINAL-TRAME + WA_FINAL-LABST1 + WA_FINAL-TRAME1.

      MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING TOTALAMT2 .
      CLEAR WA_FINAL.

    ENDLOOP.

    LOOP AT GT_FINAL INTO WA_FINAL.
*      FIN_TOTQTY = WA_FINAL-PLNMG - WA_FINAL-TOTALAMT2.
*      MOVE FIN_TOTQTY TO WA_FINAL-FINALQTY.
*      MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING FINALQTY.*
*      CLEAR WA_FINAL.
       WA_FINAL-FINALQTY = WA_FINAL-PLNMG - WA_FINAL-TOTALAMT2.
*      MOVE FIN_TOTQTY TO WA_FINAL-FINALQTY.
      MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING FINALQTY.

      CLEAR WA_FINAL.

    ENDLOOP.


    LOOP AT GT_FINAL INTO WA_FINAL.

*      TOT_QTY3  =  WA_FINAL-FINALQTY * WA_FINAL-VOLUM .
*      MOVE  TOT_QTY3 TO WA_FINAL-TOTALAMT3  .
*      MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING TOTALAMT3.
*      CLEAR WA_FINAL.
      WA_FINAL-TOTALAMT3  =  WA_FINAL-FINALQTY * WA_FINAL-VOLUM .
*      MOVE  TOT_QTY3 TO WA_FINAL-TOTALAMT3  .
      MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING TOTALAMT3.
      CLEAR WA_FINAL.


    ENDLOOP.


*******************************************End 1151********************************

    LOOP AT GT_FINAL INTO WA_FINAL.
      IF WA_FINAL-FINALQTY NE 0 AND WA_FINAL-FINALQTY < 0.
        WA_FINAL-FINALQTY = ''.
        WA_FINAL-TOTALAMT3 = ''.
        MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING FINALQTY TOTALAMT3.
        CLEAR WA_FINAL.

      ENDIF.

    ENDLOOP.

    """"""""""""""""""""""""""""""""""""""""""""""""""Only for Unit 3 """""""""""""""""""""""""""""""""""""""""""""""""

  ELSEIF SO_WERKS EQ 'IEQ1003'.

    LOOP AT GT_PBED INTO WA_PBED ."WHERE BDZEI = WA_PBIM-BDZEI .
*    READ TABLE GT_PBED INTO WA_PBED. "WITH KEY BDZEI = WA_PBIM-BDZEI.

      WA_FINAL-PDATU = WA_PBED-PDATU.
      WA_FINAL-AENAM = WA_PBED-AENAM.
      WA_FINAL-LAEDA = WA_PBED-LAEDA.
      WA_FINAL-MEINS = WA_PBED-MEINS.
      WA_FINAL-PLNMG = WA_PBED-PLNMG.
      WA_FINAL-BDZEI = WA_PBED-BDZEI.

      LOOP AT GT_PBIM INTO WA_PBIM WHERE BDZEI = WA_PBED-BDZEI .
*      IF SY-SUBRC = 0.
        WA_FINAL-MATNR = WA_PBIM-MATNR.
*        WA_FINAL-MATNR1 = WA_PBIM-MATNR.
        WA_FINAL-WERKS = WA_PBIM-WERKS.
        WA_FINAL-WERKS1 = WA_PBIM-WERKS.
        WA_FINAL-BEDAE = WA_PBIM-BEDAE.
        WA_FINAL-VERVS = WA_PBIM-VERVS.
        WA_FINAL-VERSB = WA_PBIM-VERSB.
        WA_FINAL-PBDNR = WA_PBIM-PBDNR.
*    WA_FINAL-BDZEI = WA_PBIM-BDZEI.
*      ENDIF.

        READ TABLE GT_T001W INTO WA_T001W WITH KEY  WERKS = WA_PBIM-WERKS.

        IF SY-SUBRC = 0.
          WA_FINAL-NAME2 = WA_T001W-NAME1.
*          TRANSLATE WA_FINAL-NAME1  TO UPPER CASE.
        ENDIF.

        READ TABLE GT_MARA INTO WA_MARA WITH KEY MATNR = WA_PBIM-MATNR.

        IF SY-SUBRC = 0.
          WA_FINAL-MTART = WA_MARA-MTART.
          WA_FINAL-VOLUM = WA_MARA-VOLUM.
        ENDIF.

        READ TABLE GT_MAKT INTO WA_MAKT WITH KEY MATNR = WA_MARA-MATNR.

        IF SY-SUBRC = 0.
          WA_FINAL-MAKTX = WA_MAKT-MAKTX.
        ENDIF.

        READ TABLE GT_MARCSP INTO WA_MARCSP WITH KEY MATNR = WA_FINAL-MATNR.
        IF SY-SUBRC = 0.
          WA_FINAL-SOBSL = WA_MARCSP-SOBSL.
        ENDIF.

        LOOP AT GT_MARD INTO WA_MARD WHERE MATNR  = WA_PBIM-MATNR AND WERKS = WA_PBIM-WERKS.

          WA_FINAL-LABST = WA_FINAL-LABST + WA_MARD-LABST + WA_MARD-INSME.
*      WA_FINAL-INSME = WA_FINAL-INSME + WA_MARD-INSME.

        ENDLOOP.

        READ TABLE GT_MARC INTO WA_MARC WITH KEY MATNR = WA_PBIM-MATNR WERKS = WA_PBIM-WERKS.

        WA_FINAL-TRAME = WA_MARC-TRAME.
        WA_FINAL-EISBE = WA_MARC-EISBE.      "Safty Stock
        CALL FUNCTION 'HR_99S_INTERVAL_BETWEEN_DATES'
          EXPORTING
            BEGDA = WA_PBED-PDATU
            ENDDA = P_DATE
          IMPORTING
            DAYS  = LV_DAYS.

        IF LV_DAYS NE 0.

          WA_FINAL-DAY = LV_DAYS.

        ENDIF.

        APPEND WA_FINAL TO GT_FINAL.
        CLEAR WA_FINAL.

        DELETE GT_FINAL WHERE PLNMG EQ 0.

      ENDLOOP.

    ENDLOOP.


    SORT GT_FINAL BY WERKS MATNR.

    LOOP AT GT_FINAL INTO WA_FINAL .
      LV_WERKCOUNT = LV_WERKCOUNT  + 1.
      LV_MATCOUNT = LV_MATCOUNT  + 1.
      AT NEW WERKS .
        WA_FINAL-V_WERKCOUNT = LV_WERKCOUNT.
        MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING V_WERKCOUNT.
      ENDAT.
      AT NEW MATNR .
        WA_FINAL-V_MATCOUNT = LV_MATCOUNT.
        MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING V_MATCOUNT.
      ENDAT.
      CLEAR WA_FINAL.
    ENDLOOP.

    LOOP AT GT_FINAL INTO WA_FINAL.
      IF WA_FINAL-V_MATCOUNT = 0 AND WA_FINAL-V_WERKCOUNT = 0.
        WA_FINAL-LABST = ' '.
        WA_FINAL-TRAME = ' '.

        MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING LABST TRAME .
      ENDIF.
      CLEAR WA_FINAL.
    ENDLOOP.

    IF SO_MTART IS NOT INITIAL.
      DELETE GT_FINAL WHERE MTART EQ ' '.
    ENDIF.

    LOOP AT GT_FINAL INTO WA_FINAL.

      WA_FINAL-FINALQTY = WA_FINAL-PLNMG - WA_FINAL-LABST.
      WA_FINAL-FINALQTY1 = WA_FINAL-PLNMG - WA_FINAL-LABST.

      SHIFT WA_FINAL-MATNR LEFT DELETING LEADING '0'.
      MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING FINALQTY FINALQTY1 MATNR.
      CLEAR WA_FINAL.

    ENDLOOP.

    LOOP AT GT_FINAL INTO WA_FINAL.

      IF WA_FINAL-FINALQTY1 NE 0 AND WA_FINAL-FINALQTY1 < 0.

        WA_FINAL-FINALQTY1 = ''.

        MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING FINALQTY1 .
        CLEAR WA_FINAL.

      ENDIF.

    ENDLOOP.

    LOOP AT GT_FINAL INTO WA_FINAL.
      IF WA_FINAL-FINALQTY1 NE 0 AND WA_FINAL-FINALQTY1 > 0.
        WA_FINAL-PER_QTY =  WA_FINAL-FINALQTY1 / WA_FINAL-PLNMG  * 100.
        MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING PER_QTY.
        CLEAR WA_FINAL.
      ENDIF.
    ENDLOOP.

LOOP AT GT_FINAL INTO WA_FINAL.

  IF WA_FINAL-DAY LE 0 OR WA_FINAL-PER_QTY EQ 0.

   WA_FINAL-DAY = 0.

  MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING DAY.
  CLEAR WA_FINAL.

  ENDIF.

ENDLOOP.

  ENDIF.


ENDFORM.                    "GET_DATA


*&---------------------------------------------------------------------*
*&      Form  AUTHCHECK_FORPLANTWISE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM AUTHCHECK_FORPLANTWISE.


  LOOP AT GT_FINAL INTO WA_FINAL.
    AUTHORITY-CHECK OBJECT 'ZPLANT'
    ID 'ZBUKRS' DUMMY
    ID 'ZWERKS' FIELD WA_FINAL-WERKS
    ID 'ACTVT' FIELD '03'.
    IF SY-SUBRC NE 0.
      MESSAGE 'NO AUTHORIZATION FOR THIS REPORT' TYPE 'E'.
    ENDIF.
  ENDLOOP.
*  DELETE GT_FINAL WHERE FLAG = 'X'.
ENDFORM.                    " AUTHCHECK_OVERVIEW


*&---------------------------------------------------------------------*
*&      Form  FIELDCATLOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FIELDCATLOG.

  IF SO_WERKS NE 'IEQ1003'.

    PERFORM ALV_LAYOUT USING 1 'Plant' 'WERKS' 'GT_FINAL' '' '' 'X' '' '' .
    PERFORM ALV_LAYOUT USING 3 'Plant Name' 'NAME1' 'GT_FINAL' '' '' 'X' ''  '' .
    PERFORM ALV_LAYOUT USING 5 'Material Code' 'MATNR' 'GT_FINAL' '' '' 'X' ''  ''.
    PERFORM ALV_LAYOUT USING 7 'Material Description' 'MAKTX' 'GT_FINAL' '' '' 'X' ''  ''.
    PERFORM ALV_LAYOUT USING 9 'Volume' 'VOLUM' 'GT_FINAL' '' ''  'X' '' 'X'.
    PERFORM ALV_LAYOUT USING 11 'Material Type' 'MTART' 'GT_FINAL' '' '' 'X' ''  ''.

    PERFORM ALV_LAYOUT USING 13 'User' 'AENAM ' 'GT_FINAL' '' '' '' ''  ''.
    PERFORM ALV_LAYOUT USING 15 'Req Plan' 'PBDNR' 'GT_FINAL' '' '' '' ''  ''.
    PERFORM ALV_LAYOUT USING 17 'Version' 'VERSB' 'GT_FINAL' '' '' ''  '' ''.
    PERFORM ALV_LAYOUT USING 19 'Active' 'VERVS' 'GT_FINAL' '' '' '' '' ''.
    PERFORM ALV_LAYOUT USING 21 'Req Type' 'BEDAE' 'GT_FINAL' '' '' '' ''  ''.
    PERFORM ALV_LAYOUT USING 23 'Req Point' 'BDZEI' 'GT_FINAL' '' '' '' '' ''.
    PERFORM ALV_LAYOUT USING 25 'Entry Date' 'LAEDA' 'GT_FINAL' '' '' '' ''  ''.
    PERFORM ALV_LAYOUT USING 27 'Req Date' 'PDATU' 'GT_FINAL' '' '' '' ''  ''.

    PERFORM ALV_LAYOUT USING 29 'Planned Qty' 'PLNMG' 'GT_FINAL' 'X' '' '' ''  'X'.
    PERFORM ALV_LAYOUT USING 31 'Bun' 'MEINS' 'GT_FINAL' '' '' '' ''  ''.
    PERFORM ALV_LAYOUT USING 33 'Planned Qty(Ltrs)' 'TOTALAMT' 'GT_FINAL' 'X' ''  '' '' 'X'.
*  PERFORM ALV_LAYOUT USING 27 'Unit Wise' 'SOBSL' 'GT_FINAL' '' ''  '' '' 'X'.
    PERFORM ALV_LAYOUT USING 35 'Closing Stock' 'LABST' 'GT_FINAL' 'X' ''  '' '' 'X'.
    PERFORM ALV_LAYOUT USING 37 'In Transit' 'TRAME' 'GT_FINAL' 'X' ''  '' '' 'X'.

    PERFORM ALV_LAYOUT USING 39 '1151 Closing Stock' 'LABST1' 'GT_FINAL' 'X' ''  '' '' 'X'.
    PERFORM ALV_LAYOUT USING 41 '1151 In Transit' 'TRAME1' 'GT_FINAL' 'X' ''  '' '' 'X'.

    PERFORM ALV_LAYOUT USING 43 'Fin.Plan Qty' 'FINALQTY' 'GT_FINAL' 'X' ''  '' '' 'X'.
    PERFORM ALV_LAYOUT USING 45 'Fin.PlanQty(Ltrs)' 'TOTALAMT3' 'GT_FINAL' 'X' ''  '' '' 'X'.
*
*
*  PERFORM ALV_LAYOUT USING 47 'CUMATNR' 'CUMATNR' 'GT_FINAL' '' ''  '' '' ''.
*  PERFORM ALV_LAYOUT USING 48 'WERKS1' 'WERKS1' 'GT_FINAL' '' ''  '' '' ''.
*  PERFORM ALV_LAYOUT USING 49 'V_STKCUMCOUNT' 'V_STKCUMCOUNT' 'GT_FINAL' '' ''  '' '' ''.


*  WA_SORT-FIELDNAME = 'NAME1'.
*  WA_SORT-TABNAME = 'GT_FINAL'.
*  WA_SORT-UP = 'X'.
*  APPEND WA_SORT TO GT_SORT.
*  CLEAR WA_SORT.

    WA_SORT-FIELDNAME = 'WERKS'.
    WA_SORT-TABNAME = 'GT_FINAL'.
    WA_SORT-UP = 'X'.
    APPEND WA_SORT TO GT_SORT.
    CLEAR WA_SORT.
*  WA_SORT-FIELDNAME = 'VERVS'.
*  WA_SORT-TABNAME = 'GT_FINAL'.
*  WA_SORT-UP = 'X'.
*  APPEND WA_SORT TO GT_SORT.
*  CLEAR WA_SORT.


  ELSEIF SO_WERKS EQ 'IEQ1003'.

    PERFORM ALV_LAYOUT USING 1 'Plant' 'WERKS' 'GT_FINAL' '' '' 'X' '' '' .
    PERFORM ALV_LAYOUT USING 3 'Plant Name' 'NAME2' 'GT_FINAL' '' '' 'X' ''  '' .
    PERFORM ALV_LAYOUT USING 5 'Material Code' 'MATNR' 'GT_FINAL' '' '' 'X' ''  ''.
    PERFORM ALV_LAYOUT USING 7 'Material Description' 'MAKTX' 'GT_FINAL' '' '' 'X' ''  ''.
*    PERFORM ALV_LAYOUT USING 9 'Volume' 'VOLUM' 'GT_FINAL' '' ''  'X' '' 'X'.
    PERFORM ALV_LAYOUT USING 11 'Material Type' 'MTART' 'GT_FINAL' '' '' 'X' ''  ''.

    PERFORM ALV_LAYOUT USING 13 'User' 'AENAM ' 'GT_FINAL' '' '' '' ''  ''.
    PERFORM ALV_LAYOUT USING 15 'Req Plan' 'PBDNR' 'GT_FINAL' '' '' '' ''  ''.
    PERFORM ALV_LAYOUT USING 17 'Version' 'VERSB' 'GT_FINAL' '' '' ''  '' ''.
    PERFORM ALV_LAYOUT USING 19 'Active' 'VERVS' 'GT_FINAL' '' '' '' '' ''.
    PERFORM ALV_LAYOUT USING 21 'Req Type' 'BEDAE' 'GT_FINAL' '' '' '' ''  ''.
    PERFORM ALV_LAYOUT USING 23 'Req Point' 'BDZEI' 'GT_FINAL' '' '' '' '' ''.
    PERFORM ALV_LAYOUT USING 25 'Entry Date' 'LAEDA' 'GT_FINAL' '' '' '' ''  ''.
    PERFORM ALV_LAYOUT USING 27 'Req Date' 'PDATU' 'GT_FINAL' '' '' '' ''  ''.

    PERFORM ALV_LAYOUT USING 29 'Planned Qty' 'PLNMG' 'GT_FINAL' 'X' '' '' ''  'X'.
    PERFORM ALV_LAYOUT USING 31 'Bun' 'MEINS' 'GT_FINAL' '' '' '' ''  ''.

    PERFORM ALV_LAYOUT USING 32 'Safety Stock' 'EISBE' 'GT_FINAL' 'X' '' '' ''  'X'.
*  PERFORM ALV_LAYOUT USING 33 'Planned Qty(Ltrs)' 'TOTALAMT' 'GT_FINAL' 'X' ''  '' '' 'X'.
*  PERFORM ALV_LAYOUT USING 27 'Unit Wise' 'SOBSL' 'GT_FINAL' '' ''  '' '' 'X'.
    PERFORM ALV_LAYOUT USING 35 'Closing Stock' 'LABST' 'GT_FINAL' 'X' ''  '' '' 'X'.
    PERFORM ALV_LAYOUT USING 37 'In Transit' 'TRAME' 'GT_FINAL' 'X' ''  '' '' 'X'.

*  PERFORM ALV_LAYOUT USING 39 '1151 Closing Stock' 'LABST1' 'GT_FINAL' 'X' ''  '' '' 'X'.
*  PERFORM ALV_LAYOUT USING 41 '1151 In Transit' 'TRAME1' 'GT_FINAL' 'X' ''  '' '' 'X'.
    PERFORM ALV_LAYOUT USING 43 'Fin.Plan Qty' 'FINALQTY' 'GT_FINAL' '' ''  '' '' 'X'.
    PERFORM ALV_LAYOUT USING 44 'Fin.Req. Qty in Nos' 'FINALQTY1' 'GT_FINAL' 'X' ''  '' '' 'X'.
    PERFORM ALV_LAYOUT USING 45 '%' 'PER_QTY' 'GT_FINAL' '' ''  '' '' ''.
    PERFORM ALV_LAYOUT USING 46 'No of Dely Days' 'DAY' 'GT_FINAL' '' ''  '' '' 'X'.

*  PERFORM ALV_LAYOUT USING 45 'Fin.PlanQty(Ltrs)' 'TOTALAMT3' 'GT_FINAL' 'X' ''  '' '' 'X'.*
*  PERFORM ALV_LAYOUT USING 47 'CUMATNR' 'CUMATNR' 'GT_FINAL' '' ''  '' '' ''.
*  PERFORM ALV_LAYOUT USING 48 'WERKS1' 'WERKS1' 'GT_FINAL' '' ''  '' '' ''.
*  PERFORM ALV_LAYOUT USING 49 'V_STKCUMCOUNT' 'V_STKCUMCOUNT' 'GT_FINAL' '' ''  '' '' ''.


*  WA_SORT-FIELDNAME = 'NAME1'.
*  WA_SORT-TABNAME = 'GT_FINAL'.
*  WA_SORT-UP = 'X'.
*  APPEND WA_SORT TO GT_SORT.
*  CLEAR WA_SORT.

    WA_SORT-FIELDNAME = 'WERKS'.
    WA_SORT-TABNAME = 'GT_FINAL'.
    WA_SORT-UP = 'X'.
    APPEND WA_SORT TO GT_SORT.
    CLEAR WA_SORT.
*  WA_SORT-FIELDNAME = 'VERVS'.
*  WA_SORT-TABNAME = 'GT_FINAL'.
*  WA_SORT-UP = 'X'.
*  APPEND WA_SORT TO GT_SORT.
*  CLEAR WA_SORT.

  ENDIF.

ENDFORM.                    "FIELDCATLOG


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
*      -->P9         text
*----------------------------------------------------------------------*
FORM ALV_LAYOUT USING P1 P2 P3 P4 P5 P6 P7 P8 P9.

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
  CLEAR WA_FCAT.

ENDFORM.                    "ALV_LAYOUT




*&---------------------------------------------------------------------*
*&      Form  BUILD_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_LAYOUT .

  LAYOUT-NO_INPUT          = 'X'.
  LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  "  GD_LAYOUT-TOTALS_TEXT       = 'Totals'(201).
  LAYOUT-COLTAB_FIELDNAME = 'CELLCOLOR'.  "CTAB_FNAME

ENDFORM.                    " BUILD_LAYOUT

*&------------------------------------------------------------------------
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


    IF WA_FINAL-PER_QTY <= 0 .
      WA_CELLCOLOR-FNAME = 'PER_QTY'.
      WA_CELLCOLOR-COLOR-COL = 5.  "color code 1-7, if outside rage defaults to 7
      WA_CELLCOLOR-COLOR-INT = '1'.  "1 = Intensified on, 0 = Intensified off
      WA_CELLCOLOR-COLOR-INV = '1'.  "1 = text colour, 0 = background colour
      APPEND WA_CELLCOLOR TO WA_FINAL-CELLCOLOR.
      MODIFY GT_FINAL FROM WA_FINAL INDEX LD_INDEX TRANSPORTING CELLCOLOR.
      CLEAR :WA_FINAL , WA_CELLCOLOR.
    ENDIF.

    IF WA_FINAL-PER_QTY GT 0 . " AND WA_FINAL-PER_REQ GT 50 .
      WA_CELLCOLOR-FNAME = 'PER_QTY'.
      WA_CELLCOLOR-COLOR-COL = 6.  "color code 1-7, if outside rage defaults to 7
      WA_CELLCOLOR-COLOR-INT = '1'.  "1 = Intensified on, 0 = Intensified off
      WA_CELLCOLOR-COLOR-INV = '1'.  "1 = text colour, 0 = background colour
      APPEND WA_CELLCOLOR TO WA_FINAL-CELLCOLOR.
      MODIFY GT_FINAL FROM WA_FINAL INDEX LD_INDEX TRANSPORTING CELLCOLOR.
      CLEAR :WA_FINAL , WA_CELLCOLOR.
    ENDIF.


  ENDLOOP.

ENDFORM.                    " SET_CELL_COLOURS


*&---------------------------------------------------------------------*
*&      Form  ALV_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ALV_DISPLAY.
*DELETE GT_FINAL WHERE WERKS = ''.
*  DELETE ADJACENT DUPLICATES FROM GT_FINAL COMPARING WERKS NAME1 .
  LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  LAYOUT-ZEBRA = 'X'.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM     = SY-REPID
      I_CALLBACK_TOP_OF_PAGE = 'ALV_CATALOG_HEADER'
      IS_LAYOUT              = LAYOUT
      IT_FIELDCAT            = GT_FCAT[]
      IT_SORT                = GT_SORT[]
      I_DEFAULT              = 'X'
      I_SAVE                 = 'A'
    TABLES
      T_OUTTAB               = GT_FINAL[].
*    EXCEPTIONS
*      OPERATION_NO_AUTHORIZATION = 1.

  IF SY-SUBRC <> 0.
    WRITE: 'FAILURE'.
  ENDIF.


ENDFORM.                    "ALV_DISPLAY



*&---------------------------------------------------------------------*
*&      Form  ALV_CATALOG_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ALV_CATALOG_HEADER.

  DATA : LIT_HEADER TYPE  SLIS_T_LISTHEADER,
         LS_LINE TYPE SLIS_LISTHEADER.


  DATA :
         LV_WERKS(100) TYPE C.
*         LV_BEDAT(50) TYPE C,
*         LV_LIFNR(100) TYPE C.
*         LV_BEDAT1 TYPE SY-DATUM.

  CLEAR : LV_WERKS.
*          LV_LIFNR,
  IF SO_WERKS NE 'IEQ1003'.

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
    LS_LINE-INFO = 'Planned Demand Detail At Plant Level' .
    APPEND LS_LINE TO LIT_HEADER.

  ELSEIF SO_WERKS EQ 'IEQ1003'.

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
    LS_LINE-INFO = 'Planned Demand Detail At Unit-3' .
    APPEND LS_LINE TO LIT_HEADER.

  ENDIF.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = LIT_HEADER.

*  CALL FUNCTION 'CONVERSION_EXIT_SDATE_OUTPUT'
*    EXPORTING
*      INPUT  = LV_BEDAT
*    IMPORTING
*      OUTPUT = LV_BEDAT.

**  CALL FUNCTION 'CONVERSION_EXIT_SDATE_OUTPUT'
**
**  EXPORTING
**    INPUT         = LV_BEDAT.

*      I_LOGO             = ' '.


ENDFORM.                    "ALV_CATALOG_HEADER
