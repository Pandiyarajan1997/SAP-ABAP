*&---------------------------------------------------------------------*
*& Report  ZSD_SALES_REGISTER
*&
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*

REPORT ZSD_SALES_REGISTER.


TYPES : BEGIN OF GS_VBRK,
        VBELN TYPE VBAK-VBELN,
        VTWEG LIKE VBRK-VTWEG,
        FKDAT TYPE VBRK-FKDAT,
        FKART TYPE VBRK-FKART,
        KUNRG TYPE VBRK-KUNRG,
        KNUMV TYPE VBRK-KNUMV,
        WAERK TYPE VBRK-WAERK,
        BUKRS LIKE VBRK-BUKRS,
        GJAHR LIKE VBRK-GJAHR,
        NETWR TYPE VBRK-NETWR,
        SPART LIKE VBRK-SPART,
        END OF GS_VBRK.

TYPES : BEGIN OF GS_VBRP,
        VBELN TYPE VBELN_VF,
        POSNR TYPE VBRP-POSNR,
        WERKS TYPE WERKS_D,
        AUBEL TYPE VBELN_VA,
        VKBUR LIKE VBRP-VKBUR,
        VGBEL TYPE VBRP-VGBEL,
        VGPOS TYPE VBRP-VGPOS,
        MATNR TYPE VBRP-MATNR,
        ARKTX TYPE VBRP-ARKTX,
        FKIMG TYPE VBRP-FKIMG,
        MEINS TYPE VBRP-MEINS,
        PRCTR TYPE VBRP-PRCTR,
        KURSK TYPE VBRP-KURSK,
        END OF GS_VBRP.

TYPES : BEGIN OF GS_VBAK,
        VBELN TYPE VBELN_VA,
        AUDAT TYPE VBAK-AUDAT,
        KUNNR TYPE KUNAG,
        END OF GS_VBAK.

TYPES : BEGIN OF GS_LIKP,
        VBELN TYPE VBELN_VL,
        LFDAT TYPE LFDAT_V,
        KUNNR TYPE KUNWE,
        END OF GS_LIKP.

TYPES : BEGIN OF GS_T001W,
        WERKS TYPE WERKS_D,
        NAME1 TYPE NAME1,
        KUNNR TYPE KUNNR_WK,
        END OF GS_T001W.

TYPES : BEGIN OF GS_KNA1,
        KUNNR TYPE KNA1-KUNNR,
        NAME1 TYPE KNA1-NAME1,
        END OF GS_KNA1.

TYPES : BEGIN OF GS_T007A,
        MWSKZ TYPE T007A-MWSKZ,
        MWART TYPE T007A-MWART,
        TEXT1 TYPE TEXT1,
        END OF GS_T007A.

TYPES : BEGIN OF GS_T685A,
        KSCHL TYPE T685A-KSCHL,
        END OF GS_T685A.

TYPES: BEGIN OF GS_KONV,
       KNUMV TYPE KONV-KNUMV,
       KPOSN TYPE KONV-KPOSN,
       KSCHL TYPE KONV-KSCHL,
       KWERT TYPE KONV-KWERT,
       KBETR TYPE KONV-KBETR,
       KAWRT TYPE KONV-KAWRT,
       KUNNR TYPE KUNNR_KO,
       MWSK1 TYPE KONV-MWSK1,
       KAPPL TYPE KONV-KAPPL,
       WAERS TYPE KONV-WAERS,
       KKURS TYPE KONV-KKURS,
       END OF GS_KONV.

TYPES: BEGIN OF GS_MSEG,
       MBLNR TYPE MSEG-MBLNR,
       MATNR TYPE MSEG-MATNR,
       MAT_KDAUF TYPE MSEG-MAT_KDAUF,
       MAT_KDPOS TYPE MSEG-MAT_KDPOS,
       END OF GS_MSEG.

TYPES: BEGIN OF GS_SER03,
       OBKNR TYPE SER03-OBKNR,
       MBLNR TYPE SER03-MBLNR,
       DATUM TYPE SER03-DATUM,
       END OF GS_SER03.

TYPES: BEGIN OF GS_OBJK,
       OBKNR TYPE OBJK-OBKNR,
       OBZAE TYPE OBJK-OBZAE,
       SERNR TYPE OBJK-SERNR,
       MATNR TYPE OBJK-MATNR,
       END OF GS_OBJK.

TYPES: BEGIN OF GS_MARA,
       MATNR TYPE MARA-MATNR,
       VOLUM TYPE MARA-VOLUM,
       VOLEH TYPE MARA-VOLEH,
       END OF GS_MARA.

TYPES : BEGIN OF GS_TVFKT,
        FKART TYPE TVFKT-FKART,
        VTEXT TYPE TVFKT-VTEXT,
        END OF GS_TVFKT.

TYPES : BEGIN OF GS_TSPAT,
        SPART TYPE TSPAT-SPART,
        VTEXT TYPE TSPAT-VTEXT,
        END OF GS_TSPAT.

TYPES : BEGIN OF GS_FINAL,
        VKBUR TYPE VBRP-VKBUR,
        WERKS TYPE T001W-WERKS,
        VTWEG TYPE VBRK-VTWEG,            "added by savariar 08/10/2014
        SPART TYPE VBRK-SPART,
        DTEXT TYPE TSPAT-VTEXT,         "Added by savariar s on 08/10/2014.
        FKART TYPE VBRK-FKART,
        VTEXT TYPE TVFKT-VTEXT,
        VBELN TYPE VBAK-VBELN,
        FKDAT TYPE VBRK-FKDAT,
        KUNRG TYPE VBRK-KUNRG,
        NAME1 TYPE KNA1-NAME1,
        BUKRS TYPE VBRK-BUKRS,
*        GJAHR TYPE VBRK-GJAHR,
        NETWR TYPE VBRK-NETWR,
        AUBEL TYPE VBRP-AUBEL,
        AUDAT TYPE VBAK-AUDAT,
        VGBEL TYPE VBRP-VGBEL,
        LFDAT TYPE LIKP-LFDAT,
        MATNR TYPE VBRP-MATNR,
        ARKTX TYPE VBRP-ARKTX,
        MEINS TYPE VBRP-MEINS,
        FKIMG TYPE VBRP-FKIMG,
        VOLUM TYPE MARA-VOLUM,
        VOLEH TYPE MARA-VOLEH,
        PRCTR TYPE VBRP-PRCTR,
        PRICE TYPE P DECIMALS 2,
        VALUE TYPE P DECIMALS 2,
        DISC TYPE P DECIMALS 2,
        ADISC TYPE P DECIMALS 2,
        CRBT TYPE P DECIMALS 2,
        YBSD TYPE P DECIMALS 2,
        Y029 TYPE P DECIMALS 2,
        YBRD TYPE P DECIMALS 2,
        BED  TYPE P DECIMALS 2,
        ECESS TYPE P DECIMALS 2,
        SHECSS TYPE P DECIMALS 2,
        TOT_LIT TYPE P DECIMALS 2,
        EX_DUTY TYPE P DECIMALS 2,
        VAT TYPE P DECIMALS 2,
        AVAT TYPE P DECIMALS 2,
        CST TYPE P DECIMALS 2,
        BTAX_AMT TYPE P DECIMALS 2,
        TTAX_AMT TYPE P DECIMALS 2,
        TOT_INV_AMT TYPE P DECIMALS 2,
        INV_AMT TYPE P DECIMALS 2,
        TCSTDS TYPE P DECIMALS 2,
        END OF GS_FINAL.

*************************************************************************************

******            Internal table & Work area Declaration                        *****
*************************************************************************************

DATA: GT_VBRK TYPE TABLE OF GS_VBRK,
      WA_VBRK TYPE GS_VBRK,
      GT_VBRP TYPE TABLE OF GS_VBRP,
      WA_VBRP TYPE GS_VBRP,
      GT_VBAK TYPE TABLE OF GS_VBAK,
      WA_VBAK TYPE GS_VBAK,
      GT_LIKP TYPE TABLE OF GS_LIKP,
      WA_LIKP TYPE GS_LIKP,
      GT_T001W TYPE TABLE OF GS_T001W,
      WA_T001W TYPE GS_T001W,
      GT_KNA1 TYPE TABLE OF GS_KNA1,
      WA_KNA1 TYPE GS_KNA1,
      GT_T007A TYPE TABLE OF GS_T007A,
      WA_T007A TYPE GS_T007A,
      GT_T685A TYPE TABLE OF GS_T685A,
      WA_T685A TYPE GS_T685A,
      GT_KONV TYPE TABLE OF GS_KONV,
      WA_KONV TYPE GS_KONV,
      GT_FINAL TYPE TABLE OF GS_FINAL,
      WA_FINAL TYPE GS_FINAL,
      GT_MSEG TYPE TABLE OF GS_MSEG,
      WA_MSEG TYPE GS_MSEG,
      GT_SER03 TYPE TABLE OF GS_SER03,
      WA_SER03 TYPE GS_SER03,
      GT_MARA TYPE TABLE OF GS_MARA,                      " ADDED BY RAM ON 29/09/2014
      WA_MARA TYPE GS_MARA,
      GT_TVFKT TYPE TABLE OF GS_TVFKT,
      WA_TVFKT TYPE GS_TVFKT,
      GT_OBJK TYPE TABLE OF GS_OBJK,
      WA_OBJK TYPE GS_OBJK,

      GT_TSPAT TYPE TABLE OF GS_TSPAT,
      WA_TSPAT TYPE GS_TSPAT.


DATA : OR_BUKRS TYPE VBRK-BUKRS,

       OR_VBELN TYPE VBRK-VBELN,           " Added by savariar on 29/09/2014.
       OR_WERKS TYPE T001W-WERKS,
       OR_FKDAT TYPE VBRK-FKDAT,          " Billing Date
       OR_VTWEG TYPE VBRK-VTWEG,          " Distribution channel
       OR_SPART TYPE VBRK-SPART,          " Division
       OR_VKBUR TYPE VBRP-VKBUR.          " Sales Office

*       OR_AUBEL TYPE VBRP-AUBEL,
*       OR_FKDAT TYPE VBRK-FKDAT.         " Modify by savari

DATA: T_VALUE TYPE P DECIMALS 2,
      N_VALUE TYPE P DECIMALS 2,
      FLAG TYPE C.

DATA : STR_FILE TYPE STRING,                            "Added by savariar s
       V_BUTXT TYPE T001-BUTXT,
       V_NAME1 TYPE T001W-NAME1.



DATA: LS_VARIANT TYPE DISVARIANT.
DATA: LV_CUR TYPE KONV-KKURS.

*&---------------------------------------------------------------------*
*&  ALV Structure & Internal Table
*&---------------------------------------------------------------------*

DATA: GT_FCAT TYPE SLIS_T_FIELDCAT_ALV,
      WA_FCAT TYPE SLIS_FIELDCAT_ALV,
      V_LAYOUT TYPE SLIS_LAYOUT_ALV,
      GT_EVENTS TYPE SLIS_T_EVENT,
      WA_EVENTS TYPE SLIS_ALV_EVENT.

DATA : LAYOUT TYPE SLIS_LAYOUT_ALV.


*************************************************************************************
******           selection-screen                                            ********                                           *****
*************************************************************************************

SELECTION-SCREEN : BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS: LV_BUKRS FOR OR_BUKRS OBLIGATORY,
                LV_WERKS FOR OR_WERKS,
                LV_VBELN FOR OR_VBELN,
                LV_FKDAT FOR OR_FKDAT OBLIGATORY,
                LV_VTWEG FOR OR_VTWEG,
                LV_SPART FOR OR_SPART,
                LV_VKBUR FOR OR_VKBUR OBLIGATORY.
*
*                LV_AUBEL FOR OR_AUBEL,
*                LV_FKDAT FOR OR_FKDAT.
PARAMETERS: P_CB AS CHECKBOX USER-COMMAND CBC MODIF ID TB2.
PARAMETERS: P_SR AS CHECKBOX USER-COMMAND CBC MODIF ID TB2.

SELECTION-SCREEN : END OF BLOCK B1.

*************************************************************************************
********** START-OF-SELECTION .
*************************************************************************************

START-OF-SELECTION.
  PERFORM GET_DATA.

*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA .

  SELECT "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
    VBELN
    POSNR
    WERKS
    AUBEL
    VKBUR
    VGBEL
    VGPOS
    MATNR
    ARKTX
    FKIMG
    MEINS
    PRCTR
    KURSK FROM VBRP INTO TABLE GT_VBRP
    WHERE WERKS IN LV_WERKS AND VBELN IN LV_VBELN AND VKBUR IN LV_VKBUR.

  IF GT_VBRP[] IS NOT INITIAL .

    IF P_CB NE 'X'.

      SELECT
        VBELN
        VTWEG
        FKDAT
        FKART
        KUNRG
        KNUMV
        FKART
        WAERK
        BUKRS
        GJAHR
        NETWR
        SPART FROM VBRK INTO CORRESPONDING FIELDS OF TABLE GT_VBRK FOR ALL ENTRIES IN GT_VBRP
        WHERE  VBELN = GT_VBRP-VBELN AND
               FKDAT IN LV_FKDAT AND BUKRS IN LV_BUKRS AND SPART IN LV_SPART AND VTWEG IN LV_VTWEG AND
               FKART NOT IN ('S1', 'YBRE', 'ZS1','S3','S4' )
              AND  FKSTO NE 'X'.
    ELSEIF P_CB EQ 'X'.
      SELECT
         VBELN
         VTWEG
         FKDAT
         FKART
         KUNRG
         KNUMV
         FKART
         WAERK
         BUKRS
         GJAHR
         NETWR
         SPART FROM VBRK INTO CORRESPONDING FIELDS OF TABLE GT_VBRK FOR ALL ENTRIES IN GT_VBRP
         WHERE  VBELN = GT_VBRP-VBELN AND
                FKDAT IN LV_FKDAT AND BUKRS IN LV_BUKRS AND SPART IN LV_SPART AND VTWEG IN LV_VTWEG AND
                FKSTO EQ 'X' .
    ENDIF.
  ENDIF.
  IF P_SR EQ 'X'.
*
    SELECT
    VBELN
    VTWEG
    FKDAT
    FKART
    KUNRG
    KNUMV
    FKART
    WAERK
    BUKRS
    GJAHR
    NETWR
    SPART FROM VBRK INTO CORRESPONDING FIELDS OF TABLE GT_VBRK FOR ALL ENTRIES IN GT_VBRP
    WHERE  VBELN = GT_VBRP-VBELN AND
           FKDAT IN LV_FKDAT AND BUKRS IN LV_BUKRS AND SPART IN LV_SPART AND VTWEG IN LV_VTWEG AND
           FKART EQ 'YBRE'  .
  ENDIF.

  IF GT_VBRK[] IS NOT INITIAL.
    SELECT
      KNUMV
      KPOSN
      KSCHL
      KWERT
      KBETR
      KAWRT
      KUNNR
      MWSK1
      KAPPL
      WAERS
      KKURS
      FROM PRCD_ELEMENTS INTO TABLE GT_KONV FOR ALL ENTRIES IN GT_VBRK
      WHERE KNUMV = GT_VBRK-KNUMV AND
            KSCHL IN  ('PR00', 'PR01', 'ZS00', 'ZPF1', 'Y004', 'Y007', 'ZEXP', 'ZECS', 'JHCS','ZHCS', 'JIVC', 'ZINS', 'ZSTO', 'JHCT', 'ZINS', 'VPRS', 'JVSR',
                'YBSD', 'Y029', 'Y007', 'YBRD',
               'ZKFR', 'JIVP', 'JADD', 'ZTCS', 'ZSBS', 'JSVD', 'JEC3', 'JEC4', 'ZAED' , 'YBAD' ) AND
            KINAK = ' '.

    SELECT
      KUNNR
      NAME1 FROM KNA1 INTO  TABLE GT_KNA1 FOR ALL ENTRIES IN GT_VBRK
      WHERE KUNNR = GT_VBRK-KUNRG.


    SELECT
   FKART
   VTEXT FROM TVFKT INTO  TABLE GT_TVFKT FOR ALL ENTRIES IN  GT_VBRK                  " ADDED BY RAM ON 29/09/2014
   WHERE FKART = GT_VBRK-FKART.

    SELECT
      SPART
      VTEXT FROM TSPAT INTO TABLE GT_TSPAT FOR ALL ENTRIES IN GT_VBRK
      WHERE SPART = GT_VBRK-SPART.

  ENDIF.

  IF GT_VBRP[] IS NOT INITIAL .
    SELECT
      VBELN
      AUDAT
      KUNNR FROM VBAK INTO  TABLE GT_VBAK FOR ALL ENTRIES IN  GT_VBRP
      WHERE VBELN = GT_VBRP-AUBEL.

    SELECT
      VBELN
      LFDAT
      KUNNR FROM LIKP INTO TABLE GT_LIKP FOR ALL ENTRIES IN GT_VBRP
      WHERE VBELN = GT_VBRP-VGBEL.

    SELECT
    MATNR
    VOLUM
    VOLEH FROM MARA INTO  TABLE GT_MARA FOR ALL ENTRIES IN  GT_VBRP                   " ADDED BY RAM ON 29/09/2014
    WHERE MATNR = GT_VBRP-MATNR.
  ENDIF.


  SORT GT_MSEG BY MBLNR.
  DELETE ADJACENT DUPLICATES FROM GT_MSEG COMPARING MBLNR.
*  ENDIF.


  LOOP AT GT_VBRP INTO WA_VBRP.

    READ TABLE GT_VBRK INTO WA_VBRK WITH KEY VBELN = WA_VBRP-VBELN.

    IF SY-SUBRC = 0.
* Price
      LOOP AT GT_KONV INTO WA_KONV WHERE KNUMV = WA_VBRK-KNUMV AND KPOSN = WA_VBRP-POSNR .
        CASE WA_KONV-KSCHL.
          WHEN 'PR00' OR 'PR01'.
            WA_KONV-KSCHL = 'PR01'.
            WA_FINAL-PRICE = WA_KONV-KBETR.
            WA_FINAL-VALUE = WA_KONV-KWERT.
* Cash Discount
          WHEN 'Y004'.
            WA_FINAL-DISC = WA_FINAL-DISC + WA_KONV-KWERT.
* Customer rebate
          WHEN 'Y007'.
            WA_FINAL-CRBT = WA_FINAL-CRBT + WA_KONV-KWERT.
* Excise Duty

          WHEN 'ZEXP' OR 'ZECS' OR 'JA1X' OR 'ZHCS'.
            WA_FINAL-EX_DUTY = WA_FINAL-EX_DUTY +  WA_KONV-KWERT.
* VAT Rate
          WHEN 'JIVP'.
            WA_FINAL-VAT = WA_FINAL-VAT + WA_KONV-KWERT.
* Additional VAT Rate
          WHEN 'JVSR'.
            WA_FINAL-AVAT = WA_FINAL-AVAT + WA_KONV-KWERT.
* Special Discount
          WHEN 'YBSD'.
            WA_FINAL-YBSD = WA_FINAL-YBSD + WA_KONV-KWERT.
* Product Discount
          WHEN  'Y029'.
            WA_FINAL-Y029 = WA_FINAL-Y029 + WA_KONV-KWERT.
*  Special Freight Discount
          WHEN 'YBRD'.
            WA_FINAL-YBRD = WA_FINAL-YBRD + WA_KONV-KWERT.
*   Additonal Discount
          WHEN 'YBAD'.
            WA_FINAL-ADISC = WA_FINAL-ADISC + WA_KONV-KWERT.
* CST Rate
          WHEN 'JIVC'.
            WA_FINAL-CST = WA_FINAL-CST + WA_KONV-KWERT.
          WHEN 'JTCS'.
            WA_FINAL-TCSTDS = WA_FINAL-TCSTDS + WA_KONV-KWERT.
          WHEN 'ZEXP'.
            WA_FINAL-BED = WA_FINAL-BED + WA_KONV-KWERT.
          WHEN 'ZHCS'.
            WA_FINAL-ECESS = WA_FINAL-ECESS + WA_KONV-KWERT.
          WHEN 'ZECS'.
            WA_FINAL-SHECSS = WA_FINAL-SHECSS + WA_KONV-KWERT.
        ENDCASE.

      ENDLOOP.

      READ TABLE GT_KNA1 INTO WA_KNA1 WITH KEY KUNNR = WA_VBRK-KUNRG BINARY SEARCH. "#EC CI_SORTED " Added by <IT-CAR Tool> during Code Remediation
      IF SY-SUBRC = 0.
        WA_FINAL-NAME1 = WA_KNA1-NAME1.
      ENDIF.

      READ TABLE GT_TVFKT INTO WA_TVFKT WITH  KEY FKART = WA_VBRK-FKART BINARY SEARCH.  "#EC CI_SORTED " ADDED BY RAM ON 29/09/2014Added by <IT-CAR Tool> during Code Remediation
      IF SY-SUBRC = 0.
        WA_FINAL-VTEXT = WA_TVFKT-VTEXT.
      ENDIF.
      READ TABLE GT_VBAK INTO WA_VBAK WITH  KEY VBELN = WA_VBRP-AUBEL BINARY SEARCH. "#EC CI_SORTED " Added by <IT-CAR Tool> during Code Remediation
      IF SY-SUBRC = 0.
        WA_FINAL-AUDAT = WA_VBAK-AUDAT.
      ENDIF.
      READ TABLE GT_LIKP INTO WA_LIKP WITH  KEY VBELN = WA_VBRP-VGBEL BINARY SEARCH. "#EC CI_SORTED " Added by <IT-CAR Tool> during Code Remediation
      IF SY-SUBRC = 0.
        WA_FINAL-VGBEL = WA_LIKP-VBELN.
        WA_FINAL-LFDAT = WA_LIKP-LFDAT.
      ENDIF.
      WA_FINAL-WERKS = WA_VBRP-WERKS.
      WA_FINAL-MATNR = WA_VBRP-MATNR.
      WA_FINAL-ARKTX = WA_VBRP-ARKTX.
      WA_FINAL-FKIMG = WA_VBRP-FKIMG.
      WA_FINAL-MEINS = WA_VBRP-MEINS.
      WA_FINAL-PRCTR = WA_VBRP-PRCTR.

      WA_FINAL-KUNRG = WA_VBRK-KUNRG.
      WA_FINAL-VBELN = WA_VBRK-VBELN.
      WA_FINAL-FKDAT = WA_VBRK-FKDAT.
      WA_FINAL-FKART = WA_VBRK-FKART.
      WA_FINAL-AUBEL = WA_VBRP-AUBEL.
      WA_FINAL-VKBUR = WA_VBRP-VKBUR.
      WA_FINAL-SPART = WA_VBRK-SPART.
      WA_FINAL-NETWR = WA_VBRK-NETWR.
      WA_FINAL-VTWEG = WA_VBRK-VTWEG.     "Added by savariar on 09/10/2014.
      APPEND WA_FINAL TO GT_FINAL.
      CLEAR: WA_FINAL,WA_VBRP,WA_VBRK,WA_KONV,WA_KNA1,WA_LIKP,WA_VBAK,N_VALUE,T_VALUE,WA_MSEG.
    ENDIF.
  ENDLOOP.

  LOOP AT GT_FINAL INTO WA_FINAL.
    READ TABLE GT_MARA INTO WA_MARA WITH  KEY MATNR = WA_FINAL-MATNR BINARY SEARCH. "#EC CI_SORTED " Added by <IT-CAR Tool> during Code Remediation
    IF SY-SUBRC = 0.
      WA_FINAL-VOLUM = WA_MARA-VOLUM.
      WA_FINAL-VOLEH = WA_MARA-VOLEH.
    ENDIF.
*    WA_FINAL-INV_AMT  =  WA_FINAL-TOT_INV_AMT - WA_FINAL-CRBT.
    WA_FINAL-TOT_LIT = WA_MARA-VOLUM * WA_FINAL-FKIMG.
    READ TABLE GT_TSPAT INTO WA_TSPAT WITH KEY SPART = WA_FINAL-SPART BINARY SEARCH.   "#EC CI_SORTED "Added by savariar s on 08/10/2014.Added by <IT-CAR Tool> during Code Remediation
    WA_FINAL-DTEXT = WA_TSPAT-VTEXT."DISC + CRBT + YBSD + Y029 + Y007 + YBRD + ADISC + BED + ECESS + SHECSS + EX_DUTY + TCSTDS
    WA_FINAL-BTAX_AMT = WA_FINAL-VALUE + WA_FINAL-DISC + WA_FINAL-CRBT + WA_FINAL-YBSD + WA_FINAL-Y029 + WA_FINAL-YBRD + WA_FINAL-ADISC + WA_FINAL-BED + WA_FINAL-ECESS + WA_FINAL-SHECSS .
    WA_FINAL-TTAX_AMT = WA_FINAL-CST + WA_FINAL-VAT + WA_FINAL-AVAT.   "Added by savariar on 08/10/2014
    MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING  VOLUM VOLEH  TOT_LIT DTEXT TTAX_AMT BTAX_AMT.
    CLEAR WA_FINAL.
  ENDLOOP.

  LOOP AT GT_FINAL INTO WA_FINAL.
    WA_FINAL-TOT_INV_AMT = WA_FINAL-BTAX_AMT +  WA_FINAL-TTAX_AMT  .
    MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING TOT_INV_AMT  .
    CLEAR WA_FINAL.
  ENDLOOP.

  SORT GT_FINAL BY KUNRG.
  DELETE GT_FINAL WHERE FKIMG = 0.
ENDFORM.                    " GET_DATA




*************************************************************************************
********** Display List Using The ALV Grid.
*************************************************************************************

END-OF-SELECTION.

  PERFORM ALV_LAYOUT USING 1 'Sales Office' 'VKBUR' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 2 'Plant' 'WERKS' 'GT_FINAL' '' .
  PERFORM ALV_LAYOUT USING 3 'Distribution Channel' 'VTWEG' 'GT_FINAL' ''.   "Added by savariar s on 09/10/2014.
  PERFORM ALV_LAYOUT USING 4 'Division' 'SPART' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 5 'Division Text' 'DTEXT' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 6 'Billing Type' 'FKART' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 7 'Billing Desc' 'VTEXT' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 8 'Billing Doc' 'VBELN' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 9 'Billing Date' 'FKDAT' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 10 'Sales order No.' 'AUBEL' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 11 'Delivery Challan No' 'VGBEL' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 12'Delivery Challan Date' 'LFDAT' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 13 'Customer Code.' 'KUNRG ' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 14 'Customer Name' 'NAME1' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 15 'Material Code' 'MATNR' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 16 'Material Desc' 'ARKTX' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 17 'Billing QTY' 'FKIMG' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 18 'UOM' 'MEINS' 'GT_FINAL'  ''.
  PERFORM ALV_LAYOUT USING 19 'Base Value' 'VALUE' 'GT_FINAL' '' .
  PERFORM ALV_LAYOUT USING 20 'Volume' 'VOLUM' 'GT_FINAL' '' .
  PERFORM ALV_LAYOUT USING 21  'Total in Ltr' 'TOT_LIT' 'GT_FINAL' '' .
  PERFORM ALV_LAYOUT USING 22 'Volume L/G' 'VOLEH' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 23 'Cash Discount' 'DISC' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 24 'Customer Rebate' 'CRBT' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 25 'Special Discount' 'YBSD' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 26 'Product Discount' 'Y029' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 27 'Turnover Rebate Discount' 'Y007' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 28 'Special Freight Discount' 'YBRD' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 29 'Additonal Discount' 'ADISC' 'GT_FINAL' ''  .
  PERFORM ALV_LAYOUT USING 30 'BED' 'BED' 'GT_FINAL' '' .
  PERFORM ALV_LAYOUT USING 31 'ECESS' 'ECESS' 'GT_FINAL'  ''.
  PERFORM ALV_LAYOUT USING 32 'SHECSS' 'SHECSS' 'GT_FINAL'  ''.
  PERFORM ALV_LAYOUT USING 33 'CST  Amount' 'CST' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 34 'VAT  Amount' 'VAT' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 35 'Add.VAT  Amount' 'AVAT' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 36 'Excise Duty' 'EX_DUTY' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 37 'TOTAL_TAX_AMOUNT' 'TTAX_AMT' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 38 'Before Tax_Amt' 'BTAX_AMT' 'GT_FINAL' ''.
  PERFORM ALV_LAYOUT USING 39 'Total Invoice Amount' 'TOT_INV_AMT' 'GT_FINAL' '' .
  PERFORM ALV_LAYOUT USING 40 'Profit Center' 'PRCTR' 'GT_FINAL' ''.

*&---------------------------------------------------------------------*
*&     ALV Grid Display
*&---------------------------------------------------------------------*

  LS_VARIANT-REPORT = SY-REPID.
  PERFORM ALV_GRID_DISPLAY.

*&---------------------------------------------------------------------*
*&      Form  ALV_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P1  text
*      -->P_P2  text
*      -->P_P3  text
*      -->P_P4  text
*      -->P_P5  text
*----------------------------------------------------------------------*
FORM ALV_LAYOUT  USING    P1 P2 P3 P4 P5.
  CLEAR WA_FCAT.
  WA_FCAT-COL_POS = P1.
  WA_FCAT-SELTEXT_L = P2.
  WA_FCAT-FIELDNAME = P3.
  WA_FCAT-TABNAME = P4.
  WA_FCAT-DO_SUM = P5.
  APPEND WA_FCAT TO GT_FCAT.


ENDFORM.                    " ALV_LAYOUT


*&---------------------------------------------------------------------*
*&      Form  ALV_GRID_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ALV_GRID_DISPLAY .

   LAYOUT-COLWIDTH_OPTIMIZE = 'X'.                                  "Added by S.Savariar as on 20/10/2014.
   LAYOUT-ZEBRA = 'X'.

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
    IS_LAYOUT                         = LAYOUT
     IT_FIELDCAT                       = GT_FCAT[]
*   IT_EXCLUDING                      =
*   IT_SPECIAL_GROUPS                 =
*   IT_SORT                           =
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
      T_OUTTAB                          = GT_FINAL[]
* EXCEPTIONS
*   PROGRAM_ERROR                     = 1
*   OTHERS                            = 2
            .
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " ALV_GRID_DISPLAY

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
         RV_BUKRS(100) TYPE C,
          RV_FKDAT(100) TYPE C.

  CLEAR : RV_BUKRS,
         RV_FKDAT.

  DATA :  V_DATE(10) .

  CONCATENATE SY-DATUM+6(2) '.' SY-DATUM+4(2) '.' SY-DATUM+0(4) INTO V_DATE.   "Added by savariar s on 08/10/2014



  IF LV_BUKRS-HIGH IS NOT INITIAL.
    CONCATENATE '' LV_BUKRS-LOW 'To' LV_BUKRS-HIGH INTO RV_BUKRS SEPARATED BY SPACE.
  ELSE.
    CONCATENATE '' LV_BUKRS-LOW INTO RV_BUKRS SEPARATED BY SPACE.
  ENDIF.


*  CLEAR LS_LINE.
  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY = 'Company Code : '.
  LS_LINE-INFO = RV_BUKRS.
  APPEND LS_LINE TO LIT_HEADER.
  CLEAR LS_LINE.


  LS_LINE-TYP = 'S'.
  LS_LINE-KEY = 'Date : '.
  LS_LINE-INFO = V_DATE.
  APPEND LS_LINE TO LIT_HEADER.

  CLEAR LS_LINE.
  LS_LINE-TYP  = 'H'.
  LS_LINE-KEY = ' '.
  LS_LINE-INFO = 'Sales Register' .
  APPEND LS_LINE TO LIT_HEADER.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = LIT_HEADER
      I_LOGO             = 'ZLOGO'.


ENDFORM.                    "ALV_CATALOG_HEADER
