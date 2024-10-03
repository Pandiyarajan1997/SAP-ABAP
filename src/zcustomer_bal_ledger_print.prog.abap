**&---------------------------------------------------------------------*
**& Report  ZCUST_BAL_LEDGER_PRINT3
**&
**&---------------------------------------------------------------------*
**&
**&
**&---------------------------------------------------------------------*
*
REPORT ZCUSTOMER_BAL_LEDGER_PRINT.


TABLES:BSID.

DATA : TOTAL TYPE CHAR20. " created by sri

TYPES: BEGIN OF GS_KNA1,
       KUNNR TYPE KNA1-KUNNR,                  " Customer Number
       NAME1 TYPE KNA1-NAME1,                  " Customer Name
       ADRNR TYPE KNA1-ADRNR,                  " Address Number
       LIFNR TYPE KNA1-LIFNR,                  " Vendor Code " Added on 17-10-2014  By Govind
       END OF GS_KNA1.

DATA: GT_KNA1 TYPE TABLE OF GS_KNA1,     "test
      WA_KNA1 TYPE GS_KNA1.
TYPES : BEGIN OF GS_T003T,
       BLART TYPE T003T-BLART,
       LTEXT TYPE T003T-LTEXT,
       SPRAS TYPE T003T-SPRAS,
       END OF GS_T003T.

DATA : GT_T003T TYPE  TABLE OF GS_T003T,
       WA_T003T TYPE GS_T003T.
*********************** Added on 17-10-2014  By Govind ********************************
DATA : GT_T003T1 TYPE  TABLE OF GS_T003T,
       WA_T003T1 TYPE GS_T003T.

DATA : GT_T003T2 TYPE  TABLE OF GS_T003T,
       WA_T003T2 TYPE GS_T003T.
*********************** Added on 17-10-2014  By Govind ********************************

TYPES: BEGIN OF GS_BSID,
       PRCTR TYPE BSID-PRCTR,                  " Profit Center
       KUNNR TYPE BSID-KUNNR,                  " Customer Number
       AUGBL TYPE BSID-AUGBL,
       BUDAT TYPE BSID-BUDAT,                  " Posting Date in the Document
       BLDAT TYPE BSID-BLDAT,
       BLART TYPE BSID-BLART,
       ZFBDT TYPE BSID-ZFBDT,                  " Baseline Date for Due Date Calculation
       DMBTR TYPE BSID-DMBTR,                  " Amount in Local Currency
       SHKZG TYPE BSID-SHKZG,                  " Debit/Credit Indicator
       XBLNR TYPE BSID-XBLNR,                  " Reference Document Number
       BELNR TYPE BSID-BELNR,                  " Bill Number
       GSBER TYPE BSID-GSBER,                  " Business Area
       SGTXT TYPE BSID-SGTXT,                  " Text Or Remarks
       VBELN TYPE BSID-VBELN,
       UMSKZ TYPE BSID-UMSKZ,
       BUKRS TYPE BSID-BUKRS,
       GJAHR TYPE BSID-GJAHR,
       SKNTO TYPE BSAD-SKNTO,
              END OF GS_BSID.

DATA: GT_BSID TYPE TABLE OF GS_BSID,
      GT_BSID1 TYPE TABLE OF GS_BSID,
      WA_BSID TYPE GS_BSID,
      WA_BSID1 TYPE GS_BSID.

*********************** Added on 17-10-2014 ********************************

TYPES : BEGIN OF GS_BSIK,

        PRCTR TYPE BSIK-PRCTR ,        "Profit Center
        KUNNR TYPE BSIK-LIFNR,         "Vendor
        AUGBL TYPE BSIK-AUGBL,         "Clrng doc.
        BUDAT TYPE BSIK-BUDAT,         "Posting Date
        BLDAT TYPE BSIK-BLDAT,         "Document Date
        BLART TYPE BSIK-BLART,         "Document Type
        ZFBDT TYPE BSIK-ZFBDT,         "Baseline Date
        DMBTR TYPE BSIK-DMBTR,         "Amount in LC
        SHKZG TYPE BSIK-SHKZG,         "Debit/Credit
        XBLNR TYPE BSIK-XBLNR,         "Reference
        BELNR TYPE BSIK-BELNR,         "Document Number
        GSBER TYPE BSIK-GSBER,         "Business Area
        SGTXT TYPE BSIK-SGTXT,         "Text
        VBELN TYPE BSIK-EBELN,         "Purchasing Doc.
        UMSKZ TYPE BSIK-UMSKZ,         "Special G/L ind
        BUKRS TYPE BSIK-BUKRS,         "Company Code
        GJAHR TYPE BSIK-GJAHR,         "Fiscal Year
  END OF GS_BSIK.


DATA: GT_BSIK TYPE TABLE OF GS_BSIK,
       WA_BSIK TYPE GS_BSIK.
DATA: GT_BSIK1 TYPE TABLE OF GS_BSIK,
       WA_BSIK1 TYPE GS_BSIK.

DATA: GT_BSIK2 TYPE TABLE OF GS_BSIK,
       WA_BSIK2 TYPE GS_BSIK.

DATA: GT_BSIK3 TYPE TABLE OF GS_BSIK,
       WA_BSIK3 TYPE GS_BSIK.

TYPES : BEGIN OF GS_BSAK,

        PRCTR TYPE BSAK-PRCTR ,        "Profit Center
        KUNNR TYPE BSAK-LIFNR,         "Vendor
        AUGBL TYPE BSAK-AUGBL,         "Clrng doc.
        BUDAT TYPE BSAK-BUDAT,         "Posting Date
        BLDAT TYPE BSAK-BLDAT,         "Document Date
        BLART TYPE BSAK-BLART,         "Document Type
        ZFBDT TYPE BSAK-ZFBDT,         "Baseline Date
        DMBTR TYPE BSAK-DMBTR,         "Amount in LC
        SHKZG TYPE BSAK-SHKZG,         "Debit/Credit
        XBLNR TYPE BSAK-XBLNR,         "Reference
        BELNR TYPE BSAK-BELNR,         "Document Number
        GSBER TYPE BSAK-GSBER,         "Business Area
        SGTXT TYPE BSAK-SGTXT,         "Text
        VBELN TYPE BSAK-EBELN,         "Purchasing Doc.
        UMSKZ TYPE BSAK-UMSKZ,         "Special G/L ind
        BUKRS TYPE BSAK-BUKRS,         "Company Code
        GJAHR TYPE BSAK-GJAHR,         "Fiscal Year
  END OF GS_BSAK.


DATA: GT_BSAK TYPE TABLE OF GS_BSAK,
       WA_BSAK TYPE GS_BSAK.

DATA: GT_BSAK1 TYPE TABLE OF GS_BSAK,
       WA_BSAK1 TYPE GS_BSAK.
DATA: GT_BSAK2 TYPE TABLE OF GS_BSAK,
       WA_BSAK2 TYPE GS_BSAK.

DATA: GT_BSAK3 TYPE TABLE OF GS_BSAK,
       WA_BSAK3 TYPE GS_BSAK.


*********************** Added on 17-10-2014  By Govind ********************************


TYPES: BEGIN OF GS_BSAD,
       PRCTR TYPE BSAD-PRCTR,                  " Profit Center
       KUNNR TYPE BSAD-KUNNR,                  " Customer Number
       AUGBL TYPE BSAD-AUGBL,
       BUDAT TYPE BSAD-BUDAT,                  " Posting Date in the Document
       BLDAT TYPE BSAD-BLDAT,
       BLART TYPE BSAD-BLART,
       ZFBDT TYPE BSAD-ZFBDT,                  " Baseline Date for Due Date Calculation
       DMBTR TYPE BSAD-DMBTR,                  " Amount in Local Currency
       SHKZG TYPE BSAD-SHKZG,                  " Debit/Credit Indicator
       XBLNR TYPE BSAD-XBLNR,                  " Reference Document Number
       BELNR TYPE BSAD-BELNR,                  " Bill Number
       GSBER TYPE BSAD-GSBER,                  " Business Area
       SGTXT TYPE BSAD-SGTXT,                  " Text OR Remarks
       VBELN TYPE BSAD-VBELN,                  " Billing Doc.No
       UMSKZ TYPE BSAD-UMSKZ,                  " Spl.Gl.Ind
       BUKRS TYPE BSAD-BUKRS,
       GJAHR TYPE BSAD-GJAHR,
       SKNTO TYPE BSAD-SKNTO ,
       UMSKS type bsad-UMSKS,
       AUGDT type bsad-AUGDT,
       ZUONR type bsad-ZUONR,
       BUZEI type bsad-BUZEI,
       END OF GS_BSAD.

DATA: GT_BSAD TYPE TABLE OF GS_BSAD,
      WA_BSAD TYPE GS_BSAD,
      WA_BSAD1 TYPE GS_BSAD,
      GT_BSAD1 TYPE TABLE OF GS_BSAD,
      GT_BSAD2 TYPE TABLE OF GS_BSAD.

TYPES: BEGIN OF GS_ADRC,
       ADDRNUMBER TYPE ADRC-ADDRNUMBER,        " Address Number
       STREET TYPE ADRC-STREET,                " Street
       CITY1 TYPE ADRC-CITY1,                  " District
       CITY2 TYPE ADRC-CITY2,                  " City
       POST_CODE1 TYPE ADRC-POST_CODE1,        " Postal Code
       END OF GS_ADRC.

DATA: GT_ADRC TYPE TABLE OF GS_ADRC WITH HEADER LINE,
      WA_ADRC TYPE GS_ADRC.

TYPES: BEGIN OF GS_FINAL,
       KUNNR TYPE BSAD-KUNNR,                  " Customer Number
       UMSKZ TYPE BSID-UMSKZ,                  " Spl.GL Ind
       BELNR TYPE BSAD-BELNR,                  " Bill Number
       VBELN TYPE BSAD-VBELN,                  " Billing Document Number
       XBLNR TYPE BSID-XBLNR,                  " Reference Document Number
       BLDAT TYPE BSAD-BLDAT,                  " Posting Date in the Document
       CR_AMT TYPE BSAD-DMBTR,                 " Cr Amount
       DR_AMT TYPE BSAD-DMBTR,                 " Dr Amount
       BUDAT  TYPE BSID-BUDAT,                 " Posting Date
       NAME1  TYPE KNA1-NAME1,                 " Customer Name
       BLART TYPE BSID-BLART,                  " Bill Doc. Type
       SGTXT TYPE BSID-SGTXT,                  " Text OR Remarks
       GSBER TYPE BSID-GSBER,                  " Business Area
       SHKZG TYPE BSID-SHKZG,                  " Dbit/Crdit ind.
       BAL_AMT TYPE BSAD-DMBTR,                " Balance Amount
       LTEXT TYPE T003T-LTEXT,                  " Bill Doc .Type Descrption
       BUKRS TYPE BSID-BUKRS ,
       GJAHR TYPE BSID-GJAHR,
       SKNTO TYPE BSAD-SKNTO ,
       AUGBL TYPE BSAD-AUGBL,
*       RUN_BAL TYPE BSAD-DMBTR,                " Balance Amount


       END OF GS_FINAL.

DATA: GT_FINAL TYPE TABLE OF GS_FINAL WITH HEADER LINE,
        WA_FINAL TYPE GS_FINAL.

TYPES: BEGIN OF GS_FAGLFLEXA,
       DOCNR TYPE FAGLFLEXA-DOCNR,
       PRCTR TYPE FAGLFLEXA-PRCTR,
       BSCHL TYPE FAGLFLEXA-BSCHL,
       END OF GS_FAGLFLEXA.

DATA: GT_FAGLFLEXA TYPE TABLE OF GS_FAGLFLEXA,
      WA_FAGLFLEXA TYPE GS_FAGLFLEXA.

TYPES : BEGIN OF GS_KNC1,
        KUNNR TYPE KNC1-KUNNR,
        BUKRS TYPE KNC1-BUKRS,
        UMSAV TYPE KNC1-UMSAV,
        GJAHR TYPE KNC1-GJAHR,
        END OF GS_KNC1.

DATA: GT_KNC1 TYPE TABLE OF GS_KNC1,
      WA_KNC1 TYPE GS_KNC1.

TYPES : BEGIN OF GS_LFA1,
        LIFNR TYPE LFA1-LIFNR,
*        BUKRS TYPE LFB1-BUKRS,
        KUNNR TYPE LFA1-KUNNR,
        END OF GS_LFA1.

DATA : GT_LFA1 TYPE TABLE OF GS_LFA1,
       WA_LFA1 TYPE GS_LFA1.


DATA: OR_BUDAT TYPE  BSAD-BUDAT,
      OR_BUKRS TYPE BSID-BUKRS,
      OR_KUNNR TYPE KNA1-KUNNR,
      FM_NAME TYPE RS38L_FNAM,
      OR_PRCTR TYPE CEPC-PRCTR,
      OR_UMSKZ TYPE BSID-UMSKZ.

DATA: L_KUNNR TYPE KNA1-KUNNR,
      L_PRCTR TYPE CEPC-PRCTR.

DATA: B_DATE TYPE SY-DATUM.


DATA: LV_OPN TYPE BSAD-DMBTR,
      RV_OPN TYPE BSAD-DMBTR,
      LV_TOTAL TYPE BSAD-DMBTR,
      LV_TRANS TYPE BSAD-DMBTR,
      LV_DATE TYPE SY-DATUM,
      LV_FRDAT TYPE SY-DATUM,
      LV_TODAT TYPE SY-DATUM,
      LV_SUM TYPE BSAD-DMBTR,
      LV_BUK TYPE T001-BUKRS.


*&---------------------------------------------------------------------*
*&  ALV Structure & Internal Table
*&---------------------------------------------------------------------*

DATA: GT_FCAT TYPE SLIS_T_FIELDCAT_ALV,
      WA_FCAT TYPE SLIS_FIELDCAT_ALV,
      IT_LAYOUT TYPE SLIS_LAYOUT_ALV ,
            GT_EVENTS TYPE SLIS_T_EVENT,
      WA_EVENTS TYPE SLIS_ALV_EVENT,
      KEY TYPE SLIS_KEYINFO_ALV,
       GT_HEADER TYPE SLIS_T_LISTHEADER,
WA_HEADER TYPE SLIS_LISTHEADER.

DATA: GT_SORT TYPE SLIS_T_SORTINFO_ALV,
      WA_SORT TYPE SLIS_SORTINFO_ALV.

DATA: LS_VARIANT TYPE DISVARIANT.
LS_VARIANT-REPORT = SY-REPID.




DATA:IN_WORDS TYPE SPELL,
      CURREN TYPE SY-WAERS.


********** Graph*****************

DATA: GT_GRAPH       TYPE TABLE OF GPRVAL WITH HEADER LINE,
X_TEXTS        TYPE TABLE OF GPRTXT WITH HEADER LINE.


*&---------------------------------------------------------------------*
*&  Selection Screen Fields
*&---------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:  SO_KUNNR FOR OR_KUNNR NO INTERVALS NO-EXTENSION OBLIGATORY,
                 SO_BUKRS FOR OR_BUKRS NO INTERVALS NO-EXTENSION OBLIGATORY,
                 SO_PRCTR FOR OR_PRCTR .

SELECTION-SCREEN: END OF BLOCK B1.

SELECTION-SCREEN: BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
SELECT-OPTIONS: SO_BUDAT FOR OR_BUDAT ,
                SO_UMSKZ FOR OR_UMSKZ .
*PARAMETERS :    P_BUDAT TYPE BSAD-BUDAT  .

SELECTION-SCREEN: END OF BLOCK B2.

SELECTION-SCREEN: BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-003.
PARAMETERS : P_VN AS CHECKBOX.
PARAMETERS : P_SD AS CHECKBOX  .

SELECTION-SCREEN: END OF BLOCK B3.

INITIALIZATION.

  SO_BUDAT-LOW = '20140401'.
  SO_BUDAT-HIGH = '20150331'.
  SO_BUDAT-OPTION = 'BT'.
  SO_BUDAT-SIGN = 'I'.
  APPEND SO_BUDAT.


AT SELECTION-SCREEN .
  IF SO_PRCTR IS NOT INITIAL.
    SELECT SINGLE PRCTR FROM CEPC INTO L_PRCTR WHERE PRCTR IN SO_PRCTR .
    IF SY-SUBRC NE 0.
      MESSAGE 'Enter Valid Profit Center' TYPE 'E'.
    ENDIF.
  ENDIF.
  IF SO_KUNNR IS NOT INITIAL.
    SELECT SINGLE KUNNR FROM KNA1 INTO L_KUNNR WHERE KUNNR IN SO_KUNNR .
    IF SY-SUBRC NE 0.
      MESSAGE 'Enter Valid Customer Number' TYPE 'E'.
    ENDIF.
  ENDIF.


*&---------------------------------------------------------------------*
*&  Main Logic
*&---------------------------------------------------------------------*
  LV_DATE = SO_BUDAT-HIGH  + 1.

START-OF-SELECTION.



  PERFORM FATCH_DATA1.
  PERFORM READ_DATA1.
  PERFORM PRINT_FORMATFORM.
  PERFORM TOP-OF-PAGE.





*&---------------------------------------------------------------------*
*&      Form  FATCH_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FATCH_DATA1 .

  SELECT "#EC CI_NOORDER " Added by <IT-CAR Tool> during Code Remediation
    BUKRS
       FROM T001 INTO LV_BUK WHERE BUKRS IN SO_BUKRS .

  ENDSELECT .

  SELECT
     DOCNR
     PRCTR
     BSCHL FROM FAGLFLEXA INTO CORRESPONDING FIELDS OF TABLE GT_FAGLFLEXA
     WHERE PRCTR IN SO_PRCTR.

  SORT GT_FAGLFLEXA BY DOCNR.
  DELETE ADJACENT DUPLICATES FROM GT_FAGLFLEXA COMPARING DOCNR.

  IF P_SD NE 'X'.
    IF GT_FAGLFLEXA[] IS NOT INITIAL.

      SELECT
        PRCTR
        KUNNR
        UMSKZ
        BUDAT
        BLDAT
        BLART
        ZFBDT
        DMBTR
        SHKZG
        XBLNR
        BELNR
        GSBER
        SGTXT
        VBELN
        BUKRS
        GJAHR
        SKNTO
         FROM BSID INTO CORRESPONDING FIELDS OF TABLE GT_BSID FOR ALL ENTRIES IN GT_FAGLFLEXA
        WHERE BELNR = GT_FAGLFLEXA-DOCNR AND BUDAT IN SO_BUDAT
           AND BUKRS IN SO_BUKRS AND
              KUNNR IN SO_KUNNR AND UMSKZ IN SO_UMSKZ AND UMSKZ <> 'H'.



      SELECT
     PRCTR
     KUNNR
     UMSKZ
     BUDAT
     BLDAT
     BLART
     ZFBDT
     DMBTR
     SHKZG
     XBLNR
     BELNR
     GSBER
     SGTXT
     VBELN
     BUKRS
     GJAHR
     SKNTO
      FROM BSID INTO CORRESPONDING FIELDS OF TABLE GT_BSID1 FOR ALL ENTRIES IN GT_FAGLFLEXA
     WHERE BELNR = GT_FAGLFLEXA-DOCNR AND BUDAT BETWEEN '01.04.2014'  AND  SO_BUDAT-LOW
        AND BUKRS IN SO_BUKRS AND
           KUNNR IN SO_KUNNR AND UMSKZ IN SO_UMSKZ AND UMSKZ <> 'H'.

    ENDIF.

    SELECT
      PRCTR
      KUNNR
      UMSKZ
      AUGBL
      BUDAT
      BLDAT
      BLART
      ZFBDT
      SKNTO  "ADDED BY RAM ON 20/2/16
      DMBTR
      SHKZG
      XBLNR
      BELNR
      GSBER
      SGTXT
      VBELN
      BUKRS
      GJAHR
      SKNTO
      UMSKS
      AUGDT
      ZUONR
      BUZEI FROM BSAD INTO CORRESPONDING FIELDS OF TABLE GT_BSAD FOR ALL ENTRIES IN GT_FAGLFLEXA
      WHERE BELNR = GT_FAGLFLEXA-DOCNR AND
             BUDAT IN SO_BUDAT
           AND BUKRS IN SO_BUKRS AND
            KUNNR IN SO_KUNNR AND UMSKZ IN SO_UMSKZ AND UMSKZ <> 'H' .

    SELECT
    PRCTR
    KUNNR
    UMSKZ
    AUGBL
    BUDAT
    BLDAT
    BLART
    ZFBDT
    SKNTO  "ADDED BY RAM ON 20/2/16
    DMBTR
    SHKZG
    XBLNR
    BELNR
    GSBER
    SGTXT
    VBELN
    BUKRS
    GJAHR
    SKNTO
    UMSKS
    AUGDT
    ZUONR
    BUZEI FROM BSAD INTO CORRESPONDING FIELDS OF TABLE GT_BSAD1 FOR ALL ENTRIES IN GT_FAGLFLEXA
    WHERE BELNR = GT_FAGLFLEXA-DOCNR AND
           BUDAT BETWEEN '01.04.2014'  AND  SO_BUDAT-LOW
         AND BUKRS IN SO_BUKRS AND
          KUNNR IN SO_KUNNR AND UMSKZ IN SO_UMSKZ AND UMSKZ <> 'H'.


  ELSEIF P_SD EQ 'X'.
    SELECT
        PRCTR
        KUNNR
        UMSKZ
        BUDAT
        BLDAT
        ZFBDT
        DMBTR
        SHKZG
        XBLNR
        BELNR
        GSBER
        SGTXT
        VBELN
        BUKRS
        GJAHR
         FROM BSID INTO CORRESPONDING FIELDS OF TABLE GT_BSID FOR ALL ENTRIES IN GT_FAGLFLEXA
        WHERE BELNR = GT_FAGLFLEXA-DOCNR AND BUDAT IN SO_BUDAT
         AND BUKRS IN SO_BUKRS AND
              KUNNR IN SO_KUNNR AND UMSKZ IN SO_UMSKZ AND UMSKZ = 'H' .

    SELECT
      PRCTR
      KUNNR
      UMSKZ
      AUGBL
      BUDAT
      BLDAT
      BLART
      ZFBDT
      DMBTR
      SHKZG
      XBLNR
      BELNR
      GSBER
      SGTXT
      VBELN
       SKNTO  "ADDED BY RAM ON 20/2/16
      BUKRS FROM BSAD INTO CORRESPONDING FIELDS OF TABLE GT_BSAD FOR ALL ENTRIES IN GT_FAGLFLEXA
      WHERE BELNR = GT_FAGLFLEXA-DOCNR AND BUDAT IN SO_BUDAT
             AND BUKRS IN SO_BUKRS AND
            KUNNR IN SO_KUNNR AND UMSKZ IN SO_UMSKZ AND UMSKZ = 'H'. "AND AUGDT BETWEEN LV_DATE AND SY-DATUM.

  ENDIF.

  SORT GT_BSAD BY BELNR AUGBL ASCENDING.

  APPEND LINES OF GT_BSAD TO GT_BSID.
*     APPEND LINES OF GT_BSID TO GT_BSAD1.

  APPEND LINES OF GT_BSAD1 TO GT_BSID1.
*    SORT GT_BSAD BY BELNR AUGBL.
*  DELETE ADJACENT DUPLICATES FROM GT_BSAD COMPARING BELNR AUGBL.
*  APPEND LINES OF GT_BSAD TO GT_BSID1.


  IF GT_BSID[] IS NOT INITIAL.

    SELECT
      KUNNR
      NAME1
      ADRNR
      LIFNR  FROM KNA1 INTO TABLE GT_KNA1 FOR ALL ENTRIES IN GT_BSID
      WHERE KUNNR = GT_BSID-KUNNR.

    SELECT BLART
  LTEXT
      SPRAS FROM  T003T INTO TABLE GT_T003T FOR ALL ENTRIES IN GT_BSID WHERE BLART = GT_BSID-BLART AND SPRAS = 'E' ORDER BY PRIMARY KEY.  " Added by <IT-CAR Tool> during Code Remediation
*********************** Added on 17-10-2014  By Govind ********************************
    SELECT BLART
        LTEXT
            SPRAS FROM  T003T INTO TABLE GT_T003T1 FOR ALL ENTRIES IN GT_BSAD WHERE BLART = GT_BSAD-BLART AND SPRAS = 'E' ORDER BY PRIMARY KEY.  " Added by <IT-CAR Tool> during Code Remediation

    APPEND  LINES OF GT_T003T1 TO GT_T003T.

    SELECT BLART
        LTEXT
            SPRAS FROM  T003T INTO TABLE GT_T003T2 FOR ALL ENTRIES IN GT_BSIK WHERE BLART = GT_BSIK-BLART AND SPRAS = 'E' ORDER BY PRIMARY KEY.  " Added by <IT-CAR Tool> during Code Remediation
    APPEND  LINES OF GT_T003T2 TO GT_T003T.

*********************** Added on 17-10-2014  By Govind ********************************
  ENDIF.



  IF GT_KNA1[] IS  NOT INITIAL.
    SELECT ADDRNUMBER
       STREET
       CITY1
       CITY2
       POST_CODE1 FROM ADRC INTO TABLE GT_ADRC FOR ALL ENTRIES IN GT_KNA1 WHERE ADDRNUMBER = GT_KNA1-ADRNR.

  ENDIF.

  SELECT KUNNR BUKRS UMSAV GJAHR FROM KNC1 INTO TABLE GT_KNC1 FOR ALL ENTRIES IN GT_BSID WHERE KUNNR = GT_BSID-KUNNR AND GJAHR = GT_BSID-GJAHR AND BUKRS = GT_BSID-BUKRS.
*

*********************** Added on 17-10-2014  By Govind ********************************
  IF P_VN  = 'X'.
    SELECT
       PRCTR
       LIFNR
       AUGBL
       BUDAT
       BLDAT
       BLART
       ZFBDT
       DMBTR
       SHKZG
       XBLNR
       BELNR
       GSBER
       SGTXT
       EBELN
       UMSKZ
       BUKRS
       GJAHR
      FROM BSIK INTO CORRESPONDING FIELDS OF TABLE GT_BSIK FOR ALL ENTRIES IN GT_KNA1
     WHERE LIFNR = GT_KNA1-LIFNR AND BUDAT IN SO_BUDAT
        AND BUKRS IN SO_BUKRS AND
           UMSKZ IN SO_UMSKZ  AND UMSKZ <> 'H'  AND BLART IN ('KR','AB') .

    SELECT
      PRCTR
      LIFNR
      AUGBL
      BUDAT
      BLDAT
      BLART
      ZFBDT
      DMBTR
      SHKZG
      XBLNR
      BELNR
      GSBER
      SGTXT
      EBELN
      UMSKZ
      BUKRS
      GJAHR
     FROM BSIK INTO CORRESPONDING FIELDS OF TABLE GT_BSIK2 FOR ALL ENTRIES IN GT_KNA1
    WHERE LIFNR = GT_KNA1-LIFNR AND BUDAT IN SO_BUDAT
       AND BUKRS IN SO_BUKRS AND
          UMSKZ IN SO_UMSKZ  AND UMSKZ <> 'H'  AND BLART IN ('KA') . " Added By Govind For KA Type on 05-06-2015


    SELECT
     PRCTR
     LIFNR
     AUGBL
     BUDAT
     BLDAT
     BLART
     ZFBDT
     DMBTR
     SHKZG
     XBLNR
     BELNR
     GSBER
     SGTXT
     EBELN
     UMSKZ
     BUKRS
     GJAHR
    FROM BSIK INTO CORRESPONDING FIELDS OF TABLE GT_BSIK1 FOR ALL ENTRIES IN GT_KNA1
   WHERE LIFNR = GT_KNA1-LIFNR AND BUDAT BETWEEN '01.04.2014'  AND  SO_BUDAT-LOW
     AND BUKRS IN SO_BUKRS AND
         UMSKZ IN SO_UMSKZ  AND UMSKZ <> 'H' AND BLART IN ('KR','AB') .

    SELECT
           PRCTR
           LIFNR
           AUGBL
           BUDAT
           BLDAT
           BLART
           ZFBDT
           DMBTR
           SHKZG
           XBLNR
           BELNR
           GSBER
           SGTXT
           EBELN
           UMSKZ
           BUKRS
           GJAHR
          FROM BSIK INTO CORRESPONDING FIELDS OF TABLE GT_BSIK3 FOR ALL ENTRIES IN GT_KNA1
         WHERE LIFNR = GT_KNA1-LIFNR AND BUDAT BETWEEN '01.04.2014'  AND  SO_BUDAT-LOW
           AND BUKRS IN SO_BUKRS AND
               UMSKZ IN SO_UMSKZ  AND UMSKZ <> 'H' AND BLART IN ('KA') .


    SELECT LIFNR KUNNR FROM LFA1 INTO TABLE GT_LFA1 FOR ALL ENTRIES IN GT_KNA1 WHERE LIFNR = GT_KNA1-LIFNR.

    APPEND LINES OF GT_BSIK TO GT_BSID.
    APPEND LINES OF GT_BSIK2 TO GT_BSID.

    SELECT
       PRCTR
       LIFNR
       AUGBL
       BUDAT
       BLDAT
       BLART
       ZFBDT
       DMBTR
       SHKZG
       XBLNR
       BELNR
       GSBER
       SGTXT
       EBELN
       UMSKZ
       BUKRS
       GJAHR
      FROM BSAK INTO CORRESPONDING FIELDS OF TABLE GT_BSAK FOR ALL ENTRIES IN GT_KNA1
     WHERE LIFNR = GT_KNA1-LIFNR AND BUDAT IN SO_BUDAT
        AND BUKRS IN SO_BUKRS AND
           UMSKZ IN SO_UMSKZ  AND UMSKZ <> 'H'  AND BLART IN ('KR','AB').


    SELECT
       PRCTR
       LIFNR
       AUGBL
       BUDAT
       BLDAT
       BLART
       ZFBDT
       DMBTR
       SHKZG
       XBLNR
       BELNR
       GSBER
       SGTXT
       EBELN
       UMSKZ
       BUKRS
       GJAHR
      FROM BSAK INTO CORRESPONDING FIELDS OF TABLE GT_BSAK2 FOR ALL ENTRIES IN GT_KNA1
     WHERE LIFNR = GT_KNA1-LIFNR AND BUDAT IN SO_BUDAT
        AND BUKRS IN SO_BUKRS AND
           UMSKZ IN SO_UMSKZ  AND UMSKZ <> 'H'  AND BLART IN ('KA'). " Added By Govind For KA Type on 05-06-2015

    SELECT
    PRCTR
    LIFNR
    AUGBL
    BUDAT
    BLDAT
    BLART
    ZFBDT
    DMBTR
    SHKZG
    XBLNR
    BELNR
    GSBER
    SGTXT
    EBELN
    UMSKZ
    BUKRS
    GJAHR
   FROM BSAK INTO CORRESPONDING FIELDS OF TABLE GT_BSAK1 FOR ALL ENTRIES IN GT_KNA1
  WHERE LIFNR = GT_KNA1-LIFNR AND BUDAT BETWEEN '01.04.2014'  AND  SO_BUDAT-LOW
    AND BUKRS IN SO_BUKRS AND
        UMSKZ IN SO_UMSKZ  AND UMSKZ <> 'H' AND BLART IN ('KR','AB') .


    SELECT
          PRCTR
          LIFNR
          AUGBL
          BUDAT
          BLDAT
          BLART
          ZFBDT
          DMBTR
          SHKZG
          XBLNR
          BELNR
          GSBER
          SGTXT
          EBELN
          UMSKZ
          BUKRS
          GJAHR
         FROM BSAK INTO CORRESPONDING FIELDS OF TABLE GT_BSAK3 FOR ALL ENTRIES IN GT_KNA1
        WHERE LIFNR = GT_KNA1-LIFNR AND BUDAT BETWEEN '01.04.2014'  AND  SO_BUDAT-LOW
          AND BUKRS IN SO_BUKRS AND
              UMSKZ IN SO_UMSKZ  AND UMSKZ <> 'H' AND BLART IN ('KA') .

    APPEND LINES OF GT_BSAK TO GT_BSID.
    APPEND LINES OF GT_BSAK2 TO GT_BSID.

    APPEND LINES OF GT_BSIK1 TO GT_BSID1.
    APPEND LINES OF GT_BSIK3 TO GT_BSID1.
    APPEND LINES OF GT_BSAK1 TO GT_BSID1.
  ENDIF.
*********************** Added on 17-10-2014  By Govind ********************************

ENDFORM.                    " FATCH_DATA



*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_DATA1 .


  LOOP AT GT_KNC1 INTO WA_KNC1.
    LV_TOTAL =  WA_KNC1-UMSAV.
  ENDLOOP.

  LOOP AT GT_BSID1 INTO WA_BSID1 .
    IF  WA_BSID1-BUDAT < SO_BUDAT-LOW.
      IF WA_BSID1-SHKZG = 'S'.
        LV_OPN = LV_OPN  + WA_BSID1-DMBTR.
      ELSEIF WA_BSID1-SHKZG = 'H'.
        LV_OPN  = LV_OPN - WA_BSID1-DMBTR.
      ENDIF.
    ENDIF.
    CLEAR WA_BSID1.
  ENDLOOP.




  LOOP AT GT_BSID INTO WA_BSID.
    IF WA_BSID-SHKZG = 'H'.
      WA_FINAL-CR_AMT = WA_BSID-DMBTR.
*     total   =  WA_FINAL-CR_AMT - LV_OPN.
    ELSEIF WA_BSID-SHKZG = 'S'.
      WA_FINAL-DR_AMT = WA_BSID-DMBTR.
    ENDIF.

    WA_FINAL-XBLNR = WA_BSID-XBLNR.
    WA_FINAL-BLDAT = WA_BSID-BLDAT.
    WA_FINAL-KUNNR = WA_BSID-KUNNR.
    WA_FINAL-AUGBL = WA_BSID-AUGBL.
    WA_FINAL-BELNR = WA_BSID-BELNR.
    WA_FINAL-BUDAT = WA_BSID-BUDAT.
    WA_FINAL-GSBER = WA_BSID-GSBER.
    WA_FINAL-SGTXT = WA_BSID-SGTXT.
    WA_FINAL-VBELN = WA_BSID-VBELN.
    WA_FINAL-UMSKZ = WA_BSID-UMSKZ.
    WA_FINAL-BUKRS = WA_BSID-BUKRS.
    WA_FINAL-BLART = WA_BSID-BLART.
    WA_FINAL-SKNTO = WA_BSID-SKNTO.
    READ TABLE GT_T003T INTO WA_T003T WITH KEY BLART = WA_BSID-BLART.
    WA_FINAL-LTEXT = WA_T003T-LTEXT.
    "  WA_FINAL-BLART = WA_T003T-BLART.
    READ TABLE GT_KNA1 INTO WA_KNA1 WITH KEY KUNNR = WA_BSID-KUNNR.
    WA_FINAL-NAME1 = WA_KNA1-NAME1.
    WA_FINAL-SHKZG = WA_BSID-SHKZG.
    APPEND WA_FINAL TO GT_FINAL.
    CLEAR : WA_FINAL,WA_BSID.
  ENDLOOP.



  B_DATE = SO_BUDAT-HIGH.

  SORT GT_FINAL BY BUDAT SHKZG.

*SELECT SINGLE * FROM KNC1 INTO WA_KNC1
*   WHERE KUNNR = WA_KNA1-KUNNR AND UMSAV > 0
*   AND BUKRS = '1000'  AND GJAHR = '2014'.

  LOOP AT GT_FINAL INTO WA_FINAL.

    LV_SUM = LV_SUM + WA_FINAL-DR_AMT - WA_FINAL-CR_AMT .

    WA_FINAL-BAL_AMT = LV_SUM.

    MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING BAL_AMT.
    AT END OF KUNNR.
      CLEAR :LV_SUM,WA_FINAL,WA_BSID.
    ENDAT.

  ENDLOOP.
************************ Added on 17-10-2014  By Govind ********************************
  LOOP AT GT_FINAL INTO WA_FINAL WHERE KUNNR = ' '.
    WA_FINAL-KUNNR = WA_KNA1-KUNNR.
    MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING KUNNR.
    CLEAR  WA_FINAL.
  ENDLOOP.
  """"""""""""""""""""""""""""""ADDED BY RAM ON 22/2/2016

*  DELETE GT_FINAL[] WHERE BLART = 'AB' AND SKNTO = '0.00' . " BLART = 'AB' AND  "change by sri 22/02/2017



  LOOP AT GT_FINAL INTO WA_FINAL WHERE BLART = 'AB'.




    IF WA_FINAL-SHKZG = 'C'. "and WA_FINAL-CR_AMT = ' '.
      WA_FINAL-CR_AMT = WA_FINAL-SKNTO .
    ENDIF .
    IF WA_FINAL-SHKZG = 'D' .
      WA_FINAL-DR_AMT = WA_FINAL-SKNTO .
    ENDIF .


    MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING CR_AMT DR_AMT.
    CLEAR WA_FINAL .
  ENDLOOP .




  "ENDED BY RAM ON 22/2/2016


*********************** Added on 17-10-2014  By Govind ********************************


  RV_OPN = LV_TOTAL - LV_OPN.

ENDFORM.                    " READ_DATA





*&---------------------------------------------------------------------*
*&      Form  ALV_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P1  text
*      -->P2  text
*      -->P3  text
*      -->P4  text
*      -->P5  text
*----------------------------------------------------------------------*
FORM ALV_LAYOUT  USING    P1 P2 P3 P4 P5 P6 P7.
  CLEAR WA_FCAT.
  WA_FCAT-COL_POS = P1.
  WA_FCAT-SELTEXT_L = P2.
  WA_FCAT-FIELDNAME = P3.
  WA_FCAT-TABNAME = P4.
  WA_FCAT-DO_SUM = P5.
  WA_FCAT-NO_OUT = P6.
  WA_FCAT-NO_ZERO = P7.

  APPEND WA_FCAT TO GT_FCAT.
  CLEAR WA_FCAT.

ENDFORM.                    " ALV_LAYOUT







*&---------------------------------------------------------------------*
*&      Form  TOP-OF-PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM TOP-OF-PAGE.
*ALV Header declarations
*IF SY-TABIX = '1'.
  REFRESH :  GT_HEADER.

  DATA: T_LINE LIKE WA_HEADER-INFO,
  LD_LINES TYPE I,
  LD_LINESC(10) TYPE C.
*TITLE
  WA_HEADER-TYP = 'H'.
  WA_HEADER-INFO = 'Customer Wise Ledger Balance'.
  APPEND WA_HEADER TO GT_HEADER.
  CLEAR WA_HEADER.


*DATE
  WA_HEADER-TYP = 'S'.
  WA_HEADER-KEY = 'Date: '.
  CONCATENATE SY-DATUM+6(2) '.'
  SY-DATUM+4(2) '.'
  SY-DATUM(4) INTO WA_HEADER-INFO. "todays date
  APPEND WA_HEADER TO GT_HEADER.
  CLEAR: WA_HEADER.

  WA_HEADER-TYP = 'S'.
  WA_HEADER-KEY = 'Customer No :'.
  CONCATENATE '' SO_KUNNR+3(15) INTO WA_HEADER-INFO.
  APPEND WA_HEADER TO GT_HEADER.
  CLEAR : WA_HEADER.

  WA_HEADER-TYP = 'S'.
  WA_HEADER-KEY = 'Customer name'.
  CONCATENATE '' WA_KNA1-NAME1 INTO WA_HEADER-INFO.
  APPEND WA_HEADER TO GT_HEADER.
  CLEAR WA_HEADER.
*TOTAL NO. OF RECORDS SELECTED
*  WA_HEADER-TYP = 'S'.
*  WA_HEADER-KEY = 'Addres'.
*  CONCATENATE '' GT_ADRC-STREET+3(20) INTO WA_HEADER-INFO.
*  APPEND WA_HEADER TO GT_HEADER.
*  CLEAR WA_HEADER.


  DESCRIBE TABLE GT_FINAL LINES LD_LINES.
  LD_LINESC = LD_LINES.
  CONCATENATE 'Total No. of Records Selected: ' LD_LINESC
INTO T_LINE SEPARATED BY SPACE.


  WA_HEADER-TYP = 'A'.
  WA_HEADER-INFO = T_LINE.
  APPEND WA_HEADER TO GT_HEADER.
  CLEAR: WA_HEADER, T_LINE.

*  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
*    EXPORTING
*      IT_LIST_COMMENTARY = GT_HEADER
*      I_LOGO             = 'ZJNPL_LOGO'.


*  ENDIF.
ENDFORM.                    "APPLICATION_SERVER


*
*CALL FUNCTION 'HR_IN_CHG_INR_WRDS'
*  EXPORTING
*    AMT_IN_NUM               =
** IMPORTING
**   AMT_IN_WORDS             =
** EXCEPTIONS
**   DATA_TYPE_MISMATCH       = 1
**   OTHERS                   = 2
*          .
*IF SY-SUBRC <> 0.
** Implement suitable error handling here
*ENDIF.









*&---------------------------------------------------------------------*
*&      Form  PRINT_FORMATFORM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PRINT_FORMATFORM .

*&---------------------------------------------------------------------*
*&   Calling SMARTFORMS
*&---------------------------------------------------------------------*

*LV_FRDAT  = SO_BUDAT-LOW.
*LV_TODAT   = SO_BUDAT-HIGH.
*LV_OPN = LV_OPN.



*
*if wa_final-bukrs = '1000'.
*
*
*  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
*    EXPORTING
**      IT_LIST_COMMENTARY = it_f
*      I_LOGO             = 'ZSHEENLAC'.
*
*  elseif  wa_final-bukrs = '1700'.
*
*    CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
*    EXPORTING
**      IT_LIST_COMMENTARY = it_f
*      I_LOGO             = 'ZNOROO_LOGO'.
*
*    endif.

*
*CALL FUNCTION '/1BCDWB/SF00000426'
*  EXPORTING
**   ARCHIVE_INDEX              =
**   ARCHIVE_INDEX_TAB          =
**   ARCHIVE_PARAMETERS         =
**   CONTROL_PARAMETERS         =
**   MAIL_APPL_OBJ              =
**   MAIL_RECIPIENT             =
**   MAIL_SENDER                =
**   OUTPUT_OPTIONS             =
**   USER_SETTINGS              = 'X'
*    BAL_DATE                   =  B_DATE
*    LV_OPN                     =  LV_OPN
*    LV_FRDT                    =  SO_BUDAT-LOW
*    LV_TODT                    =  SO_BUDAT-HIGH
*    LV_BUK                     =  LV_BUK
** IMPORTING
**   DOCUMENT_OUTPUT_INFO       =
**   JOB_OUTPUT_INFO            =
**   JOB_OUTPUT_OPTIONS         =
*  TABLES
*    GT_KNA1                    =  GT_KNA1[]
*    GT_BSID                    =  GT_FINAL[]
** EXCEPTIONS
**   FORMATTING_ERROR           = 1
**   INTERNAL_ERROR             = 2
**   SEND_ERROR                 = 3
**   USER_CANCELED              = 4
**   OTHERS                     = 5
*          .
*IF SY-SUBRC <> 0.
** Implement suitable error handling here
*ENDIF.
*
*



  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      FORMNAME           = 'ZSF_CUS_ACCOUNT_REC_LED'
*     VARIANT            = ' '
*     DIRECT_CALL        = ' '
    IMPORTING
      FM_NAME            = FM_NAME
    EXCEPTIONS
      NO_FORM            = 1
      NO_FUNCTION_MODULE = 2
      OTHERS             = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


  CALL FUNCTION FM_NAME  "/1BCDWB/SF00000426'
    EXPORTING
*   ARCHIVE_INDEX              =
*   ARCHIVE_INDEX_TAB          =
*   ARCHIVE_PARAMETERS         =
*   CONTROL_PARAMETERS         =
*   MAIL_APPL_OBJ              =
*   MAIL_RECIPIENT             =
*   MAIL_SENDER                =
*   OUTPUT_OPTIONS             =
*   USER_SETTINGS              = 'X'
      BAL_DATE                   = B_DATE
      LV_OPN                     = LV_OPN
      LV_FRDT                    = SO_BUDAT-LOW
      LV_TODT                    = SO_BUDAT-HIGH
      LV_BUK                     = LV_BUK
* IMPORTING
*   DOCUMENT_OUTPUT_INFO       =
*   JOB_OUTPUT_INFO            =
*   JOB_OUTPUT_OPTIONS         =
    TABLES
      GT_KNA1                    = GT_KNA1[]
      GT_BSID                    = GT_FINAL[]
* EXCEPTIONS
*   FORMATTING_ERROR           = 1
*   INTERNAL_ERROR             = 2
*   SEND_ERROR                 = 3
*   USER_CANCELED              = 4
*   OTHERS                     = 5
            .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.



*
**
*  CALL FUNCTION 'SPELL_AMOUNT'
*    EXPORTING
*      AMOUNT    = WA_BSAD-DMBTR
*      CURRENCY  = 'I  NR'
**     FILLER    = ' '
*      LANGUAGE  = SY-LANGU
*    IMPORTING
*      IN_WORDS  = IN_WORDS
*    EXCEPTIONS
*      NOT_FOUND = 1
*      TOO_LARGE = 2
*      OTHERS    = 3.
*  IF SY-SUBRC <> 0.
** Implement suitable error handling here
*  ENDIF.






ENDFORM.                    " PRINT_FORMATFORM
