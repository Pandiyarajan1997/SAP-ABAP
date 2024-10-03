*&---------------------------------------------------------------------*
*& Report  ZCUST_BAL_LEDGER
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
*&---------------------   CREATED  ------------------------------------*
*&Functional                   : Mr.RAMACHANDRAN M                     *
*& Developer                   : Mr.RAMACHANDRAN M                     *
*& Created On                  : 14/ Augest 2015                         *
*& Company                     : Sheenlac Paints Pvt. Ltd              *
*& Title                       : Payment advices
*& Report Name                 : ZCUST_BAL_LEDGER                      *
*& Development Id              : kpabap                                *
*& Solman call No              :                                       *
*& Transport Request           :                                       *
*& Related Information         : Driver Program For Customer       *
*&                             : Ledger and Balance Confirmation Form  *
*&---------------------------------------------------------------------*
REPORT ZCUST_BAL_PAYMENT_ADVICE.

*&---------------------------------------------------------------------*
*&  Sturcture & Internal Table Decleration
*&---------------------------------------------------------------------*

TABLES : LFA1 , BSAK .


DATA : WA_LFA1   TYPE  LFA1.
DATA : WA_LFA2   TYPE  LFA1.
DATA : WA_BSAK   TYPE  BSAK.

DATA : WA_BSAK1 TYPE BSAK .
DATA : WA_BSAK2 TYPE BSAK .
DATA : WA_ADR6 TYPE ADR6 .
DATA : WA_ADR2 TYPE ADR2 .

DATA : WA_LFBK TYPE LFBK .

DATA : WA_ADR3 TYPE ADR3.

DATA : IT_LFA1   TYPE  TABLE  OF LFA1 WITH HEADER LINE.
DATA : IT_LFA2   TYPE  TABLE  OF LFA1 WITH HEADER LINE.
DATA : IT_BSAK   TYPE  TABLE  OF BSAK WITH HEADER LINE.

DATA : IT_BSAK1   TYPE  TABLE  OF BSAK WITH HEADER LINE.

DATA : IT_BSAK2   TYPE  TABLE  OF BSAK WITH HEADER LINE.

DATA : IT_ADR2   TYPE  TABLE  OF ADR2 WITH HEADER LINE.
DATA : IT_ADR6   TYPE  TABLE  OF ADR6 WITH HEADER LINE.

DATA : IT_LFBK   TYPE  TABLE  OF LFBK WITH HEADER LINE.

DATA : IT_ADR3   TYPE  TABLE  OF ADR3 WITH HEADER LINE.




TYPES : BEGIN OF GS_LFA1,
        LIFNR TYPE LFA1-LIFNR,
        KUNNR TYPE LFA1-KUNNR,
        END OF GS_LFA1.

*DATA : GT_LFA1 TYPE TABLE OF LFA1,
*       WA_LFA1 TYPE GS_LFA1.


TYPES: BEGIN OF GS_KNA1,
       KUNNR TYPE KNA1-KUNNR,                  " Customer Number
       NAME1 TYPE KNA1-NAME1,                  " Customer Name
       ADRNR TYPE KNA1-ADRNR,                  " Address Number
       LIFNR TYPE KNA1-LIFNR,                  " Vendor Code
       END OF GS_KNA1.

DATA: GT_KNA1 TYPE TABLE OF GS_KNA1,
      WA_KNA1 TYPE GS_KNA1.

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
              END OF GS_BSID.

DATA: GT_BSID TYPE TABLE OF GS_BSID,
      GT_BSID1 TYPE TABLE OF GS_BSID,
      WA_BSID TYPE GS_BSID,
      WA_BSID1 TYPE GS_BSID.


DATA : FM_GET TYPE RS38L_FNAM.
DATA : CONTROL_PARAM  TYPE SSFCTRLOP .


TYPES: BEGIN OF GS_ADRC,
       ADDRNUMBER TYPE ADRC-ADDRNUMBER,        " Address Number
       STREET TYPE ADRC-STREET,                " Street
       CITY1 TYPE ADRC-CITY1,                  " District
       CITY2 TYPE ADRC-CITY2,                  " City
       POST_CODE1 TYPE ADRC-POST_CODE1,        " Postal Code
       END OF GS_ADRC.

DATA: GT_ADRC TYPE TABLE OF GS_ADRC WITH HEADER LINE,
      WA_ADRC TYPE GS_ADRC.

*DATA: GT_FINAL TYPE TABLE OF GS_FINAL WITH HEADER LINE,
*      WA_FINAL TYPE GS_FINAL.

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


DATA: OR_BUDAT TYPE BSAD-BUDAT,
      OR_BUKRS TYPE BSID-BUKRS,
      OR_AUGBL TYPE BSAK-AUGBL,
      OR_KUNNR TYPE KNA1-KUNNR,
      FM_NAME TYPE RS38L_FNAM,
      OR_PRCTR TYPE CEPC-PRCTR,
      OR_UMSKZ TYPE BSID-UMSKZ,
      OR_LIFNR TYPE LFA1-LIFNR .

DATA: L_LIFNR TYPE LFA1-LIFNR,
      L_PRCTR TYPE CEPC-PRCTR.

DATA: B_DATE TYPE SY-DATUM.


DATA: LV_OPN TYPE BSAD-DMBTR,
      RV_OPN TYPE BSAD-DMBTR,
      LV_TOTAL TYPE BSAD-DMBTR,
      LV_TRANS TYPE BSAD-DMBTR,
      LV_DATE TYPE SY-DATUM,
      LV_FRDAT TYPE SY-DATUM,
      LV_TODAT TYPE SY-DATUM,
      LV_SUM TYPE BSAD-DMBTR.

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

********** Graph*****************

DATA: GT_GRAPH       TYPE TABLE OF GPRVAL WITH HEADER LINE,
X_TEXTS        TYPE TABLE OF GPRTXT WITH HEADER LINE.


*&---------------------------------------------------------------------*
*&  Selection Screen Fields
*&---------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: SO_LIFNR FOR OR_LIFNR OBLIGATORY,
                SO_BUDAT FOR OR_BUDAT NO-DISPLAY ,
                SO_AUGBL FOR OR_AUGBL OBLIGATORY .

SELECTION-SCREEN: END OF BLOCK B1.

INITIALIZATION.

*  SO_BUDAT-LOW = '20140401'.
*  SO_BUDAT-HIGH = '20150331'.
*  SO_BUDAT-OPTION = 'BT'.
*  SO_BUDAT-SIGN = 'I'.
*  APPEND SO_BUDAT.


AT SELECTION-SCREEN .
  IF SO_LIFNR IS NOT INITIAL.
    SELECT SINGLE LIFNR FROM LFA1 INTO L_LIFNR WHERE LIFNR IN SO_LIFNR .
    IF SY-SUBRC NE 0.
      MESSAGE 'Enter Valid Vendor Number' TYPE 'E'.
    ENDIF.
  ENDIF.


*&---------------------------------------------------------------------*
*&  Main Logic
*&---------------------------------------------------------------------*
  LV_DATE = SO_BUDAT-HIGH  + 1.

*START-OF-SELECTION.

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

  SELECT
     *
      FROM LFA1 INTO TABLE IT_LFA1 WHERE LIFNR IN SO_LIFNR .

      SELECT
     *
      FROM LFA1 INTO TABLE IT_LFA2 WHERE LIFNR IN SO_LIFNR .


  SELECT * FROM  ADR6
      INTO CORRESPONDING FIELDS OF TABLE IT_ADR6 FOR ALL ENTRIES IN IT_LFA1
  WHERE ADDRNUMBER = IT_LFA1-ADRNR .

  SELECT * FROM  ADR2
     INTO CORRESPONDING FIELDS OF TABLE IT_ADR2 FOR ALL ENTRIES IN IT_LFA1
 WHERE ADDRNUMBER = IT_LFA1-ADRNR .

  SELECT * FROM  LFBK
      INTO CORRESPONDING FIELDS OF TABLE IT_LFBK FOR ALL ENTRIES IN IT_LFA1
  WHERE LIFNR = IT_LFA1-LIFNR .

*  SELECT * FROM  BSAK
*        INTO CORRESPONDING FIELDS OF TABLE IT_BSAK FOR ALL ENTRIES IN IT_LFA1
*    WHERE LIFNR IN SO_LIFNR AND AUGBL IN SO_AUGBL AND SHKZG <> 'S'.

  SELECT * FROM  BSAK
        INTO TABLE IT_BSAK WHERE LIFNR IN SO_LIFNR AND AUGBL IN SO_AUGBL AND SHKZG <> 'S'.



*  SELECT * FROM  BSAK
*      INTO CORRESPONDING FIELDS OF TABLE IT_BSAK1 FOR ALL ENTRIES IN IT_LFA1
*  WHERE LIFNR IN SO_LIFNR AND AUGBL IN SO_AUGBL AND SHKZG EQ 'S' .

  SELECT * FROM  BSAK
      INTO TABLE IT_BSAK1
  WHERE LIFNR IN SO_LIFNR AND AUGBL IN SO_AUGBL AND SHKZG EQ 'S' .

*    SELECT * FROM  BSAK
*        INTO CORRESPONDING FIELDS OF TABLE IT_BSAK2 FOR ALL ENTRIES IN IT_BSAK
*    WHERE AUGBL = IT_BSAK-AUGBL AND LIFNR IN SO_LIFNR AND SHKZG <> 'S' .

    SELECT * FROM  BSAK
   INTO TABLE IT_BSAK2
    WHERE AUGBL IN SO_AUGBL AND LIFNR IN SO_LIFNR AND SHKZG <> 'S' .

  SELECT * FROM ADR3
    INTO CORRESPONDING FIELDS OF TABLE IT_ADR3 FOR ALL ENTRIES IN IT_LFA1
    WHERE ADDRNUMBER = IT_LFA1-ADRNR .


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

*
* LOOP AT IT_BSAK  INTO WA_BSAK .
*
*   SELECT

  " ENDLOOP .





*  LOOP AT GT_BSID1 INTO WA_BSID1 .
*
*    IF  WA_BSID1-BUDAT < SO_BUDAT-LOW.
*      IF WA_BSID1-SHKZG = 'S'.
*        LV_OPN = LV_OPN  + WA_BSID1-DMBTR.
*      ELSEIF WA_BSID1-SHKZG = 'H'.
*        LV_OPN  = LV_OPN - WA_BSID1-DMBTR.
*      ENDIF.
*    ENDIF.
*    CLEAR WA_BSID1.
*  ENDLOOP.
*
*
*
*

*
*
*  B_DATE = SO_BUDAT-HIGH.
*
*  SORT GT_FINAL BY BUDAT SHKZG.
*
**SELECT SINGLE * FROM KNC1 INTO WA_KNC1
**   WHERE KUNNR = WA_KNA1-KUNNR AND UMSAV > 0
**   AND BUKRS = '1000'  AND GJAHR = '2014'.
*
*  LOOP AT GT_FINAL INTO WA_FINAL.
*
*    LV_SUM = LV_SUM + WA_FINAL-DR_AMT - WA_FINAL-CR_AMT .
*
*    WA_FINAL-BAL_AMT = LV_SUM.
*
*
*
*    MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING BAL_AMT.
*    AT END OF KUNNR.
*      CLEAR :LV_SUM,WA_FINAL,WA_BSID.
*    ENDAT.
*
*  ENDLOOP.

*
*  RV_OPN = LV_TOTAL - LV_OPN.

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
  WA_HEADER-INFO = 'Payment Advice Pdf'.
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

*  WA_HEADER-TYP = 'S'.
*  WA_HEADER-KEY = 'Customer No :'.
*  CONCATENATE '' SO_KUNNR+3(15) INTO WA_HEADER-INFO.
*  APPEND WA_HEADER TO GT_HEADER.
*  CLEAR : WA_HEADER.

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


*  DESCRIBE TABLE GT_FINAL LINES LD_LINES.
*  LD_LINESC = LD_LINES.
*  CONCATENATE 'Total No. of Records Selected: ' LD_LINESC
*INTO T_LINE SEPARATED BY SPACE.


  WA_HEADER-TYP = 'A'.
  WA_HEADER-INFO = T_LINE.
  APPEND WA_HEADER TO GT_HEADER.
  CLEAR: WA_HEADER, T_LINE.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = GT_HEADER
      I_LOGO             = 'ZLOGO'.
*  ENDIF.
ENDFORM.                    "APPLICATION_SERVER







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


  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      FORMNAME = 'ZSF_VEND_ACC_DET'
    IMPORTING
      FM_NAME  = FM_GET.
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.



  CALL FUNCTION FM_GET
    EXPORTING
      CONTROL_PARAMETERS = CONTROL_PARAM
*     WA_T001W           = WA_FRPLNT
*     IS_T001W           = WA_TOPLNT
*     WA_MKPF            = WA_MKPF
    TABLES
      IT_ADR2            = IT_ADR2
      IT_ADR6            = IT_ADR6
      IT_LFA1            = IT_LFA1
      IT_BSAK            = IT_BSAK
      IT_BSAK1           = IT_BSAK1
      IT_BSAK2           = IT_BSAK2
      IT_LFBK            = IT_LFBK
      IT_ADR3            = IT_ADR3
      IT_LFA2            = IT_LFA2

    EXCEPTIONS
      FORMATTING_ERROR   = 1
      INTERNAL_ERROR     = 2
      SEND_ERROR         = 3
*     USER_CANCELED      = 4
*     OTHERS             =             5
    .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.
ENDFORM .                    "PRINT_FORMATFORM
