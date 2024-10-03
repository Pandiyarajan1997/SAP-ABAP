**&---------------------PSKGDT------------------------------------------*
**& Report  ZCUST_BAL_LEDGER_PRINT3
**&
**&---------------------------------------------------------------------*
**&
**&
**&---------------------------------------------------------------------*
*
REPORT ZCUST_BAL_LEDGER_PRINT_EMAIL.

TABLES:BSID.
TABLES:KNA1.
TABLES:ZMAIL_ID.

TYPES: BEGIN OF GS_KNA1,
       KUNNR TYPE KNA1-KUNNR,                  " Customer Number
       NAME1 TYPE KNA1-NAME1,                  " Customer Name
       ADRNR TYPE KNA1-ADRNR,                  " Address Number
       LIFNR TYPE KNA1-LIFNR,                  " Vendor Code
       END OF GS_KNA1.

DATA: GT_KNA1 TYPE TABLE OF GS_KNA1,
      WA_KNA1 TYPE GS_KNA1.
TYPES : BEGIN OF GS_T003T,
       BLART TYPE T003T-BLART,
       LTEXT TYPE T003T-LTEXT,
       SPRAS TYPE T003T-SPRAS,
       END OF GS_T003T.

DATA : GT_T003T TYPE  TABLE OF GS_T003T,
       WA_T003T TYPE GS_T003T.

DATA : GT_T003T1 TYPE  TABLE OF GS_T003T,
       WA_T003T1 TYPE GS_T003T.

DATA : GT_T003T2 TYPE  TABLE OF GS_T003T,
       WA_T003T2 TYPE GS_T003T.

TYPES: BEGIN OF GS_BSID,
       PRCTR TYPE BSID-PRCTR,                  " Profit Center
       KUNNR TYPE BSID-KUNNR,                  " Customer Number
       AUGBL TYPE BSID-AUGBL,
       BUDAT TYPE BSID-BUDAT,                  " Posting Date in the Document
       BLDAT TYPE BSID-BLDAT,
       BLART TYPE BSID-BLART,
       BSCHL TYPE BSID-BSCHL,                  "Posting key
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
      GTT_BSID TYPE TABLE OF GS_BSID,
      WA_BSID TYPE GS_BSID,
      WAT_BSID TYPE GS_BSID,
      WA_BSID1 TYPE GS_BSID.

DATA:DBT_AMT TYPE BSID-DMBTR.

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
        BUKRS TYPE BSAK-BUKRS,
        LIFNR TYPE BSAK-LIFNR,
        UMSKS TYPE BSAK-UMSKS,
        UMSKZ TYPE BSAK-UMSKZ,
        AUGDT TYPE BSAK-AUGDT,
        AUGBL TYPE BSAK-AUGBL,
        ZUONR TYPE BSAK-ZUONR,
        GJAHR TYPE BSAK-GJAHR,
        BELNR TYPE BSAK-BELNR,
        BUZEI TYPE BSAK-BUZEI,
        BUDAT TYPE BSAK-BUDAT,
        BLDAT TYPE BSAK-BLDAT,
        XBLNR TYPE BSAK-XBLNR,
        BLART TYPE BSAK-BLART,
        DMBTR TYPE BSAK-DMBTR,
        BSCHL TYPE BSAK-BSCHL,
        SGTXT TYPE BSAK-SGTXT,
        GSBER TYPE BSAK-GSBER,
        SHKZG TYPE BSAK-SHKZG,
        SKNTO TYPE BSAK-SKNTO,
      END OF GS_BSAK.


DATA: GT_BSAK TYPE TABLE OF GS_BSAK,
       WA_BSAK TYPE GS_BSAK.

DATA: GT_BSAK1 TYPE TABLE OF GS_BSAK,
       WA_BSAK1 TYPE GS_BSAK.
DATA: GT_BSAK2 TYPE TABLE OF GS_BSAK,
       WA_BSAK2 TYPE GS_BSAK.

DATA: GT_BSAK3 TYPE TABLE OF GS_BSAK,
       WA_BSAK3 TYPE GS_BSAK.

DATA: LV_IND TYPE I.

TYPES: BEGIN OF GS_BSAD,
       PRCTR TYPE BSAD-PRCTR,                  " Profit Center
       KUNNR TYPE BSAD-KUNNR,                  " Customer Number
       AUGBL TYPE BSAD-AUGBL,
       BUDAT TYPE BSAD-BUDAT,                  " Posting Date in the Document
       BLDAT TYPE BSAD-BLDAT,
       BLART TYPE BSAD-BLART,
       BSCHL TYPE BSAD-BSCHL,                  " "Posting key
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
       UMSKS TYPE BSAD-UMSKS,
       AUGDT TYPE BSAD-AUGDT,
       ZUONR TYPE BSAD-ZUONR,
       BUZEI TYPE BSAD-BUZEI,
       END OF GS_BSAD.

DATA: GT_BSAD TYPE TABLE OF GS_BSAD,
      WA_BSAD TYPE GS_BSAD,
      WA_BSAD1 TYPE GS_BSAD,
      GT_BSAD1 TYPE TABLE OF GS_BSAD,
      GT_BSAD2 TYPE TABLE OF GS_BSAD.

DATA: GT_BSAD7 TYPE TABLE OF GS_BSAD,
      WA_BSAD7 TYPE GS_BSAD.

TYPES: BEGIN OF GS_ADRC,
       ADDRNUMBER TYPE ADRC-ADDRNUMBER,        " Address Number
       STREET TYPE ADRC-STREET,                " Street
       CITY1 TYPE ADRC-CITY1,                  " District
       CITY2 TYPE ADRC-CITY2,                  " City
       POST_CODE1 TYPE ADRC-POST_CODE1,        " Postal Code
       END OF GS_ADRC.

DATA: GT_ADRC TYPE TABLE OF GS_ADRC WITH HEADER LINE,
      WA_ADRC TYPE GS_ADRC.

TYPES : BEGIN OF TY_CHK,
         BELNR TYPE BSAD-BELNR,                  " Bill Number
         BUDAT  TYPE BSID-BUDAT,                 " Posting Date
         BLART TYPE BSID-BLART,                  " Bill Doc. Type
         CR_AMT TYPE BSAD-DMBTR,                 " Cr Amount
         DR_AMT TYPE BSAD-DMBTR,                 " Dr Amount
         SHKZG TYPE BSID-SHKZG,
        END OF TY_CHK.

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
       BSCHL TYPE BSID-BSCHL,                  "Posting key
       SGTXT TYPE char100, "BSID-SGTXT,                  " Text OR Remarks
       GSBER TYPE BSID-GSBER,                  " Business Area
       SHKZG TYPE BSID-SHKZG,                  " Dbit/Crdit ind.
       BAL_AMT TYPE BSAD-DMBTR,                " Balance Amount
       LTEXT TYPE T003T-LTEXT,                  " Bill Doc .Type Descrption
       BUKRS TYPE BSID-BUKRS ,
       GJAHR TYPE BSID-GJAHR,
       SKNTO TYPE BSAD-SKNTO ,
       LV_COUNT TYPE INT1,
       STA TYPE INT1,
       T_TTTEXT TYPE CHAR20,                  " blart text field
       END OF GS_FINAL.

DATA: T_TTTEXT TYPE CHAR20,                  " blart text field
      LVV_BELNR TYPE BSID-BELNR,
      LVV_BLART TYPE BSID-BLART,
      LV_FLAG2  TYPE CHAR1.

DATA: GT_FINAL TYPE TABLE OF GS_FINAL WITH HEADER LINE,
      WA_FINAL TYPE GS_FINAL,
      GT_FINAL1 TYPE TABLE OF GS_FINAL WITH HEADER LINE,
      WA_FINAL1 TYPE GS_FINAL,
      GT_FINAL2 TYPE TABLE OF GS_FINAL WITH HEADER LINE,
      WA_FINAL2 TYPE GS_FINAL,
      GT_FINAL3 TYPE TABLE OF GS_FINAL WITH HEADER LINE,
      WA_FINAL3 TYPE GS_FINAL,
      GT_FINAL4 TYPE TABLE OF GS_FINAL WITH HEADER LINE,
      WA_FINAL4 TYPE GS_FINAL,
      GT_FINAL_T TYPE TABLE OF GS_FINAL WITH HEADER LINE,
      WA_FINAL_T TYPE GS_FINAL,
      ITT_FINAL TYPE TABLE OF GS_FINAL,
      WAT_FINAL TYPE GS_FINAL,
      ITT_FINAL1 TYPE TABLE OF GS_FINAL,
      WAT_FINAL1 TYPE GS_FINAL,
      ITT_FINAL2 TYPE TABLE OF GS_FINAL,
      WAT_FINAL3 TYPE GS_FINAL,
      GS_CHK TYPE TY_CHK.

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
      LV_BUK TYPE T001-BUKRS,
      LV_CRAMT TYPE BSAD-DMBTR,
      LV_DBAMT TYPE BSAD-DMBTR,
      LV_CDISC TYPE BSAD-DMBTR.

************************************ PR@$@TH

DATA: LV_IND1 TYPE SY-TABIX,
      LV_IND2  TYPE SY-TABIX,
      LV_IND3 TYPE SY-TABIX,
      LV_IND4  TYPE SY-TABIX,
      LV_INC TYPE SHKZG,
      LV_CRDR TYPE SHKZG.

DATA: IT_EMAIL TYPE TABLE OF ZMAIL_ID,
      WA_EMAIL TYPE ZMAIL_ID.

TYPES: BEGIN OF TEM_KNA1,
       KUNNR TYPE KNA1-KUNNR,
       NAME1 TYPE KNA1-NAME1,
       ADRNR TYPE KNA1-ADRNR,
       END OF TEM_KNA1.
DATA: Z_KNA1 TYPE TABLE OF TEM_KNA1,
      Y_KNA1 TYPE TEM_KNA1.

TYPES: BEGIN OF TEM_KNB1,
       KUNNR TYPE KNB1-KUNNR,
       BUKRS TYPE KNB1-BUKRS,
       END OF TEM_KNB1.

DATA: Z_KNB1 TYPE TABLE OF TEM_KNB1,
      Y_KNB1 TYPE TEM_KNB1.

DATA: I_OTF       TYPE ITCOO    OCCURS 0 WITH HEADER LINE,
      I_TLINE     LIKE TLINE    OCCURS 0 WITH HEADER LINE,
      I_RECORD    LIKE SOLISTI1 OCCURS 0 WITH HEADER LINE,
      I_XSTRING   TYPE XSTRING,
* Objects to send mail.
      I_OBJPACK   LIKE SOPCKLSTI1 OCCURS 0 WITH HEADER LINE,
      I_OBJTXT    LIKE SOLISTI1   OCCURS 0 WITH HEADER LINE,
      I_OBJBIN    LIKE SOLIX      OCCURS 0 WITH HEADER LINE,
      I_OBJBIN1    LIKE SOLIX      OCCURS 0 WITH HEADER LINE,
      I_OBJBIN_F    LIKE SOLIX      OCCURS 0 WITH HEADER LINE,
      I_RECLIST   LIKE SOMLRECI1  OCCURS 0 WITH HEADER LINE,
* Work Area declarations
      WA_OBJHEAD  TYPE SOLI_TAB,
      W_CTRLOP    TYPE SSFCTRLOP,
      W_COMPOP    TYPE SSFCOMPOP,
      W_RETURN    TYPE SSFCRESCL,
      WA_BUFFER   TYPE STRING,
* Variables declarations
      V_FORM_NAME TYPE RS38L_FNAM,
      V_LEN_IN    LIKE SOOD-OBJLEN.
DATA: S_EMAIL TYPE  ADR6-SMTP_ADDR.
*DATA: S_EMAIL1 TYPE  ADR6-SMTP_ADDR VALUE 'prasath@sphinaxinfosystems.com'.

DATA: ASCII_DATA     TYPE SOLISTI1,   "your data returned by SO_DOCUMENT_READ_API1
      STRING_DATA    TYPE STRING,
      XSTRING_DATA   TYPE XSTRING.

DATA: SM_NAME TYPE KNA1-NAME1.
DATA: COUNT TYPE I VALUE 1 .

TYPES: BEGIN OF TY_EMAIL_DEL,
       STATS TYPE I,
       KUNNR TYPE KNA1-KUNNR,
       NAME1 TYPE KNA1-NAME1,
       EMAIL TYPE ADR6-SMTP_ADDR,
       O_BAL TYPE BSAD-DMBTR,
       END OF TY_EMAIL_DEL.

DATA: IT_STS TYPE TABLE OF TY_EMAIL_DEL,
      WA_STS TYPE TY_EMAIL_DEL.

DATA: STS TYPE KNA1-KUNNR.
DATA: O_BAL TYPE BSAD-DMBTR.
DATA: CUR_DAT TYPE SY-DATUM.
DATA: DAY TYPE I.
DATA: F_D TYPE CHAR8,
      T_D TYPE CHAR8.


TYPES: BEGIN OF TY_BUDAT,
  LOW TYPE BSID-BUDAT,
  HIGH TYPE BSID-BUDAT,
  END OF TY_BUDAT.

TYPES: BEGIN OF TY_EMAIL_CHAR,
STATS(1) TYPE C,
KUNNR(10) TYPE C,
NAME1(35) TYPE C,
EMAIL(241) TYPE C,
O_BAL(15) TYPE C,
END OF TY_EMAIL_CHAR.

DATA: WA_STS_CHAR TYPE TY_EMAIL_CHAR.

DATA:   IT_MESSAGE TYPE STANDARD TABLE OF SOLISTI1 INITIAL SIZE 0
              WITH HEADER LINE.
DATA:   IT_ATTACH TYPE STANDARD TABLE OF SOLISTI1 INITIAL SIZE 0
              WITH HEADER LINE.

CONSTANTS: CON_TAB  TYPE C VALUE CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB,
           CON_CRET TYPE C VALUE CL_ABAP_CHAR_UTILITIES=>CR_LF.

DATA:   T_PACKING_LIST LIKE SOPCKLSTI1 OCCURS 0 WITH HEADER LINE,
        T_CONTENTS LIKE SOLISTI1 OCCURS 0 WITH HEADER LINE,
        T_RECEIVERS LIKE SOMLRECI1 OCCURS 0 WITH HEADER LINE,
        T_ATTACHMENT LIKE SOLISTI1 OCCURS 0 WITH HEADER LINE,
        T_OBJECT_HEADER LIKE SOLISTI1 OCCURS 0 WITH HEADER LINE,
        W_CNT TYPE I,
        W_SENT_ALL(1) TYPE C,
        W_DOC_DATA LIKE SODOCCHGI1,
        GD_ERROR    TYPE SY-SUBRC,
        GD_RECIEVER TYPE SY-SUBRC.

DATA: P_EMAIL   TYPE SOMLRECI1-RECEIVER VALUE 'ramachandran@sphinaxinfosystems.com'.
DATA: TOT TYPE INT2.
DATA: TOT_C(6) TYPE C.

********************************* PR@$@TH

SELECTION-SCREEN: BEGIN OF BLOCK EMAIL_REP WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: SO_KUNNR FOR KNA1-KUNNR.
PARAMETERS: SO_BUKRS LIKE KNB1-BUKRS.
SELECT-OPTIONS: SO_BUDAT FOR BSID-BUDAT.
SELECTION-SCREEN: END OF BLOCK EMAIL_REP.

INITIALIZATION.
  SO_BUKRS = 1000 .
  CUR_DAT = SY-DATUM.
  DATA: ID_PAR_DAYS	TYPE T009B-BUTAG ,
        ID_PAR_MONTH  TYPE T009B-BUMON ,
        ID_PAR_YEAR	TYPE T009B-BDATJ .

  ID_PAR_MONTH =  CUR_DAT+4(2) .
  ID_PAR_YEAR = CUR_DAT(4) .

  IF CUR_DAT+4(2) = 01 .
    ID_PAR_YEAR = ID_PAR_YEAR - 1 .

    CONCATENATE  ID_PAR_YEAR'1201' INTO SO_BUDAT-LOW .
    CONCATENATE ID_PAR_YEAR'1231' INTO SO_BUDAT-HIGH .
  ELSE.
    ID_PAR_MONTH = ID_PAR_MONTH - 1 .

    CALL FUNCTION 'NUMBER_OF_DAYS_PER_MONTH_GET'
      EXPORTING
        PAR_MONTH = ID_PAR_MONTH
        PAR_YEAR  = ID_PAR_YEAR
      IMPORTING
        PAR_DAYS  = ID_PAR_DAYS.

    CONCATENATE  ID_PAR_YEAR ID_PAR_MONTH '01' INTO SO_BUDAT-LOW .
    CONCATENATE ID_PAR_YEAR ID_PAR_MONTH ID_PAR_DAYS INTO SO_BUDAT-HIGH .
  ENDIF.
  APPEND: SO_BUDAT.


START-OF-SELECTION.
  PERFORM: FETCHDATA.
  PERFORM: STATUS_EMAIL.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  FETCHDATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FETCHDATA .
 DATA: lr_kunnr TYPE RANGE OF KNA1-kunnr.


 SELECT kunnr FROM ZCUS_EXCL_STMNT INTO TABLE @DATA(lt_kunnr).
"write the column carrid of your internal table in your range
 lr_kunnr = VALUE #( FOR <fs_kunnr> IN lt_kunnr
                    (
                      sign = 'I'
                      option = 'EQ'
                      low = <fs_kunnr>-kunnr
                      high = ''
                     ) ).

  SELECT KUNNR BUKRS FROM KNB1 INTO TABLE Z_KNB1 WHERE BUKRS EQ SO_BUKRS AND LOEVM <> 'X' AND KUNNR IN SO_KUNNR .
  SORT Z_KNB1 BY KUNNR.
  IF lr_kunnr IS NOT INITIAL.
    DELETE Z_KNB1 WHERE KUNNR IN LR_KUNNR.
  ENDIF.

  SELECT KUNNR NAME1 ADRNR FROM KNA1 INTO TABLE Z_KNA1 FOR ALL ENTRIES IN Z_KNB1 WHERE KUNNR EQ Z_KNB1-KUNNR.
  SELECT * FROM ZMAIL_ID INTO TABLE IT_EMAIL.
  DELETE IT_EMAIL WHERE EMAIL_ID IS INITIAL .
*  SORT Z_KNB1 BY KUNNR.
  IF Z_KNB1 IS NOT INITIAL.

    LOOP AT Z_KNB1 INTO Y_KNB1.
      DATA: E_C(1) TYPE C.
      CLEAR: I_OTF[],I_TLINE[],I_RECORD[],I_XSTRING,I_OBJPACK[],I_OBJTXT[],I_OBJBIN[],I_RECLIST[],WA_OBJHEAD[],W_CTRLOP,W_COMPOP,W_RETURN,WA_BUFFER,V_LEN_IN.
      REFRESH:IT_ATTACH[] .
      READ TABLE Z_KNA1 INTO Y_KNA1 WITH KEY KUNNR = Y_KNB1-KUNNR.
      CLEAR: SM_NAME,STS.
      SM_NAME = Y_KNA1-NAME1.
      STS = Y_KNA1-KUNNR.
      CLEAR:S_EMAIL,WA_EMAIL.
      IF Y_KNA1-ADRNR IS NOT INITIAL.
*        SELECT SINGLE SMTP_ADDR FROM ADR6 INTO S_EMAIL WHERE ADDRNUMBER EQ Y_KNA1-ADRNR."Commented by SPLABAP during code remediation
        "Added by SPLABAP during code remediation
        SELECT SMTP_ADDR UP TO 1 ROWS
          FROM ADR6 INTO S_EMAIL WHERE ADDRNUMBER EQ Y_KNA1-ADRNR
          ORDER BY PRIMARY KEY.
ENDSELECT.
        READ TABLE IT_EMAIL INTO WA_EMAIL WITH KEY EMAIL_ID = S_EMAIL.
      ENDIF.
      IF S_EMAIL IS NOT INITIAL AND WA_EMAIL IS INITIAL.
        CLEAR: LV_BUK.
        SELECT BUKRS  FROM T001 INTO LV_BUK WHERE BUKRS EQ Y_KNB1-BUKRS.
          CLEAR: GT_BSAD.
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
             BUKRS
             GJAHR
             SKNTO
             UMSKS
             AUGDT
             ZUONR
             BUZEI FROM BSAD INTO CORRESPONDING FIELDS OF TABLE GT_BSAD WHERE BUDAT <= SO_BUDAT-HIGH AND BUDAT >= SO_BUDAT-LOW
                                                            AND BUKRS EQ Y_KNB1-BUKRS AND KUNNR EQ Y_KNB1-KUNNR AND UMSKZ <> 'H' .

          CLEAR:GT_BSID.
          SELECT
             PRCTR
             KUNNR
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
             UMSKZ
             BUKRS
             GJAHR
             SKNTO FROM BSID INTO CORRESPONDING FIELDS OF TABLE GT_BSID WHERE BUDAT <= SO_BUDAT-HIGH
                                                  AND BUDAT >= SO_BUDAT-LOW AND BUKRS EQ Y_KNB1-BUKRS AND KUNNR EQ Y_KNB1-KUNNR AND UMSKZ <> 'H'.

          CLEAR:GT_BSAD1.
          SELECT PRCTR KUNNR UMSKZ AUGBL BUDAT BLDAT BLART BSCHL ZFBDT DMBTR SHKZG XBLNR BELNR GSBER SGTXT VBELN BUKRS GJAHR SKNTO UMSKS AUGDT ZUONR BUZEI FROM BSAD
            INTO CORRESPONDING FIELDS OF TABLE GT_BSAD1
                  WHERE  BUDAT BETWEEN '01.04.2014'
                   AND SO_BUDAT-LOW
                   AND BUKRS EQ Y_KNB1-BUKRS
                   AND KUNNR EQ Y_KNB1-KUNNR
                   AND UMSKZ <> 'H' .

          CLEAR: GT_BSID1.
          SELECT PRCTR KUNNR AUGBL BUDAT BLDAT BLART BSCHL ZFBDT DMBTR SHKZG XBLNR BELNR GSBER SGTXT VBELN UMSKZ BUKRS GJAHR SKNTO FROM BSID
            INTO CORRESPONDING FIELDS OF TABLE GT_BSID1
                  WHERE BUDAT BETWEEN '01.04.2014'
                  AND  SO_BUDAT-LOW
                  AND BUKRS EQ Y_KNB1-BUKRS
                   AND KUNNR EQ Y_KNB1-KUNNR
                   AND UMSKZ <> 'H'.

          CLEAR:GT_BSAD7.
          APPEND LINES OF GT_BSID TO GTT_BSID.
          SORT GT_BSAD BY BELNR AUGBL.
          APPEND LINES OF GT_BSAD TO GT_BSID.
          APPEND LINES OF GT_BSAD TO GT_BSAD7.
          DELETE GT_BSAD7 WHERE SHKZG = 'S'.
          APPEND LINES OF GT_BSAD1 TO GT_BSID1.

          IF GT_BSID[] IS NOT INITIAL.

            REFRESH :GT_BSAK.
            "SELECT LIFNR UMSKZ BELNR XBLNR BLDAT DMBTR BUDAT BLART BSCHL SGTXT GSBER SHKZG BUKRS GJAHR SKNTO
            SELECT BUKRS LIFNR UMSKS UMSKZ AUGDT AUGBL ZUONR GJAHR BELNR BUZEI BUDAT BLDAT XBLNR BLART DMBTR BSCHL SGTXT GSBER SHKZG SKNTO
             FROM BSAK INTO TABLE  GT_BSAK
              FOR ALL ENTRIES IN GT_BSID
              WHERE BELNR = GT_BSID-BELNR
              AND   BUKRS = GT_BSID-BUKRS
              AND   GJAHR = GT_BSID-GJAHR.

            CLEAR: GT_KNA1[].
            SELECT KUNNR NAME1 ADRNR LIFNR FROM KNA1
              INTO TABLE GT_KNA1
              FOR ALL ENTRIES IN GT_BSID
               WHERE KUNNR = GT_BSID-KUNNR.

            CLEAR: GT_T003T.
            SELECT BLART LTEXT SPRAS FROM  T003T"#EC CI_NOORDER  "Added by SPLABAP during code remediation
              INTO TABLE GT_T003T
              FOR ALL ENTRIES IN GT_BSID
              WHERE BLART = GT_BSID-BLART
               AND SPRAS = 'E'.

            CLEAR:GT_T003T1.
            SELECT  BLART LTEXT SPRAS FROM  T003T"#EC CI_NOORDER  "Added by SPLABAP during code remediation
              INTO TABLE GT_T003T1
              FOR ALL ENTRIES IN GT_BSAD
              WHERE BLART = GT_BSAD-BLART
              AND SPRAS = 'E'.

            APPEND  LINES OF GT_T003T1 TO GT_T003T.

            CLEAR:GT_T003T2.
            SELECT  BLART LTEXT SPRAS FROM  T003T"#EC CI_NOORDER  "Added by SPLABAP during code remediation
              INTO TABLE GT_T003T2
              FOR ALL ENTRIES IN GT_BSIK
              WHERE BLART = GT_BSIK-BLART
              AND SPRAS = 'E'.

            APPEND  LINES OF GT_T003T2 TO GT_T003T.

          ENDIF.
          IF GT_KNA1[] IS  NOT INITIAL.
            CLEAR: GT_ADRC.
            SELECT  ADDRNUMBER STREET CITY1 CITY2 POST_CODE1 FROM ADRC
              INTO TABLE GT_ADRC
              FOR ALL ENTRIES IN GT_KNA1
              WHERE ADDRNUMBER = GT_KNA1-ADRNR.

          ENDIF.
          CLEAR: GT_KNC1.
          IF GT_BSID IS NOT INITIAL.
            SELECT  KUNNR BUKRS UMSAV GJAHR FROM KNC1
              INTO TABLE GT_KNC1
              FOR ALL ENTRIES IN GT_BSID
               WHERE KUNNR = GT_BSID-KUNNR
              AND GJAHR = GT_BSID-GJAHR
              AND BUKRS = GT_BSID-BUKRS.
          ENDIF.
        ENDSELECT.

        CLEAR: LV_TOTAL.
        LOOP AT GT_KNC1 INTO WA_KNC1.
          LV_TOTAL =  WA_KNC1-UMSAV.
        ENDLOOP.

        CLEAR:WA_KNC1.
****************Created by prabhu on 18.09.2020
***      CLEAR DBT_AMT.

        LOOP AT GTT_BSID INTO WAT_BSID.

          WAT_FINAL-KUNNR = WAT_BSID-KUNNR.
          WAT_FINAL-BELNR = WAT_BSID-BELNR.
          WAT_FINAL-SHKZG  = WAT_BSID-SHKZG.
          WAT_FINAL-BLART  = WAT_BSID-BLART.

          IF WAT_BSID-SHKZG = 'H'.
            WAT_FINAL-CR_AMT = WAT_BSID-DMBTR.

          ELSEIF WAT_BSID-SHKZG = 'S'.
            WAT_FINAL-DR_AMT = WAT_BSID-DMBTR.

          ENDIF.

          APPEND WAT_FINAL TO  ITT_FINAL.
          CLEAR WAT_FINAL.

        ENDLOOP.


        LOOP AT ITT_FINAL INTO WAT_FINAL WHERE BLART = 'AB'.

          LV_IND4 = SY-TABIX.

          CLEAR LV_CRDR.

          IF WAT_FINAL-SHKZG = 'H'.
            LV_CRDR = 'S'.

          ELSEIF WAT_FINAL-SHKZG = 'S'.
            LV_CRDR = 'H'.

          ENDIF.

          READ TABLE  ITT_FINAL INTO WAT_FINAL1 WITH KEY BELNR = WAT_FINAL-BELNR
                                                SHKZG = LV_CRDR.

          IF SY-SUBRC = 0 AND

             WAT_FINAL1-CR_AMT + WAT_FINAL1-DR_AMT =  WAT_FINAL-CR_AMT +  WAT_FINAL-DR_AMT.

            LV_IND3 = SY-TABIX.

            DELETE ITT_FINAL INDEX LV_IND3.
            DELETE ITT_FINAL INDEX LV_IND4.
            CLEAR: WAT_FINAL1 ,WAT_FINAL,LV_IND3,LV_IND4.
          ENDIF.

        ENDLOOP.

***      LOOP AT ITT_FINAL INTO WAT_FINAL.
***
***        IF WAT_FINAL-SHKZG = 'H'.
***          DBT_AMT =  DBT_AMT + WAT_FINAL-CR_AMT.
***        ENDIF.
***        CLEAR WAT_FINAL.
***
***      ENDLOOP.

****************Created by prabhu on 18.09.2020

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




        SORT GT_BSID BY BUDAT BELNR BLART SHKZG.

        CLEAR : SY-SUBRC .
        LOOP AT GT_BSID INTO WA_BSID .

          IF SY-SUBRC = 0.
            WA_FINAL-XBLNR = WA_BSID-XBLNR.
            WA_FINAL-BLDAT = WA_BSID-BLDAT.
            WA_FINAL-KUNNR = WA_BSID-KUNNR.
            WA_FINAL-BELNR = WA_BSID-BELNR.
            WA_FINAL-BUDAT = WA_BSID-BUDAT.
            WA_FINAL-BSCHL = WA_BSID-BSCHL.
            WA_FINAL-GSBER = WA_BSID-GSBER.
            WA_FINAL-SGTXT = WA_BSID-SGTXT.
            WA_FINAL-VBELN = WA_BSID-VBELN.
            WA_FINAL-UMSKZ = WA_BSID-UMSKZ.
            WA_FINAL-BUKRS = WA_BSID-BUKRS.
            WA_FINAL-GJAHR = WA_BSID-GJAHR.

          ENDIF.

          READ TABLE GT_T003T INTO WA_T003T WITH KEY BLART = WA_BSID-BLART.

          WA_FINAL-LTEXT = WA_T003T-LTEXT.
          WA_FINAL-BLART = WA_BSID-BLART.

          READ TABLE GT_KNA1 INTO WA_KNA1 WITH KEY KUNNR = WA_BSID-KUNNR.

          WA_FINAL-NAME1 = WA_KNA1-NAME1.
          WA_FINAL-SHKZG = WA_BSID-SHKZG.


          IF GS_CHK-BUDAT = WA_BSID-BUDAT
            AND GS_CHK-BLART = WA_BSID-BLART
            AND GS_CHK-BELNR = WA_BSID-BELNR
            AND GS_CHK-SHKZG = WA_BSID-SHKZG
            AND WA_BSID-SHKZG = 'H'.

            WA_FINAL-SKNTO = LV_CDISC + WA_BSID-SKNTO.
            LV_CDISC = WA_FINAL-SKNTO.
            WA_FINAL-CR_AMT = LV_CRAMT + WA_BSID-DMBTR .
            LV_CRAMT = WA_FINAL-CR_AMT.

            DELETE GT_FINAL INDEX LV_IND.

          ELSEIF GS_CHK-BUDAT = WA_BSID-BUDAT

            AND GS_CHK-BLART = WA_BSID-BLART
            AND GS_CHK-BELNR = WA_BSID-BELNR
            AND GS_CHK-SHKZG = WA_BSID-SHKZG
            AND WA_BSID-SHKZG = 'S'.

            WA_FINAL-DR_AMT = LV_DBAMT + WA_BSID-DMBTR.
            LV_DBAMT = WA_FINAL-DR_AMT.

            DELETE GT_FINAL INDEX LV_IND.

          ELSEIF GS_CHK-BUDAT = WA_BSID-BUDAT
            AND GS_CHK-BLART = WA_BSID-BLART
            AND GS_CHK-BELNR = WA_BSID-BELNR
            AND GS_CHK-SHKZG = WA_BSID-SHKZG
            AND WA_BSID-SHKZG = 'C'
            AND WA_T003T-BLART = 'AB'.

            WA_FINAL-CR_AMT = LV_CRAMT + WA_BSID-DMBTR.
            LV_CRAMT = WA_FINAL-CR_AMT.

            DELETE GT_FINAL INDEX LV_IND.

          ELSEIF GS_CHK-BUDAT = WA_BSID-BUDAT
            AND GS_CHK-BLART = WA_BSID-BLART
            AND GS_CHK-BELNR = WA_BSID-BELNR
            AND WA_BSID-SHKZG = 'D'
            AND WA_T003T-BLART = 'AB'.

            WA_FINAL-DR_AMT = LV_DBAMT + WA_BSID-DMBTR.
            LV_DBAMT = WA_FINAL-DR_AMT.

            DELETE GT_FINAL INDEX LV_IND.

          ELSEIF ( GS_CHK-BUDAT <> WA_BSID-BUDAT
                    AND GS_CHK-BLART <> WA_BSID-BLART
                    AND GS_CHK-BELNR <> WA_BSID-BELNR
                    AND WA_BSID-SHKZG <> 'S' )
              OR ( ( GS_CHK-BUDAT <> WA_BSID-BUDAT
                    OR GS_CHK-BLART <> WA_BSID-BLART
                    OR GS_CHK-BELNR <> WA_BSID-BELNR )
                  AND WA_BSID-SHKZG <> 'S' )
              OR ( GS_CHK-SHKZG <> WA_BSID-SHKZG
                   AND WA_BSID-SHKZG <> 'S' ) .

            WA_FINAL-SKNTO = WA_BSID-SKNTO.
            LV_CDISC = WA_FINAL-SKNTO .
            WA_FINAL-CR_AMT = WA_BSID-DMBTR.
            LV_CRAMT = WA_FINAL-CR_AMT .

          ELSEIF ( GS_CHK-BUDAT <> WA_BSID-BUDAT
                  AND GS_CHK-BLART <> WA_BSID-BLART
                  AND GS_CHK-BELNR <> WA_BSID-BELNR
                  AND WA_BSID-SHKZG <> 'H' )
              OR ( ( GS_CHK-BUDAT <> WA_BSID-BUDAT
                    OR GS_CHK-BLART <> WA_BSID-BLART
                    OR GS_CHK-BELNR <> WA_BSID-BELNR )
                 AND WA_BSID-SHKZG <> 'H' )
             OR ( GS_CHK-SHKZG <> WA_BSID-SHKZG
                 AND WA_BSID-SHKZG <> 'H' ) .

            WA_FINAL-DR_AMT = WA_BSID-DMBTR .
            LV_DBAMT = WA_FINAL-DR_AMT  .

          ELSEIF ( GS_CHK-BUDAT <> WA_BSID-BUDAT
                  AND GS_CHK-BLART <> WA_BSID-BLART
                  AND GS_CHK-BELNR <> WA_BSID-BELNR
                  AND GS_CHK-SHKZG <> WA_BSID-SHKZG
                  AND WA_BSID-SHKZG = 'C' )
               OR  WA_T003T-BLART = 'AB'.

            WA_FINAL-CR_AMT = WA_BSID-DMBTR.
            LV_CRAMT = WA_FINAL-CR_AMT.

          ELSEIF ( GS_CHK-BUDAT <> WA_BSID-BUDAT
                  AND GS_CHK-BLART <> WA_BSID-BLART
                  AND GS_CHK-BELNR <> WA_BSID-BELNR
               AND GS_CHK-SHKZG <> WA_BSID-SHKZG
               AND WA_BSID-SHKZG = 'D')
               OR WA_T003T-BLART = 'AB'.

            WA_FINAL-DR_AMT = WA_BSID-DMBTR.
            LV_DBAMT = WA_FINAL-DR_AMT.

          ENDIF.
          GS_CHK-BELNR = WA_BSID-BELNR.
          GS_CHK-BUDAT = WA_BSID-BUDAT.
          GS_CHK-BLART = WA_BSID-BLART.
          GS_CHK-SHKZG = WA_BSID-SHKZG.


          APPEND WA_FINAL TO GT_FINAL.
          CLEAR : WA_FINAL,WA_BSID.

          DESCRIBE TABLE GT_FINAL LINES LV_IND.

        ENDLOOP.



        "added  by jestop jeswin on 8/10/2020


        DATA: TEMP TYPE BSAK-BELNR,
              INDEX TYPE SY-TABIX.
        CLEAR TEMP.

        LOOP AT GT_FINAL INTO WA_FINAL WHERE BLART = 'AB'.
          READ TABLE GT_BSAK INTO WA_BSAK WITH KEY BELNR = WA_FINAL-BELNR GJAHR = WA_FINAL-GJAHR BUKRS = WA_FINAL-BUKRS.
          LOOP AT GT_BSAK INTO WA_BSAK WHERE BELNR = WA_FINAL-BELNR.
            IF WA_BSAK-SHKZG IS NOT INITIAL .
              WA_BSAK3-DMBTR = WA_BSAK-DMBTR.
              WA_BSAK3-BELNR = WA_BSAK-BELNR.
              WA_BSAK3-GJAHR = WA_BSAK-GJAHR.
              WA_BSAK3-BUKRS = WA_BSAK-BUKRS.
              WA_BSAK3-BLDAT = WA_BSAK-BLDAT.
              WA_BSAK3-SHKZG = WA_BSAK-SHKZG.
              WA_BSAK3-BLART = WA_BSAK-BLART.
              WA_BSAK3-LIFNR = WA_FINAL-KUNNR.
              WA_BSAK3-BUDAT = WA_BSAK-BUDAT.
              WA_BSAK3-BUZEI = WA_BSAK-BUZEI.
              WA_BSAK3-BSCHL = WA_BSAK-BSCHL.
              APPEND WA_BSAK3 TO GT_BSAK3.
              CLEAR : WA_FINAL4,WA_BSAK,WA_BSAK3.
            ENDIF.
          ENDLOOP.
        ENDLOOP.
        SORT GT_BSAK3 BY LIFNR BELNR BLDAT DMBTR BUZEI.
        DELETE ADJACENT DUPLICATES FROM GT_BSAK3 COMPARING LIFNR BELNR BLDAT DMBTR BUZEI.
        LOOP AT GT_BSAK3 INTO WA_BSAK3.
          IF WA_BSAK3-SHKZG = 'S'.
            WA_FINAL4-CR_AMT = WA_BSAK3-DMBTR.
          ELSEIF WA_BSAK3-SHKZG = 'H'.
            WA_FINAL4-DR_AMT = WA_BSAK3-DMBTR.
          ENDIF.
          WA_FINAL4-BELNR = WA_BSAK3-BELNR.
          WA_FINAL4-GJAHR = WA_BSAK3-GJAHR.
          WA_FINAL4-BUKRS = WA_BSAK3-BUKRS.
          WA_FINAL4-BLDAT = WA_BSAK3-BLDAT.
          WA_FINAL4-SHKZG = WA_BSAK3-SHKZG.
          WA_FINAL4-BLART = WA_BSAK3-BLART.
          WA_FINAL4-KUNNR = WA_BSAK3-LIFNR.
          WA_FINAL4-BUDAT = WA_BSAK3-BUDAT.
          WA_FINAL4-BSCHL = WA_BSAK3-BSCHL.
          APPEND WA_FINAL4 TO GT_FINAL4.
          CLEAR : WA_FINAL4,WA_BSAK,WA_BSAK3.
        ENDLOOP.
        LOOP AT GT_FINAL4 INTO WA_FINAL4.
          READ TABLE GT_FINAL INTO WA_FINAL WITH KEY KUNNR = WA_FINAL4-KUNNR BELNR = WA_FINAL4-BELNR BLDAT = WA_FINAL4-BLDAT.
          IF SY-SUBRC = 0.
            DELETE GT_FINAL WHERE KUNNR = WA_FINAL4-KUNNR AND BELNR = WA_FINAL4-BELNR AND BLDAT = WA_FINAL4-BLDAT.
          ENDIF.
        ENDLOOP.
        APPEND LINES OF GT_FINAL4 TO GT_FINAL.
        REFRESH : GT_FINAL4.
        CLEAR: GT_BSID[].
        CLEAR: GS_CHK ,WA_BSID, WA_T003T, WA_KNA1 .

        APPEND LINES OF GT_FINAL TO GT_FINAL1.
        APPEND LINES OF GT_FINAL TO GT_FINAL2.



        TYPES: BEGIN OF A,
               LN TYPE I,
               END OF A.
        DATA: GT_LINE TYPE TABLE OF A,
              GS_LINE TYPE A.

        LOOP AT GT_FINAL1 INTO WA_FINAL1.

          IF GS_CHK-BUDAT = WA_FINAL1-BUDAT AND
               GS_CHK-BLART = WA_FINAL1-BLART AND
               GS_CHK-BELNR = WA_FINAL1-BELNR.
            IF WA_FINAL1-CR_AMT GT GS_CHK-CR_AMT.
              GS_LINE-LN = SY-TABIX.
            ENDIF.
          ENDIF.

          GS_CHK-BELNR = WA_FINAL1-BELNR.
          GS_CHK-BUDAT = WA_FINAL1-BUDAT.
          GS_CHK-BLART = WA_FINAL1-BLART.
          GS_CHK-CR_AMT = WA_FINAL1-CR_AMT.
          GS_CHK-DR_AMT = WA_FINAL1-DR_AMT.

        ENDLOOP.

        B_DATE = SO_BUDAT-HIGH.
        SORT GT_FINAL BY BUDAT SHKZG.

        LOOP AT GT_FINAL INTO WA_FINAL.


          READ TABLE GT_FINAL2 INTO WA_FINAL2 WITH KEY BELNR = WA_FINAL-BELNR
                                                       SHKZG = WA_FINAL-SHKZG .

          IF WA_FINAL2-SKNTO IS NOT INITIAL .

            WA_FINAL2-CR_AMT = WA_FINAL2-CR_AMT - WA_FINAL2-SKNTO.

            APPEND WA_FINAL2 TO GT_FINAL3.
            CLEAR: WA_FINAL2-CR_AMT.

            WA_FINAL2-CR_AMT = WA_FINAL2-SKNTO.
            WA_FINAL2-LV_COUNT = 1.
            APPEND WA_FINAL2 TO GT_FINAL3.
            DELETE GT_FINAL WHERE BELNR = WA_FINAL2-BELNR AND SHKZG = 'H' .
          ENDIF.

        ENDLOOP.

        APPEND LINES OF GT_FINAL3 TO GT_FINAL.

        LOOP AT GT_FINAL INTO WA_FINAL.

          LV_SUM = LV_SUM + WA_FINAL-DR_AMT - WA_FINAL-CR_AMT .
          WA_FINAL-BAL_AMT = LV_SUM.

          MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING BAL_AMT.

          AT END OF KUNNR.
            CLEAR :LV_SUM,WA_FINAL,WA_BSID.
          ENDAT.

        ENDLOOP.

        LOOP AT GT_FINAL INTO WA_FINAL WHERE KUNNR = ' '.
          WA_FINAL-KUNNR = WA_KNA1-KUNNR.

          MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING KUNNR.
          CLEAR  WA_FINAL.

        ENDLOOP.

        CLEAR: RV_OPN.
        RV_OPN = LV_TOTAL - LV_OPN.
        SORT GT_FINAL BY BUDAT SHKZG.

        LOOP AT GT_FINAL INTO WA_FINAL .


          LV_IND2 = SY-TABIX.

          IF WA_FINAL-BLART = 'AB'.
            CLEAR LV_INC.
            IF WA_FINAL-SHKZG = 'H'.
              LV_INC = 'S'.

            ELSEIF WA_FINAL-SHKZG = 'S'.
              LV_INC = 'H'.

            ENDIF.

            READ TABLE  GT_FINAL INTO WA_FINAL_T WITH KEY BELNR = WA_FINAL-BELNR
                                                          SHKZG = LV_INC.

            IF SY-SUBRC = 0 AND
              WA_FINAL_T-CR_AMT + WA_FINAL_T-DR_AMT = WA_FINAL-CR_AMT + WA_FINAL-DR_AMT.

              LV_IND1 = SY-TABIX.

              DELETE GT_FINAL INDEX LV_IND1.
              DELETE GT_FINAL INDEX LV_IND2.

              CLEAR: WA_FINAL ,WA_FINAL_T,LV_IND1,LV_IND2.

            ENDIF.

          ENDIF.

          CASE WA_FINAL-BLART.
            WHEN 'RV'.
              WA_FINAL-T_TTTEXT = 'INVOICE'.
            WHEN 'DG'.
              WA_FINAL-T_TTTEXT = 'CREDIT NOTE'.
            WHEN 'DZ'.
              IF WA_FINAL-LV_COUNT IS NOT INITIAL.
                WA_FINAL-T_TTTEXT = 'AUTO CREDIT NOTE'.
                CLEAR WA_FINAL-LV_COUNT.
              ELSE.
                WA_FINAL-T_TTTEXT = 'RECEIPT'.

              ENDIF.
            WHEN 'VG'.
              WA_FINAL-T_TTTEXT = 'SALES RETURN'.
            WHEN 'DA'.
              WA_FINAL-T_TTTEXT = 'CHEQUE RETURN'.
            WHEN 'DR'.
              WA_FINAL-T_TTTEXT = 'DEBIT NOTE'.
            WHEN 'AB'.
              CASE WA_FINAL-BSCHL .
                WHEN 40.
                  WA_FINAL-T_TTTEXT = 'AUTO CREDIT NOTE'.
                WHEN 17.
                  IF WA_FINAL-SKNTO IS NOT INITIAL.
                    WA_FINAL-T_TTTEXT = 'AUTO CREDIT NOTE'.
                    CLEAR WA_FINAL-SKNTO.
                  ELSE.
                    WA_FINAL-T_TTTEXT = 'FREIGHT / INTEREST'.
                  ENDIF.
                WHEN 27.
                  WA_FINAL-T_TTTEXT = 'FREIGHT / INTEREST'.
                WHEN 04.
                  WA_FINAL-T_TTTEXT = 'FREIGHT / INTEREST'.
                WHEN OTHERS.
                  WA_FINAL-T_TTTEXT = 'ACCOUNTING DOCUMENT'.
              ENDCASE.
            WHEN 'KR'.
              WA_FINAL-T_TTTEXT = 'COMMISSION'.
            WHEN 'SA'.
              WA_FINAL-T_TTTEXT = 'JOURNAL ENTRY'.
            WHEN OTHERS.
              LV_FLAG2 = ABAP_TRUE.
          ENDCASE.
          IF LV_FLAG2 IS INITIAL.
            MODIFY GT_FINAL FROM WA_FINAL INDEX LV_IND2.

          ENDIF.


          CLEAR LV_FLAG2.
*** created on 17.09.2020
          CLEAR:WA_FINAL.
        ENDLOOP.

        CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
          EXPORTING
            FORMNAME           = 'ZSF_CUS_ACCOUNT_LEDGER_MAIL'
*           VARIANT            = ' '
*           DIRECT_CALL        = ' '
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

        W_CTRLOP-GETOTF = ABAP_TRUE.
        W_CTRLOP-NO_DIALOG = ABAP_TRUE.
        W_COMPOP-TDNOPREV = ABAP_TRUE.
        W_CTRLOP-PREVIEW = SPACE.
        W_COMPOP-TDDEST = 'LOCL'.

        IF GT_FINAL[] IS NOT INITIAL.

          PERFORM: EXCEL_ATTACH.

          CLEAR O_BAL.
          O_BAL = LV_OPN .
          CALL FUNCTION FM_NAME "'/1BCDWB/SF00000509'
            EXPORTING
*         ARCHIVE_INDEX              =
*         ARCHIVE_INDEX_TAB          =
*         ARCHIVE_PARAMETERS         =
           CONTROL_PARAMETERS         = W_CTRLOP
*         MAIL_APPL_OBJ              =
*         MAIL_RECIPIENT             =
*         MAIL_SENDER                =
           OUTPUT_OPTIONS             = W_COMPOP
           USER_SETTINGS              = ABAP_TRUE "'X'
              BAL_DATE                   = B_DATE
              LV_FRDT                    = SO_BUDAT-LOW
              LV_TODT                    = SO_BUDAT-HIGH
              LV_OPN                     = LV_OPN
              LV_BUK                     = LV_BUK
           IMPORTING
*         DOCUMENT_OUTPUT_INFO       =
              JOB_OUTPUT_INFO            = W_RETURN
*         JOB_OUTPUT_OPTIONS         =
            TABLES
              GT_KNA1                    = GT_KNA1[]
              GT_BSID                    = GT_FINAL[]
              ITT_FINAL                  = ITT_FINAL[]
       EXCEPTIONS
         FORMATTING_ERROR           = 1
         INTERNAL_ERROR             = 2
         SEND_ERROR                 = 3
         USER_CANCELED              = 4
         OTHERS                     = 5.
          IF SY-SUBRC <> 0.
*        Implement suitable error handling here
          ENDIF.

          I_OTF[] = W_RETURN-OTFDATA[].

          CALL FUNCTION 'CONVERT_OTF'
            EXPORTING
              FORMAT                = 'PDF'  "'ASCII'
              MAX_LINEWIDTH         = 132
*             ARCHIVE_INDEX         = ' '
*             COPYNUMBER            = 0
*             ASCII_BIDI_VIS2LOG    = ' '
*             PDF_DELETE_OTFTAB     = ' '
*             PDF_USERNAME          = ' '
*             PDF_PREVIEW           = ' '
*             USE_CASCADING         = ' '
*             MODIFIED_PARAM_TABLE  =
            IMPORTING
              BIN_FILESIZE          = V_LEN_IN
              BIN_FILE              = I_XSTRING
            TABLES
              OTF                   = I_OTF
              LINES                 = I_TLINE
            EXCEPTIONS
              ERR_MAX_LINEWIDTH     = 1
              ERR_FORMAT            = 2
              ERR_CONV_NOT_POSSIBLE = 3
              ERR_BAD_OTF           = 4
              OTHERS                = 5.
          IF SY-SUBRC <> 0.
*          Implement suitable error handling here
          ENDIF.

          CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
            EXPORTING
              BUFFER                = I_XSTRING
*         APPEND_TO_TABLE       = ' '
*       IMPORTING
*         OUTPUT_LENGTH         =
            TABLES
              BINARY_TAB            = I_OBJBIN[].

          DATA: IN_MAILID TYPE AD_SMTPADR.
          CLEAR IN_MAILID.

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ADDED BY PR@$@TH (testing purpose)
*        CLEAR: S_EMAIL.
*        IF E_C = 'X'.
*          S_EMAIL = 'ramachandran@sphinaxinfosystems.com'.
*          CLEAR: E_C .
*        ELSE.
*          S_EMAIL = 'prasath@sphinaxinfosystems.com' .
*          E_C = 'X' .
*        ENDIF.
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ENDED BY PR@$@TH

          IN_MAILID = S_EMAIL .
          PERFORM SEND_MAIL USING IN_MAILID .
          COMMIT WORK.
          COUNT = COUNT + 1 .
        ENDIF.
        CLEAR: GT_FINAL[] , WA_FINAL , GT_FINAL1[] ,WA_FINAL1 ,GT_FINAL2[] ,WA_FINAL2 ,GT_FINAL3[] ,WA_FINAL3 , LV_OPN , LV_BUK ,GT_KNA1[],GT_BSAK3,GT_FINAL4.
        REFRESH : GT_FINAL[] ,GT_BSID[],GT_BSAD[],GT_BSAK[],GT_BSIK[],GT_BSAD1[],GT_BSID1[],ITT_FINAL[],I_OTF[],I_TLINE[],GT_BSAK3[],GT_FINAL4[].
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " FETCHDATA

*&---------------------------------------------------------------------*
*&      Form  SEND_MAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IN_MAILID  text
*----------------------------------------------------------------------*
FORM SEND_MAIL USING IN_MAILID.

  DATA: SALUTATION TYPE STRING.
  DATA: BODY TYPE STRING.
  DATA: FOOTER TYPE STRING.
  DATA: SUB TYPE CHAR50.
  DATA: BODY1(500) TYPE C.
  DATA: TEM_BDY TYPE STRING.

  DATA: LO_SEND_REQUEST TYPE REF TO CL_BCS,
        LO_DOCUMENT     TYPE REF TO CL_DOCUMENT_BCS,
        LO_SENDER       TYPE REF TO IF_SENDER_BCS,
        LO_RECIPIENT    TYPE REF TO IF_RECIPIENT_BCS VALUE IS INITIAL,LT_MESSAGE_BODY TYPE BCSY_TEXT,
        LX_DOCUMENT_BCS TYPE REF TO CX_DOCUMENT_BCS,
        LV_SENT_TO_ALL  TYPE OS_BOOLEAN.

  DATA: SENDER_MAIL TYPE  ADR6-SMTP_ADDR VALUE 'CustomerConfirmation@sheenlac.in' .

  DATA:SM_NAM TYPE SO_OBJ_DES.
  DATA:HEADER TYPE CHAR100.
  DATA:CONTENT TYPE CHAR100.
  CLEAR: SALUTATION , BODY ,FOOTER ,LO_SEND_REQUEST ,LO_DOCUMENT ,LO_SENDER ,LO_RECIPIENT ,LV_SENT_TO_ALL.

  LO_SEND_REQUEST = CL_BCS=>CREATE_PERSISTENT( ).

  CLEAR: LT_MESSAGE_BODY.
  TRANSLATE SM_NAME TO LOWER CASE.
  DATA: F_DAT TYPE CHAR10,
        T_DAT TYPE CHAR10.
  CONCATENATE SO_BUDAT-LOW+6(2) '.' SO_BUDAT-LOW+4(2) '.' SO_BUDAT-LOW(4) INTO F_DAT.
  CONCATENATE SO_BUDAT-HIGH+6(2) '.' SO_BUDAT-HIGH+4(2) '.' SO_BUDAT-HIGH(4) INTO T_DAT.
  FOOTER = 'Dear Sir/Madam,'.
  APPEND FOOTER TO LT_MESSAGE_BODY.
  CLEAR: FOOTER.

  FOOTER = ' '.
  APPEND FOOTER TO LT_MESSAGE_BODY.
  CLEAR: FOOTER.

  CONCATENATE '  We are pleased to sending you the confirmation of balance for the period ending(' T_DAT ')' INTO TEM_BDY SEPARATED BY SPACE.

  FOOTER = TEM_BDY .
  APPEND FOOTER TO LT_MESSAGE_BODY.
  CLEAR: FOOTER.

  FOOTER = 'Kindly Confirm the same with in 15 days of else the balance as per our records is treated as correct'.
  APPEND FOOTER TO LT_MESSAGE_BODY.
  CLEAR: FOOTER.

  FOOTER = '-In case of any query Contact ( +91-9445864644 / sathishg@sheenlac.in or +91-7338832775 / manikandan@jnpl.in )'.
  APPEND FOOTER TO LT_MESSAGE_BODY.
  CLEAR: FOOTER.

  FOOTER = ' '.
  APPEND FOOTER TO LT_MESSAGE_BODY.
  CLEAR: FOOTER.

  FOOTER = 'Any dispute " Chennai Jurisdiction "'.
  APPEND FOOTER TO LT_MESSAGE_BODY.
  CLEAR: FOOTER.

  DATA: WA_T247 TYPE T247.

  SELECT SINGLE * FROM T247 INTO WA_T247 WHERE SPRAS = 'EN' AND MNR = SO_BUDAT-LOW+4(2) .

  SHIFT Y_KNB1-KUNNR LEFT DELETING LEADING '0' .

  CONCATENATE 'Confirmation of Balance' Y_KNB1-KUNNR 'For' WA_T247-KTX '/' SO_BUDAT-LOW(4) INTO SUB SEPARATED BY SPACE.

  LO_DOCUMENT = CL_DOCUMENT_BCS=>CREATE_DOCUMENT(
  I_TYPE = 'RAW'
  I_TEXT = LT_MESSAGE_BODY
  I_SUBJECT = SUB ).

  DATA: MON TYPE I,
        F_KTX TYPE T247-KTX,
        T_KTX TYPE T247-KTX.
  IF SO_BUDAT-LOW(4) = SO_BUDAT-HIGH(4) AND SO_BUDAT-LOW+4(2) = SO_BUDAT-HIGH+4(2).
    SELECT SINGLE KTX FROM T247 INTO F_KTX WHERE MNR = SO_BUDAT-LOW+4(2) AND SPRAS = 'EN'.
    TRANSLATE SM_NAME TO UPPER CASE.
    CONCATENATE SM_NAME'_' F_KTX SO_BUDAT-LOW(4) '_Statement' INTO SM_NAM.
  ELSE.
    SELECT SINGLE KTX FROM T247 INTO F_KTX WHERE MNR = SO_BUDAT-LOW+4(2) AND SPRAS = 'EN'.
    SELECT SINGLE KTX FROM T247 INTO T_KTX WHERE MNR = SO_BUDAT-HIGH+4(2) AND SPRAS = 'EN'.
    TRANSLATE SM_NAME TO UPPER CASE.
    IF SO_BUDAT-LOW(4) = SO_BUDAT-HIGH(4).
      CONCATENATE SM_NAME '_' F_KTX '-' T_KTX  SO_BUDAT-LOW(4) '_Statement' INTO SM_NAM.
    ELSE.
      CONCATENATE SM_NAME '_' F_KTX SO_BUDAT-LOW(4) '_' T_KTX SO_BUDAT-HIGH(4) '_Statement' INTO SM_NAM .
    ENDIF.
  ENDIF.

  TRY.
      LO_DOCUMENT->ADD_ATTACHMENT(
      EXPORTING
      I_ATTACHMENT_TYPE = 'PDF'
      I_ATTACHMENT_SUBJECT = SM_NAM
      I_ATT_CONTENT_HEX = I_OBJBIN[] ).
    CATCH CX_DOCUMENT_BCS INTO LX_DOCUMENT_BCS.
  ENDTRY.

  TRY.
      LO_DOCUMENT->ADD_ATTACHMENT(
      EXPORTING
      I_ATTACHMENT_TYPE = 'XLS'
      I_ATTACHMENT_SUBJECT = SM_NAM
      I_ATT_CONTENT_HEX = I_OBJBIN_F[] ).
    CATCH CX_DOCUMENT_BCS INTO LX_DOCUMENT_BCS.
  ENDTRY.


  LO_SEND_REQUEST->SET_DOCUMENT( LO_DOCUMENT ).
*  LO_SENDER = CL_SAPUSER_BCS=>CREATE( SY-UNAME ).     Manual Email Id Inserted Variable Name SENDER_MAIL
  LO_SENDER = CL_CAM_ADDRESS_BCS=>CREATE_INTERNET_ADDRESS( SENDER_MAIL ).
  LO_SEND_REQUEST->SET_SENDER( LO_SENDER ).
  IF sy-sysid = 'DEV' OR
     sy-sysid = 'QAS'.
  IN_MAILID = 'no-reply@sheenlac.in'.
  LO_RECIPIENT = CL_CAM_ADDRESS_BCS=>CREATE_INTERNET_ADDRESS( IN_MAILID ).
  ELSE.
  LO_RECIPIENT = CL_CAM_ADDRESS_BCS=>CREATE_INTERNET_ADDRESS( IN_MAILID ).
  ENDIF.
  LO_SEND_REQUEST->ADD_RECIPIENT( EXPORTING I_RECIPIENT = LO_RECIPIENT I_EXPRESS = ABAP_TRUE ).
  LO_SEND_REQUEST->ADD_RECIPIENT( LO_RECIPIENT ).
  LO_SEND_REQUEST->SEND( EXPORTING I_WITH_ERROR_SCREEN = ABAP_TRUE RECEIVING RESULT = LV_SENT_TO_ALL ).
  TOT = TOT + 1 .
  COMMIT WORK.
  CLEAR: I_OBJBIN_F[],I_OBJBIN[].
ENDFORM.                    " SEND_MAIL
*&---------------------------------------------------------------------*
*&      Form  STATUS_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM STATUS_EMAIL .

  DATA: F_E TYPE STRING.
  DATA: HDAT_F(10) TYPE C,
        HDAT_T(10) TYPE C.
  TYPES: BEGIN OF TY_LINES,
         LINE TYPE CHAR255,
         END OF TY_LINES.
  DATA:  TI_LINES TYPE STANDARD TABLE OF TY_LINES,
         WA_LINES TYPE TY_LINES.
  DATA:  TITLE TYPE STRING.

  CONCATENATE SO_BUDAT-LOW+6(2) '.' SO_BUDAT-LOW+4(2) '.' SO_BUDAT-LOW(4) INTO HDAT_F .
  CONCATENATE SO_BUDAT-HIGH+6(2) '.' SO_BUDAT-HIGH+4(2) '.' SO_BUDAT-HIGH(4) INTO HDAT_T .
  CONCATENATE 'Customer Statement From ' HDAT_F ' To ' HDAT_T  INTO F_E  SEPARATED BY SPACE.

  TITLE =  F_E.

  WA_LINES = 'Dear Developer,'.
  APPEND WA_LINES TO TI_LINES .

  WA_LINES = '   This mail has been generated automatically. Please do not reply'.
  APPEND WA_LINES TO TI_LINES .

  TOT_C = TOT .
  CONCATENATE 'Email Send Sucessfully: ' TOT_C ' Customers' INTO WA_LINES.
  APPEND WA_LINES TO TI_LINES .

  WA_LINES = '______________________________________________________________________'.
  APPEND: WA_LINES TO TI_LINES .
  WA_LINES = 'Thanks&Regards'.
  APPEND: WA_LINES TO TI_LINES .
  WA_LINES = 'SIS_TEAM'.
  APPEND: WA_LINES TO TI_LINES .

  CALL FUNCTION 'EFG_GEN_SEND_EMAIL'
    EXPORTING
      I_TITLE                = TITLE
      I_SENDER               = 'no-reply@sheenlac.in'
      I_RECIPIENT            = 'prasath@sphinaxinfosystems.com'
      I_FLG_COMMIT           = 'X'
      I_FLG_SEND_IMMEDIATELY = 'X'
    TABLES
      I_TAB_LINES            = TI_LINES
    EXCEPTIONS
      NOT_QUALIFIED          = 1
      FAILED                 = 2
      OTHERS                 = 3.

  PRINT-CONTROL FUNCTION 'SAPBLD'.
  WRITE:/'Email Sending Status', SY-ULINE.
  PRINT-CONTROL FUNCTION 'SAOFF'.

  IF TOT < 1.
    WRITE:/ 'No Emails Send' COLOR COL_NEGATIVE .
  ELSE.
    WRITE:/ 'Email Send Sucessfully', TOT ,'Customer' COLOR COL_POSITIVE .
  ENDIF.
ENDFORM.                    " STATUS_EMAIL


*&---------------------------------------------------------------------*
*&      Form  EXCEL_ATTACH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM EXCEL_ATTACH .

  DATA: LV_TTTEXT(21) TYPE C.
  DATA: TOT_D TYPE BSAD-DMBTR.
  DATA: TOT_C TYPE BSAD-DMBTR.
  DATA: TOT TYPE BSAD-DMBTR.
  DATA: CLB TYPE BSAD-DMBTR.
  DATA: T_OPN TYPE BSAD-DMBTR.
  DATA: C_OPN(15) TYPE C.
  DATA: DE_C(15) TYPE C.
  DATA: CR_C(15) TYPE C.
  DATA: CL_C(15) TYPE C.
  DATA: C_TIME(11) TYPE C.
  DATA: C_S_P(150) TYPE C.  "CITY STATE PINCODE
  DATA: DOC_DAT(10) TYPE C. "DOCUMENT DATE
  DATA: F_DAT(10) TYPE C.  "FROM DATE
  DATA: T_DAT(10) TYPE C.  "TO DATE
  DATA: C_DAT(10) TYPE C.  "CURRENT DATE.
  DATA: DAY_NAM TYPE T246-LANGT.  ""DTRESR-WEEKDAY. "WEEK DAY NAME
  DATA: TIME TYPE SY-UZEIT.
  DATA: T_TY(2) TYPE C.
  DATA: WA_ADRC TYPE ADRC.
  DATA: REGIO TYPE  T005U-BEZEI.
  DATA: REG TYPE  KNA1-REGIO.
  DATA:LV_BELNR TYPE  BSID-BELNR.
  DATA:LV_BLART TYPE  BSID-BLART.
  DATA: TTT(22) TYPE C. "TRANSACTION TYPE CONCAT FIELD
  DATA: COLLECTION  TYPE  BSID-DMBTR.
  DATA: CC(15) TYPE C. " COLLECTION CHAR
  DATA: CREDITNOTE  TYPE  BSID-DMBTR.
  DATA: CNC(15) TYPE C. "CREDIT NOTE CHAR
  DATA: DEBITNOTE TYPE  BSID-DMBTR.
  DATA: INVOICE TYPE  BSID-DMBTR.
  DATA: IC(15) TYPE C . "INVOICE CHAR
  DATA: DC TYPE I,  "DEBIT COUNT
        CC1 TYPE I.  "CREDIT COUNT
  DATA: DCC(4) TYPE C,  "DEBIT COUNT CHARACTER
        CCC(4) TYPE C.  "CREDIT COUNT CHARACTER
  DATA: CREDITLIMIT TYPE KNKK-KLIMK.  "#EC CI_USAGE_OK[2227014] "Added by SPLABAP during code remediation   "CREDIT LIMIT
  DATA: CRL_C(15) TYPE C ." CREDIT LIMIT CHARACTER
  DATA: BYTES TYPE I.
  DATA: CREDITNOTEAMT(15) TYPE C.
  DATA: CONT(255) TYPE C.


  TYPES: BEGIN OF TY_EXFI,
  DOC_DAT(10) TYPE C ,
  TTT(21) TYPE C,
  BELNR TYPE BSID-BELNR,
  XBLNR TYPE BSID-XBLNR,
  SGTXT TYPE BSID-SGTXT,
  D_AMT TYPE BSID-DMBTR,
  C_AMT TYPE BSID-DMBTR,
  B_AMT TYPE BSID-DMBTR,
  BLART TYPE BSID-BLART,
  END OF TY_EXFI.

  DATA: IT_EXFI TYPE TABLE OF TY_EXFI,
        WA_EXFI TYPE TY_EXFI.

  DATA :  SALESOFFICE TYPE  KNVV-VKBUR,
          SO_NAM  TYPE  TVKBT-BEZEI.

  CLEAR: LV_TTTEXT ,TOT_D,TOT_C,TOT,CLB,T_OPN,C_OPN,DE_C,CR_C,CL_C,C_TIME,C_S_P,DOC_DAT,F_DAT,T_DAT,C_DAT,DAY_NAM,
         TIME,T_TY,WA_ADRC,REGIO,REG,LV_BELNR,LV_BLART,TTT,COLLECTION,CC,CREDITNOTE,CNC,DEBITNOTE,INVOICE,IC,DC,
         CC1,DCC,CCC, CREDITLIMIT,CRL_C,BYTES,CREDITNOTEAMT,IT_ATTACH .

  CONCATENATE ' ' ' ' ' ' 'STATEMENT OF ACCOUNT' INTO IT_ATTACH SEPARATED BY CON_TAB .
  CONCATENATE CON_CRET IT_ATTACH  INTO IT_ATTACH.
  APPEND IT_ATTACH .

  CONCATENATE ' ' ' ' INTO IT_ATTACH SEPARATED BY CON_TAB .
  CONCATENATE CON_CRET IT_ATTACH  INTO IT_ATTACH.
  APPEND IT_ATTACH.

  CLEAR:WA_ADRC.

  LOOP AT GT_KNA1 INTO WA_KNA1.
*    SELECT SINGLE * FROM ADRC INTO WA_ADRC WHERE ADDRNUMBER = WA_KNA1-ADRNR."Commented by SPLABAP during code remediation
    "Added by SPLABAP during code remediation
    SELECT  * UP TO 1 ROWS
      FROM ADRC INTO WA_ADRC WHERE ADDRNUMBER = WA_KNA1-ADRNR
      ORDER BY PRIMARY KEY.
ENDSELECT.
*    SELECT SINGLE VKBUR FROM KNVV INTO SALESOFFICE WHERE KUNNR = WA_KNA1-KUNNR AND VKORG = LV_BUK."Commented by SPLABAP during code remediation
    "Added by SPLABAP during code remediation
    SELECT VKBUR UP TO 1 ROWS
      FROM KNVV INTO SALESOFFICE WHERE KUNNR = WA_KNA1-KUNNR AND VKORG = LV_BUK
      ORDER BY PRIMARY KEY.
ENDSELECT.
    SELECT SINGLE BEZEI FROM TVKBT INTO SO_NAM WHERE VKBUR = SALESOFFICE AND SPRAS = 'EN'.
    EXIT."#EC CI_NOORDER  "Added by SPLABAP during code remediation
  ENDLOOP.

  CONCATENATE ' ' ' ' INTO IT_ATTACH SEPARATED BY CON_TAB .
  CONCATENATE CON_CRET IT_ATTACH  INTO IT_ATTACH.
  APPEND IT_ATTACH.

  CONCATENATE WA_ADRC-NAME1 ' ' ' ' ' ' ' ' ' '  'Dealer Code:' WA_KNA1-KUNNR  INTO IT_ATTACH SEPARATED BY CON_TAB .
  CONCATENATE CON_CRET IT_ATTACH  INTO IT_ATTACH.
  APPEND IT_ATTACH.

  CONCATENATE WA_ADRC-STREET ' ' ' ' ' ' ' '  ' ' 'Sales Office:' SO_NAM INTO IT_ATTACH SEPARATED BY CON_TAB .
  CONCATENATE CON_CRET IT_ATTACH  INTO IT_ATTACH.
  APPEND IT_ATTACH.
  CLEAR: C_S_P.
  CONCATENATE WA_ADRC-CITY1 '-' WA_ADRC-POST_CODE1 ',INDIA' INTO C_S_P.

  SELECT SINGLE REGIO FROM KNA1 INTO REG WHERE KUNNR = WA_KNA1-KUNNR.
  SELECT SINGLE BEZEI FROM T005U INTO REGIO WHERE SPRAS = 'EN' AND LAND1 = 'IN' AND BLAND = REG.

  CONCATENATE C_S_P '' '' '' '' ''  'Territory:' REGIO  INTO IT_ATTACH SEPARATED BY CON_TAB .
  CONCATENATE CON_CRET IT_ATTACH  INTO IT_ATTACH.
  APPEND IT_ATTACH.

  CONCATENATE '' '' '' '' '' ''  'Currency:' 'INR' INTO IT_ATTACH SEPARATED BY CON_TAB.
  CONCATENATE CON_CRET IT_ATTACH  INTO IT_ATTACH.
  APPEND IT_ATTACH.

  CONCATENATE ' ' ' ' INTO IT_ATTACH SEPARATED BY CON_TAB .
  CONCATENATE CON_CRET IT_ATTACH  INTO IT_ATTACH.
  APPEND IT_ATTACH.

  CLEAR: F_DAT , T_DAT , C_DAT .
  CONCATENATE SO_BUDAT-LOW+6(2) '/' SO_BUDAT-LOW+4(2) '/' SO_BUDAT-LOW(4) INTO F_DAT .
  CONCATENATE SO_BUDAT-HIGH+6(2) '/' SO_BUDAT-HIGH+4(2) '/' SO_BUDAT-HIGH(4) INTO T_DAT.
  CONCATENATE SY-DATUM+6(2) '/' SY-DATUM+4(2) '/' SY-DATUM(4) INTO C_DAT.
  CLEAR:DAY_NAM.
  CALL FUNCTION 'GET_WEEKDAY_NAME'
    EXPORTING
     DATE                 = SY-DATUM
      LANGUAGE             = sy-langu
*     WEEKDAY_NUMBER       = ' '
   IMPORTING
*     LANGU_BACK           =
     LONGTEXT             = DAY_NAM
*     SHORTTEXT            =
   EXCEPTIONS
     CALENDAR_ID          = 1
     DATE_ERROR           = 2
     NOT_FOUND            = 3
     WRONG_INPUT          = 4
     OTHERS               = 5
            .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.

**  CALL FUNCTION 'DATE_TO_DAY'
**    EXPORTING
**      DATE    = SY-DATUM
**    IMPORTING
**      WEEKDAY = DAY_NAM.
  DAY_NAM = DAY_NAM(3).
  CLEAR: TIME , T_TY.
  CALL FUNCTION 'HRVE_CONVERT_TIME'
    EXPORTING
      TYPE_TIME       = 'A'
      INPUT_TIME      = SY-UZEIT
*     INPUT_AM_PM     = 'AM'
    IMPORTING
      OUTPUT_TIME     = TIME
      OUTPUT_AM_PM    = T_TY
    EXCEPTIONS
      PARAMETER_ERROR = 1
      OTHERS          = 2.

  CLEAR:C_TIME.
  CONCATENATE TIME(2)':' TIME+2(2) ':' TIME+4(2) ' ' T_TY INTO C_TIME.
  CONCATENATE 'Statement of the account for the period of ' F_DAT ' to ' T_DAT ' generated on ' DAY_NAM C_DAT ' at ' C_TIME INTO CONT SEPARATED BY SPACE.
  CONCATENATE CONT '' INTO IT_ATTACH .
  CONCATENATE CON_CRET IT_ATTACH  INTO IT_ATTACH.
  APPEND IT_ATTACH.

  CONCATENATE ' ' ' ' INTO IT_ATTACH SEPARATED BY CON_TAB .
  CONCATENATE CON_CRET IT_ATTACH  INTO IT_ATTACH.
  APPEND IT_ATTACH.

  CLEAR: TOT.
  MOVE LV_OPN TO TOT.

  LOOP AT GT_FINAL INTO WA_FINAL.

    CASE WA_FINAL-BLART.
      WHEN 'DG'.
        LV_TTTEXT = WA_FINAL-T_TTTEXT.
        IF WA_FINAL-T_TTTEXT = 'CREDIT NOTE' AND
          WA_FINAL-SHKZG = 'H'.

          CREDITNOTE = CREDITNOTE + WA_FINAL-CR_AMT.

        ELSEIF  WA_FINAL-T_TTTEXT = 'CREDIT NOTE' AND
        WA_FINAL-SHKZG = 'S'.

          DEBITNOTE = DEBITNOTE + WA_FINAL-DR_AMT.

        ENDIF.

      WHEN 'DZ'.
        LV_TTTEXT = WA_FINAL-T_TTTEXT.

        IF  WA_FINAL-T_TTTEXT = 'AUTO CREDIT NOTE'AND
            WA_FINAL-SHKZG = 'H'.
          CREDITNOTE = CREDITNOTE + WA_FINAL-CR_AMT.
        ELSEIF  WA_FINAL-T_TTTEXT = 'AUTO CREDIT NOTE'AND
                WA_FINAL-SHKZG = 'S'.
          DEBITNOTE = DEBITNOTE + WA_FINAL-DR_AMT.
        ELSEIF  WA_FINAL-T_TTTEXT = 'COLLECTION'AND
                WA_FINAL-SHKZG = 'H'.
          COLLECTION = COLLECTION + WA_FINAL-CR_AMT .
        ELSEIF  WA_FINAL-T_TTTEXT = 'COLLECTION'AND
                WA_FINAL-SHKZG = 'S'.
          COLLECTION = COLLECTION + WA_FINAL-DR_AMT .
        ELSEIF  WA_FINAL-T_TTTEXT = 'RECEIPT'AND
               WA_FINAL-SHKZG = 'H'.
          COLLECTION = COLLECTION + WA_FINAL-CR_AMT .
        ELSEIF  WA_FINAL-T_TTTEXT = 'RECEIPT'AND
                WA_FINAL-SHKZG = 'S'.
          COLLECTION = COLLECTION + WA_FINAL-DR_AMT .
        ENDIF.

      WHEN 'AB'.
        LV_TTTEXT = WA_FINAL-T_TTTEXT.
        IF WA_FINAL-T_TTTEXT = 'CREDIT NOTE'
         OR WA_FINAL-T_TTTEXT = 'AUTO CREDIT NOTE'.
          IF WA_FINAL-SHKZG = 'H'.
            CREDITNOTE = CREDITNOTE + WA_FINAL-CR_AMT.
          ELSEIF WA_FINAL-SHKZG = 'S'.
            DEBITNOTE = DEBITNOTE + WA_FINAL-DR_AMT.
          ENDIF.
        ENDIF.

      WHEN 'DR'.
        LV_TTTEXT = WA_FINAL-T_TTTEXT.
        IF WA_FINAL-T_TTTEXT = 'DEBIT NOTE'.
          DEBITNOTE = DEBITNOTE + WA_FINAL-DR_AMT.
        ENDIF.
      WHEN 'RV'.
        LV_TTTEXT = WA_FINAL-T_TTTEXT.
        INVOICE = INVOICE + WA_FINAL-DR_AMT.
      WHEN 'VG'.
        LV_TTTEXT = WA_FINAL-T_TTTEXT.
      WHEN 'DA'.
        LV_TTTEXT = WA_FINAL-T_TTTEXT.
      WHEN 'KR'.
        LV_TTTEXT = WA_FINAL-T_TTTEXT.
      WHEN 'SA'.
        LV_TTTEXT = WA_FINAL-T_TTTEXT.
      WHEN OTHERS.
        SELECT SINGLE LTEXT FROM T003T INTO LV_TTTEXT
          WHERE SPRAS = 'EN' AND BLART = WA_FINAL-BLART.
        TRANSLATE LV_TTTEXT TO UPPER CASE.
    ENDCASE.

    CONCATENATE WA_FINAL-BLART '-' LV_TTTEXT INTO TTT .

    IF WA_FINAL-BSCHL = 27 .
      WA_FINAL-SHKZG = 'H'.
    ENDIF.

    IF WA_FINAL-SHKZG = 'S'.
      TOT = TOT + WA_FINAL-DR_AMT.
      TOT_D = TOT_D + WA_FINAL-DR_AMT .
      DC = DC + 1 .
    ELSEIF WA_FINAL-SHKZG = 'H'.
      TOT = TOT - WA_FINAL-CR_AMT .
      TOT_C = TOT_C + WA_FINAL-CR_AMT.
      CC1 = CC1 + 1.
    ENDIF.
    CLEAR: DOC_DAT.
    CONCATENATE WA_FINAL-BLDAT+6(2) '-' WA_FINAL-BLDAT+4(2) '-' WA_FINAL-BLDAT(4) INTO DOC_DAT.
    WA_EXFI-DOC_DAT = DOC_DAT.
    WA_EXFI-TTT = TTT .
    WA_EXFI-SGTXT = WA_FINAL-SGTXT.
    WA_EXFI-XBLNR = WA_FINAL-XBLNR.
    WA_EXFI-BELNR = WA_FINAL-BELNR.
    WA_EXFI-D_AMT = WA_FINAL-DR_AMT .
    WA_EXFI-C_AMT = WA_FINAL-CR_AMT .
    WA_EXFI-B_AMT = TOT.
    WA_EXFI-BLART = WA_FINAL-BLART .
    CLB = TOT .
    APPEND WA_EXFI TO IT_EXFI .
    CLEAR: LV_TTTEXT,WA_FINAL,WA_EXFI .
  ENDLOOP.

  CCC = CC1 .
  DCC = DC .
  DE_C = TOT_D."#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
  CR_C = TOT_C."#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
  CL_C = CLB."#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
  C_OPN = LV_OPN ."#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation


  CONCATENATE 'Opening Balance(Rs)' 'Debit Items' 'Credit Items' 'Total Debit (Rs)' 'Total Credit (Rs)' 'Closing Bal (Rs)' 'Credit Limit (Rs)' INTO IT_ATTACH SEPARATED BY CON_TAB.
  CONCATENATE CON_CRET IT_ATTACH  INTO IT_ATTACH.
  APPEND IT_ATTACH.

  CLEAR: CREDITLIMIT.
  CALL FUNCTION 'CREDIT_EXPOSURE'
    EXPORTING
      KKBER       = LV_BUK
      KUNNR       = WA_KNA1-KUNNR
    IMPORTING
      CREDITLIMIT = CREDITLIMIT.

  CRL_C = CREDITLIMIT ."#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation

  CONCATENATE C_OPN DCC CCC DE_C CR_C CL_C CRL_C INTO IT_ATTACH SEPARATED BY CON_TAB .
  CONCATENATE CON_CRET IT_ATTACH  INTO IT_ATTACH.
  APPEND IT_ATTACH.

  CLEAR: C_OPN , DCC , CCC , DE_C , CR_C , CL_C , CRL_C .

  CONCATENATE ' ' ' ' INTO IT_ATTACH SEPARATED BY CON_TAB .
  CONCATENATE CON_CRET IT_ATTACH  INTO IT_ATTACH.
  APPEND IT_ATTACH.

  CONCATENATE 'Transaction Date' 'Transaction Type' 'Transaction Number' 'Reference' 'Text' 'Debit Amount' 'Credit Amount' 'Balance Amount' INTO IT_ATTACH SEPARATED BY CON_TAB .
  CONCATENATE CON_CRET IT_ATTACH  INTO IT_ATTACH.
  APPEND IT_ATTACH.

  CLEAR: C_OPN.
  IF LV_OPN > 0.
    C_OPN = LV_OPN ."#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation

    CONCATENATE ' ' ' ' 'Opening balance... ' ' ' ' ' C_OPN ' ' C_OPN INTO IT_ATTACH SEPARATED BY CON_TAB .
    CONCATENATE CON_CRET IT_ATTACH  INTO IT_ATTACH.
    APPEND IT_ATTACH.

  ELSE.
    T_OPN = -1 * LV_OPN.
    MOVE T_OPN TO C_OPN."#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation

    CONCATENATE '-' C_OPN INTO C_OPN.
    CONCATENATE ' ' ' ' 'Opening balance... ' ' ' ' ' C_OPN C_OPN INTO IT_ATTACH SEPARATED BY CON_TAB .
    CONCATENATE CON_CRET IT_ATTACH  INTO IT_ATTACH.
    APPEND IT_ATTACH.

  ENDIF.

  CC = COLLECTION."#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
  IC = INVOICE ."#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
  IF WA_EXFI-BLART = 'DG'.
    CNC = CREDITNOTE."#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
  ELSEIF WA_EXFI-BLART = 'DR'.
    CNC = DEBITNOTE."#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
  ENDIF.

  LOOP AT IT_EXFI INTO WA_EXFI.
    SHIFT WA_EXFI-DOC_DAT LEFT DELETING LEADING SPACE.
    SHIFT WA_EXFI-TTT  LEFT DELETING LEADING SPACE.
    SHIFT WA_EXFI-BELNR LEFT DELETING LEADING SPACE.
    SHIFT WA_EXFI-XBLNR LEFT DELETING LEADING SPACE.
    DE_C = WA_EXFI-D_AMT ."#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
    CR_C = WA_EXFI-C_AMT ."#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
    CONCATENATE '-' CR_C  INTO CR_C.
    IF WA_EXFI-B_AMT < 0.
      WA_EXFI-B_AMT = -1 * WA_EXFI-B_AMT .
      CL_C = WA_EXFI-B_AMT ."#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
      CONCATENATE '-' CL_C INTO CL_C .
    ELSE.
      CL_C = WA_EXFI-B_AMT ."#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
    ENDIF.

    CONCATENATE WA_EXFI-DOC_DAT WA_EXFI-TTT WA_EXFI-BELNR WA_EXFI-XBLNR WA_EXFI-SGTXT DE_C CR_C CL_C INTO IT_ATTACH SEPARATED BY CON_TAB .
    CONCATENATE CON_CRET IT_ATTACH  INTO IT_ATTACH.
    APPEND IT_ATTACH.

    CLEAR : DE_C , CR_C , CL_C , WA_EXFI.

  ENDLOOP.

  CONCATENATE ' ' ' ' INTO IT_ATTACH SEPARATED BY CON_TAB .
  CONCATENATE CON_CRET IT_ATTACH  INTO IT_ATTACH.
  APPEND IT_ATTACH.

  CONCATENATE 'Document Summary:' ' ' INTO IT_ATTACH SEPARATED BY CON_TAB .
  CONCATENATE CON_CRET IT_ATTACH  INTO IT_ATTACH.
  APPEND IT_ATTACH.

  CONCATENATE ' ' ' ' INTO IT_ATTACH SEPARATED BY CON_TAB .
  CONCATENATE CON_CRET IT_ATTACH  INTO IT_ATTACH.
  APPEND IT_ATTACH.

  CONCATENATE 'Document Description:' 'Debit Amount (Rs.)' 'Credit Amount (Rs.)' INTO IT_ATTACH SEPARATED BY CON_TAB .
  CONCATENATE CON_CRET IT_ATTACH  INTO IT_ATTACH.
  APPEND IT_ATTACH.

  CONCATENATE 'Collection:' ' ' CC INTO IT_ATTACH SEPARATED BY CON_TAB .
  CONCATENATE CON_CRET IT_ATTACH  INTO IT_ATTACH.
  APPEND IT_ATTACH.
  IF WA_EXFI-BLART = 'DG'.
    CONCATENATE 'Credit Note:' ' ' CNC INTO IT_ATTACH SEPARATED BY CON_TAB .
    CONCATENATE CON_CRET IT_ATTACH  INTO IT_ATTACH.
    APPEND IT_ATTACH.
  ELSEIF WA_EXFI-BLART = 'DR'.
    CONCATENATE 'Credit Note:' CNC  '' INTO IT_ATTACH SEPARATED BY CON_TAB .
    CONCATENATE CON_CRET IT_ATTACH  INTO IT_ATTACH.
    APPEND IT_ATTACH.
  ENDIF.
  CONCATENATE 'Invoice:' IC '' INTO IT_ATTACH SEPARATED BY CON_TAB .
  CONCATENATE CON_CRET IT_ATTACH  INTO IT_ATTACH.
  APPEND IT_ATTACH.

  CONCATENATE ' ' ' ' INTO IT_ATTACH SEPARATED BY CON_TAB .
  CONCATENATE CON_CRET IT_ATTACH  INTO IT_ATTACH.
  APPEND IT_ATTACH .

  CONCATENATE 'Credit Note:' ' ' INTO IT_ATTACH SEPARATED BY CON_TAB .
  CONCATENATE CON_CRET IT_ATTACH  INTO IT_ATTACH.
  APPEND IT_ATTACH .

  CONCATENATE ' ' ' ' INTO IT_ATTACH SEPARATED BY CON_TAB .
  CONCATENATE CON_CRET IT_ATTACH  INTO IT_ATTACH.
  APPEND IT_ATTACH .

  CONCATENATE 'Document Number' 'Document Date' 'Amount (Rs.)' 'Credit Note Type' 'Text' INTO IT_ATTACH SEPARATED BY CON_TAB .
  CONCATENATE CON_CRET IT_ATTACH  INTO IT_ATTACH.
  APPEND IT_ATTACH.
  LOOP AT IT_EXFI INTO WA_EXFI WHERE BLART = 'DG' OR BLART = 'DR' .
    IF WA_EXFI IS NOT INITIAL.

      IF WA_EXFI-BLART = 'DG'.
        CREDITNOTEAMT  = WA_EXFI-C_AMT ."#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
        SHIFT WA_EXFI-BELNR LEFT DELETING LEADING SPACE.
        SHIFT WA_EXFI-DOC_DAT LEFT DELETING LEADING SPACE.
        SHIFT WA_EXFI-TTT LEFT DELETING LEADING SPACE.
        SHIFT WA_EXFI-SGTXT LEFT DELETING LEADING SPACE.

        CONCATENATE WA_EXFI-BELNR WA_EXFI-DOC_DAT CREDITNOTEAMT  WA_EXFI-TTT WA_EXFI-SGTXT  INTO IT_ATTACH SEPARATED BY CON_TAB .
        CONCATENATE CON_CRET IT_ATTACH  INTO IT_ATTACH.
        APPEND IT_ATTACH .
      ELSEIF WA_EXFI-BLART = 'DR'.
        CREDITNOTEAMT  = WA_EXFI-D_AMT ."#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
        SHIFT WA_EXFI-BELNR LEFT DELETING LEADING SPACE.
        SHIFT WA_EXFI-DOC_DAT LEFT DELETING LEADING SPACE.
        SHIFT WA_EXFI-TTT LEFT DELETING LEADING SPACE.
        SHIFT WA_EXFI-SGTXT LEFT DELETING LEADING SPACE.

        CONCATENATE WA_EXFI-BELNR WA_EXFI-DOC_DAT CREDITNOTEAMT  WA_EXFI-TTT WA_EXFI-SGTXT  INTO IT_ATTACH SEPARATED BY CON_TAB .
        CONCATENATE CON_CRET IT_ATTACH  INTO IT_ATTACH.
        APPEND IT_ATTACH .

      ENDIF.
    ENDIF.
  ENDLOOP.
  CONCATENATE ' ' ' ' INTO IT_ATTACH SEPARATED BY CON_TAB .
  CONCATENATE CON_CRET IT_ATTACH  INTO IT_ATTACH.
  APPEND IT_ATTACH .
  IT_ATTACH = 'NOTE:Kindly Confirm the same with in 15 days of else the balance as per our records is treated as correct'.
  CONCATENATE CON_CRET IT_ATTACH INTO IT_ATTACH.
  APPEND IT_ATTACH.
CLEAR: WA_EXFI .

  REFRESH: I_OBJBIN_F[] ,I_OBJBIN1 .
  LOOP AT IT_ATTACH.
    STRING_DATA = IT_ATTACH.
    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        TEXT   = STRING_DATA
      IMPORTING
        BUFFER = XSTRING_DATA
      EXCEPTIONS
        FAILED = 1
        OTHERS = 2.
    IF SY-SUBRC <> 0.
*       Implement suitable error handling here
    ENDIF.
    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        BUFFER          = XSTRING_DATA
*       APPEND_TO_TABLE = ' '
      IMPORTING
        OUTPUT_LENGTH   = BYTES
      TABLES
        BINARY_TAB      = I_OBJBIN1.
    APPEND: I_OBJBIN1 TO I_OBJBIN_F[].
  ENDLOOP.
CLEAR : IT_ATTACH,STRING_DATA,XSTRING_DATA.
ENDFORM.                    " EXCEL_ATTACH
