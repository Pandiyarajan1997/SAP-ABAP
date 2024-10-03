*&---------------------------------------------------------------------*
*& Report ZFIR_GST_REPORT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZFIR_GST_REPORT.

TABLES: bseg,bkpf,skat .

TYPES: BEGIN OF ty_f,
         bukrs   TYPE bkpf-bukrs, "Company code
         belnr   TYPE bkpf-belnr,  " Document Number
         gjahr   TYPE bkpf-gjahr,  " Fiscal Year
         blart   TYPE bkpf-blart,  " Document Type
         bldat   TYPE bkpf-bldat,  " Document Date
         budat   TYPE bkpf-budat,  " Posting Date
         monat   TYPE bkpf-monat,  " Posting Period
         cpudt   TYPE bkpf-cpudt,  " Entry Date
         tcode   TYPE bkpf-tcode,  " Transaction Code
         xblnr   TYPE bkpf-xblnr,  " Reference
         xmwst   TYPE bkpf-xmwst,  " Calculate tax
         buzei   TYPE bsis-buzei,  " Line item
         mwskz   TYPE bsis-mwskz,  " Tax code
         hkont   TYPE bsis-hkont,  " G/L Account
         dmbtr   TYPE bsis-dmbtr,  " Amount in LC
         sgtxt   TYPE bsis-sgtxt,  " Text
         shkzg   TYPE bsis-shkzg,  " Debit/Credit Ind.
         txt50   TYPE skat-txt50,   " Expense GL Name
         s_no    TYPE bsis-buzei,  " Line
         lifnr   TYPE bsik-lifnr,  " Vendor code
         stenr   TYPE  lfa1-stenr,   " GST Number
         name1   TYPE  lfa1-name1,   " Vendor Name
*
*  BELNR TYPE BKPF-BELNR,  " Document Number
*  GJAHR TYPE BKPF-GJAHR,  " Fiscal Year
*  BLART TYPE BKPF-BLART,  " Document Type
*  BLDAT TYPE BKPF-BLDAT,  " Document Date
*  BUDAT TYPE BKPF-BUDAT,  " Posting Date
*  MONAT TYPE BKPF-MONAT,  " Posting Period
*  CPUDT TYPE BKPF-CPUDT,  " Entry Date
*  TCODE TYPE BKPF-TCODE,  " Transaction Code
*  XBLNR TYPE BKPF-XBLNR,  " Reference
*  AWKEY TYPE BKPF-AWKEY,  " Reference Key
*  XMWST TYPE BKPF-XMWST,  " Calculate tax
*  BUKRS TYPE BSIS-BUKRS,  " Company code
*  BUZEI TYPE BSET-BUZEI,  " Line item
*  MWSKZ TYPE BSET-MWSKZ,  " Tax code
*  HKONT TYPE BSET-HKONT,  " G/L Account
*  TXGRP TYPE BSET-TXGRP,  " Group indicator
         hwbas   TYPE bset-hwbas,  " LC tax base amount
*  HWSTE TYPE BSET-HWSTE,  " LC tax amount

*  SGTXT TYPE BSIK-SGTXT,  " Text

         kostl   TYPE bseg-kostl,  " Costcenter.
         e_gl    TYPE bset-hkont,  " Expense GL Code
         ed_gl   TYPE skat-txt50,  " Expense GL Code

*  TXT50  TYPE  SKAT-TXT50,   " Expense GL Name
*  C_Name1 TYPE  KNA1-NAME1,   " Vendor Name
         ktext   TYPE cskt-ktext,      " Cost center Description
         anln1   TYPE bseg-anln1,   " Asset No
         anln2   TYPE bseg-anln2,   " Asset Sno
         a_txt50 TYPE anla-txt50, " Asset Description
         del     TYPE i,
*  KUNNR TYPE BSID-KUNNR,  " Customer code,
       END OF ty_f.

TYPES: BEGIN OF ty_bkpf,
         bukrs TYPE bkpf-bukrs, "Company code
         belnr TYPE bkpf-belnr,  " Document Number
         gjahr TYPE bkpf-gjahr,  " Fiscal Year
         blart TYPE bkpf-blart,  " Document Type
         bldat TYPE bkpf-bldat,  " Document Date
         budat TYPE bkpf-budat,  " Posting Date
         monat TYPE bkpf-monat,  " Posting Period
         cpudt TYPE bkpf-cpudt,  " Entry Date
         tcode TYPE bkpf-tcode,  " Transaction Code
         xblnr TYPE bkpf-xblnr,  " Reference
         xmwst TYPE bkpf-xmwst,  " Calculate tax
         buzei TYPE bsis-buzei,  " Line item
         mwskz TYPE bsis-mwskz,  " Tax code
         hkont TYPE bsis-hkont,  " G/L Account
         dmbtr TYPE bsis-dmbtr,  " Amount in LC
         sgtxt TYPE bsis-sgtxt,  " Text
         shkzg TYPE bsis-shkzg,  " Debit/Credit Ind.
         txt50 TYPE skat-txt50,   " Expense GL Name
*  AWKEY TYPE BKPF-AWKEY,  " Reference Key
*  XMWST TYPE BKPF-XMWST,  " Calculate tax
*  HKONT TYPE BSIS-HKONT,  " GL

       END OF ty_bkpf.
TYPES: BEGIN OF ty_bseg,
*  BELNR TYPE BKPF-BELNR,
*  GJAHR TYPE BKPF-GJAHR,
*  BUZEI TYPE BSIS-BUZEI,  " Line item
         hkont TYPE bseg-hkont,  " G/L Account
         xref3 TYPE bseg-xref3,  " Ref
         kostl TYPE bseg-kostl,  " Cost Center
         koart TYPE bseg-koart,
         bewar TYPE bseg-bewar,
         anln1 TYPE bseg-anln1,
         anln2 TYPE bseg-anln2,
*  BUZEI TYPE BSEG-BUZEI,  " Item no
*  KUNNR TYPE BSEG-KUNNR,  " Customer
       END OF ty_bseg.
TYPES: BEGIN OF ty_bseg1,
         belnr TYPE bkpf-belnr,  " Document Number
         gjahr TYPE bkpf-gjahr,  " Fiscal Year
         hkont TYPE bseg-hkont,  " G/L Account
         xref3 TYPE bseg-xref3,  " Ref
         buzei TYPE bseg-buzei,  " Item no
*  KUNNR TYPE BSEG-KUNNR,  " Customer
       END OF ty_bseg1.

TYPES: BEGIN OF ty_bsis,
         bukrs TYPE bkpf-bukrs,
         belnr TYPE bkpf-belnr,
         gjahr TYPE bkpf-gjahr,
         buzei TYPE bsis-buzei,  " Line item
         mwskz TYPE bsis-mwskz,  " Tax code
         hkont TYPE bsis-hkont,  " G/L Account
         dmbtr TYPE bsis-dmbtr,  " Amount in LC
         sgtxt TYPE bsis-sgtxt,  " Text
         kostl TYPE bsis-kostl, " cost center
         bschl TYPE bsis-bschl, " Posting Key
         xref3 TYPE bsis-xref3,  " Reference Key 3
         bewar TYPE bsis-bewar,  " Transaction Type
       END OF ty_bsis.

TYPES: BEGIN OF ty_bset,
         bukrs TYPE bset-bukrs,
         belnr TYPE bset-belnr,
         gjahr TYPE bset-gjahr,
         buzei TYPE bset-buzei,  " Line item
         mwskz TYPE bset-mwskz,  " Tax code
         hkont TYPE bset-hkont,  " G/L Account
         txgrp TYPE bset-txgrp,  " Group indicator
         hwbas TYPE bset-hwbas,  " LC tax base amount
         hwste TYPE bset-hwste,  " LC tax amount
         ktosl TYPE bset-ktosl,  " Transaction
       END OF ty_bset.

TYPES: BEGIN OF ty_lfa1,
         lifnr TYPE lfa1-lifnr,
         name1 TYPE lfa1-name1,
         stenr TYPE lfa1-stenr,
       END OF ty_lfa1.

TYPES: BEGIN OF ty_kna1,
         kunnr TYPE kna1-kunnr,
         name1 TYPE kna1-name1,
       END OF ty_kna1.

TYPES: BEGIN OF ty_skat,
         saknr TYPE  skat-saknr,
         txt50 TYPE  skat-txt50,
       END OF ty_skat.

TYPES: BEGIN OF ty_c,
         belnr TYPE bsid-belnr,
         gjahr TYPE bsid-gjahr,
         kunnr TYPE bsid-kunnr,
         sgtxt TYPE bsid-sgtxt,
         name1 TYPE kna1-name1,
       END OF ty_c.

TYPES: BEGIN OF ty_v,
         belnr TYPE bsik-belnr,
         gjahr TYPE bsik-gjahr,
         lifnr TYPE bsik-lifnr,  " Vendor Code
         sgtxt TYPE bsik-sgtxt,  " Text
         name1 TYPE lfa1-name1,
         stenr TYPE lfa1-stenr,
       END OF ty_v.

TYPES: BEGIN OF ty_ven,
         belnr TYPE bsik-belnr,
         gjahr TYPE bsik-gjahr,
         lifnr TYPE bsik-lifnr,  " Vendor Code
         sgtxt TYPE bsik-sgtxt,  " Text
         name1 TYPE lfa1-name1,
         stenr TYPE lfa1-stenr,
       END OF ty_ven.

TYPES: BEGIN OF ty_bk,
         belnr TYPE bkpf-belnr,
         gjahr TYPE bkpf-gjahr,
         buzei TYPE bsas-buzei,
       END OF ty_bk.

TYPES: BEGIN OF ty_a,
         belnr TYPE bkpf-belnr,
         gjahr TYPE bkpf-gjahr,
         s_no  TYPE bsas-buzei,
       END OF ty_a.
TYPES: BEGIN OF ty_anla,
         txt50 TYPE anla-txt50,
       END OF ty_anla.

TYPES: BEGIN OF ty_cskt,
         kostl TYPE cskt-kostl,
         ktext TYPE cskt-ktext,
       END OF ty_cskt.

TYPES: BEGIN OF ty_bsas,
         bukrs TYPE bsas-bukrs,
         belnr TYPE bsas-belnr,
         gjahr TYPE bsas-gjahr,
         xref3 TYPE bsas-xref3,
         buzei TYPE bsas-buzei,
       END OF ty_bsas.

TYPES: BEGIN OF ty_bs,
         bukrs TYPE bsas-bukrs,
         belnr TYPE bsas-belnr,
         gjahr TYPE bsas-gjahr,
         xref3 TYPE bsas-xref3,
         buzei TYPE bsas-buzei,
*  XREF  TYPE BSAS-XREF3,
       END OF ty_bs.

DATA: it_f     TYPE TABLE OF ty_f,
      wa_f     TYPE ty_f,
      it_f1    TYPE TABLE OF ty_f,
      wa_f1    TYPE ty_f,
      it_v     TYPE TABLE OF ty_v,
      wa_v     TYPE ty_v,
      it_c     TYPE TABLE OF ty_c,
      wa_c     TYPE ty_c,
      it_v1    TYPE TABLE OF ty_v,
      wa_v1    TYPE ty_v,
      it_ven   TYPE TABLE OF ty_ven,
      wa_ven   TYPE ty_ven,
      it_c1    TYPE TABLE OF ty_c,
      wa_c1    TYPE ty_c,
      it_a     TYPE TABLE OF ty_a,
      wa_a     TYPE ty_a,
      it_bk    TYPE TABLE OF ty_bk,
      wa_bk    TYPE ty_bk,
      it_bs    TYPE TABLE OF ty_bs,
      wa_bs    TYPE ty_bs,
      it_bkpf  TYPE TABLE OF ty_bkpf,
      wa_bkpf  TYPE ty_bkpf,
      it_bkpf1 TYPE TABLE OF ty_bkpf,
      wa_bkpf1 TYPE ty_bkpf,
      it_bset  TYPE TABLE OF ty_bset,
      wa_bset  TYPE ty_bset,
      it_bsas  TYPE TABLE OF ty_bsas,
      wa_bsas  TYPE ty_bsas,
      it_bsas1 TYPE TABLE OF ty_bsas,
      wa_bsas1 TYPE ty_bsas,
      it_bsas2 TYPE TABLE OF ty_bsas,
      wa_bsas2 TYPE ty_bsas,
      it_bsis  TYPE TABLE OF ty_bsis,
      wa_bsis  TYPE ty_bsis,
      it_bsis1 TYPE TABLE OF ty_bsis,
      wa_bsis1 TYPE ty_bsis,
      it_bsis2 TYPE TABLE OF ty_bsis,
      wa_bsis2 TYPE ty_bsis,
      it_bseg  TYPE TABLE OF ty_bseg,
      wa_bseg  TYPE ty_bseg,
      it_bseg1 TYPE TABLE OF ty_bseg1,
      wa_bseg1 TYPE ty_bseg1,
      it_skat  TYPE TABLE OF ty_skat,
      wa_skat  TYPE ty_skat,
      it_kna1  TYPE TABLE OF ty_kna1,
      wa_kna1  TYPE ty_kna1,
      it_cskt  TYPE TABLE OF ty_cskt,
      wa_cskt  TYPE ty_cskt,
      it_anla  TYPE TABLE OF ty_anla,
      wa_anla  TYPE ty_anla,
      it_lfa1  TYPE TABLE OF ty_lfa1,
      wa_lfa1  TYPE ty_lfa1.


DATA: y       TYPE bkpf-gjahr,
      d       TYPE bkpf-belnr,
      i       TYPE bsik-buzei,
      doc(14) TYPE c,
      ref     TYPE bseg-xref3,
      a       TYPE i,
      b       TYPE i,
      x       TYPE i,
      b1      TYPE i,
      c       TYPE i,
      c1      TYPE i,
      e       TYPE i,
      lin     TYPE bseg-txgrp,
      len     TYPE i,
      acco    TYPE bseg-belnr,
      acco1   TYPE bseg-belnr,
      var     TYPE string,
      le      TYPE i,
      var1    TYPE string.
DATA repid TYPE sy-repid.
TYPE-POOLS: cxtab, slis.
DATA: control_cols TYPE cxtab_column.

DATA: w_fcat TYPE slis_fieldcat_alv,
      i_fcat TYPE slis_t_fieldcat_alv,
      layout TYPE slis_layout_alv.
DATA : it_tvarvc TYPE STANDARD TABLE OF hrpp_sel_st_hkont INITIAL SIZE 0,
       w_tvarvc  TYPE hrpp_sel_st_hkont.
*DATA: A TYPE I VALUE 0.
START-OF-SELECTION.
  SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
  PARAMETERS: rb_1 RADIOBUTTON GROUP rg1 DEFAULT 'X',
              rb_2 RADIOBUTTON GROUP rg1.
**              RB_3 RADIOBUTTON GROUP RG1.
  SELECT-OPTIONS: s_hkont FOR bseg-hkont,             " GL
                  s_budat FOR bkpf-budat OBLIGATORY,
                  s_gjahr FOR bkpf-gjahr.             " Year
  PARAMETERS: rb_3 RADIOBUTTON GROUP rg2 DEFAULT 'X', " Posting Date
              rb_4 RADIOBUTTON GROUP rg2.             " Entry Date
*                  ss_HKONT FOR BSEG-HKONT .  " Date
*  PARAMETERS:     s_GJAHR Type BKPF-GJAHR OBLIGATORY.
  SELECTION-SCREEN: END OF BLOCK b1.

*INITIALIZATION.
*ss_HKONT-LOW   = '0016007000'."0016007005
*ss_HKONT-High  = '0016999999'.
*ss_HKONT-Option = 'BT'.
*ss_HKONT-Sign  = 'I'.
*APPEND ss_HKONT.
  TYPES: BEGIN OF ty_hkont ,
           sign(1)   TYPE c,
           option(2) TYPE c,
           low       TYPE bseg-hkont,
           high      TYPE bseg-hkont,
         END   OF ty_hkont.

*  DATA: it_hkont TYPE TABLE OF ty_hkont,
*        wa_hkont TYPE ty_hkont.
  DATA: it_skat1 TYPE STANDARD TABLE OF skat,
        wa_skat1 TYPE skat.
    PERFORM prog.
**** Begin of change ABAP DEVK921366
*  IF NOT s_hkont[] IS INITIAL.
*    it_hkont[] = s_hkont[].
*    CLEAR s_hkont[].
*  ENDIF.
**** Begin of change ABAP DEVK921366
**AT SELECTION-SCREEN OUTPUT.
*  IF s_hkont[] IS INITIAL.
**S_aufn1 = 'VCN*'.
**INITIALIZATION.
*    s_hkont-low   = '0016007000'.                           "0016007005
*    s_hkont-high  = '0016007999'.
*    s_hkont-option = 'BT'.
*    s_hkont-sign  = 'I'.
*    APPEND s_hkont.
*    IF rb_4 IS NOT INITIAL.
*      IF s_gjahr IS INITIAL.
*        MESSAGE 'Please Enter Fiscal Year' TYPE 'I' DISPLAY LIKE 'E'.
*        STOP.
*
*      ENDIF.
*    ENDIF.
*    PERFORM prog.
*  ELSE.
*
*
*    LOOP AT s_hkont INTO wa_hkont.
*
*      IF s_hkont-high IS NOT INITIAL.
*        IF ( wa_hkont-low GE 0016007000  AND wa_hkont-low LE 0016999999 ) AND
*           ( wa_hkont-high GE 0016007000  AND wa_hkont-high LE 0016999999 ).
**          ELSEIF wa_HKONT-HIGH GE 0016007000  AND wa_HKONT-HIGH LE 0016999999.
*          IF s_gjahr IS INITIAL.
*            MESSAGE 'Please Enter Fiscal Year' TYPE 'I' DISPLAY LIKE 'E'.
*            STOP.
*          ENDIF.
*          PERFORM prog.
*        ELSE.
*          MESSAGE 'Please Enter GST GL account only' TYPE 'I' DISPLAY LIKE 'E'.
*          STOP.
**            Call TRANSACTION 'ZGST_REPORT' .
*        ENDIF.
**       ENDLOOP.
**        READ TABLE IT_SKAT1 INTO WA_SKAT1 INDEX 1.
*      ELSE.
*        IF wa_hkont-low GE 0016007000  AND wa_hkont-low LE 0016999999.
*          IF s_gjahr IS INITIAL.
*            MESSAGE 'Please Enter Fiscal Year' TYPE 'I' DISPLAY LIKE 'E'.
*            STOP.
*          ENDIF.
*          PERFORM prog.
*        ELSE.
*          MESSAGE 'Please Enter GST GL account only' TYPE 'I' DISPLAY LIKE 'E'.
*          STOP.
**             Call TRANSACTION 'ZGST_REPORT'.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.
*
**  SELECT
**  MESSAGE 'Please Enter GST GL account' TYPE 'E'.
*  ENDIF.


*Select A~BELNR A~BLART A~BLDAT A~BUDAT A~BUZEI A~GJAHR A~MONAT A~MWSKZ A~XBLNR A~ZUONR A~BSCHL A~BUZID A~SGTXT
*       B~KOART B~LIFNR B~HKONT B~HWBAS B~DMBTR B~KOSTL B~TXGRP B~QSSKZ INTO TABLE IT_F FROM BSIS AS A INNER JOIN BSEG AS B on A~BELNR EQ B~BELNR
*       WHERE A~BUDAT EQ S_BUDAT AND A~HKONT EQ S_HKONT AND A~BUZID EQ 'T'.
FORM prog.

  IF rb_3 IS NOT INITIAL.
    SELECT b~belnr b~gjahr b~lifnr b~sgtxt b1~name1 b1~stenr INTO TABLE it_v FROM bsik AS b
  INNER JOIN lfa1 AS b1 ON b~lifnr EQ b1~lifnr
  WHERE b~budat IN s_budat AND b~gjahr IN s_gjahr.
    APPEND LINES OF it_v TO it_ven.
*       IT_VEN[] =  IT_V[].

    SELECT c~belnr c~gjahr c~lifnr c~sgtxt c1~name1 c1~stenr INTO TABLE it_v1 FROM bsak AS c
      INNER JOIN lfa1 AS c1 ON c~lifnr EQ c1~lifnr
      WHERE c~budat IN s_budat AND c~gjahr IN s_gjahr.
    APPEND LINES OF it_v1 TO it_ven.
*      IT_VEN[] =  IT_V1[]..

    SELECT d~belnr d~gjahr d~kunnr d~sgtxt d1~name1 INTO TABLE it_c FROM bsid AS d
      INNER JOIN kna1 AS d1 ON d~kunnr EQ d1~kunnr
      WHERE d~budat IN s_budat AND d~gjahr IN s_gjahr.
    APPEND LINES OF it_c TO it_ven.

    SELECT  e~belnr e~gjahr e~kunnr e~sgtxt e1~name1 INTO TABLE it_c1 FROM bsad AS e
      INNER JOIN kna1 AS e1 ON e~kunnr EQ e1~kunnr
      WHERE e~budat IN s_budat AND e~gjahr IN s_gjahr.
    APPEND LINES OF it_c1 TO it_ven.

    CLEAR: wa_v1,wa_c,wa_c1,wa_v,it_c,it_c1,it_v,it_v1.

*     SELECT A~BELNR  A~GJAHR A~BLART A~BLDAT A~BUDAT A~MONAT A~CPUDT A~TCODE A~XBLNR A~AWKEY A~XMWST B~BUKRS
*       INTO TABLE IT_F FROM BKPF AS A INNER JOIN BSIS AS B ON A~BELNR = B~BELNR WHERE  B~HKONT IN S_HKONT and A~BUDAT IN S_BUDAT AND B~BUDAT IN S_BUDAT  .

    SELECT a~bukrs a~belnr  a~gjahr a~blart a~bldat a~budat a~monat a~cpudt a~tcode a~xblnr a~xmwst b~buzei b~mwskz b~hkont b~dmbtr b~sgtxt b~shkzg c~txt50
       INTO TABLE it_bkpf1 FROM bkpf AS a INNER JOIN bsis AS b ON a~bukrs EQ b~bukrs AND a~belnr EQ b~belnr AND a~gjahr EQ b~gjahr AND a~budat EQ b~budat
                                      INNER JOIN skat AS c ON b~hkont EQ c~saknr
                                      WHERE  b~hkont IN s_hkont  AND a~budat IN s_budat AND a~gjahr IN s_gjahr.

    APPEND LINES OF it_bkpf1 TO it_bkpf.
    CLEAR it_bkpf1.
    SELECT a~bukrs a~belnr  a~gjahr a~blart a~bldat a~budat a~monat a~cpudt a~tcode a~xblnr a~xmwst b~buzei b~mwskz b~hkont b~dmbtr b~sgtxt b~shkzg c~txt50
      INTO TABLE it_bkpf1 FROM bkpf AS a INNER JOIN bsas AS b ON a~bukrs EQ b~bukrs AND a~belnr EQ b~belnr AND a~gjahr EQ b~gjahr AND a~budat EQ b~budat
                                     INNER JOIN skat AS c ON b~hkont EQ c~saknr
                                     WHERE  b~hkont IN s_hkont  AND a~budat IN s_budat AND a~gjahr IN s_gjahr.
    APPEND LINES OF it_bkpf1 TO it_bkpf.
******************************* 24.12.2019
  ELSEIF rb_4 IS NOT INITIAL.
    SELECT b~belnr b~gjahr b~lifnr b~sgtxt b1~name1 b1~stenr INTO TABLE it_v FROM bsik AS b
        INNER JOIN lfa1 AS b1 ON b~lifnr EQ b1~lifnr
        WHERE b~cpudt IN s_budat AND b~gjahr IN s_gjahr.
    APPEND LINES OF it_v TO it_ven.
*       IT_VEN[] =  IT_V[].

    SELECT c~belnr c~gjahr c~lifnr c~sgtxt c1~name1 c1~stenr INTO TABLE it_v1 FROM bsak AS c
      INNER JOIN lfa1 AS c1 ON c~lifnr EQ c1~lifnr
      WHERE c~cpudt IN s_budat AND c~gjahr IN s_gjahr.
    APPEND LINES OF it_v1 TO it_ven.
*      IT_VEN[] =  IT_V1[]..

    SELECT d~belnr d~gjahr d~kunnr d~sgtxt d1~name1 INTO TABLE it_c FROM bsid AS d
      INNER JOIN kna1 AS d1 ON d~kunnr EQ d1~kunnr
      WHERE d~cpudt IN s_budat AND d~gjahr IN s_gjahr.
    APPEND LINES OF it_c TO it_ven.

    SELECT  e~belnr e~gjahr e~kunnr e~sgtxt e1~name1 INTO TABLE it_c1 FROM bsad AS e
      INNER JOIN kna1 AS e1 ON e~kunnr EQ e1~kunnr
      WHERE e~cpudt IN s_budat AND e~gjahr IN s_gjahr.
    APPEND LINES OF it_c1 TO it_ven.

    CLEAR: wa_v1,wa_c,wa_c1,wa_v,it_c,it_c1,it_v,it_v1.

*     SELECT A~BELNR  A~GJAHR A~BLART A~BLDAT A~BUDAT A~MONAT A~CPUDT A~TCODE A~XBLNR A~AWKEY A~XMWST B~BUKRS
*       INTO TABLE IT_F FROM BKPF AS A INNER JOIN BSIS AS B ON A~BELNR = B~BELNR WHERE  B~HKONT IN S_HKONT and A~BUDAT IN S_BUDAT AND B~BUDAT IN S_BUDAT  .

    SELECT a~bukrs a~belnr  a~gjahr a~blart a~bldat a~budat a~monat a~cpudt a~tcode a~xblnr a~xmwst b~buzei b~mwskz b~hkont b~dmbtr b~sgtxt b~shkzg c~txt50
       INTO TABLE it_bkpf1 FROM bkpf AS a INNER JOIN bsis AS b ON a~bukrs EQ b~bukrs AND a~belnr EQ b~belnr AND a~gjahr EQ b~gjahr AND a~budat EQ b~budat
                                      INNER JOIN skat AS c ON b~hkont EQ c~saknr
                                      WHERE   a~cpudt IN s_budat AND a~gjahr IN s_gjahr AND b~hkont IN s_hkont ." AND B~CPUDT IN S_BUDAT.

    APPEND LINES OF it_bkpf1 TO it_bkpf.
    CLEAR it_bkpf1.
    SELECT a~bukrs a~belnr  a~gjahr a~blart a~bldat a~budat a~monat a~cpudt a~tcode a~xblnr a~xmwst b~buzei b~mwskz b~hkont b~dmbtr b~sgtxt b~shkzg c~txt50
      INTO TABLE it_bkpf1 FROM bkpf AS a INNER JOIN bsas AS b ON a~bukrs EQ b~bukrs AND a~belnr EQ b~belnr AND a~gjahr EQ b~gjahr AND a~budat EQ b~budat
                                     INNER JOIN skat AS c ON b~hkont EQ c~saknr
                                     WHERE  a~cpudt IN s_budat AND b~gjahr IN s_gjahr AND b~hkont IN s_hkont ." AND B~CPUDT IN S_BUDAT.
    APPEND LINES OF it_bkpf1 TO it_bkpf.
  ENDIF.
*******************************24.12.2019


  IF it_bkpf IS NOT INITIAL.
    SELECT bukrs belnr gjahr xref3 buzei  INTO TABLE it_bsas FROM bsas FOR ALL ENTRIES IN it_bkpf WHERE bukrs EQ it_bkpf-bukrs AND belnr EQ it_bkpf-belnr
      AND gjahr EQ it_bkpf-gjahr AND hkont EQ '0017010000' .

    SELECT bukrs belnr gjahr buzei mwskz hkont dmbtr sgtxt kostl bschl xref3 bewar FROM bsis INTO TABLE it_bsis FOR ALL ENTRIES IN it_bkpf
       WHERE bukrs EQ it_bkpf-bukrs AND belnr EQ it_bkpf-belnr AND gjahr EQ it_bkpf-gjahr AND hkont IN s_hkont.

    SELECT bukrs belnr gjahr buzei mwskz hkont dmbtr sgtxt kostl bschl xref3 bewar FROM bsis INTO TABLE it_bsis2 FOR ALL ENTRIES IN it_bkpf
      WHERE bukrs EQ it_bkpf-bukrs AND belnr EQ it_bkpf-belnr AND gjahr EQ it_bkpf-gjahr AND hkont NOT IN s_hkont  AND hkont NOT IN ('0043004005','0018000002','0027000002')
      AND bschl NOT IN (31,22,01,12)." AND BUZEI NE 1.
    APPEND LINES OF it_bsis2 TO it_bsis1.
    CLEAR: it_bsis2.
    SELECT bukrs belnr  gjahr buzei mwskz hkont dmbtr sgtxt kostl bschl xref3 FROM bsas INTO TABLE it_bsis2 FOR ALL ENTRIES IN it_bkpf
      WHERE bukrs EQ it_bkpf-bukrs AND belnr EQ it_bkpf-belnr AND gjahr EQ it_bkpf-gjahr AND hkont NOT IN s_hkont  AND hkont NOT IN ('0043004005','0018000002','0027000002')
       AND bschl NOT IN (31,22,01,12)." AND BUZEI NE 1.
    APPEND LINES OF it_bsis2 TO it_bsis1.
    SELECT bukrs belnr gjahr xref3 buzei INTO TABLE it_bs FROM bsis FOR ALL ENTRIES IN it_bkpf WHERE bukrs EQ it_bkpf-bukrs AND belnr EQ it_bkpf-belnr
      AND gjahr EQ it_bkpf-gjahr AND xref3 NE  ' ' AND xref3 NOT LIKE 'TCode%'."-I6//Rec.Typ-'.
    APPEND LINES OF it_bs TO it_bsas.
  ENDIF.
  IF it_bsas IS NOT INITIAL.
    SELECT bukrs belnr gjahr xref3 buzei FROM bsas INTO TABLE it_bsas2 FOR ALL ENTRIES IN it_bsas WHERE xref3 EQ it_bsas-xref3 AND belnr NE it_bsas-belnr
      AND  gjahr EQ it_bsas-gjahr.
    APPEND LINES OF it_bsas2 TO it_bsas1.
    CLEAR: it_bsas2.
    SELECT bukrs belnr gjahr xref3 buzei FROM bsis INTO TABLE it_bsas2 FOR ALL ENTRIES IN it_bsas WHERE xref3 EQ it_bsas-xref3 AND belnr NE it_bsas-belnr
      AND  gjahr EQ it_bsas-gjahr.
    APPEND LINES OF it_bsas2 TO it_bsas1.
  ENDIF.
*      LOOP AT IT_F INTO WA_F.
*
*
*        APPEND WA_F TO IT_F1.
*      ENDLOOP.
  CLEAR it_bsis2.
  LOOP AT it_bsis1 INTO wa_bsis1.
    CASE wa_bsis1-buzei.
      WHEN 1.
        IF wa_bsis1-kostl NE '' AND ( wa_bsis1-bschl EQ '50' OR wa_bsis1-bschl EQ '40' OR wa_bsis1-bschl EQ '70' OR wa_bsis1-bschl EQ '75'  ) .
          APPEND wa_bsis1 TO it_bsis2.
        ELSEIF  wa_bsis1-bschl EQ '70' OR wa_bsis1-bschl EQ '75'.
          APPEND wa_bsis1 TO it_bsis2.
        ENDIF.
      WHEN OTHERS.
        APPEND wa_bsis1 TO it_bsis2.
    ENDCASE.
*        IF WA_BSIS1-BUZEI EQ '1' AND WA_BSIS1-KOSTL NE '' AND ( WA_BSIS1-BSCHL EQ '50' OR WA_BSIS1-BSCHL EQ '40' ) .
*          APPEND WA_BSIS1 TO IT_BSIS2.
*        ELSEIF WA_BSIS1-BUZEI EQ '1' AND WA_BSIS1-KOSTL NE '' AND ( WA_BSIS1-BSCHL NE '50' OR WA_BSIS1-BSCHL NE '40' ).
*        ELSE.
*          APPEND WA_BSIS1 TO IT_BSIS2.
*        ENDIF.
  ENDLOOP.
  CLEAR it_bsis1.
  it_bsis1[] = it_bsis2[].
  CLEAR  it_bsis2.
  SELECT saknr txt50 FROM skat INTO TABLE it_skat WHERE ktopl EQ 'KPPL'.
  SELECT kostl ktext FROM cskt INTO TABLE it_cskt WHERE kokrs EQ 'KPPL'.
  SORT it_bkpf BY belnr buzei.
*     SORT IT_BSIS1 BY BELNR BUZEI.
  SORT it_bsis1 BY  belnr buzei ASCENDING.
*     SORT IT_BSIS1 BY BELNR ASCENDING .

*     DELETE ADJACENT DUPLICATES FROM IT_F.
*     MOVE-CORRESPONDING IT_VEN TO IT_F.
  acco = 0.
  b = 0.
*     IT_F[] = IT_VEN[].
  LOOP AT it_bkpf INTO wa_bkpf.

    READ TABLE it_ven INTO wa_ven WITH KEY belnr = wa_bkpf-belnr  gjahr = wa_bkpf-gjahr.
    MOVE-CORRESPONDING wa_bkpf TO wa_f1.
    IF wa_ven IS NOT INITIAL.
      MOVE-CORRESPONDING wa_ven TO wa_f1.
    ENDIF.
    CASE wa_bkpf-shkzg.
      WHEN 'H'. " Credit
        wa_f1-dmbtr = wa_bkpf-dmbtr * -1.
*	WHEN C. " Debit
    ENDCASE.
    IF acco EQ 0.
      b = 1.
      acco = wa_bkpf-belnr.
    ELSEIF acco EQ wa_bkpf-belnr.
      b = b + 1.
    ELSEIF acco NE wa_bkpf-belnr.
      b = 1.
      acco = wa_bkpf-belnr.
    ENDIF.
    wa_f1-s_no = b.
*CASE WA_BKPF-MWSKZ.
*  WHEN 21 or 22 or 23 or 25 or 27 OR 41 OR 42 OR 43 OR 44 OR 47 OR 51 OR 52 OR 53 OR 54.
*    WA_F1-Del = 0.
*  WHEN OTHERS.
    CASE wa_bkpf-hkont.
      WHEN 16007003 OR 16007006 OR 16007009 OR
                      16007012 OR
                      16007103 OR
                      16007106 OR
                      16007109 OR
                      16007111 OR
                      16007112 OR
                      16007113 OR
                      16007201.
        wa_f1-del = 0.
      WHEN OTHERS.
        wa_f1-del = 1.
    ENDCASE.

*ENDCASE.
    APPEND wa_f1 TO it_f1.
    CLEAR: wa_ven, wa_bkpf, wa_f1.
  ENDLOOP.
  a = 0.
  b = 0.
  acco = 0.
  lin = 0.
  CLEAR: it_f,wa_f,wa_f1.
*--Start of changes on 20.11.2022
  IF it_f1 IS NOT INITIAL.
    SELECT bukrs belnr gjahr buzei mwskz hkont txgrp hwbas hwste ktosl
      INTO TABLE it_bset
      FROM bset FOR ALL ENTRIES IN it_f1
      WHERE bukrs EQ it_f1-bukrs
      AND belnr EQ it_f1-belnr
      AND gjahr EQ it_f1-gjahr
      AND buzei EQ it_f1-s_no.
  ENDIF.
*** Extract TVARVC entries for GST
  SELECT sign opti low high
       FROM tvarvc INTO TABLE it_tvarvc
    WHERE name = sy-repid.
*--End of changes on 20.11.2022
  LOOP AT  it_f1 INTO wa_f1 .
    IF acco1 EQ 0.
      acco1 = wa_f1-belnr.
    ELSEIF acco1 NE wa_f1-belnr.
      c1 = 0.
      a = 0.
      acco1 = wa_f1-belnr.
    ENDIF.
    CASE wa_f1-xmwst.
      WHEN 'X'.
        var1 = ''.
*--Start of changes on 20.11.2022
        READ TABLE it_bset INTO wa_bset WITH KEY bukrs = wa_f1-bukrs
                                                 belnr = wa_f1-belnr
                                                 gjahr = wa_f1-gjahr
                                                 buzei = wa_f1-s_no.
*--End of changes on 20.11.2022
        IF sy-subrc EQ 0.
          var = wa_bset-ktosl.
          le = strlen( var ).
          e   = le - 1.
          var1 = var+e(1).

          IF rb_2 IS NOT INITIAL.
            IF acco EQ 0.
              wa_f1-hwbas = wa_bset-hwbas.
              lin = wa_bset-txgrp.
              acco = wa_f1-belnr.
            ELSEIF acco EQ wa_f1-belnr AND lin NE wa_bset-txgrp.
              wa_f1-hwbas = wa_bset-hwbas.
              lin = wa_bset-txgrp.
            ELSEIF acco EQ wa_f1-belnr AND lin EQ wa_bset-txgrp AND var1 EQ '1' .
              wa_f1-hwbas = wa_bset-hwbas.
              lin = wa_bset-txgrp.
            ELSEIF acco NE wa_f1-belnr.
              wa_f1-hwbas = wa_bset-hwbas.
              lin = wa_bset-txgrp.
              acco = wa_f1-belnr.
            ENDIF.
          ELSE.
            wa_f1-hwbas = wa_bset-hwbas.
          ENDIF.
          CASE wa_f1-shkzg.
            WHEN 'H'.
              wa_f1-hwbas = wa_f1-hwbas * -1.
          ENDCASE.
        ENDIF.
        READ TABLE it_bsas INTO wa_bsas WITH KEY bukrs = wa_f1-bukrs belnr = wa_f1-belnr gjahr = wa_f1-gjahr.
        b = sy-tabix.
        x = strlen( wa_bsas-xref3 ).
        IF sy-subrc EQ 0.
          y = wa_bsas-gjahr.
          READ TABLE it_bsas1 INTO wa_bsas1 WITH KEY xref3 =  wa_bsas-xref3.
          IF wa_bsas1-belnr IS NOT INITIAL.
            wa_bsas1-buzei = wa_bsas1-buzei - 1.
            SELECT hkont  xref3 kostl koart bewar anln1 anln2 INTO TABLE it_bseg FROM bseg WHERE bukrs EQ wa_bsas1-bukrs
              AND belnr EQ wa_bsas1-belnr AND gjahr EQ  y
             AND buzei = wa_bsas1-buzei."  EBELP EQ I AND BUZID EQ 'M'.
            READ TABLE it_bseg INTO wa_bseg INDEX 1.
            wa_f1-kostl = wa_bsis1-kostl.
            wa_f1-e_gl = wa_bseg-hkont.
            wa_f1-anln1 = wa_bseg-anln1.
            wa_f1-anln2 = wa_bseg-anln2.
            PERFORM del_bsas.
          ELSE.
            READ TABLE it_bsis1 INTO wa_bsis1 WITH KEY xref3 = wa_bsas-xref3  gjahr = wa_bsas-gjahr.
            c = sy-tabix.
            wa_f1-e_gl = wa_bsis1-hkont.
            wa_f1-kostl = wa_bsis1-kostl.
            IF wa_bsis1-bschl EQ 70 OR wa_bsis1-bschl EQ 75  .
*                       Y = WA_BSIS1-XREF3+0(4).
*                       SELECT BELNR GJAHR BUZEI FROM BSIS INTO TABLE IT_BK WHERE XREF3 EQ WA_BSIS1-XREF3 AND BELNR NE WA_F1-BELNR AND GJAHR EQ WA_F1-GJAHR.
*                         READ TABLE IT_BK INTO WA_BK INDEX 1.
*                         WA_BK-BUZEI = WA_BK-BUZEI - 1.
              SELECT hkont  xref3 kostl koart bewar anln1 anln2 INTO TABLE it_bseg FROM bseg
                WHERE bukrs EQ wa_bsis1-bukrs AND belnr EQ wa_bsis1-belnr AND gjahr EQ  wa_bsis1-gjahr
             AND buzei = wa_bsis1-buzei."  EBELP EQ I AND BUZID EQ 'M'.
              READ TABLE it_bseg INTO wa_bseg INDEX 1.
              wa_f1-e_gl = wa_bseg-hkont.
              wa_f1-kostl = wa_bseg-kostl.
*                      WA_F1-E_GL = WA_BSEG-HKONT.
              wa_f1-anln1 = wa_bseg-anln1.
              wa_f1-anln2 = wa_bseg-anln2.
            ENDIF.
            PERFORM del_bsas.
          ENDIF.
          PERFORM del_bsis1.
          CLEAR x.
        ELSE.
          READ TABLE it_bsis1 INTO wa_bsis1 WITH KEY bukrs = wa_f1-bukrs belnr = wa_f1-belnr  gjahr = wa_f1-gjahr .
          c = sy-tabix.
          wa_f1-e_gl = wa_bsis1-hkont.
          wa_f1-kostl = wa_bsis1-kostl.
          IF wa_bsis1-bschl EQ 70 OR wa_bsis1-bschl EQ 75.
*                       Y = WA_BSIS1-XREF3+0(4).
*                       SELECT BELNR GJAHR BUZEI FROM BSIS INTO TABLE IT_BK WHERE XREF3 EQ WA_BSIS1-XREF3 AND BELNR NE WA_F1-BELNR AND GJAHR EQ WA_F1-GJAHR.
*                         READ TABLE IT_BK INTO WA_BK INDEX 1.
*                         WA_BK-BUZEI = WA_BK-BUZEI - 1.
            SELECT hkont  xref3 kostl koart bewar anln1 anln2 INTO TABLE it_bseg FROM bseg
              WHERE bukrs EQ wa_bsis1-bukrs AND belnr EQ wa_bsis1-belnr AND gjahr EQ  wa_bsis1-gjahr
           AND buzei = wa_bsis1-buzei."  EBELP EQ I AND BUZID EQ 'M'.
            READ TABLE it_bseg INTO wa_bseg INDEX 1.
            wa_f1-e_gl = wa_bseg-hkont.
            wa_f1-kostl = wa_bseg-kostl.
*                      WA_F1-E_GL = WA_BSEG-HKONT.
            wa_f1-anln1 = wa_bseg-anln1.
            wa_f1-anln2 = wa_bseg-anln2.
          ENDIF.
          PERFORM del_bsis1.
        ENDIF.
*             APPEND WA_F1 TO IT_F.
      WHEN OTHERS.
        READ TABLE it_bsis1 INTO wa_bsis1 WITH KEY bukrs = wa_f1-bukrs belnr = wa_f1-belnr gjahr = wa_f1-gjahr  .
        c = sy-tabix.
        wa_f1-e_gl = wa_bsis1-hkont.
        wa_f1-kostl = wa_bsis1-kostl.
*            IF C IS NOT INITIAL AND ( WA_F1-BLART EQ 'DR' OR WA_F1-BLART EQ 'SA' OR WA_F1-BLART EQ 'SB' ) .
*              DELETE IT_BSIS1 INDEX C.
*            ELSE.
        PERFORM del_bsis1.
*            ENDIF.

*       PERFORM DEL_BSIS1.
*            APPEND WA_F1 TO IT_F.
    ENDCASE.

    READ TABLE it_skat INTO wa_skat WITH KEY saknr = wa_f1-e_gl.
    wa_f1-ed_gl = wa_skat-txt50.
    READ TABLE it_cskt INTO wa_cskt WITH KEY kostl = wa_f1-kostl.
    wa_f1-ktext = wa_cskt-ktext.
    SELECT txt50 FROM anla INTO TABLE it_anla WHERE anln1 EQ wa_f1-anln1 AND anln2 EQ wa_f1-anln2.
    READ TABLE it_anla INTO wa_anla INDEX 1.
    wa_f1-a_txt50 = wa_anla-txt50.
    APPEND wa_f1 TO it_f.
    CLEAR: wa_bset,wa_f1,it_anla, wa_anla, wa_cskt, wa_skat,wa_bsis,wa_bsis1,wa_bseg,wa_bsas,wa_bsas1.

  ENDLOOP.

*--New changes for base amount by ABAP on 30.11.2022
  LOOP AT it_f ASSIGNING FIELD-SYMBOL(<lf_f>).
    READ TABLE it_bset INTO wa_bset WITH KEY bukrs = <lf_f>-bukrs
                                             belnr = <lf_f>-belnr
                                             gjahr = <lf_f>-gjahr
                                             buzei = <lf_f>-s_no.
    IF sy-subrc = 0.
      <lf_f>-hwbas = wa_bset-hwbas.
      CASE <lf_f>-shkzg.
        WHEN 'H'.
          <lf_f>-hwbas = <lf_f>-hwbas * -1.
      ENDCASE.
    ENDIF.
*** Begin of change ABAP 01/12/2022
    IF <lf_f>-hkont IN it_tvarvc.
      CLEAR <lf_f>-hwbas.
    ENDIF.
*** end of change ABAP 01/12/2022
    CLEAR : wa_bset.
  ENDLOOP.
**** Begin of change ABAP DEVK921366
*  IF NOT it_hkont IS INITIAL.
*    DELETE it_f WHERE hkont NOT IN it_hkont[].
*  ENDIF.
*** Begin of change ABAP DEVK921366
*--New changes for base amount by ABAP on 30.11.2022
  CLEAR it_f1.
  it_f1[] = it_f[].
  CLEAR: wa_f, it_f.




  w_fcat-fieldname = 'BUDAT'.
  w_fcat-ref_tabname = 'BKPF'.
*  W_FCAT-EMPHASIZE = 'C510'.
  w_fcat-outputlen = '12'.
  w_fcat-col_pos = '1'.
  APPEND w_fcat TO i_fcat.
  CLEAR w_fcat.

  w_fcat-fieldname = 'BLDAT'.
  w_fcat-ref_tabname = 'BKPF'.
*  W_FCAT-EMPHASIZE = 'C110'.
  w_fcat-outputlen = '12'.
  w_fcat-col_pos = '2'.
  APPEND w_fcat TO i_fcat.
  CLEAR w_fcat.

  w_fcat-fieldname = 'BELNR'.
  w_fcat-ref_tabname = 'BKPF'.
*  W_FCAT-EMPHASIZE = 'C311'.
  w_fcat-outputlen = '12'.
  w_fcat-col_pos = '3'.
  APPEND w_fcat TO i_fcat.
  CLEAR w_fcat.

  w_fcat-fieldname = 'GJAHR'.
  w_fcat-ref_tabname = 'BKPF'.
*  W_FCAT-EMPHASIZE = 'C311'.
  w_fcat-outputlen = '6'.
  w_fcat-col_pos = '4'.
  APPEND w_fcat TO i_fcat.
  CLEAR w_fcat.

  w_fcat-fieldname = 'BLART'.
  w_fcat-ref_tabname = 'BKPF'.
*  W_FCAT-EMPHASIZE = 'C311'.
  w_fcat-outputlen = '5'.
  w_fcat-col_pos = '5'.
  APPEND w_fcat TO i_fcat.
  CLEAR w_fcat.

  w_fcat-fieldname = 'XBLNR'.
  w_fcat-ref_tabname = 'BKPF'.
*  W_FCAT-EMPHASIZE = 'C311'.
  w_fcat-outputlen = '20'.
  w_fcat-col_pos = '6'.
  APPEND w_fcat TO i_fcat.
  CLEAR w_fcat.

*  W_FCAT-FIELDNAME = 'ZUONR'.
*  W_FCAT-REF_TABNAME = 'BSIS'.
**  W_FCAT-EMPHASIZE = 'C311'.
*  W_FCAT-OUTPUTLEN = '20'.
*  W_FCAT-COL_POS = '7'.
*  APPEND W_FCAT TO I_FCAT.
*  CLEAR W_FCAT.

  w_fcat-fieldname = 'LIFNR'.
*  W_FCAT-REF_TABNAME = 'BSIK'.
  w_fcat-seltext_l = 'Vendor /Customer'.
*  W_FCAT-EMPHASIZE = 'C311'.
  w_fcat-outputlen = '8'.
  w_fcat-col_pos = '8'.
  APPEND w_fcat TO i_fcat.
  CLEAR w_fcat.

  w_fcat-fieldname = 'STENR'.
*  W_FCAT-REF_TABNAME = 'LFA1'.
  w_fcat-seltext_l = 'GST NO'.
*  W_FCAT-EMPHASIZE = 'C311'.
  w_fcat-outputlen = '18'.
  w_fcat-col_pos = '10'.
  APPEND w_fcat TO i_fcat.
  CLEAR w_fcat.

  w_fcat-fieldname = 'NAME1'.
*  W_FCAT-REF_TABNAME = 'LFA1'.
  w_fcat-seltext_l = 'Vendor /Customer Name'.
*  W_FCAT-EMPHASIZE = 'C311'.
  w_fcat-outputlen = '40'.
  w_fcat-col_pos = '9'.
  APPEND w_fcat TO i_fcat.
  CLEAR w_fcat.

  w_fcat-fieldname = 'HKONT'.
  w_fcat-seltext_l = 'GST GL'.
  w_fcat-ref_tabname = 'BSET'.
*  W_FCAT-EMPHASIZE = 'C311'.
  w_fcat-outputlen = '18'.
  w_fcat-col_pos = '11'.
  APPEND w_fcat TO i_fcat.
  CLEAR w_fcat.

  w_fcat-fieldname = 'TXT50'.
  w_fcat-seltext_l = 'GST GL Des.'.
*  W_FCAT-REF_TABNAME = 'SKAT'.
*  W_FCAT-EMPHASIZE = 'C311'.
  w_fcat-outputlen = '20'.
  w_fcat-col_pos = '12'.
  APPEND w_fcat TO i_fcat.
  CLEAR w_fcat.

  w_fcat-fieldname = 'HWBAS'.
*  W_FCAT-REF_TABNAME = 'BSEG'.
  w_fcat-seltext_l = 'Base Amount'.
  w_fcat-emphasize = 'C110'.
  w_fcat-outputlen = '25'.
  w_fcat-col_pos = '13'.
  APPEND w_fcat TO i_fcat.
  CLEAR w_fcat.

  w_fcat-fieldname = 'DMBTR'.
*  W_FCAT-REF_TABNAME = 'BSEG'.
  w_fcat-seltext_l = 'Tax Amount'.
  w_fcat-emphasize = 'C110'.
  w_fcat-outputlen = '20'.
  w_fcat-col_pos = '14'.
  APPEND w_fcat TO i_fcat.
  CLEAR w_fcat.

  w_fcat-fieldname = 'KOSTL'.
  w_fcat-ref_tabname = 'BSEG'.
*  W_FCAT-EMPHASIZE = 'C311'.
  w_fcat-outputlen = '12'.
  w_fcat-col_pos = '15'.
  APPEND w_fcat TO i_fcat.
  CLEAR w_fcat.

  w_fcat-fieldname = 'KTEXT'.
*  W_FCAT-REF_TABNAME = 'CSKT'.
  w_fcat-seltext_l = 'Cost Center Des.'.
*  W_FCAT-EMPHASIZE = 'C311'.
  w_fcat-outputlen = '20'.
  w_fcat-col_pos = '16'.
  APPEND w_fcat TO i_fcat.
  CLEAR w_fcat.

  w_fcat-fieldname = 'MWSKZ'.
  w_fcat-seltext_l = 'Tax Code.'.
*  W_FCAT-REF_TABNAME = 'BSET'.
*  W_FCAT-EMPHASIZE = 'C311'.
  w_fcat-outputlen = '6'.
  w_fcat-col_pos = '17'.
  APPEND w_fcat TO i_fcat.
  CLEAR w_fcat.

  w_fcat-fieldname = 'E_GL'.
*  W_FCAT-REF_TABNAME = 'BSEG'.
  w_fcat-seltext_l = 'Exp. GL Code'.
*  W_FCAT-EMPHASIZE = 'C311'.
  w_fcat-outputlen = '12'.
  w_fcat-col_pos = '18'.
  APPEND w_fcat TO i_fcat.
  CLEAR w_fcat.

  w_fcat-fieldname = 'ED_GL'.
*  W_FCAT-REF_TABNAME = 'SKAT'.
  w_fcat-seltext_l = 'Exp GL Code Des.'.
*  W_FCAT-EMPHASIZE = 'C311'.
  w_fcat-outputlen = '20'.
  w_fcat-col_pos = '19'.
  APPEND w_fcat TO i_fcat.
  CLEAR w_fcat.

  w_fcat-fieldname = 'SGTXT'.
  w_fcat-ref_tabname = 'BSIK'.
*  W_FCAT-EMPHASIZE = 'C311'.
  w_fcat-outputlen = '42'.
  w_fcat-col_pos = '20'.
  APPEND w_fcat TO i_fcat.
  CLEAR w_fcat.

*  W_FCAT-FIELDNAME = 'AWKEY'.
*  W_FCAT-REF_TABNAME = 'BKPF'.
*  W_FCAT-EMPHASIZE = 'C311'.
*  W_FCAT-OUTPUTLEN = '40'.
*  W_FCAT-COL_POS = '21'.
*  APPEND W_FCAT TO I_FCAT.
*  CLEAR W_FCAT.
***********************24.12.2019
  w_fcat-fieldname = 'ANLN1'.
  w_fcat-ref_tabname = 'BSEG'.
*  W_FCAT-EMPHASIZE = 'C311'.
  w_fcat-outputlen = '12'.
  w_fcat-col_pos = '21'.
  APPEND w_fcat TO i_fcat.
  CLEAR w_fcat.

  w_fcat-fieldname = 'ANLN2'.
  w_fcat-ref_tabname = 'BSEG'.
*  W_FCAT-EMPHASIZE = 'C311'.
  w_fcat-outputlen = '6'.
  w_fcat-col_pos = '21'.
  APPEND w_fcat TO i_fcat.
  CLEAR w_fcat.

  w_fcat-fieldname = 'A_TXT50'.
*  W_FCAT-REF_TABNAME = 'BSIS'.
  w_fcat-seltext_l = 'Asset Des.'.
*  W_FCAT-EMPHASIZE = 'C311'.
  w_fcat-outputlen = '25'.
  w_fcat-col_pos = '21'.
  APPEND w_fcat TO i_fcat.
  CLEAR w_fcat.
***********************24.12.2019
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*
      i_callback_program = repid
*     I_CALLBACK_PF_STATUS_SET          = ' '
*     I_CALLBACK_USER_COMMAND           = ' '
*     I_CALLBACK_TOP_OF_PAGE            = 'HEADER'
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME   =
*     I_BACKGROUND_ID    = ' '
      i_grid_title       = 'GST REPORT'
*     I_GRID_SETTINGS    =
      is_layout          = layout
      it_fieldcat        = i_fcat
    TABLES
      t_outtab           = it_f1.

ENDFORM.
FORM del_bsis1.
  IF c GT 0.
    CASE wa_f1-del.
      WHEN 0.
        DELETE it_bsis1 INDEX c.
        c1 = 0.
      WHEN 1.
        IF c1 EQ 0.
          c1 = 1.
        ELSE.
          c1 = 0.
          DELETE it_bsis1 INDEX c.
        ENDIF.
*                      WHEN OTHERS.
    ENDCASE.
  ENDIF.
ENDFORM.
FORM del_bsas.
  CASE wa_f1-del.
    WHEN 0.
      DELETE it_bsas INDEX b.
      a = 0.
    WHEN 1.
      IF a EQ 0.
        a = 1.
      ELSE.
        a = 0.
        DELETE it_bsas INDEX b.
      ENDIF.
*                      WHEN OTHERS.
  ENDCASE.
ENDFORM.




























******TABLES : BKPF, BSEG, lfa1, SKAT.
******TYPES: BEGIN OF TY_FINAL,
******  BELNR TYPE BKPF-BELNR,    " Document Number
******  XMWST TYPE BKPF-XMWST,    " Calculate tax
******  GJAHR TYPE BKPF-GJAHR,    " Fiscal Year
******  BLART TYPE BKPF-BLART,    " Document Type
******  BLDAT TYPE BKPF-BLDAT,    " Document Date
******  BUDAT TYPE BKPF-BUDAT,    " Posting Date
******  CPUDT TYPE BKPF-CPUDT,    " Entry Date
******  MONAT TYPE BKPF-MONAT,    " Posting Period
******  XBLNR TYPE BKPF-XBLNR,    " Reference
******  BUZEI TYPE BSEG-BUZEI,    " Line item
******  BUZID TYPE BSEG-BUZID,    " Line item ID
******  QSSKZ TYPE BSEG-QSSKZ,    " Withholding Tax Code
******  HKONT TYPE BSEG-HKONT,    " G/L Account
******  LIFNR TYPE BSEG-LIFNR,    " Vendor
******  BSCHL TYPE BSEG-BSCHL,    " Posting Key
******  ZUONR TYPE BSEG-ZUONR,    " Assignment
******  DMBTR TYPE BSEG-DMBTR,    " Amount Base
******  CGST  TYPE BSEG-DMBTR,    " Amount CGST
******  SGST  TYPE BSEG-DMBTR,    " Amount SGST
******  IGST  TYPE BSEG-DMBTR,    " Amount IGST
******  MWSKZ TYPE BSEG-MWSKZ,    " TAX CODE
******  SGTXT TYPE BSEG-SGTXT,    " Text
******  name1 TYPE lfa1-name1,    " Vendor Name
******  TXT50 TYPE SKAT-TXT50,    " GL description
******  STENR TYPE lfa1-STENR,    " GST NO
****** END OF TY_FINAL.
******
******Types: BEGIN OF TY_BKPF,
******  BELNR TYPE BKPF-BELNR,    " Document Number
******  XMWST TYPE BKPF-XMWST,    " Calculate tax
******  GJAHR TYPE BKPF-GJAHR,    " Fiscal Year
******  BLART TYPE BKPF-BLART,    " Document Type
******  BLDAT TYPE BKPF-BLDAT,    " Document Date
******  BUDAT TYPE BKPF-BUDAT,    " Posting Date
******  CPUDT TYPE BKPF-CPUDT,    " Entry Date
******  MONAT TYPE BKPF-MONAT,    " Posting Period
******  XBLNR TYPE BKPF-XBLNR,    " Reference
******  AWKEY TYPE BKPF-AWKEY,    " Reference Key Invoice doc. Nu
******  END OF TY_BKPF.
******
******  TYPES: BEGIN OF TY_BSEG,
******  BELNR TYPE BSEG-BELNR,    " Document Number
******  BUZEI TYPE BSEG-BUZEI,    " Line item
******  BUZID TYPE BSEG-BUZID,    " Line item ID
******  QSSKZ TYPE BSEG-QSSKZ,    " Withholding Tax Code
******  HKONT TYPE BSEG-HKONT,    " G/L Account
******  LIFNR TYPE BSEG-LIFNR,    " Vendor
******  BSCHL TYPE BSEG-BSCHL,    " Posting Key
******  ZUONR TYPE BSEG-ZUONR,    " Assignment
******  DMBTR TYPE BSEG-DMBTR,    " Amount Base
******  SGST  TYPE BSEG-DMBTR,    " Amount SGST
******  IGST  TYPE BSEG-DMBTR,    " Amount IGST
******  MWSKZ TYPE BSEG-MWSKZ,    " TAX CODE
******  SGTXT TYPE BSEG-SGTXT,    " Text
******  CGST  TYPE BSEG-DMBTR,    " Amount CGST
******
******    END OF TY_BSEG.
******
******TYPES: BEGIN OF TY_F,
******  BLART     TYPE BKPF-BLART,    " Document Type
******  GJAHR     TYPE BKPF-GJAHR,    " Fiscal Year
******  BUDAT     TYPE  BKPF-BUDAT,   " Posting Date
******  BLDAT     TYPE  BKPF-BLDAT,   " Document Date
******  BELNR     TYPE  BKPF-BELNR,   " Document Number
******  XBLNR     TYPE  BKPF-XBLNR,   " Referance Number
******  ZUONR     TYPE  BSEG-ZUONR,   " Assignment
******  LIFNR     TYPE  BSEG-LIFNR,   " Vendor Code
*******  STENR     TYPE  lfa1-STENR,  " GST Number
*******  name1     TYPE  lfa1-name1,  " Vendor Name
******  HKONT     TYPE  BSEG-HKONT,   " Expense GL Code
*******  TXT50     TYPE  SKAT-TXT50,  " Expense GL Name
******  HWBAS     TYPE  BSEG-HWBAS,   " Taxable Value
******  DMBTR     TYPE  BSEG-DMBTR,   " GST Value
******  MWSKZ     TYPE  BSEG-MWSKZ,   " Tax Code
******  KOSTL     TYPE  BSEG-KOSTL,   " GL Code Cost Centre
******  SGTXT     TYPE  BSEG-SGTXT,   " Text
******  XREF3     TYPE  BSEG-XREF3,   " Reference Key 3
******  BUZID     TYPE  BSEG-BUZID,   " Line item ID
*******  GST_LD    TYPE  SKAT-TXT50,  " GST Ledger Description
******  STENR     TYPE  lfa1-STENR,   " GST Number
******  name1     TYPE  lfa1-name1,   " Vendor Name
******  TXT50     TYPE  SKAT-TXT50,   " Expense GL Name
******  GST_L     TYPE  BSEG-HKONT,   " GST Ledger code
******  GST_LD    TYPE  SKAT-TXT50,   " GST Ledger Description
******  AWKEY     TYPE BKPF-AWKEY,    " Reference Key Invoice doc. Nu
******  KTEXT     TYPE CSKT-KTEXT,    " Cost center Description
******  END OF TY_F.
******
******TYPES: BEGIN OF TY_B,
******  BELNR TYPE  BSEG-BELNR,   " Document Number
******  KOART TYPE  BSEG-KOART,   " Accoutn Type
******  ZUONR TYPE  BSEG-ZUONR,   " Assignment
******  LIFNR TYPE  BSEG-LIFNR,   " Vendor Code
******  HKONT TYPE  BSEG-HKONT,   " Expense GL Code
******  HWBAS TYPE  BSEG-HWBAS,   " Taxable Value
******  DMBTR TYPE  BSEG-DMBTR,   " GST Value
******  MWSKZ TYPE  BSEG-MWSKZ,   " Tax Code
******  KOSTL TYPE  BSEG-KOSTL,   " GL Code Cost Centre
******  SGTXT TYPE  BSEG-SGTXT,   " Text
******  XREF3 TYPE  BSEG-XREF3,   " Reference Key 3
******  BUZID TYPE  BSEG-BUZID,   " Line item ID
******  TXGRP TYPE  BSEG-TXGRP,   " Group Indicator
******  QSSKZ TYPE  BSEG-QSSKZ,   " Withholding tax
******  Dele(1)  TYPE  C,
******
******END OF TY_B.
******
******
******TYPES: BEGIN OF ty_lfa1,
******       lifnr TYPE lfa1-lifnr,
******       name1 TYPE lfa1-name1,
*******       bukrs TYPE lfb1-bukrs,
******       STENR TYPE lfa1-STENR,
******  END OF ty_lfa1.
******
******TYPES: BEGIN OF TY_SKAT,
******    TXT50 TYPE  SKAT-TXT50,
******End Of TY_SKAT.
******
******TYPES: BEGIN OF TY_CSKT,
******  KTEXT TYPE CSKT-KTEXT,
******  END OF TY_CSKT.
******
******DATA: IT_BKPF TYPE TABLE OF TY_BKPF,
******      WA_BKPF TYPE TY_BKPF,
******      IT_BSEG TYPE TABLE OF TY_BSEG,
******      WA_BSEG TYPE TY_BSEG,
******      it_lfa1 TYPE TABLE OF ty_lfa1,
******      wa_lfa1 TYPE ty_lfa1,
******      IT_FINAL TYPE TABLE OF TY_FINAL,
******      WA_FINAL TYPE TY_FINAL,
******      IT_FINAL1 TYPE TABLE OF TY_FINAL,
******      WA_FINAL1 TYPE TY_FINAL,
******      IT_SKAT Type TABLE OF TY_SKAT,
******      WA_SKAT TYPE TY_SKAT,
******      IT_CSKT Type TABLE OF TY_CSKT,
******      WA_CSKT TYPE TY_CSKT,
******      Ven TYPE BSEG-LIFNR,
******      IT_F TYPE TABLE OF TY_F,
******      WA_F TYPE TY_F,
******      IT_B TYPE TABLE OF TY_B,
******      WA_B TYPE TY_B,
******      IT_B1 TYPE TABLE OF TY_B,
******      WA_B1 TYPE TY_B,
******      IT_B2 TYPE TABLE OF TY_B,
******      WA_B2 TYPE TY_B,
******      Vend TYPE BSEG-LIFNR,
******      GL TYPE BSEG-HKONT,
******      COS TYPE BSEG-KOSTL.
******
******DATA REPID TYPE SY-REPID.
******TYPE-POOLS: CXTAB, SLIS.
******DATA: CONTROL_COLS TYPE CXTAB_COLUMN.
******
******   DATA: W_FCAT TYPE SLIS_FIELDCAT_ALV,
******         I_FCAT TYPE SLIS_T_FIELDCAT_ALV,
******         LAYOUT TYPE SLIS_LAYOUT_ALV.
******
******START-OF-SELECTION.
******SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE Text-001.
******  PARAMETERS: RB_1 RADIOBUTTON GROUP RG1 DEFAULT 'X',
******              RB_2 RADIOBUTTON GROUP RG1.
*******              RB_3 RADIOBUTTON GROUP RG1.
******  SELECT-OPTIONS: s_HKONT FOR BSEG-HKONT,             " GL
******                  s_BUDAT FOR BKPF-BUDAT OBLIGATORY.  " Date
******  PARAMETERS:     s_GJAHR Type BKPF-GJAHR OBLIGATORY.
******SELECTION-SCREEN: END OF BLOCK B1.
******
******
******    Select BELNR XMWST GJAHR BLART BLDAT BUDAT CPUDT MONAT XBLNR AWKEY FROM BKPF into TABLE IT_BKPF WHERE BUDAT IN S_BUDAT and XMWST EQ 'X'.
*******ELSEIF RB_2 IS NOT INITIAL.
*******    Select BELNR XMWST GJAHR BLART BLDAT BUDAT CPUDT MONAT XBLNR FROM BKPF into TABLE IT_BKPF WHERE BLDAT in S_BUDAT and XMWST EQ 'X'.
*******Else.
*******    Select BELNR XMWST GJAHR BLART BLDAT BUDAT CPUDT MONAT XBLNR FROM BKPF into TABLE IT_BKPF WHERE CPUDT in S_BUDAT and XMWST EQ 'X'.
******
******
******
******
******
******Break ABAP.
******IF RB_1 IS NOT INITIAL.
******SELECT lifnr name1 STENR FROM lfa1 INTO TABLE it_lfa1.
******  LOOP AT IT_BKPF INTO WA_BKPF.
******    SELECT BELNR  BUZEI BUZID QSSKZ HKONT LIFNR BSCHL ZUONR DMBTR FROM BSEG INTO TABLE IT_BSEG Where BELNR = WA_BKPF-BELNR AND GJAHR = WA_BKPF-GJAHR.
******        Sort it_bseg by BELNR ASCENDING lifnr DESCENDING.
******        LOOP AT IT_BSEG INTO WA_BSEG.
******               IF wa_bseg-LIFNR NE ''.
*******                 wa_final-LIFNR = wa_bseg-LIFNR.
******                 ven = WA_BSEG-LIFNR.
******                 READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_final-lifnr.
*******                 wa_final-name1 = wa_lfa1-name1.
*******                 wa_final-STENR = wa_lfa1-STENR.
******               ELSE.
******                 wa_final-LIFNR = ven.
******                 READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_final-lifnr.
*******                 wa_final-name1 = wa_lfa1-name1.
*******                 wa_final-STENR = wa_lfa1-STENR.
******
******               ENDIF.
******          IF wa_bseg-QSSKZ EQ '' and wa_bseg-MWSKZ NE '' and wa_bseg-BSCHL NE '31' .
******              wa_final-BELNR = wa_bkpf-BELNR.
******              wa_final-XMWST = wa_bkpf-XMWST.
******              wa_final-GJAHR = wa_bkpf-GJAHR.
******              wa_final-BLART = wa_bkpf-BLART.
******              wa_final-BLDAT = wa_bkpf-BLDAT.
******              wa_final-BUDAT = wa_bkpf-BUDAT.
******              wa_final-CPUDT = wa_bkpf-CPUDT.
******              wa_final-MONAT = wa_bkpf-MONAT.
******              wa_final-XBLNR = wa_bkpf-XBLNR.
******              wa_final-BUZEI = wa_bseg-BUZEI.
******              wa_final-BUZID = wa_bseg-BUZID.
******              wa_final-QSSKZ = wa_bseg-QSSKZ.
******              wa_final-HKONT = wa_bseg-HKONT.
******              wa_final-name1 = wa_lfa1-name1.
******              wa_final-STENR = wa_lfa1-STENR.
******              wa_final-LIFNR = ven.
******              Select TXT50 INTO TABLE it_SKAT FROM SKAT where SKAT~SAKNR = WA_final-HKONT and KTOPL = 'KPPL'.
******                 READ TABLE it_SKAT INTO wa_SKAT INDEX 1.
******                 WA_FINAL-TXT50 = WA_SKAT-TXT50.
******
******              wa_final-BSCHL = wa_bseg-BSCHL.
******              wa_final-ZUONR = wa_bseg-ZUONR.
******              CASE wa_bseg-HKONT.
******                WHEN 16007004 or 16007005 or 16007006 or 16007007 or 16007008 or 16007009 or 16007101 or 16007102 or 16007103 or
******                     16007107	OR 16007108 OR 16007109.
******                    wa_final-CGST  = wa_bseg-DMBTR.
******                    wa_final-DMBTR = ''.
******               	WHEN OTHERS.
******                    wa_final-CGST  = ''.
******                 wa_final-DMBTR = wa_bseg-DMBTR.
******              ENDCASE.
*******              IF wa_bseg-HKONT IN  GST." (16007004 16007005) ." or wa_bseg-HKONT EQ 16007005 or wa_bseg-HKONT EQ 16007006 or wa_bseg-HKONT EQ 16007007 or wa_bseg-HKONT EQ 16007008 or wa_bseg-HKONT EQ 16007009 .
*******                 wa_final-CGST  = wa_bseg-DMBTR.
*******                 wa_final-DMBTR = ''.
*******              ELSE.
*******                  wa_final-CGST  = ''.
*******                 wa_final-DMBTR = wa_bseg-DMBTR.
*******              ENDIF.
******              wa_final-MWSKZ = wa_bseg-MWSKZ.
******              wa_final-SGTXT = wa_bseg-SGTXT.
******              APPEND wa_final to it_final.
******          ENDIF.
******              Clear: wa_bseg, wa_skat.
******       ENDLOOP.
******       Clear: wa_bkpf,ven,wa_lfa1.
******   ENDLOOP.
******   PERFORM ALV.
******    ELSE.
******   LOOP AT IT_BKPF INTO WA_BKPF.
******      SELECT BELNR KOART ZUONR LIFNR HKONT HWBAS DMBTR MWSKZ KOSTL SGTXT XREF3 BUZID TXGRP QSSKZ FROM BSEG INTO TABLE IT_B  Where BELNR = WA_BKPF-BELNR
******        AND GJAHR = WA_BKPF-GJAHR AND BUZID EQ 'T'.
******
******      SELECT BELNR KOART ZUONR LIFNR HKONT HWBAS DMBTR MWSKZ KOSTL SGTXT XREF3 BUZID TXGRP QSSKZ FROM BSEG INTO TABLE IT_B2  Where BELNR EQ WA_BKPF-BELNR
******        AND GJAHR = WA_BKPF-GJAHR  AND KOART EQ 'K'.
******        READ TABLE IT_B2 INTO WA_B2 INDEX 1.
******        Vend = WA_B2-LIFNR.
******
*******        Sort it_bseg by BELNR ASCENDING lifnr DESCENDING.
******      LOOP AT  IT_B INTO WA_B.
******        SELECT BELNR KOART ZUONR LIFNR HKONT HWBAS DMBTR MWSKZ KOSTL SGTXT XREF3 BUZID TXGRP QSSKZ FROM BSEG INTO TABLE IT_B1  Where BELNR EQ WA_B-BELNR
******        AND GJAHR = WA_BKPF-GJAHR  AND TXGRP = WA_B-TXGRP AND BUZID NE 'T'.
******        LOOP AT IT_B1 INTO WA_B1.
******            WA_F-BUDAT  = WA_BKPF-BUDAT.
******            WA_F-BLDAT  = WA_BKPF-BLDAT.
******            WA_F-BELNR  = WA_BKPF-BELNR.
******            WA_F-GJAHR  = WA_BKPF-GJAHR.
******            WA_F-BLART  = WA_BKPF-BLART.
******            WA_F-XBLNR  = WA_BKPF-XBLNR.
******            WA_F-ZUONR  = WA_B-ZUONR.
******            WA_F-LIFNR  = VEND.
******            SELECT lifnr name1 STENR FROM lfa1 INTO TABLE it_lfa1 WHERE LIFNR EQ VEND.
******            READ TABLE IT_LFA1 INTO WA_LFA1 INDEX 1.
******            WA_F-STENR  = WA_LFA1-STENR.
******            WA_F-name1  = WA_LFA1-name1.
******            WA_F-HKONT  = WA_B-HKONT.
******            SELECT TXT50 FROM SKAT INTO TABLE IT_SKAT WHERE SAKNR EQ WA_B-HKONT AND KTOPL = 'KPPL'.
******            READ TABLE IT_SKAT INTO WA_SKAT INDEX 1.
******            WA_F-TXT50  = WA_SKAT-TXT50.
******            WA_F-HWBAS  = WA_B-HWBAS.
******            WA_F-DMBTR  = WA_B-DMBTR.
******            WA_F-KOSTL  = WA_B1-KOSTL.
******            SELECT KTEXT FROM CSKT INTO TABLE IT_CSKT WHERE KOSTL EQ WA_B1-KOSTL.
******            READ TABLE IT_CSKT INTO WA_CSKT INDEX 1.
******            WA_F-KTEXT  = WA_CSKT-KTEXT.
******            WA_F-MWSKZ  = WA_B-MWSKZ.
******            WA_F-GST_L  = WA_B1-HKONT.
******            Clear: WA_SKAT, IT_SKAT.
******            SELECT TXT50 FROM SKAT INTO TABLE IT_SKAT WHERE SAKNR EQ WA_B1-HKONT AND KTOPL = 'KPPL'.
******            READ TABLE IT_SKAT INTO WA_SKAT INDEX 1.
******            WA_F-GST_LD  = WA_SKAT-TXT50.
******            WA_F-SGTXT  = WA_B-SGTXT.
******            WA_F-AWKEY  = WA_BKPF-AWKEY.
******            APPEND WA_F TO IT_F.
******        ENDLOOP.
******       ENDLOOP.
******      CLEAR: WA_F, WA_SKAT, IT_SKAT,WA_CSKT, IT_CSKT, IT_LFA1, WA_LFA1,VEND, WA_BKPF, IT_B,IT_B1,IT_B2, WA_B,IT_B1,IT_B2.
******
******      ENDLOOP.
******    PERFORM ALV1.
******    ENDIF.
******
******FORM ALV1.
******  W_FCAT-FIELDNAME = 'BUDAT'.
******  W_FCAT-REF_TABNAME = 'BKPF'.
*******  W_FCAT-EMPHASIZE = 'C510'.
******  W_FCAT-OUTPUTLEN = '12'.
******  W_FCAT-COL_POS = '1'.
******  APPEND W_FCAT TO I_FCAT.
******  CLEAR W_FCAT.
******
******  W_FCAT-FIELDNAME = 'BLDAT'.
******  W_FCAT-REF_TABNAME = 'BKPF'.
******  W_FCAT-EMPHASIZE = 'C110'.
******  W_FCAT-OUTPUTLEN = '12'.
******  W_FCAT-COL_POS = '2'.
******  APPEND W_FCAT TO I_FCAT.
******  CLEAR W_FCAT.
******
******  W_FCAT-FIELDNAME = 'BELNR'.
******  W_FCAT-REF_TABNAME = 'BKPF'.
******  W_FCAT-EMPHASIZE = 'C311'.
******  W_FCAT-OUTPUTLEN = '12'.
******  W_FCAT-COL_POS = '3'.
******  APPEND W_FCAT TO I_FCAT.
******  CLEAR W_FCAT.
******
******  W_FCAT-FIELDNAME = 'GJAHR'.
******  W_FCAT-REF_TABNAME = 'BKPF'.
******  W_FCAT-EMPHASIZE = 'C311'.
******  W_FCAT-OUTPUTLEN = '6'.
******  W_FCAT-COL_POS = '4'.
******  APPEND W_FCAT TO I_FCAT.
******  CLEAR W_FCAT.
******
******  W_FCAT-FIELDNAME = 'BLART'.
******  W_FCAT-REF_TABNAME = 'BKPF'.
******  W_FCAT-EMPHASIZE = 'C311'.
******  W_FCAT-OUTPUTLEN = '5'.
******  W_FCAT-COL_POS = '5'.
******  APPEND W_FCAT TO I_FCAT.
******  CLEAR W_FCAT.
******
******  W_FCAT-FIELDNAME = 'XBLNR'.
******  W_FCAT-REF_TABNAME = 'BKPF'.
******  W_FCAT-EMPHASIZE = 'C311'.
******  W_FCAT-OUTPUTLEN = '20'.
******  W_FCAT-COL_POS = '6'.
******  APPEND W_FCAT TO I_FCAT.
******  CLEAR W_FCAT.
******
******  W_FCAT-FIELDNAME = 'ZUONR'.
******  W_FCAT-REF_TABNAME = 'BSEG'.
******  W_FCAT-EMPHASIZE = 'C311'.
******  W_FCAT-OUTPUTLEN = '20'.
******  W_FCAT-COL_POS = '7'.
******  APPEND W_FCAT TO I_FCAT.
******  CLEAR W_FCAT.
******
******  W_FCAT-FIELDNAME = 'LIFNR'.
******  W_FCAT-REF_TABNAME = 'BSEG'.
******  W_FCAT-EMPHASIZE = 'C311'.
******  W_FCAT-OUTPUTLEN = '8'.
******  W_FCAT-COL_POS = '8'.
******  APPEND W_FCAT TO I_FCAT.
******  CLEAR W_FCAT.
******
******  W_FCAT-FIELDNAME = 'STENR'.
******  W_FCAT-REF_TABNAME = 'LFA1'.
******  W_FCAT-EMPHASIZE = 'C311'.
******  W_FCAT-OUTPUTLEN = '18'.
******  W_FCAT-COL_POS = '9'.
******  APPEND W_FCAT TO I_FCAT.
******  CLEAR W_FCAT.
******
******  W_FCAT-FIELDNAME = 'NAME1'.
******  W_FCAT-REF_TABNAME = 'LFA1'.
******  W_FCAT-EMPHASIZE = 'C311'.
******  W_FCAT-OUTPUTLEN = '40'.
******  W_FCAT-COL_POS = '10'.
******  APPEND W_FCAT TO I_FCAT.
******  CLEAR W_FCAT.
******
******  W_FCAT-FIELDNAME = 'HKONT'.
******  W_FCAT-REF_TABNAME = 'BSEG'.
******  W_FCAT-EMPHASIZE = 'C311'.
******  W_FCAT-OUTPUTLEN = '12'.
******  W_FCAT-COL_POS = '11'.
******  APPEND W_FCAT TO I_FCAT.
******  CLEAR W_FCAT.
******
******  W_FCAT-FIELDNAME = 'TXT50'.
******  W_FCAT-REF_TABNAME = 'SKAT'.
******  W_FCAT-EMPHASIZE = 'C311'.
******  W_FCAT-OUTPUTLEN = '20'.
******  W_FCAT-COL_POS = '12'.
******  APPEND W_FCAT TO I_FCAT.
******  CLEAR W_FCAT.
******
******  W_FCAT-FIELDNAME = 'HWBAS'.
******  W_FCAT-REF_TABNAME = 'BSEG'.
******  W_FCAT-EMPHASIZE = 'C311'.
******  W_FCAT-OUTPUTLEN = '25'.
******  W_FCAT-COL_POS = '13'.
******  APPEND W_FCAT TO I_FCAT.
******  CLEAR W_FCAT.
******
******    W_FCAT-FIELDNAME = 'DMBTR'.
******  W_FCAT-REF_TABNAME = 'BSEG'.
******  W_FCAT-EMPHASIZE = 'C311'.
******  W_FCAT-OUTPUTLEN = '20'.
******  W_FCAT-COL_POS = '14'.
******  APPEND W_FCAT TO I_FCAT.
******  CLEAR W_FCAT.
******
******  W_FCAT-FIELDNAME = 'KOSTL'.
******  W_FCAT-REF_TABNAME = 'BSEG'.
******  W_FCAT-EMPHASIZE = 'C311'.
******  W_FCAT-OUTPUTLEN = '12'.
******  W_FCAT-COL_POS = '15'.
******  APPEND W_FCAT TO I_FCAT.
******  CLEAR W_FCAT.
******
******  W_FCAT-FIELDNAME = 'KTEXT'.
******  W_FCAT-REF_TABNAME = 'CSKT'.
******  W_FCAT-EMPHASIZE = 'C311'.
******  W_FCAT-OUTPUTLEN = '20'.
******  W_FCAT-COL_POS = '16'.
******  APPEND W_FCAT TO I_FCAT.
******  CLEAR W_FCAT.
******
******  W_FCAT-FIELDNAME = 'MWSKZ'.
******  W_FCAT-REF_TABNAME = 'BSEG'.
******  W_FCAT-EMPHASIZE = 'C311'.
******  W_FCAT-OUTPUTLEN = '6'.
******  W_FCAT-COL_POS = '17'.
******  APPEND W_FCAT TO I_FCAT.
******  CLEAR W_FCAT.
******
******  W_FCAT-FIELDNAME = 'GST_L'.
*******  W_FCAT-REF_TABNAME = 'BSEG'.
******  W_FCAT-Seltext_L = 'GST Ledger Code'.
******  W_FCAT-EMPHASIZE = 'C311'.
******  W_FCAT-OUTPUTLEN = '12'.
******  W_FCAT-COL_POS = '18'.
******  APPEND W_FCAT TO I_FCAT.
******  CLEAR W_FCAT.
******
******  W_FCAT-FIELDNAME = 'GST_LD'.
*******  W_FCAT-REF_TABNAME = 'SKAT'.
******  W_FCAT-SELTEXT_L = 'GST Ledger Code Des.'.
******  W_FCAT-EMPHASIZE = 'C311'.
******  W_FCAT-OUTPUTLEN = '20'.
******  W_FCAT-COL_POS = '19'.
******  APPEND W_FCAT TO I_FCAT.
******  CLEAR W_FCAT.
******
******  W_FCAT-FIELDNAME = 'SGTXT'.
******  W_FCAT-REF_TABNAME = 'BSEG'.
******  W_FCAT-EMPHASIZE = 'C311'.
******  W_FCAT-OUTPUTLEN = '42'.
******  W_FCAT-COL_POS = '20'.
******  APPEND W_FCAT TO I_FCAT.
******  CLEAR W_FCAT.
******
******  W_FCAT-FIELDNAME = 'AWKEY'.
******  W_FCAT-REF_TABNAME = 'BKPF'.
******  W_FCAT-EMPHASIZE = 'C311'.
******  W_FCAT-OUTPUTLEN = '40'.
******  W_FCAT-COL_POS = '21'.
******  APPEND W_FCAT TO I_FCAT.
******  CLEAR W_FCAT.
******
******   CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
******   EXPORTING
*******
******     I_CALLBACK_PROGRAM                = REPID
*******   I_CALLBACK_PF_STATUS_SET          = ' '
*******   I_CALLBACK_USER_COMMAND           = ' '
*******     I_CALLBACK_TOP_OF_PAGE            = 'HEADER'
*******   I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*******   I_CALLBACK_HTML_END_OF_LIST       = ' '
*******   I_STRUCTURE_NAME                  =
*******   I_BACKGROUND_ID                   = ' '
******     I_GRID_TITLE                      = 'GST REPORT'
*******   I_GRID_SETTINGS                   =
******     IS_LAYOUT                         = LAYOUT
******     IT_FIELDCAT                       = I_FCAT
******    TABLES
******      T_OUTTAB                          = it_f.
******
******
******ENDFORM.
******
******Form ALV.
*******Sort it_final by BUZEI ASCENDING.
******Sort it_final by BELNR ASCENDING.
******IF S_HKONT IS NOT INITIAL.
******   LOOP AT IT_Final INTO Wa_Final WHERE HKONT IN s_HKONT.
******    APPEND wa_final to it_final1.
******   ENDLOOP.
******   clear it_final.
******   it_final[] = it_final1[].
******ENDIF.
******
******
******
******
******  W_FCAT-FIELDNAME = 'BELNR'.
******  W_FCAT-REF_TABNAME = 'BKPF'.
******  W_FCAT-EMPHASIZE = 'C311'.
******  W_FCAT-OUTPUTLEN = '12'.
******  W_FCAT-COL_POS = '1'.
******  APPEND W_FCAT TO I_FCAT.
******  CLEAR W_FCAT.
******
******  W_FCAT-FIELDNAME = 'BUZEI'.
******  W_FCAT-REF_TABNAME = 'BSEG'.
******  W_FCAT-EMPHASIZE = 'C311'.
******  W_FCAT-OUTPUTLEN = '4'.
******  W_FCAT-COL_POS = '2'.
******  APPEND W_FCAT TO I_FCAT.
******  CLEAR W_FCAT.
******
******  W_FCAT-FIELDNAME = 'GJAHR'.
******  W_FCAT-REF_TABNAME = 'BKPF'.
******  W_FCAT-EMPHASIZE = 'C110'.
******  W_FCAT-OUTPUTLEN = '6'.
******  W_FCAT-COL_POS = '3'.
******  APPEND W_FCAT TO I_FCAT.
******  CLEAR W_FCAT.
******
******  W_FCAT-FIELDNAME = 'BLDAT'.
******  W_FCAT-REF_TABNAME = 'BKPF'.
******  W_FCAT-EMPHASIZE = 'C110'.
******  W_FCAT-OUTPUTLEN = '12'.
******  W_FCAT-COL_POS = '3'.
******  APPEND W_FCAT TO I_FCAT.
******  CLEAR W_FCAT.
******
******    W_FCAT-FIELDNAME = 'BUDAT'.
******  W_FCAT-REF_TABNAME = 'BKPF'.
******  W_FCAT-EMPHASIZE = 'C110'.
******  W_FCAT-OUTPUTLEN = '12'.
******  W_FCAT-COL_POS = '3'.
******  APPEND W_FCAT TO I_FCAT.
******  CLEAR W_FCAT.
******
******    W_FCAT-FIELDNAME = 'CPUDT'.
******  W_FCAT-REF_TABNAME = 'BKPF'.
******  W_FCAT-EMPHASIZE = 'C110'.
******  W_FCAT-OUTPUTLEN = '12'.
******  W_FCAT-COL_POS = '4'.
******  APPEND W_FCAT TO I_FCAT.
******  CLEAR W_FCAT.
******
******  W_FCAT-FIELDNAME = 'MONAT'.
******  W_FCAT-REF_TABNAME = 'BKPF'.
******  W_FCAT-EMPHASIZE = 'C110'.
******  W_FCAT-OUTPUTLEN = '4'.
******  W_FCAT-COL_POS = '5'.
******  APPEND W_FCAT TO I_FCAT.
******  CLEAR W_FCAT.
******
******  W_FCAT-FIELDNAME = 'XBLNR'.
******  W_FCAT-REF_TABNAME = 'BKPF'.
******  W_FCAT-EMPHASIZE = 'C110'.
******  W_FCAT-OUTPUTLEN = '20'.
******  W_FCAT-COL_POS = '6'.
******  APPEND W_FCAT TO I_FCAT.
******  CLEAR W_FCAT.
******
******  W_FCAT-FIELDNAME = 'HKONT'.
******  W_FCAT-REF_TABNAME = 'BSEG'.
******  W_FCAT-EMPHASIZE = 'C110'.
******  W_FCAT-OUTPUTLEN = '10'.
******  W_FCAT-COL_POS = '7'.
******  APPEND W_FCAT TO I_FCAT.
******  CLEAR W_FCAT.
******
******  W_FCAT-FIELDNAME = 'TXT50'.
******  W_FCAT-REF_TABNAME = 'SKAT'.
******  W_FCAT-EMPHASIZE = 'C110'.
******  W_FCAT-OUTPUTLEN = '25'.
******  W_FCAT-COL_POS = '8'.
******  APPEND W_FCAT TO I_FCAT.
******  CLEAR W_FCAT.
******
******  W_FCAT-FIELDNAME = 'LIFNR'.
******  W_FCAT-REF_TABNAME = 'BSEG'.
******  W_FCAT-EMPHASIZE = 'C110'.
******  W_FCAT-OUTPUTLEN = '10'.
******  W_FCAT-COL_POS = '9'.
******  APPEND W_FCAT TO I_FCAT.
******  CLEAR W_FCAT.
******
******  W_FCAT-FIELDNAME = 'name1'.
*******  W_FCAT-REF_TABNAME = 'lfa1'.
******  W_FCAT-SELTEXT_L  = 'Vendor Name'.
******  W_FCAT-EMPHASIZE = 'C110'.
******  W_FCAT-OUTPUTLEN = '25'.
******  W_FCAT-COL_POS = '10'.
******  APPEND W_FCAT TO I_FCAT.
******  CLEAR W_FCAT.
******
******  W_FCAT-FIELDNAME = 'STENR'.
*******  W_FCAT-REF_TABNAME = 'lfa1'.
******  W_FCAT-SELTEXT_L  = 'GST No'.
******  W_FCAT-EMPHASIZE = 'C110'.
******  W_FCAT-OUTPUTLEN = '20'.
******  W_FCAT-COL_POS = '11'.
******  APPEND W_FCAT TO I_FCAT.
******  CLEAR W_FCAT.
******
******  W_FCAT-FIELDNAME = 'BSCHL'.
******  W_FCAT-REF_TABNAME = 'BSEG'.
******  W_FCAT-EMPHASIZE = 'C110'.
******  W_FCAT-OUTPUTLEN = '4'.
******  W_FCAT-COL_POS = '12'.
******  APPEND W_FCAT TO I_FCAT.
******  CLEAR W_FCAT.
******
******  W_FCAT-FIELDNAME = 'ZUONR'.
******  W_FCAT-REF_TABNAME = 'BSEG'.
******  W_FCAT-EMPHASIZE = 'C110'.
******  W_FCAT-OUTPUTLEN = '20'.
******  W_FCAT-COL_POS = '13'.
******  APPEND W_FCAT TO I_FCAT.
******  CLEAR W_FCAT.
******
******  W_FCAT-FIELDNAME = 'DMBTR'.
******  W_FCAT-REF_TABNAME = 'BSEG'.
******  W_FCAT-EMPHASIZE = 'C110'.
******  W_FCAT-OUTPUTLEN = '12'.
******  W_FCAT-COL_POS = '14'.
******  APPEND W_FCAT TO I_FCAT.
******  CLEAR W_FCAT.
******
******  W_FCAT-FIELDNAME = 'CGST'.
*******  W_FCAT-REF_TABNAME = 'IT_Final'.
******  W_FCAT-SELTEXT_L  = 'GST Value'.
******  W_FCAT-EMPHASIZE = 'C110'.
******  W_FCAT-OUTPUTLEN = '12'.
******  W_FCAT-COL_POS = '15'.
******  APPEND W_FCAT TO I_FCAT.
******  CLEAR W_FCAT.
******
******  W_FCAT-FIELDNAME = 'MWSKZ'.
******  W_FCAT-REF_TABNAME = 'BSEG'.
******  W_FCAT-EMPHASIZE = 'C110'.
******  W_FCAT-OUTPUTLEN = '6'.
******  W_FCAT-COL_POS = '16'.
******  APPEND W_FCAT TO I_FCAT.
******  CLEAR W_FCAT.
******
******  W_FCAT-FIELDNAME = 'SGTXT'.
******  W_FCAT-REF_TABNAME = 'BSEG'.
******  W_FCAT-EMPHASIZE = 'C110'.
******  W_FCAT-OUTPUTLEN = '25'.
******  W_FCAT-COL_POS = '17'.
******  APPEND W_FCAT TO I_FCAT.
******  CLEAR W_FCAT.
******
******      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
******   EXPORTING
*******
******     I_CALLBACK_PROGRAM                = REPID
*******   I_CALLBACK_PF_STATUS_SET          = ' '
*******   I_CALLBACK_USER_COMMAND           = ' '
*******     I_CALLBACK_TOP_OF_PAGE            = 'HEADER'
*******   I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*******   I_CALLBACK_HTML_END_OF_LIST       = ' '
*******   I_STRUCTURE_NAME                  =
*******   I_BACKGROUND_ID                   = ' '
******     I_GRID_TITLE                      = 'GST REPORT'
*******   I_GRID_SETTINGS                   =
******     IS_LAYOUT                         = LAYOUT
******     IT_FIELDCAT                       = I_FCAT
******    TABLES
******      T_OUTTAB                          = it_final.
******ENDFORM.
******
*******ELSE.
*******
******* SELECT A~BUDAT A~BLDAT A~BELNR A~XBLNR B~ZUONR B~LIFNR B~HKONT B~HWBAS B~DMBTR B~MWSKZ B~KOSTL B~SGTXT B~XREF3 B~BUZID INTO TABLE IT_F
*******        FROM BKPF AS A INNER JOIN BSEG AS B ON A~BELNR EQ B~BELNR WHERE A~BUDAT IN S_BUDAT and B~KOSTL IN S_HKONT.
*******
*******
*******
*******ENDIF.
