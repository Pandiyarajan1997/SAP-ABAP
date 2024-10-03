*&---------------------------------------------------------------------*
*& Report ZFI_TDS_REP_FINAL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfi_tds_rep_final.


TABLES: bsis,bsas,with_item,bsak.

TYPES: BEGIN OF ty_pan,
         j_1ipanno TYPE j_1imovend-j_1ipanno,
       END OF ty_pan.


TYPES: BEGIN OF ty_b,
         hkont TYPE bsis-hkont, " G/L Account
         blart TYPE bsis-blart, " Document Type
       END OF ty_b.

TYPES: BEGIN OF ty_td,
         bukrs TYPE bsis-bukrs, " Company Code
         blart TYPE bsis-blart, " Document Type
*  LIFNR TYPE BSAK-LIFNR, " Vendor
         zuonr TYPE bsis-zuonr, " Assignment
         gjahr TYPE bsis-gjahr, " Fiscal Year
         belnr TYPE bsis-belnr, " Document Number
         buzei TYPE bsis-buzei, " Line item
         budat TYPE bsis-budat, " Posting Date
         bldat TYPE bsis-bldat, " Document Date
*  CPUDT TYPE BSIS-CPUDT, " Entry Date
         waers TYPE bsis-waers, " Currency
         shkzg TYPE bsis-shkzg, " Debit/Credit.
         hkont TYPE bsis-hkont, " G/L Account
         dmbtr TYPE bsis-dmbtr, " Amount in LC
         sgtxt TYPE bsis-sgtxt, " Text
       END OF ty_td.


TYPES: BEGIN OF ty_f,
         bukrs     TYPE bsak-bukrs, " Company Code
*LIFNR TYPE BSAK-LIFNR, " Vendor
         zuonr     TYPE bsak-zuonr, " Assignment
         gjahr     TYPE bsak-gjahr, " Fiscal Year
         belnr     TYPE bsak-belnr, " Document Number
         buzei     TYPE bsak-buzei, " Line item
         budat     TYPE bsak-budat, " Posting Date
         bldat     TYPE bsak-bldat, " Document Date
         waers     TYPE bsak-waers, " Currency
         shkzg     TYPE bsak-shkzg, " Debit / Credit
         dmbtr     TYPE bsis-dmbtr, " Amount in LC
         sgtxt     TYPE bsis-sgtxt, " Text
         skfbt     TYPE bsak-skfbt, " Discount base
         witht     TYPE with_item-witht,        " Withholding tax type
         wt_withcd TYPE with_item-wt_withcd,    " Withholding tax code
         wt_qsshh  TYPE with_item-wt_qsshh,     " W/tax base amnt in LC
         wt_qbshh  TYPE with_item-wt_qbshh,    " W/tax aux. amount in LC
         wt_acco   TYPE with_item-wt_acco,      " Customer/vendor account no.
         qsatz     TYPE with_item-qsatz,        " Withholding tax rate
         hkont     TYPE with_item-hkont, " GL
         vname     TYPE string,
         gl_n1     TYPE bsak-hkont,
         txt20     TYPE skat-txt20,
         gl_n      TYPE skat-txt20,
         wt_d      TYPE t059u-text40,
         blart     TYPE bsis-blart, " Document Type
         cpudt     TYPE bsak-cpudt, " Entry Date
         xblnr     TYPE bkpf-xblnr, " Reference
         bktxt     TYPE bkpf-bktxt, " Document Header Text
         ppnam     TYPE bkpf-ppnam, " Parked by
         usnam     TYPE bkpf-usnam, " User name
         name_text TYPE adrp-name_text, " User Name Desc.
         j_1ipanno TYPE j_1imovend-j_1ipanno, " PAN
         flag      TYPE c,
       END OF ty_f.

TYPES: BEGIN OF ty_wh,
         witht     TYPE with_item-witht,        " Withholding tax type
         wt_withcd TYPE with_item-wt_withcd,    " Withholding tax code
         wt_qsshh  TYPE with_item-wt_qsshh,     " W/tax base amnt in LC
         wt_qbshh  TYPE with_item-wt_qbshh,    " W/tax aux. amount in LC
         wt_acco   TYPE with_item-wt_acco,      " Customer/vendor account no.
         qsatz     TYPE with_item-qsatz,        " Withholding tax rate
         hkont     TYPE with_item-hkont, " GL
       END OF ty_wh.

TYPES: BEGIN OF ty_gl,
         saknr TYPE skat-saknr,
         txt20 TYPE skat-txt20,
       END OF ty_gl.

TYPES: BEGIN OF ty_v,
         lifnr TYPE lfa1-lifnr,
         name1 TYPE lfa1-name1,
       END OF ty_v.

TYPES: BEGIN OF ty_c,
         kunnr TYPE kna1-kunnr,
         name1 TYPE kna1-name1,
       END OF ty_c.

TYPES: BEGIN OF ty_wt,
         witht  TYPE t059u-witht,
         text40 TYPE t059u-text40,
       END OF ty_wt.

TYPES: BEGIN OF ty_ad,
*BLART TYPE BSIS-BLART, " Document Type
         cpudt TYPE bsak-cpudt, " Entry Date
         xblnr TYPE bkpf-xblnr, " Reference
         bktxt TYPE bkpf-bktxt, " Document Header Text
         ppnam TYPE bkpf-ppnam, " Parked by
         usnam TYPE bkpf-usnam, " User name
*NAME_TEXT TYPE ADRP-NAME_TEXT, " User Name Desc.
       END OF ty_ad.

TYPES: BEGIN OF ty_un,
         bname     TYPE usr21-bname, " User
         name_text TYPE adrp-name_text, " User Name Desc.
       END OF ty_un.

TYPES: BEGIN OF ty_gla,
         racct TYPE faglflext-racct,
         hslvt TYPE faglflext-hslvt,
         hsl01 TYPE faglflext-hsl01,
         hsl02 TYPE faglflext-hsl02,
         hsl03 TYPE faglflext-hsl03,
         hsl04 TYPE faglflext-hsl04,
         hsl05 TYPE faglflext-hsl05,
         hsl06 TYPE faglflext-hsl06,
         hsl07 TYPE faglflext-hsl07,
         hsl08 TYPE faglflext-hsl08,
         hsl09 TYPE faglflext-hsl09,
         hsl10 TYPE faglflext-hsl10,
         hsl11 TYPE faglflext-hsl11,
         hsl12 TYPE faglflext-hsl12,
         hsl13 TYPE faglflext-hsl13,
         hsl14 TYPE faglflext-hsl14,
         hsl15 TYPE faglflext-hsl15,
         hsl16 TYPE faglflext-hsl16,

       END OF ty_gla.

TYPES: BEGIN OF ty_g,
         hkont TYPE with_item-hkont, " GL
         gl_n  TYPE skat-txt20,
         dmbtr TYPE bsis-dmbtr, " Amount in LC
       END OF ty_g.
*
*TYPES: BEGIN OF ty_cl,
*         belnr   TYPE bsak-belnr, " Document Number
*         blart   TYPE bsis-blart, " Document Type
*         hkont   TYPE with_item-hkont, " GL
*         wt_acco TYPE with_item-wt_acco,      " Customer/vendor account no.
*         wt_qsshh   TYPE with_item-wt_qsshh, " Amount in LC
*       END OF ty_cl.
*DATA: lt_amt_clr TYPE STANDARD TABLE OF ty_cl,
*      lw_amt_clr TYPE ty_cl.

DATA: it_f    TYPE TABLE OF ty_f,
      wa_f    TYPE ty_f,
      it_f1   TYPE TABLE OF ty_f,
      wa_f1   TYPE ty_f,
      it_f2   TYPE TABLE OF ty_td,
      wa_f2   TYPE ty_td,
      it_v    TYPE TABLE OF ty_v,
      wa_v    TYPE ty_v,
      it_b    TYPE TABLE OF ty_b,
      wa_b    TYPE ty_b,
      it_b1   TYPE TABLE OF ty_b,
      wa_b1   TYPE ty_b,
      it_c    TYPE TABLE OF ty_c,
      wa_c    TYPE ty_c,
      it_pan  TYPE TABLE OF ty_pan,
      wa_pan  TYPE ty_pan,
      it_wt   TYPE TABLE OF ty_wt,
      wa_wt   TYPE ty_wt,
      it_ad   TYPE TABLE OF ty_ad,
      wa_ad   TYPE ty_ad,
      it_un   TYPE TABLE OF ty_un,
      wa_un   TYPE ty_un,
      it_wh   TYPE TABLE OF ty_wh,
      wa_wh   TYPE ty_wh,
      it_gl   TYPE TABLE OF ty_gl,
      wa_gl   TYPE ty_gl,
      d1      TYPE faglflext-rpmax,
      d2      TYPE faglflext-rpmax,
      f       TYPE bsis-gjahr,
      it_gla  TYPE TABLE OF ty_gla,
      wa_gla  TYPE ty_gla,
      it_gla1 TYPE TABLE OF ty_gla,
      wa_gla1 TYPE ty_gla,
      it_g    TYPE TABLE OF ty_g,
      wa_g    TYPE ty_g.

DATA: w_fcat                 TYPE slis_fieldcat_alv,
      i_fcat                 TYPE slis_t_fieldcat_alv,
      layout                 TYPE slis_layout_alv,
      g_top_of_page          TYPE slis_formname VALUE 'F_TOP_OF_PAGE', "for avl header.
      gt_callback_subroutine TYPE slis_formname VALUE 'USER_COMMAND'.

DATA: ld_currm TYPE bkpf-monat,
      ld_curry TYPE bkpf-gjahr,
      ld_prevm TYPE bkpf-monat,
      ld_prevy TYPE bkpf-gjahr.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

*  PARAMETERS: RB_1 RADIOBUTTON GROUP RG1 DEFAULT 'X' user-command pc, " TDS Opening
*              RB_2 RADIOBUTTON GROUP RG1,             " TDS entry based on date
**              RB_3 RADIOBUTTON GROUP RG1,             " Entry Date
*              DATE TYPE BSAK-BUDAT MODIF ID 1.
  PARAMETERS : p_bukrs TYPE bukrs OBLIGATORY.
  SELECT-OPTIONS: s_budat FOR bsak-budat MODIF ID 2,
*                  S_GJAHR FOR BSAK-GJAHR," OBLIGATORY.
                  s_hkont FOR bsis-hkont.
*  PARAMETERS : wt_acco   TYPE with_item-wt_acco.
*  PARAMETERS : p_belnr TYPE bkpf-belnr.
  PARAMETERS: r1 TYPE c RADIOBUTTON GROUP r,
              r2 TYPE c RADIOBUTTON GROUP r.
  PARAMETERS: p_c AS CHECKBOX.
SELECTION-SCREEN: END OF BLOCK b1.

START-OF-SELECTION.

*AT SELECTION-SCREEN OUTPUT.
*    IF rb_1 = 'X'.
*       LOOP AT SCREEN.
*         IF screen-group1 = '2'.
**             screen-active = 0.
*             SCREEN-INPUT = 0.
*             screen-invisible = 1.
*             MODIFY SCREEN.
*         ENDIF.
*       ENDLOOP.
*     ELSEIF rb_2 = 'X'.
*       LOOP AT SCREEN.
*         IF screen-group1 = '1'.
**             screen-active = 0.
*             SCREEN-INPUT = 0.
*             screen-invisible = 1.
*             MODIFY SCREEN.
*         ENDIF.
*       ENDLOOP.
*    ENDIF.

START-OF-SELECTION.

* IF RB_1 IS NOT INITIAL.
*   IF DATE IS INITIAL.
*     MESSAGE ' Enter Date Field ' TYPE 'S' DISPLAY LIKE 'E'.
*     EXIT.
*   ENDIF.
*      IF S_HKONT IS INITIAL.
*     MESSAGE ' Enter TDS GL ' TYPE 'S' DISPLAY LIKE 'E'.
*     EXIT.
*   ENDIF.
* ELSE.
  IF s_hkont IS INITIAL.
    MESSAGE ' Enter TDS GL ' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.
  IF s_budat IS INITIAL.
    MESSAGE ' Enter Date Field ' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  CALL FUNCTION 'GET_CURRENT_YEAR'
    EXPORTING
      bukrs = '1080'
      date  = s_budat-low
    IMPORTING
      currm = ld_currm
      curry = ld_curry
      prevm = ld_prevm
      prevy = ld_prevy.

  f = ld_curry.
  CALL FUNCTION 'GET_CURRENT_YEAR'
    EXPORTING
      bukrs = '1080'
      date  = s_budat-high
    IMPORTING
      currm = ld_currm
      curry = ld_curry
      prevm = ld_prevm
      prevy = ld_prevy.

  IF f NE ld_curry.
    MESSAGE ' Enter Date based on fiscal year ' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

* ENDIF.

  SELECT witht text40 INTO TABLE it_wt FROM t059u WHERE land1 EQ 'IN'.
  SELECT saknr txt20 INTO TABLE it_gl FROM skat WHERE ktopl = 'YAIN'.
  SELECT lifnr name1 INTO TABLE it_v FROM lfa1.
  SELECT kunnr name1 INTO TABLE it_c FROM kna1.

  CALL FUNCTION 'GET_CURRENT_YEAR'
    EXPORTING
      bukrs = '1080'
      date  = s_budat-high
    IMPORTING
      currm = ld_currm
      curry = ld_curry
      prevm = ld_prevm
      prevy = ld_prevy.

  IF p_c IS NOT INITIAL .
    SELECT racct hslvt hsl01 hsl02 hsl03 hsl04 hsl05 hsl06 hsl07 hsl08 hsl09 hsl10 hsl11 hsl12 hsl13 hsl14 hsl15 hsl16
    FROM faglflext INTO TABLE it_gla WHERE ryear = ld_curry AND racct IN s_hkont.

    LOOP AT it_gla INTO wa_gla.

      COLLECT wa_gla INTO it_gla1.

    ENDLOOP.

    CLEAR: it_gla.
    it_gla[] = it_gla1[].
    CLEAR: it_gla1.
  ENDIF.

*LOOP AT IT_GLA INTO WA_GLA.
*  CASE ld_currm.
*    WHEN 01.
*      WA_G-DMBTR = WA_GLA-HSLVT .
*    WHEN 02.
*      WA_G-DMBTR = WA_GLA-HSLVT + WA_GLA-HSL01.
*    WHEN 03.
*      WA_G-DMBTR = WA_GLA-HSLVT + WA_GLA-HSL01 + WA_GLA-HSL02 .
*    WHEN 04.
*      WA_G-DMBTR = WA_GLA-HSLVT + WA_GLA-HSL01 + WA_GLA-HSL02 + WA_GLA-HSL03 .
*    WHEN 05.
*      WA_G-DMBTR = WA_GLA-HSLVT + WA_GLA-HSL01 + WA_GLA-HSL02 + WA_GLA-HSL03 + WA_GLA-HSL04 .
*    WHEN 06.
*      WA_G-DMBTR = WA_GLA-HSLVT + WA_GLA-HSL01 + WA_GLA-HSL02 + WA_GLA-HSL03 + WA_GLA-HSL04 + WA_GLA-HSL05 .
*    WHEN 07.
*      WA_G-DMBTR = WA_GLA-HSLVT + WA_GLA-HSL01 + WA_GLA-HSL02 + WA_GLA-HSL03 + WA_GLA-HSL04 + WA_GLA-HSL05 + WA_GLA-HSL06 .
*    WHEN 08.
*      WA_G-DMBTR = WA_GLA-HSLVT + WA_GLA-HSL01 + WA_GLA-HSL02 + WA_GLA-HSL03 + WA_GLA-HSL04 + WA_GLA-HSL05 + WA_GLA-HSL06 + WA_GLA-HSL07 .
*    WHEN 09.
*      WA_G-DMBTR = WA_GLA-HSLVT + WA_GLA-HSL01 + WA_GLA-HSL02 + WA_GLA-HSL03 + WA_GLA-HSL04 + WA_GLA-HSL05 + WA_GLA-HSL06 + WA_GLA-HSL07 + WA_GLA-HSL08 .
*    WHEN 10.
*      WA_G-DMBTR = WA_GLA-HSLVT + WA_GLA-HSL01 + WA_GLA-HSL02 + WA_GLA-HSL03 + WA_GLA-HSL04 + WA_GLA-HSL05 + WA_GLA-HSL06 + WA_GLA-HSL07 + WA_GLA-HSL08 + WA_GLA-HSL09 .
*    WHEN 11.
*      WA_G-DMBTR = WA_GLA-HSLVT + WA_GLA-HSL01 + WA_GLA-HSL02 + WA_GLA-HSL03 + WA_GLA-HSL04 + WA_GLA-HSL05 + WA_GLA-HSL06 + WA_GLA-HSL07 + WA_GLA-HSL08 + WA_GLA-HSL09 + WA_GLA-HSL10 .
*    WHEN 12.
*      WA_G-DMBTR = WA_GLA-HSLVT + WA_GLA-HSL01 + WA_GLA-HSL02 + WA_GLA-HSL03 + WA_GLA-HSL04 + WA_GLA-HSL05 + WA_GLA-HSL06 + WA_GLA-HSL07 + WA_GLA-HSL08 + WA_GLA-HSL09 + WA_GLA-HSL10 + WA_GLA-HSL11 .
**    WHEN OTHERS.
**      WA_G-DMBTR =  WA_GLA-HSLVT + WA_GLA-HSL01 + WA_GLA-HSL02 + WA_GLA-HSL03 + WA_GLA-HSL04 + WA_GLA-HSL05 + WA_GLA-HSL06 + WA_GLA-HSL07 + WA_GLA-HSL08 + WA_GLA-HSL09 + WA_GLA-HSL10 + WA_GLA-HSL11 + WA_GLA-HSL12 + WA_GLA-HSL13 + WA_GLA-HSL14 +
**WA_GLA-HSL15 + WA_GLA-HSL16.
*  ENDCASE.
**  APPEND WA_GLA TO IT_GLA1.
*  READ TABLE IT_GL INTO WA_GL WITH KEY SAKNR = WA_GLA-RACCT.
*  WA_G-GL_N  = WA_GL-TXT20.
*  WA_G-HKONT = WA_GLA-RACCT.
*  APPEND WA_G TO IT_G.
*
*ENDLOOP.


** IF RB_1 IS NOT INITIAL.
***SELECT A~BUKRS A~ZUONR A~GJAHR A~BELNR A~BUZEI A~BUDAT A~BLDAT A~CPUDT A~WAERS A~SKFBT B~WITHT B~WT_WITHCD B~WT_QSSHH
***  B~WT_QBSHHA B~WT_ACCO B~QSATZ B~HKONT INTO TABLE IT_F1 FROM BSIK AS A INNER JOIN WITH_ITEM AS B ON A~BELNR EQ B~BELNR AND
***  A~BUZEI EQ B~BUZEI AND A~GJAHR EQ B~GJAHR WHERE A~BUDAT IN S_BUDAT AND A~QSSKZ EQ 'XX' ."AND B~WT_WITHCD NE ''. "AND A~QSSHB GT 0 .
***APPEND LINES OF IT_F1 TO IT_F.
***Clear IT_F1.
***SELECT A~BUKRS A~ZUONR A~GJAHR A~BELNR A~BUZEI A~BUDAT A~BLDAT A~CPUDT A~WAERS A~SKFBT B~WITHT B~WT_WITHCD B~WT_QSSHH
***  B~WT_QBSHHA B~WT_ACCO B~QSATZ B~HKONT INTO TABLE IT_F1 FROM BSAK AS A INNER JOIN WITH_ITEM AS B ON A~BELNR EQ B~BELNR AND
***  A~BUZEI EQ B~BUZEI AND A~GJAHR EQ B~GJAHR WHERE A~BUDAT IN S_BUDAT AND A~QSSKZ EQ 'XX' ."AND B~WT_WITHCD NE ''. "AND A~QSSHB GT 0 .
***APPEND LINES OF IT_F1 TO IT_F.
***Clear IT_F1.
**
**   Break KPPLTEST.
**
**SELECT BUKRS ZUONR GJAHR BELNR BUZEI BUDAT BLDAT CPUDT WAERS SKFBT INTO TABLE IT_F1 FROM BSIK WHERE BUDAT LE DATE AND QSSKZ EQ 'XX' AND QSSHB GT 0 . "AND A~QSSHB GT 0 .
**APPEND LINES OF IT_F1 TO IT_F.
**Clear IT_F1.
**SELECT BUKRS ZUONR GJAHR BELNR BUZEI BUDAT BLDAT CPUDT WAERS SKFBT INTO TABLE IT_F1 FROM BSAK WHERE BUDAT LE DATE AND AUGDT GT DATE AND QSSKZ EQ 'XX' AND QSSHB GT 0 . "AND B~WT_WITHCD NE ''. "AND A~QSSHB GT 0 .
**APPEND LINES OF IT_F1 TO IT_F.
**Clear IT_F1.
**
** ELSEIF RB_2 IS NOT INITIAL.
***SELECT A~BUKRS A~ZUONR A~GJAHR A~BELNR A~BUZEI A~BUDAT A~BLDAT A~CPUDT A~WAERS A~SKFBT B~WITHT B~WT_WITHCD B~WT_QSSHH
***  B~WT_QBSHHA B~WT_ACCO B~QSATZ B~HKONT INTO TABLE IT_F1 FROM BSIK AS A INNER JOIN WITH_ITEM AS B ON A~BELNR EQ B~BELNR AND
***  A~BUZEI EQ B~BUZEI AND A~GJAHR EQ B~GJAHR WHERE A~BLDAT IN S_BUDAT AND A~QSSKZ EQ 'XX' AND B~WT_WITHCD NE ' '. "AND A~QSSHB GT 0 .
***APPEND LINES OF IT_F1 TO IT_F.
***Clear IT_F1.
***SELECT A~BUKRS A~ZUONR A~GJAHR A~BELNR A~BUZEI A~BUDAT A~BLDAT A~CPUDT A~WAERS A~SKFBT B~WITHT B~WT_WITHCD B~WT_QSSHH
***  B~WT_QBSHHA B~WT_ACCO B~QSATZ B~HKONT INTO TABLE IT_F1 FROM BSAK AS A INNER JOIN WITH_ITEM AS B ON A~BELNR EQ B~BELNR AND
***  A~BUZEI EQ B~BUZEI AND A~GJAHR EQ B~GJAHR WHERE A~BLDAT IN S_BUDAT AND A~QSSKZ EQ 'XX' AND B~WT_WITHCD NE ' '. "AND A~QSSHB GT 0 .
***APPEND LINES OF IT_F1 TO IT_F.
***Clear IT_F1.
**
**SELECT BUKRS ZUONR GJAHR BELNR BUZEI BUDAT BLDAT CPUDT WAERS SKFBT INTO TABLE IT_F1 FROM BSIK WHERE BLDAT IN S_BUDAT AND QSSKZ EQ 'XX' AND QSSHB GT 0 . "AND B~WT_WITHCD NE ''. "AND A~QSSHB GT 0 .
**APPEND LINES OF IT_F1 TO IT_F.
**Clear IT_F1.
**SELECT BUKRS ZUONR GJAHR BELNR BUZEI BUDAT BLDAT CPUDT WAERS SKFBT INTO TABLE IT_F1 FROM BSAK WHERE BLDAT IN S_BUDAT AND QSSKZ EQ 'XX' AND QSSHB GT 0 . "AND B~WT_WITHCD NE ''. "AND A~QSSHB GT 0 .
**APPEND LINES OF IT_F1 TO IT_F.
**Clear IT_F1.
**
** ELSEIF RB_3 IS NOT INITIAL.
***SELECT A~BUKRS A~ZUONR A~GJAHR A~BELNR A~BUZEI A~BUDAT A~BLDAT A~CPUDT A~WAERS A~SKFBT B~WITHT B~WT_WITHCD B~WT_QSSHH
***  B~WT_QBSHHA B~WT_ACCO B~QSATZ B~HKONT INTO TABLE IT_F1 FROM BSIK AS A INNER JOIN WITH_ITEM AS B ON A~BELNR EQ B~BELNR AND
***  A~BUZEI EQ B~BUZEI AND A~GJAHR EQ B~GJAHR WHERE A~CPUDT IN S_BUDAT AND A~QSSKZ EQ 'XX' AND B~WT_WITHCD NE ' '. "AND A~QSSHB GT 0 .
***APPEND LINES OF IT_F1 TO IT_F.
***Clear IT_F1.
***SELECT A~BUKRS A~ZUONR A~GJAHR A~BELNR A~BUZEI A~BUDAT A~BLDAT A~CPUDT A~WAERS A~SKFBT B~WITHT B~WT_WITHCD B~WT_QSSHH
***  B~WT_QBSHHA B~WT_ACCO B~QSATZ B~HKONT INTO TABLE IT_F1 FROM BSAK AS A INNER JOIN WITH_ITEM AS B ON A~BELNR EQ B~BELNR AND
***  A~BUZEI EQ B~BUZEI AND A~GJAHR EQ B~GJAHR WHERE A~CPUDT IN S_BUDAT AND A~QSSKZ EQ 'XX' AND B~WT_WITHCD NE ' '. "AND A~QSSHB GT 0 .
***APPEND LINES OF IT_F1 TO IT_F.
***Clear IT_F1.
**SELECT BUKRS ZUONR GJAHR BELNR BUZEI BUDAT BLDAT CPUDT WAERS SKFBT INTO TABLE IT_F1 FROM BSIK WHERE CPUDT IN S_BUDAT AND QSSKZ EQ 'XX' AND QSSHB GT 0 ."AND B~WT_WITHCD NE ''. "AND A~QSSHB GT 0 .
**APPEND LINES OF IT_F1 TO IT_F.
**Clear IT_F1.
**SELECT BUKRS ZUONR GJAHR BELNR BUZEI BUDAT BLDAT CPUDT WAERS SKFBT INTO TABLE IT_F1 FROM BSAK WHERE CPUDT IN S_BUDAT AND QSSKZ EQ 'XX' AND QSSHB GT 0 ."AND B~WT_WITHCD NE ''. "AND A~QSSHB GT 0 .
**APPEND LINES OF IT_F1 TO IT_F.
**Clear IT_F1.
**
**
** ENDIF.

*Break KPPLTEST.

*IF RB_1 IS NOT INITIAL.
*SELECT BUKRS BLART ZUONR GJAHR BELNR BUZEI BUDAT BLDAT WAERS SHKZG HKONT DMBTR INTO TABLE IT_F2 FROM BSIS WHERE BUDAT LE DATE AND HKONT IN S_HKONT.
**APPEND LINES OF IT_F1 TO IT_F.
**Clear IT_F1.
*SELECT BUKRS BLART ZUONR GJAHR BELNR BUZEI BUDAT BLDAT WAERS SHKZG HKONT DMBTR FROM BSAS APPENDING CORRESPONDING FIELDS OF TABLE IT_F2
*  WHERE BUDAT LE DATE AND AUGDT GT DATE AND HKONT IN S_HKONT . "
**APPEND LINES OF IT_F1 TO IT_F.
**Clear IT_F1.
*ELSE.
  SELECT bukrs blart zuonr gjahr belnr buzei budat bldat waers shkzg hkont dmbtr sgtxt
    INTO TABLE it_f2 FROM bsis
    WHERE bukrs = p_bukrs AND budat IN s_budat  AND hkont IN s_hkont." AND belnr = p_belnr.

  SELECT bukrs blart zuonr gjahr belnr buzei budat bldat waers shkzg hkont dmbtr sgtxt
    FROM bsas APPENDING CORRESPONDING FIELDS OF TABLE it_f2
  WHERE bukrs = p_bukrs AND budat IN s_budat AND hkont IN s_hkont." AND belnr = p_belnr.
*IF r2 = abap_true.
*  SORT it_f2 by belnr.
*  DELETE ADJACENT DUPLICATES FROM it_f2 COMPARING belnr.
*ENDIF.
*ENDIF.
  MOVE-CORRESPONDING it_f2 TO it_f.

  LOOP AT it_f INTO wa_f.

    IF r1 = abap_true.
      IF wa_f-shkzg = 'H'.
        wa_f-dmbtr      =    ( wa_f-dmbtr * -1 ) .
      ENDIF.
      SELECT
      witht
      wt_withcd
      wt_qsshh
      wt_qbshh
      wt_acco
      qsatz
      hkont  FROM with_item INTO TABLE it_wh WHERE belnr EQ wa_f-belnr AND gjahr EQ wa_f-gjahr AND wt_withcd NE ''.
      READ TABLE it_wh INTO wa_wh INDEX 1.
      IF sy-subrc EQ 0.
        wa_f-witht       =  wa_wh-witht.
        wa_f-wt_withcd   =  wa_wh-wt_withcd.
        wa_f-wt_qsshh    =  wa_wh-wt_qsshh.
        wa_f-wt_qbshh    =  wa_wh-wt_qbshh .
        wa_f-wt_acco     =  wa_wh-wt_acco.
        wa_f-qsatz       =  wa_wh-qsatz.
        wa_f-hkont       =  wa_wh-hkont.
      ELSE.
        wa_f-wt_qsshh   =    wa_f-dmbtr .
      ENDIF.
    ELSE.
      DATA: lv_vbeln TYPE vbeln_vf.
      SELECT SINGLE xblnr AS vbeln,awkey INTO @DATA(lw_vbeln)
      FROM bkpf WHERE bukrs = @wa_f-bukrs AND
                      belnr EQ @wa_f-belnr AND
                      gjahr EQ @wa_f-gjahr.

      lv_vbeln = lw_vbeln-vbeln.
      SELECT SINGLE kunag AS kunnr,knumv
             FROM vbrk INTO @DATA(lw_vbrk)
        WHERE vbeln = @lv_vbeln.
      IF sy-subrc = 0.
        SELECT SINGLE name1 j_1ipanno FROM kna1 INTO ( wa_f-vname, wa_f-j_1ipanno ) WHERE kunnr = lw_vbrk-kunnr.
        SELECT SINGLE *
          FROM prcd_elements
          INTO @DATA(lw_prcd_elements)
          WHERE knumv = @lw_vbrk-knumv AND
                kwert = @wa_f-dmbtr AND
                kschl = 'JTC1'.
        IF sy-subrc = 0.
          wa_f-qsatz = lw_prcd_elements-kbetr.
          wa_f-dmbtr = lw_prcd_elements-kwert .
          wa_f-wt_qsshh = lw_prcd_elements-kawrt.
          wa_f-wt_acco = lw_vbrk-kunnr.
          CLEAR lv_vbeln.
        ELSE.
          lv_vbeln = lw_vbeln-awkey.
          SELECT SINGLE kunag AS kunnr,knumv
                 FROM vbrk INTO @lw_vbrk
            WHERE vbeln = @lv_vbeln.
          SELECT SINGLE *
            FROM prcd_elements
            INTO @lw_prcd_elements
            WHERE knumv = @lw_vbrk-knumv AND
                  kwert = @wa_f-dmbtr AND
                  kschl = 'JTC1'.
          IF sy-subrc = 0.
            wa_f-qsatz = lw_prcd_elements-kbetr.
            wa_f-dmbtr = lw_prcd_elements-kwert .
            wa_f-wt_qsshh = lw_prcd_elements-kawrt.
            wa_f-wt_acco = lw_vbrk-kunnr.
          ENDIF.
        ENDIF.
        CLEAR lv_vbeln.
        READ TABLE it_gl INTO wa_gl WITH KEY saknr = wa_f-hkont.
        wa_f-txt20  = wa_gl-txt20.
        wa_f-wt_d = |TCS@ { wa_f-qsatz }%|.
      ENDIF.
    ENDIF.


    SELECT hkont blart FROM bsis INTO TABLE it_b1 WHERE bukrs = p_bukrs AND belnr = wa_f-belnr AND gjahr = wa_f-gjahr  AND bschl NOT IN ('21', '22', '31', '32')
    AND buzid NE 'T' AND hkont NOT IN s_hkont.
    APPEND LINES OF it_b1 TO it_b.
    CLEAR: it_b1.

*  BREAK KPPLTEST.

    SELECT hkont blart FROM bsas INTO TABLE it_b1 WHERE bukrs = p_bukrs AND belnr EQ wa_f-belnr AND gjahr EQ wa_f-gjahr AND bschl NOT IN ('21', '22', '31', '32')
    AND buzid NE 'T' AND hkont NOT IN s_hkont.
    APPEND LINES OF it_b1 TO it_b.
    CLEAR: it_b1.
    READ TABLE it_b INTO wa_b INDEX 1.
    wa_f-gl_n1 = wa_b-hkont.
    READ TABLE it_gl INTO wa_gl WITH KEY saknr = wa_f-gl_n1.
    wa_f-gl_n  = wa_gl-txt20.
*WA_F-BLART = WA_B-BLART.

    IF r1 = abap_true.
      READ TABLE it_v INTO wa_v WITH KEY lifnr = wa_f-wt_acco.
      READ TABLE it_wt INTO wa_wt WITH KEY witht = wa_f-witht.

      CLEAR: wa_gl.
      READ TABLE it_gl INTO wa_gl WITH KEY saknr = wa_f-hkont.
      wa_f-txt20  = wa_gl-txt20.

      wa_f-vname = wa_v-name1.
*WA_F-GL_N  = WA_GL-TXT20.
      wa_f-wt_d = wa_wt-text40.

*    SELECT j_1ipanno FROM j_1imovend INTO TABLE it_pan WHERE lifnr = wa_f-wt_acco.
*    READ TABLE it_pan INTO wa_pan INDEX 1.
*
*    wa_f-j_1ipanno = wa_pan-j_1ipanno.
*    IF wa_f-j_1ipanno IS INITIAL.
      SELECT SINGLE j_1ipanno FROM lfa1
        INTO wa_f-j_1ipanno WHERE lifnr = wa_f-wt_acco.
    ENDIF.
*    ENDIF.

    SELECT
    cpudt
    xblnr
    bktxt
    ppnam
    usnam
    FROM bkpf INTO TABLE it_ad WHERE belnr EQ wa_f-belnr AND gjahr EQ wa_f-gjahr.
    READ TABLE it_ad INTO wa_ad INDEX 1.
    IF wa_ad-ppnam IS NOT INITIAL.
      SELECT a~bname b~name_text INTO TABLE it_un FROM  usr21 AS a INNER JOIN adrp AS b ON a~persnumber EQ b~persnumber WHERE a~bname EQ wa_ad-ppnam.
      READ TABLE it_un INTO wa_un INDEX 1.
    ELSE.
      SELECT a~bname b~name_text INTO TABLE it_un FROM  usr21 AS a INNER JOIN adrp AS b ON a~persnumber EQ b~persnumber WHERE a~bname EQ wa_ad-usnam.
      READ TABLE it_un INTO wa_un INDEX 1.

    ENDIF.

    wa_f-cpudt = wa_ad-cpudt.
    wa_f-xblnr = wa_ad-xblnr.
    wa_f-bktxt = wa_ad-bktxt.
*WA_F1-PPNAM = WA_AD-PPNAM.
    wa_f-usnam = wa_un-bname.
    wa_f-name_text = wa_un-name_text.


    APPEND wa_f TO it_f1.

    CLEAR: wa_f, wa_gl, wa_wt, wa_v, it_b, wa_b, it_wh, wa_wh,it_un,wa_un,it_ad,wa_ad.

  ENDLOOP.
  IF p_c IS NOT INITIAL .
    LOOP AT it_gla INTO wa_gla.
      CASE ld_currm.
        WHEN 01.
          wa_f1-dmbtr = wa_gla-hslvt .
        WHEN 02.
          wa_f1-dmbtr = wa_gla-hslvt + wa_gla-hsl01.
        WHEN 03.
          wa_f1-dmbtr = wa_gla-hslvt + wa_gla-hsl01 + wa_gla-hsl02 .
        WHEN 04.
          wa_f1-dmbtr = wa_gla-hslvt + wa_gla-hsl01 + wa_gla-hsl02 + wa_gla-hsl03 .
        WHEN 05.
          wa_f1-dmbtr = wa_gla-hslvt + wa_gla-hsl01 + wa_gla-hsl02 + wa_gla-hsl03 + wa_gla-hsl04 .
        WHEN 06.
          wa_f1-dmbtr = wa_gla-hslvt + wa_gla-hsl01 + wa_gla-hsl02 + wa_gla-hsl03 + wa_gla-hsl04 + wa_gla-hsl05 .
        WHEN 07.
          wa_f1-dmbtr = wa_gla-hslvt + wa_gla-hsl01 + wa_gla-hsl02 + wa_gla-hsl03 + wa_gla-hsl04 + wa_gla-hsl05 + wa_gla-hsl06 .
        WHEN 08.
          wa_f1-dmbtr = wa_gla-hslvt + wa_gla-hsl01 + wa_gla-hsl02 + wa_gla-hsl03 + wa_gla-hsl04 + wa_gla-hsl05 + wa_gla-hsl06 + wa_gla-hsl07 .
        WHEN 09.
          wa_f1-dmbtr = wa_gla-hslvt + wa_gla-hsl01 + wa_gla-hsl02 + wa_gla-hsl03 + wa_gla-hsl04 + wa_gla-hsl05 + wa_gla-hsl06 + wa_gla-hsl07 + wa_gla-hsl08 .
        WHEN 10.
          wa_f1-dmbtr = wa_gla-hslvt + wa_gla-hsl01 + wa_gla-hsl02 + wa_gla-hsl03 + wa_gla-hsl04 + wa_gla-hsl05 + wa_gla-hsl06 + wa_gla-hsl07 + wa_gla-hsl08 + wa_gla-hsl09 .
        WHEN 11.
          wa_f1-dmbtr = wa_gla-hslvt + wa_gla-hsl01 + wa_gla-hsl02 + wa_gla-hsl03 + wa_gla-hsl04 + wa_gla-hsl05 + wa_gla-hsl06 + wa_gla-hsl07 + wa_gla-hsl08 + wa_gla-hsl09 + wa_gla-hsl10 .
        WHEN 12.
          wa_f1-dmbtr = wa_gla-hslvt + wa_gla-hsl01 + wa_gla-hsl02 + wa_gla-hsl03 + wa_gla-hsl04 + wa_gla-hsl05 + wa_gla-hsl06 + wa_gla-hsl07 + wa_gla-hsl08 + wa_gla-hsl09 + wa_gla-hsl10 + wa_gla-hsl11 .
*    WHEN OTHERS.
*      WA_G-DMBTR =  WA_GLA-HSLVT + WA_GLA-HSL01 + WA_GLA-HSL02 + WA_GLA-HSL03 + WA_GLA-HSL04 + WA_GLA-HSL05 + WA_GLA-HSL06 + WA_GLA-HSL07 + WA_GLA-HSL08 + WA_GLA-HSL09 + WA_GLA-HSL10 + WA_GLA-HSL11 + WA_GLA-HSL12 + WA_GLA-HSL13 + WA_GLA-HSL14 +
*WA_GLA-HSL15 + WA_GLA-HSL16.
      ENDCASE.
*  APPEND WA_GLA TO IT_GLA1.
      READ TABLE it_gl INTO wa_gl WITH KEY saknr = wa_gla-racct.
      wa_f1-txt20  = wa_gl-txt20.
      wa_f1-hkont = wa_gla-racct.




      APPEND wa_f1 TO it_f1.
      CLEAR: wa_f1, wa_gl,wa_gla.

    ENDLOOP.
  ENDIF.
  SORT it_f1 BY blart budat.

*  DELETE it_f1 WHERE wt_acco NE wt_acco.
  CLEAR it_f[].
  LOOP AT it_f1 ASSIGNING FIELD-SYMBOL(<fs_wa_f1>).
    IF r1 = abap_true.
      IF <fs_wa_f1>-blart = 'RE' OR
         <fs_wa_f1>-blart = 'KA' OR
         <fs_wa_f1>-blart = 'KR' .
        DATA l_amount TYPE wt_bs.
        IF <fs_wa_f1>-wt_qsshh < 0 AND <fs_wa_f1>-flag IS INITIAL .
          l_amount = <fs_wa_f1>-wt_qsshh * -1.
          READ TABLE it_f1 ASSIGNING FIELD-SYMBOL(<fs_wa_f>)
                                     WITH KEY  hkont =  <fs_wa_f1>-hkont
                                               wt_acco = <fs_wa_f1>-wt_acco
                                               blart = <fs_wa_f1>-blart
                                               wt_qsshh = l_amount
                                               flag = ''.
          IF sy-subrc = 0.
            <fs_wa_f>-flag = 'X'. " Delete Flag
            <fs_wa_f1>-flag = 'X'. " Delete Flag
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      IF <fs_wa_f1>-shkzg = 'H'.
        <fs_wa_f1>-dmbtr      =    ( <fs_wa_f1>-dmbtr * -1 ) .
        <fs_wa_f1>-wt_qsshh      =    ( <fs_wa_f1>-wt_qsshh * -1 ) .
      ENDIF.
      READ TABLE it_f ASSIGNING FIELD-SYMBOL(<fs1>)
      WITH KEY belnr = <fs_wa_f1>-belnr.
      IF sy-subrc = 0.
        <fs1>-dmbtr = <fs1>-dmbtr + <fs_wa_f1>-dmbtr.
        <fs1>-wt_qsshh = <fs1>-wt_qsshh + <fs_wa_f1>-wt_qsshh.
      ELSE.
        APPEND <fs_wa_f1> TO it_f.
      ENDIF.

    ENDIF.
  ENDLOOP.
  IF r1 = abap_true.
    DELETE it_f1 WHERE flag = 'X'.
  ELSE.
    CLEAR it_f1[].
    it_f1 = it_f.
  ENDIF.


  w_fcat-fieldname = 'BUKRS'.
  w_fcat-ref_tabname = 'BSAK'.
  w_fcat-emphasize = 'C311'.
*  W_FCAT-OUTPUTLEN = '6'.
  w_fcat-col_pos = '1'.
  APPEND w_fcat TO i_fcat.
  CLEAR w_fcat.

  w_fcat-fieldname = 'GJAHR'.
  w_fcat-ref_tabname = 'BSAK'.
  w_fcat-emphasize = 'C311'.
*  W_FCAT-OUTPUTLEN = '6'.
  w_fcat-col_pos = '1'.
  APPEND w_fcat TO i_fcat.
  CLEAR w_fcat.

  w_fcat-fieldname = 'BELNR'.
  w_fcat-ref_tabname = 'BSAK'.
  w_fcat-hotspot = 'X'.
  w_fcat-emphasize = 'C311'.
*  W_FCAT-OUTPUTLEN = '6'.
  w_fcat-col_pos = '1'.
  APPEND w_fcat TO i_fcat.
  CLEAR w_fcat.

  w_fcat-fieldname = 'BLART'.
  w_fcat-ref_tabname = 'BSAK'.
  w_fcat-emphasize = 'C110'.
*  W_FCAT-OUTPUTLEN = '6'.
  w_fcat-col_pos = '1'.
  APPEND w_fcat TO i_fcat.
  CLEAR w_fcat.

  w_fcat-fieldname = 'BUDAT'.
  w_fcat-ref_tabname = 'BSAK'.
  w_fcat-emphasize = 'C110'.
*  W_FCAT-OUTPUTLEN = '6'.
  w_fcat-col_pos = '1'.
  APPEND w_fcat TO i_fcat.
  CLEAR w_fcat.

  w_fcat-fieldname = 'BLDAT'.
  w_fcat-ref_tabname = 'BSAK'.
  w_fcat-emphasize = 'C110'.
*  W_FCAT-OUTPUTLEN = '6'.
  w_fcat-col_pos = '1'.
  APPEND w_fcat TO i_fcat.
  CLEAR w_fcat.


  w_fcat-fieldname = 'CPUDT'.
  w_fcat-ref_tabname = 'BSAK'.
  w_fcat-emphasize = 'C110'.
*  W_FCAT-OUTPUTLEN = '6'.
  w_fcat-col_pos = '1'.
  APPEND w_fcat TO i_fcat.
  CLEAR w_fcat.

  w_fcat-fieldname = 'DMBTR'.
  w_fcat-ref_tabname = 'BSIS'.
*  W_FCAT-seltext_m = 'GL Based Amt'.
  w_fcat-emphasize = 'C110'.
*  W_FCAT-OUTPUTLEN = '6'.
  w_fcat-col_pos = '2'.
  APPEND w_fcat TO i_fcat.
  CLEAR w_fcat.

  w_fcat-fieldname = 'HKONT'.
  w_fcat-ref_tabname = 'BSAK'.
  w_fcat-emphasize = 'C110'.
*  W_FCAT-OUTPUTLEN = '6'.
  w_fcat-col_pos = '1'.
  APPEND w_fcat TO i_fcat.
  CLEAR w_fcat.

  w_fcat-fieldname = 'TXT20'.
*  W_FCAT-seltext_m = 'GL Description'.
  w_fcat-ref_tabname = 'SKAT'.
  w_fcat-emphasize = 'C110'.
*  W_FCAT-OUTPUTLEN = '6'.
  w_fcat-col_pos = '1'.
  APPEND w_fcat TO i_fcat.
  CLEAR w_fcat.


  w_fcat-fieldname = 'GL_N1'.
  w_fcat-seltext_m = 'Expense GL'.
*  W_FCAT-REF_TABNAME = 'BSAK'.
  w_fcat-emphasize = 'C110'.
*  W_FCAT-OUTPUTLEN = '6'.
  w_fcat-col_pos = '1'.
  APPEND w_fcat TO i_fcat.
  CLEAR w_fcat.

  w_fcat-fieldname = 'GL_N'.
  w_fcat-seltext_m = 'Exp.GL Description'.
*  W_FCAT-REF_TABNAME = 'BSAK'.
  w_fcat-emphasize = 'C110'.
*  W_FCAT-OUTPUTLEN = '6'.
  w_fcat-col_pos = '1'.
  APPEND w_fcat TO i_fcat.
  CLEAR w_fcat.

  w_fcat-fieldname = 'WT_ACCO'.
  w_fcat-ref_tabname = 'WITH_ITEM'.
  w_fcat-emphasize = 'C110'.
*  W_FCAT-OUTPUTLEN = '6'.
  w_fcat-col_pos = '1'.
  APPEND w_fcat TO i_fcat.
  CLEAR w_fcat.

  w_fcat-fieldname = 'VNAME'.
  w_fcat-seltext_m = 'Name'.
*  W_FCAT-REF_TABNAME = 'LFA1'.
  w_fcat-emphasize = 'C110'.
*  W_FCAT-OUTPUTLEN = '6'.
  w_fcat-col_pos = '1'.
  APPEND w_fcat TO i_fcat.
  CLEAR w_fcat.

  w_fcat-fieldname = 'WAERS'.
  w_fcat-ref_tabname = 'BSAK'.
  w_fcat-emphasize = 'C110'.
*  W_FCAT-OUTPUTLEN = '6'.
  w_fcat-col_pos = '1'.
  APPEND w_fcat TO i_fcat.
  CLEAR w_fcat.

  w_fcat-fieldname = 'WITHT'.
  w_fcat-ref_tabname = 'WITH_ITEM'.
  w_fcat-emphasize = 'C110'.
*  W_FCAT-OUTPUTLEN = '6'.
  w_fcat-col_pos = '1'.
  APPEND w_fcat TO i_fcat.
  CLEAR w_fcat.

  w_fcat-fieldname = 'WT_WITHCD'.
  w_fcat-ref_tabname = 'WITH_ITEM'.
  w_fcat-emphasize = 'C110'.
*  W_FCAT-OUTPUTLEN = '6'.
  w_fcat-col_pos = '1'.
  APPEND w_fcat TO i_fcat.
  CLEAR w_fcat.

  w_fcat-fieldname = 'WT_D'.
  w_fcat-seltext_m = 'Withholding tax Des.'.
*  W_FCAT-REF_TABNAME = 'WITH_ITEM'.
  w_fcat-emphasize = 'C110'.
*  W_FCAT-OUTPUTLEN = '6'.
  w_fcat-col_pos = '1'.
  APPEND w_fcat TO i_fcat.
  CLEAR w_fcat.

*   W_FCAT-FIELDNAME = 'WT_QBSHH'.
*  W_FCAT-seltext_m = 'Amount in local currency'.
*  W_FCAT-EMPHASIZE = 'C110'.
**  W_FCAT-OUTPUTLEN = '6'.
*  W_FCAT-COL_POS = '2'.
*  APPEND W_FCAT TO I_FCAT.
*  CLEAR W_FCAT.

*  W_FCAT-FIELDNAME = 'SKFBT'.
*  W_FCAT-REF_TABNAME = 'BSAK'.
*  W_FCAT-EMPHASIZE = 'C110'.
**  W_FCAT-OUTPUTLEN = '6'.
*  W_FCAT-COL_POS = '2'.
*  APPEND W_FCAT TO I_FCAT.
*  CLEAR W_FCAT.

  w_fcat-fieldname = 'WT_QSSHH '.
  w_fcat-ref_tabname = 'WITH_ITEM'.
  w_fcat-emphasize = 'C110'.
*  W_FCAT-OUTPUTLEN = '6'.
  w_fcat-col_pos = '2'.
  APPEND w_fcat TO i_fcat.
  CLEAR w_fcat.


  w_fcat-fieldname = 'QSATZ'.
  w_fcat-ref_tabname = 'WITH_ITEM'.
  w_fcat-emphasize = 'C110'.
*  W_FCAT-OUTPUTLEN = '6'.
  w_fcat-col_pos = '2'.
  APPEND w_fcat TO i_fcat.
  CLEAR w_fcat.

  w_fcat-fieldname = 'XBLNR'.
  w_fcat-ref_tabname = 'BKPF'.
  w_fcat-emphasize = 'C110'.
*  W_FCAT-OUTPUTLEN = '6'.
  w_fcat-col_pos = '2'.
  APPEND w_fcat TO i_fcat.
  CLEAR w_fcat.

  w_fcat-fieldname = 'SGTXT'.
  w_fcat-ref_tabname = 'BSIS'.
  w_fcat-emphasize = 'C110'.
*  W_FCAT-OUTPUTLEN = '6'.
  w_fcat-col_pos = '2'.
  APPEND w_fcat TO i_fcat.
  CLEAR w_fcat.

  w_fcat-fieldname = 'J_1IPANNO'.
  w_fcat-ref_tabname = 'J_1IMOVEND'.
  w_fcat-emphasize = 'C110'.
*  W_FCAT-OUTPUTLEN = '6'.
  w_fcat-col_pos = '2'.
  APPEND w_fcat TO i_fcat.
  CLEAR w_fcat.

  w_fcat-fieldname = 'USNAM'.
*  W_FCAT-REF_TABNAME = 'WITH_ITEM'.
  w_fcat-seltext_m = 'User ID'.
  w_fcat-emphasize = 'C110'.
*  W_FCAT-OUTPUTLEN = '6'.
  w_fcat-col_pos = '2'.
  APPEND w_fcat TO i_fcat.
  CLEAR w_fcat.

  w_fcat-fieldname = 'NAME_TEXT'.
*  W_FCAT-REF_TABNAME = 'WITH_ITEM'.
  w_fcat-seltext_m = 'User Name'.

  w_fcat-emphasize = 'C110'.
*  W_FCAT-OUTPUTLEN = '6'.
  w_fcat-col_pos = '2'.
  APPEND w_fcat TO i_fcat.
  CLEAR w_fcat.



  layout-colwidth_optimize = 'X'.
  IF r1 = abap_true.
    DATA i_grid_title TYPE lvc_title.
    i_grid_title = 'TDS'.
  ELSE.
    i_grid_title = 'TCS'.
  ENDIF.

  DELETE it_f1 WHERE dmbtr = 0.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*
      i_callback_program      = sy-repid
      i_callback_user_command = gt_callback_subroutine
*     I_CALLBACK_PF_STATUS_SET          = ' '
*     I_CALLBACK_USER_COMMAND = ' '
*     I_CALLBACK_TOP_OF_PAGE  = 'HEADER'
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME        =
*     I_BACKGROUND_ID         = ' '
      i_grid_title            = i_grid_title
*     I_GRID_SETTINGS         =
      is_layout               = layout
      it_fieldcat             = i_fcat
*     is_variant              = variant     "<----  Here
*     i_save                  = 'A'             "<----  Here
*     I_SAVE                  = 'X'
*     IS_VARIANT              = 'X'
    TABLES
      t_outtab                = it_f1.



FORM user_command  USING p_ucomm    LIKE sy-ucomm
                       p_selfield TYPE slis_selfield.

  "p_ucomm will hold user action like double click, clicking a button ,etc
  CASE p_ucomm.
    WHEN '&IC1'.        " SAP standard code for double-clicking
      CLEAR: wa_f1.
      IF p_selfield-fieldname = 'BELNR'.
        READ TABLE it_f1 INTO wa_f1 INDEX p_selfield-tabindex.
        IF wa_f1-belnr IS NOT INITIAL.
          SET PARAMETER ID 'BLN' FIELD wa_f1-belnr.
          SET PARAMETER ID 'BUK' FIELD wa_f1-bukrs.
          SET PARAMETER ID 'GJR' FIELD wa_f1-gjahr.
          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
        ENDIF.

      ENDIF.
  ENDCASE.
ENDFORM.
**************************************************
***call function 'SAPGUI_PROGRESS_INDICATOR'.
************************************************
*PERFORM enable_layout_settings.

*TYPES: BEGIN OF TY_C,
*BUKRS TYPE BSAD-BUKRS, " Company Code
*KUNNR TYPE BSAD-KUNNR, " Customer
*ZUONR TYPE BSAD-ZUONR, " Assignment
*GJAHR TYPE BSAD-GJAHR, " Fiscal Year
*BELNR TYPE BSAD-BELNR, " Document Number
*BUZEI TYPE BSAD-BUZEI, " Line item
*BUDAT TYPE BSAD-BUDAT, " Posting Date
*BLDAT TYPE BSAD-BLDAT, " Document Date
*CPUDT TYPE BSAD-CPUDT, " Entry Date
*WAERS TYPE BSAD-WAERS, " Currency
*HKONT TYPE BSAD-HKONT, " GL
*SKFBT TYPE BSAD-SKFBT, " Discount base
*  END OF TY_C.

" QSSKZ - Withholding Tax Code [ XX ]
" QSSHB - Withhold.tax base {GT 0]


"WT_WITHCD Withholding tax code [NE '']
















*
*Company Code
*Document Number
*Fiscal Year
*Line item
*Withholding tax type
*Withholding tax code
*W/tax base amnt in LC
*W/tax aux. amount in LC
*Customer/vendor account no.
***Withholding tax rate
**
**LOOP AT IT_GLA INTO WA_GLA.
**  CASE ld_currm.
**    WHEN 01.
**      WA_G-DMBTR = WA_GLA-HSLVT .
**    WHEN 02.
**      WA_G-DMBTR = WA_GLA-HSLVT + WA_GLA-HSL01 + WA_GLA-HSL02.
**    WHEN 03.
**      WA_G-DMBTR = WA_GLA-HSLVT + WA_GLA-HSL01 + WA_GLA-HSL02 + WA_GLA-HSL03.
**    WHEN 04.
**      WA_G-DMBTR = WA_GLA-HSLVT + WA_GLA-HSL01 + WA_GLA-HSL02 + WA_GLA-HSL03 + WA_GLA-HSL04.
**    WHEN 05.
**      WA_G-DMBTR = WA_GLA-HSLVT + WA_GLA-HSL01 + WA_GLA-HSL02 + WA_GLA-HSL03 + WA_GLA-HSL04 + WA_GLA-HSL05.
**    WHEN 06.
**      WA_G-DMBTR = WA_GLA-HSLVT + WA_GLA-HSL01 + WA_GLA-HSL02 + WA_GLA-HSL03 + WA_GLA-HSL04 + WA_GLA-HSL05 + WA_GLA-HSL06.
**    WHEN 07.
**      WA_G-DMBTR = WA_GLA-HSLVT + WA_GLA-HSL01 + WA_GLA-HSL02 + WA_GLA-HSL03 + WA_GLA-HSL04 + WA_GLA-HSL05 + WA_GLA-HSL06 + WA_GLA-HSL07.
**    WHEN 08.
**      WA_G-DMBTR = WA_GLA-HSLVT + WA_GLA-HSL01 + WA_GLA-HSL02 + WA_GLA-HSL03 + WA_GLA-HSL04 + WA_GLA-HSL05 + WA_GLA-HSL06 + WA_GLA-HSL07 + WA_GLA-HSL08.
**    WHEN 09.
**      WA_G-DMBTR = WA_GLA-HSLVT + WA_GLA-HSL01 + WA_GLA-HSL02 + WA_GLA-HSL03 + WA_GLA-HSL04 + WA_GLA-HSL05 + WA_GLA-HSL06 + WA_GLA-HSL07 + WA_GLA-HSL08 + WA_GLA-HSL09.
**    WHEN 10.
**      WA_G-DMBTR = WA_GLA-HSLVT + WA_GLA-HSL01 + WA_GLA-HSL02 + WA_GLA-HSL03 + WA_GLA-HSL04 + WA_GLA-HSL05 + WA_GLA-HSL06 + WA_GLA-HSL07 + WA_GLA-HSL08 + WA_GLA-HSL09 + WA_GLA-HSL10.
**    WHEN 11.
**      WA_G-DMBTR = WA_GLA-HSLVT + WA_GLA-HSL01 + WA_GLA-HSL02 + WA_GLA-HSL03 + WA_GLA-HSL04 + WA_GLA-HSL05 + WA_GLA-HSL06 + WA_GLA-HSL07 + WA_GLA-HSL08 + WA_GLA-HSL09 + WA_GLA-HSL10 + WA_GLA-HSL11.
**    WHEN 12.
**      WA_G-DMBTR = WA_GLA-HSLVT + WA_GLA-HSL01 + WA_GLA-HSL02 + WA_GLA-HSL03 + WA_GLA-HSL04 + WA_GLA-HSL05 + WA_GLA-HSL06 + WA_GLA-HSL07 + WA_GLA-HSL08 + WA_GLA-HSL09 + WA_GLA-HSL10 + WA_GLA-HSL11 + WA_GLA-HSL12.
**    WHEN OTHERS.
**      WA_G-DMBTR =  WA_GLA-HSLVT + WA_GLA-HSL01 + WA_GLA-HSL02 + WA_GLA-HSL03 + WA_GLA-HSL04 + WA_GLA-HSL05 + WA_GLA-HSL06 + WA_GLA-HSL07 + WA_GLA-HSL08 + WA_GLA-HSL09 + WA_GLA-HSL10 + WA_GLA-HSL11 + WA_GLA-HSL12 + WA_GLA-HSL13 + WA_GLA-HSL14 +
**WA_GLA-HSL15 + WA_GLA-HSL16.
**  ENDCASE.
***  APPEND WA_GLA TO IT_GLA1.
**  READ TABLE IT_GL INTO WA_GL WITH KEY SAKNR = WA_GLA-RACCT.
**  WA_G-GL_N  = WA_GL-TXT20.
**  WA_G-HKONT = WA_GLA-RACCT.
**  APPEND WA_G TO IT_G.
**
**ENDLOOP.
















*
*Company Code
*Document Number
*Fiscal Year
*Line item
*Withholding tax type
*Withholding tax code
*W/tax base amnt in LC
*W/tax aux. amount in LC
*Customer/vendor account no.
*Withholding tax rate
