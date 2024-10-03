*&---------------------------------------------------------------------*
*& Report  ZALL_MASTER_QLT_TEST
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT zfi_master_rep.

*&---------------------------------------------------------------------*
*& Report  ZALL_MASTER
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*




TABLES: lfa1,j_1imovend, lfb1, kna1, j_1imocust,knb1,mara,makt,mard,mbew,marc,sscrfields,ska1,skb1.

TABLES: anla, anlh, anlz, anlb, anlc.

*DATA REPID TYPE SY-REPID.
*TYPE-POOLS: CXTAB, SLIS.
*DATA: CONTROL_COLS TYPE CXTAB_COLUMN.

*data: variant type disvariant.     "<----  Here
*  variant-report = sy-repid.           "<----  Here
*  variant-username  = sy-uname.

DATA : flag1 TYPE char2, flag2 TYPE i, flag3 TYPE i.
DATA : v_lines TYPE i,
       c_lines TYPE string.

  SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

*SELECT-OPTIONS P_DATE  FOR SKA1-ERDAT.
    PARAMETERS: rb_1 RADIOBUTTON GROUP rbs DEFAULT 'X' USER-COMMAND pc.
************* Modified start by bharathi as on 29-12-2021 ******************
*SELECTION-SCREEN:BEGIN OF BLOCK SB1 WITH FRAME TITLE text-010.
*  PARAMETERS: SRB_1 RADIOBUTTON GROUP SRBS  user-command pc MODIF ID 1 ,
*              SRB_2 RADIOBUTTON GROUP SRBS DEFAULT 'X' MODIF ID 1.
*SELECTION-SCREEN:END OF BLOCK SB1.

    PARAMETERS: srb_1 NO-DISPLAY ,
    srb_2 DEFAULT 'X' NO-DISPLAY.

************* Modified end by  bharathi as on 29-12-2021 ******************
    PARAMETERS: rb_2 RADIOBUTTON GROUP rbs,
                rb_3 RADIOBUTTON GROUP rbs,
                rb_4 RADIOBUTTON GROUP rbs,
                rb_5 RADIOBUTTON GROUP rbs.
***PARAMETERS: rb_1 RADIOBUTTON GROUP rbs.
***
    SELECTION-SCREEN: BEGIN OF BLOCK b2  WITH FRAME .
      SELECT-OPTIONS: lifnr FOR  lfa1-lifnr MODIF ID 1,
                      pd_v  FOR  ska1-erdat MODIF ID 1.
      SELECTION-SCREEN SKIP 1.
*PARAMETERS: p_date1 type mara-ersda,p_date1a type mara-ersda.
    SELECTION-SCREEN: END OF BLOCK b2.
***
***
***PARAMETERS:       rb_2 RADIOBUTTON GROUP rbs.
    SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME.
      SELECT-OPTIONS: kunnr FOR kna1-kunnr MODIF ID 2,
                     pd_c  FOR  ska1-erdat MODIF ID 2.
      SELECTION-SCREEN SKIP 1.
*PARAMETERS: p_date2 type mara-ersda,p_date2a type mara-ersda.
    SELECTION-SCREEN: END OF BLOCK b3.
***
***
***PARAMETERS:    rb_3 RADIOBUTTON GROUP rbs.
    SELECTION-SCREEN: BEGIN OF BLOCK b4 WITH FRAME.
      SELECT-OPTIONS:  matnr FOR mara-matnr MODIF ID 3,
                      pd_m  FOR  ska1-erdat MODIF ID 3.
      SELECTION-SCREEN SKIP 1.
*PARAMETERS p_date3 type mara-ersda MODIF ID FFF .
*PARAMETERS p_date3a type mara-ersda MODIF ID FF1.
    SELECTION-SCREEN: END OF BLOCK b4.

    SELECTION-SCREEN: BEGIN OF BLOCK b5 WITH FRAME.
      SELECT-OPTIONS:  saknr FOR ska1-saknr MODIF ID 4,
                       pd_gl  FOR  ska1-erdat MODIF ID 4,
*                   PD_GLC  FOR  SKA1-ERDAT MODIF ID 4,
                       ktopl FOR ska1-ktopl DEFAULT 'KPPL' MODIF ID 4,
                       bukrs FOR skb1-bukrs DEFAULT '1080' MODIF ID 4.
      SELECTION-SCREEN SKIP 1.
    SELECTION-SCREEN: END OF BLOCK b5.
*****************************************
    SELECTION-SCREEN : BEGIN OF BLOCK b7 WITH FRAME TITLE TEXT-002.
      SELECT-OPTIONS: p_anln1 FOR anla-anln1 MODIF ID 5,
                      p_erdat FOR anla-erdat MODIF ID 6.
      PARAMETERS: rb1_1 RADIOBUTTON GROUP rbs1 DEFAULT 'X' USER-COMMAND pc MODIF ID 5,
                  rb1_2 RADIOBUTTON GROUP rbs1 MODIF ID 5,
                  rb1_3 RADIOBUTTON GROUP rbs1 MODIF ID 5.
      SELECTION-SCREEN: BEGIN OF BLOCK b6 WITH FRAME.
        SELECT-OPTIONS: p_zujhr FOR anla-zujhr MODIF ID 7.
      SELECTION-SCREEN: END OF BLOCK b6.
    SELECTION-SCREEN: END OF BLOCK b7.

*****************************************
*PARAMETERS : P_GSBER LIKE BSEG-GSBER.
  SELECTION-SCREEN: END OF BLOCK b1.

*p_date3 =  sy-datum.
AT SELECTION-SCREEN OUTPUT.
  IF rb_1 = 'X'.
    LOOP AT SCREEN.
      IF screen-group1 = '2' OR screen-group1 = '3' OR screen-group1 = '4' OR screen-group1 = '5' OR screen-group1 = '6' OR screen-group1 = '7'.
*             screen-active = 0.
        screen-input = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSEIF rb_2 = 'X'.
    LOOP AT SCREEN.
      IF screen-group1 = '1' OR screen-group1 = '3' OR screen-group1 = '4' OR screen-group1 = '5' OR screen-group1 = '6' OR screen-group1 = '7'.
*             screen-active = 0.
        screen-input = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSEIF rb_3 = 'X'.
    LOOP AT SCREEN.
      IF screen-group1 = '1' OR screen-group1 = '2' OR screen-group1 = '4' OR screen-group1 = '5' OR screen-group1 = '6' OR screen-group1 = '7'.
*             screen-active = 0.
        screen-input = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSEIF rb_4 = 'X'.
    LOOP AT SCREEN.
      IF screen-group1 = '1' OR screen-group1 = '2' OR screen-group1 = '3' OR screen-group1 = '5' OR screen-group1 = '6' OR screen-group1 = '7'.
*             screen-active = 0.
        screen-input = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSEIF rb_5 = 'X'.
    LOOP AT SCREEN.
      IF screen-group1 = '1' OR screen-group1 = '2' OR screen-group1 = '3' OR screen-group1 = '4' .
*             screen-active = 0.
        screen-input = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
  IF rb1_1 = 'X'.
    LOOP AT SCREEN.
      IF screen-group1 = '7'.
        screen-input = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSEIF rb1_2 = 'X' .
    LOOP AT SCREEN.
      IF screen-group1 = '7'.
        screen-input = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

  ELSEIF rb1_3 = 'X'.
    LOOP AT SCREEN.
      IF screen-group1 = '6'.
        screen-input = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

START-OF-SELECTION.

  CLEAR: c_lines,v_lines.

  DATA : t_header  TYPE slis_t_listheader,
         wa_header TYPE slis_listheader.

  TYPES : BEGIN OF ty_final_material,
            matnr TYPE mara-matnr, " Material No
            maktx TYPE makt-maktx, " Description
            werks TYPE mard-werks, " Plant
            mtart TYPE mara-mtart, " Movement Type
            matkl TYPE mara-matkl, " Movement Group
            meins TYPE mara-meins, " Unit
            bklas TYPE mbew-bklas, " Value Cls
            verpr TYPE mbew-verpr, " Price
            ernam TYPE mara-ernam, " Created By
            ersda TYPE mara-ersda, " Created On
          END OF ty_final_material.

  TYPES : BEGIN OF ty_final_vendor ,
            lifnr     TYPE lfa1-lifnr,
            name1     TYPE lfa1-name1,
*****
            witht     TYPE lfbw-witht,       "withholding tax type
            text40    TYPE t059u-text40,    " text
*****
            stras     TYPE lfa1-stras,
            ort01     TYPE lfa1-ort01,
            regio     TYPE lfa1-regio,
            pstlz     TYPE lfa1-pstlz,
            land1     TYPE lfa1-land1,
            adrnr     TYPE lfa1-adrnr,
            ktokk     TYPE lfa1-ktokk,
            erdat     TYPE lfa1-erdat,
            ernam     TYPE lfa1-ernam,
            stenr     TYPE lfa1-stenr,
            loevm     TYPE lfa1-loevm,
            sperr     TYPE lfa1-sperr,
            sperm     TYPE lfa1-sperm,
            stcd5     TYPE lfa1-stcd5,  " MSME No.
            brsch     TYPE lfa1-brsch,  " Vendor category
*     J_1IEXCD type J_1IMOVEND-J_1IEXCD,
*     J_1IEXRN type J_1IMOVEND-J_1IEXRN,
*     J_1IEXRG type J_1IMOVEND-J_1IEXRG,
*     J_1IEXDI type J_1IMOVEND-J_1IEXDI,
*     J_1IEXCO type J_1IMOVEND-J_1IEXCO,
*     J_1ILSTNO  type J_1IMOVEND-J_1ILSTNO,
*     J_1ICSTNO  type J_1IMOVEND-J_1ICSTNO,

*     J_1ISERN  type J_1IMOVEND-J_1ISERN,
            telf1     TYPE lfa1-telf1,
            telf2     TYPE lfa1-telf2,
*      ADRNR TYPE LFA1-ADRNR,
            smtp_addr TYPE adr6-smtp_addr,
            bukrs     TYPE lfb1-bukrs,
            akont     TYPE lfb1-akont,
            qsskz     TYPE lfb1-qsskz,
            cerdt     TYPE lfb1-cerdt, "Certification date
            j_1ipanno TYPE j_1imovend-j_1ipanno,
            brtxt     TYPE t016t-brtxt,
            atta(1)   TYPE c,           " Attachment Indicator

          END OF ty_final_vendor.



  TYPES : BEGIN OF ty_final_customer,
            kunnr TYPE kna1-kunnr,
            name1 TYPE kna1-name1,
            stras TYPE kna1-stras,
            ort01 TYPE kna1-ort01,
            regio TYPE kna1-regio,
            pstlz TYPE kna1-pstlz,
            land1 TYPE kna1-land1,
            adrnr TYPE kna1-adrnr,
            ktokd TYPE kna1-ktokd,
            erdat TYPE kna1-erdat,
            ernam TYPE kna1-ernam,
            loevm TYPE kna1-loevm,
            faksd TYPE kna1-faksd,
            lifsd TYPE kna1-lifsd,
*    J_1IEXCD type J_1IMOCUST-J_1IEXCD ,
*    J_1IEXRN type J_1IMOCUST-J_1IEXRN ,
*    J_1IEXRG type J_1IMOCUST-J_1IEXRG ,
*    J_1IEXDI type J_1IMOCUST-J_1IEXDI ,
*    J_1IEXCO type J_1IMOCUST-J_1IEXCO ,
*    J_1ILSTNO type J_1IMOCUST-J_1ILSTNO,
*    J_1ICSTNO type J_1IMOCUST-J_1ICSTNO,
*    J_1IPANNO type J_1IMOCUST-J_1IPANNO,
*    J_1ISERN type J_1IMOCUST-J_1ISERN,
            bukrs TYPE knb1-bukrs,
            akont TYPE knb1-akont,
          END OF ty_final_customer.

  TYPES: BEGIN OF ty_glfinal,
           ktopl   TYPE  ska1-ktopl,       " Chart of Accounts
           saknr   TYPE  ska1-saknr,       " G/L Account
           ktoks   TYPE  ska1-ktoks,       " Account Group
           xspea   TYPE  ska1-xspea,       " Blocked for creation
           xspeb   TYPE  ska1-xspeb,       " Blocked for posting
           xspep   TYPE  ska1-xspep,       " Blocked for planning
           xbilk   TYPE  ska1-xbilk,       " Balance Sheet Account
           erdat   TYPE  ska1-erdat,       " Created on
           ernam   TYPE  ska1-ernam,       " Created by
           txt20   TYPE  skat-txt20,       " Short Text
           txt50   TYPE  skat-txt50,       " G/L Acct Long Text
           ktplt   TYPE  t004t-ktplt,
           txt30   TYPE  t077z-txt30,
           fstag   TYPE  skb1-fstag,       " Field status group
           mitkz   TYPE  skb1-mitkz,       " Recon. account for acct type
           bukrs   TYPE  skb1-bukrs,       " Company Code
           g_erdat TYPE  skb1-erdat,       " GL Created on
           g_ernam TYPE  skb1-ernam,       " GL Created by
*    VALUE_NEW TYPE CDPOS-VALUE_NEW, " New value
*    VALUE_OLD TYPE CDPOS-VALUE_OLD, " Old value
*    USERNAME  TYPE CDHDR-USERNAME,  " User
*    UDATE  TYPE CDHDR-UDATE,        " Date



         END OF ty_glfinal.

  DATA: it_glfinal  TYPE TABLE OF ty_glfinal,
        wa_glfinal  TYPE ty_glfinal,
        it_glfinal1 TYPE TABLE OF ty_glfinal,
        wa_glfinal1 TYPE ty_glfinal.



  TYPES: BEGIN OF ty_skb1,
           saknr TYPE  skat-saknr,
           fstag TYPE  skb1-fstag,       " Field status group
           mitkz TYPE  skb1-mitkz,       " Recon. account for acct type
           bukrs TYPE  skb1-bukrs,       " Company Code
           erdat TYPE  skb1-erdat,       " Created on
           ernam TYPE  skb1-ernam,       " Created by
         END OF ty_skb1.
  DATA: it_skb1 TYPE TABLE OF ty_skb1,
        wa_skb1 TYPE ty_skb1.

  TYPES: BEGIN OF ty_skat,
           saknr TYPE  skat-saknr,
           txt20 TYPE  skat-txt20,       " Short Text
           txt50 TYPE  skat-txt50,       " G/L Acct Long Text
         END OF ty_skat.
  DATA: it_skat TYPE TABLE OF ty_skat,
        wa_skat TYPE ty_skat.

  TYPES: BEGIN OF ty_t004t,
           ktopl TYPE  t004t-ktopl,
           ktplt TYPE  t004t-ktplt,
         END OF ty_t004t.

  TYPES: BEGIN OF ty_t077z,
           ktopl TYPE  t004t-ktopl,   " add on 04.02.2020
           ktoks TYPE  t077z-ktoks,
           txt30 TYPE  t077z-txt30,
         END OF ty_t077z.

  DATA: it_t004t TYPE TABLE OF ty_t004t,
        wa_t004t TYPE  ty_t004t,
        it_t077z TYPE TABLE OF ty_t077z,
        wa_t077z TYPE  ty_t077z.
  DATA: idx TYPE sy-tabix.

  TYPES: BEGIN OF ty_cdhdr,
           changenr TYPE cdhdr-changenr,      " Document number
           username TYPE cdhdr-username,      " User
           udate    TYPE cdhdr-udate,         " Date
         END OF ty_cdhdr.

  TYPES: BEGIN OF ty_cdpos,
           tabkey    TYPE cdpos-tabkey,        " Table Key GL
           value_new TYPE cdpos-value_new,     " New value
           value_old TYPE cdpos-value_old,     " Old value
           fname     TYPE cdpos-fname,         " Field Name
         END OF ty_cdpos.

  TYPES: BEGIN OF ty_cdpos1,
           tabkey    TYPE cdpos-tabkey,        " Table Key GL
           value_new TYPE cdpos-value_new,     " New value
           value_old TYPE cdpos-value_old,     " Old value
           fname     TYPE cdpos-fname,         " Field Name
           username  TYPE cdhdr-username,      " User
           udate     TYPE cdhdr-udate,         " Date
         END OF ty_cdpos1.



  DATA: it_cdpos  TYPE TABLE OF ty_cdpos,
        wa_cdpos  TYPE ty_cdpos,
        it_cdpos1 TYPE TABLE OF ty_cdpos1,
        wa_cdpos1 TYPE ty_cdpos1,
        it_cdhdr  TYPE TABLE OF ty_cdhdr,
        wa_cdhdr  TYPE ty_cdhdr.

  DATA: len   TYPE i,
        c(10) TYPE c.



  TYPES: BEGIN OF ty_anla,
           anln1 TYPE  anla-anln1,
           anln2 TYPE  anla-anln2,
           anlkl TYPE  anla-anlkl,
           txt50 TYPE  anla-txt50,
           sernr TYPE  anla-sernr,
           invnr TYPE  anla-invnr,
           menge TYPE  anla-menge,
           meins TYPE  anla-meins,
           aktiv TYPE  anla-aktiv,
           zugdt TYPE  anla-zugdt,
           zujhr TYPE  anla-zujhr,
           zuper TYPE  anla-zuper,
           lifnr TYPE  anla-lifnr, " Vendor
           bukrs TYPE  anla-bukrs, " Company Code

         END OF ty_anla.

  TYPES: BEGIN OF ty_anlc,
*    ANLN1 TYPE ANLC-ANLN1,
           ndabj TYPE anlc-ndabj,          " Expired useful life
         END OF ty_anlc.

  DATA: it_anlc TYPE TABLE OF ty_anlc,
        wa_anlc TYPE ty_anlc.

  TYPES: BEGIN OF ty_anlb,
           afabg TYPE  anlb-afabg,
           afasl TYPE  anlb-afasl,
           ndjar TYPE  anlb-ndjar,

         END OF ty_anlb.
  TYPES: BEGIN OF ty_cskt,
           ltext TYPE cskt-ltext,
         END OF ty_cskt.

  TYPES : BEGIN OF ty_fasset,
            bukrs   TYPE  anla-bukrs,
            anln1   TYPE  anla-anln1,
            anln2   TYPE  anla-anln2,
            anlkl   TYPE  anla-anlkl,
            txt50   TYPE  anla-txt50,
            sernr   TYPE  anla-sernr,
            invnr   TYPE  anla-invnr,
            menge   TYPE  anla-menge,
            meins   TYPE  anla-meins,
            aktiv   TYPE  anla-aktiv,
            zugdt   TYPE  anla-zugdt,
            zujhr   TYPE  anla-zujhr,
            zuper   TYPE  anla-zuper,
            lifnr   TYPE  anla-lifnr,
            erdat   TYPE  anla-erdat,
            ernam   TYPE  anla-ernam,
            aenam   TYPE  anla-aenam,
            aedat   TYPE  anla-aedat,
            kostl   TYPE  anlz-kostl,
            werks   TYPE  anlz-werks,
            stort   TYPE  anlz-stort,   " Asset Location
            anlhtxt TYPE  anlh-anlhtxt,
            ndabj   TYPE  anlc-ndabj,
            afabg   TYPE  anlb-afabg,
            afasl   TYPE  anlb-afasl,
            ndjar   TYPE  anlb-ndjar,
            ltext   TYPE  cskt-ltext,
            buf     TYPE  i,            " Balance useful life.
            atta(1) TYPE c,           " Attachment Indicator
          END OF ty_fasset.

  DATA: it_fasset   TYPE TABLE OF ty_fasset,
        it_fasset_1 TYPE TABLE OF ty_fasset,
        wa_fasset   TYPE ty_fasset,
        wa_fasset_1 TYPE ty_fasset,
        it_anla     TYPE TABLE OF ty_anla,
        wa_anla     TYPE ty_anla,
        it_anlb     TYPE TABLE OF ty_anlb,
        wa_anlb     TYPE ty_anlb,
        it_cskt     TYPE TABLE OF ty_cskt,
        wa_cskt     TYPE ty_cskt,
        con(6)      TYPE c.
*        BUF TYPE  I.

  TYPES: BEGIN OF ty_atta,
           instid_a TYPE srgbtbrel-instid_a,
           catid_b  TYPE srgbtbrel-catid_a,
         END OF ty_atta.
  DATA: it_atta                TYPE TABLE OF ty_atta,
        wa_atta                TYPE ty_atta,
        g_top_of_page          TYPE slis_formname VALUE 'F_TOP_OF_PAGE', "for avl header.
        gt_callback_subroutine TYPE slis_formname VALUE 'USER_COMMAND'.


******************* ADDED BY BHARATHI 30.12.2021
  DATA:BEGIN OF it_tax OCCURS 0,
         lifnr  LIKE lfbw-lifnr,
         witht  LIKE lfbw-witht,
         text40 LIKE t059u-text40,
       END OF it_tax.
**************
  IF rb_3 = 'X'.

*  clear: LIFNR,p_date1,p_date1a.
*  clear: KUNNR,p_date2,p_date2a.

    DATA: it_final_material_4 TYPE TABLE OF ty_final_material,
          wa_final_material_4 TYPE ty_final_material.


    IF matnr IS INITIAL AND pd_m IS NOT INITIAL.

      SELECT mara~matnr makt~maktx marc~werks mara~mtart mara~matkl mara~meins mbew~bklas mbew~verpr mara~ernam mara~ersda
            INTO TABLE it_final_material_4
      FROM mara JOIN makt ON mara~matnr = makt~matnr
            JOIN marc ON marc~matnr = mara~matnr  JOIN mbew ON mbew~matnr = marc~matnr AND marc~werks = mbew~bwkey
      WHERE  mara~ersda IN pd_m.

    ELSEIF matnr IS NOT INITIAL AND pd_m IS NOT INITIAL.

      SELECT mara~matnr makt~maktx marc~werks mara~mtart mara~matkl mara~meins mbew~bklas mbew~verpr mara~ernam mara~ersda
            INTO TABLE it_final_material_4
      FROM mara JOIN makt ON mara~matnr = makt~matnr
            JOIN marc ON marc~matnr = mara~matnr  JOIN mbew ON mbew~matnr = marc~matnr AND marc~werks = mbew~bwkey
      WHERE  mara~matnr IN matnr AND mara~ersda IN pd_m.

    ELSEIF matnr IS NOT INITIAL AND pd_m IS INITIAL.

      SELECT mara~matnr makt~maktx marc~werks mara~mtart mara~matkl mara~meins mbew~bklas mbew~verpr mara~ernam mara~ersda
            INTO TABLE it_final_material_4
      FROM mara JOIN makt ON mara~matnr = makt~matnr
            JOIN marc ON marc~matnr = mara~matnr  JOIN mbew ON mbew~matnr = marc~matnr AND marc~werks = mbew~bwkey
      WHERE  mara~matnr IN matnr.

    ELSEIF matnr IS INITIAL AND pd_m IS INITIAL.

      SELECT mara~matnr makt~maktx marc~werks mara~mtart mara~matkl mara~meins mbew~bklas mbew~verpr mara~ernam mara~ersda
            INTO TABLE it_final_material_4
      FROM mara JOIN makt ON mara~matnr = makt~matnr
      JOIN marc ON marc~matnr = mara~matnr  JOIN mbew ON mbew~matnr = marc~matnr AND marc~werks = mbew~bwkey.
*          where  mara~matnr in MATNR AND mara~ersda IN PD_M.
    ENDIF.



    DATA: w_fcat_m4 TYPE slis_fieldcat_alv,
          i_fcat_m4 TYPE slis_t_fieldcat_alv,
          layout_m4 TYPE slis_layout_alv.

    w_fcat_m4-fieldname = 'MATNR'.
    w_fcat_m4-ref_tabname = 'MARA'.
    w_fcat_m4-emphasize = 'C311'.
    w_fcat_m4-outputlen = '12'.
    w_fcat_m4-col_pos = '1'.
    APPEND w_fcat_m4 TO i_fcat_m4.
    CLEAR w_fcat_m4.

    w_fcat_m4-fieldname = 'MAKTX'.
    w_fcat_m4-ref_tabname = 'MAKT'.
    w_fcat_m4-emphasize = 'C311'.
    w_fcat_m4-outputlen = '12'.
    w_fcat_m4-col_pos = '2'.
    APPEND w_fcat_m4 TO i_fcat_m4.
    CLEAR w_fcat_m4.


    w_fcat_m4-fieldname = 'WERKS'.
    w_fcat_m4-ref_tabname = 'MARC'.
    w_fcat_m4-emphasize = 'C110'.
    w_fcat_m4-outputlen = '12'.
    w_fcat_m4-col_pos = '3'.
    APPEND w_fcat_m4 TO i_fcat_m4.
    CLEAR w_fcat_m4.

    w_fcat_m4-fieldname = 'MTART'.
    w_fcat_m4-ref_tabname = 'MARA'.
    w_fcat_m4-emphasize = 'C110'.
    w_fcat_m4-outputlen = '12'.
    w_fcat_m4-col_pos = '4'.
    APPEND w_fcat_m4 TO i_fcat_m4.
    CLEAR w_fcat_m4.

    w_fcat_m4-fieldname = 'MATKL'.
    w_fcat_m4-ref_tabname = 'MARA'.
    w_fcat_m4-emphasize = 'C110'.
    w_fcat_m4-outputlen = '12'.
    w_fcat_m4-col_pos = '5'.
    APPEND w_fcat_m4 TO i_fcat_m4.
    CLEAR w_fcat_m4.

    w_fcat_m4-fieldname = 'MEINS'.
    w_fcat_m4-ref_tabname = 'MARA'.
    w_fcat_m4-emphasize = 'C110'.
    w_fcat_m4-outputlen = '12'.
    w_fcat_m4-col_pos = '6'.
    APPEND w_fcat_m4 TO i_fcat_m4.
    CLEAR w_fcat_m4.


    w_fcat_m4-fieldname = 'BKLAS'.
    w_fcat_m4-ref_tabname = 'MBEW'.
    w_fcat_m4-emphasize = 'C110'.
    w_fcat_m4-outputlen = '12'.
    w_fcat_m4-col_pos = '7'.
    APPEND w_fcat_m4 TO i_fcat_m4.
    CLEAR w_fcat_m4.


    w_fcat_m4-fieldname = 'VERPR'.
    w_fcat_m4-ref_tabname = 'MBEW'.
    w_fcat_m4-emphasize = 'C110'.
    w_fcat_m4-outputlen = '12'.
    w_fcat_m4-col_pos = '8'.
    APPEND w_fcat_m4 TO i_fcat_m4.
    CLEAR w_fcat_m4.

    w_fcat_m4-fieldname = 'ERNAM'.
    w_fcat_m4-ref_tabname = 'MARA'.
    w_fcat_m4-emphasize = 'C110'.
    w_fcat_m4-outputlen = '12'.
    w_fcat_m4-col_pos = '9'.
    APPEND w_fcat_m4 TO i_fcat_m4.
    CLEAR w_fcat_m4.


    w_fcat_m4-fieldname = 'ERSDA'.
    w_fcat_m4-ref_tabname = 'MARA'.
    w_fcat_m4-emphasize = 'C110'.
    w_fcat_m4-outputlen = '12'.
    w_fcat_m4-col_pos = '10'.
    APPEND w_fcat_m4 TO i_fcat_m4.
    CLEAR w_fcat_m4.

    DESCRIBE TABLE it_final_material_4 LINES v_lines.
    c_lines = v_lines.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program     = sy-repid
        i_callback_top_of_page = 'TOP_OF_PAGE'
        i_grid_title           = 'MATERIAL MASTER LIST'
        is_layout              = layout_m4
        it_fieldcat            = i_fcat_m4
      TABLES
        t_outtab               = it_final_material_4.

  ELSEIF rb_2 = 'X'.

* clear: LIFNR,p_date1,p_date1a.
*  clear: MATNR,p_date3,p_date3a.

    DATA: it_final_customer TYPE TABLE OF ty_final_customer,
          wa_final_customer TYPE ty_final_customer.

    IF kunnr IS INITIAL AND pd_c IS NOT INITIAL.

      SELECT kna1~kunnr
             kna1~name1
             kna1~stras
             kna1~ort01
             kna1~regio
             kna1~pstlz
             kna1~land1
             kna1~adrnr
             kna1~ktokd
             kna1~erdat
             kna1~ernam
             kna1~loevm
             kna1~faksd
             kna1~lifsd
             knb1~bukrs
             knb1~akont
             INTO TABLE it_final_customer FROM kna1 "JOIN J_1IMOCUST ON KNA1~KUNNR = J_1IMOCUST~KUNNR
             JOIN knb1 ON kna1~kunnr = knb1~kunnr
      WHERE  kna1~erdat IN pd_c.


    ELSEIF  kunnr IS INITIAL AND pd_c IS INITIAL.

      SELECT kna1~kunnr kna1~name1 kna1~stras
                kna1~ort01 kna1~regio kna1~pstlz
                kna1~land1 kna1~adrnr kna1~ktokd
                kna1~erdat kna1~ernam
                kna1~loevm kna1~faksd kna1~lifsd
                knb1~bukrs knb1~akont
                INTO TABLE it_final_customer FROM kna1 "JOIN J_1IMOCUST ON KNA1~KUNNR = J_1IMOCUST~KUNNR
      JOIN knb1 ON kna1~kunnr = knb1~kunnr.
*                    WHERE KNA1~KUNNR IN KUNNR and KNA1~ERDAT IN PD_C.

    ELSEIF  kunnr IS NOT INITIAL AND pd_c IS INITIAL.

      SELECT kna1~kunnr kna1~name1 kna1~stras
                kna1~ort01 kna1~regio kna1~pstlz
                kna1~land1 kna1~adrnr kna1~ktokd
                kna1~erdat kna1~ernam
                kna1~loevm kna1~faksd kna1~lifsd
                knb1~bukrs knb1~akont
                INTO TABLE it_final_customer FROM kna1 "JOIN J_1IMOCUST ON KNA1~KUNNR = J_1IMOCUST~KUNNR
                JOIN knb1 ON kna1~kunnr = knb1~kunnr
      WHERE kna1~kunnr IN kunnr.

    ELSEIF  kunnr IS NOT INITIAL AND pd_c IS NOT INITIAL.

      SELECT kna1~kunnr kna1~name1 kna1~stras
                kna1~ort01 kna1~regio kna1~pstlz
                kna1~land1 kna1~adrnr kna1~ktokd
                kna1~erdat kna1~ernam
                kna1~loevm kna1~faksd kna1~lifsd
                knb1~bukrs knb1~akont
                INTO TABLE it_final_customer FROM kna1 "JOIN J_1IMOCUST ON KNA1~KUNNR = J_1IMOCUST~KUNNR
                JOIN knb1 ON kna1~kunnr = knb1~kunnr
      WHERE kna1~kunnr IN kunnr AND kna1~erdat IN pd_c.


    ENDIF.

    DATA: w_fcat_c4 TYPE slis_fieldcat_alv,
          i_fcat_c4 TYPE slis_t_fieldcat_alv,
          layout_c4 TYPE slis_layout_alv.

    w_fcat_c4-fieldname = 'KUNNR'.
    w_fcat_c4-ref_tabname = 'KNA1'.
    w_fcat_c4-emphasize = 'C311'.
    w_fcat_c4-outputlen = '12'.
    w_fcat_c4-col_pos = '1'.
    APPEND w_fcat_c4 TO i_fcat_c4.
    CLEAR w_fcat_c4.

    w_fcat_c4-fieldname = 'BUKRS'.
    w_fcat_c4-ref_tabname = 'KNB1'.
    w_fcat_c4-emphasize = 'C110'.
    w_fcat_c4-outputlen = '12'.
    w_fcat_c4-col_pos = '2'.
    APPEND w_fcat_c4 TO i_fcat_c4.
    CLEAR w_fcat_c4.

    w_fcat_c4-fieldname = 'NAME1'.
    w_fcat_c4-ref_tabname = 'KNA1'.
    w_fcat_c4-emphasize = 'C110'.
    w_fcat_c4-outputlen = '12'.
    w_fcat_c4-col_pos = '3'.
    APPEND w_fcat_c4 TO i_fcat_c4.
    CLEAR w_fcat_c4.

    w_fcat_c4-fieldname = 'STRAS'.
    w_fcat_c4-ref_tabname = 'KNA1'.
    w_fcat_c4-emphasize = 'C110'.
    w_fcat_c4-outputlen = '12'.
    w_fcat_c4-col_pos = '4'.
    APPEND w_fcat_c4 TO i_fcat_c4.
    CLEAR w_fcat_c4.

    w_fcat_c4-fieldname = 'ORT01'.
    w_fcat_c4-ref_tabname = 'KNA1'.
    w_fcat_c4-emphasize = 'C110'.
    w_fcat_c4-outputlen = '12'.
    w_fcat_c4-col_pos = '5'.
    APPEND w_fcat_c4 TO i_fcat_c4.
    CLEAR w_fcat_c4.

    w_fcat_c4-fieldname = 'REGIO'.
    w_fcat_c4-ref_tabname = 'KNA1'.
    w_fcat_c4-emphasize = 'C110'.
    w_fcat_c4-outputlen = '12'.
    w_fcat_c4-col_pos = '6'.
    APPEND w_fcat_c4 TO i_fcat_c4.
    CLEAR w_fcat_c4.

    w_fcat_c4-fieldname = 'PSTLZ'.
    w_fcat_c4-ref_tabname = 'KNA1'.
    w_fcat_c4-emphasize = 'C110'.
    w_fcat_c4-outputlen = '12'.
    w_fcat_c4-col_pos = '7'.
    APPEND w_fcat_c4 TO i_fcat_c4.
    CLEAR w_fcat_c4.

    w_fcat_c4-fieldname = 'LAND1'.
    w_fcat_c4-ref_tabname = 'KNA1'.
    w_fcat_c4-emphasize = 'C110'.
    w_fcat_c4-outputlen = '12'.
    w_fcat_c4-col_pos = '8'.
    APPEND w_fcat_c4 TO i_fcat_c4.
    CLEAR w_fcat_c4.

    w_fcat_c4-fieldname = 'ADRNR'.
    w_fcat_c4-ref_tabname = 'KNA1'.
    w_fcat_c4-emphasize = 'C110'.
    w_fcat_c4-outputlen = '12'.
    w_fcat_c4-col_pos = '9'.
    APPEND w_fcat_c4 TO i_fcat_c4.
    CLEAR w_fcat_c4.

    w_fcat_c4-fieldname = 'KTOKD'.
    w_fcat_c4-ref_tabname = 'KNA1'.
    w_fcat_c4-emphasize = 'C110'.
    w_fcat_c4-outputlen = '12'.
    w_fcat_c4-col_pos = '10'.
    APPEND w_fcat_c4 TO i_fcat_c4.
    CLEAR w_fcat_c4.

    w_fcat_c4-fieldname = 'AKONT'.
    w_fcat_c4-ref_tabname = 'KNB1'.
    w_fcat_c4-emphasize = 'C110'.
    w_fcat_c4-outputlen = '12'.
    w_fcat_c4-col_pos = '11'.
    APPEND w_fcat_c4 TO i_fcat_c4.
    CLEAR w_fcat_c4.

*           W_FCAT_C4-FIELDNAME = 'J_1IEXCD'.
*           W_FCAT_C4-REF_TABNAME = 'J_1IMOCUST'.
*           W_FCAT_C4-EMPHASIZE = 'C110'.
*           W_FCAT_C4-OUTPUTLEN = '12'.
*           W_FCAT_C4-COL_POS = '12'.
*           APPEND W_FCAT_C4 TO I_FCAT_C4.
*           CLEAR W_FCAT_C4.
*
*
*           W_FCAT_C4-FIELDNAME = 'J_1IEXRN'.
*           W_FCAT_C4-REF_TABNAME = 'J_1IMOCUST'.
*           W_FCAT_C4-EMPHASIZE = 'C110'.
*           W_FCAT_C4-OUTPUTLEN = '12'.
*           W_FCAT_C4-COL_POS = '13'.
*           APPEND W_FCAT_C4 TO I_FCAT_C4.
*           CLEAR W_FCAT_C4.
*
*           W_FCAT_C4-FIELDNAME = 'J_1IEXRG'.
*           W_FCAT_C4-REF_TABNAME = 'J_1IMOCUST'.
*           W_FCAT_C4-EMPHASIZE = 'C110'.
*           W_FCAT_C4-OUTPUTLEN = '12'.
*           W_FCAT_C4-COL_POS = '14'.
*           APPEND W_FCAT_C4 TO I_FCAT_C4.
*           CLEAR W_FCAT_C4.
*
*           W_FCAT_C4-FIELDNAME = 'J_1IEXDI'.
*           W_FCAT_C4-REF_TABNAME = 'J_1IMOCUST'.
*           W_FCAT_C4-EMPHASIZE = 'C110'.
*           W_FCAT_C4-OUTPUTLEN = '12'.
*           W_FCAT_C4-COL_POS = '15'.
*           APPEND W_FCAT_C4 TO I_FCAT_C4.
*           CLEAR W_FCAT_C4.
*
*           W_FCAT_C4-FIELDNAME = 'J_1IEXCO'.
*           W_FCAT_C4-REF_TABNAME = 'J_1IMOCUST'.
*           W_FCAT_C4-EMPHASIZE = 'C110'.
*           W_FCAT_C4-OUTPUTLEN = '12'.
*           W_FCAT_C4-COL_POS = '16'.
*           APPEND W_FCAT_C4 TO I_FCAT_C4.
*           CLEAR W_FCAT_C4.
*
*           W_FCAT_C4-FIELDNAME = 'J_1ILSTNO'.
*           W_FCAT_C4-REF_TABNAME = 'J_1IMOCUST'.
*           W_FCAT_C4-EMPHASIZE = 'C110'.
*           W_FCAT_C4-OUTPUTLEN = '12'.
*           W_FCAT_C4-COL_POS = '17'.
*           APPEND W_FCAT_C4 TO I_FCAT_C4.
*           CLEAR W_FCAT_C4.
*
*           W_FCAT_C4-FIELDNAME = 'J_1IPANNO'.
*           W_FCAT_C4-REF_TABNAME = 'J_1IMOCUST'.
*           W_FCAT_C4-EMPHASIZE = 'C110'.
*           W_FCAT_C4-OUTPUTLEN = '12'.
*           W_FCAT_C4-COL_POS = '18'.
*           APPEND W_FCAT_C4 TO I_FCAT_C4.
*           CLEAR W_FCAT_C4.
*
*           W_FCAT_C4-FIELDNAME = 'J_1ISERN'.
*           W_FCAT_C4-REF_TABNAME = 'J_1IMOCUST'.
*           W_FCAT_C4-EMPHASIZE = 'C110'.
*           W_FCAT_C4-OUTPUTLEN = '12'.
*           W_FCAT_C4-COL_POS = '19'.
*           APPEND W_FCAT_C4 TO I_FCAT_C4.
*           CLEAR W_FCAT_C4.

    w_fcat_c4-fieldname = 'ERDAT'.
    w_fcat_c4-ref_tabname = 'KNA1'.
    w_fcat_c4-emphasize = 'C110'.
    w_fcat_c4-outputlen = '12'.
    w_fcat_c4-col_pos = '20'.
    APPEND w_fcat_c4 TO i_fcat_c4.
    CLEAR w_fcat_c4.

    w_fcat_c4-fieldname = 'ERNAM'.
    w_fcat_c4-ref_tabname = 'KNA1'.
    w_fcat_c4-emphasize = 'C110'.
    w_fcat_c4-outputlen = '12'.
    w_fcat_c4-col_pos = '21'.
    APPEND w_fcat_c4 TO i_fcat_c4.
    CLEAR w_fcat_c4.

    w_fcat_c4-fieldname = 'LOEVM'.
    w_fcat_c4-ref_tabname = 'KNA1'.
    w_fcat_c4-emphasize = 'C110'.
    w_fcat_c4-outputlen = '12'.
    w_fcat_c4-col_pos = '22'.
    APPEND w_fcat_c4 TO i_fcat_c4.
    CLEAR w_fcat_c4.

    w_fcat_c4-fieldname = 'FAKSD'.
    w_fcat_c4-ref_tabname = 'KNA1'.
    w_fcat_c4-emphasize = 'C110'.
    w_fcat_c4-outputlen = '12'.
    w_fcat_c4-col_pos = '23'.
    APPEND w_fcat_c4 TO i_fcat_c4.
    CLEAR w_fcat_c4.

    w_fcat_c4-fieldname = 'LIFSD'.
    w_fcat_c4-ref_tabname = 'KNA1'.
    w_fcat_c4-emphasize = 'C110'.
    w_fcat_c4-outputlen = '12'.
    w_fcat_c4-col_pos = '24'.
    APPEND w_fcat_c4 TO i_fcat_c4.
    CLEAR w_fcat_c4.

    DESCRIBE TABLE it_final_customer LINES v_lines.
    c_lines = v_lines.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program     = sy-repid
        i_background_id        = 'ALV_BACKGROUND'
        i_callback_top_of_page = 'TOP_OF_PAGE'
        i_grid_title           = 'CUSTOMER MASTER LIST'
        is_layout              = layout_c4
        it_fieldcat            = i_fcat_c4
      TABLES
        t_outtab               = it_final_customer.


  ELSEIF rb_1 = 'X'. " Vendor

************ Modified start by bharathi as on 29-12-2021 ******************

************* Modified end by bharathi as on 29-12-2021 ******************
*   clear: MATNR,p_date3,p_date3a.
*  clear: KUNNR,p_date2,p_date2a.

*            Data: it_final_Vendor type table of ty_final_Vendor WITH HEADER LINE,
    DATA: it_final_vendor  TYPE TABLE OF ty_final_vendor,
          wa_final_vendor  TYPE ty_final_vendor,
          it_final_vendor1 TYPE TABLE OF ty_final_vendor,
          wa_final_vendor1 TYPE ty_final_vendor.

    TYPES: BEGIN OF ty_pan,
             j_1ipanno TYPE j_1imovend-j_1ipanno,
           END OF ty_pan.
    TYPES: BEGIN OF ty_t016t,
             brtxt TYPE t016t-brtxt,
           END OF ty_t016t.
    DATA: it_pan   TYPE TABLE OF ty_pan,
          wa_pan   TYPE ty_pan,
          it_t016t TYPE TABLE OF ty_t016t,
          wa_t016t TYPE ty_t016t.
    DATA : vendor_a TYPE string.
    DATA : asset_a TYPE string.

    IF srb_1 EQ 'X'.

      IF lifnr IS INITIAL AND pd_v IS NOT INITIAL.

        SELECT  lfa1~lifnr
                lfa1~name1
                lfa1~stras
                lfa1~ort01
                lfa1~regio
                lfa1~pstlz
                lfa1~land1
                lfa1~adrnr
                lfa1~ktokk
                lfa1~erdat
                lfa1~ernam
                lfa1~stenr
                lfa1~loevm
                lfa1~sperr
                lfa1~sperm
                lfa1~stcd5
                lfa1~brsch
                lfa1~telf1  "TELEPHONE NUMBER 1 ADDED BY BHARATHI ON 04.05.2022
                lfa1~telf2  "TELEPHONE NUMBER 2 ADDED BY BHARATHI ON 04.05.2022
*                    LFA1~ADRNR  "Address Number  ADDED BY BHARATHI ON 04.05.2022

*                    J_1IMOVEND~J_1IPANNO
                lfb1~bukrs
                lfb1~akont
                lfb1~qsskz
                lfb1~cerdt
                lfbw~witht
          INTO CORRESPONDING FIELDS OF TABLE it_final_vendor FROM lfa1 JOIN lfb1 ON lfa1~lifnr = lfb1~lifnr
          LEFT JOIN lfbw ON lfbw~lifnr = lfb1~lifnr

        WHERE  lfa1~erdat IN pd_v.

      ELSEIF lifnr IS NOT INITIAL AND pd_v IS NOT INITIAL.

        SELECT  lfa1~lifnr
         lfa1~name1
         lfa1~stras
         lfa1~ort01
         lfa1~regio
         lfa1~pstlz
         lfa1~land1
         lfa1~adrnr
         lfa1~ktokk
         lfa1~erdat
         lfa1~ernam
         lfa1~stenr
         lfa1~loevm
         lfa1~sperr
         lfa1~sperm
         lfa1~stcd5
         lfa1~brsch
         lfa1~telf1  "TELEPHONE NUMBER 1 ADDED BY BHARATHI ON 04.05.2022
         lfa1~telf2  "TELEPHONE NUMBER 2 ADDED BY BHARATHI ON 04.05.2022
*                    LFA1~ADRNR  "Address Number  ADDED BY BHARATHI ON 04.05.2022
*                    J_1IMOVEND~J_1IPANNO
         lfb1~bukrs
         lfb1~akont
         lfb1~qsskz
         lfb1~cerdt
         lfbw~witht
     INTO CORRESPONDING FIELDS OF TABLE it_final_vendor
          FROM lfa1
          JOIN lfb1 ON lfa1~lifnr = lfb1~lifnr
          LEFT JOIN lfbw ON lfbw~lifnr = lfb1~lifnr
        WHERE lfa1~lifnr IN lifnr AND lfa1~erdat IN pd_v.

      ELSEIF lifnr IS NOT INITIAL AND pd_v IS INITIAL.

        SELECT  lfa1~lifnr
         lfa1~name1
         lfa1~stras
         lfa1~ort01
         lfa1~regio
         lfa1~pstlz
         lfa1~land1
         lfa1~adrnr
         lfa1~ktokk
         lfa1~erdat
         lfa1~ernam
         lfa1~stenr
         lfa1~loevm
         lfa1~sperr
         lfa1~sperm
         lfa1~stcd5
         lfa1~brsch
         lfa1~telf1  "TELEPHONE NUMBER 1 ADDED BY BHARATHI ON 04.05.2022
         lfa1~telf2  "TELEPHONE NUMBER 2 ADDED BY BHARATHI ON 04.05.2022
*                    LFA1~ADRNR  "Address Number  ADDED BY BHARATHI ON 04.05.2022
*                    J_1IMOVEND~J_1IPANNO
         lfb1~bukrs
         lfb1~akont
         lfb1~qsskz
         lfb1~cerdt
         lfbw~witht
     INTO CORRESPONDING FIELDS OF TABLE it_final_vendor
          FROM lfa1
          JOIN lfb1 ON lfa1~lifnr = lfb1~lifnr
          LEFT JOIN lfbw ON lfbw~lifnr = lfb1~lifnr
        WHERE lfa1~lifnr IN lifnr.

      ELSEIF lifnr IS INITIAL AND pd_v IS INITIAL.

        SELECT  lfa1~lifnr
         lfa1~name1
         lfa1~stras
         lfa1~ort01
         lfa1~regio
         lfa1~pstlz
         lfa1~land1
         lfa1~adrnr
         lfa1~ktokk
         lfa1~erdat
         lfa1~ernam
         lfa1~stenr
         lfa1~loevm
         lfa1~sperr
         lfa1~sperm
         lfa1~stcd5
         lfa1~brsch
         lfa1~telf1  "TELEPHONE NUMBER 1 ADDED BY BHARATHI ON 04.05.2022
         lfa1~telf2  "TELEPHONE NUMBER 2 ADDED BY BHARATHI ON 04.05.2022
*                    LFA1~ADRNR  "Address Number  ADDED BY BHARATHI ON 04.05.2022
*                    J_1IMOVEND~J_1IPANNO
         lfb1~bukrs
         lfb1~akont
         lfb1~qsskz
         lfb1~cerdt
         lfbw~witht
     INTO CORRESPONDING FIELDS OF TABLE it_final_vendor
          FROM lfa1
          JOIN lfb1 ON lfa1~lifnr = lfb1~lifnr
        LEFT JOIN lfbw ON lfa1~lifnr = lfbw~lifnr.
      ENDIF.
    ELSE.

      IF lifnr IS INITIAL AND pd_v IS NOT INITIAL.

        SELECT  lfa1~lifnr
                lfa1~name1
                lfa1~stras
                lfa1~ort01
                lfa1~regio
                lfa1~pstlz
                lfa1~land1
                lfa1~adrnr
                lfa1~ktokk
                lfa1~erdat
                lfa1~ernam
                lfa1~stenr
                lfa1~loevm
                lfa1~sperr
                lfa1~sperm
                lfa1~stcd5
                lfa1~brsch
                lfa1~telf1  "TELEPHONE NUMBER 1 ADDED BY BHARATHI ON 04.05.2022
                lfa1~telf2  "TELEPHONE NUMBER 2 ADDED BY BHARATHI ON 04.05.2022
*                    LFA1~ADRNR  "Address Number  ADDED BY BHARATHI ON 04.05.2022
*                    J_1IMOVEND~J_1IPANNO
                lfb1~bukrs
                lfb1~akont
                lfb1~qsskz
                lfb1~cerdt
          INTO CORRESPONDING FIELDS OF TABLE it_final_vendor FROM lfa1 JOIN lfb1 ON lfa1~lifnr = lfb1~lifnr
        WHERE  lfa1~erdat IN pd_v.

      ELSEIF lifnr IS NOT INITIAL AND pd_v IS NOT INITIAL.

        SELECT  lfa1~lifnr
         lfa1~name1
         lfa1~stras
         lfa1~ort01
         lfa1~regio
         lfa1~pstlz
         lfa1~land1
         lfa1~adrnr
         lfa1~ktokk
         lfa1~erdat
         lfa1~ernam
         lfa1~stenr
         lfa1~loevm
         lfa1~sperr
         lfa1~sperm
         lfa1~stcd5
         lfa1~brsch
         lfa1~telf1  "TELEPHONE NUMBER 1 ADDED BY BHARATHI ON 04.05.2022
         lfa1~telf2  "TELEPHONE NUMBER 2 ADDED BY BHARATHI ON 04.05.2022
*                    LFA1~ADRNR  "Address Number  ADDED BY BHARATHI ON 04.05.2022
*                    J_1IMOVEND~J_1IPANNO
         lfb1~bukrs
         lfb1~akont
         lfb1~qsskz
         lfb1~cerdt
     INTO CORRESPONDING FIELDS OF TABLE it_final_vendor
          FROM lfa1
          JOIN lfb1 ON lfa1~lifnr = lfb1~lifnr
        WHERE lfa1~lifnr IN lifnr AND lfa1~erdat IN pd_v.

      ELSEIF lifnr IS NOT INITIAL AND pd_v IS INITIAL.

        SELECT  lfa1~lifnr
         lfa1~name1
         lfa1~stras
         lfa1~ort01
         lfa1~regio
         lfa1~pstlz
         lfa1~land1
         lfa1~adrnr
         lfa1~ktokk
         lfa1~erdat
         lfa1~ernam
         lfa1~stenr
         lfa1~loevm
         lfa1~sperr
         lfa1~sperm
         lfa1~stcd5
         lfa1~brsch
         lfa1~telf1  "TELEPHONE NUMBER 1 ADDED BY BHARATHI ON 04.05.2022
         lfa1~telf2  "TELEPHONE NUMBER 2 ADDED BY BHARATHI ON 04.05.2022
*                    LFA1~ADRNR  "Address Number  ADDED BY BHARATHI ON 04.05.2022
*                    J_1IMOVEND~J_1IPANNO
         lfb1~bukrs
         lfb1~akont
         lfb1~qsskz
         lfb1~cerdt
     INTO CORRESPONDING FIELDS OF TABLE it_final_vendor
          FROM lfa1
          JOIN lfb1 ON lfa1~lifnr = lfb1~lifnr
        WHERE lfa1~lifnr IN lifnr.

      ELSEIF lifnr IS INITIAL AND pd_v IS INITIAL.

        SELECT  lfa1~lifnr
         lfa1~name1
         lfa1~stras
         lfa1~ort01
         lfa1~regio
         lfa1~pstlz
         lfa1~land1
         lfa1~adrnr
         lfa1~ktokk
         lfa1~erdat
         lfa1~ernam
         lfa1~stenr
         lfa1~loevm
         lfa1~sperr
         lfa1~sperm
         lfa1~stcd5
         lfa1~brsch
         lfa1~telf1  "TELEPHONE NUMBER 1 ADDED BY BHARATHI ON 04.05.2022
         lfa1~telf2  "TELEPHONE NUMBER 2 ADDED BY BHARATHI ON 04.05.2022
*                    LFA1~ADRNR  "Address Number  ADDED BY BHARATHI ON 04.05.2022
*                    J_1IMOVEND~J_1IPANNO
         lfb1~bukrs
         lfb1~akont
         lfb1~qsskz
         lfb1~cerdt

     INTO CORRESPONDING FIELDS OF TABLE it_final_vendor
          FROM lfa1
        JOIN lfb1 ON lfa1~lifnr = lfb1~lifnr.
      ENDIF.
    ENDIF.
*JOIN LFBW ON LFBW~LIFNR = LFB1~LIFNR
    DELETE ADJACENT DUPLICATES FROM it_final_vendor COMPARING ALL FIELDS.
*********************************************************************************** Hide By Viknesh 12.05.2022
**IF SRB_1 EQ 'X'.
**SELECT LFBW~LIFNR LFBW~WITHT T059U~TEXT40  INTO CORRESPONDING FIELDS OF TABLE IT_TAX
**                FROM LFBW LEFT JOIN T059U ON LFBW~WITHT = T059U~WITHT.
**
**LOOP AT IT_FINAL_VENDOR.
**
**    READ TABLE IT_TAX WITH KEY LIFNR = IT_FINAL_VENDOR-LIFNR WITHT = IT_FINAL_VENDOR-WITHT.
**    IT_FINAL_VENDOR-WITHT = IT_TAX-WITHT.
**    IT_FINAL_VENDOR-TEXT40 = IT_TAX-TEXT40.
**    MODIFY IT_FINAL_VENDOR.
**    CLEAR: IT_TAX.
**  ENDLOOP.
**ENDIF.
*********************************************************************************** Hide By Viknesh 12.05.2022
    LOOP AT it_final_vendor INTO wa_final_vendor.
      SELECT SINGLE smtp_addr INTO wa_final_vendor-smtp_addr FROM adr6 WHERE addrnumber = wa_final_vendor-adrnr.
      SELECT j_1ipanno FROM j_1imovend INTO CORRESPONDING FIELDS OF TABLE it_pan WHERE lifnr = wa_final_vendor-lifnr.
      SELECT brtxt FROM t016t INTO CORRESPONDING FIELDS OF TABLE it_t016t WHERE brsch = wa_final_vendor-brsch.

      READ TABLE it_pan INTO wa_pan INDEX 1.
      READ TABLE it_t016t INTO wa_t016t INDEX 1.

      wa_final_vendor-j_1ipanno = wa_pan-j_1ipanno.
      wa_final_vendor-brtxt = wa_t016t-brtxt.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_final_vendor-lifnr
        IMPORTING
          output = vendor_a.
      SELECT DISTINCT instid_a catid_b FROM srgbtbrel INTO TABLE it_atta WHERE  instid_a = vendor_a.

      IF sy-subrc = 0.
        wa_final_vendor-atta = 'Y'.
      ELSE.
        wa_final_vendor-atta = 'N'.
      ENDIF.


      APPEND wa_final_vendor TO it_final_vendor1.
      CLEAR: wa_pan, it_pan,it_t016t,wa_t016t ,it_atta,wa_atta.
    ENDLOOP.
    CLEAR: it_final_vendor, wa_final_vendor.
    it_final_vendor[] = it_final_vendor1[].

    DESCRIBE TABLE it_final_vendor LINES v_lines.

    DATA: w_fcat_v4 TYPE slis_fieldcat_alv,
          i_fcat_v4 TYPE slis_t_fieldcat_alv,
          layout_v4 TYPE slis_layout_alv.


    w_fcat_v4-fieldname = 'LIFNR'.
    w_fcat_v4-ref_tabname = 'LFA1'.
    w_fcat_v4-emphasize = 'C311'.
    w_fcat_v4-outputlen = '12'.
    w_fcat_v4-col_pos = '1'.
    APPEND w_fcat_v4 TO i_fcat_v4.
    CLEAR w_fcat_v4.

    w_fcat_v4-fieldname = 'BUKRS'.
    w_fcat_v4-ref_tabname = 'LFB1'.
    w_fcat_v4-emphasize = 'C110'.
    w_fcat_v4-outputlen = '12'.
    w_fcat_v4-col_pos = '2'.
    APPEND w_fcat_v4 TO i_fcat_v4.
    CLEAR w_fcat_v4.

    w_fcat_v4-fieldname = 'NAME1'.
    w_fcat_v4-ref_tabname = 'LFA1'.
    w_fcat_v4-emphasize = 'C110'.
    w_fcat_v4-outputlen = '12'.
    w_fcat_v4-col_pos = '3'.
    APPEND w_fcat_v4 TO i_fcat_v4.
    CLEAR w_fcat_v4.

    w_fcat_v4-fieldname = 'STRAS'.
    w_fcat_v4-ref_tabname = 'LFA1'.
    w_fcat_v4-emphasize = 'C110'.
    w_fcat_v4-outputlen = '12'.
    w_fcat_v4-col_pos = '4'.
    APPEND w_fcat_v4 TO i_fcat_v4.
    CLEAR w_fcat_v4.

    w_fcat_v4-fieldname = 'ORT01'.
    w_fcat_v4-ref_tabname = 'LFA1'.
    w_fcat_v4-emphasize = 'C110'.
    w_fcat_v4-outputlen = '12'.
    w_fcat_v4-col_pos = '5'.
    APPEND w_fcat_v4 TO i_fcat_v4.
    CLEAR w_fcat_v4.

    w_fcat_v4-fieldname = 'REGIO'.
    w_fcat_v4-ref_tabname = 'LFA1'.
    w_fcat_v4-emphasize = 'C110'.
    w_fcat_v4-outputlen = '12'.
    w_fcat_v4-col_pos = '6'.
    APPEND w_fcat_v4 TO i_fcat_v4.
    CLEAR w_fcat_v4.

    w_fcat_v4-fieldname = 'PSTLZ'.
    w_fcat_v4-ref_tabname = 'LFA1'.
    w_fcat_v4-emphasize = 'C110'.
    w_fcat_v4-outputlen = '12'.
    w_fcat_v4-col_pos = '7'.
    APPEND w_fcat_v4 TO i_fcat_v4.
    CLEAR w_fcat_v4.

    w_fcat_v4-fieldname = 'LAND1'.
    w_fcat_v4-ref_tabname = 'LFA1'.
    w_fcat_v4-emphasize = 'C110'.
    w_fcat_v4-outputlen = '12'.
    w_fcat_v4-col_pos = '8'.
    APPEND w_fcat_v4 TO i_fcat_v4.
    CLEAR w_fcat_v4.

    w_fcat_v4-fieldname = 'ADRNR'.
    w_fcat_v4-ref_tabname = 'LFA1'.
    w_fcat_v4-emphasize = 'C110'.
    w_fcat_v4-outputlen = '12'.
    w_fcat_v4-col_pos = '9'.
    APPEND w_fcat_v4 TO i_fcat_v4.
    CLEAR w_fcat_v4.

    w_fcat_v4-fieldname = 'KTOKK'.
    w_fcat_v4-ref_tabname = 'LFA1'.
    w_fcat_v4-emphasize = 'C110'.
    w_fcat_v4-outputlen = '12'.
    w_fcat_v4-col_pos = '10'.
    APPEND w_fcat_v4 TO i_fcat_v4.
    CLEAR w_fcat_v4.

    w_fcat_v4-fieldname = 'AKONT'.
    w_fcat_v4-ref_tabname = 'LFB1'.
    w_fcat_v4-emphasize = 'C110'.
    w_fcat_v4-outputlen = '12'.
    w_fcat_v4-col_pos = '11'.
    APPEND w_fcat_v4 TO i_fcat_v4.
    CLEAR w_fcat_v4.

    w_fcat_v4-fieldname = 'QSSKZ'.
    w_fcat_v4-ref_tabname = 'LFB1'.
    w_fcat_v4-emphasize = 'C110'.
    w_fcat_v4-outputlen = '12'.
    w_fcat_v4-col_pos = '12'.
    APPEND w_fcat_v4 TO i_fcat_v4.
    CLEAR w_fcat_v4.

*           W_FCAT_V4-FIELDNAME = 'J_1IEXCD'.
*           W_FCAT_V4-REF_TABNAME = 'J_1IMOVEND'.
*           W_FCAT_V4-EMPHASIZE = 'C110'.
*           W_FCAT_V4-OUTPUTLEN = '12'.
*           W_FCAT_V4-COL_POS = '13'.
*           APPEND W_FCAT_V4 TO I_FCAT_V4.
*           CLEAR W_FCAT_V4.
*
*           W_FCAT_V4-FIELDNAME = 'J_1IEXRN'.
*           W_FCAT_V4-REF_TABNAME = 'J_1IMOVEND'.
*           W_FCAT_V4-EMPHASIZE = 'C110'.
*           W_FCAT_V4-OUTPUTLEN = '12'.
*           W_FCAT_V4-COL_POS = '14'.
*           APPEND W_FCAT_V4 TO I_FCAT_V4.
*           CLEAR W_FCAT_V4.
*
*           W_FCAT_V4-FIELDNAME = 'J_1IEXRG'.
*           W_FCAT_V4-REF_TABNAME = 'J_1IMOVEND'.
*           W_FCAT_V4-EMPHASIZE = 'C110'.
*           W_FCAT_V4-OUTPUTLEN = '12'.
*           W_FCAT_V4-COL_POS = '15'.
*           APPEND W_FCAT_V4 TO I_FCAT_V4.
*           CLEAR W_FCAT_V4.
*
*           W_FCAT_V4-FIELDNAME = 'J_1IEXDI'.
*           W_FCAT_V4-REF_TABNAME = 'J_1IMOVEND'.
*           W_FCAT_V4-EMPHASIZE = 'C110'.
*           W_FCAT_V4-OUTPUTLEN = '12'.
*           W_FCAT_V4-COL_POS = '16'.
*           APPEND W_FCAT_V4 TO I_FCAT_V4.
*           CLEAR W_FCAT_V4.
*
*           W_FCAT_V4-FIELDNAME = 'J_1IEXCO'.
*           W_FCAT_V4-REF_TABNAME = 'J_1IMOVEND'.
*           W_FCAT_V4-EMPHASIZE = 'C110'.
*           W_FCAT_V4-OUTPUTLEN = '12'.
*           W_FCAT_V4-COL_POS = '17'.
*           APPEND W_FCAT_V4 TO I_FCAT_V4.
*           CLEAR W_FCAT_V4.
*
*           W_FCAT_V4-FIELDNAME = 'J_1ILSTNO'.
*           W_FCAT_V4-REF_TABNAME = 'J_1IMOVEND'.
*           W_FCAT_V4-EMPHASIZE = 'C110'.
*           W_FCAT_V4-OUTPUTLEN = '12'.
*           W_FCAT_V4-COL_POS = '18'.
*           APPEND W_FCAT_V4 TO I_FCAT_V4.
*           CLEAR W_FCAT_V4.
*
*           W_FCAT_V4-FIELDNAME = 'J_1ICSTNO'.
*           W_FCAT_V4-REF_TABNAME = 'J_1IMOVEND'.
*           W_FCAT_V4-EMPHASIZE = 'C110'.
*           W_FCAT_V4-OUTPUTLEN = '12'.
*           W_FCAT_V4-COL_POS = '19'.
*           APPEND W_FCAT_V4 TO I_FCAT_V4.
*           CLEAR W_FCAT_V4.
*
    w_fcat_v4-fieldname = 'J_1IPANNO'.
    w_fcat_v4-ref_tabname = 'J_1IMOVEND'.
    w_fcat_v4-emphasize = 'C110'.
    w_fcat_v4-outputlen = '12'.
    w_fcat_v4-col_pos = '25'.
    APPEND w_fcat_v4 TO i_fcat_v4.
    CLEAR w_fcat_v4.
*
*           W_FCAT_V4-FIELDNAME = 'J_1ISERN'.
*           W_FCAT_V4-REF_TABNAME = 'J_1IMOVEND'.
*           W_FCAT_V4-EMPHASIZE = 'C110'.
*           W_FCAT_V4-OUTPUTLEN = '12'.
*           W_FCAT_V4-COL_POS = '21'.
*           APPEND W_FCAT_V4 TO I_FCAT_V4.
*           CLEAR W_FCAT_V4.

    w_fcat_v4-fieldname = 'ERDAT'.
    w_fcat_v4-ref_tabname = 'LFA1'.
    w_fcat_v4-emphasize = 'C110'.
    w_fcat_v4-outputlen = '12'.
    w_fcat_v4-col_pos = '22'.
    APPEND w_fcat_v4 TO i_fcat_v4.
    CLEAR w_fcat_v4.

    w_fcat_v4-fieldname = 'ERNAM'.
    w_fcat_v4-ref_tabname = 'LFA1'.
    w_fcat_v4-emphasize = 'C110'.
    w_fcat_v4-outputlen = '12'.
    w_fcat_v4-col_pos = '23'.
    APPEND w_fcat_v4 TO i_fcat_v4.
    CLEAR w_fcat_v4.

    w_fcat_v4-fieldname = 'STENR'.
    w_fcat_v4-seltext_s = 'GST NO'.
*           W_FCAT_V4-REF_TABNAME = 'LFA1'.
    w_fcat_v4-emphasize = 'C110'.
    w_fcat_v4-outputlen = '16'.
    w_fcat_v4-col_pos = '24'.
    APPEND w_fcat_v4 TO i_fcat_v4.
    CLEAR w_fcat_v4.
******************************************** 1 ADD on 02.01.2020
    w_fcat_v4-fieldname = 'STCD5'.
*           W_FCAT_V4-seltext_s = 'GST NO'.
    w_fcat_v4-ref_tabname = 'LFA1'.
    w_fcat_v4-emphasize = 'C110'.
    w_fcat_v4-outputlen = '10'.
    w_fcat_v4-col_pos = '25'.
    APPEND w_fcat_v4 TO i_fcat_v4.
    CLEAR w_fcat_v4.

*           W_FCAT_V4-FIELDNAME = 'BRSCH'.
**           W_FCAT_V4-seltext_s = 'GST NO'.
*           W_FCAT_V4-REF_TABNAME = 'LFA1'.
*           W_FCAT_V4-EMPHASIZE = 'C110'.
*           W_FCAT_V4-OUTPUTLEN = '10'.
*           W_FCAT_V4-COL_POS = '25'.
*           APPEND W_FCAT_V4 TO I_FCAT_V4.
*           CLEAR W_FCAT_V4.

    w_fcat_v4-fieldname = 'BRTXT'.
    w_fcat_v4-seltext_m = 'MSME category '.
*           W_FCAT_V4-REF_TABNAME = 'T016T'.
    w_fcat_v4-emphasize = 'C110'.
    w_fcat_v4-outputlen = '16'.
    w_fcat_v4-col_pos = '25'.
    APPEND w_fcat_v4 TO i_fcat_v4.
    CLEAR w_fcat_v4.
********************************************* 1 ADD on 02.01.2020

    w_fcat_v4-fieldname = 'LOEVM'.
*           W_FCAT_V4-REF_TABNAME = 'LFA1'.
    w_fcat_v4-seltext_l = 'Deletion flag'.
    w_fcat_v4-emphasize = 'C110'.
    w_fcat_v4-outputlen = '10'.
    w_fcat_v4-col_pos = '26'.
    APPEND w_fcat_v4 TO i_fcat_v4.
    CLEAR w_fcat_v4.

    w_fcat_v4-fieldname = 'SPERR'.
*           W_FCAT_V4-REF_TABNAME = 'LFA1'.
    w_fcat_v4-seltext_l = 'Posting Block'.
    w_fcat_v4-emphasize = 'C110'.
    w_fcat_v4-outputlen = '10'.
    w_fcat_v4-col_pos = '27'.
    APPEND w_fcat_v4 TO i_fcat_v4.
    CLEAR w_fcat_v4.

    w_fcat_v4-fieldname = 'SPERM'.
*           W_FCAT_V4-REF_TABNAME = 'LFA1'.
    w_fcat_v4-seltext_l = 'Purch. block'.
    w_fcat_v4-emphasize = 'C110'.
    w_fcat_v4-outputlen = '10'.
    w_fcat_v4-col_pos = '28'.
    APPEND w_fcat_v4 TO i_fcat_v4.
    CLEAR w_fcat_v4.

    w_fcat_v4-fieldname = 'ATTA'.
*           W_FCAT_V4-REF_TABNAME = 'LFA1'.
    w_fcat_v4-seltext_l = 'Attachment'.
    w_fcat_v4-emphasize = 'C110'.
    w_fcat_v4-outputlen = '10'.
    w_fcat_v4-col_pos = '28'.
    w_fcat_v4-hotspot = 'X'.
    APPEND w_fcat_v4 TO i_fcat_v4.
    CLEAR w_fcat_v4.

    w_fcat_v4-fieldname = 'CERDT'.
*           W_FCAT_V4-REF_TABNAME = 'LFA1'.
    w_fcat_v4-seltext_l = 'Agrmt Expiry Date'.
    w_fcat_v4-emphasize = 'C110'.
    w_fcat_v4-outputlen = '10'.
    w_fcat_v4-col_pos = '28'.
*           W_FCAT_V4-HOTSPOT = 'X'.
    APPEND w_fcat_v4 TO i_fcat_v4.
    CLEAR w_fcat_v4.
******************** modified by bharathi on 29.12.2021
*           IF SRB_1 EQ 'X'.
*           W_FCAT_V4-FIELDNAME = 'WITHT'.
**           W_FCAT_V4-REF_TABNAME = 'LFA1'.
*           W_FCAT_V4-seltext_L = 'withholding tax type'.
*           W_FCAT_V4-EMPHASIZE = 'C110'.
*           W_FCAT_V4-OUTPUTLEN = '10'.
*           W_FCAT_V4-COL_POS = '28'.
*           W_FCAT_V4-HOTSPOT = 'X'.
*           APPEND W_FCAT_V4 TO I_FCAT_V4.
*           CLEAR W_FCAT_V4.

*           W_FCAT_V4-FIELDNAME = 'TEXT40'.
**           W_FCAT_V4-REF_TABNAME = 'LFA1'.
*           W_FCAT_V4-seltext_L = 'Description of tax type'.
*           W_FCAT_V4-EMPHASIZE = 'C110'.
*           W_FCAT_V4-OUTPUTLEN = '30'.
*           W_FCAT_V4-COL_POS = '28'.
**           W_FCAT_V4-HOTSPOT = 'X'.
*           APPEND W_FCAT_V4 TO I_FCAT_V4.
*           CLEAR W_FCAT_V4.
*           endif.
************************************** TELEPHONE NUMNERS BHARATHI ADDED ON 04.05.2022
    w_fcat_v4-fieldname = 'SMTP_ADDR'.
    w_fcat_v4-ref_tabname = 'ADR6'.
    w_fcat_v4-emphasize = 'C110'.
    w_fcat_v4-outputlen = '40'.
    w_fcat_v4-col_pos = '29'.
    APPEND w_fcat_v4 TO i_fcat_v4.
    CLEAR w_fcat_v4.

    w_fcat_v4-fieldname = 'TELF1'.
    w_fcat_v4-seltext_l = 'Telephone Number'.
*           W_FCAT_V4-REF_TABNAME = 'LFA1'.
    w_fcat_v4-emphasize = 'C110'.
    w_fcat_v4-outputlen = '12'.
    w_fcat_v4-col_pos = '30'.
    APPEND w_fcat_v4 TO i_fcat_v4.
    CLEAR w_fcat_v4.

    w_fcat_v4-fieldname = 'TELF2'.
    w_fcat_v4-seltext_l = 'Mobile Number'.
*           W_FCAT_V4-REF_TABNAME = 'LFA1'.
    w_fcat_v4-emphasize = 'C110'.
    w_fcat_v4-outputlen = '12'.
    w_fcat_v4-col_pos = '31'.
    APPEND w_fcat_v4 TO i_fcat_v4.
    CLEAR w_fcat_v4.





    DESCRIBE TABLE it_final_vendor LINES v_lines.
*           BREAK abap.
    c_lines = v_lines.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program      = sy-repid
        i_background_id         = 'ALV_BACKGROUND'
        i_callback_user_command = gt_callback_subroutine
        i_callback_top_of_page  = 'TOP_OF_PAGE'
        i_grid_title            = 'VENDOR MASTER LIST'
        is_layout               = layout_v4
        it_fieldcat             = i_fcat_v4
      TABLES
        t_outtab                = it_final_vendor.



  ELSEIF rb_4 = 'X'.



    SELECT ktopl ktplt FROM t004t INTO TABLE it_t004t.
    SELECT  ktopl ktoks txt30 FROM t077z INTO TABLE it_t077z.

    IF saknr IS INITIAL AND pd_gl IS NOT INITIAL .
*    SELECT SKA1~KTOPL SKA1~SAKNR SKA1~KTOKS SKA1~XSPEA  SKA1~XSPEB  SKA1~XSPEP  SKA1~XBILK  SKA1~ERDAT
*           SKA1~ERNAM SKB1~FSTAG  SKB1~MITKZ  SKB1~BUKRS SKAT~TXT20 SKAT~TXT50 INTO TABLE IT_GLFINAL FROM SKA1 JOIN SKB1 ON SKA1~SAKNR = SKB1~SAKNR
*           JOIN SKAT ON SKA1~SAKNR = SKAT~SAKNR WHERE SKA1~ERDAT IN PD_GL and SKA1~KTOPL IN KTOPL and SKB1~BUKRS IN BUKRS AND SKAT~KTOPL IN KTOPL.
      SELECT ska1~ktopl ska1~saknr ska1~ktoks ska1~xspea  ska1~xspeb  ska1~xspep  ska1~xbilk  ska1~erdat
             ska1~ernam skat~txt20 skat~txt50 INTO TABLE it_glfinal FROM ska1 JOIN skat ON ska1~saknr = skat~saknr AND ska1~ktopl = skat~ktopl
      WHERE ska1~ktopl IN ktopl AND ska1~erdat IN pd_gl.
    ELSEIF saknr IS NOT INITIAL AND pd_gl IS NOT INITIAL.
*  SELECT SKA1~KTOPL SKA1~SAKNR SKA1~KTOKS SKA1~XSPEA  SKA1~XSPEB  SKA1~XSPEP  SKA1~XBILK  SKA1~ERDAT
*           SKA1~ERNAM SKB1~FSTAG  SKB1~MITKZ  SKB1~BUKRS SKAT~TXT20 SKAT~TXT50 INTO TABLE IT_GLFINAL FROM SKA1 JOIN SKB1 ON SKA1~SAKNR = SKB1~SAKNR
*           JOIN SKAT ON SKA1~SAKNR = SKAT~SAKNR WHERE SKA1~ERDAT IN PD_GL AND SKA1~SAKNR IN SAKNR and SKA1~KTOPL IN KTOPL and SKB1~BUKRS IN BUKRS
*            AND SKAT~KTOPL IN KTOPL.
      SELECT ska1~ktopl ska1~saknr ska1~ktoks ska1~xspea  ska1~xspeb  ska1~xspep  ska1~xbilk  ska1~erdat
             ska1~ernam skat~txt20 skat~txt50 INTO TABLE it_glfinal FROM ska1 JOIN skat ON ska1~saknr = skat~saknr AND ska1~ktopl = skat~ktopl
      WHERE ska1~saknr IN saknr AND ska1~ktopl IN ktopl AND ska1~erdat IN pd_gl.
    ELSEIF saknr IS NOT INITIAL AND pd_gl IS INITIAL.
*  SELECT SKA1~KTOPL SKA1~SAKNR SKA1~KTOKS SKA1~XSPEA  SKA1~XSPEB  SKA1~XSPEP  SKA1~XBILK  SKA1~ERDAT
*           SKA1~ERNAM SKB1~FSTAG  SKB1~MITKZ  SKB1~BUKRS SKAT~TXT20 SKAT~TXT50 INTO TABLE IT_GLFINAL FROM SKA1 JOIN SKB1 ON SKA1~SAKNR = SKB1~SAKNR
*           JOIN SKAT ON SKA1~SAKNR = SKAT~SAKNR WHERE SKA1~SAKNR IN SAKNR and SKA1~KTOPL IN KTOPL and SKB1~BUKRS IN BUKRS AND SKAT~KTOPL IN KTOPL.
      SELECT ska1~ktopl ska1~saknr ska1~ktoks ska1~xspea  ska1~xspeb  ska1~xspep  ska1~xbilk  ska1~erdat
             ska1~ernam skat~txt20 skat~txt50 INTO TABLE it_glfinal FROM ska1 JOIN skat ON ska1~saknr = skat~saknr AND ska1~ktopl = skat~ktopl
      WHERE ska1~saknr IN saknr AND ska1~ktopl IN ktopl.
    ELSEIF saknr IS INITIAL AND pd_gl IS INITIAL.
*  SELECT SKA1~KTOPL SKA1~SAKNR SKA1~KTOKS SKA1~XSPEA  SKA1~XSPEB  SKA1~XSPEP  SKA1~XBILK  SKA1~ERDAT
*            SKA1~ERNAM   INTO TABLE IT_GLFINAL FROM SKA1 JOIN SKB1 ON SKA1~SAKNR = SKB1~SAKNR
*            AND SKA1~ERDAT = SKB1~ERDAT WHERE SKA1~KTOPL IN KTOPL and SKB1~BUKRS IN BUKRS.

      SELECT ska1~ktopl ska1~saknr ska1~ktoks ska1~xspea  ska1~xspeb  ska1~xspep  ska1~xbilk  ska1~erdat
               ska1~ernam skat~txt20 skat~txt50 INTO TABLE it_glfinal FROM ska1 JOIN skat ON ska1~saknr = skat~saknr AND ska1~ktopl = skat~ktopl
      WHERE ska1~ktopl IN ktopl.
    ENDIF.
******************
* SKAT~TXT20 SKAT~TXT50

*JOIN SKAT ON SKA1~SAKNR = SKAT~SAKNR and SKA1~KTOPL = SKAT~KTOPL
*******************
*IF PD_GLC IS NOT INITIAL.
*
*ENDIF.
*****        Select CHANGENR  USERNAME UDATE FROM CDHDR INTO TABLE IT_CDHDR WHERE UDATE IN PD_GLC AND TCODE EQ 'FS00'.
*****          LOOP AT IT_CDHDR INTO WA_CDHDR.
*****
*****            SELECT TABKEY VALUE_NEW VALUE_OLD FNAME FROM CDPOS INTO TABLE IT_CDPOS WHERE CHANGENR EQ WA_CDHDR-CHANGENR AND TABNAME EQ 'SKAT' AND
*****              ( FNAME EQ 'TXT50' OR FNAME EQ 'TXT20' ).
*****              LOOP AT IT_CDPOS INTO WA_CDPOS.
*****                Len = STRLEN( WA_CDPOS-TABKEY ).
*****                Len = Len - 10.
*****                C = WA_CDPOS-TABKEY+len(10).
*****                WA_CDPOS1-TABKEY     =   C.
******                WA_CDPOS1-TABKEY     =   WA_CDPOS-TABKEY.
*****                WA_CDPOS1-VALUE_NEW  =   WA_CDPOS-VALUE_NEW.
*****                WA_CDPOS1-VALUE_OLD  =   WA_CDPOS-VALUE_OLD.
*****                WA_CDPOS1-FNAME      =   WA_CDPOS-FNAME.
*****                WA_CDPOS1-USERNAME   =   WA_CDHDR-USERNAME.
*****                WA_CDPOS1-UDATE      =   WA_CDHDR-UDATE.
*****                APPEND WA_CDPOS1 TO IT_CDPOS1.
*****                Clear: WA_CDPOS1,WA_CDPOS, Len, C.
*****              ENDLOOP.
*****                Clear: WA_CDHDR.
*****          ENDLOOP.
*****          SORT IT_CDPOS1 BY UDATE DESCENDING.
    LOOP AT it_glfinal INTO wa_glfinal .
      idx = sy-tabix.
      READ TABLE it_t004t INTO wa_t004t WITH KEY ktopl = wa_glfinal-ktopl.
      READ TABLE it_t077z INTO wa_t077z WITH KEY ktoks = wa_glfinal-ktoks ktopl = wa_glfinal-ktopl .  " add on 04.02.2020 (KTOPL = WA_GLFINAL-KTOPL)
*      READ TABLE IT_CDPOS1 INTO WA_CDPOS1 WITH KEY TABKEY = WA_GLFINAL-SAKNR.
*      Select SAKNR TXT20 TXT50 FROM SKAT INTO TABLE IT_SKAT WHERE SAKNR = WA_GLFINAL-SAKNR and KTOPL = WA_GLFINAL-KTOPL.
*      READ TABLE IT_SKAT INTO WA_SKAT WITH KEY SAKNR = WA_GLFINAL-SAKNR.
*      WA_GLFINAL-TXT20  = WA_SKAT-TXT20.
*      WA_GLFINAL-TXT50  = WA_SKAT-TXT50 .

      SELECT saknr fstag mitkz bukrs erdat ernam FROM skb1 INTO TABLE it_skb1 WHERE  saknr = wa_glfinal-saknr AND bukrs IN bukrs .
*        READ TABLE IT_SKB1 INTO WA_SKB1 WITH KEY SAKNR = WA_GLFINAL-SAKNR.
      LOOP AT it_skb1 INTO wa_skb1.
        IF  wa_skb1-bukrs NE '' .
          wa_glfinal-fstag  = wa_skb1-fstag .
          wa_glfinal-mitkz  = wa_skb1-mitkz .
          wa_glfinal-bukrs  = wa_skb1-bukrs .
          wa_glfinal-ktplt  = wa_t004t-ktplt.
          wa_glfinal-txt30  = wa_t077z-txt30.
          wa_glfinal-g_erdat = wa_skb1-erdat.
          wa_glfinal-g_ernam = wa_skb1-ernam.
          wa_glfinal-fstag  = wa_skb1-fstag .
          APPEND wa_glfinal TO it_glfinal1.
        ENDIF.

      ENDLOOP.
*      WA_GLFINAL-MITKZ  = WA_SKB1-MITKZ .
*      WA_GLFINAL-BUKRS  = WA_SKB1-BUKRS .
*      WA_GLFINAL-KTPLT  = WA_T004T-KTPLT.
*      WA_GLFINAL-TXT30  = WA_T077Z-TXT30.

*      MODIFY IT_GLFINAL FROM WA_GLFINAL INDEX idx TRANSPORTING KTPLT TXT30 FSTAG MITKZ BUKRS.
      CLEAR: wa_t004t,wa_t077z,wa_skb1.
    ENDLOOP.

    CLEAR it_glfinal.
    it_glfinal[] = it_glfinal1[].
    SORT it_glfinal BY erdat ASCENDING.

    DATA: w_fcat_v5 TYPE slis_fieldcat_alv,
          i_fcat_v5 TYPE slis_t_fieldcat_alv,
          layout_v5 TYPE slis_layout_alv.


    w_fcat_v5-fieldname = 'KTOPL'.
    w_fcat_v5-ref_tabname = 'SKA1'.
    w_fcat_v5-emphasize = 'C311'.
    w_fcat_v5-outputlen = '12'.
    w_fcat_v5-col_pos = '1'.
    APPEND w_fcat_v5 TO i_fcat_v5.
    CLEAR w_fcat_v5.

    w_fcat_v5-fieldname = 'KTPLT'.
    w_fcat_v5-ref_tabname = 'T004T'.
    w_fcat_v5-emphasize = 'C110'.
    w_fcat_v5-outputlen = '30'.
    w_fcat_v5-col_pos = '2'.
    APPEND w_fcat_v5 TO i_fcat_v5.
    CLEAR w_fcat_v5..

    w_fcat_v5-fieldname = 'BUKRS'.
    w_fcat_v5-ref_tabname = 'SKB1'.
    w_fcat_v5-emphasize = 'C110'.
    w_fcat_v5-outputlen = '6'.
    w_fcat_v5-col_pos = '3'.
    APPEND w_fcat_v5 TO i_fcat_v5.
    CLEAR w_fcat_v5.

    w_fcat_v5-fieldname = 'SAKNR'.
    w_fcat_v5-ref_tabname = 'SKA1'.
    w_fcat_v5-emphasize = 'C110'.
    w_fcat_v5-outputlen = '12'.
    w_fcat_v5-col_pos = '4'.
    APPEND w_fcat_v5 TO i_fcat_v5.
    CLEAR w_fcat_v5.

    w_fcat_v5-fieldname = 'TXT20'.
    w_fcat_v5-ref_tabname = 'SKAT'.
    w_fcat_v5-emphasize = 'C110'.
    w_fcat_v5-outputlen = '25'.
    w_fcat_v5-col_pos = '5'.
    APPEND w_fcat_v5 TO i_fcat_v5.
    CLEAR w_fcat_v5.

    w_fcat_v5-fieldname = 'TXT50'.
    w_fcat_v5-ref_tabname = 'SKAT'.
    w_fcat_v5-emphasize = 'C110'.
    w_fcat_v5-outputlen = '35'.
    w_fcat_v5-col_pos = '6'.
    APPEND w_fcat_v5 TO i_fcat_v5.
    CLEAR w_fcat_v5.

    w_fcat_v5-fieldname = 'KTOKS'.
    w_fcat_v5-ref_tabname = 'SKA1'.
    w_fcat_v5-emphasize = 'C110'.
    w_fcat_v5-outputlen = '10'.
    w_fcat_v5-col_pos = '7'.
    APPEND w_fcat_v5 TO i_fcat_v5.
    CLEAR w_fcat_v5.

    w_fcat_v5-fieldname = 'TXT30'.
*           W_FCAT_V5-REF_TABNAME = 'T077Z'.Account Group
    w_fcat_v5-seltext_l = 'Acc. Group Description'.
    w_fcat_v5-emphasize = 'C110'.
    w_fcat_v5-outputlen = '35'.
    w_fcat_v5-col_pos = '8'.
    APPEND w_fcat_v5 TO i_fcat_v5.
    CLEAR w_fcat_v5.

    w_fcat_v5-fieldname = 'XSPEA'.
    w_fcat_v5-ref_tabname = 'SKA1'.
    w_fcat_v5-emphasize = 'C110'.
    w_fcat_v5-outputlen = '4'.
    w_fcat_v5-col_pos = '9'.
    APPEND w_fcat_v5 TO i_fcat_v5.
    CLEAR w_fcat_v5.

    w_fcat_v5-fieldname = 'XSPEB'.
    w_fcat_v5-ref_tabname = 'SKA1'.
    w_fcat_v5-emphasize = 'C110'.
    w_fcat_v5-outputlen = '4'.
    w_fcat_v5-col_pos = '10'.
    APPEND w_fcat_v5 TO i_fcat_v5.
    CLEAR w_fcat_v5.

    w_fcat_v5-fieldname = 'XSPEP'.
    w_fcat_v5-ref_tabname = 'SKA1'.
    w_fcat_v5-emphasize = 'C110'.
    w_fcat_v5-outputlen = '4'.
    w_fcat_v5-col_pos = '11'.
    APPEND w_fcat_v5 TO i_fcat_v5.
    CLEAR w_fcat_v5.

    w_fcat_v5-fieldname = 'XBILK'.
    w_fcat_v5-ref_tabname = 'SKA1'.
    w_fcat_v5-emphasize = 'C110'.
    w_fcat_v5-outputlen = '4'.
    w_fcat_v5-col_pos = '12'.
    APPEND w_fcat_v5 TO i_fcat_v5.
    CLEAR w_fcat_v5.

    w_fcat_v5-fieldname = 'FSTAG'.
    w_fcat_v5-ref_tabname = 'SKB1'.
    w_fcat_v5-emphasize = 'C110'.
    w_fcat_v5-outputlen = '12'.
    w_fcat_v5-col_pos = '13'.
    APPEND w_fcat_v5 TO i_fcat_v5.
    CLEAR w_fcat_v5.

    w_fcat_v5-fieldname = 'MITKZ'.
    w_fcat_v5-ref_tabname = 'SKB1'.
    w_fcat_v5-emphasize = 'C110'.
    w_fcat_v5-outputlen = '4'.
    w_fcat_v5-col_pos = '14'.
    APPEND w_fcat_v5 TO i_fcat_v5.
    CLEAR w_fcat_v5.

    w_fcat_v5-fieldname = 'ERDAT'.
    w_fcat_v5-ref_tabname = 'SKA1'.
    w_fcat_v5-emphasize = 'C110'.
    w_fcat_v5-outputlen = '12'.
    w_fcat_v5-col_pos = '15'.
    APPEND w_fcat_v5 TO i_fcat_v5.
    CLEAR w_fcat_v5.

    w_fcat_v5-fieldname = 'ERNAM'.
*           W_FCAT_V5-seltext_s = 'GST NO'.
    w_fcat_v5-ref_tabname = 'SKA1'.
    w_fcat_v5-emphasize = 'C110'.
    w_fcat_v5-outputlen = '12'.
    w_fcat_v5-col_pos = '16'.
    APPEND w_fcat_v5 TO i_fcat_v5.
    CLEAR w_fcat_v5.

    w_fcat_v5-fieldname = 'G_ERDAT'.
    w_fcat_v5-seltext_l = 'GL with compay code created on'.
*           W_FCAT_V5-REF_TABNAME = 'SKA1'.
    w_fcat_v5-emphasize = 'C110'.
    w_fcat_v5-outputlen = '12'.
    w_fcat_v5-col_pos = '17'.
    APPEND w_fcat_v5 TO i_fcat_v5.
    CLEAR w_fcat_v5.

    w_fcat_v5-fieldname = 'G_ERNAM'.
    w_fcat_v5-seltext_l = 'GL with compay code created by'.
*           W_FCAT_V5-REF_TABNAME = 'SKA1'.
    w_fcat_v5-emphasize = 'C110'.
    w_fcat_v5-outputlen = '12'.
    w_fcat_v5-col_pos = '18'.
    APPEND w_fcat_v5 TO i_fcat_v5.
    CLEAR w_fcat_v5.

    DESCRIBE TABLE it_glfinal LINES v_lines.
    c_lines = v_lines.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program     = sy-repid
        i_grid_title           = 'GL MASTER LIST'
        i_callback_top_of_page = 'TOP_OF_PAGE'
        is_layout              = layout_v5
        it_fieldcat            = i_fcat_v5
      TABLES
        t_outtab               = it_glfinal.

  ELSEIF rb_5 = 'X'.
    IF rb1_1 IS NOT INITIAL AND rb1_2 IS INITIAL AND rb1_3 IS INITIAL.
      SELECT
      anla~bukrs anla~anln1 anla~anln2 anla~anlkl anla~txt50 anla~sernr anla~invnr anla~menge anla~meins anla~aktiv anla~zugdt
      anla~zujhr anla~zuper anla~lifnr anla~erdat anla~ernam anla~aenam anla~aedat anlz~kostl anlz~werks anlz~stort anlh~anlhtxt
        INTO TABLE it_fasset FROM anla JOIN anlz ON anla~anln1 = anlz~anln1
      JOIN anlh ON anlz~anln1 = anlh~anln1 WHERE anla~anln1 IN p_anln1 AND erdat IN p_erdat.
    ELSEIF rb1_1 IS INITIAL AND rb1_2 IS NOT INITIAL AND rb1_3 IS INITIAL.
      SELECT
      anla~bukrs anla~anln1 anla~anln2 anla~anlkl anla~txt50 anla~sernr anla~invnr anla~menge anla~meins anla~aktiv anla~zugdt
      anla~zujhr anla~zuper anla~lifnr anla~erdat anla~ernam anla~aenam anla~aedat anlz~kostl anlz~werks anlz~stort anlh~anlhtxt
        INTO TABLE it_fasset FROM anla JOIN anlz ON anla~anln1 = anlz~anln1
      JOIN anlh ON anlz~anln1 = anlh~anln1 WHERE anla~anln1 IN p_anln1 AND aktiv IN p_erdat.
    ELSEIF rb1_1 IS INITIAL AND rb1_2 IS INITIAL AND rb1_3 IS NOT INITIAL.
      SELECT
      anla~bukrs anla~anln1 anla~anln2 anla~anlkl anla~txt50 anla~sernr anla~invnr anla~menge anla~meins anla~aktiv anla~zugdt
      anla~zujhr anla~zuper anla~lifnr anla~erdat anla~ernam anla~aenam anla~aedat anlz~kostl anlz~werks anlz~stort anlh~anlhtxt
        INTO TABLE it_fasset FROM anla JOIN anlz ON anla~anln1 = anlz~anln1
      JOIN anlh ON anlz~anln1 = anlh~anln1 WHERE anla~anln1 IN p_anln1.
      "" and ZUJHR IN P_ZUJHR.
    ENDIF.


    LOOP AT it_fasset INTO wa_fasset.
      COLLECT wa_fasset INTO it_fasset_1.
    ENDLOOP.

    LOOP AT it_fasset_1 INTO wa_fasset_1.

      SELECT afabg
             afasl
      ndjar INTO TABLE it_anlb FROM anlb WHERE anln1 = wa_fasset_1-anln1 AND afabe = 1.
      LOOP AT it_anlb INTO wa_anlb.
        wa_fasset_1-afabg = wa_anlb-afabg.
        wa_fasset_1-afasl = wa_anlb-afasl.
        wa_fasset_1-ndjar = wa_anlb-ndjar.
      ENDLOOP.
      SELECT ltext FROM cskt INTO TABLE it_cskt WHERE kostl = wa_fasset_1-kostl.
      LOOP AT it_cskt INTO wa_cskt.
        wa_fasset_1-ltext = wa_cskt-ltext.
      ENDLOOP.
*BREAK ABAP.
      SELECT ndabj FROM anlc INTO TABLE it_anlc WHERE anln1 = wa_fasset_1-anln1 AND anln2 = wa_fasset_1-anln2.

      SORT it_anlc DESCENDING.
      READ TABLE it_anlc INTO wa_anlc INDEX 1.
      wa_fasset_1-ndabj = wa_anlc-ndabj.
      wa_fasset_1-buf    = wa_anlb-ndjar - wa_anlc-ndabj.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_fasset_1-anln1
        IMPORTING
          output = wa_fasset_1-anln1.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_fasset_1-anln2
        IMPORTING
          output = wa_fasset_1-anln2.

      CONCATENATE  wa_fasset_1-bukrs  wa_fasset_1-anln1 wa_fasset_1-anln2 INTO asset_a.

      SELECT DISTINCT instid_a catid_b FROM srgbtbrel INTO TABLE it_atta WHERE  instid_a = asset_a.

      IF sy-subrc = 0.
        wa_fasset_1-atta = 'Y'.
      ELSE.
        wa_fasset_1-atta = 'N'.
      ENDIF.


      MODIFY it_fasset_1 FROM wa_fasset_1 TRANSPORTING afabg afasl ndjar ltext ndabj buf atta WHERE anln1 = wa_fasset_1-anln1 AND anln2 = wa_fasset_1-anln2.
      CLEAR: it_atta, wa_atta, asset_a.
    ENDLOOP.

    DATA: w_fcat_m5 TYPE slis_fieldcat_alv,
          i_fcat_m5 TYPE slis_t_fieldcat_alv,
          layout_m5 TYPE slis_layout_alv.


    w_fcat_m5-fieldname = 'BUKRS'.
    w_fcat_m5-ref_tabname = 'ANLA'.
    w_fcat_m5-emphasize = 'C311'.
    w_fcat_m5-outputlen = '8'.
    w_fcat_m5-col_pos = '1'.
    APPEND w_fcat_m5 TO i_fcat_m5.
    CLEAR w_fcat_m5.

    w_fcat_m5-fieldname = 'ANLN1'.
    w_fcat_m5-ref_tabname = 'ANLA'.
    w_fcat_m5-emphasize = 'C311'.
    w_fcat_m5-outputlen = '8'.
    w_fcat_m5-col_pos = '2'.
    APPEND w_fcat_m5 TO i_fcat_m5.
    CLEAR w_fcat_m5.

    w_fcat_m5-fieldname = 'ANLN2'.
    w_fcat_m5-ref_tabname = 'ANLA'.
    w_fcat_m5-emphasize = 'C311'.
    w_fcat_m5-outputlen = '5'.
    w_fcat_m5-col_pos = '3'.
    APPEND w_fcat_m5 TO i_fcat_m5.
    CLEAR w_fcat_m5.

    w_fcat_m5-fieldname = 'ANLKL'.
    w_fcat_m5-ref_tabname = 'ANLA'.
    w_fcat_m5-emphasize = 'C311'.
    w_fcat_m5-outputlen = '8'.
    w_fcat_m5-col_pos = '4'.
    APPEND w_fcat_m5 TO i_fcat_m5.
    CLEAR w_fcat_m5.

    w_fcat_m5-fieldname = 'TXT50'.
    w_fcat_m5-ref_tabname = 'ANLA'.
    w_fcat_m5-emphasize = 'C110'.
    w_fcat_m5-outputlen = '40'.
    w_fcat_m5-col_pos = '5'.
    APPEND w_fcat_m5 TO i_fcat_m5.
    CLEAR w_fcat_m5.

    w_fcat_m5-fieldname = 'SERNR'.
    w_fcat_m5-ref_tabname = 'ANLA'.
    w_fcat_m5-emphasize = 'C110'.
    w_fcat_m5-outputlen = '12'.
    w_fcat_m5-col_pos = '6'.
    APPEND w_fcat_m5 TO i_fcat_m5.
    CLEAR w_fcat_m5.

    w_fcat_m5-fieldname = 'INVNR'.
    w_fcat_m5-ref_tabname = 'ANLA'.
    w_fcat_m5-emphasize = 'C110'.
    w_fcat_m5-outputlen = '12'.
    w_fcat_m5-col_pos = '7'.
    APPEND w_fcat_m5 TO i_fcat_m5.
    CLEAR w_fcat_m5.

    w_fcat_m5-fieldname = 'MENGE'.
    w_fcat_m5-ref_tabname = 'ANLA'.
    w_fcat_m5-emphasize = 'C110'.
    w_fcat_m5-outputlen = '8'.
    w_fcat_m5-col_pos = '8'.
    APPEND w_fcat_m5 TO i_fcat_m5.
    CLEAR w_fcat_m5.

    w_fcat_m5-fieldname = 'MEINS'.
    w_fcat_m5-ref_tabname = 'ANLA'.
    w_fcat_m5-emphasize = 'C110'.
    w_fcat_m5-outputlen = '4'.
    w_fcat_m5-col_pos = '9'.
    APPEND w_fcat_m5 TO i_fcat_m5.
    CLEAR w_fcat_m5.

    w_fcat_m5-fieldname = 'AKTIV'.
    w_fcat_m5-ref_tabname = 'ANLA'.
    w_fcat_m5-emphasize = 'C110'.
    w_fcat_m5-outputlen = '10'.
    w_fcat_m5-col_pos = '10'.
    APPEND w_fcat_m5 TO i_fcat_m5.
    CLEAR w_fcat_m5.

    w_fcat_m5-fieldname = 'ZUGDT'.
    w_fcat_m5-ref_tabname = 'ANLA'.
    w_fcat_m5-emphasize = 'C110'.
    w_fcat_m5-outputlen = '10'.
    w_fcat_m5-col_pos = '11'.
    APPEND w_fcat_m5 TO i_fcat_m5.
    CLEAR w_fcat_m5.

    w_fcat_m5-fieldname = 'ZUJHR'.
    w_fcat_m5-ref_tabname = 'ANLA'.
    w_fcat_m5-emphasize = 'C110'.
    w_fcat_m5-outputlen = '6'.
    w_fcat_m5-col_pos = '12'.
    APPEND w_fcat_m5 TO i_fcat_m5.
    CLEAR w_fcat_m5.

    w_fcat_m5-fieldname = 'ZUPER'.
    w_fcat_m5-ref_tabname = 'ANLA'.
    w_fcat_m5-emphasize = 'C110'.
    w_fcat_m5-outputlen = '4'.
    w_fcat_m5-col_pos = '13'.
    APPEND w_fcat_m5 TO i_fcat_m5.
    CLEAR w_fcat_m5.

    w_fcat_m5-fieldname = 'LIFNR'.   " Not Needed
    w_fcat_m5-ref_tabname = 'ANLA'.
    w_fcat_m5-emphasize = 'C110'.
    w_fcat_m5-outputlen = '8'.
    w_fcat_m5-col_pos = '14'.
    APPEND w_fcat_m5 TO i_fcat_m5.
    CLEAR w_fcat_m5.

    w_fcat_m5-fieldname = 'KOSTL'.     " Not Needed
    w_fcat_m5-ref_tabname = 'ANLZ'.
    w_fcat_m5-emphasize = 'C110'.
    w_fcat_m5-outputlen = '10'.
    w_fcat_m5-col_pos = '15'.
    APPEND w_fcat_m5 TO i_fcat_m5.
    CLEAR w_fcat_m5.

    w_fcat_m5-fieldname = 'LTEXT'.   " Not Needed
    w_fcat_m5-ref_tabname = 'CSKT'.
    w_fcat_m5-emphasize = 'C110'.
    w_fcat_m5-outputlen = '25'.
    w_fcat_m5-col_pos = '16'.
    APPEND w_fcat_m5 TO i_fcat_m5.
    CLEAR w_fcat_m5.

    w_fcat_m5-fieldname = 'WERKS'.   " Not Needed
    w_fcat_m5-ref_tabname = 'ANLZ'.
    w_fcat_m5-emphasize = 'C110'.
    w_fcat_m5-outputlen = '6'.
    w_fcat_m5-col_pos = '17'.
    APPEND w_fcat_m5 TO i_fcat_m5.
    CLEAR w_fcat_m5.

    w_fcat_m5-fieldname = 'ANLHTXT'. " Not needed
    w_fcat_m5-ref_tabname = 'ANLH'.
    w_fcat_m5-emphasize = 'C110'.
    w_fcat_m5-outputlen = '40'.
    w_fcat_m5-col_pos = '18'.
    APPEND w_fcat_m5 TO i_fcat_m5.
    CLEAR w_fcat_m5.

    w_fcat_m5-fieldname = 'AFABG'. " Not Needed
    w_fcat_m5-ref_tabname = 'ANLB'.
    w_fcat_m5-emphasize = 'C110'.
    w_fcat_m5-outputlen = '10'.
    w_fcat_m5-col_pos = '19'.
    APPEND w_fcat_m5 TO i_fcat_m5.
    CLEAR w_fcat_m5.

    w_fcat_m5-fieldname = 'AFASL'." Not Needed.
    w_fcat_m5-ref_tabname = 'ANLB'.
    w_fcat_m5-emphasize = 'C110'.
    w_fcat_m5-outputlen = '6'.
    w_fcat_m5-col_pos = '20'.
    APPEND w_fcat_m5 TO i_fcat_m5.
    CLEAR w_fcat_m5.

    w_fcat_m5-fieldname = 'NDJAR'.
    w_fcat_m5-ref_tabname = 'ANLB'.
    w_fcat_m5-emphasize = 'C110'.
    w_fcat_m5-outputlen = '6'.
    w_fcat_m5-col_pos = '21'.
    APPEND w_fcat_m5 TO i_fcat_m5.
    CLEAR w_fcat_m5.

    w_fcat_m5-fieldname = 'NDABJ'.
    w_fcat_m5-ref_tabname = 'ANLC'.
    w_fcat_m5-emphasize = 'C110'.
    w_fcat_m5-outputlen = '6'.
    w_fcat_m5-col_pos = '22'.
    APPEND w_fcat_m5 TO i_fcat_m5.
    CLEAR w_fcat_m5.

    w_fcat_m5-fieldname = 'ERNAM'.
    w_fcat_m5-ref_tabname = 'ANLA'.
    w_fcat_m5-emphasize = 'C110'.
    w_fcat_m5-outputlen = '10'.
    w_fcat_m5-col_pos = '23'.
    APPEND w_fcat_m5 TO i_fcat_m5.
    CLEAR w_fcat_m5.

    w_fcat_m5-fieldname = 'ERDAT'.
    w_fcat_m5-ref_tabname = 'ANLA'.
    w_fcat_m5-emphasize = 'C110'.
    w_fcat_m5-outputlen = '10'.
    w_fcat_m5-col_pos = '24'.
    APPEND w_fcat_m5 TO i_fcat_m5.
    CLEAR w_fcat_m5.

    w_fcat_m5-fieldname = 'AENAM'.
    w_fcat_m5-ref_tabname = 'ANLA'.
    w_fcat_m5-emphasize = 'C110'.
    w_fcat_m5-outputlen = '10'.
    w_fcat_m5-col_pos = '25'.
    APPEND w_fcat_m5 TO i_fcat_m5.
    CLEAR w_fcat_m5.

    w_fcat_m5-fieldname = 'AEDAT'.
    w_fcat_m5-ref_tabname = 'ANLA'.
    w_fcat_m5-emphasize = 'C110'.
    w_fcat_m5-outputlen = '10'.
    w_fcat_m5-col_pos = '26'.
    APPEND w_fcat_m5 TO i_fcat_m5.
    CLEAR w_fcat_m5.

    w_fcat_m5-fieldname = 'BUF'.
*           W_FCAT_M5-REF_TABNAME = 'ANLA'.
    w_fcat_m5-seltext_l = 'Remaining Useful Life'.
    w_fcat_m5-emphasize = 'C110'.
    w_fcat_m5-outputlen = '10'.
    w_fcat_m5-col_pos = '27'.
    APPEND w_fcat_m5 TO i_fcat_m5.
    CLEAR w_fcat_m5.
*           ANLZ~STORT
    w_fcat_m5-fieldname = 'STORT'.
    w_fcat_m5-ref_tabname = 'ANLZ'.
    w_fcat_m5-emphasize = 'C110'.
    w_fcat_m5-outputlen = '10'.
    w_fcat_m5-col_pos = '28'.
    APPEND w_fcat_m5 TO i_fcat_m5.
    CLEAR w_fcat_m5.

    w_fcat_m5-fieldname = 'ATTA'.
*           W_FCAT_M5-REF_TABNAME = 'ANLZ'.
    w_fcat_m5-seltext_l = 'Attachment'.
    w_fcat_m5-emphasize = 'C110'.
    w_fcat_m5-outputlen = '10'.
    w_fcat_m5-col_pos = '29'.
    w_fcat_m5-hotspot = 'X'.
    APPEND w_fcat_m5 TO i_fcat_m5.
    CLEAR w_fcat_m5.

    DESCRIBE TABLE it_fasset_1 LINES v_lines.
    c_lines = v_lines.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_grid_title            = 'ASSET MASTER LIST'
        i_callback_top_of_page  = 'TOP_OF_PAGE'
        i_callback_program      = sy-repid
        i_callback_user_command = gt_callback_subroutine
        is_layout               = layout_m4
        it_fieldcat             = i_fcat_m5
      TABLES
        t_outtab                = it_fasset_1.
  ENDIF.



FORM user_command  USING p_ucomm    LIKE sy-ucomm
                         p_selfield TYPE slis_selfield.
  BREAK abap.
  "p_ucomm will hold user action like double click, clicking a button ,etc
  CASE p_ucomm.
    WHEN '&IC1'.
      DATA: ls_object    TYPE sibflporb,
            save_request TYPE sgs_flag.     " SAP standard code for double-clicking
      CLEAR: wa_final_vendor, wa_fasset_1.
      IF p_selfield-fieldname = 'ATTA'.

        IF it_final_vendor IS NOT INITIAL.
          READ TABLE it_final_vendor INTO wa_final_vendor INDEX p_selfield-tabindex.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = wa_final_vendor-lifnr
            IMPORTING
              output = vendor_a.
          SELECT DISTINCT instid_a catid_b FROM srgbtbrel INTO TABLE it_atta WHERE  instid_a = vendor_a.

          READ TABLE it_atta INTO wa_atta INDEX 1.


          ls_object-instid = wa_atta-instid_a.
          ls_object-typeid = 'LFA1'.
          ls_object-catid = 'BO'.
          CALL FUNCTION 'GOS_ATTACHMENT_LIST_POPUP'
            EXPORTING
              is_object       = ls_object
              ip_mode         = 'D' " Display mode
            IMPORTING
              ep_save_request = save_request.
          IF save_request = 'X'.
            COMMIT WORK.
          ENDIF.

          CLEAR : vendor_a.
        ELSEIF it_fasset_1 IS NOT INITIAL.

          READ TABLE it_fasset_1 INTO wa_fasset_1 INDEX p_selfield-tabindex.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = wa_fasset_1-anln1
            IMPORTING
              output = wa_fasset_1-anln1.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = wa_fasset_1-anln2
            IMPORTING
              output = wa_fasset_1-anln2.

          CONCATENATE  wa_fasset_1-bukrs  wa_fasset_1-anln1 wa_fasset_1-anln2 INTO asset_a.

          SELECT DISTINCT instid_a catid_b FROM srgbtbrel INTO TABLE it_atta WHERE  instid_a = asset_a.

          READ TABLE it_atta INTO wa_atta INDEX 1.

          ls_object-instid = wa_atta-instid_a.
          ls_object-typeid = 'BUS1022'.
          ls_object-catid = 'BO'.
          CALL FUNCTION 'GOS_ATTACHMENT_LIST_POPUP'
            EXPORTING
              is_object       = ls_object
              ip_mode         = 'D' " Display mode
            IMPORTING
              ep_save_request = save_request.
          IF save_request = 'X'.
            COMMIT WORK.
          ENDIF.

          CLEAR : asset_a.

        ENDIF.
      ENDIF.
  ENDCASE.


ENDFORM.
*********************************
FORM top_of_page.
  CLEAR: wa_header,t_header[].
  wa_header-typ = 'H'.
  CONCATENATE 'KARAIKAL PORT PRIVATE LIMITED'
  '' INTO wa_header-info.
  APPEND wa_header TO t_header.
  CLEAR wa_header.
* wa_header-typ = 'S'.
* wa_header-info = 'MASTERS REPORT'.
* APPEND wa_header to t_header.
* CLEAR wa_header.
  wa_header-typ = 'S'.
  DATA: sp TYPE string,
        da TYPE string.
  CONCATENATE  sy-datum+6(2) '.'
  sy-datum+4(2) '.'
  sy-datum(4) INTO da.

  CONCATENATE 'Report Date'  ':' INTO sp SEPARATED BY space(1).

  CONCATENATE sp da
* CONCATENATE sp  SY-DATUM+6(2) '.'
* SY-DATUM+4(2) '.'
* SY-DATUM(4)
  INTO wa_header-info SEPARATED BY ' '.
  APPEND wa_header TO t_header .
  CLEAR wa_header.

  wa_header-typ = 'S'.
  wa_header-info = ' '.
  APPEND wa_header TO t_header.
  CLEAR wa_header.

  wa_header-typ = 'S'.
  CONCATENATE 'Count' space ':'
  c_lines INTO wa_header-info SEPARATED BY ' '.
  APPEND wa_header TO t_header.
  CLEAR wa_header.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = t_header.
ENDFORM.
********************************
