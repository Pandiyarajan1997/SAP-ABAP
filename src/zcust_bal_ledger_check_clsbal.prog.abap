**&---------------------PSKGDT------------------------------------------*
**& Report  ZCUST_BAL_LEDGER_PRINT3
*Created By: Pandiarajan
*Created On: 16.04.2024
*Purpose : get the retailer opening & closing balance
*---------------------------------------------------------------*
REPORT zcust_bal_ledger_check_clsbal.


TABLES:bsid.
TABLES:kna1.
TABLES:zmail_id.

TYPES: BEGIN OF gs_kna1,
         kunnr TYPE kna1-kunnr,                  " Customer Number
         name1 TYPE kna1-name1,                  " Customer Name
         adrnr TYPE kna1-adrnr,                  " Address Number
         lifnr TYPE kna1-lifnr,                  " Vendor Code
       END OF gs_kna1.

TYPES: BEGIN OF ty_output,
         distributor TYPE zdist,
         dist_name   TYPE name1,
         werks       TYPE werks_d,
         dealer      TYPE kunnr,
         dealer_name TYPE name1,
         st_date     TYPE sy-datum,
         ed_date     TYPE sy-datum,
         lv_opbal    TYPE bsad-dmbtr,
         lv_clbal    TYPE bsad-dmbtr,
         doc_type    TYPE blart,
       END OF ty_output.

DATA: gt_output TYPE TABLE OF ty_output,
      gs_output TYPE ty_output.

DATA: gt_kna1 TYPE TABLE OF gs_kna1,
      wa_kna1 TYPE gs_kna1.
TYPES : BEGIN OF gs_t003t,
          blart TYPE t003t-blart,
          ltext TYPE t003t-ltext,
          spras TYPE t003t-spras,
        END OF gs_t003t.

DATA : gt_t003t TYPE  TABLE OF gs_t003t,
       wa_t003t TYPE gs_t003t.

DATA : gt_t003t1 TYPE  TABLE OF gs_t003t,
       wa_t003t1 TYPE gs_t003t.

DATA : gt_t003t2 TYPE  TABLE OF gs_t003t,
       wa_t003t2 TYPE gs_t003t.

TYPES: BEGIN OF gs_bsid,
         prctr TYPE bsid-prctr,                  " Profit Center
         kunnr TYPE bsid-kunnr,                  " Customer Number
         augbl TYPE bsid-augbl,
         budat TYPE bsid-budat,                  " Posting Date in the Document
         bldat TYPE bsid-bldat,
         blart TYPE bsid-blart,
         bschl TYPE bsid-bschl,                  "Posting key
         zfbdt TYPE bsid-zfbdt,                  " Baseline Date for Due Date Calculation
         dmbtr TYPE bsid-dmbtr,                  " Amount in Local Currency
         shkzg TYPE bsid-shkzg,                  " Debit/Credit Indicator
         xblnr TYPE bsid-xblnr,                  " Reference Document Number
         belnr TYPE bsid-belnr,                  " Bill Number
         gsber TYPE bsid-gsber,                  " Business Area
         sgtxt TYPE bsid-sgtxt,                  " Text Or Remarks
         vbeln TYPE bsid-vbeln,
         umskz TYPE bsid-umskz,
         bukrs TYPE bsid-bukrs,
         gjahr TYPE bsid-gjahr,
         sknto TYPE bsad-sknto,
         flag  TYPE char1,
       END OF gs_bsid.

DATA: gt_bsid  TYPE TABLE OF gs_bsid,
      gt_bsid1 TYPE TABLE OF gs_bsid,
      gtt_bsid TYPE TABLE OF gs_bsid,
      wa_bsid  TYPE gs_bsid,
      wat_bsid TYPE gs_bsid,
      wa_bsid1 TYPE gs_bsid.

DATA:dbt_amt TYPE bsid-dmbtr.

TYPES : BEGIN OF gs_bsik,

          prctr TYPE bsik-prctr ,        "Profit Center
          kunnr TYPE bsik-lifnr,         "Vendor
          augbl TYPE bsik-augbl,         "Clrng doc.
          budat TYPE bsik-budat,         "Posting Date
          bldat TYPE bsik-bldat,         "Document Date
          blart TYPE bsik-blart,         "Document Type
          zfbdt TYPE bsik-zfbdt,         "Baseline Date
          dmbtr TYPE bsik-dmbtr,         "Amount in LC
          shkzg TYPE bsik-shkzg,         "Debit/Credit
          xblnr TYPE bsik-xblnr,         "Reference
          belnr TYPE bsik-belnr,         "Document Number
          gsber TYPE bsik-gsber,         "Business Area
          sgtxt TYPE bsik-sgtxt,         "Text
          vbeln TYPE bsik-ebeln,         "Purchasing Doc.
          umskz TYPE bsik-umskz,         "Special G/L ind
          bukrs TYPE bsik-bukrs,         "Company Code
          gjahr TYPE bsik-gjahr,         "Fiscal Year
        END OF gs_bsik.


DATA: gt_bsik TYPE TABLE OF gs_bsik,
      wa_bsik TYPE gs_bsik.
DATA: gt_bsik1 TYPE TABLE OF gs_bsik,
      wa_bsik1 TYPE gs_bsik.

DATA: gt_bsik2 TYPE TABLE OF gs_bsik,
      wa_bsik2 TYPE gs_bsik.

DATA: gt_bsik3 TYPE TABLE OF gs_bsik,
      wa_bsik3 TYPE gs_bsik.

TYPES : BEGIN OF gs_bsak,
          bukrs TYPE bsak-bukrs,
          lifnr TYPE bsak-lifnr,
          umsks TYPE bsak-umsks,
          umskz TYPE bsak-umskz,
          augdt TYPE bsak-augdt,
          augbl TYPE bsak-augbl,
          zuonr TYPE bsak-zuonr,
          gjahr TYPE bsak-gjahr,
          belnr TYPE bsak-belnr,
          buzei TYPE bsak-buzei,
          budat TYPE bsak-budat,
          bldat TYPE bsak-bldat,
          xblnr TYPE bsak-xblnr,
          blart TYPE bsak-blart,
          dmbtr TYPE bsak-dmbtr,
          bschl TYPE bsak-bschl,
          sgtxt TYPE bsak-sgtxt,
          gsber TYPE bsak-gsber,
          shkzg TYPE bsak-shkzg,
          sknto TYPE bsak-sknto,
        END OF gs_bsak.


DATA: gt_bsak TYPE TABLE OF gs_bsak,
      wa_bsak TYPE gs_bsak.

DATA: gt_bsak1 TYPE TABLE OF gs_bsak,
      wa_bsak1 TYPE gs_bsak.
DATA: gt_bsak2 TYPE TABLE OF gs_bsak,
      wa_bsak2 TYPE gs_bsak.

DATA: gt_bsak3 TYPE TABLE OF gs_bsak,
      wa_bsak3 TYPE gs_bsak.

DATA: lv_ind TYPE i.

TYPES: BEGIN OF gs_bsad,
         prctr TYPE bsad-prctr,                  " Profit Center
         kunnr TYPE bsad-kunnr,                  " Customer Number
         augbl TYPE bsad-augbl,
         budat TYPE bsad-budat,                  " Posting Date in the Document
         bldat TYPE bsad-bldat,
         blart TYPE bsad-blart,
         bschl TYPE bsad-bschl,                  " "Posting key
         zfbdt TYPE bsad-zfbdt,                  " Baseline Date for Due Date Calculation
         dmbtr TYPE bsad-dmbtr,                  " Amount in Local Currency
         shkzg TYPE bsad-shkzg,                  " Debit/Credit Indicator
         xblnr TYPE bsad-xblnr,                  " Reference Document Number
         belnr TYPE bsad-belnr,                  " Bill Number
         gsber TYPE bsad-gsber,                  " Business Area
         sgtxt TYPE bsad-sgtxt,                  " Text OR Remarks
         vbeln TYPE bsad-vbeln,                  " Billing Doc.No
         umskz TYPE bsad-umskz,                  " Spl.Gl.Ind
         bukrs TYPE bsad-bukrs,
         gjahr TYPE bsad-gjahr,
         sknto TYPE bsad-sknto,
         umsks TYPE bsad-umsks,
         augdt TYPE bsad-augdt,
         zuonr TYPE bsad-zuonr,
         buzei TYPE bsad-buzei,
         flag  TYPE char1,
       END OF gs_bsad.

DATA: gt_bsad  TYPE TABLE OF gs_bsad,
      wa_bsad  TYPE gs_bsad,
      wa_bsad1 TYPE gs_bsad,
      gt_bsad1 TYPE TABLE OF gs_bsad,
      gt_bsad2 TYPE TABLE OF gs_bsad.

DATA: gt_bsad7 TYPE TABLE OF gs_bsad,
      wa_bsad7 TYPE gs_bsad.

TYPES: BEGIN OF gs_adrc,
         addrnumber TYPE adrc-addrnumber,        " Address Number
         street     TYPE adrc-street,                " Street
         city1      TYPE adrc-city1,                  " District
         city2      TYPE adrc-city2,                  " City
         post_code1 TYPE adrc-post_code1,        " Postal Code
       END OF gs_adrc.

DATA: gt_adrc TYPE TABLE OF gs_adrc WITH HEADER LINE,
      wa_adrc TYPE gs_adrc.

TYPES : BEGIN OF ty_chk,
          belnr  TYPE bsad-belnr,                  " Bill Number
          budat  TYPE bsid-budat,                 " Posting Date
          blart  TYPE bsid-blart,                  " Bill Doc. Type
          cr_amt TYPE bsad-dmbtr,                 " Cr Amount
          dr_amt TYPE bsad-dmbtr,                 " Dr Amount
          shkzg  TYPE bsid-shkzg,
        END OF ty_chk.

TYPES: BEGIN OF gs_final,
         kunnr    TYPE bsad-kunnr,                  " Customer Number
         umskz    TYPE bsid-umskz,                  " Spl.GL Ind
         belnr    TYPE bsad-belnr,                  " Bill Number
         vbeln    TYPE bsad-vbeln,                  " Billing Document Number
         xblnr    TYPE bsid-xblnr,                  " Reference Document Number
         bldat    TYPE bsad-bldat,                  " Posting Date in the Document
         cr_amt   TYPE bsad-dmbtr,                 " Cr Amount
         dr_amt   TYPE bsad-dmbtr,                 " Dr Amount
         budat    TYPE bsid-budat,                 " Posting Date
         name1    TYPE kna1-name1,                 " Customer Name
         blart    TYPE bsid-blart,                  " Bill Doc. Type
         bschl    TYPE bsid-bschl,                  "Posting key
         sgtxt    TYPE bsid-sgtxt,                  " Text OR Remarks
         gsber    TYPE bsid-gsber,                  " Business Area
         shkzg    TYPE bsid-shkzg,                  " Dbit/Crdit ind.
         bal_amt  TYPE bsad-dmbtr,                " Balance Amount
         ltext    TYPE t003t-ltext,                  " Bill Doc .Type Descrption
         bukrs    TYPE bsid-bukrs,
         gjahr    TYPE bsid-gjahr,
         sknto    TYPE bsad-sknto,
         lv_count TYPE int1,
         sta      TYPE int1,
         t_tttext TYPE char20,                  " blart text field
       END OF gs_final.

DATA: t_tttext  TYPE char20,                  " blart text field
      lvv_belnr TYPE bsid-belnr,
      lvv_blart TYPE bsid-blart,
      lv_flag2  TYPE char1.

DATA: gt_final   TYPE TABLE OF gs_final WITH HEADER LINE,
      wa_final   TYPE gs_final,
      gt_final1  TYPE TABLE OF gs_final WITH HEADER LINE,
      wa_final1  TYPE gs_final,
      gt_final2  TYPE TABLE OF gs_final WITH HEADER LINE,
      wa_final2  TYPE gs_final,
      gt_final3  TYPE TABLE OF gs_final WITH HEADER LINE,
      wa_final3  TYPE gs_final,
      gt_final4  TYPE TABLE OF gs_final WITH HEADER LINE,
      wa_final4  TYPE gs_final,
      gt_final_t TYPE TABLE OF gs_final WITH HEADER LINE,
      wa_final_t TYPE gs_final,
      itt_final  TYPE TABLE OF gs_final,
      wat_final  TYPE gs_final,
      itt_final1 TYPE TABLE OF gs_final,
      wat_final1 TYPE gs_final,
      itt_final2 TYPE TABLE OF gs_final,
      wat_final3 TYPE gs_final,
      gs_chk     TYPE ty_chk.

TYPES: BEGIN OF gs_faglflexa,
         docnr TYPE faglflexa-docnr,
         prctr TYPE faglflexa-prctr,
         bschl TYPE faglflexa-bschl,
       END OF gs_faglflexa.

DATA: gt_faglflexa TYPE TABLE OF gs_faglflexa,
      wa_faglflexa TYPE gs_faglflexa.

TYPES : BEGIN OF gs_knc1,
          kunnr TYPE knc1-kunnr,
          bukrs TYPE knc1-bukrs,
          umsav TYPE knc1-umsav,
          gjahr TYPE knc1-gjahr,
        END OF gs_knc1.

DATA: gt_knc1 TYPE TABLE OF gs_knc1,
      wa_knc1 TYPE gs_knc1.

TYPES : BEGIN OF gs_lfa1,
          lifnr TYPE lfa1-lifnr,
          kunnr TYPE lfa1-kunnr,
        END OF gs_lfa1.

DATA : gt_lfa1 TYPE TABLE OF gs_lfa1,
       wa_lfa1 TYPE gs_lfa1.

DATA: or_budat TYPE  bsad-budat,
      or_bukrs TYPE bsid-bukrs,
      or_kunnr TYPE kna1-kunnr,
      fm_name  TYPE rs38l_fnam,
      or_prctr TYPE cepc-prctr,
      or_umskz TYPE bsid-umskz.

DATA: l_kunnr TYPE kna1-kunnr,
      l_prctr TYPE cepc-prctr.

DATA: b_date TYPE sy-datum.

DATA: lv_opn   TYPE bsad-dmbtr,
      rv_opn   TYPE bsad-dmbtr,
      lv_total TYPE bsad-dmbtr,
      lv_trans TYPE bsad-dmbtr,
      lv_date  TYPE sy-datum,
      lv_frdat TYPE sy-datum,
      lv_todat TYPE sy-datum,
      lv_sum   TYPE bsad-dmbtr,
      lv_buk   TYPE t001-bukrs,
      lv_cramt TYPE bsad-dmbtr,
      lv_dbamt TYPE bsad-dmbtr,
      lv_cdisc TYPE bsad-dmbtr.

************************************ PR@$@TH

DATA: lv_ind1 TYPE sy-tabix,
      lv_ind2 TYPE sy-tabix,
      lv_ind3 TYPE sy-tabix,
      lv_ind4 TYPE sy-tabix,
      lv_inc  TYPE shkzg,
      lv_crdr TYPE shkzg.

DATA: it_email TYPE TABLE OF zmail_id,
      wa_email TYPE zmail_id.

TYPES: BEGIN OF tem_kna1,
         kunnr TYPE kna1-kunnr,
         name1 TYPE kna1-name1,
         adrnr TYPE kna1-adrnr,
       END OF tem_kna1.
DATA: z_kna1 TYPE TABLE OF tem_kna1,
      y_kna1 TYPE tem_kna1.

TYPES: BEGIN OF tem_knb1,
         kunnr TYPE knb1-kunnr,
         bukrs TYPE knb1-bukrs,
         name1 TYPE kna1-name1,
       END OF tem_knb1.

DATA: z_knb1 TYPE TABLE OF tem_knb1,
      y_knb1 TYPE tem_knb1.

DATA: i_otf       TYPE itcoo    OCCURS 0 WITH HEADER LINE,
      i_tline     LIKE tline    OCCURS 0 WITH HEADER LINE,
      i_record    LIKE solisti1 OCCURS 0 WITH HEADER LINE,
      i_xstring   TYPE xstring,
* Objects to send mail.
      i_objpack   LIKE sopcklsti1 OCCURS 0 WITH HEADER LINE,
      i_objtxt    LIKE solisti1   OCCURS 0 WITH HEADER LINE,
      i_objbin    LIKE solix      OCCURS 0 WITH HEADER LINE,
      i_objbin1   LIKE solix      OCCURS 0 WITH HEADER LINE,
      i_objbin_f  LIKE solix      OCCURS 0 WITH HEADER LINE,
      i_reclist   LIKE somlreci1  OCCURS 0 WITH HEADER LINE,
* Work Area declarations
      wa_objhead  TYPE soli_tab,
      w_ctrlop    TYPE ssfctrlop,
      w_compop    TYPE ssfcompop,
      w_return    TYPE ssfcrescl,
      wa_buffer   TYPE string,
* Variables declarations
      v_form_name TYPE rs38l_fnam,
      v_len_in    LIKE sood-objlen.
DATA: s_email TYPE  adr6-smtp_addr.
*DATA: S_EMAIL1 TYPE  ADR6-SMTP_ADDR VALUE 'prasath@sphinaxinfosystems.com'.

DATA: ascii_data   TYPE solisti1,   "your data returned by SO_DOCUMENT_READ_API1
      string_data  TYPE string,
      xstring_data TYPE xstring.

DATA: sm_name TYPE kna1-name1.
DATA: count TYPE i VALUE 1 .

TYPES: BEGIN OF ty_email_del,
         stats TYPE i,
         kunnr TYPE kna1-kunnr,
         name1 TYPE kna1-name1,
         email TYPE adr6-smtp_addr,
         o_bal TYPE bsad-dmbtr,
       END OF ty_email_del.

DATA: it_sts TYPE TABLE OF ty_email_del,
      wa_sts TYPE ty_email_del.

DATA: sts TYPE kna1-kunnr.
DATA: o_bal TYPE bsad-dmbtr.
DATA: cur_dat TYPE sy-datum.
DATA: day TYPE i.
DATA: f_d TYPE char8,
      t_d TYPE char8.


TYPES: BEGIN OF ty_budat,
         low  TYPE bsid-budat,
         high TYPE bsid-budat,
       END OF ty_budat.

TYPES: BEGIN OF ty_email_char,
         stats(1)   TYPE c,
         kunnr(10)  TYPE c,
         name1(35)  TYPE c,
         email(241) TYPE c,
         o_bal(15)  TYPE c,
       END OF ty_email_char.

DATA: wa_sts_char TYPE ty_email_char.

DATA:   it_message TYPE STANDARD TABLE OF solisti1 INITIAL SIZE 0
              WITH HEADER LINE.
DATA:   it_attach TYPE STANDARD TABLE OF solisti1 INITIAL SIZE 0
              WITH HEADER LINE.

CONSTANTS: con_tab  TYPE c VALUE cl_abap_char_utilities=>horizontal_tab,
           con_cret TYPE c VALUE cl_abap_char_utilities=>cr_lf.

DATA: t_packing_list  LIKE sopcklsti1 OCCURS 0 WITH HEADER LINE,
      t_contents      LIKE solisti1 OCCURS 0 WITH HEADER LINE,
      t_receivers     LIKE somlreci1 OCCURS 0 WITH HEADER LINE,
      t_attachment    LIKE solisti1 OCCURS 0 WITH HEADER LINE,
      t_object_header LIKE solisti1 OCCURS 0 WITH HEADER LINE,
      w_cnt           TYPE i,
      w_sent_all(1)   TYPE c,
      w_doc_data      LIKE sodocchgi1,
      gd_error        TYPE sy-subrc,
      gd_reciever     TYPE sy-subrc.

DATA: p_email   TYPE somlreci1-receiver VALUE 'ramachandran@sphinaxinfosystems.com'.
DATA: tot TYPE int2.
DATA: tot_c(6) TYPE c.

********************************* PR@$@TH

SELECTION-SCREEN: BEGIN OF BLOCK email_rep WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS : so_dist   FOR kna1-kunnr.           "distributor code
  SELECT-OPTIONS: so_kunnr FOR kna1-kunnr.
  PARAMETERS: so_bukrs LIKE knb1-bukrs.
  SELECT-OPTIONS: so_budat FOR bsid-budat.
SELECTION-SCREEN: END OF BLOCK email_rep.

INITIALIZATION.
  so_bukrs = 'DMS1' .
  cur_dat = sy-datum.
  DATA: id_par_days	 TYPE t009b-butag,
        id_par_month TYPE t009b-bumon,
        id_par_year	 TYPE t009b-bdatj.

  id_par_month =  cur_dat+4(2) .
  id_par_year = cur_dat(4) .

  IF cur_dat+4(2) = 01 .
    id_par_year = id_par_year - 1 .

    CONCATENATE  id_par_year'1201' INTO so_budat-low .
    CONCATENATE id_par_year'1231' INTO so_budat-high .
  ELSE.
    id_par_month = id_par_month - 1 .

    CALL FUNCTION 'NUMBER_OF_DAYS_PER_MONTH_GET'
      EXPORTING
        par_month = id_par_month
        par_year  = id_par_year
      IMPORTING
        par_days  = id_par_days.

    CONCATENATE  id_par_year id_par_month '01' INTO so_budat-low .
    CONCATENATE id_par_year id_par_month id_par_days INTO so_budat-high .
  ENDIF.
  APPEND: so_budat.


START-OF-SELECTION.
  PERFORM: fetchdata.
*  PERFORM: STATUS_EMAIL.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  FETCHDATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fetchdata .
  DATA: lr_kunnr TYPE RANGE OF kna1-kunnr.

  TYPES: BEGIN OF ty_customers,
           kunnr TYPE kunnr,
         END OF ty_customers.

  DATA:  lt_kunnr TYPE TABLE OF ty_customers.

  DATA : lv_bsad  TYPE sy-tabix,
         lv_bsid  TYPE sy-tabix,
         lv_bsad1 TYPE sy-tabix,
         lv_bsid1 TYPE sy-tabix.
* SELECT kunnr FROM ZCUS_EXCL_STMNT INTO TABLE @DATA(lt_kunnr).
  "write the column carrid of your internal table in your range
***************fetch only DMS distributor***************
  SELECT bwkey,bukrs FROM t001k INTO TABLE @DATA(lt_t001k) WHERE bukrs = 'DMS1'.
  IF sy-subrc = 0.
***************get the business area for the distributor**************
    SELECT kunnr,werks,name1 FROM kna1 INTO TABLE @DATA(lt_distributor)
                             FOR ALL ENTRIES IN @lt_t001k
                             WHERE kunnr IN @so_dist
                             AND   werks EQ @lt_t001k-bwkey.
  ENDIF.

  CLEAR: gt_t003t.
  SELECT blart ltext spras FROM  t003t "#EC CI_NOORDER  "Added by SPLABAP during code remediation
    INTO TABLE gt_t003t
    WHERE spras = 'E'.

  LOOP AT lt_distributor INTO DATA(ls_distributor).
*BSID
    REFRESH lt_kunnr.
    SELECT a~kunnr
           FROM bsad AS a INNER JOIN faglflexa AS b
           ON  a~gjahr = b~ryear
           AND a~belnr = b~docnr
           INTO TABLE lt_kunnr
           WHERE a~budat <= so_budat-high
           AND a~budat >= so_budat-low
           AND a~bukrs EQ 'DMS1'
*         AND a~kunnr EQ y_knb1-kunnr
           AND a~umskz <> 'H'
           AND b~rldnr  = '0L'
           AND b~rbukrs = 'DMS1'
           AND b~docln  = '000001'
           AND b~rbusa  = ls_distributor-werks.

*BSAD
    SELECT a~kunnr
           FROM bsid AS a INNER JOIN faglflexa AS b
           ON  a~gjahr = b~ryear
           AND a~belnr = b~docnr
           APPENDING TABLE lt_kunnr
           WHERE a~budat <= so_budat-high
           AND a~budat >= so_budat-low
           AND a~bukrs EQ 'DMS1'
*         AND a~kunnr EQ y_knb1-kunnr
           AND a~umskz <> 'H'
           AND b~rldnr  = '0L'
           AND b~rbukrs = 'DMS1'
           AND b~docln  = '000001'
           AND b~rbusa  = ls_distributor-werks.

    SORT lt_kunnr.
    DELETE ADJACENT DUPLICATES FROM lt_kunnr.

    REFRESH lr_kunnr.
    lr_kunnr = VALUE #( FOR <fs_kunnr> IN lt_kunnr
                       (
                         sign = 'I'
                         option = 'EQ'
                         low = <fs_kunnr>-kunnr
                         high = ''
                        ) ).

    REFRESH: z_knb1.
    IF so_kunnr IS INITIAL.
      IF lr_kunnr[] IS NOT INITIAL.
        SELECT a~kunnr , a~bukrs, b~name1 FROM knb1 AS a
             INNER JOIN kna1 AS b ON a~kunnr = b~kunnr
             INTO TABLE @z_knb1 WHERE a~bukrs EQ 'DMS1' AND a~kunnr IN @lr_kunnr .
      ENDIF.
    ELSE.
      SELECT a~kunnr , a~bukrs, b~name1 FROM knb1 AS a
             INNER JOIN kna1 AS b ON a~kunnr = b~kunnr
             INTO TABLE @z_knb1 WHERE a~bukrs EQ 'DMS1' AND a~kunnr IN @so_kunnr.
    ENDIF.

    SORT z_knb1 BY kunnr.

    IF z_knb1 IS NOT INITIAL.

      LOOP AT z_knb1 INTO y_knb1.

*        CLEAR: lv_buk.
*        SELECT SINGLE bukrs  FROM t001
*          INTO lv_buk
*          WHERE bukrs EQ y_knb1-bukrs.
**********************BSAD*******************
        REFRESH : gt_bsad.

        SELECT a~prctr  a~kunnr a~umskz a~augbl a~budat a~bldat a~blart
               a~bschl  a~zfbdt a~dmbtr a~shkzg a~xblnr a~belnr b~rbusa a~sgtxt
               a~vbeln a~bukrs a~gjahr a~sknto a~umsks a~augdt a~zuonr a~buzei
               FROM bsad AS a INNER JOIN faglflexa AS b
               ON  a~gjahr = b~ryear
               AND a~belnr = b~docnr
               INTO CORRESPONDING FIELDS OF TABLE gt_bsad
               WHERE a~budat <= so_budat-high
               AND a~budat >= so_budat-low
               AND a~bukrs EQ 'DMS1'
               AND a~kunnr EQ y_knb1-kunnr
               AND a~umskz <> 'H'
               AND b~rldnr  = '0L'
               AND b~rbukrs = y_knb1-bukrs
               AND b~docln  = '000001'
               AND b~rbusa  = ls_distributor-werks.

**********************BSID*******************
        REFRESH: gt_bsid.
        SELECT a~prctr  a~kunnr a~augbl a~budat a~bldat a~blart
               a~bschl  a~zfbdt a~dmbtr a~shkzg a~xblnr a~belnr b~rbusa a~sgtxt
               a~vbeln  a~umskz a~bukrs a~gjahr a~sknto
               FROM bsid AS a INNER JOIN faglflexa AS b
               ON  a~gjahr = b~ryear
               AND a~belnr = b~docnr
               INTO CORRESPONDING FIELDS OF TABLE gt_bsid
               WHERE a~budat <= so_budat-high
               AND a~budat >= so_budat-low
               AND a~bukrs EQ 'DMS1'
               AND a~kunnr EQ y_knb1-kunnr
               AND a~umskz <> 'H'
               AND b~rldnr  = '0L'
               AND b~rbukrs = y_knb1-bukrs
               AND b~docln  = '000001'
               AND b~rbusa  = ls_distributor-werks.

******************************BSAD 2 times*************************

        REFRESH:gt_bsad1.
        SELECT a~prctr  a~kunnr a~umskz a~augbl a~budat a~bldat a~blart
               a~bschl  a~zfbdt a~dmbtr a~shkzg a~xblnr a~belnr b~rbusa a~sgtxt
               a~vbeln a~bukrs a~gjahr a~sknto a~umsks a~augdt a~zuonr a~buzei
               FROM bsad AS a INNER JOIN faglflexa AS b
               ON  a~gjahr = b~ryear
               AND a~belnr = b~docnr
               INTO CORRESPONDING FIELDS OF TABLE gt_bsad1
               WHERE a~budat BETWEEN '01.04.2023'
               AND   so_budat-low
               AND   a~bukrs EQ y_knb1-bukrs
               AND   a~kunnr EQ y_knb1-kunnr
               AND   a~umskz <> 'H'
               AND   b~rldnr  = '0L'
               AND   b~rbukrs = y_knb1-bukrs
               AND   b~docln  = '000001'
               AND   b~rbusa  = ls_distributor-werks.

******************************BSID 2 times*************************

        REFRESH: gt_bsid1.
        SELECT a~prctr  a~kunnr a~augbl a~budat a~bldat a~blart
               a~bschl  a~zfbdt a~dmbtr a~shkzg a~xblnr a~belnr b~rbusa a~sgtxt
               a~vbeln  a~umskz a~bukrs a~gjahr a~sknto
               FROM bsid AS a INNER JOIN faglflexa AS b
               ON  a~gjahr = b~ryear
               AND a~belnr = b~docnr
               INTO CORRESPONDING FIELDS OF TABLE gt_bsid1
               WHERE a~budat BETWEEN '01.04.2023'
               AND  so_budat-low
               AND  a~bukrs EQ y_knb1-bukrs
               AND  a~kunnr EQ y_knb1-kunnr
               AND  a~umskz <> 'H'
               AND  b~rldnr  = '0L'
               AND  b~rbukrs = y_knb1-bukrs
               AND  b~docln  = '000001'
               AND  b~rbusa  = ls_distributor-werks.


**********************Excess payment removal process*************
        SELECT * FROM zdms_dz_removal INTO TABLE @DATA(lt_delete)
                                      WHERE distributor = @ls_distributor-kunnr
                                      AND   dealer      = @y_knb1-kunnr.
        IF sy-subrc = 0.
          CLEAR : lv_bsad , lv_bsid , lv_bsad1 , lv_bsid1.

***************check the bsad table*****************
          LOOP AT gt_bsad ASSIGNING FIELD-SYMBOL(<fs_bsad>).
            READ TABLE lt_delete TRANSPORTING NO FIELDS WITH KEY belnr = <fs_bsad>-belnr gjahr = <fs_bsad>-gjahr.
            IF sy-subrc = 0.
              <fs_bsad>-flag = 'X'.
            ENDIF.
          ENDLOOP.
          DELETE gt_bsad WHERE flag = 'X'.
***************check the bsid table*****************
          LOOP AT gt_bsid ASSIGNING FIELD-SYMBOL(<fs_bsid>).
            READ TABLE lt_delete TRANSPORTING NO FIELDS WITH KEY belnr = <fs_bsid>-belnr gjahr = <fs_bsid>-gjahr.
            IF sy-subrc = 0.
              <fs_bsid>-flag = 'X'.
            ENDIF.
          ENDLOOP.
          DELETE gt_bsid WHERE flag = 'X'.
***************check the bsad1 table*****************
          LOOP AT gt_bsad1 ASSIGNING FIELD-SYMBOL(<fs_bsad1>).
            READ TABLE lt_delete TRANSPORTING NO FIELDS WITH KEY belnr = <fs_bsad1>-belnr gjahr = <fs_bsad1>-gjahr.
            IF sy-subrc = 0.
              <fs_bsad1>-flag = 'X'.
            ENDIF.
          ENDLOOP.
          DELETE gt_bsad1 WHERE flag = 'X'.
***************check the bsid1 table*****************
          LOOP AT gt_bsid1 ASSIGNING FIELD-SYMBOL(<fs_bsid1>).
            READ TABLE lt_delete TRANSPORTING NO FIELDS WITH KEY belnr = <fs_bsid1>-belnr gjahr = <fs_bsid1>-gjahr.
            IF sy-subrc = 0.
              <fs_bsid1>-flag = 'X'.
            ENDIF.
          ENDLOOP.
          DELETE gt_bsid1 WHERE flag = 'X'.
        ENDIF.


        REFRESH :gt_bsad7.
        REFRESH: gtt_bsid.

        APPEND LINES OF gt_bsid TO gtt_bsid.
        SORT gt_bsad BY belnr augbl.
        APPEND LINES OF gt_bsad TO gt_bsid.
        APPEND LINES OF gt_bsad TO gt_bsad7.
        DELETE gt_bsad7 WHERE shkzg = 'S'.
        APPEND LINES OF gt_bsad1 TO gt_bsid1.

        IF gt_bsid[] IS NOT INITIAL.
          CLEAR: gt_kna1[].
          SELECT kunnr name1 adrnr lifnr FROM kna1
            INTO TABLE gt_kna1
            FOR ALL ENTRIES IN gt_bsid
             WHERE kunnr = gt_bsid-kunnr.
*          REFRESH :gt_bsak.
*          "SELECT LIFNR UMSKZ BELNR XBLNR BLDAT DMBTR BUDAT BLART BSCHL SGTXT GSBER SHKZG BUKRS GJAHR SKNTO
*          SELECT bukrs lifnr umsks umskz augdt augbl zuonr gjahr belnr buzei budat bldat xblnr blart dmbtr bschl sgtxt gsber shkzg sknto
*           FROM bsak INTO TABLE  gt_bsak
*            FOR ALL ENTRIES IN gt_bsid
*            WHERE belnr = gt_bsid-belnr
*            AND   bukrs = gt_bsid-bukrs
*            AND   gjahr = gt_bsid-gjahr.

        ENDIF.


*        REFRESH: gt_knc1.
*        IF gt_bsid IS NOT INITIAL.
*          SELECT  kunnr bukrs umsav gjahr FROM knc1
*            INTO TABLE gt_knc1
*            FOR ALL ENTRIES IN gt_bsid
*             WHERE kunnr = gt_bsid-kunnr
*            AND gjahr = gt_bsid-gjahr
*            AND bukrs = gt_bsid-bukrs.
*        ENDIF.
**    ENDSELECT.
*
*        CLEAR: lv_total.
*        LOOP AT gt_knc1 INTO wa_knc1.
*          lv_total =  wa_knc1-umsav.
*        ENDLOOP.
*
*        CLEAR:wa_knc1.


        LOOP AT gtt_bsid INTO wat_bsid.

          wat_final-kunnr = wat_bsid-kunnr.
          wat_final-belnr = wat_bsid-belnr.
          wat_final-shkzg  = wat_bsid-shkzg.
          wat_final-blart  = wat_bsid-blart.

          IF wat_bsid-shkzg = 'H'.
            wat_final-cr_amt = wat_bsid-dmbtr.

          ELSEIF wat_bsid-shkzg = 'S'.
            wat_final-dr_amt = wat_bsid-dmbtr.

          ENDIF.

          APPEND wat_final TO  itt_final.
          CLEAR wat_final.

        ENDLOOP.


        LOOP AT itt_final INTO wat_final WHERE blart = 'AB'.

          lv_ind4 = sy-tabix.

          CLEAR lv_crdr.

          IF wat_final-shkzg = 'H'.
            lv_crdr = 'S'.

          ELSEIF wat_final-shkzg = 'S'.
            lv_crdr = 'H'.

          ENDIF.

          READ TABLE  itt_final INTO wat_final1 WITH KEY belnr = wat_final-belnr
                                                shkzg = lv_crdr.

          IF sy-subrc = 0 AND

             wat_final1-cr_amt + wat_final1-dr_amt =  wat_final-cr_amt +  wat_final-dr_amt.

            lv_ind3 = sy-tabix.

            DELETE itt_final INDEX lv_ind3.
            DELETE itt_final INDEX lv_ind4.
            CLEAR: wat_final1 ,wat_final,lv_ind3,lv_ind4.
          ENDIF.

        ENDLOOP.

        LOOP AT gt_bsid1 INTO wa_bsid1 .

          IF  wa_bsid1-budat < so_budat-low.

            IF wa_bsid1-shkzg = 'S'.

              lv_opn = lv_opn  + wa_bsid1-dmbtr.

            ELSEIF wa_bsid1-shkzg = 'H'.

              lv_opn  = lv_opn - wa_bsid1-dmbtr.

            ENDIF.

          ENDIF.

          CLEAR wa_bsid1.

        ENDLOOP.




        SORT gt_bsid BY budat belnr blart shkzg.

        CLEAR : sy-subrc .
        LOOP AT gt_bsid INTO wa_bsid .

*          IF sy-subrc = 0.
          wa_final-xblnr = wa_bsid-xblnr.
          wa_final-bldat = wa_bsid-bldat.
          wa_final-kunnr = wa_bsid-kunnr.
          wa_final-belnr = wa_bsid-belnr.
          wa_final-budat = wa_bsid-budat.
          wa_final-bschl = wa_bsid-bschl.
          wa_final-gsber = wa_bsid-gsber.
          wa_final-sgtxt = wa_bsid-sgtxt.
          wa_final-vbeln = wa_bsid-vbeln.
          wa_final-umskz = wa_bsid-umskz.
          wa_final-bukrs = wa_bsid-bukrs.
          wa_final-gjahr = wa_bsid-gjahr.

*          ENDIF.

          READ TABLE gt_t003t INTO wa_t003t WITH KEY blart = wa_bsid-blart.

          wa_final-ltext = wa_t003t-ltext.
          wa_final-blart = wa_bsid-blart.

          READ TABLE gt_kna1 INTO wa_kna1 WITH KEY kunnr = wa_bsid-kunnr.

          wa_final-name1 = wa_kna1-name1.
          wa_final-shkzg = wa_bsid-shkzg.


          IF gs_chk-budat = wa_bsid-budat
            AND gs_chk-blart = wa_bsid-blart
            AND gs_chk-belnr = wa_bsid-belnr
            AND gs_chk-shkzg = wa_bsid-shkzg
            AND wa_bsid-shkzg = 'H'.

            wa_final-sknto = lv_cdisc + wa_bsid-sknto.
            lv_cdisc = wa_final-sknto.
            wa_final-cr_amt = lv_cramt + wa_bsid-dmbtr .
            lv_cramt = wa_final-cr_amt.

            DELETE gt_final INDEX lv_ind.

          ELSEIF gs_chk-budat = wa_bsid-budat

            AND gs_chk-blart = wa_bsid-blart
            AND gs_chk-belnr = wa_bsid-belnr
            AND gs_chk-shkzg = wa_bsid-shkzg
            AND wa_bsid-shkzg = 'S'.

            wa_final-dr_amt = lv_dbamt + wa_bsid-dmbtr.
            lv_dbamt = wa_final-dr_amt.

            DELETE gt_final INDEX lv_ind.

          ELSEIF gs_chk-budat = wa_bsid-budat
            AND gs_chk-blart = wa_bsid-blart
            AND gs_chk-belnr = wa_bsid-belnr
            AND gs_chk-shkzg = wa_bsid-shkzg
            AND wa_bsid-shkzg = 'C'
            AND wa_t003t-blart = 'AB'.

            wa_final-cr_amt = lv_cramt + wa_bsid-dmbtr.
            lv_cramt = wa_final-cr_amt.

            DELETE gt_final INDEX lv_ind.

          ELSEIF gs_chk-budat = wa_bsid-budat
            AND gs_chk-blart = wa_bsid-blart
            AND gs_chk-belnr = wa_bsid-belnr
            AND wa_bsid-shkzg = 'D'
            AND wa_t003t-blart = 'AB'.

            wa_final-dr_amt = lv_dbamt + wa_bsid-dmbtr.
            lv_dbamt = wa_final-dr_amt.

            DELETE gt_final INDEX lv_ind.

          ELSEIF ( gs_chk-budat <> wa_bsid-budat
                    AND gs_chk-blart <> wa_bsid-blart
                    AND gs_chk-belnr <> wa_bsid-belnr
                    AND wa_bsid-shkzg <> 'S' )
              OR ( ( gs_chk-budat <> wa_bsid-budat
                    OR gs_chk-blart <> wa_bsid-blart
                    OR gs_chk-belnr <> wa_bsid-belnr )
                  AND wa_bsid-shkzg <> 'S' )
              OR ( gs_chk-shkzg <> wa_bsid-shkzg
                   AND wa_bsid-shkzg <> 'S' ) .

            wa_final-sknto = wa_bsid-sknto.
            lv_cdisc = wa_final-sknto .
            wa_final-cr_amt = wa_bsid-dmbtr.
            lv_cramt = wa_final-cr_amt .

          ELSEIF ( gs_chk-budat <> wa_bsid-budat
                  AND gs_chk-blart <> wa_bsid-blart
                  AND gs_chk-belnr <> wa_bsid-belnr
                  AND wa_bsid-shkzg <> 'H' )
              OR ( ( gs_chk-budat <> wa_bsid-budat
                    OR gs_chk-blart <> wa_bsid-blart
                    OR gs_chk-belnr <> wa_bsid-belnr )
                 AND wa_bsid-shkzg <> 'H' )
             OR ( gs_chk-shkzg <> wa_bsid-shkzg
                 AND wa_bsid-shkzg <> 'H' ) .

            wa_final-dr_amt = wa_bsid-dmbtr .
            lv_dbamt = wa_final-dr_amt  .

          ELSEIF ( gs_chk-budat <> wa_bsid-budat
                  AND gs_chk-blart <> wa_bsid-blart
                  AND gs_chk-belnr <> wa_bsid-belnr
                  AND gs_chk-shkzg <> wa_bsid-shkzg
                  AND wa_bsid-shkzg = 'C' )
               OR  wa_t003t-blart = 'AB'.

            wa_final-cr_amt = wa_bsid-dmbtr.
            lv_cramt = wa_final-cr_amt.

          ELSEIF ( gs_chk-budat <> wa_bsid-budat
                  AND gs_chk-blart <> wa_bsid-blart
                  AND gs_chk-belnr <> wa_bsid-belnr
               AND gs_chk-shkzg <> wa_bsid-shkzg
               AND wa_bsid-shkzg = 'D')
               OR wa_t003t-blart = 'AB'.

            wa_final-dr_amt = wa_bsid-dmbtr.
            lv_dbamt = wa_final-dr_amt.

          ENDIF.
          gs_chk-belnr = wa_bsid-belnr.
          gs_chk-budat = wa_bsid-budat.
          gs_chk-blart = wa_bsid-blart.
          gs_chk-shkzg = wa_bsid-shkzg.


          APPEND wa_final TO gt_final.
          CLEAR : wa_final,wa_bsid.

          DESCRIBE TABLE gt_final LINES lv_ind.

        ENDLOOP.


        DATA: temp  TYPE bsak-belnr,
              index TYPE sy-tabix.
        CLEAR temp.

*        LOOP AT gt_final INTO wa_final WHERE blart = 'AB'.
*          READ TABLE gt_bsak INTO wa_bsak WITH KEY belnr = wa_final-belnr gjahr = wa_final-gjahr bukrs = wa_final-bukrs.
*          LOOP AT gt_bsak INTO wa_bsak WHERE belnr = wa_final-belnr.
*            IF wa_bsak-shkzg IS NOT INITIAL .
*              wa_bsak3-dmbtr = wa_bsak-dmbtr.
*              wa_bsak3-belnr = wa_bsak-belnr.
*              wa_bsak3-gjahr = wa_bsak-gjahr.
*              wa_bsak3-bukrs = wa_bsak-bukrs.
*              wa_bsak3-bldat = wa_bsak-bldat.
*              wa_bsak3-shkzg = wa_bsak-shkzg.
*              wa_bsak3-blart = wa_bsak-blart.
*              wa_bsak3-lifnr = wa_final-kunnr.
*              wa_bsak3-budat = wa_bsak-budat.
*              wa_bsak3-buzei = wa_bsak-buzei.
*              wa_bsak3-bschl = wa_bsak-bschl.
*              APPEND wa_bsak3 TO gt_bsak3.
*              CLEAR : wa_final4,wa_bsak,wa_bsak3.
*            ENDIF.
*          ENDLOOP.
*        ENDLOOP.
*
*        SORT gt_bsak3 BY lifnr belnr bldat dmbtr buzei.
*        DELETE ADJACENT DUPLICATES FROM gt_bsak3 COMPARING lifnr belnr bldat dmbtr buzei.
*        LOOP AT gt_bsak3 INTO wa_bsak3.
*          IF wa_bsak3-shkzg = 'S'.
*            wa_final4-cr_amt = wa_bsak3-dmbtr.
*          ELSEIF wa_bsak3-shkzg = 'H'.
*            wa_final4-dr_amt = wa_bsak3-dmbtr.
*          ENDIF.
*          wa_final4-belnr = wa_bsak3-belnr.
*          wa_final4-gjahr = wa_bsak3-gjahr.
*          wa_final4-bukrs = wa_bsak3-bukrs.
*          wa_final4-bldat = wa_bsak3-bldat.
*          wa_final4-shkzg = wa_bsak3-shkzg.
*          wa_final4-blart = wa_bsak3-blart.
*          wa_final4-kunnr = wa_bsak3-lifnr.
*          wa_final4-budat = wa_bsak3-budat.
*          wa_final4-bschl = wa_bsak3-bschl.
*          APPEND wa_final4 TO gt_final4.
*          CLEAR : wa_final4,wa_bsak,wa_bsak3.
*        ENDLOOP.
*        LOOP AT gt_final4 INTO wa_final4.
*          READ TABLE gt_final INTO wa_final WITH KEY kunnr = wa_final4-kunnr belnr = wa_final4-belnr bldat = wa_final4-bldat.
*          IF sy-subrc = 0.
*            DELETE gt_final WHERE kunnr = wa_final4-kunnr AND belnr = wa_final4-belnr AND bldat = wa_final4-bldat.
*          ENDIF.
*        ENDLOOP.
*        APPEND LINES OF gt_final4 TO gt_final.
**        REFRESH : gt_final4.
*        CLEAR: gt_bsid[].
*        CLEAR: gs_chk ,wa_bsid, wa_t003t, wa_kna1 .
*
*        APPEND LINES OF gt_final TO gt_final1.
*        APPEND LINES OF gt_final TO gt_final2.



*        TYPES: BEGIN OF a,
*                 ln TYPE i,
*               END OF a.
*        DATA: gt_line TYPE TABLE OF a,
*              gs_line TYPE a.

*        LOOP AT gt_final1 INTO wa_final1.
*
*          IF gs_chk-budat = wa_final1-budat AND
*               gs_chk-blart = wa_final1-blart AND
*               gs_chk-belnr = wa_final1-belnr.
*            IF wa_final1-cr_amt GT gs_chk-cr_amt.
*              gs_line-ln = sy-tabix.
*            ENDIF.
*          ENDIF.
*
*          gs_chk-belnr = wa_final1-belnr.
*          gs_chk-budat = wa_final1-budat.
*          gs_chk-blart = wa_final1-blart.
*          gs_chk-cr_amt = wa_final1-cr_amt.
*          gs_chk-dr_amt = wa_final1-dr_amt.
*
*        ENDLOOP.
*
*        b_date = so_budat-high.
*        SORT gt_final BY budat shkzg.

*        LOOP AT gt_final INTO wa_final.
*
*
*          READ TABLE gt_final2 INTO wa_final2 WITH KEY belnr = wa_final-belnr
*                                                       shkzg = wa_final-shkzg .
*
*          IF wa_final2-sknto IS NOT INITIAL .
*
*            wa_final2-cr_amt = wa_final2-cr_amt - wa_final2-sknto.
*
*            APPEND wa_final2 TO gt_final3.
*            CLEAR: wa_final2-cr_amt.
*
*            wa_final2-cr_amt = wa_final2-sknto.
*            wa_final2-lv_count = 1.
*            APPEND wa_final2 TO gt_final3.
*            DELETE gt_final WHERE belnr = wa_final2-belnr AND shkzg = 'H' .
*          ENDIF.
*
*        ENDLOOP.
*
*        APPEND LINES OF gt_final3 TO gt_final.

*        LOOP AT gt_final INTO wa_final.
*
*          lv_sum = lv_sum + wa_final-dr_amt - wa_final-cr_amt .
*          wa_final-bal_amt = lv_sum.
*
*          MODIFY gt_final FROM wa_final TRANSPORTING bal_amt.
*
*          AT END OF kunnr.
*            CLEAR :lv_sum,wa_final,wa_bsid.
*          ENDAT.
*
*        ENDLOOP.

*        LOOP AT gt_final INTO wa_final WHERE kunnr = ' '.
*          wa_final-kunnr = wa_kna1-kunnr.
*
*          MODIFY gt_final FROM wa_final TRANSPORTING kunnr.
*          CLEAR  wa_final.
*
*        ENDLOOP.

*        CLEAR: rv_opn.
*        rv_opn = lv_total - lv_opn.
        SORT gt_final BY budat shkzg.

        LOOP AT gt_final INTO wa_final .


          lv_ind2 = sy-tabix.

          IF wa_final-blart = 'AB'.
            CLEAR lv_inc.
            IF wa_final-shkzg = 'H'.
              lv_inc = 'S'.

            ELSEIF wa_final-shkzg = 'S'.
              lv_inc = 'H'.

            ENDIF.

            READ TABLE  gt_final INTO wa_final_t WITH KEY belnr = wa_final-belnr
                                                          shkzg = lv_inc.

            IF sy-subrc = 0 AND
              wa_final_t-cr_amt + wa_final_t-dr_amt = wa_final-cr_amt + wa_final-dr_amt.

              lv_ind1 = sy-tabix.

              DELETE gt_final INDEX lv_ind1.
              DELETE gt_final INDEX lv_ind2.

              CLEAR: wa_final ,wa_final_t,lv_ind1,lv_ind2.

            ENDIF.
            CONTINUE.
          ENDIF.

*          CASE wa_final-blart.
*            WHEN 'RV'.
*              wa_final-t_tttext = 'INVOICE'.
*            WHEN 'DG'.
*              wa_final-t_tttext = 'CREDIT NOTE'.
*            WHEN 'DZ'.
*              IF wa_final-lv_count IS NOT INITIAL.
*                wa_final-t_tttext = 'AUTO CREDIT NOTE'.
*                CLEAR wa_final-lv_count.
*              ELSE.
*                wa_final-t_tttext = 'RECEIPT'.
*
*              ENDIF.
*            WHEN 'VG'.
*              wa_final-t_tttext = 'SALES RETURN'.
*            WHEN 'DA'.
*              wa_final-t_tttext = 'CHEQUE RETURN'.
*            WHEN 'DR'.
*              wa_final-t_tttext = 'DEBIT NOTE'.
*            WHEN 'AB'.
*              CASE wa_final-bschl .
*                WHEN 40.
*                  wa_final-t_tttext = 'AUTO CREDIT NOTE'.
*                WHEN 17.
*                  IF wa_final-sknto IS NOT INITIAL.
*                    wa_final-t_tttext = 'AUTO CREDIT NOTE'.
*                    CLEAR wa_final-sknto.
*                  ELSE.
*                    wa_final-t_tttext = 'FREIGHT / INTEREST'.
*                  ENDIF.
*                WHEN 27.
*                  wa_final-t_tttext = 'FREIGHT / INTEREST'.
*                WHEN 04.
*                  wa_final-t_tttext = 'FREIGHT / INTEREST'.
*                WHEN OTHERS.
*                  wa_final-t_tttext = 'ACCOUNTING DOCUMENT'.
*              ENDCASE.
*            WHEN 'KR'.
*              wa_final-t_tttext = 'COMMISSION'.
*            WHEN 'SA'.
*              wa_final-t_tttext = 'JOURNAL ENTRY'.
*            WHEN OTHERS.
*              lv_flag2 = abap_true.
*          ENDCASE.
*          IF lv_flag2 IS INITIAL.
*            MODIFY gt_final FROM wa_final INDEX lv_ind2.
*
*          ENDIF.
*
*
*          CLEAR lv_flag2.
**** created on 17.09.2020
*          CLEAR:wa_final.
        ENDLOOP.


************************added for closing balance***********
        DATA : bal_amt  TYPE  bsad-dmbtr,
               ls_bsid  TYPE zcus_struc_bsid,
               d_tot    TYPE  bsid-dmbtr,
               c_tot    TYPE  bsid-dmbtr,
               doc_type TYPE blart.

        CLEAR : bal_amt,c_tot,d_tot.
        LOOP AT gt_final[] INTO DATA(ls_final).
***
***   IF WA_BSID-SHKZG = 'H'.
***           DBT_AMT = DBT_AMT + WA_BSID-DBT_AMT .
***    ENDIF.

          IF ls_final-cr_amt IS NOT INITIAL .
            c_tot = c_tot + ls_final-cr_amt.
          ELSEIF ls_final-dr_amt IS NOT INITIAL.
            d_tot = d_tot + ls_final-dr_amt .
          ENDIF.
          CLEAR : ls_final.
        ENDLOOP.

        bal_amt = d_tot - c_tot .
        bal_amt = lv_opn + bal_amt .

        IF bal_amt LT 0.
          doc_type = 'CR'.
        ELSE.
          doc_type = 'DR'.
        ENDIF.

        APPEND VALUE #( distributor = ls_distributor-kunnr
                        dist_name   = ls_distributor-name1
                        werks       = ls_distributor-werks
                        dealer      = y_knb1-kunnr
                        dealer_name = y_knb1-name1
                        st_date     = so_budat-low
                        ed_date     = so_budat-high
                        lv_opbal    = lv_opn
                        lv_clbal    = bal_amt
                        doc_type    = doc_type ) TO gt_output.

*      CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
*        EXPORTING
*          formname           = 'ZSF_CUS_ACCOUNT_LEDGER_MAIL'
**         VARIANT            = ' '
**         DIRECT_CALL        = ' '
*        IMPORTING
*          fm_name            = fm_name
*        EXCEPTIONS
*          no_form            = 1
*          no_function_module = 2
*          OTHERS             = 3.
*      IF sy-subrc <> 0.
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*      ENDIF.
*
*      w_ctrlop-getotf = abap_true.
*      w_ctrlop-no_dialog = abap_true.
*      w_compop-tdnoprev = abap_true.
*      w_ctrlop-preview = space.
*      w_compop-tddest = 'LOCL'.
*
*      IF gt_final[] IS NOT INITIAL.
*
*        PERFORM: excel_attach.
*
*        CLEAR o_bal.
*        o_bal = lv_opn .
*        CALL FUNCTION fm_name "'/1BCDWB/SF00000509'
*          EXPORTING
**           ARCHIVE_INDEX      =
**           ARCHIVE_INDEX_TAB  =
**           ARCHIVE_PARAMETERS =
*            control_parameters = w_ctrlop
**           MAIL_APPL_OBJ      =
**           MAIL_RECIPIENT     =
**           MAIL_SENDER        =
*            output_options     = w_compop
*            user_settings      = abap_true "'X'
*            bal_date           = b_date
*            lv_frdt            = so_budat-low
*            lv_todt            = so_budat-high
*            lv_opn             = lv_opn
*            lv_buk             = lv_buk
*          IMPORTING
**           DOCUMENT_OUTPUT_INFO       =
*            job_output_info    = w_return
**           JOB_OUTPUT_OPTIONS =
*          TABLES
*            gt_kna1            = gt_kna1[]
*            gt_bsid            = gt_final[]
*            itt_final          = itt_final[]
*          EXCEPTIONS
*            formatting_error   = 1
*            internal_error     = 2
*            send_error         = 3
*            user_canceled      = 4
*            OTHERS             = 5.
*        IF sy-subrc <> 0.
**        Implement suitable error handling here
*        ENDIF.
*
*        i_otf[] = w_return-otfdata[].
*
*        CALL FUNCTION 'CONVERT_OTF'
*          EXPORTING
*            format                = 'PDF'  "'ASCII'
*            max_linewidth         = 132
**           ARCHIVE_INDEX         = ' '
**           COPYNUMBER            = 0
**           ASCII_BIDI_VIS2LOG    = ' '
**           PDF_DELETE_OTFTAB     = ' '
**           PDF_USERNAME          = ' '
**           PDF_PREVIEW           = ' '
**           USE_CASCADING         = ' '
**           MODIFIED_PARAM_TABLE  =
*          IMPORTING
*            bin_filesize          = v_len_in
*            bin_file              = i_xstring
*          TABLES
*            otf                   = i_otf
*            lines                 = i_tline
*          EXCEPTIONS
*            err_max_linewidth     = 1
*            err_format            = 2
*            err_conv_not_possible = 3
*            err_bad_otf           = 4
*            OTHERS                = 5.
*        IF sy-subrc <> 0.
**          Implement suitable error handling here
*        ENDIF.
*
*        CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
*          EXPORTING
*            buffer     = i_xstring
**           APPEND_TO_TABLE       = ' '
**       IMPORTING
**           OUTPUT_LENGTH         =
*          TABLES
*            binary_tab = i_objbin[].
*
*        DATA: in_mailid TYPE ad_smtpadr.
*        CLEAR in_mailid.
*
**~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ADDED BY PR@$@TH (testing purpose)
**        CLEAR: S_EMAIL.
**        IF E_C = 'X'.
**          S_EMAIL = 'ramachandran@sphinaxinfosystems.com'.
**          CLEAR: E_C .
**        ELSE.
**          S_EMAIL = 'prasath@sphinaxinfosystems.com' .
**          E_C = 'X' .
**        ENDIF.
**~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ENDED BY PR@$@TH
*
*        in_mailid = s_email .
*        PERFORM send_mail USING in_mailid .
*        COMMIT WORK.
*        count = count + 1 .
*    ENDIF.
        CLEAR: gt_final[] , wa_final , gt_final1[] ,wa_final1 ,gt_final2[] ,
               wa_final2 ,gt_final3[] ,wa_final3 , lv_opn , lv_buk ,gt_bsak3,gt_final4,doc_type.
        REFRESH : gt_final[] ,gt_bsid[],gt_bsad[],gt_bsak[],gt_bsik[],gt_bsad1[],gt_bsid1[],itt_final[],i_otf[],i_tline[],gt_bsak3[],gt_final4[].

      ENDLOOP.
    ENDIF.

    CLEAR : ls_distributor.
  ENDLOOP.


***************Call the display alv screen************
  SORT : gt_output BY distributor.

  CALL FUNCTION 'Z_POPUP_ALV'
    TABLES
      it_alv = gt_output.



ENDFORM.                    " FETCHDATA

*&---------------------------------------------------------------------*
*&      Form  SEND_MAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IN_MAILID  text
*----------------------------------------------------------------------*
FORM send_mail USING in_mailid.

  DATA: salutation TYPE string.
  DATA: body TYPE string.
  DATA: footer TYPE string.
  DATA: sub TYPE char50.
  DATA: body1(500) TYPE c.
  DATA: tem_bdy TYPE string.

  DATA: lo_send_request TYPE REF TO cl_bcs,
        lo_document     TYPE REF TO cl_document_bcs,
        lo_sender       TYPE REF TO if_sender_bcs,
        lo_recipient    TYPE REF TO if_recipient_bcs VALUE IS INITIAL,lt_message_body TYPE bcsy_text,
        lx_document_bcs TYPE REF TO cx_document_bcs,
        lv_sent_to_all  TYPE os_boolean.

  DATA: sender_mail TYPE  adr6-smtp_addr VALUE 'CustomerConfirmation@sheenlac.in' .

  DATA:sm_nam TYPE so_obj_des.
  DATA:header TYPE char100.
  DATA:content TYPE char100.
  CLEAR: salutation , body ,footer ,lo_send_request ,lo_document ,lo_sender ,lo_recipient ,lv_sent_to_all.

  lo_send_request = cl_bcs=>create_persistent( ).

  CLEAR: lt_message_body.
  TRANSLATE sm_name TO LOWER CASE.
  DATA: f_dat TYPE char10,
        t_dat TYPE char10.
  CONCATENATE so_budat-low+6(2) '.' so_budat-low+4(2) '.' so_budat-low(4) INTO f_dat.
  CONCATENATE so_budat-high+6(2) '.' so_budat-high+4(2) '.' so_budat-high(4) INTO t_dat.
  footer = 'Dear Sir/Madam,'.
  APPEND footer TO lt_message_body.
  CLEAR: footer.

  footer = ' '.
  APPEND footer TO lt_message_body.
  CLEAR: footer.

  CONCATENATE '  We are pleased to sending you the confirmation of balance for the period ending(' t_dat ')' INTO tem_bdy SEPARATED BY space.

  footer = tem_bdy .
  APPEND footer TO lt_message_body.
  CLEAR: footer.

  footer = 'Kindly Confirm the same with in 15 days of else the balance as per our records is treated as correct'.
  APPEND footer TO lt_message_body.
  CLEAR: footer.

  footer = '-In case of any query Contact ( +91-9445864644 / sathishg@sheenlac.in or +91-7338832775 / manikandan@jnpl.in )'.
  APPEND footer TO lt_message_body.
  CLEAR: footer.

  footer = ' '.
  APPEND footer TO lt_message_body.
  CLEAR: footer.

  footer = 'Any dispute " Chennai Jurisdiction "'.
  APPEND footer TO lt_message_body.
  CLEAR: footer.

  DATA: wa_t247 TYPE t247.

  SELECT SINGLE * FROM t247 INTO wa_t247 WHERE spras = 'EN' AND mnr = so_budat-low+4(2) .

  SHIFT y_knb1-kunnr LEFT DELETING LEADING '0' .

  CONCATENATE 'Confirmation of Balance' y_knb1-kunnr 'For' wa_t247-ktx '/' so_budat-low(4) INTO sub SEPARATED BY space.

  lo_document = cl_document_bcs=>create_document(
  i_type = 'RAW'
  i_text = lt_message_body
  i_subject = sub ).

  DATA: mon   TYPE i,
        f_ktx TYPE t247-ktx,
        t_ktx TYPE t247-ktx.
  IF so_budat-low(4) = so_budat-high(4) AND so_budat-low+4(2) = so_budat-high+4(2).
    SELECT SINGLE ktx FROM t247 INTO f_ktx WHERE mnr = so_budat-low+4(2) AND spras = 'EN'.
    TRANSLATE sm_name TO UPPER CASE.
    CONCATENATE sm_name'_' f_ktx so_budat-low(4) '_Statement' INTO sm_nam.
  ELSE.
    SELECT SINGLE ktx FROM t247 INTO f_ktx WHERE mnr = so_budat-low+4(2) AND spras = 'EN'.
    SELECT SINGLE ktx FROM t247 INTO t_ktx WHERE mnr = so_budat-high+4(2) AND spras = 'EN'.
    TRANSLATE sm_name TO UPPER CASE.
    IF so_budat-low(4) = so_budat-high(4).
      CONCATENATE sm_name '_' f_ktx '-' t_ktx  so_budat-low(4) '_Statement' INTO sm_nam.
    ELSE.
      CONCATENATE sm_name '_' f_ktx so_budat-low(4) '_' t_ktx so_budat-high(4) '_Statement' INTO sm_nam .
    ENDIF.
  ENDIF.

  TRY.
      lo_document->add_attachment(
      EXPORTING
      i_attachment_type = 'PDF'
      i_attachment_subject = sm_nam
      i_att_content_hex = i_objbin[] ).
    CATCH cx_document_bcs INTO lx_document_bcs.
  ENDTRY.

  TRY.
      lo_document->add_attachment(
      EXPORTING
      i_attachment_type = 'XLS'
      i_attachment_subject = sm_nam
      i_att_content_hex = i_objbin_f[] ).
    CATCH cx_document_bcs INTO lx_document_bcs.
  ENDTRY.


  lo_send_request->set_document( lo_document ).
*  LO_SENDER = CL_SAPUSER_BCS=>CREATE( SY-UNAME ).     Manual Email Id Inserted Variable Name SENDER_MAIL
  lo_sender = cl_cam_address_bcs=>create_internet_address( sender_mail ).
  lo_send_request->set_sender( lo_sender ).
  IF sy-sysid = 'DEV' OR
     sy-sysid = 'QAS'.
    in_mailid = 'no-reply@sheenlac.in'.
    lo_recipient = cl_cam_address_bcs=>create_internet_address( in_mailid ).
  ELSE.
    lo_recipient = cl_cam_address_bcs=>create_internet_address( in_mailid ).
  ENDIF.
  lo_send_request->add_recipient( EXPORTING i_recipient = lo_recipient i_express = abap_true ).
  lo_send_request->add_recipient( lo_recipient ).
  lo_send_request->send( EXPORTING i_with_error_screen = abap_true RECEIVING result = lv_sent_to_all ).
  tot = tot + 1 .
  COMMIT WORK.
  CLEAR: i_objbin_f[],i_objbin[].
ENDFORM.                    " SEND_MAIL
*&---------------------------------------------------------------------*
*&      Form  STATUS_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM status_email .

  DATA: f_e TYPE string.
  DATA: hdat_f(10) TYPE c,
        hdat_t(10) TYPE c.
  TYPES: BEGIN OF ty_lines,
           line TYPE char255,
         END OF ty_lines.
  DATA: ti_lines TYPE STANDARD TABLE OF ty_lines,
        wa_lines TYPE ty_lines.
  DATA:  title TYPE string.

  CONCATENATE so_budat-low+6(2) '.' so_budat-low+4(2) '.' so_budat-low(4) INTO hdat_f .
  CONCATENATE so_budat-high+6(2) '.' so_budat-high+4(2) '.' so_budat-high(4) INTO hdat_t .
  CONCATENATE 'Customer Statement From ' hdat_f ' To ' hdat_t  INTO f_e  SEPARATED BY space.

  title =  f_e.

  wa_lines = 'Dear Developer,'.
  APPEND wa_lines TO ti_lines .

  wa_lines = '   This mail has been generated automatically. Please do not reply'.
  APPEND wa_lines TO ti_lines .

  tot_c = tot .
  CONCATENATE 'Email Send Sucessfully: ' tot_c ' Customers' INTO wa_lines.
  APPEND wa_lines TO ti_lines .

  wa_lines = '______________________________________________________________________'.
  APPEND: wa_lines TO ti_lines .
  wa_lines = 'Thanks&Regards'.
  APPEND: wa_lines TO ti_lines .
  wa_lines = 'SIS_TEAM'.
  APPEND: wa_lines TO ti_lines .

  CALL FUNCTION 'EFG_GEN_SEND_EMAIL'
    EXPORTING
      i_title                = title
      i_sender               = 'no-reply@sheenlac.in'
      i_recipient            = 'prasath@sphinaxinfosystems.com'
      i_flg_commit           = 'X'
      i_flg_send_immediately = 'X'
    TABLES
      i_tab_lines            = ti_lines
    EXCEPTIONS
      not_qualified          = 1
      failed                 = 2
      OTHERS                 = 3.

  PRINT-CONTROL FUNCTION 'SAPBLD'.
  WRITE:/'Email Sending Status', sy-uline.
  PRINT-CONTROL FUNCTION 'SAOFF'.

  IF tot < 1.
    WRITE:/ 'No Emails Send' COLOR COL_NEGATIVE .
  ELSE.
    WRITE:/ 'Email Send Sucessfully', tot ,'Customer' COLOR COL_POSITIVE .
  ENDIF.
ENDFORM.                    " STATUS_EMAIL


*&---------------------------------------------------------------------*
*&      Form  EXCEL_ATTACH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM excel_attach .

  DATA: lv_tttext(21) TYPE c.
  DATA: tot_d TYPE bsad-dmbtr.
  DATA: tot_c TYPE bsad-dmbtr.
  DATA: tot TYPE bsad-dmbtr.
  DATA: clb TYPE bsad-dmbtr.
  DATA: t_opn TYPE bsad-dmbtr.
  DATA: c_opn(15) TYPE c.
  DATA: de_c(15) TYPE c.
  DATA: cr_c(15) TYPE c.
  DATA: cl_c(15) TYPE c.
  DATA: c_time(11) TYPE c.
  DATA: c_s_p(150) TYPE c.  "CITY STATE PINCODE
  DATA: doc_dat(10) TYPE c. "DOCUMENT DATE
  DATA: f_dat(10) TYPE c.  "FROM DATE
  DATA: t_dat(10) TYPE c.  "TO DATE
  DATA: c_dat(10) TYPE c.  "CURRENT DATE.
  DATA: day_nam TYPE t246-langt.  ""DTRESR-WEEKDAY. "WEEK DAY NAME
  DATA: time TYPE sy-uzeit.
  DATA: t_ty(2) TYPE c.
  DATA: wa_adrc TYPE adrc.
  DATA: regio TYPE  t005u-bezei.
  DATA: reg TYPE  kna1-regio.
  DATA:lv_belnr TYPE  bsid-belnr.
  DATA:lv_blart TYPE  bsid-blart.
  DATA: ttt(22) TYPE c. "TRANSACTION TYPE CONCAT FIELD
  DATA: collection  TYPE  bsid-dmbtr.
  DATA: cc(15) TYPE c. " COLLECTION CHAR
  DATA: creditnote  TYPE  bsid-dmbtr.
  DATA: cnc(15) TYPE c. "CREDIT NOTE CHAR
  DATA: debitnote TYPE  bsid-dmbtr.
  DATA: invoice TYPE  bsid-dmbtr.
  DATA: ic(15) TYPE c . "INVOICE CHAR
  DATA: dc  TYPE i,  "DEBIT COUNT
        cc1 TYPE i.  "CREDIT COUNT
  DATA: dcc(4) TYPE c,  "DEBIT COUNT CHARACTER
        ccc(4) TYPE c.  "CREDIT COUNT CHARACTER
  DATA: creditlimit TYPE knkk-klimk. "#EC CI_USAGE_OK[2227014] "Added by SPLABAP during code remediation   "CREDIT LIMIT
  DATA: crl_c(15) TYPE c ." CREDIT LIMIT CHARACTER
  DATA: bytes TYPE i.
  DATA: creditnoteamt(15) TYPE c.
  DATA: cont(255) TYPE c.


  TYPES: BEGIN OF ty_exfi,
           doc_dat(10) TYPE c,
           ttt(21)     TYPE c,
           belnr       TYPE bsid-belnr,
           xblnr       TYPE bsid-xblnr,
           sgtxt       TYPE bsid-sgtxt,
           d_amt       TYPE bsid-dmbtr,
           c_amt       TYPE bsid-dmbtr,
           b_amt       TYPE bsid-dmbtr,
           blart       TYPE bsid-blart,
         END OF ty_exfi.

  DATA: it_exfi TYPE TABLE OF ty_exfi,
        wa_exfi TYPE ty_exfi.

  DATA : salesoffice TYPE  knvv-vkbur,
         so_nam      TYPE  tvkbt-bezei.

  CLEAR: lv_tttext ,tot_d,tot_c,tot,clb,t_opn,c_opn,de_c,cr_c,cl_c,c_time,c_s_p,doc_dat,f_dat,t_dat,c_dat,day_nam,
         time,t_ty,wa_adrc,regio,reg,lv_belnr,lv_blart,ttt,collection,cc,creditnote,cnc,debitnote,invoice,ic,dc,
         cc1,dcc,ccc, creditlimit,crl_c,bytes,creditnoteamt,it_attach .

  CONCATENATE ' ' ' ' ' ' 'STATEMENT OF ACCOUNT' INTO it_attach SEPARATED BY con_tab .
  CONCATENATE con_cret it_attach  INTO it_attach.
  APPEND it_attach .

  CONCATENATE ' ' ' ' INTO it_attach SEPARATED BY con_tab .
  CONCATENATE con_cret it_attach  INTO it_attach.
  APPEND it_attach.

  CLEAR:wa_adrc.

  LOOP AT gt_kna1 INTO wa_kna1.
*    SELECT SINGLE * FROM ADRC INTO WA_ADRC WHERE ADDRNUMBER = WA_KNA1-ADRNR."Commented by SPLABAP during code remediation
    "Added by SPLABAP during code remediation
    SELECT  * UP TO 1 ROWS
      FROM adrc INTO wa_adrc WHERE addrnumber = wa_kna1-adrnr
      ORDER BY PRIMARY KEY.
    ENDSELECT.
*    SELECT SINGLE VKBUR FROM KNVV INTO SALESOFFICE WHERE KUNNR = WA_KNA1-KUNNR AND VKORG = LV_BUK."Commented by SPLABAP during code remediation
    "Added by SPLABAP during code remediation
    SELECT vkbur UP TO 1 ROWS
      FROM knvv INTO salesoffice WHERE kunnr = wa_kna1-kunnr AND vkorg = lv_buk
      ORDER BY PRIMARY KEY.
    ENDSELECT.
    SELECT SINGLE bezei FROM tvkbt INTO so_nam WHERE vkbur = salesoffice AND spras = 'EN'.
    EXIT.    "#EC CI_NOORDER  "Added by SPLABAP during code remediation
  ENDLOOP.

  CONCATENATE ' ' ' ' INTO it_attach SEPARATED BY con_tab .
  CONCATENATE con_cret it_attach  INTO it_attach.
  APPEND it_attach.

  CONCATENATE wa_adrc-name1 ' ' ' ' ' ' ' ' ' '  'Dealer Code:' wa_kna1-kunnr  INTO it_attach SEPARATED BY con_tab .
  CONCATENATE con_cret it_attach  INTO it_attach.
  APPEND it_attach.

  CONCATENATE wa_adrc-street ' ' ' ' ' ' ' '  ' ' 'Sales Office:' so_nam INTO it_attach SEPARATED BY con_tab .
  CONCATENATE con_cret it_attach  INTO it_attach.
  APPEND it_attach.
  CLEAR: c_s_p.
  CONCATENATE wa_adrc-city1 '-' wa_adrc-post_code1 ',INDIA' INTO c_s_p.

  SELECT SINGLE regio FROM kna1 INTO reg WHERE kunnr = wa_kna1-kunnr.
  SELECT SINGLE bezei FROM t005u INTO regio WHERE spras = 'EN' AND land1 = 'IN' AND bland = reg.

  CONCATENATE c_s_p '' '' '' '' ''  'Territory:' regio  INTO it_attach SEPARATED BY con_tab .
  CONCATENATE con_cret it_attach  INTO it_attach.
  APPEND it_attach.

  CONCATENATE '' '' '' '' '' ''  'Currency:' 'INR' INTO it_attach SEPARATED BY con_tab.
  CONCATENATE con_cret it_attach  INTO it_attach.
  APPEND it_attach.

  CONCATENATE ' ' ' ' INTO it_attach SEPARATED BY con_tab .
  CONCATENATE con_cret it_attach  INTO it_attach.
  APPEND it_attach.

  CLEAR: f_dat , t_dat , c_dat .
  CONCATENATE so_budat-low+6(2) '/' so_budat-low+4(2) '/' so_budat-low(4) INTO f_dat .
  CONCATENATE so_budat-high+6(2) '/' so_budat-high+4(2) '/' so_budat-high(4) INTO t_dat.
  CONCATENATE sy-datum+6(2) '/' sy-datum+4(2) '/' sy-datum(4) INTO c_dat.
  CLEAR:day_nam.
  CALL FUNCTION 'GET_WEEKDAY_NAME'
    EXPORTING
      date        = sy-datum
      language    = sy-langu
*     WEEKDAY_NUMBER       = ' '
    IMPORTING
*     LANGU_BACK  =
      longtext    = day_nam
*     SHORTTEXT   =
    EXCEPTIONS
      calendar_id = 1
      date_error  = 2
      not_found   = 3
      wrong_input = 4
      OTHERS      = 5.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

**  CALL FUNCTION 'DATE_TO_DAY'
**    EXPORTING
**      DATE    = SY-DATUM
**    IMPORTING
**      WEEKDAY = DAY_NAM.
  day_nam = day_nam(3).
  CLEAR: time , t_ty.
  CALL FUNCTION 'HRVE_CONVERT_TIME'
    EXPORTING
      type_time       = 'A'
      input_time      = sy-uzeit
*     INPUT_AM_PM     = 'AM'
    IMPORTING
      output_time     = time
      output_am_pm    = t_ty
    EXCEPTIONS
      parameter_error = 1
      OTHERS          = 2.

  CLEAR:c_time.
  CONCATENATE time(2)':' time+2(2) ':' time+4(2) ' ' t_ty INTO c_time.
  CONCATENATE 'Statement of the account for the period of ' f_dat ' to ' t_dat ' generated on ' day_nam c_dat ' at ' c_time INTO cont SEPARATED BY space.
  CONCATENATE cont '' INTO it_attach .
  CONCATENATE con_cret it_attach  INTO it_attach.
  APPEND it_attach.

  CONCATENATE ' ' ' ' INTO it_attach SEPARATED BY con_tab .
  CONCATENATE con_cret it_attach  INTO it_attach.
  APPEND it_attach.

  CLEAR: tot.
  MOVE lv_opn TO tot.

  LOOP AT gt_final INTO wa_final.

    CASE wa_final-blart.
      WHEN 'DG'.
        lv_tttext = wa_final-t_tttext.
        IF wa_final-t_tttext = 'CREDIT NOTE' AND
          wa_final-shkzg = 'H'.

          creditnote = creditnote + wa_final-cr_amt.

        ELSEIF  wa_final-t_tttext = 'CREDIT NOTE' AND
        wa_final-shkzg = 'S'.

          debitnote = debitnote + wa_final-dr_amt.

        ENDIF.

      WHEN 'DZ'.
        lv_tttext = wa_final-t_tttext.

        IF  wa_final-t_tttext = 'AUTO CREDIT NOTE'AND
            wa_final-shkzg = 'H'.
          creditnote = creditnote + wa_final-cr_amt.
        ELSEIF  wa_final-t_tttext = 'AUTO CREDIT NOTE'AND
                wa_final-shkzg = 'S'.
          debitnote = debitnote + wa_final-dr_amt.
        ELSEIF  wa_final-t_tttext = 'COLLECTION'AND
                wa_final-shkzg = 'H'.
          collection = collection + wa_final-cr_amt .
        ELSEIF  wa_final-t_tttext = 'COLLECTION'AND
                wa_final-shkzg = 'S'.
          collection = collection + wa_final-dr_amt .
        ELSEIF  wa_final-t_tttext = 'RECEIPT'AND
               wa_final-shkzg = 'H'.
          collection = collection + wa_final-cr_amt .
        ELSEIF  wa_final-t_tttext = 'RECEIPT'AND
                wa_final-shkzg = 'S'.
          collection = collection + wa_final-dr_amt .
        ENDIF.

      WHEN 'AB'.
        lv_tttext = wa_final-t_tttext.
        IF wa_final-t_tttext = 'CREDIT NOTE'
         OR wa_final-t_tttext = 'AUTO CREDIT NOTE'.
          IF wa_final-shkzg = 'H'.
            creditnote = creditnote + wa_final-cr_amt.
          ELSEIF wa_final-shkzg = 'S'.
            debitnote = debitnote + wa_final-dr_amt.
          ENDIF.
        ENDIF.

      WHEN 'DR'.
        lv_tttext = wa_final-t_tttext.
        IF wa_final-t_tttext = 'DEBIT NOTE'.
          debitnote = debitnote + wa_final-dr_amt.
        ENDIF.
      WHEN 'RV'.
        lv_tttext = wa_final-t_tttext.
        invoice = invoice + wa_final-dr_amt.
      WHEN 'VG'.
        lv_tttext = wa_final-t_tttext.
      WHEN 'DA'.
        lv_tttext = wa_final-t_tttext.
      WHEN 'KR'.
        lv_tttext = wa_final-t_tttext.
      WHEN 'SA'.
        lv_tttext = wa_final-t_tttext.
      WHEN OTHERS.
        SELECT SINGLE ltext FROM t003t INTO lv_tttext
          WHERE spras = 'EN' AND blart = wa_final-blart.
        TRANSLATE lv_tttext TO UPPER CASE.
    ENDCASE.

    CONCATENATE wa_final-blart '-' lv_tttext INTO ttt .

    IF wa_final-bschl = 27 .
      wa_final-shkzg = 'H'.
    ENDIF.

    IF wa_final-shkzg = 'S'.
      tot = tot + wa_final-dr_amt.
      tot_d = tot_d + wa_final-dr_amt .
      dc = dc + 1 .
    ELSEIF wa_final-shkzg = 'H'.
      tot = tot - wa_final-cr_amt .
      tot_c = tot_c + wa_final-cr_amt.
      cc1 = cc1 + 1.
    ENDIF.
    CLEAR: doc_dat.
    CONCATENATE wa_final-bldat+6(2) '-' wa_final-bldat+4(2) '-' wa_final-bldat(4) INTO doc_dat.
    wa_exfi-doc_dat = doc_dat.
    wa_exfi-ttt = ttt .
    wa_exfi-sgtxt = wa_final-sgtxt.
    wa_exfi-xblnr = wa_final-xblnr.
    wa_exfi-belnr = wa_final-belnr.
    wa_exfi-d_amt = wa_final-dr_amt .
    wa_exfi-c_amt = wa_final-cr_amt .
    wa_exfi-b_amt = tot.
    wa_exfi-blart = wa_final-blart .
    clb = tot .
    APPEND wa_exfi TO it_exfi .
    CLEAR: lv_tttext,wa_final,wa_exfi .
  ENDLOOP.

  ccc = cc1 .
  dcc = dc .
  de_c = tot_d. "#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
  cr_c = tot_c. "#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
  cl_c = clb. "#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
  c_opn = lv_opn . "#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation


  CONCATENATE 'Opening Balance(Rs)' 'Debit Items' 'Credit Items' 'Total Debit (Rs)' 'Total Credit (Rs)' 'Closing Bal (Rs)' 'Credit Limit (Rs)' INTO it_attach SEPARATED BY con_tab.
  CONCATENATE con_cret it_attach  INTO it_attach.
  APPEND it_attach.

  CLEAR: creditlimit.
  CALL FUNCTION 'CREDIT_EXPOSURE'
    EXPORTING
      kkber       = lv_buk
      kunnr       = wa_kna1-kunnr
    IMPORTING
      creditlimit = creditlimit.

  crl_c = creditlimit . "#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation

  CONCATENATE c_opn dcc ccc de_c cr_c cl_c crl_c INTO it_attach SEPARATED BY con_tab .
  CONCATENATE con_cret it_attach  INTO it_attach.
  APPEND it_attach.

  CLEAR: c_opn , dcc , ccc , de_c , cr_c , cl_c , crl_c .

  CONCATENATE ' ' ' ' INTO it_attach SEPARATED BY con_tab .
  CONCATENATE con_cret it_attach  INTO it_attach.
  APPEND it_attach.

  CONCATENATE 'Transaction Date' 'Transaction Type' 'Transaction Number' 'Reference' 'Text' 'Debit Amount' 'Credit Amount' 'Balance Amount' INTO it_attach SEPARATED BY con_tab .
  CONCATENATE con_cret it_attach  INTO it_attach.
  APPEND it_attach.

  CLEAR: c_opn.
  IF lv_opn > 0.
    c_opn = lv_opn . "#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation

    CONCATENATE ' ' ' ' 'Opening balance... ' ' ' ' ' c_opn ' ' c_opn INTO it_attach SEPARATED BY con_tab .
    CONCATENATE con_cret it_attach  INTO it_attach.
    APPEND it_attach.

  ELSE.
    t_opn = -1 * lv_opn.
    MOVE t_opn TO c_opn. "#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation

    CONCATENATE '-' c_opn INTO c_opn.
    CONCATENATE ' ' ' ' 'Opening balance... ' ' ' ' ' c_opn c_opn INTO it_attach SEPARATED BY con_tab .
    CONCATENATE con_cret it_attach  INTO it_attach.
    APPEND it_attach.

  ENDIF.

  cc = collection. "#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
  ic = invoice . "#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
  IF wa_exfi-blart = 'DG'.
    cnc = creditnote. "#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
  ELSEIF wa_exfi-blart = 'DR'.
    cnc = debitnote. "#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
  ENDIF.

  LOOP AT it_exfi INTO wa_exfi.
    SHIFT wa_exfi-doc_dat LEFT DELETING LEADING space.
    SHIFT wa_exfi-ttt  LEFT DELETING LEADING space.
    SHIFT wa_exfi-belnr LEFT DELETING LEADING space.
    SHIFT wa_exfi-xblnr LEFT DELETING LEADING space.
    de_c = wa_exfi-d_amt . "#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
    cr_c = wa_exfi-c_amt . "#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
    CONCATENATE '-' cr_c  INTO cr_c.
    IF wa_exfi-b_amt < 0.
      wa_exfi-b_amt = -1 * wa_exfi-b_amt .
      cl_c = wa_exfi-b_amt . "#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
      CONCATENATE '-' cl_c INTO cl_c .
    ELSE.
      cl_c = wa_exfi-b_amt . "#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
    ENDIF.

    CONCATENATE wa_exfi-doc_dat wa_exfi-ttt wa_exfi-belnr wa_exfi-xblnr wa_exfi-sgtxt de_c cr_c cl_c INTO it_attach SEPARATED BY con_tab .
    CONCATENATE con_cret it_attach  INTO it_attach.
    APPEND it_attach.

    CLEAR : de_c , cr_c , cl_c , wa_exfi.

  ENDLOOP.

  CONCATENATE ' ' ' ' INTO it_attach SEPARATED BY con_tab .
  CONCATENATE con_cret it_attach  INTO it_attach.
  APPEND it_attach.

  CONCATENATE 'Document Summary:' ' ' INTO it_attach SEPARATED BY con_tab .
  CONCATENATE con_cret it_attach  INTO it_attach.
  APPEND it_attach.

  CONCATENATE ' ' ' ' INTO it_attach SEPARATED BY con_tab .
  CONCATENATE con_cret it_attach  INTO it_attach.
  APPEND it_attach.

  CONCATENATE 'Document Description:' 'Debit Amount (Rs.)' 'Credit Amount (Rs.)' INTO it_attach SEPARATED BY con_tab .
  CONCATENATE con_cret it_attach  INTO it_attach.
  APPEND it_attach.

  CONCATENATE 'Collection:' ' ' cc INTO it_attach SEPARATED BY con_tab .
  CONCATENATE con_cret it_attach  INTO it_attach.
  APPEND it_attach.
  IF wa_exfi-blart = 'DG'.
    CONCATENATE 'Credit Note:' ' ' cnc INTO it_attach SEPARATED BY con_tab .
    CONCATENATE con_cret it_attach  INTO it_attach.
    APPEND it_attach.
  ELSEIF wa_exfi-blart = 'DR'.
    CONCATENATE 'Credit Note:' cnc  '' INTO it_attach SEPARATED BY con_tab .
    CONCATENATE con_cret it_attach  INTO it_attach.
    APPEND it_attach.
  ENDIF.
  CONCATENATE 'Invoice:' ic '' INTO it_attach SEPARATED BY con_tab .
  CONCATENATE con_cret it_attach  INTO it_attach.
  APPEND it_attach.

  CONCATENATE ' ' ' ' INTO it_attach SEPARATED BY con_tab .
  CONCATENATE con_cret it_attach  INTO it_attach.
  APPEND it_attach .

  CONCATENATE 'Credit Note:' ' ' INTO it_attach SEPARATED BY con_tab .
  CONCATENATE con_cret it_attach  INTO it_attach.
  APPEND it_attach .

  CONCATENATE ' ' ' ' INTO it_attach SEPARATED BY con_tab .
  CONCATENATE con_cret it_attach  INTO it_attach.
  APPEND it_attach .

  CONCATENATE 'Document Number' 'Document Date' 'Amount (Rs.)' 'Credit Note Type' 'Text' INTO it_attach SEPARATED BY con_tab .
  CONCATENATE con_cret it_attach  INTO it_attach.
  APPEND it_attach.
  LOOP AT it_exfi INTO wa_exfi WHERE blart = 'DG' OR blart = 'DR' .
    IF wa_exfi IS NOT INITIAL.

      IF wa_exfi-blart = 'DG'.
        creditnoteamt  = wa_exfi-c_amt . "#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
        SHIFT wa_exfi-belnr LEFT DELETING LEADING space.
        SHIFT wa_exfi-doc_dat LEFT DELETING LEADING space.
        SHIFT wa_exfi-ttt LEFT DELETING LEADING space.
        SHIFT wa_exfi-sgtxt LEFT DELETING LEADING space.

        CONCATENATE wa_exfi-belnr wa_exfi-doc_dat creditnoteamt  wa_exfi-ttt wa_exfi-sgtxt  INTO it_attach SEPARATED BY con_tab .
        CONCATENATE con_cret it_attach  INTO it_attach.
        APPEND it_attach .
      ELSEIF wa_exfi-blart = 'DR'.
        creditnoteamt  = wa_exfi-d_amt . "#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
        SHIFT wa_exfi-belnr LEFT DELETING LEADING space.
        SHIFT wa_exfi-doc_dat LEFT DELETING LEADING space.
        SHIFT wa_exfi-ttt LEFT DELETING LEADING space.
        SHIFT wa_exfi-sgtxt LEFT DELETING LEADING space.

        CONCATENATE wa_exfi-belnr wa_exfi-doc_dat creditnoteamt  wa_exfi-ttt wa_exfi-sgtxt  INTO it_attach SEPARATED BY con_tab .
        CONCATENATE con_cret it_attach  INTO it_attach.
        APPEND it_attach .

      ENDIF.
    ENDIF.
  ENDLOOP.
  CONCATENATE ' ' ' ' INTO it_attach SEPARATED BY con_tab .
  CONCATENATE con_cret it_attach  INTO it_attach.
  APPEND it_attach .
  it_attach = 'NOTE:Kindly Confirm the same with in 15 days of else the balance as per our records is treated as correct'.
  CONCATENATE con_cret it_attach INTO it_attach.
  APPEND it_attach.
  CLEAR: wa_exfi .

  REFRESH: i_objbin_f[] ,i_objbin1 .
  LOOP AT it_attach.
    string_data = it_attach.
    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text   = string_data
      IMPORTING
        buffer = xstring_data
      EXCEPTIONS
        failed = 1
        OTHERS = 2.
    IF sy-subrc <> 0.
*       Implement suitable error handling here
    ENDIF.
    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = xstring_data
*       APPEND_TO_TABLE = ' '
      IMPORTING
        output_length = bytes
      TABLES
        binary_tab    = i_objbin1.
    APPEND: i_objbin1 TO i_objbin_f[].
  ENDLOOP.
  CLEAR : it_attach,string_data,xstring_data.
ENDFORM.                    " EXCEL_ATTACH
