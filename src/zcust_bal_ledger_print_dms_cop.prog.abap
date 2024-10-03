*=======================================================================
*  Author                    : T.Pandiarajan
*
*  Date                      : 07.03.2024
*
*  Requester Name            : Ramakrishnan
*
*  Business Logic            : Report for generate the pdf for retailer account statement - DMS
*
*  Released on Date          :
*
* Hardcoded                  : company code  - 'DMS1';
*=======================================================================

REPORT zcust_bal_ledger_print_dms_cop.

TABLES:bsid,kna1.

TYPES: BEGIN OF gs_kna1,
         kunnr TYPE kna1-kunnr,                  " Customer Number
         name1 TYPE kna1-name1,                  " Customer Name
         adrnr TYPE kna1-adrnr,                  " Address Number
         lifnr TYPE kna1-lifnr,                  " Vendor Code
       END OF gs_kna1.

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

Data: gt_bsid_new  TYPE TABLE OF gs_bsid.

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

DATA: lv_ind   TYPE i,
      lv_index TYPE sy-tabix.

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
         sgtxt    TYPE char100, "bsid-sgtxt,                  " Text OR Remarks
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
         cputm    TYPE cputm,                  " created time
         cpudt    TYPE cpudt,                  " created date

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
*DATA: s_email1 TYPE  adr6-smtp_addr VALUE 'prasath@sphinaxinfosystems.com'.

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

DATA: p_email   TYPE somlreci1-receiver
                                VALUE 'prasath@sphinaxinfosystems.com'.

DATA: lv_bin_value TYPE xstring,
      lv_string    TYPE string,
      it_lines     TYPE TABLE OF tline,
      it_otf       TYPE TABLE OF itcoo.

DATA : lv_bsad  TYPE sy-tabix,
       lv_bsid  TYPE sy-tabix,
       lv_bsad1 TYPE sy-tabix,
       lv_bsid1 TYPE sy-tabix.

**********range table for belnr***********
TYPES: t_belnr TYPE RANGE OF belnr.

DATA l_round_off TYPE i.
********************************* PR@$@TH
********************************************************************** Zakir
TYPES:BEGIN OF ty_dist,
        name        TYPE string,
        street      TYPE adrc-street,
        city        TYPE string,
        phone       TYPE adrc-tel_number,
        postal_code TYPE adrc-post_code1,
      END OF ty_dist,
      BEGIN OF ty_dealer,
        name        TYPE string,
        street      TYPE adrc-street,
        city        TYPE string,
        phone       TYPE adrc-tel_number,
        postal_code TYPE adrc-post_code1,
      END OF ty_dealer.

DATA: lv_dist  TYPE ty_dist,
      lv_dealr TYPE ty_dealer.
**********************************************************************  Zakir
SELECTION-SCREEN: BEGIN OF BLOCK email_rep WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_dist LIKE kna1-kunnr OBLIGATORY.           "distributor code
  PARAMETERS: p_reta LIKE kna1-kunnr OBLIGATORY.           "Retailer code
  PARAMETERS: so_bukrs LIKE knb1-bukrs.                     "company code
  SELECT-OPTIONS: so_budat FOR bsid-budat.                  "date
  PARAMETERS: r1 RADIOBUTTON GROUP fg.                      "pdf
  PARAMETERS: r2 RADIOBUTTON GROUP fg.                      "excel
  PARAMETERS: p_c1 AS CHECKBOX .                            "get the PDF string
SELECTION-SCREEN: END OF BLOCK email_rep.

INITIALIZATION.

  LOOP AT SCREEN.
    IF screen-name = 'P_C1'.
      screen-active = 1.
      screen-invisible = 1.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

  so_bukrs = 'DMS1'.
  cur_dat = sy-datum.
  DATA: id_par_days	 TYPE t009b-butag,
        id_par_month TYPE t009b-bumon,
        id_par_year	 TYPE t009b-bdatj.

  id_par_month =  cur_dat+4(2) .
  id_par_year = cur_dat(4) .

  IF cur_dat+4(2) = 01 .
    id_par_year = id_par_year - 1 .

    CONCATENATE  id_par_year'1201' INTO so_budat-low .
    so_budat-high = sy-datum.
*    CONCATENATE id_par_year'1231' INTO so_budat-high .
  ELSE.
    id_par_month = id_par_month - 1 .

    CALL FUNCTION 'NUMBER_OF_DAYS_PER_MONTH_GET'
      EXPORTING
        par_month = id_par_month
        par_year  = id_par_year
      IMPORTING
        par_days  = id_par_days.

    CONCATENATE  id_par_year id_par_month '01' INTO so_budat-low .
*    CONCATENATE id_par_year id_par_month id_par_days INTO so_budat-high .
    so_budat-high = sy-datum.
  ENDIF.
  APPEND: so_budat.

START-OF-SELECTION.

***************get the business area for the distributor**************
  SELECT SINGLE werks FROM kna1 INTO @DATA(lv_gsber) WHERE kunnr = @p_dist.

  SELECT kunnr bukrs FROM knb1
    INTO TABLE z_knb1
    WHERE bukrs EQ so_bukrs
*    AND loevm <> 'X'
     AND kunnr EQ p_reta.

  SELECT kunnr name1 adrnr FROM kna1
    INTO TABLE z_kna1
    FOR ALL ENTRIES IN z_knb1
     WHERE kunnr EQ z_knb1-kunnr.


  SORT z_knb1 BY kunnr.
  IF z_knb1 IS NOT INITIAL.

    LOOP AT z_knb1 INTO y_knb1.
      CLEAR: i_otf[],i_tline[],i_record[],i_xstring,i_objpack[],i_objtxt[],i_objbin[],i_reclist[],wa_objhead[],w_ctrlop,w_compop,w_return,
      wa_buffer,v_len_in.

      READ TABLE z_kna1 INTO y_kna1 WITH KEY kunnr = y_knb1-kunnr.
      CLEAR: sm_name,sts.
      sm_name = y_kna1-name1.
      sts = y_kna1-kunnr.

      CLEAR: lv_buk.
      SELECT SINGLE bukrs  FROM t001
        INTO lv_buk
        WHERE bukrs EQ y_knb1-bukrs.
**********************BSAD*******************
      CLEAR: gt_bsad.
      SELECT a~prctr  a~kunnr a~umskz a~augbl a~budat a~bldat a~blart
             a~bschl  a~zfbdt a~dmbtr a~shkzg a~xblnr a~belnr b~rbusa a~sgtxt
             a~vbeln a~bukrs a~gjahr a~sknto a~umsks a~augdt a~zuonr a~buzei
             FROM bsad AS a INNER JOIN faglflexa AS b
             ON  a~gjahr = b~ryear
             AND a~belnr = b~docnr
             INTO CORRESPONDING FIELDS OF TABLE gt_bsad
             WHERE a~budat <= so_budat-high
             AND a~budat >= so_budat-low
             AND a~bukrs EQ y_knb1-bukrs
             AND a~kunnr EQ y_knb1-kunnr
             AND a~umskz <> 'H'
             AND b~rldnr  = '0L'
             AND b~rbukrs = y_knb1-bukrs
             AND b~docln  = '000001'
             AND b~rbusa  = lv_gsber.

**********************BSID*******************
      CLEAR: gt_bsid.
      SELECT a~prctr  a~kunnr a~augbl a~budat a~bldat a~blart
             a~bschl  a~zfbdt a~dmbtr a~shkzg a~xblnr a~belnr b~rbusa a~sgtxt
             a~vbeln  a~umskz a~bukrs a~gjahr a~sknto
             FROM bsid AS a INNER JOIN faglflexa AS b
             ON  a~gjahr = b~ryear
             AND a~belnr = b~docnr
             INTO CORRESPONDING FIELDS OF TABLE gt_bsid
             WHERE a~budat <= so_budat-high
             AND a~budat >= so_budat-low
             AND a~bukrs EQ y_knb1-bukrs
             AND a~kunnr EQ y_knb1-kunnr
             AND a~umskz <> 'H'
             AND b~rldnr  = '0L'
             AND b~rbukrs = y_knb1-bukrs
             AND b~docln  = '000001'
             AND b~rbusa  = lv_gsber.

******************************BSAD 2 times*************************

      CLEAR:gt_bsad1.
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
             AND   b~rbusa  = lv_gsber.

******************************BSID 2 times*************************

      CLEAR: gt_bsid1.
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
             AND  b~rbusa  = lv_gsber.
**********************Excess payment removal process*************
      SELECT * FROM zdms_dz_removal INTO TABLE @DATA(lt_delete)
                                    WHERE distributor = @p_dist
                                    AND   dealer      = @p_reta.
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



      DATA: l_total_amt   TYPE bapidmbtr,
            l_total_amtn  TYPE bapidmbtr,
            l_debit_total TYPE bapidmbtr,
            l_debit_Rem   TYPE bapidmbtr,
            l_final_bal   TYPE bapidmbtr.


      refresh: gt_bsid_new.
      SELECT a~prctr  a~kunnr a~augbl a~budat a~bldat a~blart
             a~bschl  a~zfbdt a~dmbtr a~shkzg a~xblnr a~belnr b~rbusa a~sgtxt
             a~vbeln  a~umskz a~bukrs a~gjahr a~sknto
             FROM bsid AS a INNER JOIN faglflexa AS b
             ON  a~gjahr = b~ryear
             AND a~belnr = b~docnr
             INTO CORRESPONDING FIELDS OF TABLE gt_bsid_new
             WHERE a~bukrs EQ y_knb1-bukrs
             AND a~kunnr EQ y_knb1-kunnr
             AND a~umskz <> 'H'
             AND b~rldnr  = '0L'
             AND b~rbukrs = y_knb1-bukrs
             AND b~docln  = '000001'
             AND b~rbusa  = lv_gsber.


      IF lt_delete[] is NOT INITIAL.
        LOOP AT gt_bsid_new ASSIGNING FIELD-SYMBOL(<fs_bsid_new>).
          READ TABLE lt_delete TRANSPORTING NO FIELDS WITH KEY belnr = <fs_bsid_new>-belnr gjahr = <fs_bsid_new>-gjahr.
          IF sy-subrc = 0.
            <fs_bsid_new>-flag = 'X'.
          ENDIF.
        ENDLOOP.
        DELETE gt_bsid_new WHERE flag = 'X'.
      ENDIF.

*Calulating Available Credit Balance
      LOOP AT gt_bsid_new INTO DATA(lw_bsid) WHERE shkzg = 'H'. " H-Credit amount from Sub-Dealers
        IF lw_bsid-blart = 'VG'.
* sales return amount should not be part of the credit note calculation
          CONTINUE.
        ENDIF.

        IF lw_bsid-blart = 'RV'.
* Invoice Cancellation amount should not be part of the credit note calculation
          CONTINUE.
        ENDIF.

*round up all the credit items. This is due to the Migration roundoff error
        CLEAR l_round_off.
        l_round_off = ceil( lw_bsid-dmbtr ).
        lw_bsid-dmbtr = l_round_off.

        l_total_amt = l_total_amt + lw_bsid-dmbtr.
      ENDLOOP.

*Calculating Available debit Balance

      CLEAR: l_debit_total, l_total_amtn.
      LOOP AT gt_bsid_new INTO lw_bsid WHERE shkzg = 'S'. " S-Debit amount from Sub-Dealers

        CLEAR l_round_off.
        l_round_off = lw_bsid-dmbtr.
        lw_bsid-dmbtr = l_round_off.

        IF lw_bsid-blart = 'RV'.
          IF lw_bsid-budat >= '20240401'.
            SELECT SINGLE vbeln, fksto FROM vbrk INTO @DATA(ls_invcan) WHERE vbeln = @lw_bsid-xblnr.
            IF sy-subrc = 0. " AND ls_invcan-fksto = abap_true.
              IF ls_invcan-fksto = abap_true.
*Invoice already cancelled so they should not be considered to check Credit notes
                CONTINUE.
              ENDIF.

* check if there is a sales return of this invoice
              SELECT SINGLE ref_invoiceno,invoice_no, accounting_doc INTO @DATA(lv_Sretinv) FROM zsd_retrn_hd_dms
                                              WHERE ref_invoiceno = @lw_bsid-xblnr.
              IF sy-subrc = 0.
*Original Invoice Sales return already done so removed from credit note calculation
                CONTINUE.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.


*do credit debit adjustment
        IF l_total_amt GE lw_bsid-dmbtr.
          l_total_amt = l_total_amt - lw_bsid-dmbtr.
        ELSE.
          IF l_total_amt > 0.
            CLEAR l_total_amtn.
            l_total_amtn = l_total_amt.
          ENDIF.
          CLEAR l_total_amt.

          l_debit_total = l_debit_total + lw_bsid-dmbtr.
        ENDIF.
      ENDLOOP.

*if no invoices or debitnotes to adjust then Credit balance will be existing so move to
* l_total_amtn for display
      IF l_total_amt > 0.
        CLEAR l_total_amtn.
        l_total_amtn = l_total_amt.
        CLEAR l_total_amt.
      ENDIF.

      SELECT * FROM zdms_retail_acnt
               INTO TABLE @DATA(lt_zdms_retail_acnt)
             WHERE distributor = @p_dist
               AND retailer = @p_reta.
      IF sy-subrc = 0.
        SORT lt_zdms_retail_acnt BY distributor retailer pos_date.
      ENDIF.

      SELECT * FROM zfi_dms_cust_pay INTO TABLE @DATA(lt_dms_cust_pay) WHERE kunnr = @p_reta.

      CLEAR:gt_bsad7.
      APPEND LINES OF gt_bsid TO gtt_bsid.
      SORT gt_bsad BY belnr augbl.
      APPEND LINES OF gt_bsad TO gt_bsid.
      APPEND LINES OF gt_bsad TO gt_bsad7.
      DELETE gt_bsad7 WHERE shkzg = 'S'.
      APPEND LINES OF gt_bsad1 TO gt_bsid1.

      REFRESH: gt_kna1.
      SELECT kunnr name1 adrnr lifnr FROM kna1
        INTO TABLE gt_kna1 WHERE kunnr = p_reta.

      IF gt_kna1[] IS  NOT INITIAL.
        CLEAR: gt_adrc.
        SELECT  addrnumber street city1 city2 post_code1 FROM adrc
          INTO TABLE gt_adrc
          FOR ALL ENTRIES IN gt_kna1
          WHERE addrnumber = gt_kna1-adrnr.

      ENDIF.

      REFRESH: gt_t003t.
      SELECT blart ltext spras FROM  t003t "#EC CI_NOORDER  "Added by SPLABAP during code remediation
        INTO TABLE gt_t003t
        WHERE spras = 'E'.


      LOOP AT gtt_bsid INTO wat_bsid.

*        CLEAR l_round_off.
*        l_round_off = wat_bsid-dmbtr.
*        wat_bsid-dmbtr = l_round_off.

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

*        CLEAR l_round_off.
*        l_round_off = wa_bsid1-dmbtr.
*        wa_bsid1-dmbtr = l_round_off.

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

*        IF sy-subrc = 0.
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

*        ENDIF.

*        CLEAR l_round_off.
*        l_round_off = wa_bsid-dmbtr.
*        wa_bsid-dmbtr = l_round_off.

        READ TABLE gt_t003t INTO wa_t003t WITH KEY blart = wa_bsid-blart.
        wa_final-ltext = wa_t003t-ltext.
        wa_final-blart = wa_bsid-blart.

        READ TABLE gt_kna1 INTO wa_kna1 WITH KEY kunnr = wa_bsid-kunnr.
        wa_final-name1 = wa_kna1-name1.
        wa_final-shkzg = wa_bsid-shkzg.


        IF gs_chk-budat = wa_bsid-budat AND gs_chk-blart = wa_bsid-blart AND gs_chk-belnr = wa_bsid-belnr
          AND gs_chk-shkzg = wa_bsid-shkzg AND wa_bsid-shkzg = 'H'.

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

        ELSEIF gs_chk-budat = wa_bsid-budat AND gs_chk-blart = wa_bsid-blart AND gs_chk-belnr = wa_bsid-belnr
          AND gs_chk-shkzg = wa_bsid-shkzg AND wa_bsid-shkzg = 'C' AND wa_t003t-blart = 'AB'.

          wa_final-cr_amt = lv_cramt + wa_bsid-dmbtr.
          lv_cramt = wa_final-cr_amt.

          DELETE gt_final INDEX lv_ind.

        ELSEIF gs_chk-budat = wa_bsid-budat AND gs_chk-blart = wa_bsid-blart AND gs_chk-belnr = wa_bsid-belnr
          AND wa_bsid-shkzg = 'D' AND wa_t003t-blart = 'AB'.

          wa_final-dr_amt = lv_dbamt + wa_bsid-dmbtr.
          lv_dbamt = wa_final-dr_amt.

          DELETE gt_final INDEX lv_ind.

        ELSEIF ( gs_chk-budat <> wa_bsid-budat AND gs_chk-blart <> wa_bsid-blart AND gs_chk-belnr <> wa_bsid-belnr
                  AND wa_bsid-shkzg <> 'S' )
            OR ( ( gs_chk-budat <> wa_bsid-budat OR gs_chk-blart <> wa_bsid-blart OR gs_chk-belnr <> wa_bsid-belnr )
                AND wa_bsid-shkzg <> 'S' )
            OR ( gs_chk-shkzg <> wa_bsid-shkzg AND wa_bsid-shkzg <> 'S' ) .

          wa_final-sknto = wa_bsid-sknto.
          lv_cdisc = wa_final-sknto .
          wa_final-cr_amt = wa_bsid-dmbtr.
          lv_cramt = wa_final-cr_amt .

        ELSEIF ( gs_chk-budat <> wa_bsid-budat AND gs_chk-blart <> wa_bsid-blart AND gs_chk-belnr <> wa_bsid-belnr
                AND wa_bsid-shkzg <> 'H' )
            OR ( ( gs_chk-budat <> wa_bsid-budat OR gs_chk-blart <> wa_bsid-blart OR gs_chk-belnr <> wa_bsid-belnr )
               AND wa_bsid-shkzg <> 'H' )
           OR ( gs_chk-shkzg <> wa_bsid-shkzg AND wa_bsid-shkzg <> 'H' ) .

          wa_final-dr_amt = wa_bsid-dmbtr .
          lv_dbamt = wa_final-dr_amt  .

        ELSEIF ( gs_chk-budat <> wa_bsid-budat AND gs_chk-blart <> wa_bsid-blart AND gs_chk-belnr <> wa_bsid-belnr
                AND gs_chk-shkzg <> wa_bsid-shkzg AND wa_bsid-shkzg = 'C' ) OR  wa_t003t-blart = 'AB'.

          wa_final-cr_amt = wa_bsid-dmbtr.
          lv_cramt = wa_final-cr_amt.

        ELSEIF ( gs_chk-budat <> wa_bsid-budat AND gs_chk-blart <> wa_bsid-blart AND gs_chk-belnr <> wa_bsid-belnr
             AND gs_chk-shkzg <> wa_bsid-shkzg AND wa_bsid-shkzg = 'D') OR wa_t003t-blart = 'AB'.

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

      CLEAR: gt_bsid[].
      CLEAR: gs_chk ,wa_bsid, wa_t003t, wa_kna1 .

      APPEND LINES OF gt_final TO gt_final1.
      APPEND LINES OF gt_final TO gt_final2.



      TYPES: BEGIN OF a,
               ln TYPE i,
             END OF a.
      DATA: gt_line TYPE TABLE OF a,
            gs_line TYPE a.

      LOOP AT gt_final1 INTO wa_final1.

        IF gs_chk-budat = wa_final1-budat AND
             gs_chk-blart = wa_final1-blart AND
             gs_chk-belnr = wa_final1-belnr.
          IF wa_final1-cr_amt GT gs_chk-cr_amt.
            gs_line-ln = sy-tabix.
          ENDIF.
        ENDIF.

        gs_chk-belnr = wa_final1-belnr.
        gs_chk-budat = wa_final1-budat.
        gs_chk-blart = wa_final1-blart.
        gs_chk-cr_amt = wa_final1-cr_amt.
        gs_chk-dr_amt = wa_final1-dr_amt.

      ENDLOOP.

      b_date = so_budat-high.
      SORT gt_final BY budat shkzg.

      LOOP AT gt_final INTO wa_final.


        READ TABLE gt_final2 INTO wa_final2 WITH KEY belnr = wa_final-belnr
                                                     shkzg = wa_final-shkzg .

        IF wa_final2-sknto IS NOT INITIAL .

          wa_final2-cr_amt = wa_final2-cr_amt - wa_final2-sknto.

          APPEND wa_final2 TO gt_final3.
          CLEAR: wa_final2-cr_amt.

          wa_final2-cr_amt = wa_final2-sknto.
          wa_final2-lv_count = 1.
          APPEND wa_final2 TO gt_final3.
          DELETE gt_final WHERE belnr = wa_final2-belnr AND shkzg = 'H' .
        ENDIF.

      ENDLOOP.

      APPEND LINES OF gt_final3 TO gt_final.

      LOOP AT gt_final INTO wa_final.

        lv_sum = lv_sum + wa_final-dr_amt - wa_final-cr_amt .
        wa_final-bal_amt = lv_sum.

        MODIFY gt_final FROM wa_final TRANSPORTING bal_amt.

        AT END OF kunnr.
          CLEAR :lv_sum,wa_final,wa_bsid.
        ENDAT.

      ENDLOOP.

      LOOP AT gt_final INTO wa_final WHERE kunnr = ' '.
        wa_final-kunnr = wa_kna1-kunnr.

        MODIFY gt_final FROM wa_final TRANSPORTING kunnr.
        CLEAR  wa_final.

      ENDLOOP.

*      CLEAR: rv_opn.
*      rv_opn = lv_total - lv_opn.
      SORT gt_final BY budat shkzg.
***************************get the created date & time for the document sorting*************
      IF gt_final[] IS NOT INITIAL.
        SELECT belnr,gjahr,cpudt,cputm FROM bkpf INTO TABLE @DATA(lt_bkpf)
                                                 FOR ALL ENTRIES IN @gt_final
                                                 WHERE bukrs EQ 'DMS1'
                                                 AND   belnr EQ @gt_final-belnr
                                                 AND   gjahr EQ @gt_final-gjahr.
        IF sy-subrc = 0.
          SORT : lt_bkpf BY belnr gjahr.
        ENDIF.
      ENDIF.

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

        ENDIF.

****************read the time from bkpf************
        READ TABLE lt_bkpf INTO DATA(ls_bkpf) WITH KEY belnr = wa_final-belnr
                                                       gjahr = wa_final-gjahr BINARY SEARCH.
        IF sy-subrc = 0.
          wa_final-cputm = ls_bkpf-cputm.
          wa_final-cpudt = ls_bkpf-cpudt.
        ENDIF.
        CLEAR : ls_bkpf.

        CASE wa_final-blart.
          WHEN 'RV'.
            IF wa_final-shkzg = 'H'.
              wa_final-t_tttext = 'Cancel Invoice'.
            ELSE.
              wa_final-t_tttext = 'Sales Invoice'.

              IF wa_final-budat <= '20240331'.
                READ TABLE lt_zdms_retail_acnt INTO DATA(ls_dmsretacnt)
                  WITH KEY distributor = p_dist retailer = p_reta ref_doc = wa_final-xblnr.
                IF sy-subrc = 0.
                  wa_final-xblnr = ls_dmsretacnt-invoice.
                ENDIF.
              ENDIF.
            ENDIF.

          WHEN 'DG'.
            wa_final-t_tttext = 'Credit Note'.
          WHEN 'DZ'.
            IF wa_final-lv_count IS NOT INITIAL.
              wa_final-t_tttext = 'AUTO CREDIT NOTE'.
              CLEAR wa_final-lv_count.
            ELSE.
              wa_final-t_tttext = 'Payment Recd'.
            ENDIF.

            READ TABLE lt_dms_cust_pay INTO DATA(ls_custpay) WITH KEY kunnr = p_reta
                                                                      distributor = p_dist
                                                                      xblnr = wa_final-xblnr.
            IF sy-subrc = 0.
              IF ls_custpay-db_remarks IS NOT INITIAL. "remarks replaced to db_remarks
                wa_final-sgtxt = ls_custpay-db_remarks. "remarks replaced to db_remarks
              ENDIF.
            ENDIF.

          WHEN 'VG'.
            wa_final-t_tttext = 'Sales Return'.
            IF wa_final-budat <= '20240331'.
              CLEAR ls_dmsretacnt.
              READ TABLE lt_zdms_retail_acnt INTO ls_dmsretacnt
                WITH KEY distributor = p_dist retailer = p_reta ref_doc = wa_final-xblnr.
              IF sy-subrc = 0.
                wa_final-xblnr = ls_dmsretacnt-invoice.
              ENDIF.
            ELSEIF wa_final-budat GE '20240401'.
              wa_final-xblnr = wa_final-vbeln.
              SELECT SINGLE ref_invoiceno FROM zsd_retrn_hd_dms
                                          INTO @DATA(ls_refinv)
                                          WHERE invoice_no = @wa_final-vbeln.
              IF sy-subrc = 0.
                wa_final-sgtxt = |Returns for sales invoice - { ls_refinv }|.
              ENDIF.
              CLEAR : ls_refinv.
            ENDIF.
          WHEN 'DA'.
            wa_final-t_tttext = 'CHEQUE RETURN'.
          WHEN 'DR'.
            wa_final-t_tttext = 'Debit Note'.
          WHEN 'AB'.
            CASE wa_final-bschl .
              WHEN 40.
                wa_final-t_tttext = 'AUTO CREDIT NOTE'.
              WHEN 17.
                IF wa_final-sknto IS NOT INITIAL.
                  wa_final-t_tttext = 'AUTO CREDIT NOTE'.
                  CLEAR wa_final-sknto.
                ELSE.
                  wa_final-t_tttext = 'FREIGHT / INTEREST'.
                ENDIF.
              WHEN 27.
                wa_final-t_tttext = 'FREIGHT / INTEREST'.
              WHEN 04.
                wa_final-t_tttext = 'FREIGHT / INTEREST'.
              WHEN OTHERS.
                wa_final-t_tttext = 'ACCOUNTING DOCUMENT'.
            ENDCASE.
          WHEN 'KR'.
            wa_final-t_tttext = 'COMMISSION'.
          WHEN 'SA'.
            wa_final-t_tttext = 'JOURNAL ENTRY'.
          WHEN OTHERS.
            lv_flag2 = abap_true.
        ENDCASE.
        IF lv_flag2 IS INITIAL.

*do the round off for credit amounts
          CLEAR l_round_off.
          l_round_off = wa_final-cr_amt.
          wa_final-cr_amt = l_round_off.

*do the round off for debit amounts
          CLEAR l_round_off.
          l_round_off = wa_final-dr_amt.
          wa_final-dr_amt = l_round_off.

          MODIFY gt_final FROM wa_final INDEX lv_ind2.

        ENDIF.

        CLEAR lv_flag2.
*** created on 17.09.2020
        CLEAR:wa_final.
      ENDLOOP.

*do the round off for opening balace
      CLEAR l_round_off.
      l_round_off = lv_opn.
      lv_opn = l_round_off.


      IF gt_final[] IS NOT INITIAL OR lv_opn IS NOT INITIAL.
*******sorting by posting date created date & time
        SORT : gt_final BY budat cpudt cputm.
        IF lv_opn < 0.
          lv_opn = lv_opn * -1.
          DATA(cr_amt1) = lv_opn.
        ELSE.
          DATA(dr_amt1) = lv_opn.
        ENDIF.
        DATA(ls_opnitm) = VALUE gs_final( bldat = so_budat-low cr_amt = cr_amt1 dr_amt = dr_amt1  sgtxt = 'Opening Balance... ... ...' ).
        INSERT ls_opnitm INTO gt_final INDEX 1.

        IF r1 IS NOT INITIAL.
          PERFORM data_to_adf .
*          CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
*            EXPORTING
*              formname           = 'ZSF_CUS_ACCOUNT_LEDGER_DMS'
**             VARIANT            = ' '
**             DIRECT_CALL        = ' '
*            IMPORTING
*              fm_name            = fm_name
*            EXCEPTIONS
*              no_form            = 1
*              no_function_module = 2
*              OTHERS             = 3.
*          IF sy-subrc <> 0.
*            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*          ENDIF.
**added By samsudeen M ON 23.08.2023
*          IF p_c1 EQ abap_true.
**Control Parameters for Smartforms
*            w_ctrlop-no_dialog  = abap_true.
*            w_ctrlop-preview    = space.
*            w_ctrlop-getotf     = abap_true.
*          ENDIF.
**Added By Samsudeen M on 23.08.2023
*          CALL FUNCTION fm_name "'/1BCDWB/SF00000509'
*            EXPORTING
**             ARCHIVE_INDEX      =
**             ARCHIVE_INDEX_TAB  =
**             ARCHIVE_PARAMETERS =
*              control_parameters = w_ctrlop
**             MAIL_APPL_OBJ      =
**             MAIL_RECIPIENT     =
**             MAIL_SENDER        =
**             output_options     = w_compop
**             USER_SETTINGS      = ABAP_TRUE "'X'
*              bal_date           = b_date
*              lv_frdt            = so_budat-low
*              lv_todt            = so_budat-high
*              lv_opn             = lv_opn
*              lv_buk             = lv_buk
*              distributor        = p_dist
****            DEBIT_TOT                  = DBT_AMT
*            IMPORTING
**             DOCUMENT_OUTPUT_INFO       =
*              job_output_info    = w_return
**             JOB_OUTPUT_OPTIONS =
*            TABLES
*              gt_kna1            = gt_kna1[]
*              gt_bsid            = gt_final[]
*              itt_final          = itt_final[]
*            EXCEPTIONS
*              formatting_error   = 1
*              internal_error     = 2
*              send_error         = 3
*              user_canceled      = 4
*              OTHERS             = 5.
*          IF sy-subrc = 0.
**Added By Samsudeen M on 23.08.2023
*            IF p_c1 EQ abap_true.
*              CLEAR: lv_bin_value, it_otf.
*              it_otf = w_return-otfdata[].
*              CALL FUNCTION 'CONVERT_OTF'
*                EXPORTING
*                  format                = 'PDF'
*                IMPORTING
*                  bin_file              = lv_bin_value
*                TABLES
*                  otf                   = it_otf
*                  lines                 = it_lines
*                EXCEPTIONS
*                  err_max_linewidth     = 1
*                  err_format            = 2
*                  err_conv_not_possible = 3
*                  err_bad_otf           = 4
*                  OTHERS                = 5.
*              IF sy-subrc = 0.
**          ep_xstring = lv_bin_value.
*                CLEAR lv_string.
*                CALL FUNCTION 'SCMS_BASE64_ENCODE_STR'
*                  EXPORTING
*                    input  = lv_bin_value
*                  IMPORTING
*                    output = lv_string.
**Added By Samsudeen M on 23.08.2023
*              ENDIF.
*              EXPORT lv_string FROM lv_string TO MEMORY ID 'ZCUST_DMS'.
*            ENDIF.
*          ENDIF.
*

        ELSEIF r2 IS NOT INITIAL.

          INCLUDE ole2incl.
          DATA: application TYPE ole2_object,
                workbook    TYPE ole2_object,
                sheet       TYPE ole2_object,
                cells       TYPE ole2_object,
                column      TYPE ole2_object,
                range       TYPE ole2_object,
                borders     TYPE ole2_object,
                h_cell      TYPE ole2_object.
          DATA : v_row TYPE int2.
          CONSTANTS: row_max TYPE i VALUE 256.

          DATA: adr TYPE string.
          DATA: regio TYPE  t005u-bezei.
          DATA: reg TYPE  kna1-regio.
          DATA: f_dat(10)   TYPE c,
                t_dat(10)   TYPE c,
                c_dat(10)   TYPE c,
                day_nam     TYPE t246-langt,  ""DTRESR-WEEKDAY,
                time        TYPE sy-uzeit,
                t_ty(2)     TYPE c,
                c_time(11)  TYPE c,
                detail(225) TYPE c.
          DATA: debitnote TYPE  bsid-dmbtr.
          DATA: invoice TYPE  bsid-dmbtr.
          DATA: creditnote  TYPE  bsid-dmbtr.
          DATA: collection  TYPE  bsid-dmbtr.
          DATA: lv_tttext(21) TYPE c.
          DATA: lv_belnr TYPE  bsid-belnr.
          DATA: lv_blart TYPE  bsid-blart.
          DATA: ttt(22) TYPE c.
          DATA: tot TYPE bsad-dmbtr.
          DATA: tot_d TYPE bsad-dmbtr.
          DATA: dc  TYPE i,  "DEBIT COUNT
                cc1 TYPE i.  "CREDIT COUNT
          DATA: tot_c TYPE bsad-dmbtr.
          DATA: doc_dat(10) TYPE c.
          DATA: clb TYPE bsad-dmbtr.
          DATA: creditlimit TYPE knkk-klimk. "#EC CI_USAGE_OK[2227014]"Added by SPLABAP during code remediation
          DATA: crl_c(15) TYPE c .
          DATA: c_opn(15) TYPE c.
          DATA: c(4)  TYPE c,
                tc(3) TYPE c,
                t     TYPE int2.
          DATA: fcc(4)    TYPE c,
                tcc(4)    TYPE c,
                tc1(3)    TYPE c,
                fc1(3)    TYPE c,
                t1        TYPE int2,
                f1        TYPE int2,
                dbt_amtlx TYPE bsid-dmbtr.

          TYPES: BEGIN OF ty_exfi,
                   doc_dat   TYPE bsid-budat,
                   ttt(21)   TYPE c,
                   belnr     TYPE bsid-belnr,
                   xblnr     TYPE bsid-xblnr,
                   sgtxt     TYPE bsid-sgtxt,
                   d_amt     TYPE bsid-dmbtr,
                   c_amt     TYPE bsid-dmbtr,
                   b_amt     TYPE bsid-dmbtr,
                   blart     TYPE bsid-blart,
                   dbt_amtlx TYPE bsid-dmbtr,
                 END OF ty_exfi.

          DATA: it_exfi  TYPE TABLE OF ty_exfi,
                wa_exfi  TYPE ty_exfi,
                it_exfi1 TYPE TABLE OF ty_exfi,
                wa_exfi1 TYPE ty_exfi.

          DATA: name(10) TYPE c.
          DATA: filln(60) TYPE c.

          DATA : salesoffice TYPE  knvv-vkbur,
                 so_nam      TYPE  tvkbt-bezei.

          CREATE OBJECT application 'excel.application'.
          SET PROPERTY OF application 'visible' = 1.

          CALL METHOD OF
            application
              'Workbooks' = workbook.
          CALL METHOD OF
            workbook
            'Add'.
          CALL METHOD OF
              application
              'Worksheets' = sheet
            EXPORTING
              #1           = 1.
          CALL METHOD OF
            sheet
            'Activate'.
          SET PROPERTY OF sheet 'Name' = 'Sheet1'.

          v_row = v_row + 1 .
          PERFORM fill_cell  USING  v_row 4 'Statement of Accounts'.

          v_row = v_row + 1 .
          PERFORM fill_cell USING v_row 1 ' '.

          v_row = v_row + 1 .
          PERFORM fill_cell USING v_row 1 ' '.

          LOOP AT gt_kna1 INTO wa_kna1.
            "Commented by SPLABAP during code remediation
*            SELECT SINGLE ADDRNUMBER STREET CITY1 CITY2 POST_CODE1 FROM ADRC
*              INTO WA_ADRC
*              WHERE ADDRNUMBER = WA_KNA1-ADRNR.
            "Added by SPLABAP during code remediation
            SELECT addrnumber street city1 city2 post_code1 FROM adrc
            INTO wa_adrc UP TO 1 ROWS
            WHERE addrnumber = wa_kna1-adrnr
              ORDER BY PRIMARY KEY.
            ENDSELECT.
            "Commented by SPLABAP during code remediation
*            SELECT SINGLE VKBUR FROM KNVV
*               INTO SALESOFFICE
*              WHERE KUNNR = WA_KNA1-KUNNR
*              AND VKORG = LV_BUK.
            "Added by SPLABAP during code remediation
            SELECT  vkbur FROM knvv UP TO 1 ROWS
             INTO salesoffice
            WHERE kunnr = wa_kna1-kunnr
            AND vkorg = lv_buk
              ORDER BY PRIMARY KEY.
            ENDSELECT.
            SELECT SINGLE bezei FROM tvkbt
              INTO so_nam
              WHERE vkbur = salesoffice
              AND spras = 'EN'.
            EXIT. "#EC CI_NOORDER  "Added by SPLABAP during code remediation
          ENDLOOP.

          v_row = v_row + 1 .
          PERFORM fill_cell USING v_row 1 'NAME'.
          PERFORM fill_cell USING v_row 2 wa_kna1-name1.
          PERFORM fill_cell USING v_row 7 'Dealer Code:'.
          PERFORM fill_cell USING v_row 8 wa_kna1-kunnr.
          name = wa_kna1-kunnr .


          v_row = v_row + 1 .
          PERFORM fill_cell USING v_row 1 'STREET'.
          PERFORM fill_cell USING v_row 2 wa_adrc-street.
          PERFORM fill_cell USING v_row 7 'Sales Office:'.
          PERFORM fill_cell USING v_row 8 so_nam.

          v_row = v_row + 1 .
          PERFORM fill_cell USING v_row 1 'CITY'.

          CONCATENATE wa_adrc-city1 wa_adrc-city2 '-' wa_adrc-post_code1 INTO adr .
          PERFORM fill_cell USING v_row 2 adr.
          SELECT SINGLE regio FROM kna1 INTO reg WHERE kunnr = wa_kna1-kunnr.
          SELECT SINGLE bezei FROM t005u INTO regio WHERE spras = 'EN' AND land1 = 'IN' AND bland = reg.
          PERFORM fill_cell USING v_row 7 'TERRITORY'.
          PERFORM fill_cell USING v_row 8 regio.

          v_row = v_row + 1 .
          PERFORM fill_cell USING v_row 7 'CURRENCY'.
          PERFORM fill_cell USING v_row 8 'INR'.

          v_row = v_row + 1 .
          PERFORM fill_cell USING v_row 1 ' '.

          CONCATENATE so_budat-low+6(2) '/' so_budat-low+4(2) '/' so_budat-low(4) INTO f_dat .
          CONCATENATE so_budat-high+6(2) '/' so_budat-high+4(2) '/' so_budat-high(4) INTO t_dat.
          CONCATENATE sy-datum+6(2) '/' sy-datum+4(2) '/' sy-datum(4) INTO c_dat.
          CLEAR:day_nam.

          CALL FUNCTION 'GET_WEEKDAY_NAME'
            EXPORTING
              date        = sy-datum
              language    = sy-langu
*             WEEKDAY_NUMBER       = ' '
            IMPORTING
*             LANGU_BACK  =
              longtext    = day_nam
*             SHORTTEXT   =
            EXCEPTIONS
              calendar_id = 1
              date_error  = 2
              not_found   = 3
              wrong_input = 4
              OTHERS      = 5.
          IF sy-subrc <> 0.
* Implement suitable error handling here
          ENDIF.
**          CALL FUNCTION 'DATE_TO_DAY'
**            EXPORTING
**              DATE    = SY-DATUM
**            IMPORTING
**              WEEKDAY = DAY_NAM.
          day_nam = day_nam(3).
          CLEAR: time , t_ty.
          CALL FUNCTION 'HRVE_CONVERT_TIME'
            EXPORTING
              type_time       = 'A'
              input_time      = sy-uzeit
*             INPUT_AM_PM     = 'AM'
            IMPORTING
              output_time     = time
              output_am_pm    = t_ty
            EXCEPTIONS
              parameter_error = 1
              OTHERS          = 2.
          CLEAR:c_time.
          CONCATENATE time(2)':' time+2(2) ':' time+4(2) ' ' t_ty INTO c_time.
          CONCATENATE 'Statement of the account for the period of ' f_dat ' to ' t_dat ' generated on ' day_nam ' ' c_dat ' at ' c_time INTO detail SEPARATED BY space.

          v_row = v_row + 1 .
          PERFORM fill_cell USING v_row 1 detail.

          v_row = v_row + 1 .
          PERFORM fill_cell USING v_row 1 ' '.
          tot = lv_opn .


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
            wa_exfi-doc_dat = wa_final-bldat.
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

          LOOP AT itt_final INTO wat_final.

            IF wat_final-shkzg = 'H'.
              dbt_amtlx =  dbt_amtlx + wat_final-cr_amt.
            ENDIF.
            CLEAR wat_final.

          ENDLOOP.

          CLEAR creditlimit.
*          CALL FUNCTION 'CREDIT_EXPOSURE'
*            EXPORTING
*              kkber       = lv_buk
*              kunnr       = wa_kna1-kunnr
*            IMPORTING
*              creditlimit = creditlimit.

          v_row = v_row + 1 .
          PERFORM fill_cell USING v_row 1 'Opening Balance(Rs)'.
          PERFORM fill_cell USING v_row 2 'Debit Items'.
          PERFORM fill_cell USING v_row 3 'Credit Items'.
          PERFORM fill_cell USING v_row 4 'Total Debit (Rs)'.
          PERFORM fill_cell USING v_row 5 'Total Credit (Rs)'.
          PERFORM fill_cell USING v_row 6 'Closing Bal (Rs)'.
          PERFORM fill_cell USING v_row 7 'Deposit Amt'.
          PERFORM fill_cell USING v_row 8 'Credit Limit (Rs)'.

          v_row = v_row + 1 .
          PERFORM fill_cell USING v_row 1 lv_opn.
          PERFORM fill_cell USING v_row 2 dc.
          PERFORM fill_cell USING v_row 3 cc1.
          PERFORM fill_cell USING v_row 4 tot_d.
          PERFORM fill_cell USING v_row 5 tot_c.
          PERFORM fill_cell USING v_row 6 clb.
          PERFORM fill_cell USING v_row 7 dbt_amtlx.
          PERFORM fill_cell USING v_row 8 creditlimit.

          v_row = v_row + 1 .
          PERFORM fill_cell USING v_row 1 ' '.

          v_row = v_row + 1 .
          PERFORM fill_cell USING v_row 1 'Transaction Date'.
          PERFORM fill_cell USING v_row 2 'Transaction Type'.
          PERFORM fill_cell USING v_row 3 'Transaction Number'.
          PERFORM fill_cell USING v_row 4 'Reference'.
          PERFORM fill_cell USING v_row 5 'Text'.
          PERFORM fill_cell USING v_row 6 'Debit Amount'.
          PERFORM fill_cell USING v_row 7 'Credit Amount'.
          PERFORM fill_cell USING v_row 8 'Balance Amount'.

          v_row = v_row + 1 .
          PERFORM fill_cell USING v_row 1 'Opening balance... ... ...'.
          PERFORM fill_cell USING v_row 8 lv_opn.

          LOOP AT it_exfi INTO wa_exfi .
            v_row = v_row + 1 .
            PERFORM fill_cell USING v_row 1 wa_exfi-doc_dat.
            PERFORM fill_cell USING v_row 2 wa_exfi-ttt.
            PERFORM fill_cell USING v_row 3 wa_exfi-belnr.
            PERFORM fill_cell USING v_row 4 wa_exfi-xblnr.
            PERFORM fill_cell USING v_row 5 wa_exfi-sgtxt.
            PERFORM fill_cell USING v_row 6 wa_exfi-d_amt.
            PERFORM fill_cell USING v_row 7 wa_exfi-c_amt.
            PERFORM fill_cell USING v_row 8 wa_exfi-b_amt.
            t = v_row.
          ENDLOOP.

          v_row = v_row + 1 .
          PERFORM fill_cell USING v_row 1 ' '.

          v_row = v_row + 1 .
          PERFORM fill_cell USING v_row 1 'DOCUMENT SUMMARY'.
          v_row = v_row + 1 .
          PERFORM fill_cell USING v_row 1 'DOCUMENT DESCRIPTION'.
          PERFORM fill_cell USING v_row 2 'DEBIT AMT (Rs)'.
          PERFORM fill_cell USING v_row 3 'CREDIT AMT (RS)'.
          v_row = v_row + 1 .
          PERFORM fill_cell USING v_row 1 'COLLECTION'.
          PERFORM fill_cell USING v_row 3 collection.
          v_row = v_row + 1 .
          PERFORM fill_cell USING v_row 1 'CREDIT NOTE'.
          PERFORM fill_cell USING v_row 2 debitnote.
          PERFORM fill_cell USING v_row 3 creditnote.
          v_row = v_row + 1 .
          PERFORM fill_cell USING v_row 1 'INVOICE'.
          PERFORM fill_cell USING v_row 2 invoice.

          v_row = v_row + 1 .
          PERFORM fill_cell USING v_row 1 ' '.

          v_row = v_row + 1 .
          PERFORM fill_cell USING v_row 1 'CREDIT NOTE'.
          v_row = v_row + 1 .
          PERFORM fill_cell USING v_row 1 'DOC.NO'.
          PERFORM fill_cell USING v_row 2 'DATE'.
          PERFORM fill_cell USING v_row 3 'AMOUNT (RS)'.
          PERFORM fill_cell USING v_row 4 'CREDITNOTE TYPE'.
          PERFORM fill_cell USING v_row 5 'TEXT'.
          f1 = v_row + 1.
          LOOP AT it_exfi INTO wa_exfi WHERE blart = 'DG'.
            v_row = v_row + 1 .
            PERFORM fill_cell USING v_row 1 wa_exfi-belnr.
            PERFORM fill_cell USING v_row 2 wa_exfi-doc_dat.
            PERFORM fill_cell USING v_row 3 wa_exfi-c_amt .
            PERFORM fill_cell USING v_row 4 wa_exfi-ttt.
            PERFORM fill_cell USING v_row 5 wa_exfi-sgtxt.
            t1 = v_row .
          ENDLOOP.

*----------------------------------DATE FORMATE.

          tc = t.
          CONCATENATE 'A' tc INTO c.

          CALL METHOD OF
              application
              'RANGE'     = cells
              NO
              FLUSH

            EXPORTING
              #1          = 'A16'
              #2          = c.

          SET PROPERTY OF cells 'NumberFormat' = 'dd/mm/yyyy;@'.

          tc1 = t1.
          fc1 = f1.
          CONCATENATE 'B' fc1 INTO fcc.
          CONCATENATE 'B' tc1 INTO tcc.

          CALL METHOD OF
              application
              'RANGE'     = cells
              NO
              FLUSH

            EXPORTING
              #1          = fcc
              #2          = tcc.

          SET PROPERTY OF cells 'NumberFormat' = 'dd/mm/yyyy;@'.
*----------------------------------DATE FORMATE.

*-----------------------------MERGE CELL
          CALL METHOD OF
              application
              'RANGE'     = range
              NO
              FLUSH

            EXPORTING
              #1          = 'D1'
              #2          = 'E1'.

          CALL METHOD OF
            range
            'MERGE'.

          CALL METHOD OF
              application
              'RANGE'     = range
              NO
              FLUSH

            EXPORTING
              #1          = 'A9'
              #2          = 'H9'.

          CALL METHOD OF
            range
            'MERGE'.

          CALL METHOD OF
              application
              'RANGE'     = range
              NO
              FLUSH

            EXPORTING
              #1          = 'A15'
              #2          = 'G15'.

          CALL METHOD OF
            range
            'MERGE'.

          CALL METHOD OF
              application
              'RANGE'     = range
              NO
              FLUSH

            EXPORTING
              #1          = 'G11'
              #2          = 'H11'.

          CALL METHOD OF
            range
            'MERGE'.

          CALL METHOD OF
              application
              'RANGE'     = range
              NO
              FLUSH

            EXPORTING
              #1          = 'G12'
              #2          = 'H12'.

          CALL METHOD OF
            range
            'MERGE'.
*----------------------------------MERGE CELL
*-----------------------------AUTO FIT
          CALL METHOD OF
            application
              'COLUMNS' = column.
          CALL METHOD OF
            column
            'AUTOFIT'.
*-----------------------------AUTO FIT
*-----------------------------CENTER
          CALL METHOD OF
              range
              'Borders' = borders
              NO
              FLUSH

            EXPORTING
              #1        = 7.
          SET PROPERTY OF borders 'LineStyle' = 1 .
*-----------------------------CENTER
*          CONCATENATE 'C:\Users\user_2\Desktop\customer ledger\' NAME '.XLS' INTO FILLN .
*          CALL METHOD OF
*              SHEET
*              'SaveAs'
*            EXPORTING
*              #1       = FILLN "'C:\excelgeet.xls'     "filename
*              #2       = 1.                          "fileFormat
        ENDIF.
      ELSE.
*        MESSAGE 'No Transaction Found' TYPE 'S' DISPLAY LIKE 'E' .
*        SUBMIT zcust_bal_ledger_print_dms VIA SELECTION-SCREEN .
      ENDIF.
      CLEAR: gt_final[] , wa_final , gt_final1[] ,wa_final1 ,gt_final2[] ,wa_final2 ,gt_final3[] ,wa_final3 , lv_opn , lv_buk ,gt_kna1[].
    ENDLOOP.
  ELSE.
*    MESSAGE 'No Customer Found' TYPE 'S' DISPLAY LIKE 'E' .
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  FILL_CELL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->ROW        text
*      -->COL        text
*      -->VAL        text
*----------------------------------------------------------------------*
FORM fill_cell  USING row col val.
  CALL METHOD OF
      sheet
      'Cells' = cells
    EXPORTING
      #1      = row
      #2      = col.
  SET PROPERTY OF cells 'Value' = val.
ENDFORM.                    "FILL_CELL
*&---------------------------------------------------------------------*
*& Form Data_to_ADF
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
FORM data_to_adf .
  DATA: fm_name TYPE funcname.
  DATA : docparams TYPE sfpdocparams.
  DATA : formoutput TYPE fpformoutput.
  DATA:l_formname TYPE  tdsfname VALUE 'ZDMS_RETAILER_ACC_FORM'.
  DATA:ls_funcname  TYPE  funcname.
  DATA:l_outputparams TYPE  sfpoutputparams.

  CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
    EXPORTING
      i_name     = l_formname
    IMPORTING
      e_funcname = ls_funcname.
*    CATCH cx_fp_api_repository.
*    CATCH cx_fp_api_usage.
*    CATCH cx_fp_api_internal.

  IF p_c1 EQ abap_true.
    l_outputparams-nodialog = 'X'.
    l_outputparams-getpdf   = 'X'.
    l_outputparams-preview  = space.
  ELSE.
    l_outputparams-nodialog = 'X'.
    l_outputparams-preview = 'X'.
  ENDIF.


  CALL FUNCTION 'FP_JOB_OPEN'
    CHANGING
      ie_outputparams = l_outputparams.
  IF sy-subrc = 0.

    CALL FUNCTION ls_funcname "'/1BCDWB/SM00000080'
      EXPORTING
        /1bcdwb/docparams  = docparams
        bal_date           = b_date
        lv_frdt            = so_budat-low
        lv_todt            = so_budat-high
        lv_opn             = lv_opn
        lv_buk             = lv_buk
        distributor        = p_dist
        gt_kna1            = gt_kna1[]
        gt_bsid            = gt_final[]
        itt_final          = itt_final[]
        l_total_amt        = l_total_amtn
        l_debit_total      = l_debit_total
        l_final_bal        = l_final_bal
      IMPORTING
        /1bcdwb/formoutput = formoutput
      EXCEPTIONS
        usage_error        = 1
        system_error       = 2
        internal_error     = 3
        OTHERS             = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CALL FUNCTION 'FP_JOB_CLOSE'.

    DATA(out_pdf) = CONV xstring( formoutput-pdf ).
    CLEAR lv_string.
    CALL FUNCTION 'SCMS_BASE64_ENCODE_STR'
      EXPORTING
        input  = out_pdf
      IMPORTING
        output = lv_string.

    EXPORT lv_string FROM lv_string TO MEMORY ID 'ZCUST_DMS'.

  ENDIF.

ENDFORM.
