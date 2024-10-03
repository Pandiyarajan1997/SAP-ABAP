*&---------------------------------------------------------------------*
*& Report  ZVEND_BAL_LEDGER_PRINT_EMAIL
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT zvend_bal_ledger_print.

TABLES:bsik,lfa1.

TYPES: BEGIN OF gs_lfa1,
         lifnr TYPE lfa1-lifnr,                  " Vendor Code
         name1 TYPE lfa1-name1,                  " Customer Name
         adrnr TYPE lfa1-adrnr,                  " Address Number
       END OF gs_lfa1.

DATA: gt_lfa1 TYPE TABLE OF gs_lfa1,
      wa_lfa1 TYPE gs_lfa1.
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

TYPES: BEGIN OF gs_bsik,
         prctr TYPE bsik-prctr,                  " Profit Center
         lifnr TYPE bsik-lifnr,                  " Customer Number
         augbl TYPE bsik-augbl,
         budat TYPE bsik-budat,                  " Posting Date in the Document
         bldat TYPE bsik-bldat,
         blart TYPE bsik-blart,
         zfbdt TYPE bsik-zfbdt,                  " Baseline Date for Due Date Calculation
         dmbtr TYPE bsik-dmbtr,                  " Amount in Local Currency
         shkzg TYPE bsik-shkzg,                  " Debit/Credit Indicator
         xblnr TYPE bsik-xblnr,                  " Reference Document Number
         belnr TYPE bsik-belnr,                  " Bill Number
         gsber TYPE bsik-gsber,                  " Business Area
         sgtxt TYPE bsik-sgtxt,                  " Text Or Remarks
         ebeln TYPE bsik-ebeln,
         umskz TYPE bsik-umskz,
         bukrs TYPE bsik-bukrs,
         gjahr TYPE bsik-gjahr,
         sknto TYPE bsak-sknto,
       END OF gs_bsik.

DATA: gt_bsik  TYPE TABLE OF gs_bsik,
      gt_bsik1 TYPE TABLE OF gs_bsik,
      wa_bsik  TYPE gs_bsik,
      wa_bsik1 TYPE gs_bsik.
DATA: lv_ind TYPE i.

TYPES: BEGIN OF gs_bsak,
         prctr TYPE bsak-prctr,                  " Profit Center
         lifnr TYPE bsak-lifnr,                  " Customer Number
         augbl TYPE bsak-augbl,
         budat TYPE bsak-budat,                  " Posting Date in the Document
         bldat TYPE bsak-bldat,
         blart TYPE bsak-blart,
         zfbdt TYPE bsak-zfbdt,                  " Baseline Date for Due Date Calculation
         dmbtr TYPE bsak-dmbtr,                  " Amount in Local Currency
         shkzg TYPE bsak-shkzg,                  " Debit/Credit Indicator
         xblnr TYPE bsak-xblnr,                  " Reference Document Number
         belnr TYPE bsak-belnr,                  " Bill Number
         gsber TYPE bsak-gsber,                  " Business Area
         sgtxt TYPE bsak-sgtxt,                  " Text OR Remarks
         ebeln TYPE bsak-ebeln,                  " Billing Doc.No
         umskz TYPE bsak-umskz,                  " Spl.Gl.Ind
         bukrs TYPE bsak-bukrs,
         gjahr TYPE bsak-gjahr,
         sknto TYPE bsak-sknto,
         umsks TYPE bsak-umsks,
         augdt TYPE bsak-augdt,
         zuonr TYPE bsak-zuonr,
         buzei TYPE bsak-buzei,
       END OF gs_bsak.

DATA: gt_bsak  TYPE TABLE OF gs_bsak,
      wa_bsak  TYPE gs_bsak,
      wa_bsak1 TYPE gs_bsak,
      gt_bsak1 TYPE TABLE OF gs_bsak,
      gt_bsak2 TYPE TABLE OF gs_bsak.
DATA: gt_bsak7 TYPE TABLE OF gs_bsak,
      wa_bsak7 TYPE gs_bsak.

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
          belnr  TYPE bsak-belnr,                  " Bill Number
          budat  TYPE bsik-budat,                 " Posting Date
          blart  TYPE bsik-blart,                  " Bill Doc. Type
          cr_amt TYPE bsak-dmbtr,                 " Cr Amount
          dr_amt TYPE bsak-dmbtr,                 " Dr Amount
          shkzg  TYPE bsik-shkzg,
        END OF ty_chk.

TYPES: BEGIN OF gs_final,
         lifnr    TYPE bsak-lifnr,                  " Customer Number
         umskz    TYPE bsik-umskz,                  " Spl.GL Ind
         belnr    TYPE bsak-belnr,                  " Bill Number
         ebeln    TYPE bsak-ebeln,                  " Billing Document Number
         xblnr    TYPE bsik-xblnr,                  " Reference Document Number
         bldat    TYPE bsak-bldat,                  " Posting Date in the Document
         cr_amt   TYPE bsak-dmbtr,                 " Cr Amount
         dr_amt   TYPE bsak-dmbtr,                 " Dr Amount
         budat    TYPE bsik-budat,                 " Posting Date
         name1    TYPE lfa1-name1,                 " Customer Name
         blart    TYPE bsik-blart,                  " Bill Doc. Type
         bschl    TYPE bsid-bschl,
         sgtxt    TYPE bsik-sgtxt,                  " Text OR Remarks
         gsber    TYPE bsik-gsber,                  " Business Area
         shkzg    TYPE bsik-shkzg,                  " Dbit/Crdit ind.
         bal_amt  TYPE bsak-dmbtr,                " Balance Amount
         ltext    TYPE t003t-ltext,                  " Bill Doc .Type Descrption
         bukrs    TYPE bsik-bukrs,
         gjahr    TYPE bsik-gjahr,
         sknto    TYPE bsak-sknto,
         lv_count TYPE int1,
         sta      TYPE int1,
         t_tttext TYPE char20,
       END OF gs_final.

DATA: gt_final  TYPE TABLE OF gs_final WITH HEADER LINE,
      wa_final  TYPE gs_final,
      gt_final1 TYPE TABLE OF gs_final WITH HEADER LINE,
      wa_final1 TYPE gs_final,
      gt_final2 TYPE TABLE OF gs_final WITH HEADER LINE,
      wa_final2 TYPE gs_final,
      gt_final3 TYPE TABLE OF gs_final WITH HEADER LINE,
      wa_final3 TYPE gs_final,
      gs_chk    TYPE ty_chk.

TYPES: BEGIN OF gs_faglflexa,
         docnr TYPE faglflexa-docnr,
         prctr TYPE faglflexa-prctr,
         bschl TYPE faglflexa-bschl,
       END OF gs_faglflexa.

DATA: gt_faglflexa TYPE TABLE OF gs_faglflexa,
      wa_faglflexa TYPE gs_faglflexa.

TYPES : BEGIN OF gs_lfc1,
          lifnr TYPE lfc1-lifnr,
          bukrs TYPE lfc1-bukrs,
          umsav TYPE lfc1-umsav,
          gjahr TYPE lfc1-gjahr,
        END OF gs_lfc1.

DATA: gt_lfc1 TYPE TABLE OF gs_lfc1,
      wa_lfc1 TYPE gs_lfc1.

DATA: or_budat TYPE  bsak-budat,
      or_bukrs TYPE bsik-bukrs,
      or_lifnr TYPE lfa1-lifnr,
      fm_name  TYPE rs38l_fnam,
      or_prctr TYPE cepc-prctr,
      or_umskz TYPE bsik-umskz.

DATA: l_lifnr TYPE lfa1-lifnr,
      l_prctr TYPE cepc-prctr.

DATA: b_date TYPE sy-datum.

DATA: lv_opn   TYPE bsak-dmbtr,
      rv_opn   TYPE bsak-dmbtr,
      lv_total TYPE bsak-dmbtr,
      lv_trans TYPE bsak-dmbtr,
      lv_date  TYPE sy-datum,
      lv_frdat TYPE sy-datum,
      lv_todat TYPE sy-datum,
      lv_sum   TYPE bsak-dmbtr,
      lv_buk   TYPE t001-bukrs,
      lv_cramt TYPE bsak-dmbtr,
      lv_dbamt TYPE bsak-dmbtr,
      lv_cdisc TYPE bsak-dmbtr.

************************************ PR@$@TH

TYPES: BEGIN OF tem_lfa1,
         lifnr TYPE lfa1-lifnr,
         name1 TYPE lfa1-name1,
         adrnr TYPE lfa1-adrnr,
       END OF tem_lfa1.

DATA: z_lfa1 TYPE TABLE OF tem_lfa1,
      y_lfa1 TYPE tem_lfa1.

TYPES: BEGIN OF tem_lfb1,
         lifnr TYPE lfb1-lifnr,
         bukrs TYPE lfb1-bukrs,
       END OF tem_lfb1.

DATA: z_lfb1 TYPE TABLE OF tem_lfb1,
      y_lfb1 TYPE tem_lfb1.

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
DATA: s_email1 TYPE  adr6-smtp_addr VALUE 'prasath@sphinaxinfosystems.com'.
DATA: sm_name TYPE lfa1-name1.
DATA: count TYPE i VALUE 1 .

TYPES: BEGIN OF ty_email_del,
         lifnr TYPE lfa1-kunnr,
         name1 TYPE lfa1-name1,
         email TYPE adr6-smtp_addr,
         stats TYPE i,
       END OF ty_email_del.

DATA: it_sts TYPE TABLE OF ty_email_del,
      wa_sts TYPE ty_email_del.

DATA: sts TYPE lifnr.
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
         lifnr(10)  TYPE c,
         name1(35)  TYPE c,
         email(241) TYPE c,
       END OF ty_email_char.

DATA: wa_sts_char TYPE ty_email_char.

DATA: it_message TYPE STANDARD TABLE OF solisti1 INITIAL SIZE 0
                 WITH HEADER LINE.
DATA: it_attach TYPE STANDARD TABLE OF solisti1 INITIAL SIZE 0
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
DATA: tot TYPE int2.

TYPES:BEGIN OF ty_doc,
        belnr TYPE bsak-belnr,
      END OF ty_doc.

DATA: it_doc TYPE TABLE OF ty_doc,
      wa_doc TYPE ty_doc.

DATA: it_vbsegk TYPE TABLE OF vbsegk,
      wa_vbsegk TYPE vbsegk.

DATA: lv_bin_value TYPE xstring,
      lv_string    TYPE string,
      it_lines     TYPE TABLE OF tline,
      it_otf       TYPE TABLE OF itcoo.

********************************* PR@$@TH

SELECTION-SCREEN: BEGIN OF BLOCK email_rep WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: so_lifnr FOR lfa1-lifnr NO INTERVALS NO-EXTENSION OBLIGATORY.
  PARAMETERS: so_bukrs LIKE lfb1-bukrs .
  SELECT-OPTIONS: so_budat FOR bsik-budat.
  PARAMETERS: p_c1 AS CHECKBOX .

SELECTION-SCREEN: END OF BLOCK email_rep.

INITIALIZATION.

  LOOP AT SCREEN.
    IF screen-name = 'P_C1'.
      screen-active = 1.
      screen-invisible = 1.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

  so_bukrs = 1000 .
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

  SELECT lifnr bukrs FROM lfb1 INTO TABLE z_lfb1 WHERE bukrs EQ so_bukrs AND loevm <> 'X' AND lifnr IN so_lifnr .
  SELECT lifnr name1 adrnr FROM lfa1 INTO TABLE z_lfa1 FOR ALL ENTRIES IN z_lfb1 WHERE lifnr EQ z_lfb1-lifnr.
  SORT z_lfb1 BY lifnr.
  IF z_lfb1 IS NOT INITIAL.
    LOOP AT z_lfb1 INTO y_lfb1.
      CLEAR: i_otf[],i_tline[],i_record[],i_xstring,i_objpack[],i_objtxt[],i_objbin[],i_reclist[],wa_objhead[],w_ctrlop,w_compop,w_return,wa_buffer,v_len_in.

      READ TABLE z_lfa1 INTO y_lfa1 WITH KEY lifnr = y_lfb1-lifnr.
      CLEAR: sm_name,sts.
      sm_name = y_lfa1-name1.
      sts = y_lfa1-lifnr.
      CLEAR: lv_buk.
      SELECT bukrs  FROM t001 INTO lv_buk WHERE bukrs EQ y_lfb1-bukrs.
        CLEAR: gt_bsak.
        SELECT
           prctr
           lifnr
           umskz
           augbl
           budat
           bldat
           blart
           zfbdt
           dmbtr
           shkzg
           xblnr
           belnr
           gsber
           sgtxt
           ebeln
           bukrs
           gjahr
           sknto
           umsks
           augdt
           zuonr
        buzei FROM bsak INTO CORRESPONDING FIELDS OF TABLE gt_bsak WHERE budat IN so_budat AND bukrs EQ y_lfb1-bukrs AND lifnr EQ y_lfb1-lifnr AND umskz <> 'H' AND umskz <> 'F' . "SO_LIFNR SO_BUKRS

        CLEAR:gt_bsik.
        SELECT
           prctr
           lifnr
           augbl
           budat
           bldat
           blart
           zfbdt
           dmbtr
           shkzg
           xblnr
           belnr
           gsber
           sgtxt
           ebeln
           umskz
           bukrs
           gjahr
        sknto FROM bsik INTO CORRESPONDING FIELDS OF TABLE gt_bsik WHERE budat IN so_budat AND bukrs EQ y_lfb1-bukrs AND lifnr EQ y_lfb1-lifnr AND umskz <> 'H' AND umskz <> 'F' .

        CLEAR:gt_bsak1.
        SELECT
          prctr
          lifnr
          umskz
          augbl
          budat
          bldat
          blart
          zfbdt
          dmbtr
          shkzg
          xblnr
          belnr
          gsber
          sgtxt
          ebeln
          bukrs
          gjahr
          sknto
          umsks
          augdt
          zuonr
          buzei FROM bsak INTO CORRESPONDING FIELDS OF TABLE gt_bsak1
        WHERE  budat BETWEEN '01.04.2014' AND so_budat-low AND bukrs EQ y_lfb1-bukrs AND lifnr EQ y_lfb1-lifnr AND umskz <> 'H' AND umskz <> 'F' .

        CLEAR: gt_bsik1.
        SELECT
          prctr
          lifnr
          augbl
          budat
          bldat
          blart
          zfbdt
          dmbtr
          shkzg
          xblnr
          belnr
          gsber
          sgtxt
          ebeln
          umskz
          bukrs
          gjahr
          sknto FROM bsik INTO CORRESPONDING FIELDS OF TABLE gt_bsik1
        WHERE budat BETWEEN '01.04.2014'  AND  so_budat-low AND bukrs EQ y_lfb1-bukrs AND lifnr EQ y_lfb1-lifnr AND umskz <> 'H' AND umskz <> 'F' .

        REFRESH it_doc.
        LOOP AT gt_bsak INTO wa_bsak.
          wa_doc-belnr =  wa_bsak-belnr .
          APPEND wa_doc TO it_doc .
        ENDLOOP.

        LOOP AT  gt_bsik INTO wa_bsik.
          wa_doc-belnr = wa_bsik-belnr.
          APPEND wa_doc TO it_doc.
        ENDLOOP.

        LOOP AT gt_bsak1 INTO wa_bsak1.
          wa_doc-belnr =  wa_bsak1-belnr .
          APPEND wa_doc TO it_doc .
        ENDLOOP.

        LOOP AT  gt_bsik1 INTO wa_bsik1.
          wa_doc-belnr = wa_bsik1-belnr.
          APPEND wa_doc TO it_doc.
        ENDLOOP.

        SELECT * FROM vbsegk INTO TABLE it_vbsegk FOR ALL ENTRIES IN it_doc WHERE belnr = it_doc-belnr AND lifnr = so_lifnr .
        LOOP AT it_vbsegk INTO wa_vbsegk.
          DELETE gt_bsak WHERE belnr = wa_vbsegk-belnr.
          DELETE gt_bsik WHERE belnr = wa_vbsegk-belnr.
          DELETE gt_bsak1 WHERE belnr = wa_vbsegk-belnr.
          DELETE gt_bsik1 WHERE belnr = wa_vbsegk-belnr.
        ENDLOOP.

        CLEAR:gt_bsak7.
        SORT gt_bsak BY belnr augbl.
        APPEND LINES OF gt_bsak TO gt_bsik.
        APPEND LINES OF gt_bsak TO gt_bsak7.
        DELETE gt_bsak7 WHERE shkzg = 'S'.
        APPEND LINES OF gt_bsak1 TO gt_bsik1.

        IF gt_bsik[] IS NOT INITIAL.
          CLEAR: gt_lfa1[].
          SELECT
            lifnr
            name1
            adrnr FROM lfa1 INTO TABLE gt_lfa1 FOR ALL ENTRIES IN gt_bsik
          WHERE lifnr = gt_bsik-lifnr.
          CLEAR: gt_t003t.
          SELECT blart
                 ltext
          spras FROM  t003t INTO TABLE gt_t003t FOR ALL ENTRIES IN gt_bsik WHERE blart = gt_bsik-blart AND spras = 'E' ORDER BY PRIMARY KEY.  " Added by <IT-CAR Tool> during Code Remediation
          CLEAR:gt_t003t1.
          SELECT  blart
                  ltext
          spras FROM  t003t INTO TABLE gt_t003t1 FOR ALL ENTRIES IN gt_bsak WHERE blart = gt_bsak-blart AND spras = 'E' ORDER BY PRIMARY KEY.  " Added by <IT-CAR Tool> during Code Remediation

          APPEND  LINES OF gt_t003t1 TO gt_t003t.
          CLEAR:gt_t003t2.
          SELECT  blart
                  ltext
          spras FROM  t003t INTO TABLE gt_t003t2 FOR ALL ENTRIES IN gt_bsik WHERE blart = gt_bsik-blart AND spras = 'E' ORDER BY PRIMARY KEY.  " Added by <IT-CAR Tool> during Code Remediation
          APPEND  LINES OF gt_t003t2 TO gt_t003t.

        ENDIF.
        IF gt_lfa1[] IS  NOT INITIAL.
          CLEAR: gt_adrc.
          SELECT  addrnumber
                  street
                  city1
                  city2
          post_code1 FROM adrc INTO TABLE gt_adrc FOR ALL ENTRIES IN gt_lfa1 WHERE addrnumber = gt_lfa1-adrnr.

        ENDIF.
        CLEAR: gt_lfc1.
        IF gt_bsik IS NOT INITIAL.
          SELECT  lifnr
                  bukrs
                  umsav
          gjahr FROM lfc1 INTO TABLE gt_lfc1 FOR ALL ENTRIES IN gt_bsik WHERE lifnr = gt_bsik-lifnr AND gjahr = gt_bsik-gjahr AND bukrs = gt_bsik-bukrs.
        ENDIF.
      ENDSELECT.

      CLEAR: lv_total.
      LOOP AT gt_lfc1 INTO wa_lfc1.
        lv_total =  wa_lfc1-umsav.
      ENDLOOP.
      CLEAR:wa_lfc1.

      LOOP AT gt_bsik1 INTO wa_bsik1 .
        IF  wa_bsik1-budat < so_budat-low.
          IF wa_bsik1-shkzg = 'S'.
            lv_opn = lv_opn  + wa_bsik1-dmbtr.
          ELSEIF wa_bsik1-shkzg = 'H'.
            lv_opn  = lv_opn - wa_bsik1-dmbtr.
          ENDIF.
        ENDIF.
        CLEAR wa_bsik1.
      ENDLOOP.

      SORT gt_bsik BY budat belnr blart shkzg.

      LOOP AT gt_bsik INTO wa_bsik.
        wa_final-xblnr = wa_bsik-xblnr.
        wa_final-bldat = wa_bsik-bldat.
        wa_final-lifnr = wa_bsik-lifnr.
        wa_final-belnr = wa_bsik-belnr.
        wa_final-budat = wa_bsik-budat.
        wa_final-gsber = wa_bsik-gsber.
        wa_final-sgtxt = wa_bsik-sgtxt.
        wa_final-ebeln = wa_bsik-ebeln.
        wa_final-umskz = wa_bsik-umskz.
        wa_final-bukrs = wa_bsik-bukrs.
        READ TABLE gt_t003t INTO wa_t003t WITH KEY blart = wa_bsik-blart.
        wa_final-ltext = wa_t003t-ltext.
        wa_final-blart = wa_bsik-blart.
        READ TABLE gt_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_bsik-lifnr.
        wa_final-name1 = wa_lfa1-name1.
        wa_final-shkzg = wa_bsik-shkzg.
        IF gs_chk-budat = wa_bsik-budat AND gs_chk-blart = wa_bsik-blart AND gs_chk-belnr = wa_bsik-belnr
                                        AND gs_chk-shkzg = wa_bsik-shkzg AND wa_bsik-shkzg = 'H'.
          wa_final-sknto = lv_cdisc + wa_bsik-sknto.
          lv_cdisc = wa_final-sknto.
          wa_final-cr_amt = lv_cramt + wa_bsik-dmbtr .
          lv_cramt = wa_final-cr_amt.
          DELETE gt_final INDEX lv_ind.
        ELSEIF gs_chk-budat = wa_bsik-budat AND gs_chk-blart = wa_bsik-blart AND gs_chk-belnr = wa_bsik-belnr
                                            AND gs_chk-shkzg = wa_bsik-shkzg AND wa_bsik-shkzg = 'S'.
          wa_final-dr_amt = lv_dbamt + wa_bsik-dmbtr.
          lv_dbamt = wa_final-dr_amt.
          DELETE gt_final INDEX lv_ind.
        ELSEIF gs_chk-budat = wa_bsik-budat AND gs_chk-blart = wa_bsik-blart AND gs_chk-belnr = wa_bsik-belnr
                                            AND gs_chk-shkzg = wa_bsik-shkzg AND wa_bsik-shkzg = 'C' AND wa_t003t-blart = 'AB'.
          wa_final-cr_amt = lv_cramt + wa_bsik-dmbtr.
          lv_cramt = wa_final-cr_amt.
          DELETE gt_final INDEX lv_ind.
        ELSEIF gs_chk-budat = wa_bsik-budat AND gs_chk-blart = wa_bsik-blart AND gs_chk-belnr = wa_bsik-belnr
                                            AND wa_bsik-shkzg = 'D' AND wa_t003t-blart = 'AB'.
          wa_final-dr_amt = lv_dbamt + wa_bsik-dmbtr.
          lv_dbamt = wa_final-dr_amt.
          DELETE gt_final INDEX lv_ind.
        ELSEIF ( gs_chk-budat <> wa_bsik-budat AND gs_chk-blart <> wa_bsik-blart AND gs_chk-belnr <> wa_bsik-belnr AND wa_bsik-shkzg <> 'S' )
            OR ( ( gs_chk-budat <> wa_bsik-budat OR gs_chk-blart <> wa_bsik-blart OR gs_chk-belnr <> wa_bsik-belnr ) AND wa_bsik-shkzg <> 'S' ) OR
                 ( gs_chk-shkzg <> wa_bsik-shkzg AND wa_bsik-shkzg <> 'S' ) .
          wa_final-sknto = wa_bsik-sknto.
          lv_cdisc = wa_final-sknto .
          wa_final-cr_amt = wa_bsik-dmbtr.
          lv_cramt = wa_final-cr_amt .
        ELSEIF ( gs_chk-budat <> wa_bsik-budat AND gs_chk-blart <> wa_bsik-blart AND gs_chk-belnr <> wa_bsik-belnr AND wa_bsik-shkzg <> 'H' )
            OR ( ( gs_chk-budat <> wa_bsik-budat OR gs_chk-blart <> wa_bsik-blart OR gs_chk-belnr <> wa_bsik-belnr ) AND wa_bsik-shkzg <> 'H' ) OR
               ( gs_chk-shkzg <> wa_bsik-shkzg AND wa_bsik-shkzg <> 'H' ) .
          wa_final-dr_amt = wa_bsik-dmbtr .
          lv_dbamt = wa_final-dr_amt  .
        ELSEIF ( gs_chk-budat <> wa_bsik-budat AND gs_chk-blart <> wa_bsik-blart AND gs_chk-belnr <> wa_bsik-belnr AND
                 gs_chk-shkzg <> wa_bsik-shkzg AND wa_bsik-shkzg = 'C' )  OR  wa_t003t-blart = 'AB'.
          wa_final-cr_amt = wa_bsik-dmbtr.
          lv_cramt = wa_final-cr_amt.
        ELSEIF ( gs_chk-budat <> wa_bsik-budat AND gs_chk-blart <> wa_bsik-blart AND gs_chk-belnr <> wa_bsik-belnr AND
                 gs_chk-shkzg <> wa_bsik-shkzg AND wa_bsik-shkzg = 'D') OR wa_t003t-blart = 'AB'.
          wa_final-dr_amt = wa_bsik-dmbtr.
          lv_dbamt = wa_final-dr_amt.
        ENDIF.
        gs_chk-belnr = wa_bsik-belnr.
        gs_chk-budat = wa_bsik-budat.
        gs_chk-blart = wa_bsik-blart.
        gs_chk-shkzg = wa_bsik-shkzg.
        APPEND wa_final TO gt_final.
        CLEAR : wa_final,wa_bsik.
        DESCRIBE TABLE gt_final LINES lv_ind.
      ENDLOOP.

      CLEAR: gt_bsik[].
      CLEAR: gs_chk ,wa_bsik, wa_t003t, wa_lfa1 .

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
        READ TABLE gt_final2 INTO wa_final2 WITH KEY belnr = wa_final-belnr shkzg = wa_final-shkzg .
        IF wa_final2-sknto IS NOT INITIAL .
          wa_final2-cr_amt = wa_final2-cr_amt - wa_final2-sknto.
          APPEND wa_final2 TO gt_final3.
          CLEAR: wa_final2-cr_amt.
          wa_final2-cr_amt = wa_final2-sknto.
          APPEND wa_final2 TO gt_final3.
          DELETE gt_final WHERE belnr = wa_final2-belnr AND shkzg = 'H' .
        ENDIF.
      ENDLOOP.

      APPEND LINES OF gt_final3 TO gt_final.

      LOOP AT gt_final INTO wa_final.
        lv_sum = lv_sum + wa_final-dr_amt - wa_final-cr_amt .
        wa_final-bal_amt = lv_sum.
        MODIFY gt_final FROM wa_final TRANSPORTING bal_amt.
        AT END OF lifnr.
          CLEAR :lv_sum,wa_final,wa_bsik.
        ENDAT.
      ENDLOOP.

      LOOP AT gt_final INTO wa_final WHERE lifnr = ' '.
        wa_final-lifnr = wa_lfa1-lifnr.
        MODIFY gt_final FROM wa_final TRANSPORTING lifnr.
        CLEAR  wa_final.
      ENDLOOP.
      CLEAR: rv_opn.
      rv_opn = lv_total - lv_opn.
      SORT gt_final BY budat shkzg.


      CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
        EXPORTING
          formname           = 'ZSF_VEN_ACCOUNT_LEDGER_MAIL'
*         VARIANT            = ' '
*         DIRECT_CALL        = ' '
        IMPORTING
          fm_name            = fm_name
        EXCEPTIONS
          no_form            = 1
          no_function_module = 2
          OTHERS             = 3.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      IF p_c1 EQ abap_true.
*Control Parameters for Smartforms
        w_ctrlop-no_dialog  = abap_true.
        w_ctrlop-preview    = space.
        w_ctrlop-getotf     = abap_true.
      ELSE.
        w_ctrlop-getotf = abap_true.
        w_ctrlop-no_dialog = abap_true.
        w_compop-tdnoprev = abap_true.
        w_ctrlop-preview = space.
        w_compop-tddest = 'LOCL'.
      ENDIF.

      IF gt_final[] IS NOT INITIAL .

        CALL FUNCTION fm_name "'/1BCDWB/SF00000527'
          EXPORTING
*           ARCHIVE_INDEX    =
*           ARCHIVE_INDEX_TAB    =
*           ARCHIVE_PARAMETERS   =
           CONTROL_PARAMETERS   = W_CTRLOP
*           MAIL_APPL_OBJ    =
*           MAIL_RECIPIENT   =
*           MAIL_SENDER      =
*           OUTPUT_OPTIONS   = W_COMPOP
*           USER_SETTINGS    = ABAP_TRUE"'X'
            bal_date         = b_date
            lv_frdt          = so_budat-low
            lv_todt          = so_budat-high
            lv_opn           = lv_opn
            lv_buk           = lv_buk
        IMPORTING
*           DOCUMENT_OUTPUT_INFO =
           JOB_OUTPUT_INFO  = W_RETURN
*           JOB_OUTPUT_OPTIONS   =
          TABLES
            gt_lfa1          = gt_lfa1[]
            gt_bsid          = gt_final[]
          EXCEPTIONS
            formatting_error = 1
            internal_error   = 2
            send_error       = 3
            user_canceled    = 4
            OTHERS           = 5.
        IF sy-subrc <> 0.
*        Implement suitable error handling here
        else.
            IF p_c1 EQ abap_true.
              CLEAR: lv_bin_value, it_otf.
              it_otf = w_return-otfdata[].
              CALL FUNCTION 'CONVERT_OTF'
                EXPORTING
                  format                = 'PDF'
                IMPORTING
                  bin_file              = lv_bin_value
                TABLES
                  otf                   = it_otf
                  lines                 = it_lines
                EXCEPTIONS
                  err_max_linewidth     = 1
                  err_format            = 2
                  err_conv_not_possible = 3
                  err_bad_otf           = 4
                  OTHERS                = 5.
              IF sy-subrc = 0.
*          ep_xstring = lv_bin_value.
                CLEAR lv_string.
                CALL FUNCTION 'SCMS_BASE64_ENCODE_STR'
                  EXPORTING
                    input  = lv_bin_value
                  IMPORTING
                    output = lv_string.
              ENDIF.
              EXPORT lv_string FROM lv_string TO MEMORY ID 'ZVEND'.
            ENDIF.
        ENDIF.
      ELSE.
        MESSAGE 'No Transaction Found' TYPE 'S' DISPLAY LIKE 'E' .
        SUBMIT zvend_bal_ledger_print VIA SELECTION-SCREEN .
      ENDIF.
      CLEAR: gt_final[] , wa_final , gt_final1[] ,wa_final1 ,gt_final2[] ,wa_final2 ,gt_final3[] ,wa_final3 , lv_opn , lv_buk ,gt_lfa1[].
    ENDLOOP.
  ELSE.
    MESSAGE 'No Vendor Found' TYPE 'S' DISPLAY LIKE 'E' .
  ENDIF.

ENDFORM.                    " FETCHDATA
