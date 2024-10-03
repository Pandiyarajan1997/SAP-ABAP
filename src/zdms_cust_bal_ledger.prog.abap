*=======================================================================
*  Author                    : T.Pandiarajan
*
*  Date                      : 06.01.2024
*
*  Requester Name            : Ramakrishnan
*
*  Business Logic            : Customer Ledger - Distributor Opening balance apr1
*
* Hardcoded                  : gv_bukrs = 'DMS1'.
*                              so_bukrs = 1000 .
*=======================================================================
REPORT zdms_cust_bal_ledger.

TABLES:bsid.
TABLES:kna1.
TABLES:zmail_id.

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
       END OF gs_bsid.

DATA: gt_bsid  TYPE TABLE OF gs_bsid,
      gt_bsid1 TYPE TABLE OF gs_bsid,
      gtt_bsid TYPE TABLE OF gs_bsid,
      wat_bsid TYPE gs_bsid,
      wa_bsid1 TYPE gs_bsid.

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


DATA: gt_bsik TYPE TABLE OF gs_bsik.

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

DATA: gt_bsak TYPE TABLE OF gs_bsak.

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
       END OF gs_bsad.

DATA: gt_bsad  TYPE TABLE OF gs_bsad,
      gt_bsad1 TYPE TABLE OF gs_bsad.

DATA: gt_bsad7 TYPE TABLE OF gs_bsad.

TYPES: BEGIN OF gs_adrc,
         addrnumber TYPE adrc-addrnumber,        " Address Number
         street     TYPE adrc-street,                " Street
         city1      TYPE adrc-city1,                  " District
         city2      TYPE adrc-city2,                  " City
         post_code1 TYPE adrc-post_code1,        " Postal Code
       END OF gs_adrc.

DATA: gt_adrc TYPE TABLE OF gs_adrc WITH HEADER LINE.

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
      wat_final1 TYPE gs_final.

TYPES : BEGIN OF gs_knc1,
          kunnr TYPE knc1-kunnr,
          bukrs TYPE knc1-bukrs,
          umsav TYPE knc1-umsav,
          gjahr TYPE knc1-gjahr,
        END OF gs_knc1.

DATA: gt_knc1 TYPE TABLE OF gs_knc1,
      wa_knc1 TYPE gs_knc1.

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
         werks TYPE kna1-werks,
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


TYPES : BEGIN OF ty_final,
          account    TYPE char10,
          doctyp     TYPE char02,
          busarea    TYPE werks_d,
          refdoc     TYPE kunnr,
          docdate    TYPE sy-datum,
          pdate      TYPE sy-datum,
          gl_account TYPE char10,
          openbal    TYPE bsad-dmbtr,
          item_txt   TYPE char100,
        END OF ty_final.
DATA : gt_excel TYPE TABLE OF ty_final.
TYPES: BEGIN OF alv,
         account(10) TYPE c,
         name1       TYPE name1_gp,
         acctyp      TYPE koart,
         blart       TYPE blart,
         refdoc      TYPE xblnr1,
         fisyear     TYPE gjahr,
         docdate     TYPE bldat,
         pdate       TYPE budat,
         gl_account  TYPE saknr,
         glacc_txt   TYPE txt50,
         amt         TYPE wrbtr,
         item_txt    TYPE sgtxt,
         gsber       TYPE gsber,
*         busarea     TYPE bwkey,
         docno       TYPE belnr_d,
         msgtyp      TYPE bapi_mtype,
         message     TYPE string,
       END OF alv.
DATA: gt_alv  TYPE TABLE OF alv.
DATA lo_gr_alv       TYPE REF TO cl_salv_table. " Variables for ALV properties



********************************* PR@$@TH

SELECTION-SCREEN: BEGIN OF BLOCK email_rep WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: so_kunnr FOR kna1-kunnr.
  PARAMETERS: so_bukrs LIKE knb1-bukrs NO-DISPLAY.
  SELECT-OPTIONS: so_budat FOR bsid-budat.
  PARAMETERS: p_chk3 AS CHECKBOX MODIF ID bl3.
SELECTION-SCREEN: END OF BLOCK email_rep.
DATA: gv_bukrs TYPE bukrs.
gv_bukrs = 'DMS1'.

INITIALIZATION.
  so_bukrs = 1000 .

START-OF-SELECTION.
  PERFORM: fetchdata.
  PERFORM: process.
  PERFORM: alv_display.

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
***************fetch only DMS distributor***************
  SELECT bwkey, bukrs FROM t001k INTO TABLE @DATA(lt_t001k) WHERE bukrs = 'DMS1'.
  IF sy-subrc = 0.
    SELECT kunnr name1 adrnr werks FROM kna1 INTO TABLE z_kna1
      FOR ALL ENTRIES IN lt_t001k
      WHERE kunnr IN so_kunnr
    AND   werks = lt_t001k-bwkey.
  ENDIF.
  IF z_kna1 IS NOT INITIAL.
    SELECT kunnr FROM zcus_excl_stmnt INTO TABLE @DATA(lt_kunnr).
    "write the column carrid of your internal table in your range
    lr_kunnr = VALUE #( FOR <fs_kunnr> IN lt_kunnr
                       (
                         sign = 'I'
                         option = 'EQ'
                         low = <fs_kunnr>-kunnr
                         high = ''
                        ) ).
    SELECT kunnr bukrs FROM knb1 INTO TABLE z_knb1 FOR ALL ENTRIES IN z_kna1
                                                   WHERE bukrs EQ so_bukrs
                                                   AND kunnr EQ z_kna1-kunnr.
    SORT z_knb1 BY kunnr.
    IF lr_kunnr IS NOT INITIAL.
      DELETE z_knb1 WHERE kunnr IN lr_kunnr.
    ENDIF.

*    SELECT kunnr name1 adrnr FROM kna1 INTO TABLE z_kna1 FOR ALL ENTRIES IN z_knb1 WHERE kunnr EQ z_knb1-kunnr.
    SELECT * FROM zmail_id INTO TABLE it_email.
    DELETE it_email WHERE email_id IS INITIAL .
*  SORT Z_KNB1 BY KUNNR.
    IF z_knb1 IS NOT INITIAL.

      LOOP AT z_knb1 INTO y_knb1.
        DATA: e_c(1) TYPE c.
        CLEAR: i_otf[],i_tline[],i_record[],i_xstring,i_objpack[],i_objtxt[],i_objbin[],i_reclist[],wa_objhead[],w_ctrlop,w_compop,w_return,wa_buffer,v_len_in.
        READ TABLE z_kna1 INTO y_kna1 WITH KEY kunnr = y_knb1-kunnr.
        CLEAR: sm_name,sts.
        sm_name = y_kna1-name1.
        sts = y_kna1-kunnr.
        CLEAR:s_email,wa_email.
        IF y_kna1-adrnr IS NOT INITIAL.
*        SELECT SINGLE SMTP_ADDR FROM ADR6 INTO S_EMAIL WHERE ADDRNUMBER EQ Y_KNA1-ADRNR."Commented by SPLABAP during code remediation
          "Added by SPLABAP during code remediation
          SELECT smtp_addr UP TO 1 ROWS
            FROM adr6 INTO s_email WHERE addrnumber EQ y_kna1-adrnr
            ORDER BY PRIMARY KEY.
          ENDSELECT.
          READ TABLE it_email INTO wa_email WITH KEY email_id = s_email.
        ENDIF.
        IF s_email IS NOT INITIAL AND wa_email IS INITIAL.
          CLEAR: lv_buk.
          SELECT bukrs  FROM t001 INTO lv_buk WHERE bukrs EQ y_knb1-bukrs.
            CLEAR: gt_bsad.
            SELECT
               prctr
               kunnr
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
               vbeln
               bukrs
               gjahr
               sknto
               umsks
               augdt
               zuonr
               buzei FROM bsad INTO CORRESPONDING FIELDS OF TABLE gt_bsad WHERE budat <= so_budat-high AND budat >= so_budat-low
            AND bukrs EQ y_knb1-bukrs AND kunnr EQ y_knb1-kunnr AND umskz <> 'H' .

            CLEAR:gt_bsid.
            SELECT
               prctr
               kunnr
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
               vbeln
               umskz
               bukrs
               gjahr
               sknto FROM bsid INTO CORRESPONDING FIELDS OF TABLE gt_bsid WHERE budat <= so_budat-high
            AND budat >= so_budat-low AND bukrs EQ y_knb1-bukrs AND kunnr EQ y_knb1-kunnr AND umskz <> 'H'.

            CLEAR:gt_bsad1.
            SELECT prctr kunnr umskz augbl budat bldat blart bschl zfbdt dmbtr shkzg xblnr belnr gsber sgtxt vbeln bukrs gjahr sknto umsks augdt zuonr buzei FROM bsad
              INTO CORRESPONDING FIELDS OF TABLE gt_bsad1
                    WHERE  budat BETWEEN '01.04.2014'
                     AND so_budat-low
                     AND bukrs EQ y_knb1-bukrs
                     AND kunnr EQ y_knb1-kunnr
            AND umskz <> 'H' .

            CLEAR: gt_bsid1.
            SELECT prctr kunnr augbl budat bldat blart bschl zfbdt dmbtr shkzg xblnr belnr gsber sgtxt vbeln umskz bukrs gjahr sknto FROM bsid
              INTO CORRESPONDING FIELDS OF TABLE gt_bsid1
                    WHERE budat BETWEEN '01.04.2014'
                    AND  so_budat-low
                    AND bukrs EQ y_knb1-bukrs
                     AND kunnr EQ y_knb1-kunnr
            AND umskz <> 'H'.

            CLEAR:gt_bsad7.
            APPEND LINES OF gt_bsid TO gtt_bsid.
            SORT gt_bsad BY belnr augbl.
            APPEND LINES OF gt_bsad TO gt_bsid.
            APPEND LINES OF gt_bsad TO gt_bsad7.
            DELETE gt_bsad7 WHERE shkzg = 'S'.
            APPEND LINES OF gt_bsad1 TO gt_bsid1.

            IF gt_bsid[] IS NOT INITIAL.

              REFRESH :gt_bsak.
              "SELECT LIFNR UMSKZ BELNR XBLNR BLDAT DMBTR BUDAT BLART BSCHL SGTXT GSBER SHKZG BUKRS GJAHR SKNTO
              SELECT bukrs lifnr umsks umskz augdt augbl zuonr gjahr belnr buzei budat bldat xblnr blart dmbtr bschl sgtxt gsber shkzg sknto
               FROM bsak INTO TABLE  gt_bsak
                FOR ALL ENTRIES IN gt_bsid
                WHERE belnr = gt_bsid-belnr
                AND   bukrs = gt_bsid-bukrs
              AND   gjahr = gt_bsid-gjahr.

              CLEAR: gt_kna1[].
              SELECT kunnr name1 adrnr lifnr FROM kna1
                INTO TABLE gt_kna1
                FOR ALL ENTRIES IN gt_bsid
              WHERE kunnr = gt_bsid-kunnr.

              CLEAR: gt_t003t.
              SELECT blart ltext spras FROM  t003t "#EC CI_NOORDER  "Added by SPLABAP during code remediation
                INTO TABLE gt_t003t
                FOR ALL ENTRIES IN gt_bsid
                WHERE blart = gt_bsid-blart
              AND spras = 'E'.

              CLEAR:gt_t003t1.
              SELECT  blart ltext spras FROM  t003t "#EC CI_NOORDER  "Added by SPLABAP during code remediation
                INTO TABLE gt_t003t1
                FOR ALL ENTRIES IN gt_bsad
                WHERE blart = gt_bsad-blart
              AND spras = 'E'.

              APPEND  LINES OF gt_t003t1 TO gt_t003t.

              CLEAR:gt_t003t2.
              SELECT  blart ltext spras FROM  t003t "#EC CI_NOORDER  "Added by SPLABAP during code remediation
                INTO TABLE gt_t003t2
                FOR ALL ENTRIES IN gt_bsik
                WHERE blart = gt_bsik-blart
              AND spras = 'E'.

              APPEND  LINES OF gt_t003t2 TO gt_t003t.

            ENDIF.
            IF gt_kna1[] IS  NOT INITIAL.
              CLEAR: gt_adrc.
              SELECT  addrnumber street city1 city2 post_code1 FROM adrc
                INTO TABLE gt_adrc
                FOR ALL ENTRIES IN gt_kna1
              WHERE addrnumber = gt_kna1-adrnr.

            ENDIF.
            CLEAR: gt_knc1.
            IF gt_bsid IS NOT INITIAL.
              SELECT  kunnr bukrs umsav gjahr FROM knc1
                INTO TABLE gt_knc1
                FOR ALL ENTRIES IN gt_bsid
                 WHERE kunnr = gt_bsid-kunnr
                AND gjahr = gt_bsid-gjahr
              AND bukrs = gt_bsid-bukrs.
            ENDIF.
          ENDSELECT.

          CLEAR: lv_total.
          LOOP AT gt_knc1 INTO wa_knc1.
            lv_total =  wa_knc1-umsav.
          ENDLOOP.

          CLEAR:wa_knc1.
****************Created by prabhu on 18.09.2020
***      CLEAR DBT_AMT.

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

***      LOOP AT ITT_FINAL INTO WAT_FINAL.
***
***        IF WAT_FINAL-SHKZG = 'H'.
***          DBT_AMT =  DBT_AMT + WAT_FINAL-CR_AMT.
***        ENDIF.
***        CLEAR WAT_FINAL.
***
***      ENDLOOP.

****************Created by prabhu on 18.09.2020

          LOOP AT gt_bsid1 INTO wa_bsid1 .

            IF  wa_bsid1-budat < so_budat-low.

              IF wa_bsid1-shkzg = 'S'.

                lv_opn = lv_opn  + wa_bsid1-dmbtr.

              ELSEIF wa_bsid1-shkzg = 'H'.

                lv_opn  = lv_opn - wa_bsid1-dmbtr.

              ENDIF.
            ENDIF.
            CLEAR : wa_bsid1.
          ENDLOOP.
          TYPES : BEGIN OF ty_final,
                    account    TYPE char10,
                    doctyp     TYPE char02,
                    busarea    TYPE werks_d,
                    refdoc     TYPE kunnr,
                    docdate    TYPE sy-datum,
                    pdate      TYPE sy-datum,
                    gl_account TYPE char10,
                    openbal    TYPE bsad-dmbtr,
                    item_txt   TYPE char100,
                  END OF ty_final.
*******************append the data final alv screen***************
          APPEND VALUE #( account    = '10002452'
                          doctyp     = 'TD'
                          busarea    = y_kna1-werks
                          refdoc     = y_kna1-kunnr
                          docdate    = '20230401'
                          pdate      = '20230401'
                          gl_account = '18000001'
                          openbal    = lv_opn
                          item_txt   = |opening outstanding of distributor - { y_kna1-kunnr } | ) TO gt_excel.

          CLEAR: gt_final[] , wa_final , gt_final1[] ,wa_final1 ,gt_final2[] ,wa_final2 ,gt_final3[] ,wa_final3 , lv_opn , lv_buk ,gt_kna1[],gt_final4.
          REFRESH : gt_final[] ,gt_bsid[],gt_bsad[],gt_bsak[],gt_bsik[],gt_bsad1[],gt_bsid1[],itt_final[],i_otf[],i_tline[],gt_final4[].
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Include          ZDMS_OPENING_BALANCE_POST_CLS
*&---------------------------------------------------------------------*
CLASS lcl_handle_events DEFINITION. " Variables for events
  PUBLIC SECTION.

    METHODS:
      on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function. "Event for User command

    METHODS: on_double_click FOR EVENT double_click OF cl_salv_events_table
      IMPORTING row column. "Event for Double click on alv display


ENDCLASS.
CLASS lcl_handle_events IMPLEMENTATION.

  METHOD on_user_command.
    PERFORM handle_user_command USING e_salv_function.
  ENDMETHOD.

  METHOD on_double_click.
    IF column EQ 'DOCNO'.
      READ TABLE gt_alv INTO DATA(wa_alv) INDEX  row.
      SELECT SINGLE * FROM bkpf INTO @DATA(ls_bkpf) WHERE bukrs EQ @gv_bukrs
                                                  AND belnr EQ @wa_alv-docno
      AND gjahr EQ @wa_alv-fisyear.
      IF sy-subrc = 0. " Exists?
        SET PARAMETER ID 'BLN' FIELD wa_alv-docno. "Document Number

        SET PARAMETER ID 'BUK' FIELD gv_bukrs. "Company Code

        SET PARAMETER ID 'GJR' FIELD wa_alv-fisyear. "Fiscal Year

        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.


FORM process.
  DATA: l_lifnr   TYPE elifn,
        l_kunnr   TYPE kunnr,
        l_venname TYPE name1_gp,
        l_cusname TYPE name1_gp.
  DATA: lv_fisyear TYPE bapi0002_4-fiscal_year,
        lv_month   TYPE bapi0002_4-fiscal_period,
        l_return   TYPE bapireturn1.

*************customer block check***********
  DATA : lo_block TYPE REF TO zcl_common_check.
  CREATE OBJECT lo_block.
  DATA : lt_block TYPE TABLE OF zsd_st_cust_block.

  REFRESH gt_alv.
**************call customer check*********
  REFRESH : lt_block.
  lo_block->customer_block_check(
    EXPORTING
      bukrs                     = '1000'      " Company Code
      vkorg                     = '1000'      " Sales Organization
    CHANGING
      cust_tab                  = lt_block    " Table Type for Customer Block Check
      ).
  IF lt_block IS NOT INITIAL.
    SORT : lt_block BY kunnr block.
  ENDIF.

  LOOP AT gt_excel ASSIGNING FIELD-SYMBOL(<fs_excel>).
    CLEAR: l_lifnr,l_kunnr,l_venname,l_cusname.
    IF <fs_excel>-account IS NOT INITIAL.
      DATA(l_account) = |{ <fs_excel>-account ALPHA = IN }|.
    ENDIF.
    DATA(l_glacc) = |{ <fs_excel>-gl_account ALPHA = IN }|.
*** function module to get fiscal year ***
    CLEAR: lv_fisyear,lv_month,l_return.
    CALL FUNCTION 'BAPI_COMPANYCODE_GET_PERIOD'
      EXPORTING
        companycodeid = gv_bukrs
        posting_date  = <fs_excel>-docdate
      IMPORTING
        fiscal_year   = lv_fisyear
        fiscal_period = lv_month
        return        = l_return.

*****************check distributor block or not*******************
    IF lt_block IS NOT INITIAL.
      <fs_excel>-refdoc = |{ <fs_excel>-refdoc ALPHA = IN }|.
      READ TABLE lt_block TRANSPORTING NO FIELDS WITH KEY kunnr = <fs_excel>-refdoc
                                                          block = abap_true BINARY SEARCH.
      IF sy-subrc = 0.
        APPEND VALUE #( account = l_account
                        name1   = l_venname
                        acctyp = 'K'
                        refdoc  = <fs_excel>-refdoc
                        msgtyp = 'E'
                        message = |Distributor is blocked| ) TO gt_alv.
        CONTINUE.
      ENDIF.
    ENDIF.

****************check amount zero***********

    IF <fs_excel>-openbal IS INITIAL.
      APPEND VALUE #( account = l_account
                      name1   = l_venname
                      acctyp = 'K'
                      refdoc  = <fs_excel>-refdoc
                      msgtyp = 'E'
                      message = |Distributor opening balance zero| ) TO gt_alv.
      CONTINUE.
    ENDIF.

*    IF p_rad1 = abap_true. "Vendor Posting
  "Checking Vendor Number
  SELECT SINGLE lifnr, name1 FROM lfa1 INTO ( @l_lifnr , @l_venname ) WHERE lifnr = @l_account.
    IF sy-subrc NE 0.
      APPEND VALUE #( account = l_account
                      name1   = l_venname
                      acctyp = 'K'
                      refdoc  = <fs_excel>-refdoc
                      msgtyp = 'E'
                      message = |Incorrect Vendor Number| ) TO gt_alv.
      CONTINUE.
    ENDIF.
    "GL Account existence checks
    SELECT SINGLE saknr, txt50 FROM skat INTO @DATA(l_glacc_dtls) WHERE saknr = @l_glacc.
      IF sy-subrc NE 0.
        APPEND VALUE #( account = l_account
                        name1   = l_venname
                        acctyp  = 'K'
                        refdoc  = <fs_excel>-refdoc
                        msgtyp  = 'E'
                        message = |Incorrect GL Account Number| ) TO gt_alv.
        CONTINUE.
      ENDIF.
      "Reference Document Checks
      SELECT SINGLE * FROM bsip INTO @DATA(ls_bsip) WHERE bukrs = @gv_bukrs
                                                    AND lifnr = @l_lifnr
                                                    AND gjahr = @lv_fisyear
      AND xblnr = @<fs_excel>-refdoc.
        IF sy-subrc = 0.
          APPEND VALUE #( account = l_account
                          name1   = l_venname
                          acctyp  = 'K'
                          refdoc  = <fs_excel>-refdoc
                          msgtyp  = 'E'
                          message = |Already Document with same reference Available| ) TO gt_alv.
          CONTINUE.
        ENDIF.
        "Business Area checks
        IF <fs_excel>-busarea IS NOT INITIAL.
          SELECT SINGLE * FROM tgsbt INTO @DATA(l_busarea) WHERE spras = @sy-langu
          AND gsber = @<fs_excel>-busarea.
            IF sy-subrc NE 0.
              APPEND VALUE #( account = l_account
                              name1   = l_venname
                              acctyp  = 'K'
                              refdoc  = <fs_excel>-refdoc
                              gl_account = l_glacc_dtls-saknr
                              glacc_txt = l_glacc_dtls-txt50
                              msgtyp  = 'E'
                              message = |Incorrect Business Area| ) TO gt_alv.
              CONTINUE.
            ENDIF.
          ENDIF.
          "Adding the Success Msg
          APPEND VALUE #( account = l_lifnr
                          name1 = l_venname
                          acctyp = 'K'
                          blart = <fs_excel>-doctyp
                          refdoc = <fs_excel>-refdoc
                          fisyear = lv_fisyear
                          docdate = <fs_excel>-docdate
                          pdate = <fs_excel>-pdate
                          gl_account = l_glacc_dtls-saknr
                          glacc_txt = l_glacc_dtls-txt50
                          amt = <fs_excel>-openbal
                          item_txt = <fs_excel>-item_txt
                          gsber  = <fs_excel>-busarea
                          msgtyp = 'S'
                          message = |No errror in lineitems| ) TO gt_alv.

        ENDLOOP.
ENDFORM.

FORM alv_display.
  DATA: lo_gr_functions TYPE REF TO cl_salv_functions_list.

  DATA: lo_event_handler TYPE REF TO lcl_handle_events, " Variables for events
        lo_events        TYPE REF TO cl_salv_events_table.

  DATA: lo_grid        TYPE REF TO cl_salv_form_layout_grid, " Variables for header
        lo_layout_logo TYPE REF TO cl_salv_form_layout_logo,
        lo_content     TYPE REF TO cl_salv_form_element.

  DATA: lo_layout TYPE REF TO cl_salv_layout, " Variables for enabling Save button
        lv_key    TYPE salv_s_layout_key.

  DATA: lo_display TYPE REF TO cl_salv_display_settings. " Variable for layout settings

  DATA: lo_selections TYPE REF TO cl_salv_selections, " Variables for selection mode and column properties
        lo_columns    TYPE REF TO cl_salv_columns,
        lo_column     TYPE REF TO cl_salv_column_table.
  DATA: lr_aggregations TYPE REF TO cl_salv_aggregations.
  DATA: lr_groups TYPE REF TO cl_salv_sorts .
  DATA: toolbar TYPE REF TO cl_salv_functions_list .
* create the alv object
  TRY.
      CALL METHOD cl_salv_table=>factory
        IMPORTING
          r_salv_table = lo_gr_alv
        CHANGING
          t_table      = gt_alv.
    CATCH cx_salv_msg.
  ENDTRY.

  lo_gr_alv->set_screen_status(
      pfstatus      =  'POSTING'
      report        =  sy-repid
      set_functions = lo_gr_alv->c_functions_all ).

* Let's show all default buttons of ALV
  lo_gr_functions = lo_gr_alv->get_functions( ).
  lo_gr_functions->set_all( value  = if_salv_c_bool_sap=>true ).

* Fit the columns
  lo_columns = lo_gr_alv->get_columns( ).
  lo_columns->set_optimize( 'X' ).


* Apply zebra style to lv_rows
  lo_display = lo_gr_alv->get_display_settings( ).
  lo_display->set_striped_pattern( cl_salv_display_settings=>true ).

* Register events
  lo_events = lo_gr_alv->get_event( ).
  CREATE OBJECT lo_event_handler.
  SET HANDLER lo_event_handler->on_user_command FOR lo_events.
  SET HANDLER lo_event_handler->on_double_click FOR lo_events.

  TRY.
      lo_column ?= lo_columns->get_column( 'ACCOUNT' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Account code' ).
      lo_column->set_medium_text( 'Account' ).
      lo_column->set_short_text( 'Account' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'NAME1' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Name' ).
      lo_column->set_medium_text( 'Name' ).
      lo_column->set_short_text( 'Name' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'ACCTYP' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Account type' ).
      lo_column->set_medium_text( 'Account type' ).
      lo_column->set_short_text( 'Acctyp' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'REFDOC' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Reference Doc' ).
      lo_column->set_medium_text( 'Reference' ).
      lo_column->set_short_text( 'Reference' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'DOCDATE' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Document date' ).
      lo_column->set_medium_text( 'Document date' ).
      lo_column->set_short_text( 'Docdate' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'PDATE' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Posting Date' ).
      lo_column->set_medium_text( 'Posting Date' ).
      lo_column->set_short_text( 'Postdate' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'BLART' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Document Type' ).
      lo_column->set_medium_text( 'Document Type' ).
      lo_column->set_short_text( 'Doctype' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'FISYEAR' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Fiscal Year' ).
      lo_column->set_medium_text( 'Fiscal Year' ).
      lo_column->set_short_text( 'Fisyear' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'GL_ACCOUNT' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'GL Account' ).
      lo_column->set_medium_text( 'GL Account' ).
      lo_column->set_short_text( 'GL Account' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'GLACC_TXT' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'GL Acctext' ).
      lo_column->set_medium_text( 'GL Acctext' ).
      lo_column->set_short_text( 'GL Acctext' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.


  TRY.
      lo_column ?= lo_columns->get_column( 'AMT' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Amount' ).
      lo_column->set_medium_text( 'Amount' ).
      lo_column->set_short_text( 'Amount' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'MSGTYP' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Msgtyp' ).
      lo_column->set_medium_text( 'Msgtyp' ).
      lo_column->set_short_text( 'Msgtyp' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'MESSAGE' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Message' ).
      lo_column->set_medium_text( 'Message' ).
      lo_column->set_short_text( 'Message' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  lo_gr_alv->display( ).
ENDFORM.


FORM handle_user_command USING i_ucomm TYPE salv_de_function.

  CASE i_ucomm.
    WHEN '&INV_POST'.
      PERFORM f_posting.
      lo_gr_alv->refresh( ).
  ENDCASE.
ENDFORM.
FORM f_posting.
  DATA: lv_fund TYPE bapiacgl09-fund.
  DATA: lv_msg_text TYPE string.
  DATA: lv_objtyp TYPE bapiache09-obj_type.
  DATA: lv_objkey TYPE bapiache09-obj_key.
  DATA: lv_objsys TYPE bapiache09-obj_sys.
  DATA: lt_glaccount  TYPE TABLE OF bapiacgl09,
        lt_payable    TYPE TABLE OF bapiacap09,
        lt_recievable TYPE TABLE OF bapiacar09,
        lt_curramnt   TYPE TABLE OF bapiaccr09,
        lt_return     TYPE TABLE OF bapiret2.
  DATA: lv_belnr TYPE belnr_d.

  LOOP AT gt_alv ASSIGNING FIELD-SYMBOL(<fs_alv>) WHERE msgtyp = 'S'.
    DATA(l_headers) = VALUE bapiache09( bus_act = 'RFBU'
                                        username = sy-uname
                                        comp_code = gv_bukrs
                                        doc_date = <fs_alv>-docdate
                                        pstng_date = <fs_alv>-pdate
                                        ref_doc_no = <fs_alv>-refdoc
                                        fisc_year = <fs_alv>-fisyear
                                        doc_type = <fs_alv>-blart ).

    REFRESH: lt_glaccount,lt_curramnt,lt_return.

*    IF p_rad1 = abap_true.
    lt_payable = VALUE #( ( itemno_acc = '1'
                            vendor_no = <fs_alv>-account
                            item_text = <fs_alv>-item_txt
                            bus_area = <fs_alv>-gsber  ) ).

    lt_glaccount = VALUE #( ( itemno_acc = '2'
                              gl_account = <fs_alv>-gl_account
                              item_text = <fs_alv>-item_txt
                              bus_area = <fs_alv>-gsber ) ).

    lt_curramnt = VALUE #( ( itemno_acc = '1'
                             currency = 'INR'
                             amt_doccur = ( <fs_alv>-amt * -1 ) )
                           ( itemno_acc = '2'
                             currency = 'INR'
                             amt_doccur = <fs_alv>-amt ) ).

*    ELSE.
*      lt_recievable = VALUE #( ( itemno_acc = '1'
*                                 customer = <fs_alv>-account
*                                 item_text = <fs_alv>-item_txt
*                                 bus_area = <fs_alv>-gsber  ) ).
*
*      lt_glaccount  = VALUE #( ( itemno_acc = '2'
*                                 gl_account = <fs_alv>-gl_account
*                                 item_text = <fs_alv>-item_txt
*                                 bus_area = <fs_alv>-gsber ) ).
*
*      lt_curramnt = VALUE #( (   itemno_acc = '1'
*                                 currency = 'INR'
*                                 amt_doccur =  <fs_alv>-amt )
*                             (   itemno_acc = '2'
*                                 currency = 'INR'
*                                 amt_doccur = ( <fs_alv>-amt * -1 ) ) ).
*    ENDIF.

*** Document Check Before posting ***
    CALL FUNCTION 'BAPI_ACC_DOCUMENT_CHECK'
      EXPORTING
        documentheader    = l_headers
      TABLES
        accountgl         = lt_glaccount
        accountreceivable = lt_recievable
        accountpayable    = lt_payable
        currencyamount    = lt_curramnt
        return            = lt_return.
    READ TABLE lt_return INTO DATA(lw_ret) WITH KEY type = 'E'.
    IF sy-subrc EQ 0.
      CLEAR lv_msg_text.
      LOOP AT lt_return INTO DATA(l_ret) WHERE ( type = 'E' OR type = 'A' ).
        lv_msg_text = |{ lv_msg_text },{ l_ret-message }|.
      ENDLOOP.
      <fs_alv>-msgtyp = 'E'.
      <fs_alv>-message = lv_msg_text.
    ELSE.
      IF p_chk3 NE abap_true.
        REFRESH: lt_return.
        CLEAR: lv_objkey,lv_objsys,lv_objtyp.
*** Function Module to create Debit note ***
        CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
          EXPORTING
            documentheader    = l_headers
          IMPORTING
            obj_type          = lv_objtyp
            obj_key           = lv_objkey
            obj_sys           = lv_objsys
          TABLES
            accountgl         = lt_glaccount
            accountreceivable = lt_recievable
            accountpayable    = lt_payable
            currencyamount    = lt_curramnt
            return            = lt_return.
        COMMIT WORK AND WAIT.
        CLEAR lv_belnr.
        lv_belnr = lv_objkey+0(10).
        <fs_alv>-docno = lv_belnr.
        <fs_alv>-msgtyp = 'S'.
        <fs_alv>-message = |Document { lv_belnr } Posted successfully|.
      ELSE.
        <fs_alv>-msgtyp = 'S'.
        <fs_alv>-message = |Document Ready to Post|.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.
