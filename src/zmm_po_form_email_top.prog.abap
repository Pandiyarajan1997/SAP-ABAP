*&---------------------------------------------------------------------*
*& Include          ZMM_PO_FORM_EMAIL_TOP
*&---------------------------------------------------------------------*
TYPES: BEGIN OF ty_ekko,
         ebeln TYPE ebeln,
         aedat TYPE aedat,
         lifnr TYPE lifnr,
       END OF ty_ekko.

DATA: gt_bsart TYPE RANGE OF ekko-bsart,
      gs_bsart LIKE LINE OF gt_bsart.

TYPES: BEGIN OF ty_alv,
         po_num   TYPE ebeln,
         ven_id   TYPE lifnr,
         venmail  TYPE char100,
         msg(150) TYPE c,
       END OF ty_alv.

DATA: gt_alv TYPE TABLE OF ty_alv,
      gs_alv TYPE ty_alv.

DATA: gt_ekko TYPE TABLE OF ty_ekko,
      gs_ekko TYPE ty_ekko.

DATA: gt_logs TYPE STANDARD TABLE OF zpo_email_log,
      gs_logs TYPE zpo_email_log.

DATA:is_ekko     TYPE  ekko,
     is_pekko    TYPE  pekko,
     is_nast     TYPE  nast,
     iv_from_mem TYPE  c,
     iv_druvo    TYPE  druvo,
     it_ekpo     TYPE TABLE OF ekpo  WITH HEADER LINE,
     it_ekpa     TYPE TABLE OF  ekpa,
     it_pekpo    TYPE TABLE OF  pekpo,
     it_eket     TYPE TABLE OF  eket,
     it_tkomv    TYPE TABLE OF  komv,
     it_ekkn     TYPE TABLE OF  ekkn,
     it_ekek     TYPE TABLE OF  ekek,
     it_komk     TYPE TABLE OF  komk,
     it_tab      TYPE TABLE OF  zmm_gst_po_struc,
     l_xkomk     LIKE TABLE OF komk WITH HEADER LINE,
     xscreen.

TABLES: cpkme,
        ekvkp,
        ekko,
        pekko,
        rm06p,
        ekpo,
        pekpo,
        pekpov,
        pekpos,
        eket,
        ekek,
        ekes,
        ekeh,
        ekkn,
        ekpa,
        ekbe,
        eine, *eine,
        lfa1,
        likp,
        *lfa1,
        kna1,
        komk,
        komp,
        komvd,
        ekomd,
        econf_out,
        thead, *thead,
        sadr,
        mdpa,
        mdpm,
        mkpf,
        tinct,
        ttxit,
        tmsi2,
        tq05,
        tq05t,
        t001,
        t001w,
        t006, *t006,
        t006a, *t006a,
        t024,
        t024e,
        t027a,
        t027b,
        t052,
        t161n,
        t163d,
        t166a,
        t165p,
        t166c,
        t166k,
        t166p,
        t166t,
        t166u,
        t165m,
        t165a,
        tmamt,
        *mara,                                               "HTN 4.0C
        mara,
        marc,
        mt06e,
        makt,
        vbak,
        vbkd,
        *vbkd,
        vbap.
TABLES: drad,
        drat.
TABLES: addr1_sel,
        addr1_val.
TABLES: v_htnm, rampl,tmppf.           "HTN-Abwicklung

TABLES: stxh.              "schnellerer Zugriff auf Texte Dienstleistung

TABLES: t161.              "Abgebotskennzeichen für Dienstleistung

DATA: ls_ekko TYPE ekko,
      ls_ekpo TYPE uekpo.

*/.. Work Area Declarations
DATA: wa_return  TYPE ssfcrescl,
      lw_content TYPE soli,
      l_devtype  TYPE rspoptype,
      lw_otf     TYPE itcoo.
*/..Variables Decalrations
DATA: gv_fname        TYPE rs38l_fnam,        "fucntion MODULE
      gv_subject      TYPE so_obj_des,
      gv_title        TYPE so_obj_des,
      lv_bin_filesize TYPE i,
      lv_transfer_bin TYPE sx_boolean,
      lv_len          TYPE so_obj_len.

DATA: control               TYPE ssfctrlop,
      output_options        TYPE ssfcompop,
* Internal table decleration
      gt_ssfctrlop          TYPE TABLE OF ssfctrlop,
* Work area decleration
      gw_ssfcompop          TYPE ssfcompop,
      gw_nast               TYPE nast,
      gs_job_output_options TYPE ssfcresop.

DATA: fm_name TYPE rs38l_fnam,
      v_fname TYPE rs38l_fnam.
*/.. OTF & PDF Internal Table Declaration
DATA: li_otf         TYPE TABLE OF itcoo,
      li_pdf_tab     TYPE TABLE OF tline,
      li_content_txt TYPE soli_tab,
      li_content_hex TYPE solix_tab,
      li_objhead     TYPE soli_tab,
      i_xstring      TYPE xstring,
      i_objbin       LIKE solix      OCCURS 0 WITH HEADER LINE.
*            gi_main_text    TYPE bcsy_text.

DATA: file_size TYPE i.
DATA: filename TYPE string.


DATA: pdf_content TYPE  solix_tab,
      p_app1      TYPE string,
      cf_retcode  TYPE sy-subrc,
      l_message   TYPE string,
      l_pdf       TYPE LINE OF solix_tab.
DATA: pdf_bin TYPE xstring.

DATA: gv_ebeln TYPE ekko-ebeln,
      gv_aedat TYPE ekko-aedat,
      gv_bsart TYPE ekko-bsart,
      v_len_in LIKE sood-objlen.
" Mail Generation Variables**
DATA: salutation TYPE string.
DATA: body TYPE string.
DATA: footer TYPE string.
DATA: lo_send_request TYPE REF TO cl_bcs,
      lo_document     TYPE REF TO cl_document_bcs,
      lo_sender       TYPE REF TO if_sender_bcs,
      sender_mail     TYPE  adr6-smtp_addr,
      lo_recipient    TYPE REF TO if_recipient_bcs VALUE IS INITIAL,lt_message_body TYPE bcsy_text,
      lo_recipient1   TYPE REF TO if_recipient_bcs VALUE IS INITIAL, " if_recipient_bcs VALUE IS INITIAL,"lt_message_body TYPE bcsy_text,
      lo_recipient2   TYPE REF TO if_recipient_bcs VALUE IS INITIAL,
      lo_recipient3   TYPE REF TO if_recipient_bcs VALUE IS INITIAL,
      lx_document_bcs TYPE REF TO cx_document_bcs,
      lv_sent_to_all  TYPE os_boolean.
DATA : lv_vbeln TYPE vbrk-vbeln,
       lv_ind   TYPE char20,
       lv_netwr TYPE string,
       lv_tot   TYPE char50.
DATA: gv_venmail TYPE char70.
DATA: gv_venmail1 TYPE char250.
DATA:   repeat(1) TYPE c.
DATA : lv_sub TYPE sood-objdes.
DATA: in_mailid  TYPE ad_smtpadr,
      in_mailid1 TYPE ad_smtpadr,
      in_mailid2 TYPE ad_smtpadr,
      in_mailid3 TYPE ad_smtpadr.

DATA: gv_date  TYPE tzntstmpl,
      gv_date1 TYPE tzntstmpl.

* Internal Table and Work Area Declaration for FieldCatlog
DATA: gs_layout TYPE  slis_layout_alv,
* Internal Table Declaration for FieldCatlog
      gt_fcat   TYPE  slis_t_fieldcat_alv,
* Work Area Declaration for FieldCatlog
      gs_fcat   TYPE slis_fieldcat_alv.
