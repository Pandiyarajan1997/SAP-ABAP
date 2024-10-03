*&---------------------------------------------------------------------*
*& Include          ZCREDIT_NOTE_PRG_TOP
*&---------------------------------------------------------------------*
TABLES:bseg,bkpf,bset,t001w,adrc,makt,marc.
TABLES: nast,                          "Messages
        tnapr,                         "Programs & Forms
        itcpo,                         "Communicationarea for Spool
        arc_params,                    "Archive parameters
        toa_dara,                      "Archive parameters
        addr_key.
TYPES:BEGIN OF ty_bseg,
        bukrs TYPE bseg-bukrs,
        belnr TYPE bseg-belnr,
        gjahr TYPE bseg-gjahr,
        buzei TYPE bseg-buzei,
        buzid TYPE bseg-buzid,
        augdt TYPE bseg-augdt,
        augbl TYPE bseg-augbl,
        umskz TYPE bseg-umskz,
        umsks TYPE bseg-umsks,
        shkzg TYPE bseg-shkzg,
        dmbtr TYPE bseg-dmbtr,
        wrbtr TYPE bseg-wrbtr,
        kzbtr TYPE bseg-kzbtr,
        shzuz TYPE bseg-shzuz,
        matnr TYPE bseg-matnr,
        werks TYPE bseg-werks,
        menge TYPE bseg-menge,
        meins TYPE bseg-meins,
        ebeln TYPE bseg-ebeln,
        ebelp TYPE bseg-ebelp,
      END OF ty_bseg.

DATA:it_bseg TYPE TABLE OF ty_bseg,
     wa_bseg TYPE  ty_bseg.

TYPES:BEGIN OF ty_bkpf,
        bukrs TYPE bkpf-bukrs,
        belnr TYPE bkpf-belnr,
        gjahr TYPE bkpf-gjahr,
        blart TYPE bkpf-blart,
        bldat TYPE bkpf-bldat,
        budat TYPE bkpf-budat,
        tcode TYPE bkpf-tcode,
        xblnr TYPE bkpf-xblnr,
      END OF ty_bkpf.

DATA: it_bkpf	TYPE TABLE OF	ty_bkpf,
      wa_bkpf TYPE  ty_bkpf.

TYPES : BEGIN OF ty_bset,
          bukrs TYPE bset-bukrs,
          belnr TYPE bset-belnr,
          gjahr TYPE bset-gjahr,
          shkzg TYPE bset-shkzg,
          fwste TYPE bset-fwste,
          kschl TYPE bset-kschl,
          kbetr TYPE bset-kbetr,
        END OF ty_bset.

DATA:it_bset TYPE TABLE OF ty_bset,
     wa_bset TYPE  ty_bset.

TYPES: BEGIN OF ty_lfa1,
         lifnr TYPE lfa1-lifnr,
         name1 TYPE lfa1-name1,
         ort01 TYPE lfa1-ort01,
         pstlz TYPE lfa1-pstlz,
         regio TYPE lfa1-regio,
         stras TYPE lfa1-stras,
         adrnr TYPE lfa1-adrnr,
         kunnr TYPE lfa1-kunnr,
         kraus TYPE lfa1-kraus,
         werks TYPE lfa1-werks,
       END OF ty_lfa1.

DATA:it_lfa1 TYPE TABLE OF ty_lfa1,
     wa_lfa1 TYPE  ty_lfa1.

TYPES: BEGIN OF ty_makt,
         matnr TYPE makt-matnr,
         spras TYPE makt-spras,
         maktx TYPE makt-maktx,
       END OF ty_makt.

DATA:it_makt TYPE TABLE OF ty_makt,
     wa_makt TYPE  ty_makt.

TYPES: BEGIN OF ty_marc,
         matnr TYPE marc-matnr,
         werks TYPE marc-werks,
         steuc TYPE marc-steuc,
       END OF ty_marc.

DATA:it_marc TYPE TABLE OF ty_marc,
     wa_marc TYPE TABLE OF ty_marc.

TYPES: BEGIN OF ty_t001w,
         werks TYPE t001w-werks,
         name1 TYPE t001w-name1,
         kunnr TYPE t001w-kunnr,
         lifnr TYPE t001w-lifnr,
         stras TYPE t001w-stras,
         pstlz TYPE t001w-pstlz,
         ort01 TYPE t001w-ort01,
         regio TYPE t001w-regio,
         cityc TYPE t001w-cityc,
         adrnr TYPE t001w-adrnr,
       END OF ty_t001w.

DATA:it_t001w TYPE TABLE OF ty_t001w,
     wa_t001w TYPE  ty_t001w.



TYPES: BEGIN OF ty_adrc,
         addrnumber TYPE adrc-addrnumber,
         name1      TYPE adrc-name1,
         city2      TYPE adrc-city2,
         street     TYPE adrc-street,
         house_num1 TYPE adrc-house_num1,
         region     TYPE adrc-region,
         sort1      TYPE adrc-sort1,
       END OF ty_adrc.

DATA:it_adrc TYPE TABLE OF ty_adrc,
     wa_adrc TYPE  ty_adrc.

TYPES :BEGIN OF ty_final,
         bukrs       TYPE bkpf-bukrs,
         belnr       TYPE bkpf-belnr,
         gjahr       TYPE bkpf-gjahr,
         blart       TYPE bkpf-blart,
         bldat       TYPE bkpf-bldat,
         budat       TYPE bkpf-budat,
         tcode       TYPE bkpf-tcode,
         xblnr       TYPE bkpf-xblnr,

         bseg_bukrs  TYPE bseg-bukrs,
         bseg_belnr  TYPE bseg-belnr,
         bseg_gjahr  TYPE bseg-gjahr,
         buzei       TYPE bseg-buzei,
         buzid       TYPE bseg-buzid,
         augdt       TYPE bseg-augdt,
         augbl       TYPE bseg-augbl,
         umskz       TYPE bseg-umskz,
         umsks       TYPE bseg-umsks,
         shkzg       TYPE bseg-shkzg,
         dmbtr       TYPE bseg-dmbtr,
         wrbtr       TYPE bseg-wrbtr,
         kzbtr       TYPE bseg-kzbtr,
         shzuz       TYPE bseg-shzuz,
         matnr       TYPE bseg-matnr,
         werks       TYPE bseg-werks,
         menge       TYPE bseg-menge,
         meins       TYPE bseg-meins,
         ebeln       TYPE bseg-ebeln,
         ebelp       TYPE bseg-ebelp,

         bset_bukrs  TYPE bset-bukrs,
         bset_belnr  TYPE bset-belnr,
         bset_gjahr  TYPE bset-gjahr,
         bset_shkzg  TYPE bset-shkzg,
         fwste       TYPE bset-fwste,
         kschl       TYPE bset-kschl,
         kbetr       TYPE bset-kbetr,

         lifnr       TYPE lfa1-lifnr,
         name1       TYPE lfa1-name1,
         ort01       TYPE lfa1-ort01,
         pstlz       TYPE lfa1-pstlz,
         regio       TYPE lfa1-regio,
         stras       TYPE lfa1-stras,
         adrnr       TYPE lfa1-adrnr,
         kunnr       TYPE lfa1-kunnr,
         kraus       TYPE lfa1-kraus,
         lfa1_werks  TYPE lfa1-werks,

         makt_matnr  TYPE makt-matnr,
         spras       TYPE makt-spras,
         maktx       TYPE makt-maktx,

         marc_matnr  TYPE marc-matnr,
         marc_werks  TYPE marc-werks,
         steuc       TYPE marc-steuc,

         t001w_werks TYPE t001w-werks,
         t001w_name1 TYPE t001w-name1,
         t001w_kunnr TYPE t001w-kunnr,
         t001w_lifnr TYPE t001w-lifnr,
         t001w_stras TYPE t001w-stras,
         t001w_pstlz TYPE t001w-pstlz,
         t001w_ort01 TYPE t001w-ort01,
         t001w_regio TYPE t001w-regio,
         cityc       TYPE t001w-cityc,
         t001w_adrnr TYPE t001w-adrnr,

         addrnumber  TYPE adrc-addrnumber,
         adrc_name1  TYPE adrc-name1,
         city2       TYPE adrc-city2,
         street      TYPE adrc-street,
         house_num1  TYPE adrc-house_num1,
         region      TYPE adrc-region,
         sort1       TYPE adrc-sort1,

       END OF ty_final.

TYPES:BEGIN OF ty_bkpf1,
        bukrs TYPE bseg-bukrs,
        belnr TYPE bseg-belnr,
        gjahr TYPE bseg-gjahr,
      END OF ty_bkpf1.

TYPES:BEGIN OF ty_bseg1,
        bukrs TYPE bseg-bukrs,
        belnr TYPE bseg-belnr,
        gjahr TYPE bseg-gjahr,
        lifnr TYPE elifn,
      END OF ty_bseg1.

TYPES: BEGIN OF ty_bp001,         "Structure for BP vendor Classification
         partner       TYPE bu_partner,
         group_feature TYPE bp_group_feature,
       END OF ty_bp001.

TYPES: BEGIN OF ty_ibps,
         supplier        TYPE lifnr,
         businesspartner TYPE bu_partner,
       END OF ty_ibps.
*****************************************************************
* Internal table Declarations ***
DATA:it_final TYPE TABLE OF ty_final,
     wa_final TYPE  ty_final.

DATA:fm_name   TYPE rs38l_fnam,
     fm_name1  TYPE rs38l_fnam,
     smf_name  TYPE tdsfname VALUE 'ZCREDIT_NOTE',
     smf_name1 TYPE tdsfname VALUE 'ZDEBIT_NOTE'.

"added on 15.10.2022 by samsudeen
DATA: ls_control_param      TYPE ssfctrlop.
DATA: ls_composer_param     TYPE ssfcompop.
DATA: ls_recipient          TYPE swotobjid.
DATA: ls_sender             TYPE swotobjid.
DATA: w_return    TYPE ssfcrescl.

DATA: i_otf       TYPE itcoo    OCCURS 0 WITH HEADER LINE,
      i_tline     LIKE tline    OCCURS 0 WITH HEADER LINE,
      i_record    LIKE solisti1 OCCURS 0 WITH HEADER LINE,
      i_xstring   TYPE xstring,
* Objects to send mail.
      i_objpack   LIKE sopcklsti1 OCCURS 0 WITH HEADER LINE,
      i_objtxt    LIKE solisti1   OCCURS 0 WITH HEADER LINE,
      i_objbin    LIKE solix      OCCURS 0 WITH HEADER LINE,
      i_reclist   LIKE somlreci1  OCCURS 0 WITH HEADER LINE,
* Variables declarations
      v_form_name TYPE rs38l_fnam,
      v_len_in    LIKE sood-objlen.
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
      lx_send_req_bcs TYPE REF TO cx_send_req_bcs,
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
DATA: gt_bkpf TYPE TABLE OF ty_bkpf1,
      gs_bkpf TYPE ty_bkpf1.
DATA: gt_bseg TYPE TABLE OF ty_bseg1,
      gs_bseg TYPE ty_bseg1.
DATA: gt_bp001 TYPE STANDARD TABLE OF ty_bp001,
      gs_bp001 TYPE ty_bp001.
DATA: gt_ibps TYPE TABLE OF ty_ibps,
      gs_ibps TYPE ty_ibps.

TYPES: BEGIN OF output,
         docno   TYPE belnr_d,
         fisyr   TYPE gjahr,
         vendor  TYPE lifnr,
         venname TYPE name1_gp,
         mail    TYPE ad_smtpadr,
         ccmail  TYPE char250,
         msg     TYPE char50,
       END OF output.
DATA: gt_output TYPE TABLE OF output.
DATA lo_gr_alv       TYPE REF TO cl_salv_table. " Variables for ALV properties
****************************************************************************
