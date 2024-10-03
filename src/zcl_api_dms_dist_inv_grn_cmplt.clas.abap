class ZCL_API_DMS_DIST_INV_GRN_CMPLT definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .

  data:
    it_vbrk1     TYPE TABLE OF zstr_bill_dms .
  data:
    it_vbrp1     TYPE TABLE OF zstr_bill_item_dms .
  data:
    it_btno1     TYPE TABLE OF zstr_lips_bno_dms .
  data:
    it_customer1 TYPE TABLE OF kunnr_sty .

  methods PROCESS
    importing
      !INV_BEGDA type BEGDA
      !INV_ENDDA type ENDDA
      !GRN_BEGDA type BEGDA
      !GRN_ENDDA type ENDDA
      !CUST_NO type KUNNR_TTY
    exporting
      !IT_VBRK like IT_VBRK1
      !IT_VBRP like IT_VBRP1
      !IT_BTNO like IT_BTNO1
      !IT_CUSTOMER like IT_CUSTOMER1 .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_API_DMS_DIST_INV_GRN_CMPLT IMPLEMENTATION.


  METHOD IF_HTTP_EXTENSION~HANDLE_REQUEST.
    "Created by: Pandiarajan
    "Created on: 05.03.2024
    "Reference by: Ramakrishnan J
    "Purpose : send the grn completed invoice details to DMS
*-------------------------------------------------------------*
    TYPES: BEGIN OF body,
             inv_from TYPE datum,
             inv_to   TYPE datum,
             grn_from TYPE datum,
             grn_to   TYPE datum,
             customer TYPE kunnr_tty,
           END OF body.

    TYPES: BEGIN OF ty_res,
             customer TYPE  kunnr,
             cusname  TYPE  name1_gp,
             type     TYPE  bapi_mtype,
             msg      TYPE  string,
             invoices TYPE  zdist_inv_api_resp_invtab_tt,
           END OF ty_res.

    DATA: gs_input TYPE body.

    DATA :v_jsonload TYPE string.

    DATA: gt_response TYPE STANDARD TABLE OF ty_res,
          gs_response TYPE ty_res.


    DATA: lt_vbrk  TYPE TABLE OF zstr_bill,
          lt_vbrp  TYPE TABLE OF zstr_bill_item_app,
          lt_batch TYPE TABLE OF zstr_lips_bno.

    DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
    CREATE OBJECT lo_log_upd.

    CALL METHOD server->request->get_cdata RECEIVING data = DATA(lv_data).

** deserialize the INPUT our required INPUT ***
    /ui2/cl_json=>deserialize(
   EXPORTING
     json         = lv_data
     pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
   CHANGING
     data         = gs_input ).


    DATA date_frm TYPE sy-datum.
    DATA date_to TYPE sy-datum.

    DATA: it_vbrk     TYPE TABLE OF zstr_bill_dms,
          it_vbrp     TYPE TABLE OF zstr_bill_item_dms,
          it_btno     TYPE TABLE OF zstr_lips_bno_dms,
          it_customer TYPE TABLE OF kunnr_sty.

    DATA: ls_invoices TYPE zdist_inv_api_resp_invtab,
          ls_items    TYPE zdist_inv_api_resp_invitmtab,
          ls_batch    TYPE zdist_inv_api_resp_invitmbtch.

    IF gs_input-inv_from IS NOT INITIAL AND gs_input-grn_from IS NOT INITIAL." AND date_to IS INITIAL.

      CLEAR : gs_response.
      gs_response-type = 'E'.
      gs_response-msg  = 'Provide GRN dates or INV dates'.
      APPEND gs_response TO gt_response.

    ENDIF.

    IF gt_response IS INITIAL.

      IF gs_input-inv_from IS INITIAL." AND date_to IS INITIAL.
        gs_input-inv_from = sy-datum.
        gs_input-inv_from+6(2) = '01'.

        IF sy-datum+4(2) = '01'.
          gs_input-inv_from+0(4) = gs_input-inv_from+0(4) - 1.
          gs_input-inv_from+4(2) = '12'.
        ELSE.
          gs_input-inv_from+4(2) = gs_input-inv_from+4(2) - 1.
        ENDIF.

      ENDIF.

      IF gs_input-inv_to IS INITIAL.
        gs_input-inv_to  = sy-datum.
      ENDIF.
      IF gs_input-grn_to IS INITIAL.
        gs_input-grn_to  = sy-datum.
      ENDIF.

      REFRESH: it_vbrk, it_vbrp, it_btno, it_customer.
***********get the invoice details*******************
      CALL METHOD process
        EXPORTING
          inv_begda   = gs_input-inv_from
          inv_endda   = gs_input-inv_to
          grn_begda   = gs_input-grn_from
          grn_endda   = gs_input-grn_to
          cust_no     = gs_input-customer
        IMPORTING
          it_vbrk     = it_vbrk
          it_vbrp     = it_vbrp
          it_btno     = it_btno
          it_customer = it_customer.

      IF it_customer[] IS NOT INITIAL.
        SORT it_vbrk BY kunag fkdat.
        LOOP AT it_customer ASSIGNING FIELD-SYMBOL(<fs_customer>).
          CLEAR gs_response.
          gs_response-customer = <fs_customer>-kunnr.
          gs_response-type = 'S'.
          gs_response-msg  = | 'Invoice details for - { <fs_customer>-kunnr } |.
          SELECT SINGLE name1 FROM kna1 INTO gs_response-cusname WHERE kunnr = <fs_customer>-kunnr.
          IF sy-subrc = 0.
          ENDIF.
          LOOP AT it_vbrk ASSIGNING FIELD-SYMBOL(<fs_vbrk>) WHERE kunag =  <fs_customer>-kunnr.
            CLEAR ls_invoices.
            ls_invoices-vbeln = <fs_vbrk>-vbeln.
            ls_invoices-fkdat = <fs_vbrk>-fkdat.
            ls_invoices-fkart = <fs_vbrk>-fkart.
            IF ls_invoices-fkart = 'YBBR'.
              ls_invoices-bill_type = 'Invoice'.
            ELSE.
              ls_invoices-bill_type = 'Sales Returns'.
            ENDIF.
            ls_invoices-werks_dist = <fs_vbrk>-werks_dist.
            ls_invoices-netwr = <fs_vbrk>-netwr.
            ls_invoices-mwsbk = <fs_vbrk>-mwsbk.
            ls_invoices-dmbtr = <fs_vbrk>-dmbtr.
            ls_invoices-bukrs = <fs_vbrk>-bukrs.
            ls_invoices-gjahr = <fs_vbrk>-gjahr.
            ls_invoices-aubel = <fs_vbrk>-aubel.
            ls_invoices-zcustype = <fs_vbrk>-zcustype.
            ls_invoices-z_grn_allow = <fs_vbrk>-z_grn_allow.
            ls_invoices-remarks = <fs_vbrk>-remarks.
            ls_invoices-orig_inv = <fs_vbrk>-orig_inv.

            LOOP AT it_vbrp ASSIGNING FIELD-SYMBOL(<fs_vbrp>) WHERE vbeln = ls_invoices-vbeln.
              CLEAR ls_items.
              ls_items-vbeln    = <fs_vbrp>-vbeln.
              ls_items-posnr    = <fs_vbrp>-posnr.
              ls_items-matnr    = <fs_vbrp>-matnr.
              ls_items-arktx    = <fs_vbrp>-arktx.
              ls_items-fkimg    = <fs_vbrp>-fkimg.
              ls_items-vrkme    = <fs_vbrp>-vrkme.
              ls_items-netwr    = <fs_vbrp>-netwr.
              ls_items-mwsbp    = <fs_vbrp>-mwsbp.
*            ls_items-werks    = <fs_vbrp>-werks.
*            ls_items-meins    = <fs_vbrp>-meins.
              ls_items-cgst     = <fs_vbrp>-cgst.
              ls_items-sgst     = <fs_vbrp>-sgst.
              ls_items-igst     = <fs_vbrp>-igst.
              ls_items-jtc1     = <fs_vbrp>-jtc1.
*            ls_items-cgst_per = <fs_vbrp>-cgst_per.
*            ls_items-sgst_per = <fs_vbrp>-sgst_per.
*            ls_items-igst_per = <fs_vbrp>-igst_per.
*            ls_items-aubel    = <fs_vbrp>-aubel.
*            ls_items-aupos    = <fs_vbrp>-aupos.

              LOOP AT it_btno ASSIGNING FIELD-SYMBOL(<fs_btno>) WHERE vbeln = ls_items-vbeln AND posnr = ls_items-posnr .
                CLEAR ls_batch.
                ls_batch-matnr = <fs_btno>-matnr.
                ls_batch-charg = <fs_btno>-charg.
                ls_batch-lfimg = <fs_btno>-lfimg.
                ls_batch-vrkme = <fs_btno>-vrkme.
                APPEND ls_batch TO ls_items-batch.
              ENDLOOP.
              APPEND ls_items TO ls_invoices-items.
            ENDLOOP.

            APPEND ls_invoices TO gs_response-invoices.
          ENDLOOP.
          APPEND gs_response TO gt_response.
        ENDLOOP.
      ENDIF.
    ENDIF.

    IF gt_response IS NOT INITIAL.
      CLEAR:v_jsonload.
** serialize the output for response ***
      /ui2/cl_json=>serialize(
      EXPORTING
       data         =  gt_response
       pretty_name  = /ui2/cl_json=>pretty_mode-user
      RECEIVING
       r_json         = DATA(lv_body) ).

*        v_jsonload = | { '[' } { lv_body } { ']' } |.
      v_jsonload = lv_body.

      CALL METHOD lo_log_upd->log_entry_store
        EXPORTING
          apiname         = 'DMSGRNCOMPL'
          ijson           = lv_data
          ojson           = v_jsonload
        EXCEPTIONS
          apiname_missing = 1
          json_missing    = 2
          OTHERS          = 3.
      IF sy-subrc <> 0.

      ENDIF.

*Set JSON Content-Type
      CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
      CALL METHOD server->response->set_cdata( data = v_jsonload ).
    ENDIF.
  ENDMETHOD.


  METHOD PROCESS.

    TYPES: BEGIN OF ty_lips,
             vbeln TYPE vbeln_vl,
             posnr TYPE posnr_vl,
             matnr TYPE matnr,
             werks TYPE werks_d,
             charg TYPE charg_d,
             lfimg TYPE lfimg,
             meins TYPE meins,
             vrkme TYPE vrkme,
             ntgew TYPE	ntgew_15,
             brgew TYPE	brgew_15,
             gewei TYPE	gewei,
             volum TYPE	volum_15,
             voleh TYPE	voleh,
           END OF ty_lips.

    DATA: it_lips TYPE TABLE OF ty_lips,
          ls_lips TYPE ty_lips.

    DATA: it_delno TYPE TABLE OF zstr_bill_item_dms,
          ls_delno TYPE zstr_bill_item_dms.
    DATA: ls_batno TYPE zstr_lips_bno_dms.

    DATA : wa_vbrp2 TYPE zstr_bill_item_dms.

    DATA: lv_vkorg TYPE vkorg,
          lv_vtweg TYPE vtweg.

    DATA: ls_customer TYPE kunnr_sty.
    TYPES: t_vbeln TYPE RANGE OF vbeln.

*Defaulting vkorg, vtweg as 1000 and 20 to extract only sheenlac
    lv_vkorg = '1000'.
    lv_vtweg = '20'.

    TYPES: t_kunnr TYPE RANGE OF kunnr.

    SELECT bwkey, bukrs FROM t001k INTO TABLE @DATA(lt_t001k) WHERE bukrs = 'DMS1'.
    IF sy-subrc = 0.
      SELECT kunnr, werks, zcustype FROM kna1
        FOR ALL ENTRIES IN @lt_t001k
        WHERE werks = @lt_t001k-bwkey
        INTO TABLE @DATA(lt_kna1).
      IF sy-subrc = 0.
        IF cust_no[] IS NOT INITIAL.
          DATA(lr_kunnr) = VALUE t_kunnr( FOR ls_kna1 IN cust_no
                                 LET s = 'I'
                                     o = 'EQ'
                                 IN sign   = s
                                    option = o
                               ( low = |{ ls_kna1-kunnr ALPHA = IN }| ) ).
        ELSE.
          lr_kunnr = VALUE t_kunnr( FOR ls_kna11 IN lt_kna1
                         LET s = 'I'
                             o = 'EQ'
                         IN sign   = s
                            option = o
                       ( low = |{ ls_kna11-kunnr ALPHA = IN }| ) ).
        ENDIF.

      ENDIF.
    ENDIF.

*select only customres with Plant assigned in customer master
    IF lr_kunnr[] IS NOT INITIAL.

* get the invoices where GRN is completed in SAP .
      IF grn_begda IS NOT INITIAL AND grn_endda IS NOT INITIAL.

        SELECT vbeln FROM zdms_grn_header
          APPENDING TABLE @DATA(lt_vbeln)
          WHERE distributor IN @lr_kunnr
          AND   erdat BETWEEN @grn_begda AND @grn_endda.

      ELSEIF inv_begda IS NOT INITIAL AND inv_endda IS NOT INITIAL.

        SELECT vbeln FROM zdms_grn_header
          APPENDING TABLE lt_vbeln
          WHERE distributor IN lr_kunnr
          AND   invoice_date BETWEEN inv_begda AND inv_endda.

      ENDIF.

      SORT lt_vbeln BY vbeln.
*enter all the final invoice numbers into the final selection range
      DATA(lr_vbeln)  = VALUE t_vbeln( FOR ls_vbeln IN lt_vbeln
                            LET s = 'I'
                                o = 'EQ'
                            IN sign   = s
                               option = o
                          ( low = |{ ls_vbeln-vbeln ALPHA = IN }| ) ).
*

      CHECK lt_vbeln IS NOT INITIAL.
*based on all the vbeln select all the data from VBRK and exclude Cancelled invoices
      SELECT vbeln fkdat fkart vkorg vtweg spart kunag netwr mwsbk bukrs gjahr knumv xblnr AS orig_inv
        FROM vbrk INTO CORRESPONDING FIELDS OF TABLE it_vbrk
       WHERE vbeln IN lr_vbeln
         AND vkorg EQ lv_vkorg
         AND vtweg EQ lv_vtweg
         AND bukrs = '1000'
         AND kunag IN lr_kunnr
         AND fksto NE abap_true.

    ENDIF.

    SORT it_vbrk   ASCENDING  BY vbeln.

* get billing details
    IF it_vbrk[] IS NOT INITIAL.
      SELECT vbeln posnr matnr arktx fkimg vrkme netwr mwsbp
             werks meins aubel aupos vgbel
        FROM vbrp INTO CORRESPONDING FIELDS OF TABLE it_vbrp FOR ALL ENTRIES IN it_vbrk
       WHERE vbeln EQ it_vbrk-vbeln.
      IF sy-subrc = 0.
* to get the batch no's for each material as per invoice
        it_delno[] = it_vbrp[].
        SORT it_delno BY vgbel.
        DELETE ADJACENT DUPLICATES FROM it_delno COMPARING vgbel.

        SELECT vbeln posnr matnr werks charg lfimg meins vrkme ntgew brgew gewei volum voleh
          FROM lips INTO TABLE it_lips
          FOR ALL ENTRIES IN it_delno
          WHERE vbeln EQ it_delno-vgbel AND charg NE ''.

        IF sy-subrc EQ 0.
          SORT it_lips BY vbeln.
        ENDIF.
      ENDIF.

* for getting the pricing conditions
      SELECT *
        FROM prcd_elements INTO TABLE @DATA(it_konv)  "KONV
        FOR ALL ENTRIES IN @it_vbrk
            WHERE knumv EQ @it_vbrk-knumv AND
                  kschl  IN ('JOCG','JOSG','JOIG','JTC1') AND
                  kinak EQ '' .
    ENDIF.

    SORT it_vbrp   ASCENDING  BY vbeln posnr.

    LOOP AT it_vbrk INTO DATA(wa_vbrk).

      CLEAR ls_customer.
      ls_customer-kunnr = wa_vbrk-kunag.
      APPEND ls_customer TO it_customer.

** sales order no
      READ TABLE it_vbrp INTO wa_vbrp2 WITH  KEY vbeln = wa_vbrk-vbeln BINARY SEARCH.
      IF sy-subrc = 0.
        wa_vbrk-aubel  = wa_vbrp2-aubel .
      ENDIF.

      wa_vbrk-dmbtr = wa_vbrk-netwr + wa_vbrk-mwsbk.

      READ TABLE lt_kna1 INTO DATA(ls_kna1_t) WITH KEY kunnr = wa_vbrk-kunag.
      IF sy-subrc = 0.
        wa_vbrk-werks_dist = ls_kna1_t-werks.
        wa_vbrk-zcustype = ls_kna1_t-zcustype.
      ENDIF.

      MODIFY it_vbrk FROM wa_vbrk TRANSPORTING dmbtr aubel werks_dist zcustype z_grn_allow remarks .
    ENDLOOP.


    SORT it_vbrk ASCENDING BY vbeln.
    SORT it_konv ASCENDING BY knumv kposn kschl.

    LOOP AT it_vbrp INTO DATA(wa_vbrp).

      CLEAR wa_vbrk.
      READ TABLE it_vbrk INTO wa_vbrk WITH  KEY vbeln = wa_vbrp-vbeln BINARY SEARCH.
      IF sy-subrc  = 0.
        READ TABLE it_konv INTO DATA(wa_konv) WITH KEY knumv = wa_vbrk-knumv
                                                       kposn = wa_vbrp-posnr
                                                       kschl = 'JOCG' BINARY SEARCH .
        IF sy-subrc  = 0.
          wa_vbrp-cgst     = wa_konv-kwert .
          wa_vbrp-cgst_per = wa_konv-kbetr.  " / 10.
        ENDIF.
        READ TABLE it_konv INTO wa_konv WITH  KEY knumv = wa_vbrk-knumv
                                                  kposn = wa_vbrp-posnr
                                                  kschl = 'JOSG' BINARY SEARCH.
        IF sy-subrc  = 0.
          wa_vbrp-sgst     = wa_konv-kwert .
          wa_vbrp-sgst_per = wa_konv-kbetr.  " / 10.
        ENDIF.
        READ TABLE it_konv INTO wa_konv WITH  KEY knumv = wa_vbrk-knumv
                                                  kposn = wa_vbrp-posnr
                                                  kschl = 'JOIG' BINARY SEARCH.
        IF sy-subrc  = 0.
          wa_vbrp-igst     = wa_konv-kwert .
          wa_vbrp-igst_per = wa_konv-kbetr. " / 10.
        ENDIF.
        READ TABLE it_konv INTO wa_konv WITH  KEY knumv = wa_vbrk-knumv
                                                  kposn = wa_vbrp-posnr
                                                  kschl = 'JTC1' BINARY SEARCH.
        IF sy-subrc  = 0.
          wa_vbrp-jtc1    = wa_konv-kwert .
        ENDIF.
      ENDIF.
      MODIFY it_vbrp FROM wa_vbrp TRANSPORTING igst cgst sgst jtc1 igst_per cgst_per sgst_per.

*Based on the delivery no and material, Get all the batch no and input in the table
      LOOP AT it_lips INTO ls_lips WHERE vbeln = wa_vbrp-vgbel AND matnr = wa_vbrp-matnr.
        CLEAR ls_batno.
        ls_batno-vbeln   = wa_vbrp-vbeln.
        ls_batno-vbeln_d = ls_lips-vbeln.
        ls_batno-posnr   = wa_vbrp-posnr.
        ls_batno-matnr   = ls_lips-matnr.
        ls_batno-werks   = ls_lips-werks.
        ls_batno-charg   = ls_lips-charg.
        ls_batno-lfimg   = ls_lips-lfimg.
        ls_batno-meins   = ls_lips-meins.
        ls_batno-vrkme   = ls_lips-vrkme.
        ls_batno-ntgew   = ls_lips-ntgew.
        ls_batno-brgew   = ls_lips-brgew.
        ls_batno-gewei   = ls_lips-gewei.
        ls_batno-volum   = ls_lips-volum.
        ls_batno-voleh   = ls_lips-voleh.

        APPEND ls_batno TO it_btno.
      ENDLOOP.
    ENDLOOP.

    SORT it_customer BY kunnr.
    DELETE ADJACENT DUPLICATES FROM it_customer COMPARING kunnr.
  ENDMETHOD.
ENDCLASS.
