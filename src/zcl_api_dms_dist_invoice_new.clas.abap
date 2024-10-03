class ZCL_API_DMS_DIST_INVOICE_NEW definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_DMS_DIST_INVOICE_NEW IMPLEMENTATION.


  METHOD if_http_extension~handle_request.

    TYPES: BEGIN OF body,
             fromdate TYPE datum,
             todate   TYPE datum,
             customer TYPE kunnr_tty,
           END OF body.
    DATA: gs_input TYPE body.

    DATA :v_jsonload TYPE string.

    DATA: gt_response TYPE STANDARD TABLE OF zdist_inv_api_resp_new,
          gs_response TYPE zdist_inv_api_resp_new.


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

    IF gs_input-fromdate IS INITIAL." AND date_to IS INITIAL.
      gs_input-fromdate = sy-datum.
      gs_input-fromdate+6(2) = '01'.

      IF sy-datum+4(2) = '01'.
        gs_input-fromdate+0(4) = gs_input-fromdate+0(4) - 1.
        gs_input-fromdate+4(2) = '11'.
      elseIF sy-datum+4(2) = '02'.
        gs_input-fromdate+0(4) = gs_input-fromdate+0(4) - 1.
        gs_input-fromdate+4(2) = '12'.
      ELSE.
        gs_input-fromdate+4(2) = gs_input-fromdate+4(2) - 2.
      ENDIF.

    ENDIF.

    IF gs_input-todate IS INITIAL.
      gs_input-todate = sy-datum.
    ENDIF.

    REFRESH: it_vbrk, it_vbrp, it_btno, it_customer.

    CALL FUNCTION 'ZGET_CUSTOMER_BILL_FOR_DMS'
      EXPORTING
        begda       = gs_input-fromdate
        endda       = gs_input-todate
        cust_no     = gs_input-customer
      TABLES
        it_vbrk     = it_vbrk
        it_vbrp     = it_vbrp
        it_btno     = it_btno
        it_customer = it_customer.



    IF it_customer[] IS NOT INITIAL.
      SORT it_vbrk BY kunag fkdat.
      LOOP AT it_customer ASSIGNING FIELD-SYMBOL(<fs_customer>).
        CLEAR gs_response.
        gs_response-customer = <fs_customer>-kunnr.
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
            apiname         = 'DMSSHEENINV'
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
    ENDIF.
  ENDMETHOD.
ENDCLASS.
