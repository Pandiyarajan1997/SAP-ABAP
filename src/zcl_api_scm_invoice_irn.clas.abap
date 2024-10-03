class ZCL_API_SCM_INVOICE_IRN definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_SCM_INVOICE_IRN IMPLEMENTATION.


  METHOD if_http_extension~handle_request.
*----------------------------------------------------------------------------------
*&Created by: Samsudeen M
*&Created On: 13.06.2023
*&Purpose : For Invoice Process & E-invoice Process
*&Reference: Ramakrishnan J & Praveen Kumar & Gopal raja
*-------------------------------------------------------------------------------------
    TYPES: BEGIN OF ty_input,
             orderid  TYPE zorder_id,
             salesdoc TYPE vbeln_va,
           END OF ty_input.

    DATA: gt_input  TYPE TABLE OF ty_input,
          gt_update TYPE TABLE OF ty_input,
          gs_input  TYPE ty_input.
    DATA : eway TYPE j_1ig_ewaybill.

    TYPES: BEGIN OF output,
             order_id TYPE zorder_id,
             sales_no TYPE vbeln_va,
             invno    TYPE vbeln_vf,
             irn      TYPE j_1ig_irn,
             sqrcode  TYPE j_1ig_sign_inv,
             type     TYPE bapi_mtype,
             msg      TYPE string,
             form     TYPE string,
           END OF output.
    DATA: gt_output TYPE TABLE OF output.

    DATA: lv_data TYPE string.
    DATA: lr_data TYPE REF TO data.
    DATA:lv_belnr   TYPE belnr_d,
         lv_invoice TYPE vbeln_vf,
         lv_bukrs   TYPE bukrs,
         lv_gjahr   TYPE gjahr,
         l_einv     TYPE zst_irn_out,
         l_eway     TYPE j_1ig_ewaybill,
         lt_return  TYPE bapiret2_t.
    DATA: lv_msg TYPE string.

    DATA: lv_body    TYPE string,
          v_jsonload TYPE string.
    DATA: lv_string TYPE string,
          lv_errstr TYPE string.

    DATA: lo_object_cls TYPE REF TO zcl_scm_sales_to_invoice.
    CREATE OBJECT lo_object_cls.

    DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
    CREATE OBJECT lo_log_upd.

    DATA: lo_common_check TYPE REF TO zcl_common_check.       "obj dec for global class common check
    CREATE OBJECT lo_common_check.

    CONSTANTS: c_invoice TYPE char02 VALUE '15',
               c_pgi     TYPE char02 VALUE '13',
               c_lgort   TYPE lgort_d VALUE '0006',
               c_irn     TYPE char02 VALUE '17',
               c_comp    TYPE char02 VALUE '18'.

    "Input of API Request
    CLEAR lv_data.
    CALL METHOD server->request->get_cdata RECEIVING data = lv_data.
    DATA(lv_data_tmp) = |[ { lv_data } ]|.
*    CONDENSE lv_data_tmp NO-GAPS.
    SHIFT lv_data_tmp LEFT DELETING LEADING ''.

** Deserialize the input our required input ***
    /ui2/cl_json=>deserialize(
   EXPORTING
     json         = lv_data_tmp
     pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
   CHANGING
     data         = gt_input ).

    IF gt_input IS NOT INITIAL.
      REFRESH: gt_output.

      gs_input = VALUE #( gt_input[ 1 ] OPTIONAL ).

      DATA(l_vbeln) = CONV vbeln_va( |{ gs_input-salesdoc  ALPHA = IN }| ).
      "Orderid Checks
      SELECT SINGLE * FROM zsd_scm_header INTO @DATA(lw_header) WHERE order_id = @gs_input-orderid
                                                                AND   vbeln = @l_vbeln.


      IF sy-subrc = 0.
        "Delivery Completed Means only Process for Invoice
        IF lw_header-dcomp EQ abap_true.
          "First Check whether Invoice is Generated Already or Not
          IF lw_header-icomp EQ abap_true.
            "IRN Details Fetch
            SELECT SINGLE * FROM j_1ig_invrefnum INTO @DATA(l_irn)
                            WHERE bukrs = @lw_header-bukrs
                            AND docno = @lw_header-invoice_no
                            AND doc_year = @lw_header-gjahr.
            IF sy-subrc EQ 0.

            ENDIF.
*** Billing Document Smartforms conversion to string **
            CLEAR:lv_string,lv_errstr.
            CALL FUNCTION 'ZSD_BILLINVOICE_PDF_GENERATE'
              EXPORTING
                im_vbeln       = lw_header-invoice_no
                im_gjahr       = lw_header-gjahr
              IMPORTING
                ep_xstring     = lv_string
                error_message  = lv_errstr
              EXCEPTIONS
                so_num_missing = 1
                OTHERS         = 2.
            IF sy-subrc = 0.

            ENDIF.
            APPEND VALUE #( order_id = gs_input-orderid
                            sales_no = l_vbeln
                            invno    = lw_header-invoice_no
                            irn      = l_irn-irn
                            sqrcode  = l_irn-signed_qrcode
                            type     = 'S'
                            msg      = |Invoice Already Completed|
                            form     = lv_string ) TO gt_output.
          ELSE.
            SELECT SINGLE * FROM zsd_spl_sale_hd INTO @DATA(l_salelog) WHERE ordid = @gs_input-orderid
                                                                       AND vbeln =   @l_vbeln.
            IF l_salelog-bukrs IS NOT INITIAL.
              lo_common_check->fi_period_check(
                                EXPORTING
                                  posting_date =  sy-datum          " Field of type DATS
                                  acct_type    =  'D'               " Account type
                                  com_code     =  l_salelog-bukrs   " Company Code
                                IMPORTING
                                  type         =  DATA(gv_type)
                                EXCEPTIONS
                                  mandatory    = 1
                                  OTHERS       = 2
                              ).
              IF sy-subrc = 0 AND gv_type = 'E'.
                lv_msg = |{ lv_msg } ; FI Posting Period Not Opened - Check with Accounts Team|.
                CLEAR : gv_type.
              ENDIF.
*********************Customer not extended to company code Check*************************
              "Primary Order ID and Sales Order Combination Check
              SELECT SINGLE kunnr,bukrs FROM knb1 INTO @DATA(gs_knb1) WHERE bukrs = @l_salelog-bukrs AND
                                                                            kunnr = @l_salelog-kunag.
              IF sy-subrc <> 0.
                lv_msg = |{ lv_msg } ; Customer Code { l_salelog-kunag } not Extended to Company code { l_salelog-bukrs } - Check With Sales Coordination team|.
              ENDIF.
*----------------------------------------------------------------------------------

              SELECT SINGLE auart FROM vbak INTO @DATA(l_fkart) WHERE vbeln =   @l_vbeln.
              DATA lo_cl_common_check TYPE REF TO zcl_common_check.
              CREATE OBJECT lo_cl_common_check.
              CALL METHOD lo_cl_common_check->sd_inv_no_range_check
                EXPORTING
                  auart  = l_fkart
                  vkbur  = l_salelog-vkbur
                  werks  = l_salelog-plant
                IMPORTING
                  numki  = DATA(l_range_intern)
                  return = DATA(l_ret).
              IF l_ret IS NOT INITIAL AND l_range_intern IS INITIAL.
                lv_msg = |{ lv_msg } { l_ret }|.
              ENDIF.
            ENDIF.

            IF lv_msg IS INITIAL.

*** Billing Document Creation Process
              IF lw_header-status EQ c_pgi.
                REFRESH: lt_return. CLEAR: lv_belnr,lv_invoice,lv_bukrs,lv_gjahr.
                CALL METHOD lo_object_cls->create_invoice
                  EXPORTING
                    delivery_no  = lw_header-delivery_no
                  IMPORTING
                    bill_invoice = lv_invoice
                    acco_invoice = lv_belnr
                    bukrs        = lv_bukrs
                    gjahr        = lv_gjahr
                    return       = lt_return.
                IF lv_invoice IS NOT INITIAL.
                  lw_header-invoice_no = lv_invoice.
                  lw_header-belnr      = lv_belnr.
                  lw_header-bukrs      = lv_bukrs.
                  lw_header-gjahr      = lv_gjahr.
                  lw_header-status     = c_invoice.
                  lw_header-msgtyp     = 'S'.
                  lw_header-msg        = |Billing Document Created|.

*** Financing Applicable Checks & Enter into Financing Table ***
                  SELECT SINGLE * FROM vbrk INTO @DATA(l_vbrk) WHERE vbeln = @lv_invoice
                    AND bukrs = @lv_bukrs AND gjahr = @lv_gjahr.
                  IF sy-subrc EQ 0.
                    CALL METHOD lo_object_cls->financing
                      EXPORTING
                        ls_vbrk = l_vbrk
                        booster = l_salelog-booster.
                  ENDIF.
*** If Invoice is Generated But Accounting Document is Not Generated ***
                  IF lv_belnr IS INITIAL.
                    INSERT INTO zsd_accdoc_log VALUES @( VALUE #( mandt      = sy-mandt
                                                                  sonumber   = l_vbeln
                                                                  billnumber = lv_invoice
                                                                  message    = |Account Document Not Generated| ) ).
                  ENDIF.
                ELSE.
                  lw_header-msgtyp = 'E'.
                  lw_header-msg = |Invoice is Not Generated|.
                ENDIF.
              ENDIF.
*** Billing Document Smartforms conversion to string **
              CLEAR:lv_string,lv_errstr.
              CALL FUNCTION 'ZSD_BILLINVOICE_PDF_GENERATE'
                EXPORTING
                  im_vbeln       = lw_header-invoice_no
                  im_gjahr       = lw_header-gjahr
                IMPORTING
                  ep_xstring     = lv_string
                  error_message  = lv_errstr
                EXCEPTIONS
                  so_num_missing = 1
                  OTHERS         = 2.
              IF sy-subrc = 0.

              ENDIF.
*** E-invoice Generation
              IF lw_header-status = c_invoice AND lw_header-invoice_no IS NOT INITIAL.
                IF ( sy-sysid EQ 'DEV' OR sy-sysid EQ 'QAS' ).
                  SELECT SINGLE vbeln,fkart FROM vbrk INTO @DATA(ls_vbrk) WHERE vbeln = @lw_header-invoice_no.
                  SELECT SINGLE * FROM j_1ig_invrefnum INTO @l_irn
                    WHERE irn NE @space.
                  IF sy-subrc EQ 0.
                    IF l_irn-irn IS NOT INITIAL.
*  ***einvoice irn invrefnum details.--->>
                      l_irn-bukrs         = lw_header-bukrs.
                      l_irn-doc_year      = lw_header-gjahr.
                      l_irn-docno         = lw_header-invoice_no.
                      l_irn-version       = l_irn-version.
                      l_irn-doc_type      = l_irn-doc_type.
                      l_irn-irn           = l_irn-irn.
                      l_irn-ack_no        = l_irn-ack_no.
                      l_irn-ack_date      = sy-datum.
                      l_irn-irn_status    = l_irn-irn_status.
                      l_irn-ernam         = sy-uname.
                      l_irn-erdat         = sy-datum.
                      l_irn-erzet         = sy-uzeit.
                      l_irn-signed_inv    = l_irn-signed_inv.
                      l_irn-signed_qrcode = l_irn-signed_qrcode.
                      MODIFY j_1ig_invrefnum FROM l_irn.
                      eway-bukrs = lw_header-bukrs.
                      eway-docno = lw_header-invoice_no.
                      eway-doctyp = ls_vbrk-fkart.
                      eway-gjahr = lw_header-gjahr.
                      eway-ebillno = '123'.
                      eway-erdat = sy-datum.
                      eway-ernam = sy-uname.
                      eway-status = 'A'.
                      MODIFY j_1ig_ewaybill FROM eway.
                      lw_header-irn    = abap_true.
                      lw_header-status = c_irn.
                      lw_header-icomp  = abap_true.
                      lw_header-msgtyp = 'S'.
                      lw_header-msg    = |Sales to E-invoice Process Completed|.
                      lw_header-mail = abap_true.
                    ENDIF.
                  ENDIF.
                ELSE.
                  CLEAR: lt_return,l_einv,l_eway.
                  CALL FUNCTION 'ZSD_FM_IRN_CREATE'
                    EXPORTING
                      im_vbeln  = lw_header-invoice_no
                    IMPORTING
                      einv_out  = l_einv
                      eway_out  = l_eway
                    TABLES
                      error_msg = lt_return.
                  IF l_einv-irn IS NOT INITIAL.
*  ***einvoice irn invrefnum details.--->>
                    l_irn-bukrs         = l_einv-bukrs.
                    l_irn-doc_year      = l_einv-gjahr.
                    l_irn-docno         = l_einv-vbeln.
                    l_irn-version       = l_einv-version.
                    l_irn-doc_type      = l_einv-doctyp.
                    l_irn-irn           = l_einv-irn.
                    l_irn-ack_no        = l_einv-ack_no.
                    l_irn-ack_date      = l_einv-ack_date.
                    l_irn-irn_status    = l_einv-irn_status.
                    l_irn-ernam         = l_einv-ernam.
                    l_irn-erdat         = l_einv-erdat.
                    l_irn-erzet         = l_einv-erzet.
                    l_irn-signed_inv    = l_einv-signed_inv.
                    l_irn-signed_qrcode = l_einv-signed_qrcode.
                    MODIFY j_1ig_invrefnum FROM l_irn.
                    lw_header-irn    = abap_true.
                    lw_header-status = c_irn.
                    lw_header-icomp  = abap_true.
                    lw_header-msgtyp = 'S'.
                    lw_header-msg    = |Sales to E-invoice Process Completed|.
                  ELSE.
                    lw_header-msgtyp = 'E'.
                    LOOP AT lt_return INTO DATA(lw_ret) WHERE ( type = 'E' OR type = 'A' ).
                      lw_header-msg = |{ lw_header-msg },{ lw_ret-message } { lw_ret-message_v1 }|.
                    ENDLOOP.
                  ENDIF.
                ENDIF.
              ENDIF.
*Email Triggering Method
              CALL METHOD lo_object_cls->mail_sent
                EXPORTING
                  vbeln     = lv_invoice
                  gjahr     = lw_header-gjahr
                IMPORTING
                  mail_sent = lw_header-mail.
              MODIFY zsd_scm_header FROM lw_header.
              APPEND VALUE #( order_id = lw_header-order_id
                              sales_no = lw_header-vbeln
                              invno    = lw_header-invoice_no
                              irn      = l_irn-irn
                              sqrcode  = l_irn-signed_qrcode
                              type     = lw_header-msgtyp
                              msg      = lw_header-msg
                              form     = lv_string ) TO gt_output.

            ELSE.
*FI Period Open Check & Customer not extended to company code.

              APPEND VALUE #( order_id = gs_input-orderid
                  sales_no = l_vbeln
                  type     = 'E'
                  msg      = lv_msg ) TO gt_output.


            ENDIF.

          ENDIF.

          "If Delivery is not Processed Means Invoice Process Not allowed
        ELSE.
          APPEND VALUE #( order_id = gs_input-orderid
                          sales_no = l_vbeln
                          type     = 'E'
                          msg      = |Delivery is not Completed| ) TO gt_output.
        ENDIF.
      ELSE.
        APPEND VALUE #( order_id = gs_input-orderid
                        sales_no = l_vbeln
                        type     = 'E'
                        msg      = |Order ID is Not Present in log table| ) TO gt_output.
      ENDIF.
    ENDIF.

    IF gt_output[] IS NOT INITIAL.
      CLEAR: lv_body,v_jsonload.
** serialize the output for response ***
      /ui2/cl_json=>serialize(
      EXPORTING
       data         =  gt_output
       pretty_name  = /ui2/cl_json=>pretty_mode-user
      RECEIVING
       r_json         = lv_body ).

*Output Entry in Log Table
      CALL METHOD lo_log_upd->log_entry_store
        EXPORTING
          apiname         = 'BILLINVOICE'
          ijson           = lv_data
          ojson           = lv_body
        EXCEPTIONS
          apiname_missing = 1
          json_missing    = 2
          OTHERS          = 3.
      IF sy-subrc = 0.

      ENDIF.

      v_jsonload = | { '[' } { lv_body } { ']' } |.
*Set JSON Content-Type
      CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
      CALL METHOD server->response->set_cdata( data = v_jsonload ).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
