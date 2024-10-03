CLASS zcl_rf_callback_payment_api DEFINITION
  PUBLIC
  FINAL.

  PUBLIC SECTION.
    INTERFACES: if_http_extension.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_rf_callback_payment_api IMPLEMENTATION.
  METHOD if_http_extension~handle_request.
    TYPES:BEGIN OF ty_amt,
            value           TYPE string,
            formatted_Value TYPE string,
            currency        TYPE string,
          END OF ty_amt,
          BEGIN OF ty_input,
            paymentId             TYPE string,
            merchantPaymentRefId  TYPE string,
            amount                TYPE ty_amt,
            paymentDate           TYPE string,
            redirectUrl           TYPE string,
            redirectConfirmUrl    TYPE string,
            redirectCancelUrl     TYPE string,
            callbackUrl           TYPE string,
            paymenturl            TYPE string,
            status                TYPE string,
            autoCapture           TYPE string,
            uncapturedAmount      TYPE ty_amt,
            accountId             TYPE string,
            merchantCustomerRefId TYPE string,
          END OF ty_input,
          BEGIN OF ty_addr,
            id            TYPE string,
            address_Line1 TYPE string,
            city          TYPE string,
            state         TYPE string,
            landmark      TYPE string,
            pincode       TYPE string,
            country       TYPE string,
          END OF ty_addr,
          BEGIN OF TY_cdata,
            payment_Id           TYPE string,
            merchantPaymentRefId TYPE string,
            merchantId           TYPE string,
            amount               TYPE ty_amt,
            payment_Date         TYPE string,
            callback_Url         TYPE string,
            paymentUrl           TYPE string,
            status               TYPE string,
            auto_Capture         TYPE string,
            uncaptured_Amount    TYPE ty_amt,
            account_Id           TYPE string,
*          voids type TABLE of string,
            address              TYPE ty_addr,
            invoice_date         TYPE string,
          END OF ty_cdata.

    DATA: gs_input   TYPE ty_input,
          gs_rf_cust TYPE zsd_sf_cust_inv,
          gs_cdata   TYPE ty_cdata.
    DATA: ls_true  TYPE string VALUE 'true',
          ls_repl  TYPE string VALUE '"true"',
          ls_false TYPE string VALUE 'False',
          gs_repl  TYPE string VALUE '"False"'.

    TYPES: BEGIN OF ty_msg,
             merchantPaymentRefId TYPE string,
*             paymentURL TYPE string,
*             paymentId TYPE string,
             Customer             TYPE kunnr,
             status               TYPE string,
             message              TYPE string,
           END OF ty_msg.
    TYPES:BEGIN OF ty_resp,
            sap_client           TYPE string,
            merchantPaymentRefId TYPE string,
          END OF ty_resp.
    DATA: gs_data TYPE ty_resp.
    DATA: gt_response TYPE TABLE OF ty_msg.
    DATA: gw_response TYPE ty_msg.
    DATA: v_jsonload TYPE string.

    DATA: lv_body TYPE string.

    DATA: lv_data TYPE string.

    DATA: lv_RV_VALUE TYPE string,
          lv_other    TYPE c.
*    DATA:lcl_signature TYPE REF TO zcl_signature_verification.
*    CREATE OBJECT lcl_signature.
    DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
    CREATE OBJECT lo_log_upd.
**Deserialise cdata to struture
    CALL METHOD server->request->get_cdata
      RECEIVING
        data = lv_data.

    /ui2/cl_json=>deserialize(
      EXPORTING
        json             = lv_data
        pretty_name      = /ui2/cl_json=>pretty_mode-camel_case
      CHANGING
        data             = gs_cdata
    ).

** Check Status is Captured and update the table.
    IF gs_cdata-status = 'CAPTURED'.

      lv_rv_value = server->request->get_header_field( name = 'rpf-webhook-signature' ).

      "Not yet developed HMAC SHA256 Verification
*    TRY.
*    lcl_signature->verify_signature(
*      EXPORTING
*        iv_payload    = lv_data
*        iv_sig_header = lv_rv_value
*        iv_secret     = 'whsk_test_sb_35io3HXHLa4WUvTRzqNHKP89fgqr0Pzi'
*      RECEIVING
*        rv_valid      = data(Flag)
*    ).
*    CATCH cx_sy_conversion_error INTO DATA(lx_exception).
*ENDTRY.

      server->request->get_form_field(
         EXPORTING
           name               = 'merchantPaymentRefId'
         RECEIVING
           value              = DATA(lo_ref_id)
       ).

      SELECT SINGLE  FROM zsd_sf_cust_inv
      FIELDS *
      WHERE invoicekey = @lo_ref_id
      INTO @gs_rf_cust.
      IF sy-subrc = 0.


        IF gs_rf_cust-status = '14' .
          gs_rf_cust-status = '16'.
          gs_rf_cust-response_msg = 'Payment Approved'.
          gs_rf_cust-inv_approvedat = sy-uzeit.
          gs_rf_cust-inv_approvedon = sy-datum.
          gs_rf_cust-inv_approvedby = sy-uname.
          gs_rf_cust-response_status = 'CAPTURED'.
          MODIFY zsd_sf_cust_inv FROM gs_rf_cust .
          IF  sy-subrc = 0.
            COMMIT WORK.
            APPEND VALUE #( merchantPaymentRefId = gs_rf_cust-invoicekey
*                        paymentURL = gs_rf_cust-payment_url
                    customer = gs_rf_cust-custno
                    status = 'Success'
                    message = 'Payment approved for the given merchantPaymentRefId' ) TO gt_response.
          ENDIF.
        ENDIF.

      ENDIF.
      IF gs_rf_cust-status = '16'.
        APPEND VALUE #( merchantPaymentRefId = lo_ref_id
*                       paymentURL = gs_input-paymenturl
                        customer = gs_input-merchantcustomerrefid
                        status = 'Success'
                        message = 'Payment already approved for the given merchantPaymentRefId' ) TO gt_response.
      ENDIF.

      IF lo_ref_id IS INITIAL.

        APPEND VALUE #( merchantPaymentRefId = lo_ref_id
                        customer = gs_rf_cust-custno
                        status = 'Error'
                        message = 'No Input values Found' ) TO gt_response.
      ENDIF.
** serialize the output for response ***
      /ui2/cl_json=>serialize(
      EXPORTING
       data         =  gt_response
       pretty_name  = /ui2/cl_json=>pretty_mode-user
      RECEIVING
       r_json         = lv_body ).

      v_jsonload = | { '[' } { lv_body } { ']' } |.
*Set JSON Content-Type
      CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
      CALL METHOD server->response->set_cdata( data = v_jsonload ).

      lv_data = |merchantPaymentRefId={ lo_ref_id }|.
    ENDIF.
**********************************************************When Multiple callback to capture in API logs/ capture logs
    IF lv_body IS INITIAL.
      lv_body = 'No output Json'.
    ENDIF.
    CALL METHOD lo_log_upd->log_entry_store
      EXPORTING
        apiname         = 'RF_CALLBACK_API'
        ijson           = lv_data
        ojson           = lv_body
        distributor     = gs_rf_cust-custno
      EXCEPTIONS
        apiname_missing = 1
        json_missing    = 2
        OTHERS          = 3.

    CLEAR: lv_body.
  ENDMETHOD.

ENDCLASS.
