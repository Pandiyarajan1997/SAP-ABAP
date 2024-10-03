FUNCTION ZFI_VEN_PAY_WHATSAPP.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(ZTYPE) TYPE  ZSEN_TYPE OPTIONAL
*"     REFERENCE(BUKRS) TYPE  BUKRS OPTIONAL
*"     REFERENCE(TOTAMT) TYPE  DMBTR OPTIONAL
*"     REFERENCE(USER) TYPE  SY-UNAME OPTIONAL
*"     REFERENCE(PRSLID) TYPE  ZCA_DE_BELNR1 OPTIONAL
*"     REFERENCE(RF_CUST_INV) TYPE  ZSD_SF_CUST_INV OPTIONAL
*"     REFERENCE(LS_CUST_COMM) TYPE  ZCUS_CF_CUMM OPTIONAL
*"  EXPORTING
*"     REFERENCE(ZOUTPUT) TYPE  CHAR20
*"----------------------------------------------------------------------
  DATA: LT_whatsapp TYPE TABLE OF zven_pay_whatsap,
        ls_whatsapp TYPE zven_pay_whatsap.

  DATA: lv_wappurl TYPE string.
  DATA: lv_token TYPE string.

  DATA: lt_tvarvc TYPE TABLE OF tvarvc,
        ls_tvarvc TYPE tvarvc.

  DATA: lv_url TYPE string.

  DATA: lv_totamt(25) TYPE c.
  DATA: lv_date TYPE sy-datum.
  DATA: lv_date_txt(12) TYPE c.
  DATA: lo_http_client TYPE REF TO if_http_client.
  DATA: ls_fm_struc  TYPE qisrsuser_data.
  DATA: lv_TEMPLATE TYPE char20.

  DATA :ls_json    TYPE string,
        v_jsonload TYPE string.

  DATA: lv_response   TYPE string, "API Response
        lv_codes      TYPE i,      "STATUS Code
        lv_http_error TYPE string. "STATUS Description


*Select all from the whatsapp integration table
IF RF_CUST_INV is INITIAL.
  SELECT *
    FROM zven_pay_whatsap
    INTO TABLE lt_whatsapp.
 ENDIF.

*Select the endpoint URL
  SELECT SINGLE low INTO lv_wappurl FROM tvarvc WHERE name = 'ZVENPAY_URL' AND type = 'P'.

*select the authentication token
  SELECT * FROM tvarvc INTO TABLE lt_tvarvc WHERE name = 'ZVENPAY_WAPPTOK' AND type = 'S'.
  IF sy-subrc = 0.
    LOOP AT lt_tvarvc INTO ls_tvarvc.
      IF sy-tabix = 1.
        lv_token = ls_tvarvc-low.
      ELSE.
        CONCATENATE lv_token ls_tvarvc-low INTO lv_token.
      ENDIF.
    ENDLOOP.
  ENDIF.

* GET DATE INTO DD-MMM-YYYY format.
IF RF_CUST_INV is initial.
  lv_date = sy-datum.
  CALL FUNCTION 'CONVERSION_EXIT_SDATE_OUTPUT'
    EXPORTING
      input  = lv_date
    IMPORTING
      output = lv_date_txt.
  IF sy-subrc = 0.
    CONDENSE lv_date_txt.
    REPLACE ALL OCCURRENCES OF '.' IN lv_date_txt WITH '-'.
  ENDIF.

  IF totamt IS NOT INITIAL.
    lv_totamt = totamt.
    CONDENSE lv_totamt.
    REPLACE ALL OCCURRENCES OF ',' IN lv_totamt WITH ''.
  ENDIF.

* get user name details
  CALL FUNCTION 'ISR_GET_USER_DETAILS'
    EXPORTING
      id_user_id   = user "Release ID
    CHANGING
      is_user_data = ls_fm_struc.
  IF sy-subrc = 0.
    CONDENSE ls_fm_struc-fullname.
  ENDIF.
  ENDIF.

*to Send First Approval email.
IF lt_whatsapp is not INITIAL.
  LOOP AT LT_whatsapp INTO ls_whatsapp WHERE ztype = ZTYPE.

    CLEAR lv_url.
    lv_url = lv_wappurl.
    CONCATENATE lv_url ls_whatsapp-zwhatsappno INTO lv_url.
    CONDENSE lv_url.

    lv_TEMPLATE = ls_whatsapp-send_template.
    CONDENSE lv_TEMPLATE.

    cl_http_client=>create_by_url(
     EXPORTING
     url = lv_url
     IMPORTING
     client = lo_http_client
     EXCEPTIONS
     argument_not_found = 1
     plugin_not_active = 2
     internal_error = 3
     OTHERS = 4 ).

    CHECK lo_http_client IS BOUND.
    lo_http_client->propertytype_logon_popup = if_http_client=>co_disabled.

    lo_http_client->request->set_method( 'POST' ).

    lo_http_client->request->set_content_type(
     EXPORTING
     content_type = if_rest_media_type=>gc_appl_json ).

    CONCATENATE 'Bearer' lv_token INTO lv_token SEPARATED BY space.

    lo_http_client->request->set_header_field( EXPORTING name  = 'Authorization' value = lv_token ).

    CLEAR v_jsonload.


    CONCATENATE '{'
                '"template_name":"' lv_TEMPLATE '",'
                '"broadcast_name":"' lv_TEMPLATE '",'
                '"parameters": ['
                '{"name": "cocode","value":"' bukrs '"},'
                '{"name": "totamt","value":"' lv_totamt '"},'
                '{"name": "date","value":"' lv_date_txt '"},'
                '{"name": "proposer","value":"' ls_fm_struc-fullname '"},'
                '{"name": "proposalid","value":"' prslid '"}]'
                '}' INTO v_jsonload.

    lo_http_client->request->set_cdata( EXPORTING data = v_jsonload ).

    lo_http_client->send(
    EXCEPTIONS
    http_communication_failure = 1
    http_invalid_state = 2 ).

    CHECK sy-subrc = 0.

    lo_http_client->receive(
     EXCEPTIONS
     http_communication_failure = 1
     http_invalid_state = 2
     http_processing_failed = 3 ).

    CLEAR lv_codes.
    lo_http_client->response->get_status(
    IMPORTING
      code = lv_codes ).

    lo_http_client->response->get_status(
    IMPORTING
      reason = lv_http_error ).


    CLEAR lv_response.
    lv_response = lo_http_client->response->get_cdata( ).

    IF lv_codes = 200.
      ZOUTPUT = 'SUCCESS'.
    else.
      ZOUTPUT = 'FAILURE'.
    ENDIF.

  ENDLOOP.
ELSEIF RF_CUST_INV is NOT INITIAL.

    CLEAR lv_url.
    lv_url = lv_wappurl.
    CONCATENATE lv_url ls_cust_comm-phone  INTO lv_url.
    CONDENSE lv_url.

    lv_TEMPLATE = 'rupifyinvoice'. "ls_whatsapp-send_template.
    CONDENSE lv_TEMPLATE.

    cl_http_client=>create_by_url(
     EXPORTING
     url = lv_url
     IMPORTING
     client = lo_http_client
     EXCEPTIONS
     argument_not_found = 1
     plugin_not_active = 2
     internal_error = 3
     OTHERS = 4 ).

    CHECK lo_http_client IS BOUND.
    lo_http_client->propertytype_logon_popup = if_http_client=>co_disabled.

    lo_http_client->request->set_method( 'POST' ).

    lo_http_client->request->set_content_type(
     EXPORTING
     content_type = if_rest_media_type=>gc_appl_json ).

    CONCATENATE 'Bearer' lv_token INTO lv_token SEPARATED BY space.

    lo_http_client->request->set_header_field( EXPORTING name  = 'Authorization' value = lv_token ).

    CLEAR v_jsonload.

    data(lv_amount) = conv string(  rf_cust_inv-dueamount ).
    data(lv_inv_date) = |{ rf_cust_inv-invoicedate+6(2) }-{ rf_cust_inv-invoicedate+4(2) }-{ rf_cust_inv-invoicedate+0(4) } |.
  CONCATENATE '{'
    '"template_name": "' lv_TEMPLATE '",'
    '"broadcast_name": "' lv_TEMPLATE '",'
    '"parameters": ['
       '{ "name": "name", "value": "' RF_CUST_INV-custname '"},'
       '{  "name": "invoice_number", "value": "' rf_cust_inv-invoiceno '"},'
       '{  "name": "total_amount", "value": "' lv_amount '"},'
       '{ "name": "order_date", "value": "' lv_inv_date '"},'
       '{ "name": "file_link", "value": "' rf_cust_inv-payment_url '"},'
       '{ "name": "time_taken", "value": "10"}]'
'}'  INTO v_jsonload.

    lo_http_client->request->set_cdata( EXPORTING data = v_jsonload ).

    lo_http_client->send(
    EXCEPTIONS
    http_communication_failure = 1
    http_invalid_state = 2 ).

    CHECK sy-subrc = 0.

    lo_http_client->receive(
     EXCEPTIONS
     http_communication_failure = 1
     http_invalid_state = 2
     http_processing_failed = 3 ).

    CLEAR lv_codes.
    lo_http_client->response->get_status(
    IMPORTING
      code = lv_codes ).

    lo_http_client->response->get_status(
    IMPORTING
      reason = lv_http_error ).


    CLEAR lv_response.
    lv_response = lo_http_client->response->get_cdata( ).

    IF lv_codes = 200.
      ZOUTPUT = 'SUCCESS'.
    else.
      ZOUTPUT = 'FAILURE'.
    ENDIF.
ENDIF.

ENDFUNCTION.
