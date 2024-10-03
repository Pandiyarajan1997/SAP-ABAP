CLASS zcl_eway_create_by_irn_api DEFINITION
  PUBLIC
  FINAL.

  PUBLIC SECTION.
    INTERFACES if_http_extension.
    TYPES:BEGIN OF ty_input,
            Invoice_no TYPE vbeln,
            distance   TYPE char05,
            transid    TYPE char26,
            transname  TYPE char50,
            transdoc   TYPE char12,
            transdate  TYPE char12,
            vehno      TYPE char12,
          END OF ty_input,
          BEGIN OF ty_response,
            Ewb_No         TYPE char12,
            Ewb_dt         TYPE char10,
            Ewb_valid_till TYPE char25,
            Message        TYPE bapiret2-message,
            type           TYPE bapiret2-type,
          END OF ty_response.
    METHODS validation IMPORTING gs_input  TYPE ty_input
                       EXPORTING iv_return TYPE ty_response.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: lv_data TYPE string,
          lv_body TYPE string.
    DATA: ls_request TYPE ty_input.   "Request data
    DATA: ls_response TYPE ty_response. "Response data
************API log**********
    DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
************SARAL API for EWAY******
    DATA:lo_dms_eway TYPE REF TO zcl_dms_einvoice_process.
************HTTP Framework (iHTTP) HTTP Request Handler for Errors************
    DATA: lo_http_error TYPE REF TO cl_http_ext_http_error.
ENDCLASS.



CLASS zcl_eway_create_by_irn_api IMPLEMENTATION.
  METHOD if_http_extension~handle_request.
    CREATE OBJECT lo_log_upd.
    CREATE OBJECT lo_dms_eway.
*-----------Getting input data in json------------*
    CALL METHOD server->request->get_cdata RECEIVING data = lv_data.

** Deserialize the input our required input ***
    /ui2/cl_json=>deserialize(
    EXPORTING
     json         = lv_data
     pretty_name  = /ui2/cl_json=>pretty_mode-user
     assoc_arrays = abap_true
    CHANGING
     data         = ls_request ).

*--------Validation---------*
    CALL METHOD me->validation
      EXPORTING
        gs_input  = ls_request
      IMPORTING
        iv_return = DATA(err_ret).
    IF err_ret IS INITIAL.
*------------call Eway bill create by IRN API-------------*
      lo_dms_eway->generate_ewb_by_irn(
        EXPORTING
          ls_request     = ls_request
        IMPORTING
          ewb_no         = ls_response-ewb_no
          ewb_dt         = ls_response-ewb_dt
          ewb_valid_till = ls_response-ewb_valid_till
          distributor    = DATA(ls_distributor)
          dealer         = DATA(ls_dealer)
          return         = DATA(return)
      ).

      IF return IS INITIAL.
        ls_response-message = |E-way Bill created For the IRN|.
        ls_response-type = 'S'.
      ELSE.
      SPLIT return at '"errorMessage":"' INTO data(str1) data(str2).
      REPLACE all OCCURRENCES OF '"}],"infoDtls":null}' IN str2 WITH space.
      CONDENSE str2.
        ls_response-message = str2.
        ls_response-type = 'E'.
      ENDIF.
    ENDIF.

** serialize the output for response ***
    /ui2/cl_json=>serialize(
    EXPORTING
     data         =  COND #( WHEN ls_response IS NOT INITIAL THEN ls_response ELSE err_ret )
     pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
    RECEIVING
     r_json         = lv_body ).

*Output Entry in Log Table
    CALL METHOD lo_log_upd->log_entry_store
      EXPORTING
        apiname         = 'EWB_CREATE_BY_IRN'
        ijson           = lv_data
        ojson           = lv_body
        distributor     = ls_distributor
        retailer        = ls_dealer
      EXCEPTIONS
        apiname_missing = 1
        json_missing    = 2
        OTHERS          = 3.
*Set JSON Content-Type
    CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
    CALL METHOD server->response->set_cdata EXPORTING data = lv_body .
  ENDMETHOD.

  METHOD validation.
    IF ls_request-invoice_no IS INITIAL.
      iv_return =  VALUE #( message = 'Please Enter Invoice No' type = 'E' ) .
      EXIT.
    ENDIF.

    IF ls_request-invoice_no IS NOT INITIAL.
      SELECT SINGLE FROM zdms_invoice_irn FIELDS docno WHERE docno = @ls_request-invoice_no INTO @DATA(ls_irn_dtls).
      IF Sy-subrc NE 0.
        iv_return =  VALUE #( message = 'Incorrect Invoice No' type = 'E' ) .
        EXIT.
      ENDIF.
      SELECT SINGLE FROM zdms_eway_trdtls FIELDS * WHERE docno = @ls_request-invoice_no INTO @DATA(ls_eway_trans).
      IF sy-subrc = 0 and ls_eway_trans-status = 'A'.
       iv_return =  VALUE #( message = 'EwayBill is already generated for this Invoice' type = 'E' ) .
        EXIT.
      ENDIF.
    ENDIF.

*    IF ls_request-distance IS INITIAL.
*      iv_return =  VALUE #( message = 'Please Enter distance' type = 'E' ) .
*      EXIT.
*    ENDIF.

    IF ls_request-transid IS INITIAL.
      iv_return =  VALUE #( message = 'Please Enter transid' type = 'E' ) .
      EXIT.
    ENDIF.

    IF ls_request-transname IS INITIAL.
      iv_return =  VALUE #( message = 'Please Enter transname' type = 'E' ) .
      EXIT.
    ENDIF.

    IF ls_request-transdoc IS INITIAL.
      iv_return =  VALUE #( message = 'Please Enter transdoc' type = 'E' ) .
      EXIT.
    ENDIF.

    IF ls_request-transdate IS INITIAL.
      iv_return =  VALUE #( message = 'Please Enter transdate' type = 'E' ) .
      EXIT.
    ENDIF.

    IF ls_request-vehno IS INITIAL.
      iv_return =  VALUE #( message = 'Please Enter vehno' type = 'E' ) .
      EXIT.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
