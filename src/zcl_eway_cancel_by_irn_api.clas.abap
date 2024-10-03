CLASS zcl_eway_cancel_by_irn_api DEFINITION
  PUBLIC
  FINAL .

  PUBLIC SECTION.
    INTERFACES if_http_extension.
    TYPES:BEGIN OF ty_input,
            invoice_no  TYPE vbeln,
*            cancel_rsn_code TYPE char1,
            cancel_rmrk TYPE char50,
          END OF ty_input,
          BEGIN OF ty_response,
            ewaybill_no TYPE char12,
            canceldate  TYPE char25,
            Message     TYPE bapiret2-message,
            type        TYPE bapiret2-type,
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
ENDCLASS.



CLASS zcl_eway_cancel_by_irn_api IMPLEMENTATION.
  METHOD if_http_extension~handle_request.
    CREATE OBJECT lo_log_upd.
    CREATE OBJECT lo_dms_eway.
*    CREATE OBJECT lo_main.
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
*------------call Eway bill cancel by IRN API-------------*
      lo_dms_eway->cancel_ewb_by_irn(
        EXPORTING
          invoiceno  = ls_request-invoice_no
          cancel_rmrk = ls_request-cancel_rmrk
        IMPORTING
          ewaybill_no = ls_response-ewaybill_no
          canceldate  = ls_response-canceldate
          distributor = DATA(ls_distributor)
          dealer      = DATA(ls_dealer)
          return      = DATA(return)
      ).
      IF return IS INITIAL.
        ls_response-message = |E-way Bill is Cancelled Sucessfully|  ##NO_TEXT .
        ls_response-type = 'S'.
      ELSE.
        ls_response-message = return.
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
    CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' )   ##NO_TEXT .
    CALL METHOD server->response->set_cdata EXPORTING data = lv_body .

  ENDMETHOD.

  METHOD validation.
    IF ls_request-invoice_no IS INITIAL.
      iv_return =  VALUE #( message = 'Please Enter Invoice No' type = 'E' )  ##NO_TEXT .
      EXIT.
    ENDIF.

    IF ls_request-invoice_no IS NOT INITIAL.
      SELECT FROM zdms_eway_trdtls FIELDS * WHERE docno = @ls_request-invoice_no ORDER BY erdat DESCENDING ,erzet DESCENDING INTO table @DATA(lt_eway_trans).
     IF Sy-subrc NE 0.
        iv_return =  VALUE #( message = |No Data exists| type = 'E' )  ##NO_TEXT  .
        EXIT.
      ENDIF.

      data(ls_eway_trans) = VALUE #( lt_eway_trans[ 1 ] OPTIONAL ).
      IF ls_eway_trans-status = 'C'.
        iv_return =  VALUE #( message = |Ewaybill is already cancelled for this invoice  | type = 'E' )   ##NO_TEXT .
        EXIT.
      ELSEIF ls_eway_trans-status = 'E'.
        iv_return =  VALUE #( message = |Ewaybill validity has been Expired| type = 'E' )   ##NO_TEXT .
        EXIT.
*      ELSEIF ls_eway_trans-status = space.
*        iv_return =  VALUE #( message = |No EWB exist for the Invoice { ls_request-ewaybill_no }| type = 'E' ) .
*        EXIT.
      ENDIF.

      IF ls_request-cancel_rmrk IS INITIAL.
        iv_return =  VALUE #( message = |Enter Cancel Remarks | type = 'E' )   ##NO_TEXT .
        EXIT.
      ENDIF.

    ENDIF.
  ENDMETHOD.

ENDCLASS.
