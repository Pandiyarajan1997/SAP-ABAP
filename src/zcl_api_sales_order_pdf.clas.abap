class ZCL_API_SALES_ORDER_PDF definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_SALES_ORDER_PDF IMPLEMENTATION.


  METHOD if_http_extension~handle_request.
*Created By: Samsudeen M
*Createed On: 06.05.2023
*Purpose : Sending the Sales Order Form To MIS
*---------------------------------------------------------------*
    TYPES: BEGIN OF input,
             salesordno TYPE vbeln_va,
           END OF input.
    DATA: gs_input TYPE input.

    DATA: lr_request  TYPE REF TO if_http_request,
          lr_response TYPE REF TO if_http_response.
    DATA: lv_value   TYPE string,
          lv_data    TYPE string,
          ep_xstring TYPE string,
          lv_msg     TYPE string,
          lv_vbeln   TYPE vbeln_va.

    DATA: v_jsonload TYPE string.

    lr_request = server->request.
    lr_response = server->response.
* Check the Calling Method
    IF lr_request->get_method( ) EQ 'GET'.
      CALL METHOD lr_request->get_cdata RECEIVING data = DATA(lv_data_tmp).

** Deserialize the input our required input ***
      /ui2/cl_json=>deserialize(
      EXPORTING
       json         = lv_data_tmp
       pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
       assoc_arrays = abap_true
      CHANGING
       data         = gs_input ).

      lv_vbeln = gs_input-salesordno.

      IF  lv_vbeln IS NOT INITIAL.
*To get the PDF of Sales Order
        CLEAR: ep_xstring,lv_msg.
        CALL FUNCTION 'ZSD_SALES_ORDER_PDF_GENERATE'
          EXPORTING
            im_vbeln       = lv_vbeln
          IMPORTING
            ep_xstring     = ep_xstring
            error_message  = lv_msg
          EXCEPTIONS
            so_num_missing = 1
            OTHERS         = 2.
        IF sy-subrc <> 0.
          CLEAR lv_msg.
          lv_msg = |Sales Order Number Missing|.
** serialize the output for response ***
          /ui2/cl_json=>serialize(
          EXPORTING
           data         =  lv_msg
           pretty_name  = /ui2/cl_json=>pretty_mode-user
          RECEIVING
           r_json         = DATA(lv_error) ).
          v_jsonload = |[ { lv_error } ]|.
*Set JSON Content-Type
          CALL METHOD lr_response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
          CALL METHOD lr_response->set_cdata( data = v_jsonload ).
        ELSE.
          CLEAR lv_data.
          lv_data = ep_xstring.
          CALL METHOD lr_response->set_cdata( lv_data ).
          CALL METHOD lr_response->set_status( code = 200 reason = '' ).
          SELECT SINGLE mimetype FROM tdwp INTO lv_value WHERE dappl = 'PDF'.
          IF sy-subrc = 0.
            CALL METHOD lr_response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
          ENDIF.
        ENDIF.

      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
