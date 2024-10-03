class ZCL_API_SALESORD_ERR_REMOVE definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_SALESORD_ERR_REMOVE IMPLEMENTATION.


  METHOD if_http_extension~handle_request.

***** Purpose of this API is for removing the error
*        message of SPL to Direct Dealers and Distributors *****
    "Created by: Samsudeen M
    "Created on: 12.05.2023
    "Reference by: Ramakrishnan J & Praveen Kumar
    "-----------------------------------------------------------------------------------*
    TYPES: BEGIN OF input,
             orderid    TYPE zorder_id,
             salesorder TYPE vbeln,
           END OF input.
    DATA: gt_input TYPE TABLE OF input.

    TYPES: BEGIN OF ty_msg,
             orderid    TYPE zorder_id,
             salesorder TYPE vbeln,
             status     TYPE bapi_mtype,
             message    TYPE string,
           END OF ty_msg.

    DATA: gt_response TYPE TABLE OF ty_msg.

    DATA: lv_body TYPE string.
    DATA: v_jsonload TYPE string.
    DATA: lv_msgtxt TYPE string.
    DATA: lv_data TYPE string.
    DATA: lt_sale_itm TYPE STANDARD TABLE OF zsd_spl_sale_it,
          lw_sale_itm TYPE zsd_spl_sale_it,
          lw_sale_hdr TYPE zsd_spl_sale_hd.

    CALL METHOD server->request->get_cdata RECEIVING data = lv_data.
    DATA(lv_data_tmp) = |[ { lv_data } ]|.


    REFRESH: gt_input.
** Deserialize the input our required input ***
    /ui2/cl_json=>deserialize(
    EXPORTING
     json         = lv_data_tmp
     pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
     assoc_arrays = abap_true
    CHANGING
     data         = gt_input ). "lr_data ).


    IF gt_input IS NOT INITIAL.
      REFRESH: gt_response.
      LOOP AT gt_input ASSIGNING FIELD-SYMBOL(<fs_input>).
        DATA(l_ordid) = |{ <fs_input>-orderid }|.
        DATA(l_vbeln) = |{ <fs_input>-salesorder ALPHA = IN }|.
        SELECT SINGLE * FROM zsd_scm_header
          INTO @DATA(l_errorclr)
          WHERE order_id = @l_ordid
          AND vbeln = @l_vbeln
          AND msgtyp = 'E'.
        IF sy-subrc NE 0.
          APPEND VALUE #( salesorder = l_vbeln
                          status       = 'E'
                          message    = |Orderid and Sales Order Combination is not present in log table| ) TO gt_response.
          CONTINUE.
        ELSE.
          CLEAR: l_errorclr-msg, l_errorclr-msgtyp.
          MODIFY zsd_scm_header FROM l_errorclr.
* Then Credit check is relaeased
          APPEND VALUE #( salesorder = l_vbeln
                          status       = 'S'
                          message    = |Error is removed successfully| ) TO gt_response.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF gt_response IS NOT INITIAL.
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
    ENDIF.

  ENDMETHOD.
ENDCLASS.
