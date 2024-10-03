class ZCL_API_SCM_LOG_RESPONSE definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_SCM_LOG_RESPONSE IMPLEMENTATION.


  METHOD if_http_extension~handle_request.
*----------------------------------------------------------------------------------
*&Created by: Samsudeen M
*&Created On: 25.04.2023
*&Purpose : For Sending Response for MIS Of SCM Process
*&Reference: Ramakrishnan J & Praveen Kumar
*-------------------------------------------------------------------------------------
    TYPES: BEGIN OF ty_input,
             orderid  TYPE zorder_id,
             salesdoc TYPE vbeln_va,
           END OF ty_input.
    TYPES: BEGIN OF ty_output,
             order_id    TYPE	zorder_id,
             vbeln       TYPE  vbeln_va,
             delivery_no TYPE vbeln_vl,
             invoice_no  TYPE vbeln_vf,
             irn         TYPE j_1ig_irn,
             ewaybill    TYPE j_1ig_ebillno,
             sqrcode     TYPE j_1ig_sign_inv,
             status      TYPE zscm_status,
             msgtyp      TYPE bapi_mtype,
             msg         TYPE char120,
           END OF ty_output.

    DATA: gt_input TYPE TABLE OF ty_input,
          gs_input TYPE ty_input.

    DATA: lt_header     TYPE STANDARD TABLE OF zsd_scm_header,
          lt_header_tmp TYPE STANDARD TABLE OF zsd_scm_header,
          lw_header     TYPE zsd_scm_header.

    DATA: gt_output TYPE TABLE OF ty_output,
          gs_output TYPE ty_output.

    DATA: lv_data TYPE string.
    DATA: lr_data TYPE REF TO data.

    DATA: lv_body    TYPE string,
          v_jsonload TYPE string.

    "Input of API Request
    CLEAR lv_data.
    CALL METHOD server->request->get_cdata RECEIVING data = lv_data.

    CONDENSE lv_data NO-GAPS.
    SHIFT lv_data LEFT DELETING LEADING ''.

** Deserialize the input our required input ***
    /ui2/cl_json=>deserialize(
   EXPORTING
     json         = lv_data
     pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
   CHANGING
     data         = gt_input ).

    IF gt_input IS NOT INITIAL.
      SORT gt_input[] BY orderid salesdoc.
      SELECT * FROM zsd_scm_header INTO TABLE lt_header
               FOR ALL ENTRIES IN gt_input
               WHERE order_id = gt_input-orderid
               AND vbeln = gt_input-salesdoc.
      IF sy-subrc = 0.
        SORT lt_header[] BY order_id vbeln.
      ENDIF.

      REFRESH: lt_header_tmp.
      LOOP AT gt_input ASSIGNING FIELD-SYMBOL(<fs_input>).
        "Orderid Checks
        READ TABLE lt_header ASSIGNING FIELD-SYMBOL(<fs_header>) WITH KEY order_id = <fs_input>-orderid
                                                                          vbeln    = <fs_input>-salesdoc.
        IF sy-subrc <> 0.
          APPEND VALUE #( order_id = <fs_input>-orderid
                          vbeln    = <fs_input>-salesdoc
                          msg      = |Order ID { <fs_input>-orderid } is not Present in log table| ) TO lt_header_tmp.
        ELSE.
          APPEND <fs_header> TO lt_header_tmp.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF lt_header_tmp[] IS NOT INITIAL.

      REFRESH: gt_output.
      LOOP AT lt_header_tmp ASSIGNING FIELD-SYMBOL(<fs_output>).
        CLEAR gs_output.
        MOVE-CORRESPONDING <fs_output> TO gs_output.
        "IRN Number
        SELECT SINGLE * FROM j_1ig_invrefnum INTO @DATA(l_irn) WHERE bukrs = @<fs_output>-bukrs
                                                               AND docno = @<fs_output>-invoice_no
                                                               AND doc_year = @<fs_output>-gjahr.
        IF sy-subrc = 0.
          gs_output-irn = l_irn-irn.
          gs_output-sqrcode = l_irn-signed_qrcode.
        ENDIF.
        "Eway Bill Number
        SELECT SINGLE * FROM j_1ig_ewaybill INTO @DATA(l_ebillno) WHERE bukrs = @<fs_output>-bukrs
                                                                  AND docno = @<fs_output>-invoice_no
                                                                  AND gjahr = @<fs_output>-gjahr.
        IF sy-subrc = 0.
          gs_output-ewaybill = l_ebillno-ebillno.
        ENDIF.
        APPEND gs_output TO gt_output.
      ENDLOOP.

      CLEAR: lv_body,v_jsonload.
** serialize the output for response ***
      /ui2/cl_json=>serialize(
      EXPORTING
       data         =  gt_output
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
