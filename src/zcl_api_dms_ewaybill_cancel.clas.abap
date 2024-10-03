class ZCL_API_DMS_EWAYBILL_CANCEL definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_DMS_EWAYBILL_CANCEL IMPLEMENTATION.


  METHOD IF_HTTP_EXTENSION~HANDLE_REQUEST.
*----------------------------------------------------------------------------------
*&Created by: Puratchiveeran
*&Created On: 18.03.2024
*&Purpose : API for Creating E-Waybill Number
*&Reference: Ramakrishnan J
*-------------------------------------------------------------------------------------

    DATA: gt_input TYPE STANDARD TABLE OF zst_ewaybill,
          gs_input TYPE zst_ewaybill.

    DATA: lt_header TYPE STANDARD TABLE OF zsd_scm_header,
          lw_header TYPE zsd_scm_header.

    TYPES: BEGIN OF output,
             msg TYPE string,
           END OF output.
    DATA: gt_output TYPE TABLE OF output.

    DATA: lv_data TYPE string.
    DATA: lr_data TYPE REF TO data.

    DATA: lv_body    TYPE string,
          v_jsonload TYPE string.
    DATA: lv_response  TYPE string,
          l_ewaybill   TYPE j_1ig_ewaybill,
          et_error_msg TYPE bapiret2_t.
    FIELD-SYMBOLS <fs_log> TYPE zsd_scm_header.

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
     data         = gs_input ).

    REFRESH: gt_output.
    IF gs_input IS NOT INITIAL.

      "Company code Checks
      SELECT SINGLE bukrs FROM t001 INTO @DATA(l_compcode) WHERE bukrs = @gs_input-bukrs.
      IF sy-subrc NE 0.
        APPEND VALUE #( msg = |Incorrect Company Code| ) TO gt_output.
      ENDIF.

      "Document Type Checks
      SELECT SINGLE * FROM vbrk INTO @DATA(l_vbeln) WHERE vbeln = @gs_input-vbeln.
      IF sy-subrc NE 0.
        APPEND VALUE #( msg = |Incorrect Invoice Number| ) TO gt_output.
      ELSE.
        "Document Type Checks
        IF gs_input-doctyp IS NOT INITIAL.
          IF gs_input-doctyp NE l_vbeln-fkart.
            APPEND VALUE #( msg = |Incorrect Document Type| ) TO gt_output.
          ENDIF.
        ELSE.
          APPEND VALUE #( msg = |Document Type is missing| ) TO gt_output.
        ENDIF.
      ENDIF.

      "Fiscal Year Checks
      IF gs_input-gjahr IS INITIAL.
        APPEND VALUE #( msg = |Fiscal Year is missing| ) TO gt_output.
      ENDIF.

      "IRN Checks
      IF gs_input-irn IS INITIAL.
        APPEND VALUE #( msg = |IRN Number is missing| ) TO gt_output.
      ENDIF.

      "Vehicle Number Checks
      IF gs_input-vehno IS INITIAL.
        APPEND VALUE #( msg = |Vehicle Number is missing| ) TO gt_output.
      ENDIF.

      IF gs_input-transid IS INITIAL.
        APPEND VALUE #( msg = |Transaction id is missing| ) TO gt_output.
      ENDIF.

      IF gs_input-transname IS INITIAL.
        APPEND VALUE #( msg = |Transporter Name is missing| ) TO gt_output.
      ENDIF.

      IF gs_input-vehtyp IS INITIAL.
        APPEND VALUE #( msg = |Vehicle Type is missing| ) TO gt_output.
      ENDIF.

      IF gs_input-transmode IS INITIAL.
        APPEND VALUE #( msg = |Transportation Mode is missing| ) TO gt_output.
      ENDIF.

      IF gs_input-distance IS INITIAL.
        APPEND VALUE #( msg = |Distance is missing| ) TO gt_output.
      ENDIF.

      IF gs_input-transdoc IS INITIAL.
        APPEND VALUE #( msg = |Transporter Doc No is missing| ) TO gt_output.
      ENDIF.

      IF gs_input-transdt IS INITIAL.
        APPEND VALUE #( msg = |Transporter date is missing| ) TO gt_output.
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

      v_jsonload = | { '[' } { lv_body } { ']' } |.
*Set JSON Content-Type
      CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
      CALL METHOD server->response->set_cdata( data = v_jsonload ).
    ELSE.
      "Actual Ewaybill Generation after Error Checks
      CLEAR: lv_response,l_ewaybill. REFRESH et_error_msg.
      CALL FUNCTION 'ZSD_FM_EWAYBILL_CREATE'
        EXPORTING
          im_ewb    = gs_input
        IMPORTING
          response  = lv_response
          eway_out  = l_ewaybill
        TABLES
          error_msg = et_error_msg.
      IF et_error_msg IS INITIAL AND l_ewaybill-ebillno IS NOT INITIAL.
        MODIFY j_1ig_ewaybill FROM l_ewaybill.
        SELECT SINGLE * FROM zsd_scm_header INTO @<fs_log> WHERE invoice_no = @gs_input-vbeln.
        IF sy-subrc = 0.
          <fs_log>-status = '17'.
          <fs_log>-ewaybill = abap_true.
          <fs_log>-msg = |Sales to E-invoice Process Completed|.
        ENDIF.
        "Response Sets
        /ui2/cl_json=>serialize(
        EXPORTING
        data         =  l_ewaybill
        pretty_name  = /ui2/cl_json=>pretty_mode-user
        RECEIVING
        r_json         = lv_body ).
        v_jsonload = | { '[' } { lv_body } { ']' } |.
*Set JSON Content-Type
        CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
        CALL METHOD server->response->set_cdata( data = v_jsonload ).
      ELSE.
        LOOP AT et_error_msg INTO DATA(lw_error) .
          APPEND VALUE #( msg = lw_error-message ) TO gt_output.
        ENDLOOP.
        IF gt_output IS NOT INITIAL.
          "Response Sets
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
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
