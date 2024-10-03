class ZCL_API_DMS_SO_GET_STATUS definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_DMS_SO_GET_STATUS IMPLEMENTATION.


  METHOD if_http_extension~handle_request.

**** Purpose of this API is for Customer incoming Payment entry Posting ***
    TYPES: BEGIN OF ty_input,
             order_id TYPE  string, "zorder_id, "*Order Type  YDMS
*             order_type   TYPE string, "auart,
*             distributor TYPE string, "kunnr,
*             dealer_code TYPE string, "kunnr,
*             ref_doc_no  TYPE string, "xblnr,
*             material    TYPE string, "matnr,
*             quantity    TYPE string, "quant,
*             uom         TYPE string, "meins,
*             discount    TYPE string, "skfbp,
           END OF ty_input.


    TYPES: BEGIN OF ty_input1,
             order_id TYPE  zorder_id,
           END OF ty_input1.
*    DATA: gt_input TYPE STANDARD TABLE OF zsd_st_dms_saleord_hd.
    DATA: gt_input TYPE TABLE OF ty_input,
          gs_input TYPE ty_input.
    DATA: gt_input_tb TYPE TABLE OF ty_input1,
          gs_input_tb TYPE ty_input1.

    TYPES: BEGIN OF ty_msg,
             order_id       TYPE string,
             distributor    TYPE string, "kunnr,
             dealer_sold    TYPE string, "kunnr,
             dealer_ship    TYPE string, "kunnr,
             sale_order     TYPE string,
             invoice_no     TYPE string,
             einvoice_no    TYPE string,
             signed_inv     TYPE string,
             esigned_qrcode TYPE string,
             status         TYPE string,
             message        TYPE string,
           END OF ty_msg.

    DATA: gt_response TYPE TABLE OF ty_msg.
    DATA: gw_response TYPE ty_msg.

    DATA: lv_body TYPE string.
    DATA: v_jsonload TYPE string.

    DATA: lv_data TYPE string.
    DATA: lr_data TYPE REF TO data.

    DATA: lw_sale_hdr TYPE zsd_dms_sale_hdr.

*** Field Symbols for splitting input ***
    FIELD-SYMBOLS:
      <data>        TYPE data,
      <results>     TYPE data,
      <structure>   TYPE any,
      <table>       TYPE ANY TABLE,
      <field>       TYPE any,
      <field_value> TYPE data.

    CLEAR lv_data.
    CALL METHOD server->request->get_cdata RECEIVING data = lv_data.

    REPLACE ALL OCCURRENCES OF '['  IN lv_data WITH ''.
    REPLACE ALL OCCURRENCES OF ']'  IN lv_data WITH ''.

    lv_data = | { '{' } { '"Results":' } { '[ ' } { lv_data } { ']' } { '}' } |.
*    REPLACE ALL OCCURRENCES OF '##'  IN lv_data WITH ''.
    CONDENSE lv_data NO-GAPS.
    SHIFT lv_data LEFT DELETING LEADING ''.

** Deserialize the input our required input ***
    /ui2/cl_json=>deserialize(
    EXPORTING
     json         = lv_data
     pretty_name  = /ui2/cl_json=>pretty_mode-user
     assoc_arrays = abap_true
    CHANGING
     data         = lr_data ).


    IF lr_data IS BOUND.

      ASSIGN lr_data->* TO <data>.
      ASSIGN COMPONENT `RESULTS` OF STRUCTURE <data> TO <results>.
      ASSIGN <results>->* TO <table>.

      REFRESH gt_input.
      LOOP AT <table> ASSIGNING <structure>.

        ASSIGN <structure>->* TO <data>.
        ASSIGN COMPONENT `ORDER_ID` OF STRUCTURE <data> TO <field>.
        IF <field> IS ASSIGNED.
*          lr_data = <field>.
          ASSIGN <field>->* TO <field_value>.
          gs_input-order_id = <field_value>.
          DATA l_ord_id TYPE zorder_id.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = gs_input-order_id
            IMPORTING
              output = l_ord_id.
          gs_input_tb-order_id = l_ord_id.
        ENDIF.
        UNASSIGN: <field>, <field_value>.
        APPEND gs_input_tb TO gt_input_tb.
        CLEAR gs_input.
      ENDLOOP.
    ENDIF.

    IF gt_input_tb IS NOT INITIAL.

      SELECT *
        FROM zsd_dms_sale_hdr
        INTO TABLE @DATA(lt_sale_hdr)
        FOR ALL ENTRIES IN @gt_input_tb
        WHERE ordid = @gt_input_tb-order_id.
      IF sy-subrc = 0.
        LOOP AT gt_input_tb INTO gs_input_tb.
          READ TABLE lt_sale_hdr TRANSPORTING NO FIELDS WITH KEY ordid = gs_input_tb-order_id.
          IF sy-subrc <> 0.
*** Response Part ***
            gw_response-order_id = gs_input_tb-order_id.
            gw_response-status = 'Order Not Found'.
            gw_response-message = 'Order id is invalid'.
            APPEND gw_response TO gt_response.
          ENDIF.
        ENDLOOP.
        DATA lt_irn TYPE STANDARD TABLE OF j_1ig_invrefnum.
        SELECT
               irn
               signed_inv
               signed_qrcode
               FROM j_1ig_invrefnum
               INTO TABLE lt_irn
               FOR ALL ENTRIES IN lt_sale_hdr
               WHERE  bukrs = lt_sale_hdr-bukrs AND
                      docno = lt_sale_hdr-vbeln_vf AND
                      doc_year = lt_sale_hdr-gjahr AND
                      doc_type = 'INV' .

        LOOP AT lt_sale_hdr INTO lw_sale_hdr.

          gw_response-order_id = lw_sale_hdr-ordid.
          CASE lw_sale_hdr-status.
            WHEN '11'.  gw_response-status = '11-Pending for Sales Order' .
            WHEN '12'.  gw_response-status = '12-Pending for Delivery'.
            WHEN '13'.  gw_response-status = '13-Pending for PGI'.
            WHEN '14'.  gw_response-status = '14-Pending for Invoice'.
            WHEN '15'.  gw_response-status = '15-Pending for A/C Document'.
            WHEN '16'.  gw_response-status = '16-Pending for E-Invoice'.
            WHEN '17'.  gw_response-status = '17-Pending for E-Way Bill'.
            WHEN '18'.  gw_response-status = '18-Pending to Complete'.
            WHEN '19'.  gw_response-status = '19-Completed'.
            WHEN OTHERS.
          ENDCASE.

          gw_response-order_id        =   lw_sale_hdr-ordid.
          gw_response-distributor        =   lw_sale_hdr-distb.
          gw_response-dealer_sold        =   lw_sale_hdr-kunag.
          gw_response-dealer_ship        =   lw_sale_hdr-kunwe.
          gw_response-sale_order      =   lw_sale_hdr-vbeln.
          gw_response-invoice_no      =   lw_sale_hdr-vbeln_vf.
          gw_response-message         =   lw_sale_hdr-errmsg.
*
*        SELECT SINGLE
*               irn,
*               signed_inv,
*               signed_qrcode
*               INTO
*               @DATA(lw_irn)
*               FROM j_1ig_invrefnum
*          WHERE  bukrs = @lw_sale_hdr-bukrs AND
*                 docno = @lw_sale_hdr-vbeln_vf AND
*                 doc_year = @lw_sale_hdr-gjahr AND
*                 doc_type = 'INV'.
          READ TABLE lt_irn INTO DATA(lw_irn) WITH KEY bukrs = lw_sale_hdr-bukrs
                                                       docno = lw_sale_hdr-vbeln_vf
                                                       doc_year = lw_sale_hdr-gjahr
                                                       doc_type = 'INV'.
          IF sy-subrc = 0.
            gw_response-einvoice_no     =   lw_irn-irn.
            gw_response-signed_inv      =   lw_irn-signed_inv.
            gw_response-esigned_qrcode  =   lw_irn-signed_qrcode.
          ENDIF.
          APPEND gw_response TO gt_response.
        ENDLOOP.
      ELSE.
*** Response Part ***
        gw_response-order_id = lw_sale_hdr-ordid.
        gw_response-status = 'Order Not Found'.
        gw_response-message = 'Order id is invalid'.
        APPEND gw_response TO gt_response.
      ENDIF.
    ELSE.
*** Response Part ***
      gw_response-order_id = lw_sale_hdr-ordid.
      gw_response-status = 'Order Not Found'.
      gw_response-message = 'Order id is invalid'.
      APPEND gw_response TO gt_response.

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

  ENDMETHOD.
ENDCLASS.
