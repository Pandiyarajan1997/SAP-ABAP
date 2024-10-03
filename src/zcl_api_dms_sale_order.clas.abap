class ZCL_API_DMS_SALE_ORDER definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_DMS_SALE_ORDER IMPLEMENTATION.


  METHOD if_http_extension~handle_request.

*** Purpose of this API is for Customer incoming Payment entry Posting ***
    TYPES: BEGIN OF ty_input,
             order_id    TYPE string, "zorder_id, *Order Type	YDMS
             order_type	 TYPE string, "auart,
             distributor TYPE string, "kunnr,
             dealer_code TYPE string, "kunnr,
             ref_doc_no  TYPE string, "xblnr,
             material    TYPE string, "matnr,
             quantity    TYPE string, "quant,
             uom         TYPE string, "meins,
             discount    TYPE string, "skfbp,
           END OF ty_input.


*    DATA: gt_input TYPE STANDARD TABLE OF zsd_st_dms_saleord_hd.
    DATA: gs_input TYPE zsd_st_dms_saleord_hd.

    TYPES: BEGIN OF ty_msg,
             order_id   TYPE string,
             sale_order TYPE string,
             status     TYPE string,
             message    TYPE string,
           END OF ty_msg.
*VBELN  VBELN CHAR  10  0 0 Sales and Distribution Document Number
*VBELN_VL VBELN_VL  CHAR  10  0 0 Delivery
*MBLNR  MBLNR CHAR  10  0 0 Number of Material Document
*VBELN_VF VBELN_VF  CHAR  10  0 0 Billing Document
*BELNR  BELNR_D CHAR  10  0 0 Accounting Document Number
*STATUS Z_STATS CHAR  2 0 0 Status Code
*ERRMSG BAPI_MSG  CHAR  220 0 0 Message Text

    DATA: gt_response TYPE TABLE OF ty_msg.
    DATA: gw_response TYPE ty_msg.

    DATA: lv_body TYPE string.
    DATA: v_jsonload TYPE string.

    DATA: lv_data TYPE string.
    DATA: lr_data TYPE REF TO data.
    DATA: "lt_sale_hdr TYPE STANDARD TABLE OF zsd_dms_sale_hdr,
      lt_sale_itm TYPE STANDARD TABLE OF zsd_dms_sale_itm,
      lw_sale_hdr TYPE zsd_dms_sale_hdr,
      lw_sale_itm TYPE zsd_dms_sale_itm.

*** Field Symbols for splitting input ***
    FIELD-SYMBOLS:
      <data>        TYPE data,
      <results>     TYPE data,
      <structure>   TYPE any,
      <structure1>  TYPE any,
      <table>       TYPE ANY TABLE,
      <field>       TYPE any,
      <field_value> TYPE data.

    CALL METHOD server->request->get_cdata RECEIVING data = lv_data.

*    REPLACE ALL OCCURRENCES OF '['  IN lv_data WITH ''.
*    REPLACE ALL OCCURRENCES OF ']'  IN lv_data WITH ''.
*
*    lv_data = | { '{' } { '"Results":' } { '[ ' } { lv_data } { ']' } { '}' } |.
*    REPLACE ALL OCCURRENCES OF '##'  IN lv_data WITH ''.
*    CONDENSE lv_data NO-GAPS.
*    SHIFT lv_data LEFT DELETING LEADING ''.

** Deserialize the input our required input ***
    /ui2/cl_json=>deserialize(
    EXPORTING
     json         = lv_data
     pretty_name  = /ui2/cl_json=>pretty_mode-user
     assoc_arrays = abap_true
    CHANGING
     data         = gs_input ). "lr_data ).

*    IF lr_data IS BOUND.
*
*      ASSIGN lr_data->* TO <data>.
*      ASSIGN COMPONENT `RESULTS` OF STRUCTURE <data> TO <results>.
*      ASSIGN <results>->* TO <table>.
*
*      REFRESH gt_input.
*      LOOP AT <table> ASSIGNING <structure>.
*
*        ASSIGN <structure>->* TO <data>.
*        ASSIGN COMPONENT `ORDER_ID` OF STRUCTURE <data> TO <field>.
*        IF <field> IS ASSIGNED.
**          lr_data = <field>.
*          ASSIGN <field>->* TO <field_value>.
*          gs_input-order_id = <field_value>.
*        ENDIF.
*        UNASSIGN: <field>, <field_value>.
*
*        ASSIGN COMPONENT `ORDER_TYPE` OF STRUCTURE <data> TO <field>.
*        IF <field> IS ASSIGNED.
**          lr_data = <field>.
*          ASSIGN <field>->* TO <field_value>.
*          gs_input-order_type = <field_value>.
*        ENDIF.
*        UNASSIGN: <field>, <field_value>.
*
*        ASSIGN COMPONENT `DISTRIBUTOR` OF STRUCTURE <data> TO <field>.
*        IF <field> IS ASSIGNED.
**          lr_data = <field>.
*          ASSIGN <field>->* TO <field_value>.
*          gs_input-distributor = <field_value>.
*        ENDIF.
*        UNASSIGN: <field>, <field_value>.
*
*        ASSIGN COMPONENT `DEALER_CODE` OF STRUCTURE <data> TO <field>.
*        IF <field> IS ASSIGNED.
**          lr_data = <field>.
*          ASSIGN <field>->* TO <field_value>.
*          gs_input-dealer_code = <field_value>.
*        ENDIF.
*        UNASSIGN: <field>, <field_value>.
*
*        ASSIGN COMPONENT `REF_DOC_NO` OF STRUCTURE <data> TO <field>.
*        IF <field> IS ASSIGNED.
**          lr_data = <field>.
*          ASSIGN <field>->* TO <field_value>.
*          gs_input-ref_doc_no = <field_value>.
*        ENDIF.
*        UNASSIGN: <field>, <field_value>.
*
*        ASSIGN COMPONENT `DISCOUNT` OF STRUCTURE <data> TO <field>.
*        IF <field> IS ASSIGNED.
**          lr_data = <field>.
*          ASSIGN <field>->* TO <field_value>.
*          gs_input-discount = <field_value>.
*        ENDIF.
*        UNASSIGN: <field>, <field_value>.
*        DATA lw_item TYPE zsd_st_dms_saleord_it.
*        LOOP AT gs_input-item ASSIGNING <structure1>.
*          ASSIGN <structure1>->* TO <data>.
*          ASSIGN COMPONENT `MATERIAL` OF STRUCTURE <data> TO <field>.
*          IF <field> IS ASSIGNED.
**          lr_data = <field>.
*            ASSIGN <field>->* TO <field_value>.
*            lw_item-material = <field_value>.
*          ENDIF.
*          UNASSIGN: <field>, <field_value>.
*
*          ASSIGN COMPONENT `QUANTITY` OF STRUCTURE <data> TO <field>.
*          IF <field> IS ASSIGNED.
**          lr_data = <field>.
*            ASSIGN <field>->* TO <field_value>.
*            lw_item-quantity = <field_value>.
*          ENDIF.
*          UNASSIGN: <field>, <field_value>.
*
*          ASSIGN COMPONENT `UOM` OF STRUCTURE <data> TO <field>.
*          IF <field> IS ASSIGNED.
**          lr_data = <field>.
*            ASSIGN <field>->* TO <field_value>.
*            lw_item-uom = <field_value>.
*          ENDIF.
*          UNASSIGN: <field>, <field_value>.
*
*          ASSIGN COMPONENT `ITEM_DISCOUNT` OF STRUCTURE <data> TO <field>.
*          IF <field> IS ASSIGNED.
**          lr_data = <field>.
*            ASSIGN <field>->* TO <field_value>.
*            lw_item-item_discount = <field_value>.
*          ENDIF.
*          UNASSIGN: <field>, <field_value>.
*          APPEND lw_item TO gs_input-item.
*        ENDLOOP.
*        APPEND gs_input TO gt_input.
*        CLEAR gs_input.
*      ENDLOOP.
*    ENDIF.
    IF gs_input IS NOT INITIAL.
      lw_sale_hdr-ordid    = gs_input-order_id. " VALUE #( gt_input[ 1 ]-order_id OPTIONAL ).
      lw_sale_hdr-auart    = gs_input-order_type. "VALUE #( gt_input[ 1 ]-order_type OPTIONAL ).
      lw_sale_hdr-vkorg    = 'SDMS'. "Sales Organization  SDMS
      lw_sale_hdr-vtweg    = '20'. " DIstrubtion CHannel
      lw_sale_hdr-spart    = '10'. " Division
      lw_sale_hdr-distb    =  gs_input-distributor. "VALUE #( gt_input[ 1 ]-distributor  OPTIONAL ).

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lw_sale_hdr-ordid
        IMPORTING
          output = lw_sale_hdr-ordid.
      SELECT SINGLE ordid FROM
          zsd_dms_sale_hdr INTO lw_sale_hdr-ordid
          WHERE ordid = lw_sale_hdr-ordid.
      IF sy-subrc = 0.
        gw_response-order_id = lw_sale_hdr-ordid.
        gw_response-status = 'Error'.
        gw_response-message = 'Order ID is already exist'.
        APPEND gw_response TO gt_response.
      ENDIF.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lw_sale_hdr-distb
        IMPORTING
          output = lw_sale_hdr-distb.

      lw_sale_hdr-kunag    =  gs_input-dealer_sold. "VALUE #( gt_input[ 1 ]-dealer_code  OPTIONAL ).
      lw_sale_hdr-kunwe    =  gs_input-dealer_ship. "VALUE #( gt_input[ 1 ]-dealer_code  OPTIONAL ).

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lw_sale_hdr-kunag
        IMPORTING
          output = lw_sale_hdr-kunag.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lw_sale_hdr-kunwe
        IMPORTING
          output = lw_sale_hdr-kunwe.

      SELECT SINGLE werks
        FROM kna1 INTO lw_sale_hdr-werks
        WHERE kunnr =  lw_sale_hdr-distb.
      IF sy-subrc <> 0.
        gw_response-order_id = lw_sale_hdr-ordid.
        gw_response-status = 'Error'.
        gw_response-message = 'Distributor Plant not found'.
        APPEND gw_response TO gt_response.
      ENDIF.

      SELECT SINGLE vkbur
        FROM knvv INTO lw_sale_hdr-vkbur
        WHERE  kunnr = lw_sale_hdr-kunag AND
               vwerk = lw_sale_hdr-werks.
      IF sy-subrc <> 0.
        gw_response-order_id = lw_sale_hdr-ordid.
        gw_response-status = 'Error'.
        gw_response-message = 'Dealer Sales office not found'.
        APPEND gw_response TO gt_response.
      ENDIF.

      SELECT SINGLE kunnr FROM kna1
        INTO @DATA(l_kunag)
        WHERE kunnr = @lw_sale_hdr-kunag.
      IF sy-subrc <> 0.
        gw_response-order_id = lw_sale_hdr-ordid.
        gw_response-status = 'Error'.
        gw_response-message = 'Dealer Sold Party Code not found'.
        APPEND gw_response TO gt_response.
      ENDIF.
      SELECT SINGLE kunnr FROM kna1
        INTO @DATA(l_kunwe)
        WHERE kunnr = @lw_sale_hdr-kunwe.
      IF sy-subrc <> 0.
        gw_response-order_id = lw_sale_hdr-ordid.
        gw_response-status = 'Error'.
        gw_response-message = 'Dealer Ship to Code not found'.
        APPEND gw_response TO gt_response.
      ENDIF.
      SELECT SINGLE kunnr FROM kna1
        INTO @DATA(l_dist)
        WHERE kunnr = @lw_sale_hdr-distb.
      IF sy-subrc <> 0.
        gw_response-order_id = lw_sale_hdr-ordid.
        gw_response-status = 'Error'.
        gw_response-message = 'Distributor Code not found'.
        APPEND gw_response TO gt_response.
      ENDIF.
      SELECT SINGLE loevm FROM knb1
        INTO @DATA(l_flag)
        WHERE kunnr = @lw_sale_hdr-distb.
      IF sy-subrc <> 0.
        gw_response-order_id = lw_sale_hdr-ordid.
        gw_response-status = 'Error'.
        gw_response-message = 'Distributor Code is locked'.
        APPEND gw_response TO gt_response.
      ENDIF.
      SELECT SINGLE loevm FROM knb1
        INTO l_flag
        WHERE kunnr = lw_sale_hdr-kunag.
      IF sy-subrc <> 0.
        gw_response-order_id = lw_sale_hdr-ordid.
        gw_response-status = 'Error'.
        gw_response-message = 'Dealer Code is locked'.
        APPEND gw_response TO gt_response.
      ENDIF.
      SELECT SINGLE loevm FROM knb1
        INTO l_flag
        WHERE kunnr = lw_sale_hdr-kunwe.
      IF sy-subrc <> 0.
        gw_response-order_id = lw_sale_hdr-ordid.
        gw_response-status = 'Error'.
        gw_response-message = 'Dealer Code is locked'.
        APPEND gw_response TO gt_response.
      ENDIF.

      lw_sale_hdr-xblnr    =  gs_input-ref_doc_no." VALUE #( gt_input[ 1 ]-ref_doc_no  OPTIONAL ).
      lw_sale_hdr-skfbp    = gs_input-discount.
      lw_sale_hdr-cdate    = sy-datum.
      lw_sale_hdr-aedat    = sy-datum.
      lw_sale_hdr-status   = '11'. "Initial
      LOOP AT gs_input-item INTO DATA(lw_data).
        lw_sale_itm-ordid    = gs_input-order_id.
        lw_sale_itm-matnr    = lw_data-material.
        lw_sale_itm-quant    = lw_data-quantity.
        lw_sale_itm-meins    = lw_data-uom.
        lw_sale_itm-skfbp    = lw_data-item_discount.
        lw_sale_itm-cdate    = sy-datum.
        APPEND lw_sale_itm TO lt_sale_itm.
        SELECT SINGLE matnr FROM mara INTO @DATA(l_mara)
              WHERE matnr = @lw_sale_itm-matnr.
        IF sy-subrc <> 0.
          gw_response-order_id = lw_sale_hdr-ordid.
          gw_response-status = 'Error'.
          gw_response-message = |Maretial { lw_sale_itm-matnr } is not found|.
          APPEND gw_response TO gt_response.
        ENDIF.
        SELECT SINGLE matnr FROM marc INTO @DATA(l_werk)
              WHERE matnr = @lw_sale_itm-matnr AND
                    werks = @lw_sale_hdr-werks.
        IF sy-subrc <> 0.
          gw_response-order_id = lw_sale_hdr-ordid.
          gw_response-status = 'Error'.
          gw_response-message = |Plant { lw_sale_hdr-werks } is not found for Maretial { lw_sale_itm-matnr }|.
          APPEND gw_response TO gt_response.
        ENDIF.
        IF lw_sale_itm-quant = 0.
          gw_response-order_id = lw_sale_hdr-ordid.
          gw_response-status = 'Error'.
          gw_response-message = |Maretial { lw_sale_itm-matnr } quantity is zero|.
          APPEND gw_response TO gt_response.
        ENDIF.
      ENDLOOP.
      IF gt_response IS INITIAL.
        DATA : lo_main TYPE REF TO zcl_dms_sale_order_process.
        DATA: lt_return TYPE STANDARD TABLE OF bapiret2.
        CREATE OBJECT lo_main.
        CALL METHOD lo_main->create_sale_order
          EXPORTING
*           order_id      =
            iv_header     = lw_sale_hdr
            iv_item       = lt_sale_itm
          IMPORTING
            sale_order_no = lw_sale_hdr-vbeln
            return        = lt_return.
        IF lw_sale_hdr-vbeln IS NOT INITIAL.
          lw_sale_hdr-status   = '12'. "Sale Order
        ENDIF.
        MODIFY zsd_dms_sale_hdr FROM lw_sale_hdr .
        MODIFY zsd_dms_sale_itm FROM TABLE lt_sale_itm .
*** Response Part ***
        gw_response-order_id = lw_sale_hdr-ordid.
        gw_response-status = 'Sale Order is created'.
        gw_response-sale_order = lw_sale_hdr-vbeln.
        gw_response-message = ''.
        APPEND gw_response TO gt_response.
      ENDIF.
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
