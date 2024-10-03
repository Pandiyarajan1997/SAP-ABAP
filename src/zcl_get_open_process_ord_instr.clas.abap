class ZCL_GET_OPEN_PROCESS_ORD_INSTR definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_GET_OPEN_PROCESS_ORD_INSTR IMPLEMENTATION.


  METHOD if_http_extension~handle_request.

*** Purpose of this API is for Customer incoming Payment entry Posting ***
    TYPES: BEGIN OF ty_input,
             plant   TYPE werks_d,
             orderno TYPE aufnr,
             matnr   TYPE matnr,
           END OF ty_input.

    DATA: gt_input TYPE STANDARD TABLE OF ty_input,
          gs_input TYPE ty_input.

    DATA: gt_response TYPE STANDARD TABLE OF zpp_st_order_details_hd_ins.
    DATA: gw_response LIKE LINE OF gt_response.
*
    DATA: lv_subrc TYPE sy-subrc.
    DATA: lv_message TYPE string.
    DATA: lv_body TYPE string.
    DATA: v_jsonload TYPE string.
    DATA: lv_response TYPE string.
    DATA: lv_data TYPE string.
*
    DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
    CREATE OBJECT lo_log_upd.
*
    CALL METHOD server->request->get_cdata RECEIVING data = lv_data.

** Deserialize the input our required input ***
    /ui2/cl_json=>deserialize(
    EXPORTING
     json         = lv_data
     pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
     assoc_arrays = abap_true
    CHANGING
     data         = gs_input ).

    DATA(l_plant) = gs_input-plant.
    DATA(l_orderno) = gs_input-orderno.
    DATA(l_matnr) = gs_input-matnr.

    DATA: it_header TYPE STANDARD TABLE OF  zpp_st_order_details_ins,
          it_comp   TYPE STANDARD TABLE OF  zpp_st_order_item_ins.

    CALL FUNCTION 'ZMM_GET_OPEN_PROCESS_ORD'
      EXPORTING
        plant     = l_plant
        orderno   = l_orderno
        matnr     = l_matnr
      TABLES
        it_header = it_header
        it_comp   = it_comp
*     EXCEPTIONS
*       INVALID_PLANT       = 1
*       OTHERS    = 2
      .
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
*    v_jsonload = '[ ['.
    DATA lv_str TYPE string.
    DESCRIBE TABLE it_header LINES DATA(l_hdrline).
    LOOP AT it_header INTO DATA(lw_hd).
      lv_str = lw_hd-target_quantity.

      CONCATENATE v_jsonload '{"ORDER_NUMBER":"' lw_hd-order_number '",'
                             '"PRODUCTION_PLANT":"' lw_hd-production_plant '",'
                             '"MATERIAL":"' lw_hd-material '",'
                             '"MATERIAL_TEXT":"' lw_hd-material_text '",'
                             '"ACTUAL_START_DATE":"' lw_hd-actual_start_date '",'
                             '"TARGET_QUANTITY":"' lv_str '",'
                             '"UNIT":"' lw_hd-unit '",'
                             '"BATCH":"' lw_hd-batch '",'
                             '"ORDER_TYPE":"' lw_hd-order_type '",'
                             '"ITEM":[' INTO v_jsonload.
*      MOVE-CORRESPONDING lw_hd TO gw_response.
      DATA(lt_items) = it_comp.
      DELETE lt_items WHERE order_number NE lw_hd-order_number.
      DESCRIBE TABLE lt_items LINES DATA(l_itemline).
      LOOP AT lt_items INTO DATA(lw_item)." WHERE order_number = lw_hd-order_number.
        lv_str = lw_item-req_quan.
        IF l_itemline NE sy-tabix.
          REPLACE ALL OCCURRENCES OF '"' IN lw_item-matnr_pro WITH ''.
          REPLACE ALL OCCURRENCES OF '"' IN lw_item-matnr_pro1 WITH ''.
          REPLACE ALL OCCURRENCES OF '"' IN lw_item-matnr_pro2 WITH ''.
          REPLACE ALL OCCURRENCES OF '"' IN lw_item-matnr_pro3 WITH ''.
          REPLACE ALL OCCURRENCES OF '''' IN lw_item-matnr_pro WITH ''.
          REPLACE ALL OCCURRENCES OF '''' IN lw_item-matnr_pro1 WITH ''.
          REPLACE ALL OCCURRENCES OF '''' IN lw_item-matnr_pro2 WITH ''.
          REPLACE ALL OCCURRENCES OF '''' IN lw_item-matnr_pro3 WITH ''.
          CONCATENATE v_jsonload '{"ORDER_NUMBER":"' lw_item-order_number '",'
                                 '"MATERIAL":"' lw_item-material '",'
                                 '"PROD_PLANT":"' lw_item-prod_plant '",'
                                 '"REQ_QUAN":"' lv_str '",'
                                 '"ENTRY_UOM":"' lw_item-entry_uom '",'
                                 '"MATNR_PRO":"' lw_item-matnr_pro '",'
                                 '"MATNR_PRO1":"' lw_item-matnr_pro1 '",'
                                 '"MATNR_PRO2":"' lw_item-matnr_pro2 '",'
                                 '"MATNR_PRO3":"' lw_item-matnr_pro3 '"},' INTO v_jsonload.
*        APPEND lw_item TO gw_response-item.
        ELSE.
          CONCATENATE v_jsonload '{"ORDER_NUMBER":"' lw_item-order_number '",'
                                 '"MATERIAL":"' lw_item-material '",'
                                 '"PROD_PLANT":"' lw_item-prod_plant '",'
                                 '"REQ_QUAN":"' lv_str '",'
                                 '"ENTRY_UOM":"' lw_item-entry_uom '",'
                                 '"MATNR_PRO":"' lw_item-matnr_pro '",'
                                 '"MATNR_PRO1":"' lw_item-matnr_pro1 '",'
                                 '"MATNR_PRO2":"' lw_item-matnr_pro2 '",'
                                 '"MATNR_PRO3":"' lw_item-matnr_pro3 '"}' INTO v_jsonload.
        ENDIF.
      ENDLOOP.
      IF l_hdrline NE sy-tabix.
        CONCATENATE v_jsonload ']},' INTO v_jsonload.
      ELSE.
        CONCATENATE v_jsonload ']}' INTO v_jsonload.
      ENDIF.
*      APPEND gw_response TO gt_response.
    ENDLOOP.
*** Response Part ***
    IF v_jsonload IS NOT INITIAL.
*    IF gt_response[] IS NOT INITIAL.
*** serialize the output for response ***
*      /ui2/cl_json=>serialize(
*      EXPORTING
*       data         =  gt_response
*       pretty_name  = /ui2/cl_json=>pretty_mode-user
*      RECEIVING
*       r_json         = lv_body ).
*
      v_jsonload = |[{ v_jsonload }]|.

      CALL METHOD lo_log_upd->log_entry_store
        EXPORTING
          apiname         = 'PROCESSORD_INS'
          ijson           = lv_data
          ojson           = v_jsonload
        EXCEPTIONS
          apiname_missing = 1
          json_missing    = 2
          OTHERS          = 3.
      IF sy-subrc <> 0.

      ENDIF.

*Set JSON Content-Type
      CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
      CALL METHOD server->response->set_cdata( data = v_jsonload ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
