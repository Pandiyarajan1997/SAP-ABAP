class ZCL_TOSTOCK_API definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_TOSTOCK_API IMPLEMENTATION.


  METHOD if_http_extension~handle_request.

    TYPES : BEGIN OF lv_input,
              comp_code TYPE bukrs,
            END OF lv_input.

    TYPES : BEGIN OF lv_output,
              dist_code   TYPE kunnr,
              plant       TYPE werks_d,
              material    TYPE matnr,
              qty         TYPE labst,
              uom         TYPE meins,
              stock_price TYPE wrbtr,
              stock_lit   TYPE labst,
              type        TYPE bapi_mtype,
              msg         TYPE string,
            END OF lv_output.


    DATA : lt_input TYPE TABLE OF lv_input.
    DATA : ls_input TYPE lv_input.

    DATA : lt_output TYPE TABLE OF lv_output.
    DATA : ls_output TYPE lv_output.

    DATA : gt_output TYPE TABLE OF zst_dist_stock_dms.
    DATA : gs_output TYPE zst_dist_stock_dms.

    DATA : lv_data TYPE string.
    DATA : lv_msg TYPE string.
    DATA : lv_number TYPE num10.
    DATA : lv_body TYPE string.

    DATA : lo_logupd TYPE REF TO zcl_api_log_entries_store.
    CREATE OBJECT lo_logupd.

    CLEAR : lv_data.
    CALL METHOD server->request->get_cdata RECEIVING data = lv_data.

    /ui2/cl_json=>deserialize(
      EXPORTING
        json             = lv_data                 " JSON string
        pretty_name      = /ui2/cl_json=>pretty_mode-camel_case                 " Pretty Print property names

      CHANGING
        data             = lt_input                 " Data to serialize
    ).

    IF lt_input IS NOT INITIAL.


      CLEAR : ls_input.
      CLEAR : lv_msg.

      LOOP AT lt_input INTO ls_input.

        IF ls_input-comp_code IS INITIAL.
          lv_msg = | { lv_msg } Company Code Is Missing |.

        ELSE.

*================================================================================================================
          TRANSLATE ls_input-comp_code TO UPPER CASE.
          SELECT bwkey,bukrs FROM t001k INTO TABLE @DATA(lt_t001k) WHERE  bukrs = @ls_input-comp_code.


          IF sy-subrc = 0.
***********FETCH the datas from mard & mara**********
            SELECT c~kunnr,
                   a~bwkey,
                   a~matnr,
                   a~lbkum,
                   b~meins,a~salk3 INTO TABLE @gt_output
                           FROM mbew AS a
                           INNER JOIN mara AS b
                           ON b~matnr = a~matnr
                           INNER JOIN kna1 AS c
                           ON c~werks = a~bwkey
                           FOR ALL ENTRIES IN @lt_t001k
                           WHERE a~bwkey = @lt_t001k-bwkey
                           AND lbkum NE ' '.
            IF sy-subrc = 0.

              "fetch the data from the MARM table

              SELECT matnr,meinh FROM marm
                INTO TABLE @DATA(it_marm)
                FOR ALL ENTRIES IN @gt_output
                WHERE matnr = @gt_output-material.

              SORT: it_marm BY matnr meinh.
              SORT : gt_output BY plant material.
            ENDIF.


            CALL FUNCTION 'NUMBER_GET_NEXT'
              EXPORTING
                nr_range_nr             = '01'
                object                  = 'ZSNRO_TOST'
              IMPORTING
                number                  = lv_number
              EXCEPTIONS
                interval_not_found      = 1
                number_range_not_intern = 2
                object_not_found        = 3
                quantity_is_0           = 4
                quantity_is_not_1       = 5
                interval_overflow       = 6
                buffer_overflow         = 7
                OTHERS                  = 8.
            IF sy-subrc <> 0.
* Implement suitable error handling here
            ENDIF.

*  DATA : ls_output LIKE LINE OF gt_output.

            LOOP AT gt_output INTO gs_output.
              READ TABLE it_marm INTO DATA(ls_marm) WITH KEY matnr = gs_output-material
                                                             meinh = 'L' BINARY SEARCH.
              IF sy-subrc = 0.

                CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
                  EXPORTING
                    i_matnr  = gs_output-material
                    i_in_me  = gs_output-uom
                    i_out_me = 'L'
                    i_menge  = gs_output-qty
                  IMPORTING
                    e_menge  = gs_output-stock_lit.


              ELSE.

                READ TABLE it_marm INTO ls_marm WITH KEY matnr = gs_output-material
                                                            meinh = 'KG' BINARY SEARCH.
                IF sy-subrc = 0.

                  CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
                    EXPORTING
                      i_matnr  = gs_output-material
                      i_in_me  = gs_output-uom
                      i_out_me = 'KG'
                      i_menge  = gs_output-qty
                    IMPORTING
                      e_menge  = gs_output-stock_lit.

                ENDIF.
              ENDIF.


              MODIFY gt_output FROM gs_output.
*                CLEAR : gs_output.



*====================================================================================================================
**-------------------------------------------------------------------------------------------------------------------




              IF lv_msg IS NOT INITIAL.
                ls_output-dist_code = gs_output-dist_code.
                ls_output-material = gs_output-material.
                ls_output-plant = gs_output-plant.
                ls_output-qty = gs_output-qty.
                ls_output-uom = gs_output-uom.
                ls_output-stock_price = gs_output-stock_price.
                ls_output-stock_lit = gs_output-stock_lit.
                ls_output-type = 'E'.
                ls_output-msg = lv_msg.
                APPEND ls_output TO lt_output.

*                DATA(l_update) = VALUE ztb_tostock(
*                    mandt     = sy-mandt
*                    seq_no    = lv_number
*                    comp_code     = ls_input-comp_code
*                    mat_no    = gs_output-material
*                    qty       = gs_output-qty
*                    uom       = gs_output-uom
*                    chng_time = sy-timlo
*                    chng_date = sy-datum
*                    type      = 'E'
*                    message   = lv_msg
*                    user_name = sy-uname
*                ).
*                INSERT ztb_tostock FROM l_update.

              ELSE.

                CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                  EXPORTING
                    wait = 'X'.

                ls_output-dist_code = gs_output-dist_code.
                ls_output-material = gs_output-material.
                ls_output-plant = gs_output-plant.
                ls_output-qty = gs_output-qty.
                ls_output-uom = gs_output-uom.
                ls_output-stock_price = gs_output-stock_price.
                ls_output-stock_lit = gs_output-stock_lit.
                ls_output-type = 'S'.
                ls_output-msg = 'required data found'.
                APPEND ls_output TO lt_output.
*
*                l_update = VALUE ztb_tostock(
*                    mandt     = sy-mandt
*                    seq_no    = lv_number
*                    comp_code     = ls_input-comp_code
*                    mat_no    = gs_output-material
*                    qty       = gs_output-qty
*                    uom       = gs_output-uom
*                    chng_time = sy-timlo
*                    chng_date = sy-datum
*                        type      = 'S'
*                        message   = 'required data found'
*                    user_name = sy-uname
*                ).
*                INSERT ztb_tostock FROM l_update.

              ENDIF.
*            ENDIF.


              CLEAR : gs_output.

            ENDLOOP.
          ENDIF.

        ENDIF.
      ENDLOOP.
    ENDIF.


    IF lt_output IS NOT INITIAL.

      /ui2/cl_json=>serialize(
        EXPORTING
          data             =  lt_output                " Data to serialize
          pretty_name      =  /ui2/cl_json=>pretty_mode-user                 " Pretty Print property names
        RECEIVING
          r_json           =  lv_body                " JSON string
      ).

    ENDIF.

    CALL METHOD lo_logupd->log_entry_store
      EXPORTING
        apiname         = 'TOTAL_STOCK'
        ijson           = lv_data
        ojson           = lv_body
      EXCEPTIONS
        apiname_missing = 1
        json_missing    = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.

    ENDIF.

*Set JSON Content-Type
    CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
    CALL METHOD server->response->set_cdata( data = lv_body ).

  ENDMETHOD.
ENDCLASS.
