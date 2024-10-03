class ZCL_API_SAFETY_STOCK definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_SAFETY_STOCK IMPLEMENTATION.


    METHOD if_http_extension~handle_request.

      TYPES : BEGIN OF lv_input,
                plant      TYPE werks_d,
                mat_no     TYPE matnr18,
                safety_stk TYPE eisbe,
              END OF lv_input.

      TYPES : BEGIN OF lv_output,
                seq_no         TYPE num10,
                plant          TYPE werks_d,
                mat_no         TYPE matnr18,
                safety_stk_old TYPE eisbe,
                safety_stk_new TYPE eisbe,
                type           TYPE bapi_mtype,
                msg            TYPE string,
              END OF lv_output.

      DATA : lt_input TYPE TABLE OF lv_input.
      DATA : ls_input TYPE lv_input.
      DATA : lt_output TYPE TABLE OF lv_output.
      DATA : ls_output TYPE lv_output.

      DATA : lv_data TYPE string.
      DATA : lv_msg TYPE string.
      DATA : lv_message TYPE string.

      DATA : lt_stock TYPE TABLE OF zstr_safety_stock.

      DATA : lv_headdata TYPE STANDARD TABLE OF bapimathead.
      DATA : ls_headdata TYPE bapimathead.
      DATA : lv_plandata TYPE TABLE OF bapi_marc.
      DATA : ls_plandata TYPE bapi_marc.
      DATA : ls_plandatax TYPE bapi_marcx.
*    DATA : lv_return TYPE TABLE OF bapiret2.
      DATA : ls_return TYPE bapiret2.
      DATA : lv_body TYPE string.

      DATA : lv_number TYPE num10.

      DATA: lo_logupd TYPE REF TO zcl_api_log_entries_store.
      CREATE OBJECT lo_logupd.

      CLEAR : lv_data.
      CALL METHOD server->request->get_cdata RECEIVING data = lv_data.

*----------------Deserialize the input---------------*

      /ui2/cl_json=>deserialize(
        EXPORTING
          json             =  lv_data                                            " JSON string
          pretty_name      =  /ui2/cl_json=>pretty_mode-camel_case               " Pretty Print property names
        CHANGING
          data             =  lt_input                                           " Data to serialize
      ).



      IF lt_input IS NOT INITIAL.
*-----------------validating Input-------------------*

        LOOP AT lt_input INTO ls_input.

          IF ls_input-plant IS INITIAL.
            lv_msg = | { lv_msg } Plant data is missing |.
          ENDIF.

          IF ls_input-mat_no IS INITIAL.
            lv_msg = | { lv_msg } material data is missing |.

          ELSE.
            DATA(lv_matnr) = |{ ls_input-mat_no ALPHA = IN }|.
            SELECT SINGLE * FROM marc
              INTO @DATA(ls_marc)
              WHERE werks = @ls_input-plant
              AND matnr = @lv_matnr.
            IF sy-subrc <> 0.
              lv_msg = |{ lv_msg } material number { lv_matnr } is invalid |.
            ENDIF.
          ENDIF.


          IF ls_input-safety_stk EQ ls_marc-eisbe.
            lv_message = | { lv_msg } Already the same quantity being exist |.
          ENDIF.




          CALL FUNCTION 'NUMBER_GET_NEXT'
            EXPORTING
              nr_range_nr             = '01'
              object                  = 'ZSAFETY_RA'
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

*--------------------------------------------------------------------------------------------
*--------------------------------------------------------------------------------------------
          IF lv_message IS NOT INITIAL.

            ls_output-seq_no = lv_number.
            ls_output-plant = ls_input-plant.
            ls_output-mat_no = lv_matnr.
            ls_output-safety_stk_old = ls_marc-eisbe.
            ls_output-safety_stk_new = ls_input-safety_stk.
            ls_output-type = 'S'.
            ls_output-msg = lv_message.
            APPEND ls_output TO lt_output.

            DATA(l_update) = VALUE ztb_safety_stock(
              mandt          = sy-mandt
              seq_no         = lv_number
              plant          = ls_input-plant
              mat_no         = ls_input-mat_no
              safety_stk_old = ls_marc-eisbe
              safety_stk_new = ls_input-safety_stk
              type           = 'S'
              message        = lv_message
              chng_date      = sy-datum
              chng_time      = sy-timlo
              user_name      = sy-uname
          ).
            INSERT ztb_safety_stock FROM l_update.
*--------------------------------------------------------------------
          ELSEIF lv_msg IS NOT INITIAL.
            ls_output-seq_no = lv_number.
            ls_output-plant = ls_input-plant.
            ls_output-mat_no = lv_matnr.
            ls_output-safety_stk_old = ls_marc-eisbe.
            ls_output-safety_stk_new = ls_input-safety_stk.
            ls_output-type = 'E'.
            ls_output-msg = lv_msg.
            APPEND ls_output TO lt_output.

            l_update = VALUE ztb_safety_stock(
              mandt          = sy-mandt
              seq_no         = lv_number
              plant          = ls_input-plant
              mat_no         = ls_input-mat_no
              safety_stk_old = ls_marc-eisbe
              safety_stk_new = ls_input-safety_stk
              type           = 'E'
              message        = lv_msg
              chng_date      = sy-datum
              chng_time      = sy-timlo
              user_name      = sy-uname
          ).
            INSERT ztb_safety_stock FROM l_update.

          ELSE.
*---------------------if LV_MSG contains any data--------------*

*            ls_output-seq_no = lv_number.
*            ls_output-plant = ls_input-plant.
*            ls_output-mat_no = lv_matnr.
*            ls_output-safety_stk_old = ls_marc-eisbe.
*            ls_output-safety_stk_new = ls_input-safety_stk.
*            ls_output-type = 'S'.
*            ls_output-msg = lv_msg.
*            APPEND ls_output TO lt_output.
*
*            l_update = VALUE ztb_safety_stock(
*             mandt          = sy-mandt
*             seq_no         = lv_number
*             plant          = ls_input-plant
*             mat_no         = ls_input-mat_no
*             safety_stk_old = ls_marc-eisbe
*             safety_stk_new = ls_input-safety_stk
*             type           = 'S'
*             message        = lv_msg
*             chng_date      = sy-datum
*             chng_time      = sy-timlo
*             user_name      = sy-uname
*         ).
*            INSERT ztb_safety_stock FROM l_update.


*--------------------changing safety stock-----------------------*



            ls_plandata-plant = ls_input-plant.
            ls_headdata-material = lv_matnr.
            ls_headdata-mrp_view = 'X'.
            ls_plandata-safety_stk = ls_input-safety_stk.
            ls_plandatax-safety_stk = 'X'.
            ls_plandatax-plant = ls_plandata-plant.


            CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
              EXPORTING
                headdata   = ls_headdata
                plantdata  = ls_plandata
                plantdatax = ls_plandatax
              IMPORTING
                return     = ls_return.



*---------------------if error ( 'E' ) returns through RETURN-------------------------*

            IF ls_return-type = 'E' OR ls_return-type = 'A'.
              CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
              CLEAR : lv_msg.


              lv_msg = |{ lv_msg } { ls_return-message }|.

              APPEND VALUE #(
                   seq_no     = lv_number
                   plant      = ls_input-plant
                   mat_no     = lv_matnr
                   safety_stk_old = ls_marc-eisbe
                   type       = ls_return-type
                   msg        = lv_msg
               ) TO lt_output.

              l_update = VALUE ztb_safety_stock(
                  mandt          = sy-mandt
                  seq_no         = lv_number
                  plant          = ls_input-plant
                  mat_no         = ls_input-mat_no
                  safety_stk_old = ls_marc-eisbe
                  safety_stk_new = ls_input-safety_stk
                  type           = ls_return-type
                  message        = lv_msg
                  chng_date      = sy-datum
                  chng_time      = sy-timlo
                  user_name      = sy-uname
              ).
              INSERT ztb_safety_stock FROM l_update.
              CLEAR : ls_return.
*----------------------if success ( 'S' ) returns through RETURN-------------------*

            ELSE.

              lv_msg = | { lv_msg } { ls_return-message } |.

              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                EXPORTING
                  wait = 'X'.

              APPEND VALUE #(
                  seq_no     = lv_number
                  plant      = ls_input-plant
                  mat_no     = lv_matnr
                  safety_stk_old = ls_marc-eisbe
                  safety_stk_new = ls_input-safety_stk
                  type       = ls_return-type
                  msg        = lv_msg
              ) TO lt_output.


              l_update = VALUE ztb_safety_stock(
                  mandt          = sy-mandt
                  seq_no         = lv_number
                  plant          = ls_input-plant
                  mat_no         = ls_input-mat_no
                  safety_stk_old = ls_marc-eisbe
                  safety_stk_new = ls_input-safety_stk
                  type           = ls_return-type
                  message        = lv_msg
                  chng_date      = sy-datum
                  chng_time      = sy-timlo
                  user_name      = sy-uname
              ).
              INSERT ztb_safety_stock FROM l_update.
              CLEAR : ls_return.
            ENDIF.
          ENDIF.
        ENDLOOP.

      ENDIF.
*--------------------------serializing output into JSON--------------------*

      IF lt_output IS NOT INITIAL.

        /ui2/cl_json=>serialize(
          EXPORTING
            data             =  lt_output                                              " Data to serialize
            pretty_name      =  /ui2/cl_json=>pretty_mode-user                         " Pretty Print property names

          RECEIVING
            r_json           =  lv_body                " JSON string
        ).

      ENDIF.
*-------------------------storing INPUT and OUTPUT JSON-----------------------------*

      CALL METHOD lo_logupd->log_entry_store
        EXPORTING
          apiname         = 'SAFETY_STOCK'
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
