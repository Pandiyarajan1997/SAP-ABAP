class ZCL_API_SAPSO_CLOSE definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_SAPSO_CLOSE IMPLEMENTATION.


  METHOD if_http_extension~handle_request.

    "Created by: Pandiarajan
    "Created on: 30.09.2023
    "Reference by: Ramakrishnan J & Praveen Kumar
    "-----------------------------------------------------------------------------------*


    TYPES: BEGIN OF ty_msg,

             orderid    TYPE zorder_id,
             salesorder TYPE vbeln,
             status     TYPE bapi_mtype,
             message    TYPE string,

           END OF ty_msg.

    TYPES: BEGIN OF ty_input,

             orderid    TYPE zorder_id,
             salesorder TYPE vbeln,

           END OF ty_input.

    DATA: gs_input     TYPE ty_input.

    DATA: gt_response TYPE TABLE OF ty_msg,
          lv_msg      TYPE string.

    DATA: lv_body TYPE string.
    DATA: v_jsonload TYPE string.
    DATA: lv_data TYPE string.
    DATA: lt_orderid    TYPE TABLE OF rsdsselopt,
          lt_salesorder TYPE TABLE OF rsdsselopt,
          ls_orderid    TYPE rsdsselopt.

    DATA : lt_item    TYPE TABLE OF  bapisditm,
           lt_return  TYPE TABLE OF  bapiret2,
           lt_itemx   TYPE TABLE OF  bapisditmx,
           ls_headerx TYPE  bapisdh1x.


    DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
    CREATE OBJECT lo_log_upd.


    CALL METHOD server->request->get_cdata RECEIVING data = lv_data.

** Deserialize the input our required input ***
    /ui2/cl_json=>deserialize(
    EXPORTING
     json         = lv_data
     pretty_name  = /ui2/cl_json=>pretty_mode-user
     assoc_arrays = abap_true
    CHANGING
     data         = gs_input ).


    IF gs_input-orderid IS INITIAL OR gs_input-salesorder IS INITIAL.             "check data passed or not

      lv_msg = |{ lv_msg } ; Fill the both input fields |.

    ELSE.

      SELECT SINGLE vbeln FROM zsd_spl_sale_hd
                  INTO @DATA(lv_vbeln)
                  WHERE ordid = @gs_input-orderid AND vbeln = @gs_input-salesorder.

      IF sy-subrc <> 0.         "check sales or creation

        lv_msg = |{ lv_msg } ; Order id and Sales order Mismatch - { gs_input-orderid } / { gs_input-salesorder } |.

      ELSE.

        SELECT SINGLE vbeln FROM zsd_scm_header
                      INTO lv_vbeln
                      WHERE order_id = gs_input-orderid AND vbeln = gs_input-salesorder.

        IF sy-subrc = 0.              "check delivery process

          lv_msg = |{ lv_msg } ; delivery already done - { gs_input-orderid } / { gs_input-salesorder }|.

        ELSE.

** fetch the sales order material ***

          SELECT posnr FROM vbap
                       INTO TABLE @DATA(lt_posnr)
                       WHERE vbeln = @gs_input-salesorder.

          LOOP AT lt_posnr ASSIGNING FIELD-SYMBOL(<fs_posnr>).

            APPEND VALUE bapisditm( itm_number = <fs_posnr>
                                    reason_rej = '07' ) TO lt_item.

            APPEND VALUE bapisditmx( itm_number = <fs_posnr>
                                     reason_rej = abap_true
                                     updateflag = 'U' ) TO lt_itemx.

          ENDLOOP.

          IF lt_item IS NOT INITIAL.

            ls_headerx-updateflag = 'U'.

**** CHANGE_SALESORDER_REJECT_REASONS ****

            CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
              EXPORTING
                salesdocument    = gs_input-salesorder
                order_header_inx = ls_headerx
              TABLES
                return           = lt_return
                order_item_in    = lt_item
                order_item_inx   = lt_itemx.

            READ TABLE lt_return INTO DATA(ls_return) WITH KEY type = 'E'.

            IF sy-subrc = 0.

              lv_msg = |{ lv_msg } ; { ls_return-message } |.

            ELSE.

              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                EXPORTING
                  wait = 'X'.

              APPEND VALUE #( orderid    = gs_input-orderid
                              salesorder = gs_input-salesorder
                              status     = 'S'
                              message    = |Sales order Closed successfully - { gs_input-orderid } / { gs_input-salesorder } | ) TO gt_response.

            ENDIF.

          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.


    IF lv_msg IS NOT INITIAL.

      APPEND VALUE #( orderid    = gs_input-orderid
                      salesorder = gs_input-salesorder
                      status     = 'E'
                      message    = | { lv_msg } | ) TO gt_response.
    ENDIF.

    IF gt_response IS NOT INITIAL.

** serialize the output for response ***
      /ui2/cl_json=>serialize(
      EXPORTING
       data         =  gt_response
       pretty_name  = /ui2/cl_json=>pretty_mode-user
      RECEIVING
       r_json         = lv_body ).

*Output Entry in Log Table
      CALL METHOD lo_log_upd->log_entry_store
        EXPORTING
          apiname         = 'SO_REJ_REASON'
          ijson           = lv_data
          ojson           = lv_body
        EXCEPTIONS
          apiname_missing = 1
          json_missing    = 2
          OTHERS          = 3.
      IF sy-subrc = 0.

      ENDIF.

      v_jsonload = | { '[' } { lv_body } { ']' } |.
*Set JSON Content-Type
      CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
      CALL METHOD server->response->set_cdata( data = v_jsonload ).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
