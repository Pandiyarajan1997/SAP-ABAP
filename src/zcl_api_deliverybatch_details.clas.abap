class ZCL_API_DELIVERYBATCH_DETAILS definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_DELIVERYBATCH_DETAILS IMPLEMENTATION.


  METHOD if_http_extension~handle_request.
*----------------------------------------------------------------------------------
*&Created by: Samsudeen M
*&Created On: 24.05.2023
*&Purpose : For Sending the Batch Details of outbound Delivery
*&Reference: Ramakrishnan J
*-------------------------------------------------------------------------------------
    TYPES: BEGIN OF ty_input,
             deliveryno TYPE vbeln_vl,
           END OF ty_input.

    TYPES: BEGIN OF ty_output,
             deliveryno TYPE vbeln_vl,
             plant      TYPE werks_d,
             material   TYPE matnr,
             stloc      TYPE lgort_d,
             batch      TYPE charg_d,
             delqty     TYPE lfimg,
             type       TYPE bapi_mtype,
             msg        TYPE string,
           END OF ty_output.

    DATA: gt_input TYPE TABLE OF ty_input,
          gs_input TYPE ty_input.
    DATA: gt_output TYPE TABLE OF ty_output.

    DATA: lv_body    TYPE string,
          v_jsonload TYPE string.

    DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
    CREATE OBJECT lo_log_upd.

    "Input of API Request
    CALL METHOD server->request->get_cdata RECEIVING data = DATA(lv_data).

    CALL METHOD lo_log_upd->log_entry_store
      EXPORTING
        apiname         = 'DELDTLS_IN'
        json            = lv_data
      EXCEPTIONS
        apiname_missing = 1
        json_missing    = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.

    ENDIF.

    DATA(lv_data_tmp) = |[ { lv_data } ]|.
    CONDENSE lv_data_tmp NO-GAPS.

** Deserialize the input our required input ***
    /ui2/cl_json=>deserialize(
   EXPORTING
     json         = lv_data_tmp
     pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
   CHANGING
     data         = gt_input ).

    IF gt_input IS NOT INITIAL.
      LOOP AT gt_input ASSIGNING FIELD-SYMBOL(<fs_input>).
        IF <fs_input>-deliveryno IS INITIAL.
          APPEND VALUE #( deliveryno   = <fs_input>-deliveryno
                          type     = 'E'
                          msg      = |Deliveryno ID is Missing| ) TO gt_output.
        ELSE.
          SELECT SINGLE vbeln FROM lips
            INTO @DATA(l_deliveryno)
            WHERE vbeln = @<fs_input>-deliveryno.
          IF sy-subrc NE 0.
            APPEND VALUE #( deliveryno   = <fs_input>-deliveryno
                            type         = 'E'
                            msg          = |Incorrect Outbound Delivery Number| ) TO gt_output.
          ELSE.
*Outbound Delivery Details
            SELECT  vbeln,
                    posnr,
                    matnr,
                    werks,
                    lgort,
                    charg,
                    lfimg,
                    meins,
                    arktx,
                    vgbel,
                    vgpos,
                    bwart
              INTO TABLE @DATA(gt_deliv_item)
              FROM lips
              WHERE vbeln = @<fs_input>-deliveryno AND
              bwart = '601' AND "Movement type
              lgort NE @space AND
              charg NE @space AND
              lfimg NE 0.
            IF sy-subrc EQ 0.
              LOOP AT gt_deliv_item ASSIGNING FIELD-SYMBOL(<fs_del_item>).
                APPEND VALUE #( deliveryno = <fs_del_item>-vbeln
                                plant      = <fs_del_item>-werks
                                material   = <fs_del_item>-matnr
                                stloc      = <fs_del_item>-lgort
                                batch      = <fs_del_item>-charg
                                delqty     = <fs_del_item>-lfimg
                                type       = 'S' ) TO gt_output.
              ENDLOOP.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
    CLEAR: lv_body,v_jsonload.
** serialize the output for response ***
    /ui2/cl_json=>serialize(
    EXPORTING
     data         =  gt_output
     pretty_name  = /ui2/cl_json=>pretty_mode-user
    RECEIVING
     r_json         = lv_body ).

    v_jsonload = | { '[' } { lv_body } { ']' } |.

    CALL METHOD lo_log_upd->log_entry_store
      EXPORTING
        apiname         = 'DELDTLS_OT'
        json            = v_jsonload
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


  ENDMETHOD.
ENDCLASS.
