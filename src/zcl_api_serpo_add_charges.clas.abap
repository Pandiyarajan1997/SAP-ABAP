class ZCL_API_SERPO_ADD_CHARGES definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_SERPO_ADD_CHARGES IMPLEMENTATION.


  METHOD if_http_extension~handle_request.
*----------------------------------------------------------------------------------
*&Created by: Samsudeen M
*&Created On: 08.05.2023
*&Purpose : For Adding Additional Charges Service Lineitems in Service Purchase Order
*&Reference: Ramakrishnan J & Gopal raja
*-------------------------------------------------------------------------------------
    TYPES: BEGIN OF ty_input,
             ebeln   TYPE ebeln,
             ebelp   TYPE ebelp,
             addqty  TYPE menge_d,
             addchgs TYPE netwr,
             text    TYPE ktext1,
           END OF ty_input.

    TYPES: BEGIN OF ty_output,
             ebeln TYPE ebeln,
             ebelp TYPE ebelp,
             msg   TYPE string,
           END OF ty_output.

    DATA: gt_input TYPE TABLE OF ty_input,
          gs_input TYPE ty_input.
    DATA: gt_output TYPE TABLE OF ty_output.

    DATA: lv_data TYPE string,
          lv_msg  TYPE string.

    DATA: lv_body    TYPE string,
          v_jsonload TYPE string.
    DATA: lo_main TYPE REF TO zcl_scm_servicepo_to_invoice.
    DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
    CREATE OBJECT lo_log_upd.

    "Input of API Request
    CLEAR lv_data.
    CALL METHOD server->request->get_cdata RECEIVING data = lv_data.
    DATA(lv_data_tmp) = |[ { lv_data } ]|.
    CONDENSE lv_data_tmp NO-GAPS.
    SHIFT lv_data_tmp LEFT DELETING LEADING ''.

** Deserialize the input our required input ***
    /ui2/cl_json=>deserialize(
   EXPORTING
     json         = lv_data_tmp
     pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
   CHANGING
     data         = gt_input ).

    CREATE OBJECT lo_main.

    IF gt_input IS NOT INITIAL.
      DATA(l_podtls) = gt_input[ 1 ].
      IF l_podtls-ebeln IS INITIAL.
        lv_msg = |PUrchase Order Number Missing|.
      ELSEIF l_podtls-ebelp IS INITIAL.
        lv_msg = |Purchase Order Item Number Missing|.
      ELSEIF l_podtls-addchgs IS INITIAL.
        lv_msg = |Additional Charges is Missing|.
      ELSEIF l_podtls-addqty IS INITIAL.
        lv_msg = |Additional Quantity is Missing|.
      ELSE.
        SELECT SINGLE * FROM ekpo
           INTO @DATA(l_polineitem)
           WHERE ebeln = @l_podtls-ebeln
           AND ebelp = @l_podtls-ebelp.
        IF sy-subrc NE 0.
          lv_msg = |Kindly Check Purchase Order Details in Input|.
        ELSE.
          SELECT SINGLE * FROM zmm_esno_invoice
            INTO @DATA(l_logdtls)
            WHERE ebeln = @l_podtls-ebeln
            AND ebelp = @l_podtls-ebelp.
          IF sy-subrc = 0.
*Additional Charges ALready Done
            IF l_logdtls-addn_charges EQ abap_true.
              lv_msg = |Additional Charges Already added|.
            ENDIF.
*Entrysheet and Invoice ALready Done
            IF l_logdtls-ent_sno IS NOT INITIAL AND l_logdtls-belnr IS NOT INITIAL.
              lv_msg = |Entrysheet and Invoice Already Created|.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      REFRESH: gt_output.
      IF lv_msg IS INITIAL.
        CALL METHOD lo_main->po_change
          EXPORTING
            ebeln        = l_podtls-ebeln
            ebelp        = l_podtls-ebelp
            addn_qty     = l_podtls-addqty
            addn_charges = l_podtls-addchgs
            text         = l_podtls-text
          IMPORTING
            success      = DATA(lv_success)
            return       = DATA(lt_return).
        IF lv_success = abap_true.
          l_logdtls-ebeln = l_podtls-ebeln.
          l_logdtls-ebelp = l_podtls-ebelp.
          l_logdtls-addn_charges = lv_success.
          l_logdtls-erdat  = sy-datum.
          l_logdtls-erzet = sy-uzeit.
          MODIFY zmm_esno_invoice FROM l_logdtls.
          APPEND VALUE #( ebeln = l_podtls-ebeln
                          ebelp = l_podtls-ebelp
                          msg   = |PO Changed Successfully| ) TO gt_output.
        ELSE.
          CLEAR lv_msg.
          LOOP AT lt_return INTO DATA(lw_ret) WHERE type = 'E' OR type = 'A'.
            lv_msg = |{ lv_msg },{ lw_ret-message }|.
          ENDLOOP.
          l_logdtls-ebeln = l_podtls-ebeln.
          l_logdtls-ebelp = l_podtls-ebelp.
          l_logdtls-msg   = lv_msg.
          l_logdtls-erdat  = sy-datum.
          l_logdtls-erzet = sy-uzeit.
          l_logdtls-type = 'E'.
          MODIFY zmm_esno_invoice FROM l_logdtls.
          APPEND VALUE #( ebeln = l_podtls-ebeln
                          ebelp = l_podtls-ebelp
                          msg   = |{ lv_msg }| ) TO gt_output.
        ENDIF.
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
            apiname         = 'POADDCHARGES'
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

      ELSE.
        v_jsonload = | { '[' } { lv_msg } { ']' } |.

        CALL METHOD lo_log_upd->log_entry_store
          EXPORTING
            apiname         = 'POADDCHARGES'
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

    ENDIF.



  ENDMETHOD.
ENDCLASS.
