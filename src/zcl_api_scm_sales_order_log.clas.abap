class ZCL_API_SCM_SALES_ORDER_LOG definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_SCM_SALES_ORDER_LOG IMPLEMENTATION.


  METHOD if_http_extension~handle_request.
*----------------------------------------------------------------------------------
*&Created by: Samsudeen M
*&Created On: 12.04.2023
*&Purpose : For Filling Log table of SCM Sales order to Invoice Process
*&Reference: Ramakrishnan J & Praveen Kumar & Gopal raja
*-------------------------------------------------------------------------------------
    TYPES: BEGIN OF ty_input,
             orderid  TYPE zorder_id,
             salesdoc TYPE vbeln_va,
             process  TYPE char01,
           END OF ty_input.

    DATA: gt_input  TYPE TABLE OF ty_input,
          gt_update TYPE TABLE OF ty_input,
          gs_input  TYPE ty_input.

    DATA: lt_header TYPE STANDARD TABLE OF zsd_scm_header,
          lw_header TYPE zsd_scm_header.

    TYPES: BEGIN OF output,
             order_id TYPE zorder_id,
             sales_no TYPE vbeln_va,
             type     TYPE bapi_mtype,
             msg      TYPE string,
           END OF output.
    DATA: gt_output TYPE TABLE OF output.

    DATA: lv_data TYPE string.
    DATA: lr_data TYPE REF TO data.

    DATA: lv_body    TYPE string,
          v_jsonload TYPE string.

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

    IF gt_input IS NOT INITIAL.
      LOOP AT gt_input ASSIGNING FIELD-SYMBOL(<fs_input>).
        CASE <fs_input>-process.
*Delivery Process Initiated
          WHEN 'D'.
            "Orderid Checks
            SELECT SINGLE * FROM zsd_scm_header INTO @DATA(l_orderid) WHERE order_id = @<fs_input>-orderid.
            IF sy-subrc <> 0.
              CLEAR lw_header.
              lw_header-mandt = sy-mandt.
              lw_header-order_id = <fs_input>-orderid.
              "Sales Order Number checks
              DATA(l_vbeln) = |{ <fs_input>-salesdoc  ALPHA = IN }|.
              SELECT SINGLE vbeln FROM vbak INTO @DATA(l_salesno) WHERE vbeln = @l_vbeln.
              IF sy-subrc NE 0.
                APPEND VALUE #( order_id = <fs_input>-orderid
                                sales_no = l_vbeln
                                type     = 'E'
                                msg      = TEXT-002 ) TO gt_output.
                CONTINUE.
              ENDIF.
              lw_header-vbeln      = l_salesno.
              lw_header-erdat      = sy-datum.
              lw_header-ernam      = sy-uname.
              lw_header-entry_time = sy-uzeit.
              lw_header-dflag      = abap_true.
              lw_header-status     = '11'.
              lw_header-msgtyp     = 'S'.
              MODIFY zsd_scm_header FROM lw_header.
              APPEND VALUE #( order_id = <fs_input>-orderid
                              sales_no = l_vbeln
                              type     = 'S'
                              msg      = TEXT-003 ) TO gt_output.
            ELSE.
              APPEND VALUE #( order_id = <fs_input>-orderid
                              sales_no = l_vbeln
                              type     = 'E'
                              msg      = TEXT-004 ) TO gt_output.
            ENDIF.
*Invoice Process Initiated Means
          WHEN 'I'.
            "Orderid Checks
            SELECT SINGLE * FROM zsd_scm_header INTO @l_orderid WHERE order_id = @<fs_input>-orderid.
            IF sy-subrc = 0.
              IF l_orderid-icomp = abap_true.
                APPEND VALUE #( order_id = <fs_input>-orderid
                                sales_no = l_orderid-vbeln
                                type     = 'S'
                                msg      = TEXT-006 ) TO gt_output.
                CONTINUE.
              ENDIF.
              IF l_orderid-dcomp = abap_true AND l_orderid-iflag = space.
                l_orderid-iflag = abap_true.
                l_orderid-status = '14'.
                MODIFY zsd_scm_header FROM l_orderid.
                APPEND VALUE #( order_id = <fs_input>-orderid
                                sales_no = l_vbeln
                                type     = 'S'
                                msg      = TEXT-003 ) TO gt_output.
              ELSE.
                APPEND VALUE #( order_id = <fs_input>-orderid
                                sales_no = l_vbeln
                                type     = 'E'
                                msg      = TEXT-005 ) TO gt_output.
              ENDIF.
            ELSE.
              APPEND VALUE #( order_id = <fs_input>-orderid
                              sales_no = l_vbeln
                              type     = 'E'
                              msg      = TEXT-007 ) TO gt_output.
            ENDIF.
          WHEN OTHERS.
            APPEND VALUE #( order_id = <fs_input>-orderid
                            sales_no = l_vbeln
                            type     = 'E'
                            msg      = TEXT-001 ) TO gt_output.
            CONTINUE.
        ENDCASE.
      ENDLOOP.
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

    ENDIF.

  ENDMETHOD.
ENDCLASS.
