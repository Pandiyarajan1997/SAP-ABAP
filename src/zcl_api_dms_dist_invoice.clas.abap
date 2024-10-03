class ZCL_API_DMS_DIST_INVOICE definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_DMS_DIST_INVOICE IMPLEMENTATION.


  METHOD if_http_extension~handle_request.

    TYPES: BEGIN OF body,
             customer TYPE kunnr,
           END OF body.
    DATA: gt_input TYPE TABLE OF body.
    DATA: gt_input_tmp TYPE TABLE OF body.
    DATA: lv_message TYPE string.
    DATA :ls_json    TYPE string,
          v_jsonload TYPE string.

    DATA: it_inputparams TYPE tihttpnvp.
    DATA: lr_data TYPE REF TO data.
    DATA: lr_customer TYPE /isdfps/rg_t_kunnr.
    DATA: gt_response TYPE STANDARD TABLE OF zdist_inv_api_resp,
          gs_response TYPE zdist_inv_api_resp.
    DATA: lv_batch TYPE string.

    DATA: lt_vbrk  TYPE TABLE OF zstr_bill,
          lt_vbrp  TYPE TABLE OF zstr_bill_item_app,
          lt_batch TYPE TABLE OF zstr_lips_bno.
*** Field Symbols for splitting input ***
    FIELD-SYMBOLS:
      <data>        TYPE data,
      <results>     TYPE data,
      <structure>   TYPE any,
      <table>       TYPE ANY TABLE,
      <field>       TYPE any,
      <field_value> TYPE data.


* get the request attributes
    REFRESH: it_inputparams.
    CALL METHOD server->request->get_header_fields
      CHANGING
        fields = it_inputparams.               " Header fields.

    LOOP AT it_inputparams ASSIGNING FIELD-SYMBOL(<fs_params>).
      TRANSLATE <fs_params>-name TO UPPER CASE.
    ENDLOOP.

    CALL METHOD server->request->get_cdata RECEIVING data = DATA(lv_data).

    IF lv_data IS NOT INITIAL.

      REPLACE ALL OCCURRENCES OF '['  IN lv_data WITH ''.
      REPLACE ALL OCCURRENCES OF ']'  IN lv_data WITH ''.

      lv_data = | { '{' } { '"Results":' } { '[ ' } { lv_data } { ']' } { '}' } |.
      REPLACE ALL OCCURRENCES OF '##'  IN lv_data WITH ''.
      CONDENSE lv_data NO-GAPS.
      SHIFT lv_data LEFT DELETING LEADING ''.

** deserialize the INPUT our required INPUT ***
      /ui2/cl_json=>deserialize(
      EXPORTING
       json         = lv_data
       pretty_name  = /ui2/cl_json=>pretty_mode-user
      CHANGING
       data         = lr_data ).

      IF lr_data IS BOUND.

        ASSIGN lr_data->* TO <data>.
        ASSIGN COMPONENT `RESULTS` OF STRUCTURE <data> TO <results>.
        ASSIGN <results>->* TO <table>.
        REFRESH gt_input.
        LOOP AT <table> ASSIGNING <structure>.
          ASSIGN <structure>->* TO <data>.
          ASSIGN COMPONENT 'CUSTOMER' OF STRUCTURE <data>  TO <field>.
          IF <field> IS ASSIGNED.
            lr_data = <field>.
            ASSIGN lr_data->* TO <field_value>.
            APPEND VALUE #( customer = <field_value> ) TO gt_input.
            UNASSIGN: <field>, <field_value>.
          ENDIF.
        ENDLOOP.
      ENDIF.

    ENDIF.
*-------- Parameters from API ----------*
    DATA(date_frm) = CONV fkdat( VALUE #( it_inputparams[ name = 'DATE_FRM' ]-value OPTIONAL ) ).
    DATA(date_to) =  CONV fkdat( VALUE #( it_inputparams[ name = 'DATE_TO' ]-value OPTIONAL ) ).


    IF date_frm IS INITIAL AND date_to IS INITIAL.
      CLEAR lv_message.
      lv_message = |Date From and Date to is Mandatory|.
    ENDIF.

    IF lv_message IS NOT INITIAL.
      v_jsonload = |[{ lv_message } ]|.
*Set JSON Content-Type
      CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
      CALL METHOD server->response->set_cdata( data = v_jsonload ).
    ELSE.
      IF date_frm IS NOT INITIAL AND date_to IS NOT INITIAL.
        IF gt_input IS NOT INITIAL.
          DELETE ADJACENT DUPLICATES FROM gt_input COMPARING customer.
          REFRESH lr_customer.
          LOOP AT gt_input ASSIGNING FIELD-SYMBOL(<fs_input>).
            DATA(lv_customer) = | { <fs_input>-customer ALPHA = IN }|.
            CONDENSE lv_customer NO-GAPS.
            SELECT SINGLE kunnr FROM kna1 INTO @DATA(lv_kunnr) WHERE kunnr = @lv_customer.
            IF sy-subrc NE 0.
              APPEND VALUE #( customer = <fs_input>-customer ) TO gt_input_tmp.
              DELETE gt_input WHERE customer = <fs_input>-customer.
            ELSE.
              APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_input>-customer ) TO lr_customer.
            ENDIF.
          ENDLOOP.
        ENDIF.
*------- Function Module to Get Distributor Invoices --------*
        REFRESH: lt_vbrk,lt_vbrp,lt_batch.
        CALL FUNCTION 'ZGET_DISTRIBUTOR_INVOICES'
          EXPORTING
            customer   = lr_customer
            begda      = date_frm
            endda      = date_to
          TABLES
            it_vbrk    = lt_vbrk
            it_vbrp    = lt_vbrp
            it_btno    = lt_batch
          EXCEPTIONS
            input_date = 1
            OTHERS     = 2.
        IF sy-subrc = 0.
          LOOP AT lt_vbrk ASSIGNING FIELD-SYMBOL(<fs_vbrk>).

            CLEAR gs_response.
            gs_response-customer = <fs_vbrk>-kunag.

            SELECT SINGLE name1 FROM kna1 INTO gs_response-cusname WHERE kunnr = <fs_vbrk>-kunag.
            IF sy-subrc = 0.

            ENDIF.
            MOVE-CORRESPONDING <fs_vbrk> TO gs_response-header. "Invoice Header Details

            IF gs_response-header-fkart = 'YBRE'.
              SELECT SINGLE xblnr INTO @gs_response-header-xblnr FROM vbrk WHERE vbeln = @<fs_vbrk>-vbeln.
              IF sy-subrc = 0.

              ENDIF.
            ENDIF.

            LOOP AT lt_vbrp ASSIGNING FIELD-SYMBOL(<fs_vbrp>) WHERE vbeln = <fs_vbrk>-vbeln. "Invoice Item Details
              LOOP AT lt_batch ASSIGNING FIELD-SYMBOL(<fs_batch>) WHERE vbeln = <fs_vbrk>-vbeln AND posnr = <fs_vbrp>-posnr . "Invoice Delivery Details
                lv_batch = |{ lv_batch } / { <fs_batch>-charg }|.
              ENDLOOP.
              CONDENSE lv_batch.
              SHIFT lv_batch LEFT DELETING LEADING '/'.
              APPEND VALUE #( vbeln = <fs_vbrp>-vbeln
                              posnr = <fs_vbrp>-posnr
                              matnr = <fs_vbrp>-matnr
                              arktx = <fs_vbrp>-arktx
                              fkimg = <fs_vbrp>-fkimg
                              vrkme = <fs_vbrp>-vrkme
                              netwr = <fs_vbrp>-netwr
                              mwsbp = <fs_vbrp>-mwsbp
                              batch = lv_batch
                              meins = <fs_vbrp>-meins
                              cgst = <fs_vbrp>-cgst
                              sgst = <fs_vbrp>-sgst
                              igst = <fs_vbrp>-igst
                           ) TO gs_response-item.

            ENDLOOP.
            APPEND gs_response TO gt_response.
            CLEAR lv_batch.
          ENDLOOP.
          SORT gt_response[] by customer.
        ENDIF.
        IF gt_response IS NOT INITIAL.
          CLEAR:v_jsonload.
** serialize the output for response ***
          /ui2/cl_json=>serialize(
          EXPORTING
           data         =  gt_response
           pretty_name  = /ui2/cl_json=>pretty_mode-user
          RECEIVING
           r_json         = DATA(lv_body) ).
        ENDIF.
      ENDIF.
      v_jsonload = | { '[' } { lv_body } { ']' } |.
*Set JSON Content-Type
      CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
      CALL METHOD server->response->set_cdata( data = v_jsonload ).
    ENDIF.


  ENDMETHOD.
ENDCLASS.
