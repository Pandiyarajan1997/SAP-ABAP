class ZCL_ASN_INTERNAL_TEST_API definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ASN_INTERNAL_TEST_API IMPLEMENTATION.


  METHOD if_http_extension~handle_request.

    TYPES: BEGIN OF ty_input,
             distributor TYPE string,
           END OF ty_input.
    DATA: gs_input TYPE ty_input.

    TYPES: BEGIN OF ty_dist_stk,
             distributor TYPE string.
    TYPES: lt_sub_stk TYPE zdist_stock_tt.
    TYPES END OF ty_dist_stk.

    DATA: gt_output TYPE TABLE OF ty_dist_stk,
          gs_output TYPE ty_dist_stk.

    CALL METHOD server->request->get_cdata RECEIVING data = DATA(lv_data).

    /ui2/cl_json=>deserialize(
    EXPORTING
     json         = lv_data
     pretty_name  = /ui2/cl_json=>pretty_mode-user
     assoc_arrays = abap_true
    CHANGING
     data         = gs_input ).
    DATA(l_kunnr) = |{ gs_input-distributor ALPHA = IN }|.
    SELECT a~vbeln,
           a~fkdat,
           a~kunag,
           b~posnr,
           b~fkimg,
           b~matnr INTO TABLE @DATA(lt_invoice) UP TO 15 ROWS
                   FROM vbrk AS a INNER JOIN
                   vbrp AS b ON a~vbeln = b~vbeln
                   WHERE kunag = @l_kunnr
                   AND fksto NE 'X'.
    IF sy-subrc = 0.
      SORT lt_invoice[] BY vbeln posnr.
    ENDIF.
    LOOP AT lt_invoice ASSIGNING FIELD-SYMBOL(<fs_invoice>).
      APPEND VALUE #( invoice_no = <fs_invoice>-vbeln
                      invoice_date = <fs_invoice>-fkdat
                      material = <fs_invoice>-matnr
                      qty = <fs_invoice>-fkimg ) TO gs_output-lt_sub_stk.
    ENDLOOP.

    gs_output-distributor = l_kunnr.
    APPEND gs_output TO gt_output.

    DATA: lv_body TYPE string.
    DATA: v_jsonload TYPE string.

    IF gt_output[] IS NOT INITIAL.
      DATA: lw_test TYPE zst_scm_sales_invoice.
** serialize the output for response ***
      /ui2/cl_json=>serialize(
      EXPORTING
       data         =  lw_test
       pretty_name  = /ui2/cl_json=>pretty_mode-user
      RECEIVING
       r_json         = lv_body ).
    ENDIF.

    v_jsonload = lv_body.
*Set JSON Content-Type
    CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
    CALL METHOD server->response->set_cdata( data = v_jsonload ).
  ENDMETHOD.
ENDCLASS.
