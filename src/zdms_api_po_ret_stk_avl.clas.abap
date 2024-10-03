CLASS zdms_api_po_ret_stk_avl DEFINITION
  PUBLIC
  FINAL.

  PUBLIC SECTION.
    INTERFACES if_http_extension.
    TYPES:BEGIN OF ty_input,
            Type         TYPE zde_type_stk,
            Invoice_no   TYPE vbeln,
            Invoice_date TYPE char10,
          END OF ty_input,
          BEGIN OF ty_output,
            Invoice_no  TYPE vbeln,
            Distributor TYPE kunnr,
            Customer    TYPE kunnr,
            Plant       TYPE werks_d,
            Material    TYPE matnr,
            Batch       TYPE charg_d,
            Qty         TYPE vbap-zmeng,
          END OF ty_output,
          tt_output TYPE TABLE OF ty_output WITH EMPTY KEY,
          BEGIN OF ty_erresp,
*        Invoice_no type vbeln,
            message TYPE string,
            type    TYPE char1,
          END OF ty_erresp,
          BEGIN OF ty_let,
            Invoice_no  TYPE vbeln,
            distributor TYPE kunnr,
          END OF ty_let.


    DATA: gs_input  TYPE ty_input,
          gs_output TYPE TABLE OF ty_output,
          gs_erresp TYPE ty_erresp.
    DATA: gs_vbrk TYPE vbrk,
          gs_kna1 TYPE kna1.
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS validate_input
      IMPORTING
        !gv_invoice  TYPE vbeln
        !GV_date     TYPE fkdat
        !type        TYPE zde_type_stk
      EXPORTING
        !gv_response TYPE ty_erresp
        !gs_kna1     TYPE kna1 .
    METHODS get_f4_data
      IMPORTING
        !iv_invoice      TYPE vbeln
        !gs_kna1         TYPE kna1
        !type            TYPE zde_type_stk
      EXPORTING
        VALUE(gt_output) TYPE tt_output .
ENDCLASS.



CLASS zdms_api_po_ret_stk_avl IMPLEMENTATION.


  METHOD if_http_extension~handle_request.
    DATA: lv_body TYPE string.
    DATA: lv_data     TYPE string,
          lv_response TYPE ty_erresp,
          gt_output   TYPE tt_output.
************API log**********
    DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
    CREATE OBJECT lo_log_upd.
************dms error log**********
    DATA: lo_errorlog_dms TYPE REF TO zcl_api_dms_error_log_entries.
    CREATE OBJECT lo_errorlog_dms.

    CALL METHOD server->request->get_cdata RECEIVING data = lv_data.

** Deserialize the input our required input ***
    /ui2/cl_json=>deserialize(
    EXPORTING
     json         = lv_data
     pretty_name  = /ui2/cl_json=>pretty_mode-user
     assoc_arrays = abap_true
    CHANGING
     data         = gs_input ).
***************input alpha conversion************
    DATA(inv_date) = |{ gs_input-invoice_date+0(4) }{  gs_input-invoice_date+5(2) }{  gs_input-invoice_date+8(2) }|.
    gs_input-invoice_no  = |{ gs_input-invoice_no ALPHA = IN }|.
***************Validate_inputs************
    me->validate_input(
      EXPORTING
        gv_invoice     = gs_input-invoice_no
        gv_date = CONV #( inv_date )
        type      = gs_input-type
      IMPORTING
        gv_response    = lv_response
        gs_kna1        = gs_kna1
    ).

    IF lv_response IS INITIAL.
      REFRESH: gt_output[].
      me->get_f4_data(
        EXPORTING
          iv_invoice = gs_input-invoice_no
          gs_kna1    = gs_kna1
          type       = gs_input-type
        IMPORTING
          gt_output  = gt_output
      ).
      SORT gt_output BY material batch.
    ELSE.
** serialize the output for response ***
      /ui2/cl_json=>serialize(
      EXPORTING
       data         =  lv_response
       pretty_name  = /ui2/cl_json=>pretty_mode-user
      RECEIVING
       r_json         = lv_body ).
*Output Entry in Log Table
      CALL METHOD lo_log_upd->log_entry_store
        EXPORTING
          apiname         = 'DMS_INV_STOCK_CHK'
          ijson           = lv_data
          ojson           = lv_body
*         distributor     = gs_input-distributor
*         retailer        = gs_input-dealer
        EXCEPTIONS
          apiname_missing = 1
          json_missing    = 2
          OTHERS          = 3.
*Set JSON Content-Type
      CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
      CALL METHOD server->response->set_cdata( data = lv_body ).
    ENDIF.
    IF gt_output IS NOT INITIAL.
** serialize the output for response ***
      /ui2/cl_json=>serialize(
      EXPORTING
       data         =  gt_output
       pretty_name  = /ui2/cl_json=>pretty_mode-user
      RECEIVING
       r_json         = lv_body ).
*Output Entry in Log Table
      CALL METHOD lo_log_upd->log_entry_store
        EXPORTING
          apiname         = 'DMS_INV_STOCK_CHK'
          ijson           = lv_data
          ojson           = lv_body
*         distributor     = gs_input-distributor
*         retailer        = gs_input-dealer
        EXCEPTIONS
          apiname_missing = 1
          json_missing    = 2
          OTHERS          = 3.
    ELSE.
      lv_response = VALUE #( message = 'No Data Found' type = 'E' ).
    ENDIF.
*Set JSON Content-Type
    CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
    CALL METHOD server->response->set_cdata( data = lv_body ).
  ENDMETHOD.


  METHOD validate_input.
    SELECT SINGLE FROM vbrk
    FIELDS *
    WHERE vbeln = @gv_invoice AND fkdat = @gv_date
    INTO @gs_vbrk.
    IF Sy-subrc NE  0.
      gv_response-message = 'Incorrect Invoice no given. Please check the Invoice no'.
      gv_response-type = 'E'.
      EXIT.
    ENDIF.

*        gv_response-invoice_no = gv_invoice.
    SELECT SINGLE FROM kna1
    FIELDS *
    WHERE kunnr = @gs_vbrk-kunag
    INTO @gs_kna1.
    IF sy-subrc NE 0 .
      gv_response-message = 'Customer in invoice is not available in customer master '.
      gv_response-type = 'E'.
      EXIT.
    ENDIF.

    IF type = 'PR'.
      IF gs_kna1-werks IS INITIAL.
        gv_response-message = |Plant is not available for Customer { gs_vbrk-kunag } in Invoice { gs_vbrk-vbeln }|.
        gv_response-type = 'E'.
        EXIT.
      ENDIF.
    ENDIF.


  ENDMETHOD.


  METHOD get_f4_data.
    CASE type .
      WHEN 'PR'.
*------------VBRP invoice material select query--------------*
        SELECT FROM vbrp AS a
        INNER JOIN kna1 AS b
        ON b~kunnr = a~kunag_ana
        FIELDS a~vbeln,a~posnr,b~kunnr,a~matnr AS old_matnr,b~werks, a~matnr AS new_matnr
        WHERE a~vbeln = @iv_invoice AND b~werks = @gs_kna1-werks
        INTO TABLE @DATA(gt_vbrp).
        IF sy-subrc = 0.
          SELECT
           FROM mara
           FIELDS matnr ,mtart, bismt , lvorm
           FOR ALL ENTRIES IN @gt_vbrp
           WHERE matnr = @gt_vbrp-old_matnr INTO TABLE @DATA(gt_mara_new).
          IF sy-subrc = 0.
            LOOP AT gt_vbrp ASSIGNING FIELD-SYMBOL(<fs_data>).
              IF <fs_data>-old_matnr+0(1) = 'Z'.
                <fs_data>-new_matnr = <fs_data>-old_matnr.
              ELSE.
                READ TABLE gt_mara_new INTO DATA(gs_mara) WITH KEY matnr = <fs_data>-old_matnr.
                IF sy-subrc = 0 AND gs_mara-mtart = 'HAWA'.
                  <fs_data>-new_matnr = gs_mara-matnr.
                  CONTINUE.
                ELSE.
                  <fs_data>-new_matnr = gs_mara-bismt.
                ENDIF.
              ENDIF.
            ENDLOOP.
*------------MCHB Batch select query--------------*
            SELECT FROM mchb FIELDS  matnr, werks, clabs,charg
            FOR ALL ENTRIES IN @gt_vbrp
          WHERE matnr = @gt_vbrp-new_matnr AND werks = @gs_kna1-werks AND clabs <> 0
          INTO TABLE @DATA(gt_mchb).
            IF sy-subrc = 0.
              LOOP AT gt_mchb INTO DATA(gs_mchb) .
                READ TABLE gt_vbrp INTO DATA(gs_vbrp) WITH KEY new_matnr = gs_mchb-matnr.
                IF Sy-subrc = 0.
                  APPEND VALUE #(  invoice_no = gs_vbrp-vbeln
                                  distributor = gs_vbrp-kunnr
                                   material =  gs_vbrp-old_matnr
                                   plant = gs_mchb-werks
                                   batch = gs_mchb-charg
                                   qty = gs_mchb-clabs ) TO gt_output.
                ENDIF.

              ENDLOOP.
            ENDIF.
          ENDIF.
        ENDIF.
      WHEN 'SR'.
        SELECT FROM zi_dms_vbrk_cv
        FIELDS vbeln,kunnr AS customer,matnr AS material
        WHERE  vbeln = @iv_invoice AND werks = @gs_kna1-werks
        INTO TABLE @DATA(gt_stock_sr_data).

        select from vbfa as a
        INNER JOIN lips as b
        on b~vbeln = a~vbelv
        INNER JOIN likp as c
        on c~vbeln = b~vbeln
       FIELDS a~vbeln,
        c~kunnr as customer,b~werks as plant,b~matnr as material,
        b~charg as batch,b~lfimg as qty
        FOR ALL ENTRIES IN @gt_stock_sr_data
        WHERE a~vbeln = @gt_stock_sr_data-vbeln
               and a~vbtyp_n = 'M'
                and a~vbtyp_v = 'J'
                and b~charg <> ' '
        INTO TABLE @DATA(gt_stock_data).

       READ TABLE gt_stock_data INTO DATA(gs_data) WITH KEY  vbeln = iv_invoice .
       If sy-subrc = 0 and gs_data-plant is not INITIAL.
       data(dist_plant) = VALUE werks_d(  ).
          SELECT SINGLE FROM kna1 FIELDS kunnr WHERE werks = @dist_plant INTO @DATA(dist).
       ENDIF.
*        DATA(dist_plant) = VALUE #( gt_stock_data[ vbeln = iv_invoice ]-plant OPTIONAL ).
*        IF dist_plant IS NOT INITIAL.
*          SELECT SINGLE FROM kna1 FIELDS kunnr WHERE werks = @dist_plant INTO @DATA(dist).
*        ENDIF.
        gt_output = VALUE #( FOR <ls_stk> IN gt_stock_data
                           LET ls_data = VALUE ty_let( invoice_no = iv_invoice distributor = dist  )
                           IN ( CORRESPONDING #( BASE ( ls_data ) <ls_stk> ) ) ).
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
