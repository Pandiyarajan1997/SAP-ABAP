class ZCL_API_OPEN_SKU_STO_DETAILS definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_OPEN_SKU_STO_DETAILS IMPLEMENTATION.


  METHOD if_http_extension~handle_request.
    "Created by: Samsudeen M
    "Created on: 24.01.2022
    "Purpose: Open Stock transfer Invoice or Purchase order details
    "Reference by: Suresh BV and Ramakrishnan J
*-------------------------------------------------------------------------------*
    TYPES: BEGIN OF ty_input,
             from_loc TYPE string,
             customer TYPE string,
           END OF ty_input.
    DATA: gw_input TYPE ty_input.

    TYPES: BEGIN OF ty_inv,
             vbeln   TYPE vbeln_vf,
             doctyp  TYPE fkart,
             bukrs   TYPE bukrs,
             fkdat   TYPE fkdat,
             kunag   TYPE kunnr,
             posnr   TYPE posnr_vf,
             fkimg   TYPE fkimg,
             vrkme   TYPE vrkme,
             ntgew   TYPE ntgew_15,
             brgew   TYPE brgew_15,
             matnr   TYPE matnr,
             arktx   TYPE arktx,
             cusname TYPE name1_gp,
           END OF ty_inv.
    DATA: gt_open_inv TYPE TABLE OF ty_inv.

    TYPES: BEGIN OF ty_output,
             from_loc  TYPE werks_d,
             f_name    TYPE name1_gp,
             customer  TYPE kunnr,
             cusname   TYPE name1_gp,
             inv_no    TYPE vbeln_vf,
             inv_item  TYPE posnr_vf,
             matnr     TYPE matnr,
             matdes    TYPE txz01,
             inv_qty   TYPE fkimg,
             netqty_kg TYPE ntgew_15,
             grsqty_kg TYPE brgew_15,
           END OF ty_output.
    DATA: gt_output TYPE TABLE OF ty_output.

    DATA: lr_fkart    TYPE RANGE OF vbrk-fkart,
          lr_customer TYPE RANGE OF vbrk-kunag.
    DATA: lv_from_plant TYPE werks_d,
          lv_customer   TYPE kunnr,
          lv_date       TYPE d,
          lv_fkdat      TYPE d.
    DATA:   lv_message TYPE string.
    DATA: lv_body TYPE string.
*Get Body data
    DATA: lv_data    TYPE string,
          v_jsonload TYPE string.
****API LOG Object

    DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
    CREATE OBJECT lo_log_upd.

    CLEAR lv_data.
    CALL METHOD server->request->get_cdata RECEIVING data = lv_data.

**Deserialize Converted JSON to Internal Table **
    /ui2/cl_json=>deserialize(
    EXPORTING
     json         = lv_data
     pretty_name  = /ui2/cl_json=>pretty_mode-user
    CHANGING
     data         = gw_input ). "Stock API Table

    IF gw_input IS INITIAL.
      CLEAR lv_message.
      lv_message = TEXT-001.

    ELSEIF gw_input-from_loc IS INITIAL.
      CLEAR lv_message.
      lv_message = TEXT-002.
    ENDIF.
*** From location plant Validation *****
    IF gw_input-from_loc IS NOT INITIAL.
      CLEAR lv_message.
      SELECT SINGLE werks FROM t001w INTO @DATA(lv_werks) WHERE werks = @gw_input-from_loc.
      IF sy-subrc NE 0.
        lv_message = TEXT-003.
      ENDIF.
    ENDIF.
*** Customer Number Validation *****
    IF gw_input-customer IS NOT INITIAL.
      CLEAR lv_message.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = gw_input-customer
        IMPORTING
          output = gw_input-customer.
      SELECT SINGLE kunnr FROM kna1 INTO @DATA(lv_kunnr) WHERE kunnr = @gw_input-customer.
      IF sy-subrc NE 0.
        lv_message = TEXT-004.
      ENDIF.
    ENDIF.
********* If Input has any errors it stops here *******
    IF lv_message IS NOT INITIAL.
      CLEAR lv_body.
      lv_body = |"Error": " { lv_message } "|.
      v_jsonload = lv_body.
*Set JSON Content-Type
      CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
      CALL METHOD server->response->set_cdata( data = v_jsonload ).
    ENDIF.
*---------- IF No errors in input then further Process starts ----------------------------*
    CHECK lv_message IS INITIAL.
    CLEAR: lv_from_plant,lv_customer,lv_date,lv_fkdat.
    "From Location
    lv_from_plant = gw_input-from_loc.
    lv_customer = lv_kunnr.
********** invoice date ***
    SELECT SINGLE low FROM tvarvc INTO @DATA(lv_inv_date) WHERE name = 'INVOICE_STO_API'
                                                          AND type = 'P'.
    IF sy-subrc = 0.
***** From Date convert to internal **************
      CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
        EXPORTING
          date_external            = lv_inv_date
        IMPORTING
          date_internal            = lv_inv_date
        EXCEPTIONS
          date_external_is_invalid = 1
          OTHERS                   = 2.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.
      SELECT low FROM tvarvc INTO TABLE @DATA(lt_tvarvc) WHERE name = 'INVOICE_STO_TYPE'
                                                         AND type = 'S'.
      IF sy-subrc = 0.
        LOOP AT lt_tvarvc INTO DATA(ls_tvarvc).
          APPEND VALUE #( sign = 'I'
                          option = 'EQ'
                          low = ls_tvarvc-low ) TO lr_fkart.
        ENDLOOP.
      ENDIF.
      " --------------- Customer Exclusion Maintained in TVARVC ------------------------------------*
      SELECT low FROM tvarvc INTO TABLE @DATA(lt_tvarvc1) WHERE name = 'INVOICE_CUST_EXCLUSION'
                                                          AND type = 'S'.
      IF sy-subrc = 0.
        CLEAR lr_customer.
        LOOP AT lt_tvarvc1 INTO DATA(ls_tvarvc1).
          APPEND VALUE #(  sign = 'I'
                           option = 'EQ'
                           low = ls_tvarvc1-low ) TO lr_customer.
        ENDLOOP.
      ENDIF.
*---------- Branch to SKU Transfer -----------------------------------------------------------*
      REFRESH:gt_open_inv.
**-------- Fetching Invoice Details for open PO ----------------------------------------*
      SELECT a~vbeln
             a~fkart AS doctyp
             a~bukrs
             a~fkdat
             a~kunag
             b~posnr
             b~fkimg
             b~vrkme
             b~ntgew
             b~brgew
             b~matnr
             b~arktx
             c~name1 AS cusname INTO CORRESPONDING FIELDS OF TABLE gt_open_inv
                                FROM vbrk AS a
                                INNER JOIN vbrp AS b ON a~vbeln = b~vbeln
                                INNER JOIN kna1 AS c ON a~kunag = c~kunnr
                                WHERE a~fksto NE 'X'
                                AND a~fkart IN lr_fkart
                                AND a~fktyp = 'L'
                                AND ( a~fkdat BETWEEN lv_inv_date AND sy-datum )
                                AND b~werks = lv_from_plant.
      IF sy-subrc = 0.
        SORT gt_open_inv[] BY vbeln posnr matnr ASCENDING.
        IF lr_customer IS NOT INITIAL.
          LOOP AT lr_customer INTO DATA(lw_cus).
            DELETE gt_open_inv WHERE kunag EQ lw_cus-low.
          ENDLOOP.
        ENDIF.
        IF lv_customer IS NOT INITIAL.
          DELETE gt_open_inv WHERE kunag NE lv_customer.
        ENDIF.
***** selecting from plant name for send in response ******
        SELECT SINGLE name1 INTO @DATA(lv_name) FROM t001w WHERE werks = @lv_from_plant.
        IF sy-subrc = 0.
        ENDIF.
*** selecting ebill number based on invoice *****
        SELECT * FROM j_1ig_ewaybill INTO TABLE @DATA(gt_ebill)
                                     FOR ALL ENTRIES IN @gt_open_inv
                                     WHERE bukrs = @gt_open_inv-bukrs
                                     AND doctyp = @gt_open_inv-doctyp
                                     AND docno = @gt_open_inv-vbeln
                                     AND status = 'A'.
        IF sy-subrc = 0.
          SORT gt_ebill[] BY docno.
        ENDIF.
**---- IRN Number for Invoice Fetching from J_1IG_INVREFNUM ------*
        SELECT bukrs,
               docno,
               doc_year,
               doc_type,
               irn  FROM j_1ig_invrefnum INTO TABLE @DATA(gt_irn)
                    FOR ALL ENTRIES IN @gt_open_inv
                    WHERE bukrs = @gt_open_inv-bukrs
                    AND doc_type = 'INV'
                    AND docno = @gt_open_inv-vbeln
                    AND irn_status = 'ACT'.
        IF sy-subrc = 0.
          SORT gt_irn[] BY docno.
        ENDIF.
      ELSE.
        CLEAR lv_body.
        lv_body = |No data for given customer input|.
        v_jsonload = | { '[' } { lv_body } { ']' } |.
*Set JSON Content-Type
        CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
        CALL METHOD server->response->set_cdata( data = v_jsonload ).
      ENDIF.
    ENDIF.
    REFRESH: gt_output.
    LOOP AT gt_open_inv ASSIGNING FIELD-SYMBOL(<fs_open_inv>).
      DATA(lv_irnno) = VALUE #( gt_irn[ bukrs = <fs_open_inv>-bukrs
                                        doc_type = 'INV'
                                        docno = <fs_open_inv>-vbeln ]-irn OPTIONAL ).
*                                        doc_year = <fs_open_inv>-fkdat+0(4) ]-irn OPTIONAL ).
      DATA(lv_ebillno) = VALUE #( gt_ebill[ bukrs = <fs_open_inv>-bukrs
                                            doctyp = <fs_open_inv>-doctyp
                                            docno = <fs_open_inv>-vbeln ]-ebillno OPTIONAL ).
*                                            gjahr = <fs_open_inv>-fkdat+0(4) ]-ebillno OPTIONAL ).
      IF lv_irnno IS NOT INITIAL AND lv_ebillno IS INITIAL.
        APPEND VALUE #( from_loc = lv_from_plant
                        f_name = lv_name
                        customer = <fs_open_inv>-kunag
                        cusname = <fs_open_inv>-cusname
                        inv_no = <fs_open_inv>-vbeln
                        inv_item = <fs_open_inv>-posnr
                        matnr = <fs_open_inv>-matnr
                        matdes =  <fs_open_inv>-arktx
                        inv_qty = <fs_open_inv>-fkimg
                        netqty_kg = <fs_open_inv>-ntgew
                        grsqty_kg = <fs_open_inv>-brgew  ) TO gt_output.
        CONTINUE.
      ENDIF.
      IF lv_ebillno IS INITIAL AND lv_irnno IS INITIAL.
        APPEND VALUE #( from_loc = lv_from_plant
                        f_name = lv_name
                        customer = <fs_open_inv>-kunag
                        cusname = <fs_open_inv>-cusname
                        inv_no = <fs_open_inv>-vbeln
                        inv_item = <fs_open_inv>-posnr
                        matnr = <fs_open_inv>-matnr
                        matdes =  <fs_open_inv>-arktx
                        inv_qty = <fs_open_inv>-fkimg
                        netqty_kg = <fs_open_inv>-ntgew
                        grsqty_kg = <fs_open_inv>-brgew  ) TO gt_output.
        CONTINUE.
      ENDIF.
    ENDLOOP.
    IF gt_output IS NOT INITIAL.
      CLEAR: lv_body,v_jsonload.
** serialize the output for response ***
      /ui2/cl_json=>serialize(
      EXPORTING
       data         =  gt_output
       pretty_name  = /ui2/cl_json=>pretty_mode-user
      RECEIVING
       r_json         = lv_body ).

********************changed by Pandiarajan on 25.10.2023*********************************
*Output Entry in Log Table
      CALL METHOD lo_log_upd->log_entry_store
        EXPORTING
          apiname         = 'OPEN_SKU_STO'
          ijson           = lv_data
          ojson           = lv_body
        EXCEPTIONS
          apiname_missing = 1
          json_missing    = 2
          OTHERS          = 3.

*****************************************************************************************

      v_jsonload = | { '[' } { lv_body } { ']' } |.
*Set JSON Content-Type
      CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
      CALL METHOD server->response->set_cdata( data = v_jsonload ).
    ELSE.
      CLEAR lv_body.
      lv_body = |No open invoice available for input|.
      v_jsonload = | { '[' } { lv_body } { ']' } |.
*Set JSON Content-Type
      CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
      CALL METHOD server->response->set_cdata( data = v_jsonload ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
