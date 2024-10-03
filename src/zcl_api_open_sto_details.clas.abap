class ZCL_API_OPEN_STO_DETAILS definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_OPEN_STO_DETAILS IMPLEMENTATION.


  METHOD if_http_extension~handle_request.
    "Created by: Samsudeen M
    "Created on: 24.01.2022
    "Purpose: Open Stock transfer Invoice or Purchase order details
    "Reference by: Suresh BV and Ramakrishnan J
*-------------------------------------------------------------------------------*
    TYPES: BEGIN OF ty_input,
             from_loc TYPE string,
             to_loc   TYPE string,
           END OF ty_input.
    DATA: gw_input TYPE ty_input.

    TYPES: BEGIN OF ty_po,
             ebeln     TYPE ebeln,
             ebelp     TYPE ebelp,
             bsart     TYPE bsart,
             matnr     TYPE matnr,
             txz01     TYPE txz01,
             werks     TYPE werks_d,
             po_qty    TYPE bstmg,
             po_uom    TYPE bstme,
             net_vol   TYPE entge,
             gross_vol TYPE brgew,
           END OF ty_po.
    DATA: gt_open_po TYPE TABLE OF ty_po.

    TYPES: BEGIN OF ty_inv,
             inv_no    TYPE vbeln_vf,
             kunag     TYPE kunnr,
             posnr     TYPE posnr_vf,
             fkimg     TYPE fkimg,
             vrkme     TYPE vrkme,
             netqty_kg TYPE ntgew_15,
             grsqty_kg TYPE brgew_15,
             aubel     TYPE vbeln_va,
             aupos     TYPE posnr_vf,
             material  TYPE matnr,
             arktx     TYPE arktx,
             batch     TYPE charg_d,
             werks     TYPE werks_d,
             bukrs     TYPE bukrs,
             doctyp    TYPE fkart,
             fkdat     TYPE fkdat,
             to_plant  TYPE werks_d,
           END OF ty_inv.
    DATA: gt_open_inv TYPE TABLE OF ty_inv.

    TYPES: BEGIN OF ty_output,
             from_plant         TYPE werks_d,
             f_name             TYPE name1_gp,
             to_plant           TYPE werks_d,
             t_name             TYPE name1_gp,
             doc_no             TYPE vbeln_vf,
             item               TYPE posnr_vf,
             matnr              TYPE matnr,
             matdes             TYPE txz01,
             qty                TYPE fkimg,
             netqty_kg          TYPE bstmg,
             grsqty_kg          TYPE bstmg,
             identification(20) TYPE c,
           END OF ty_output.
    DATA: gt_output TYPE TABLE OF ty_output.

    DATA: lv_from_plant TYPE werks_d,
          lv_to_plant   TYPE werks_d,
          lv_customer   TYPE kunnr,
          lv_date       TYPE d,
          lv_fkdat      TYPE d.
    DATA: lv_message TYPE string.
    DATA: lv_body TYPE string.
    DATA: lr_werks TYPE RANGE OF t001w-werks.

    DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
    CREATE OBJECT lo_log_upd.
*Get Body data
    DATA: lv_data    TYPE string,
          v_jsonload TYPE string.
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
    "From Location
    lv_from_plant =  gw_input-from_loc.
    lv_to_plant =  gw_input-to_loc.
*** From location plant Validation *****
    IF lv_from_plant IS NOT INITIAL.
      CLEAR lv_message.
      SELECT SINGLE werks FROM t001w INTO @DATA(lv_werks) WHERE werks = @lv_from_plant.
      IF sy-subrc NE 0.
        lv_message = TEXT-003.
      ENDIF.
    ENDIF.
**** To location plant Validation *****
*    IF gw_input-to_loc IS NOT INITIAL.
*      CLEAR lv_message.
*      SELECT SINGLE werks FROM t001w INTO @DATA(lv_werks1) WHERE werks = @lv_to_plant.
*      IF sy-subrc NE 0.
*        lv_message = TEXT-004.
*      ENDIF.
*    ENDIF.
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
    CLEAR: lv_customer,lr_werks.

*---------- Branch to Branch Transfer -----------------------------------------------------------*
    REFRESH: gt_open_inv,gt_open_po.
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
    ENDIF.
    SELECT vbeln,
           fkart,
           fkdat,
           bukrs,
           kunag FROM vbrk INTO TABLE @DATA(gt_open_hdr)
                 WHERE fkart = 'YSTO'
                 AND ( fkdat BETWEEN @lv_inv_date AND @sy-datum )
                 AND fksto NE 'X'.
    IF sy-subrc = 0.
      SORT gt_open_hdr[] BY vbeln.
      SELECT vbeln,
             posnr,
             fkimg,
             vrkme,
             ntgew,
             brgew,
             aubel,
             aupos,
             matnr,
             arktx,
             charg,
             werks FROM vbrp INTO TABLE @DATA(gt_open_itm)
                   FOR ALL ENTRIES IN @gt_open_hdr
                   WHERE vbeln = @gt_open_hdr-vbeln
                   AND werks = @lv_from_plant.
      IF sy-subrc = 0.
        SORT gt_open_itm[] BY vbeln posnr.
      ENDIF.
      SELECT werks, name1, kunnr FROM t001w INTO TABLE @DATA(gt_pname).
      IF sy-subrc = 0.
        SORT gt_pname[] BY werks.
      ENDIF.


*** selecting ebill number based ON invoice *****
      SELECT * FROM j_1ig_ewaybill INTO TABLE @DATA(gt_ebill)
                                   FOR ALL ENTRIES IN @gt_open_hdr
                                   WHERE bukrs = @gt_open_hdr-bukrs
                                   AND doctyp = @gt_open_hdr-fkart
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
                  FOR ALL ENTRIES IN @gt_open_hdr
                  WHERE bukrs = @gt_open_hdr-bukrs
                  AND doc_type = 'INV'
                  AND docno = @gt_open_hdr-vbeln
                  AND irn_status = 'ACT'.
      IF sy-subrc = 0.
        SORT gt_irn[] BY docno.
      ENDIF.

    ENDIF.
*** Purchase Order Details  ****
    SELECT a~ebeln
           a~ebelp
           a~matnr
           a~txz01
           a~werks
           a~menge AS po_qty
           a~meins AS po_uom
           a~ntgew AS net_vol
           a~brgew AS gross_vol
           b~bsart
                   INTO CORRESPONDING FIELDS OF TABLE gt_open_po
                   FROM ekpo AS a
                   INNER JOIN ekko AS b ON a~ebeln = b~ebeln
                   WHERE a~werks = lv_to_plant
                   AND a~elikz NE 'X'
                   AND b~bsart EQ 'UB'
                   AND b~bstyp = 'F'
                   AND ( b~aedat BETWEEN lv_inv_date AND lv_fkdat )
                   AND b~reswk = lv_from_plant.
    IF sy-subrc = 0.
      SORT gt_open_po[] BY ebeln ebelp matnr.
    ENDIF.
    REFRESH: gt_output.
    LOOP AT gt_open_hdr ASSIGNING FIELD-SYMBOL(<fs_open_inv>).
      DATA(lv_irnno) = VALUE #( gt_irn[ bukrs = <fs_open_inv>-bukrs
                                        doc_type = 'INV'
                                        docno = <fs_open_inv>-vbeln
                                        doc_year = <fs_open_inv>-fkdat+0(4) ]-irn OPTIONAL ).
      DATA(lv_ebillno) = VALUE #( gt_ebill[ bukrs = <fs_open_inv>-bukrs
                                            doctyp = <fs_open_inv>-fkart
                                            docno = <fs_open_inv>-vbeln
                                            gjahr = <fs_open_inv>-fkdat+0(4) ]-ebillno OPTIONAL ).

      DATA(lv_tplant) = VALUE #( gt_pname[ kunnr = <fs_open_inv>-kunag ]-werks OPTIONAL ).

      IF lv_irnno IS NOT INITIAL AND lv_ebillno IS INITIAL.
        LOOP AT gt_open_itm ASSIGNING FIELD-SYMBOL(<fs_open_item>) WHERE vbeln = <fs_open_inv>-vbeln
                                                                   AND werks   = lv_from_plant.
          APPEND VALUE #( from_plant = <fs_open_item>-werks
                          f_name = VALUE #( gt_pname[ werks = <fs_open_item>-werks ]-name1 OPTIONAL )
                          to_plant = lv_tplant
                          t_name = VALUE #( gt_pname[ werks = lv_tplant ]-name1 OPTIONAL )
                          matnr = <fs_open_item>-matnr
                          matdes = <fs_open_item>-arktx
                          doc_no = <fs_open_inv>-vbeln
                          item = <fs_open_item>-posnr
                          qty = <fs_open_item>-fkimg
                          netqty_kg = <fs_open_item>-ntgew
                          grsqty_kg = <fs_open_item>-brgew
                          identification = 'INVOICE' ) TO gt_output.
        ENDLOOP.
      ENDIF.
      IF lv_ebillno IS INITIAL AND lv_irnno IS INITIAL.
        LOOP AT gt_open_itm ASSIGNING FIELD-SYMBOL(<fs_open_item1>) WHERE vbeln = <fs_open_inv>-vbeln.
          APPEND VALUE #( from_plant = <fs_open_item1>-werks
                           f_name = VALUE #( gt_pname[ werks = <fs_open_item1>-werks ]-name1 OPTIONAL )
                           to_plant = lv_tplant
                           t_name = VALUE #( gt_pname[ werks = lv_tplant ]-name1 OPTIONAL )
                           matnr = <fs_open_item1>-matnr
                           matdes = <fs_open_item1>-arktx
                           doc_no = <fs_open_inv>-vbeln
                           item = <fs_open_item1>-posnr
                           qty = <fs_open_item1>-fkimg
                           netqty_kg = <fs_open_item1>-ntgew
                           grsqty_kg = <fs_open_item1>-brgew
                           identification = 'INVOICE' ) TO gt_output.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

    IF gt_open_po IS NOT INITIAL.
      LOOP AT gt_open_po ASSIGNING FIELD-SYMBOL(<fs_open_po>).
        APPEND VALUE #( from_plant = lv_from_plant
                        f_name = VALUE #( gt_pname[ werks = lv_from_plant ]-name1 OPTIONAL )
                        to_plant = <fs_open_po>-werks
                        t_name = VALUE #( gt_pname[ werks = <fs_open_po>-werks ]-name1 OPTIONAL )
                        matnr = <fs_open_po>-matnr
                        matdes = <fs_open_po>-txz01
                        doc_no = <fs_open_po>-ebeln
                        item = <fs_open_po>-ebelp
                        qty = <fs_open_po>-po_qty
                        netqty_kg = <fs_open_po>-net_vol
                        grsqty_kg = <fs_open_po>-gross_vol
                        identification = 'PURCHASE ORDER' ) TO gt_output.
      ENDLOOP.
    ENDIF.

    IF lv_to_plant IS NOT INITIAL.
      DELETE gt_output[] WHERE to_plant NE lv_to_plant.
    ENDIF.

    IF gt_output IS NOT INITIAL.
      CLEAR: lv_body,v_jsonload.
** serialize the output for response ***
      /ui2/cl_json=>serialize(
      EXPORTING
       data         =  gt_output
       pretty_name  = /ui2/cl_json=>pretty_mode-user
      RECEIVING
       r_json         = lv_body ).

***************store the log table*************
      CALL METHOD lo_log_upd->log_entry_store
        EXPORTING
          apiname         = 'OPEN_PLANT_STO'
          ijson           = lv_data
          ojson           = lv_body
*         distributor     = gw_input-from_loc
*         retailer        = gw_input-to_loc
        EXCEPTIONS
          apiname_missing = 1
          json_missing    = 2
          OTHERS          = 3.

      v_jsonload = | { '[' } { lv_body } { ']' } |.
*Set JSON Content-Type
      CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
      CALL METHOD server->response->set_cdata( data = v_jsonload ).
    ELSE.
      CLEAR lv_body.
      lv_body = |No open Purchase order available for input|.
      v_jsonload = | { '[' } { lv_body } { ']' } |.

***************store the log table*************
      CALL METHOD lo_log_upd->log_entry_store
        EXPORTING
          apiname         = 'OPEN_PLANT_STO'
          ijson           = lv_data
          ojson           = lv_body
*         distributor     = gw_input-from_loc
*         retailer        = gw_input-to_loc
        EXCEPTIONS
          apiname_missing = 1
          json_missing    = 2
          OTHERS          = 3.
*Set JSON Content-Type
      CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
      CALL METHOD server->response->set_cdata( data = v_jsonload ).
    ENDIF.


  ENDMETHOD.
ENDCLASS.
