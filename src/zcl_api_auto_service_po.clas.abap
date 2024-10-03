class ZCL_API_AUTO_SERVICE_PO definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_AUTO_SERVICE_PO IMPLEMENTATION.


  METHOD if_http_extension~handle_request.
**----------------------------------------------------------------------*
    "Created by: Samsudeen M
    "Created on: 23.01.2023
    "Changed On: 10.07.2023
    "Purpose: Auto service Po creation and changes
    "Reference by: Ramakrishnan J
*-----------------------------------------------------------------------*
*    TYPES: BEGIN OF ty_cust,
*             to_plant   TYPE  werks_d,
*             customer   TYPE  kunnr,
*             value      TYPE  netpr,
*             glaccount  TYPE  hkont,
*             costcenter TYPE  kostl,
*           END OF ty_cust.
    DATA: lt_cust TYPE STANDARD TABLE OF zst_autosr_po_cust.
    DATA: gt_output TYPE zmm_api_log_tt.
    DATA: gt_input TYPE ztt_autosr_po,
          gs_input TYPE zst_autosr_po.
    DATA: gt_po_out TYPE zmm_api_log_tt. "Func Module Output table.
    DATA: lv_from TYPE datum,
          lv_to   TYPE datum.
    DATA: header TYPE bapimepoheader.
    DATA: lt_poitems  TYPE TABLE OF bapimepoitem,
          lt_accitem  TYPE TABLE OF bapimepoaccount,
          lt_poitmtxt TYPE TABLE OF bapimepotext.

    DATA :ls_json    TYPE string,
          v_jsonload TYPE string.

    DATA: lv_response1 TYPE string,
          lv_pckg_no   TYPE packno.

    DATA: lv_body TYPE string.

    DATA: lv_message TYPE string.
    DATA: return TYPE bapiret2_t.
*Get Body data
    DATA: lv_data TYPE string.
    DATA: lv_remarks TYPE string.
    DATA : l_item_value TYPE netpr.
    DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
    CREATE OBJECT lo_log_upd.

    CALL METHOD server->request->get_cdata RECEIVING data = lv_data.

    DATA(lv_data_tmp) = |[ { lv_data } ]|.
** Deserialize the input our required input ***
    /ui2/cl_json=>deserialize(
    EXPORTING
     json         = lv_data
     pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
    CHANGING
     data         = gs_input ).

*----- Fetching API Log from MIS Stored in log table -------------*
    SELECT * FROM zmm_serv_po_log INTO TABLE @DATA(lt_apilog) WHERE type = 'S'.
*----------- Error Analyzing from input --------------------------*
    CLEAR: gt_output.
    IF gs_input IS INITIAL.
      APPEND VALUE #( truck_reqno = gs_input-truck_reqno
                      type        = 'E'
                      message     = |No Input is Available| ) TO gt_output.
    ELSE.
      CLEAR lv_remarks.
*----------- Unique Identification Number not Present in Input -----------------------------*
      IF gs_input-truck_reqno IS INITIAL.
        lv_remarks = |Truck Request Number is Missing|.
      ELSE.
        TRANSLATE gs_input-truck_reqno TO UPPER CASE.
        "If it is Present then check whether it is already present in SAP
        DATA(ls_apilog) = VALUE #( lt_apilog[ truck_reqno = gs_input-truck_reqno ] OPTIONAL ).
        IF ls_apilog IS NOT INITIAL AND ls_apilog-type EQ 'S'.
          lv_remarks = |Already this truck request { gs_input-truck_reqno } Number is available |.
        ENDIF.
      ENDIF.
*------------- Vendor Code is Present or Not -----------------------------------------------*
      IF gs_input-vendor IS INITIAL.
        lv_remarks = | { lv_remarks } ,Please enter Vendor Number|.
      ELSE.
        DATA(lv_lifnr) = |{ gs_input-vendor ALPHA = IN }|.
        gs_input-vendor = lv_lifnr.
        SELECT SINGLE * INTO @DATA(l_vendtls) FROM lfa1 WHERE lifnr = @lv_lifnr.
        IF sy-subrc NE 0.
          lv_remarks = | { lv_remarks } , Please enter Correct Vendor Number|.
        ELSE.
          SELECT SINGLE * FROM lfm1 INTO @DATA(l_porgdata)
            WHERE lifnr = @l_vendtls-lifnr.

          SELECT SINGLE lebre
            FROM lfm1 INTO @DATA(l_serv_po_chk)
            WHERE lifnr = @l_vendtls-lifnr
              AND ekorg = '1000'.
          IF l_serv_po_chk = abap_true.
            lv_remarks = | { lv_remarks } ,{ l_vendtls-lifnr } - Kindly remove the Indicator for S-Based IV in Vendor Master |.
          ENDIF.
        ENDIF.
      ENDIF.
*----------- From Plant Checks present or not and exists in SAP or Not ------------------------------*
      IF gs_input-from_plant IS INITIAL.
        lv_remarks = | { lv_remarks } , Please enter From Plant|.
      ELSE.
        SELECT SINGLE bukrs FROM t001k INTO @DATA(lv_bukrs)
          WHERE bwkey = @gs_input-from_plant.
        SELECT SINGLE * FROM t001w INTO @DATA(ls_frm_plant) WHERE werks = @gs_input-from_plant.
        IF sy-subrc NE 0.
          lv_remarks = | { lv_remarks } , Please enter Valid from Plant|.
        ENDIF.
      ENDIF.
*----------- To Plant Checks present or not and exists in SAP or Not ------------------------------*
      LOOP AT gs_input-details ASSIGNING FIELD-SYMBOL(<fs>).
        DATA(lv_toplant) = <fs>-to_plant.
        IF  lv_toplant IS NOT INITIAL.
          SELECT SINGLE * FROM t001w INTO @DATA(ls_to_plant) WHERE werks = @lv_toplant.
          IF sy-subrc NE 0.
            lv_remarks = | { lv_remarks } , Please enter Valid To Plant in line|.
          ELSE.
            DATA(lv_identify) = 'B2B'.
            SELECT SINGLE zkostl FROM zmm_po_plant_cc INTO @DATA(lv_costcenter) WHERE zwerks = @lv_toplant.
            IF sy-subrc NE 0.
              lv_remarks = | { lv_remarks } , Costcenter is missing for plant |.
            ELSE.
              SELECT SINGLE kostl, prctr FROM csks
              INTO @DATA(l_csks)
              WHERE kokrs = '1000' AND
                    kostl = @lv_costcenter AND
                    datbi = '99991231'.
              IF l_csks-prctr = '' .
                lv_remarks = |{ lv_remarks } , Profit center missing for Cost center({ lv_costcenter }). Please Contact the Costing Team|.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
*----------- To Customer Code Checks present or not and exists in SAP or Not ------------------------------*
        DATA(lv_customer) = <fs>-customer.
        IF  ( lv_toplant IS NOT INITIAL AND lv_customer IS NOT INITIAL ) OR
            ( lv_toplant IS INITIAL AND lv_customer IS INITIAL ).
                lv_remarks = |{ lv_remarks } , Enter values any one for To Plant or Customer|.
        ENDIF.
        IF lv_customer IS NOT INITIAL.
          DATA(lv_kunnr) = |{ lv_customer ALPHA = IN }|.
*        gs_input-customer = lv_kunnr.
          SELECT SINGLE * FROM kna1 INTO @DATA(ls_cusdtls) WHERE kunnr = @lv_kunnr.
          IF sy-subrc NE 0.
            lv_remarks = | { lv_remarks } , Please enter Valid Customer|.
          ELSE.
            lv_costcenter = ls_cusdtls-zkostl.
            IF lv_costcenter IS INITIAL.
              lv_remarks = | { lv_remarks } , Costcenter is missing for Customer { lv_kunnr }|.
            ELSE.
              lv_identify = 'B2S'.

              SELECT SINGLE kostl, prctr FROM csks
              INTO @l_csks
              WHERE kokrs = '1000' AND
                    kostl = @lv_costcenter AND
                    datbi = '99991231'.
              IF l_csks-prctr = '' .
                lv_remarks = |{ lv_remarks } , Profit center missing for Cost center({ lv_costcenter }). Please Contact the Costing Team|.
              ENDIF.

            ENDIF.
          ENDIF.
        ENDIF.

*GL Account Classification
        CASE lv_identify.
            "Business to Business
          WHEN 'B2B'.
            SELECT SINGLE * FROM tvarvc INTO @DATA(ls_glacc1) WHERE name = 'SERVICE_PO_GLACCOUNT_B2B'
                                                             AND type = 'P'.
            IF sy-subrc = 0.
              DATA(lv_glacc) = CONV saknr( |{ ls_glacc1-low ALPHA = IN }| ).
            ENDIF.
            "Business to Customer
          WHEN 'B2S'.
            SELECT SINGLE * FROM tvarvc INTO ls_glacc1 WHERE name = 'SERVICE_PO_GLACCOUNT_B2S'
                                                       AND type = 'P'.
            IF sy-subrc = 0.
              lv_glacc = |{ ls_glacc1-low ALPHA = IN }|.
            ENDIF.
        ENDCASE.

        l_item_value =  l_item_value + <fs>-ivalue.

        APPEND VALUE #( to_plant = lv_toplant
                        customer = lv_customer
                        value    = <fs>-ivalue
                        glaccount = lv_glacc
                        costcenter = lv_costcenter ) TO lt_cust.

      ENDLOOP.

      IF gs_input-value IS INITIAL.
        lv_remarks = | { lv_remarks } , Please enter amount|.
      ELSE.
        IF l_item_value NE gs_input-value.
          lv_remarks = | { lv_remarks } , Details Item amounts is not matched from Header|.
        ENDIF.
      ENDIF.

      IF gs_input-short_text IS INITIAL .
        lv_remarks = | { lv_remarks } , Short Text is missing in Header|.
      ENDIF.
*Vendor GST Classification Check *
      CASE l_vendtls-ven_class.
*Registered Vendors
        WHEN ''.
          SELECT SINGLE * FROM zmm_regven_txbp
            INTO @DATA(l_regven)
            WHERE vendor = @lv_lifnr.
          IF sy-subrc = 0.
            DATA(lv_taxcode) = l_regven-taxcode.
          ELSE.
            lv_remarks = |{ lv_remarks }, Registered Vendor TaxCode and Bplace is Not Maintained|.
          ENDIF.
*Non Registered Vendors
        WHEN OTHERS.
*If Sending goods to Our Plants then Derivation of tax codes
          IF ls_to_plant IS NOT INITIAL.
*If From Region and To Region Is Same
            IF ls_frm_plant-regio = ls_to_plant-regio.
              SELECT SINGLE * FROM zmm_serpo_tax_bp INTO @DATA(ls_taxdtls)
                WHERE from_region = @ls_frm_plant-regio
                AND to_region = @ls_to_plant-regio.
              IF sy-subrc = 0.
                lv_taxcode = ls_taxdtls-taxcode.
              ELSE.
                lv_remarks = |{ lv_remarks }, Taxcode and Business Place Not Maintained|.
              ENDIF.
*If From Region and To Region is Different
            ELSE.
              CLEAR ls_to_plant-regio.
              SELECT SINGLE * FROM zmm_serpo_tax_bp INTO ls_taxdtls
                WHERE from_region = ls_frm_plant-regio
                AND to_region = ls_to_plant-regio.
              IF sy-subrc = 0.
                lv_taxcode = ls_taxdtls-taxcode.
              ELSE.
                lv_remarks = |{ lv_remarks }, Taxcode and Business Place Not Maintained|.
              ENDIF.
            ENDIF.
*If Sending goods to Customer then Derivation of tax codes
          ELSEIF ls_cusdtls IS NOT INITIAL.
            IF ls_frm_plant-regio = ls_cusdtls-regio.
              SELECT SINGLE * FROM zmm_serpo_tax_bp INTO ls_taxdtls
                WHERE from_region = ls_frm_plant-regio
                AND to_region = ls_cusdtls-regio.
              IF sy-subrc = 0.
                lv_taxcode = ls_taxdtls-taxcode.
              ELSE.
                lv_remarks = |{ lv_remarks }, Taxcode and Business Place Not Maintained|.
              ENDIF.
*If From Region and To Region is Different
            ELSE.
              CLEAR ls_cusdtls-regio.
              SELECT SINGLE * FROM zmm_serpo_tax_bp INTO ls_taxdtls
                WHERE from_region = ls_frm_plant-regio
                AND to_region = ls_cusdtls-regio.
              IF sy-subrc = 0.
                lv_taxcode = ls_taxdtls-taxcode.
              ELSE.
                lv_remarks = |{ lv_remarks }, Taxcode and Business Place Not Maintained|.
              ENDIF.
            ENDIF.
          ENDIF.
      ENDCASE.
*Error Message Handling
      IF lv_remarks IS NOT INITIAL.
        CONDENSE lv_remarks.
        SHIFT lv_remarks LEFT DELETING LEADING ','.
        APPEND VALUE #( truck_reqno = gs_input-truck_reqno
                        type = 'E'
                        message = lv_remarks ) TO gt_output.
      ENDIF.
    ENDIF.
*------------- If no error Actual process starts ---------------------------------------*

    CLEAR: lv_from,lv_to.
    CALL FUNCTION 'OIL_MONTH_GET_FIRST_LAST'
      EXPORTING
        i_month     = sy-datum+4(2)
        i_year      = sy-datum+0(4)
      IMPORTING
        e_first_day = lv_from
        e_last_day  = lv_to
      EXCEPTIONS
        wrong_date  = 1
        OTHERS      = 2.
    IF sy-subrc = 0.

    ENDIF.
*If Service PO exits for Vendor on the Current Month *
    SELECT ebeln, bukrs, lifnr, aedat FROM ekko INTO TABLE @DATA(gt_ekko)
                                      WHERE lifnr EQ @l_vendtls-lifnr
                                      AND bstyp = 'F'
                                      AND bsart = 'ZSR'
                                      AND ( aedat BETWEEN @lv_from AND @lv_to )
                                      AND bukrs = @lv_bukrs
                                      AND loekz = @space. " Deletion Mark is null
    IF sy-subrc EQ 0.
      SORT gt_ekko[] BY ebeln DESCENDING.
      SORT gt_ekko[] BY aedat DESCENDING.
      DATA(lv_ponum) = VALUE #( gt_ekko[ 1 ]-ebeln OPTIONAL ).
      SELECT ebeln, ebelp, packno FROM ekpo INTO TABLE @DATA(gt_ekpo)
        WHERE ebeln = @lv_ponum.
      IF sy-subrc EQ 0.
        SELECT SINGLE frgke FROM ekko
              INTO @DATA(l_release_flag) WHERE ebeln = @lv_ponum AND frgzu = 'XX'.
        IF l_release_flag NE '3'.
          lv_remarks = |{ lv_remarks }, The purchase order has not been released|.
          CONDENSE lv_remarks.
          SHIFT lv_remarks LEFT DELETING LEADING ','.
          APPEND VALUE #( truck_reqno = gs_input-truck_reqno
                          type = 'E'
                          message = lv_remarks ) TO gt_output.
        ELSE.
          SORT gt_ekpo[] BY ebelp DESCENDING.
          DATA(lv_poitem) = VALUE #( gt_ekpo[ 1 ]-ebelp OPTIONAL ).
          lv_pckg_no = VALUE #( gt_ekpo[ 1 ]-packno OPTIONAL ).
          lv_poitem = lv_poitem + 10.
          REFRESH lt_poitems.
          lt_poitems = VALUE #( ( po_item = lv_poitem
                                 short_text = gs_input-short_text
                                 plant = ls_frm_plant-werks
                                 matl_group = 'YBSVS1'
                                 quantity = '1'
                                 po_unit = 'AU'
                                 net_price = CONV bapicurext( gs_input-value )
                                 tax_code = lv_taxcode
                                 unlimited_dlv = ''
                                 item_cat = 'D'
                                 acctasscat = 'F'
                                 pckg_no = lv_pckg_no ) ).
        ENDIF.
        DATA(lv_flag) = 'U'. "Updating Existing Purchase Order
        DATA(lv_podate) = VALUE #( gt_ekko[ 1 ]-aedat OPTIONAL ).
      ENDIF.
    ELSE.
      lv_poitem = '10'.
      REFRESH lt_poitems.
      lt_poitems = VALUE #( ( po_item      = lv_poitem
                             short_text    = gs_input-short_text
                             plant         = ls_frm_plant-werks
                             matl_group    = 'YBSVS1'
                             quantity      = '1'
                             po_unit       = 'AU'
                             net_price     = CONV bapicurext( gs_input-value )
                             tax_code      = lv_taxcode
                             unlimited_dlv = ''
                             item_cat      = 'D'
                             acctasscat    = 'F'
                             pckg_no       = lv_pckg_no ) ).
      lv_flag = 'N'.
      lv_podate = sy-datum.
      CLEAR header.
      header-comp_code    = lv_bukrs.
      header-doc_type     = 'ZSR'.
      header-creat_date   = lv_podate.
      header-vendor       = l_vendtls-lifnr.
      header-langu        = sy-langu.

      IF l_porgdata-zterm IS NOT INITIAL.
        header-pmnttrms   = l_porgdata-zterm.
      ELSE.
        header-pmnttrms   = 'NT60'.
      ENDIF.

      header-purch_org    = l_porgdata-ekorg.

      IF l_porgdata-ekgrp IS NOT INITIAL.
        header-pur_group  = l_porgdata-ekgrp.
      ELSE.
        header-pur_group  = '116'.
      ENDIF.
      header-currency     = 'INR'.
      header-doc_date     = lv_podate.
    ENDIF.

    IF gt_output[] IS INITIAL.


      DATA: lv_fund TYPE char10.
      CLEAR: lv_fund.
      CALL FUNCTION 'ZFUND_GET'
        EXPORTING
          date       = lv_podate
        IMPORTING
          lv_fund    = lv_fund
        EXCEPTIONS
          enter_date = 1
          OTHERS     = 2.
      IF sy-subrc <> 0.
      ENDIF.

      REFRESH: lt_accitem.

      DATA lv_serial_no  TYPE dzekkn VALUE '01'.
      LOOP AT lt_cust INTO DATA(lw_cust).
        APPEND VALUE #( po_item    = lv_poitem
                        serial_no  = lv_serial_no
                        quantity   = '1'
                        net_value  = lw_cust-value
                        gl_account = lw_cust-glaccount
                        costcenter = lw_cust-costcenter
                        fund       = lv_fund )  TO lt_accitem.

        lv_serial_no = lv_serial_no + 1.
      ENDLOOP.
      IF gs_input-item_text IS NOT INITIAL.
*          CLEAR: lt_poitmtxt.
        DATA(lv_itemtext) = CONV tdline( gs_input-item_text ).
        APPEND VALUE #( po_number = lv_ponum
                        po_item   = lv_poitem
                        text_id   = 'F03'
                        text_form = '*'
                        text_line = lv_itemtext  ) TO lt_poitmtxt.
      ENDIF.
*------------- All Creation and change of Purchase Order Happens inside this FM ----------*
      CLEAR gt_po_out.
      CALL FUNCTION 'ZMM_SERVICE_PO_MAINTAIN' "  IN UPDATE TASK "BACKGROUND TASK AS SEPARATE UNIT
        EXPORTING
          poheader    = header
          po_flag     = lv_flag
          ponumber    = lv_ponum
        TABLES
          lt_cust     = lt_cust
          lt_item     = lt_poitems
          lt_accitem  = lt_accitem
          lt_messages = gt_po_out
          lt_itemtext = lt_poitmtxt.
    ELSE.
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
          apiname         = 'AUTOSERVPO'
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
*------  API Response After Processed -----------------------------------------------------*
    DATA lt_serv_po_log TYPE STANDARD TABLE OF zmm_serv_po_log.
    DATA lv_seqnr TYPE seqnr.
    IF gt_po_out IS NOT INITIAL.
      READ TABLE gt_po_out ASSIGNING FIELD-SYMBOL(<fs_log>) INDEX 1.
      IF sy-subrc = 0.
        <fs_log>-truck_reqno = gs_input-truck_reqno.
      ENDIF.
      LOOP AT lt_cust INTO lw_cust.
        lv_seqnr = lv_seqnr + 1.
        APPEND VALUE #(
                                             truck_reqno = gs_input-truck_reqno
                                             seqnr       = lv_seqnr
                                             type        = VALUE #( gt_po_out[ 1 ]-type )
                                             lifnr       = l_vendtls-lifnr
                                             ebeln       = VALUE #( gt_po_out[ 1 ]-ebeln OPTIONAL )
                                             ebelp       = VALUE #( gt_po_out[ 1 ]-ebelp OPTIONAL )
                                             txz01       = gs_input-short_text
                                             from_plant  = gs_input-from_plant
                                             to_plant    = lw_cust-to_plant
                                             kunnr       = lw_cust-customer
                                             value       = lw_cust-value
                                             erdat       = sy-datum
                                             message     = VALUE #( gt_po_out[ 1 ]-message ) ) TO lt_serv_po_log.
*      DATA(polog) = VALUE zmm_serv_po_log( mandt       = sy-mandt
*                                           truck_reqno = gs_input-truck_reqno
*                                           type        = VALUE #( gt_po_out[ 1 ]-type )
*                                           lifnr       = l_vendtls-lifnr
*                                           ebeln       = VALUE #( gt_po_out[ 1 ]-ebeln OPTIONAL )
*                                           ebelp       = VALUE #( gt_po_out[ 1 ]-ebelp OPTIONAL )
*                                           txz01       = gs_input-short_text
*                                           from_plant  = gs_input-from_plant
*                                           to_plant    = lv_toplant
*                                           kunnr       = lv_kunnr
*                                           value       = gs_input-value
*                                           erdat       = sy-datum
*                                           message     = VALUE #( gt_po_out[ 1 ]-message ) ).
      ENDLOOP.
      IF lt_serv_po_log IS NOT INITIAL.
        MODIFY zmm_serv_po_log FROM TABLE lt_serv_po_log.
      ENDIF.
      CLEAR: lv_body,v_jsonload,lv_flag,lt_poitems,lt_accitem,lt_poitmtxt,header.
** serialize the output for response ***
      /ui2/cl_json=>serialize(
      EXPORTING
       data         =  gt_po_out
       pretty_name  = /ui2/cl_json=>pretty_mode-user
      RECEIVING
       r_json         = lv_body ).

      v_jsonload = | { '[' } { lv_body } { ']' } |.

      CALL METHOD lo_log_upd->log_entry_store
        EXPORTING
          apiname         = 'AUTOSERVPO'
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


  ENDMETHOD.
ENDCLASS.
