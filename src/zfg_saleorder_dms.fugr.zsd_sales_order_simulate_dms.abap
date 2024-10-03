FUNCTION zsd_sales_order_simulate_dms.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IM_HEADER) TYPE  ZSD_SALE_HD_DMS
*"     REFERENCE(IM_ITEM) TYPE  ZSD_TT_SOITEM_DMS
*"  EXPORTING
*"     REFERENCE(SALESORDER_NO) TYPE  VBELN
*"     REFERENCE(MESSAGE) TYPE  STRING
*"     REFERENCE(MSG_TYPE) TYPE  CHAR1
*"----------------------------------------------------------------------
  "Created by: Pandiarajan
  "Created on: 25.09.2024
  "Purpose : Direct Sales Order simulation from SPL to Director or Distributor
  "Reference: Ramakrishnan J
*"----------------------------------------------------------------------

* Data declarations.
  DATA: v_vbeln            TYPE vbak-vbeln.
  DATA: header             TYPE bapisdhd1.
  DATA: headerx            TYPE bapisdhd1x.
  DATA: item               TYPE bapisditm.
  DATA: lt_item_bapi       TYPE STANDARD TABLE OF bapisditm.
  DATA: itemx              TYPE bapisditmx.
  DATA: lt_itemx           TYPE STANDARD TABLE OF bapisditmx.
  DATA: partner            TYPE bapiparnr.
  DATA: lt_partner         TYPE STANDARD TABLE OF bapiparnr.
  DATA: lt_return_m        TYPE STANDARD TABLE OF bapiret2.

  DATA: lw_schedules_inx   TYPE  bapischdlx.
  DATA: lw_schedules_in TYPE  bapischdl,
        lw_condition    TYPE  bapicond,
        lw_conditionx   TYPE  bapicondx.

  DATA: lt_schedules_inx   TYPE STANDARD TABLE OF bapischdlx.
  DATA: lt_schedules_in TYPE STANDARD TABLE OF bapischdl,
        lt_condition    TYPE STANDARD TABLE OF bapicond,
        lt_conditionx   TYPE STANDARD TABLE OF bapicondx.

  DATA: lv_posnr TYPE vbap-posnr,
        lv_num2  TYPE numc2,
        v_curr   TYPE waers_v.

  DATA(lw_header) = im_header.
  DATA(lt_item) = im_item.
  DATA :

    ls_order_header    TYPE bapisdhead,
    ls_order_item      TYPE bapiitemin,
    lt_order_items_in  TYPE TABLE OF bapiitemin,
    lt_order_items_out TYPE TABLE OF bapiitemex,
    ls_order_itemout   LIKE LINE OF lt_order_items_out,
    ls_order_partner   TYPE bapipartnr,
    lt_order_partners  TYPE TABLE OF bapipartnr,
    lt_order_condition TYPE TABLE OF bapicond,
    lt_order_messages  TYPE TABLE OF bapiret2,
    ls_solns           TYPE stck_sol_prop,
    ls_auart           TYPE auart,
    lt_auart           TYPE TABLE OF auart,
*    ls_sales_area      TYPE tvakz,
    lv_netprice_diff   TYPE netpr,
    lv_reforder_price  TYPE netpr,
    lv_stprs           TYPE stprs.
  IF lw_header IS NOT INITIAL AND lw_header-ordid IS NOT INITIAL.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lw_header-kunag
      IMPORTING
        output = lw_header-kunag.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lw_header-kunwg
      IMPORTING
        output = lw_header-kunwg.

    CLEAR lv_posnr.

    LOOP AT lt_item  INTO DATA(lw_item).
      lv_posnr = lv_posnr + 1 .
      item-itm_number = lv_posnr.
      item-material   = lw_item-material .
      item-plant      = lw_header-plant.
      item-target_qty = lw_item-qty.
      item-sales_unit = lw_item-uom.

      IF lw_header-xblnr IS NOT INITIAL.
        item-purch_no_c = lw_header-xblnr.
      ELSE.
        item-purch_no_c = 'Direct Sale Order' .
*Append Pepeup Order no to the reference text
        IF lw_header-ordid IS NOT INITIAL.
          CONCATENATE lw_header-ordid item-purch_no_c INTO item-purch_no_c SEPARATED BY '-'.
        ENDIF.
      ENDIF.

      APPEND item TO lt_item_bapi.
      CLEAR item.
    ENDLOOP.

* ITEM DATA
    itemx-updateflag = 'I'.
* Line item number.
    itemx-itm_number = 'X'.
* Material
    itemx-material = 'X'.

    itemx-plant   = 'X'.
* Quantity
    itemx-target_qty = 'X'.
    itemx-purch_no_c = 'X'.

    itemx-purch_date = 'X'.
    APPEND itemx TO lt_itemx.

* Sales document type
    header-doc_type   =  lw_header-auart .""'YBBR'.
    headerx-doc_type  = 'X'.

* Sales organization
    header-sales_org  = lw_header-vkorg.
    headerx-sales_org = 'X'.

** Distribution channel
    header-distr_chan  = lw_header-vtweg.
    headerx-distr_chan = 'X'.

* Division
    header-division =  lw_header-spart.
    headerx-division = 'X'.

* order date
    header-purch_date = sy-datum.
    headerx-purch_date = 'X'.

* Sales Office
    header-sales_off = lw_header-vkbur.
    headerx-sales_off = 'X'.

* So date
    header-price_date = lw_header-sodate.
    headerx-price_date = 'X'.

*    header-purch_no_c = 'Direct Sale Order' .

    IF lw_header-xblnr IS NOT INITIAL.
      header-purch_no_c = lw_header-xblnr.
    ELSE.
      header-purch_no_c = 'Direct Sale Order' .
*Append Pepeup Order no to the reference text
      IF lw_header-ordid IS NOT INITIAL.
        CONCATENATE lw_header-ordid header-purch_no_c INTO header-purch_no_c SEPARATED BY '-'.
      ENDIF.
    ENDIF.

    headerx-purch_no_c = 'X'.
    headerx-updateflag = 'I'.

* Partner data
* Sold to
    partner-partn_role = 'AG'.
    partner-partn_numb = lw_header-kunag.
    APPEND partner TO lt_partner.

* Ship to
    partner-partn_role = 'WE'.
    partner-partn_numb = lw_header-kunwg.
    APPEND partner TO lt_partner.

***Currency based on Sales Organization
    IF lw_header-vkorg IS NOT INITIAL.
      CLEAR: v_curr.
      SELECT SINGLE waers FROM tvko INTO v_curr WHERE vkorg = lw_header-vkorg.
    ENDIF.

    CLEAR lv_posnr.
    LOOP AT lt_item INTO lw_item.

      lv_posnr = lv_posnr + 1 .
      lv_num2 = lv_num2 + 1 .

      lw_schedules_in-itm_number =  lv_posnr .
      lw_schedules_in-sched_line = lv_posnr.
      lw_schedules_in-req_qty    = lw_item-qty.

      APPEND lw_schedules_in TO lt_schedules_in.


*   Fill schedule line flags
      lw_schedules_inx-itm_number  = lv_posnr.
      lw_schedules_inx-sched_line  = lv_posnr .
      lw_schedules_inx-updateflag  = 'X'.
      lw_schedules_inx-req_qty     = 'X'.
      APPEND lw_schedules_inx TO lt_schedules_inx.
      CLEAR :lw_schedules_inx,lw_schedules_in.

**    Z010 - Discount Percentage in Currency Material Level
      IF lw_item-disper1 IS NOT INITIAL.
*          CLEAR: lw_condition.
        lw_condition-itm_number = lv_posnr.
        lw_condition-cond_count = lv_num2.
        lw_condition-cond_type  = 'Z013'.
        DATA(lv_percentage) = lw_item-disper1.
        "Calculation based flag
        SELECT SINGLE low FROM tvarvc INTO @DATA(l_flag) WHERE name = 'ZSD_SPL_SALES_FLAG'
                                                         AND type  = 'P'.
        IF sy-subrc EQ 0.
          IF l_flag = abap_true.
            lv_percentage = lv_percentage * 10.
          ENDIF.
          lw_condition-cond_value = lv_percentage.
          lw_condition-currency   = v_curr.
          APPEND lw_condition TO lt_condition.
        ENDIF.
      ENDIF.
      IF lw_item-disper2 IS NOT INITIAL.
*          CLEAR: lw_condition.
        lw_condition-itm_number = lv_posnr.
        lw_condition-cond_count = lv_num2.
        lw_condition-cond_type  = 'Z014'.
        lv_percentage = lw_item-disper2.
        "Calculation based flag
        SELECT SINGLE low FROM tvarvc
          INTO @l_flag WHERE name = 'ZSD_SPL_SALES_FLAG'
          AND type  = 'P'.
        IF sy-subrc EQ 0.
          IF l_flag = abap_true.
            lv_percentage = lv_percentage * 10.
          ENDIF.
        ENDIF.
        lw_condition-cond_value = lv_percentage.
        lw_condition-currency   = v_curr.
        APPEND lw_condition TO lt_condition.
      ENDIF.
*Discount Amount Z011 & Z012
      IF lw_item-disvalue1 IS NOT INITIAL.
        lw_condition-itm_number = lv_posnr.
        lw_condition-cond_count = lv_num2.
        lw_condition-cond_type  = 'Z015'.
        lw_condition-cond_value = lw_item-disvalue1.
        lw_condition-currency   = v_curr.
        APPEND lw_condition TO lt_condition.
      ENDIF.
      IF lw_item-disvalue2 IS NOT INITIAL.
        lw_condition-itm_number = lv_posnr.
        lw_condition-cond_count = lv_num2.
        lw_condition-cond_type  = 'Z016'.
        lw_condition-cond_value = lw_item-disvalue2.
        lw_condition-currency   = v_curr.
        APPEND lw_condition TO lt_condition.
      ENDIF.
      IF lt_condition IS NOT INITIAL.
        CLEAR : lw_conditionx.
        lw_conditionx-itm_number = lv_posnr.
        lw_conditionx-cond_count = abap_true.
        lw_conditionx-cond_type  = abap_true.
        lw_conditionx-cond_value = abap_true.
        lw_conditionx-currency   = abap_true.
        APPEND lw_conditionx TO lt_conditionx.
      ENDIF.

      ls_order_item-material = lw_item-material.
      ls_order_item-req_qty = lw_item-qty.
      ls_order_item-plant = lw_header-plant.
      APPEND ls_order_item TO lt_order_items_in.

    ENDLOOP.

    IF lt_item_bapi[] IS NOT INITIAL.


*        "Identify allowed sales area for above order type
*        SELECT SINGLE * FROM tvakz INTO ls_sales_area WHERE auart =  iv_auart.

      ls_order_header-doc_type = lw_header-auart.
      ls_order_header-sales_org = lw_header-vkorg.
      ls_order_header-distr_chan = lw_header-vtweg.
      ls_order_header-division = lw_header-spart.
      ls_order_header-price_date = lw_header-sodate.

      ls_order_partner-partn_role  = 'AG'.
      ls_order_partner-partn_numb = lw_header-kunag.
      APPEND ls_order_partner TO lt_order_partners.

      ls_order_partner-partn_role  = 'WE'.
      ls_order_partner-partn_numb = lw_header-kunwg.
      APPEND ls_order_partner TO lt_order_partners.

      CALL FUNCTION 'BAPI_SALESORDER_SIMULATE'
        EXPORTING
          order_header_in    = ls_order_header
        TABLES
          order_items_in     = lt_order_items_in
          order_partners     = lt_order_partners
          order_items_out    = lt_order_items_out
          order_condition_ex = lt_order_condition
          messagetable       = lt_order_messages.

* Check the return table.
      LOOP AT lt_order_messages INTO DATA(return_m) WHERE type = 'E' OR type = 'A'.
        message = |{ message } , { return_m-message } |.
      ENDLOOP.

      IF message IS INITIAL.

        SORT : lt_order_condition BY itm_number cond_type.

        LOOP AT lt_order_items_out ASSIGNING FIELD-SYMBOL(<fs_sale_itm>).
*----------------------------------------------------------------------------------
*      Changed by - Pandiarajan
*      changed on date - 12.09.2023
*      Reference by: Ramakrishnan J
*********************HSN & Profit Center Missing Check*****************************
          SELECT SINGLE matnr,steuc,prctr FROM marc INTO @DATA(gs_marc) WHERE matnr = @<fs_sale_itm>-material
                                                                        AND   werks = @lw_header-plant.
          IF sy-subrc = 0.
            IF gs_marc-steuc IS INITIAL.
              message = |{ message } ; HSN Missing for Material { <fs_sale_itm>-material }|.
            ENDIF.
            IF gs_marc-prctr IS INITIAL.
              message = |{ message } ; Profit Center Missing for Material { <fs_sale_itm>-material }|.
            ENDIF.
          ENDIF.
*----------------------------------------------------------------------------------
****************************PR00 & VPRS Missing Check*****************************
**********************check the netvalue*******************
          IF <fs_sale_itm>-net_value LE 0.
            message = |{ message } ; Net Value is zero for Materials { <fs_sale_itm>-material }|.
          ENDIF.
          IF <fs_sale_itm>-itm_number IS NOT INITIAL.
            IF lw_header-bukrs = 'DMS1'.
              DATA(lv_price) = 'ZPR0'.
            ELSE.
              lv_price = 'PRAP'.
            ENDIF.
            READ TABLE lt_order_condition TRANSPORTING cond_value INTO DATA(gv_condvalue) WITH KEY itm_number = <fs_sale_itm>-itm_number
                                                                                             cond_type = lv_price BINARY SEARCH.
            IF sy-subrc <> 0 OR gv_condvalue IS INITIAL.
              message = |{ message } ; Selling Price { lv_price } not Maintained for Materials { <fs_sale_itm>-material }|.
            ENDIF.
            READ TABLE lt_order_condition TRANSPORTING cond_value INTO gv_condvalue WITH KEY itm_number = <fs_sale_itm>-itm_number
                                                                                      cond_type = 'VPRS' BINARY SEARCH.
            IF sy-subrc <> 0 OR gv_condvalue IS INITIAL.
              message = |{ message } ; Internal Price (VPRS) not Maintained for Materials { <fs_sale_itm>-material }|.
            ENDIF.
**********************Cgst & Sgst value Mismatch Check*****************************
            READ TABLE lt_order_condition TRANSPORTING cond_value INTO DATA(lv_igst) WITH KEY itm_number = <fs_sale_itm>-itm_number
                                                                                        cond_type = 'JOIG' BINARY SEARCH.
            IF sy-subrc NE 0.
              READ TABLE lt_order_condition TRANSPORTING cond_value INTO DATA(lv_cgst) WITH KEY itm_number = <fs_sale_itm>-itm_number
                                                                                          cond_type = 'JOCG' BINARY SEARCH.
              IF sy-subrc = 0.
                READ TABLE lt_order_condition TRANSPORTING cond_value INTO DATA(lv_sgst) WITH KEY itm_number = <fs_sale_itm>-itm_number
                                                                                            cond_type = 'JOSG' BINARY SEARCH.
                IF sy-subrc = 0.
                  IF lv_cgst-cond_value <> lv_sgst-cond_value.
                    message = |{ message } ; Cgst/Sgst Values are different in Sales Order for Material { <fs_sale_itm>-material }|.
                  ENDIF.
                ENDIF.
              ELSE.
                message = |{ message } ; IGST or Cgst/Sgst not Maintained for Materials { <fs_sale_itm>-material }|.
              ENDIF.
            ENDIF.
          ENDIF.
*----------------------------------------------------------------------------------
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.
  IF message IS INITIAL.
    msg_type = 'S'.
  ELSE.
    msg_type = 'E'.
  ENDIF.

ENDFUNCTION.
