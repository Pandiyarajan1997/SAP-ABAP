FUNCTION zsd_spl_sales_order_create.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IM_HEADER) TYPE  ZSD_SPL_SALE_HD
*"     REFERENCE(IM_ITEM) TYPE  ZSD_TT_SOITEM
*"  EXPORTING
*"     REFERENCE(SALESORDER_NO) TYPE  VBELN
*"  TABLES
*"      RETURN TYPE  BAPIRET2_T
*"----------------------------------------------------------------------
  "Created by: Samsudeen M
  "Created on: 25.03.2023
  "Purpose : Direct Sales Order from SPL to Director or Distributor
  "Reference: Ramakrishnan J
*"----------------------------------------------------------------------

* Data declarations.
  DATA: v_vbeln            TYPE vbak-vbeln.
  DATA: header             TYPE bapisdhead1.
  DATA: headerx            TYPE bapisdhead1x.
  DATA: item               TYPE bapisditem.
  DATA: lt_item_bapi       TYPE STANDARD TABLE OF bapisditem.
  DATA: itemx              TYPE bapisditemx.
  DATA: lt_itemx           TYPE STANDARD TABLE OF bapisditemx.
  DATA: partner            TYPE bapipartnr.
  DATA: lt_partner         TYPE STANDARD TABLE OF bapipartnr.
  DATA: lt_return_m        TYPE STANDARD TABLE OF bapiret2.

  DATA: lw_schedules_inx   TYPE  bapischdlx.
  DATA: lw_schedules_in TYPE  bapischdl,
        lw_condition    TYPE  bapicondition.

  DATA: lt_schedules_inx   TYPE STANDARD TABLE OF bapischdlx.
  DATA: lt_schedules_in TYPE STANDARD TABLE OF bapischdl,
        lt_condition    TYPE STANDARD TABLE OF bapicondition.

  DATA: lv_posnr TYPE vbap-posnr,
        lv_num2  TYPE numc2,
        v_curr   TYPE waers_v.

  DATA(lw_header) = im_header.
  DATA(lt_item) = im_item.

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
      IF lw_item-strg_loc IS NOT INITIAL AND im_header-bukrs NE '1000'.
        item-store_loc  = lw_item-strg_loc.
      ENDIF.
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

**********storage loc.************
    IF im_header-strg_loc IS NOT INITIAL AND im_header-bukrs NE '1000'.
      itemx-store_loc  = abap_true.
    ENDIF.

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
        lw_condition-cond_type  = 'Z008'.
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
        lw_condition-cond_type  = 'Z009'.
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
        lw_condition-cond_type  = 'Z011'.
        lw_condition-cond_value = lw_item-disvalue1.
        lw_condition-currency   = v_curr.
        APPEND lw_condition TO lt_condition.
      ENDIF.
      IF lw_item-disvalue2 IS NOT INITIAL.
        lw_condition-itm_number = lv_posnr.
        lw_condition-cond_count = lv_num2.
        lw_condition-cond_type  = 'Z012'.
        lw_condition-cond_value = lw_item-disvalue2.
        lw_condition-currency   = v_curr.
        APPEND lw_condition TO lt_condition.
      ENDIF.
    ENDLOOP.

    IF lt_item_bapi[] IS NOT INITIAL.

* Call the BAPI to create the sales order.
      CALL FUNCTION 'BAPI_SALESDOCU_CREATEFROMDATA1'
        EXPORTING
          sales_header_in     = header
          sales_header_inx    = headerx
        IMPORTING
          salesdocument_ex    = v_vbeln
        TABLES
          return              = lt_return_m
          sales_items_in      = lt_item_bapi
          sales_items_inx     = lt_itemx
          sales_schedules_in  = lt_schedules_in
          sales_schedules_inx = lt_schedules_inx
          sales_partners      = lt_partner
          sales_conditions_in = lt_condition.

* Check the return table.
      LOOP AT lt_return_m INTO DATA(return_m) WHERE type = 'E' OR type = 'A'.
*      it_error-text = return-message.
        APPEND return_m TO return.
      ENDLOOP.
      IF sy-subrc NE 0.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
        salesorder_no = v_vbeln.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFUNCTION.
