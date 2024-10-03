class ZCL_DMS_SALE_ORDER_PROCESS definition
  public
  final
  create public .

public section.

  types:
    zsd_dms_sale_itm_ty TYPE STANDARD TABLE OF zsd_dms_sale_itm .

  data ZSD_DMS_SALE_ITM_TT type ZSD_DMS_SALE_ITM_TY .

  methods CREATE_SALE_ORDER
    importing
      !ORDER_ID type ZORDER_ID optional
      !IV_HEADER type ZSD_DMS_SALE_HDR optional
      !IV_ITEM like ZSD_DMS_SALE_ITM_TT optional
    exporting
      value(SALE_ORDER_NO) type VBELN
      !RETURN type BAPIRET2_T .
  methods CREATE_OB_DELIVERY
    importing
      !ORDER_ID type ZORDER_ID optional
      !IV_HEADER type ZSD_DMS_SALE_HDR optional
      !IV_ITEM like ZSD_DMS_SALE_ITM_TT optional
      !LGORT type LGORT_D optional
    exporting
      !DELIVERY_NO type VBELN
      !RETURN type BAPIRET2_T .
  methods POST_GOODS_ISSUE
    importing
      !DELIVERY_NO type VBELN
      !LGORT type LGORT_D optional
    exporting
      !PGI_NO type MBLNR
      !RETURN type BAPIRET2_T .
  methods CREATE_INVOICE
    importing
      !DELIVERY_NO type VBELN
    exporting
      !BILL_INVOICE type VBELN_VF
      !ACCO_INVOICE type BELNR_D
      !BUKRS type BUKRS
      !GJAHR type GJAHR
      !RETURN type BAPIRET2_T .
  methods UPDATE_DMS_TABLE
    importing
      !IV_HEADER type ZSD_DMS_SALE_HDR
      !IV_ITEM like ZSD_DMS_SALE_ITM_TT
    exporting
      !RETURN type BAPIRET2_T .
  methods CREATE_IRN_QRCODE
    importing
      !DISTRIBUTOR_CODE type KUNNR
      !VBELN type VBELN_VF
      !BUKRS type BUKRS
      !GJAHR type GJAHR
      !DATE type SY-DATUM optional
    exporting
      !IRN type J_1IG_IRN
      !SIGNED_INV type J_1IG_SIGN_INV
      !SIGNED_QRCODE type J_1IG_SIGN_QRCODE
      !RETURN type STRING .
protected section.
private section.
ENDCLASS.



CLASS ZCL_DMS_SALE_ORDER_PROCESS IMPLEMENTATION.


  METHOD CREATE_INVOICE.
    DATA: vbsk_i TYPE vbsk.
    DATA: d_success TYPE c.
    DATA: xkomfk   TYPE STANDARD TABLE OF komfk,
          w_xkomfk TYPE komfk,
          xkomv    TYPE STANDARD TABLE OF komv,
          xthead   TYPE STANDARD TABLE OF theadvb,
          xvbfs    TYPE STANDARD TABLE OF vbfs,
          xvbpa    TYPE STANDARD TABLE OF vbpavb,
          xvbrk    TYPE STANDARD TABLE OF vbrkvb,
          xvbrp    TYPE STANDARD TABLE OF vbrpvb,
          xvbss    TYPE STANDARD TABLE OF vbss,
          xkomfkgn TYPE STANDARD TABLE OF komfkgn.

    REFRESH: xkomfk, xkomv,
    xthead, xvbfs,
    xvbpa, xvbrk,
    xvbrp, xvbss.

    CLEAR : xkomfk, xkomv,
    xthead, xvbfs,
    xvbpa, xvbrk,
    xvbrp, xvbss,
    vbsk_i.

    vbsk_i-smart = 'F'.
    w_xkomfk-vbeln = delivery_no.
    w_xkomfk-vbtyp = 'J'.

    APPEND w_xkomfk TO xkomfk.

    CALL FUNCTION 'RV_INVOICE_CREATE'
      EXPORTING
        vbsk_i       = vbsk_i
        with_posting = 'C'
      TABLES
        xkomfk       = xkomfk
        xkomv        = xkomv
        xthead       = xthead
        xvbfs        = xvbfs
        xvbpa        = xvbpa
        xvbrk        = xvbrk
        xvbrp        = xvbrp
        xvbss        = xvbss.

    IF sy-subrc EQ 0.
      COMMIT WORK.
      SELECT SINGLE vbeln FROM vbfa
        INTO bill_invoice
        WHERE vbelv = delivery_no AND vbtyp_n = 'M'.

      SELECT SINGLE belnr
                    bukrs
                    gjahr FROM vbrk
        INTO ( acco_invoice,
               bukrs,
               gjahr )
        WHERE vbeln = bill_invoice.
    ENDIF.



  ENDMETHOD.


  method CREATE_IRN_QRCODE.
  endmethod.


  METHOD CREATE_OB_DELIVERY.
    DATA:lt_request      TYPE STANDARD TABLE OF bapideliciousrequest,
         lw_request      TYPE bapideliciousrequest,
         lt_createditems TYPE STANDARD TABLE OF bapideliciouscreateditems,
         lw_createditems TYPE bapideliciouscreateditems,
         lt_return       TYPE STANDARD TABLE OF bapiret2.
    DATA lv_posnr TYPE posnr.

    DATA: ls_komph   TYPE komph,
          ls_bdcom   TYPE bdcom,
          lt_bdbatch TYPE STANDARD TABLE OF bdbatch INITIAL SIZE 0.
    DATA: i_vbkok   TYPE vbkok,
          lt_prot   TYPE STANDARD TABLE OF prott,
          vbpok_tab TYPE vbpok,
          lt_vbpok  TYPE STANDARD TABLE OF vbpok.
    DATA lt_item LIKE zsd_dms_sale_itm_tt.
    IF order_id IS NOT INITIAL.
      DATA l_order_id TYPE zorder_id.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = order_id
        IMPORTING
          output = l_order_id.

      SELECT SINGLE *
        FROM zsd_dms_sale_hdr
        INTO @DATA(lw_header)
        WHERE ordid = @l_order_id.

      SELECT *
        FROM zsd_dms_sale_itm
        INTO TABLE  lt_item
        WHERE ordid = l_order_id.
    ELSE.
      lw_header = iv_header.
      lt_item = iv_item.
    ENDIF.
* vbeln = 1000481332
    LOOP AT lt_item INTO DATA(lw_item).
      lv_posnr = lv_posnr + 1.
      lw_request-document_item = lv_posnr.
      lw_request-document_numb = lw_header-vbeln.
      lw_request-ship_to = lw_header-kunag.
      lw_request-sold_to = lw_header-kunwe.
      lw_request-stge_loc = lgort.
      lw_request-delivery_date = sy-datum.
      lw_request-sales_organisation = lw_header-vkorg.
      lw_request-distribution_channel = lw_header-vtweg.
      lw_request-division = lw_header-spart.
      lw_request-plant = lw_header-werks.
      lw_request-quantity_sales_uom = lw_item-quant.
      lw_request-sales_unit = lw_item-meins.
      lw_request-base_uom = lw_item-meins.
      lw_request-material = lw_item-matnr.
      lw_request-delivery_date = sy-datum.
      lw_request-delivery_time = sy-uzeit.
      lw_request-transp_plan_date = sy-datum.
      lw_request-loading_date = sy-datum.
      lw_request-goods_issue_date = sy-datum.
      lw_request-extdelv_no = 'DMS_Delivery'.
      lw_request-document_type = 'A'. "Delivery
      lw_request-document_type_predecessor = 'A'. "Sales Ord
      lw_request-document_type_delivery = 'LF'.
      APPEND lw_request TO lt_request.
    ENDLOOP.

    CALL FUNCTION 'BAPI_DELIVERYPROCESSING_EXEC'
      TABLES
        request      = lt_request
        createditems = lt_createditems
        return       = lt_return.

    LOOP AT lt_return INTO DATA(lw_return) WHERE type = 'A' OR type = 'E'.
      APPEND lw_return TO return.
    ENDLOOP.
    IF sy-subrc <> 0.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'H'.
      delivery_no = VALUE #( lt_createditems[ 1 ]-document_numb OPTIONAL ).
    ENDIF.

  ENDMETHOD.


  METHOD CREATE_SALE_ORDER.

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

    DATA : i_messtab  TYPE STANDARD TABLE OF bdcmsgcoll,
           lt_messtab TYPE bdcmsgcoll,
           l_message  TYPE bapiret2-message.

    DATA: lv_datec TYPE char10.

*      DATA lv_spart TYPE mara-spart.
    DATA: lv_posnr TYPE vbap-posnr,
          lv_num2  TYPE numc2,
          v_curr   TYPE waers_v.
    DATA: lt_item LIKE zsd_dms_sale_itm_tt.
    IF order_id IS NOT INITIAL.
      DATA l_order_id TYPE zorder_id.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = order_id
        IMPORTING
          output = l_order_id.
      SELECT SINGLE *
        FROM zsd_dms_sale_hdr
        INTO @DATA(lw_header)
        WHERE ordid = @l_order_id.

      SELECT *
        FROM zsd_dms_sale_itm
        INTO TABLE lt_item
        WHERE ordid = l_order_id.
    ELSE.
      lw_header = iv_header.
      lt_item = iv_item.
    ENDIF.
    IF lw_header IS NOT INITIAL AND lw_header-ordid IS NOT INITIAL.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lw_header-kunag
        IMPORTING
          output = lw_header-kunag.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lw_header-kunwe
        IMPORTING
          output = lw_header-kunwe.

      CLEAR lv_posnr.

      LOOP AT lt_item  INTO DATA(lw_item).
        lv_posnr = lv_posnr + 1 .
        item-itm_number = lv_posnr.
        item-material   = lw_item-matnr .
        item-plant      = lw_header-werks.
        item-target_qty = lw_item-quant.
        item-sales_unit = lw_item-meins.

        item-purch_no_c = 'DMS Sale Order' .

*Append Pepeup Order no to the reference text
        IF lw_item-ordid IS NOT INITIAL.
          CONCATENATE item-purch_no_c lw_item-ordid INTO item-purch_no_c SEPARATED BY '-'.
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
* Plant
      item-plant    = lw_header-werks.
      itemx-plant   = 'X'.
* Quantity
      itemx-target_qty = 'X'.
      itemx-purch_no_c = 'X'.

      itemx-purch_date = 'X'.
      APPEND itemx TO lt_itemx.

******************************************************************************


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

* sales office
      header-sales_off = lw_header-vkbur.

* order date
      header-purch_date = sy-datum.
      headerx-purch_date = 'X'.


      header-purch_no_c = 'DMS Sale Order' .

*Append Pepeup Order no to the reference text
      IF lw_item-ordid IS NOT INITIAL.
        CONCATENATE header-purch_no_c lw_item-ordid INTO header-purch_no_c SEPARATED BY '-'.
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
      partner-partn_numb = lw_header-kunwe.
      APPEND partner TO lt_partner.
*
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
        lw_schedules_in-req_qty    = lw_item-quant.

        APPEND lw_schedules_in TO lt_schedules_in.


*   Fill schedule line flags
        lw_schedules_inx-itm_number  = lv_posnr.
        lw_schedules_inx-sched_line  = lv_posnr .
        lw_schedules_inx-updateflag  = 'X'.
        lw_schedules_inx-req_qty     = 'X'.
        APPEND lw_schedules_inx TO lt_schedules_inx.
        CLEAR :lw_schedules_inx,lw_schedules_in.

**    Z008 - Discount Percentage Price
*        IF  it_material-dis_percent IS NOT INITIAL.
*          lt_condition-itm_number = lv_posnr.
*          lt_condition-cond_count = lv_num2.
*          lt_condition-cond_type  = 'Z008'.
*          lt_condition-cond_value = lw_item-item_dicount.
*          APPEND lt_condition.
*        ENDIF.

**    Z010 - Discount Value in Currency Material Level
        IF lw_item-skfbp IS NOT INITIAL.
*          CLEAR: lw_condition.
          lw_condition-itm_number = lv_posnr.
          lw_condition-cond_count = lv_num2.
          lw_condition-cond_type  = 'Z010'.
          lw_condition-cond_value = lw_item-skfbp.
          lw_condition-currency   = v_curr.
          APPEND lw_condition TO lt_condition.
        ENDIF.
      ENDLOOP.
*
**    Z009 - Discount Value in Currency Header Level
*      IF lw_header-skfbp IS NOT INITIAL.
**          CLEAR: lw_condition.
**          lw_condition-itm_number = lv_posnr.
**          lw_condition-cond_count = lv_num2.
*        lw_condition-cond_type  = 'Z009'.
*        lw_condition-cond_value =  lw_header-skfbp.
*        lw_condition-currency   = v_curr.
*        APPEND lw_condition TO lt_condition.
*      ENDIF.

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
              wait = 'X'
* IMPORTING
*             RETURN        =
            .
          sale_order_no = v_vbeln.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD POST_GOODS_ISSUE.

    DATA: ls_komph   TYPE komph,
          ls_bdcom   TYPE bdcom,
          lt_bdbatch TYPE STANDARD TABLE OF bdbatch INITIAL SIZE 0.
    DATA: i_vbkok   TYPE vbkok,
          lt_prot   TYPE STANDARD TABLE OF prott,
          vbpok_tab TYPE vbpok,
          lt_vbpok  TYPE STANDARD TABLE OF vbpok,
          lw_return	TYPE bapiret2.
    .

    SELECT  vbeln,
            posnr,
            matnr,
            werks,
            lfimg,
            meins,
            arktx,
            vgbel,
            vgpos,
            bwart
            INTO TABLE @DATA(gt_deliv_item)
            FROM lips
            WHERE vbeln = @delivery_no AND
                  bwart = '601' AND "Movement type
                  lgort NE @space AND
                  charg NE @space AND
                  lfimg NE 0.

    LOOP AT gt_deliv_item INTO DATA(itab).

      i_vbkok-vbeln_vl = itab-vbeln.
      i_vbkok-vbeln = itab-vbeln.
      i_vbkok-wabuc    = 'X'.       "<- automatic pgi

*      delivery = itab-vbeln.
*****TABLE DATA

* VERPO_TAB-VBELN = ITAB-VBELN.
* VERPO_TAB-POSNR = ITAB-POSNR.
* VERPO_TAB-TMENG = ITAB-LFIMG.
* VERPO_TAB-VRKME = 'EA'.
* VERPO_TAB-MATNR = ITAB-MATNR.
* VERPO_TAB-LGORT = 'S005'.

* APPEND VERPO_TAB.

      vbpok_tab-vbeln_vl = itab-vbeln.
      vbpok_tab-posnr_vl = itab-posnr.
      vbpok_tab-vbeln = itab-vbeln.
      vbpok_tab-posnn = itab-posnr.
      vbpok_tab-matnr = itab-matnr.
      vbpok_tab-werks = itab-werks.
* VBPOK_TAB-LIANP = 'X'.
      vbpok_tab-pikmg = itab-lfimg.
      vbpok_tab-ndifm = itab-lfimg.
      vbpok_tab-lgort = lgort.
      APPEND vbpok_tab TO lt_vbpok.

* PROT-VBELN = ITAB-VBELN.
* PROT-POSNR = ITAB-POSNR.
* PROT-MATNR = ITAB-MATNR.
* PROT-ARKTX = ITAB-ARKTX.
* PROT-LFIMG = ITAB-LFIMG.
* PROT-VRKME = 'EA'.
* APPEND PROT.

    ENDLOOP.
    IF sy-subrc = 0.
      CALL FUNCTION 'WS_DELIVERY_UPDATE'
        EXPORTING
          vbkok_wa           = i_vbkok
          synchron           = 'X'
          no_messages_update = ' '
          commit             = 'X'
          delivery           = delivery_no
          update_picking     = ' '
        TABLES
          vbpok_tab          = lt_vbpok
          prot               = lt_prot.

      LOOP AT lt_prot INTO DATA(lw_prot) WHERE msgty = 'A' OR msgty = 'E'.
        lw_return-type = lw_prot-msgty.
        lw_return-id = lw_prot-msgid.
        lw_return-number       = lw_prot-msgno.
        lw_return-message_v1 = lw_prot-msgv1.
        lw_return-message_v2 = lw_prot-msgv2.
        lw_return-message_v3 = lw_prot-msgv3.
        lw_return-message_v4 = lw_prot-msgv4.
        APPEND lw_return TO return.
      ENDLOOP.
      IF sy-subrc <> 0.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
      ENDIF.
    ENDIF.

    SELECT SINGLE mblnr FROM mseg
      INTO pgi_no
      WHERE vbeln_im = delivery_no.
  ENDMETHOD.


  METHOD UPDATE_DMS_TABLE.

  ENDMETHOD.
ENDCLASS.
