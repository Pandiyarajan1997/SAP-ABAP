*&---------------------------------------------------------------------*
*& Report ZSD_DMS_SAL_INVOICE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsd_dms_sal_invoice.
TABLES zsd_dms_sale_hdr.

* declare object for dragdrop object.
DATA : lo_main TYPE REF TO zcl_dms_sale_order_process.
DATA : lo_main1 TYPE REF TO zcl_dms_einvoice_process.

DATA: gt_return TYPE STANDARD TABLE OF bapiret2.
DATA: gt_header TYPE STANDARD TABLE OF zsd_dms_sale_hdr.
DATA: gt_item TYPE STANDARD TABLE OF zsd_dms_sale_itm.

SELECT-OPTIONS: s_ordid FOR zsd_dms_sale_hdr-ordid MATCHCODE OBJECT zsd_dms_ordid.
SELECT-OPTIONS: s_distb FOR zsd_dms_sale_hdr-distb.

************************************************************
*   INITIALIZATION                                         *
************************************************************
INITIALIZATION.

************************************************************
*   At selection screen events                             *
************************************************************

************************************************************
*   Start-Of-Selection                                     *
************************************************************
START-OF-SELECTION.

  CREATE OBJECT lo_main.
  CREATE OBJECT lo_main1.

  SELECT *
    FROM zsd_dms_sale_hdr
    INTO TABLE gt_header
    WHERE ordid IN s_ordid[] AND
          distb IN s_distb[] AND
          status NE '19' AND " Completed
          errmsg EQ ''. " No Error's
  IF sy-subrc = 0.
    SORT gt_header BY ordid status.
    SELECT *
      FROM zsd_dms_sale_itm
      INTO TABLE gt_item
      FOR ALL ENTRIES IN gt_header
      WHERE ordid = gt_header-ordid.
    SORT gt_item BY ordid.
  ENDIF.

  " Create Sale Order Status = 12 from Inital Status = 11
  PERFORM f_dms_sale_order_process USING '11'.
*
*  " Create OBD  Status = 13 from Sales Order Status - 12
  PERFORM f_dms_sale_order_process USING '12'.

*  " Create PGI Status = 14 from Delivery No Status - 13
  PERFORM f_dms_sale_order_process USING '13'.

*  " Create Invoice Status - 15 and Accouting No Status - 16 From PGI Status = 14.
  PERFORM f_dms_sale_order_process USING '14'.

*  " Create e-Invoice IRN Status - 17 from Accouting No Status - 16
  PERFORM f_dms_sale_order_process USING '16'.
*
**  " Create e-Waybill I Status - 18  from e-Invoice Status - 17
*  PERFORM f_dms_sale_order_process USING '17'.
*
*  " Complete Status - 19 from e-Waybill I Status - 18.
  PERFORM f_dms_sale_order_process USING '18'.

  IF gt_header IS NOT INITIAL.
    MODIFY zsd_dms_sale_hdr FROM TABLE gt_header.

    DATA: gr_alv TYPE REF TO cl_salv_table.
    DATA: lr_columns TYPE REF TO cl_salv_columns_table.
    CALL METHOD cl_salv_table=>factory
      EXPORTING
        list_display = if_salv_c_bool_sap=>false
      IMPORTING
        r_salv_table = gr_alv
      CHANGING
        t_table      = gt_header.

*    lr_columns = gr_alv->get_columns( ).
*
*    lr_columns->set_color_column( value = 'IT_COLORS' ).

* display ALV
    gr_alv->display( ).
  ENDIF.
*&---------------------------------------------------------------------*
*& Form f_dms_sale_order_process
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_dms_sale_order_process USING iv_status.

  DATA: lt_itm_upd TYPE zcl_dms_sale_order_process=>zsd_dms_sale_itm_ty.
  DATA: l_err_flag TYPE c.
  DATA(lt_header_temp) = gt_header.
  DELETE lt_header_temp WHERE status NE iv_status. " Intial
  LOOP AT lt_header_temp ASSIGNING FIELD-SYMBOL(<lw_header>).
    CLEAR: gt_return, lt_itm_upd[].
    lt_itm_upd = gt_item.
    DELETE lt_itm_upd WHERE ordid NE <lw_header>-ordid.
    CASE iv_status.
      WHEN '11'. " Create Sale Order
        CALL METHOD lo_main->create_sale_order
          EXPORTING
*           order_id      =
            iv_header     = <lw_header>
            iv_item       = lt_itm_upd
          IMPORTING
            sale_order_no = <lw_header>-vbeln
            return        = gt_return.
        IF <lw_header>-vbeln IS NOT INITIAL.
          READ TABLE gt_header ASSIGNING FIELD-SYMBOL(<lw_orignal_hdr>) WITH KEY ordid = <lw_header>-ordid.
          IF sy-subrc = 0.
            <lw_header>-vbeln = CONV vbeln( |{ <lw_header>-vbeln ALPHA = IN }| ).
            <lw_orignal_hdr>-vbeln = <lw_header>-vbeln.
            <lw_orignal_hdr>-aedat = sy-datum.
            <lw_orignal_hdr>-status = '12'.
            <lw_orignal_hdr>-errmsg = ''.
          ENDIF.
        ENDIF.

      WHEN '12'. " Create OBD from Sales Order

        SELECT  matnr,
                labst FROM mard  INTO TABLE @DATA(lt_tot_stock)
          FOR ALL ENTRIES IN @lt_itm_upd
          WHERE matnr = @lt_itm_upd-matnr AND
                werks =  @<lw_header>-werks.
        LOOP AT lt_itm_upd INTO DATA(lw_item).
          DATA(l_total_qty) = VALUE #( lt_tot_stock[ matnr = lw_item-matnr ]-labst OPTIONAL ).
          IF l_total_qty < lw_item-quant.
            READ TABLE gt_header ASSIGNING  <lw_orignal_hdr> WITH KEY ordid = <lw_header>-ordid.
            IF sy-subrc = 0.
              <lw_orignal_hdr>-aedat = sy-datum.
              <lw_orignal_hdr>-errmsg = |Delivery is not possible for Material: { lw_item-matnr } quantity is more than available stock'|.
              l_err_flag = 'X'.
            ENDIF.
          ENDIF.
        ENDLOOP.
        IF l_err_flag IS INITIAL.
          CALL METHOD lo_main->create_ob_delivery
            EXPORTING
*             order_id    =
              iv_header   = <lw_header>
              iv_item     = lt_itm_upd
              lgort       = 'D1'
            IMPORTING
              delivery_no = <lw_header>-vbeln_vl " Delivery No
              return      = gt_return.
          IF <lw_header>-vbeln_vl IS NOT INITIAL.
            READ TABLE gt_header ASSIGNING  <lw_orignal_hdr> WITH KEY ordid = <lw_header>-ordid.
            IF sy-subrc = 0.
              <lw_header>-vbeln_vl = CONV vbeln( |{ <lw_header>-vbeln_vl ALPHA = IN }| ).
              <lw_orignal_hdr>-vbeln_vl = <lw_header>-vbeln_vl.
              <lw_orignal_hdr>-aedat = sy-datum.
              <lw_orignal_hdr>-status = '13'.
              <lw_orignal_hdr>-errmsg = ''.
            ENDIF.
          ENDIF.
        ENDIF.
      WHEN '13'. " Create PGI from Delivery No

        CALL METHOD lo_main->post_goods_issue
          EXPORTING
            delivery_no = <lw_header>-vbeln_vl
            lgort       = 'D1'
          IMPORTING
            pgi_no      = <lw_header>-mblnr
            return      = gt_return.
        IF <lw_header>-mblnr IS NOT INITIAL.
          READ TABLE gt_header ASSIGNING  <lw_orignal_hdr> WITH KEY ordid = <lw_header>-ordid.
          IF sy-subrc = 0.
            <lw_header>-mblnr = CONV vbeln( |{ <lw_header>-mblnr ALPHA = IN }| ).
            <lw_orignal_hdr>-mblnr = <lw_header>-mblnr.
            <lw_orignal_hdr>-aedat = sy-datum.
            <lw_orignal_hdr>-status = '14'.  " PGI Number
            <lw_orignal_hdr>-errmsg = ''.
          ENDIF.
        ENDIF.
      WHEN '14'. " Create Invoice and Accouting No.

        CALL METHOD lo_main->create_invoice
          EXPORTING
            delivery_no  = <lw_header>-vbeln_vl
          IMPORTING
            bill_invoice = <lw_header>-vbeln_vf " Billing Invoice
            acco_invoice = <lw_header>-belnr    " Accounting Document No
            bukrs        = <lw_header>-bukrs
            gjahr        = <lw_header>-gjahr
            return       = gt_return.
        IF <lw_header>-vbeln_vf IS NOT INITIAL.
          READ TABLE gt_header ASSIGNING  <lw_orignal_hdr> WITH KEY ordid = <lw_header>-ordid.
          IF sy-subrc = 0.
            <lw_header>-vbeln_vf = CONV vbeln( |{ <lw_header>-vbeln_vf ALPHA = IN }| ).
            <lw_orignal_hdr>-vbeln_vf = <lw_header>-vbeln_vf.
            <lw_orignal_hdr>-bukrs = <lw_header>-bukrs.
            <lw_orignal_hdr>-gjahr = <lw_header>-gjahr.
            <lw_orignal_hdr>-aedat = sy-datum.
            <lw_orignal_hdr>-status = '15'.
            <lw_orignal_hdr>-errmsg = ''.
          ENDIF.
        ENDIF.
        IF <lw_header>-belnr IS NOT INITIAL.
          READ TABLE gt_header ASSIGNING  <lw_orignal_hdr> WITH KEY ordid = <lw_header>-ordid.
          IF sy-subrc = 0.
            <lw_header>-belnr = CONV belnr( |{ <lw_header>-belnr ALPHA = IN }| ).
            <lw_orignal_hdr>-belnr = <lw_header>-belnr.
            <lw_orignal_hdr>-aedat = sy-datum.
            <lw_orignal_hdr>-status = '16'.
            <lw_orignal_hdr>-errmsg = ''.
          ENDIF.
        ELSE.
          <lw_orignal_hdr>-errmsg = 'Account Document is not created'.
        ENDIF.

      WHEN '16'. " Create e-Invoice and IRN
        DATA irn TYPE j_1ig_invrefnum-irn.

        SELECT SINGLE irn
          FROM j_1ig_invrefnum INTO irn
          WHERE bukrs = <lw_header>-bukrs AND
                docno = <lw_header>-vbeln_vf AND
                doc_year = <lw_header>-gjahr AND
                doc_type  = 'INV'.
        IF  sy-subrc <> 0.
          CALL METHOD lo_main1->create_irn_qrcode
            EXPORTING
              distributor_code = <lw_header>-distb
              vbeln            = <lw_header>-vbeln_vf
              bukrs            = <lw_header>-bukrs
              gjahr            = <lw_header>-gjahr
*             DATE             = <lw_header>
            IMPORTING
              irn              = irn
              signed_inv       = DATA(signed_inv)
              signed_qrcode    = DATA(signed_qrcode)
              return           = DATA(return).
        ENDIF.
        IF irn IS NOT INITIAL AND return IS INITIAL.
          READ TABLE gt_header ASSIGNING  <lw_orignal_hdr> WITH KEY ordid = <lw_header>-ordid.
          IF sy-subrc = 0.
            <lw_orignal_hdr>-aedat = sy-datum.
            <lw_orignal_hdr>-status = '17'.
            <lw_orignal_hdr>-errmsg = ''.
          ENDIF.
        ELSE.
          READ TABLE gt_header ASSIGNING  <lw_orignal_hdr> WITH KEY ordid = <lw_header>-ordid.
          IF sy-subrc = 0.
            <lw_orignal_hdr>-errmsg = return.
          ENDIF.
        ENDIF.
      WHEN '17'. " Create e-WayBill

      WHEN '18'. " From Status 18 - e-WayBill to be Status 19 Complete
        READ TABLE gt_header ASSIGNING  <lw_orignal_hdr> WITH KEY ordid = <lw_header>-ordid.
        IF sy-subrc = 0.
          <lw_orignal_hdr>-aedat = sy-datum.
          <lw_orignal_hdr>-status = '19'.
          <lw_orignal_hdr>-errmsg = ''.
        ENDIF.
    ENDCASE.
    READ TABLE gt_header ASSIGNING <lw_orignal_hdr> WITH KEY ordid = <lw_header>-ordid.
    LOOP AT gt_return INTO DATA(lw_return) WHERE type = 'E'.
      IF <lw_orignal_hdr>-errmsg IS INITIAL.
        <lw_orignal_hdr>-errmsg = |Error: : { lw_return-message } { lw_return-message_v1 } { lw_return-message_v2 } |.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
ENDFORM.
