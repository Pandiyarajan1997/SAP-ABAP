class ZCL_API_DMS_EINV_CANCEL definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .

  types:
    BEGIN OF ty_resp,
        orderid     TYPE zorder_id,
        salesdoc    TYPE vbeln,
        distributor TYPE kunnr,
        retailer    TYPE kunnr,
      END OF ty_resp .

  data GS_INPUT type TY_RESP .

  methods INVOICE_CANCELLATION
    importing
      !INV_NO type VBELN_VF
    exporting
      !LV_MSG type STRING
      !LV_TYPE type BAPI_MTYPE
      !RTRN_INV type VBELN .
  methods PGI_REVERSE
    importing
      !DELIVERY_NO type VBELN_VL
    exporting
      !LV_MSG type STRING
      !LV_TYPE type BAPI_MTYPE .
  methods SORDER_CANCEL
    importing
      !SALESORDER_NO type VBELN
    exporting
      !LV_MSG type STRING
      !LV_TYPE type BAPI_MTYPE .
  methods VALIDATIONS
    exporting
      !TYPE type BAPI_MTYPE
      !MESSAGE type STRING .
  methods DELIVERY_CANCEL
    importing
      !DELIVERY_NO type VBELN_VL
    exporting
      !TYPE type BAPI_MTYPE
      !MESSAGE type STRING .
  PROTECTED SECTION.
private section.
ENDCLASS.



CLASS ZCL_API_DMS_EINV_CANCEL IMPLEMENTATION.


  METHOD delivery_cancel.

    DATA : header_data    TYPE  bapiobdlvhdrchg,
           header_control TYPE  bapiobdlvhdrctrlchg,
           delivery       TYPE  bapiobdlvhdrchg-deliv_numb,
           lt_return      TYPE TABLE OF bapiret2.

**********header data passing************
    header_data-deliv_numb    = delivery_no.
    header_control-deliv_numb = delivery_no.
    delivery                  = delivery_no.
    header_control-dlv_del    = 'X'.

    CALL FUNCTION 'BAPI_OUTB_DELIVERY_CHANGE'
      EXPORTING
        header_data    = header_data
        header_control = header_control
        delivery       = delivery
      TABLES
        return         = lt_return.
***********check the bapi response************
    IF lt_return IS INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
      message = 'Delivery Deleted'.
      type    = 'S'.
    ELSE.
      LOOP AT lt_return INTO DATA(ls_return).
        message = |{ ls_return-message } , { message }|.
      ENDLOOP.
      type    = 'E'.
    ENDIF.
**********Update the response to the log table*************
    UPDATE zsd_scm_hd_dms  SET   cncl_msg    = message cncl_type  = type
                                 cncl_status = '12'
                           WHERE order_id    = gs_input-orderid.
    IF sy-subrc = 0.
      COMMIT WORK.
    ENDIF.
  ENDMETHOD.


  METHOD if_http_extension~handle_request.
    "Created by: Pandiarajan
    "Created on: 05.03.2024
    "Reference by: Ramakrishnan J
    "Purpose : Cancel the E-Invoice process through DMS
*-------------------------------------------------------------*

    TYPES: BEGIN OF ty_res,
             orderid     TYPE zorder_id,
             salesdoc    TYPE vbeln,
             distributor TYPE kunnr,
             retailer    TYPE kunnr,
             type        TYPE bapi_mtype,
             msg         TYPE string,
           END OF ty_res.

    DATA : lv_body  TYPE string.

    DATA: gs_response TYPE ty_res,
          lv_dms      TYPE zorder_id.

    DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
    CREATE OBJECT lo_log_upd.
************dms error log**********
    DATA: lo_errorlog_dms TYPE REF TO zcl_api_dms_error_log_entries.
    CREATE OBJECT lo_errorlog_dms.

    CALL METHOD server->request->get_cdata RECEIVING data = DATA(lv_data).

** deserialize the INPUT our required INPUT ***
    /ui2/cl_json=>deserialize(
   EXPORTING
     json         = lv_data
     pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
   CHANGING
     data         = gs_input ).
***************input alpha conversion************
    gs_input-distributor = |{ gs_input-distributor ALPHA = IN }|.
    gs_input-retailer    = |{ gs_input-retailer ALPHA = IN }|.
    gs_input-salesdoc    = |{ gs_input-salesdoc ALPHA = IN }|.
***************call the validations process**********
    CALL METHOD validations
      IMPORTING
        type    = gs_response-type
        message = gs_response-msg.
**************check the response***********
    IF gs_response-type IS INITIAL.
      gs_response-type = 'E'.
    ENDIF.
    gs_response-distributor = gs_input-distributor.
    gs_response-retailer    = gs_input-retailer.
    gs_response-orderid     = gs_input-orderid.
    gs_response-salesdoc    = gs_input-salesdoc.

    IF gs_response IS NOT INITIAL.
*********************error log***********
      SELECT SINGLE * FROM zsd_sale_hd_dms INTO @DATA(ls_sales) WHERE ordid = @gs_input-orderid.
      IF gs_response-type = 'E'.
        lo_errorlog_dms->log_entry_store(
          EXPORTING
            type                = 25
            status              = 10
            dms_orderid         = gs_input-orderid
            distributor         = ls_sales-distributor
            plant               = ls_sales-plant
            dealer              = ls_sales-kunag
            msg                 = gs_response-msg ).
      ENDIF.

      CLEAR:lv_body .
** serialize the output for response ***
      /ui2/cl_json=>serialize(
      EXPORTING
       data         =  gs_response
       pretty_name  = /ui2/cl_json=>pretty_mode-user
      RECEIVING
       r_json         = lv_body ).

***************store the log table*************
      CALL METHOD lo_log_upd->log_entry_store
        EXPORTING
          apiname         = 'DMS_INV_CNCL'
          ijson           = lv_data
          ojson           = lv_body
          distributor     = gs_input-distributor
          retailer        = gs_input-retailer
        EXCEPTIONS
          apiname_missing = 1
          json_missing    = 2
          OTHERS          = 3.
      IF sy-subrc <> 0.

      ENDIF.

*Set JSON Content-Type
      CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
      CALL METHOD server->response->set_cdata( data = lv_body ).
*******delete the double click check*********
      DELETE FROM zsd_invoice_dms WHERE invoice_no = gs_input-orderid.
    ENDIF.
  ENDMETHOD.


  METHOD invoice_cancellation.

    DATA : lt_return  TYPE TABLE OF bapireturn1,
           lt_success TYPE TABLE OF bapivbrksuccess,
           ls_inv     TYPE zdms_inv_credit.

************call the bapi for invoice cancellation*********
    CALL FUNCTION 'BAPI_BILLINGDOC_CANCEL1'
      EXPORTING
        billingdocument = inv_no
      TABLES
        return          = lt_return
        success         = lt_success.
***********check the bapi response************
    READ TABLE lt_return INTO DATA(ls_return) WITH KEY type = 'S'.
    IF sy-subrc = 0.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
***********check the bapi response************
      IF lt_success IS NOT INITIAL.
        READ TABLE lt_success INTO DATA(ls_success) INDEX 1.
        rtrn_inv = ls_success-bill_doc.
      ENDIF.
      lv_type = 'S'.
      lv_msg  = 'Invoice is cancelled'.

********************update the invoice details for automatic credit note******************
      SELECT SINGLE belnr,netwr,fkdat,gjahr,kunag FROM vbrk INTO @DATA(ls_vbrk)
                                                  WHERE vbeln = @inv_no.
      IF sy-subrc = 0.
************************Get the distributor plant*******************
        SELECT SINGLE werks FROM kna1 INTO @DATA(lv_plant) WHERE kunnr = @gs_input-distributor.
        ls_inv-invoice_no   = inv_no.
        ls_inv-inv_type     = 'C'.
        ls_inv-inv_date     = ls_vbrk-fkdat.
        ls_inv-ac_doc       = ls_vbrk-belnr.
        ls_inv-fisc_year    = ls_vbrk-gjahr.
        ls_inv-distributor  = gs_input-distributor.
        ls_inv-dist_plant   = lv_plant.
        ls_inv-dealer       = ls_vbrk-kunag.
        ls_inv-created_on   = sy-datum.
        ls_inv-created_by   = sy-uname.
        ls_inv-created_time = sy-uzeit.
        ls_inv-status       = '10'.
        ls_inv-net_amount   = ls_vbrk-netwr.
        MODIFY zdms_inv_credit FROM ls_inv.
        CLEAR ls_inv.
      ENDIF.

    ELSE.
      LOOP AT lt_return INTO ls_return.
        lv_msg  = |{ ls_return-message } , { lv_msg }|.
        lv_type = 'E'.
      ENDLOOP.
    ENDIF.
**********Update the response to the log table*************
    UPDATE zsd_scm_hd_dms  SET   cncl_msg   = lv_msg   cncl_type   = lv_type
                                 rev_inv    = rtrn_inv cncl_status = '14'
                           WHERE order_id   = gs_input-orderid.
    IF sy-subrc = 0.
      COMMIT WORK.
    ENDIF.

  ENDMETHOD.


  METHOD pgi_reverse.
    DATA : lv_mblnr TYPE mblnr,
           lt_mesg  TYPE TABLE OF bdcmsgcoll,
           ls_bdc   TYPE bdcdata,
           lt_bdc   TYPE TABLE OF bdcdata.

    WAIT UP TO 1 SECONDS.
***********Unlock the delivery no************
    CALL FUNCTION 'DEQUEUE_EVVBLKE'
      EXPORTING
        vbeln = delivery_no.

*******************Call the pgi reverse fm - VL09***********
*    CALL FUNCTION 'JIT06_REVERSE_GOODS_ISSUE'
*      EXPORTING
*        i_vbeln = delivery_no
*        i_budat = delivery_date
*        i_tcode = 'VL09'
*        i_vbtyp = 'J'
*      TABLES
*        t_mesg  = lt_mesg.
    CLEAR ls_bdc.
    ls_bdc-program  = 'RVV50L09'.
    ls_bdc-dynpro   = '1000'.
    ls_bdc-dynbegin = 'X'.
    APPEND ls_bdc TO lt_bdc.

    CLEAR ls_bdc.
    ls_bdc-fnam  = 'BDC_OKCODE'.
    ls_bdc-fval   = '=ONLI'.
    APPEND ls_bdc TO lt_bdc.

    CLEAR ls_bdc.
    ls_bdc-fnam  = 'I_VBELN-LOW'.
    ls_bdc-fval   = delivery_no.
    APPEND ls_bdc TO lt_bdc.

    CLEAR ls_bdc.
    ls_bdc-fnam  = 'LF_ANAUS'.
    ls_bdc-fval   = 'X'.
    APPEND ls_bdc TO lt_bdc.

******************call the tcode vl09*****************
    CALL TRANSACTION 'VL09' USING lt_bdc
                            MODE 'N'
                            UPDATE 'A'
                            MESSAGES INTO lt_mesg.
************response check**********
    SELECT SINGLE mblnr,mjahr FROM mkpf INTO @DATA(ls_mkpf) WHERE le_vbeln = @delivery_no
                                                            AND   tcode2   = 'VL09'.
    IF sy-subrc = 0.
      lv_type = 'S'.
      lv_msg  = 'PGI reversed successfully'.
    ELSE.
      lv_type = 'E'.
      lv_msg  = 'PGI is not Reversed - Retry'.
    ENDIF.
**********Update the response to the log table*************
    UPDATE zsd_scm_hd_dms SET cncl_msg    = lv_msg cncl_type   = lv_type
                              cncl_status = '13'   rev_pgi     = ls_mkpf-mblnr
                          WHERE order_id  = gs_input-orderid.
    IF sy-subrc = 0.
      COMMIT WORK.
    ENDIF.
  ENDMETHOD.


  METHOD sorder_cancel.
    DATA : lt_item    TYPE TABLE OF  bapisditm,
           lt_return  TYPE TABLE OF  bapiret2,
           lt_itemx   TYPE TABLE OF  bapisditmx,
           ls_headerx TYPE  bapisdh1x,
           lv_status  TYPE zscm_status.

** fetch the sales order material ***
    SELECT posnr FROM vbap
                 INTO TABLE @DATA(lt_posnr)
                 WHERE vbeln = @salesorder_no.
    IF sy-subrc NE 0.
      lv_msg = 'Incorrect salesorder_no'.
      lv_type = 'E'.
      EXIT.
    ENDIF.
    LOOP AT lt_posnr ASSIGNING FIELD-SYMBOL(<fs_posnr>).

      APPEND VALUE bapisditm( itm_number = <fs_posnr>
                              reason_rej = '07' ) TO lt_item.

      APPEND VALUE bapisditmx( itm_number = <fs_posnr>
                               reason_rej = abap_true
                               updateflag = 'U' ) TO lt_itemx.

    ENDLOOP.

    IF lt_item IS NOT INITIAL.

      ls_headerx-updateflag = 'U'.

**** CHANGE_SALESORDER_REJECT_REASONS ****

      CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
        EXPORTING
          salesdocument    = salesorder_no
          order_header_inx = ls_headerx
        TABLES
          return           = lt_return
          order_item_in    = lt_item
          order_item_inx   = lt_itemx.

      READ TABLE lt_return INTO DATA(ls_return) WITH KEY type = 'E'.
      IF sy-subrc = 0.
        lv_msg    = ls_return-message.
        lv_type   = 'E'.
        lv_status = '11'.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
        lv_type   = 'S'.
        lv_msg    = |Sales order Closed successfully - { salesorder_no }|.
        lv_status = '19'.

**********Update the response to the sales order log table*************
        UPDATE zsd_sale_hd_dms SET   sostat  = '12'      cncl_date = sy-datum
                                     cncl_by = sy-uname  cncl_time = sy-uzeit
                                     msg     = lv_msg
                               WHERE ordid  = gs_input-orderid.
        IF sy-subrc = 0.
          COMMIT WORK.
        ENDIF.
      ENDIF.
**********Update the response to the scm log table*************
      UPDATE zsd_scm_hd_dms  SET   cncl_msg    = lv_msg    cncl_type = lv_type
                                   cncl_status = lv_status cncl_date = sy-datum
                                   cncl_by     = sy-uname  cncl_time = sy-uzeit
                                   status      = lv_status
                             WHERE order_id    = gs_input-orderid.
      IF sy-subrc = 0.
        COMMIT WORK.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD validations.
*******************local variable dec*************
    DATA : lv_sec     TYPE int4,
           lv_rtrninv TYPE vbeln,
           lv_days    TYPE i,
           lv_status  TYPE zscm_status. "processing status 14 - inv , 13 - pgi , 12 - delivery , 11 - sorder
***************einv cancellation class declaration************
    DATA : lo_main1 TYPE REF TO zcl_dms_einvoice_process.
    CREATE OBJECT lo_main1.

*Multiple click on front end so avoiding duplicates
    SELECT SINGLE * FROM zsd_invoice_dms
      INTO @DATA(l_dupl_ordid)
      WHERE invoice_no = @gs_input-orderid.
    IF sy-subrc = 0.
      message = |Cancellation - { gs_input-orderid } in processing |.
      EXIT.
    ELSE.
      DATA(l_orderid_chk) = VALUE zsd_invoice_dms( mandt      = sy-mandt
                                                   invoice_no = gs_input-orderid ).
      INSERT zsd_invoice_dms FROM l_orderid_chk.
    ENDIF.

*************check the salesorder created or not*************
    SELECT SINGLE * FROM zsd_sale_hd_dms  INTO @DATA(ls_order) WHERE ordid       = @gs_input-orderid
                                                               AND   vbeln       = @gs_input-salesdoc
                                                               AND   distributor = @gs_input-distributor
                                                               AND   kunag       = @gs_input-retailer.
    IF sy-subrc NE 0.
      message = | Sales order is not created - { gs_input-salesdoc } |.
      EXIT.
    ELSEIF ls_order-sostat = '12'.
      message = | Already Sales order is Cancelled - { gs_input-salesdoc } |.
      type    = 'S'.
      EXIT.
    ENDIF.
*************check the duplicate sales return*********
    SELECT SINGLE * FROM zsd_scm_hd_dms INTO @DATA(ls_header) WHERE order_id = @gs_input-orderid.
    IF sy-subrc NE 0.
      lv_status = '11'.
    ELSEIF sy-subrc = 0 AND ls_header-cncl_status = 19 AND ls_header-cncl_type = 'S'.
*      "IRN Details Fetch
*      SELECT SINGLE * FROM zdms_invoice_irn INTO @DATA(l_irn)
*                      WHERE bukrs    = 'DMS1'
*                      AND   docno    = @ls_header-invoice_no
*                      AND   doc_year = @ls_header-gjahr.

      message = | Already Invoice to Salesorder is Cancelled |.
      type    = 'S'.
      EXIT.
    ELSE.
      IF ls_header-cncl_status IS INITIAL.
************check the processing status*********
        IF ls_header-invoice_no IS NOT INITIAL.
          lv_status = '14'.     "start from invoice cancellation
        ELSEIF ls_header-gdsmvt_no IS NOT INITIAL.
          lv_status = '13'.     "start from PGI cancellation
        ELSEIF ls_header-delivery_no IS NOT INITIAL.
          lv_status = '12'.     "start from delivery cancellation
        ELSEIF ls_header-delivery_no IS INITIAL.
          lv_status = '11'.     "start from salesorder cancellation
        ENDIF.
      ELSE.
************Incase message type is success*********
        IF ls_header-cncl_type = 'S'.
          lv_status = ls_header-cncl_status - 1.
        ELSE.
          lv_status   = ls_header-cncl_status.
        ENDIF.
      ENDIF.

**********check the retailer code*************
      SELECT SINGLE werks FROM kna1 INTO @DATA(ls_werks) WHERE kunnr = @gs_input-retailer.
      IF sy-subrc NE 0.
        message = | Retailer is not found - { gs_input-retailer }|.
        EXIT.
      ENDIF.
**********check the distributor code*************
      SELECT SINGLE werks FROM kna1 INTO ls_werks WHERE kunnr = gs_input-distributor.
      IF sy-subrc NE 0.
        message = | Distributor is not found - { gs_input-distributor }|.
        EXIT.
      ELSE.
        IF ls_werks IS INITIAL.
          message = | Distributor plant not maintained - { gs_input-distributor }|.
          EXIT.
        ENDIF.
      ENDIF.

****************process only for invoice is created***************
      IF lv_status = '14' OR lv_status = '15'.
***************Get the invoice created date************
        SELECT SINGLE vbeln,erdat FROM vbrk INTO @DATA(ls_vbrk) WHERE vbeln = @ls_header-invoice_no.
******************check the invoice cancellation time period exceed or not
        SELECT SINGLE * FROM zdms_inv_excep INTO @DATA(ls_inv) WHERE invoice_no = @ls_header-invoice_no.
        IF sy-subrc NE 0.
* Get the Directory Range from TVARVC
          SELECT SINGLE * FROM  tvarvc INTO @DATA(ls_range)
                   WHERE  name = 'INV_CANCL_DAYS' AND type = 'P'.
          IF sy-subrc = 0.
            lv_days = ls_range-low.
          ENDIF.

          IF sy-datum - ls_vbrk-erdat GT lv_days.
            message = | Invoice Cancellation Time Period Exceed - { ls_header-invoice_no } |.
            EXIT.
          ENDIF.
        ENDIF.

**********check the E-invoice details***********
        SELECT SINGLE * FROM zdms_invoice_irn INTO @DATA(ls_einv) WHERE docno  = @ls_header-invoice_no
                                                                  AND   distrb = @gs_input-distributor
                                                                  AND   dealer = @gs_input-retailer.
        IF sy-subrc EQ 0 AND ls_einv-irn_status = 'ACT'.
************check the 24 hours**************
          CALL FUNCTION 'SALP_SM_CALC_TIME_DIFFERENCE'
            EXPORTING
              date_1  = ls_einv-erdat
              time_1  = ls_einv-erzet
              date_2  = sy-datum
              time_2  = sy-uzeit
            IMPORTING
              seconds = lv_sec.

          IF lv_sec GT '84600'.
            message = | E-Invoice Cancellation Time Period Exceed |.
            EXIT.
          ENDIF.
****************call the cancellation irn******************
          lo_main1->cancel_irn_qrcode(
            EXPORTING
              distributor_code = gs_input-distributor      " Customer Number
              vbeln            = ls_header-invoice_no      " Billing Document
              bukrs            = 'DMS1'                    " Company Code
              gjahr            = ls_einv-doc_year          " Fiscal Year
              irn              = ls_einv-irn               " Invoice Reference Number
           IMPORTING
              return           = message
              type             = type ).
***********fill the response***********
          IF type = 'S'.
            type    = 'S'.
            message = 'E-Invoice cancelled successfully'.
**********update the response to the LOG table*************
            UPDATE zsd_scm_hd_dms  SET   cncl_msg   = message  cncl_type = type
                                         cncl_status = '15'
                                   WHERE order_id   = gs_input-orderid.
            IF sy-subrc = 0.
              COMMIT WORK.
            ENDIF.
            lv_status = '14'.
          ELSE.
            type = 'E'.
**********update the response to the LOG table*************
            UPDATE zsd_scm_hd_dms  SET   cncl_msg    = message   cncl_type = type
                                         cncl_status = '15'
                                   WHERE order_id   = gs_input-orderid.
            IF sy-subrc = 0.
              COMMIT WORK.
            ENDIF.
            EXIT.
          ENDIF.
        ELSE.
          lv_status = 14.
        ENDIF.
      ENDIF.
    ENDIF.

**************Invoice cancellation process***********
    IF lv_status = 14 AND ls_header-invoice_no IS NOT INITIAL.
      CALL METHOD invoice_cancellation
        EXPORTING
          inv_no   = ls_header-invoice_no           " Billing Document
        IMPORTING
          lv_msg   = message
          lv_type  = type
          rtrn_inv = lv_rtrninv.
      IF type = 'S'.
        lv_status = '13'.
      ELSE.
        EXIT.
      ENDIF.
    ENDIF.

**************PGI reverse process*********
    IF lv_status = 13 AND ls_header-delivery_no IS NOT INITIAL AND ls_header-gdsmvt_no IS NOT INITIAL.
      CALL METHOD pgi_reverse
        EXPORTING
          delivery_no   = ls_header-delivery_no   " Delivery no
        IMPORTING
          lv_msg        = message
          lv_type       = type.
      IF type = 'S'.
        lv_status = '12'.
      ELSE.
        EXIT.
      ENDIF.
    ENDIF.

*****************delete the delivery************
    IF lv_status = 12 AND ls_header-delivery_no IS NOT INITIAL.
      CALL METHOD delivery_cancel
        EXPORTING
          delivery_no = ls_header-delivery_no  " Delivery no
        IMPORTING
          type        = type
          message     = message.
      IF type = 'S'.
        lv_status = '11'.
      ELSE.
        EXIT.
      ENDIF.
    ENDIF.

**************Sales order cancellation**********
    IF lv_status = 11 AND ls_order-vbeln IS NOT INITIAL.
      CALL METHOD sorder_cancel
        EXPORTING
          salesorder_no = ls_order-vbeln   " Sales and Distribution Document Number
        IMPORTING
          lv_msg        = message
          lv_type       = type.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
