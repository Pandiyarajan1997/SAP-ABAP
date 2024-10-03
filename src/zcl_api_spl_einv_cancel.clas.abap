CLASS zcl_api_spl_einv_cancel DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_http_extension.
    TYPES:
      BEGIN OF ty_req,
        orderid  TYPE zorder_id,
        salesdoc TYPE vbeln,
      END OF ty_req .

    DATA gs_input TYPE ty_req .
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS Validation EXPORTING msgtype   TYPE bapi_mtype
                                 message   TYPE string
                                 iv_status TYPE zscm_status
                                 ls_header TYPE zsd_scm_header
                                 ls_order  TYPE zsd_spl_sale_hd.
    METHODS Cancellation_process IMPORTING VALUE(iv_status) TYPE zscm_status
                                           ls_header        TYPE zsd_scm_header
                                           ls_order         TYPE zsd_spl_sale_hd
                                 EXPORTING lv_type          TYPE bapi_mtype
                                           lv_msg           TYPE string.
    METHODS Einvoice_cancellation IMPORTING inv_no  TYPE vbeln_vf
                                  EXPORTING lv_msg  TYPE string
                                            lv_type TYPE bapi_mtype.
    METHODS invoice_cancellation IMPORTING inv_no   TYPE vbeln_vf
                                 EXPORTING lv_msg   TYPE string
                                           lv_type  TYPE bapi_mtype
                                           rtrn_inv TYPE vbeln .
    METHODS pgi_reverse  IMPORTING delivery_no    TYPE vbeln_vl
                         EXPORTING lv_msg         TYPE string
                                   VALUE(lv_type) TYPE bapi_mtype.

    METHODS delivery_cancel IMPORTING delivery_no TYPE vbeln_vl
                            EXPORTING type        TYPE bapi_mtype
                                      message     TYPE string .

    METHODS  sorder_cancel IMPORTING salesorder_no TYPE vbeln
                                     ls_stat       TYPE zscm_status  OPTIONAL
                           EXPORTING lv_msg        TYPE string
                                     lv_type       TYPE bapi_mtype .

ENDCLASS.



CLASS zcl_api_spl_einv_cancel IMPLEMENTATION.
  METHOD if_http_extension~handle_request.
    IF server->request->get_method( ) EQ 'POST'.
      TYPES: BEGIN OF ty_res,
               orderid  TYPE zorder_id,
               salesdoc TYPE vbeln,
*               distributor TYPE kunnr,
*             retailer    TYPE kunnr,
               type     TYPE bapi_mtype,
               msg      TYPE string,
             END OF ty_res.

      DATA : lv_body  TYPE string.

      DATA: gs_response TYPE ty_res,
            lv_dms      TYPE zorder_id.

      DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
      CREATE OBJECT lo_log_upd.

      CALL METHOD server->request->get_cdata RECEIVING data = DATA(lv_data) .
** deserialize the INPUT our required INPUT ***
      /ui2/cl_json=>deserialize(
     EXPORTING
       json         = lv_data
       pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
     CHANGING
       data         = gs_input ).
***************input alpha conversion************
*      gs_input-distributor = |{ gs_input-distributor ALPHA = IN }|.
*    gs_input-retailer    = |{ gs_input-retailer ALPHA = IN }|.
      gs_input-salesdoc    = |{ gs_input-salesdoc ALPHA = IN }|.
***************call the validations**********
      CALL METHOD validation
        IMPORTING
          msgtype   = DATA(type)
          message   = DATA(msg)
          iv_status = DATA(lv_status)
          ls_header = DATA(gs_header)
          ls_order  = DATA(ls_order).

      IF msg IS INITIAL.
        CALL METHOD cancellation_process
          EXPORTING
            iv_status = lv_status
            ls_header = gs_header
            ls_order  = ls_order
          IMPORTING
            lv_type   = type
            lv_msg    = msg.
      ENDIF.
      gs_response = VALUE #( "distributor = gs_input-distributor
                             orderid = gs_input-orderid
                             salesdoc = gs_input-salesdoc
                             type = SWITCH #( type WHEN 'E' THEN 'E' WHEN '' THEN 'E' ELSE 'S' )
                             msg = msg ).
      CLEAR:lv_body .
** serialize the output for response ***
      /ui2/cl_json=>serialize(
      EXPORTING
       data         =  gs_response
       pretty_name  = /ui2/cl_json=>pretty_mode-user
      RECEIVING
       r_json         = lv_body ).
**************store the log table*************
      CALL METHOD lo_log_upd->log_entry_store
        EXPORTING
          apiname         = 'SPL_INV_CNCL'
          ijson           = lv_data
          ojson           = lv_body
*         distributor     = gs_input-distributor
*         retailer        = gs_input-retailer
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

  METHOD validation.
******************local variable dec*************
    DATA : lv_sec     TYPE int4,
           lv_rtrninv TYPE vbeln,
           lv_days    TYPE i.
*           lv_status  TYPE zscm_status. "processing status 14 - inv , 13 - pgi , 12 - delivery , 11 - sorder
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
    SELECT SINGLE FROM zsd_spl_sale_hd FIELDS * WHERE ordid = @gs_input-orderid
                                         AND   vbeln       = @gs_input-salesdoc
                                      "   AND    kunag  = @gs_input-distributor
                               INTO @ls_order.
    IF sy-subrc NE 0.
      message = | Sales order is not created - { gs_input-salesdoc } |.
      EXIT.
    ELSEIF ls_order-sostat = '12'.
      message = | Already Sales order is Cancelled - { gs_input-salesdoc } |.
      msgtype   = 'S'.
      EXIT.
    ENDIF.

*************check the duplicate sales return*********
    SELECT SINGLE FROM zsd_scm_header FIELDS * WHERE order_id = @gs_input-orderid INTO @ls_header.
    IF sy-subrc NE 0.
      iv_status = '11'.
    ELSEIF sy-subrc = 0 AND ls_header-status = 19 AND ls_header-msgtyp = 'S'.
      message = | Already Invoice to Salesorder is Cancelled |.
      msgtype    = 'S'.
      EXIT.
    ELSE.
      IF ls_header-cncl_stat IS INITIAL.
************check the processing status*********
        IF ls_header-invoice_no IS NOT INITIAL.
          SELECT SINGLE FROM j_1ig_invrefnum FIELDS * WHERE docno = @ls_header-invoice_no INTO @DATA(ls_einv).
          IF sy-subrc = 0.
            iv_status = '15'.          ""start from E-invoice cancellation
          ELSE.
            iv_status = '14'.     "start from invoice cancellation
          ENDIF.
        ELSEIF ls_header-gdsmvt_no IS NOT INITIAL.
          iv_status = '13'.     "start from PGI cancellation
        ELSEIF ls_header-delivery_no IS NOT INITIAL.
          iv_status = '12'.     "start from delivery cancellation
        ELSEIF ls_header-delivery_no IS INITIAL.
          iv_status = '11'.     "start from salesorder cancellation
        ENDIF.
      ELSE.
************In case,message type is success*********
        IF ls_header-msgtyp = 'S'.
          iv_status = ls_header-status - 1.
        ELSE.
          iv_status   = ls_header-status.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD cancellation_process.
****************process only for invoice is created***************
    IF iv_status = '15' OR iv_status = '14'.
      DATA(ls_stat) = iv_status.
      CALL METHOD einvoice_cancellation
        EXPORTING
          inv_no  = ls_header-invoice_no           " Billing Document
        IMPORTING
          lv_msg  = lv_msg
          lv_type = lv_type.
      IF lv_type = 'S'.
        iv_status = '14'.
      ELSE.
        EXIT.
      ENDIF.
    ENDIF.
**************Invoice cancellation process***********
    IF iv_status = 14 AND ls_header-invoice_no IS NOT INITIAL.
      ls_stat = iv_status.
      CALL METHOD invoice_cancellation
        EXPORTING
          inv_no   = ls_header-invoice_no           " Billing Document
        IMPORTING
          lv_msg   = lv_msg
          lv_type  = lv_type
          rtrn_inv = DATA(lv_rtrninv).
      IF lv_type = 'S'.
        iv_status = '13'.
      ELSE.
        EXIT.
      ENDIF.
    ENDIF.

**************PGI reverse process*********
    IF iv_status = 13 AND ls_header-delivery_no IS NOT INITIAL AND ls_header-gdsmvt_no IS NOT INITIAL.
      CALL METHOD pgi_reverse
        EXPORTING
          delivery_no = ls_header-delivery_no   " Delivery no
        IMPORTING
          lv_msg      = lv_msg
          lv_type     = lv_type.
      IF lv_type = 'S'.
        iv_status = '12'.
      ELSE.
        EXIT.
      ENDIF.
    ENDIF.

*****************delete the delivery************
    IF iv_status = 12 AND ls_header-delivery_no IS NOT INITIAL.
      CALL METHOD delivery_cancel
        EXPORTING
          delivery_no = ls_header-delivery_no  " Delivery no
        IMPORTING
          type        = lv_type
          message     = lv_msg.
      IF lv_type = 'S'.
        iv_status = '11'.
      ELSE.
        EXIT.
      ENDIF.
    ENDIF.

**************Sales order cancellation**********
    IF iv_status = 11 AND ls_order-vbeln IS NOT INITIAL.
      CALL METHOD sorder_cancel
        EXPORTING
          salesorder_no = ls_order-vbeln   " Sales and Distribution Document Number
          ls_stat       = ls_stat
        IMPORTING
          lv_msg        = lv_msg
          lv_type       = lv_type.
    ENDIF.


  ENDMETHOD.

  METHOD invoice_cancellation.
    DATA : lt_return  TYPE TABLE OF bapireturn1,
           lt_success TYPE TABLE OF bapivbrksuccess.

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
    ELSE.
      LOOP AT lt_return INTO ls_return.
        lv_msg  = |{ ls_return-message } , { lv_msg }|.
        lv_type = 'E'.
      ENDLOOP.
    ENDIF.
**********Update the response to the log table*************
    UPDATE zsd_scm_header  SET    cncl_msg   = lv_msg   cncl_type   = lv_type
                                     cncl_stat = '14' cncl_date = sy-datum cncl_time = sy-uzeit
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
    UPDATE zsd_scm_header SET  cncl_msg   = lv_msg   cncl_type   = lv_type
                                     cncl_stat = '13' cncl_date = sy-datum cncl_time = sy-uzeit
                          WHERE order_id  = gs_input-orderid.
    IF sy-subrc = 0.
      COMMIT WORK.
    ENDIF.
  ENDMETHOD.

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
        IF ls_return-id = 'VL' AND ls_return-number = 066.
          message = 'This document cannot be deleted'.
          EXIT.
        ENDIF.
        message = |{ ls_return-message } , { message }|.
      ENDLOOP.
      type    = 'E'.
    ENDIF.
**********Update the response to the log table*************
    UPDATE zsd_scm_header  SET   cncl_msg   = message   cncl_type   = type
                                     cncl_stat = '12' cncl_date = sy-datum cncl_time = sy-uzeit
                           WHERE order_id    = gs_input-orderid.
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
        UPDATE zsd_spl_sale_hd SET   sostat  = lv_status     "cncl_date = sy-datum
                                     "cncl_by = sy-uname  cncl_time = sy-uzeit
                                     msg     = lv_msg
                               WHERE ordid  = gs_input-orderid.
        IF sy-subrc = 0.
          COMMIT WORK.
        ENDIF.
      ENDIF.
**********Update the response to the scm log table*************
      IF ls_stat IS NOT INITIAL.
        lv_msg = 'Invoice to Sales order cancelled'.
      ENDIF.
      UPDATE zsd_scm_header  SET    cncl_msg   = lv_msg   cncl_type   = lv_type
                                     cncl_stat = lv_status cncl_date = sy-datum cncl_time = sy-uzeit
                             WHERE order_id    = gs_input-orderid.
      IF sy-subrc = 0.
        COMMIT WORK.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD einvoice_cancellation.
    DATA:lv_days       TYPE i,
         lv_sec        TYPE int4,
         et_error_msg  TYPE bapiret2_t,
         im_cancel_out TYPE zsd_irn_cout.
***************Get the invoice created date************
    SELECT SINGLE vbeln,erdat FROM vbrk INTO @DATA(ls_vbrk) WHERE vbeln = @inv_no.
******************check the invoice cancellation duration exceeded or not
*       SELECT SINGLE * FROM zsd_scm_header INTO @DATA(ls_inv) WHERE invoice_no = @ls_header-invoice_no.
*         IF sy-subrc = 0.
** Get the Directory Range from TVARVC
*          SELECT SINGLE * FROM  tvarvc INTO @DATA(ls_range)
*                   WHERE  name = 'INV_CANCL_DAYS' AND type = 'P'.
*          IF sy-subrc = 0.
*            lv_days = ls_range-low.
*          ENDIF.
*
*          IF ls_inv-cncl_date - ls_vbrk-erdat GT lv_days.
*            lv_msg = | Invoice Cancellation Time Period Exceed - { ls_header-invoice_no } |.
*            EXIT.
*          ENDIF.
*        ENDIF.
**********check the E-invoice details***********
    SELECT SINGLE * FROM j_1ig_invrefnum WHERE docno = @inv_no  INTO @DATA(ls_jinvref) .
    IF sy-subrc = 0.
************check the 24 hours**************
      CALL FUNCTION 'SALP_SM_CALC_TIME_DIFFERENCE'
        EXPORTING
          date_1  = ls_jinvref-erdat
          time_1  = ls_jinvref-erzet
          date_2  = sy-datum
          time_2  = sy-uzeit
        IMPORTING
          seconds = lv_sec.

      IF lv_sec GT '84600'.
        lv_msg = | E-Invoice Cancellation Time Period Exceeded |.
        lv_type = 'E'.
        EXIT.
      ENDIF.
**********Call ClearTax IRN cancellation API**********
      CALL FUNCTION 'ZSD_FM_IRN_CANCEL'
        EXPORTING
          im_irn        = ls_jinvref-irn
          im_vbeln      = ls_jinvref-docno
        IMPORTING
*         ex_error      =
          ex_cancel_out = im_cancel_out
        TABLES
          t_error_msg   = et_error_msg
        EXCEPTIONS
          update_error  = 1
          OTHERS        = 2.

      IF et_error_msg[] IS INITIAL.
        ls_jinvref-irn = im_cancel_out-irn.
        ls_jinvref-irn_status = im_cancel_out-irn_status.
        ls_jinvref-cancel_date = im_cancel_out-cancel_date.
        ls_jinvref-ernam = im_cancel_out-ernam.
        UPDATE j_1ig_invrefnum FROM ls_jinvref.
**********Update the response to the log table*************
        lv_msg = 'E-Invoice cancelled successfully'.
        lv_type = 'S'.
        UPDATE zsd_scm_header  SET   cncl_msg   = lv_msg   cncl_type   = lv_type
                                     cncl_stat = '15' cncl_date = sy-datum cncl_time = sy-uzeit
                               WHERE order_id   = gs_input-orderid.
        IF sy-subrc = 0.
          COMMIT WORK.
        ENDIF.
      ELSE.
        lv_type = 'E'.
        LOOP AT et_error_msg INTO DATA(ls_err_msg).
          lv_msg = |{ ls_err_msg-message },{ lv_msg }|.
        ENDLOOP.
      ENDIF.
    ELSE.

    ENDIF.
  ENDMETHOD.

ENDCLASS.
