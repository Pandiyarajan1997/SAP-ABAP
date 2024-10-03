*&---------------------------------------------------------------------*
*& Include          ZSD_SCM_SALES_TO_INVOICE_CLS
*&---------------------------------------------------------------------*
CLASS sales_to_invoice DEFINITION.
  PUBLIC SECTION.

    METHODS  background_job_chk
      IMPORTING pname TYPE syst-cprog
      CHANGING  l_msg TYPE char50.

    METHODS:
      delivery_process, "invoice_process,
      alv_display.
ENDCLASS.

CLASS sales_to_invoice IMPLEMENTATION.

  METHOD background_job_chk.
    "Checking Previous Job is Running or Not
    CALL FUNCTION 'ZCHECK_ACTIVE_BACKGRD_JOB'
      EXPORTING
        program_name         = pname
      EXCEPTIONS
        program_name_missing = 1
        excess_job           = 2
        OTHERS               = 3.
    IF sy-subrc <> 0.
      CASE sy-subrc.
        WHEN '1'.    l_msg = |Program Name is Missing|.
        WHEN '2'.    l_msg = |More than One Background Job is Running|.
        WHEN OTHERS. l_msg = |Other Errors|.
      ENDCASE.
    ENDIF.
  ENDMETHOD.

**----- Delivery to Post Goods Issue Process ------*
  METHOD delivery_process.
    REFRESH: gt_delivery.
    SELECT * FROM zsd_scm_header
             INTO TABLE gt_delivery
             WHERE order_id IN s_ordid
             AND vbeln IN s_vbeln
             AND msgtyp NE 'E'.
    IF sy-subrc = 0.
      LOOP AT gt_delivery ASSIGNING FIELD-SYMBOL(<fs_delivery>).
        CLEAR: gs_update.
        PERFORM delivery_to_pgi USING <fs_delivery> CHANGING gs_update.
        MODIFY zsd_scm_header FROM gs_update.
        PERFORM invoice_to_irn USING gs_update CHANGING gs_update.
        MODIFY zsd_scm_header FROM gs_update.
        APPEND gs_update TO gt_alv.
        CLEAR gs_update.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
**---- Invoice & E-invoice Creation ----*
*  METHOD invoice_process.
*    REFRESH: gt_invoice.
*    SELECT * FROM zsd_scm_header
*             INTO TABLE gt_invoice
*             WHERE order_id IN s_ordid
*             AND vbeln IN s_vbeln
*             AND delivery_no NE space
*             AND gdsmvt_no NE space
*             AND msgtyp NE 'E'.
*    IF sy-subrc = 0.
*      LOOP AT gt_invoice ASSIGNING FIELD-SYMBOL(<fs_invoice>).
*        CLEAR: gs_update.
*
*        MODIFY zsd_scm_header FROM gs_update.
*        APPEND gs_update TO gt_alv.
*      ENDLOOP.
*    ENDIF.
*  ENDMETHOD.
*---- ALV Display ----*
  METHOD alv_display.
    DATA: lo_gr_alv       TYPE REF TO cl_salv_table, " Variables for ALV properties
          lo_gr_functions TYPE REF TO cl_salv_functions_list.

    DATA: lo_grid        TYPE REF TO cl_salv_form_layout_grid, " Variables for header
          lo_layout_logo TYPE REF TO cl_salv_form_layout_logo,
          lo_content     TYPE REF TO cl_salv_form_element,

          lv_title       TYPE string,
          lv_rows        TYPE string.


    DATA: lo_layout TYPE REF TO cl_salv_layout, " Variables for enabling Save button
          lv_key    TYPE salv_s_layout_key.

    DATA: lo_display TYPE REF TO cl_salv_display_settings. " Variable for layout settings

    DATA: lo_selections TYPE REF TO cl_salv_selections, " Variables for selection mode and column properties
          lo_columns    TYPE REF TO cl_salv_columns,
          lo_column     TYPE REF TO cl_salv_column_table.
    DATA: lr_aggregations TYPE REF TO cl_salv_aggregations.
    DATA: lr_groups TYPE REF TO cl_salv_sorts .
    DATA: toolbar TYPE REF TO cl_salv_functions_list .
* create the alv object
    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = lo_gr_alv
          CHANGING
            t_table      = gt_alv.
      CATCH cx_salv_msg.
    ENDTRY.
* Fit the columns
    lo_columns = lo_gr_alv->get_columns( ).
    lo_columns->set_optimize( 'X' ).
* Apply zebra style to lv_rows
    lo_display = lo_gr_alv->get_display_settings( ).
    lo_display->set_striped_pattern( cl_salv_display_settings=>true ).

    TRY.
        lo_column ?= lo_columns->get_column( 'MANDT' ).
        lo_column->set_visible( if_salv_c_bool_sap=>false ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'IRN' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'IRN Flag' ).
        lo_column->set_medium_text( 'IRN Flag' ).
        lo_column->set_short_text( 'IRN Flag' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'EWAYBILL' ).
        lo_column->set_visible( if_salv_c_bool_sap=>false ).
        lo_column->set_long_text( 'IRN Flag' ).
        lo_column->set_medium_text( 'IRN Flag' ).
        lo_column->set_short_text( 'IRN Flag' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'MSG' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Message' ).
        lo_column->set_medium_text( 'Message' ).
        lo_column->set_short_text( 'Message' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    lo_gr_alv->display( ).
  ENDMETHOD.

ENDCLASS.
*Delivery to Post Goods Issue Perfor
*& Form delivery_to_pgi
*&---------------------------------------------------------------------*
FORM delivery_to_pgi  USING    p_delivery TYPE zsd_scm_header
                      CHANGING p_lv_log TYPE zsd_scm_header.
  DATA: lv_vbeln TYPE vbeln_vl,
        lv_mblnr TYPE mblnr.

  p_lv_log = p_delivery.
*Outbound Delivery Creation
  IF p_delivery-status EQ c_sal_status AND p_delivery-vbeln IS NOT INITIAL.
*sales Order lock checks
    CALL FUNCTION 'ENQUEUE_EVVBAKE'
      EXPORTING
        mode_vbak      = 'E'
        mandt          = sy-mandt
        vbeln          = p_delivery-vbeln
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      p_lv_log-msg = |Sales Order is Locked by Some Other User|.
    ELSE.
      CLEAR: lv_vbeln,lv_msg.
      PERFORM outbound_delivery USING p_delivery-vbeln CHANGING lv_vbeln lv_msg.
      IF lv_vbeln IS NOT INITIAL.
        p_lv_log-status = c_del_status.
        p_lv_log-delivery_no = lv_vbeln.
        p_lv_log-msgtyp = c_success.
        p_lv_log-msg = |Delivery Completed|.
      ELSE.
        p_lv_log-msgtyp = c_error.
        p_lv_log-msg = lv_msg.
      ENDIF.
    ENDIF.
  ENDIF.
*Post Goods Issues Creation
  IF p_lv_log-status EQ c_del_status AND p_lv_log-delivery_no IS NOT INITIAL.
    CLEAR: lv_mblnr,lv_msg.
    PERFORM post_goods_issue USING p_lv_log-delivery_no CHANGING lv_mblnr lv_msg.
    IF lv_mblnr IS NOT INITIAL.
      p_lv_log-status    = c_pgi_status.
      p_lv_log-gdsmvt_no = lv_mblnr.
      p_lv_log-msgtyp    = c_success.
      p_lv_log-dcomp     = abap_true.
      p_lv_log-msg       = |Post Goods Issue Completed|.
    ELSE.
      p_lv_log-msgtyp = c_error.
      p_lv_log-msg = lv_msg.
    ENDIF.
  ENDIF.
ENDFORM.
*Invoice to E-invoice Process*
*& Form invoice_to_irn
*&---------------------------------------------------------------------*
FORM invoice_to_irn  USING    p_invoice TYPE zsd_scm_header
                     CHANGING p_lv_log TYPE zsd_scm_header.
  DATA:lv_belnr   TYPE belnr_d,
       lv_invoice TYPE vbeln_vf,
       lv_bukrs   TYPE bukrs,
       lv_gjahr   TYPE gjahr,
       l_einv     TYPE zst_irn_out,
       l_eway     TYPE j_1ig_ewaybill,
       lv_flag    TYPE flag,
       lt_return  TYPE bapiret2_t.

  p_lv_log = p_invoice.
*Delivery is lockecd By Someone Means
  CALL FUNCTION 'ENQUEUE_EVVBLKE'
    EXPORTING
      mode_likp      = 'E'
      mandt          = sy-mandt
      vbeln          = p_invoice-delivery_no
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.
  IF sy-subrc <> 0.
    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
      EXPORTING
        msgid               = 'MC'
        msgnr               = '601'
      IMPORTING
        message_text_output = p_lv_log-msg.
  ELSE.
    IF p_invoice-status EQ c_pgi_status AND p_invoice-gdsmvt_no IS NOT INITIAL.
      CLEAR: lv_belnr,lv_invoice,lv_bukrs,lv_gjahr.
      PERFORM billing_document_create USING p_invoice-delivery_no CHANGING lv_belnr
                                                                             lv_invoice
                                                                             lv_bukrs
                                                                             lv_gjahr
                                                                             lv_msg.
      IF lv_invoice IS NOT INITIAL AND lv_belnr IS NOT INITIAL.
        p_lv_log-status = c_inv_status.
        p_lv_log-invoice_no = lv_invoice.
        p_lv_log-belnr = lv_belnr.
        p_lv_log-bukrs = lv_bukrs.
        p_lv_log-gjahr = lv_gjahr.
        p_lv_log-msgtyp = c_success.
        p_lv_log-msg = |Billing Document Completed|.
        "Mail Sending Method
        CALL METHOD lo_object_cls->mail_sent
          EXPORTING
            vbeln     = lv_invoice
          IMPORTING
            mail_sent = DATA(lv_mailflag).
        IF lv_mailflag = abap_true.
          p_lv_log-mail = abap_true.
        ENDIF.
      ELSE.
        p_lv_log-msgtyp = c_error.
        p_lv_log-msg = lv_msg.
      ENDIF.
    ENDIF.
*Sales Invoice Completed then E-invoice Generation
    IF p_lv_log-status EQ c_inv_status AND p_lv_log-invoice_no IS NOT INITIAL.
      CLEAR: l_einv,l_eway,lv_msg.
      PERFORM irn_ewaybill_generation USING p_lv_log-invoice_no CHANGING l_einv l_eway lv_msg.
      IF l_einv-irn IS NOT INITIAL.
*  ***einvoice irn invrefnum details.--->>
        PERFORM update_irn_table USING l_einv.
        p_lv_log-irn    = abap_true.
        p_lv_log-status = c_irn_status.
        p_lv_log-icomp  = abap_true.
        p_lv_log-msgtyp = c_success.
        p_lv_log-msg    = |IRN Completed|.
      ENDIF.
      IF lv_msg IS NOT INITIAL.
        p_lv_log-msgtyp = c_error.
        p_lv_log-msg    = lv_msg.
      ENDIF.
*Once IRN Completed then status
      IF p_lv_log-status EQ c_irn_status.
        p_lv_log-status = c_com_status.
        p_lv_log-msg    = |Sales to E-invoice Process Completed|.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form outbound_delivery
*&---------------------------------------------------------------------*
FORM outbound_delivery  USING p_sorder TYPE vbeln_va
                        CHANGING p_vbeln TYPE vbeln_vl
                                 p_msg TYPE string.

  DATA(lv_lgort) = c_lgort.
  CALL METHOD lo_object_cls->create_ob_delivery
    EXPORTING
      sales_document = p_sorder
      lgort          = lv_lgort
    IMPORTING
      delivery_no    = p_vbeln
      return         = DATA(lt_return)
      msg            = p_msg.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form post_goods_issue
*&---------------------------------------------------------------------*
FORM post_goods_issue  USING    p_delno TYPE vbeln_vl
                       CHANGING p_lv_mblnr TYPE mblnr
                                p_lv_msg TYPE string.

  DATA(lv_lgort) = c_lgort.

  CALL METHOD lo_object_cls->post_goods_issue
    EXPORTING
      delivery_no = p_delno
      lgort       = lv_lgort
    IMPORTING
      pgi_no      = p_lv_mblnr
      return      = DATA(lt_return)
      msg         = p_lv_msg.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form billing_document_create
*&---------------------------------------------------------------------*
FORM billing_document_create  USING    p_delno TYPE vbeln_vl
                              CHANGING p_lv_belnr
                                       p_lv_invoice
                                       p_lv_bukrs
                                       p_lv_gjahr
                                       p_lv_msg.
*  DATA(l_delno) = VALUE #( gt_header[ order_id = p_header-order_id ]-delivery_no OPTIONAL ).
  CALL METHOD lo_object_cls->create_invoice
    EXPORTING
      delivery_no  = p_delno
    IMPORTING
      bill_invoice = p_lv_invoice
      acco_invoice = p_lv_belnr
      bukrs        = p_lv_bukrs
      gjahr        = p_lv_gjahr
      return       = DATA(lt_return).
  LOOP AT lt_return INTO DATA(lw_ret) WHERE type = 'E' OR type = 'A'.
    CLEAR l_msg.
    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
      EXPORTING
        msgid               = lw_ret-id
        msgnr               = lw_ret-number
        msgv1               = lw_ret-message_v1
        msgv2               = lw_ret-message_v2
        msgv3               = lw_ret-message_v3
        msgv4               = lw_ret-message_v4
      IMPORTING
        message_text_output = l_msg.
    p_lv_msg = |{ p_lv_msg }/{ l_msg }|.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form irn_ewaybill_generation
*&---------------------------------------------------------------------*
FORM irn_ewaybill_generation  USING    p_invno TYPE vbeln_vf
                              CHANGING p_l_einv TYPE zst_irn_out
                                       p_l_eway TYPE j_1ig_ewaybill
                                       p_lv_msg.
  DATA: lt_ret TYPE bapiret2_t.
  "E-invoice Generation
  CALL FUNCTION 'ZSD_FM_IRN_CREATE'
    EXPORTING
      im_vbeln  = p_invno
    IMPORTING
      einv_out  = p_l_einv
      eway_out  = p_l_eway
    TABLES
      error_msg = lt_ret.
  LOOP AT lt_ret INTO DATA(lw_ret) WHERE type = 'E' OR type = 'A'.
    CLEAR l_msg.
    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
      EXPORTING
        msgid               = lw_ret-id
        msgnr               = lw_ret-number
        msgv1               = lw_ret-message_v1
        msgv2               = lw_ret-message_v2
        msgv3               = lw_ret-message_v3
        msgv4               = lw_ret-message_v4
      IMPORTING
        message_text_output = l_msg.
    p_lv_msg = |{ p_lv_msg }/{ l_msg }|.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form update_irn_table
*&---------------------------------------------------------------------*
FORM update_irn_table USING p_irndtls TYPE zst_irn_out.
  DATA: wa_j_1ig_invrefnum TYPE j_1ig_invrefnum.
  CLEAR wa_j_1ig_invrefnum.
  wa_j_1ig_invrefnum-bukrs         = p_irndtls-bukrs.
  wa_j_1ig_invrefnum-doc_year      = p_irndtls-gjahr.
  wa_j_1ig_invrefnum-docno         = p_irndtls-vbeln.
  wa_j_1ig_invrefnum-version       = p_irndtls-version.
  wa_j_1ig_invrefnum-doc_type      = p_irndtls-doctyp.
  wa_j_1ig_invrefnum-irn           = p_irndtls-irn.
  wa_j_1ig_invrefnum-ack_no        = p_irndtls-ack_no.
  wa_j_1ig_invrefnum-ack_date      = p_irndtls-ack_date.
  wa_j_1ig_invrefnum-irn_status    = p_irndtls-irn_status.
  wa_j_1ig_invrefnum-ernam         = p_irndtls-ernam.
  wa_j_1ig_invrefnum-erdat         = p_irndtls-erdat.
  wa_j_1ig_invrefnum-erzet         = p_irndtls-erzet.
  wa_j_1ig_invrefnum-signed_inv    = p_irndtls-signed_inv.
  wa_j_1ig_invrefnum-signed_qrcode = p_irndtls-signed_qrcode.
  MODIFY j_1ig_invrefnum FROM wa_j_1ig_invrefnum.
ENDFORM.
