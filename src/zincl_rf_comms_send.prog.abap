*&---------------------------------------------------------------------*
*& Include          ZINCL_RF_COMMS_SEND
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form Send_Mail
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM Send_Mail USING VALUE(ls_input) TYPE zsd_sf_cust_inv
                     VALUE(gt_comms) LIKE it_comms
                   CHANGING  ls_mail_resp TYPE string.
  CONSTANTS:gc_raw     TYPE char03 VALUE 'HTM'.

  DATA: lv_hex        TYPE xstring,
        lt_hex        TYPE solix_tab,
        iv_importance TYPE bcs_docimp VALUE 5.
  DATA: lv_stat TYPE char20.
  DATA: lt_comms TYPE TABLE OF zcus_cf_cumm.
        DATA: lt_bin TYPE solix_tab.
        DATA: gc_subject TYPE so_obj_des .
**********************************************************************Fetch Customer maintanence Data with status - payment accepted by Finance
  """"Get Customer Name from ADRC
  SELECT FROM kna1
  FIELDS kunnr,adrnr
  WHERE kunnr = @ls_input-custno
  INTO TABLE @DATA(gt_cust).
  IF sy-subrc = 0.
    SELECT FROM adrc
    FIELDS addrnumber,name1,name4
    FOR ALL ENTRIES IN @gt_cust
    WHERE addrnumber = @gt_cust-adrnr
    INTO TABLE @DATA(gt_cust_name).
  ENDIF.

*      LOOP AT gt_cust_comms INTO DATA(gs_cust_comms).
  lt_comms = VALUE #( FOR ls_comm IN gt_comms WHERE ( kunnr = ls_input-custno and com_type = 30 )
                             ( CORRESPONDING #( ls_comm ) )  ).
****Fetching Channel Manager pernr, name and E-mail****
    Select SINGLE from zcust_blk_chk fields CH_PERNR where kunnr = @ls_input-custno into @data(chnl_manager_pernr).
    IF sy-subrc = 0.
    Select SINGLE from pa0105 fields USRID_LONG where pernr = @chnl_manager_pernr
                                                  and subty = '0010'
                                                  and endda >= @sy-datum
                                                  and begda <= @sy-datum
                                                into @data(chnl_manager_mail).
    IF sy-subrc = 0.
     Select SINGLE from pa0001 fields SNAME where pernr = @chnl_manager_pernr
                                                  and endda >= @sy-datum
                                                  and begda <= @sy-datum
                                                into @data(chnl_manager_name).
    ENDIF.
    endif.
**********************************************************************
  DATA(gs_cust_name) = VALUE #( gt_cust_name[ name1 = ls_input-custname ]-name4 OPTIONAL ).
  TRY.
      "send request
      DATA(lo_send_req) = cl_bcs=>create_persistent( ).

      "set sender
      lo_send_req->set_sender( i_sender = cl_cam_address_bcs=>create_internet_address(
                                      i_address_string =   CONV #( 'customerfeedback@sheenlac.in' )
                                            i_address_name   = CONV #( 'Sheenlac' )
                                          ) ).

      DATA(ls_merchant) = |{ ls_input-custno ALPHA = OUT }|.
      DATA(ls_inv_date) = |{ ls_input-invoicedate+6(2) }-{ ls_input-invoicedate+4(2) }-{ ls_input-invoicedate+0(4) }|.
      DATA(lt_body) = VALUE bcsy_text(
          ( line = '<HTML> <BODY>'  )
          ( line = |<p style="font-size:14;" > Dear { gs_cust_name }({ ls_input-custname }) ,</p>| ) (  )
 ( line = |Invoice No { ls_input-invoiceno } for the value of { ls_input-invoiceamount } , Adjusted due amount { ls_input-dueamount } has been raised by Sheenlac on { ls_inv_date }.</br>| ) (  )
          ( line = |Provide your acceptance for discounting by clicking on the following link:| ) (  )
          ( line = |<a style="color:blue" href =" { ls_input-payment_url }"> { ls_input-payment_url } </a></br></br></br>| ) (  ) (  ) (  )
          ( line =  |</br></br>| ) (  ) (  )
          ( line = |<p style="font-size:14;" ><strong>Note:The link will expire in 30 Days.</strong></p>| )
          ( line = |</BODY></HTML>| )
      ).

        gc_subject = |RupiFi Payment Request ({ ls_input-invoiceno })|.
      "set document
      DATA(lo_document) = cl_document_bcs=>create_document(
                            i_type         = gc_raw
                            i_subject      = gc_subject
                            i_importance   = iv_importance
                            i_text         = lt_body
                          ).
       PERFORM convert_to_binary USING ls_input CHANGING lt_bin.
    lo_document->add_attachment(
          EXPORTING
            i_attachment_type     = 'PDF'
            i_attachment_subject  = |Invoice-{ ls_input-invoiceno }|
            i_att_content_hex     = lt_bin
        ).
      lo_send_req->set_document( i_document = lo_document  ).
      "set receiver
      LOOP AT lt_comms ASSIGNING FIELD-SYMBOL(<fs_comm>).
        lo_send_req->add_recipient(
          EXPORTING
            i_recipient  = cl_cam_address_bcs=>create_internet_address(
                              i_address_string = <fs_comm>-email
                              i_address_name   = CONV #( ls_input-custname )
                           )
            i_express    = 'X'
        ).
* MAIL CC To Channel Manager
              lo_send_req->add_recipient(
                EXPORTING
                  i_recipient  = cl_cam_address_bcs=>create_internet_address(
                                   i_address_string = chnl_manager_mail
                                   i_address_name   = conv #( chnl_manager_name )
                                 )
                  i_copy       = abap_true
              ).
*              CATCH cx_send_req_bcs.
      ENDLOOP.

      DATA(lv_send_mail) = lo_send_req->send(  ).
      IF sy-subrc = 0.
        "commit to send email
        COMMIT WORK.
*        DATA(i_true) = `Mail sent successfully`.
        ls_mail_resp = |Customer Name:{ ls_input-custname }, InvoiceNo:{ ls_input-invoiceno }, Customer E-Mail:{ <fs_comm>-email },Channel Manager Pernr : { chnl_manager_pernr }, Channel Manager E-mail:{ chnl_manager_mail } Mail sent successfully|.
      ELSE.
*        DATA(i_false) = `Error in sending Mail`.
        ls_mail_resp = |Error in sending Mail|.
      ENDIF.
      "Exception handling
    CATCH cx_bcs INTO DATA(gs_bcs).
      DATA(es_message) =  | Error text:{ gs_bcs->get_text( ) } Error Type:{ gs_bcs->error_type } |.
*      Message es_message TYPE 'E'.
  ENDTRY.
**********************************************************************Send to Whatsapp
* DATA(gs_whats_cust) =  VALUE #( gt_comms[  fintype = 'RF' kunnr = ls_input-custno com_type = 20 ] OPTIONAL ).
*Loop at gt_comms INTO DATA(gs_whats_cust) WHERE com_type = 20.
*   me->rf_whatsapp_dist(
*     EXPORTING
*       ls_output    = ls_output
*       gs_cust_inv  = ls_input        "structure zsd_sf_cust_inv
*       gs_comm_cust = gs_whats_cust
*     IMPORTING
*     gv_http_stat = lv_stat
*   ).
*   ENDLOOP.
*      ENDLOOP.
*    ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form Send_whatsapp
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM Send_whatsapp USING VALUE(gs_cust_inv) TYPE zsd_sf_cust_inv
                         VALUE(gt_comms) LIKE it_comms
                   CHANGING gv_http_stat TYPE string.
   DATA: lv_stat TYPE char20,
          lv_msg  TYPE string,
          ls_templ_name type char50.

    DATA(ls_inv_date) = |{ gs_cust_inv-invoicedate+6(2) }-{ gs_cust_inv-invoicedate+4(2) }-{ gs_cust_inv-invoicedate+0(4) }|.
    "Template name and parameters differs from business purpose
   ls_templ_name = 'invoice_copy2'.
*----------Get Time Taken value-------------*
    SELECT SINGLE low FROM tvarvc INTO @DATA(ls_fname)
            WHERE name = 'ZWAPP_TIME_TAKEN'
            AND type = 'P'.
       if sy-subrc = 0.
       data(ls_time_taken) = CONV char2( ls_fname ).
       endif.
    data(lt_parameters) = value ztt_key_value_pair(  ( name = 'name' value = gs_cust_inv-custname )
                                                     ( name = 'invoice_number' value = gs_cust_inv-invoiceno )
                                                     ( name = 'total_amount' value = gs_cust_inv-invoiceamount )
                                                     ( name = 'total_price' value = gs_cust_inv-dueamount )
                                                     ( name = 'order_date' value = ls_inv_date )
                                                     ( name = 'file_link' value = gs_cust_inv-payment_url )
                                                     ( name = 'time_taken' value = ls_time_taken )
                                                     ).
**********************************************************************
    LOOP AT gt_comms INTO DATA(gs_whats_cust) WHERE fintype = 'RF' and kunnr = gs_cust_inv-custno and com_type = 20.
      CALL FUNCTION 'ZFM_GEN_WHATSAPP'
        EXPORTING
          iv_template_name  = ls_templ_name
          iv_mobile         = gs_whats_cust-phone
          it_template_data  = lt_parameters
        IMPORTING
          iv_status         =  lv_stat
        EXCEPTIONS
          incorrect_mob_num = 1
        .
      IF sy-subrc ne 0.
        lv_msg = 'Incorrect mob number format:add "91" in prefix'.
      ENDIF.
     lv_msg = |Customer Name:{ gs_cust_inv-custname }, Customer Whatsappno:{ gs_whats_cust-phone } InvoiceNo:{ gs_cust_inv-invoiceno } ,Chat sent|.
      IF lv_stat = 'SUCCESS'.
        gv_http_stat = |{ lv_msg }|.
      ELSE.
        lv_msg = |Error in sending chat|.
        gv_http_stat = |{ lv_msg }|.
      ENDIF.
    ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form Build_ALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM Build_ALV .
  DATA: columns    TYPE REF TO cl_salv_columns_table,
        column     TYPE REF TO cl_salv_column,
        colNames   TYPE salv_t_column_ref,
        colName    LIKE LINE OF colNames,
        txtMedium  TYPE scrtext_m,
        lo_header  TYPE REF TO cl_salv_form_layout_grid,
        lo_h_label TYPE REF TO cl_salv_form_label,
        lo_h_flow  TYPE REF TO cl_salv_form_layout_flow.
  DATA: lo_functions TYPE REF TO cl_salv_functions_list,
        lo_display   TYPE REF TO cl_salv_display_settings.

  DATA lo_gr_alv       TYPE REF TO cl_salv_table. " Variables for ALV properties

* create the alv object
  IF gt_salv_data[] IS NOT INITIAL.
    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = lo_gr_alv
          CHANGING
            t_table      = gt_salv_data.

        TRY.
            columns = lo_gr_alv->get_columns( ).
                          lo_functions = lo_gr_alv->get_functions( ).
              lo_functions->set_default( abap_true ).
            columns->set_optimize(
                value = abap_true
            ).
            column = columns->get_column( columnname = 'SENT_STAT' ).
            column->set_long_text( value = 'Sent Status' ).

            column = columns->get_column( columnname = 'PAYMENT_ID' ).
            column->set_long_text( value = 'Payment ID' ).

            column = columns->get_column( columnname = 'PAYMENT_DATE' ).
            column->set_long_text( value = 'Payment date' ).

          CATCH cx_salv_not_found.
        ENDTRY.
      CATCH cx_salv_msg.
    ENDTRY.
    lo_gr_alv->display( ).
  ELSE.
    WRITE: 'No Files to Upload'.
  ENDIF.


ENDFORM.
FORM convert_to_binary USING p_input TYPE zsd_sf_cust_inv
                                CHANGING pt_binary TYPE solix_tab.

  DATA(l_set_print) = VALUE lbbil_print_data_to_read(  hd_gen = abap_true
                                                             hd_adr = abap_true
                                                             hd_gen_descript = abap_true
                                                             hd_org = abap_true
                                                             hd_part_add = abap_true
                                                             hd_kond = abap_true
                                                             hd_fin = abap_true
                                                             hd_ref = abap_true
                                                             hd_tech = abap_true
                                                             it_gen = abap_true
                                                             it_adr = abap_true
                                                             it_price = abap_true
                                                             it_kond = abap_true
                                                             it_ref = abap_true
                                                             it_refdlv = abap_true
                                                             it_reford = abap_true
                                                             it_refpurord = abap_true
                                                             it_refvag = abap_true
                                                             it_refvg2 = abap_true
                                                             it_refvkt = abap_true
                                                             it_tech = abap_true
                                                             it_fin = abap_true
                                                             it_confitm = abap_true
                                                             it_confbatch = abap_true
                                                             msr_hd = abap_true
                                                             msr_it = abap_true ).
        DATA(lv_objkey) = CONV nast-objky( p_input-invoiceno ).
        DATA ls_bil_invoice    TYPE lbbil_invoice.
        DATA ls_docpara TYPE sfpdocparams.
        DATA ls_outpara TYPE sfpoutputparams.
        DATA ls_output  TYPE fpformoutput.
        DATA  ls_frmname TYPE fpname.
        DATA: lv_fm    TYPE rs38l_fnam,
              i_objbin TYPE STANDARD TABLE OF solix,
              l_app1   TYPE string.
        CALL FUNCTION 'LB_BIL_INV_OUTP_READ_PRTDATA'
          EXPORTING
            if_bil_number         = lv_objkey
            is_print_data_to_read = l_set_print
            if_parvw              = 'RE'
            if_parnr              = p_input-custno
            if_language           = sy-langu
          IMPORTING
            es_bil_invoice        = ls_bil_invoice
          EXCEPTIONS
            records_not_found     = 1
            records_not_requested = 2
            OTHERS                = 3.
        IF sy-subrc = 0.
          ls_outpara-getpdf = abap_true.

          CALL FUNCTION 'FP_JOB_OPEN'
            CHANGING
              ie_outputparams = ls_outpara
            EXCEPTIONS
              cancel          = 1
              usage_error     = 2
              system_error    = 3
              internal_error  = 4
              OTHERS          = 5.

          CLEAR: lv_fm,ls_frmname.
          ls_frmname = 'ZSD_IRN_QR_EINVOICE_V1'.

          "Get Respective Function module Name based on form Name
          TRY.
              CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
                EXPORTING
                  i_name     = ls_frmname
                IMPORTING
                  e_funcname = lv_fm.
            CATCH cx_root.
              RETURN.
          ENDTRY.

          CALL FUNCTION lv_fm
            EXPORTING
              /1bcdwb/docparams  = ls_docpara
              is_bil_invoice     = ls_bil_invoice
            IMPORTING
              /1bcdwb/formoutput = ls_output
            EXCEPTIONS
              usage_error        = 1
              system_error       = 2
              internal_error     = 3
              OTHERS             = 4.


          CALL FUNCTION 'FP_JOB_CLOSE'
            EXCEPTIONS
              usage_error    = 1
              system_error   = 2
              internal_error = 3
              OTHERS         = 4.

              ENDIF.


CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
    EXPORTING
      buffer                = ls_output-pdf
    TABLES
      binary_tab            = pt_binary
            .

ENDFORM.
