FUNCTION zinvoice_mail_with_pdf.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(VBELN) TYPE  VBELN_VF
*"     REFERENCE(GJAHR) TYPE  GJAHR
*"  EXPORTING
*"     REFERENCE(MAIL_SENT) TYPE  FLAG
*"----------------------------------------------------------------------
  TABLES: nast.
  DATA: repeat(1) TYPE c.
  DATA: ls_bil_invoice    TYPE lbbil_invoice.
  DATA ls_docpara TYPE sfpdocparams.
  DATA ls_outpara TYPE sfpoutputparams.
  DATA ls_output  TYPE fpformoutput.
  DATA  ls_frmname TYPE fpname.
  DATA: lv_fm        TYPE rs38l_fnam.
* Objects to send mail.
  DATA: i_objpack LIKE sopcklsti1 OCCURS 0 WITH HEADER LINE,
        i_objtxt  LIKE solisti1   OCCURS 0 WITH HEADER LINE,
        i_objbin  LIKE solix      OCCURS 0 WITH HEADER LINE,
        i_reclist LIKE somlreci1  OCCURS 0 WITH HEADER LINE,
        w_return  TYPE ssfcrescl.

  DATA: v_len_in    LIKE sood-objlen.

  DATA: salutation TYPE string.
  DATA: body TYPE string.
  DATA: footer TYPE string.
  DATA: lo_send_request TYPE REF TO cl_bcs,
        lo_document     TYPE REF TO cl_document_bcs,
        lo_sender       TYPE REF TO if_sender_bcs,
        lo_recipient    TYPE REF TO if_recipient_bcs VALUE IS INITIAL,lt_message_body TYPE bcsy_text,
        lo_recipient1   TYPE REF TO if_recipient_bcs VALUE IS INITIAL, " if_recipient_bcs VALUE IS INITIAL,"lt_message_body TYPE bcsy_text,
        lo_recipient2   TYPE REF TO if_recipient_bcs VALUE IS INITIAL,
        lo_recipient3   TYPE REF TO if_recipient_bcs VALUE IS INITIAL,
        lx_document_bcs TYPE REF TO cx_document_bcs,
        lv_sent_to_all  TYPE os_boolean.

  DATA : lv_sub TYPE sood-objdes.

  IF vbeln IS NOT INITIAL AND gjahr IS NOT INITIAL.
*    ls_bil_invoice-hd_gen-bil_number = vbeln.

    SELECT SINGLE * FROM vbrk INTO @DATA(l_invhdr)
      WHERE vbeln = @vbeln
      AND gjahr = @gjahr.
    IF sy-subrc EQ 0.
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
      DATA(lv_objkey) = CONV nast-objky( l_invhdr-vbeln ).
      CALL FUNCTION 'LB_BIL_INV_OUTP_READ_PRTDATA'
        EXPORTING
          if_bil_number         = lv_objkey
          is_print_data_to_read = l_set_print
          if_parvw              = 'RE'
          if_parnr              = l_invhdr-kunag
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
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.

        CLEAR: lv_fm,ls_frmname.
*        ls_frmname = 'ZSD_IRN_QR_EINVOICE_YBDP'.
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
        IF sy-subrc <> 0.

        ENDIF.

        CALL FUNCTION 'FP_JOB_CLOSE'
          EXCEPTIONS
            usage_error    = 1
            system_error   = 2
            internal_error = 3
            OTHERS         = 4.

        DATA(out_pdf) = CONV xstring( ls_output-pdf ).

        CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
          EXPORTING
            buffer     = out_pdf
          TABLES
            binary_tab = i_objbin[].
      ENDIF.
    ENDIF.
  ENDIF.

  IF i_objbin[] IS NOT INITIAL.

    DATA(lv_head) = CONV char50( |Sheenlac Paints Limited Invoice No : { vbeln }| ).
    "create send request
    TRY.
        lo_send_request = cl_bcs=>create_persistent( ).
      CATCH  cx_send_req_bcs INTO DATA(lx_send_req_bcs4).
    ENDTRY.
    "create message body and subject
    salutation ='Dear Customer ,'.
    APPEND salutation TO lt_message_body.
    APPEND INITIAL LINE TO lt_message_body.
    body = 'With reference to your order, the above invoice has been generated at our end.'.
    APPEND body TO lt_message_body.
    APPEND INITIAL LINE TO lt_message_body.
    body = 'If you have any queries / doubts, kindly contact us on 8300030404 (or) send us the mail to ccchennai@sheenlac.in'.
    APPEND body TO lt_message_body.
    APPEND INITIAL LINE TO lt_message_body.
    footer = 'Thanks & Best Regards,'.
    APPEND footer TO lt_message_body.
    footer = 'Customer Care (Sheenlac Paints Ltd)'.
    APPEND footer TO lt_message_body.
    "put your text into the document
    TRY.
        lo_document = cl_document_bcs=>create_document(
        i_type = 'RAW'
        i_text = lt_message_body
        i_subject = lv_head ).
      CATCH cx_document_bcs INTO DATA(lx_document_bcs1).
    ENDTRY.

    lv_sub = vbeln.
    TRY.
        lo_document->add_attachment(
        EXPORTING
        i_attachment_type = 'PDF'
        i_attachment_subject = lv_sub
        i_att_content_hex = i_objbin[] ).
      CATCH cx_document_bcs INTO lx_document_bcs.
    ENDTRY.
* Pass the document to send request
    TRY.
        lo_send_request->set_document( lo_document ).
      CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs3).
    ENDTRY.
*** Selecting Sender Mail ID ***
    SELECT SINGLE low FROM tvarvc INTO @DATA(lv_sender)
                      WHERE name = 'ZRLB_SENDER_MAIL'
                      AND type = 'P'.
    IF sy-subrc = 0.
      "Create sender
      DATA(sender_mail) = CONV adr6-smtp_addr( lv_sender ).
    ENDIF.

    TRY.
        lo_sender = cl_cam_address_bcs=>create_internet_address( sender_mail ).
      CATCH cx_address_bcs INTO DATA(lx_address_bcs2).
    ENDTRY.

    TRY.
        lo_send_request->set_sender( lo_sender ).
      CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs6).
    ENDTRY.
    IF sy-sysid = 'DEV' OR sy-sysid = 'QAS'.
** Development and Quality Means  **
      SELECT SINGLE low FROM tvarvc
                     INTO @DATA(lv_mail_to)
                     WHERE name = 'ZRLB_INVOICE'
                     AND type = 'P'.
      IF sy-subrc = 0.
        DATA(mailid_to) =  CONV ad_smtpadr( lv_mail_to ).
      ENDIF.
*Production
    ELSE.
      SELECT * FROM tvarvc INTO TABLE @DATA(lt_tvarvc)
                                     WHERE name = 'ZRLB_INVOICE_PATNER_TYPE'
                                     AND type = 'S'.
      IF sy-subrc EQ 0.
        DATA lr_parvw TYPE RANGE OF parvw.

        lr_parvw = VALUE #(  FOR <lfs_tvarvc> IN lt_tvarvc
                                    ( sign = 'I'
                                      option = 'EQ'
                                      low = <lfs_tvarvc>-low )
                              ).
      ENDIF.

      CHECK lr_parvw IS NOT INITIAL.
      SELECT pernr INTO TABLE @DATA(lt_pernr)
        FROM vbpa
       WHERE vbeln = @vbeln
         AND parvw IN @lr_parvw ORDER BY PRIMARY KEY."EQ 'L5'
      IF sy-subrc = 0.
        SELECT usrid_long INTO TABLE @DATA(lt_usrid_long)
          FROM pa0105 FOR ALL ENTRIES IN @lt_pernr
          WHERE pernr = @lt_pernr-pernr
          AND   subty = '0010'.
      ENDIF.
      SELECT SINGLE adrnr INTO @DATA(lv_adrnr) FROM kna1 WHERE kunnr = @l_invhdr-kunag .
      SELECT smtp_addr INTO @DATA(lv_smtp_addr) UP TO 1 ROWS FROM adr6 WHERE addrnumber = @lv_adrnr ORDER BY PRIMARY KEY.
      ENDSELECT.
      mailid_to = lv_smtp_addr.
    ENDIF.
*----------------------------------------------------------------------------------------------*
*To Mail Recipient

    IF mailid_to IS NOT INITIAL.
      "Create recipient
      TRY.
          lo_recipient = cl_cam_address_bcs=>create_internet_address( mailid_to ).
        CATCH cx_address_bcs INTO DATA(lx_address_bcs1).
      ENDTRY.
*set recipient
      TRY.
          lo_send_request->add_recipient(
             EXPORTING
             i_recipient = lo_recipient
              i_express = abap_true
             ).
        CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs5).
      ENDTRY.
    ENDIF.
    "Sales Officer & Sales Manager Involves in this Sale Invoice"
    LOOP AT lt_usrid_long ASSIGNING FIELD-SYMBOL(<mail_id>).
      CLEAR mailid_to.
      mailid_to = <mail_id>-usrid_long.
      TRY.
          lo_recipient1 = cl_cam_address_bcs=>create_internet_address( mailid_to ).
        CATCH cx_address_bcs INTO DATA(lx_address_bcs5).
      ENDTRY.
*set recipient
      TRY.
          lo_send_request->add_recipient(
             EXPORTING
             i_recipient = lo_recipient1
             i_copy = 'X'
             i_express = abap_true
             ).
        CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs7).
      ENDTRY.
    ENDLOOP.
*--------------------------------------------------------------------------------------------------*
*CC mail ID Recipient
    REFRESH: lt_tvarvc.
    SELECT * FROM tvarvc INTO TABLE lt_tvarvc
                         WHERE name = 'ZRLB_INVOICE_MAIL_CC'
                         AND type = 'S'.
    IF sy-subrc EQ 0.
      LOOP AT lt_tvarvc ASSIGNING FIELD-SYMBOL(<fs_tvarvc>).
        DATA(mailid_cc) = CONV ad_smtpadr( <fs_tvarvc>-low ).
        TRY.
            lo_recipient2 = cl_cam_address_bcs=>create_internet_address( mailid_cc ).
          CATCH cx_address_bcs INTO DATA(lx_address_bcs3).
        ENDTRY.

        TRY.
            lo_send_request->add_recipient(
             EXPORTING
             i_recipient = lo_recipient2
             i_copy = 'X'
             i_express = abap_true
             ).
          CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs1).
        ENDTRY.
      ENDLOOP.
    ENDIF.
*--------------------------------------------------------------------------------------------------------*
* send email
    TRY.
        lo_send_request->send(
        EXPORTING
        i_with_error_screen = abap_true
        RECEIVING
        result = lv_sent_to_all ).
      CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs).
    ENDTRY.
    IF sy-tcode NE 'VF01'.
      COMMIT WORK.
    ENDIF.
    CHECK sy-subrc IS INITIAL.
    mail_sent = abap_true.
  ENDIF.

ENDFUNCTION.
