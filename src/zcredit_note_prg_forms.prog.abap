*&---------------------------------------------------------------------*
*& Include          ZCREDIT_NOTE_PRG_FORMS
*&---------------------------------------------------------------------*
FORM f_screen_adjust.
  LOOP AT SCREEN.
    IF screen-group1 = 'BP2' .
      screen-invisible = '1'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form protocol_update
*&---------------------------------------------------------------------*
FORM protocol_update .
  CALL FUNCTION 'NAST_PROTOCOL_UPDATE'
    EXPORTING
      msg_arbgb = syst-msgid
      msg_nr    = syst-msgno
      msg_ty    = syst-msgty
      msg_v1    = syst-msgv1
      msg_v2    = syst-msgv2
      msg_v3    = syst-msgv3
      msg_v4    = syst-msgv4
    EXCEPTIONS
      OTHERS    = 1.
ENDFORM.

FORM add_smfrm_prot.

  DATA: lt_errortab             TYPE tsferror.
* DATA: LF_MSGNR                TYPE SY-MSGNO.
  FIELD-SYMBOLS: <fs_errortab>  TYPE LINE OF tsferror.

* get smart form protocoll
  CALL FUNCTION 'SSF_READ_ERRORS'
    IMPORTING
      errortab = lt_errortab.

* add smartform protocoll to nast protocoll
  LOOP AT lt_errortab ASSIGNING <fs_errortab>.
*   CLEAR LF_MSGNR.
*   LF_MSGNR = <FS_ERRORTAB>-ERRNUMBER.
    CALL FUNCTION 'NAST_PROTOCOL_UPDATE'
      EXPORTING
        msg_arbgb = <fs_errortab>-msgid
*       MSG_NR    = LF_MSGNR
        msg_nr    = <fs_errortab>-msgno
        msg_ty    = <fs_errortab>-msgty
        msg_v1    = <fs_errortab>-msgv1
        msg_v2    = <fs_errortab>-msgv2
        msg_v3    = <fs_errortab>-msgv3
        msg_v4    = <fs_errortab>-msgv4
      EXCEPTIONS
        OTHERS    = 1.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_initial_selection
FORM f_data_selection .
** Select data based on input **
  SELECT bukrs
         belnr
         gjahr FROM bkpf INTO TABLE gt_bkpf
                         WHERE belnr IN s_belnr
                         AND bukrs EQ s_bukrs
                         AND gjahr EQ s_gjahr
                         AND ( tcode = 'MIRO' OR tcode = 'FB60' OR tcode = 'FB65' OR tcode = 'SE37'
                                              OR tcode = ' ' ).
  IF sy-subrc EQ 0.
    SORT gt_bkpf[] BY belnr.
** Select BSEG DATA for Vendor data **
    SELECT bukrs
           belnr
           gjahr
           lifnr FROM bseg INTO TABLE gt_bseg
                           FOR ALL ENTRIES IN gt_bkpf
                           WHERE bukrs = gt_bkpf-bukrs
                           AND belnr = gt_bkpf-belnr
                           AND gjahr = gt_bkpf-gjahr.
    IF sy-subrc EQ 0.
      SORT gt_bseg[] BY bukrs belnr lifnr DESCENDING.
      DELETE ADJACENT DUPLICATES FROM gt_bseg COMPARING bukrs belnr.
      SELECT lifnr,name1 FROM lfa1 INTO TABLE @DATA(gt_lfa1).
** For CC mail we have some custom table to add CC recipient **
** Select Business partner No **
      SELECT supplier
             businesspartner FROM ibpsupplier INTO TABLE gt_ibps
                                      FOR ALL ENTRIES IN gt_bseg
                                      WHERE supplier = gt_bseg-lifnr.
      IF sy-subrc EQ 0.
        SORT gt_ibps[] BY supplier.
** Select business partner type **
        SELECT partner
               group_feature FROM bp001 INTO TABLE gt_bp001.
        IF sy-subrc EQ 0.
          SORT gt_bp001[] BY partner.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.

FORM actual_process.

  REFRESH gt_output.
  LOOP AT gt_bkpf INTO gs_bkpf.

    s_belnr = gs_bkpf-belnr.
**** Normal Debit Note Display ***
    IF p_disp1 EQ 'X' AND p_email NE 'X' AND p_email1 NE 'X'.

      CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
        EXPORTING
          formname           = smf_name
*         VARIANT            = ' '
*         DIRECT_CALL        = ' '
        IMPORTING
          fm_name            = fm_name
        EXCEPTIONS
          no_form            = 1
          no_function_module = 2
          OTHERS             = 3.

      IF sy-subrc <> 0.
        PERFORM protocol_update.
      ENDIF.

      PERFORM call_form USING smf_name.
    ENDIF.

**** Discount charges Form Display ***
    IF p_disp2 EQ 'X' AND p_email NE 'X' AND p_email1 NE 'X'.

      CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
        EXPORTING
          formname           = smf_name1
*         VARIANT            = ' '
*         DIRECT_CALL        = ' '
        IMPORTING
          fm_name            = fm_name
        EXCEPTIONS
          no_form            = 1
          no_function_module = 2
          OTHERS             = 3.

      IF sy-subrc <> 0.
        PERFORM protocol_update.
      ENDIF.

      PERFORM call_form USING smf_name1.
    ENDIF.
*** Manual Auto mail From normal form selection screen ***
    IF p_disp1 EQ 'X' AND p_email EQ 'X' .

      CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
        EXPORTING
          formname           = smf_name
*         VARIANT            = ' '
*         DIRECT_CALL        = ' '
        IMPORTING
          fm_name            = fm_name
        EXCEPTIONS
          no_form            = 1
          no_function_module = 2
          OTHERS             = 3.

      IF sy-subrc <> 0.
        PERFORM protocol_update.
      ENDIF.

      PERFORM send_automail USING smf_name.
    ENDIF.
*** Manual Auto mail From Discount charges form selection screen ***
    IF p_disp2 EQ 'X' AND p_email EQ 'X'.

      CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
        EXPORTING
          formname           = smf_name1
*         VARIANT            = ' '
*         DIRECT_CALL        = ' '
        IMPORTING
          fm_name            = fm_name
        EXCEPTIONS
          no_form            = 1
          no_function_module = 2
          OTHERS             = 3.

      IF sy-subrc <> 0.
        PERFORM protocol_update.
      ENDIF.

      PERFORM send_automail USING smf_name1.
    ENDIF.
**** this auto mail for Discount charges from Bapi for MIS ****
    IF p_disp1 EQ 'X' AND p_email1 EQ 'X'.

      CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
        EXPORTING
          formname           = smf_name1
*         VARIANT            = ' '
*         DIRECT_CALL        = ' '
        IMPORTING
          fm_name            = fm_name
        EXCEPTIONS
          no_form            = 1
          no_function_module = 2
          OTHERS             = 3.

      IF sy-subrc <> 0.
        PERFORM protocol_update.
      ENDIF.

      PERFORM send_automail USING smf_name1.

    ENDIF.
    CLEAR gs_bkpf.
  ENDLOOP.

ENDFORM.

FORM call_form USING smf_name TYPE tdsfname.
**** Old Debit note Display ***
  IF smf_name EQ 'ZCREDIT_NOTE' .

    CALL FUNCTION fm_name "'/1BCDWB/SF00000571'
      EXPORTING
        s_bukrs          = s_bukrs
        s_gjahr          = s_gjahr
      TABLES
        s_belnr          = s_belnr
      EXCEPTIONS
        formatting_error = 1
        internal_error   = 2
        send_error       = 3
        user_canceled    = 4
        OTHERS           = 5.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
*** Debit note for Discount charges Display ***
  ELSEIF smf_name EQ 'ZDEBIT_NOTE' .

    CALL FUNCTION fm_name
      EXPORTING
        ip_belnr         = gs_bkpf-belnr
        ip_bukrs         = gs_bkpf-bukrs
        ip_year          = gs_bkpf-gjahr
      EXCEPTIONS
        formatting_error = 1
        internal_error   = 2
        send_error       = 3
        user_canceled    = 4
        OTHERS           = 5.
    IF sy-subrc <> 0.
      PERFORM protocol_update.
* get SmartForm protocoll and store it in the NAST protocoll
      PERFORM add_smfrm_prot.
    ENDIF.

  ENDIF.
ENDFORM.
FORM send_automail USING smf_name TYPE tdsfname.

  IF smf_name EQ 'ZCREDIT_NOTE'.

    IF p_disp1 = 'X' AND p_email EQ 'X'.

      ls_control_param-getotf = abap_true.
      ls_control_param-no_dialog = abap_true.
      ls_composer_param-tdnoprev = abap_true.
      ls_control_param-preview = space.
      ls_composer_param-tddest = 'LOCL'.

      CALL FUNCTION fm_name "'/1BCDWB/SF00000571'
        EXPORTING
          archive_index      = toa_dara
          archive_parameters = arc_params
          control_parameters = ls_control_param
*         MAIL_APPL_OBJ      =
          mail_recipient     = ls_recipient
          mail_sender        = ls_sender
          output_options     = ls_composer_param
*         USER_SETTINGS      = 'X'
          user_settings      = abap_true
          s_bukrs            = s_bukrs
          s_gjahr            = s_gjahr
          is_nast            = nast
          is_repeat          = repeat
        IMPORTING
*         DOCUMENT_OUTPUT_INFO       =
          job_output_info    = w_return
*         JOB_OUTPUT_OPTIONS =
        TABLES
          s_belnr            = s_belnr
        EXCEPTIONS
          formatting_error   = 1
          internal_error     = 2
          send_error         = 3
          user_canceled      = 4
          OTHERS             = 5.
      IF sy-subrc <> 0.
        PERFORM protocol_update.
* get SmartForm protocoll and store it in the NAST protocoll
        PERFORM add_smfrm_prot.
      ENDIF.
** Convert the smartforms to PDF **
      i_otf[] = w_return-otfdata[].
      CALL FUNCTION 'CONVERT_OTF'
        EXPORTING
          format                = 'PDF'
          max_linewidth         = 132
        IMPORTING
          bin_filesize          = v_len_in
          bin_file              = i_xstring   " This is NOT Binary. This is Hexa
        TABLES
          otf                   = i_otf
          lines                 = i_tline
        EXCEPTIONS
          err_max_linewidth     = 1
          err_format            = 2
          err_conv_not_possible = 3
          OTHERS                = 4.
      CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
        EXPORTING
          buffer     = i_xstring
        TABLES
          binary_tab = i_objbin[].
** Send mail.
      PERFORM send_email.
      PERFORM alv_display.
    ENDIF.

  ELSEIF smf_name EQ 'ZDEBIT_NOTE'.

    ls_control_param-getotf = abap_true.
    ls_control_param-no_dialog = abap_true.
    ls_composer_param-tdnoprev = abap_true.
    ls_control_param-preview = space.
    ls_composer_param-tddest = 'LOCL'.

**** Manually Sending Auto mail for Discount Charges ***
    IF p_disp2 = 'X' AND p_email EQ 'X'.

      CALL FUNCTION fm_name
        EXPORTING
          archive_index      = toa_dara
          archive_parameters = arc_params
          control_parameters = ls_control_param
*         MAIL_APPL_OBJ      =
          mail_recipient     = ls_recipient
          mail_sender        = ls_sender
          output_options     = ls_composer_param
*         USER_SETTINGS      = 'X'
          user_settings      = abap_true
          ip_belnr           = gs_bkpf-belnr
          ip_bukrs           = gs_bkpf-bukrs
          ip_year            = gs_bkpf-gjahr
        IMPORTING
*         DOCUMENT_OUTPUT_INFO       =
          job_output_info    = w_return.
*         JOB_OUTPUT_OPTIONS =
      IF sy-subrc <> 0.
        PERFORM protocol_update.
* get SmartForm protocoll and store it in the NAST protocoll
        PERFORM add_smfrm_prot.
      ENDIF.
* convert the smartforms to pdf **
      i_otf[] = w_return-otfdata[].
      CALL FUNCTION 'CONVERT_OTF'
        EXPORTING
          format                = 'PDF'
          max_linewidth         = 132
        IMPORTING
          bin_filesize          = v_len_in
          bin_file              = i_xstring   " This is NOT Binary. This is Hexa
        TABLES
          otf                   = i_otf
          lines                 = i_tline
        EXCEPTIONS
          err_max_linewidth     = 1
          err_format            = 2
          err_conv_not_possible = 3
          OTHERS                = 4.
      CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
        EXPORTING
          buffer     = i_xstring
        TABLES
          binary_tab = i_objbin[].
** Send mail.
      PERFORM send_email.
      PERFORM alv_display.
**** Through Bapi Auto Mail sending logic ***
    ELSEIF p_disp1 EQ 'X' AND p_email1 EQ 'X' AND p_disp2 NE 'X' AND p_email NE 'X'.

      CALL FUNCTION fm_name
        EXPORTING
          archive_index      = toa_dara
          archive_parameters = arc_params
          control_parameters = ls_control_param
*         MAIL_APPL_OBJ      =
          mail_recipient     = ls_recipient
          mail_sender        = ls_sender
          output_options     = ls_composer_param
*         USER_SETTINGS      = 'X'
          user_settings      = abap_true
          ip_belnr           = gs_bkpf-belnr
          ip_bukrs           = gs_bkpf-bukrs
          ip_year            = gs_bkpf-gjahr
        IMPORTING
*         DOCUMENT_OUTPUT_INFO       =
          job_output_info    = w_return.
*         JOB_OUTPUT_OPTIONS =
      IF sy-subrc <> 0.
        PERFORM protocol_update.
* get SmartForm protocoll and store it in the NAST protocoll
        PERFORM add_smfrm_prot.
      ENDIF.
* convert the smartforms to pdf **
      i_otf[] = w_return-otfdata[].
      CALL FUNCTION 'CONVERT_OTF'
        EXPORTING
          format                = 'PDF'
          max_linewidth         = 132
        IMPORTING
          bin_filesize          = v_len_in
          bin_file              = i_xstring   " This is NOT Binary. This is Hexa
        TABLES
          otf                   = i_otf
          lines                 = i_tline
        EXCEPTIONS
          err_max_linewidth     = 1
          err_format            = 2
          err_conv_not_possible = 3
          OTHERS                = 4.
      CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
        EXPORTING
          buffer     = i_xstring
        TABLES
          binary_tab = i_objbin[].
** Send mail.
      PERFORM send_email.
      PERFORM alv_display.
    ENDIF.
  ENDIF.

ENDFORM.
FORM send_email.
  DATA: lv_adrnr TYPE ad_addrnum.
  DATA: it_mailid1 TYPE TABLE OF ad_smtpadr.
  DATA: lv_venid(04) TYPE c.
  DATA: lv_venmail TYPE ad_smtpadr.

  lv_tot     = 'Sheenlac Paints Limited Debit Memo'.
  "create send request
  TRY.
      lo_send_request = cl_bcs=>create_persistent( ).
    CATCH  cx_send_req_bcs INTO DATA(lx_send_req_bcs4).

  ENDTRY.

  "create message body and subject
  REFRESH lt_message_body.
  salutation ='Dear Sir/Madam ,'.
  APPEND salutation TO lt_message_body.
  APPEND INITIAL LINE TO lt_message_body.
  body = 'Please find the attached Debit Memo'.
  APPEND body TO lt_message_body.
  APPEND INITIAL LINE TO lt_message_body.
  body = 'If you have any queries / doubts, kindly send us a mail to accountsreply@sheenlac.in'.
  APPEND body TO lt_message_body.
  APPEND INITIAL LINE TO lt_message_body.
  body = 'Please Send the acknowledgement to the transaction to the above E-mail ID immediately'.
  APPEND body TO lt_message_body.
  APPEND INITIAL LINE TO lt_message_body.
  footer = 'Thanks & Best Regards,'.
  APPEND footer TO lt_message_body.
  footer = 'Sheenlac Paints Ltd'.
  APPEND footer TO lt_message_body.

  "put your text into the document
  TRY.
      lo_document = cl_document_bcs=>create_document(
      i_type = 'RAW'
      i_text = lt_message_body
      i_subject = lv_tot ).
    CATCH cx_document_bcs INTO DATA(lx_document_bcs1).
  ENDTRY.

  TRY.
      lo_document->add_attachment(
      EXPORTING
      i_attachment_type = 'PDF'
      i_attachment_subject = 'Debit Memo'
      i_att_content_hex = i_objbin[] ).
    CATCH cx_document_bcs INTO lx_document_bcs.
  ENDTRY.
* Pass the document to send request
  TRY.
      lo_send_request->set_document( lo_document ).
    CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs3).
  ENDTRY.
  "Create sender
  SELECT SINGLE * FROM tvarvc INTO @DATA(ls_tvarvc1) WHERE name = 'ZDEBIT_MEMO'
                                             AND type = 'P'.
  sender_mail = ls_tvarvc1-low .
  "Sender Mail ID
  TRY.
      lo_sender = cl_cam_address_bcs=>create_internet_address( sender_mail ).
    CATCH cx_address_bcs INTO DATA(lx_address_bcs2).
  ENDTRY.

  TRY.
      lo_send_request->set_sender( lo_sender ).
    CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs6).
  ENDTRY.

** changed ON 15.10.2022 by samsudeen **
  IF sy-sysid = 'DEV' OR sy-sysid = 'QAS'.
** Development and Quality Means  **
    SELECT SINGLE low FROM tvarvc
                   INTO @DATA(ls_tvarvc)
                   WHERE name = 'ZRLB_INVOICE'
                   AND type = 'P'.

    CLEAR in_mailid.
    in_mailid = ls_tvarvc.
    CLEAR gv_venmail.
    gv_venmail = in_mailid.
    CONDENSE gv_venmail NO-GAPS.
** PRD Means **
  ELSE.
** Getting Vendor TO Mail ID from ADR6 table **
    CLEAR gs_bseg.
    READ TABLE gt_bseg INTO gs_bseg WITH KEY bukrs = gs_bkpf-bukrs
                                             belnr = gs_bkpf-belnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      CLEAR lv_adrnr.
      SELECT SINGLE * FROM lfa1 INTO @DATA(ls_lfa1) WHERE lifnr = @gs_bseg-lifnr. "Address Number
      IF sy-subrc EQ 0.
        lv_adrnr = ls_lfa1-adrnr.
        CLEAR lv_venmail.
        SELECT SINGLE smtp_addr FROM adr6 INTO lv_venmail WHERE addrnumber = lv_adrnr.
        CLEAR in_mailid.
        in_mailid = lv_venmail. "Vendor Mail ID
      ENDIF.
    ENDIF.

    CONDENSE lv_venmail NO-GAPS.
    CLEAR gv_venmail.
    gv_venmail = lv_venmail.

    SELECT * FROM zven_credemail INTO TABLE @DATA(lt_mail).
    CLEAR gs_ibps.
    READ TABLE gt_ibps INTO gs_ibps WITH KEY supplier = gs_bseg-lifnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      CLEAR: gs_bp001,lv_venid.
      READ TABLE gt_bp001 INTO gs_bp001 WITH KEY partner = gs_ibps-businesspartner.
      IF sy-subrc EQ 0.
        lv_venid = gs_bp001-group_feature. "ID for Identifying Vendor Type
      ENDIF.
    ENDIF.
    "CC Recipient from custom table based on vendor type **
    CLEAR gv_venmail1.
    LOOP AT lt_mail INTO DATA(ls_mail) WHERE ven_id = lv_venid.
      in_mailid1 = ls_mail-zemail.
      APPEND in_mailid1 TO it_mailid1.
      CONDENSE in_mailid1 NO-GAPS.
      CONCATENATE '|' ls_mail-zemail '|'  '|' gv_venmail1 '|' INTO gv_venmail1 SEPARATED BY space.
      CLEAR in_mailid1.
    ENDLOOP.
    REPLACE ALL OCCURRENCES OF '|' IN gv_venmail1 WITH ''.
  ENDIF.
  "Create recipient to Mail ID
  IF in_mailid IS NOT INITIAL.

    TRY.
        lo_recipient = cl_cam_address_bcs=>create_internet_address( in_mailid ).
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

    IF it_mailid1 IS NOT INITIAL.

      LOOP AT it_mailid1 INTO in_mailid1.

        TRY.
            lo_recipient1 = cl_cam_address_bcs=>create_internet_address( in_mailid1 ).
          CATCH cx_address_bcs INTO DATA(lx_address_bcs).
        ENDTRY.
        "Set recipient
        TRY.
            lo_send_request->add_recipient(
             EXPORTING
             i_recipient = lo_recipient1
             i_copy = 'X'
             i_express = abap_true
             ).
            in_mailid2 = in_mailid1.
          CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs1).
        ENDTRY.

        CLEAR in_mailid1.
      ENDLOOP.

    ENDIF.
* Send email
    TRY.
        lo_send_request->send(
        EXPORTING
        i_with_error_screen = abap_true
        RECEIVING
        result = lv_sent_to_all ).
      CATCH cx_send_req_bcs INTO lx_send_req_bcs.
    ENDTRY.
    COMMIT WORK.

    DATA(ls_output) = VALUE output( docno = gs_bkpf-belnr
                                    fisyr = gs_bkpf-gjahr
                                    vendor = ls_lfa1-lifnr
                                    venname = ls_lfa1-name1
                                    mail = lv_venmail
                                    ccmail = gv_venmail
                                    msg = |'Mail sent Successfully'| ).
    APPEND ls_output TO gt_output.
    UPDATE zdebit_ref_mis SET mail_sent = 'X'
                              ven_email = lv_venmail
                              remarks = 'Mail sent Successfully'
                              WHERE doc_no = gs_bkpf-belnr
                              AND gjahr = gs_bkpf-gjahr.
  ELSE.
*** If No Mail ID presents for Sending Mail to Vendor ****
    DATA(ls_output1) = VALUE output( docno = gs_bkpf-belnr
                                  fisyr = gs_bkpf-gjahr
                                  vendor = ls_lfa1-lifnr
                                  venname = ls_lfa1-name1
                                  mail = 'No Mail ID'
                                  ccmail = gv_venmail
                                  msg = |'No Mail Sent'| ).
    APPEND ls_output TO gt_output.
    UPDATE zdebit_ref_mis SET mail_sent = 'X'
                          ven_email = lv_venmail
                          remarks = 'Mail not sent'
                          WHERE doc_no = gs_bkpf-belnr
                          AND gjahr = gs_bkpf-gjahr.
  ENDIF.
  CLEAR: in_mailid1,in_mailid2,it_mailid1.
ENDFORM.
FORM alv_display.

  DATA: lr_columns    TYPE REF TO cl_salv_columns_table,
        lr_column     TYPE REF TO cl_salv_column_table,
        lo_column     TYPE REF TO cl_salv_column,
        lr_selections TYPE REF TO cl_salv_selections,
        l_text        TYPE string.
  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = lo_gr_alv
        CHANGING
          t_table      = gt_output ).
    CATCH cx_salv_msg.
  ENDTRY.

  lo_gr_alv->set_screen_status(
          pfstatus      =  'STANDARD_FULLSCREEN'
          report        =  'SAPLKKBL'
          set_functions = lo_gr_alv->c_functions_all ).
*columns optimize
  lr_columns = lo_gr_alv->get_columns( ).
  lr_columns->set_optimize( abap_true ).

*multiple row SELECTION
  lr_selections = lo_gr_alv->get_selections( ).
  lr_selections->set_selection_mode( if_salv_c_selection_mode=>cell ).

*column description change
  TRY.
      lo_column = lr_columns->get_column( 'DOCNO' ).
      lo_column->set_long_text( 'Doc Number' ).
      lo_column->set_medium_text( 'Doc Number' ).
      lo_column->set_short_text( 'Doc Number' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column = lr_columns->get_column( 'FISYR' ).
      lo_column->set_long_text( 'Fisc year' ).
      lo_column->set_medium_text( 'Fisc year' ).
      lo_column->set_short_text( 'Fisc year' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column = lr_columns->get_column( 'VENDOR' ).
      lo_column->set_long_text( 'Vendor' ).
      lo_column->set_medium_text( 'Vendor' ).
      lo_column->set_short_text( 'Vendor' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column = lr_columns->get_column( 'VENNAME' ).
      lo_column->set_long_text( 'Vendor Name' ).
      lo_column->set_medium_text( 'Vendor Name' ).
      lo_column->set_short_text( 'VenName' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column = lr_columns->get_column( 'MAIL' ).
      lo_column->set_long_text( 'Vendor Mail' ).
      lo_column->set_medium_text( 'Vendor Mail' ).
      lo_column->set_short_text( 'Ven Mail' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column = lr_columns->get_column( 'CCMAIL' ).
      lo_column->set_long_text( 'CC Mail' ).
      lo_column->set_medium_text( 'CC Mail' ).
      lo_column->set_short_text( 'CC Mail' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column = lr_columns->get_column( 'MSG' ).
      lo_column->set_long_text( 'Message' ).
      lo_column->set_medium_text( 'Message' ).
      lo_column->set_short_text( 'Message' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  lo_gr_alv->display( ).
ENDFORM.
