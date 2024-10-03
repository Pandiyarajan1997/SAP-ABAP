*----------------------------------------------------------------------*
*      Print of a invoice by SAPscript SMART FORMS               *
*----------------------------------------------------------------------*

REPORT zrlb_invoice.

TABLES adr6.

DATA: i_otf       TYPE itcoo    OCCURS 0 WITH HEADER LINE,
      i_tline     LIKE tline    OCCURS 0 WITH HEADER LINE,
      i_record    LIKE solisti1 OCCURS 0 WITH HEADER LINE,
      i_xstring   TYPE xstring,
* Objects to send mail.
      i_objpack   LIKE sopcklsti1 OCCURS 0 WITH HEADER LINE,
      i_objtxt    LIKE solisti1   OCCURS 0 WITH HEADER LINE,
      i_objbin    LIKE solix      OCCURS 0 WITH HEADER LINE,
      i_reclist   LIKE somlreci1  OCCURS 0 WITH HEADER LINE,
* Work Area declarations
      wa_objhead  TYPE soli_tab,
      w_ctrlop    TYPE ssfctrlop,
      w_compop    TYPE ssfcompop,
      w_return    TYPE ssfcrescl,
      wa_buffer   TYPE string,
* Variables declarations
      v_form_name TYPE rs38l_fnam,
      v_len_in    LIKE sood-objlen.

DATA : lv_msg TYPE char30 .

* declaration of data
INCLUDE rlb_invoice_data_declare.
* definition of forms
INCLUDE rlb_invoice_form01.
INCLUDE rlb_print_forms.

*---------------------------------------------------------------------*
*       FORM ENTRY
*---------------------------------------------------------------------*
FORM entry USING return_code us_screen.

  DATA: lf_retcode TYPE sy-subrc.
  CLEAR retcode.
  xscreen = us_screen.
  PERFORM processing USING us_screen
                     CHANGING lf_retcode.
  IF lf_retcode NE 0.
    return_code = 1.
  ELSE.
    return_code = 0.
  ENDIF.

ENDFORM.                    "ENTRY
*---------------------------------------------------------------------*
*       FORM PROCESSING                                               *
*---------------------------------------------------------------------*
FORM processing USING proc_screen
                CHANGING cf_retcode.


  DATA: ls_print_data_to_read TYPE lbbil_print_data_to_read.
  DATA: ls_bil_invoice TYPE lbbil_invoice.
  DATA : is_bill_invoice TYPE lbbil_invoice.
  DATA: lf_fm_name            TYPE rs38l_fnam.
  DATA: ls_control_param      TYPE ssfctrlop.
  DATA: ls_composer_param     TYPE ssfcompop.
  DATA: ls_recipient          TYPE swotobjid.
  DATA: ls_sender             TYPE swotobjid.
  DATA: lf_formname           TYPE tdsfname.
  DATA: ls_addr_key           LIKE addr_key.
  DATA: ls_dlv-land           LIKE vbrk-land1.
  DATA: ls_job_info           TYPE ssfcrescl.

*  CASE sy-tcode.
*    WHEN 'VF01' OR 'VF02' OR 'VF03'.
*
*    WHEN OTHERS. EXIT .
*  ENDCASE.
* SmartForm from customizing table TNAPR
  lf_formname = tnapr-sform.

* BEGIN: Country specific extension for Hungary
  DATA: lv_ccnum TYPE idhuccnum,
        lv_error TYPE c.

* If a valid entry exists for the form in customizing view
* IDHUBILLINGOUT then the localized output shall be used.
  SELECT SINGLE ccnum INTO lv_ccnum FROM idhubillingout WHERE
    kschl = nast-kschl.

  IF sy-subrc EQ 0.
    IF lv_ccnum IS INITIAL.
      lv_ccnum = 1.
    ENDIF.

    IF ( nast-delet IS INITIAL OR nast-dimme IS INITIAL ).

      nast-delet = 'X'.
      nast-dimme = 'X'.

      sy-msgid = 'IDFIHU'.
      sy-msgty = 'W'.
      sy-msgno = 201.
      sy-msgv1 = nast-objky.

      CALL FUNCTION 'NAST_PROTOCOL_UPDATE'
        EXPORTING
          msg_arbgb = sy-msgid
          msg_nr    = sy-msgno
          msg_ty    = sy-msgty
          msg_v1    = sy-msgv1
          msg_v2    = ''
          msg_v3    = ''
          msg_v4    = ''
        EXCEPTIONS
          OTHERS    = 1.
    ENDIF.
  ELSE.
    CLEAR lv_ccnum.
  ENDIF.
* END: Country specific extension for Hungary
  " Started by Puratchi
  DATA : l_fmname     TYPE funcname,
         l_params     TYPE sfpoutputparams,
         l_docparams  TYPE sfpdocparams,
         l_formoutput TYPE fpformoutput.
  IF 1 = 2.
    lf_formname = 'ZSD_IRN_QR_EINVOICE_NEW'. " Ssrtforms
  ENDIF.

  SELECT SINGLE bukrs FROM vbrk
    INTO @DATA(l_comcode)
    WHERE vbeln = @nast-objky(10).
  IF l_comcode = 'DMS1' OR l_comcode = 'ALPN' OR l_comcode = 'F001' OR l_comcode = 'F002'.
*    lf_formname = 'ZSD_IRN_QR_EINVOICE_DMS'.
    lf_formname = 'ZSD_INV_DMS_NEW'.
  ENDIF.
  PERFORM set_print_data_to_read USING    lf_formname
                                 CHANGING ls_print_data_to_read
                                 cf_retcode.
  PERFORM get_data USING    ls_print_data_to_read
                   CHANGING ls_addr_key
                            ls_dlv-land
                            ls_bil_invoice
                            cf_retcode.

*  IF   ls_bil_invoice-hd_gen-bil_type EQ 'YBDP'
*    OR ls_bil_invoice-hd_gen-bil_type EQ 'YBTE'
*    OR ls_bil_invoice-hd_gen-bil_type EQ 'YIRE'
*    OR ls_bil_invoice-hd_gen-bil_type EQ 'YBTR'.
*    lf_formname = 'ZSD_IRN_QR_EINVOICE_YBDP'.
*  ENDIF.
*  PERFORM set_print_param USING    ls_addr_key
*                                   ls_dlv-land
*                          CHANGING ls_control_param
*                                   ls_composer_param
*                                   ls_recipient
*                                   ls_sender
*                                   cf_retcode.


  CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
    EXPORTING
      i_name     = lf_formname
    IMPORTING
      e_funcname = l_fmname
*     E_INTERFACE_TYPE           =
    .
*  l_params-getpdf = 'X'.

*  l_params-dest = 'LP01'.
*  IF sy-tcode = 'VF01'.
*    l_params-dest = 'ZLP01'.
*  ENDIF.
  l_params-nodialog = ''.
  l_params-preview = 'X'.
*  IF sy-tcode NE 'VF01'.
*    l_params-getpdf = 'X'.
*  ENDIF.

  CALL FUNCTION 'FP_JOB_OPEN'
    CHANGING
      ie_outputparams = l_params
    EXCEPTIONS
      cancel          = 1
      usage_error     = 2
      system_error    = 3
      internal_error  = 4
      OTHERS          = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  l_docparams-langu = 'E'.

  CALL FUNCTION l_fmname
    EXPORTING
      /1bcdwb/docparams  = l_docparams
      is_bil_invoice     = ls_bil_invoice
    IMPORTING
      /1bcdwb/formoutput = l_formoutput
    EXCEPTIONS
      usage_error        = 1
      system_error       = 2
      internal_error     = 3
      OTHERS             = 4.
  IF sy-subrc <> 0.
    cf_retcode = sy-subrc.
    PERFORM protocol_update.
  ELSE.
*    IF sy-tcode NE 'VF01'.
*      DATA: lo_object_cls TYPE REF TO zcl_scm_sales_to_invoice.
*      CREATE OBJECT lo_object_cls.
*      DATA(l_invoice) = ls_bil_invoice-hd_gen-bil_number.
*      IF ls_bil_invoice-hd_gen-bil_date+4(4) BETWEEN 0401 AND 1231.
*        DATA(l_year) = ls_bil_invoice-hd_gen-bil_date(4).
*      ELSE.
*        l_year = ls_bil_invoice-hd_gen-bil_date(4) - 1.
*      ENDIF.
**Email Triggering Method
*      CALL METHOD lo_object_cls->digital_sign
*        EXPORTING
*          vbeln    = l_invoice
*          bukrs    = '1000'
*          gjahr    = l_year
*          pdf      = l_formoutput-pdf
*        IMPORTING
*          sign_pdf = DATA(sign_pdf).
*
**    SELECT SINGLE pdf FROM zhr_emp_letters INTO l_formoutput-pdf.
**      l_formoutput-pdf = sign_pdf.
*
*      DATA(lv_pdf) = l_formoutput-pdf.
*
*      DATA: lv_dummy    TYPE c,
*            lv_clen     TYPE i,
*            lv_line(72) TYPE c,
*            lv_len      TYPE i,
*            lv_size     TYPE i,
*            lv_hex(144) TYPE x.
*      DATA: lt_otf_data TYPE tsfotf,
*            w_otf_data  TYPE itcoo.
*      FIELD-SYMBOLS: <l_fs> TYPE c.
*
*      lv_size = 72.
*      DESCRIBE FIELD lv_dummy LENGTH lv_clen IN BYTE MODE.
*      lv_size = lv_size * lv_clen.
*      lv_len = xstrlen( lv_pdf ).
*      WHILE lv_len > lv_size.
*        lv_hex(lv_size) = lv_pdf(lv_size).
*        ASSIGN lv_hex(lv_size) TO <l_fs> CASTING.
*        lv_line = <l_fs>.
*        w_otf_data = lv_line.
*        APPEND w_otf_data TO lt_otf_data.
*        SHIFT lv_pdf LEFT BY lv_size PLACES IN BYTE MODE.
*        lv_len = xstrlen( lv_pdf ).
*      ENDWHILE.
*      IF lv_len >= 0.
*        CLEAR: lv_hex, w_otf_data, lv_line.
*        lv_hex = lv_pdf(lv_len).
*        ASSIGN lv_hex(lv_size) TO <l_fs> CASTING.
*        lv_line = <l_fs>.
*        w_otf_data = lv_line.
*        APPEND w_otf_data TO lt_otf_data.
*      ENDIF.
*
*      CALL FUNCTION 'SSFCOMP_PDF_PREVIEW'
*        EXPORTING
*          i_otf                    = lt_otf_data[]
*        EXCEPTIONS
*          convert_otf_to_pdf_error = 1
*          cntl_error               = 2
*          OTHERS                   = 3.
*      IF sy-subrc <> 0.
**Not Applicable
*      ENDIF.
*    ENDIF.
  ENDIF.

  DATA  ls_result         TYPE sfpjoboutput.
  CALL FUNCTION 'FP_JOB_CLOSE'
    IMPORTING
      e_result       = ls_result
    EXCEPTIONS
      usage_error    = 1
      system_error   = 2
      internal_error = 3
      OTHERS         = 4.
  IF sy-subrc <> 0.
    cf_retcode = sy-subrc.
    PERFORM protocol_update.
  ENDIF.

*  IF sy-tcode = 'VF01' AND
*    ( ls_bil_invoice-hd_gen-bil_type EQ 'YBDP'
*       OR ls_bil_invoice-hd_gen-bil_type EQ 'YBTE' ).
*
*    DATA: lo_object_cls TYPE REF TO zcl_scm_sales_to_invoice.
*    CREATE OBJECT lo_object_cls.
*
*    IF ls_bil_invoice-hd_gen-bil_date+4(4) BETWEEN 0401 AND 1231.
*      DATA(l_year) = ls_bil_invoice-hd_gen-bil_date(4).
*    ELSE.
*      l_year = ls_bil_invoice-hd_gen-bil_date(4) - 1.
*    ENDIF.
**    IF ls_bil_invoice-hd_gen-bil_date+4(4) BETWEEN 0101 AND 0331.
**      l_year  = ls_bil_invoice-hd_gen-bil_date(4) - 1.
**    ENDIF.
*
*    DATA(l_invoice) = ls_bil_invoice-hd_gen-bil_number.
**Email Triggering Method
*    CALL METHOD lo_object_cls->mail_sent
*      EXPORTING
*        vbeln     = l_invoice
*        gjahr     = l_year
*      IMPORTING
*        mail_sent = DATA(l_flag).
*  ENDIF.
*  " Ended by Puratchi
*
  IF 1 = 2.
* determine print data
    PERFORM set_print_data_to_read USING    lf_formname
                                   CHANGING ls_print_data_to_read
                                   cf_retcode.

    IF cf_retcode = 0.
* select print data
      PERFORM get_data USING    ls_print_data_to_read
                       CHANGING ls_addr_key
                                ls_dlv-land
                                ls_bil_invoice
                                cf_retcode.
    ENDIF.

    IF cf_retcode = 0.
      PERFORM set_print_param USING    ls_addr_key
                                       ls_dlv-land
                              CHANGING ls_control_param
                                       ls_composer_param
                                       ls_recipient
                                       ls_sender
                                       cf_retcode.
    ENDIF.
*
    IF cf_retcode = 0.
* determine smartform function module for invoice
      CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
        EXPORTING
          formname           = lf_formname
*         variant            = ' '
*         direct_call        = ' '
        IMPORTING
          fm_name            = lf_fm_name
        EXCEPTIONS
          no_form            = 1
          no_function_module = 2
          OTHERS             = 3.
      IF sy-subrc <> 0.
*   error handling
        cf_retcode = sy-subrc.
        PERFORM protocol_update.
      ENDIF.
    ENDIF.

    IF cf_retcode = 0.
      PERFORM check_repeat.
      IF ls_composer_param-tdcopies EQ 0.
        nast_anzal = 1.
      ELSE.
        nast_anzal = ls_composer_param-tdcopies.
      ENDIF.
      ls_composer_param-tdcopies = 1.

      DO nast_anzal TIMES.
* In case of repetition only one time archiving
        IF sy-index > 1 AND nast-tdarmod = 3.
          nast_tdarmod = nast-tdarmod.
          nast-tdarmod = 1.
          ls_composer_param-tdarmod = 1.
        ENDIF.
        IF sy-index NE 1 AND repeat IS INITIAL.
          repeat = 'X'.
        ENDIF.
* BEGIN: Country specific extension for Hungary
        IF lv_ccnum IS NOT INITIAL.
          IF nast-repid IS INITIAL.
            nast-repid = 1.
          ELSE.
            nast-repid = nast-repid + 1.
          ENDIF.
          nast-pfld1 = lv_ccnum.
        ENDIF.
* END: Country specific extension for Hungary
* call smartform invoice
        IF sy-ucomm = 'PRNT'.
          ls_control_param-getotf = abap_true.
        ENDIF.

        CALL FUNCTION lf_fm_name
          EXPORTING
            archive_index      = toa_dara
            archive_parameters = arc_params
            control_parameters = ls_control_param
*           mail_appl_obj      =
            mail_recipient     = ls_recipient
            mail_sender        = ls_sender
            output_options     = ls_composer_param
            user_settings      = space
            is_bil_invoice     = ls_bil_invoice
            is_nast            = nast
            is_repeat          = repeat
          IMPORTING
            job_output_info    = ls_job_info
*           document_output_info =
*           job_output_options =
          EXCEPTIONS
            formatting_error   = 1
            internal_error     = 2
            send_error         = 3
            user_canceled      = 4
            OTHERS             = 5.
        IF sy-subrc <> 0.
*   error handling
          cf_retcode = sy-subrc.
          PERFORM protocol_update.
* get SmartForm protocoll and store it in the NAST protocoll
          PERFORM add_smfrm_prot.
        ENDIF.
      ENDDO.
******* Begin of changes BY CFABAP on 16/07/2021
*      DATA ls_fpcontent TYPE fpcontent.
** LS_FPCONTENT-no_dialog = 'X'.
*      IF sy-ucomm = 'PRNT'.
*        CALL FUNCTION 'HR_IT_DISPLAY_WITH_PDF'
**      EXPORTING
**        IV_PDF    =
*          TABLES
*            otf_table = ls_job_info-otfdata.
*      ENDIF.
****** End of changes BY CFABAP on 16/07/2021
** get SmartForm spoolid and store it in the NAST protocoll
*      DATA ls_spoolid LIKE LINE OF ls_job_info-spoolids.
*      LOOP AT ls_job_info-spoolids INTO ls_spoolid.
*        IF ls_spoolid NE space.
*          PERFORM protocol_update_spool USING '342' ls_spoolid
*                                              space space space.
*        ENDIF.
*      ENDLOOP.
*      ls_composer_param-tdcopies = nast_anzal.
*      IF NOT nast_tdarmod IS INITIAL.
*        nast-tdarmod = nast_tdarmod.
*        CLEAR nast_tdarmod.
*      ENDIF.
    ENDIF.

  ENDIF.
*
*  IF sy-tcode EQ 'VF01' .
**** getting TO mail ID ***
*    SELECT * FROM tvarvc INTO TABLE @DATA(lt_tvarvc)
*                         WHERE name = 'ZRLB_INVOICE_ORDER_TYPE'
*                         AND type = 'S'.
*    IF sy-subrc EQ 0.
*      DATA(lv_order) = VALUE #( lt_tvarvc[ low = ls_bil_invoice-hd_gen-bil_type ]-low OPTIONAL ).
*    ENDIF.
*    IF ls_bil_invoice-hd_gen-bil_type EQ lv_order."'YBDP' .
**    IF lv_order IS NOT INITIAL.
*      CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
*        EXPORTING
*          formname           = 'ZSD_GST_TAX_INVOICE_NEW'
**         variant            = ' '
**         direct_call        = ' '
*        IMPORTING
*          fm_name            = v_form_name
*        EXCEPTIONS
*          no_form            = 1
*          no_function_module = 2
*          OTHERS             = 3.
*      IF sy-subrc <> 0.
**   error handling
*        cf_retcode = sy-subrc.
*        PERFORM protocol_update.
*      ENDIF.
*
*      ls_control_param-getotf = abap_true.
*      ls_control_param-no_dialog = abap_true.
*      ls_composer_param-tdnoprev = abap_true.
*      ls_control_param-preview = space.
*      ls_composer_param-tddest = 'LOCL'.
*
*      CALL FUNCTION lf_fm_name
*        EXPORTING
*          archive_index      = toa_dara
*          archive_parameters = arc_params
*          control_parameters = ls_control_param
*  "       mail_appl_obj      =
*          mail_recipient     = ls_recipient
*          mail_sender        = ls_sender
*          output_options     = ls_composer_param
*          "USER_SETTINGS        = SPACE
*          user_settings      = abap_true
*          is_bil_invoice     = ls_bil_invoice
*          is_bill_invoice    = is_bill_invoice
*          is_nast            = nast
*          is_repeat          = repeat
*        IMPORTING
*          job_output_info    = w_return
*        " DOCUMENT_OUTPUT_INFO = LS_DOC_INFO
*        " JOB_OUTPUT_OPTIONS = LS_JOB_OUTPUT
*        EXCEPTIONS
*          formatting_error   = 1
*          internal_error     = 2
*          send_error         = 3
*          user_canceled      = 4
*          OTHERS             = 5.
*      IF sy-subrc <> 0.
**   error handling
*        cf_retcode = sy-subrc.
*        PERFORM protocol_update.
** get SmartForm protocoll and store it in the NAST protocoll
*        PERFORM add_smfrm_prot.
*      ENDIF.
*
*      DATA : lv_kunnr      TYPE kna1-kunnr,
*             lv1_kunnr     TYPE kna1-kunnr,
*             "LV_ADRNR TYPE KNA1-ADRNR,
*             lv_adrnr      TYPE so_rec_ext,
*             lv_smtp_addr  TYPE adr6-smtp_addr,
*             lv_usrid_long TYPE pa0105-usrid_long.
*      DATA : lv_pernr TYPE vbpa-pernr.
*      DATA : it_lb_t_bil_hd_part_add TYPE TABLE OF lb_t_bil_hd_part_add.
*      DATA : wa_ls_bil_invoice-hd_part_add TYPE lb_t_bil_hd_part_add.
*      CLEAR : lv_adrnr,lv_smtp_addr.
*
*      DATA: in_mailid  TYPE ad_smtpadr,
*            in_mailid1 TYPE ad_smtpadr.
**            in_mailid2 TYPE ad_smtpadr,
**            in_mailid3 TYPE ad_smtpadr.
**** getting TO mail ID ***
*      CLEAR lt_tvarvc[].
*      SELECT * FROM tvarvc INTO TABLE lt_tvarvc
*                           WHERE name = 'ZRLB_INVOICE_PATNER_TYPE'
*                           AND type = 'S'.
*      IF sy-subrc EQ 0.
*        DATA lr_parvw TYPE RANGE OF parvw.
*
*        lr_parvw = VALUE #(  FOR <lfs_tvarvc> IN lt_tvarvc
*                                    ( sign = 'I'
*                                      option = 'EQ'
*                                      low = <lfs_tvarvc>-low )
*                              ).
*      ENDIF.
*      CLEAR lt_tvarvc[].
**      DATA: lt_mailcc TYPE TABLE OF ad_smtpadr,
**            ls_mailcc TYPE ad_smtpadr.
*      CHECK lr_parvw IS NOT INITIAL.
**      SELECT pernr INTO lv_pernr UP TO 1 ROWS FROM vbpa WHERE vbeln = ls_bil_invoice-hd_ref-order_numb AND parvw EQ 'L5'   ORDER BY PRIMARY KEY.
**      ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation
**      SELECT usrid_long INTO lv_usrid_long UP TO 1 ROWS FROM pa0105 WHERE pernr = lv_pernr AND subty = '0010'  ORDER BY PRIMARY KEY.
**      ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation
*      SELECT pernr INTO TABLE @DATA(lt_pernr)
*        FROM vbpa
*       WHERE vbeln = @ls_bil_invoice-hd_ref-order_numb
*         AND parvw IN @lr_parvw ORDER BY PRIMARY KEY."EQ 'L5'
*      IF sy-subrc = 0.
*        SELECT usrid_long INTO TABLE @DATA(lt_usrid_long)
*          FROM pa0105 FOR ALL ENTRIES IN @lt_pernr
*          WHERE pernr = @lt_pernr-pernr
*          AND   subty = '0010'.
*      ENDIF.
*
*      i_otf[] = w_return-otfdata[].
*
*      CALL FUNCTION 'CONVERT_OTF'
*        EXPORTING
*          format                = 'PDF'
*          max_linewidth         = 132
*        IMPORTING
*          bin_filesize          = v_len_in
*          bin_file              = i_xstring   " This is NOT Binary. This is Hexa
*        TABLES
*          otf                   = i_otf
*          lines                 = i_tline
*        EXCEPTIONS
*          err_max_linewidth     = 1
*          err_format            = 2
*          err_conv_not_possible = 3
*          OTHERS                = 4.
*      CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
*        EXPORTING
*          buffer     = i_xstring
*        TABLES
*          binary_tab = i_objbin[].
*      DATA: salutation TYPE string.
*      DATA: body TYPE string.
*      DATA: footer TYPE string.
*      DATA: lo_send_request TYPE REF TO cl_bcs,
*            lo_document     TYPE REF TO cl_document_bcs,
*            lo_sender       TYPE REF TO if_sender_bcs,
*            sender_mail     TYPE  adr6-smtp_addr,
*            lo_recipient    TYPE REF TO if_recipient_bcs VALUE IS INITIAL,lt_message_body TYPE bcsy_text,
*            lo_recipient1   TYPE REF TO if_recipient_bcs VALUE IS INITIAL, " if_recipient_bcs VALUE IS INITIAL,"lt_message_body TYPE bcsy_text,
*            lo_recipient2   TYPE REF TO if_recipient_bcs VALUE IS INITIAL,
*            lo_recipient3   TYPE REF TO if_recipient_bcs VALUE IS INITIAL,
*            lx_document_bcs TYPE REF TO cx_document_bcs,
*            lv_sent_to_all  TYPE os_boolean.
*      DATA : lv_vbeln TYPE vbrk-vbeln,
*             lv_ind   TYPE char20,
*             lv_netwr TYPE string,
*             lv_tot   TYPE char50.
*      DATA : lv_sub TYPE sood-objdes.
*      lv_sub = ls_bil_invoice-hd_gen-bil_number.
*      lv_vbeln = ls_bil_invoice-hd_gen-bil_number.
*      lv_ind = ls_bil_invoice-hd_gen-bil_date .
*      lv_netwr = ls_bil_invoice-hd_gen-bil_netwr.
*      CONCATENATE 'Sheenlac Paints Limited Invoice No :' lv_vbeln INTO lv_tot SEPARATED BY space ."'Date:' LV_IND 'Value :' LV_NETWR INTO LV_TOT SEPARATED BY SPACE .
*      "create send request
*      "create send request
*      TRY.
*          lo_send_request = cl_bcs=>create_persistent( ).
*        CATCH  cx_send_req_bcs INTO DATA(lx_send_req_bcs4).
*      ENDTRY.
*      "create message body and subject
*      salutation ='Dear Customer ,'.
*      APPEND salutation TO lt_message_body.
*      APPEND INITIAL LINE TO lt_message_body.
*      body = '          With reference to your order, the above invoice has been generated at our end.'.
*      APPEND body TO lt_message_body.
*      APPEND INITIAL LINE TO lt_message_body.
*      body = 'If you have any queries / doubts, kindly contact us on 8300030404 (or) send us the mail to ccchennai@sheenlac.in'.
*      APPEND body TO lt_message_body.
*      APPEND INITIAL LINE TO lt_message_body.
*      footer = 'Thanks & Best Regards,'.
*      APPEND footer TO lt_message_body.
*      footer = 'Customer Care (Sheenlac Paints Ltd)'.
*      APPEND footer TO lt_message_body.
*
*      "put your text into the document
*      TRY.
*          lo_document = cl_document_bcs=>create_document(
*          i_type = 'RAW'
*          i_text = lt_message_body
*          i_subject = lv_tot ).
*        CATCH cx_document_bcs INTO DATA(lx_document_bcs1).
*      ENDTRY.
*
*      TRY.
*          lo_document->add_attachment(
*          EXPORTING
*          i_attachment_type = 'PDF'
*          i_attachment_subject = lv_sub
*          i_att_content_hex = i_objbin[] ).
*        CATCH cx_document_bcs INTO lx_document_bcs.
*      ENDTRY.
** Add attachment
** Pass the document to send request
*      TRY.
*          lo_send_request->set_document( lo_document ).
*        CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs3).
*      ENDTRY.
**** Selecting Sender Mail ID ***
*      SELECT SINGLE low FROM tvarvc INTO @DATA(lv_sender)
*                        WHERE name = 'ZRLB_SENDER_MAIL'
*                        AND type = 'P'.
*      IF sy-subrc = 0.
*        "Create sender
*        sender_mail = lv_sender.
*      ENDIF.
*
*      TRY.
*          lo_sender = cl_cam_address_bcs=>create_internet_address( sender_mail ).
*        CATCH cx_address_bcs INTO DATA(lx_address_bcs2).
*      ENDTRY.
*
*      TRY.
*          lo_send_request->set_sender( lo_sender ).
*        CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs6).
*      ENDTRY.
*
*      lv1_kunnr = ls_bil_invoice-hd_gen-bill_to_party .
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*        EXPORTING
*          input  = lv1_kunnr
*        IMPORTING
*          output = lv_kunnr.
*
**** changed on 09.10.2022 by Samsudeen **
*      IF sy-sysid = 'DEV' OR sy-sysid = 'QAS'.
*** Development and Quality Means  **
*        SELECT SINGLE low FROM tvarvc
*                       INTO @DATA(lv_mail_to)
*                       WHERE name = 'ZRLB_INVOICE'
*                       AND type = 'P'.
*        IF sy-subrc = 0.
*          in_mailid = lv_mail_to.
*        ELSE.
*          CLEAR in_mailid.
*        ENDIF.
*
*      ELSEIF sy-sysid = 'PRD'.
** prd SYSTEM means **
*        DATA: lt_mailcc TYPE TABLE OF ad_smtpadr,
*              ls_mailcc TYPE ad_smtpadr.
*        REFRESH lt_mailcc.
*        CLEAR: lv_adrnr,lv_smtp_addr.
*        SELECT SINGLE adrnr INTO lv_adrnr FROM kna1 WHERE kunnr = ls_bil_invoice-hd_gen-sold_to_party .
*        SELECT smtp_addr INTO lv_smtp_addr UP TO 1 ROWS FROM adr6 WHERE addrnumber = lv_adrnr ORDER BY PRIMARY KEY.
*        ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation
**** getting CC mail ID ***
*        SELECT * FROM tvarvc INTO TABLE lt_tvarvc
*                             WHERE name = 'ZRLB_INVOICE_MAIL_CC'
*                             AND type = 'S'.
*        IF sy-subrc EQ 0.
*          LOOP AT lt_tvarvc ASSIGNING FIELD-SYMBOL(<lfs_tvarvcc>).
*            ls_mailcc = <lfs_tvarvcc>-low.
*            APPEND ls_mailcc TO lt_mailcc.
*            CLEAR ls_mailcc.
*          ENDLOOP.
*        ENDIF.
*
*        CLEAR:lt_tvarvc[], in_mailid.
*        in_mailid = lv_smtp_addr.
**          in_mailid1 = lv_usrid_long.
*      ENDIF.
*** End of Changes on 09.10.2022 ***
*      IF in_mailid IS NOT INITIAL. " Customer ID
*        "Create recipient
*        TRY.
*            lo_recipient = cl_cam_address_bcs=>create_internet_address( in_mailid ).
*          CATCH cx_address_bcs INTO DATA(lx_address_bcs1).
*        ENDTRY.
**set recipient
*        TRY.
*            lo_send_request->add_recipient(
*               EXPORTING
*               i_recipient = lo_recipient
*                i_express = abap_true
*               ).
*          CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs5).
*        ENDTRY.
*      ENDIF.
*      LOOP AT lt_usrid_long ASSIGNING FIELD-SYMBOL(<mail_id>).
*        in_mailid1 = <mail_id>-usrid_long.
*        TRY.
*            lo_recipient1 = cl_cam_address_bcs=>create_internet_address( in_mailid1 ).
*          CATCH cx_address_bcs INTO DATA(lx_address_bcs5).
*        ENDTRY.
**set recipient
*        TRY.
*            lo_send_request->add_recipient(
*               EXPORTING
*               i_recipient = lo_recipient1
*               i_copy = 'X'
*               i_express = abap_true
*               ).
*          CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs7).
*        ENDTRY.
*      ENDLOOP.
**        IF in_mailid1 IS NOT INITIAL.
**          TRY.
**              lo_recipient1 = cl_cam_address_bcs=>create_internet_address( in_mailid1 ).
**            CATCH cx_address_bcs INTO DATA(lx_address_bcs5).
**          ENDTRY.
***set recipient
**          TRY.
**              lo_send_request->add_recipient(
**                 EXPORTING
**                 i_recipient = lo_recipient1
**                  i_express = abap_true
**                 ).
**            CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs7).
**          ENDTRY.
**        ENDIF.
*
*      IF lt_mailcc[] IS NOT INITIAL.
*        LOOP AT lt_mailcc ASSIGNING FIELD-SYMBOL(<lfs_mailcc>).
*          TRY.
*              lo_recipient2 = cl_cam_address_bcs=>create_internet_address( <lfs_mailcc> ).
*            CATCH cx_address_bcs INTO DATA(lx_address_bcs3).
*          ENDTRY.
*
*          TRY.
*              lo_send_request->add_recipient(
*               EXPORTING
*               i_recipient = lo_recipient2
*               i_copy = 'X'
*               i_express = abap_true
*               ).
**                in_mailid2 = <lfs_mailcc>.
*            CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs1).
*          ENDTRY.
*        ENDLOOP.
**          CLEAR in_mailid2.
*      ENDIF.
** Send email
*      TRY.
*          lo_send_request->send(
*          EXPORTING
*          i_with_error_screen = abap_true
*          RECEIVING
*          result = lv_sent_to_all ).
*        CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs).
*      ENDTRY.
*
**      COMMIT WORK.
*      CLEAR : in_mailid.
**        CLEAR in_mailid1,in_mailid2,in_mailid3.
*    ENDIF.
*  ENDIF.


ENDFORM.                    "PROCESSING
