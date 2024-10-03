*&---------------------------------------------------------------------*
*& Include          ZMM_AUTO_PO_CREATE_CLS
*&---------------------------------------------------------------------*
CLASS lcl_po_create DEFINITION.
  PUBLIC SECTION.

    METHODS  background_job_chk
      IMPORTING pname TYPE syst-cprog
      CHANGING  l_msg TYPE char50.

    METHODS: log_data_fetch, initiated_status, no_mapping_status, mail_sending, alv_display.
ENDCLASS.

CLASS lcl_po_create IMPLEMENTATION.

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

  METHOD log_data_fetch.
    REFRESH: gt_logs.
    SELECT * FROM zmm_autopo_log
      INTO TABLE gt_logs
      WHERE uniqueid IN s_unqid
      AND type IN ( 'I' , 'E' , 'N' ).
    IF sy-subrc = 0.
      SORT gt_logs[] BY uniqueid.
    ENDIF.
  ENDMETHOD.

  METHOD initiated_status.
*New Initiated Status entries
    LOOP AT gt_logs ASSIGNING <fs_logs> WHERE ( type = c_initiated OR type = c_error ).
      CLEAR: lv_ebeln, lv_msg.
      PERFORM purchase_order USING <fs_logs> CHANGING lv_ebeln lv_msg.
      IF lv_ebeln IS NOT INITIAL.
        <fs_logs>-ebeln = lv_ebeln.
        <fs_logs>-type  = 'P'.
        <fs_logs>-msg   = TEXT-001.
      ELSE.
        <fs_logs>-type  = 'E'.
        <fs_logs>-msg   = lv_msg.
      ENDIF.
      MODIFY zmm_autopo_log FROM <fs_logs>.
    ENDLOOP.
  ENDMETHOD.
*No Mapping Entries Processing
  METHOD no_mapping_status.
    LOOP AT gt_logs ASSIGNING <fs_logs> WHERE ( type = c_nomap ).
      SELECT SINGLE * FROM zmm_po_mapping
        INTO @DATA(l_po_mapping)
        WHERE werks = @<fs_logs>-werks
        AND matnr = @<fs_logs>-matnr
        AND datbi GE @sy-datum
        AND datab LE @sy-datum
        AND loevm NE @space.
      IF sy-subrc EQ 0.
        <fs_logs>-reswk = l_po_mapping-reswk.
        <fs_logs>-lifnr = l_po_mapping-lifnr.
        CLEAR: lv_ebeln, lv_msg.
        PERFORM purchase_order USING <fs_logs> CHANGING lv_ebeln lv_msg.
        IF lv_ebeln IS NOT INITIAL.
          <fs_logs>-ebeln = lv_ebeln.
          <fs_logs>-type  = 'P'.
          <fs_logs>-msg   = TEXT-001.
        ELSE.
          <fs_logs>-type  = 'E'.
          <fs_logs>-msg   = lv_msg.
        ENDIF.
        MODIFY zmm_autopo_log FROM <fs_logs>.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

*Email sending error Message
  METHOD mail_sending.
    DATA: lt_attachment TYPE STANDARD TABLE OF solisti1,
          ls_attachment LIKE LINE OF lt_attachment.
    DATA: lv_data TYPE string.
    DATA: lv_body(300) TYPE c,
          lv_date(10)  TYPE c.
    DATA: in_mailid  TYPE ad_smtpadr.
    DATA: lv_xstring TYPE xstring.
    DATA: lt_bin TYPE solix_tab.
    DATA : lv_sub TYPE sood-objdes.
    DATA: it_body_msg   TYPE STANDARD TABLE OF solisti1.
    DATA: salutation TYPE string.
    DATA: body TYPE string.
    DATA: footer TYPE string.
    DATA: lo_send_request TYPE REF TO cl_bcs,
          lo_document     TYPE REF TO cl_document_bcs,
          lo_sender       TYPE REF TO if_sender_bcs,
          sender_mail     TYPE  adr6-smtp_addr,
          lo_recipient    TYPE REF TO if_recipient_bcs VALUE IS INITIAL,lt_message_body TYPE bcsy_text,
          lo_recipient1   TYPE REF TO if_recipient_bcs VALUE IS INITIAL, " if_recipient_bcs VALUE IS INITIAL,"lt_message_body TYPE bcsy_text,
          lo_recipient2   TYPE REF TO if_recipient_bcs VALUE IS INITIAL,
          lo_recipient3   TYPE REF TO if_recipient_bcs VALUE IS INITIAL,
          lv_sent_to_all  TYPE os_boolean.

    DATA: lv_tot   TYPE char50.

    CLASS cl_abap_char_utilities DEFINITION LOAD.
    CONSTANTS:
      con_tab  TYPE c VALUE cl_abap_char_utilities=>horizontal_tab,
      con_cret TYPE c VALUE cl_abap_char_utilities=>cr_lf.

    REFRESH: lt_attachment.
    CONCATENATE  'Unique ID'
                 'Plant'
                 'Material Number'
                 'Quantity'
                 'Supplying plant'
                 'Vendor'
                 'Order Type'
                 'Date'
                 'Purchasing DocNumber'
                 'Status'
                 'Message'
                  INTO ls_attachment SEPARATED BY con_tab.
    APPEND ls_attachment TO lt_attachment.
    CLEAR  ls_attachment.

    CONCATENATE  ls_attachment lv_data
       INTO lv_data SEPARATED BY cl_abap_char_utilities=>newline.

    LOOP AT gt_logs ASSIGNING FIELD-SYMBOL(<fs_alv>).
      DATA(lv_qty) = CONV char20( <fs_alv>-menge ).
      CONCATENATE <fs_alv>-uniqueid
                  <fs_alv>-werks
                  <fs_alv>-matnr
                  lv_qty
                  <fs_alv>-reswk
                  <fs_alv>-lifnr
                  <fs_alv>-bsart
                  <fs_alv>-erdat
                  <fs_alv>-ebeln
                  <fs_alv>-type
                  <fs_alv>-msg
                  INTO ls_attachment SEPARATED BY con_tab.

      CONCATENATE con_cret ls_attachment
      INTO ls_attachment.
      APPEND ls_attachment TO lt_attachment.
      CLEAR ls_attachment.
    ENDLOOP.

    REFRESH: lt_bin,lt_message_body,it_body_msg.
    CALL FUNCTION 'SCMS_TEXT_TO_BINARY'
      TABLES
        text_tab   = lt_attachment
        binary_tab = lt_bin.

    "create send request
    TRY.
        lo_send_request = cl_bcs=>create_persistent( ).
      CATCH  cx_send_req_bcs INTO DATA(lx_send_req_bcs1).
    ENDTRY.

    salutation ='Dear Sir/Madam ,'.
    APPEND salutation TO lt_message_body.
    APPEND INITIAL LINE TO lt_message_body.

    CLEAR: lv_body,lv_date.
    WRITE sy-datum TO lv_date DD/MM/YYYY.
    CONCATENATE 'Purchase Order Logs' lv_date INTO lv_body SEPARATED BY space.
    CONDENSE lv_body.
    body = lv_body.
    APPEND body TO lt_message_body.
    APPEND INITIAL LINE TO lt_message_body.

    footer = 'Thanks & Best Regards,'.
    APPEND footer TO lt_message_body.

    footer = 'Sheenlac Paints Ltd'.
    APPEND footer TO lt_message_body.

    lv_tot = 'PO History logs'.
    "put your text into the document
    TRY.
        lo_document = cl_document_bcs=>create_document(
        i_type = 'RAW'
        i_text = lt_message_body
        i_subject = lv_tot ).
      CATCH cx_document_bcs INTO DATA(lc_document_bcs1).
    ENDTRY.

    APPEND LINES OF lt_attachment TO it_body_msg.


    TRY.
        lo_document->add_attachment(
        EXPORTING
        i_attachment_type = 'XLS'
        i_attachment_subject = 'PO Logs'
        i_att_content_text = lt_attachment ).
      CATCH cx_document_bcs INTO DATA(lx_document_bcs).
    ENDTRY.

* Pass the document to send request
    TRY.
        lo_send_request->set_document( lo_document ).
      CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs2).
    ENDTRY.


    "Sender Mail ID set in TVARVC
    SELECT SINGLE * FROM tvarvc
      INTO @DATA(ls_tvarvc)
      WHERE name = 'ZMM_POLOG_MAIL'
      AND type = 'P'.

    sender_mail = ls_tvarvc-low .

    "Sender Mail ID
    TRY.
        lo_sender = cl_cam_address_bcs=>create_internet_address( sender_mail ).
      CATCH cx_address_bcs INTO DATA(lx_address_bcs2).
    ENDTRY.

    TRY.
        lo_send_request->set_sender( lo_sender ).
      CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs3).
    ENDTRY.

** TO Mail Recipients **
    SELECT * FROM tvarvc INTO
      TABLE @DATA(lt_tvarvc)
      WHERE name = 'ZMM_POLOG_MAIL_TO'
      AND type = 'S'.

    LOOP AT lt_tvarvc INTO DATA(lw_data).
      in_mailid = lw_data-low.
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
        CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs4).
      ENDTRY.
    ENDLOOP.

*** CC Mail Recipients
*    TRY.
*        lo_recipient1 = cl_cam_address_bcs=>create_internet_address( in_mailid1 ).
*      CATCH cx_address_bcs INTO DATA(lx_address_bcs).
*    ENDTRY.
*    "Set recipient
*    TRY.
*        lo_send_request->add_recipient(
*         EXPORTING
*         i_recipient = lo_recipient1
*         i_copy = 'X'
*         i_express = abap_true
*         ).
*        in_mailid2 = in_mailid1.
*      CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs5).
*    ENDTRY.

* Send email
    TRY.
        lo_send_request->send(
        EXPORTING
        i_with_error_screen = abap_true
        RECEIVING
        result = lv_sent_to_all ).
      CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs).
    ENDTRY.

    COMMIT WORK.

  ENDMETHOD.

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
            t_table      = gt_logs.
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

FORM purchase_order USING p_log TYPE zmm_autopo_log
                    CHANGING p_ebeln TYPE ebeln
                             p_msg TYPE string.
  DATA: l_msg TYPE string.
  DATA: lv_taxcode TYPE mwskz.
  DATA: lo_main TYPE REF TO zcl_purchase_order_create.
  CREATE OBJECT lo_main.

*Purchasing Group Check
  SELECT SINGLE ekgrp FROM marc
    INTO @DATA(l_purgrp)
    WHERE matnr = @p_log-matnr
    AND werks = @p_log-werks.
  IF sy-subrc EQ 0.
    IF l_purgrp IS INITIAL.
      p_msg = TEXT-003.
    ENDIF.
  ELSE.
    p_msg = TEXT-003.
  ENDIF.

*If Document type is ZNB then Payment terms Check
  IF p_log-bsart EQ 'ZNB'.
    SELECT SINGLE zterm FROM lfm1
      INTO @DATA(l_payterms)
      WHERE lifnr = @p_log-lifnr.
    IF sy-subrc EQ 0.
      IF l_payterms IS INITIAL.
        p_msg = TEXT-004.
      ENDIF.
    ELSE.
      p_msg = TEXT-004.
    ENDIF.
  ENDIF.

  IF p_msg IS INITIAL.
    CLEAR: ls_header,ls_headerx.
    ls_header-comp_code   = '1000'.
    ls_header-doc_type    = p_log-bsart.
    ls_header-creat_date  = sy-datum.
    ls_header-langu       = sy-langu.
    ls_header-vendor      = p_log-lifnr.
    ls_header-purch_org   = '1000'.
    ls_header-pur_group   = l_purgrp.
    ls_header-currency    = 'INR'.
    ls_header-doc_date    = sy-datum.
    ls_header-suppl_plnt  = p_log-reswk.
    ls_header-pmnttrms    = l_payterms.

    ls_headerx-comp_code   = abap_true.
    ls_headerx-doc_type    = abap_true.
    ls_headerx-creat_date  = abap_true.
    ls_headerx-langu       = abap_true.
    ls_headerx-vendor       = abap_true.
    ls_headerx-purch_org   = abap_true.
    ls_headerx-pur_group   = abap_true.
    ls_headerx-currency    = abap_true.
    ls_headerx-doc_date    = abap_true.
    ls_headerx-suppl_plnt  = abap_true.
    ls_headerx-pmnttrms     = abap_true.

    IF ls_header-doc_type EQ 'ZNB'.
      CLEAR: lv_taxcode,lv_msg.
      PERFORM tax_code_get USING p_log CHANGING lv_taxcode lv_msg.
    ENDIF.

    REFRESH: lt_poitem.
    APPEND VALUE #( po_item       = lv_item
                    material      = p_log-matnr
                    plant         = p_log-werks
                    quantity      = p_log-menge
                    unlimited_dlv = ''
                    tax_code      = lv_taxcode
                  ) TO lt_poitem.
    IF lv_msg IS INITIAL.
      CALL METHOD lo_main->create_purchase_order
        EXPORTING
          poheader  = ls_header
          poheaderx = ls_headerx
          item_tab  = lt_poitem
        IMPORTING
          ponumber  = p_ebeln
          return    = DATA(lt_return).
      IF p_ebeln IS INITIAL.
        LOOP AT lt_return INTO DATA(lw_ret) WHERE ( type = 'E' OR type = 'A' ).
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
          p_msg = |{ p_msg }/{ l_msg }|.
        ENDLOOP.
      ENDIF.
    ELSE.
      p_msg = lv_msg.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form tax_code_get
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM tax_code_get  USING    p_p_log TYPE zmm_autopo_log
                   CHANGING p_lv_taxcode TYPE mwskz
                            p_msg TYPE string.
  DATA: lt_porgdata TYPE TABLE OF bapieine,
        lt_gendata  TYPE TABLE OF bapieina,
        lt_return   TYPE bapiret2_t.

  REFRESH: lt_gendata,lt_porgdata,lt_return.
  DATA(material) = CONV matnr18( p_p_log-matnr ).
  CALL FUNCTION 'BAPI_INFORECORD_GETLIST'
    EXPORTING
      vendor              = p_p_log-lifnr
      material            = material
      purch_org           = '1000'
      plant               = p_p_log-werks
      purchorg_data       = 'X'
      general_data        = 'X'
    TABLES
      inforecord_general  = lt_gendata
      inforecord_purchorg = lt_porgdata
      return              = lt_return.
  SORT lt_porgdata[] BY last_po DESCENDING.
  p_lv_taxcode = VALUE #( lt_porgdata[ 1 ]-tax_code OPTIONAL ).
  IF p_lv_taxcode IS INITIAL.
    p_msg = TEXT-002.
  ENDIF.
ENDFORM.
