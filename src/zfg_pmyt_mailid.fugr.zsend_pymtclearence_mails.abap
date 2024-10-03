FUNCTION zsend_pymtclearence_mails.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(ORGINV) TYPE  BELNR_D OPTIONAL
*"  EXPORTING
*"     REFERENCE(MESSAGE) TYPE  STRING
*"  TABLES
*"      IT_MAILID TYPE  /CFG/T_EMAIL
*"  EXCEPTIONS
*"      MAILID_MISSING
*"----------------------------------------------------------------------
*Created by: Samsudeen M
*Created On: 08.05.2023
*Purpose : Mail Sending Function Module
*----------------------------------------------------------------------
  DATA: lt_message_body TYPE bcsy_text.
  DATA: lo_send_request TYPE REF TO cl_bcs,
        lo_document     TYPE REF TO cl_document_bcs,
        lo_recipient    TYPE REF TO if_recipient_bcs VALUE IS INITIAL,
        lo_sender       TYPE REF TO if_sender_bcs.
  DATA: it_body_msg   TYPE STANDARD TABLE OF solisti1.
  DATA: salutation TYPE string.
  DATA: body TYPE string.
  DATA: footer TYPE string.

  IF it_mailid[] IS NOT INITIAL.
    "create send request
    TRY.
        lo_send_request = cl_bcs=>create_persistent( ).
      CATCH  cx_send_req_bcs INTO DATA(lx_send_req_bcs1).
    ENDTRY.
    salutation ='Dear Sir/Madam ,'.
    APPEND salutation TO lt_message_body.
    APPEND INITIAL LINE TO lt_message_body.


    DATA(lv_body) = |Vendor Payment Cleared against Original SAP Invoices'{ orginv }|.
    CONDENSE lv_body.
    body = lv_body.
    APPEND body TO lt_message_body.
    APPEND INITIAL LINE TO lt_message_body.

    footer = 'Thanks & Best Regards,'.
    APPEND footer TO lt_message_body.

    footer = 'Sheenlac Paints Ltd'.
    APPEND footer TO lt_message_body.

    TRY.
        lo_document = cl_document_bcs=>create_document(
        i_type = 'RAW'
        i_text = lt_message_body
        i_subject = 'Vendor Payment Clearing' ).
      CATCH cx_document_bcs INTO DATA(lc_document_bcs1).
    ENDTRY.
* Pass the document to send request
  TRY.
      lo_send_request->set_document( lo_document ).
    CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs2).
  ENDTRY.

    "Sender Mail ID set in TVARVC
    SELECT SINGLE * FROM tvarvc
      INTO @DATA(ls_tvarvc)
      WHERE name = 'SENDER_MAIL_PYMT'
      AND type = 'P'.

    DATA(sender_mail) = CONV adr6-smtp_addr( ls_tvarvc-low ).

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
    LOOP AT it_mailid ASSIGNING FIELD-SYMBOL(<fs_mailid>).
      TRY.
          lo_recipient = cl_cam_address_bcs=>create_internet_address( CONV ad_smtpadr( <fs_mailid>-low ) ).
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
* Send email
    TRY.
        lo_send_request->send(
        EXPORTING
        i_with_error_screen = abap_true
        RECEIVING
        result = DATA(lv_sent) ).
      CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs).
    ENDTRY.

    COMMIT WORK.

  ELSE.
    RAISE mailid_missing.
  ENDIF.




ENDFUNCTION.
