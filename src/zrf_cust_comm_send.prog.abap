*&---------------------------------------------------------------------*
*& Report ZRF_CUST_COMM_SEND
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zrf_cust_comm_send.

INCLUDE zincl_cust_comms_top.
INCLUDE zincl_cust_comms_sel.

START-OF-SELECTION.

  SELECT FROM zsd_sf_cust_inv
  FIELDS *
  WHERE invoicekey IN @s_invkey
    AND  custno IN @s_cust
    AND fintype = 'RF'
    AND status = '14'
  INTO TABLE @gt_rf_data.
  IF sy-subrc NE 0.
    WRITE: /'Invalid Data'.
    EXIT.
  ENDIF.
  """"Get Customer Communication data
  SELECT  FROM zcus_cf_cumm
  FIELDS *
  FOR ALL ENTRIES IN @gt_rf_data
  WHERE kunnr = @gt_rf_data-custno AND fintype = 'RF'
  INTO TABLE @DATA(It_comms).

  LOOP AT gt_rf_data INTO DATA(ls_rf_data).

    IF p_mail = abap_true.
      PERFORM Send_Mail USING ls_rf_data It_comms CHANGING ls_mail_resp.
      response = ls_mail_resp.
    ENDIF.

    IF p_whats = abap_true.
      PERFORM Send_whatsapp USING ls_rf_data it_comms CHANGING ls_whats_resp.
      IF p_mail = abap_true.
        response = |{ ls_mail_resp },{ ls_whats_resp }|.
      ELSE.
        response = ls_whats_resp.
      ENDIF.
    ENDIF.

*****************DeSerialize the OUTPUT JSON************
    /ui2/cl_json=>serialize(
      EXPORTING
        data             = ls_rf_data
      RECEIVING
        r_json           = lv_json
    ).
    DATA(v_jsonload) = VALUE string(  ).
    data(mail) = VALUE string(  ).
    data(whats) = VALUE string(  ).
    IF p_mail = abap_true.
      IF ls_mail_resp CS 'successfully'.
       mail  = 'Mail Sent Successfully'.
      ELSE.
        mail = 'Mail Not Sent'.
      ENDIF.
    ENDIF.
    response = mail.
    IF p_whats = abap_true.
      IF ls_whats_resp CS 'sent'.
       whats = 'Chat Sent Successfully'.
       If  p_mail = abap_true.
        response = |{ mail },{ whats }|.
        Else.
          response = |{ whats }|.
        Endif.
      ELSE.
     whats = 'Chat Not Sent'.
      ENDIF.
    ENDIF.
    IF mail is not initial and whats is INITIAL.
    v_jsonload = | E-mail Response : { ls_mail_resp  }|.
    Endif.
    IF whats is NOT INITIAL and mail is INITIAL.
    v_jsonload = | Whatsapp Response : { ls_whats_resp  }|.
    ENDIF.
    IF mail is NOT INITIAL and whats is not INITIAL.
        v_jsonload = | { '[' }  { '{ E-mail Response : ' }{ ls_mail_resp  }{ '}' }{ ',{ Whatsapp Response:' }{ ls_whats_resp }{ '}' }{ ']' } |.
    ENDIF.

    CALL METHOD lo_log_upd->log_entry_store
      EXPORTING
        apiname         = 'RF_RETRIGGER_COMMS'
        ijson           = lv_json
        ojson           = v_jsonload
        distributor     = ls_rf_data-custno
      EXCEPTIONS
        apiname_missing = 1
        json_missing    = 2
        OTHERS          = 3.

    DATA(lo_salv_data) = VALUE ty_salv(
        invoicekey   = ls_rf_data-invoicekey
        company_code = ls_rf_data-bukrs
        customer     = ls_rf_data-custno
        status       = ls_rf_data-status
        payment_id   = ls_rf_data-payment_refid
        payment_date = ls_rf_data-payment_date
        sent_stat    = response
    ).

    APPEND lo_salv_data TO gt_salv_data.
  ENDLOOP.

END-OF-SELECTION.
  PERFORM Build_ALV.

  "Include
  INCLUDE zincl_rf_comms_send.
