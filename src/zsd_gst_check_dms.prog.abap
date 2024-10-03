*&---------------------------------------------------------------------*
*& Report ZSD_GST_CHECK_DMS
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsd_gst_check_dms.
TABLES kna1.

DATA : lt_gst TYPE STANDARD TABLE OF zcust_gst_chk.
DATA lw_data TYPE zcust_gst_chk.
SELECT-OPTIONS so_dist FOR kna1-kunnr.
SELECT-OPTIONS so_cust  FOR kna1-kunnr.
*SELECT-OPTIONS so_gstn  FOR kna1-stcd3.
PARAMETERS : r1 RADIOBUTTON GROUP g1 DEFAULT 'X',
             r2 RADIOBUTTON GROUP g1,
             r3 RADIOBUTTON GROUP g1.
PARAMETERS : p_frm TYPE sy-datum,
             p_to  TYPE sy-datum.
DATA:ls_data TYPE zcust_gst_chk,  "" for GST Expired customers
     gt_gst  TYPE STANDARD TABLE OF zcust_gst_chk. "" for GST Expired customers

PARAMETERS: p_email AS CHECKBOX.

DATA : lo_main TYPE REF TO zcl_dms_einvoice_process.
CREATE OBJECT lo_main.


START-OF-SELECTION.
********************for new customer code creation process*****************
  IF r2 = abap_true.

*if date not provided. system will take date from yesterday to today
    IF p_frm IS INITIAL AND p_to IS INITIAL.
      p_frm = sy-datum - 1.
      p_to = sy-datum.
    ENDIF.

*select all the change documents for customer master for the provided dates
    SELECT mandant, objectclas, objectid, changenr, udate, utime, tcode
          INTO TABLE @DATA(it_cdhdr)
          FROM cdhdr
          WHERE objectclas = 'DEBI'
          AND   udate GE @p_frm
          AND   udate LE @p_to
          AND ( tcode EQ 'XD01' OR tcode EQ 'XD02' OR tcode EQ 'VD01' OR tcode EQ 'VD02' OR tcode EQ 'MASS' OR tcode EQ 'XD05' OR tcode EQ 'XD06' OR tcode EQ 'XD99' ) .

    IF it_cdhdr[] IS NOT INITIAL.

      SORT it_cdhdr BY objectid.

      DELETE ADJACENT DUPLICATES FROM it_cdhdr COMPARING objectid.

      REFRESH : so_cust[].

      LOOP AT it_cdhdr INTO DATA(ls_cdhdr).
        so_cust-low    = ls_cdhdr-objectid.
        so_cust-option = 'EQ'.
        so_cust-sign   = 'I'.
        APPEND so_cust.
      ENDLOOP.

*select customers where distributor is present and distributor is not blocked
      IF so_cust[] IS NOT INITIAL.
        SELECT kunnr FROM zcust_blk_chk
          INTO TABLE @DATA(lt_cusfil)
          WHERE bukrs = 'DMS1'
            AND kunnr IN @so_cust
*            AND block = @abap_false
            AND dist  <> @space
            AND dist_block = @abap_false.
        IF sy-subrc = 0.
*check is alredy value is there in the GST Check table then no need to try checking
          SELECT kunnr FROM zcust_gst_chk
            INTO TABLE @DATA(lt_ckunnr)
            FOR ALL ENTRIES IN @lt_cusfil
           WHERE kunnr = @lt_cusfil-kunnr.
          IF sy-subrc = 0.
            SORT lt_ckunnr BY kunnr.
          ENDIF.

          REFRESH so_cust[].
          LOOP AT lt_cusfil INTO DATA(ls_cusfil).
*if GST check is already completed then do not re-process
            READ TABLE lt_ckunnr WITH KEY kunnr = ls_cusfil-kunnr TRANSPORTING NO FIELDS.
            IF sy-subrc NE 0.
              so_cust-low    = ls_cusfil-kunnr.
              so_cust-option = 'EQ'.
              so_cust-sign   = 'I'.
              APPEND so_cust.
            ENDIF.

          ENDLOOP.
        ENDIF.
      ENDIF.
    ELSE.
      MESSAGE : 'No changes on customer master' TYPE 'S'.
    ENDIF.
  ENDIF.

  IF r1 = abap_true.
    SELECT * FROM tvarvc INTO TABLE @DATA(lt_fullrun) WHERE name = 'ZGSTCHKFULLRUN' AND type = 'S' .
    IF sy-subrc = 0.
      LOOP AT lt_fullrun INTO DATA(ls_fullrun) WHERE low = sy-uname.
        EXIT.
      ENDLOOP.
      IF sy-subrc NE 0.
        IF so_dist[] IS INITIAL AND so_cust[] IS INITIAL.
          MESSAGE : 'Provide Distributor or Customer code for Individual Run' TYPE 'E' DISPLAY LIKE 'S'.
        ENDIF.
      ENDIF.
    ELSE.
      MESSAGE : 'TVARVC Variable ZGSTCHKFULLRUN is not filled' TYPE 'E' DISPLAY LIKE 'S'.
    ENDIF.
  ENDIF.

  IF r3 = abap_true.
    SELECT * FROM tvarvc INTO TABLE lt_fullrun WHERE name = 'ZGSTCHKFULLRUN' AND type = 'S' .
    IF sy-subrc = 0.
      LOOP AT lt_fullrun INTO ls_fullrun WHERE low = sy-uname.
        EXIT.
      ENDLOOP.
      IF sy-subrc = 0.
        SELECT kunnr FROM zcust_gst_chk INTO TABLE @DATA(lt_wkunnr) WHERE msg_type = 'W'.
        IF sy-subrc = 0.
          REFRESH so_cust[].
          LOOP AT lt_wkunnr ASSIGNING FIELD-SYMBOL(<fs_wkunnr>).
            so_cust-low    = <fs_wkunnr>-kunnr.
            so_cust-option = 'EQ'.
            so_cust-sign   = 'I'.
            APPEND so_cust.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ELSE.
      MESSAGE : 'TVARVC Variable ZGSTCHKFULLRUN is not filled' TYPE 'E' DISPLAY LIKE 'S'.
    ENDIF.
  ENDIF.

*******************get the irn enabled distributor*************
  IF r2 = abap_true OR r3 = abap_true.
    IF so_cust[] IS NOT INITIAL.
      SELECT * FROM zdist_einv_dtls INTO TABLE @DATA(lt_dist) WHERE distributor IN @so_dist.
      IF sy-subrc = 0.
        SELECT a~vkorg, a~kunnr, a~name1, a~werks, a~block, a~dist, a~dist_name, a~dist_werks, a~dist_block, b~stcd3
          FROM zcust_blk_chk AS a
          INNER JOIN kna1 AS b ON a~kunnr = b~kunnr
          INTO TABLE @DATA(lt_cust) FOR ALL ENTRIES IN @lt_dist
         WHERE a~bukrs = 'DMS1' AND a~dist = @lt_dist-distributor "AND a~block = @abap_false
           AND a~dist_block = @abap_false AND   b~kunnr IN @so_cust.
        IF sy-subrc NE 0.
          MESSAGE : 'Not a E-invoice customer' TYPE 'S'.
        ENDIF.
      ENDIF.
    ENDIF.
  ELSE.
    SELECT * FROM zdist_einv_dtls INTO TABLE @lt_dist WHERE distributor IN @so_dist.
    IF sy-subrc = 0.
      SELECT a~vkorg, a~kunnr, a~name1, a~werks, a~block, a~dist, a~dist_name, a~dist_werks, a~dist_block, b~stcd3
        FROM zcust_blk_chk AS a
        INNER JOIN kna1 AS b ON a~kunnr = b~kunnr
        INTO TABLE @lt_cust FOR ALL ENTRIES IN @lt_dist
       WHERE a~bukrs = 'DMS1' AND a~dist = @lt_dist-distributor "AND a~block = @abap_false
         AND a~dist_block = @abap_false AND   b~kunnr IN @so_cust.
      IF sy-subrc NE 0.
        MESSAGE : 'Not a E-invoice customer' TYPE 'S'.
      ENDIF.
    ENDIF.
  ENDIF.

  IF  lt_cust[] IS  NOT INITIAL.
    SORT : lt_gst BY dist kunnr.
    LOOP AT lt_cust ASSIGNING FIELD-SYMBOL(<fs_cust>).
      IF <fs_cust>-stcd3 IS NOT INITIAL.
        CALL METHOD lo_main->get_gst_token
          EXPORTING
            distributor_code     = <fs_cust>-dist
          IMPORTING
            authentication_token = DATA(authentication_token)
            subscription_id      = DATA(subscription_id)
            auth_token           = DATA(auth_token)
            session_key          = DATA(session_key)
            user_name            = DATA(user_name)
            gstin                = DATA(gstin)
            return               = DATA(return).

        CONDENSE <fs_cust>-stcd3.
        CALL METHOD lo_main->get_gst_details
          EXPORTING
            distributor_code     = <fs_cust>-dist
            costomer_code        = <fs_cust>-kunnr
            authentication_token = authentication_token
            subscription_id      = subscription_id
            auth_token           = auth_token
            session_key          = session_key
            user_name            = user_name
            gstin                = gstin
            customer_gstin       = <fs_cust>-stcd3
          IMPORTING
            zgst_msg             = DATA(lw_msg)
            return               = return.
************fill the log table value***************
        IF lw_msg IS NOT INITIAL.
          lw_data-kunnr      = <fs_cust>-kunnr.
          lw_data-name1      = <fs_cust>-name1.
          lw_data-stcd3      = <fs_cust>-stcd3.
          lw_data-dist       = <fs_cust>-dist.
          lw_data-dist_name  = <fs_cust>-dist_name.
          lw_data-dist_werks = <fs_cust>-dist_werks.
          lw_data-block      = <fs_cust>-block.
          lw_data-msg_type   = 'S'.
          lw_data-gstin      = lw_msg-gstin.
          lw_data-tradename  = lw_msg-tradename.
          lw_data-legalname  = lw_msg-legalname.
          lw_data-addrbnm    = lw_msg-addrbnm.
          lw_data-addrbno    = lw_msg-addrbno.
          lw_data-addrflno   = lw_msg-addrflno.
          lw_data-addrst     = lw_msg-addrst.
          lw_data-addrloc    = lw_msg-addrloc.
          lw_data-statecode  = lw_msg-statecode.
          lw_data-addrpncd   = lw_msg-addrpncd.
          lw_data-txptype    = lw_msg-txptype.
          lw_data-status     = lw_msg-status.
          lw_data-blkstatus  = lw_msg-blkstatus.
          MODIFY zcust_gst_chk FROM lw_data.
          APPEND lw_data TO lt_gst.
          CLEAR lw_data.
        ELSE.
          lw_data-kunnr      = <fs_cust>-kunnr.
          lw_data-name1      = <fs_cust>-name1.
          lw_data-stcd3      = <fs_cust>-stcd3.
          lw_data-dist       = <fs_cust>-dist.
          lw_data-dist_name  = <fs_cust>-dist_name.
          lw_data-dist_werks = <fs_cust>-dist_werks.
          lw_data-block      = <fs_cust>-block.
          lw_data-message    = return.

          TRANSLATE return TO UPPER CASE.

          IF return CS '"ERROR_CD":"GEN4004"'.
            lw_data-msg_type   = 'W'.
          ELSEIF return CS '"ERRORCODE":"1005"'.
            lw_data-msg_type   = 'W'.
          ELSEIF return CS '"ERRORCODE":"3001"'.
            lw_data-msg_type   = 'E'.
          ELSE.
            lw_data-msg_type = 'I'.
          ENDIF.

          MODIFY zcust_gst_chk FROM lw_data.
          APPEND lw_data TO lt_gst.
*          ls_data = lw_data.
*          APPEND ls_data TO gt_gst.
          CLEAR lw_data.
        ENDIF.
        CLEAR: return, lw_msg,lw_data,ls_data.
        WAIT UP TO 1 SECONDS.
      ENDIF.
    ENDLOOP.

  ENDIF.

  IF P_EMAIl = abap_true.

    refresh gt_gst.
    SELECT * FROM zcust_gst_chk INTO TABLE gt_GST WHERE msg_type in ('W', 'E', 'I').
    IF gt_gst[] IS NOT INITIAL.
      GET REFERENCE OF gt_gst INTO DATA(lo_data_ref).
      DATA(lv_xstring) = NEW zcl_itab_to_excel( )->itab_to_xstring( ir_data_ref = lo_data_ref ).
      PERFORM send_mail USING lv_xstring.
    ENDIF.
  ENDIF.


  CALL FUNCTION 'Z_POPUP_ALV'
    EXPORTING
*     I_REPID       =
*     I_START_COLUMN            = 25
*     I_START_LINE  = 6
*     I_END_COLUMN  = 200
*     I_END_LINE    = 20
      i_title       = 'Cutomer GST Details Report'
*     I_STATUS_FIELD_NAME       = ''
*     I_HYPERLINK_COLUMN        =
*     I_HYPERLINK_DATA          =
      i_hide_column = 'MANDT'
*     I_HIDE_COLUMN_EXT         =
*     I_POPUP       =
*     I_LAYOUT      =
    TABLES
      it_alv        = lt_gst.


FORM send_mail USING lv_xstring TYPE xstring.
  TRY.
      "Create send request
      DATA(lo_send_request) = cl_bcs=>create_persistent( ).

      "Create mail body
      DATA(lt_body) = VALUE bcsy_text(
                        ( line = '<HTML> <HEAD>' )
                        ( line = '<link href="https://fonts.googleapis.com/css2?family=Chathura:wght@100;300;400;700;800&family=Plus+Jakarta+Sans:ital,wght@0,200..800;1,200..800&family=Signika+Negative:wght@30..70&display=swap" rel="stylesheet">' ) ( )
                        ( line = '<link href="https://fonts.googleapis.com/css2?family=Plus+Jakarta+Sans:ital,wght@0,200..800;1,200..800&family=Signika+Negative:wght@300..700&display=swap" rel="stylesheet">' ) ( )
                        ( line = '<style> h1 { font-family : Signika Negative; }  p1 { font-family : Signika Negative; font-size:14 } h2{ font-family: Plus Jakarta Sans; color: rgb(255, 77, 77); } </style>') ( )
                        ( line = '</HEAD>' ) ( )
                        ( line = '<BODY>' )
                        ( line = '<h1 > Dear Team, </h1>' ) ( )
                        ( line = `<h2>Please find the attachement of GST errors.</h2>` ) ( )
                        ( line = `<p1><strong>Thank You</strong></p1>` ) ( )
                        ( line = '</BODY> </HTML>' )
                      ).

      "Set up document object
      DATA(lo_document) = cl_document_bcs=>create_document(
                            i_type = 'HTM'
                            i_text = lt_body
                            i_subject = 'Invalid GST of Sub-Dealers' ).

      "Add attachment
      lo_document->add_attachment(
          i_attachment_type    = 'xls'
          i_attachment_size    = CONV #( xstrlen( lv_xstring ) )
          i_attachment_subject = 'Sub-dealers GST Errors'
          i_attachment_header  = VALUE #( ( line = 'SD_Invalid_GST.xlsx' ) )
          i_att_content_hex    = cl_bcs_convert=>xstring_to_solix( lv_xstring )
       ).

      "Add document to send request
      lo_document->set_importance( i_importance = '1' ).
      lo_send_request->set_document( lo_document ).

      "Set sender
      lo_send_request->set_sender(
        cl_cam_address_bcs=>create_internet_address(
          i_address_string = CONV #( 'customerfeedback@sheenlac.in' )
          i_address_name = CONV #( 'Sheenlac' )
        )
      ).

      lo_send_request->add_recipient(
                 EXPORTING
                   i_recipient  = cl_cam_address_bcs=>create_internet_address(
                                    i_address_string = CONV #( 'sapsupport@sheenlac.in' )
                                    i_address_name   = CONV #( 'SAP SUPPORT' )
                                  )
                   i_express    = abap_true
                   i_copy = abap_true
               ).


      "Send Email
      lo_send_request->set_priority( i_priority = '1' ).
      DATA(lv_sent_to_all) = lo_send_request->send( ).
      COMMIT WORK.

    CATCH cx_send_req_bcs INTO DATA(lx_req_bsc).
      "Error handling
    CATCH cx_document_bcs INTO DATA(lx_doc_bcs).
      "Error handling
    CATCH cx_address_bcs  INTO DATA(lx_add_bcs).
      "Error handling
  ENDTRY.

ENDFORM.
