CLASS zcl_gst_validation_api DEFINITION
  PUBLIC
  FINAL.

  PUBLIC SECTION.
    "Interfaces
    INTERFACES if_http_extension.
    "data
    DATA:BEGIN OF ls_gst_data.
           INCLUDE TYPE zgst_msg.
    DATA:  sap_region TYPE char2,
         END OF ls_gst_data.
    "Types
    TYPES:BEGIN OF ty_input,
            subdealer   TYPE kunnr,
            distributor TYPE kunnr,
          END OF ty_input.
    TYPES:BEGIN OF ty_response,        "Response data
            Subdealer   TYPE kunnr,
            gtsn        TYPE stcd3,
            distributor TYPE kunnr,
            GST_msg     LIKE ls_gst_data,
            Error_msg   TYPE string,
            msg_type    TYPE bapiret2-type,
          END OF ty_response.
    METHODS: GST_Existance_Check EXPORTING iv_status  TYPE char10
                                           iv_gstn    TYPE stcd3
                                           ls_cust    TYPE zcust_gst_chk
                                           gs_gst_chk TYPE zcust_gst_chk.

    METHODS: GST_Status_check   IMPORTING gs_status   TYPE char10
                                          gs_gstn     TYPE stcd3
                                          gs_cust     TYPE zcust_gst_chk
                                          gs_gst_chk  TYPE zcust_gst_chk
                                EXPORTING is_response TYPE ty_response.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: lv_data TYPE string,
          lv_body TYPE string.
    DATA: ls_request TYPE ty_input.   "Request data
    DATA:BEGIN OF ls_resp,
           type TYPE bapiret2-type,
           msg  TYPE bapiret2-message,
         END OF ls_resp.
    DATA: ls_response TYPE ty_response.
************API log**********
    DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
************SARAL API for GST Validation******
    DATA : lo_main TYPE REF TO zcl_dms_einvoice_process.
ENDCLASS.



CLASS zcl_gst_validation_api IMPLEMENTATION.
  METHOD if_http_extension~handle_request.
    CREATE OBJECT lo_log_upd.
    CREATE OBJECT lo_main.
*-----------Getting input data in json------------*
    CALL METHOD server->request->get_cdata RECEIVING data = lv_data.

** Deserialize the input our required input ***
    /ui2/cl_json=>deserialize(
    EXPORTING
     json         = lv_data
     pretty_name  = /ui2/cl_json=>pretty_mode-user
     assoc_arrays = abap_true
    CHANGING
     data         = ls_request ).

*-----------input alpha conversion-----------*
    ls_request-subdealer = |{ ls_request-subdealer ALPHA = IN }|.
    ls_request-distributor  = |{ ls_request-distributor ALPHA = IN }|.

    CALL METHOD gst_existance_check
      IMPORTING
        iv_status  = DATA(gs_status)
        iv_gstn    = DATA(ls_gstn)
        ls_cust    = DATA(gs_cust)
        gs_gst_chk = DATA(ls_gst_chk).

    IF ls_resp is INITIAL.
    CALL METHOD gst_status_check
      EXPORTING
        gs_status   = gs_status
        gs_gstn     = ls_gstn
        gs_cust     = gs_cust
        gs_gst_chk  = ls_gst_chk
      IMPORTING
        is_response = ls_response.
        ELSE.
        ls_response = VALUE #( distributor = ls_request-distributor subdealer = ls_request-subdealer msg_type = 'E' error_msg = ls_resp-msg ).
    ENDIF.
** serialize the output for response ***
    /ui2/cl_json=>serialize(
    EXPORTING
     data         =  ls_response
     pretty_name  = /ui2/cl_json=>pretty_mode-user
    RECEIVING
     r_json         = lv_body ).

*Output Entry in Log Table
    CALL METHOD lo_log_upd->log_entry_store
      EXPORTING
        apiname         = 'GST_VALIDATION'
        ijson           = lv_data
        ojson           = lv_body
        distributor     = ls_request-distributor
        retailer        = ls_request-subdealer
      EXCEPTIONS
        apiname_missing = 1
        json_missing    = 2
        OTHERS          = 3.
*Set JSON Content-Type
    CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
    CALL METHOD server->response->set_cdata EXPORTING data = lv_body .
  ENDMETHOD.

  METHOD gst_existance_check.
    "Check GST Existence of sub-dealer
    SELECT SINGLE FROM kna1 FIELDS stcd3 WHERE kunnr = @ls_request-subdealer INTO @DATA(lsd_gstn).
    IF sy-subrc = 0.
      CONDENSE lsd_gstn.
      iv_gstn = lsd_gstn.
      SELECT SINGLE FROM zcust_gst_chk FIELDS * WHERE kunnr = @ls_request-subdealer INTO @gs_gst_chk.
      IF Sy-subrc = 0.
        IF gs_gst_chk-block = abap_true.
          ls_resp = VALUE #( type = 'E' msg = |GST blocked for Sub-Dealer { ls_request-subdealer }| ).
          EXIT.
        ENDIF.
        iv_status = gs_gst_chk-status.
      ENDIF.
    ELSE.
      "IF Record doesn't exist
      IF lsd_gstn is initial and sy-subrc =  0.
      ls_resp = VALUE #( type = 'E' msg = |Sub-Dealer { ls_request-subdealer } GST number not Exist| ).
      EXIT.
      ELSE.
            ls_resp = VALUE #( type = 'E' msg = |Incorrect Sub-dealer code| ).
      EXIT.
      ENDIF.
    ENDIF.
    "Check GST Existence of Distributor
   SELECT SINGLE FROM kna1 FIELDS stcd3 WHERE kunnr = @ls_request-distributor INTO @lsd_gstn.
   IF sy-subrc ne 0.
   ls_resp = VALUE #( type = 'E' msg = |Incorrect Distributor code| ).
   EXIT.
   ENDIF.
    SELECT SINGLE FROM zdist_einv_dtls FIELDS * WHERE distributor = @ls_request-distributor INTO @DATA(ls_dist).
    IF sy-subrc NE 0 .
      ls_resp = VALUE #( type = 'E' msg = |Not an E-invoice Customer| ).
      EXIT.
    ENDIF.
*    SELECT SINGLE FROM zcust_blk_chk AS a
*    INNER JOIN kna1 AS b ON a~kunnr = b~kunnr
*     FIELDS *
*     WHERE a~dist = @ls_dist-distributor
*     AND   b~kunnr = @ls_request-subdealer
*     AND   a~dist_block = @abap_false
*     INTO @DATA(gs_cust).
     SELECT SINGLE FROM kna1 FIELDS kunnr WHERE kunnr = @ls_request-subdealer INTO @DATA(gs_kunnr).
     If sy-subrc = 0.
     SELECT SINGLE FROM zcust_blk_chk FIELDS * WHERE kunnr = @gs_kunnr and dist = @ls_dist-distributor and dist_block = @abap_false
                                                                                          INTO @DATA(gs_cust).
      IF sy-subrc NE 0.
      ls_resp = VALUE #( type = 'E' msg = |Not an E-invoice customer| ).
      EXIT.
    ENDIF.
     ENDIF.
*    IF sy-subrc NE 0.
*      ls_resp = VALUE #( type = 'E' msg = |Not an E-invoice customer| ).
*      EXIT.
*    ENDIF.
    ls_cust = CORRESPONDING #( gs_cust ).

  ENDMETHOD.

  METHOD gst_status_check.
    DATA:gs_error TYPE bapiret2-message,
         lw_msg   TYPE zgst_msg.
    DATA: ws_gst_chk type zcust_gst_chk.
    FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE.
    CASE gs_status.
      WHEN 'ACT'.
        ls_gst_data = CORRESPONDING #( gs_gst_chk ).
        SELECT SINGLE FROM zsd_gst_reg FIELDS region WHERE gst_key =  @ls_gst_data-statecode INTO @ls_gst_data-sap_region."#EC CI_NOORDER
*        lw_msg = CORRESPONDING #( gs_gst_chk ).
        is_response = VALUE #( subdealer = ls_request-subdealer gtsn = gs_gstn distributor = ls_request-distributor
                               gst_msg = ls_gst_data
                                 msg_type = 'S'  ).
      WHEN 'CNL' OR 'SUS' OR 'INS' OR Space .
        CALL METHOD lo_main->get_gst_token
          EXPORTING
            distributor_code     = ls_request-distributor
          IMPORTING
            authentication_token = DATA(authentication_token)
            subscription_id      = DATA(subscription_id)
            auth_token           = DATA(auth_token)
            session_key          = DATA(session_key)
            user_name            = DATA(user_name)
            gstin                = DATA(gstin)
            return               = DATA(return).

        CALL METHOD lo_main->get_gst_details
          EXPORTING
            distributor_code     = ls_request-distributor
            costomer_code        = ls_request-subdealer
            authentication_token = authentication_token
            subscription_id      = subscription_id
            auth_token           = auth_token
            session_key          = session_key
            user_name            = user_name
            gstin                = gstin
            customer_gstin       = gs_gstn
          IMPORTING
            zgst_msg             = lw_msg
            return               = return.


        IF return IS NOT INITIAL.
          TRANSLATE return TO UPPER CASE.
          SPLIT return AT '"ERRORMESSAGE":' INTO DATA(str1) DATA(str2).
          SPLIT str2 AT '}],"DATA":' INTO DATA(str3) DATA(str4).
        ELSE.
          ls_gst_data = CORRESPONDING #( BASE ( ls_gst_data ) lw_msg ).
          SELECT SINGLE FROM zsd_gst_reg FIELDS region WHERE gst_key =  @ls_gst_data-statecode INTO @ls_gst_data-sap_region."#EC CI_NOORDER
        ENDIF.
        is_response = VALUE #( subdealer = ls_request-subdealer gtsn = gs_gstn distributor = ls_request-distributor
                               gst_msg = ls_gst_data
                                error_msg = str3
                                 msg_type = COND #( WHEN return IS NOT INITIAL THEN 'E'
                                                    ELSE 'S' ) ).
        IF lw_msg IS INITIAL.
          MODIFY zcust_gst_chk FROM @( VALUE #(
              mandt      = sy-mandt
              kunnr      = gs_cust-kunnr
              name1      = gs_cust-name1
              stcd3      = gs_cust-stcd3
              dist       = gs_cust-dist
              dist_name  = gs_cust-dist_name
              dist_werks = gs_cust-dist_werks
              block      = gs_cust-block
              message    = return
              msg_type   = 'E'
          ) ).

          COMMIT WORK.
        ELSE.
    ws_gst_chk = CORRESPONDING #( BASE ( gs_gst_chk ) ls_gst_data ).
    ws_gst_chk-msg_type = 'S'.

   MODIFY zcust_gst_chk FROM ws_gst_chk.
   COMMIT WORK.
*          UPDATE zcust_gst_chk FROM @( VALUE #(
*              mandt      = sy-mandt
*             kunnr      = gs_cust-kunnr
*              name1      = gs_cust-name1
*              stcd3      = gs_cust-stcd3
*              dist       = gs_cust-dist
*              dist_name  = gs_cust-dist_name
*              dist_werks = gs_cust-dist_werks
*              block      = gs_cust-block
*              msg_type   = 'S'
*              gstin      = lw_msg-gstin
*              tradename  = lw_msg-tradename
*              legalname  = lw_msg-legalname
*              addrbnm    = lw_msg-addrbnm
*              addrbno    = lw_msg-addrbno
*              addrflno   = lw_msg-addrflno
*              addrst     = lw_msg-addrst
*              addrloc    = lw_msg-addrloc
*              statecode  = lw_msg-statecode
*              addrpncd   = lw_msg-addrpncd
*              txptype    = lw_msg-txptype
*              status     = lw_msg-status
*              blkstatus  = lw_msg-blkstatus
*          ) ).
        ENDIF.
    ENDCASE.

  ENDMETHOD.

ENDCLASS.
