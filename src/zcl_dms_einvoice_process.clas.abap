class ZCL_DMS_EINVOICE_PROCESS definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ty_response,
        ackno         TYPE string,
        ackdt         TYPE string,
        irn           TYPE string,
        signedinvoice TYPE string,
        signedqrcode  TYPE string,
        status        TYPE string,
      END OF ty_response .
  types:
    BEGIN OF ty_ewb_irn_inp,
            Invoice_no TYPE vbeln,
            distance   TYPE char05,
            transid    TYPE char26,
            transname  TYPE char50,
            transdoc   TYPE char12,
            transdate  TYPE char12,
            vehno      TYPE char12,
          END OF ty_ewb_irn_inp .

  methods CANCEL_EWB
    importing
      !DISTRIBUTOR_CODE type KUNNR
      !VBELN type VBELN_VF
      !BUKRS type BUKRS
      !GJAHR type GJAHR
      !DATE type SY-DATUM optional
    exporting
      !IRN type J_1IG_IRN
      !SIGNED_INV type J_1IG_SIGN_INV
      !SIGNED_QRCODE type J_1IG_SIGN_QRCODE
      !RETURN type STRING .
  methods GENERATE_PDF
    importing
      !INV_NO type VBELN_VF
      !INV_DATE type FKDAT
      !INV_DETAILS type ZEINV_IRN_DETAILS_ST
      !IRN type J_1IG_IRN
      !SIGNED_INV type J_1IG_SIGN_INV
      !SIGNED_QRCODE type J_1IG_SIGN_QRCODE
      !PO_NUMBER type EBELN
      !SELLER_INVOICENO type VBELN_VF
    exporting
      !PDF type FPCONTENT
      !RETURN type STRING .
  methods GENERATE_PDF_DMS
    importing
      !INV_NO type VBELN_VF
      !CUSTOMER type KUNNR optional
    exporting
      !PDF type FPCONTENT
      !RETURN type STRING .
  methods CREATE_EWB
    importing
      !DISTRIBUTOR_CODE type KUNNR
      !VBELN type VBELN_VF
      !BUKRS type BUKRS
      !GJAHR type GJAHR
      !DATE type SY-DATUM optional
    exporting
      !IRN type J_1IG_IRN
      !SIGNED_INV type J_1IG_SIGN_INV
      !SIGNED_QRCODE type J_1IG_SIGN_QRCODE
      !RETURN type STRING .
  methods GET_GST_TOKEN
    importing
      !DISTRIBUTOR_CODE type KUNNR
    exporting
      !AUTHENTICATION_TOKEN type ZSAUTH_TKN
      !SUBSCRIPTION_ID type ZSUB_ID
      !AUTH_TOKEN type CHAR100
      !SESSION_KEY type CHAR120
      !USER_NAME type ZEINV_UNAME
      !GSTIN type BPTAXNUM
      !RETURN type STRING .
  methods CREATE_IRN_QRCODE
    importing
      !DISTRIBUTOR_CODE type KUNNR
      !VBELN type VBELN_VF
      !BUKRS type BUKRS
      !GJAHR type GJAHR
      !DATE type SY-DATUM optional
    exporting
      !IRN type J_1IG_IRN
      !SIGNED_INV type J_1IG_SIGN_INV
      !SIGNED_QRCODE type J_1IG_SIGN_QRCODE
      !ACK_NO type J_1IG_ACK_NO
      !ACK_DATE type J_1IG_ACK_DATE
      !RETURN type STRING .
  methods CANCEL_IRN_QRCODE
    importing
      !DISTRIBUTOR_CODE type KUNNR
      !VBELN type VBELN_VF
      !BUKRS type BUKRS
      !GJAHR type GJAHR
      !IRN type J_1IG_IRN
      !CNLRSN type CHAR1 default '1'
      !CNLREM type STRING default 'Invoice Cancelled'
    exporting
      !RETURN type STRING
      !TYPE type BAPI_MTYPE .
  methods CREATE_IRN_QRCODE_PORETURN
    importing
      !DISTRIBUTOR_CODE type KUNNR
      !SELLER_PLANT type WERKS_D
      !PO_NUMBER type EBELN
      !INVOICENO type VBELN_VF
      !INVOICEDT type FKDAT
      !SELLER_INVOICENO type VBELN_VF
      !BUKRS type BUKRS
      !GJAHR type GJAHR
    exporting
      !IRN type J_1IG_IRN
      !ACK_NO type J_1IG_ACK_NO
      !ACK_DATE type J_1IG_ACK_DATE
      !SIGNED_INV type J_1IG_SIGN_INV
      !SIGNED_QRCODE type J_1IG_SIGN_QRCODE
      !PDF type FPCONTENT
      !RETURN type STRING .
  methods GET_IRN_DETAILS
    importing
      !IRN type J_1IG_IRN
      !DISTRIBUTOR_CODE type KUNNR
      !RETAILER type KUNNR
    exporting
      !RESPONSE type TY_RESPONSE
      !TYPE type BAPI_MTYPE
      !MESSAGE type STRING .
  methods GET_GST_DETAILS
    importing
      !DISTRIBUTOR_CODE type KUNNR
      !COSTOMER_CODE type KUNNR
      !AUTHENTICATION_TOKEN type ZSAUTH_TKN
      !SUBSCRIPTION_ID type ZSUB_ID
      !AUTH_TOKEN type CHAR100
      !SESSION_KEY type CHAR120
      !USER_NAME type ZEINV_UNAME
      !GSTIN type BPTAXNUM
      !CUSTOMER_GSTIN type STCD3
    exporting
      !ZGST_MSG type ZGST_MSG
      !RETURN type STRING .
  methods GENERATE_PDF_STO_AMAZON
    importing
      !MBLNR type MBLNR
      !MJAHR type MJAHR
    exporting
      !MESSAGE type STRING .
  methods FILL_PORETURN_DETAILS
    importing
      !DISTRIBUTOR_CODE type KUNNR
      !SELLER_PLANT type WERKS_D
      !PO_NUMBER type EBELN
      !INVOICENO type VBELN_VF
      !INVOICEDT type FKDAT
      !SELLER_INVOICENO type VBELN_VF
      !BUKRS type BUKRS
      !GJAHR type GJAHR
    exporting
      !DOCTYP type J_1IG_DOCTYP
      !DATA_TAB type ZEINV_IRN_DETAILS_ST
      !MESSAGE type STRING .
  methods GENERATE_EWB_BY_IRN
    importing
      value(LS_REQUEST) type TY_EWB_IRN_INP
    exporting
      !EWB_NO type CHAR12
      !EWB_DT type CHAR10
      !EWB_VALID_TILL type CHAR25
      !DISTRIBUTOR type KUNNR
      !DEALER type KUNNR
      !RETURN type STRING .
  methods CANCEL_EWB_BY_IRN
    importing
      !INVOICENO type VBELN
      !CANCEL_RMRK type CHAR50
    exporting
      !EWAYBILL_NO type CHAR12
      !CANCELDATE type CHAR25
      !DISTRIBUTOR type KUNNR
      !DEALER type KUNNR
      !RETURN type STRING .
  PROTECTED SECTION.
private section.

  methods GET_SARAL_TOKEN
    importing
      !DISTRIBUTOR_CODE type KUNNR
      !CUSTOMER type KUNNR optional
    exporting
      !AUTHENTICATION_TOKEN type ZSAUTH_TKN
      !SUBSCRIPTION_ID type ZSUB_ID
      !ERROR_CODE type CHAR03
      !ERROR_MSG type CHAR50 .
  methods GET_IRN_TOKEN
    importing
      !DISTRIBUTOR_CODE type KUNNR
      !CUSTOMER type KUNNR optional
      !AUTHENTICATION_TOKEN type ZSAUTH_TKN
      !SUBSCRIPTION_ID type ZSUB_ID
    exporting
      !ERR_MSG type CHAR100
      !AUTH_TOKEN type CHAR100
      !SESSION_KEY type CHAR120
      !USER_NAME type ZEINV_UNAME
      !GSTIN type BPTAXNUM .
  methods FILL_INVOICE_DETAILS
    importing
      !DISTRIBUTOR_CODE type KUNNR
      !VBELN type VBELN_VF
      !DATE type SY-DATUM optional
    exporting
      !DOCTYP type J_1IG_DOCTYP
      !DEALER type Z_DEAL_CODE
      !DATA_TAB type ZEINV_IRN_DETAILS_ST
      !MESSAGE type STRING .
  methods PREPARE_JSON_PAYLOAD
    importing
      !DATA_TAB type ZEINV_IRN_DETAILS_ST
      !INVOICENO type VBELN_VF optional
      !INVOICEDT type FKDAT optional
      !SELLER_INVOICENO type VBELN_VF optional
    exporting
      !PAYLOAD_JSON type STRING .
  methods CALL_API_CANCEL_IRN
    importing
      !DISTRIBUTOR_CODE type KUNNR
      !CUSTOMER type KUNNR optional
      !AUTHENTICATION_TOKEN type ZSAUTH_TKN
      !SUBSCRIPTION_ID type ZSUB_ID
      !AUTH_TOKEN type CHAR100
      !SESSION_KEY type CHAR120
      !USER_NAME type ZEINV_UNAME
      !GSTIN type BPTAXNUM
      !IV_PAYLOAD_JSON type STRING
    exporting
      !MESSAGE type STRING
      !TYPE type BAPI_MTYPE .
  methods CALL_API_GENERATE_IRN
    importing
      !DISTRIBUTOR_CODE type KUNNR
      !CUSTOMER type KUNNR optional
      !AUTHENTICATION_TOKEN type ZSAUTH_TKN
      !SUBSCRIPTION_ID type ZSUB_ID
      !AUTH_TOKEN type CHAR100
      !SESSION_KEY type CHAR120
      !USER_NAME type ZEINV_UNAME
      !GSTIN type BPTAXNUM
      !VBELN type VBELN_VF
      !BUKRS type BUKRS
      !GJAHR type GJAHR
      !IV_PAYLOAD_JSON type STRING
    exporting
      !EINV_IRN type ZDMS_INVOICE_IRN
      !EWAY_BILL type J_1IG_EWAYBILL
      !MESSAGE type STRING .
  methods CALL_API_GENERATE_EWAY_BY_IRN
    importing
      !DISTRIBUTOR_CODE type KUNNR
      !CUSTOMER type KUNNR
      !BUKRS type BUKRS
      !GJAHR type GJAHR
      !DOC_TYPE type J_1IG_DOCTYP
      !DOCNO type VBELN
      !AUTHENTICATION_TOKEN type ZSAUTH_TKN
      !SUBSCRIPTION_ID type ZSUB_ID
      !AUTH_TOKEN type CHAR100
      !SESSION_KEY type CHAR120
      !USER_NAME type ZEINV_UNAME
      !GSTIN type BPTAXNUM
      !IV_PAYLOAD_JSON type STRING
      !LS_REQUEST type TY_EWB_IRN_INP
    exporting
      !IV_EWAY_TRANS_DTLS type ZDMS_EWAY_TRDTLS
      !MESSAGE type STRING .
  methods CALL_API_CANCEL_EWAY_BY_IRN
    importing
      value(LV_EWAY_DETAILS) type ZDMS_EWAY_TRDTLS
      !AUTHENTICATION_TOKEN type ZSAUTH_TKN
      !SUBSCRIPTION_ID type ZSUB_ID
      !AUTH_TOKEN type CHAR100
      !SESSION_KEY type CHAR120
      !USER_NAME type ZEINV_UNAME
      !GSTIN type BPTAXNUM
      !IV_PAYLOAD_JSON type STRING
      !CANCEL_REASON type CHAR50
    exporting
      !MESSAGE type STRING
    changing
      !IV_EWAY_TRANS_DTLS type ZDMS_EWAY_TRDTLS .
ENDCLASS.



CLASS ZCL_DMS_EINVOICE_PROCESS IMPLEMENTATION.


  METHOD call_api_cancel_irn.
*    TYPES: BEGIN OF ty_response,
*             ackno         TYPE string,
*             ackdt         TYPE string,
*             irn           TYPE string,
*             signedinvoice TYPE string,
*             signedqrcode  TYPE string,
*             status        TYPE string,
*             ewbno         TYPE string,
*             ewbdt         TYPE string,
*             ewbvalidtill  TYPE string,
*           END OF ty_response.

    TYPES : BEGIN OF ty_err,
              errorcode    TYPE string,
              errormessage TYPE string,
            END OF ty_err.

    DATA : lt_err TYPE TABLE OF ty_err.

    TYPES: BEGIN OF ty_response,
             status       TYPE string,
             data         TYPE string,
             infodtls     TYPE string,
             irn          TYPE string,
             canceldate   TYPE string,
             errordetails LIKE lt_err,
           END OF ty_response.

    "API response Structure
    DATA: ls_response TYPE ty_response.
    DATA : lv_string   TYPE string,
           lv_response TYPE string.
    DATA: lo_http_client TYPE REF TO if_http_client.

*---- URL fixing for system based ----*
    CASE sy-sysid.
      WHEN 'DEV' OR 'QAS'.
        DATA(l_api_url) = 'DMS_EINV_CANCEL_IRN_TEST'.
      WHEN 'PRD'.
        l_api_url = 'DMS_EINV_CANCEL_IRN_PRD'.
    ENDCASE.
    SELECT SINGLE low FROM tvarvc INTO @DATA(l_url) WHERE name = @l_api_url
                                                    AND type = 'P'.
    IF sy-subrc = 0.
      DATA(create_url) = CONV string( l_url ).
      cl_http_client=>create_by_url(
          EXPORTING
          url = create_url
          IMPORTING
          client = lo_http_client
          EXCEPTIONS
          argument_not_found = 1
          plugin_not_active = 2
          internal_error = 3
          OTHERS = 4 ).

      CHECK lo_http_client IS BOUND.

      lo_http_client->propertytype_logon_popup = if_http_client=>co_disabled.

      lo_http_client->request->set_method(
       EXPORTING
       method = if_http_entity=>co_request_method_post ).


      lo_http_client->request->set_content_type(
       EXPORTING
       content_type = if_rest_media_type=>gc_appl_json ).

      "Header Data Fields for API Fixing
      lo_http_client->request->set_header_field( EXPORTING name  = 'AuthenticationToken' value =  CONV string( authentication_token ) ).

      lo_http_client->request->set_header_field( EXPORTING name  = 'SubscriptionId'      value =  CONV string( subscription_id ) ).

      lo_http_client->request->set_header_field( EXPORTING name  = 'Gstin'               value =  CONV string( gstin ) ).

      lo_http_client->request->set_header_field( EXPORTING name  = 'UserName'            value =  CONV string( user_name ) ).

      lo_http_client->request->set_header_field( EXPORTING name  = 'AuthToken'           value =  CONV string( auth_token ) ).

      lo_http_client->request->set_header_field( EXPORTING name  = 'sek'                 value =  CONV string( session_key )  ).

      lo_http_client->request->set_cdata(
         EXPORTING
         data = iv_payload_json ).

      lo_http_client->send(
       EXCEPTIONS
       http_communication_failure = 1
       http_invalid_state = 2 ).

      CHECK sy-subrc = 0.
      lo_http_client->receive(
       EXCEPTIONS
       http_communication_failure = 1
       http_invalid_state = 2
       http_processing_failed = 3 ).

      lo_http_client->response->get_status(
      IMPORTING
        code = DATA(lv_codes) ).

      lo_http_client->response->get_status(
      IMPORTING
        reason = DATA(lv_http_error) ).
      "Actual API Response If success
      IF lv_codes = 200.
        CLEAR : lv_response.
        lv_response = lo_http_client->response->get_cdata( ).

        IF lv_response IS NOT INITIAL.
          CONDENSE : lv_response.
* deserialize the INPUT our required INPUT ***
          /ui2/cl_json=>deserialize(
          EXPORTING
           json         = lv_response
           pretty_name  = /ui2/cl_json=>pretty_mode-user
          CHANGING
           data         = ls_response ).

          IF ls_response-irn IS NOT INITIAL AND ls_response-canceldate IS NOT INITIAL.
            message = |IRN is cancelled|.
            type    = 'S'.
          ELSE.
            LOOP AT ls_response-errordetails[] INTO DATA(ls_error).
              message = | { message } , { ls_error-errormessage } |.
            ENDLOOP.
            IF message IS INITIAL.
              message = 'IRN is not cancelled'.
            ENDIF.
            type = 'E'.
          ENDIF.
        ENDIF.

      ELSE.
        message = |Error Code: { lv_codes } - { lv_http_error }.IRN is not cancelled|.
        type    = 'E'.
      ENDIF.
    ELSE.
      message = 'URL is not maintained in STVARV'.
      type    = 'E'.
    ENDIF.

    DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
    DATA: lv_data TYPE string,
          lv_body TYPE string.
    CREATE OBJECT lo_log_upd.

*Output Entry in Log Table
    CALL METHOD lo_log_upd->log_entry_store
      EXPORTING
        apiname         = 'SARALAPI_IRN_CANCEL'
        ijson           = iv_payload_json
        ojson           = lv_response
        distributor     = distributor_code
        retailer        = customer
      EXCEPTIONS
        apiname_missing = 1
        json_missing    = 2
        OTHERS          = 3.
  ENDMETHOD.


  METHOD call_api_generate_irn.
    TYPES: BEGIN OF ty_response,
             ackno         TYPE string,
             ackdt         TYPE string,
             irn           TYPE string,
             signedinvoice TYPE string,
             signedqrcode  TYPE string,
             status        TYPE string,
             ewbno         TYPE string,
             ewbdt         TYPE string,
             ewbvalidtill  TYPE string,
           END OF ty_response.

    TYPES: BEGIN OF ty_error,
             ackno         TYPE string,
             ackdt         TYPE string,
             irn           TYPE string,
             signedinvoice TYPE string,
             signedqrcode  TYPE string,
             status        TYPE string,
             ewbno         TYPE string,
             ewbdt         TYPE string,
             ewbvalidtill  TYPE string,
           END OF ty_error.

    "API response Structure
    DATA: ls_response TYPE ty_response.
    DATA: lo_http_client TYPE REF TO if_http_client.

*---- URL fixing for system based ----*
    CASE sy-sysid.
      WHEN 'DEV' OR 'QAS'.
        DATA(l_api_url) = 'DMS_EINV_CREATE_IRN_TEST'.
      WHEN 'PRD'.
        l_api_url = 'DMS_EINV_CREATE_IRN_PRD'.
    ENDCASE.
    SELECT SINGLE low FROM tvarvc INTO @DATA(l_url) WHERE name = @l_api_url
                                                    AND type = 'P'.
    IF sy-subrc = 0.
      DATA(create_url) = CONV string( l_url ).
      cl_http_client=>create_by_url(
          EXPORTING
          url = create_url
          IMPORTING
          client = lo_http_client
          EXCEPTIONS
          argument_not_found = 1
          plugin_not_active = 2
          internal_error = 3
          OTHERS = 4 ).

      CHECK lo_http_client IS BOUND.

      lo_http_client->propertytype_logon_popup = if_http_client=>co_disabled.

      lo_http_client->request->set_method(
       EXPORTING
       method = if_http_entity=>co_request_method_post ).


      lo_http_client->request->set_content_type(
       EXPORTING
       content_type = if_rest_media_type=>gc_appl_json ).

      "Header Data Fields for API Fixing
      lo_http_client->request->set_header_field( EXPORTING name  = 'AuthenticationToken' value =  CONV string( authentication_token ) ).

      lo_http_client->request->set_header_field( EXPORTING name  = 'SubscriptionId'      value =  CONV string( subscription_id ) ).

      lo_http_client->request->set_header_field( EXPORTING name  = 'Gstin'               value =  CONV string( gstin ) ).

      lo_http_client->request->set_header_field( EXPORTING name  = 'UserName'            value =  CONV string( user_name ) ).

      lo_http_client->request->set_header_field( EXPORTING name  = 'AuthToken'           value =  CONV string( auth_token ) ).

      lo_http_client->request->set_header_field( EXPORTING name  = 'sek'                 value =  CONV string( session_key )  ).

      lo_http_client->request->set_cdata(
         EXPORTING
         data = iv_payload_json ).

      lo_http_client->send(
       EXCEPTIONS
       http_communication_failure = 1
       http_invalid_state = 2 ).

      CHECK sy-subrc = 0.
      lo_http_client->receive(
       EXCEPTIONS
       http_communication_failure = 1
       http_invalid_state = 2
       http_processing_failed = 3 ).

      lo_http_client->response->get_status(
      IMPORTING
        code = DATA(lv_codes) ).

      lo_http_client->response->get_status(
      IMPORTING
        reason = DATA(lv_http_error) ).
      "Actual API Response If success
      IF lv_codes = 200.
        DATA(lv_response) = lo_http_client->response->get_cdata( ).

******************************After get the data close the connection*******************
        lo_http_client->close( ).
** deserialize the INPUT our required INPUT ***
        /ui2/cl_json=>deserialize(
        EXPORTING
         json         = lv_response
         pretty_name  = /ui2/cl_json=>pretty_mode-user
        CHANGING
         data         = ls_response ).

        "Response of API
        IF ls_response-irn IS NOT INITIAL.

*  ***Einvoice IRN Invrefnum details.--->>
          einv_irn-bukrs         = bukrs.
          einv_irn-doc_year      = gjahr.
*  SHIFT p_einv_out-vbeln LEFT DELETING LEADING '0'.
          einv_irn-distrb        = distributor_code .
          einv_irn-docno         = vbeln.
          einv_irn-version       = 11.
*          einv_irn-doc_type      = 'INV'.

          einv_irn-irn           = ls_response-irn.
          einv_irn-ack_no        = ls_response-ackno.
          einv_irn-ack_date      = ls_response-ackdt.
          einv_irn-irn_status    = ls_response-status.

          einv_irn-ernam         = sy-uname.
          einv_irn-erdat         = sy-datum.
          einv_irn-erzet         = sy-uzeit.
          einv_irn-signed_inv    = ls_response-signedinvoice.
          einv_irn-signed_qrcode = ls_response-signedqrcode.

        ELSE.
          DATA: v_str     TYPE string,
                irn       TYPE j_1ig_irn,
                lv_irn    TYPE string,
                lobj_einv TYPE REF TO zcl_dms_einvoice_process.

          " Error Details
          IF message IS INITIAL.
            SPLIT lv_response AT '"errorDetails":' INTO v_str message.
            IF message IS INITIAL.
              SPLIT lv_response AT '"ErrorDetails":' INTO v_str message.
            ENDIF.
          ENDIF.
          IF message IS INITIAL.
            message = |IRN is not created|.
          ENDIF.

*******Getting E-Invoice BY IRN API in GET Method ****************
          CLEAR : v_str,irn,lv_irn.
          SPLIT lv_response AT '"irn":"' INTO v_str lv_irn.
          SPLIT lv_irn AT '"' INTO irn v_str.

          IF irn IS NOT INITIAL.
********************get the irn status****************
            CREATE OBJECT lobj_einv.
            lobj_einv->get_irn_details(
              EXPORTING
                irn              = irn
                distributor_code = distributor_code
                retailer         = customer
              IMPORTING
                response         = DATA(ls_output)
                type             = DATA(lv_type)
                message          = DATA(lv_msg) ).

            IF ls_output-signedqrcode IS NOT INITIAL AND ls_output-ackno IS NOT INITIAL.
*  ***einvoice irn invrefnum details.--->>
              einv_irn-bukrs         = bukrs.
              einv_irn-doc_year      = gjahr.
*  SHIFT p_einv_out-vbeln LEFT DELETING LEADING '0'.
              einv_irn-distrb        = distributor_code .
              einv_irn-docno         = vbeln.
              einv_irn-version       = 11.
*          einv_irn-doc_type      = 'INV'.

              einv_irn-irn           = ls_output-irn.
              einv_irn-ack_no        = ls_output-ackno.
              einv_irn-ack_date      = ls_output-ackdt.
              einv_irn-irn_status    = ls_output-status.

              einv_irn-ernam         = sy-uname.
              einv_irn-erdat         = sy-datum.
              einv_irn-erzet         = sy-uzeit.
              einv_irn-signed_inv    = ls_output-signedinvoice.
              einv_irn-signed_qrcode = ls_output-signedqrcode.
              CLEAR : message.
            ELSE.
              message = lv_msg.
            ENDIF.
            CLEAR : lv_msg,lv_type,ls_output.
          ENDIF.

*          SPLIT lv_response AT '"InfCd":"' INTO v_str v_duplicate.
*
*          IF v_duplicate <> 'DUPIRN'.
*            SPLIT lv_response AT '"Desc":"'  INTO v_str v_duplicate.
*          ENDIF.
*
********Duplicate IRN or Response Not getting at Initial Time*******
********Getting E-Invoice BY IRN API in GET Method ****************
*          IF v_duplicate = 'DUPIRN' OR v_duplicate = 'Duplic'.      "Duplicate IRN already Generated.
*            CLEAR: v_str.
*            SPLIT lv_response AT '"Irn":"' INTO v_str  irn.
*            message = |Invoice No { vbeln } CoCode: { bukrs } DocYear { gjahr } ia already processed with IRN: { irn }|.
*          ENDIF.
        ENDIF.
      ELSE.
        message = |Error Code: { lv_codes } - { lv_http_error }|.
      ENDIF.
    ELSE.
      message = 'URL is not maintained in STVARV'.
    ENDIF.

    DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
    DATA: lv_data TYPE string,
          lv_body TYPE string.
    CREATE OBJECT lo_log_upd.

    IF message IS NOT INITIAL.
      lv_body = |"message":"{ message }"|.
      CONCATENATE '[{' lv_body '}]' INTO lv_body.
    ELSE.
** serialize the output for response ***
      /ui2/cl_json=>serialize(
      EXPORTING
       data         =  ls_response
       pretty_name  = /ui2/cl_json=>pretty_mode-user
      RECEIVING
       r_json         = lv_body ).
    ENDIF.

*Output Entry in Log Table
    CALL METHOD lo_log_upd->log_entry_store
      EXPORTING
        apiname         = 'SARALAPI_IRN_CREATE'
        ijson           = iv_payload_json
        ojson           = lv_body
        distributor     = distributor_code
        retailer        = customer
      EXCEPTIONS
        apiname_missing = 1
        json_missing    = 2
        OTHERS          = 3.

  ENDMETHOD.


  METHOD cancel_ewb.

    CALL METHOD me->get_saral_token
      EXPORTING
        distributor_code     = distributor_code
      IMPORTING
        authentication_token = DATA(authentication_token)
        subscription_id      = DATA(subscription_id)
        error_code           = DATA(error_code)
        error_msg            = DATA(error_msg).
    IF error_msg IS INITIAL AND
       authentication_token IS NOT INITIAL AND
       subscription_id IS NOT INITIAL.
      CALL METHOD me->get_irn_token
        EXPORTING
          distributor_code     = distributor_code
          authentication_token = authentication_token
          subscription_id      = subscription_id
        IMPORTING
          err_msg              = DATA(err_msg)
          auth_token           = DATA(auth_token)
          session_key          = DATA(session_key)
          user_name            = DATA(user_name)
          gstin                = DATA(gstin).
      " Fill Data
      IF err_msg IS INITIAL AND
         auth_token IS NOT INITIAL AND
         session_key IS NOT INITIAL AND
         user_name IS NOT INITIAL AND
         gstin IS NOT INITIAL.
        CALL METHOD me->fill_invoice_details
          EXPORTING
            distributor_code = distributor_code
            vbeln            = vbeln
*           date             = DATE
          IMPORTING
            doctyp           = DATA(doctyp)
            dealer           = DATA(dealer)
            data_tab         = DATA(lw_invoice_tab)
            message          = return.
        IF lw_invoice_tab IS NOT INITIAL AND
           return IS INITIAL.
* Prepare Json File
          CALL METHOD me->prepare_json_payload
            EXPORTING
              data_tab     = lw_invoice_tab
            IMPORTING
              payload_json = DATA(payload_json).

          DATA: einv_irn  TYPE zdms_invoice_irn, "j_1ig_invrefnum,
                eway_bill	TYPE j_1ig_ewaybill.
* API Call
          CALL METHOD me->call_api_generate_irn
            EXPORTING
              distributor_code     = distributor_code
              authentication_token = authentication_token
              subscription_id      = subscription_id
              auth_token           = auth_token
              session_key          = session_key
              user_name            = user_name
              gstin                = gstin
              vbeln                = vbeln
              bukrs                = bukrs
              gjahr                = gjahr
              iv_payload_json      = payload_json
            IMPORTING
              einv_irn             = einv_irn
              eway_bill            = eway_bill
              message              = return.
          IF return IS INITIAL AND
             ( einv_irn IS NOT INITIAL OR
               eway_bill IS NOT INITIAL ).
            irn = einv_irn-irn.
            signed_inv = einv_irn-signed_inv.
            signed_qrcode = einv_irn-signed_qrcode.

****INSERT data to table j_1ig_invrefnum & j_1ig_ewaybill----->>>
            IF einv_irn IS NOT INITIAL.
              einv_irn-doc_type = doctyp.
              einv_irn-dealer = dealer.
              MODIFY zdms_invoice_irn FROM einv_irn.
            ENDIF.
            IF eway_bill IS NOT INITIAL.
              MODIFY j_1ig_ewaybill FROM eway_bill.
            ENDIF.
          ELSE.
            IF return IS INITIAL.
              return = 'IRN Creation is not generated'.
            ELSE.
              return = return.
            ENDIF.
          ENDIF.
        ELSE.
          return  = return.
        ENDIF.
      ELSE.
        IF err_msg IS INITIAL.
          return = 'Authentication Token is not generated'.
        ELSE.
          return = | Authentication Token Error: { err_msg } |.
        ENDIF.
      ENDIF.
    ELSE.
      IF error_msg IS INITIAL.
        return = 'Saral Token is not generated'.
      ELSE.
        return = | Saral Token Error: { error_msg } |.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD cancel_irn_qrcode.

    DATA : lv_status TYPE string.

*---- Billing Invoice Header Details-----*
    SELECT SINGLE
            kunag
            FROM vbrk  INTO @DATA(l_customer)
            WHERE vbeln =  @vbeln.
    CALL METHOD me->get_saral_token
      EXPORTING
        distributor_code     = distributor_code
        customer             = l_customer
      IMPORTING
        authentication_token = DATA(authentication_token)
        subscription_id      = DATA(subscription_id)
        error_code           = DATA(error_code)
        error_msg            = DATA(error_msg).
    IF error_msg IS INITIAL AND
       authentication_token IS NOT INITIAL AND
       subscription_id IS NOT INITIAL.
      CALL METHOD me->get_irn_token
        EXPORTING
          distributor_code     = distributor_code
          customer             = l_customer
          authentication_token = authentication_token
          subscription_id      = subscription_id
        IMPORTING
          err_msg              = DATA(err_msg)
          auth_token           = DATA(auth_token)
          session_key          = DATA(session_key)
          user_name            = DATA(user_name)
          gstin                = DATA(gstin).
      " Fill Data
      IF err_msg IS INITIAL AND
         auth_token IS NOT INITIAL AND
         session_key IS NOT INITIAL AND
         user_name IS NOT INITIAL AND
         gstin IS NOT INITIAL.
        DATA payload_json TYPE string.
*******************************get the irn details for double verification**********************
*        CLEAR : lv_status,return,type.
*        CALL METHOD get_irn_details
*          EXPORTING
*            distributor_code     = distributor_code
*            customer             = l_customer
*            authentication_token = authentication_token
*            subscription_id      = subscription_id
*            auth_token           = auth_token
*            session_key          = session_key
*            user_name            = user_name
*            gstin                = gstin
*            lv_irn               = irn
*          IMPORTING
*            message              = return
*            type                 = type
*            status               = lv_status.
*******************validate the IRN status***************
*        IF type = 'S'.
*          IF lv_status = 'CNL'.
*            return = 'IRN Cancelled successfully'.
*            type   = 'S'.
*            EXIT.
*          ELSEIF lv_status   = 'ACT'.
* Prepare Json File
        CONCATENATE
        '{'
        ' "Irn":"' irn '",'
        '"CnlRsn":"1",'
        '"CnlRem":"' cnlrem '" }'
        INTO payload_json RESPECTING BLANKS.

* API Call for IRN cancellation***************
        CLEAR : return,type.
        CALL METHOD me->call_api_cancel_irn
          EXPORTING
            distributor_code     = distributor_code
            customer             = l_customer
            authentication_token = authentication_token
            subscription_id      = subscription_id
            auth_token           = auth_token
            session_key          = session_key
            user_name            = user_name
            gstin                = gstin
            iv_payload_json      = payload_json
          IMPORTING
            message              = return
            type                 = type.

        IF type = 'S'.
*******************************get the irn details for double verification**********************
*              CLEAR : lv_status,return,type.
*              CALL METHOD get_irn_details
*                EXPORTING
*                  distributor_code     = distributor_code
*                  customer             = l_customer
*                  authentication_token = authentication_token
*                  subscription_id      = subscription_id
*                  auth_token           = auth_token
*                  session_key          = session_key
*                  user_name            = user_name
*                  gstin                = gstin
*                  lv_irn               = irn
*                IMPORTING
*                  message              = return
*                  type                 = type
*                  status               = lv_status.
*              IF type = 'E'.
*                EXIT.
*              ELSEIF type IS INITIAL.
*                return = |{ return } , IRN not cancelled'|.
*                type   = 'E'.
*                EXIT.
*              ENDIF.
*              IF lv_status = 'CNL'.
*                return = 'IRN Cancelled successfully'.
*                type   = 'S'.
*              ELSEIF lv_status   = 'ACT'.
*                return  = 'IRN not Cancelled'.
*                type    = 'E'.
*                EXIT.
*              ELSE.
*                return = |{ return } , IRN not cancelled'|.
*                type   = 'E'.
*              ENDIF.
*********************UPDATE THE TABLE********************
          SELECT SINGLE * FROM zdms_invoice_irn INTO @DATA(l_inv_irn)
            WHERE bukrs = 'DMS1' AND
                  distrb = @distributor_code AND
                  docno = @vbeln AND
                  doc_year = @gjahr AND
                  irn = @irn AND
                  version = 11.
          l_inv_irn-irn_status = 'CNL'.
          l_inv_irn-cancel_date = sy-datum.
          MODIFY zdms_invoice_irn FROM l_inv_irn.
*        IF eway_bill IS NOT INITIAL.
*          MODIFY j_1ig_ewaybill FROM eway_bill.
*        ENDIF.
        ELSE.
          return = |IRN Error : { return }|.
          type   = 'E'.
        ENDIF.
*          ENDIF.
*        ELSEIF type IS INITIAL.
*          type   = 'E'.
*          return = 'IRN is not cancelled,Auth error - GET IRN'.
*        ENDIF.
      ELSE.
        IF err_msg IS INITIAL.
          return = 'Authentication Token is not generated'.
        ELSE.
          return = | Authentication Token Error: { err_msg } |.
        ENDIF.
      ENDIF.
    ELSE.
      IF error_msg IS INITIAL.
        return = 'Saral Token is not generated'.
      ELSE.
        return = | Saral Token Error: { error_msg } |.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD create_ewb.

    CALL METHOD me->get_saral_token
      EXPORTING
        distributor_code     = distributor_code
      IMPORTING
        authentication_token = DATA(authentication_token)
        subscription_id      = DATA(subscription_id)
        error_code           = DATA(error_code)
        error_msg            = DATA(error_msg).
    IF error_msg IS INITIAL AND
       authentication_token IS NOT INITIAL AND
       subscription_id IS NOT INITIAL.
      CALL METHOD me->get_irn_token
        EXPORTING
          distributor_code     = distributor_code
          authentication_token = authentication_token
          subscription_id      = subscription_id
        IMPORTING
          err_msg              = DATA(err_msg)
          auth_token           = DATA(auth_token)
          session_key          = DATA(session_key)
          user_name            = DATA(user_name)
          gstin                = DATA(gstin).
      " Fill Data
      IF err_msg IS INITIAL AND
         auth_token IS NOT INITIAL AND
         session_key IS NOT INITIAL AND
         user_name IS NOT INITIAL AND
         gstin IS NOT INITIAL.
        CALL METHOD me->fill_invoice_details
          EXPORTING
            distributor_code = distributor_code
            vbeln            = vbeln
*           date             = DATE
          IMPORTING
            doctyp           = DATA(doctyp)
            dealer           = DATA(dealer)
            data_tab         = DATA(lw_invoice_tab)
            message          = return.
        IF lw_invoice_tab IS NOT INITIAL AND
           return IS INITIAL.
* Prepare Json File
          CALL METHOD me->prepare_json_payload
            EXPORTING
              data_tab     = lw_invoice_tab
            IMPORTING
              payload_json = DATA(payload_json).

          DATA: einv_irn  TYPE zdms_invoice_irn, "j_1ig_invrefnum,
                eway_bill	TYPE j_1ig_ewaybill.
* API Call
          CALL METHOD me->call_api_generate_irn
            EXPORTING
              distributor_code     = distributor_code
              authentication_token = authentication_token
              subscription_id      = subscription_id
              auth_token           = auth_token
              session_key          = session_key
              user_name            = user_name
              gstin                = gstin
              vbeln                = vbeln
              bukrs                = bukrs
              gjahr                = gjahr
              iv_payload_json      = payload_json
            IMPORTING
              einv_irn             = einv_irn
              eway_bill            = eway_bill
              message              = return.
          IF return IS INITIAL AND
             ( einv_irn IS NOT INITIAL OR
               eway_bill IS NOT INITIAL ).
            irn = einv_irn-irn.
            signed_inv = einv_irn-signed_inv.
            signed_qrcode = einv_irn-signed_qrcode.

****INSERT data to table j_1ig_invrefnum & j_1ig_ewaybill----->>>
            IF einv_irn IS NOT INITIAL.
              einv_irn-doc_type = doctyp.
              einv_irn-dealer = dealer.
              MODIFY zdms_invoice_irn FROM einv_irn.
            ENDIF.
            IF eway_bill IS NOT INITIAL.
              MODIFY j_1ig_ewaybill FROM eway_bill.
            ENDIF.
          ELSE.
            IF return IS INITIAL.
              return = 'IRN Creation is not generated'.
            ELSE.
              return = return.
            ENDIF.
          ENDIF.
        ELSE.
          return  = return.
        ENDIF.
      ELSE.
        IF err_msg IS INITIAL.
          return = 'Authentication Token is not generated'.
        ELSE.
          return = | Authentication Token Error: { err_msg } |.
        ENDIF.
      ENDIF.
    ELSE.
      IF error_msg IS INITIAL.
        return = 'Saral Token is not generated'.
      ELSE.
        return = | Saral Token Error: { error_msg } |.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD create_irn_qrcode.

*---- Billing Invoice Header Details-----*
    SELECT SINGLE
            kunag
            FROM vbrk  INTO @DATA(l_customer)
            WHERE vbeln =  @vbeln.
    CALL METHOD me->get_saral_token
      EXPORTING
        distributor_code     = distributor_code
        customer             = l_customer
      IMPORTING
        authentication_token = DATA(authentication_token)
        subscription_id      = DATA(subscription_id)
        error_code           = DATA(error_code)
        error_msg            = DATA(error_msg).
    IF error_msg IS INITIAL AND
       authentication_token IS NOT INITIAL AND
       subscription_id IS NOT INITIAL.
      CALL METHOD me->get_irn_token
        EXPORTING
          distributor_code     = distributor_code
          customer             = l_customer
          authentication_token = authentication_token
          subscription_id      = subscription_id
        IMPORTING
          err_msg              = DATA(err_msg)
          auth_token           = DATA(auth_token)
          session_key          = DATA(session_key)
          user_name            = DATA(user_name)
          gstin                = DATA(gstin).
      " Fill Data
      IF err_msg IS INITIAL AND
         auth_token IS NOT INITIAL AND
         session_key IS NOT INITIAL AND
         user_name IS NOT INITIAL AND
         gstin IS NOT INITIAL.
        CALL METHOD me->fill_invoice_details
          EXPORTING
            distributor_code = distributor_code
            vbeln            = vbeln
*           date             = DATE
          IMPORTING
            doctyp           = DATA(doctyp)
            dealer           = DATA(dealer)
            data_tab         = DATA(lw_invoice_tab)
            message          = return.
        IF lw_invoice_tab IS NOT INITIAL AND
           return IS INITIAL.
* Prepare Json File
          CALL METHOD me->prepare_json_payload
            EXPORTING
              data_tab     = lw_invoice_tab
            IMPORTING
              payload_json = DATA(payload_json).

          DATA: einv_irn  TYPE zdms_invoice_irn, "j_1ig_invrefnum,
                eway_bill	TYPE j_1ig_ewaybill.
* API Call
          CALL METHOD me->call_api_generate_irn
            EXPORTING
              distributor_code     = distributor_code
              customer             = l_customer
              authentication_token = authentication_token
              subscription_id      = subscription_id
              auth_token           = auth_token
              session_key          = session_key
              user_name            = user_name
              gstin                = gstin
              vbeln                = vbeln
              bukrs                = bukrs
              gjahr                = gjahr
              iv_payload_json      = payload_json
            IMPORTING
              einv_irn             = einv_irn
              eway_bill            = eway_bill
              message              = return.
          IF return IS INITIAL AND
             ( einv_irn IS NOT INITIAL OR
               eway_bill IS NOT INITIAL ).
            irn = einv_irn-irn.
            signed_inv = einv_irn-signed_inv.
            signed_qrcode = einv_irn-signed_qrcode.
            ack_no = einv_irn-ack_no.
            ack_date = einv_irn-ack_date.
****INSERT data to table j_1ig_invrefnum & j_1ig_ewaybill----->>>
            IF einv_irn IS NOT INITIAL.
              einv_irn-doc_type = doctyp.
              einv_irn-dealer = dealer.

              MODIFY zdms_invoice_irn FROM einv_irn.
            ENDIF.
*            IF eway_bill IS NOT INITIAL.
*              MODIFY j_1ig_ewaybill FROM eway_bill.
*            ENDIF.
          ELSE.
            IF return IS INITIAL.
              return = 'IRN Creation is not generated'.
            ELSE.
              return = return.
            ENDIF.
          ENDIF.
        ELSE.
          return  = return.
        ENDIF.
      ELSE.
        IF err_msg IS INITIAL.
          return = 'Authentication Token is not generated'.
        ELSE.
          return = | Authentication Token Error: { err_msg } |.
        ENDIF.
      ENDIF.
    ELSE.
      IF error_msg IS INITIAL.
        return = 'Saral Token is not generated'.
      ELSE.
        return = | Saral Token Error: { error_msg } |.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD create_irn_qrcode_poreturn.

    CALL METHOD me->get_saral_token
      EXPORTING
        distributor_code     = distributor_code
      IMPORTING
        authentication_token = DATA(authentication_token)
        subscription_id      = DATA(subscription_id)
        error_code           = DATA(error_code)
        error_msg            = DATA(error_msg).
    IF error_msg IS INITIAL AND
       authentication_token IS NOT INITIAL AND
       subscription_id IS NOT INITIAL.
      CALL METHOD me->get_irn_token
        EXPORTING
          distributor_code     = distributor_code
          authentication_token = authentication_token
          subscription_id      = subscription_id
        IMPORTING
          err_msg              = DATA(err_msg)
          auth_token           = DATA(auth_token)
          session_key          = DATA(session_key)
          user_name            = DATA(user_name)
          gstin                = DATA(gstin).
      " Fill Data
      IF err_msg IS INITIAL AND
         auth_token IS NOT INITIAL AND
         session_key IS NOT INITIAL AND
         user_name IS NOT INITIAL AND
         gstin IS NOT INITIAL.

        CLEAR return.

        CALL METHOD me->fill_poreturn_details
          EXPORTING
            distributor_code = distributor_code " Customer Number
            seller_plant     = seller_plant   " Plant
            po_number        = po_number            " Purchasing Document Number
            invoiceno        = invoiceno           " Billing Document
            invoicedt        = invoicedt    " Billing Date
            seller_invoiceno = seller_invoiceno  " Billing Document
            bukrs            = bukrs               " Company Code
            gjahr            = gjahr    " Fiscal Year
          IMPORTING
            doctyp           = DATA(doctyp)
            data_tab         = DATA(lw_invoice_tab)
            message          = return.
        IF lw_invoice_tab IS NOT INITIAL AND
           return IS INITIAL.
* Prepare Json File
          CALL METHOD me->prepare_json_payload
            EXPORTING
              data_tab         = lw_invoice_tab
              invoiceno        = invoiceno           " Billing Document
              invoicedt        = invoicedt    " Billing Date
              seller_invoiceno = seller_invoiceno  " Billing Document
            IMPORTING
              payload_json     = DATA(payload_json).

          DATA: einv_irn  TYPE zdms_invoice_irn,
                eway_bill	TYPE j_1ig_ewaybill.
* API Call
          SELECT SINGLE * FROM  zdms_invoice_irn INTO @DATA(lw_irn) WHERE docno = @invoiceno
                                                                    AND irn_status = 'ACT'.
          IF sy-subrc <> 0.
            CALL METHOD me->call_api_generate_irn
              EXPORTING
                distributor_code     = distributor_code
                authentication_token = authentication_token
                subscription_id      = subscription_id
                auth_token           = auth_token
                session_key          = session_key
                user_name            = user_name
                gstin                = gstin
                vbeln                = invoiceno
                bukrs                = bukrs
                gjahr                = gjahr
                iv_payload_json      = payload_json
              IMPORTING
                einv_irn             = einv_irn
                eway_bill            = eway_bill
                message              = return.
            IF return IS INITIAL AND
               ( einv_irn IS NOT INITIAL OR
                 eway_bill IS NOT INITIAL ).

              irn = einv_irn-irn.

              signed_inv = einv_irn-signed_inv.
              signed_qrcode = einv_irn-signed_qrcode.
              ack_no = einv_irn-ack_no.
              ack_date = einv_irn-ack_date.

              einv_irn-doc_type = doctyp.

              SELECT SINGLE lifnr INTO @DATA(l_lifnr) FROM ekko
                WHERE ebeln = @po_number.
              einv_irn-dealer = l_lifnr.

              CALL METHOD me->generate_pdf
                EXPORTING
                  inv_details      = lw_invoice_tab
                  inv_no           = invoiceno           " Billing Document
                  inv_date         = invoicedt    " Billing Date
                  irn              = irn
                  signed_inv       = signed_inv
                  signed_qrcode    = signed_qrcode
                  po_number        = po_number
                  seller_invoiceno = seller_invoiceno
                IMPORTING
                  pdf              = pdf
                  return           = return.
              einv_irn-pdf = pdf.
              MODIFY zdms_invoice_irn FROM einv_irn.
            ELSE.
              IF return IS INITIAL.
                return = 'IRN Creation is not generated'.
              ELSE.
                return = return.
              ENDIF.
            ENDIF.
          ELSE.
            irn = lw_irn-irn.
            signed_inv = lw_irn-signed_inv.
            signed_qrcode = lw_irn-signed_qrcode.
            pdf = lw_irn-pdf.
          ENDIF.
        ELSE.
          return  = return.
        ENDIF.
      ELSE.
        IF err_msg IS INITIAL.
          return = 'Authentication Token is not generated'.
        ELSE.
          return = | Authentication Token Error: { err_msg } |.
        ENDIF.
      ENDIF.
    ELSE.
      IF error_msg IS INITIAL.
        return = 'Saral Token is not generated'.
      ELSE.
        return = | Saral Token Error: { error_msg } |.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD fill_invoice_details.
    DATA: lv_date TYPE char10.
    CONSTANTS: c_version TYPE char05 VALUE '1.1',
               c_gst     TYPE char10 VALUE  'GST',
               c_b2b     TYPE char03 VALUE  'B2B',
               c_null    TYPE char07 VALUE  'null',
               c_zero    TYPE char01 VALUE  '0'.

    DATA: v_tcs    TYPE string,
          v_assval TYPE string,
          v_igst   TYPE string,
          v_cgst   TYPE string,
          v_sgst   TYPE string,
          v_amtval TYPE string.

*    REFRESH:  ret_tab.
    IF vbeln IS NOT INITIAL.
*---- Billing Invoice Header Details-----*
      SELECT  vbeln,      "Doc No.
              fkart,      "Doc type   & Item Is Service
              vkorg,
              knumv,
              fkdat,     "Doc date
              bukrs,
              xblnr,
              kunag
              FROM vbrk  INTO TABLE  @DATA(l_tab_vbrk)
              WHERE vbeln =  @vbeln.
      IF sy-subrc NE 0.
*        APPEND VALUE #( type = 'E'
*                        id = vbeln
*                        message = |Incorrect Billing Invoice Number| ) TO ret_tab.
        message = |Error: Incorrect Billing Invoice Number { vbeln }|.
        EXIT.
      ENDIF.
    ENDIF.
*---- Initial checks success then Further Selection ---*
    IF message IS INITIAL.
      IF l_tab_vbrk[] IS NOT INITIAL.
*----- Billing Invoice item details -----*
        SELECT a~vbeln,
               a~posnr,     "SL no of Item
               a~fkimg,     "Item Qty
               a~vrkme,     "Item Unit
               a~netwr,
               a~vgbel,
               a~matnr,
               a~arktx,
               a~werks,
               b~steuc      "Item HsnCd
                       FROM vbrp AS a
                       INNER JOIN marc AS b
                       ON a~matnr = b~matnr
                       AND a~werks = b~werks
                       INTO TABLE @DATA(l_tab_vbrp)
                       FOR ALL ENTRIES IN @l_tab_vbrk
                       WHERE a~vbeln = @l_tab_vbrk-vbeln .
        IF sy-subrc = 0.
          SORT l_tab_vbrp[] BY vbeln posnr.
        ENDIF.
*----- Condition Data for Billing Invoice -------*
        "Price details from prcd_elements---->>>
        SELECT knumv,
               kposn,
               kschl,
               kbetr,
               kwert,
               kawrt
                     FROM prcd_elements   ""konv
                     INTO TABLE @DATA(l_tab_prcd)
                     FOR ALL ENTRIES IN @l_tab_vbrk
                     WHERE knumv = @l_tab_vbrk-knumv .
        IF sy-subrc = 0.
          SORT l_tab_prcd[] BY knumv kposn.
        ENDIF.
*----- Buyer Details Based on Invoice Customer Number -----*
        DATA(lv_customer) = CONV kunnr( VALUE #( l_tab_vbrk[ 1 ]-kunag OPTIONAL ) ).
        IF lv_customer IS NOT INITIAL.
          SELECT SINGLE * FROM kna1 INTO @DATA(l_dealer_dtls) WHERE kunnr = @lv_customer.
          IF sy-subrc = 0.
            SELECT SINGLE * FROM adrc INTO @DATA(l_dealer_addr) WHERE addrnumber = @l_dealer_dtls-adrnr.
            IF sy-subrc = 0.

            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
*------ Based on Billing Invoice Item data --------*
      IF l_tab_vbrp[] IS NOT INITIAL.
        "Get the Seller Gstin--->>
        SELECT werks,
               name1,
               adrnr,
               j_1bbranch FROM t001w
                          INTO TABLE @DATA(lt_tab_t001w)
                          FOR ALL ENTRIES IN  @l_tab_vbrp
                          WHERE werks EQ @l_tab_vbrp-werks.
        IF lt_tab_t001w IS NOT INITIAL.
          SELECT bukrs,
                 branch,
                 gstin    "Seller GSTIN
                       FROM j_1bbranch
                       INTO TABLE @DATA(l_tab_branch)
                       FOR ALL ENTRIES IN @lt_tab_t001w
                       WHERE branch = @lt_tab_t001w-j_1bbranch.
          "Get the material description->>
          SELECT matnr,
                 maktx FROM makt
                       INTO TABLE @DATA(l_tab_makt)
                       FOR ALL ENTRIES IN @l_tab_vbrp
                       WHERE matnr EQ @l_tab_vbrp-matnr
                       AND spras EQ @sy-langu.
          IF sy-subrc = 0.
            SORT l_tab_makt[] BY matnr.
          ENDIF.

          SELECT matnr,
                 steuc,
                 werks FROM marc
                       INTO TABLE @DATA(l_tab_marc)
                       FOR ALL ENTRIES IN @l_tab_vbrp
                       WHERE matnr EQ @l_tab_vbrp-matnr.
          IF sy-subrc = 0.
            SORT l_tab_marc[] BY matnr werks.
          ENDIF.
        ENDIF.
*----- Seller Details based on invoice plant  ------*
*        DATA(lv_werks) = CONV werks_d( VALUE #( l_tab_vbrp[ 1 ]-werks OPTIONAL ) ).
*        IF lv_werks IS NOT INITIAL.
        SELECT SINGLE * FROM kna1 INTO @DATA(l_dist_dtls) WHERE kunnr = @distributor_code.
        IF sy-subrc = 0.
          SELECT SINGLE * FROM adrc INTO @DATA(l_dist_addr) WHERE addrnumber = @l_dist_dtls-adrnr.
        ENDIF.
*      ENDIF.
      ENDIF.
*------- Data Population for Gloabal Structures ------*
      CLEAR: data_tab.
      data_tab-distributor = l_dist_dtls-kunnr. "Distributor Code.
      data_tab-body-version = c_version.
*---- Transaction Details ----*
      DATA(l_trandtls) = VALUE zeinv_trans_st( taxsch = c_gst
                                               suptyp = c_b2b
                                               regrev = 'N' ).
      data_tab-body-trandtls =  l_trandtls.
*----- Billing Document Details based on Invoice ------*
      DATA(lv_doctype) = VALUE #( l_tab_vbrk[ 1 ]-fkart OPTIONAL ).

      CASE lv_doctype.
        WHEN 'YDMS'."'YRF2' OR 'YBFS' OR 'YBDP' OR 'YBBR' OR 'IV' OR 'YSTO' OR 'YBTE' OR 'YBEO' OR
          doctyp    = 'INV'.
        WHEN 'YRMS'."'YBRE' OR 'YIRE' OR 'YFRE' OR
          doctyp    = 'CRN'.
        WHEN 'ZDRV'."'ZSIN' OR 'ZMSP' OR 'ZML2' OR 'ZL2' OR 'L2'.
          doctyp    = 'DBN'.
      ENDCASE.

      DATA(lv_inv_date) = VALUE #( l_tab_vbrk[ 1 ]-fkdat OPTIONAL ).
      WRITE lv_inv_date TO lv_date DD/MM/YYYY.
      REPLACE ALL OCCURRENCES OF '.' IN lv_date WITH '/'.
      DATA(vbeln_tmp) = vbeln.
      SHIFT vbeln_tmp LEFT DELETING LEADING '0'.
      DATA(l_docdtls) = VALUE zirn_docdls_st( typ = doctyp
                                              no = vbeln_tmp
                                              dt = lv_date ).
      data_tab-docdtls = l_docdtls.
*----------------------------------------------------------------------------------------------------------
*----- Seller Details which Means Distributor ----*
      SELECT SINGLE taxnum FROM dfkkbptaxnum INTO @DATA(l_dist_gstin) WHERE partner = @l_dist_dtls-kunnr.
      IF sy-subrc = 0.
        IF sy-sysid NE 'PRD'.
          data_tab-sellerdtls-gstin = '29AABCR7796N000'.
        ELSE.
          CONDENSE l_dist_gstin.
          data_tab-sellerdtls-gstin = l_dist_gstin. "Seller GSTIN
        ENDIF.
        "Seller Name
        IF l_dist_dtls-name1 IS NOT INITIAL.
          data_tab-sellerdtls-lglnm = l_dist_dtls-name1.
          data_tab-sellerdtls-trdnm = l_dist_dtls-name1.
        ELSE.
          data_tab-sellerdtls-lglnm = c_null.
          data_tab-sellerdtls-trdnm = c_null.
        ENDIF.
        "seller Address Details
        IF l_dist_dtls-stras IS NOT INITIAL.
          data_tab-sellerdtls-addr1 = l_dist_dtls-stras.
          data_tab-sellerdtls-addr2 = l_dist_dtls-stras.
        ELSE.
          IF l_dist_addr-street IS NOT INITIAL.
            data_tab-sellerdtls-addr1 = l_dist_addr-street.
            data_tab-sellerdtls-addr2 = l_dist_addr-street.
          ENDIF.
        ENDIF.
        "Seller Location
        IF l_dist_dtls-ort01 IS NOT INITIAL.
          data_tab-sellerdtls-loc = l_dist_dtls-ort01.
        ELSE.
          IF l_dist_addr-location IS NOT INITIAL.
            data_tab-sellerdtls-loc = l_dist_addr-location.
          ENDIF.
        ENDIF.
        "Seller Postal Code

        IF sy-sysid NE 'PRD'.
          data_tab-sellerdtls-pin = '560037'.
        ELSE.
          IF l_dist_dtls-pstlz IS NOT INITIAL.
            data_tab-sellerdtls-pin = l_dist_dtls-pstlz.
          ELSE.
            IF l_dist_addr-post_code1 IS NOT INITIAL.
              data_tab-sellerdtls-pin = l_dist_addr-post_code1.
            ENDIF.
          ENDIF.
        ENDIF.
        "Seller state Code
        IF l_dist_gstin IS NOT INITIAL.
          data_tab-sellerdtls-stcd = data_tab-sellerdtls-gstin(2).
          SHIFT data_tab-sellerdtls-stcd LEFT DELETING LEADING '0'.
        ENDIF.
        "Seller Phone Number
        IF l_dist_dtls-telf1 IS NOT INITIAL.
          data_tab-sellerdtls-ph = l_dist_dtls-telf1.
        ELSE.
          data_tab-sellerdtls-ph = c_null.
        ENDIF.
        "Seller Email
        SELECT SINGLE smtp_addr FROM adr6 INTO @DATA(l_seller_mail) WHERE addrnumber = @l_dist_addr-addrnumber.
        IF sy-subrc = 0.
          data_tab-sellerdtls-em = l_seller_mail.
        ELSE.
          data_tab-sellerdtls-em = c_null.
        ENDIF.
      ELSE.
        message = | Error: GST No is not maintained for Distributor { l_dist_dtls-kunnr } |.
      ENDIF.
*--------------------------------------------------------------------------------------------------------*
      "Buyer Details
      SELECT SINGLE taxnum FROM dfkkbptaxnum INTO @DATA(l_dealer_gstin) WHERE partner = @l_dealer_dtls-kunnr.
      IF sy-subrc = 0.
        dealer = l_dealer_dtls-kunnr.
        IF sy-sysid NE 'PRD'.
          data_tab-buyerdtls-gstin = '29AGHPG8602R1ZR'.
        ELSE.
          CONDENSE l_dealer_gstin.
          data_tab-buyerdtls-gstin = l_dealer_gstin. "Seller GSTIN
        ENDIF.
        "Buyer Name
        IF l_dealer_dtls-name1 IS NOT INITIAL.
          data_tab-buyerdtls-lglnm = l_dealer_dtls-name1. "Buyer Name
          data_tab-buyerdtls-trdnm = l_dealer_dtls-name1. "Buyer Name
        ELSE.
          data_tab-buyerdtls-lglnm = c_null.
          data_tab-buyerdtls-trdnm = c_null.
        ENDIF.
        "Buyer Address Details
        IF l_dealer_dtls-stras IS NOT INITIAL.
          data_tab-buyerdtls-addr1 = l_dealer_dtls-stras.
          data_tab-buyerdtls-addr2 = l_dealer_dtls-stras.
        ELSE.
          IF l_dealer_addr-street IS NOT INITIAL.
            data_tab-buyerdtls-addr1 = l_dealer_addr-street.
            data_tab-buyerdtls-addr2 = l_dealer_addr-street.
          ENDIF.
        ENDIF.
        "Buyer Location
        IF l_dealer_dtls-ort01 IS NOT INITIAL.
          data_tab-buyerdtls-loc = l_dealer_dtls-ort01.
        ELSE.
          IF l_dealer_addr-location IS NOT INITIAL.
            data_tab-buyerdtls-loc = l_dealer_addr-location.
          ENDIF.
        ENDIF.
        "Buyer Postal Code
        IF sy-sysid NE 'PRD'.
          data_tab-buyerdtls-pin = '560037'.
        ELSE.
          IF l_dealer_dtls-pstlz IS NOT INITIAL.
            data_tab-buyerdtls-pin = l_dealer_dtls-pstlz.
          ELSE.
            IF l_dealer_addr-post_code1 IS NOT INITIAL.
              data_tab-buyerdtls-addr1 = l_dealer_addr-post_code1.
            ENDIF.
          ENDIF.
        ENDIF.
        "Buyer state Code
        IF l_dealer_gstin IS NOT INITIAL.
          data_tab-buyerdtls-stcd = data_tab-buyerdtls-gstin(2).
          data_tab-buyerdtls-pos = data_tab-buyerdtls-gstin(2).
          SHIFT data_tab-buyerdtls-stcd LEFT DELETING LEADING '0'.
          SHIFT data_tab-buyerdtls-pos LEFT DELETING LEADING '0'.
        ENDIF.
      ELSE.
        message = | Error: GST No is not maintained for Dealor { l_dealer_dtls-kunnr } |.
      ENDIF.
*--------------------------------------------------------------------------------------------------------------------*
      DATA(lv_condno) = VALUE #( l_tab_vbrk[ 1 ]-knumv OPTIONAL ).
      "Invoice Items Details
      LOOP AT l_tab_vbrp ASSIGNING FIELD-SYMBOL(<fs_vbrp>).
        "Material HSN code
        DATA(lv_hsncd) = VALUE #( l_tab_marc[ matnr = <fs_vbrp>-matnr werks = <fs_vbrp>-werks ]-steuc OPTIONAL ).
        REPLACE ALL OCCURRENCES OF '.' IN lv_hsncd WITH space.
        CONDENSE lv_hsncd NO-GAPS.
        IF lv_hsncd IS INITIAL.
          lv_hsncd = c_null.
        ENDIF.
        "Lineitem Qty
        IF <fs_vbrp>-fkimg IS INITIAL.
          <fs_vbrp>-fkimg = c_null.
        ENDIF.
        "UOM Conversion
        IF <fs_vbrp>-vrkme = 'EA'.         "Each
          DATA(l_vrkme) = 'OTH'.
        ELSEIF <fs_vbrp>-vrkme = 'KG'.     "Kilograms
          l_vrkme = 'KGS'.
        ELSEIF <fs_vbrp>-vrkme = 'BT'.     "Bottle
          l_vrkme = 'BTL'.
        ELSEIF <fs_vbrp>-vrkme = 'L'.      "Litre
          l_vrkme = 'LTR'.
        ELSEIF <fs_vbrp>-vrkme = 'M'.      "Metre
          l_vrkme = 'MTR'.
        ELSEIF <fs_vbrp>-vrkme = 'PAA'.    "Pairs
          l_vrkme = 'PRS'.
        ELSE.
          l_vrkme = <fs_vbrp>-vrkme.
        ENDIF.

        IF <fs_vbrp>-netwr IS NOT INITIAL.
          DATA(l_uprice) = CONV kbetr_kond( <fs_vbrp>-netwr DIV <fs_vbrp>-fkimg ).
          DATA(l_amount) = <fs_vbrp>-netwr.
          DATA(l_assamt) = <fs_vbrp>-netwr.
        ELSE.
          l_uprice = c_zero.
          l_amount = c_zero.
          l_assamt = c_zero.
        ENDIF.
*----- Condition data amount Population --------*
        IF lv_condno IS NOT INITIAL.
          "CGST Condition
          DATA(ls_cgst_cond) = VALUE #( l_tab_prcd[ knumv = lv_condno
                                                    kposn = <fs_vbrp>-posnr
                                                    kschl = 'JOCG' ] OPTIONAL ).
          IF ls_cgst_cond IS NOT INITIAL.
            DATA(l_cgst) =  ls_cgst_cond-kwert.
            DATA(l_cgst_rate) = CONV kbetr_kond( ls_cgst_cond-kbetr ).
          ELSE.
            l_cgst = c_zero.
            l_cgst_rate = c_zero.
          ENDIF.
          "SGST Condition
          DATA(ls_sgst_cond) = VALUE #( l_tab_prcd[ knumv = lv_condno
                                                    kposn = <fs_vbrp>-posnr
                                                    kschl = 'JOSG' ] OPTIONAL ).
          IF ls_sgst_cond IS NOT INITIAL.
            DATA(l_sgst) =  ls_sgst_cond-kwert.
            DATA(l_sgst_rate) = CONV kbetr_kond( ls_sgst_cond-kbetr ).
          ELSE.
            l_sgst = c_zero.
            l_sgst_rate = c_zero.
          ENDIF.
          "IGST Condition
          DATA(ls_igst_cond) = VALUE #( l_tab_prcd[ knumv = lv_condno
                                                    kposn = <fs_vbrp>-posnr
                                                    kschl = 'JOIG' ] OPTIONAL ).
          IF ls_igst_cond IS NOT INITIAL.
            DATA(l_igst) = ls_igst_cond-kwert.
            DATA(l_igst_rate) = CONV kbetr_kond( ls_igst_cond-kbetr ).
          ELSE.
            l_igst = c_zero.
            l_igst_rate = c_zero.
          ENDIF.
          "JTC1 Condition
          DATA(ls_jtc1_cond) = VALUE #( l_tab_prcd[ knumv = lv_condno
                                                    kposn = <fs_vbrp>-posnr
                                                    kschl = 'JTC1' ] OPTIONAL ).
          IF ls_jtc1_cond IS NOT INITIAL.
            DATA(l_tcs) =  ls_jtc1_cond-kwert.
          ELSE.
            l_tcs = c_zero.
          ENDIF.
        ELSE.
          l_cgst = c_zero.
          l_cgst_rate = c_zero.
          l_sgst = c_zero.
          l_sgst_rate = c_zero.
          l_igst = c_zero.
          l_igst_rate = c_zero.
          l_tcs = c_zero.
        ENDIF.

        DATA(l_total_gst) = CONV string( l_igst_rate + l_cgst_rate + l_sgst_rate ).
        CONDENSE l_total_gst.
        DATA(l_totalitem_amt) = CONV string( l_assamt + l_igst + l_cgst + l_sgst + l_tcs ).
        CONDENSE l_totalitem_amt.

        APPEND VALUE #( slno       = <fs_vbrp>-posnr
                        prddesc    = VALUE #( l_tab_makt[ matnr = <fs_vbrp>-matnr ]-maktx OPTIONAL )
                        isservc    = 'N'
                        hsncd      = lv_hsncd
                        qty        = <fs_vbrp>-fkimg
                        freeqty    = c_zero
                        unit       = l_vrkme
                        unitprice  = l_uprice
                        totamt     = l_amount
                        assamt     = l_assamt
                        gstrt      = l_total_gst
                        igstamt    = l_igst
                        cgstamt    = l_cgst
                        sgstamt    = l_sgst
                        othchrg    = l_tcs
                        totitemval = l_totalitem_amt
                      ) TO data_tab-itemlist.

        CLEAR: l_cgst,l_sgst,l_igst,l_tcs,ls_cgst_cond,ls_sgst_cond,ls_igst_cond,ls_jtc1_cond,
               l_uprice,l_amount,l_assamt,l_total_gst,l_totalitem_amt.
      ENDLOOP.
*---- Invoice Total Value Population ----*\
      IF data_tab-itemlist IS NOT INITIAL.
        LOOP AT data_tab-itemlist INTO DATA(lw_itemlist).
****assessable Value, Total Value & igst Value
          v_assval = v_assval + lw_itemlist-assamt.
          v_amtval = v_amtval + lw_itemlist-totamt.
          v_tcs    = v_tcs    + lw_itemlist-othchrg.
          v_igst   = v_igst   + lw_itemlist-igstamt.
          v_cgst   = v_cgst   + lw_itemlist-cgstamt.
          v_sgst   = v_sgst   + lw_itemlist-sgstamt.
        ENDLOOP.
        data_tab-valdtls-assval      = v_assval.
        data_tab-valdtls-cgstval     = v_cgst.
        data_tab-valdtls-sgstval     = v_sgst.
        data_tab-valdtls-igstval     = v_igst.
        data_tab-valdtls-cesval      = c_zero.
        data_tab-valdtls-stcesval    = c_zero.
        data_tab-valdtls-stcesval    = c_zero.
        data_tab-valdtls-othchrg     = c_zero.
        data_tab-valdtls-rndoffamt   = c_zero.
        data_tab-valdtls-totinvval   = v_amtval + v_cgst + v_sgst + v_igst + v_tcs.
        data_tab-valdtls-totinvvalfc = c_zero.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD fill_poreturn_details.
    DATA: lv_date TYPE char10.
    CONSTANTS: c_version TYPE char05 VALUE '1.1',
               c_gst     TYPE char10 VALUE  'GST',
               c_b2b     TYPE char03 VALUE  'B2B',
               c_null    TYPE char07 VALUE  'null',
               c_zero    TYPE char01 VALUE  '0'.

    DATA: v_tcs       TYPE string,
          v_assval    TYPE string,
          v_igst      TYPE string,
          v_cgst      TYPE string,
          v_sgst      TYPE string,
          v_amtval    TYPE string,
          l_cgst      TYPE netpr,
          l_sgst      TYPE netpr,
          l_igst      TYPE netpr,
          lv_roundtot TYPE i.

    "Get the Seller Gstin--->>
    SELECT SINGLE
           werks,
           name1,
           adrnr,
           j_1bbranch FROM t001w
                      INTO  @DATA(l_dealer_dtls)
*                          FOR ALL ENTRIES IN  @l_tab_item
                      WHERE werks EQ @seller_plant.
    IF l_dealer_dtls IS NOT INITIAL.

      "Get Plant address details for Seller Addr1
      SELECT SINGLE
             a~werks,
             a~pstlz,
             a~land1,
             a~regio,
             b~bezei,
             c~addrnumber,      "Seller Legal Name
             c~name1,           "Despatch from Name\
             c~house_num1,
             c~street,          "Street     &"Despatch from add1
             c~str_suppl3,      "
             c~location,        "Despatch from add1
             c~post_code1,
             c~region,
             c~mc_city1
                    FROM ( ( t001w AS a
                      INNER JOIN t005u AS b
                      ON  b~spras = @sy-langu
                      AND a~land1 = b~land1
                      AND a~regio = b~bland )
                      INNER JOIN adrc AS c
                      ON  a~adrnr = c~addrnumber )
                     INTO @DATA(l_dealer_addr)
                WHERE a~werks = @seller_plant.

      SELECT SINGLE
             bukrs,
             branch,
             gstin    "Seller GSTIN
                   FROM j_1bbranch
                   INTO @DATA(l_buyer_gstin)
*                   FOR ALL ENTRIES IN @lt_tab_t001w
                   WHERE bukrs = '1000' AND
                         branch = @l_dealer_dtls-j_1bbranch.

    ENDIF.
*----- Seller Details based on invoice plant  ------*
*        DATA(lv_werks) = CONV werks_d( VALUE #( l_tab_item[ 1 ]-werks OPTIONAL ) ).
*        IF lv_werks IS NOT INITIAL.
    SELECT SINGLE * FROM kna1 INTO @DATA(l_dist_dtls) WHERE kunnr = @distributor_code.
    IF sy-subrc = 0.
      SELECT SINGLE * FROM adrc INTO @DATA(l_dist_addr) WHERE addrnumber = @l_dist_dtls-adrnr.
    ENDIF.
**      ENDIF.
*      ENDIF.
*------- Data Population for Gloabal Structures ------*
    CLEAR: data_tab.
    data_tab-distributor = l_dist_dtls-kunnr. "Distributor Code.
    data_tab-body-version = c_version.
*---- Transaction Details ----*
    DATA(l_trandtls) = VALUE zeinv_trans_st( taxsch = c_gst
                                             suptyp = c_b2b
                                             regrev = 'N' ).
    data_tab-body-trandtls =  l_trandtls.
    doctyp    = 'INV'."'DBN'.

    DATA(lv_inv_date) = invoicedt.
    WRITE lv_inv_date TO lv_date DD/MM/YYYY.
    REPLACE ALL OCCURRENCES OF '.' IN lv_date WITH '/'.
    DATA(invoiceno_tmp) = invoiceno.
    SHIFT invoiceno_tmp LEFT DELETING LEADING '0'.
    DATA(l_docdtls) = VALUE zirn_docdls_st( typ = doctyp
                                            no = invoiceno_tmp
                                            dt = lv_date ).
    data_tab-docdtls = l_docdtls.
*----------------------------------------------------------------------------------------------------------
*----- Seller Details which Means Distributor ----*
    SELECT SINGLE taxnum FROM dfkkbptaxnum INTO @DATA(l_dist_gstin) WHERE partner = @l_dist_dtls-kunnr.
    IF sy-subrc = 0.
      IF sy-sysid NE 'PRD'.
        data_tab-sellerdtls-gstin = '29AABCR7796N000'.
      ELSE.
        CONDENSE l_dist_gstin.
        data_tab-sellerdtls-gstin = l_dist_gstin. "Seller GSTIN
      ENDIF.
      "Seller Name
      IF l_dist_dtls-name1 IS NOT INITIAL.
        data_tab-sellerdtls-lglnm = l_dist_dtls-name1.
        data_tab-sellerdtls-trdnm = l_dist_dtls-name1.
      ELSE.
        data_tab-sellerdtls-lglnm = c_null.
        data_tab-sellerdtls-trdnm = c_null.
      ENDIF.
      "seller Address Details
      IF l_dist_dtls-stras IS NOT INITIAL.
        data_tab-sellerdtls-addr1 = l_dist_dtls-stras.
        data_tab-sellerdtls-addr2 = l_dist_dtls-stras.
      ELSE.
        IF l_dist_addr-street IS NOT INITIAL.
          data_tab-sellerdtls-addr1 = l_dist_addr-street.
          data_tab-sellerdtls-addr2 = l_dist_addr-street.
        ENDIF.
      ENDIF.
      "Seller Location
      IF l_dist_dtls-ort01 IS NOT INITIAL.
        data_tab-sellerdtls-loc = l_dist_dtls-ort01.
      ELSE.
        IF l_dist_addr-location IS NOT INITIAL.
          data_tab-sellerdtls-loc = l_dist_addr-location.
        ENDIF.
      ENDIF.
      "Seller Postal Code

      IF sy-sysid NE 'PRD'.
        data_tab-sellerdtls-pin = '560037'.
      ELSE.
        IF l_dist_dtls-pstlz IS NOT INITIAL.
          data_tab-sellerdtls-pin = l_dist_dtls-pstlz.
        ELSE.
          IF l_dist_addr-post_code1 IS NOT INITIAL.
            data_tab-sellerdtls-pin = l_dist_addr-post_code1.
          ENDIF.
        ENDIF.
      ENDIF.
      "Seller state Code
      IF l_dist_gstin IS NOT INITIAL.
        data_tab-sellerdtls-stcd = data_tab-sellerdtls-gstin(2).
        SHIFT data_tab-sellerdtls-stcd LEFT DELETING LEADING '0'.
      ENDIF.
      "Seller Phone Number
      IF l_dist_dtls-telf1 IS NOT INITIAL.
        data_tab-sellerdtls-ph = l_dist_dtls-telf1.
      ELSE.
        data_tab-sellerdtls-ph = c_null.
      ENDIF.
      "Seller Email
      SELECT SINGLE smtp_addr FROM adr6 INTO @DATA(l_seller_mail) WHERE addrnumber = @l_dist_addr-addrnumber.
      IF sy-subrc = 0.
        data_tab-sellerdtls-em = l_seller_mail.
      ELSE.
        data_tab-sellerdtls-em = c_null.
      ENDIF.
    ELSE.
      message = | Error: GST No is not maintained for Distributor { l_dist_dtls-kunnr } |.
    ENDIF.
*--------------------------------------------------------------------------------------------------------*
    "Buyer Details
*    SELECT SINGLE taxnum FROM dfkkbptaxnum INTO @DATA(l_dealer_gstin) WHERE partner = @l_dealer_dtls-kunnr.
    IF l_buyer_gstin IS NOT INITIAL.
      IF sy-sysid NE 'PRD'.
        data_tab-buyerdtls-gstin = '29AGHPG8602R1ZR'.
      ELSE.
        CONDENSE l_buyer_gstin-gstin.
        data_tab-buyerdtls-gstin = l_buyer_gstin-gstin. "Seller GSTIN
      ENDIF.
      "Buyer Name
      IF l_dealer_dtls-name1 IS NOT INITIAL.
        data_tab-buyerdtls-lglnm = l_dealer_dtls-name1. "Buyer Name
        data_tab-buyerdtls-trdnm = l_dealer_dtls-name1. "Buyer Name
      ELSE.
        data_tab-buyerdtls-lglnm = c_null.
        data_tab-buyerdtls-trdnm = c_null.
      ENDIF.
      "Buyer Address Details
*      IF l_dealer_dtls-stras IS NOT INITIAL.
*        data_tab-buyerdtls-addr1 = l_dealer_dtls-stras.
*        data_tab-buyerdtls-addr2 = l_dealer_dtls-stras.
*      ELSE.
      IF l_dealer_addr-street IS NOT INITIAL.
        data_tab-buyerdtls-addr1 = l_dealer_addr-street.
        data_tab-buyerdtls-addr2 = l_dealer_addr-street.
      ENDIF.
*      ENDIF.
      "Buyer Location
*      IF l_dealer_dtls-ort01 IS NOT INITIAL.
*        data_tab-buyerdtls-loc = l_dealer_dtls-ort01.
*      ELSE.
      IF l_dealer_addr-location IS NOT INITIAL.
        data_tab-buyerdtls-loc = l_dealer_addr-location.
      ENDIF.
*      ENDIF.
      "Buyer Postal Code
      IF sy-sysid NE 'PRD'.
        data_tab-buyerdtls-pin = '560037'.
      ELSE.
*        IF l_dealer_dtls-pstlz IS NOT INITIAL.
*          data_tab-buyerdtls-pin = l_dealer_dtls-pstlz.
*        ELSE.
        IF l_dealer_addr-post_code1 IS NOT INITIAL.
*          data_tab-buyerdtls-addr1 = l_dealer_addr-post_code1.
          data_tab-buyerdtls-pin = l_dealer_addr-post_code1.
        ENDIF.
*        ENDIF.
      ENDIF.
      "Buyer state Code
      IF l_buyer_gstin-gstin IS NOT INITIAL.
        data_tab-buyerdtls-stcd = data_tab-buyerdtls-gstin(2).
        data_tab-buyerdtls-pos = data_tab-buyerdtls-gstin(2).
        SHIFT data_tab-buyerdtls-stcd LEFT DELETING LEADING '0'.
        SHIFT data_tab-buyerdtls-pos LEFT DELETING LEADING '0'.
      ENDIF.
    ENDIF.
*--------------------------------------------------------------------------------------------------------------------*
*    DATA(lv_condno) = VALUE #( l_tab_vbrk[ 1 ]-knumv OPTIONAL ).


    "Get the material description->>
    SELECT a~ebeln,
           a~ebelp,
           a~matnr,
           b~steuc, " Matnr HSN Code
           a~txz01 AS maktx,
           a~werks,
           a~meins, "Order Unit
           a~menge, "Ordered Qty
           a~netpr, "Net unit Price
           a~netwr, "Net Price
           a~mwskz  "Tax code
           FROM ekpo AS a INNER JOIN marc AS b ON a~matnr = b~matnr
                                              AND a~werks = b~werks
      INTO TABLE @DATA(l_tab_item)
      WHERE a~ebeln = @po_number.
    IF sy-subrc = 0.
*** GST Variable for Calculation **
      SELECT kschl, kbetr, mwsk1 FROM konp
                                 INTO TABLE @DATA(lt_gst)
                                 FOR ALL ENTRIES IN @l_tab_item
                                 WHERE kappl = 'TX'
                                 AND kschl IN ( 'JIIG','JISG','JICG','JTC1' )
                                 AND mwsk1 EQ @l_tab_item-mwskz.
      IF sy-subrc = 0.
        SORT : lt_gst BY kschl mwsk1.
      ENDIF.
    ENDIF.
    "PO Items Details
    LOOP AT l_tab_item ASSIGNING FIELD-SYMBOL(<fs_vbrp>).
      "Material HSN code
      DATA(lv_hsncd) = <fs_vbrp>-steuc .
      REPLACE ALL OCCURRENCES OF '.' IN lv_hsncd WITH space.
      CONDENSE lv_hsncd NO-GAPS.
      IF lv_hsncd IS INITIAL.
        lv_hsncd = c_null.
      ENDIF.
*           a~netpr, "Net Price
      "Lineitem Qty
      IF <fs_vbrp>-menge IS INITIAL.
        <fs_vbrp>-menge = c_null.
      ENDIF.
      "UOM Conversion
      IF <fs_vbrp>-meins = 'EA'.         "Each
        DATA(l_meins) = 'OTH'.
      ELSEIF <fs_vbrp>-meins = 'KG'.     "Kilograms
        l_meins = 'KGS'.
      ELSEIF <fs_vbrp>-meins = 'BT'.     "Bottle
        l_meins = 'BTL'.
      ELSEIF <fs_vbrp>-meins = 'L'.      "Litre
        l_meins = 'LTR'.
      ELSEIF <fs_vbrp>-meins = 'M'.      "Metre
        l_meins = 'MTR'.
      ELSEIF <fs_vbrp>-meins = 'PAA'.    "Pairs
        l_meins = 'PRS'.
      ELSE.
        l_meins = <fs_vbrp>-meins.
      ENDIF.

      IF <fs_vbrp>-netwr IS NOT INITIAL.
        DATA(l_uprice) = <fs_vbrp>-netpr. "CONV kbetr_kond( <fs_vbrp>-netwr DIV <fs_vbrp>-menge ).
        DATA(l_amount) = <fs_vbrp>-netwr.
        DATA(l_assamt) = <fs_vbrp>-netwr.
      ELSE.
        l_uprice = c_zero.
        l_amount = c_zero.
        l_assamt = c_zero.
      ENDIF.
**----- Condition data amount Population --------*

**********Integrated GST Calculation ************************************************************
      READ TABLE lt_gst INTO DATA(ls_gst) WITH KEY kschl = 'JIIG'
                                                   mwsk1 = <fs_vbrp>-mwskz BINARY SEARCH.
      IF sy-subrc EQ 0.
        DATA(l_itax) = ( ls_gst-kbetr / 10 ).
        l_igst       = ( <fs_vbrp>-netwr * l_itax ) / 100.
      ELSE.
        l_itax = c_zero.
        l_igst = c_zero.
      ENDIF.
************** State GST Calculation ***********************************************************
      READ TABLE lt_gst INTO DATA(ls_gst1) WITH KEY kschl = 'JICG'
                                                    mwsk1 = <fs_vbrp>-mwskz BINARY SEARCH.
      IF sy-subrc EQ 0.
        DATA(l_ctax) = ( ls_gst1-kbetr / 10 ).
        l_cgst       = ( <fs_vbrp>-netwr * l_ctax ) / 100.
      ELSE.
        l_ctax = c_zero.
        l_cgst = c_zero.
      ENDIF.
***************** Central GST Calculation ********************************************************
      READ TABLE lt_gst INTO DATA(ls_gst2) WITH KEY kschl = 'JISG'
                                           mwsk1 = <fs_vbrp>-mwskz BINARY SEARCH.
      IF sy-subrc EQ 0.
        DATA(l_stax) = ( ls_gst1-kbetr / 10 ).
        l_sgst       = ( <fs_vbrp>-netwr * l_stax ) / 100.
      ELSE.
        l_stax = c_zero.
        l_sgst = c_zero.
      ENDIF.


      DATA(l_total_gst) = CONV string( l_itax + l_ctax + l_stax ).
      CONDENSE l_total_gst.
      DATA(l_totalitem_amt) = CONV string( l_assamt + l_igst + l_cgst + l_sgst   ).
      CONDENSE l_totalitem_amt.

      APPEND VALUE #( slno       = <fs_vbrp>-ebelp
                      prddesc    = <fs_vbrp>-maktx
                      isservc    = 'N'
                      hsncd      = lv_hsncd
                      qty        = <fs_vbrp>-menge
                      freeqty    = c_zero
                      unit       = l_meins
                      unitprice  = l_uprice
                      totamt     = l_amount
                      assamt     = l_assamt
                      gstrt      = l_total_gst
                      igstamt    = l_igst
                      cgstamt    = l_cgst
                      sgstamt    = l_sgst
                      othchrg    = c_zero
                      totitemval = l_totalitem_amt
                    ) TO data_tab-itemlist.
      CLEAR: l_cgst,l_sgst,l_igst,l_itax,l_ctax,l_stax,ls_gst2,ls_gst1,ls_gst,
             l_uprice,l_amount,l_assamt,l_total_gst,l_totalitem_amt.
    ENDLOOP.
*---- Invoice Total Value Population ----*\
    IF data_tab-itemlist IS NOT INITIAL.
      LOOP AT data_tab-itemlist INTO DATA(lw_itemlist).
****assessable Value, Total Value & igst Value
        v_assval = v_assval + lw_itemlist-assamt.
        v_amtval = v_amtval + lw_itemlist-totamt.
        v_tcs    = v_tcs    + lw_itemlist-othchrg.
        v_igst   = v_igst   + lw_itemlist-igstamt.
        v_cgst   = v_cgst   + lw_itemlist-cgstamt.
        v_sgst   = v_sgst   + lw_itemlist-sgstamt.
      ENDLOOP.
      data_tab-valdtls-assval      = v_assval.
      data_tab-valdtls-cgstval     = v_cgst.
      data_tab-valdtls-sgstval     = v_sgst.
      data_tab-valdtls-igstval     = v_igst.
      data_tab-valdtls-cesval      = c_zero.
      data_tab-valdtls-stcesval    = c_zero.
      data_tab-valdtls-stcesval    = c_zero.
      data_tab-valdtls-othchrg     = c_zero.
      data_tab-valdtls-totinvval   = v_amtval + v_cgst + v_sgst + v_igst.
      data_tab-valdtls-totinvvalfc = c_zero.
*************rounding off process*************
      CLEAR : lv_roundtot.
      lv_roundtot                  = data_tab-valdtls-totinvval.
      data_tab-valdtls-rndoffamt   = lv_roundtot - data_tab-valdtls-totinvval.

      CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
        CHANGING
          value = data_tab-valdtls-rndoffamt.

      data_tab-valdtls-totinvval   = lv_roundtot.
    ENDIF.
*    ENDIF.
  ENDMETHOD.


  METHOD generate_pdf.
*    DATA(l_set_print) = VALUE lbbil_print_data_to_read(  hd_gen = abap_true
*                                                            hd_adr = abap_true
*                                                            hd_gen_descript = abap_true
*                                                            hd_org = abap_true
*                                                            hd_part_add = abap_true
*                                                            hd_kond = abap_true
*                                                            hd_fin = abap_true
*                                                            hd_ref = abap_true
*                                                            hd_tech = abap_true
*                                                            it_gen = abap_true
*                                                            it_adr = abap_true
*                                                            it_price = abap_true
*                                                            it_kond = abap_true
*                                                            it_ref = abap_true
*                                                            it_refdlv = abap_true
*                                                            it_reford = abap_true
*                                                            it_refpurord = abap_true
*                                                            it_refvag = abap_true
*                                                            it_refvg2 = abap_true
*                                                            it_refvkt = abap_true
*                                                            it_tech = abap_true
*                                                            it_fin = abap_true
*                                                            it_confitm = abap_true
*                                                            it_confbatch = abap_true
*                                                            msr_hd = abap_true
*                                                            msr_it = abap_true ).
*    DATA(lv_objkey) = CONV nast-objky( vbeln ).
*    DATA ls_bil_invoice    TYPE lbbil_invoice.
    DATA ls_docpara TYPE sfpdocparams.
    DATA ls_outpara TYPE sfpoutputparams.
    DATA ls_output  TYPE fpformoutput.
    DATA ls_frmname TYPE fpname.
    DATA lv_fm    TYPE rs38l_fnam.
*    DATA: lv_fm    TYPE rs38l_fnam,
*          i_objbin TYPE STANDARD TABLE OF solix,
*          l_app1   TYPE string.
*    CALL FUNCTION 'LB_BIL_INV_OUTP_READ_PRTDATA'
*      EXPORTING
*        if_bil_number         = lv_objkey
*        is_print_data_to_read = l_set_print
*        if_parvw              = 'RE'
*        if_parnr              = customer
*        if_language           = sy-langu
*      IMPORTING
*        es_bil_invoice        = ls_bil_invoice
*      EXCEPTIONS
*        records_not_found     = 1
*        records_not_requested = 2
*        OTHERS                = 3.
*    IF sy-subrc = 0.

*    ls_outpara-preview = abap_true.
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

    ls_frmname = 'ZSD_IRN_QR_EINVOICE_RETURN'.

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
        is_bil_invoice     = inv_details
        inv_no             = inv_no
        inv_date           = inv_date
        inv_details        = inv_details
        irn                = irn
        signed_inv         = signed_inv
        signed_qrcode      = signed_qrcode
        po_number          = po_number
        seller_invoiceno   = seller_invoiceno
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

*      DATA(lv_pdf) = CONV xstring( ls_output-pdf ).
    pdf = ls_output-pdf.
*    ENDIF.
*        return = | Saral Token Error: { error_msg } |.
  ENDMETHOD.


  METHOD get_irn_token.
    "API response Structure
    DATA: ls_response TYPE zeinv_acc_token_res_st.
    DATA: lo_http_client TYPE REF TO if_http_client.

    IF distributor_code IS NOT INITIAL.
*------ Distributor Token Details ----*
      DATA(lv_kunnr) = CONV kunnr( |{ distributor_code ALPHA = IN }| ).
      "Checks for Distributor Code
      SELECT SINGLE stcd3 FROM kna1
        INTO @DATA(l_gstin)
        WHERE kunnr = @lv_kunnr.
      IF sy-subrc = 0.
        "Credentials for Distributor for E-invoice Process
        SELECT SINGLE * FROM zdist_einv_dtls
          INTO @DATA(l_dist_dtls)
          WHERE distributor = @lv_kunnr.
        IF sy-subrc = 0.
*          "Chec
*          SELECT SINGLE * FROM zinv_acc_token
*            INTO @DATA(l_token_dtls)
*            WHERE distributor = @lv_kunnr.
*          IF sy-subrc = 0.
          "Checking the Validity of Tokens
*            IF l_token_dtls-valid_date EQ sy-datum AND l_token_dtls-valid_time GT sy-uzeit.
*              auth_token = l_token_dtls-auth_token.
*              session_key = l_token_dtls-ses_key.
          user_name = l_dist_dtls-uname.

          gstin = l_dist_dtls-gstin = l_gstin.
          IF sy-sysid NE 'PRD'.
            gstin = l_dist_dtls-gstin = '29AABCR7796N000'.
          ENDIF.
*            ELSE.
*---- URL fixing for system based ----*
          CASE sy-sysid.
            WHEN 'DEV' OR 'QAS'.
              DATA(l_api_url) = 'ACTUAL_EINV_TOKEN_DEMO'.
            WHEN 'PRD'.
              l_api_url = 'ACTUAL_EINV_TOKEN_PRD'.
          ENDCASE.
          SELECT SINGLE low FROM tvarvc
            INTO @DATA(l_url)
            WHERE name = @l_api_url AND
                  type = 'P'.
          IF sy-subrc = 0.
            DATA(create_url) = CONV string( l_url ).
            cl_http_client=>create_by_url(
                EXPORTING
                url = create_url
                IMPORTING
                client = lo_http_client
                EXCEPTIONS
                argument_not_found = 1
                plugin_not_active = 2
                internal_error = 3
                OTHERS = 4 ).

            CHECK lo_http_client IS BOUND.
            lo_http_client->propertytype_logon_popup = if_http_client=>co_disabled.

            lo_http_client->request->set_method(
             EXPORTING
             method = if_http_entity=>co_request_method_post ).

            lo_http_client->request->set_content_type(
             EXPORTING
             content_type = if_rest_media_type=>gc_appl_json ).

*              DATA(l_sauth_token) = l_token_dtls-sauth_token.
*              l_sauth_token = CONV string( l_sauth_token ).
*              CONDENSE l_sauth_token NO-GAPS.

            "Header Data Fields for API Fixing
            lo_http_client->request->set_header_field( EXPORTING name  = 'AuthenticationToken' value =  authentication_token ).

            lo_http_client->request->set_header_field( EXPORTING name  = 'SubscriptionId'      value =  CONV string( subscription_id ) ).

* Hard code the UserName and Password for Dev and QAS to Relyon UAT system details
* this is to avoid by mistake using the production one incase of any client refresh from PRD to QAS and DEV

            IF sy-sysid = 'DEV' or sy-sysid = 'QAS'.
              clear: l_dist_dtls-uname, l_dist_dtls-password.
              l_dist_dtls-uname    =  'eRelyon'.
              l_dist_dtls-password =  'Saral@123'.
            endif.

            lo_http_client->request->set_header_field( EXPORTING name  = 'UserName'            value =  CONV string( l_dist_dtls-uname ) ).

            lo_http_client->request->set_header_field( EXPORTING name  = 'password'            value =  CONV string( l_dist_dtls-password ) ).

            lo_http_client->request->set_header_field( EXPORTING name  = 'Gstin'               value =  CONV string( l_dist_dtls-gstin )  ).

            lo_http_client->request->set_cdata(
               EXPORTING
               data = '{ JSON_Payload }' ).

            lo_http_client->send(
             EXCEPTIONS
             http_communication_failure = 1
             http_invalid_state = 2 ).

            CHECK sy-subrc = 0.
            lo_http_client->receive(
             EXCEPTIONS
             http_communication_failure = 1
             http_invalid_state = 2
             http_processing_failed = 3 ).

            lo_http_client->response->get_status(
            IMPORTING
              code = DATA(lv_codes) ).

            lo_http_client->response->get_status(
            IMPORTING
              reason = DATA(lv_http_error) ).
            "Actual API Response If success
            IF lv_codes = 200.
              DATA(lv_response) = lo_http_client->response->get_cdata( ).
** deserialize the INPUT our required INPUT ***
              /ui2/cl_json=>deserialize(
              EXPORTING
               json         = lv_response
               pretty_name  = /ui2/cl_json=>pretty_mode-user
              CHANGING
               data         = ls_response ).
              IF ls_response IS NOT INITIAL.
                "Response of API
                IF ls_response-status = 1.
                  auth_token = ls_response-data-authtoken. "Government Authentication Token
                  session_key = ls_response-data-sek. "Session Keys
                  user_name = l_dist_dtls-uname.
                  gstin = l_dist_dtls-gstin.

                  SPLIT ls_response-data-tokenexpiry AT ' ' INTO DATA(lv_date) DATA(lv_time).
                  REPLACE ALL OCCURRENCES OF '-' IN lv_date WITH ''.

                  "Time Conversion to Internal
                  DATA(l_time) = lv_time.
                  CLEAR lv_time.
                  CALL FUNCTION 'CONVERSION_EXIT_TIMTV_INPUT'
                    EXPORTING
                      input       = l_time
                    IMPORTING
                      output      = lv_time
                    EXCEPTIONS
                      wrong_input = 1
                      OTHERS      = 2.

**---- Updating the Log table with the Token ------*
*                    l_token_dtls-auth_token = auth_token.
*                    l_token_dtls-ses_key = session_key.
*                    l_token_dtls-valid_date = lv_date.
*                    l_token_dtls-valid_time = lv_time.
*                    MODIFY zinv_acc_token FROM l_token_dtls.
                ELSE.
                  err_msg = lv_response.
                ENDIF.
              ENDIF.
            ELSE.
              DATA(lv_errresp) = lo_http_client->response->get_cdata( ).
              err_msg = lv_errresp.
            ENDIF.
            lo_http_client->close( ).
          ENDIF.
*            ENDIF.
*          ELSE.
*            err_msg = |Distributor Token Details Missing|.
*          ENDIF.
        ELSE.
          err_msg = |No Distributor Data available in table|.
        ENDIF.
      ENDIF.
    ELSE.
      err_msg = |Incorrect Distributor Code|.
    ENDIF.

    DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
    DATA: lv_data TYPE string,
          lv_body TYPE string.
    CREATE OBJECT lo_log_upd.

    lv_data = |"DISTRIBUTOR_CODE":"{ distributor_code }","AUTHENTICATION_TOKEN":"{ authentication_token }","SUBSCRIPTION_ID":"{ subscription_id }|.
    CONCATENATE '{' lv_data '}' INTO lv_data.
    IF err_msg IS NOT INITIAL.
      lv_body = |"ERR_MSG":"{ err_msg }"|.
      CONCATENATE '[{' lv_body '}]' INTO lv_body.
    ELSE.
      lv_body = |"AUTH_TOKEN":"{ auth_token }","SESSION_KEY":"{ session_key }"|.
      CONCATENATE '[{' lv_body '}]' INTO lv_body.
    ENDIF.

*Output Entry in Log Table
    CALL METHOD lo_log_upd->log_entry_store
      EXPORTING
        apiname         = 'SARALAPI_AUTHTOKENSESKEY'
        ijson           = lv_data
        ojson           = lv_body
        distributor     = distributor_code
        retailer        = customer
      EXCEPTIONS
        apiname_missing = 1
        json_missing    = 2
        OTHERS          = 3.
  ENDMETHOD.


  METHOD get_saral_token.

    DATA: lr_data TYPE REF TO data.
*    DATA: ls_fields TYPE zinv_acc_token.
    DATA: ls_fields TYPE zdms_saral_token.
    DATA: lo_http_client TYPE REF TO if_http_client.
    DATA: lv_actual_time TYPE tims,
          lv_session(02) TYPE c.
    FIELD-SYMBOLS:
      <data>        TYPE data,
      <results>     TYPE any,
      <structure>   TYPE any,
      <table>       TYPE ANY TABLE,
      <field>       TYPE any,
      <field_value> TYPE data.

    IF distributor_code IS NOT INITIAL.
      DATA(lv_kunnr) = CONV kunnr( |{ distributor_code ALPHA = IN }| ).
      SELECT SINGLE kunnr FROM kna1 INTO @DATA(lv_customer) WHERE kunnr = @lv_kunnr.
      IF sy-subrc = 0.
        SELECT SINGLE * FROM zdist_einv_dtls INTO @DATA(ls_dist_dtls) WHERE distributor = @lv_kunnr.
        IF sy-subrc = 0.
**--- Checking the authentication token table in SAP -------*
*          SELECT SINGLE * FROM zinv_acc_token INTO @DATA(ls_auth_token) WHERE distributor = @lv_kunnr.
          SELECT SINGLE * FROM zdms_saral_token INTO @DATA(ls_auth_token) WHERE bukrs = 'DMS1'.
          IF sy-subrc = 0.
            DATA(lv_datum) = sy-datum.
            DATA(lv_time) = sy-uzeit.

            CASE sy-sysid.
              WHEN 'DEV' OR 'QAS'.
                CLEAR: lv_actual_time,lv_session.
                CALL FUNCTION 'HRVE_CONVERT_TIME'
                  EXPORTING
                    type_time       = 'A'
                    input_time      = lv_time
                  IMPORTING
                    output_time     = lv_actual_time
                    output_am_pm    = lv_session
                  EXCEPTIONS
                    parameter_error = 1
                    OTHERS          = 2.
                IF sy-subrc <> 0.

                ENDIF.
              WHEN 'PRD'.
                lv_actual_time = lv_time.
            ENDCASE.
*            IF lv_datum LE ls_auth_token-date_in OR lv_actual_time LE ls_auth_token-time.
            IF  lv_datum LE ls_auth_token-valid_date AND
                lv_actual_time LE ls_auth_token-valid_time.
              authentication_token = ls_auth_token-sauth_token. "Authentication Token
              subscription_id = ls_auth_token-sub_id. "Subscription ID
            ENDIF.
          ENDIF.
        ELSE.
          error_msg = |No Distributor Data available|.
        ENDIF.
      ELSE.
        error_msg = |Distributor Number Incoorect|.
      ENDIF.
    ENDIF.

*--- If needs to generate the Token Means -----*
    IF authentication_token IS INITIAL AND subscription_id IS INITIAL AND error_msg IS INITIAL.
*---- Generating the SARAL Authentication Token --------*
      CASE sy-sysid.
        WHEN 'DEV' OR 'QAS'.
          DATA(lv_url_name) = 'SARAL_AUTH_TOKEN_DEMO'.
        WHEN 'PRD'.
          lv_url_name = 'SARAL_AUTH_TOKEN_PRD'.
      ENDCASE.
      SELECT SINGLE low FROM tvarvc INTO @DATA(l_saral_url) WHERE name = @lv_url_name
                                                            AND type = 'P'.
      IF sy-subrc = 0.
        DATA(create_url) = CONV string( l_saral_url ).
        cl_http_client=>create_by_url(
            EXPORTING
            url = create_url
            IMPORTING
            client = lo_http_client
            EXCEPTIONS
            argument_not_found = 1
            plugin_not_active = 2
            internal_error = 3
            OTHERS = 4 ).

        CHECK lo_http_client IS BOUND.
        lo_http_client->propertytype_logon_popup = if_http_client=>co_disabled.


        lo_http_client->request->set_method(
         EXPORTING
         method = if_http_entity=>co_request_method_get ).


        lo_http_client->request->set_content_type(
         EXPORTING
         content_type = if_rest_media_type=>gc_appl_json ).

* Hard code the Client id and Client secret for Dev and QAS to Relyon UAT system
* this is to avoid by mistake using the production one

        IF sy-sysid = 'DEV' or sy-sysid = 'QAS'.
          CLEAR: ls_dist_dtls-client_id, ls_dist_dtls-client_secret.

          ls_dist_dtls-client_id     = '43cc5941-2be5-4e2f-9a09-2536f0e29fbf'.
          ls_dist_dtls-client_secret = 'Fsx2IecwjFtWEsk0fxqEKs18jMjCN62v'.
        ENDIF.

        "Header Data Fields for API Fixing
        lo_http_client->request->set_header_field( EXPORTING name  = 'ClientId' value = CONV string( ls_dist_dtls-client_id ) ).

        lo_http_client->request->set_header_field( EXPORTING name = 'ClientSecret' value = CONV string( ls_dist_dtls-client_secret ) ).

        lo_http_client->request->set_cdata(
           EXPORTING
           data = '{ JSON_Payload }' ).


        lo_http_client->send(
         EXCEPTIONS
         http_communication_failure = 1
         http_invalid_state = 2 ).


        CHECK sy-subrc = 0.
        lo_http_client->receive(
         EXCEPTIONS
         http_communication_failure = 1
         http_invalid_state = 2
         http_processing_failed = 3 ).


        lo_http_client->response->get_status(
        IMPORTING
          code = DATA(lv_codes) ).

        lo_http_client->response->get_status(
        IMPORTING
          reason = DATA(lv_http_error) ).
        "Actual API Response If success
        IF lv_codes = 200.
          DATA(lv_response) = lo_http_client->response->get_cdata( ).
        ENDIF.
        IF lv_response IS NOT INITIAL.
          CALL METHOD /ui2/cl_json=>deserialize
            EXPORTING
              json         = lv_response
              pretty_name  = /ui2/cl_json=>pretty_mode-user
              assoc_arrays = abap_true
            CHANGING
              data         = lr_data.
          IF lr_data IS BOUND.
            ASSIGN lr_data->* TO <data>.
            ASSIGN COMPONENT 'AUTHENTICATIONTOKEN' OF STRUCTURE <data> TO <field>.
            IF <field> IS ASSIGNED.
              lr_data = <field>.
              ASSIGN lr_data->* TO <field_value>.
              ls_fields-sauth_token = <field_value>.
            ENDIF.
            UNASSIGN: <field>,<field_value>.

            ASSIGN COMPONENT 'AUTHENTICATIONVALIDTILLDATETIM' OF STRUCTURE <data> TO <field>.
            IF <field> IS ASSIGNED.
              lr_data = <field>.
              ASSIGN lr_data->* TO <field_value>.
              DATA(lv_date_time) = CONV string( <field_value> ).
              SPLIT lv_date_time AT ' ' INTO DATA(str1) DATA(str2).
              IF sy-sysid NE 'PRD'.
                SPLIT str1 AT '/' INTO DATA(lv_month) DATA(lv_days).
                SPLIT lv_days AT '/' INTO lv_days DATA(lv_year).
                REPLACE ALL OCCURRENCES OF 'AM' IN str2 WITH ''.
                REPLACE ALL OCCURRENCES OF 'PM' IN str2 WITH ''.
                DATA(lv_date) =   |{ lv_days }.{ lv_month }.{ lv_year }| .
              ELSE.
                lv_date =   |{ str1(2) }.{ str1+3(2) }.{ str1+6(4) }| .
              ENDIF.
              CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
                EXPORTING
                  date_external            = lv_date
                IMPORTING
                  date_internal            = ls_fields-valid_date
                EXCEPTIONS
                  date_external_is_invalid = 1
                  OTHERS                   = 2.
              IF sy-subrc <> 0.

              ENDIF.
              DATA(l_time) = str2.
              CALL FUNCTION 'CONVERSION_EXIT_TIMTV_INPUT'
                EXPORTING
                  input       = l_time
                IMPORTING
                  output      = ls_fields-valid_time
                EXCEPTIONS
                  wrong_input = 1
                  OTHERS      = 2.
              IF sy-subrc <> 0.

              ENDIF.
            ENDIF.
            UNASSIGN: <field_value>,<field_value>.

            ASSIGN COMPONENT 'SUBSCRIPTIONID' OF STRUCTURE <data> TO <field>.
            IF <field> IS ASSIGNED.
              lr_data = <field>.
              ASSIGN lr_data->* TO <field_value>.
              ls_fields-sub_id = <field_value>.
            ENDIF.
            UNASSIGN: <field>,<field_value>.

*          MODIFY zinv_acc_token FROM ls_fields.
            ls_fields-bukrs = 'DMS1'.
            MODIFY zdms_saral_token FROM ls_fields.
            authentication_token = ls_fields-sauth_token. "Authentication Token
            subscription_id = ls_fields-sub_id. "Subscription ID
          ENDIF.
        ELSE.
          error_code = lv_codes. "Error Codes
          DATA(lv_errresponse) = lo_http_client->response->get_cdata( ).
          error_msg = lv_errresponse. "Error Messages
        ENDIF.

        lo_http_client->close( ).
      ENDIF.
    ENDIF.

    DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
    DATA: lv_data TYPE string,
          lv_body TYPE string.
    CREATE OBJECT lo_log_upd.

    lv_data = |"DISTRIBUTOR_CODE":"{ distributor_code }","ClientId":"{ ls_dist_dtls-client_id }","ClientSecret":"{ ls_dist_dtls-client_secret }|.
    CONCATENATE '{' lv_data '}' INTO lv_data.
    IF authentication_token IS INITIAL.
      lv_body = |"ERROR_CODE":"{ error_code }","ERROR_MSG":"{ error_msg }"|.
      CONCATENATE '[{' lv_body '}]' INTO lv_body.
    ELSE.
      lv_body = |"AUTHENTICATION_TOKEN":"{ authentication_token }","SUBSCRIPTION_ID":"{ ls_dist_dtls-client_id }","ClientSecret":"{ ls_dist_dtls-client_secret }|.
      CONCATENATE '[{' lv_body '}]' INTO lv_body.
    ENDIF.

*Output Entry in Log Table
    CALL METHOD lo_log_upd->log_entry_store
      EXPORTING
        apiname         = 'SARALAPI_AUTH_TOKEN'
        ijson           = lv_data
        ojson           = lv_body
        distributor     = distributor_code
        retailer        = customer
      EXCEPTIONS
        apiname_missing = 1
        json_missing    = 2
        OTHERS          = 3.
  ENDMETHOD.


  METHOD prepare_json_payload.
    DATA lv_trans TYPE string.
    DATA lv_docdtls TYPE string.
    DATA lv_selldtls TYPE string.
    DATA lv_buydtls TYPE string.
    DATA lv_itmdtls TYPE string.
    DATA lv_valdtls TYPE string.
    DATA lv_refdtls TYPE string.
    DATA lv_ewbdtls TYPE string.

    IF data_tab IS NOT INITIAL.

*data_tab-DISTRIBUTORirn

      DATA(lw_body) = data_tab-body.
      DATA(lw_trandtls) = lw_body-trandtls.
      CONCATENATE
       '"Irn": "",'
       '"Version": "' lw_body-version '",'
       '"TranDtls": {'
       '"TaxSch": "' lw_trandtls-taxsch '",'
       '"SupTyp": "' lw_trandtls-suptyp '",'
       '"RegRev": "' lw_trandtls-regrev '"'
       '},'  INTO lv_trans.

      DATA(lw_docdtls) = data_tab-docdtls.
      CONCATENATE
       '"DocDtls": {'
       '"Typ": "' lw_docdtls-typ '",'
       '"No": "' lw_docdtls-no '",'
       '"Dt": "' lw_docdtls-dt '"'
       '},'  INTO lv_docdtls.

      DATA(lw_sellerdtls) = data_tab-sellerdtls.
      IF lw_sellerdtls-pin IS INITIAL.
        lw_sellerdtls-pin = '0'.
      ENDIF.
      CONCATENATE
       '"SellerDtls": {'
       '"Gstin": "' lw_sellerdtls-gstin '",'
       '"LglNm": "' lw_sellerdtls-lglnm '",'
       '"TrdNm": "' lw_sellerdtls-trdnm '",'
       '"Addr1": "' lw_sellerdtls-addr1 '",'
       '"Loc": "' lw_sellerdtls-loc '",'
       '"Pin": ' lw_sellerdtls-pin ','
       '"Stcd": "' lw_sellerdtls-stcd '",'
       '"Ph": "' lw_sellerdtls-ph '"'
       '},'  INTO lv_selldtls.
*SELLERDTLS
*    GSTIN
*    LGLNM
*    TRDNM
*    ADDR1
*    LOC
*    PIN
*    STCD
*    PH
*    EM
      DATA(lw_buyerdtls) = data_tab-buyerdtls.
      IF lw_buyerdtls-pin IS INITIAL.
        lw_buyerdtls-pin = '0'.
      ENDIF.
      CONCATENATE
       '"BuyerDtls": {'
       '"Gstin": "' lw_buyerdtls-gstin '",'
       '"LglNm": "' lw_buyerdtls-lglnm '",'
       '"TrdNm": "' lw_buyerdtls-trdnm '",'
       '"Pos": "' lw_buyerdtls-pos '",'
       '"Addr1": "' lw_buyerdtls-addr1 '",'
       '"Addr2": "' lw_buyerdtls-addr2 '",'
       '"Loc": "' lw_buyerdtls-loc '",'
       '"Pin": ' lw_buyerdtls-pin ','
       '"Stcd": "' lw_buyerdtls-stcd '"'
       '},'  INTO lv_buydtls.

      DATA(lt_itemlist) = data_tab-itemlist.
      LOOP AT lt_itemlist INTO DATA(lw_items).
        IF lw_items-qty IS INITIAL. lw_items-qty = '0'. ENDIF.
        IF lw_items-freeqty IS INITIAL. lw_items-freeqty = '0'. ENDIF.
        IF lw_items-totamt IS INITIAL. lw_items-totamt = '0'. ENDIF.
        IF lw_items-discount IS INITIAL. lw_items-discount = '0'. ENDIF.
        IF lw_items-pretaxval IS INITIAL. lw_items-pretaxval = '0'. ENDIF.
        IF lw_items-assamt IS INITIAL. lw_items-assamt = '0'. ENDIF.
        IF lw_items-gstrt IS INITIAL. lw_items-gstrt = '0'. ENDIF.
        IF lw_items-igstamt IS INITIAL. lw_items-igstamt = '0'. ENDIF.
        IF lw_items-cgstamt IS INITIAL. lw_items-cgstamt = '0'. ENDIF.
        IF lw_items-sgstamt IS INITIAL. lw_items-sgstamt = '0'. ENDIF.
        IF lw_items-cesamt IS INITIAL. lw_items-cesamt = '0'. ENDIF.
        IF lw_items-cesrt IS INITIAL. lw_items-cesrt = '0'. ENDIF.
        IF lw_items-cesnonadvlamt IS INITIAL. lw_items-cesnonadvlamt = '0'. ENDIF.
        IF lw_items-statecesrt IS INITIAL. lw_items-statecesrt = '0'. ENDIF.
        IF lw_items-statecesamt IS INITIAL. lw_items-statecesamt = '0'. ENDIF.
        IF lw_items-statecesnonadvlamt IS INITIAL. lw_items-statecesnonadvlamt = '0'. ENDIF.
        IF lw_items-othchrg IS INITIAL. lw_items-othchrg = '0'. ENDIF.
        IF lw_items-totitemval IS INITIAL. lw_items-totitemval = '0'. ENDIF.
*        lw_items-cgstamt = lw_items-totitemval * '0.09'.
*        lw_items-sgstamt = lw_items-totitemval * '0.09'.
*        lw_items-igstamt = '0'.
        IF lv_itmdtls IS INITIAL.
          CONCATENATE
           '{'
           '"SlNo": "' lw_items-slno '",'
           '"PrdDesc": "' lw_items-prddesc '",'
           '"IsServc": "' lw_items-isservc '",'
           '"HsnCd": "' lw_items-hsncd '",'
           '"Qty": ' lw_items-qty ','
           '"FreeQty": ' lw_items-freeqty ','
           '"Unit": "' lw_items-unit '",'
           '"UnitPrice": ' lw_items-unitprice ','
           '"TotAmt": ' lw_items-totamt ','
           '"Discount": ' lw_items-discount ','
           '"PreTaxVal": ' lw_items-pretaxval ','
           '"AssAmt": ' lw_items-assamt ','
           '"GstRt": ' lw_items-gstrt ','
           '"IgstAmt": ' lw_items-igstamt ','
           '"CgstAmt": ' lw_items-cgstamt ','
           '"SgstAmt": ' lw_items-sgstamt ','
           '"CesAmt": ' lw_items-cesamt ','
           '"CesRt": ' lw_items-cesrt ','
           '"CesNonAdvlAmt": ' lw_items-cesnonadvlamt ','
           '"StateCesRt": ' lw_items-statecesrt ','
           '"StateCesAmt": ' lw_items-statecesamt ','
           '"StateCesNonAdvlAmt":' lw_items-statecesnonadvlamt ','
           '"OthChrg": ' lw_items-othchrg ','
           '"TotItemVal": ' lw_items-totitemval ''
           '}'  INTO lv_itmdtls.
        ELSE.
          CONCATENATE lv_itmdtls ','
           '{'
           '"SlNo": "' lw_items-slno '",'
           '"PrdDesc": "' lw_items-prddesc '",'
           '"IsServc": "' lw_items-isservc '",'
           '"HsnCd": "' lw_items-hsncd '",'
           '"Qty": ' lw_items-qty ','
           '"FreeQty": ' lw_items-freeqty ','
           '"Unit": "' lw_items-unit '",'
           '"UnitPrice": ' lw_items-unitprice ','
           '"TotAmt": ' lw_items-totamt ','
           '"Discount": ' lw_items-discount ','
           '"PreTaxVal": ' lw_items-pretaxval ','
           '"AssAmt": ' lw_items-assamt ','
           '"GstRt": ' lw_items-gstrt ','
           '"IgstAmt": ' lw_items-igstamt ','
           '"CgstAmt": ' lw_items-cgstamt ','
           '"SgstAmt": ' lw_items-sgstamt ','
           '"CesAmt": ' lw_items-cesamt ','
           '"CesRt": ' lw_items-cesrt ','
           '"CesNonAdvlAmt": ' lw_items-cesnonadvlamt ','
           '"StateCesRt": ' lw_items-statecesrt ','
           '"StateCesAmt": ' lw_items-statecesamt ','
           '"StateCesNonAdvlAmt":' lw_items-statecesnonadvlamt ','
           '"OthChrg": ' lw_items-othchrg ','
           '"TotItemVal": ' lw_items-totitemval ''
           '}'  INTO lv_itmdtls.
        ENDIF.
      ENDLOOP.

      DATA(lw_valdtls) = data_tab-valdtls.

      IF lw_valdtls-assval IS INITIAL. lw_valdtls-assval = '0'. ENDIF.
      IF lw_valdtls-cgstval IS INITIAL. lw_valdtls-cgstval = '0'. ENDIF.
      IF lw_valdtls-sgstval IS INITIAL. lw_valdtls-sgstval = '0'. ENDIF.
      IF lw_valdtls-igstval IS INITIAL. lw_valdtls-igstval = '0'. ENDIF.
      IF lw_valdtls-cesval IS INITIAL. lw_valdtls-cesval = '0'. ENDIF.
      IF lw_valdtls-stcesval IS INITIAL. lw_valdtls-stcesval = '0'. ENDIF.
      IF lw_valdtls-discount IS INITIAL. lw_valdtls-discount = '0'. ENDIF.
      IF lw_valdtls-othchrg IS INITIAL. lw_valdtls-othchrg = '0'. ENDIF.
      IF lw_valdtls-rndoffamt IS INITIAL. lw_valdtls-rndoffamt = '0'. ENDIF.
      IF lw_valdtls-totinvval IS INITIAL. lw_valdtls-totinvval = '0'. ENDIF.
      IF lw_valdtls-totinvvalfc IS INITIAL. lw_valdtls-totinvvalfc = '0'. ENDIF.

      CONCATENATE
       '"ValDtls": {'
       '"AssVal": ' lw_valdtls-assval ','
       '"CgstVal": ' lw_valdtls-cgstval ','
       '"SgstVal": ' lw_valdtls-sgstval ','
       '"IgstVal": ' lw_valdtls-igstval ','
       '"CesVal": ' lw_valdtls-cesval ','
       '"StCesVal": ' lw_valdtls-stcesval ','
       '"Discount": ' lw_valdtls-discount ','
       '"OthChrg": ' lw_valdtls-othchrg ','
       '"RndOffAmt": ' lw_valdtls-rndoffamt ','
       '"TotInvVal": ' lw_valdtls-totinvval ','
       '"TotInvValFc": ' lw_valdtls-totinvvalfc ''
       '}'  INTO lv_valdtls.

*      DATA(l_date) = |{ sy-datum+6(2) }/{ sy-datum+4(2) }/{ sy-datum(4) }|.
*      DATA(l_inv_date) = |{ invoicedt+6(2) }/{ invoicedt+4(2) }/{ invoicedt(4) }|.
*      CONCATENATE
*      '"RefDtls": {'
*          '"InvRm": "PrevDoc",'
*          '"DocPerdDtls": {'
*          '"InvStDt": "' l_inv_date '",'
*          '"InvEndDt": "' l_inv_date '"'
*          '},'
*          '"PrecDocDtls": ['
*            '{'
*              '"InvNo": "' invoiceno '",'
*              '"InvDt": "' l_date '",'
*              '"OthRefNo":"' seller_invoiceno '"'
*            '}'
*          ']'
*     '}' INTO lv_refdtls.

*      IF invoiceno = ''.
*        lv_refdtls = ''.
*      ELSE.
*        CONCATENATE lv_valdtls ',' INTO lv_valdtls.
*      ENDIF.

*      DATA(lw_ewbdtls) = data_tab-ewbdtls.
*      IF lw_ewbdtls-distance IS INITIAL.
*        lw_ewbdtls-distance = '0'.
*      ENDIF.
*      CONCATENATE
*        '"EwbDtls": {'
*        '"TransId": "' lw_ewbdtls-transid '",'
*        '"TransName": "' lw_ewbdtls-transname '",'
*        '"TransMode": "' lw_ewbdtls-transmode '",'
*        '"Distance": ' lw_ewbdtls-distance ','
*        '"TransDocNo": "' lw_ewbdtls-transdocno '",'
*        '"TransDocDt": "' lw_ewbdtls-transdocdt '",'
*        '"VehNo": "' lw_ewbdtls-vehno '",'
*        '"VehType": "' lw_ewbdtls-vehtype '"'
*        '}' INTO lv_ewbdtls.



*EWBDTLS

    ENDIF.
    CONCATENATE
     '{'
        lv_trans
        lv_docdtls
        lv_selldtls
        lv_buydtls
        '"ItemList": ['
         lv_itmdtls
        '],'
        lv_valdtls
*        lv_refdtls
        lv_ewbdtls
    '}' INTO payload_json.
  ENDMETHOD.


  METHOD get_gst_details.
*
    "API response Structure
    DATA: ls_response TYPE zgst_msg.
    DATA: lo_http_client TYPE REF TO if_http_client.

*---- URL fixing for system based ----*
    CASE sy-sysid.
      WHEN 'DEV' OR 'QAS'.
        DATA(l_api_url) = 'DMS_GST_TEST'.
      WHEN 'PRD'.
        l_api_url = 'DMS_GST_PRD'.
    ENDCASE.
    SELECT SINGLE low FROM tvarvc INTO @DATA(l_url)
      WHERE name = @l_api_url  AND type = 'P'.
    IF sy-subrc = 0.
      DATA(create_url) = |{ CONV string( l_url ) }{ customer_gstin }|.
      cl_http_client=>create_by_url(
          EXPORTING
          url = create_url
          IMPORTING
          client = lo_http_client
          EXCEPTIONS
          argument_not_found = 1
          plugin_not_active = 2
          internal_error = 3
          OTHERS = 4 ).

      CHECK lo_http_client IS BOUND.

      lo_http_client->propertytype_logon_popup = if_http_client=>co_disabled.

      lo_http_client->request->set_method(
       EXPORTING
       method = if_http_entity=>co_request_method_get ).


      lo_http_client->request->set_content_type(
       EXPORTING
       content_type = if_rest_media_type=>gc_appl_json ).

      "Header Data Fields for API Fixing
      lo_http_client->request->set_header_field( EXPORTING name  = 'AuthenticationToken' value =  CONV string( authentication_token ) ).

      lo_http_client->request->set_header_field( EXPORTING name  = 'SubscriptionId'      value =  CONV string( subscription_id ) ).

      lo_http_client->request->set_header_field( EXPORTING name  = 'Gstin'               value =  CONV string( gstin ) ).

      lo_http_client->request->set_header_field( EXPORTING name  = 'user_name'           value =  CONV string( user_name ) ).

      lo_http_client->request->set_header_field( EXPORTING name  = 'AuthToken'           value =  CONV string( auth_token ) ).

      lo_http_client->request->set_header_field( EXPORTING name  = 'sek'                 value =  CONV string( session_key )  ).

      lo_http_client->send(
       EXCEPTIONS
       http_communication_failure = 1
       http_invalid_state = 2 ).

      CHECK sy-subrc = 0.
      lo_http_client->receive(
       EXCEPTIONS
       http_communication_failure = 1
       http_invalid_state = 2
       http_processing_failed = 3 ).

      lo_http_client->response->get_status(
      IMPORTING
        code = DATA(lv_codes) ).

      lo_http_client->response->get_status(
      IMPORTING
        reason = DATA(lv_http_error) ).
      "Actual API Response If success
      IF lv_codes = 200.
        DATA(lv_response) = lo_http_client->response->get_cdata( ).
** deserialize the INPUT our required INPUT ***
        /ui2/cl_json=>deserialize(
        EXPORTING
         json         = lv_response
         pretty_name  = /ui2/cl_json=>pretty_mode-user
        CHANGING
         data         = ls_response ).
        IF ls_response-gstin IS INITIAL.
          DATA(message) = lv_response.
          RETURN = message.
        ELSE.
          zgst_msg = ls_response.
        ENDIF.
      ELSE.
        lv_response = lo_http_client->response->get_cdata( ).
        message = |Error Code: { lv_codes } - { lv_response }|.
        RETURN = message.
      ENDIF.
    ELSE.
      message = 'URL is not maintained in STVARV'.
      RETURN = message.
    ENDIF.

    DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
    DATA: lv_data TYPE string,
          lv_body TYPE string.
    CREATE OBJECT lo_log_upd.

    IF message IS NOT INITIAL.
      lv_body = |"message":"{ message }"|.
      CONCATENATE '[{' lv_body '}]' INTO lv_body.
    ELSE.
** serialize the output for response ***
      /ui2/cl_json=>serialize(
      EXPORTING
       data         =  ls_response
       pretty_name  = /ui2/cl_json=>pretty_mode-user
      RECEIVING
       r_json         = lv_body ).
    ENDIF.

*Output Entry in Log Table
  DATA ijson TYPE string.
    CONCATENATE '[{"URL":"' create_url '",'
                   '"DISTRIBUTOR_CODE":"'   DISTRIBUTOR_CODE '",'
                   '"COSTOMER_CODE":"'   COSTOMER_CODE '",'
                   '"AUTHENTICATION_TOKEN":"'   AUTHENTICATION_TOKEN '",'
                   '"SUBSCRIPTION_ID":"'   SUBSCRIPTION_ID '",'
                   '"AUTH_TOKEN":"'   AUTH_TOKEN '",'
                   '"SESSION_KEY":"'   SESSION_KEY '",'
                   '"USER_NAME":"'   USER_NAME '",'
                   '"GSTIN":"'   GSTIN '",'
                   '"CUSTOMER_GSTIN":"'   CUSTOMER_GSTIN '"}]' INTO ijson.
*    DATA(ijson) = |[URL { create_url }, COSTOMER_CODE { costomer_code }, CUSTOMER_GSTIN{ customer_gstin }|.

    CALL METHOD lo_log_upd->log_entry_store
      EXPORTING
        apiname         = 'SARALAPI_GST_DETAILS'
        ijson           = ijson
        ojson           = lv_body
        distributor     = distributor_code
        retailer        = costomer_code
      EXCEPTIONS
        apiname_missing = 1
        json_missing    = 2
        OTHERS          = 3.

  ENDMETHOD.


  METHOD get_gst_token.


    CALL METHOD me->get_saral_token
      EXPORTING
        distributor_code     = distributor_code
*        customer             = customer
      IMPORTING
        authentication_token = authentication_token
        subscription_id      = subscription_id
        error_code           = DATA(error_code)
        error_msg            = DATA(error_msg).
    IF error_msg IS INITIAL AND
       authentication_token IS NOT INITIAL AND
       subscription_id IS NOT INITIAL.
      CALL METHOD me->get_irn_token
        EXPORTING
          distributor_code     = distributor_code
*          customer             = customer
          authentication_token = authentication_token
          subscription_id      = subscription_id
        IMPORTING
          err_msg              = DATA(err_msg)
          auth_token           = auth_token
          session_key          = session_key
          user_name            = user_name
          gstin                = gstin.
      " Fill Data
      IF err_msg IS INITIAL AND
         auth_token IS NOT INITIAL AND
         session_key IS NOT INITIAL AND
         user_name IS NOT INITIAL AND
         gstin IS NOT INITIAL.
      ELSE.
        IF err_msg IS INITIAL.
          return = 'Authentication Token is not generated'.
        ELSE.
          return = | Authentication Token Error: { err_msg } |.
        ENDIF.
      ENDIF.
    ELSE.
      IF error_msg IS INITIAL.
        return = 'Saral Token is not generated'.
      ELSE.
        return = | Saral Token Error: { error_msg } |.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_irn_details.

*---- Billing Invoice Header Details-----*
*************************call saral api for get auth token & subscr id****************
    CALL METHOD me->get_saral_token
      EXPORTING
        distributor_code     = distributor_code
        customer             = retailer
      IMPORTING
        authentication_token = DATA(authentication_token)
        subscription_id      = DATA(subscription_id)
        error_code           = DATA(error_code)
        error_msg            = DATA(error_msg).
    IF error_msg IS INITIAL AND
       authentication_token IS NOT INITIAL AND
       subscription_id IS NOT INITIAL.
*************************call govt api for get session key & username***************
      CALL METHOD me->get_irn_token
        EXPORTING
          distributor_code     = distributor_code
          customer             = retailer
          authentication_token = authentication_token
          subscription_id      = subscription_id
        IMPORTING
          err_msg              = DATA(err_msg)
          auth_token           = DATA(auth_token)
          session_key          = DATA(session_key)
          user_name            = DATA(user_name)
          gstin                = DATA(gstin).
      " Fill Data
      IF err_msg IS INITIAL AND
         auth_token IS NOT INITIAL AND
         session_key IS NOT INITIAL AND
         user_name IS NOT INITIAL AND
         gstin IS NOT INITIAL.
      ELSE.
        IF err_msg IS INITIAL.
          message = 'Authentication Token is not generated'.
        ELSE.
          message = | Authentication Token Error: { err_msg } |.
        ENDIF.
        type = 'E'.
        EXIT.
      ENDIF.
    ELSE.
      IF error_msg IS INITIAL.
        message = 'Saral Token is not generated'.
      ELSE.
        message = | Saral Token Error: { error_msg } |.
      ENDIF.
      type = 'E'.
      EXIT.
    ENDIF.

********************get the irn details process**************

    DATA : lv_params TYPE string.
    DATA : lo_http_client TYPE REF TO if_http_client.

*---- URL fixing for system based ----*
    CASE sy-sysid.
      WHEN 'DEV' OR 'QAS'.
        DATA(l_api_url) = 'DMS_EINV_GET_IRN_TEST'.
      WHEN 'PRD'.
        l_api_url = 'DMS_EINV_GET_IRN_PRD'.
    ENDCASE.
    SELECT SINGLE low FROM tvarvc INTO @DATA(l_url) WHERE name = @l_api_url
                                                    AND type = 'P'.
    IF sy-subrc = 0.
******************concatenate the url and IRN as params***************
      lv_params = |{ l_url }{ irn }|.
      CONDENSE : lv_params.
      cl_http_client=>create_by_url(
          EXPORTING
          url = lv_params
          IMPORTING
          client = lo_http_client
          EXCEPTIONS
          argument_not_found = 1
          plugin_not_active = 2
          internal_error = 3
          OTHERS = 4 ).

      CHECK lo_http_client IS BOUND.

      lo_http_client->propertytype_logon_popup = if_http_client=>co_disabled.

      lo_http_client->request->set_method(
       EXPORTING
       method = if_http_entity=>co_request_method_get ).


      lo_http_client->request->set_content_type(
       EXPORTING
       content_type = if_rest_media_type=>gc_appl_json ).

      "Header Data Fields for API Fixing
      lo_http_client->request->set_header_field( EXPORTING name  = 'AuthenticationToken' value =  CONV string( authentication_token ) ).

      lo_http_client->request->set_header_field( EXPORTING name  = 'SubscriptionId'      value =  CONV string( subscription_id ) ).

      lo_http_client->request->set_header_field( EXPORTING name  = 'Gstin'               value =  CONV string( gstin ) ).

      lo_http_client->request->set_header_field( EXPORTING name  = 'UserName'            value =  CONV string( user_name ) ).

      lo_http_client->request->set_header_field( EXPORTING name  = 'AuthToken'           value =  CONV string( auth_token ) ).

      lo_http_client->request->set_header_field( EXPORTING name  = 'sek'                 value =  CONV string( session_key )  ).

      lo_http_client->send(
       EXCEPTIONS
       http_communication_failure = 1
       http_invalid_state = 2 ).

      CHECK sy-subrc = 0.
      lo_http_client->receive(
       EXCEPTIONS
       http_communication_failure = 1
       http_invalid_state = 2
       http_processing_failed = 3 ).

      lo_http_client->response->get_status(
      IMPORTING
        code = DATA(lv_codes) ).
      "Actual API Response
      lo_http_client->response->get_status(
      IMPORTING
        reason = DATA(lv_http_error) ).

      DATA(lv_response) = lo_http_client->response->get_cdata( ).
******************************after GET the DATA close the connection*******************
        lo_http_client->close( ).
      IF lv_codes = 200.
* deserialize the INPUT our required INPUT ***
        CLEAR : response.
        /ui2/cl_json=>deserialize(
        EXPORTING
         json         = lv_response
         pretty_name  = /ui2/cl_json=>pretty_mode-user
        CHANGING
         data         = response ).

        IF response-status IS NOT INITIAL AND response-ackno IS NOT INITIAL.
          type    = 'S'.
          message = 'IRN fetched successfully'.
        ELSE.
          type    = 'E'.
          message = |Error Code: { lv_codes } - { lv_response } - { lv_http_error }.IRN is not cancelled|.
        ENDIF.
      ELSE.
        message = |Error Code: { lv_codes } - { lv_response } - { lv_http_error }.IRN is not cancelled|.
        type    = 'E'.
      ENDIF.
    ELSE.
      message = 'URL is not maintained in STVARV'.
      type    = 'E'.
      EXIT.
    ENDIF.

    DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
    CREATE OBJECT lo_log_upd.

*Output Entry in Log Table
    CALL METHOD lo_log_upd->log_entry_store
      EXPORTING
        apiname         = 'SARALAPI_GET_IRN'
        ijson           = lv_params
        ojson           = lv_response
        distributor     = distributor_code
        retailer        = retailer
      EXCEPTIONS
        apiname_missing = 1
        json_missing    = 2
        OTHERS          = 3.

    CLEAR : lv_response.

  ENDMETHOD.


  METHOD generate_pdf_dms.
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
*    DATA(lv_objkey) = CONV nast-objky( vbeln ).
    DATA(lv_objkey) = CONV nast-objky( inv_no ).
    DATA ls_bil_invoice    TYPE lbbil_invoice.
    DATA ls_docpara TYPE sfpdocparams.
    DATA ls_outpara TYPE sfpoutputparams.
    DATA ls_output  TYPE fpformoutput.
    DATA ls_frmname TYPE fpname.
    DATA lv_fm    TYPE rs38l_fnam.
*    DATA: lv_fm    TYPE rs38l_fnam,
*          i_objbin TYPE STANDARD TABLE OF solix,
*          l_app1   TYPE string.
    CALL FUNCTION 'LB_BIL_INV_OUTP_READ_PRTDATA'
      EXPORTING
        if_bil_number         = lv_objkey
        is_print_data_to_read = l_set_print
        if_parvw              = 'RE'
        if_parnr              = customer
        if_language           = sy-langu
      IMPORTING
        es_bil_invoice        = ls_bil_invoice
      EXCEPTIONS
        records_not_found     = 1
        records_not_requested = 2
        OTHERS                = 3.

*    ls_outpara-preview = abap_true.
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

*    ls_frmname = 'ZSD_IRN_QR_EINVOICE_DMS'.
    ls_frmname = 'ZSD_INV_DMS_NEW'.

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

*      DATA(lv_pdf) = CONV xstring( ls_output-pdf ).
    pdf = ls_output-pdf.
*    ENDIF.
*        return = | Saral Token Error: { error_msg } |.
  ENDMETHOD.


  METHOD generate_pdf_sto_amazon.
    DATA: lv_date TYPE char10.
    DATA: data_tab TYPE zeinv_irn_details_st.
    CONSTANTS: c_version TYPE char05 VALUE '1.1',
               c_gst     TYPE char10 VALUE  'GST',
               c_b2b     TYPE char03 VALUE  'B2B',
               c_null    TYPE char07 VALUE  'null',
               c_zero    TYPE char01 VALUE  '0'.

    DATA: v_tcs       TYPE string,
          v_assval    TYPE string,
          v_igst      TYPE string,
          v_cgst      TYPE string,
          v_sgst      TYPE string,
          v_amtval    TYPE string,
          lv_roundtot TYPE i.
    SELECT ebeln,
           ebelp,
           werks,
           matnr,
           menge,
           meins,
           erfmg,
           erfme,
           shkzg,
           dmbtr,
           charg,
           budat_mkpf FROM mseg
      INTO TABLE @DATA(lt_po)
      WHERE mblnr = @mblnr AND mjahr = @mjahr AND shkzg = 'H'.
    IF sy-subrc = 0.
      DATA(lw_po) = lt_po[ 1 ].
      SELECT SINGLE reswk FROM ekko
        INTO @DATA(seller_plant)
        WHERE ebeln = @lw_po-ebeln.
      SELECT SINGLE werks FROM ekpo
        INTO @DATA(recv_plant)
        WHERE ebeln = @lw_po-ebeln.
    ENDIF.

    "Get the Buyer Details
    SELECT SINGLE
           werks,
           name1,
           adrnr,
           j_1bbranch FROM t001w
                      INTO  @DATA(l_dealer_dtls)
                      WHERE werks EQ @recv_plant.
    IF l_dealer_dtls IS NOT INITIAL.

      "Get Plant address details for Seller Addr1
      SELECT SINGLE
             a~werks,
             a~pstlz,
             a~land1,
             a~regio,
             b~bezei,
             c~addrnumber,      "Seller Legal Name
             c~name1,           "Despatch from Name\
*             c~house_num1,
             c~street,          "Street     &"Despatch from add1
             c~str_suppl1,      "
             c~str_suppl2,      "
             c~home_city,
             c~city1,
             c~tel_number,
*             c~str_suppl3,      "
*             c~location,        "Despatch from add1
             c~post_code1,
             c~region,
             c~mc_city1
                    FROM ( ( t001w AS a
                      INNER JOIN t005u AS b
                      ON  b~spras = @sy-langu
                      AND a~land1 = b~land1
                      AND a~regio = b~bland )
                      INNER JOIN adrc AS c
                      ON  a~adrnr = c~addrnumber )
                     INTO @DATA(l_dealer_addr)
                WHERE a~werks = @recv_plant.

      SELECT SINGLE
             bukrs,
             branch,
             gstin    "Seller GSTIN
                   FROM j_1bbranch
                   INTO @DATA(l_buyer_gstin)
*                   FOR ALL ENTRIES IN @lt_tab_t001w
                   WHERE bukrs = '1000' AND
                         branch = @l_dealer_dtls-j_1bbranch.

    ENDIF.
    "Get the Seller Details
    SELECT SINGLE
           werks,
           name1,
           adrnr,
           j_1bbranch FROM t001w
                      INTO  @DATA(l_dist_dtls)
                      WHERE werks EQ @seller_plant.
    IF l_dist_dtls IS NOT INITIAL.

      "Get Plant address details for Seller Addr1
      SELECT SINGLE
             a~werks,
             a~pstlz,
             a~land1,
             a~regio,
             b~bezei,
             c~addrnumber,      "Seller Legal Name
             c~name1,           "Despatch from Name\
*             c~house_num1,
             c~street,          "Street     &"Despatch from add1
             c~str_suppl1,      "
             c~str_suppl2,      "
             c~home_city,
             c~city1,
             c~tel_number,
*             c~str_suppl3,      "
*             c~location,        "Despatch from add1
             c~post_code1,
             c~region,
             c~mc_city1
                    FROM ( ( t001w AS a
                      INNER JOIN t005u AS b
                      ON  b~spras = @sy-langu
                      AND a~land1 = b~land1
                      AND a~regio = b~bland )
                      INNER JOIN adrc AS c
                      ON  a~adrnr = c~addrnumber )
                     INTO @DATA(l_dist_addr)
                WHERE a~werks = @seller_plant.

      SELECT SINGLE
             bukrs,
             branch,
             gstin    "Seller GSTIN
                   FROM j_1bbranch
                   INTO @DATA(l_recv_gstin)
*                   FOR ALL ENTRIES IN @lt_tab_t001w
                   WHERE bukrs = '1000' AND
                         branch = @l_dist_dtls-j_1bbranch.

    ENDIF.


*------- Data Population for Gloabal Structures ------*
    CLEAR: data_tab.
*    data_tab-distributor = l_dist_dtls-kunnr. "Distributor Code.
*    data_tab-body-version = c_version.
**---- Transaction Details ----*
    DATA(l_trandtls) = VALUE zeinv_trans_st( taxsch = c_gst
                                             suptyp = c_b2b
                                             regrev = 'N' ).
    data_tab-body-trandtls =  l_trandtls.

    DATA(lv_inv_date) = lw_po-budat_mkpf.
    WRITE lv_inv_date TO lv_date DD/MM/YYYY.
    REPLACE ALL OCCURRENCES OF '.' IN lv_date WITH '/'.
    DATA(invoiceno_tmp) = |{ mblnr }/{ mjahr }|.
    SHIFT invoiceno_tmp LEFT DELETING LEADING '0'.
    DATA(l_docdtls) = VALUE zirn_docdls_st( typ = 'DBN'
                                            no = invoiceno_tmp
                                            dt = lv_date ).
    data_tab-docdtls = l_docdtls.
*----------------------------------------------------------------------------------------------------------
**----- Seller Details which Means Distributor ----*
*    SELECT SINGLE taxnum FROM dfkkbptaxnum INTO @DATA(l_dist_gstin) WHERE partner = @l_dist_dtls-kunnr.
*    IF sy-subrc = 0.
    IF sy-sysid NE 'PRD'.
      data_tab-sellerdtls-gstin = '29AABCR7796N000'.
    ELSE.
      CONDENSE l_recv_gstin.
      data_tab-sellerdtls-gstin = l_recv_gstin-gstin. "Seller GSTIN
    ENDIF.
    "Seller Name
    IF l_dist_dtls-name1 IS NOT INITIAL.
      data_tab-sellerdtls-lglnm = l_dist_dtls-name1.
    ELSE.
      data_tab-sellerdtls-lglnm = c_null.
    ENDIF.
    "seller Address Details
*    IF l_dist_dtls-stras IS NOT INITIAL.
*      data_tab-sellerdtls-addr1 = l_dist_dtls-stras.
*      data_tab-sellerdtls-addr2 = l_dist_dtls-stras.
*    ELSE.
    data_tab-sellerdtls-addr1 = l_dist_addr-str_suppl1.
    data_tab-sellerdtls-addr2 = l_dist_addr-str_suppl2.
    data_tab-sellerdtls-trdnm = l_dist_addr-street.           """"this field use as seller Street
    "Seller Location
*    IF l_dist_dtls-ort01 IS NOT INITIAL.
*      data_tab-sellerdtls-loc = l_dist_dtls-ort01.
*    ELSE.
    data_tab-sellerdtls-loc = |{ l_dist_addr-city1 }|.
*    ENDIF.
    "Seller Postal Code

    IF l_dist_addr-post_code1 IS NOT INITIAL.
      data_tab-sellerdtls-pin = |{ l_dist_addr-post_code1 } - { l_dist_addr-region }|.
    ENDIF.
    " Phone Number
    data_tab-sellerdtls-ph = l_dist_addr-tel_number.
    "Seller state Code
    IF l_recv_gstin IS NOT INITIAL.
      data_tab-sellerdtls-stcd = data_tab-sellerdtls-gstin(2).
      SHIFT data_tab-sellerdtls-stcd LEFT DELETING LEADING '0'.
    ENDIF.
*    "Seller Phone Number
*    IF l_dist_dtls-telf1 IS NOT INITIAL.
*      data_tab-sellerdtls-ph = l_dist_dtls-telf1.
*    ELSE.
*      data_tab-sellerdtls-ph = c_null.
*    ENDIF.
*    "Seller Email
*    SELECT SINGLE smtp_addr FROM adr6 INTO @DATA(l_seller_mail) WHERE addrnumber = @l_dist_addr-addrnumber.
*    IF sy-subrc = 0.
*      data_tab-sellerdtls-em = l_seller_mail.
*    ELSE.
*      data_tab-sellerdtls-em = c_null.
*    ENDIF.
*    ELSE.
*      message = | Error: GST No is not maintained for Distributor { l_dist_dtls-kunnr } |.
*    ENDIF.
*--------------------------------------------------------------------------------------------------------*
    "Buyer Details
*    SELECT SINGLE taxnum FROM dfkkbptaxnum INTO @DATA(l_dealer_gstin) WHERE partner = @l_dealer_dtls-kunnr.
*    IF l_buyer_gstin IS NOT INITIAL.
    IF sy-sysid NE 'PRD'.
      data_tab-buyerdtls-gstin = '29AGHPG8602R1ZR'.
    ELSE.
      CONDENSE l_buyer_gstin-gstin.
      data_tab-buyerdtls-gstin = l_buyer_gstin-gstin. "Seller GSTIN
    ENDIF.
    "Buyer Name
    IF l_dealer_dtls-name1 IS NOT INITIAL.
      data_tab-buyerdtls-lglnm = l_dealer_dtls-name1. "Buyer Name
    ELSE.
      data_tab-buyerdtls-lglnm = c_null.
    ENDIF.
    "Buyer Address Details
*      IF l_dealer_dtls-stras IS NOT INITIAL.
*        data_tab-buyerdtls-addr1 = l_dealer_dtls-stras.
*        data_tab-buyerdtls-addr2 = l_dealer_dtls-stras.
*      ELSE.
    data_tab-buyerdtls-addr1 = l_dealer_addr-str_suppl1.
    data_tab-buyerdtls-addr2 = l_dealer_addr-str_suppl2.
    data_tab-buyerdtls-trdnm = l_dealer_addr-street.             """this field use as buyer street
*      ENDIF.
    "Buyer Location
*      IF l_dealer_dtls-ort01 IS NOT INITIAL.
*        data_tab-buyerdtls-loc = l_dealer_dtls-ort01.
*      ELSE.
    data_tab-buyerdtls-loc = |{ l_dealer_addr-city1 }|.
*      ENDIF.
    "Buyer Postal Code
    IF l_dealer_addr-post_code1 IS NOT INITIAL.
      data_tab-buyerdtls-pin = |{ l_dealer_addr-post_code1 } - { l_dealer_addr-region }|.
      data_tab-buyerdtls-stcd = l_dealer_addr-tel_number.
    ENDIF.
*    "Buyer state Code
*    IF l_buyer_gstin-gstin IS NOT INITIAL.
*      data_tab-buyerdtls-stcd = data_tab-buyerdtls-gstin(2).
*      data_tab-buyerdtls-pos = data_tab-buyerdtls-gstin(2).
*      SHIFT data_tab-buyerdtls-stcd LEFT DELETING LEADING '0'.
*      SHIFT data_tab-buyerdtls-pos LEFT DELETING LEADING '0'.
*    ENDIF.
**    ENDIF.
*--------------------------------------------------------------------------------------------------------------------*
*    DATA(lv_condno) = VALUE #( l_tab_vbrk[ 1 ]-knumv OPTIONAL ).


    "Get the material description->>
    SELECT a~ebeln,
           a~ebelp,
           a~matnr,
           b~steuc, " Matnr HSN Code
           a~txz01 AS maktx,
           a~werks,
           a~meins, "Order Unit
           a~menge, "Ordered Qty
           a~netpr, "Net Price
           a~mwskz,  "Tax code
           a~bednr AS charg
           FROM ekpo AS a INNER JOIN marc AS b ON a~matnr = b~matnr
                                              AND a~werks = b~werks
      INTO TABLE @DATA(l_tab_item)
      FOR ALL ENTRIES IN @lt_po
      WHERE a~ebeln = @lt_po-ebeln AND a~ebelp = @lt_po-ebelp.
    LOOP AT l_tab_item ASSIGNING FIELD-SYMBOL(<fs>).
      <fs>-meins = VALUE #( lt_po[ ebeln = <fs>-ebeln ebelp = <fs>-ebelp ]-meins OPTIONAL ).
      <fs>-menge = VALUE #( lt_po[ ebeln = <fs>-ebeln ebelp = <fs>-ebelp ]-menge OPTIONAL ).
      <fs>-netpr = VALUE #( lt_po[ ebeln = <fs>-ebeln ebelp = <fs>-ebelp ]-dmbtr OPTIONAL ).
      <fs>-charg = VALUE #( lt_po[ ebeln = <fs>-ebeln ebelp = <fs>-ebelp ]-charg OPTIONAL ).
    ENDLOOP.
    IF sy-subrc = 0.
*** GST Variable for Calculation **
      SELECT kschl, kbetr, mwsk1 FROM konp
                                 INTO TABLE @DATA(lt_gst)
                                 FOR ALL ENTRIES IN @l_tab_item
                                 WHERE kappl = 'TX'
                                 AND kschl IN ( 'JIIG','JISG','JICG','JTC1' )
                                 AND mwsk1 EQ @l_tab_item-mwskz.
    ENDIF.
    "PO Items Details
    LOOP AT l_tab_item ASSIGNING FIELD-SYMBOL(<fs_vbrp>).
      "Material HSN code
      DATA(lv_hsncd) = <fs_vbrp>-steuc .
      REPLACE ALL OCCURRENCES OF '.' IN lv_hsncd WITH space.
      CONDENSE lv_hsncd NO-GAPS.
      IF lv_hsncd IS INITIAL.
        lv_hsncd = c_null.
      ENDIF.
*           a~netpr, "Net Price
      "Lineitem Qty
      IF <fs_vbrp>-menge IS INITIAL.
        <fs_vbrp>-menge = c_null.
      ENDIF.
      "UOM Conversion
      IF <fs_vbrp>-meins = 'EA'.         "Each
        DATA(l_meins) = 'EA'.
      ELSEIF <fs_vbrp>-meins = 'KG'.     "Kilograms
        l_meins = 'KGS'.
      ELSEIF <fs_vbrp>-meins = 'BT'.     "Bottle
        l_meins = 'BTL'.
      ELSEIF <fs_vbrp>-meins = 'L'.      "Litre
        l_meins = 'LTR'.
      ELSEIF <fs_vbrp>-meins = 'M'.      "Metre
        l_meins = 'MTR'.
      ELSEIF <fs_vbrp>-meins = 'PAA'.    "Pairs
        l_meins = 'PRS'.
      ELSE.
        l_meins = <fs_vbrp>-meins.
      ENDIF.

      IF <fs_vbrp>-netpr IS NOT INITIAL.
        DATA(l_uprice) = CONV kbetr_kond( <fs_vbrp>-netpr DIV <fs_vbrp>-menge ).
        DATA(l_amount) = <fs_vbrp>-netpr.
        DATA(l_assamt) = <fs_vbrp>-netpr.
      ELSE.
        l_uprice = c_zero.
        l_amount = c_zero.
        l_assamt = c_zero.
      ENDIF.
**----- Condition data amount Population --------*

**********Integrated GST Calculation ************************************************************
      READ TABLE lt_gst INTO DATA(ls_gst) WITH KEY kschl = 'JIIG'
                                                   mwsk1 = <fs_vbrp>-mwskz BINARY SEARCH.
      IF sy-subrc EQ 0.
        DATA(l_itax) = ( ls_gst-kbetr / 10 ).
        DATA(l_igst) = ( <fs_vbrp>-netpr * l_itax ) / 100.
      ELSE.
        l_itax = c_zero.
        l_igst = c_zero.
      ENDIF.
************** State GST Calculation ***********************************************************
      READ TABLE lt_gst INTO DATA(ls_gst1) WITH KEY kschl = 'JICG'
                                                    mwsk1 = <fs_vbrp>-mwskz BINARY SEARCH.
      IF sy-subrc EQ 0.
        DATA(l_ctax) = ( ls_gst1-kbetr / 10 ).
        DATA(l_cgst)  = ( <fs_vbrp>-netpr * l_ctax ) / 100.
      ELSE.
        l_ctax = c_zero.
        l_cgst = c_zero.
      ENDIF.
***************** Central GST Calculation ********************************************************
      READ TABLE lt_gst INTO DATA(ls_gst2) WITH KEY kschl = 'JISG'
                                           mwsk1 = <fs_vbrp>-mwskz BINARY SEARCH.
      IF sy-subrc EQ 0.
        DATA(l_stax) = ( ls_gst1-kbetr / 10 ).
        DATA(l_sgst)  = ( <fs_vbrp>-netpr * l_stax ) / 100.
      ELSE.
        l_stax = c_zero.
        l_sgst = c_zero.
      ENDIF.


      DATA(l_total_gst) = CONV string( l_itax + l_ctax + l_stax ).
      CONDENSE l_total_gst.
      DATA(l_totalitem_amt) = CONV string( l_assamt + l_igst + l_cgst + l_sgst   ).
      CONDENSE l_totalitem_amt.

      APPEND VALUE #( slno       = <fs_vbrp>-ebelp
                      prddesc    = |{ <fs_vbrp>-matnr } # { <fs_vbrp>-maktx } (Batch# { <fs_vbrp>-charg })|
                      isservc    = 'N'
                      hsncd      = lv_hsncd
                      qty        = <fs_vbrp>-menge
                      freeqty    = c_zero
                      unit       = l_meins
                      unitprice  = l_uprice
                      totamt     = l_amount
                      assamt     = l_assamt
                      gstrt      = l_total_gst
                      igstamt    = l_igst
                      cgstamt    = l_cgst
                      sgstamt    = l_sgst
                      othchrg    = c_zero
                      totitemval = l_totalitem_amt
                    ) TO data_tab-itemlist.
      CLEAR: l_cgst,l_sgst,l_igst,l_itax,l_ctax,l_stax,
             l_uprice,l_amount,l_assamt,l_total_gst,l_totalitem_amt.
    ENDLOOP.
*---- Invoice Total Value Population ----*\
    IF data_tab-itemlist IS NOT INITIAL.
      LOOP AT data_tab-itemlist INTO DATA(lw_itemlist).
****assessable Value, Total Value & igst Value
        v_assval = v_assval + lw_itemlist-assamt.
        v_amtval = v_amtval + lw_itemlist-totamt.
        v_tcs    = v_tcs    + lw_itemlist-othchrg.
        v_igst   = v_igst   + lw_itemlist-igstamt.
        v_cgst   = v_cgst   + lw_itemlist-cgstamt.
        v_sgst   = v_sgst   + lw_itemlist-sgstamt.
      ENDLOOP.
      data_tab-valdtls-assval      = v_assval.
      data_tab-valdtls-cgstval     = v_cgst.
      data_tab-valdtls-sgstval     = v_sgst.
      data_tab-valdtls-igstval     = v_igst.
      data_tab-valdtls-cesval      = c_zero.
      data_tab-valdtls-stcesval    = c_zero.
      data_tab-valdtls-stcesval    = c_zero.
      data_tab-valdtls-othchrg     = c_zero.
      data_tab-valdtls-totinvval   = v_amtval + v_cgst + v_sgst + v_igst.
      data_tab-valdtls-totinvvalfc = c_zero.
*************rounding off process*************
      CLEAR : lv_roundtot.
      lv_roundtot                  = data_tab-valdtls-totinvval.
      data_tab-valdtls-rndoffamt   = lv_roundtot - data_tab-valdtls-totinvval.
      data_tab-valdtls-totinvval   = lv_roundtot.
    ENDIF.
*    ENDIF.

    DATA ls_docpara TYPE sfpdocparams.
    DATA ls_outpara TYPE sfpoutputparams.
    DATA ls_output  TYPE fpformoutput.
    DATA ls_frmname TYPE fpname.
    DATA lv_fm    TYPE rs38l_fnam.

    ls_outpara-preview = abap_true.
*    ls_outpara-getpdf = abap_true.

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

    ls_frmname = 'ZSD_STO_WITHIN_STATE_FINAL'.

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
        is_bil_invoice     = data_tab
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
*
**      DATA(lv_pdf) = CONV xstring( ls_output-pdf ).
*    pdf = ls_output-pdf.
  ENDMETHOD.


  METHOD generate_ewb_by_irn.
    DATA: lv_time  TYPE tims,
          lv_am_pm TYPE char2.
*-----------input alpha conversion-----------*
    DATA(ls_invoice) = |{ ls_request-invoice_no ALPHA = IN }|.
    SELECT SINGLE FROM zdms_invoice_irn FIELDS * WHERE docno = @ls_invoice INTO @DATA(ls_irn_dtls).
    IF sy-subrc = 0.
      distributor = ls_irn_dtls-distrb.
      dealer = ls_irn_dtls-dealer.
      CALL METHOD me->get_saral_token
        EXPORTING
          distributor_code     = ls_irn_dtls-distrb
          customer             = ls_irn_dtls-dealer
        IMPORTING
          authentication_token = DATA(authentication_token)
          subscription_id      = DATA(subscription_id)
          error_code           = DATA(error_code)
          error_msg            = DATA(error_msg).
      IF error_msg IS INITIAL AND
        authentication_token IS NOT INITIAL AND
        subscription_id IS NOT INITIAL.
        CALL METHOD me->get_irn_token
          EXPORTING
            distributor_code     = ls_irn_dtls-distrb
            authentication_token = authentication_token
            subscription_id      = subscription_id
          IMPORTING
            err_msg              = DATA(err_msg)
            auth_token           = DATA(auth_token)
            session_key          = DATA(session_key)
            user_name            = DATA(user_name)
            gstin                = DATA(gstin).
        IF err_msg IS INITIAL AND
                auth_token IS NOT INITIAL AND
                session_key IS NOT INITIAL AND
                user_name IS NOT INITIAL AND
                gstin IS NOT INITIAL.
*----------Prepare Json for EWB create by IRN----------*
          IF ls_request-distance IS INITIAL.
            ls_request-distance = '0'.
          ENDIF.
          DATA(trns_date) = |{ ls_request-transdate+8(2) }/{  ls_request-transdate+5(2) }/{ ls_request-transdate+0(4) }|.
          CONCATENATE
            '{'
              '"Irn": "' ls_irn_dtls-irn '",'
              '"Distance": ' ls_request-distance ','
              '"TransMode": "' '1' '",'
              '"TransId": "' ls_request-transid '",'
              '"TransName": "' ls_request-transname '",'
              '"TrnDocDt": "' trns_date '",'
              '"TrnDocNo": "' ls_request-transdoc '",'
              '"VehNo": "' ls_request-vehno '",'
              '"VehType": "' 'R' '"'
            '}' INTO DATA(lv_ewbdtls).

          CALL METHOD me->call_api_generate_eway_by_irn
            EXPORTING
              distributor_code     = ls_irn_dtls-distrb                " Distributor Number
              customer             = ls_irn_dtls-dealer                " Customer Number
              bukrs                = ls_irn_dtls-bukrs                " Company Code
              gjahr                = ls_irn_dtls-doc_year             " Fiscal Year
              doc_type             = ls_irn_dtls-doc_type              " Document Type
              docno                = ls_irn_dtls-docno                " Sales and Distribution Document Number
              authentication_token = authentication_token
              subscription_id      = subscription_id
              auth_token           = auth_token
              session_key          = session_key
              user_name            = user_name                  " Einvoice Username
              gstin                = gstin                " Business Partner Tax Number
              iv_payload_json      = lv_ewbdtls               " JSON Payload
              ls_request           = ls_request               " Eway trns input
            IMPORTING
              iv_eway_trans_dtls   = DATA(ls_eway_trasn_dtls)                " Ewaybill transporter Details
              message              = return.
          IF return IS INITIAL AND ls_eway_trasn_dtls IS NOT INITIAL.
            ewb_no = ls_eway_trasn_dtls-ebillno.
            ewb_dt = |{ ls_eway_trasn_dtls-erdat+0(4) }-{ ls_eway_trasn_dtls-erdat+4(2) }-{ ls_eway_trasn_dtls-erdat+6(2) }|.
            CALL FUNCTION 'HRVE_CONVERT_TIME'
              EXPORTING
                type_time    = 'A'
                input_time   = ls_eway_trasn_dtls-aezet
              IMPORTING
                output_time  = lv_time
                output_am_pm = lv_am_pm.
            ewb_valid_till = |{ ls_eway_trasn_dtls-aedat+0(4) }-{ ls_eway_trasn_dtls-aedat+4(2) }-{ ls_eway_trasn_dtls-aedat+6(2) } { lv_time+0(2) }:{ lv_time+2(2) }:{ lv_time+4(2) } { lv_am_pm }  |.
            MODIFY zdms_eway_trdtls FROM ls_eway_trasn_dtls.
            COMMIT WORK.
          ELSE.
            IF return IS INITIAL.
*              return = 'IRN Creation is not generated'.
              return = 'Response is empty'.
            ENDIF.
          ENDIF.
        ELSE.
          IF err_msg IS INITIAL.
            return = 'Authentication Token is not generated'.
          ELSE.
            return = | Authentication Token Error: { err_msg } |.
          ENDIF.
        ENDIF.
      ELSE.
        IF error_msg IS INITIAL.
          return = 'Saral Token is not generated'.
        ELSE.
          return = | Saral Token Error: { error_msg } |.
        ENDIF.
      ENDIF.
    ELSE.
      return = 'Authentication Token is not generated'.
    ENDIF.

  ENDMETHOD.


  METHOD call_api_generate_eway_by_irn.
    TYPES: BEGIN OF ty_response,
             Ewb_No         TYPE char12,
             Ewb_dt         TYPE char10,
             Ewb_valid_till TYPE char20,
           END OF ty_response.
    "API response Structure
    DATA: ls_response TYPE ty_response.
    DATA: v_str       TYPE string.
    DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
    DATA: lv_data TYPE string,
          lv_body TYPE string.
    DATA: lo_http_client TYPE REF TO if_http_client.
*---- URL fixing for system based ----*
    CASE sy-sysid.
      WHEN 'DEV' OR 'QAS'.
        DATA(l_api_url) = 'DMS_EINV_CREATE_EWB_TEST'.
      WHEN 'PRD'.
        l_api_url = 'DMS_EINV_CREATE_EWB_PRD'.
    ENDCASE.
    SELECT SINGLE low FROM tvarvc INTO @DATA(l_url) WHERE name = @l_api_url
                                                    AND type = 'P'.
   IF sy-subrc = 0.
      DATA(create_url) = CONV string( l_url ).
    cl_http_client=>create_by_url(
          EXPORTING
          url =  create_url              "create_url
          IMPORTING
          client = lo_http_client
          EXCEPTIONS
          argument_not_found = 1
          plugin_not_active = 2
          internal_error = 3
          OTHERS = 4 ).

    CHECK lo_http_client IS BOUND.

    lo_http_client->propertytype_logon_popup = if_http_client=>co_disabled.

    lo_http_client->request->set_method(
     EXPORTING
     method = if_http_entity=>co_request_method_post ).


    lo_http_client->request->set_content_type(
     EXPORTING
     content_type = if_rest_media_type=>gc_appl_json ).

    "Header Data Fields for API Fixing
    lo_http_client->request->set_header_field( EXPORTING name  = 'AuthenticationToken' value =  CONV string( authentication_token ) ).

    lo_http_client->request->set_header_field( EXPORTING name  = 'SubscriptionId'      value =  CONV string( subscription_id ) ).

    lo_http_client->request->set_header_field( EXPORTING name  = 'Gstin'               value =  CONV string( gstin ) ).

    lo_http_client->request->set_header_field( EXPORTING name  = 'UserName'            value =  CONV string( user_name ) ).

    lo_http_client->request->set_header_field( EXPORTING name  = 'AuthToken'           value =  CONV string( auth_token ) ).

    lo_http_client->request->set_header_field( EXPORTING name  = 'sek'                 value =  CONV string( session_key )  ).

    lo_http_client->request->set_cdata(
       EXPORTING
       data = iv_payload_json ).

    lo_http_client->send(
     EXCEPTIONS
     http_communication_failure = 1
     http_invalid_state = 2 ).

    CHECK sy-subrc = 0.
    lo_http_client->receive(
     EXCEPTIONS
     http_communication_failure = 1
     http_invalid_state = 2
     http_processing_failed = 3 ).

    lo_http_client->response->get_status(
    IMPORTING
      code = DATA(lv_codes) ).

    lo_http_client->response->get_status(
    IMPORTING
      reason = DATA(lv_http_error) ).

    "Actual API Response If success
    IF lv_codes = 200.
      DATA(lv_response) = lo_http_client->response->get_cdata( ).
      DATA(test) = lo_http_client->response->get_data( ).
** deserialize the INPUT our required INPUT ***
      /ui2/cl_json=>deserialize(
      EXPORTING
       json         = lv_response
       pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
      CHANGING
       data         = ls_response ).

      IF ls_response IS NOT INITIAL.
        "Response of API
        iv_eway_trans_dtls = VALUE #(
            mandt     = sy-mandt
            bukrs     = bukrs
            distrb    = distributor_code
            dealer    = customer
            doctyp    = doc_type
            docno     = docno
            gjahr     = gjahr
            ebillno   = ls_response-ewb_no
            vehno     = ls_request-vehno
            transid   = ls_request-transid
            transname = ls_request-transname
            vehtyp    = 'R'
            transmode = '1'
            distance  = ls_request-distance
            transdoc  = ls_request-transdoc
            transdt   = |{ ls_request-transdate+8(2) }/{  ls_request-transdate+5(2) }/{ ls_request-transdate+0(4) }|
             status    = 'A'
            ernam     = sy-uname
            erdat     = |{ ls_response-ewb_dt+0(4) }{ ls_response-ewb_dt+5(2) }{ ls_response-ewb_dt+8(2) }|
            erzet     = sy-uzeit
            aedat     = |{ ls_response-ewb_valid_till+0(4) }{ ls_response-ewb_valid_till+5(2) }{ ls_response-ewb_valid_till+8(2) }|
            aenam     = sy-uname
            aezet     = |{ ls_response-ewb_valid_till+11(2) }{ ls_response-ewb_valid_till+14(2) }{ ls_response-ewb_valid_till+17(2) }|
        ).

      ELSE.
        " Error Details
        IF message IS INITIAL.
          SPLIT lv_response AT '"errorDetails":' INTO v_str message.
          IF message IS INITIAL.
            SPLIT lv_response AT '"ErrorDetails":' INTO v_str message.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      message = |Error Code: { lv_codes } - { lv_http_error }|.
    ENDIF.
    ELSE.
    message = 'URL is not maintained in STVARV'.
ENDIF.

        CREATE OBJECT lo_log_upd.
     IF message IS NOT INITIAL.
      lv_body = |"message":"{ message }"|.
      CONCATENATE '[{' lv_body '}]' INTO lv_body.
    ELSE.
** serialize the output for response ***
      /ui2/cl_json=>serialize(
      EXPORTING
       data         =  ls_response
       pretty_name  = /ui2/cl_json=>pretty_mode-user
      RECEIVING
       r_json         = lv_body ).
    ENDIF.

*Output Entry in Log Table
    CALL METHOD lo_log_upd->log_entry_store
      EXPORTING
        apiname         = 'SARAL_CREATE_EWAY_BY_IRN'
        ijson           = iv_payload_json
        ojson           = lv_body
        distributor     = distributor_code
        retailer        = customer
      EXCEPTIONS
        apiname_missing = 1
        json_missing    = 2
        OTHERS          = 3.

  ENDMETHOD.


  method CANCEL_EWB_BY_IRN.
        DATA: lv_time  TYPE tims,
          lv_am_pm TYPE char2.
*-------Eway bill cancellation code--------*
      SELECT SINGLE FROM zdms_eway_trdtls FIELDS * WHERE docno = @invoiceno and status = 'A' INTO @DATA(ls_eway_trans).
        IF Sy-subrc = 0.
        distributor = ls_eway_trans-distrb.
        dealer = ls_eway_trans-dealer.
           CALL METHOD me->get_saral_token
      EXPORTING
        distributor_code     = ls_eway_trans-distrb
        customer             = ls_eway_trans-dealer
      IMPORTING
        authentication_token = DATA(authentication_token)
        subscription_id      = DATA(subscription_id)
        error_code           = DATA(error_code)
        error_msg            = DATA(error_msg).
            IF error_msg IS INITIAL AND
      authentication_token IS NOT INITIAL AND
      subscription_id IS NOT INITIAL.
      CALL METHOD me->get_irn_token
        EXPORTING
          distributor_code     = ls_eway_trans-distrb
          authentication_token = authentication_token
          subscription_id      = subscription_id
        IMPORTING
          err_msg              = DATA(err_msg)
          auth_token           = DATA(auth_token)
          session_key          = DATA(session_key)
          user_name            = DATA(user_name)
          gstin                = DATA(gstin).
      IF err_msg IS INITIAL AND
              auth_token IS NOT INITIAL AND
              session_key IS NOT INITIAL AND
              user_name IS NOT INITIAL AND
              gstin IS NOT INITIAL.
*--------Defining Payload for EWB Cancel---------*
         CONCATENATE
          '{'
            '"ewbNo": ' ls_eway_trans-ebillno ','
            '"cancelRsnCode": ' '2' ','  "cancel_rsn_code
            '"cancelRmrk": "' cancel_rmrk '"'
          '}' INTO DATA(lv_ewbdtls).

*----------Call Eway BillCancel by IRN API-----------*
 me->call_api_cancel_eway_by_irn(
   EXPORTING
     lv_eway_details      = ls_eway_trans               " Ewaybill transporter Details
     authentication_token = authentication_token
     subscription_id      = subscription_id
     auth_token           = auth_token
     session_key          = session_key
     user_name            = user_name                             " Einvoice Username
     gstin                = gstin                                 " Business Partner Tax Number
     iv_payload_json      = lv_ewbdtls                            " JSON Payload
     cancel_reason        = cancel_rmrk                 " Cancel reason
   IMPORTING
     message              = return
   CHANGING
     iv_eway_trans_dtls   =  ls_eway_trans                " Ewaybill transporter Details
 ).
        IF return IS INITIAL AND ls_eway_trans IS NOT INITIAL.
          ewaybill_no = ls_eway_trans-ebillno.
            CALL FUNCTION 'HRVE_CONVERT_TIME'
               EXPORTING
                 type_time       = 'A'
                 input_time      = ls_eway_trans-cancelled_at
               IMPORTING
                 output_time     = lv_time
                 output_am_pm    = lv_am_pm
               .
          canceldate = |{ ls_eway_trans-cancelled_on+0(4) }-{ ls_eway_trans-cancelled_on+4(2) }-{ ls_eway_trans-cancelled_on+6(2) } { lv_time+0(2) }:{ lv_time+2(2) }:{ lv_time+4(2) } { lv_am_pm }  |.
          MODIFY zdms_eway_trdtls FROM ls_eway_trans.
          ELSE.
            IF return IS INITIAL.
            return = 'E-way Bill Is Already Cancelled'.
          ENDIF.
        ENDIF.
       ELSE.
        IF err_msg IS INITIAL.
          return = 'Authentication Token is not generated'.
        ELSE.
          return = | Authentication Token Error: { err_msg } |.
        ENDIF.
      ENDIF.
    ELSE.
      IF error_msg IS INITIAL.
        return = 'Saral Token is not generated'.
      ELSE.
        return = | Saral Token Error: { error_msg } |.
      ENDIF.
    ENDIF.
    ENDIF.
  endmethod.


  METHOD call_api_cancel_eway_by_irn.
    TYPES: BEGIN OF ty_response,
             eway_Bill_No TYPE char12,
             cancelDate   TYPE char25,
           END OF ty_response.
    "API response Structure
    DATA: ls_response TYPE ty_response.
    DATA: v_str       TYPE string.
    DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
    DATA: lv_data TYPE string,
          lv_body TYPE string.
    DATA: lo_http_client TYPE REF TO if_http_client.
*---- URL fixing for system based ----*
    CASE sy-sysid.
      WHEN 'DEV' OR 'QAS'.
        DATA(l_api_url) = 'DMS_EINV_CANCEL_EWB_TEST'.
      WHEN 'PRD'.
        l_api_url = 'DMS_EINV_CANCEL_EWB_PRD'.
    ENDCASE.
    SELECT SINGLE low FROM tvarvc INTO @DATA(l_url) WHERE name = @l_api_url
                                                    AND type = 'P'.
    IF sy-subrc = 0.
         DATA(create_url) = CONV string( l_url ).
      cl_http_client=>create_by_url(
            EXPORTING
            url =  create_url   "'https://demo.saralgsp.com/eicore/v1.03/ewaybill'                 "create_url
            IMPORTING
            client = lo_http_client
            EXCEPTIONS
            argument_not_found = 1
            plugin_not_active = 2
            internal_error = 3
            OTHERS = 4 ).

      CHECK lo_http_client IS BOUND.
      lo_http_client->propertytype_logon_popup = if_http_client=>co_disabled.

      lo_http_client->request->set_method(
       EXPORTING
       method = if_http_entity=>co_request_method_post ).


      lo_http_client->request->set_content_type(
       EXPORTING
       content_type = if_rest_media_type=>gc_appl_json ).

      "Header Data Fields for API Fixing
      lo_http_client->request->set_header_field( EXPORTING name  = 'AuthenticationToken' value =  CONV string( authentication_token ) ).

      lo_http_client->request->set_header_field( EXPORTING name  = 'SubscriptionId'      value =  CONV string( subscription_id ) ).

      lo_http_client->request->set_header_field( EXPORTING name  = 'Gstin'               value =  CONV string( gstin ) ).

      lo_http_client->request->set_header_field( EXPORTING name  = 'UserName'            value =  CONV string( user_name ) ).

      lo_http_client->request->set_header_field( EXPORTING name  = 'AuthToken'           value =  CONV string( auth_token ) ).

      lo_http_client->request->set_header_field( EXPORTING name  = 'sek'                 value =  CONV string( session_key )  ).

      lo_http_client->request->set_header_field( EXPORTING name  = 'action'                 value =  CONV string( 'CANEWB' )  ).

      lo_http_client->request->set_cdata(
         EXPORTING
         data = iv_payload_json ).

      lo_http_client->send(
       EXCEPTIONS
       http_communication_failure = 1
       http_invalid_state = 2 ).

      CHECK sy-subrc = 0.
      lo_http_client->receive(
       EXCEPTIONS
       http_communication_failure = 1
       http_invalid_state = 2
       http_processing_failed = 3 ).

      lo_http_client->response->get_status(
      IMPORTING
        code = DATA(lv_codes) ).

      lo_http_client->response->get_status(
      IMPORTING
        reason = DATA(lv_http_error) ).

      "Actual API Response If success
      IF lv_codes = 200.
        DATA(lv_response) = lo_http_client->response->get_cdata( ).
** deserialize the INPUT our required INPUT ***
        /ui2/cl_json=>deserialize(
        EXPORTING
         json         = lv_response
         pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
        CHANGING
         data         = ls_response ).

        IF ls_response IS NOT INITIAL.
          "Response of API
          TRY.
              cl_abap_timefm=>conv_time_ext_to_int(
                EXPORTING
                  time_ext      = ls_response-canceldate+11(11)                 " External Represenation of Time
                  is_24_allowed = abap_true       " Is 24:00 permitted?
                IMPORTING
                  time_int      =  DATA(ls_time_int)               " Internal Represenation of Time
              ).
            CATCH cx_abap_timefm_invalid.
          ENDTRY.

          iv_eway_trans_dtls = VALUE #( mandt          = sy-mandt
                                        bukrs          = lv_eway_details-bukrs
                                        distrb         = lv_eway_details-distrb
                                        dealer         = lv_eway_details-dealer
                                        doctyp         = lv_eway_details-doctyp
                                        docno          = lv_eway_details-docno
                                        gjahr          = lv_eway_details-gjahr
                                        ebillno        = ls_response-eway_bill_no
                                        vehno          = lv_eway_details-vehno
                                        transid        = lv_eway_details-transid
                                        transname      = lv_eway_details-transname
                                        vehtyp         = lv_eway_details-vehtyp
                                        transmode      = lv_eway_details-transmode
                                        distance       = lv_eway_details-distance
                                        transdoc       = lv_eway_details-transdoc
                                        transdt        = lv_eway_details-transdt
                                        status         =  'C'
                                        ernam          = sy-uname
                                        erdat          = sy-datum
                                        erzet          = sy-uzeit
                                        cancelled_on   = |{ ls_response-canceldate+6(4) }{ ls_response-canceldate+3(2) }{ ls_response-canceldate+0(2) }|
                                        cancelled_at   = ls_time_int
                                        cancel_remarks = cancel_reason
                                    ).
        ELSE.
          " Error Details
          IF message IS INITIAL.
            message = lv_response.
*           IF message CS '728'.
*             lv_eway_details-status = 'E'.
*             iv_eway_trans_dtls = CORRESPONDING #( lv_eway_details ).
*             ENDIF.
            IF message IS INITIAL.
              SPLIT lv_response AT '"ErrorDetails":' INTO v_str message.
            ENDIF.
          ENDIF.
        ENDIF.
      ELSE.
        message = |Error Code: { lv_codes } - { lv_http_error }|.
      ENDIF.
    ELSE.
      message = 'URL is not maintained in STVARV'.
    ENDIF.

    CREATE OBJECT lo_log_upd.
    IF message IS NOT INITIAL.
      lv_body = |"message":"{ message }"|.
      CONCATENATE '[{' lv_body '}]' INTO lv_body.
    ELSE.
** serialize the output for response ***
      /ui2/cl_json=>serialize(
      EXPORTING
       data         =  ls_response
       pretty_name  = /ui2/cl_json=>pretty_mode-user
      RECEIVING
       r_json         = lv_body ).
    ENDIF.

*Output Entry in Log Table
    CALL METHOD lo_log_upd->log_entry_store
      EXPORTING
        apiname         = 'SARAL_CANCEL_EWAY_BY_IRN'
        ijson           = iv_payload_json
        ojson           = lv_body
        distributor     = iv_eway_trans_dtls-distrb
        retailer        = iv_eway_trans_dtls-dealer
      EXCEPTIONS
        apiname_missing = 1
        json_missing    = 2
        OTHERS          = 3.

  ENDMETHOD.
ENDCLASS.
