FUNCTION ysd_fm_irn_cancel.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IM_IRN) TYPE  J_1IG_IRN
*"     REFERENCE(IM_VBELN) TYPE  VBELN_VF
*"  EXPORTING
*"     REFERENCE(EX_ERROR) TYPE  STRING
*"     REFERENCE(EX_CANCEL_OUT) TYPE  ZSD_IRN_COUT
*"  TABLES
*"      T_ERROR_MSG TYPE  BAPIRET2_T
*"  EXCEPTIONS
*"      UPDATE_ERROR
*"----------------------------------------------------------------------
  DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
  CREATE OBJECT lo_log_upd.

  TYPES : BEGIN OF ty_final_data,
            success(2)    TYPE c,
            ackno         TYPE char15,
            ackdt         TYPE char20,
            irn(64)       TYPE c,
            signedinvoice TYPE string,
            signedqrcode  TYPE string,
            status        TYPE char03,
            ewbno         TYPE char15,
            ewbdt         TYPE char20,
            ewbvalidtill  TYPE char20,
          END OF ty_final_data.

  DATA : http_client TYPE REF TO if_http_client,
         lr_data     TYPE REF TO data.

  DATA : lv_http_error_descr  TYPE string,
         response             TYPE string,
         lv_http_return_code  TYPE i,
         lv_werks             TYPE werks_d,
         lv_branch            TYPE j_1bbranc_,
         lv_gstin             TYPE j_1igstcd3,
         wa_url               TYPE zeinv_url,
         wa_yzurl             TYPE yzeinv_url,
         lv_body_irn          TYPE string,
         lv_body_gstin        TYPE string,
         lv_body_cnlrsn       TYPE string,
         lv_body_cnlrem       TYPE string,
         lv_final_body        TYPE string,
         lv_string            TYPE string,
         lv_srt1              TYPE string,
         lv_resp1             TYPE string,
         lv_resp2             TYPE string,
         lv_resp3             TYPE string,
         lv_success           TYPE string,
         lv_success1          TYPE string,
         lv_irn               TYPE j_1ig_irn,
         lv_irn3              TYPE j_1ig_irn,
         lv_irn4              TYPE j_1ig_irn,
         lv_date3             TYPE j_1ig_canc_date,
         lv_date4             TYPE j_1ig_canc_date,
         lv_cdate             TYPE j_1ig_canc_date,
         v_msg                TYPE c,  "string,
         gv_geturl            TYPE string,
         lv_ok                TYPE string,
         lv_body_cd           TYPE string,
         lv_body_efusername   TYPE string,
         lv_body_efpassword   TYPE string,
         lv_body_einvusername TYPE string,
         lv_body_einvpassword TYPE string.




  DATA: lc_msg TYPE REF TO cx_salv_msg.

  CONSTANTS : c_curly_brackets_open  TYPE char01           VALUE '{',
              c_curly_brackets_close TYPE           char01 VALUE '}',
              c_cnl                  TYPE j_1ig_irn_status VALUE 'CNL',
              c_update_error         TYPE char16           VALUE 'Updated in error'.


  IF im_irn IS NOT INITIAL.

    CLEAR: gv_url,gv_ownerid,lv_success,lv_success1,lv_irn,lv_irn4,lv_ok,lv_irn4,lv_date3,lv_date4.

*******Owner id's against GSTIN *******************
* SELECT SINGLE werks FROM vbrp INTO @DATA(lv_werks) WHERE vbeln = @im_vbeln.
    SELECT werks UP TO 1 ROWS FROM vbrp INTO lv_werks WHERE vbeln = im_vbeln ORDER BY PRIMARY KEY.
    ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation

    "Get the Seller Gstin--->>
    IF lv_werks IS NOT INITIAL.
      SELECT SINGLE j_1bbranch FROM t001w
*             INTO @DATA(lv_branch)
             INTO lv_branch
*             WHERE werks EQ @lv_werks.
             WHERE werks EQ lv_werks.
    ENDIF.

    IF lv_branch IS NOT INITIAL.
      SELECT  gstin    "Seller GSTIN
        UP TO 1 ROWS FROM j_1bbranch
*          INTO @DATA(lv_gstin)
        INTO lv_gstin
*          WHERE branch = @lv_branch.
        WHERE branch =  lv_branch ORDER BY PRIMARY KEY.
      ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation
    ENDIF.

*  lv_gstin  = gv_gstin.      "For Sandbox GSTIN 29   Comment while moving PRD
    gv_gstin = lv_gstin.      "For LIve PRD GSTIN    Uncomment while moving PRD

*    IF gv_ownerid IS INITIAL.        "GSTIN
*      SELECT SINGLE * FROM zeinv_url INTO wa_url WHERE gstin = lv_gstin.
*    ENDIF.

    SELECT SINGLE * FROM yeinv_url INTO wa_url WHERE gstin = lv_gstin AND ownerid = '1695487' .

    SELECT SINGLE * FROM yzeinv_url INTO wa_yzurl WHERE gstin = lv_gstin AND ownerid = '1695487' .



    IF wa_url IS NOT INITIAL.
      gv_gstin = wa_url-gstin .
      gv_ownerid  = wa_url-ownerid.
*      gv_auth     = wa_url-authtoken.
      gv_url      = wa_url-zirn_cnl.
      gv_geturl   = wa_url-zirn_geturl.
      CONDENSE gv_url NO-GAPS.
    ENDIF.

    IF wa_yzurl IS NOT INITIAL.
      gv_efusername = wa_yzurl-efusername .
      gv_efpassword = wa_yzurl-efpassword.
      gv_einvusername = wa_yzurl-einvusername.
      gv_einvpassword = wa_yzurl-einvpassword.
    ENDIF.

    CLEAR: lv_branch,lv_gstin, lv_werks.
    lv_body_gstin  = |"GSTIN":"{ gv_gstin }", |.
    lv_body_irn    = |"irn":"{ im_irn }",|.
    lv_body_cnlrsn = | "CnlRsn": "1",|.
    lv_body_cnlrem = | "CnlRem": "Wrong Invoice",|.
    lv_body_cd = |"CDKey":"{ gv_ownerid }",|.
    lv_body_efusername = |"EFUserName":"{ gv_efusername }",|.
    lv_body_efpassword = |"EFPassword":"{ gv_efpassword }",|.
    lv_body_einvusername = |"EInvUserName":"{ gv_einvusername }",|.
    lv_body_einvpassword = |"EInvPassword":"{ gv_einvpassword }"|.

    IF syst-sysid = 'DEV' OR syst-sysid = 'QAS'.          "Development & Quality Sandbox
      lv_final_body  = | { c_curly_brackets_open } "Push_Data_List": { c_curly_brackets_open }"Data":[ { c_curly_brackets_open }{ lv_body_irn }{ lv_body_gstin }{ lv_body_cnlrsn }{ lv_body_cnlrem }{ lv_body_cd }{
lv_body_efusername }{ lv_body_efpassword }{ lv_body_einvusername }{ lv_body_einvpassword }{ c_curly_brackets_close }]{ c_curly_brackets_close } { c_curly_brackets_close }|.
    ELSE.                                                 "Production
      lv_final_body  = | { c_curly_brackets_open } "Push_Data_List": { c_curly_brackets_open }"Data":[ { c_curly_brackets_open }{ lv_body_irn }{ lv_body_gstin }{ lv_body_cnlrsn }{ lv_body_cnlrem }{ lv_body_cd }{
lv_body_efusername }{ lv_body_efpassword }{ lv_body_einvusername }{ lv_body_einvpassword }{ c_curly_brackets_close }]{ c_curly_brackets_close } { c_curly_brackets_close }|.
    ENDIF.

*PASS the Token GSTIN Owner id from SAP Customized Table to Cancel the IRN
    TRY .
        CONDENSE lv_final_body NO-GAPS.
        cl_http_client=>create_by_url( EXPORTING url = gv_url IMPORTING client = http_client ).

        IF syst-sysid = 'DEV' OR syst-sysid = 'QAS'.          "Development & Quality Sandbox
          http_client->request->set_method( 'PUT' ).
        ELSE.                                                 "PRD
*****POST LIVE API Method********
          http_client->request->set_method( if_http_request=>co_request_method_post ).
        ENDIF.


        http_client->request->set_content_type( 'application/json; charset=utf-8' ).
******Header Details JSON **********
*        HTTP_CLIENT->REQUEST->SET_HEADER_FIELD( EXPORTING  NAME  = 'x-cleartax-auth-token' VALUE = GV_AUTH ).
*        HTTP_CLIENT->REQUEST->SET_HEADER_FIELD( EXPORTING  NAME  = 'x-cleartax-product​' VALUE = | EInvoice | ).
*        HTTP_CLIENT->REQUEST->SET_HEADER_FIELD( EXPORTING  NAME  = 'gstin'           VALUE = GV_GSTIN ).
*        HTTP_CLIENT->REQUEST->SET_HEADER_FIELD( EXPORTING  NAME  = 'owner_id'        VALUE = GV_OWNERID ).
        http_client->request->set_cdata( lv_final_body ).
        http_client->send( ).
      CATCH cx_salv_msg INTO lc_msg .                   "#EC NO_HANDLER
*        DATA(lv_string) = lc_msg->get_text( ).   "
        lv_string = lc_msg->get_text( ).   "
        ex_error = lv_string.
        EXIT.
    ENDTRY.
* Disable SAP's pop-up for user id and password:
    http_client->propertytype_logon_popup = http_client->co_disabled.    "  POPUP for user id and pwd

    CALL METHOD http_client->receive
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3.
    http_client->response->get_status( IMPORTING code   = lv_http_return_code ).
    http_client->response->get_status( IMPORTING reason = lv_http_error_descr ).

**calling API
    response = http_client->response->get_cdata( ).

*Added by: Samsudeen M
*Added On: 01.09.2023
    CALL METHOD lo_log_upd->log_entry_store
      EXPORTING
        apiname         = 'TECHIRNCANC'
        ijson           = lv_final_body
        ojson           = response
      EXCEPTIONS
        apiname_missing = 1
        json_missing    = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.

    ENDIF.

    SPLIT response AT '"Success":"' INTO lv_srt1 v_msg.

    IF v_msg = 'N'.  "IRN cancellation error
      PERFORM error_msg TABLES t_error_msg USING response.
    ENDIF.

    IF v_msg = 'Y'.
**
      IF lv_http_return_code = '200' AND lv_http_error_descr = 'OK'.


        IF syst-sysid = 'DEV' OR syst-sysid = 'QAS'.        "RESPONSE SANDBOX
          SPLIT response AT 'govt_response' INTO lv_resp1 lv_resp2 lv_resp3.
          SPLIT lv_resp2 AT '"Success":"'    INTO lv_success lv_success1.
          lv_ok = lv_success1+0(1).
          SPLIT lv_resp2 AT '"Irn":"'        INTO lv_irn3 lv_irn4.
          lv_irn = lv_irn4+0(64).
          SPLIT lv_resp2 AT '"CancelDate":"' INTO lv_date3 lv_date4.
          lv_cdate = lv_date4+0(19).
        ELSE.                                             "RESPONSE PRD
          SPLIT response AT '"Success":"'    INTO lv_success lv_success1.
          lv_ok = lv_success1+0(1).
          SPLIT response AT '"Irn":"'        INTO lv_irn3 lv_irn4.
          lv_irn = lv_irn4+0(64).
          SPLIT response AT '"CancelDate":"' INTO lv_date3 lv_date4.
          lv_cdate = lv_date4+0(19).
        ENDIF.
      ENDIF.
      IF lv_success = 'N'.
        PERFORM error_msg TABLES t_error_msg USING response.
      ENDIF.

      IF lv_ok = 'Y'.
        ex_cancel_out-irn          = lv_irn.
        ex_cancel_out-irn_status   = c_cnl.
        ex_cancel_out-cancel_date  = lv_cdate.

**update the J_1IG_INVREFNUM table
        IF lv_irn IS NOT INITIAL AND lv_cdate IS NOT INITIAL.
          UPDATE j_1ig_invrefnum
           SET irn_status  = ex_cancel_out-irn_status
               cancel_date =  ex_cancel_out-cancel_date
           WHERE irn = im_irn.
          IF sy-subrc EQ 0.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = 'X'.
          ELSE.
            RAISE update_error.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    CLEAR: lv_final_body.
  ENDIF.
ENDFUNCTION.
*&---------------------------------------------------------------------*
*&      Form  error_msg
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ERROR_MSG  text
*      -->P_RESPONSE   text
*----------------------------------------------------------------------*
FORM error_msg  TABLES p_error_msg TYPE bapiret2_t
                USING  p_response  TYPE string.
  .
******Error Messages***********
  DATA: v_errormsg  TYPE char40,
        v_errorcode TYPE char4,
        v_str       TYPE string,
        v_str1      TYPE string,
        v_msg       TYPE c,
        v_str2      TYPE string.

  TYPES: BEGIN OF t_error,
           error_code    TYPE  string,
           error_message TYPE string,
           error_source  TYPE string,
         END OF t_error.

  DATA: it_bapiret TYPE TABLE OF bapiret2,
        wa_bapiret TYPE bapiret2,
        it_error   TYPE TABLE OF t_error,
        wa_error   TYPE t_error.

  SPLIT p_response AT '},"group_id"' INTO v_str v_errorcode.
  SPLIT v_str AT '"ErrorDetails":' INTO v_str1 v_str2.

  CLEAR:v_str.
  SPLIT v_str2 AT '"error_code":"'   INTO v_str wa_error-error_code.
  CLEAR: v_str.
  SPLIT v_str2 AT '"error_message":"' INTO v_str1 wa_error-error_message.
  CLEAR: v_str.
  SPLIT v_str2 AT '"error_source":"' INTO v_str1 wa_error-error_source.

*  /ui2/cl_json=>deserialize( EXPORTING json = v_str2
*                               pretty_name = /ui2/cl_json=>pretty_mode-camel_case
*                      CHANGING data = it_error ).
*  LOOP AT it_error INTO wa_error.
  wa_bapiret-id         = wa_error-error_code.
  wa_bapiret-message    = wa_error-error_message.
  wa_bapiret-message_v1 = wa_error-error_source.
  APPEND wa_bapiret TO p_error_msg.
  CLEAR wa_bapiret.
*  ENDLOOP.

ENDFORM.                    "error_msg
