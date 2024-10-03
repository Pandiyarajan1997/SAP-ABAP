FUNCTION ysd_fm_ewaybill_cancel.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IM_VBELN) TYPE  VBELN_VF
*"     REFERENCE(IM_GSTIN) TYPE  J_1IGSTCD3 OPTIONAL
*"     REFERENCE(IM_EWBILLNO) TYPE  J_1IG_EBILLNO
*"  TABLES
*"      ERROR_MSG TYPE  BAPIRET2_T
*"----------------------------------------------------------------------
  DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
  CREATE OBJECT lo_log_upd.

  CONSTANTS : c_comma                TYPE c VALUE ',',
              c_curly_brackets_open  TYPE char01 VALUE '{',
              c_curly_brackets_close TYPE char01 VALUE '}',
              c_quatation_mark       TYPE char01 VALUE '"',
              c_colon                TYPE char01 VALUE':',
              c_version              TYPE char05 VALUE '1.01',
              c_gst                  TYPE char03 VALUE'GST',
              c_b2b                  TYPE char03 VALUE'B2B',
              c_null                 TYPE char04 VALUE'Null',
              c_box_brackets_open    TYPE char01 VALUE '[',
              c_box_brackets_close   TYPE char01 VALUE  ']'.
*               c_semicolon            TYPE char01 VALUE ';',

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


  DATA : lt_data TYPE TABLE OF ty_final_data.

  DATA : lv_body_strcture         TYPE string,
         lv_body_ewb              TYPE string,
         lv_body_cnlrsncode       TYPE string,
         lv_body_cancelrmrk       TYPE string,
         lv_final_body            TYPE string,
         token                    TYPE string,
         lv_http_error_descr      TYPE string,
         lv_http_error_descr_long TYPE xstring,
         response                 TYPE string,
         v_cancel                 TYPE char9,
         v_err                    TYPE string,
         lv_http_return_code      TYPE i,
         lv_reference             TYPE string,
         lv_ewbno                 TYPE j_1ig_ebillno,
         lv_string_qrcode_final   TYPE string,
         lv_string                TYPE string,
         lv_check1                TYPE string,
         lv_check2                TYPE string,
         wa_url                   TYPE zeinv_url,
         lv_branch                TYPE j_1bbranc_,
         lv_gstin                 TYPE j_1igstcd3,
         lv_werks                 TYPE werks_d,
         lv_ewbstatus             TYPE j_1ig_stat,
         lv_data                  TYPE string,
         lv_egen_dat              TYPE j_1ig_egendat,
         lv_mon                   TYPE string,
         lv_year                  TYPE string,
*         WA_URL             TYPE ZEINV_URL,
         wa_yzurl                 TYPE yzeinv_url.

  DATA : gv_efusername   TYPE string,
         gv_efpassword   TYPE string,
         gv_einvusername TYPE string,
         gv_einvpassword TYPE string.

  DATA:  lc_msg TYPE REF TO cx_salv_msg.
  DATA:  http_client              TYPE REF TO if_http_client.


**Create Ewaybill cancel url
  IF im_ewbillno IS NOT INITIAL.

    CLEAR: gv_eway_ownerid,lv_http_return_code,lv_http_error_descr,response,lv_ewbno,lv_ewbstatus,lv_gstin.

*    SELECT SINGLE werks FROM vbrp INTO @DATA(lv_werks) WHERE vbeln = @im_vbeln.
    SELECT werks UP TO 1 ROWS FROM vbrp INTO lv_werks WHERE vbeln = im_vbeln ORDER BY PRIMARY KEY.
    ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation
    SELECT egen_dat UP TO 1 ROWS FROM j_1ig_ewaybill INTO lv_egen_dat WHERE docno = im_vbeln ORDER BY PRIMARY KEY.
    ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation

    lv_year = lv_egen_dat(4).
    lv_mon = lv_egen_dat+4(2).


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
*        INTO @DATA(lv_gstin)
        INTO lv_gstin
        WHERE branch = lv_branch ORDER BY PRIMARY KEY.
      ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation
    ENDIF.


*****Sandbox & PRD Live GSTIN***********
    gv_eway_gstin     = lv_gstin.

******Owner id's against GSTIN *******************
******Eway Bill Cancel URL ***************

    CLEAR:gv_eway_ownerid,gv_eway_auth,gv_eway_url,lv_ewbno.

*    SELECT SINGLE * FROM zeinv_url INTO wa_url WHERE  gstin = lv_gstin.
    SELECT SINGLE * FROM yeinv_url INTO wa_url WHERE gstin = lv_gstin AND ownerid = '1550241' .

    SELECT SINGLE * FROM yzeinv_url INTO wa_yzurl WHERE gstin = lv_gstin AND ownerid = '1550241' .

    IF wa_url IS NOT INITIAL.
      gv_eway_ownerid   = wa_url-ownerid.          "Eway Bill Owner id
      gv_eway_url       = wa_url-zeway_cnl.        "Eway Bill Cancel URL
*      gv_eway_auth      = wa_url-authtoken.        "Authentication Token
    ENDIF.

    IF wa_yzurl IS NOT INITIAL.
      gv_efusername = wa_yzurl-efusername .
      gv_efpassword = wa_yzurl-efpassword.
      gv_einvusername = wa_yzurl-einvusername.
      gv_einvpassword = wa_yzurl-einvpassword.
    ENDIF.

******Cancel-1 within 24 hours
   lv_body_ewb        = |{ c_curly_brackets_open } "Push_Data_List  " : { c_box_brackets_open }{ c_curly_brackets_open }"GSTIN" :{ c_quatation_mark } { lv_gstin }{ c_quatation_mark },"EWBNumber": { c_quatation_mark }{ im_ewbillno }{ c_quatation_mark } ,|.
    lv_body_cnlrsncode = |"CancelReasonCode": "1",|.
    lv_body_cancelrmrk = | "CancelRemark": "DATA_ENTRY_MISTAKE" , "EWBUserName" : { c_quatation_mark }{ gv_einvusername } { c_quatation_mark },"EWBPassword" : { c_quatation_mark }{ gv_einvpassword }{ c_quatation_mark } { c_curly_brackets_close }{
  c_box_brackets_close } { c_comma } "Year" :{ c_quatation_mark }{ lv_year }{ c_quatation_mark }{ c_comma } "Month" : { c_quatation_mark }{ lv_mon }{ c_quatation_mark }{ c_comma } "EFUserName": { c_quatation_mark }{ gv_efusername }{ c_quatation_mark }{
  c_comma } "EFPassword": { c_quatation_mark }{ gv_efpassword }{ c_quatation_mark }{ c_comma } "CDKey": { c_quatation_mark }{ gv_eway_ownerid }{ c_quatation_mark }{ c_curly_brackets_close }|.
    lv_final_body     = | { lv_body_ewb } { lv_body_cnlrsncode } { lv_body_cancelrmrk }|.

    IF gv_eway_url IS NOT INITIAL.
      TRY .
          CONDENSE lv_final_body NO-GAPS.
          cl_http_client=>create_by_url( EXPORTING url = gv_eway_url IMPORTING client = http_client ).
          http_client->request->set_method( if_http_request=>co_request_method_post ).
          http_client->request->set_content_type( 'application/json; charset=utf-8' ).
******Header Details JSON **********
*        HTTP_CLIENT->REQUEST->SET_HEADER_FIELD( EXPORTING  NAME  = 'x-cleartax-auth-token' VALUE = GV_EWAY_AUTH ).
*        HTTP_CLIENT->REQUEST->SET_HEADER_FIELD( EXPORTING  NAME  = 'x-cleartax-product​' VALUE = | EInvoice | ).
*        HTTP_CLIENT->REQUEST->SET_HEADER_FIELD( EXPORTING  NAME  = 'gstin' VALUE = GV_EWAY_GSTIN ).
*        HTTP_CLIENT->REQUEST->SET_HEADER_FIELD( EXPORTING  NAME  = 'owner_id' VALUE = GV_EWAY_OWNERID ).
          http_client->request->set_cdata( lv_final_body ).
          http_client->send( ).
        CATCH cx_salv_msg INTO lc_msg .                 "#EC NO_HANDLER
          lv_string = lc_msg->get_text( ).   " <
          MESSAGE lv_string TYPE 'I'.  "<
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

*****Calling API*
      response = http_client->response->get_cdata( ).
    ELSE.
      wa_bapiret-message    = 'No URL Found'.
      APPEND wa_bapiret TO error_msg.
      CLEAR:wa_bapiret.
    ENDIF.

*Added By: Samsudeen M
*Added On: 01.09.2023
    CALL METHOD lo_log_upd->log_entry_store
      EXPORTING
        apiname         = 'TECHEWAYCANC'
        ijson           = lv_final_body
        ojson           = response
      EXCEPTIONS
        apiname_missing = 1
        json_missing    = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.

    ENDIF.

    IF lv_http_return_code = '200' AND lv_http_error_descr = 'OK'.
      CLEAR:v_cancel,lv_final_body.
      SPLIT response AT 'ewbStatus":"' INTO lv_check1 v_cancel.
      SPLIT response AT '"errorDetails":' INTO lv_check2 v_err.

* Ewaybill cancel status  for Einvoice API
      IF v_err = 'null}'  OR v_err = 'null'.
        CLEAR: v_err.
      ENDIF.

      IF v_err IS NOT INITIAL AND  ( v_err <> 'null}'  OR v_err <> 'null' ).
        REPLACE FIRST OCCURRENCE OF '}'  IN v_err WITH space.
        IF v_err IS NOT INITIAL.
          PERFORM error_msg1 TABLES error_msg USING v_err.
        ENDIF.
      ELSE.
        IF v_cancel = 'CANCELLED'.
          lv_ewbstatus = 'C'.
          SPLIT response AT '"ewbNumber":' INTO lv_data lv_ewbno.
        ENDIF.
      ENDIF.

    ELSE.
      PERFORM error_msg1 TABLES error_msg USING response.
    ENDIF.

* UPDATE in J_1IG_EWAYBILL Table
    IF lv_ewbno IS NOT INITIAL.
      UPDATE  j_1ig_ewaybill  SET status  = lv_ewbstatus
                                    aenam = sy-uname      "Last Person Changed for Cancelled
                                    aedat = sy-datum      "Last Changed date
                              WHERE ebillno = im_ewbillno.

      IF sy-subrc  EQ 0.
        COMMIT WORK .
        WAIT UP TO 1 SECONDS.
      ENDIF.
    ENDIF.

  ENDIF.




ENDFUNCTION.
