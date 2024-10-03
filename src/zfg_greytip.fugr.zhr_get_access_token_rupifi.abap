FUNCTION zhr_get_access_token_rupifi.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(FOCRCE_GEN) TYPE  XFELD OPTIONAL
*"     VALUE(CUSTYPE) TYPE  ZFINCODE
*"  EXPORTING
*"     VALUE(ACCESS_TOKEN) TYPE  ZRIAUTHSTR
*"  EXCEPTIONS
*"      COMMUNICATION_FAILURE
*"      STATUS_FAILURE
*"----------------------------------------------------------------------

  TYPES: BEGIN OF ty_response,
           accessToken TYPE string,
           tokenType   TYPE string,
           expiryTime  TYPE string,
         END OF ty_response.

  DATA: ls_response TYPE ty_response.

  DATA: lt_table TYPE STANDARD TABLE OF zhr_token_rupifi,
        ls_table TYPE zhr_token_rupifi.

  DATA: ls_tvarvc  TYPE tvarvc,
        ls_tvarvc1 TYPE tvarvc,
        ls_tvarvc2 TYPE tvarvc.

  DATA: lt_ZFI_CF_PARAMS TYPE TABLE OF zfi_cf_params,
        ls_ZFI_CF_PARAMS TYPE zfi_cf_params.

  DATA: str1 TYPE string,
        str2 TYPE string.

  REFRESH lt_table.
  CLEAR ls_table.

  DATA: lv_generate TYPE boolean.

  DATA: lv_url     TYPE string,
        lv_merchid TYPE string,
        lv_mercsec TYPE string.

  DATA: lv_body TYPE string.
  DATA: lo_http_client TYPE REF TO if_http_client.
  DATA: lv_response TYPE string.
  DATA: lr_data TYPE REF TO data.

  FIELD-SYMBOLS:
    <data>        TYPE data,
    <results>     TYPE any,
    <structure>   TYPE any,
    <table>       TYPE ANY TABLE,
    <field>       TYPE any,
    <field_value> TYPE data.


  CLEAR lv_generate.
  IF focrce_gen = abap_true.
    lv_generate = abap_true.
  ELSE.

*check is authentication is already existing.
    CLEAR ls_table.
    SELECT SINGLE * FROM zhr_token_rupifi INTO ls_table WHERE zcustype = custype.
    IF ls_table IS NOT INITIAL.
      IF sy-datum = ls_table-date_in AND sy-uzeit LT ls_table-time.
        access_token = ls_table-access_token.
      ELSE.
        lv_generate = abap_true.
      ENDIF.
    ELSE.
      lv_generate = abap_true.
    ENDIF.

  ENDIF.

*select the config parameters from the params table
  SELECT * FROM zfi_cf_params INTO TABLE lt_ZFI_CF_PARAMS WHERE zsystem = sy-sysid.

  IF lv_generate = abap_true.
*get URL
    CLEAR ls_ZFI_CF_PARAMS.
    READ TABLE lt_ZFI_CF_PARAMS INTO ls_ZFI_CF_PARAMS WITH KEY zsystem = sy-sysid zcustype = 'RF' zfldtype = 'RAURL'.
    IF sy-subrc = 0.
      CLEAR: lv_url.
      lv_url = ls_ZFI_CF_PARAMS-zfldvalue.
      CONDENSE lv_url.
    ELSE.
      CLEAR access_token.
      EXIT.
    ENDIF.

    cl_http_client=>create_by_url(
     EXPORTING
     url = lv_url
     IMPORTING
     client = lo_http_client
     EXCEPTIONS
     argument_not_found = 1
     plugin_not_active = 2
     internal_error = 3
     OTHERS = 4 ).


    CHECK lo_http_client IS BOUND.
    lo_http_client->propertytype_logon_popup = if_http_client=>co_disabled.

*get merchentid
    CLEAR: ls_ZFI_CF_PARAMS, lv_merchid.
    READ TABLE lt_ZFI_CF_PARAMS INTO ls_ZFI_CF_PARAMS WITH KEY zsystem = sy-sysid zcustype = 'RF' zfldtype = 'RAMID'.
    IF sy-subrc = 0.
      lv_merchid = ls_ZFI_CF_PARAMS-zfldvalue.
      CONDENSE lv_merchid.
    ENDIF.

* get Merchant Secret
    CLEAR: ls_ZFI_CF_PARAMS, lv_mercsec.
    READ TABLE lt_ZFI_CF_PARAMS INTO ls_ZFI_CF_PARAMS WITH KEY zsystem = sy-sysid zcustype = 'RF' zfldtype = 'RAMSC'.
    IF sy-subrc = 0.
      lv_mercsec = ls_ZFI_CF_PARAMS-zfldvalue.
      CONDENSE lv_mercsec.
    ENDIF.

    CLEAR lv_body.
    CONCATENATE '{"merchantId":"' lv_merchid '","merchantSecret":"' lv_mercsec '"}' INTO lv_body.

    lo_http_client->request->set_method(
     EXPORTING
     method = if_http_entity=>co_request_method_post ).


    lo_http_client->request->set_content_type(
     EXPORTING
     content_type = if_rest_media_type=>gc_appl_json ).



    lo_http_client->request->set_cdata(
     EXPORTING
     data = lv_body ).

    lo_http_client->send(
     EXCEPTIONS
     http_communication_failure = 1
     http_invalid_state = 2 ).

    IF sy-subrc NE 0.
      RAISE communication_failure.
    ENDIF.

    CHECK sy-subrc = 0.

    lo_http_client->receive(
     EXCEPTIONS
     http_communication_failure = 1
     http_invalid_state = 2
     http_processing_failed = 3 ).

    DATA lv_code TYPE i.

    lo_http_client->response->get_status(
    IMPORTING
      code = lv_code ).

    IF lv_code = 200.

      CLEAR lv_response.
      lv_response = lo_http_client->response->get_cdata( ).

      CALL METHOD /ui2/cl_json=>deserialize
        EXPORTING
          json         = lv_response
          pretty_name  = /ui2/cl_json=>pretty_mode-user
          assoc_arrays = abap_true
        CHANGING
          data         = ls_response.

      IF ls_response IS NOT INITIAL.
        clear: ls_table.
        ls_table-mandt = sy-mandt.
        ls_table-access_token = ls_response-accesstoken.
        ls_table-zcustype = custype.

*add 23 hours to the current time
        CALL FUNCTION 'C14B_ADD_TIME'
          EXPORTING
            i_starttime = sy-uzeit
            i_startdate = sy-datum
            i_addtime   = '230000'
          IMPORTING
            e_endtime   = ls_table-time
            e_enddate   = ls_table-date_in.

        DELETE FROM zhr_token_rupifi.
        COMMIT WORK AND WAIT.

        INSERT zhr_token_rupifi FROM ls_table.
        COMMIT WORK AND WAIT.

        access_token = ls_table-access_token.
      ENDIF.
    ELSE.
      CLEAR lv_response.
      lv_response = lo_http_client->response->get_cdata( ).

      RAISE communication_failure.
    ENDIF.
  ENDIF.



ENDFUNCTION.
