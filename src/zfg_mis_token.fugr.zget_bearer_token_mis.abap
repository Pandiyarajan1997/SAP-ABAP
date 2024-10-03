FUNCTION zget_bearer_token_mis.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(USERNAME) TYPE  SYST_UNAME
*"     REFERENCE(PASSWORD) TYPE  CHAR50
*"  EXPORTING
*"     REFERENCE(BEARER_TOKEN) TYPE  STRING
*"     REFERENCE(MSG) TYPE  STRING
*"  EXCEPTIONS
*"      MAINTAIN_URL_LINK
*"      INPUT_ERROR
*"----------------------------------------------------------------------
  "Created by: Samsudeen M
  "Created On: 06.04.2023
  "Purpose: To get the Common MIS Bearer Token
*------------------------------------------------------------------------
  DATA: lo_http_client TYPE REF TO if_http_client.
  DATA: lv_response   TYPE string. "API Response
  DATA :ls_json    TYPE string,
        v_jsonload TYPE string.

  IF username IS NOT INITIAL AND password IS NOT INITIAL.
    SELECT SINGLE low FROM tvarvc INTO @DATA(l_url)
                        WHERE name = 'MIS_BEARER_TOKEN_LINK'
                        AND type = 'P'.
    IF sy-subrc = 0.
      cl_http_client=>create_by_url(
      EXPORTING
      url = CONV string( l_url )
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

      DATA(l_username) = CONV string( username ).
*      SHIFT l_username LEFT DELETING LEADING '0'.
      CLEAR v_jsonload.
      CONCATENATE '{'
      '"username": "' l_username '",'
        '"password": "' password '"'
         '}' INTO v_jsonload.

      lo_http_client->request->set_cdata(
       EXPORTING
       data = v_jsonload ).


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

      CLEAR lv_response.
      IF lv_codes = 200.
        lv_response = lo_http_client->response->get_cdata( ).
        bearer_token = lv_response.
        SHIFT bearer_token LEFT DELETING LEADING '"'.
        SHIFT bearer_token RIGHT DELETING TRAILING '"'.
        CONDENSE bearer_token NO-GAPS.
      ELSE.
        msg = lv_http_error.
      ENDIF.
    ELSE.
      RAISE maintain_url_link.
    ENDIF.
  ELSE.
    RAISE input_error.
  ENDIF.
ENDFUNCTION.
