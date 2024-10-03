FUNCTION zhr_get_access_token.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(FOCRCE_GEN) TYPE  XFELD OPTIONAL
*"  EXPORTING
*"     REFERENCE(ACCESS_TOKEN) TYPE  ZACCESS
*"  EXCEPTIONS
*"      COMMUNICATION_FAILURE
*"      STATUS_FAILURE
*"----------------------------------------------------------------------
  DATA: lt_table TYPE STANDARD TABLE OF zhr_access_token,
        ls_table TYPE zhr_access_token.

  DATA: ls_tvarvc  TYPE tvarvc,
        ls_tvarvc1 TYPE tvarvc,
        ls_tvarvc2 TYPE tvarvc.

  DATA: str1 TYPE string,
        str2 TYPE string.

  REFRESH lt_table.
  CLEAR ls_table.

  DATA: lv_generate TYPE boolean.

  DATA lv_url TYPE string.
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
    SELECT * FROM zhr_access_token INTO TABLE lt_table.
    IF lt_table[] IS NOT INITIAL.
      READ TABLE lt_table INTO ls_table INDEX 1.
      IF sy-subrc EQ 0.
        IF sy-datum = ls_table-date_in AND sy-uzeit LT ls_table-time.
          access_token = ls_table-access_token.
        ELSE.
          lv_generate = abap_true.
        ENDIF.
      ENDIF.
    ELSE.
      lv_generate = abap_true.
    ENDIF.

  ENDIF.



  IF lv_generate = abap_true.

    CLEAR ls_tvarvc.
    SELECT SINGLE * FROM tvarvc INTO ls_tvarvc WHERE name = 'URL_AUTH_TOKEN'
                                               AND type = 'P'.
*initial steps
    CLEAR: lv_url.
    lv_url = ls_tvarvc-low.

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

    CLEAR ls_tvarvc1.
    SELECT SINGLE * FROM tvarvc INTO ls_tvarvc1 WHERE name = 'URL_AUTH_TOKEN_USER'
                                                AND type = 'P'.
    CLEAR str1.
    str1 = ls_tvarvc1-low.

    CLEAR ls_tvarvc2.
    SELECT SINGLE * FROM tvarvc INTO ls_tvarvc2 WHERE name = 'URL_AUTH_TOKEN_PASS'
                                                AND type = 'P'.
    CLEAR str2.
    str2 = ls_tvarvc2-low.

    lo_http_client->authenticate(
    EXPORTING
     username = str1
     password = str2 ).

    lo_http_client->request->set_method(
     EXPORTING
     method = if_http_entity=>co_request_method_post ).


    lo_http_client->request->set_content_type(
     EXPORTING
     content_type = if_rest_media_type=>gc_appl_json ).


    lo_http_client->request->set_cdata(
     EXPORTING
     data = '{ JSON_Payload }' ).

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
          data         = lr_data.

      IF lr_data IS BOUND.
        ASSIGN lr_data->* TO <data>.
        ASSIGN COMPONENT `access_token` OF STRUCTURE <data> TO <field>.
        IF <field> IS ASSIGNED.
          lr_data = <field>.
          ASSIGN lr_data->* TO <field_value>.
          ls_table-access_token = <field_value>.
        ENDIF.
        UNASSIGN: <field>, <field_value>.

        CALL FUNCTION 'C14B_ADD_TIME'
          EXPORTING
            i_starttime = sy-uzeit
            i_startdate = sy-datum
            i_addtime   = '005500'
          IMPORTING
            e_endtime   = ls_table-time
            e_enddate   = ls_table-date_in.

        DELETE FROM zhr_access_token.
        COMMIT WORK AND WAIT.

        INSERT zhr_access_token FROM ls_table.
        COMMIT WORK AND WAIT.

        access_token = ls_table-access_token.
      ENDIF.
    ELSE.
      RAISE communication_failure.
    ENDIF.
  ENDIF.



ENDFUNCTION.
