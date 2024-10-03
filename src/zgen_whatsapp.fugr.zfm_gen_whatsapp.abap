FUNCTION zfm_gen_whatsapp.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_TEMPLATE_NAME) TYPE  CHAR50
*"     VALUE(IV_MOBILE) TYPE  CHAR12
*"     VALUE(IT_TEMPLATE_DATA) TYPE  ZTT_KEY_VALUE_PAIR
*"  EXPORTING
*"     VALUE(IV_STATUS) TYPE  CHAR20
*"  EXCEPTIONS
*"      INCORRECT_MOB_NUM
*"----------------------------------------------------------------------
  TYPES:BEGIN OF ty_params,
          name  TYPE  string,
          value TYPE string,
        END OF ty_params,
        tt_params TYPE TABLE OF ty_params WITH EMPTY KEY.
  TYPES:BEGIN OF ty_template,
          template_name  TYPE string,
          broadcast_name TYPE string,
          parameters     TYPE tt_params,
        END OF ty_template.

  DATA: gs_template     TYPE  ty_template,
        gt_parameters   TYPE tt_params,
        iv_payload_json TYPE string.
  DATA: lv_wappurl TYPE string.
  DATA: lv_token TYPE string,
        lv_url   TYPE string.

  DATA: lo_http_client TYPE REF TO if_http_client,
        lv_codes       TYPE i,
        lv_http_error  TYPE string,
        lv_response    TYPE string.

  DATA: lt_tvarvc TYPE TABLE OF tvarvc,
        ls_tvarvc TYPE tvarvc.
*Select the endpoint URL
  SELECT SINGLE low INTO lv_wappurl FROM tvarvc WHERE name = 'ZVENPAY_URL' AND type = 'P'.

*select the authentication token
  SELECT * FROM tvarvc INTO TABLE lt_tvarvc WHERE name = 'ZVENPAY_WAPPTOK' AND type = 'S'.
  IF sy-subrc = 0.
    LOOP AT lt_tvarvc INTO ls_tvarvc.
      IF sy-tabix = 1.
        lv_token = ls_tvarvc-low.
      ELSE.
        CONCATENATE lv_token ls_tvarvc-low INTO lv_token.
      ENDIF.
    ENDLOOP.
  ENDIF.

  "Mobile number validation of customer
  DATA(i_len) = strlen( iv_mobile ).
  IF i_len = 10.
    iv_mobile = |91{ iv_mobile }|.
  ELSEIF i_len = 12.
    IF iv_mobile+0(2) NE 91.
      RAISE incorrect_mob_num.
    ENDIF.
  ELSE.
    RAISE incorrect_mob_num.
  ENDIF.

*  "template name to lower case
*  DATA(ls_templ_name) = to_lower( iv_template_name ).

*  READ TABLE it_template_data ASSIGNING FIELD-SYMBOL(<fs_data>) WITH KEY name = 'FILE_LINK'.
*  if sy-subrc = 0.
*  TRANSLATE <fs_data> to LOWER CASE.
*  endif.
  gs_template = VALUE #(  template_name = iv_template_name broadcast_name = iv_template_name
                         parameters = CORRESPONDING #( it_template_data ) ) .

*****************Serialize the INPUT JSON************
  /ui2/cl_json=>serialize(
  EXPORTING
    data         =  gs_template
    pretty_name  = /ui2/cl_json=>pretty_mode-low_case
  RECEIVING
    r_json         = iv_payload_json ).

*******************Pass template json to whatssapp API***********************************
  CLEAR lv_url.
  lv_url = |{ lv_wappurl }{ iv_mobile }|.

  CONDENSE lv_url.
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
  " set property logon popup disabled
  lo_http_client->propertytype_logon_popup = if_http_client=>co_disabled.

  " request set method POST
  lo_http_client->request->set_method( if_http_entity=>co_request_method_post ). " post

  " set content type application/json
  lo_http_client->request->set_content_type( EXPORTING  content_type = if_rest_media_type=>gc_appl_json ).  " JSON

  CONCATENATE 'Bearer' lv_token INTO lv_token SEPARATED BY space.

  " set bearer token in header field Authorization
  lo_http_client->request->set_header_field( EXPORTING name  =  'Authorization' value = lv_token ).

  " set payload data as cdata to request
  lo_http_client->request->set_cdata( EXPORTING data   = iv_payload_json ).

  " send request
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

  CLEAR lv_codes.
  lo_http_client->response->get_status(
  IMPORTING
    code = lv_codes ).

  lo_http_client->response->get_status(
  IMPORTING
    reason = lv_http_error ).


  CLEAR lv_response.
  lv_response = lo_http_client->response->get_cdata( ).

  IF lv_codes = 200.
    iv_status = 'SUCCESS'.
  ELSE.
    iv_status = |FAILURE :{ lv_http_error }|.
  ENDIF.

ENDFUNCTION.
