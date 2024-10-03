*&---------------------------------------------------------------------*
*& Include          ZYMARD_NSAP_UPD_DMSSTK_FORMS
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form api_call
*&---------------------------------------------------------------------*
FORM f_dms_apicall .
** getting api Link from tvarvc table **
  SELECT SINGLE * FROM tvarvc INTO @DATA(ls_tvarvc)
                  WHERE name = 'ZYMARD_NSAP_API'
  AND type = 'P'.
  IF sy-subrc EQ 0.
    "Actual API Link
    CLEAR lv_url.
    lv_url = ls_tvarvc-low.
  ENDIF.
** Date Conversion For API input **
  CLEAR lv_date.
  lv_date = |{ p_date(4) }-{ p_date+4(2) }-{ p_date+6(2) }|..

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


  lo_http_client->request->set_method(
   EXPORTING
   method = if_http_entity=>co_request_method_post ).


  lo_http_client->request->set_content_type(
   EXPORTING
   content_type = if_rest_media_type=>gc_appl_json ).

  "Header Data Fields for API
  DATA(lv_bearer_token) = |Bearer { gv_token }|.
  lo_http_client->request->set_header_field(  EXPORTING  name  = 'Authorization'  value = lv_bearer_token ).

  lo_http_client->request->set_cdata(
    EXPORTING
    data = '{ JSON_Payload }' ).

  DATA :ls_json    TYPE string,
        v_jsonload TYPE string.
** Input JSON for API **
  CONCATENATE '{'
              '"filtervalue1":"' lv_date'"'
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

** API response Code **
  lo_http_client->response->get_status(
  IMPORTING
    code = lv_codes ).
** API response error Description **
  lo_http_client->response->get_status(
  IMPORTING
    reason = lv_http_error ).

  CLEAR lv_response.
  IF lv_codes = 200.
** Actual API Response **
    lv_response = lo_http_client->response->get_cdata( ).
  ENDIF.

  REPLACE ALL OCCURRENCES OF 'u0022'   IN lv_response WITH '"'.
  REPLACE ALL OCCURRENCES OF '\'  IN lv_response WITH ''.
  REPLACE ALL OCCURRENCES OF 'rn'  IN lv_response WITH ''.
  REPLACE ALL OCCURRENCES OF '/'  IN lv_response WITH ''.
  REPLACE ALL OCCURRENCES OF '[]'  IN lv_response WITH ''.

** Serialize API Response to JSON Format **
  CLEAR lv_response1.
  CALL METHOD /ui2/cl_json=>serialize
    EXPORTING
      data        = lv_response
      pretty_name = /ui2/cl_json=>pretty_mode-user
    RECEIVING
      r_json      = lv_response1.


  REPLACE ALL OCCURRENCES OF '\'  IN lv_response1 WITH ''.
  REPLACE ALL OCCURRENCES OF '/'  IN lv_response1 WITH ''.
  SHIFT lv_response1 LEFT DELETING LEADING '"'.

**Deserialize Converted JSON to Internal Table **
  /ui2/cl_json=>deserialize(
  EXPORTING
   json         = lv_response1
   pretty_name  = /ui2/cl_json=>pretty_mode-user
  CHANGING
   data         = gt_stock ). "Stock API Table

  SORT gt_stock[] BY distributor_code item_code.
  DESCRIBE TABLE gt_stock[] LINES lv_lines. "Count of Internal Table
** Run For Specific Customer **
  IF s_kunnr IS NOT INITIAL.
    DELETE gt_stock WHERE distributor_code NOT IN s_kunnr.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_insert_data
FORM f_insert_data .
** Variable for Stock Calculation **
  DATA: lv_totalstk TYPE labst.
** Taking Data based on entered Data **
  SELECT * FROM ymard_nsap INTO TABLE gt_ymard_nsap
                           WHERE erdat EQ p_date.
  IF sy-subrc EQ 0.
    SORT gt_ymard_nsap[] BY kunnr matnr.
  ENDIF.
  IF gt_stock[] IS NOT INITIAL.
    SELECT kunnr FROM knvv INTO TABLE @DATA(lt_knvv)
                 WHERE kdgrp EQ '09'.
    IF sy-subrc EQ 0.
      SORT lt_knvv[] BY kunnr.
      DELETE ADJACENT DUPLICATES FROM lt_knvv[] COMPARING kunnr.
    ENDIF.
  ENDIF.
** Added on 07.11.2022 for making Pending Order Zero **
  SELECT SINGLE * FROM tvarvc INTO @DATA(gs_update) WHERE name = 'ZUPD_DMSSTK'
                                                    AND type = 'P'.

************Added on 12.03.2024 get the distributor plant************
  SELECT kunnr,werks FROM kna1 INTO TABLE @DATA(lt_kna1) WHERE werks NE @abap_false.
  IF sy-subrc = 0.
    SORT : lt_kna1 BY kunnr.
  ENDIF.

  LOOP AT gt_stock INTO gs_stock.
**distributor Code / Customer Number Conversion ***
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gs_stock-distributor_code
      IMPORTING
        output = gs_stock-distributor_code.
** If the Distributor code belong to SKU only **
    READ TABLE lt_knvv INTO DATA(ls_knvv) WITH KEY kunnr = gs_stock-distributor_code BINARY SEARCH.
    IF sy-subrc EQ 0.
** Spliting Date And time into SAP **
      SPLIT gs_stock-lcreateddate AT 'T' INTO DATA(lv_date1) DATA(lv_time).
      REPLACE ALL OCCURRENCES OF '-' IN lv_date1 WITH ''.
      CLEAR gs_ymard_nsap.
      LOOP AT gt_ymard_nsap INTO gs_ymard_nsap WHERE kunnr = gs_stock-distributor_code
                                               AND matnr = gs_stock-item_code
                                               AND erdat = lv_date1.
        IF sy-subrc EQ 0.
          CONTINUE.
        ENDIF.
      ENDLOOP.
      IF sy-subrc NE 0.
**distributor Code / Customer Number Conversion ***
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = gs_stock-distributor_code
          IMPORTING
            output = gs_stock-distributor_code.
** Item Code / Material Code Conversion **
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = gs_stock-item_code
          IMPORTING
            output = gs_stock-item_code.

        gs_ymard_nsap-kunnr = gs_stock-distributor_code. " Customer Number
        gs_ymard_nsap-matnr = gs_stock-item_code. "Material Code
** Spliting Date And time into SAP **
        SPLIT gs_stock-lcreateddate AT 'T' INTO DATA(lv_date2) DATA(lv_time1).
        REPLACE ALL OCCURRENCES OF '-' IN lv_date2 WITH ''.
        WRITE lv_date2 TO gs_ymard_nsap-erdat.
        gs_ymard_nsap-erzet = sy-uzeit.  "Created on Time
        gs_ymard_nsap-meins = gs_stock-uom. "Unit of Measure
        gs_ymard_nsap-labst = gs_stock-stock_accepted.
        gs_ymard_nsap-trame = gs_stock-stock_yet_to_be_accepted.
        CLEAR lv_totalstk.
        lv_totalstk = ( gs_stock-stock_accepted + gs_stock-stock_yet_to_be_accepted ). "Total Stk
        gs_ymard_nsap-tot_stk = lv_totalstk. "Total Calculated Stock

** If we set variable 'X' in tvarvc then pending order should be Zero **
        IF gs_update-low = 'X'.
          CLEAR  gs_ymard_nsap-pen_ord . "Pending Order
        ELSE. " If Not pending Order as on DMS
          gs_ymard_nsap-pen_ord = gs_stock-pending_order. "Pending Order
        ENDIF.
*********read the distributor plant****************
        READ TABLE lt_kna1 INTO DATA(ls_kna1) WITH KEY kunnr = gs_stock-distributor_code BINARY SEARCH.
        IF sy-subrc = 0.
          gs_ymard_nsap-werks = ls_kna1-werks.
        ENDIF.
        APPEND gs_ymard_nsap TO gt_ymard_nsap1.
** Data Insertion **
        INSERT ymard_nsap FROM gs_ymard_nsap.
        COMMIT WORK AND WAIT.
      ENDIF.
      CLEAR : gs_stock,ls_kna1.
    ENDIF.
  ENDLOOP.
** Count of updated data **
  DESCRIBE TABLE gt_ymard_nsap1[] LINES DATA(lv_lines1).
  IF sy-subrc EQ 0.
    WRITE: / 'Number of API Records:', lv_lines.
    WRITE: / 'Number of Records Updated:', lv_lines1.
    WRITE: / 'Executed On date:', sy-datum.
    WRITE: / 'Executed On Time:', sy-uzeit.
    WRITE: / 'Executed By:', sy-uname.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_bearer_token
*&---------------------------------------------------------------------*
FORM f_get_bearer_token CHANGING p_token p_msg.
  DATA : l_uname TYPE  syst_uname.
  DATA : l_password TYPE  char50.
  SELECT SINGLE low FROM tvarvc
    INTO l_password
    WHERE name = 'ZANS_UPD_PWORD'
  AND   type = 'P'.
  SELECT SINGLE low FROM tvarvc
    INTO l_uname
    WHERE name = 'ZANS_UPD_UNAME'
  AND   type = 'P'.

  CALL FUNCTION 'ZGET_BEARER_TOKEN_MIS'
    EXPORTING
      username          = l_uname
      password          = l_password
    IMPORTING
      bearer_token      = p_token
      msg               = p_msg
    EXCEPTIONS
      maintain_url_link = 1
      input_error       = 2
      OTHERS            = 3.

  IF sy-subrc <> 0.
    CASE sy-subrc.
      WHEN 1. p_msg = 'ULR Link Error'.
      WHEN 2. p_msg = 'Invalid Input'.
      WHEN OTHERS. p_msg = 'Unknown Error'.
    ENDCASE.
* Implement suitable error handling here
  ENDIF.
ENDFORM.
