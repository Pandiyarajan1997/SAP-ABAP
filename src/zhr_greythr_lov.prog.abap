
*&---------------------------------------------------------------------*
REPORT zhr_greythr_lov.

TABLES: zhr_greythr_lovf.

TYPES: BEGIN OF ty_output,
         id   TYPE string,
         name TYPE string,
       END OF ty_output.

TYPES:
  BEGIN OF ty_entry,       "Structure Foe Deseialize json
    type    TYPE i,
    subtype TYPE i,
    parent  TYPE string,
    name    TYPE string,
    value   TYPE string,
  END OF ty_entry .
TYPES:
  tty_entry_map TYPE SORTED TABLE OF ty_entry WITH UNIQUE KEY parent name .

TYPES: BEGIN OF ty_alv,   "structure For ALV
         field_code TYPE zfield_code,
         field_name TYPE zfield_name,
         text(150)  TYPE c,
       END OF ty_alv.

TYPES: BEGIN OF ty_shlp,
         field_code TYPE zfield_code,
         field_name TYPE zfield_name,
       END OF ty_shlp.

TYPES: BEGIN OF ty_branch,
         id          TYPE string,
         description TYPE string,
       END OF ty_branch.

DATA: lt_branch TYPE TABLE OF ty_branch,
      ls_branch TYPE ty_branch.

***Internal Table For Alv Display******
DATA: gt_display TYPE TABLE OF ty_alv,
      gs_display TYPE ty_alv.

DATA: lt_output TYPE TABLE OF ty_output,
      ls_output TYPE  ty_output.

DATA: gt_entries TYPE tty_entry_map,
      gs_entries TYPE ty_entry.

***Internal Table For Holding New values and already present table value******
DATA: lt_values TYPE STANDARD TABLE OF zhr_greythr_lov,
      ls_values TYPE zhr_greythr_lov.

**Internal Table For Holding New values and already present table value******
DATA: lt_values1 TYPE STANDARD TABLE OF zhr_greythr_lov,
      ls_values1 TYPE zhr_greythr_lov.

***Internal Table For Id selection based on selection screen **********
DATA: lt_lovf TYPE STANDARD TABLE OF zhr_greythr_lovf,
      ls_lovf TYPE zhr_greythr_lovf.

***Internal Table For Search Help on selection screen **********
DATA: lt_shlp TYPE STANDARD TABLE OF ty_shlp,
      ls_shlp TYPE ty_shlp.

****Structure for API Header*******
DATA: ls_tvarvc TYPE tvarvc.

DATA: gt_fcat   TYPE  slis_t_fieldcat_alv,
      gs_fcat   TYPE slis_fieldcat_alv,
      gs_layout TYPE slis_layout_alv.

DATA: lo_http_client TYPE REF TO if_http_client,
      create_url     TYPE string,
      lv_url         TYPE string.

DATA: lv_response   TYPE string, "API Response
      lv_codes      TYPE i,      "STATUS Code
      lv_http_error TYPE string. "STATUS Description

DATA: token TYPE zaccess,
      str1  TYPE string,
      str2  TYPE string.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: f_code FOR zhr_greythr_lovf-field_code.
SELECTION-SCREEN END OF BLOCK b1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR f_code-low.
  SELECT field_code field_name FROM zhr_greythr_lovf INTO TABLE lt_shlp.
  IF sy-subrc EQ 0.
    SORT lt_shlp BY field_code.
********Search help for f_code-low*******
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield    = 'FIELD_CODE'
        dynpprog    = 'ZHR_GREYTHR_LOV'
        dynpnr      = sy-dynnr
        dynprofield = 'F_CODE'
        value_org   = 'S'
      TABLES
        value_tab   = lt_shlp.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR f_code-high.
  SELECT field_code field_name FROM zhr_greythr_lovf INTO TABLE lt_shlp.
  IF sy-subrc EQ 0.
    SORT lt_shlp BY field_code.
*******Search help for f_code-low*******
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield    = 'FIELD_CODE'
        dynpprog    = 'ZHR_GREYTHR_LOV'
        dynpnr      = sy-dynnr
        dynprofield = 'F_CODE'
        value_org   = 'S'
      TABLES
        value_tab   = lt_shlp.
  ENDIF.


START-OF-SELECTION.

  SELECT * FROM zhr_greythr_lovf
           INTO TABLE lt_lovf
           WHERE field_code IN f_code.
  IF sy-subrc = 0.
    SELECT * FROM zhr_greythr_lov
             INTO TABLE lt_values1.
    IF sy-subrc = 0.
      SORT lt_values1[] BY field_code.
    ENDIF.
  ENDIF.

  LOOP AT lt_lovf INTO ls_lovf.

    IF ls_lovf-field_code = '16'. "Bank Branch LOV

      DATA: bank_code TYPE char10.
      CLEAR bank_code.
      REFRESH lt_branch.

      LOOP AT lt_values1 INTO ls_values1 WHERE field_code = '10'.
        bank_code = ls_values1-id.
        CLEAR create_url.
        CONCATENATE ls_lovf-api_url ls_values1-id '/branches' INTO create_url.
        CONDENSE create_url NO-GAPS.
****Actual API Call******
        PERFORM api_call.
      ENDLOOP.
    ELSE.
      CLEAR create_url.
      create_url = ls_lovf-api_url.
****Actual API Call******
      PERFORM api_call." USING ls_lovf-field_code.
    ENDIF.

  ENDLOOP.

END-OF-SELECTION.

  PERFORM alv_display.
*******Getting Access Token To process Further API***********
FORM get_access_token.

  CALL FUNCTION 'ZHR_GET_ACCESS_TOKEN'
    IMPORTING
      access_token          = token
    EXCEPTIONS
      communication_failure = 1
      status_failure        = 2
      OTHERS                = 3.

  CLEAR str1.
  str1 = token.

ENDFORM.


FORM api_call. "USING pfield_code TYPE zfield_code.

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

  IF ls_lovf-field_code ='16'.
    lo_http_client->request->set_method(
  EXPORTING
  method = if_http_entity=>co_request_method_get ).  " Bank Branch LOV GET API.
  ELSE.
    lo_http_client->request->set_method(
   EXPORTING
   method = if_http_entity=>co_request_method_post ). "LOV POST API.
  ENDIF.

  lo_http_client->request->set_content_type(
   EXPORTING
   content_type = if_rest_media_type=>gc_appl_json ).

  PERFORM get_access_token. "Getting access token Based on auth API

  SELECT SINGLE *  FROM tvarvc INTO ls_tvarvc WHERE name = 'URL_LOV_API'
                                              AND type = 'P'.

  CLEAR str2.
  str2 = ls_tvarvc-low.

  lo_http_client->request->set_header_field( EXPORTING name  = 'ACCESS-TOKEN' value = str1 ).
  lo_http_client->request->set_header_field( EXPORTING name = 'x-greythr-domain' value = str2 ).

  DATA :ls_json    TYPE string,
        v_jsonload TYPE string.

  IF ls_lovf-field_code = '16'.
    lo_http_client->request->set_cdata(
   EXPORTING
     data = '{ JSON_Payload }').
*---- Added by Samsudeen on 02.03.2023 ----*
  ELSE.
    CLEAR v_jsonload.
    v_jsonload = |[ "{ ls_lovf-param_1 }::{ ls_lovf-field_name }" ]|.
*    CONDENSE v_jsonload NO-GAPS.
**------------------------------------------------------*
*  ELSE.
*    CONCATENATE '[' '"lov::' ls_lovf-field_name'"' ']' INTO v_jsonload.
  ENDIF.

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
    code = lv_codes ).

  lo_http_client->response->get_status(
  IMPORTING
    reason = lv_http_error ).

  CLEAR lv_response.
  IF lv_codes = 200.
    lv_response = lo_http_client->response->get_cdata( ).

*  WRITE: lv_response.
    IF ls_lovf-field_code = '16'.
      CLEAR ls_branch.
      /ui2/cl_json=>deserialize(
     EXPORTING
       json         = lv_response
       pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
     CHANGING
       data         = lt_branch ).

      LOOP AT lt_branch INTO ls_branch.
        READ TABLE lt_values1 INTO ls_values1 WITH KEY id = ls_branch-id
                                                       field_code = '16'.
        IF sy-subrc EQ 0.
          ls_values1-bank_code = bank_code.
          ls_values1-value = ls_branch-description.
          MODIFY zhr_greythr_lov FROM ls_values1.
          COMMIT WORK AND WAIT.

          CLEAR gs_display.
          gs_display-field_code = ls_lovf-field_code.
          gs_display-field_name = ls_lovf-field_name.
          gs_display-text = 'Entries upated Successfully'.
          APPEND gs_display TO gt_display.
        ELSE.
          ls_values1-field_code = ls_lovf-field_code.
          ls_values1-id = ls_branch-id.
          ls_values1-bank_code = bank_code.
          ls_values1-value = ls_branch-description.
          MODIFY zhr_greythr_lov FROM ls_values1.
          COMMIT WORK AND WAIT.

          CLEAR gs_display.
          gs_display-field_code = ls_lovf-field_code.
          gs_display-field_name = ls_lovf-field_name.
          gs_display-text = 'Entries Inserted Successfully'.
          APPEND gs_display TO gt_display.
        ENDIF.
      ENDLOOP.
    ELSE.
      DATA:  lo_parse     TYPE REF TO /ui5/cl_json_parser.
*   Deserialize the data
      CREATE OBJECT lo_parse.
*   Parse the result
      lo_parse->parse( json = lv_response ).
*   Save return values into local table
      REFRESH gt_entries.
      gt_entries = lo_parse->m_entries.


      DELETE gt_entries WHERE type = '2' OR type = '3'.
*--- Added by Samsudeen M on 02.03.2023 ------*
      IF ls_lovf-field_code = '18'    OR ls_lovf-field_code = '20' OR ls_lovf-field_code = '21'
         OR ls_lovf-field_code = '22' OR ls_lovf-field_code = '23' OR ls_lovf-field_code = '24'
         OR ls_lovf-field_code = '25' OR ls_lovf-field_code = '26' OR ls_lovf-field_code = '27'
         OR ls_lovf-field_code = '28' OR ls_lovf-field_code = '29' OR ls_lovf-field_code = '30'
                                      OR ls_lovf-field_code = '31' OR ls_lovf-field_code = '32'.
        DELETE gt_entries WHERE name = '3'.
      ELSE.
        DELETE gt_entries WHERE type = '2' OR type = '3'.
      ENDIF.
*---------------------------------------------*
*      DELETE TABLE gt_entries FROM VALUE #( type = '2' ).
*      DELETE TABLE gt_entries FROM VALUE #( type = '3' ).

      REFRESH lt_values.
      LOOP AT gt_entries INTO gs_entries.
        IF sy-tabix MOD 2 NE 0.
          ls_values-mandt = sy-mandt.
          ls_values-field_code = ls_lovf-field_code.
          ls_values-id = gs_entries-value.

        ELSEIF sy-tabix MOD 2 EQ 0.
          ls_values-value = gs_entries-value.
          APPEND ls_values TO lt_values.
          CLEAR ls_values.
        ENDIF.
      ENDLOOP.

      IF lt_values[] IS NOT INITIAL.
*--- Added by Samsudeen M on 02.03.2023 -------*
        IF ls_lovf-field_code = '18'. " Costcenter Master Updation
          PERFORM f_update_costcenter.
        ELSE.
*          REFRESH gt_display,.
          DELETE FROM zhr_greythr_lov WHERE field_code = ls_lovf-field_code.
          IF sy-subrc = 0.
            COMMIT WORK.
            CLEAR gs_display.
            gs_display-field_code = ls_lovf-field_code.
            gs_display-field_name = ls_lovf-field_name.
            gs_display-text = 'Entries Deleted Successfully'.
            APPEND gs_display TO gt_display.
          ENDIF.

          SORT lt_values BY field_code id.
          INSERT zhr_greythr_lov FROM TABLE lt_values.
          IF sy-subrc = 0.
            COMMIT WORK.
            CLEAR gs_display.
            gs_display-field_code = ls_lovf-field_code.
            gs_display-field_name = ls_lovf-field_name.
            gs_display-text = 'Entries Inserted Successfully'.
            APPEND gs_display TO gt_display.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.
*---- Added by Samsudeen on 02.03.2023 -----*
FORM f_update_costcenter.
*--- Initially Deleting All the entries  -------*
  DELETE FROM zhr_greyhr_cstcr WHERE field_code = ls_lovf-field_code.
  COMMIT WORK.
  IF sy-subrc = 0.
    APPEND VALUE #( field_code = ls_lovf-field_code
                    field_name = ls_lovf-field_name
                    text = |Cost Center Data Deleted Successfully| ) TO gt_display.
  ENDIF.
*----- After Deleting Again inserting the API Values to the Tables ------*
  LOOP AT lt_values INTO DATA(lw_values).
    DATA(ls_upd) = VALUE zhr_greyhr_cstcr( mandt = sy-mandt
                                           field_code = lw_values-field_code
                                           cc_id = lw_values-id
                                           costcenter = lw_values-value ).
    INSERT zhr_greyhr_cstcr FROM ls_upd.
    COMMIT WORK.
    IF sy-subrc = 0.

    ENDIF.
  ENDLOOP.
  APPEND VALUE #( field_code = ls_lovf-field_code
                 field_name = ls_lovf-field_name
                 text = |Cost Center Data Inserted Successfully| ) TO gt_display.
ENDFORM.
*----- End of Changes on 02.03.2023 -------*
FORM alv_display.
  REFRESH gt_fcat.
  CLEAR gs_fcat.
  PERFORM f_fieldcat USING  'FIELD_CODE' 'Field code' 1  space.
  PERFORM f_fieldcat USING  'FIELD_NAME' 'Field name' 2  space.
  PERFORM f_fieldcat USING  'TEXT' 'Message' 3  space.

  gs_layout-colwidth_optimize = 'X'.
  gs_layout-zebra = 'X'.

*****ALV Display********************
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
*     i_callback_top_of_page = 'TOP-OF-PAGE '
      is_layout          = gs_layout
      it_fieldcat        = gt_fcat
    TABLES
      t_outtab           = gt_display.
ENDFORM.
FORM f_fieldcat  USING f_var1 f_var2 f_var3 f_var4.
  gs_fcat-fieldname = f_var1.
  gs_fcat-seltext_m = f_var2.
  gs_fcat-col_pos = f_var3.
  gs_fcat-edit = f_var4.
  APPEND gs_fcat TO gt_fcat.
  CLEAR gs_fcat.
ENDFORM.
