class ZCL_API_DMS_PERIOD_CHECK definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_DMS_PERIOD_CHECK IMPLEMENTATION.


  METHOD if_http_extension~handle_request.
*&Created by: Pandiarajan
*&Created On: 23.12.2023
*&Purpose   : Period Check for MM & FI - DMS
*&Reference : Ramakrishnan J
*-------------------------------------------------------------------------------------
    TYPES : BEGIN OF ty_response,
              date      TYPE sy-datum,
              mm_period TYPE char1,
              fi_period TYPE char1,
            END OF ty_response.
    DATA : lv_data     TYPE string,
           lv_body     TYPE string,
           ls_response TYPE ty_response,
           lv_comp     TYPE bukrs,
           lv_type     TYPE bapi_mtype.
*"data dec for store api log table
    DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
    CREATE OBJECT lo_log_upd.
    DATA: lo_check TYPE REF TO zcl_common_check.
    CREATE OBJECT lo_check.
    CLEAR : lv_data.
    CALL METHOD server->request->get_cdata RECEIVING data = lv_data.
** Deserialize the input our required input ***
    /ui2/cl_json=>deserialize(
   EXPORTING
     json         = lv_data
     pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
   CHANGING
     data         = ls_response ).

    IF ls_response IS INITIAL.
      ls_response-date = sy-datum.
    ENDIF.
    lv_comp = 'DMS1'.
    lo_check->mm_period_check(
      EXPORTING
        com_code  = lv_comp
        date      = ls_response-date
    IMPORTING
      type      = lv_type ).
    IF lv_type = 'S'.
      ls_response-mm_period = 'O'.
    ELSE.
      ls_response-mm_period = 'C'.
    ENDIF.
    CLEAR : lv_type.
    lo_check->fi_period_check(
      EXPORTING
        posting_date = ls_response-date
        acct_type    = 'D'
        com_code     = lv_comp
      IMPORTING
        type         = lv_type
    ).
    IF lv_type = 'S'.
      ls_response-fi_period = 'O'.
    ELSE.
      ls_response-fi_period = 'C'.
    ENDIF.
**********send the data to mis & store logs***********
    IF ls_response IS NOT INITIAL.
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
          apiname         = 'DMS_PERIOD_CHK'
          ijson           = lv_data
          ojson           = lv_body
        EXCEPTIONS
          apiname_missing = 1
          json_missing    = 2
          OTHERS          = 3.
    ENDIF.
*Set JSON Content-Type
    CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
    CALL METHOD server->response->set_cdata( data = lv_body ).

  ENDMETHOD.
ENDCLASS.
