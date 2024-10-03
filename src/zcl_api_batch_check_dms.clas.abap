class ZCL_API_BATCH_CHECK_DMS definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_BATCH_CHECK_DMS IMPLEMENTATION.


  METHOD if_http_extension~handle_request.
*Created By: Pandiarajan
*Createed On: 11.03.2024
*Purpose : check the material & batch exists or not
*---------------------------------------------------------------*
    TYPES: BEGIN OF input,
             distributor TYPE kunnr,
             material    TYPE matnr,
             batch       TYPE charg_d,
           END OF input.
    TYPES: BEGIN OF output,
             distributor TYPE kunnr,
             material    TYPE matnr,
             batch       TYPE charg_d,
             type        TYPE bapi_mtype,
             msg         TYPE string,
           END OF output.
    DATA: gs_input TYPE input.
    DATA: gs_output TYPE output.

    DATA: lv_str_output TYPE string,
          lv_msg        TYPE string.

    DATA: lr_request  TYPE REF TO if_http_request,
          lr_response TYPE REF TO if_http_response.

    DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
    CREATE OBJECT lo_log_upd.

    DATA: lv_body    TYPE string,
          v_jsonload TYPE string.

    lr_request = server->request.
    lr_response = server->response.
* Check the Calling Method
    IF lr_request->get_method( ) EQ 'GET'.
      CALL METHOD lr_request->get_cdata RECEIVING data = DATA(lv_data).
      DATA(lv_data_tmp) = |{ lv_data }|.
** Deserialize the input our required input ***
      /ui2/cl_json=>deserialize(
      EXPORTING
       json         = lv_data_tmp
       pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
       assoc_arrays = abap_true
      CHANGING
       data         = gs_input ).

      IF gs_input IS NOT INITIAL.
        gs_input-distributor = |{ gs_input-distributor ALPHA = IN }|.
        gs_input-material    = |{ gs_input-material ALPHA = IN }|.
**********fetch the distriputor Plant***********
        SELECT SINGLE werks FROM kna1 INTO @DATA(ls_werks) WHERE kunnr = @gs_input-distributor.
***********First checking in the mcha table*************
        SELECT SINGLE werks FROM mcha INTO @DATA(ls_mcha) WHERE matnr = @gs_input-material
                                                           AND  werks = @ls_werks
                                                           AND  charg = @gs_input-batch.
        IF sy-subrc = 0.
          gs_output-type = 'S'.
          gs_output-msg  = |Material - { gs_input-material } Batch - { gs_input-batch } Available |.
        ELSE.
***********checking in the mch1 table*************
          SELECT SINGLE matnr FROM mch1 INTO @DATA(ls_mch1) WHERE matnr = @gs_input-material
                                                            AND   charg = @gs_input-batch.
          IF sy-subrc = 0.
            gs_output-type = 'S'.
            gs_output-msg  = |Material - { gs_input-material } Batch - { gs_input-batch } Available |.
          ELSE.
            gs_output-type = 'E'.
            gs_output-msg  = |Material - { gs_input-material } Batch - { gs_input-batch } Not Available |.
          ENDIF.
        ENDIF.
        IF gs_input-batch IS INITIAL OR gs_input-material IS INITIAL.
          gs_output-type = 'E'.
          gs_output-msg  = |Material - { gs_input-material } Batch - { gs_input-batch } Not Available |.
        ENDIF.
        gs_output-distributor = gs_input-distributor.
        gs_output-material    = gs_input-material.
        gs_output-batch       = gs_input-batch.
** serialize the output for response ***
        /ui2/cl_json=>serialize(
        EXPORTING
         data         =  gs_output
         pretty_name  = /ui2/cl_json=>pretty_mode-user
        RECEIVING
         r_json         = lv_body ).

*Output Entry in Log Table
        CALL METHOD lo_log_upd->log_entry_store
          EXPORTING
            apiname         = 'DMS_BATCH_CHK'
            ijson           = lv_data
            ojson           = lv_body
          EXCEPTIONS
            apiname_missing = 1
            json_missing    = 2
            OTHERS          = 3.
*Set JSON Content-Type
        CALL METHOD lr_response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
        CALL METHOD lr_response->set_cdata( data = lv_body ).
      ELSE.
        v_jsonload = |No input is Captured in Input|.
*Set JSON Content-Type
        CALL METHOD lr_response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
        CALL METHOD lr_response->set_cdata( data = v_jsonload ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
