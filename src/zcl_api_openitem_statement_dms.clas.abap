class ZCL_API_OPENITEM_STATEMENT_DMS definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_OPENITEM_STATEMENT_DMS IMPLEMENTATION.


  METHOD if_http_extension~handle_request.
*Created By: Pandiarajan
*Created On: 07.03.2024
*Purpose : Sending the pdf string for retailer ledger account statement - DMS
*---------------------------------------------------------------*
    TYPES: BEGIN OF input,
             distributor TYPE kunnr,
             retailer    TYPE kunnr,
             fromdate    TYPE budat,
             todate      TYPE budat,
           END OF input.
    TYPES: BEGIN OF output,
             distributor TYPE kunnr,
             retailer    TYPE kunnr,
             fromdate    TYPE budat,
             todate      TYPE budat,
             type        TYPE bapi_mtype,
             msg         TYPE string,
             output      TYPE string,
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
        lv_body = 'DMS customer statement log'.
**input Entry in Log Table
*        CALL METHOD lo_log_upd->log_entry_store
*          EXPORTING
*            apiname         = 'DMS_CUST_LEDG'
*            ijson           = lv_data
*            ojson           = lv_body
*            distributor     = gs_input-distributor
*            retailer        = gs_input-retailer
*          EXCEPTIONS
*            apiname_missing = 1
*            json_missing    = 2
*            OTHERS          = 3.

        CLEAR : lv_msg,lv_body.
***********check the distributor code**********
        IF gs_input-distributor IS INITIAL.
          lv_msg = |Please Enter Distributor Code|.
        ELSE.
          gs_input-distributor = |{ gs_input-distributor ALPHA = IN }|.
          SELECT SINGLE werks FROM kna1 INTO @DATA(ls_werks) WHERE kunnr = @gs_input-distributor.
          IF sy-subrc NE 0.
            lv_msg = |Please Enter Valid Distributor Code|.
          ENDIF.
        ENDIF.
***********check the Retailer code**********
        IF gs_input-retailer IS INITIAL.
          lv_msg = |{ lv_msg },Please Enter Retailer Code|.
        ELSE.
          gs_input-retailer = |{ gs_input-retailer ALPHA = IN }|.
          SELECT SINGLE kunnr FROM kna1 INTO @DATA(l_customer) WHERE kunnr = @gs_input-retailer.
          IF sy-subrc NE 0.
            lv_msg = |{ lv_msg },Invalid Retailer code|.
          ENDIF.
        ENDIF.

        IF gs_input-fromdate IS INITIAL OR gs_input-todate IS INITIAL.
          lv_msg = |{ lv_msg },Check the Input Date is Missing|.
        ENDIF.

        IF lv_msg IS INITIAL.
*Customer Open item Statements
          CLEAR lv_str_output.
          CALL FUNCTION 'ZFI_CUSTOMER_STAT_PDF_DMS'
            EXPORTING
              distributor = gs_input-distributor
              retailer    = gs_input-retailer
              from_date   = gs_input-fromdate
              to_date     = gs_input-todate
            IMPORTING
              ep_string   = lv_str_output.
***********check the pdf string***********
          IF lv_str_output IS INITIAL.
            gs_output-type = 'E'.
            gs_output-msg  = 'Customer Statement PDF Is Not Generated'..
          ELSE.
            gs_output-type = 'S'.
            gs_output-msg  = 'Customer Statement PDF Generated'.
          ENDIF.
        ELSE.
          gs_output-type = 'E'.
          gs_output-msg  = lv_msg.
        ENDIF.
        gs_output-distributor = gs_input-distributor.
        gs_output-retailer    = gs_input-retailer.
        gs_output-fromdate    = gs_input-fromdate.
        gs_output-todate      = gs_input-todate.
        gs_output-output      = lv_str_output.
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
            apiname         = 'DMS_CUST_LEDG'
            ijson           = lv_data
            ojson           = lv_body
            distributor     = gs_input-distributor
            retailer        = gs_input-retailer
          EXCEPTIONS
            apiname_missing = 1
            json_missing    = 2
            OTHERS          = 3.
        IF sy-subrc = 0.

        ENDIF.
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
