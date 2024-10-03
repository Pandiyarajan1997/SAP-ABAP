class ZCL_API_OPENITEM_STATEMENTS definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_OPENITEM_STATEMENTS IMPLEMENTATION.


  METHOD if_http_extension~handle_request.
*Created By: Samsudeen M
*Createed On: 25.08.2023
*Purpose : Sending the Customer & Vendor Statement PDF string
*          to MIS System
*---------------------------------------------------------------*
    TYPES: BEGIN OF input,
             acctype  TYPE char01,
             account  TYPE lifnr,
             ccode    TYPE bukrs,
             fromdate TYPE budat,
             todate   TYPE budat,
           END OF input.
    DATA: gs_input TYPE input.

    TYPES: BEGIN OF output,
             acctype   TYPE char01,
             account   TYPE lifnr,
             ccode     TYPE bukrs,
             fromdate  TYPE budat,
             todate    TYPE budat,
             status    TYPE char01,
             message   TYPE string,
             statement TYPE string,
           END OF output.
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

*        CLEAR gs_output.
*        gs_output-acctype = gs_input-acctype.
*        gs_output-account = gs_output-account.
*        gs_output-fromdate = gs_input-fromdate.
*        gs_output-todate = gs_input-todate.
*        gs_output-ccode = gs_input-ccode.

        CLEAR lv_msg.
        IF gs_input-acctype IS INITIAL.
          lv_msg = |Please Enter Vendor OR Customer Account Type|.
        ENDIF.

        CLEAR lv_msg.
        IF gs_input-ccode IS INITIAL.
          lv_msg = |{ lv_msg }, Please Enter Company Code|.
        ENDIF.

        IF gs_input-account IS INITIAL.
          lv_msg = |{ lv_msg },Please Enter Customer Code OR Vendor Code|.
        ELSE.
          CASE gs_input-acctype.
            WHEN 'D'.
              DATA(lv_customer) = |{ gs_input-account ALPHA = IN }|.
              SELECT SINGLE kunnr FROM knb1 INTO @DATA(l_customer)
                WHERE kunnr = @lv_customer AND bukrs = @gs_input-ccode.
              IF sy-subrc NE 0.
                lv_msg = |{ lv_msg },Invalid Customer Number|.
              ENDIF.
            WHEN 'K'.
              DATA(lv_vendor) = |{ gs_input-account ALPHA = IN }|.
              SELECT SINGLE lifnr FROM lfb1 INTO @DATA(l_vendor)
                WHERE lifnr = @lv_vendor AND bukrs = @gs_input-ccode.
              IF sy-subrc NE 0.
                lv_msg = |{ lv_msg },Invalid vendor Number|.
              ENDIF.
          ENDCASE.
        ENDIF.

        IF gs_input-fromdate IS INITIAL OR gs_input-todate IS INITIAL.
          lv_msg = |{ lv_msg },Check the Input Date is Missing|.
        ENDIF.

        IF lv_msg IS INITIAL.
          CASE gs_input-acctype.
*Customer Open item Statements
            WHEN 'D'.
              CLEAR lv_str_output.
              CALL FUNCTION 'ZFI_CUSTOMER_STAT_PDF'
                EXPORTING
                  kunnr     = l_customer
                  from_date = gs_input-fromdate
                  to_date   = gs_input-todate
                  bukrs     = gs_input-ccode
                IMPORTING
                  ep_string = lv_str_output.

              CLEAR gs_output.
              gs_output-acctype = gs_input-acctype.
              gs_output-account = gs_output-account.
              gs_output-fromdate = gs_input-fromdate.
              gs_output-todate = gs_input-todate.
              gs_output-ccode = gs_input-ccode.
              gs_output-status = 'S'.
              gs_output-statement = lv_str_output.
*              gs_output-message = lv_msg.

              lv_msg = lv_str_output.
*Vendor Openitems Statements
            WHEN 'K'.
              CLEAR lv_str_output.
              CALL FUNCTION 'ZFI_VENDOR_STAT_PDF'
                EXPORTING
                  lifnr     = l_vendor
                  from_date = gs_input-fromdate
                  to_date   = gs_input-todate
                  bukrs     = gs_input-ccode
                IMPORTING
                  ep_string = lv_str_output.
              CLEAR gs_output.
              gs_output-acctype = gs_input-acctype.
              gs_output-account = gs_output-account.
              gs_output-fromdate = gs_input-fromdate.
              gs_output-todate = gs_input-todate.
              gs_output-ccode = gs_input-ccode.
              gs_output-status = 'S'.
              gs_output-statement = lv_str_output.
          ENDCASE.
        ELSE.
          CLEAR gs_output.
          gs_output-acctype = gs_input-acctype.
          gs_output-account = gs_output-account.
          gs_output-fromdate = gs_input-fromdate.
          gs_output-todate = gs_input-todate.
          gs_output-ccode = gs_input-ccode.
          gs_output-status = 'E'.
          gs_output-message = lv_msg.
        ENDIF.

*        v_jsonload = gs_output.
** serialize the output for response ***
        /ui2/cl_json=>serialize(
        EXPORTING
         data         =  gs_output
         pretty_name  = /ui2/cl_json=>pretty_mode-user
        RECEIVING
         r_json         = lv_body ).
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
