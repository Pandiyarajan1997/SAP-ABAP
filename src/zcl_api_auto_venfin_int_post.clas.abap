class ZCL_API_AUTO_VENFIN_INT_POST definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_AUTO_VENFIN_INT_POST IMPLEMENTATION.


  METHOD if_http_extension~handle_request.

** Purpose of this API is for Posting HDFC Vendor Financing Interest for Payments ***
    TYPES: BEGIN OF ty_input,
             ccode        TYPE string,
             amt          TYPE string,
             doc_date     TYPE string,
             posting_date TYPE string,
           END OF ty_input.
    DATA: gw_input TYPE ty_input.
    DATA: lv_subrc TYPE sy-subrc.
    DATA: lv_message TYPE string.
    DATA: lv_body TYPE string.
    DATA: v_jsonload TYPE string.

    DATA: lv_bukrs TYPE bukrs,
          lv_amt   TYPE wrbtr,
          lv_docno TYPE belnr_d,
          lv_ddate TYPE bldat,
          lv_pdate TYPE budat,
          lv_msgty TYPE char01,
          lv_msg   TYPE char50.

    DATA: return TYPE TABLE OF bapiret2.
    DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
    CREATE OBJECT lo_log_upd.

    CALL METHOD server->request->get_cdata RECEIVING data = DATA(lv_data).

    REPLACE ALL OCCURRENCES OF '##'  IN lv_data WITH ''.
    CONDENSE lv_data NO-GAPS.

** Deserialize the input our required input ***
    /ui2/cl_json=>deserialize(
    EXPORTING
     json         = lv_data
     pretty_name  = /ui2/cl_json=>pretty_mode-user
    CHANGING
     data         = gw_input ).

    CLEAR:  lv_message, lv_subrc.
    IF gw_input IS INITIAL.
      CLEAR lv_message.
      lv_message = 'Missing Input Parameters'.
      lv_subrc = '4'.
    ELSE.
      IF gw_input-ccode IS INITIAL.
        CLEAR lv_message.
        lv_message = 'Company Code Mandatory'.
        lv_subrc = '4'.
      ELSEIF gw_input-amt IS INITIAL.
        lv_message = 'Interest Amount Mandatory'.
        lv_subrc = '4'.
      ELSEIF gw_input-doc_date IS INITIAL.
        lv_message = 'Document Date Mandatory'.
        lv_subrc = '4'.
      ELSEIF gw_input-posting_date IS INITIAL.
        lv_message = 'Posting_date Mandatory'.
        lv_subrc = '4'.
      ENDIF.
    ENDIF.
** If error arise stop the request ***
    IF lv_message IS NOT INITIAL.
      CLEAR lv_body.
      lv_body = | { '{' } { '"Error":' } { '"' } { lv_message } { '"' } { '}' } |.
      CLEAR v_jsonload.
      v_jsonload = | { '[' } { lv_body } { ']' } |.

*Output Entry in Log Table
      CALL METHOD lo_log_upd->log_entry_store
        EXPORTING
          apiname         = 'HDFCINTEREST'
          ijson           = lv_data
          ojson           = v_jsonload
        EXCEPTIONS
          apiname_missing = 1
          json_missing    = 2
          OTHERS          = 3.
      IF sy-subrc = 0.

      ENDIF.

*      SET json content-type
      CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
      CALL METHOD server->response->set_cdata( data = v_jsonload ).
    ENDIF.

    CHECK lv_subrc IS INITIAL.
    CLEAR: lv_bukrs,lv_amt,lv_docno,lv_msgty,lv_msg,return,lv_ddate,lv_pdate.

    lv_bukrs = gw_input-ccode.
    lv_amt    = gw_input-amt.
    lv_ddate = gw_input-doc_date.
    lv_pdate = gw_input-posting_date.
** Actual Function Module Call for Clearing the vendor Invoices ***
    CALL FUNCTION 'ZVEN_FINANCE_HDFC__INT_POST'
      EXPORTING
        comp_code       = lv_bukrs
        interest_amount = lv_amt
        doc_date        = lv_ddate
        posting_date    = lv_pdate
      IMPORTING
        document_no     = lv_docno
        message         = lv_msg
      TABLES
        lt_return       = return
      EXCEPTIONS
        enter_compcode  = 1
        enter_amount    = 2
        enter_pdate     = 3
        enter_ddate     = 4
        OTHERS          = 5.
    IF sy-subrc <> 0.
      CASE sy-subrc.
        WHEN 1.
          CLEAR lv_message.
          lv_message = 'Company Code Mandatory'.
        WHEN 2.
          CLEAR lv_message.
          lv_message = 'Interst Amount Mandatory'.
        WHEN 3.
          CLEAR lv_message.
          lv_message = 'Document Date Mandatory'.
        WHEN 4.
          CLEAR lv_message.
          lv_message = 'Posting Date Mandatory'.
        WHEN 5.
          CLEAR lv_message.
          lv_message = 'Others'.
      ENDCASE.
      CLEAR lv_body.
      lv_body = | { '{' } { '"Error":' } { '"' } { lv_message } { '"' } { '}' } |.

    ELSE.
      CLEAR lv_body.
      IF return IS NOT INITIAL.
        LOOP AT return INTO DATA(wa_return).
          lv_body = | { '{' } { '"Error":' } { '"' } { wa_return-message } { '"' } { '}' } |.
        ENDLOOP.
      ELSE.
        lv_body = | { '{' } { '"Document_No":' } { '"' } { lv_docno } { '"' } { '"Message":' } { '"' } { lv_msg } { '"' } { '}' } |.
      ENDIF.
    ENDIF.
    v_jsonload = | { '[' } { lv_body } { ']' } |.

*Output Entry in Log Table
    CALL METHOD lo_log_upd->log_entry_store
      EXPORTING
        apiname         = 'HDFCINTEREST'
        ijson           = lv_data
        ojson           = v_jsonload
      EXCEPTIONS
        apiname_missing = 1
        json_missing    = 2
        OTHERS          = 3.
    IF sy-subrc = 0.

    ENDIF.

*Set JSON Content-Type
    CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
    CALL METHOD server->response->set_cdata( data = v_jsonload ).
  ENDMETHOD.
ENDCLASS.
