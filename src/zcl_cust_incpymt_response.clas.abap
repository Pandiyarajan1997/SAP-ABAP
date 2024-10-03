class ZCL_CUST_INCPYMT_RESPONSE definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CUST_INCPYMT_RESPONSE IMPLEMENTATION.


  METHOD if_http_extension~handle_request.

*** Purpose of this API is for Customer incoming Payment
*   entry Posting log table sending response back to MIS ***

    "Structure for Input from API Body
    TYPES: BEGIN OF ty_input,
             customer TYPE string,
             ref_doc  TYPE string,
           END OF ty_input.
    DATA: gt_input TYPE TABLE OF ty_input,
          gs_input TYPE ty_input.
    TYPES: BEGIN OF ty_cust,
             customer TYPE kunnr,
             ref_doc  TYPE xblnr1,
             msg(70)  TYPE c,
           END OF ty_cust.
    DATA: gt_cust_log TYPE TABLE OF ty_cust,
          gs_log      LIKE LINE OF gt_cust_log.
    "Structure for response of API After hitting ***
    TYPES: BEGIN OF ty_response,
             customer      TYPE kunnr,
             refdoc        TYPE xblnr1,
             amount        TYPE wrbtr,
             cleardocument TYPE belnr_d,
             status(01)    TYPE c,
             statusdes(30) TYPE c,
             cleared_doc1  TYPE belnr_d,
             cleared_doc2  TYPE belnr_d,
             cleared_doc3  TYPE belnr_d,
             cleared_doc4  TYPE belnr_d,
             cleared_doc5  TYPE belnr_d,
             cleared_doc6  TYPE belnr_d,
             cleared_doc7  TYPE belnr_d,
             cleared_doc8  TYPE belnr_d,
             cleared_doc9  TYPE belnr_d,
             cleared_doc10 TYPE belnr_d,
             remarks(120)  TYPE c,
           END OF ty_response.
    DATA: gt_response TYPE TABLE OF ty_response,
          gs_response LIKE LINE OF gt_response.

    DATA: lv_subrc TYPE sy-subrc.
    DATA: lv_message TYPE string.
    DATA: lv_body TYPE string.
    DATA: v_jsonload TYPE string.
    DATA: lv_date TYPE char20.
    DATA: lv_bukrs  TYPE bukrs,
          lv_kunnr  TYPE kunnr,
          lv_amt    TYPE wrbtr,
          lv_refdoc TYPE xblnr1.
    DATA: lr_data TYPE REF TO data.
    DATA: lt_domain  TYPE TABLE OF dd07v.
    DATA: gv_fiscalyr TYPE bapi0002_4-fiscal_year,
          gv_fiscalp  TYPE bapi0002_4-fiscal_period.

    DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
    CREATE OBJECT lo_log_upd.

*** Field Symbols for splitting input ***
    FIELD-SYMBOLS:
      <data>        TYPE data,
      <results>     TYPE data,
      <structure>   TYPE any,
      <table>       TYPE ANY TABLE,
      <field>       TYPE any,
      <field_value> TYPE data.

    CALL METHOD server->request->get_cdata RECEIVING data = DATA(lv_data).

    CALL METHOD lo_log_upd->log_entry_store
      EXPORTING
        apiname         = 'CUSTINCPYTRES_IN'
        json            = lv_data
      EXCEPTIONS
        apiname_missing = 1
        json_missing    = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.

    ENDIF.

    REPLACE ALL OCCURRENCES OF '['  IN lv_data WITH ''.
    REPLACE ALL OCCURRENCES OF ']'  IN lv_data WITH ''.
    lv_data = | { '{' } { '"Results":' } { '[ ' } { lv_data } { ']' } { '}' } |.

    REPLACE ALL OCCURRENCES OF '##'  IN lv_data WITH ''.
    CONDENSE lv_data NO-GAPS.
    SHIFT lv_data LEFT DELETING LEADING ''.

** Deserialize the input our required input ***
    /ui2/cl_json=>deserialize(
    EXPORTING
     json         = lv_data
     pretty_name  = /ui2/cl_json=>pretty_mode-user
    CHANGING
     data         = lr_data ).
*** Deserializing Input JSON to Internal table ***
    IF lr_data IS BOUND.
      ASSIGN lr_data->* TO <data>.
      ASSIGN COMPONENT `RESULTS` OF STRUCTURE <data> TO <results>.
      ASSIGN <results>->* TO <table>.
      LOOP AT <table> ASSIGNING <structure>.
        ASSIGN <structure>->* TO <data>.
        ASSIGN COMPONENT `CUSTOMER` OF STRUCTURE <data> TO <field>.
        IF <field> IS ASSIGNED.
          lr_data = <field>.
          ASSIGN lr_data->* TO <field_value>.
          gs_input-customer = <field_value>.
        ENDIF.
        UNASSIGN: <field>, <field_value>.

        ASSIGN COMPONENT `REF_DOC` OF STRUCTURE <data> TO <field>.
        IF <field> IS ASSIGNED.
          lr_data = <field>.
          ASSIGN lr_data->* TO <field_value>.
          gs_input-ref_doc = <field_value>.
        ENDIF.
        UNASSIGN: <field>, <field_value>.

        APPEND gs_input TO gt_input.
        CLEAR gs_input.
      ENDLOOP.
    ENDIF.
** Actual Checks and Process starts here ***
    LOOP AT gt_input ASSIGNING FIELD-SYMBOL(<fs_input>).
      CLEAR: lv_kunnr,lv_refdoc.
      lv_kunnr = <fs_input>-customer.
**customer Number Conversion ***
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lv_kunnr
        IMPORTING
          output = lv_kunnr.

      lv_refdoc = <fs_input>-ref_doc.
      gs_log-customer = lv_kunnr.
      gs_log-ref_doc = lv_refdoc.
      APPEND gs_log TO gt_cust_log.
      CLEAR gs_log.
    ENDLOOP.
*** company code variable Default ***
    SELECT SINGLE low INTO lv_bukrs FROM tvarvc
                      WHERE name = 'CUSTOMER_INPYMT_CC'
                      AND type = 'P'.
* get fiscal year and period - (requires date and company code)
    CALL FUNCTION 'BAPI_COMPANYCODE_GET_PERIOD'
      EXPORTING
        companycodeid = lv_bukrs
        posting_date  = sy-datum
      IMPORTING
        fiscal_year   = gv_fiscalyr
        fiscal_period = gv_fiscalp.
** Getting Domain Values **
    CALL FUNCTION 'GET_DOMAIN_VALUES'
      EXPORTING
        domname    = 'ZCI_STAT'
      TABLES
        values_tab = lt_domain[].

**** Selecting the data to which all response has to sent ***
    SELECT * FROM zfi_cust_inpymt
             INTO TABLE @DATA(lt_log1)
             FOR ALL ENTRIES IN @gt_cust_log
             WHERE bukrs = @lv_bukrs
             AND kunnr = @gt_cust_log-customer
             AND xblnr = @gt_cust_log-ref_doc.
    IF sy-subrc = 0.
**** Selecting the cleared Documents ***
      SELECT * FROM zdebit_ref_mis
               INTO TABLE @DATA(lt_log2)
               WHERE bukrs = @lv_bukrs
               AND gjahr = @gv_fiscalyr.
      IF sy-subrc = 0.
        SORT lt_log2[] BY account.
      ENDIF.
    ENDIF.
    REFRESH: gt_response.
    LOOP AT gt_cust_log ASSIGNING FIELD-SYMBOL(<fgs_custlog>).
      READ TABLE lt_log1 ASSIGNING FIELD-SYMBOL(<fls_log>) WITH KEY bukrs = lv_bukrs
                                                                    kunnr = <fgs_custlog>-customer
                                                                    xblnr = <fgs_custlog>-ref_doc.
      IF sy-subrc = 0.
        READ TABLE lt_log2 ASSIGNING FIELD-SYMBOL(<fls_log1>) WITH KEY bukrs = lv_bukrs
                                                                       gjahr = gv_fiscalyr
                                                                       account = <fgs_custlog>-customer
                                                                       invoice_refno = <fls_log>-xblnr.
        IF sy-subrc = 0.
          gs_response-customer = <fls_log>-kunnr.
          gs_response-refdoc = <fls_log>-xblnr.
          gs_response-amount = <fls_log>-amount.
          gs_response-cleardocument = <fls_log>-doc_no.
          gs_response-status = 'S'.
          gs_response-statusdes = VALUE #( lt_domain[ domvalue_l = <fls_log>-status ]-ddtext OPTIONAL ).
          gs_response-cleared_doc1 = <fls_log1>-actual_doc1.
          gs_response-cleared_doc2 = <fls_log1>-actual_doc2.
          gs_response-cleared_doc3 = <fls_log1>-actual_doc3.
          gs_response-cleared_doc4 = <fls_log1>-actual_doc4.
          gs_response-cleared_doc5 = <fls_log1>-actual_doc5.
          gs_response-cleared_doc6 = <fls_log1>-actual_doc6.
          gs_response-cleared_doc7 = <fls_log1>-actual_doc7.
          gs_response-cleared_doc8 = <fls_log1>-actual_doc8.
          gs_response-cleared_doc9 = <fls_log1>-actual_doc9.
          gs_response-cleared_doc10 = <fls_log1>-actual_doc10.
          gs_response-remarks = <fls_log>-remarks.
          APPEND gs_response TO gt_response.
          CLEAR gs_response.
        ELSE.
          gs_response-customer = <fgs_custlog>-customer.
          gs_response-refdoc = <fgs_custlog>-ref_doc.
          gs_response-status = 'E'.
          gs_response-statusdes = 'Error'.
          gs_response-remarks = 'Entries is not present in cleared document log table'.
          APPEND gs_response TO gt_response.
          CLEAR gs_response.
          CONTINUE.
        ENDIF.
      ELSE.
        gs_response-customer = <fgs_custlog>-customer.
        gs_response-refdoc = <fgs_custlog>-ref_doc.
        gs_response-status = 'E'.
        gs_response-statusdes = 'Error'.
        gs_response-remarks = 'Entries is not present in SAP with given input'.
        APPEND gs_response TO gt_response.
        CLEAR gs_response.
        CONTINUE.
      ENDIF.
    ENDLOOP.
*** Response Part ***
    IF gt_response[] IS NOT INITIAL.
** serialize the output for response ***
      /ui2/cl_json=>serialize(
      EXPORTING
       data         =  gt_response
       pretty_name  = /ui2/cl_json=>pretty_mode-user
      RECEIVING
       r_json         = lv_body ).

      v_jsonload = | { '[' } { lv_body } { ']' } |.

      CALL METHOD lo_log_upd->log_entry_store
        EXPORTING
          apiname         = 'CUSTINCPYTRES_OT'
          json            = v_jsonload
        EXCEPTIONS
          apiname_missing = 1
          json_missing    = 2
          OTHERS          = 3.
      IF sy-subrc <> 0.

      ENDIF.

*Set JSON Content-Type
      CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
      CALL METHOD server->response->set_cdata( data = v_jsonload ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
