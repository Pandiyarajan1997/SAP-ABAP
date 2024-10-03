class ZCL_CUST_INCOMING_PYMT_API definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CUST_INCOMING_PYMT_API IMPLEMENTATION.


  METHOD if_http_extension~handle_request.

*** Purpose of this API is for Customer incoming Payment entry Posting ***
    TYPES: BEGIN OF ty_input,
             customer TYPE kunnr,
             ref_doc  TYPE char50,
             amount   TYPE wrbtr,
             docdate  TYPE bldat,
             posdate  TYPE budat,
           END OF ty_input.

    DATA: gt_input TYPE STANDARD TABLE OF ty_input,
          gs_input TYPE ty_input.

    DATA: lo_common_check TYPE REF TO zcl_common_check.       "obj dec for global class common check
    CREATE OBJECT lo_common_check.

    TYPES: BEGIN OF ty_msg,
             customer TYPE string,
             ref_doc  TYPE string,
             amount   TYPE string,
             status   TYPE string,
             remarks  TYPE string,
           END OF ty_msg.

    DATA: gt_response TYPE TABLE OF ty_msg.


    DATA: lv_subrc TYPE sy-subrc.
    DATA: lv_message TYPE string.
    DATA: lv_body TYPE string.
    DATA: v_jsonload TYPE string.
    DATA: gv_fiscalyr TYPE bapi0002_4-fiscal_year,
          gv_fiscalp  TYPE bapi0002_4-fiscal_period.
    DATA:lv_date TYPE char20.
    DATA: lv_bukrs      TYPE bukrs,
          lv_kunnr      TYPE kunnr,
          lv_amt        TYPE wrbtr,
          lv_refdoc     TYPE xblnr1,
          lv_refdoc_str TYPE char50.
    DATA: lv_response TYPE string.
    DATA: lr_data TYPE REF TO data.
    DATA: lv_data TYPE string.
    DATA: lv_referenceid TYPE num10.
    DATA: l_msg TYPE string.

    DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
    CREATE OBJECT lo_log_upd.

    CALL METHOD server->request->get_cdata RECEIVING data = lv_data.

    REFRESH: gt_input.
** Deserialize the input our required input ***
    /ui2/cl_json=>deserialize(
    EXPORTING
     json         = lv_data
     pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
     assoc_arrays = abap_true
    CHANGING
     data         = gt_input ).

**** select data from log table for checks ****
    SELECT * FROM zfi_cust_inpymt
             INTO TABLE @DATA(lt_log_chk).
    IF sy-subrc = 0.
      SORT lt_log_chk[] BY kunnr.
    ENDIF.
*** Customer Number checks ***
    SELECT kunnr, name1 FROM kna1 INTO TABLE @DATA(lt_customer).
    IF sy-subrc = 0.
      SORT lt_customer[] BY kunnr.
    ENDIF.

    CLEAR lv_bukrs.
    SELECT SINGLE low FROM tvarvc INTO lv_bukrs
                      WHERE name = 'CUSTOMER_INPYMT_CC'
                      AND   type = 'P'.


    LOOP AT gt_input ASSIGNING FIELD-SYMBOL(<fs_input>).

*** If customer is present in input itself ***
      IF <fs_input>-customer IS NOT INITIAL.
**Customer Number Conversion ***
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = <fs_input>-customer
          IMPORTING
            output = <fs_input>-customer.
      ENDIF.
*** if reference document is not there ****
      IF <fs_input>-ref_doc IS  INITIAL.

        l_msg = |Ref Document Number is Mandatory|.
*        APPEND VALUE #(  customer = <fs_input>-customer
*                          ref_doc = <fs_input>-ref_doc
*                          amount  = <fs_input>-amount
*                          status  = 'E'
*                          remarks = 'Ref Document Number is Mandatory'  ) TO gt_response.
*        CONTINUE.
      ENDIF.

*********************FI Posting Period Open Check*********************************

*added by Pandiarajan
*added on 28.09.2023

      lo_common_check->fi_period_check(
        EXPORTING
          posting_date =  <fs_input>-posdate          " Field of type DATS
          acct_type    =  'D'              " Account type
          com_code     =  lv_bukrs         " Company Code
        IMPORTING
          type         =  DATA(lv_type)          " Single-Character Flag
*      message      =                    " Character 100
        EXCEPTIONS
          mandatory    = 1                 " Fill POSTING_DATE , ACCT_TYPE , COM_CODE
        OTHERS         = 2 ).

      IF sy-subrc = 0 AND lv_type = 'E'.

        l_msg = | { l_msg } ; FI Posting Period Not Opened - Check with Accounts Team|.

        CLEAR : lv_type.

      ENDIF.

***************************************************************************

*** Amount checks for input whether it is there or not ****
      IF <fs_input>-amount IS INITIAL.

        l_msg = | { l_msg } ; Amount is Mandatory|.

      ENDIF.

*** Document Date ****
      IF <fs_input>-docdate IS INITIAL.

        l_msg = | { l_msg } ; Document Date Missing|.

      ENDIF.

*** Posting Date ****
      IF <fs_input>-posdate IS INITIAL.

        l_msg = | { l_msg } ; posting_date Missing|.

      ENDIF.
      CLEAR: lv_kunnr,lv_refdoc.
      lv_kunnr = <fs_input>-customer.

      CLEAR lv_refdoc_str.
      CALL FUNCTION 'STRING_REVERSE'
        EXPORTING
          string    = <fs_input>-ref_doc
          lang      = sy-langu
        IMPORTING
          rstring   = lv_refdoc_str
        EXCEPTIONS
          too_small = 1
          OTHERS    = 2.
      IF sy-subrc = 0.
        CONDENSE lv_refdoc_str NO-GAPS.
        lv_refdoc = lv_refdoc_str.
      ENDIF.

      TRANSLATE lv_refdoc TO UPPER CASE.

      "Duplicate Entries Check
      READ TABLE lt_log_chk INTO DATA(ls_log_chk) WITH KEY bukrs  = lv_bukrs
                                                           kunnr  = lv_kunnr
                                                           xblnr  = lv_refdoc
                                                           orgref = <fs_input>-ref_doc
                                                           bldat  = <fs_input>-docdate
                                                           budat  = <fs_input>-posdate.
      IF sy-subrc = 0.

        l_msg = | { l_msg } ; Duplicate Entry|.

      ENDIF.

*** Customer Number checks if present in sap or not ***
      DATA(lv_kunnr_chk) = VALUE #( lt_customer[ kunnr = lv_kunnr ]-kunnr OPTIONAL ).
      IF lv_kunnr_chk IS INITIAL.

        l_msg = | { l_msg } ; Customer Number is not present in SAP|.

      ENDIF.

      DATA(lv_custname) = VALUE #( lt_customer[ kunnr = lv_kunnr ]-name1 OPTIONAL ).
* get fiscal year and period - (requires date and company code)
      CALL FUNCTION 'BAPI_COMPANYCODE_GET_PERIOD'
        EXPORTING
          companycodeid = lv_bukrs
          posting_date  = sy-datum
        IMPORTING
          fiscal_year   = gv_fiscalyr
          fiscal_period = gv_fiscalp.

      CLEAR lv_referenceid.
      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr             = '01'
          object                  = 'ZCT_REFID'
        IMPORTING
          number                  = lv_referenceid
        EXCEPTIONS
          interval_not_found      = 1
          number_range_not_intern = 2
          object_not_found        = 3
          quantity_is_0           = 4
          quantity_is_not_1       = 5
          interval_overflow       = 6
          buffer_overflow         = 7
          OTHERS                  = 8.
      IF sy-subrc <> 0.
        CASE sy-subrc.
          WHEN '1'.
            l_msg = |interval_not_found|.
          WHEN '2'.
            l_msg = |number_range_not_intern|.
          WHEN '3'.
            l_msg = |object_not_found|.
          WHEN '4'.
            l_msg = |quantity_is_0|.
          WHEN '5'.
            l_msg = |quantity_is_not_1|.
          WHEN '6'.
            l_msg = |interval_overflow|.
          WHEN '7'.
            l_msg = |buffer_overflow|.
          WHEN '8'.
            l_msg = |Others|.
        ENDCASE.
      ENDIF.
      IF l_msg IS NOT INITIAL.

        APPEND VALUE #(   customer = <fs_input>-customer
                          ref_doc  = <fs_input>-ref_doc
                          amount   = <fs_input>-amount
                          status   = 'E'
                          remarks  = l_msg  ) TO gt_response.
        CLEAR l_msg.

      ELSE.

*** If checks is finished then Inserting data into log table ***
        INSERT INTO zfi_cust_inpymt VALUES @( VALUE #( bukrs        = lv_bukrs
                                                       kunnr        = <fs_input>-customer
                                                       xblnr        = lv_refdoc
                                                       reference_id = lv_referenceid
                                                       orgref       = <fs_input>-ref_doc
                                                       sgtxt        = lv_custname
                                                       amount       = <fs_input>-amount
                                                       process_on   = sy-datum
                                                       process_by   = sy-uname
                                                       process_time = sy-uzeit
                                                       status       = '10'
                                                       gjahr        = gv_fiscalyr
                                                       bldat        = <fs_input>-docdate
                                                       budat        = <fs_input>-posdate
                                                       doc_type     = 'DZ'  )  ).
        APPEND VALUE #(  customer = <fs_input>-customer
                          ref_doc = <fs_input>-ref_doc
                          amount  = <fs_input>-amount
                          status = 'S'
                          remarks = 'Data is inserted in log table'  ) TO gt_response.

        CLEAR <fs_input>.

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
          apiname         = 'CUSTINCOMPYT'
          ijson           = lv_data
          ojson           = v_jsonload
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
