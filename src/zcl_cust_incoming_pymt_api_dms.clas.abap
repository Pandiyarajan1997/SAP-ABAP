class ZCL_CUST_INCOMING_PYMT_API_DMS definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CUST_INCOMING_PYMT_API_DMS IMPLEMENTATION.


  METHOD if_http_extension~handle_request.

*** Purpose of this API is for Customer incoming Payment entry Posting ***
    TYPES: BEGIN OF ty_input,
             customer    TYPE kunnr,
             distributor TYPE kunnr,
             ref_doc     TYPE char50,
             amount      TYPE wrbtr,
             docdate     TYPE bldat,
             posdate     TYPE budat,
             remarks     TYPE char100,
             Newref      TYPE char20,
           END OF ty_input.

    DATA: gt_input TYPE STANDARD TABLE OF ty_input,
          gs_input TYPE ty_input.

    DATA: lo_common_check TYPE REF TO zcl_common_check.       "obj dec for global class common check
    CREATE OBJECT lo_common_check.

    TYPES: BEGIN OF ty_msg,
             orderid     TYPE num10,
             customer    TYPE string,
             distributor TYPE kunnr,
             ref_doc     TYPE string,
             amount      TYPE string,
             status      TYPE string,
             remarks     TYPE string,
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
    SELECT * FROM zfi_dms_cust_pay
             INTO TABLE @DATA(lt_log_chk).
    IF sy-subrc = 0.
      SORT lt_log_chk[] BY kunnr.
    ENDIF.
*** Customer Number checks ***
    CLEAR lv_bukrs.
    lv_bukrs = 'DMS1'.
*    SELECT SINGLE low FROM tvarvc INTO lv_bukrs
*                      WHERE name = 'CUSTOMER_INPYMT_CC'
*                      AND   type = 'P'.


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
      IF <fs_input>-distributor IS NOT INITIAL.
**Customer Number Conversion ***
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = <fs_input>-distributor
          IMPORTING
            output = <fs_input>-distributor.
      ENDIF.
    ENDLOOP.
    SELECT kunnr, name1,werks FROM kna1 INTO TABLE @DATA(lt_customer)
      FOR ALL ENTRIES IN @gt_input WHERE kunnr = @gt_input-customer.
    SELECT kunnr, name1,werks FROM kna1
      FOR ALL ENTRIES IN @gt_input WHERE kunnr = @gt_input-distributor
      APPENDING TABLE @lt_customer.

    SORT lt_customer[] BY kunnr.
    LOOP AT gt_input ASSIGNING <fs_input>.
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

**added by Pandiarajan
**added on 28.09.2023
*
*      lo_common_check->fi_period_check(
*        EXPORTING
*          posting_date =  <fs_input>-posdate          " Field of type DATS
*          acct_type    =  'D'              " Account type
*          com_code     =  lv_bukrs         " Company Code
*        IMPORTING
*          type         =  DATA(lv_type)          " Single-Character Flag
**      message      =                    " Character 100
*        EXCEPTIONS
*          mandatory    = 1                 " Fill POSTING_DATE , ACCT_TYPE , COM_CODE
*        OTHERS         = 2 ).
*
*      IF sy-subrc = 0 AND lv_type = 'E'.
*
*        l_msg = | { l_msg } ; FI Posting Period Not Opened - Check with Accounts Team|.
*
*        CLEAR : lv_type.
*
*      ENDIF.

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
      DATA(lv_distributor) = <fs_input>-distributor.

      TRANSLATE <fs_input>-ref_doc TO UPPER CASE.

      "Duplicate Entries Check
      READ TABLE lt_log_chk INTO DATA(ls_log_chk) WITH KEY bukrs  = lv_bukrs
                                                           kunnr  = lv_kunnr
                                                           xblnr  = <fs_input>-ref_doc
                                                           distributor = lv_distributor
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
*** Customer Number checks if present in sap or not ***
      DATA(lv_dist_chk) = VALUE #( lt_customer[ kunnr = lv_distributor ]-kunnr OPTIONAL ).
      IF lv_dist_chk IS INITIAL.

        l_msg = | { l_msg } ; Distributor Number is not present in SAP|.
      ELSE.

        DATA(lv_wrks) = VALUE #( lt_customer[ kunnr = lv_distributor ]-werks OPTIONAL ).
        IF lv_wrks IS INITIAL.
          l_msg = | { l_msg } ; Distributor-{ lv_distributor } is not mapped to any Plants in SAP|.
        ELSE.
          SELECT SINGLE gsber FROM t134g INTO @DATA(l_bis_area)
            WHERE werks =  @lv_wrks AND  spart = '10'.

          SELECT SINGLE incgl FROM zdms_distb_gl INTO @DATA(l_incom_gl)
            WHERE kunnr = @lv_distributor AND werks =  @lv_wrks.
        ENDIF.
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
          object                  = 'ZCT_REFID1'
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
                          distributor = <fs_input>-distributor
                          ref_doc  = <fs_input>-ref_doc
                          amount   = <fs_input>-amount
                          status   = 'E'
                          remarks  = l_msg  ) TO gt_response.
        CLEAR l_msg.

      ELSE.

*** If checks is finished then Inserting data into log table ***
        INSERT INTO zfi_dms_cust_pay VALUES @( VALUE #( bukrs        = lv_bukrs
                                                       kunnr        = <fs_input>-customer
                                                       xblnr        = <fs_input>-ref_doc
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
                                                       doc_type     = 'DZ'
                                                       distributor  = <fs_input>-distributor
                                                       werks        = lv_wrks
                                                       gsber        = l_bis_area
                                                       hkont        = l_incom_gl
                                                       DB_REMARKS   = <fs_input>-remarks
                                                       NEW_REF      = <fs_input>-newref )  ).
        APPEND VALUE #(   orderid = lv_referenceid
                          customer = <fs_input>-customer
                          distributor = <fs_input>-distributor
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

*      v_jsonload = | { '[' } { lv_body } { ']' } |.
      v_jsonload = lv_body.

      CALL METHOD lo_log_upd->log_entry_store
        EXPORTING
          apiname         = 'DMSCINCOMPYT'
          ijson           = lv_data
          ojson           = v_jsonload
          distributor     = gs_input-distributor
          retailer        = gs_input-customer
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
