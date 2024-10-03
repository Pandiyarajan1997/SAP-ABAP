class ZCL_API_FI_AUTODEBIT_NOTE definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_FI_AUTODEBIT_NOTE IMPLEMENTATION.


  METHOD if_http_extension~handle_request.

    DATA: ccode     TYPE string,
          vendor    TYPE string,
          date      TYPE string,
          org_inv   TYPE string,
          inv_refno TYPE string,
          amt       TYPE string,
          disdate   TYPE string,
          disper    TYPE string.

    TYPES: BEGIN OF ty_input,
             ccode        TYPE string,
             vendor       TYPE string,
             date         TYPE string,
             org_inv      TYPE string,
             inv_refno    TYPE string,
             amt          TYPE string,
             disdate      TYPE string,
             disper       TYPE string,
             posting_date TYPE string,
           END OF ty_input.

    DATA: lwa_input TYPE ty_input.

    DATA: lv_bukrs     TYPE bukrs,
          lv_vendor    TYPE lifnr,
          lv_date      TYPE datum,
          lv_orginv    TYPE belnr_d,
          lv_invref    TYPE xblnr1,
          lv_amt       TYPE wrbtr,
          lv_ddate     TYPE datum,
          lv_dispr     TYPE zdis_per,
          lv_posdate   TYPE budat,
          lv_docno     TYPE belnr_d,
          lv_ret_amt   TYPE wrbtr,
          lv_ret_amt_s TYPE string.

    DATA: lv_msgtxt TYPE string.
    DATA: lv_fisyear TYPE bapi0002_4-fiscal_year,
          lv_month   TYPE bapi0002_4-fiscal_period,
          l_return   TYPE bapireturn1.

    DATA: lt_return TYPE TABLE OF bapiret2,
          wa_return TYPE bapiret2.

    CONSTANTS : c_comma                TYPE c VALUE ',',
                c_curly_brackets_open  TYPE char01 VALUE '{',
                c_curly_brackets_close TYPE char01 VALUE '}',
                c_quatation_mark       TYPE char01 VALUE '"',
                c_colon                TYPE char01 VALUE':'.

    DATA: lv_path_info      TYPE string,
          lv_request_method TYPE string,
          lv_response_data  TYPE string.
    DATA: it_inputparams TYPE tihttpnvp,
          ls_inputparams TYPE LINE OF tihttpnvp.

    DATA :ls_json    TYPE string,
          v_jsonload TYPE string.

    DATA: lv_response1 TYPE string.

    DATA: lv_tabix  TYPE sy-tabix,
          lv_int(6) TYPE n.

    DATA: lv_body TYPE string.

    DATA: lv_subrc TYPE sy-subrc.

    DATA: lv_message TYPE string.

    DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
    CREATE OBJECT lo_log_upd.


** get the request attributes
*    lv_path_info = server->request->get_header_field( name = '~path_info' ).
*    SHIFT lv_path_info LEFT BY 1 PLACES.
*
*    FIELD-SYMBOLS <fs_param> TYPE LINE OF tihttpnvp.

*Get Body data
    DATA: lv_data TYPE string.

    CALL METHOD server->request->get_cdata RECEIVING data = lv_data.

**Deserialize Converted JSON to Internal Table **
    /ui2/cl_json=>deserialize(
    EXPORTING
     json         = lv_data
     pretty_name  = /ui2/cl_json=>pretty_mode-user
    CHANGING
     data         = lwa_input ). "Stock API Table


    CLEAR:  lv_message, lv_subrc.
    IF lwa_input IS INITIAL.
      CLEAR lv_message.
      lv_message = 'Missing Input Parameters'.
      lv_subrc = '4'.
    ELSE.
      IF lwa_input-ccode IS INITIAL.
        CLEAR lv_message.
        lv_message = 'Company Code Mandatory'.
        lv_subrc = '4'.
      ELSEIF lwa_input-vendor IS INITIAL.
        CLEAR lv_message.
        lv_message = 'Vendor Code Mandatory'.
        lv_subrc = '4'.
      ELSEIF lwa_input-date IS INITIAL.
        CLEAR lv_message.
        lv_message = 'Debit Note Date Mandatory'.
        lv_subrc = '4'.
      ELSEIF lwa_input-org_inv IS INITIAL.
        CLEAR lv_message.
        lv_message = 'Original Invoice No Mandatory'.
        lv_subrc = '4'.
      ELSEIF lwa_input-inv_refno IS INITIAL.
        CLEAR lv_message.
        lv_message = 'Invoice Reference No Mandatory'.
        lv_subrc = '4'.
      ELSEIF lwa_input-amt IS INITIAL.
        CLEAR lv_message.
        lv_message = 'Amount Mandatory'.
        lv_subrc = '4'.
      ELSEIF lwa_input-disdate IS INITIAL.
        CLEAR lv_message.
        lv_message = 'Discount Date Mandatory'.
        lv_subrc = '4'.
      ELSEIF lwa_input-disper IS INITIAL.
        CLEAR lv_message.
        lv_message = 'Discout Period Mandatory'.
        lv_subrc = '4'.
      ELSEIF lwa_input-posting_date IS INITIAL.
        CLEAR lv_message.
        lv_message = 'Posting Date is Mandatory'.
        lv_subrc = '4'.
      ENDIF.
    ENDIF.

    IF lv_message IS NOT INITIAL.
      CONCATENATE
      '{'
      '"Error":' c_quatation_mark lv_message c_quatation_mark
      '}'
      INTO lv_body.

      CONCATENATE '[' lv_body ']'  INTO v_jsonload.

      CALL METHOD lo_log_upd->log_entry_store
        EXPORTING
          apiname         = 'DEBITNOTE'
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
    ENDIF.

    CHECK lv_subrc IS INITIAL.
    CLEAR: lv_bukrs,lv_vendor,lv_date,lv_orginv,lv_invref,lv_amt,lv_ddate,lv_dispr,lv_posdate.
    lv_bukrs = lwa_input-ccode.
    lv_vendor = lwa_input-vendor.
    lv_date = lwa_input-date.
    lv_orginv = lwa_input-org_inv.
    lv_invref = lwa_input-inv_refno.
    lv_amt = lwa_input-amt.
    lv_ddate = lwa_input-disdate.
    lv_dispr = lwa_input-disper.
    lv_posdate = lwa_input-posting_date.

    CLEAR: lv_fisyear,lv_month,l_return.
*** function module to get fiscal year ***
    CALL FUNCTION 'BAPI_COMPANYCODE_GET_PERIOD'
      EXPORTING
        companycodeid = lv_bukrs
        posting_date  = lv_posdate
      IMPORTING
        fiscal_year   = lv_fisyear
        fiscal_period = lv_month
        return        = l_return.

    CALL FUNCTION 'ZCREATE_VENDOR_DEBIT_NOTE'
      EXPORTING
        comp_code          = lv_bukrs
        vendor             = lv_vendor
        date               = lv_date
        org_inv            = lv_orginv
        invoice_refno      = lv_invref
        amount             = lv_amt
        disc_date          = lv_ddate
        dis_period         = lv_dispr
        posting_date       = lv_posdate
      IMPORTING
        document_no        = lv_docno
        amounts            = lv_ret_amt
      TABLES
        lt_return          = lt_return
      EXCEPTIONS
        enter_comp_code    = 1
        enter_vendor       = 2
        enter_inrefno      = 3
        enter_amount       = 4
        enter_date         = 5
        enter_disdate      = 6
        enter_disperiod    = 7
        incorrect_invrefno = 8
        enter_pdate        = 9
        region_missing     = 10
        OTHERS             = 11.
    IF sy-subrc <> 0.
      CASE sy-subrc.
        WHEN 1.
          CLEAR lv_message.
          lv_message = 'Company Code Mandatory'.
        WHEN 2.
          CLEAR lv_message.
          lv_message = 'Vendor Code Mandatory'.
        WHEN 3.
          CLEAR lv_message.
          lv_message = 'Invoice Reference No Mandatory'.
        WHEN 4.
          CLEAR lv_message.
          lv_message = 'Amount Mandatory'.
        WHEN 5.
          CLEAR lv_message.
          lv_message = 'Debit Note Date Mandatory'.
        WHEN 6.
          CLEAR lv_message.
          lv_message = 'Discount Date Mandatory'.
        WHEN 7.
          CLEAR lv_message.
          lv_message = 'Discout Period Mandatory'.
        WHEN 8.
          CLEAR lv_message.
          lv_message = 'Document with same invoice number already exists'.
        WHEN 9.
          CLEAR lv_message.
          lv_message = 'posting_date date is mandatory'.
        WHEN 10.
          CLEAR lv_message.
          lv_message = 'Region for Vendor is Missing'.
        WHEN OTHERS.
          CLEAR lv_message.
          lv_message = 'Unknown Error'.
      ENDCASE.

      CONCATENATE
      '{'
      '"Error":' c_quatation_mark lv_message c_quatation_mark
      '}'
      INTO lv_body.
      "Added by Samsudeen M on 21.03.2023
      DATA(l_elog) = VALUE zhdfc_pymt_elog( mandt = sy-mandt
                                            bukrs = lv_bukrs
                                            belnr = lv_orginv
                                            gjahr = lv_fisyear
                                            lifnr = lv_vendor
                                            xblnr = lv_invref
                                            budat = lv_posdate
                                            dmbtr = lv_amt
                                            blart = 'KG'
                                            dis_date = lv_dispr
                                            message = lv_message ).
      MODIFY zhdfc_pymt_elog FROM l_elog.
    ELSE.
      IF lt_return[] IS NOT INITIAL.

        lv_int = 0.
        lv_int = lines( lt_return ).

        CLEAR lv_msgtxt.
        LOOP AT lt_return INTO wa_return.

          lv_tabix = sy-tabix.
          IF lv_tabix < lv_int.
            CONCATENATE
            lv_body
            '{'
            '"Error":' c_quatation_mark wa_return-message c_quatation_mark
            '}'
            ','
            INTO lv_body.
          ELSE.
            CONCATENATE
            lv_body
            '{'
            '"Error":' c_quatation_mark wa_return-message c_quatation_mark
            '}'
            INTO lv_body.
          ENDIF.
          lv_msgtxt = |{ lv_msgtxt },{ wa_return-message }|.
        ENDLOOP.
        "Added by Samsudeen M on 21.03.2023
        l_elog = VALUE zhdfc_pymt_elog( mandt = sy-mandt
                                        bukrs = lv_bukrs
                                        belnr = lv_orginv
                                        gjahr = lv_fisyear
                                        lifnr = lv_vendor
                                        xblnr = lv_invref
                                        budat = lv_posdate
                                        dmbtr = lv_amt
                                        blart = 'KG'
                                        dis_date = lv_dispr
                                        message = lv_msgtxt ).
        MODIFY zhdfc_pymt_elog FROM l_elog.
        "End of Changes on 21.03.2023
      ELSE.
        CLEAR lv_ret_amt_s.
        lv_ret_amt_s = lv_ret_amt.
        CONCATENATE
        '{'
        '"Document_No":' c_quatation_mark lv_docno c_quatation_mark c_comma
        '"Amount":'      c_quatation_mark lv_ret_amt_s c_quatation_mark
        '}' INTO lv_body.
      ENDIF.
    ENDIF.


    CONCATENATE '[' lv_body ']'  INTO v_jsonload.

    CALL METHOD lo_log_upd->log_entry_store
      EXPORTING
        apiname         = 'DEBITNOTE'
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
