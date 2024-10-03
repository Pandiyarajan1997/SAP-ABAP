*&---------------------------------------------------------------------*
*& Report ZSD_FIN_CUST_CRBAL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsd_fin_cust_crbal.

DATA : lv_kunnr   TYPE kna1-kunnr,
       lv_fintype TYPE zsd_fin_cust_bal-fintype.

TYPES: t_kunnr TYPE RANGE OF kunnr.

SELECT-OPTIONS : so_kunnr FOR lv_kunnr.
PARAMETERS     : p_fin   TYPE zfincode.


************************data definition****************
CLASS lcl_cust_bal DEFINITION.
  PUBLIC SECTION.
*****************input json structure*************
    TYPES : BEGIN OF ty_input,
              merchantcustomerrefid TYPE string,
              phone                 TYPE string,
              updategmv             TYPE string,
              businesssegmentname   TYPE string,
            END OF ty_input.

**************amount structure***********

    TYPES : BEGIN OF ty_amnt,
              value          TYPE string,
              formattedvalue TYPE string,
              currency       TYPE string,
            END OF ty_amnt.

**************account structure***********

    TYPES : BEGIN OF ty_account,
              accountid TYPE string,
              status    TYPE string,
              balance   TYPE ty_amnt,
              limit     TYPE ty_amnt,
            END OF ty_account.
************data structure*************
    TYPES : BEGIN OF ty_data,
              merchantcustomerrefid TYPE string,
              status                TYPE string,
              statusremark          TYPE string,
              iseligiblefortxn      TYPE string,
              currentlimit          TYPE string,
              account               TYPE ty_account,
            END OF ty_data.
*****************output json structure*************
    TYPES : BEGIN OF ty_output,
              success   TYPE string,
              message   TYPE string,
              requestid TYPE string,
              timestamp TYPE string,
              data      TYPE ty_data,
            END OF ty_output.

    METHODS : fetch,
      rf_get_token IMPORTING im_custype TYPE zfincode
                   EXPORTING ex_token   TYPE zriauthstr.

ENDCLASS.

CLASS lcl_cust_bal IMPLEMENTATION.

  METHOD fetch.

**********total inv structure***********
    TYPES : BEGIN OF ty_inv,
              custno  TYPE kunnr,
              tot_inv TYPE wrbtr,
            END OF ty_inv.

**********total inv structure***********
    TYPES : BEGIN OF ty_alv,
              table TYPE zsd_fin_cust_bal,
            END OF ty_alv.


    DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
    CREATE OBJECT lo_log_upd.



    DATA : ls_injson       TYPE ty_input,
           ls_outjson      TYPE ty_output,
           iv_payload_json TYPE string,
           lv_token        TYPE string,
           gv_token        TYPE zriauthstr,
           lv_kunnr        TYPE kunnr,
           ls_balnce       TYPE  zsd_fin_cust_bal,
           lt_balnce       TYPE TABLE OF   zsd_fin_cust_bal,
           lt_totinv       TYPE TABLE OF ty_inv,
           ls_totinv       TYPE ty_inv,
           lv_amount       TYPE zdmbtr,
           lt_alv          TYPE TABLE OF ty_alv.

    DATA: lo_http_client TYPE REF TO if_http_client.


********************get the URL from the table********************
    SELECT SINGLE * FROM zfi_cf_params INTO @DATA(ls_url) WHERE zsystem  = @sy-sysid
                                                          AND   zcustype = @p_fin
                                                          AND   zfldtype = 'RFBAL'.

    IF sy-subrc = 0.
***********************get the RF customer from kna1***************
      SELECT kunnr,name1,zcustype FROM kna1 INTO TABLE @DATA(lt_kna1)
                                            WHERE kunnr  IN @so_kunnr
                                            AND zcustype = @p_fin.
*get additional RF customers from TVARVC ZRF_CUSTOMER till they are Activated

      SELECT * FROM tvarvc
         INTO TABLE @DATA(lt_tvarvc) WHERE name = 'ZRF_CUSTOMER'.

      IF sy-subrc = 0.

        DATA(lr_kunnr) = VALUE t_kunnr( FOR ls_tvarvc IN lt_tvarvc
                           LET s = 'I' o = 'EQ' IN sign = s option = o
                           ( low = |{ ls_tvarvc-low ALPHA = IN }| ) ).

        LOOP AT lr_kunnr ASSIGNING FIELD-SYMBOL(<fs_kunnr>).
          <fs_kunnr>-low = |{ <fs_kunnr>-low ALPHA = IN }|.
        ENDLOOP.

        IF lr_kunnr[] IS NOT INITIAL.
***********************get the RF customer from kna1***************
          SELECT kunnr,name1,zcustype FROM kna1 APPENDING TABLE @lt_kna1
                                                WHERE kunnr  IN @lr_kunnr.
        ENDIF.

      ENDIF.

      SORT lt_kna1 BY kunnr.
      DELETE ADJACENT DUPLICATES FROM lt_kna1 COMPARING kunnr.


      """"Get Customer Communication data
      SELECT *  FROM zcus_cf_cumm
      FOR ALL ENTRIES IN @lt_kna1
      WHERE fintype = @p_fin AND kunnr = @lt_kna1-kunnr
            AND com_type = '20'
      INTO TABLE @DATA(lt_comms).
      IF sy-subrc = 0.
        SORT : lt_comms BY kunnr.
      ENDIF.

*get the existing credit balances
      SELECT kunnr, belnr,budat,blart,shkzg,dmbtr
             FROM bsid
             INTO TABLE @DATA(lt_bsid)
             FOR ALL ENTRIES IN @lt_kna1
             WHERE bukrs = '1000' AND kunnr = @lt_kna1-kunnr AND umskz <> 'H' AND shkzg = 'H'.
      IF sy-subrc = 0.
        SORT : lt_bsid BY kunnr.

        DATA : l_total_amt TYPE wrbtr,
               lt_crbal    LIKE lt_bsid.

        REFRESH : lt_crbal.

        LOOP AT lt_bsid INTO DATA(ls_bsid).
          l_total_amt = l_total_amt + ls_bsid-dmbtr.
          AT END OF kunnr.

            APPEND VALUE #( kunnr = ls_bsid-kunnr
                            dmbtr = l_total_amt ) TO lt_crbal.
            CLEAR : l_total_amt.
          ENDAT.
        ENDLOOP.

        SORT : lt_crbal BY kunnr.
      ENDIF.
* ****************fetch the finance customer invoices***********
      SELECT custno,bukrs,status,fintype,invoiceno,invoiceamount FROM zsd_sf_cust_inv
               INTO TABLE @DATA(lt_inv)
               FOR ALL ENTRIES IN @lt_kna1
               WHERE custno    = @lt_kna1-kunnr
               AND   fintype   = @p_fin
               AND   status    IN ( '10','12','13','14','15' )
               AND   invstatus NE 'CAN'.
      IF sy-subrc = 0.
        SORT : lt_inv BY custno.
*****************calculate the pending total invoice amount*********
        LOOP AT lt_inv ASSIGNING FIELD-SYMBOL(<fs_inv>).

          ls_totinv-tot_inv =  <fs_inv>-invoiceamount + ls_totinv-tot_inv.

          AT END OF custno.
            ls_totinv-custno = <fs_inv>-custno.
            APPEND ls_totinv TO lt_totinv.
            CLEAR : ls_totinv.
          ENDAT.
        ENDLOOP.

        IF lt_totinv[] IS NOT INITIAL.
          SORT : lt_totinv BY custno.
        ENDIF.
      ENDIF.
********************get the auth token****************

      CLEAR gv_token.
      rf_get_token( EXPORTING im_custype = 'RF' IMPORTING ex_token = gv_token ).
*****************auth token conversion*******************
      DATA(create_url) = CONV string( ls_url-zfldvalue ).
      CONDENSE : gv_token.
      CLEAR    : lv_token.
      lv_token = |Bearer { gv_token }|.

      LOOP AT lt_kna1 INTO DATA(ls_kna1).
************Fill the send data rupee bal check API*********

        CLEAR : ls_injson.
        IF sy-sysid = 'DEV' OR sy-sysid = 'QAS'.
          ls_injson-merchantcustomerrefid = |{ ls_kna1-kunnr ALPHA = OUT }|.
          CONDENSE  ls_injson-merchantcustomerrefid.
        ELSE.
          ls_injson-merchantcustomerrefid = |SAP_{ ls_kna1-kunnr ALPHA = OUT }|.
          CONDENSE  ls_injson-merchantcustomerrefid.
        ENDIF.
****************read the phone num for the customer*************
        READ TABLE lt_comms INTO DATA(ls_comms) WITH KEY kunnr = ls_kna1-kunnr BINARY SEARCH.
        IF sy-subrc = 0.
          ls_injson-phone = ls_comms-phone+2(10).
        ELSE.
*******************alv display screen data filling*************88
          APPEND VALUE #( table-custno      = ls_kna1-kunnr
                          table-custname    = ls_kna1-name1
                          table-rf_custcode = ls_injson-merchantcustomerrefid
                          table-bukrs       = '1000'
                          table-fintype     = p_fin
                          table-createddate = sy-datum
                          table-createdtime = sy-uzeit
                          table-createdby   = sy-uname
                          table-type        = 'E'
                          table-msg         = |Phone number missing| ) TO lt_alv.

          CLEAR : ls_balnce.
          ls_balnce-custno      = ls_kna1-kunnr.
          ls_balnce-custname    = ls_kna1-name1.
          ls_balnce-bukrs       = '1000'.
          ls_balnce-fintype     = p_fin.
          ls_balnce-createddate = sy-datum.
          ls_balnce-createdtime = sy-uzeit.
          ls_balnce-createdby   = sy-uname.
          ls_balnce-type        = 'E'.
          ls_balnce-msg         = |Phone number missing|.
          MODIFY zsd_fin_cust_bal FROM ls_balnce.

          CONTINUE.
        ENDIF.

        ls_injson-updategmv = 'false'.
        ls_injson-businesssegmentname = 'UNKNOWN'.

*******************call the rupify URL****************
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

        lo_http_client->request->set_method(
          EXPORTING
            method = if_http_entity=>co_request_method_post ).

        lo_http_client->request->set_content_type(
          EXPORTING
            content_type = if_rest_media_type=>gc_appl_json ).

        "Header Data Fields for API Fixing
        lo_http_client->request->set_header_field( EXPORTING name  = 'Authorization' value =   lv_token ) .
*****************Serialize the INPUT JSON************
        CLEAR iv_payload_json.
        /ui2/cl_json=>serialize(
        EXPORTING
          data         =  ls_injson
          pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
        RECEIVING
          r_json         = iv_payload_json ).

        REPLACE ALL OCCURRENCES OF 'merchantcustomerrefid' IN iv_payload_json WITH 'merchantCustomerRefId'.
        REPLACE ALL OCCURRENCES OF 'phone' IN iv_payload_json WITH 'phone'.
        REPLACE ALL OCCURRENCES OF 'updategmv' IN iv_payload_json WITH 'updateGMV'.
        REPLACE ALL OCCURRENCES OF 'businesssegmentname' IN iv_payload_json WITH 'businessSegmentName'.

****************Send the response to rupify**************
        lo_http_client->request->set_cdata(
           EXPORTING
           data = iv_payload_json ).

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
          code = DATA(lv_codes) ).

        lo_http_client->response->get_status(
        IMPORTING
          reason = DATA(lv_http_error) ).
        "Actual API Response If success
        DATA(lv_response) = lo_http_client->response->get_cdata( ).

*****************DeSerialize the OUTPUT JSON************
        CLEAR : ls_outjson.
        /ui2/cl_json=>deserialize(
        EXPORTING
         json         = lv_response
         pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
        CHANGING
         data         = ls_outjson ).

******************API logs store***************
        CALL METHOD lo_log_upd->log_entry_store
          EXPORTING
            apiname         = 'RF_BAL_CHECK'
            ijson           = iv_payload_json
            ojson           = lv_response
            distributor     = ls_kna1-kunnr
          EXCEPTIONS
            apiname_missing = 1
            json_missing    = 2
            OTHERS          = 3.

        CLEAR : lv_response.
****************************calculate the final amount*****************8

        IF lv_codes = '200'.

          IF ls_outjson-success = abap_true.


            CLEAR : lv_amount, ls_totinv.
            READ TABLE lt_totinv INTO ls_totinv WITH KEY custno = ls_kna1-kunnr BINARY SEARCH.
            IF sy-subrc = 0.
*              IF ls_totinv-tot_inv GE ls_outjson-data-account-balance-value.
*                lv_amount = 0.
*              ELSE.
              lv_amount = ls_outjson-data-account-balance-value - ls_totinv-tot_inv.
*              ENDIF.
            ELSE.
              lv_amount = ls_outjson-data-account-balance-value.
            ENDIF.

*if credit balance available then add to the available credit balance
            READ TABLE lt_crbal INTO DATA(ls_crbal) WITH KEY kunnr = ls_kna1-kunnr BINARY SEARCH.
            IF sy-subrc = 0.
              lv_amount = lv_amount + ls_crbal-dmbtr.
            ENDIF.

******************modify the table customer balance table**********
            CLEAR : ls_balnce.
            ls_balnce-custno      = ls_kna1-kunnr.
            ls_balnce-custname    = ls_kna1-name1.
            ls_balnce-rf_custcode = ls_injson-merchantcustomerrefid.
            ls_balnce-cust_sts    = ls_outjson-data-status.
            ls_balnce-bukrs       = '1000'.
            ls_balnce-fintype     = p_fin.
            ls_balnce-acnt_id     = ls_outjson-data-account-accountid.
            ls_balnce-acnt_sts    = ls_outjson-data-account-status.
            ls_balnce-cr_limit    = ls_outjson-data-account-limit-value.
            ls_balnce-balance     = ls_outjson-data-account-balance-value.
            ls_balnce-cr_amount   = ls_crbal-dmbtr.
            ls_balnce-inv_amnt    = ls_totinv-tot_inv.
            ls_balnce-final_amnt  = lv_amount.
            ls_balnce-createddate = sy-datum.
            ls_balnce-createdtime = sy-uzeit.
            ls_balnce-createdby   = sy-uname.
            ls_balnce-type        = 'S'.
            ls_balnce-msg         = ls_outjson-message.
            MODIFY zsd_fin_cust_bal FROM ls_balnce.


*******************alv display screen data filling*************88
            APPEND VALUE #( table-custno      = ls_kna1-kunnr
                            table-custname    = ls_kna1-name1
                            table-rf_custcode = ls_injson-merchantcustomerrefid
                            table-cust_sts    = ls_outjson-data-status
                            table-bukrs       = '1000'
                            table-fintype     = p_fin
                            table-acnt_id     = ls_outjson-data-account-accountid
                            table-acnt_sts    = ls_outjson-data-account-status
                            table-cr_limit    = ls_outjson-data-account-limit-value
                            table-balance     = ls_outjson-data-account-balance-value
                            table-cr_amount   = ls_crbal-dmbtr
                            table-inv_amnt    = ls_totinv-tot_inv
                            table-final_amnt  = lv_amount
                            table-createddate = sy-datum
                            table-createdtime = sy-uzeit
                            table-createdby   = sy-uname
                            table-type              = 'S'
                            table-msg               = 'Customer balance fetched' ) TO lt_alv.
            CLEAR: ls_crbal, ls_totinv.
          ELSE.

            CLEAR : lv_amount, ls_totinv.
            READ TABLE lt_totinv INTO ls_totinv WITH KEY custno = ls_kna1-kunnr BINARY SEARCH.
            IF sy-subrc = 0.
*              IF ls_totinv-tot_inv GE ls_outjson-data-account-balance-value.
*                lv_amount = 0.
*              ELSE.
              lv_amount = ls_outjson-data-account-balance-value - ls_totinv-tot_inv.
*              ENDIF.
            ELSE.
              lv_amount = ls_outjson-data-account-balance-value.
            ENDIF.

*if credit balance available then add to the available credit balance
            CLEAR ls_crbal.
            READ TABLE lt_crbal INTO ls_crbal WITH KEY kunnr = ls_kna1-kunnr BINARY SEARCH.
            IF sy-subrc = 0.
              lv_amount = lv_amount + ls_crbal-dmbtr.
            ENDIF.

******************modify the table customer balance table**********
            CLEAR : ls_balnce.
            ls_balnce-custno      = ls_kna1-kunnr.
            ls_balnce-custname    = ls_kna1-name1.
            ls_balnce-bukrs       = '1000'.
            ls_balnce-fintype     = p_fin.
            ls_balnce-cr_amount   = ls_crbal-dmbtr.
            ls_balnce-inv_amnt    = ls_totinv-tot_inv.
            ls_balnce-final_amnt  = lv_amount.
            ls_balnce-createddate = sy-datum.
            ls_balnce-createdtime = sy-uzeit.
            ls_balnce-createdby   = sy-uname.
            ls_balnce-type        = 'E'.
            ls_balnce-msg         = ls_outjson-message.
            MODIFY zsd_fin_cust_bal FROM ls_balnce.

*******************alv display screen data filling*************88
            APPEND VALUE #( table-custno      = ls_kna1-kunnr
                            table-custname    = ls_kna1-name1
                            table-bukrs       = '1000'
                            table-cr_amount   = ls_crbal-dmbtr
                            table-inv_amnt    = ls_totinv-tot_inv
                            table-fintype     = p_fin
                            table-final_amnt  = lv_amount
                            table-createddate = sy-datum
                            table-createdtime = sy-uzeit
                            table-createdby   = sy-uname
                            table-type        = 'E'
                            table-msg         = ls_outjson-message ) TO lt_alv.
            CLEAR: ls_crbal, ls_totinv.
          ENDIF.

        ELSE.

          CLEAR : ls_balnce.
          ls_balnce-custno      = ls_kna1-kunnr.
          ls_balnce-custname    = ls_kna1-name1.
          ls_balnce-bukrs       = '1000'.
          ls_balnce-fintype     = p_fin.
*          ls_balnce-inv_amnt    = ls_totinv-tot_inv.
*          ls_balnce-final_amnt  = lv_amount.
          ls_balnce-createddate = sy-datum.
          ls_balnce-createdtime = sy-uzeit.
          ls_balnce-createdby   = sy-uname.
          ls_balnce-type        = 'E'.
          ls_balnce-msg         = |Error code : { lv_codes } - { ls_outjson-message }|.
          MODIFY zsd_fin_cust_bal FROM ls_balnce.

*******************alv display screen data filling*************88
          APPEND VALUE #( table-custno      = ls_kna1-kunnr
                          table-custname    = ls_kna1-name1
                          table-rf_custcode = ls_injson-merchantcustomerrefid
                          table-bukrs       = '1000'
                          table-fintype     = p_fin
                          table-inv_amnt    = ls_totinv-tot_inv
                          table-createddate = sy-datum
                          table-createdtime = sy-uzeit
                          table-createdby   = sy-uname
                          table-type              = 'E'
                          table-msg               = |Error code : { lv_codes } - { ls_outjson-message }| ) TO lt_alv.
        ENDIF.

      ENDLOOP.
    ELSE.
      MESSAGE : 'Rupify customer balance check URL not Maintained' TYPE 'S'  DISPLAY LIKE 'E'.
    ENDIF.

******************display the ALV*********************
    IF lt_alv IS NOT INITIAL AND sy-batch NE 'X'.

      CALL FUNCTION 'Z_POPUP_ALV'
*      EXPORTING
*        i_hide_column = 'MANDT'
        TABLES
          it_alv = lt_alv.

    ENDIF.


  ENDMETHOD.
  METHOD rf_get_token.
    CLEAR ex_token.

    CALL FUNCTION 'ZHR_GET_ACCESS_TOKEN_RUPIFI'
      EXPORTING
*       focrce_gen            = 'X'
        custype               = im_custype
      IMPORTING
        access_token          = ex_token
      EXCEPTIONS
        communication_failure = 1
        status_failure        = 2
        OTHERS                = 3.
    IF sy-subrc <> 0.
* Implement suitable error handling here
      CLEAR ex_token.
    ENDIF.


  ENDMETHOD.

ENDCLASS.


INITIALIZATION.

  p_fin = 'RF'.

  DATA : lobj_cust_bal TYPE REF TO lcl_cust_bal.
  CREATE OBJECT lobj_cust_bal.

START-OF-SELECTION.

  lobj_cust_bal->fetch( ).
