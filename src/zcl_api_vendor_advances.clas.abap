class ZCL_API_VENDOR_ADVANCES definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_VENDOR_ADVANCES IMPLEMENTATION.


  METHOD if_http_extension~handle_request.
*----------------------------------------------------------------------------------
*&Created by: Ramakrishnan
*&Created On: 25.12.2023
*&Purpose : The Advance entry to be posted for vendor directly
*&Reference: Ramakrishnan J
*-------------------------------------------------------------------------------------

    TYPES: BEGIN OF ty_input,
             company   TYPE bukrs,
             vendor    TYPE lifnr,
             refno     TYPE xblnr,
             amount    TYPE wrbtr,
             doc_date  TYPE bldat,
             post_date TYPE budat,
             Remarks   TYPE sgtxt,
             pymttype  TYPE char01,
             adv_type  TYPE ACPI_UMSKZ,
           END OF ty_input.

    DATA: gt_input TYPE TABLE OF ty_input.

    TYPES: BEGIN OF ty_output,
             company   TYPE bukrs,
             vendor    TYPE lifnr,
             refno     TYPE xblnr,
             amount    TYPE wrbtr,
             docno     TYPE belnr_d,
             doc_date  TYPE bldat,
             post_date TYPE budat,
             type      TYPE bapi_mtype,
             msg       TYPE string,
           END OF ty_output.
    DATA: gt_output TYPE TABLE OF ty_output.

    TYPES: BEGIN OF costcenter,
             kostl TYPE kostl,
             kokrs TYPE kokrs,
           END OF costcenter.
    DATA: ls_costcenter TYPE costcenter.

    DATA: lv_data TYPE string.
    DATA: lr_data TYPE REF TO data.

    DATA: lv_body    TYPE string,
          v_jsonload TYPE string.
    DATA: lv_msg TYPE string.

    DATA: lt_payable   TYPE TABLE OF bapiacap09,
          lt_accountgl TYPE TABLE OF bapiacgl09,
          lt_currency  TYPE TABLE OF bapiaccr09,
          lt_tax       TYPE TABLE OF BAPIACTX09.
    DATA: return TYPE bapiret2_t.

    DATA: lv_fisyear TYPE bapi0002_4-fiscal_year,
          lv_month   TYPE bapi0002_4-fiscal_period,
          l_return   TYPE bapireturn1.
    DATA: lv_fund TYPE char10.

    DATA: lv_objtyp TYPE bapiache09-obj_type.
    DATA: lv_objkey TYPE bapiache09-obj_key.
    DATA: lv_objsys TYPE bapiache09-obj_sys.

    DATA: lv_pern(8) TYPE c.

    DATA: lo_logupd TYPE REF TO zcl_api_log_entries_store.
    CREATE OBJECT lo_logupd.
    "Input Of the API from Request Parameters
    CLEAR lv_data.
    CALL METHOD server->request->get_cdata RECEIVING data = lv_data.

** Deserialize the input our required input ***
    /ui2/cl_json=>deserialize(
    EXPORTING
     json         = lv_data
     pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
    CHANGING
     data         = gt_input ).

    IF gt_input IS NOT INITIAL.
      REFRESH: gt_output.
      LOOP AT gt_input ASSIGNING FIELD-SYMBOL(<fs_input>).

        IF <fs_input>-Vendor IS INITIAL.
          APPEND VALUE #( company = <fs_input>-company vendor = <fs_input>-vendor  refno = <fs_input>-refno amount = <fs_input>-amount type = 'E'
                          docno = '' doc_date = <fs_input>-doc_date post_date = <fs_input>-post_date msg = |Vendor Number Missing| ) TO gt_output.
          CONTINUE.
        ELSE.
          DATA(lv_vendor) = |{ <fs_input>-vendor ALPHA = IN }|..
        ENDIF.

        SELECT SINGLE lifnr FROM lfa1 INTO @DATA(lv_vent) WHERE lifnr = @lv_vendor.
        IF sy-subrc NE 0.
          APPEND VALUE #( company = <fs_input>-company vendor = <fs_input>-vendor  refno = <fs_input>-refno amount = <fs_input>-amount type = 'E'
                          docno = '' doc_date = <fs_input>-doc_date post_date = <fs_input>-post_date msg = |Incorrect Vendor Number| ) TO gt_output.
          CONTINUE.
        ENDIF.


        IF <fs_input>-amount IS INITIAL.
          APPEND VALUE #( company = <fs_input>-company vendor = <fs_input>-vendor  refno = <fs_input>-refno amount = <fs_input>-amount type = 'E'
                          docno = '' doc_date = <fs_input>-doc_date post_date = <fs_input>-post_date msg = |Amount Missing| ) TO gt_output.
          CONTINUE.
        ENDIF.


        IF <fs_input>-doc_date IS INITIAL.
          APPEND VALUE #( company = <fs_input>-company vendor = <fs_input>-vendor  refno = <fs_input>-refno amount = <fs_input>-amount type = 'E'
                          docno = '' doc_date = <fs_input>-doc_date post_date = <fs_input>-post_date msg = |Doc_date Missing| ) TO gt_output.
          CONTINUE.
        ENDIF.

        IF <fs_input>-post_date IS INITIAL.
          APPEND VALUE #( company = <fs_input>-company vendor = <fs_input>-vendor  refno = <fs_input>-refno amount = <fs_input>-amount type = 'E'
                          docno = '' doc_date = <fs_input>-doc_date post_date = <fs_input>-post_date msg = |Post _date Missing| ) TO gt_output.
          CONTINUE.
        ENDIF.

        IF <fs_input>-refno IS INITIAL.
          APPEND VALUE #( company = <fs_input>-company vendor = <fs_input>-vendor  refno = <fs_input>-refno amount = <fs_input>-amount type = 'E'
                          docno = '' doc_date = <fs_input>-doc_date post_date = <fs_input>-post_date msg = |Reference no is Missing| ) TO gt_output.
          CONTINUE.
        ENDIF.

        IF <fs_input>-pymttype IS INITIAL.
          APPEND VALUE #( company = <fs_input>-company vendor = <fs_input>-vendor type = 'E' msg = |Payment type Missing| ) TO gt_output.
          CONTINUE.
        ENDIF.

        IF NOT ( <fs_input>-pymttype = 'S' OR <fs_input>-pymttype = 'E' ).
          APPEND VALUE #( company = <fs_input>-company vendor = <fs_input>-vendor type = 'E' msg = |Incorrect Payment type| ) TO gt_output.
          CONTINUE.
        ENDIF.

        IF <fs_input>-adv_type IS INITIAL.
          APPEND VALUE #( company = <fs_input>-company vendor = <fs_input>-vendor type = 'E' msg = |Advance type Missing| ) TO gt_output.
          CONTINUE.
        ENDIF.

        IF NOT ( <fs_input>-adv_type = 'A' OR <fs_input>-pymttype = 'B' ).
          APPEND VALUE #( company = <fs_input>-company vendor = <fs_input>-vendor type = 'E' msg = |Incorrect Advance type| ) TO gt_output.
          CONTINUE.
        ENDIF.

        CASE <fs_input>-pymttype.
          WHEN 'S'.
            SELECT SINGLE low FROM tvarvc
              INTO @DATA(l_glaccount)
              WHERE name = 'SCM_PYMTCLR_GL'
              AND type = 'P'.
          WHEN 'E'.
            SELECT SINGLE low FROM tvarvc
              INTO @l_glaccount
              WHERE name = 'ENET_PYMTCLR_GL'
              AND type = 'P'.
        ENDCASE.

*** FUNCTION module to GET fiscal year ***
        CLEAR: lv_fisyear,lv_month,l_return.
        CALL FUNCTION 'BAPI_COMPANYCODE_GET_PERIOD'
          EXPORTING
            companycodeid = <fs_input>-company
            posting_date  = <fs_input>-post_date
          IMPORTING
            fiscal_year   = lv_fisyear
            fiscal_period = lv_month
            return        = l_return.

        "Already existing successful records whith same Reference Checks from Log Table
        TRANSLATE <fs_input>-refno TO UPPER CASE.

        SELECT SINGLE * FROM zfi_advance_log INTO @DATA(l_advlog) WHERE bukrs = @<fs_input>-company
                                                                    AND vendor = @lv_vendor
                                                                    AND gjahr = @lv_fisyear
                                                                    AND XBLNR = @<fs_input>-refno
                                                                    AND TYPE  = 'S'.
        IF sy-subrc = 0.
          APPEND VALUE #( company = <fs_input>-company vendor = <fs_input>-vendor  refno = <fs_input>-refno amount = <fs_input>-amount type = 'E'
                          docno = '' doc_date = <fs_input>-doc_date post_date = <fs_input>-post_date msg = |Reference { <fs_input>-refno } already available| ) TO gt_output.
          CONTINUE.
        ENDIF.

        "Already existing successful records whith same Reference Checks from BSIP
        SELECT SINGLE * FROM bsip INTO @DATA(l_bsik) WHERE bukrs = @<fs_input>-company
                                                     AND lifnr = @lv_vendor
                                                     AND gjahr = @lv_fisyear
                                                     AND xblnr = @<fs_input>-refno.
        IF sy-subrc = 0.

          APPEND VALUE #( company = <fs_input>-company vendor = <fs_input>-vendor  refno = <fs_input>-refno amount = <fs_input>-amount type = 'E'
                          docno = '' doc_date = <fs_input>-doc_date post_date = <fs_input>-post_date msg = |Reference { <fs_input>-refno } already available| ) TO gt_output.
          CONTINUE.
        ELSE.

          REFRESH: lt_payable,lt_accountgl,lt_currency.
*** Actual Process starts Here for Posting Invoice ****
          DATA(ls_header) = VALUE bapiache09( bus_act = 'RFBU'
                                              username = sy-uname
                                              comp_code = <fs_input>-company
                                              doc_date = <fs_input>-doc_date
                                              pstng_date = <fs_input>-post_date
                                              fisc_year = lv_fisyear
                                              doc_type = 'KZ'
                                              ref_doc_no = <fs_input>-refno ).
**** Vendor Line items to be passed ***
          lt_payable = VALUE #(
                       ( itemno_acc = '2'
                         vendor_no = lv_vendor
                         bline_date = <fs_input>-doc_date
                         item_text  = <fs_input>-Remarks
                         sp_gl_ind  = <fs_input>-adv_type ) ).

          APPEND VALUE #( itemno_acc = '2'
                          currency = 'INR'
                          amt_doccur = <fs_input>-amount
                          amt_base = <fs_input>-amount ) TO lt_currency."Vendor Line item

          CLEAR lv_fund.
          CALL FUNCTION 'ZFUND_GET'
            EXPORTING
              date       = <fs_input>-post_date
            IMPORTING
              lv_fund    = lv_fund
            EXCEPTIONS
              enter_date = 1
              OTHERS     = 2.
          IF sy-subrc <> 0.

          ENDIF.

          DATA(l_account) = CONV hkont( |{ l_glaccount ALPHA = IN }| ).
          APPEND VALUE #( itemno_acc = '1'
                          gl_account = l_account
                          fund = lv_fund
                          item_text = <fs_input>-Remarks
                          TAX_CODE  = 'I0' ) TO lt_accountgl.   "TAX_CODE  = 'I0'

          APPEND VALUE #( ITEMNO_ACC = '1'
                          gl_account = l_account
                          TAX_CODE = 'I0' ) to lt_tax.

          APPEND VALUE #( itemno_acc = '1'
                          currency = 'INR'
                          amt_doccur = <fs_input>-amount * -1
                          amt_base =  <fs_input>-amount * -1
                        ) TO lt_currency . "G/L line Items
          REFRESH: return.
*** Document Check Before posting ***
          CALL FUNCTION 'BAPI_ACC_DOCUMENT_CHECK'
            EXPORTING
              documentheader = ls_header
            TABLES
              accountgl      = lt_accountgl
              accountpayable = lt_payable
              currencyamount = lt_currency
              accounttax     = lt_tax
              return         = return.
          CLEAR lv_msg.
          LOOP AT return INTO DATA(l_ret) WHERE type = 'E' OR type = 'A'.
            lv_msg = |{ lv_msg } { l_ret-message }|.
          ENDLOOP.
          IF lv_msg IS NOT INITIAL.
            APPEND VALUE #( company = <fs_input>-company vendor = <fs_input>-vendor  refno = <fs_input>-refno amount = <fs_input>-amount type = 'E'
                            docno = '' doc_date = <fs_input>-doc_date post_date = <fs_input>-post_date msg = lv_msg ) TO gt_output.
*           "error updation in log table
            DATA(l_update) = VALUE zfi_advance_log( mandt   = sy-mandt
                                                    vendor  = <fs_input>-vendor
                                                    bukrs   = <fs_input>-company
                                                    xblnr   = <fs_input>-refno
                                                    amount  = <fs_input>-amount
                                                    bldat   = <fs_input>-doc_date
                                                    budat   = <fs_input>-post_date
                                                    sgtxt   = <fs_input>-Remarks
                                                    erdat   = sy-datum
                                                    er_time = sy-uzeit
                                                    type    = 'E'
                                                    msg     = lv_msg
                                                    saknr   = l_account ).
            MODIFY zfi_advance_log FROM l_update.
          ELSE.
*** Function Module to create Debit note ***
            CLEAR: lv_objtyp, lv_objkey, lv_objsys.
            REFRESH return.
            CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
              EXPORTING
                documentheader = ls_header
              IMPORTING
                obj_type       = lv_objtyp
                obj_key        = lv_objkey
                obj_sys        = lv_objsys
              TABLES
                accountgl      = lt_accountgl
                accountpayable = lt_payable
                currencyamount = lt_currency
                return         = return.
            IF sy-subrc = 0.
              COMMIT WORK AND WAIT.
              CLEAR lv_msg.

              APPEND VALUE #( company = <fs_input>-company vendor = <fs_input>-vendor  refno = <fs_input>-refno amount = <fs_input>-amount type = 'S'
                              docno = lv_objkey(10) doc_date = <fs_input>-doc_date post_date = <fs_input>-post_date msg = lv_msg ) TO gt_output.

              "Success Scenerios updating the log
              l_update = VALUE zfi_advance_log( mandt   = sy-mandt
                                                vendor  = <fs_input>-vendor
                                                bukrs   = <fs_input>-company
                                                xblnr   = <fs_input>-refno
                                                amount  = <fs_input>-amount
                                                bldat   = <fs_input>-doc_date
                                                budat   = <fs_input>-post_date
                                                gjahr   = lv_fisyear
                                                belnr   = lv_objkey(10)
                                                sgtxt   = <fs_input>-Remarks
                                                erdat   = sy-datum
                                                er_time = sy-uzeit
                                                type    = 'S'
                                                msg     = lv_msg
                                                saknr   = l_account ).
              MODIFY zfi_advance_log FROM l_update.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ELSE.
      v_jsonload = | { '[' }Error: Kindly check all the input filled or not { ']' } |.
    ENDIF.

    IF gt_output[] IS NOT INITIAL.

      CLEAR: lv_body,v_jsonload.
** serialize the output for response ***
      /ui2/cl_json=>serialize(
      EXPORTING
       data         =  gt_output
       pretty_name  = /ui2/cl_json=>pretty_mode-user
      RECEIVING
       r_json         = lv_body ).

*      v_jsonload = | { '[' } { lv_body } { ']' } |.
      v_jsonload = lv_body.
    ENDIF.

    CALL METHOD lo_logupd->log_entry_store
      EXPORTING
        apiname         = 'FI_ADVANCE'
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

  ENDMETHOD.
ENDCLASS.
