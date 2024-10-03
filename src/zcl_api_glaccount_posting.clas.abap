class ZCL_API_GLACCOUNT_POSTING definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_GLACCOUNT_POSTING IMPLEMENTATION.


  METHOD if_http_extension~handle_request.
*Created By: Samsudeen M
*Createed On: 27.07.2023
*Purpose : Posting the GL Account entries from MIS
*---------------------------------------------------------------*
    TYPES: BEGIN OF input,
             postype  TYPE zpostyp,
             refdoc   TYPE xblnr1,
             compcode TYPE bukrs,
             amount   TYPE wrbtr,
             posdate  TYPE budat,
             docdate  TYPE bldat,
             itemtxt  TYPE sgtxt,
*             costctr  TYPE kostl,
           END OF input.
    TYPES: BEGIN OF output,
             postype  TYPE zpostyp,
             refdoc   TYPE xblnr1,
             compcode TYPE bukrs,
             amount   TYPE wrbtr,
             posdate  TYPE budat,
             docdate  TYPE bldat,
             itemtxt  TYPE sgtxt,
*             costctr  TYPE kostl,
             docno    TYPE belnr_d,
             fisyear  TYPE gjahr,
             type     TYPE bapi_mtype,
             msg      TYPE string,
           END OF output.
    DATA: gt_input TYPE TABLE OF input,
          gs_input TYPE input.
    DATA: gt_output TYPE TABLE OF output.
    DATA: lr_request  TYPE REF TO if_http_request,
          lr_response TYPE REF TO if_http_response.
    DATA: lv_msg TYPE string.
    DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
    CREATE OBJECT lo_log_upd.

    DATA: lt_glaccount TYPE TABLE OF bapiacgl09,
          lt_curramnt  TYPE TABLE OF bapiaccr09.
    DATA: lt_return TYPE bapiret2_t.
    DATA: lv_fisyear TYPE bapi0002_4-fiscal_year,
          lv_month   TYPE bapi0002_4-fiscal_period,
          l_return   TYPE bapireturn1,
          lv_fund    TYPE bapiacgl09-fund.
    DATA: lv_body    TYPE string,
          v_jsonload TYPE string.
    DATA: lv_objtyp TYPE bapiache09-obj_type,
          lv_objkey TYPE bapiache09-obj_key,
          lv_objsys TYPE bapiache09-obj_sys,
          l_msg     TYPE string.


    lr_request = server->request.
    lr_response = server->response.
* Check the Calling Method
    IF lr_request->get_method( ) EQ 'POST'.
      CALL METHOD lr_request->get_cdata RECEIVING data = DATA(lv_data).
      DATA(lv_data_tmp) = |[ { lv_data } ]|.
** Deserialize the input our required input ***
      REFRESH gt_input.
      /ui2/cl_json=>deserialize(
      EXPORTING
       json         = lv_data_tmp
       pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
       assoc_arrays = abap_true
      CHANGING
       data         = gt_input ).

      IF gt_input[] IS NOT INITIAL.
        REFRESH gt_output.
        LOOP AT gt_input INTO gs_input.
          DATA(l_output) = VALUE output( ).
          MOVE-CORRESPONDING gs_input TO l_output.
          CLEAR: lv_msg.
          IF gs_input-postype IS INITIAL.
            lv_msg = |Posting Type is Missing|.
          ELSE.
            SELECT SINGLE * FROM zfi_glacc_master INTO @DATA(l_glmaster)
              WHERE type = @gs_input-postype.
            IF sy-subrc NE 0.
              lv_msg = |Posting Type is not Maintained|.
            ENDIF.
          ENDIF.
          IF gs_input-refdoc IS INITIAL.
            lv_msg = |{ lv_msg },Reference is Missing|.
          ENDIF.
          IF gs_input-compcode IS INITIAL.
            lv_msg = |{ lv_msg },Company code is Missing|.
          ENDIF.
          IF gs_input-amount IS INITIAL.
            lv_msg = |{ lv_msg },Amount is Missing|.
          ENDIF.
          IF gs_input-posdate IS INITIAL.
            lv_msg = |{ lv_msg },Posting Date is Missing|.
          ELSE.
            CLEAR: lv_fisyear,lv_month,l_return.
*** function module to get fiscal year ***
            CALL FUNCTION 'BAPI_COMPANYCODE_GET_PERIOD'
              EXPORTING
                companycodeid = gs_input-compcode
                posting_date  = gs_input-posdate
              IMPORTING
                fiscal_year   = lv_fisyear
                fiscal_period = lv_month
                return        = l_return.
            IF l_return IS INITIAL.
              DATA(lv_period) = CONV num03( lv_month ).
              CALL FUNCTION 'FI_PERIOD_CHECK'
                EXPORTING
                  i_bukrs          = gs_input-compcode
                  i_gjahr          = lv_fisyear
                  i_koart          = 'S'
                  i_monat          = lv_period
                  i_glvor          = 'RFBU'
                EXCEPTIONS
                  error_period     = 1
                  error_period_acc = 2
                  invalid_input    = 3
                  OTHERS           = 4.
              IF sy-subrc <> 0.
                CLEAR l_msg.
                CALL FUNCTION 'MESSAGE_TEXT_BUILD'
                  EXPORTING
                    msgid               = sy-msgid
                    msgnr               = sy-msgno
                    msgv1               = sy-msgv1
                    msgv2               = sy-msgv1
                    msgv3               = sy-msgv1
                    msgv4               = sy-msgv1
                  IMPORTING
                    message_text_output = l_msg.
                lv_msg = |{ lv_msg },{ l_msg }|.
              ENDIF.
            ENDIF.
          ENDIF.
          IF gs_input-docdate IS INITIAL.
            lv_msg = |{ lv_msg },Document Date is Missing|.
          ENDIF.
          IF gs_input-itemtxt IS INITIAL.
            lv_msg = |{ lv_msg },Item Text is Missing|.
          ENDIF.
*          IF gs_input-costctr IS INITIAL.
*            lv_msg = |{ lv_msg },Cost Center is Missing|.
*          ENDIF.
          SELECT SINGLE * FROM zfi_glacc_post INTO @DATA(l_duplicate_chk)
            WHERE type = @gs_input-postype
            AND xblnr = @gs_input-refdoc
            AND gjahr = @lv_fisyear
            AND errtyp EQ 'S'.
          IF sy-subrc EQ 0.
            lv_msg = |{ lv_msg },Document with Same Reference already available|.
          ELSE.
            SELECT SINGLE * FROM bkpf INTO @DATA(l_bkpf)
              WHERE bukrs = @gs_input-compcode
              AND gjahr = @lv_fisyear
              AND blart = 'SA'
              AND xblnr = @gs_input-refdoc.
            IF sy-subrc EQ 0.
              lv_msg = |{ lv_msg },Document with Same Reference already available|.
            ENDIF.
          ENDIF.

          IF lv_msg IS NOT INITIAL.
            l_output-type = 'E'.
            l_output-msg = lv_msg.
          ELSE.
*header of document
            DATA(ls_header) = VALUE bapiache09( bus_act    = 'RFBU'
                                                username   = sy-uname
                                                comp_code  = gs_input-compcode
                                                doc_date   = gs_input-docdate
                                                pstng_date = gs_input-posdate
                                                ref_doc_no = gs_input-refdoc
                                                fisc_year  = lv_fisyear
                                                doc_type   = l_glmaster-doctype ).
            DATA(lv_itmcnt) = CONV posnr_acc( '1' ).
*FUND Month Get
            CLEAR lv_fund.
            CALL FUNCTION 'ZFUND_GET'
              EXPORTING
                date       = gs_input-posdate
              IMPORTING
                lv_fund    = lv_fund
              EXCEPTIONS
                enter_date = 1
                OTHERS     = 2.
            IF sy-subrc <> 0.

            ENDIF.
            DATA(lv_glacc) = |{ l_glmaster-fgl ALPHA = IN }|.
            SHIFT lv_glacc LEFT DELETING LEADING '0'.
            DATA(lv_glacc1) = |{ l_glmaster-tgl ALPHA = IN }|.
            SHIFT lv_glacc1 LEFT DELETING LEADING '0'.

            IF l_glmaster-fcmtitem EQ abap_true.
              CLEAR: lv_glacc1.
            ELSEIF l_glmaster-tcmtitem EQ abap_true.
              CLEAR: lv_glacc.
            ELSEIF l_glmaster-fcmtitem NE abap_true AND l_glmaster-tcmtitem NE abap_true.
              CLEAR: lv_glacc, lv_glacc1.
            ENDIF.

*Debit Item Entries
            lt_glaccount = VALUE #( ( itemno_acc = lv_itmcnt
                                      gl_account = l_glmaster-fgl
                                      costcenter = l_glmaster-kostl
                                      fund       = lv_fund
                                      cmmt_item  = lv_glacc
                                      item_text  = gs_input-itemtxt ) ).

*Debit Currency Lineitems
            DATA(lv_amount) = CONV char25( gs_input-amount ).
            lt_curramnt = VALUE #( ( itemno_acc = lv_itmcnt
                                     currency = 'INR'
                                     amt_doccur = lv_amount
                                     amt_base = lv_amount  ) ).

            APPEND VALUE #( itemno_acc = lv_itmcnt + 1
                            gl_account = l_glmaster-tgl
                           " costcenter = lv_costcenter
                            fund       = lv_fund
                            cmmt_item  = lv_glacc1
                            item_text  = gs_input-itemtxt ) TO lt_glaccount.
*Credit Currency Lineitems
            APPEND VALUE #( itemno_acc = lv_itmcnt + 1
                            currency   = 'INR'
                            amt_doccur = lv_amount * -1
                            amt_base   = lv_amount * -1 ) TO lt_curramnt.
*Document Check
            CLEAR lt_return.
            CALL FUNCTION 'BAPI_ACC_DOCUMENT_CHECK'
              EXPORTING
                documentheader = ls_header
              TABLES
                accountgl      = lt_glaccount
                currencyamount = lt_curramnt
                return         = lt_return.
            DELETE lt_return WHERE id = 'RW' AND number = '609'.
            CLEAR lv_msg.
            LOOP AT lt_return ASSIGNING FIELD-SYMBOL(<fs_ret>) WHERE ( type = 'E' OR type = 'A' ).
              CLEAR l_msg.
              l_msg = |{ <fs_ret>-message }{ <fs_ret>-message_v1 }{ <fs_ret>-message_v2 }{ <fs_ret>-message_v3 }|.
              lv_msg = |{ lv_msg }/{ l_msg }|.
            ENDLOOP.
            IF sy-subrc NE 0.
              REFRESH: lt_return.
              CLEAR: lv_objkey,lv_objsys,lv_objtyp,lv_msg.
*** Function Module to create Debit note ***
              CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
                EXPORTING
                  documentheader = ls_header
                IMPORTING
                  obj_type       = lv_objtyp
                  obj_key        = lv_objkey
                  obj_sys        = lv_objsys
                TABLES
                  accountgl      = lt_glaccount
                  currencyamount = lt_curramnt
                  return         = lt_return.
              COMMIT WORK AND WAIT.
              lv_msg = |Document{ lv_objkey+0(10) } Posted Successfully|.
              l_output-docno   = lv_objkey+0(10).
              l_output-fisyear = lv_objkey+14(4).
              l_output-type    = 'S'.
              l_output-msg     = lv_msg.
              DATA(log) = VALUE zfi_glacc_post( mandt  = sy-mandt
                                               type   = l_output-postype
                                               bukrs  = l_output-compcode
                                               xblnr  = l_output-refdoc
                                               gjahr  = lv_fisyear
                                               blart  = l_glmaster-doctype
                                               bldat  = l_output-docdate
                                               budat  = l_output-posdate
                                               amount = l_output-amount
                                               sgtxt  = l_output-itemtxt
                                               fgl    = l_glmaster-fgl
                                               tgl    = l_glmaster-tgl
                                               kostl  = l_glmaster-kostl
                                               belnr  = l_output-docno
                                               errtyp = l_output-type
                                               msg    = l_output-msg ).
              MODIFY zfi_glacc_post FROM log.
            ELSE.
              l_output-type    = 'E'.
              l_output-msg     = lv_msg.
            ENDIF.
          ENDIF.
          APPEND l_output TO gt_output.
          CLEAR: gs_input,lv_msg.
        ENDLOOP.

        IF gt_output IS NOT INITIAL.
          CLEAR: lv_body,v_jsonload.
** serialize the output for response ***
          /ui2/cl_json=>serialize(
          EXPORTING
           data         =  gt_output
           pretty_name  = /ui2/cl_json=>pretty_mode-user
          RECEIVING
           r_json         = lv_body ).

*Input Entry in Log Table
          CALL METHOD lo_log_upd->log_entry_store
            EXPORTING
              apiname         = 'GLPOSTING'
              ijson           = lv_data
              ojson           = lv_body
            EXCEPTIONS
              apiname_missing = 1
              json_missing    = 2
              OTHERS          = 3.
          IF sy-subrc = 0.

          ENDIF.

          v_jsonload = | { '[' } { lv_body } { ']' } |.
*Set JSON Content-Type
          CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
          CALL METHOD server->response->set_cdata( data = v_jsonload ).
        ENDIF.
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
