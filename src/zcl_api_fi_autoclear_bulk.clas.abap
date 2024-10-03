class ZCL_API_FI_AUTOCLEAR_BULK definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_FI_AUTOCLEAR_BULK IMPLEMENTATION.


  METHOD if_http_extension~handle_request.

    TYPES: BEGIN OF ty_input,
             ccode        TYPE string,
             vendor       TYPE lifnr,
             amt          TYPE string,
*             inv_refno    TYPE string,
             assignment   TYPE string,
             doc_date     TYPE string,
             posting_date TYPE string,
             pymnttype    TYPE char01,
             org_inv      TYPE ztt_fi_autoclr_bulk,
           END OF ty_input.
    DATA: gw_input TYPE ty_input.
    DATA: lv_subrc TYPE sy-subrc.
    DATA: lv_message TYPE string.
    DATA: lt_body TYPE TABLE OF string.
    DATA: lv_body TYPE string.
    DATA: v_jsonload TYPE string.

    DATA: lv_bukrs  TYPE bukrs,
          lv_vendor TYPE lifnr,
          lv_orginv TYPE belnr_d,
          lv_invref TYPE xblnr1,
          lv_amt    TYPE wrbtr,
          lv_payref TYPE zpay_ref,
          lv_assign TYPE char25,
          lv_ddate  TYPE bldat,
          lv_pdate  TYPE budat,
          lv_docno  TYPE belnr_d,
          lv_msgty  TYPE char01,
          lv_msg    TYPE char50,
          lt_docno  TYPE zfi_doc_tt.
    DATA: return TYPE TABLE OF bapiret2.

    DATA: lv_msgtxt TYPE string.
    DATA: lv_fisyear TYPE bapi0002_4-fiscal_year,
          lv_month   TYPE bapi0002_4-fiscal_period,
          l_return   TYPE bapireturn1.

    DATA : so_belnr TYPE TABLE OF feb_bsproc_belnr_rng,
           ls_belnr TYPE feb_bsproc_belnr_rng,
           so_xblnr TYPE TABLE OF feb_bsproc_xblnr_rng,
           ls_xblnr TYPE feb_bsproc_xblnr_rng.

    DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
    CREATE OBJECT lo_log_upd.

    CALL METHOD server->request->get_cdata RECEIVING data = DATA(lv_data).

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
      ELSEIF gw_input-vendor IS INITIAL.
        CLEAR lv_message.
        lv_message = 'Vendor Code Mandatory'.
        lv_subrc = '4'.
      ELSEIF gw_input-org_inv IS INITIAL.
        CLEAR lv_message.
        lv_message = 'Original invoice Document Number Mandatory'.
        lv_subrc = '4'.
*      ELSEIF gw_input-inv_refno IS INITIAL.
*        CLEAR lv_message.
*        lv_message = 'Invoice Reference No Mandatory'.
*        lv_subrc = '4'.
      ELSEIF gw_input-amt IS INITIAL.
        CLEAR lv_message.
        lv_message = 'Amount Mandatory'.
        lv_subrc = '4'.
      ELSEIF gw_input-assignment IS INITIAL.
        CLEAR lv_message.
        lv_message = 'Assignment is Mandatory'.
        lv_subrc = '4'.
      ELSEIF gw_input-doc_date IS INITIAL.
        CLEAR lv_message.
        lv_message = 'Document date is Mandatory'.
        lv_subrc = '4'.
      ELSEIF gw_input-posting_date IS INITIAL.
        CLEAR lv_message.
        lv_message = 'Posting date is Mandatory'.
        lv_subrc = '4'.
*Added On 19.05.2023
*Requested by: Nandhagopal (CFO)
*Changed by: Samsudeen M
      ELSEIF gw_input-pymnttype IS INITIAL.
        CLEAR lv_message.
        lv_message = 'Payment Type is Mandatory'.
        lv_subrc = '4'.
      ENDIF.
    ENDIF.
    IF lv_message IS INITIAL.
      LOOP AT gw_input-org_inv ASSIGNING FIELD-SYMBOL(<fs_belnr>).
        ls_belnr-low    = <fs_belnr>-orginv.
        ls_belnr-sign = 'I'.
        ls_belnr-option   = 'EQ'.
        APPEND ls_belnr TO so_belnr.
*        CLEAR : ls_belnr,ls_xblnr.
*        ls_xblnr-low    = <fs_belnr>-invref.
*        ls_xblnr-sign = 'I'.
*        ls_xblnr-option   = 'EQ'.
*        APPEND ls_xblnr TO so_xblnr.
      ENDLOOP.
      gw_input-vendor   = |{ gw_input-vendor ALPHA = IN }|.
      SELECT belnr,lifnr,xblnr,shkzg,dmbtr,blart FROM bsik
                      INTO TABLE @DATA(lt_bsik)
                      WHERE bukrs = @gw_input-ccode
                      AND belnr IN @so_belnr
                      AND lifnr = @gw_input-vendor.
*                      AND xblnr IN @so_xblnr.
      IF sy-subrc = 0.
        CLEAR : lv_amt.
        LOOP AT lt_bsik ASSIGNING FIELD-SYMBOL(<fs_bsik>).
          IF <fs_bsik>-shkzg = 'H'.
            lv_amt = lv_amt + <fs_bsik>-dmbtr.
          ELSEIF <fs_bsik>-shkzg = 'S'.
            lv_amt = lv_amt - <fs_bsik>-dmbtr.
          ENDIF.
        ENDLOOP.
        IF lv_amt LT 0.
          lv_amt = lv_amt * -1.
        ENDIF.
        IF gw_input-amt NE lv_amt.
          CLEAR lv_message.
          lv_message = 'Clearing Amount is Mismatched'.
          lv_subrc = '4'.
        ENDIF.
      ELSE.
        SELECT belnr,lifnr,xblnr,shkzg,dmbtr,blart FROM bsak
                      INTO TABLE @DATA(lt_bsak)
                      WHERE bukrs = @gw_input-ccode
                      AND belnr IN @so_belnr
                      AND lifnr = @gw_input-vendor
                      AND xblnr IN @so_xblnr.
        IF sy-subrc = 0.
          CLEAR lv_message.
          lv_message = 'Document is already cleared'.
          lv_subrc = '4'.
        ELSE.
          CLEAR lv_message.
          lv_message = 'document not found'.
          lv_subrc = '4'.
        ENDIF.
      ENDIF.
    ENDIF.
** If error arise stop the request ***
    IF lv_message IS NOT INITIAL.

      CLEAR lv_body.
      lv_body = | { '{' } { '"Error":' } { '"' } { lv_message } { '"' } { '}' } |.
      APPEND lv_body TO lt_body.
      CLEAR v_jsonload.
      v_jsonload = | { '[' } { lv_body } { ']' } |.

*Output Entry in Log Table
      CALL METHOD lo_log_upd->log_entry_store
        EXPORTING
          apiname         = 'AUTOCLEAR_INV'
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

    CLEAR: lv_bukrs,lv_vendor,lv_orginv,lv_invref,lv_amt,lv_payref,lv_assign,lv_pdate,lv_ddate,
           lv_docno,lv_msgty,lv_msg,lt_docno,return.
    lv_bukrs  = gw_input-ccode.
    lv_vendor = gw_input-vendor.
    lv_assign = gw_input-assignment.
    lv_pdate  = gw_input-posting_date.
    lv_ddate  = gw_input-doc_date.
    CLEAR: lv_fisyear,lv_month,l_return.
*** function module to get fiscal year ***
    CALL FUNCTION 'BAPI_COMPANYCODE_GET_PERIOD'
      EXPORTING
        companycodeid = lv_bukrs
        posting_date  = lv_pdate
      IMPORTING
        fiscal_year   = lv_fisyear
        fiscal_period = lv_month
        return        = l_return.
*G/L Account Fix Based on Payment Type *Changed On: 19.05.2023
    CASE gw_input-pymnttype.
      WHEN 'S'.
        SELECT SINGLE low FROM tvarvc
          INTO @DATA(l_scm_enet_gl)
          WHERE name = 'SCM_PYMTCLR_GL'
          AND type = 'P'.
      WHEN 'E'.
        SELECT SINGLE low FROM tvarvc
          INTO @l_scm_enet_gl
          WHERE name = 'ENET_PYMTCLR_GL'
          AND type = 'P'.
    ENDCASE.
    IF l_scm_enet_gl IS NOT INITIAL.
      DATA(lv_glacc) = CONV saknr( l_scm_enet_gl ).
    ENDIF.

    CHECK lv_subrc IS INITIAL.

    LOOP AT lt_bsik ASSIGNING <fs_bsik>.

      lv_orginv = <fs_bsik>-belnr.
      lv_amt    = <fs_bsik>-dmbtr.
      lv_invref = <fs_bsik>-xblnr.
*    lv_payref = gw_input-payment_reference.

** Actual Function Module Call for Clearing the vendor Invoices ***
      CALL FUNCTION 'ZAUTO_CLEARING_FOR_VENDOR_BULK'
        EXPORTING
          comp_code                = lv_bukrs
          vendor                   = lv_vendor
          org_inv_no               = lv_orginv
          amount                   = lv_amt
          inv_refno                = lv_invref
          payment_reference        = lv_payref
          assignment               = lv_assign
          doc_date                 = lv_ddate
          posting_date             = lv_pdate
          glaccount                = lv_glacc
          doc_type                 = <fs_bsik>-blart
        IMPORTING
          cleardocument_no         = lv_docno
          msg_typ                  = lv_msgty
          message                  = lv_msg
          actual_document          = lt_docno
        TABLES
          lt_return                = return
        EXCEPTIONS
          enter_compcode           = 1
          enter_vendor             = 2
          enter_inv_docno          = 3
          enter_amount             = 4
          enter_invrefno           = 5
          enter_paymentref         = 6
          enter_assignment         = 7
          enter_pdate              = 8
          enter_ddate              = 9
          document_already_cleared = 10
          document_not_found       = 11
          OTHERS                   = 12.
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
            lv_message = 'Invoice Document No Mandatory'.
          WHEN 4.
            CLEAR lv_message.
            lv_message = 'Amount Mandatory'.
          WHEN 5.
            CLEAR lv_message.
            lv_message = 'Invoice reference Mandatory'.
          WHEN 6.
            CLEAR lv_message.
            lv_message = 'Payment_reference Mandatory'.
          WHEN 7.
            CLEAR lv_message.
            lv_message = 'Assignment Mandatory'.
          WHEN 8.
            CLEAR lv_message.
            lv_message = 'Posting date Mandatory'.
          WHEN 9.
            CLEAR lv_message.
            lv_message = 'Document date Mandatory'.
          WHEN 12.
            CLEAR lv_message.
            lv_message = 'Others'.
          WHEN 11.
            CLEAR lv_message.
            lv_message = 'document_not_found'.
          WHEN 10.
            CLEAR lv_message.
            lv_message = 'document_already_cleared'.
          WHEN 11.
            CLEAR lv_message.
            lv_message = |G/L Account Missing|.
        ENDCASE.
        CLEAR lv_body.
        lv_body = | { '{' } { '"Error":' } { '"' } { lv_message } { '"' } { '}' } |.
        APPEND lv_body TO lt_body.

      ELSE.
        CLEAR lv_body.
        IF return IS NOT INITIAL.
          CLEAR lv_msgtxt.
          LOOP AT return INTO DATA(wa_return).
            lv_body = | { '{' } { '"Error":' } { '"' } { wa_return-message } { '"' } { '}' } |.
            APPEND lv_body TO lt_body.
            CLEAR lv_body.
            lv_msgtxt = |{ lv_msgtxt },{ wa_return-message }|.
          ENDLOOP.

          LOOP AT gw_input-org_inv ASSIGNING FIELD-SYMBOL(<fs_orginv>).
            "Added by Samsudeen M on 21.03.2023
            DATA(l_elog) = VALUE zhdfc_pymt_elog( mandt = sy-mandt
                                                  bukrs = lv_bukrs
                                                  belnr = <fs_orginv>-orginv
                                                  gjahr = lv_fisyear
                                                  lifnr = lv_vendor
                                                  xblnr = <fs_orginv>-invref
                                                  budat = lv_pdate
                                                  dmbtr = lv_amt
                                                  blart = 'KZ'
                                                  erdat = sy-datum
                                                  erzet = sy-uzeit
                                                  message = lv_msgtxt ).
            MODIFY zhdfc_pymt_elog FROM l_elog.
            "End of Changes on 21.03.2023
          ENDLOOP.
        ELSE.
          lv_body = | { '{' } { '"Document_No":' } { '"' } { lv_docno } { '"' } { '"Message":' } { '"' } { lv_msg } { '"' } { '}' } |.
          APPEND lv_body TO lt_body.
        ENDIF.
      ENDIF.
    ENDLOOP.
    CLEAR lv_body.
** serialize the output for response ***
    /ui2/cl_json=>serialize(
    EXPORTING
     data         =  lt_body
     pretty_name  = /ui2/cl_json=>pretty_mode-user
    RECEIVING
     r_json         =  lv_body ).
    v_jsonload = | { '[' } { lv_body } { ']' } |.

    CALL METHOD lo_log_upd->log_entry_store
      EXPORTING
        apiname         = 'AUTOCLR_INV_BULK'
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
