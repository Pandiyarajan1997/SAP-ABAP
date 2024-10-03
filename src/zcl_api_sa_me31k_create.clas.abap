class ZCL_API_SA_ME31K_CREATE definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_SA_ME31K_CREATE IMPLEMENTATION.


  METHOD if_http_extension~handle_request.

    TYPES: BEGIN OF ty_msg,
             msgtyp      TYPE bapi_mtype,
             message     TYPE bapi_msg,
             vendor      TYPE lifnr,
             agreementno TYPE ebeln,
           END OF ty_msg.
    DATA: lv_bukrs  TYPE bukrs VALUE '1000',
          lv_porg   TYPE ekorg VALUE '1000',
          lv_pgrp   TYPE ekgrp VALUE '116',
          lv_vendor TYPE lifnr.

    DATA: gt_output TYPE TABLE OF zstr_sch_agreement,
          gw_input  TYPE zstr_sch_agreement.
    DATA: lt_message TYPE TABLE OF ty_msg.
    DATA: gt_item TYPE zschagr_itm_tt,
          gs_item TYPE zstr_schagr_itm.
    DATA: lt_return TYPE TABLE OF bapiret2.
    DATA: lv_data    TYPE string,
          lv_body    TYPE string,
          lv_message TYPE string.
    DATA: v_jsonload TYPE string.
    DATA: lv_date  TYPE datum,
          lv_date1 TYPE datum.
    DATA: lt_item     TYPE TABLE OF bapimeoutitem,
          lt_itemx    TYPE TABLE OF bapimeoutitemx,
          lt_account  TYPE TABLE OF bapimeoutaccount,
          lt_accountx TYPE TABLE OF bapimeoutaccountx.
    DATA: lv_fund TYPE bapiacgl09-fund.
**********Getting the input data from request ***************************************
    CALL METHOD server->request->get_cdata RECEIVING data = lv_data.
    CONDENSE lv_data NO-GAPS.
**Deserialize Converted JSON to Internal Table **
    /ui2/cl_json=>deserialize(
    EXPORTING
     json         = lv_data
     pretty_name  = /ui2/cl_json=>pretty_mode-user
    CHANGING
     data         = gw_input ). "Stock API Table
*** Checks for Vendor number present or not ***
    IF gw_input-vendor IS INITIAL.
      CLEAR lv_message.
      lv_message = 'Please give vendor number in input'.
*** If present check whether it is present in SAP ***
    ELSE.
      CLEAR lv_vendor.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = gw_input-vendor
        IMPORTING
          output = gw_input-vendor.
*** check for vendor Number ****]
      SELECT SINGLE lifnr FROM lfa1 INTO lv_vendor WHERE lifnr = gw_input-vendor.
      IF sy-subrc NE 0.
        CLEAR lv_message.
        lv_message = | Vendor Code { gw_input-vendor } is Incorrect |.
      ENDIF.
    ENDIF.
*** If initial check error arise it stops here with messagec ***
    IF lv_message IS NOT INITIAL.
      lv_body = | '"Error": '"' { lv_message } '"' |.

      v_jsonload = |'[' { lv_body } ']'|.

*Set JSON Content-Type
      CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
      CALL METHOD server->response->set_cdata( data = v_jsonload ).
    ENDIF.
    CHECK sy-subrc IS INITIAL.
    IF gw_input-agreement_date IS NOT INITIAL.
      CLEAR lv_date.
      WRITE gw_input-agreement_date TO lv_date.
    ELSE.
      CLEAR lv_date.
      WRITE  sy-datum TO lv_date.
    ENDIF.
*** Fund Center Calculations **************************************************************
    CLEAR lv_fund.
    CASE lv_date+4(2).
      WHEN 4.
        lv_fund = 'FUNDI'.
      WHEN 5.
        lv_fund = 'FUNDII'.
      WHEN 6.
        lv_fund = 'FUNDIII'.
      WHEN 7.
        lv_fund = 'FUNDIV'.
      WHEN 8.
        lv_fund = 'FUNDV'.
      WHEN 9.
        lv_fund = 'FUNDVI'.
      WHEN 10.
        lv_fund = 'FUNDVII'.
      WHEN 11.
        lv_fund = 'FUNDVIII'.
      WHEN 12.
        lv_fund = 'FUNDIX'.
      WHEN 1.
        lv_fund = 'FUNDX'.
      WHEN 2.
        lv_fund = 'FUNDXI'.
      WHEN 3.
        lv_fund = 'FUNDXII'.
      WHEN OTHERS.
    ENDCASE.
***********************************************************************************************
    IF gw_input-valid_to IS NOT INITIAL.
      CLEAR lv_date1.
      WRITE gw_input-valid_to TO lv_date1.
    ELSE.
      CLEAR lv_date1.
      WRITE  '99991231' TO lv_date1.
    ENDIF.
    SELECT SINGLE zterm FROM lfm1 INTO @DATA(lv_pmnttrms) WHERE lifnr = @lv_vendor.
    IF lv_pmnttrms IS INITIAL.
      CLEAR lv_pmnttrms.
      lv_pmnttrms = 'NT60'.
    ENDIF.
    DATA(lv_pmnttrms1) = lv_pmnttrms.
    REPLACE ALL OCCURRENCES OF 'NT' IN lv_pmnttrms1 WITH ''.
    CONDENSE lv_pmnttrms1 NO-GAPS.
    DATA(lv_days) = lv_pmnttrms1.
    DATA(ls_header) = VALUE bapimeoutheader(  comp_code = lv_bukrs
                                              doc_type = 'ZWK'
                                              creat_date = lv_date
                                              created_by = sy-uname
                                              vendor = lv_vendor
                                              langu = sy-langu
                                              purch_org = lv_porg
                                              pur_group = lv_pgrp
                                              pmnttrms = lv_pmnttrms
                                              currency = 'INR'
                                              doc_date = lv_date
                                              dscnt1_to = lv_days
                                              vper_start = lv_date
                                              vper_end = lv_date1
                                              acum_value = '100000.00' ).

    DATA(ls_headerx) = VALUE bapimeoutheaderx( comp_code = 'X'
                                               doc_type = 'X'
                                               creat_date = 'X'
                                               created_by = 'X'
                                               vendor = 'X'
                                               langu = 'X'
                                               purch_org = 'X'
                                               pur_group = 'X'
                                               pmnttrms = 'X'
                                               currency = 'X'
                                               doc_date = 'X'
                                               dscnt1_to = 'X'
                                               vper_start = 'X'
                                               vper_end = 'X'
                                               acum_value = 'X' ).
    REFRESH: gt_item[],lt_item[],lt_itemx[].
    gt_item[] = gw_input-item.
    DATA: item_no TYPE ebelp,
          lv_qty  TYPE ktmng,
          lv_amt  TYPE bapicurext.
    CLEAR: item_no,lv_qty.
    item_no = '10'.
    DATA(lv_matgrp) = 'YBSVS2'.
    LOOP AT gt_item ASSIGNING FIELD-SYMBOL(<fs_item>).
      CLEAR: lv_qty,lv_amt.
      lv_qty = <fs_item>-qty.
      lv_amt = <fs_item>-amount.
      APPEND VALUE #( item_no = item_no
                      short_text = <fs_item>-short_text
                      matl_group = lv_matgrp
                      target_qty = lv_qty
                      net_price = lv_amt
                      po_unit = 'AU'
                      po_unit_iso = 'AU'
                      orderpr_un = 'AU'
                      price_unit =  lv_qty
                      prnt_price = 'X'
                      item_cat = 'D'
                      acctasscat = 'F'
                      unlimited_dlv = 'X'
                      pckg_no = '0000504421' ) TO lt_item.
      APPEND VALUE #( item_no = item_no
                      short_text = 'X'
                      matl_group = 'X'
                      target_qty = 'X'
                      net_price = 'X'
                      po_unit = 'X'
                      po_unit_iso = 'X'
                      orderpr_un = 'X'
                      price_unit =  'X'
                      prnt_price = 'X'
                      item_cat = 'X'
                      acctasscat = 'X'
                      unlimited_dlv = 'X'
                      pckg_no = 'X' ) TO lt_itemx.
      item_no = item_no + 10.
    ENDLOOP.
    REFRESH: lt_account,lt_accountx.
    DATA: lv_serial TYPE dzekkn.
    CLEAR lv_serial.
    lv_serial = '01'.
    LOOP AT lt_item ASSIGNING FIELD-SYMBOL(<fs_it_acc>).
      APPEND VALUE #( item_no = <fs_it_acc>-item_no
                      serial_no = lv_serial
                      quantity = <fs_it_acc>-target_qty
                      net_value = <fs_it_acc>-net_price
                      gl_account = '0044976120'
                      costcenter = 'TN10GPSALE'
                      fund = lv_fund ) TO lt_account.
      APPEND VALUE #( item_no = <fs_it_acc>-item_no
                      serial_no = lv_serial
                      item_nox = 'X'
                      serial_nox = 'X'
                      quantity = 'X'
                      net_value = 'X'
                      gl_account = 'X'
                      costcenter = 'X'
                      fund = 'X' ) TO lt_accountx.
      lv_serial = lv_serial + 1.

    ENDLOOP.
******* Function module to create contract *******************************************
    DATA: ls_doc_head TYPE bapimeoutheader,
          ls_document TYPE bapimeoutheader-number.
    REFRESH: lt_return.
    CLEAR: ls_doc_head,ls_document.
    CALL FUNCTION 'BAPI_CONTRACT_CREATE'
      EXPORTING
        header             = ls_header
        headerx            = ls_headerx
      IMPORTING
        purchasingdocument = ls_document
        exp_header         = ls_doc_head
      TABLES
        return             = lt_return
        item               = lt_item
        itemx              = lt_itemx
        account            = lt_account
        accountx           = lt_accountx.
    READ TABLE lt_return INTO DATA(ls_return) WITH KEY type = 'E'.
    IF sy-subrc EQ 0.
      REFRESH: lt_message.
      LOOP AT lt_return INTO DATA(ls_ret) WHERE type = 'E'.
        DATA(ls_message) = VALUE ty_msg( msgtyp = ls_ret-type
                                         message = ls_ret-message ).
        APPEND ls_message TO lt_message.
      ENDLOOP.
    ELSE.
      REFRESH: lt_message.
      IF ls_document IS NOT INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
        DATA(ls_msg) = VALUE ty_msg( msgtyp = 'S'
                                     message = |{ 'Contract Document Created Successsfully' }|
                                     vendor = lv_vendor
                                     agreementno = ls_document ).
        APPEND ls_msg TO lt_message.
      ENDIF.
    ENDIF.
****************************************************************************************************
    IF lt_message[] IS NOT INITIAL.
** serialize the output for response ***
      CLEAR lv_body.
      /ui2/cl_json=>serialize(
      EXPORTING
       data         =  lt_message
       pretty_name  = /ui2/cl_json=>pretty_mode-user
      RECEIVING
       r_json         = lv_body ).

      v_jsonload = |'[' { lv_body } ']'|.
*Set JSON Content-Type
      CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
      CALL METHOD server->response->set_cdata( data = v_jsonload ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
