class ZCL_API_PO_CREATE definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .

  types:
    BEGIN OF ty_input,
        dmspurid    TYPE  char10,
        compcode    TYPE  bukrs,
        plant       TYPE  werks_d,
        vendor      TYPE  lifnr,
        doctype     TYPE  esart,
        details     TYPE  ZSD_TT_PO_ITEM,
      END OF ty_input .
  types:
    gt_bdcdata_ty TYPE STANDARD TABLE OF bdcdata .
  types:
    gt_bdcmsgcoll_ty TYPE STANDARD TABLE OF bdcmsgcoll .

  data GW_BDCMSG type BDCMSGCOLL .
  data GT_BDCDATA type GT_BDCDATA_TY .
  data GT_BDCMSGCOLL type GT_BDCMSGCOLL_TY .
  data GS_INPUT type TY_INPUT .
  data GV_MESSAGE type STRING .
  data GV_TYPE type BAPI_MTYPE .
  data GV_PLANT type WERKS_D .

  methods CREATE_PURCHASE_ORDER
    importing
      !GS_INPUT type TY_INPUT
    exporting
      !PO_NUMBER type EBELN
      !TYPE type BAPI_MTYPE
      !MESSAGE type STRING .
  PROTECTED SECTION.
private section.
ENDCLASS.



CLASS ZCL_API_PO_CREATE IMPLEMENTATION.


  METHOD create_purchase_order.
**-------------------------------------------------------------*
    DATA: lt_poitem      TYPE TABLE OF bapimepoitem,
          lt_poitemx     TYPE TABLE OF bapimepoitemx,
          lt_poschedule  TYPE TABLE OF bapimeposchedule,
          lt_poschedulex TYPE TABLE OF bapimeposchedulx,
          lt_account     TYPE TABLE OF bapimepoaccount,
          lt_accountx    TYPE TABLE OF bapimepoaccountx,
          lt_headertext  TYPE TABLE OF bapimepotext,
          lt_itemtext    TYPE TABLE OF bapimepotext,
          lt_return      TYPE bapiret2_t,
          lv_msg         TYPE string,
          lt_logitem     TYPE TABLE OF zdms_po_ret_item.
    DATA: ponumber TYPE ebeln.
    DATA: lv_item  TYPE ebelp.
**------- Header DATA for purchase order preparation-------------------*

    IF gs_input-vendor IS NOT INITIAL.
      DATA(l_lifnr) = CONV elifn( |{ gs_input-vendor ALPHA = IN }| ).
      SELECT SINGLE * FROM lfb1
        INTO @DATA(ls_lfb1)
        WHERE lifnr = @l_lifnr.
      IF sy-subrc = 0.
        DATA(l_payterms) = ls_lfb1-zterm.
      ENDIF.
      SELECT SINGLE ekorg FROM t001w
        INTO @DATA(l_purorg)
        WHERE werks = @gs_input-plant.
      DATA(ls_header) = VALUE bapimepoheader( comp_code  = gs_input-compcode
                                              doc_type   = gs_input-doctype
                                              creat_date = sy-datum
                                              vendor     = l_lifnr
                                              langu      = sy-langu
                                              pmnttrms   = l_payterms
                                              purch_org  = l_purorg
                                              pur_group  = '105'
                                              currency   = 'INR'
                                              doc_date   = sy-datum ) .

      DATA(ls_headerx) = VALUE bapimepoheaderx( comp_code  = abap_true
                                                doc_type   = abap_true
                                                creat_date = abap_true
                                                vendor     = abap_true
                                                langu      = abap_true
                                                pmnttrms   = abap_true
                                                purch_org  = abap_true
                                                pur_group  = abap_true
                                                currency   = abap_true
                                                doc_date   = abap_true ) .
    ENDIF.
**------- lineitem DATA for purchase order preparation-------------------*
    IF gs_input-details IS NOT INITIAL.

      REFRESH: lt_poitem,lt_poitemx,lt_account,lt_accountx,lt_poschedule,lt_poschedulex,lt_itemtext.
      CLEAR lv_item.
      lv_item = '00010'.

      LOOP AT gs_input-details ASSIGNING FIELD-SYMBOL(<fs_item>).

      DATA(l_matnr) = CONV matnr18( |{ <fs_item>-material ALPHA = IN }| ).
        APPEND VALUE #( po_item       = lv_item
                        material      = l_matnr
*                        batch         = <fs_item>-batch
*                        stge_loc      = 'D1' "Storage Location
                        plant         = gs_input-plant
                        quantity      = <fs_item>-qty ) TO lt_poitem.
*                        po_unit       = 'EA'
*                        unlimited_dlv = ''
*                        ret_item      = abap_true
*                        acknowl_no    = gs_input-dmspurid ) TO lt_poitem.

        APPEND VALUE #( po_item       = lv_item
                        po_itemx      = abap_true
                        material      = abap_true
*                        batch         = abap_true
*                        stge_loc      = abap_true
                        plant         = abap_true
                        quantity      = abap_true ) TO lt_poitemx.
*                        po_unit       = abap_true
*                        unlimited_dlv = abap_true
*                        ret_item      = abap_true
*                        acknowl_no    = abap_true ) TO lt_poitemx.

        APPEND VALUE #( po_item  = lv_item
                        quantity = <fs_item>-qty ) TO lt_poschedule.

        APPEND VALUE #( po_item  = lv_item
                        po_itemx = abap_true
                        quantity = abap_true ) TO lt_poschedulex.

        APPEND VALUE #( po_item   = lv_item
                        text_id   = 'F01'
                        text_form = '*'
                        text_line = gs_input-dmspurid ) TO lt_itemtext.
************fill item data for log table*******
*        APPEND VALUE #( dms_orderid  = gs_input-dmspurid
*                        posnr        = lv_item
*                        material     = l_matnr
**                        batch        = <fs_item>-batch
*                        qty          = <fs_item>-qty
*                        uom          = 'EA'
*                        erdat        = sy-datum
*                        erzet        = sy-uzeit ) TO  lt_logitem.
        lv_item = lv_item + 10.

      ENDLOOP.
    ENDIF.
*------- Header Text for Reference ---------*
    REFRESH lt_headertext.
    APPEND VALUE #(  text_id = 'F03'
                     text_form = '*'
                     text_line = gs_input-dmspurid ) TO lt_headertext.

*** Function module which creates purchase order ---------------*
    CLEAR: ponumber.
    CALL FUNCTION 'BAPI_PO_CREATE1'
      EXPORTING
        poheader         = ls_header
        poheaderx        = ls_headerx
      IMPORTING
        exppurchaseorder = ponumber
      TABLES
        return           = lt_return
        poitem           = lt_poitem
        poitemx          = lt_poitemx
        poschedule       = lt_poschedule
        poschedulex      = lt_poschedulex
        potextheader     = lt_headertext
        potextitem       = lt_itemtext.
    READ TABLE lt_return TRANSPORTING NO FIELDS WITH KEY type = 'S'.
    IF sy-subrc = 0.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
      po_number = ponumber.
      message   = | Purchase return order created - { po_number } |.
      type      = 'S'.

    ELSE.

      LOOP AT lt_return INTO DATA(lw_return) WHERE type = 'E' OR type = 'A'.
        lv_msg = | { lw_return-message } , { lv_msg } |.
      ENDLOOP.
      message = lv_msg.
      type    = 'E'.
    ENDIF.
  ENDMETHOD.


  METHOD if_http_extension~handle_request.
    "Created by: Puratchiveeran
    "Created on: 21.03.2024
    "Reference by: Ramakrishnan J
    "Purpose : Create Purchase order
*-------------------------------------------------------------*

    TYPES: BEGIN OF ty_msg,
             dmspurid  TYPE  char10,
             compcode  TYPE  bukrs,
             plant     TYPE  werks_d,
             vendor    TYPE  lifnr,
             doctype   TYPE  esart,
             po_number TYPE ebeln,
             type      TYPE bapi_mtype,
             message   TYPE string,
           END OF ty_msg.

    DATA : ls_response TYPE ty_msg.

    DATA: lv_body TYPE string.
    DATA: lv_data TYPE string.
************API log**********
    DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
    CREATE OBJECT lo_log_upd.
************dms error log**********
    DATA: lo_errorlog_dms TYPE REF TO zcl_api_dms_error_log_entries.
    CREATE OBJECT lo_errorlog_dms.

    CALL METHOD server->request->get_cdata RECEIVING data = lv_data.
    CLEAR : gs_input.
** Deserialize the input our required input ***
    /ui2/cl_json=>deserialize(
    EXPORTING
     json         = lv_data
     pretty_name  = /ui2/cl_json=>pretty_mode-user
     assoc_arrays = abap_true
    CHANGING
     data         = gs_input ).
****************contains the all validations process****************
    IF gs_input-dmspurid IS INITIAL .
      ls_response-type = 'E'.
      ls_response-message = 'DMS Order ID field is mandatory'.
    ENDIF.
    IF gs_input-compcode IS INITIAL .
      ls_response-type = 'E'.
      ls_response-message = 'Company Code field is mandatory'.
    ENDIF.
    IF gs_input-plant IS INITIAL .
      ls_response-type = 'E'.
      ls_response-message = 'Plant field is mandatory'.
    ENDIF.
    IF gs_input-vendor IS INITIAL .
      IF sy-subrc NE 0.
        ls_response-type = 'E'.
        ls_response-message = 'Vendor field is mandatory'.
      ENDIF.
    ELSE.
      DATA(l_lifnr) = CONV elifn( |{ gs_input-vendor ALPHA = IN }| ).
      SELECT SINGLE * FROM lfa1 INTO @DATA(lw_lif) WHERE lifnr = @l_lifnr.
      IF sy-subrc NE 0.
        ls_response-type = 'E'.
        ls_response-message = 'Vendor is not found'.
      ENDIF.
    ENDIF.
    IF gs_input-doctype IS INITIAL .
      ls_response-type = 'E'.
      ls_response-message = 'Doc Type field is mandatory'.
    ENDIF.

    IF ls_response-type IS INITIAL.
      CALL METHOD create_purchase_order
        EXPORTING
          gs_input  = gs_input
        IMPORTING
          po_number = DATA(lv_purord)
          message   = DATA(lv_msg)
          type      = DATA(lv_type).

      MOVE-CORRESPONDING gs_input TO ls_response.
      ls_response-po_number = lv_purord.
      ls_response-message = lv_msg.
    ENDIF.

***************fill the response********
    IF ls_response IS NOT INITIAL.
** serialize the output for response ***
      /ui2/cl_json=>serialize(
      EXPORTING
       data         =  ls_response
       pretty_name  = /ui2/cl_json=>pretty_mode-user
      RECEIVING
       r_json         = lv_body ).

*Output Entry in Log Table
      CALL METHOD lo_log_upd->log_entry_store
        EXPORTING
          apiname         = 'PO_CREATE'
          ijson           = lv_data
          ojson           = lv_body
        EXCEPTIONS
          apiname_missing = 1
          json_missing    = 2
          OTHERS          = 3.
*Set JSON Content-Type
      CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
      CALL METHOD server->response->set_cdata( data = lv_body ).
*      DELETE FROM zsd_invoice_dms WHERE invoice_no = gs_input-dms_orderid.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
