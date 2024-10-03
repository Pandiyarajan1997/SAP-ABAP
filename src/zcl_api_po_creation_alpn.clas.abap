CLASS zcl_api_po_creation_alpn DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_http_extension .

    TYPES:
**********item structure************
      BEGIN OF ty_item,
        strg_loc TYPE  lgort_d,
        material TYPE matnr18,
        quantity TYPE menge_d,
        batch    TYPE charg_d,
        uom      TYPE meins,
        netprice TYPE netwr,
      END OF ty_item .
    TYPES:
      tt_item TYPE STANDARD TABLE OF ty_item WITH NON-UNIQUE DEFAULT KEY .
    TYPES:
      BEGIN OF ty_input,
        mis_id     TYPE  zorder_id,
        comp_code  TYPE  bukrs,
        cust_code  TYPE  kunnr,
        plant      TYPE  werks_d,
        vendor     TYPE  lifnr,
        invoice_no TYPE  char20,
        po_type    TYPE  char02,
        item       TYPE  tt_item,
      END OF ty_input .
    TYPES:
      gt_bdcdata_ty TYPE STANDARD TABLE OF bdcdata .
    TYPES:
      gt_bdcmsgcoll_ty TYPE STANDARD TABLE OF bdcmsgcoll .

    DATA gs_input TYPE ty_input .

    METHODS inforecord_process
      IMPORTING
        VALUE(item) TYPE tt_item
        !vendor     TYPE lifnr
        !plant      TYPE werks_d
        !ekgrp      TYPE ekgrp
        !ekorg      TYPE ekorg
      EXPORTING
        !msg_type   TYPE bapi_mtype
        !msg        TYPE string .
    METHODS create_purchase_order
      IMPORTING
        !lifnr          TYPE lifnr
        VALUE(item_tab) TYPE tt_item
        !plant          TYPE werks_d OPTIONAL
        !po_type        TYPE char02
      EXPORTING
        !po_number      TYPE ebeln
        !type           TYPE bapi_mtype
        !message        TYPE string .
    METHODS goods_reciept
      IMPORTING
        !movement_type  TYPE bwart
        !purchase_order TYPE ebeln
      EXPORTING
        !mblnr          TYPE mblnr
        !mjahr          TYPE mjahr
        !type           TYPE bapi_mtype
        !message        TYPE string .
    METHODS miro_invoice
      IMPORTING
        !purchase_order TYPE ebeln
        !comp_code      TYPE bukrs
      EXPORTING
        !docnumber      TYPE belnr_d
        !fiscalyear     TYPE gjahr
        !type           TYPE bapi_mtype
        !message        TYPE string .
    METHODS validations
      EXPORTING
        !message       TYPE string
        !type          TYPE bapi_mtype
        !fisc_yr       TYPE gjahr
        !po_docno      TYPE belnr_d
        !po_number     TYPE ebeln
        !irn           TYPE j_1ig_irn
        !signed_qrcode TYPE j_1ig_sign_qrcode
        !pdf           TYPE fpcontent
        !ack_no        TYPE j_1ig_ack_no
        !ack_date      TYPE j_1ig_ack_date .
    METHODS fill_po_details
      IMPORTING
        !vendor    TYPE lifnr
        !customer  TYPE kunnr
        !po_number TYPE ebeln
        !docno     TYPE belnr_d
        !invoicedt TYPE dats
      EXPORTING
        !data_tab  TYPE zeinv_irn_details_st
        !message   TYPE string .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_API_PO_CREATION_ALPN IMPLEMENTATION.


  METHOD create_purchase_order.
    "Created by: Pandiarajan
    "Created on: 13.02.2024
    "Reference by: Ramakrishnan J
    "Purpose : Create Purchase Return order for DMS Process
*-------------------------------------------------------------*
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
          lt_logitem     TYPE TABLE OF zdms_po_ret_item,
          lobj_batch     TYPE REF TO zcl_api_dms_grn_inv.

    CREATE OBJECT : lobj_batch.
*    DATA: ponumber TYPE ebeln.
    DATA: lv_item   TYPE ebelp,
          lv_return TYPE char1 VALUE IS INITIAL.

    IF po_type = 'PR'.
      lv_return = abap_true.
    ELSE.
      CLEAR : lv_return.
    ENDIF.

****************get the company code wise purchase org****************
    SELECT SINGLE * FROM t024w INTO @DATA(ls_t024w)
                               WHERE werks = @plant
                               AND   ekorg = @gs_input-comp_code.

*****************get the company code wise pur grp & doc type***********
    SELECT SINGLE * FROM zmm_po_grp_maint INTO @DATA(ls_purch)
                                          WHERE type  = @po_type
                                          AND   bukrs = @gs_input-comp_code.
    IF sy-subrc NE 0.
      message = 'PO group & Type not Maintained in Table - zmm_po_grp_maint'.
      type    = 'E'.
      EXIT.
    ENDIF.

*****************get the material code details**************
    SELECT a~matnr,a~werks,a~lgort,a~labst,b~xchpf FROM mard AS a
                                       INNER JOIN marc AS b ON a~matnr = b~matnr
                                       AND a~werks = b~werks
                                       INTO TABLE @DATA(lt_mard)
                                       WHERE a~werks = @plant.
    IF sy-subrc = 0.
      SORT : lt_mard BY werks lgort matnr.
    ENDIF.

*****************get the material batch details**************
    SELECT matnr,werks,lgort,charg,clabs FROM mchb INTO TABLE @DATA(lt_mchb)
                                         WHERE werks = @plant.
    IF sy-subrc = 0.
      SORT : lt_mchb BY matnr werks lgort charg.
    ENDIF.

*------- lineitem DATA for purchase order preparation-------------------*
    REFRESH: lt_poitem,lt_poitemx,lt_account,lt_accountx,lt_poschedule,lt_poschedulex,lt_itemtext.
    CLEAR lv_item.
    lv_item = '00010'.

    LOOP AT item_tab ASSIGNING FIELD-SYMBOL(<fs_item>).

      READ TABLE lt_mard INTO DATA(ls_mard) WITH KEY werks = plant
                                                     lgort = <fs_item>-strg_loc
                                                     matnr = <fs_item>-material BINARY SEARCH.
      IF sy-subrc NE 0.
        message = |Material { <fs_item>-material } not maintained in plant - { plant } - Strg Loc. { <fs_item>-strg_loc } ; { message }|.
        CONTINUE.
      ELSEIF sy-subrc = 0 AND ls_mard-xchpf = abap_false.
        CLEAR : <fs_item>-batch.
        IF ls_mard-labst LT <fs_item>-quantity AND po_type = 'PR'.
          message = | Stock not available in material - { <fs_item>-material }, { message } |.
          CONTINUE.
        ENDIF.
      ENDIF.

**********************with batch management material*********
      IF ls_mard-xchpf = abap_true.
******************Batch check*************
        IF <fs_item>-batch IS INITIAL.
          message = |Batch is missing , { message } |.
          CONTINUE.
        ENDIF.
************batch space check**************
        CONDENSE : <fs_item>-batch.

        IF po_type = 'PR'.
**********Check the stock availablity*********
          READ TABLE lt_mchb INTO DATA(ls_mchb) WITH KEY matnr = <fs_item>-material
                                                         werks = plant
                                                         lgort = <fs_item>-strg_loc
                                                         charg = <fs_item>-batch BINARY SEARCH.
          IF ls_mchb-clabs LT <fs_item>-quantity.
            message = | Stock not available in material - { <fs_item>-material } batch - { <fs_item>-batch } , { message } |.
            CONTINUE.
          ENDIF.
          CLEAR : ls_mchb.

        ELSE.
**********************create the batch*******************
          READ TABLE lt_mchb TRANSPORTING NO FIELDS WITH KEY matnr = <fs_item>-material
                                                             werks = plant
                                                             lgort = <fs_item>-strg_loc
                                                             charg = <fs_item>-batch BINARY SEARCH.
          IF sy-subrc NE 0.

            lobj_batch->batch_create(
              EXPORTING
                material   = <fs_item>-material
                plant      = plant
                batch      = <fs_item>-batch
                storageloc = <fs_item>-strg_loc
              IMPORTING
                message    = message
                type       = type ).

            IF type = 'E'.
              EXIT.
            ELSE.
              CLEAR : message.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

***********************fill the item data**************

      APPEND VALUE #( po_item       = lv_item
                      material      = <fs_item>-material
                      batch         = <fs_item>-batch
                      stge_loc      = <fs_item>-strg_loc "Storage Location
                      plant         = plant
                      quantity      = <fs_item>-quantity
                      po_unit       = <fs_item>-uom
                      unlimited_dlv = ''
                      ret_item      = lv_return
                      acknowl_no    = gs_input-mis_id ) TO lt_poitem.

      APPEND VALUE #( po_item       = lv_item
                      po_itemx      = abap_true
                      material      = abap_true
                      plant         = abap_true
                      stge_loc      = abap_true
                      ret_item      = lv_return
                      quantity      = abap_true
                      batch         = abap_true
                      po_unit       = abap_true
                      unlimited_dlv = abap_true
                      acknowl_no    = abap_true ) TO lt_poitemx.

      APPEND VALUE #( po_item  = lv_item
                      quantity = <fs_item>-quantity ) TO lt_poschedule.

      APPEND VALUE #( po_item  = lv_item
                      po_itemx = abap_true
                      quantity = abap_true ) TO lt_poschedulex.

      APPEND VALUE #( po_item   = lv_item
                      text_id   = 'F01'
                      text_form = '*'
                      text_line = <fs_item>-batch ) TO lt_itemtext.

      lv_item = lv_item + 10.

      CLEAR : ls_mard.

    ENDLOOP.

****************exit from the method if any error means****************
    IF message IS NOT INITIAL.
      type = 'E'.
      EXIT.
    ENDIF.

*******************create inforecord*************
    CALL METHOD inforecord_process
      EXPORTING
        item     = item_tab              " Structure for Inforecord Updation
        plant    = plant
        vendor   = gs_input-vendor
        ekorg    = ls_t024w-ekorg
        ekgrp    = ls_purch-ekgrp
      IMPORTING
        msg_type = type                  " Message type: S Success, E Error, W Warning, I Info, A Abort
        msg      = message.

    IF type = 'E'.
      EXIT.
    ENDIF.

**********************get the payment terms*************
    SELECT SINGLE * FROM lfb1 INTO @DATA(ls_lfb1) WHERE lifnr = @lifnr
                                                  AND   bukrs = @gs_input-comp_code.

*------- Header DATA for purchase order preparation-------------------*

    DATA(ls_header) = VALUE bapimepoheader( comp_code  = gs_input-comp_code
                                            doc_type   = ls_purch-esart
                                            creat_date = sy-datum
                                            vendor     = lifnr
                                            langu      = sy-langu
                                            pmnttrms   = ls_lfb1-zterm
                                            purch_org  = ls_t024w-ekorg
                                            pur_group  = ls_purch-ekgrp
                                            sales_pers = gs_input-mis_id
                                            our_ref    = gs_input-invoice_no
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
                                              sales_pers = abap_true
                                              our_ref    = abap_true
                                              currency   = abap_true
                                              doc_date   = abap_true ) .

*------- Header Text for Reference ---------*
    REFRESH lt_headertext.
    APPEND VALUE #(  text_id = 'F03'
                     text_form = '*'
                     text_line = gs_input-invoice_no ) TO lt_headertext.

*** Function module which creates purchase order ---------------*
    CLEAR: po_number,message,type.
    CALL FUNCTION 'BAPI_PO_CREATE1'
      EXPORTING
        poheader         = ls_header
        poheaderx        = ls_headerx
      IMPORTING
        exppurchaseorder = po_number
      TABLES
        return           = lt_return
        poitem           = lt_poitem
        poitemx          = lt_poitemx
        poschedule       = lt_poschedule
        poschedulex      = lt_poschedulex
        potextheader     = lt_headertext
        potextitem       = lt_itemtext.

    READ TABLE lt_return TRANSPORTING NO FIELDS WITH KEY type = 'S'.
    IF sy-subrc = 0 AND po_number IS NOT INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      message   = | Purchase order created - { po_number } |.
      type      = 'S'.

    ELSE.

      LOOP AT lt_return INTO DATA(lw_return) WHERE type = 'E' OR type = 'A'.
        message = | { lw_return-message } , { message } |.
      ENDLOOP.
      type    = 'E'.
    ENDIF.

    "Updating Log table
    IF gs_input-mis_id IS NOT INITIAL.
      UPDATE zalpn_po_header  SET   message   = message type  = type
                                    status    = '13'    ebeln = po_number
                              WHERE mis_ordid = gs_input-mis_id.
    ENDIF.

  ENDMETHOD.


  METHOD goods_reciept.
    "Created by: Pandiarajan
    "Created on: 25.07.2024
    "Reference by: Ramakrishnan J
    "Purpose: Goods Reciept for DMS Distributor
*-------------------------------------------------------------------------*
    DATA: lw_goodsmvt_header TYPE bapi2017_gm_head_01,
          lt_goodsmvt_item   TYPE STANDARD TABLE OF bapi2017_gm_item_create,
          lw_goodsmvt_item   TYPE bapi2017_gm_item_create,
          lt_return          TYPE STANDARD TABLE OF bapiret2.
    DATA: lw_goodsmvt_headret TYPE bapi2017_gm_head_ret,
          goodsmvt_code_tmp   TYPE bapi2017_gm_code,
          l_return            TYPE bapiret2.
    DATA: po_header       TYPE bapiekkol,
          lt_po_htxt      TYPE TABLE OF bapiekkotx,
          lt_po_lineitems TYPE TABLE OF bapiekpo,
          lt_item_text    TYPE TABLE OF bapiekpotx,
          lt_ret          TYPE TABLE OF bapireturn,
          lv_msg          TYPE string,
          lv_inv          TYPE belnr_d.

    DATA: lv_item TYPE i.

    IF purchase_order IS NOT INITIAL.
*--- Function Module to get Purchase Order Details ----*
      CLEAR: po_header,lt_ret,lt_po_lineitems.
      CALL FUNCTION 'BAPI_PO_GETDETAIL'
        EXPORTING
          purchaseorder   = purchase_order
          item_texts      = 'X'
        IMPORTING
          po_header       = po_header
        TABLES
          po_header_texts = lt_po_htxt
          po_items        = lt_po_lineitems
          po_item_texts   = lt_item_text
          return          = lt_ret.

      IF po_header IS NOT INITIAL AND lt_po_lineitems IS NOT INITIAL.
        DELETE lt_item_text WHERE text_line = space.
*------- Material Document Which Means Goods Receipt -----------*
*-- Header DATA for grn PROCESS ----*
        CLEAR: lw_goodsmvt_header.
        goodsmvt_code_tmp                 = '01'.
        lw_goodsmvt_header-pstng_date     = sy-datum.
        lw_goodsmvt_header-doc_date       = sy-datum.
        lw_goodsmvt_header-pr_uname       = sy-uname.
        lw_goodsmvt_header-header_txt     = gs_input-mis_id.
        lw_goodsmvt_header-bill_of_lading = po_header-po_number.
        lw_goodsmvt_header-ref_doc_no     = gs_input-invoice_no.
*---- Material Document Item Data -----*
        REFRESH lt_goodsmvt_item.
        LOOP AT lt_po_lineitems INTO DATA(lw_po_lineitems).
*          DATA(lv_batch) = VALUE #( lt_item_text[ po_number = lw_po_lineitems-po_number
*                                                  po_item = lw_po_lineitems-po_item
*                                                  text_id = 'F01' ]-text_line OPTIONAL ).

          APPEND VALUE #( move_type            = movement_type "Movement Type
                          mvt_ind              = 'B'
                          plant                = lw_po_lineitems-plant "Plant
                          material             = lw_po_lineitems-material "Material
                          entry_qnt            = lw_po_lineitems-quantity "Batch Number "PO Quantity OR GRN Quantity
                          po_number            = lw_po_lineitems-po_number "PO Number
                          po_item              = lw_po_lineitems-po_item
                          batch                = lw_goodsmvt_item-batch ) TO lt_goodsmvt_item.

*          CLEAR : lv_batch.
        ENDLOOP.

        REFRESH lt_return.
        CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
          EXPORTING
            goodsmvt_header  = lw_goodsmvt_header
            goodsmvt_code    = goodsmvt_code_tmp
          IMPORTING
            goodsmvt_headret = lw_goodsmvt_headret
          TABLES
            goodsmvt_item    = lt_goodsmvt_item
            return           = lt_return.

        IF lw_goodsmvt_headret-mat_doc IS NOT INITIAL.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait   = 'X'
            IMPORTING
              return = l_return.

          mblnr   = lw_goodsmvt_headret-mat_doc. "Material Document Number Or GRN Number
          mjahr   = lw_goodsmvt_headret-doc_year. "Material Document Year
          message = | Purchase return order delivery created - { mblnr } - { mjahr } |.
          type    = 'S'.

**************Invoice number range generation*************
*
**get the distributor wise number range**************
*          SELECT SINGLE * FROM zdms_po_re_seris INTO @DATA(ls_series) WHERE distributor = @gs_input-distributor.
*          IF sy-subrc = 0.
*            IF ls_series-num_range EQ '999999'.
*              message = | { message } , Distributor Number range going to exceed |.
*            ENDIF.
*          ELSE.
*            ls_series-distributor = gs_input-distributor.
*            ls_series-plant       = gv_plant.
*            ls_series-num_range   = '000001'.
*            ls_series-mandt       = sy-mandt.
*          ENDIF.
*          lv_inv     = |{ ls_series-plant }{ ls_series-num_range }|.
*          invoice_no = lv_inv.
*************update the po return log table**************
*          ls_series-num_range = ls_series-num_range + 1.
*          MODIFY zdms_po_re_seris FROM ls_series.
*          CLEAR : ls_series,lv_inv.
*
        ELSE.
*
          LOOP AT lt_return INTO DATA(lw_return) WHERE type = 'E' OR type = 'A'.
            lv_msg = | { lw_return-message } , { lv_msg } |.
          ENDLOOP.
          message = lv_msg.
          type    = 'E'.
        ENDIF.

        IF gs_input-mis_id IS NOT INITIAL.
          UPDATE zalpn_po_header SET  message    = message     type  = type
                                      mjahr = mjahr
                                      status     = '14'        mblnr = mblnr
                                WHERE mis_ordid = gs_input-mis_id.
        ENDIF.

      ELSE.
        type    = 'E'.
        message = 'purchase order number Incorrect'.
      ENDIF.

    ELSE.
      type    = 'E'.
      message = 'purchase order number missing'.
    ENDIF.

  ENDMETHOD.


  METHOD if_http_extension~handle_request.
    "Created by: Pandiarajan
    "Created on: 25.07.2024
    "Reference by: Ramakrishnan J
    "Purpose : Create Purchase order to MIRO Process
*-------------------------------------------------------------*

    TYPES: BEGIN OF ty_msg,
             mis_id    TYPE  zorder_id,
             comp_code TYPE  bukrs,
             cust_code TYPE  kunnr,
             plant     TYPE  werks_d,
             vendor    TYPE  lifnr,
             po_type   TYPE  char02,
*             invoice_no TYPE  char20,
             po_number TYPE  ebeln,
             docno     TYPE  belnr_d,
             fisc_yr   TYPE  gjahr,
*             irn       TYPE  j_1ig_irn,
*             qr_code   TYPE  j_1ig_sign_qrcode,
*             pdf       TYPE  fpcontent,
*             ack_no    TYPE  j_1ig_ack_no,
*             ack_date  TYPE  j_1ig_ack_date,
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

    DATA : lo_main TYPE REF TO zcl_dms_einvoice_process.
    CREATE OBJECT lo_main.
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

***************check the PO type*************
    IF gs_input-po_type IS INITIAL.
      gs_input-po_type = 'PO'.
    ENDIF.
***************input alpha conversion************
    gs_input-vendor    = |{ gs_input-vendor ALPHA = IN }|.
    gs_input-cust_code = |{ gs_input-cust_code ALPHA = IN }|.

    TRANSLATE gs_input-invoice_no TO UPPER CASE.
****************contains the all validations process****************
    CLEAR : ls_response.
    CALL METHOD validations
      IMPORTING
        message   = ls_response-message
        po_docno  = ls_response-docno
        fisc_yr   = ls_response-fisc_yr
        po_number = ls_response-po_number
        type      = ls_response-type.
*       irn       = ls_response-irn
*        pdf       = ls_response-pdf.
*        signed_qrcode = ls_response-qr_code
*        ack_no        = ls_response-ack_no
*        ack_date      = ls_response-ack_date.
***************fill the response********
    IF ls_response-type IS INITIAL.
      ls_response-type = 'E'.
    ENDIF.
    ls_response-plant      = gs_input-plant.
    ls_response-mis_id     = gs_input-mis_id.
    ls_response-cust_code  = gs_input-cust_code.
    ls_response-vendor     = gs_input-vendor.
    ls_response-comp_code  = gs_input-comp_code.
    ls_response-po_type    = gs_input-po_type.
*****************generate PDF**************
*    IF ls_response-po_docno IS NOT INITIAL AND ls_response-pdf IS INITIAL.
*
*      SELECT SINGLE * FROM zdms_po_return INTO @DATA(ls_header) WHERE dms_ordid = @gs_input-mis_id.
*** api call
*      SELECT SINGLE * FROM  zdms_invoice_irn INTO @DATA(lw_irn)
*                                             WHERE docno    = @ls_response-po_docno
*                                             AND   doc_year = @ls_header-mjahr.
*      CALL METHOD fill_po_details
*        EXPORTING
*          vendor    = gs_input-vendor                 " Account Number of Vendor or Creditor
*          customer  = gs_input-cust_code                 " Customer Number
*          po_number = ls_response-po_number                 " Purchasing Document Number
*          docno     = ls_response-po_docno                 " Assignment of Item Numbers: Material Doc. - Purchasing Doc.
*          invoicedt = sy-datum                 " Field of type DATS
*        IMPORTING
*          data_tab  = DATA(lw_invoice_tab)                 " Structure for IRN Generation API
*          message   = ls_response-message.
**        message          = return.
*      CALL METHOD lo_main->generate_pdf
*        EXPORTING
*          inv_details      = lw_invoice_tab
*          inv_no           = ls_response-po_docno
*          inv_date         = ls_header-inv_date
*          irn              = lw_irn-irn
*          signed_inv       = lw_irn-signed_inv
*          signed_qrcode    = lw_irn-signed_qrcode
*          po_number        = ls_response-po_number
*          seller_invoiceno = CONV vbeln( gs_input-invoice_no )
*        IMPORTING
*          pdf              = ls_response-pdf.
**        return           = return.
*    ENDIF.

    IF ls_response IS NOT INITIAL.
*********************error log***********
      IF ls_response-type = 'E'.
        lo_errorlog_dms->log_entry_store(
          EXPORTING
            type                = 24
            status              = 10
            dms_orderid         = ls_response-mis_id
            distributor         = ls_response-cust_code
            plant               = gs_input-plant
            msg                 = ls_response-message ).
      ENDIF.
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
          apiname         = 'ALPN_PO_MIRO'
          ijson           = lv_data
          ojson           = lv_body
          distributor     = gs_input-cust_code
        EXCEPTIONS
          apiname_missing = 1
          json_missing    = 2
          OTHERS          = 3.
*Set JSON Content-Type
      CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
      CALL METHOD server->response->set_cdata( data = lv_body ).
      DELETE FROM zsd_invoice_dms WHERE invoice_no = gs_input-mis_id.
    ENDIF.

  ENDMETHOD.


  METHOD inforecord_process.
    "Created by: Pandiarajan
    "Created on: 13.02.2024
    "Reference by: Ramakrishnan J
    "Purpose: Info record Creation and Updation for DMS Purchasing
*----------------------------------------------------------------------*
    DATA : lv_overallprice TYPE netwr,
           lv_overallqty   TYPE fkimg.
    DATA lv_price TYPE char20.
    DATA : lv_tax     TYPE mwskz,
           lv_msg     TYPE string,
*           lv_type    TYPE bapi_mtype,
           lt_logitem TYPE TABLE OF zdms_po_ret_item,
           lv_item    TYPE posnr VALUE '000010'.

*----- Inforecord Details based on Invoice data --*

    SELECT a~infnr,a~matnr,a~lifnr,b~ekorg,b~werks FROM eina AS a INNER JOIN eine AS b ON a~infnr = b~infnr
                                                   INTO TABLE @DATA(lt_inforecord)
*                                                   FOR ALL ENTRIES IN @item
                                                   WHERE "a~matnr = @item-material
                                                         a~lifnr = @vendor
                                                   AND   b~ekorg = @ekorg
                                                   AND   b~werks = @plant.
    IF sy-subrc = 0.
      SORT lt_inforecord[] BY matnr werks.
    ENDIF.
***************fetch the distributor plant region***************
    SELECT SINGLE werks,regio FROM t001w INTO @DATA(ls_plant) WHERE werks = @plant.
***************fetch the distributor plant region***************
    SELECT SINGLE lifnr,regio,telf1 FROM lfa1 INTO @DATA(ls_vendor) WHERE lifnr = @vendor.

**************check the taxcode************
    CLEAR : lv_tax.
    IF ls_plant-regio = ls_vendor-regio.
      lv_tax = 'PD'.          "CGST / SGCT - 18
    ELSE.
      lv_tax = 'PE'.          "IGST
    ENDIF.

*----- Inforecord Creation and Updation Process -----*
    SORT : item[] BY material.

    LOOP AT item ASSIGNING FIELD-SYMBOL(<fs_item>).

**************Get the material price************
      lv_overallprice = <fs_item>-netprice + lv_overallprice.
      lv_overallqty   = <fs_item>-quantity + lv_overallqty.

***********fill item data for log table*******
      APPEND VALUE #( dms_orderid  = gs_input-mis_id
                      posnr        = lv_item
                      material     = <fs_item>-material
                      comp_code    = gs_input-comp_code
                      batch        = <fs_item>-batch
                      qty          = <fs_item>-quantity
                      uom          = <fs_item>-uom
                      price        = <fs_item>-netprice
                      taxcode      = lv_tax
                      erdat        = sy-datum
                      erzet        = sy-uzeit ) TO  lt_logitem.
      lv_item = lv_item + 10.

      AT END OF material.

        lv_overallprice = lv_overallprice / lv_overallqty.
        WRITE lv_overallprice TO lv_price.
        CONDENSE lv_price.

**************Inforecord creation process***********
        DATA(l_data) = VALUE zdms_inforecord_st( matnr      = <fs_item>-material
                                                 lifnr      = vendor
                                                 werks      = plant
                                                 vrkme      = <fs_item>-uom
                                                 mwskz      = lv_tax
                                                 unit_price = lv_price
                                                 valid_from = sy-datum
                                                 telf1      = ls_vendor-telf1
                                                 ekgrp      = ekgrp
                                                 ekorg      = ekorg ).
        "Info Record Existence Checks
        DATA(ls_inforecord) = VALUE #( lt_inforecord[ matnr = <fs_item>-material
                                                      werks = plant ] OPTIONAL ).
        IF ls_inforecord IS INITIAL.
******************Inforecord Creation
          CALL FUNCTION 'ZDMS_INFORECORD_CREATION2'
            EXPORTING
              im_input = l_data
            IMPORTING
              msg_type = msg_type
              msg      = lv_msg.
        ELSE.
******************Inforecord Updation
          CALL FUNCTION 'ZDMS_INFORECORD_UPDATION2'
            EXPORTING
              im_input = l_data
            IMPORTING
              msg_type = msg_type
              msg      = lv_msg.
        ENDIF.

        msg      = |{ msg } , { lv_msg }|.
        CLEAR : ls_inforecord,l_data,lv_overallprice,lv_overallqty,lv_price.

      ENDAT.

      IF msg_type = 'E'.
        EXIT.
      ENDIF.

    ENDLOOP.

    IF msg_type = 'E'.
      EXIT.
    ENDIF.
*************data store to log table header & item*************
    IF gs_input IS NOT INITIAL.

      DATA(ls_loghead) = VALUE zalpn_po_header( mis_ordid   = gs_input-mis_id
                                                ref_invno   = gs_input-invoice_no
                                                customer    = gs_input-cust_code
                                                plant       = gs_input-plant
                                                vendor      = gs_input-vendor
                                                comp_code   = gs_input-comp_code
                                                po_type     = gs_input-po_type
                                                status      = '12'
                                                ernam       = sy-uname
                                                erdat       = sy-datum
                                                erzet       = sy-uzeit
                                                message     = msg
                                                type        = msg_type ) .

      MODIFY zalpn_po_header FROM ls_loghead.
      MODIFY zdms_po_ret_item FROM TABLE lt_logitem.
      IF sy-subrc = 0.
        COMMIT WORK.
      ELSE.
        ROLLBACK WORK.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD miro_invoice.
    "Created by: Pandiarajan
    "Created on: 25.07.2024
    "Reference by: Ramakrishnan J
    "Purpose : Creating Invoice Document (MIRO) based on Purchase Order
*--------------------------------------------------------------------------*
    DATA: lw_header TYPE bapi_incinv_create_header.
    DATA: lt_item TYPE STANDARD TABLE OF bapi_incinv_create_item,
          lw_item TYPE bapi_incinv_create_item.
    DATA: l_invno TYPE bapi_incinv_fld-inv_doc_no,
          l_fyear TYPE bapi_incinv_fld-fisc_year.
    DATA: lv_gross_amt TYPE bapi_rmwwr.
    DATA: lv_tcs TYPE bapi_rmwwr.
    DATA: lv_item TYPE rblgp.
    DATA:e_fwnav TYPE bset-fwste,
         e_fwnvv TYPE bset-fwste,
         e_fwste TYPE bset-fwste,
         e_fwast TYPE bset-fwste.
    DATA: lv_uom  TYPE t006-msehi,
          lv_flag TYPE isofields-unique.
    DATA: lt_caltax TYPE TABLE OF rtax1u15.
    DATA: l_return TYPE bapiret2.
    DATA: lv_msg   TYPE string.
    DATA: lt_return TYPE TABLE OF bapiret2.
    DATA: po_header       TYPE bapiekkol,
          lt_po_htxt      TYPE TABLE OF bapiekkotx,
          lt_po_lineitems TYPE TABLE OF bapiekpo,
          lt_ret          TYPE TABLE OF bapireturn,
          lv_inv          TYPE belnr_d.

    IF purchase_order IS NOT INITIAL.

      CLEAR: po_header,lt_ret,lt_po_lineitems,lt_po_htxt.
      CALL FUNCTION 'BAPI_PO_GETDETAIL'
        EXPORTING
          purchaseorder   = purchase_order
        IMPORTING
          po_header       = po_header
        TABLES
          po_header_texts = lt_po_htxt
          po_items        = lt_po_lineitems
          return          = lt_ret.

      IF po_header IS NOT INITIAL AND lt_po_lineitems IS NOT INITIAL.

        REFRESH: lt_item.
        CLEAR: lv_gross_amt,lv_item.
        lv_item = '000001'.

*******************get the material doc no***************
        SELECT a~ebeln,
               a~ebelp,
               a~mblnr,
               a~mjahr,
               a~zeile FROM mseg AS a
              INTO TABLE @DATA(lt_po)
              WHERE a~ebeln = @purchase_order.
        IF sy-subrc = 0.
          SORT : lt_po BY ebeln ebelp.
        ENDIF.

        LOOP AT lt_po_lineitems INTO DATA(lw_po_lineitems).

********************read the ref material doc no & year***************
          IF gs_input-comp_code EQ '1000'.
            READ TABLE lt_po INTO DATA(ls_po) WITH KEY ebeln = lw_po_lineitems-po_number
                                                       ebelp = lw_po_lineitems-po_item BINARY SEARCH.
          ENDIF.

          APPEND VALUE #( invoice_doc_item = lv_item
                          po_number        = lw_po_lineitems-po_number
                          po_item          = lw_po_lineitems-po_item
                          tax_code         = lw_po_lineitems-tax_code
                          quantity         = lw_po_lineitems-quantity
                          item_amount      = lw_po_lineitems-net_value
                          po_unit          = lw_po_lineitems-unit
                          po_pr_uom        = lw_po_lineitems-orderpr_un
                          ref_doc          = ls_po-mblnr
                          ref_doc_year     = ls_po-mjahr
                          ref_doc_it       = ls_po-zeile
                          ) TO lt_item.

          lv_item = lv_item + 1.
          CLEAR : lw_po_lineitems,ls_po.
        ENDLOOP.
*--- Tax calculation based on tax code ---*
        DATA:lv_titem TYPE taxps.
        CLEAR lv_titem.
        lv_titem = '000001'.
        LOOP AT lt_item INTO lw_item.
          CLEAR: e_fwnav,e_fwnvv,e_fwste,e_fwast,lt_caltax.
          DATA(lv_amount) = CONV wrbtr( lw_item-item_amount ).
          CALL FUNCTION 'CALCULATE_TAX_FROM_NET_AMOUNT'
            EXPORTING
              i_bukrs           = comp_code
              i_mwskz           = lw_item-tax_code
              i_waers           = 'INR'
              i_wrbtr           = lv_amount
            IMPORTING
              e_fwnav           = e_fwnav
              e_fwnvv           = e_fwnvv
              e_fwste           = e_fwste
              e_fwast           = e_fwast
            TABLES
              t_mwdat           = lt_caltax
            EXCEPTIONS
              bukrs_not_found   = 1
              country_not_found = 2
              mwskz_not_defined = 3
              mwskz_not_valid   = 4
              ktosl_not_found   = 5
              kalsm_not_found   = 6
              parameter_error   = 7
              knumh_not_found   = 8
              kschl_not_found   = 9
              unknown_error     = 10
              account_not_found = 11
              txjcd_not_valid   = 12
              tdt_error         = 13
              txa_error         = 14
              OTHERS            = 15.
          IF sy-subrc = 0.
            DATA(lv_sgst) = VALUE #( lt_caltax[ kschl = 'JISG' ]-wmwst OPTIONAL ).
            DATA(lv_cgst) = VALUE #( lt_caltax[ kschl = 'JICG' ]-wmwst OPTIONAL ).
            DATA(lv_igst) = VALUE #( lt_caltax[ kschl = 'JIIG' ]-wmwst OPTIONAL ).
          ENDIF.
          lv_gross_amt = lv_gross_amt + lw_item-item_amount + lv_cgst + lv_sgst + lv_igst.
          CLEAR : lw_item,lv_sgst,lv_cgst,lv_igst,lw_item,lv_amount.
        ENDLOOP.

*---- Invoice Header Data -------*
        CLEAR lw_header.
*******************invoice indicator only for normal PO************
        IF gs_input-po_type = 'PO'.
          lw_header-invoice_ind    = 'X'.
        ENDIF.
        lw_header-doc_date       = sy-datum. "Enter the document date
        lw_header-pstng_date     = sy-datum. "Enter the posting date
        lw_header-ref_doc_no     = gs_input-invoice_no.
        lw_header-ref_doc_no_long     = gs_input-mis_id.
        lw_header-comp_code      = comp_code.
        lw_header-gross_amount   = lv_gross_amt.  "Enter the gross amount(aft. tax) for the invoice
        lw_header-calc_tax_ind   = 'X'.
        lw_header-business_place = 'TN01'.
        lw_header-currency       = 'INR'.
*        lw_header-currency_iso      = 'INR'.
*--- Function Module to Create Incoming Invoice -----*
        CLEAR: l_fyear,l_invno,lt_return.
        CALL FUNCTION 'BAPI_INCOMINGINVOICE_CREATE'
          EXPORTING
            headerdata       = lw_header
          IMPORTING
            invoicedocnumber = l_invno
            fiscalyear       = l_fyear
          TABLES
            itemdata         = lt_item
            return           = lt_return.

        IF lt_return IS INITIAL AND l_invno IS NOT INITIAL AND l_fyear IS NOT INITIAL.
          CLEAR l_return.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait   = 'X'
            IMPORTING
              return = l_return.

          docnumber  = l_invno.
          fiscalyear = l_fyear.
          message    = |Purchase Order to MIGO Completed|.
          type       = 'S'.

        ELSE.

          LOOP AT lt_return INTO DATA(lw_return) WHERE type = 'E' OR type = 'A'.
            lv_msg = | { lw_return-message } , { lv_msg } |.
          ENDLOOP.
          message = lv_msg.
          type    = 'E'.
        ENDIF.

****************uodate the log table*****************
        IF gs_input-mis_id IS NOT INITIAL.
          UPDATE zalpn_po_header SET  message    = message     type  = type
                                      belnr      = docnumber   gjahr = fiscalyear
                                      status     = '16'
                                WHERE mis_ordid = gs_input-mis_id.
        ENDIF.
      ELSE.
        type    = 'E'.
        message = 'purchase order number incorrect'.
      ENDIF.

    ELSE.
      type    = 'E'.
      message = 'purchase order number missing'.
    ENDIF.

  ENDMETHOD.


  METHOD validations.

**********local variable data dec***********
    DATA : lv_purord  TYPE ebeln,
           lv_mblnr   TYPE mblnr,
           lv_mjahr   TYPE mjahr,
*           lv_belnr   TYPE belnr_d,
*           lv_fiscal TYPE gjahr,
           lv_posnr   TYPE posnr,
           lv_matnr   TYPE matnr,
           lv_charg   TYPE charg_d,
           lv_check   TYPE char1,
           lv_strg    TYPE lgort_d,
           lv_movtype TYPE bwart.

    DATA : lv_irn        TYPE  j_1ig_irn,
           lv_signed_inv TYPE  j_1ig_sign_inv.

    CLEAR : lv_purord,
            lv_mblnr,
            lv_posnr,
            lv_matnr,
            lv_charg,
            lv_check.

    IF gs_input-mis_id IS INITIAL.
      message = | Please fill dms orderid|.
      EXIT.
    ENDIF.
*Multiple click on front end so avoiding duplicates
    SELECT SINGLE * FROM zsd_invoice_dms
      INTO @DATA(l_dupl_ordid)
      WHERE invoice_no = @gs_input-mis_id.
    IF sy-subrc = 0.
      message = | Already Purchase order In Processing { gs_input-mis_id }|.
      EXIT.
    ELSE.
      DATA(l_orderid_chk) = VALUE zsd_invoice_dms( mandt      = sy-mandt
                                                   invoice_no = gs_input-mis_id ).
      INSERT zsd_invoice_dms FROM l_orderid_chk.
    ENDIF.
*************check the duplicate purchase return*********
    SELECT SINGLE * FROM zalpn_po_header INTO @DATA(ls_header) WHERE mis_ordid = @gs_input-mis_id.
    IF sy-subrc = 0 AND ls_header-status = 16 AND ls_header-type = 'S'.
      message       = |Already PO order Completed|.
      type          = 'S'.
      po_docno      = ls_header-belnr.
      fisc_yr       = ls_header-gjahr.
      po_number     = ls_header-ebeln.
      signed_qrcode = ls_header-signed_qrcode.
      irn           = ls_header-irn.
      pdf           = ls_header-pdf.
      ack_date      = ls_header-ack_date.
      ack_no        = ls_header-ack_no.
      EXIT.
    ELSE.
      lv_purord = ls_header-ebeln.
      lv_mblnr  = ls_header-mblnr.
      po_docno  = ls_header-belnr.
      lv_irn    = ls_header-irn.
    ENDIF.

    IF lv_purord IS INITIAL.

****************check PO type*************
      IF gs_input-po_type = 'PO' OR gs_input-po_type = 'PR'.

      ELSE.
        message = |Incorrect PO Type - { gs_input-po_type }|.
        EXIT.
      ENDIF.

      IF gs_input-comp_code <> '1000'.
***********************cust_code check - not for 1000 company code***************
        SELECT SINGLE kunnr,werks FROM kna1 INTO @DATA(ls_kna1)
                                            WHERE kunnr = @gs_input-cust_code.
        IF sy-subrc NE 0.
          message = | customer code is not found - { gs_input-cust_code }|.
          EXIT.
        ELSE.
          IF ls_kna1-werks IS INITIAL.
            message = | customer code Plant not Maintained - { gs_input-cust_code }|.
            EXIT.
          ELSEIF ls_kna1-werks <> gs_input-plant.
            message = | customer code Plant & input plant mismatch - { gs_input-cust_code }|.
            EXIT.
          ENDIF.
        ENDIF.
      ENDIF.
***************check the plant code***************
      SELECT SINGLE werks FROM t001w INTO @DATA(ls_sheen) WHERE werks = @gs_input-plant.
      IF sy-subrc NE 0.
        message = | Incorrect Plant code - { gs_input-plant }|.
        EXIT.
      ENDIF.

***********************cust_code check***************
      SELECT SINGLE lifnr FROM lfa1 INTO @DATA(ls_lfa1) WHERE lifnr = @gs_input-vendor.
      IF sy-subrc NE 0.
        message = | Vendor code is not found - { gs_input-vendor }|.
        EXIT.
      ENDIF.

*****************************Block the process for cust_code or customer*****************
      DATA : lobj_check TYPE REF TO zcl_common_check.
      CREATE OBJECT lobj_check.
********************for cust_code checking****************
      lobj_check->dms_process_stop(
        EXPORTING
          kunnr        = gs_input-cust_code
          process_type = 'PO'
        IMPORTING
          message      = message
          type         = type ).
      IF type = 'E'.
        EXIT.
      ENDIF.

      SORT : gs_input-item[] BY strg_loc material batch.

      LOOP AT gs_input-item[] ASSIGNING FIELD-SYMBOL(<fs_details>).

******************Material check*************
        IF <fs_details>-material IS INITIAL.
          message = |Material is missing , { message } |.
          CONTINUE.
        ELSE.
          CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
            EXPORTING
              input  = <fs_details>-material
            IMPORTING
              output = <fs_details>-material.
        ENDIF.

******************quantity check*************
        IF <fs_details>-quantity IS INITIAL.
          message = |Quantity is missing , { message } |.
          CONTINUE.
        ENDIF.

******************Netprice check*************
        IF <fs_details>-netprice IS INITIAL.
          message = |Netprice is missing , { message } |.
          CONTINUE.
        ENDIF.

******************UOM check*************
        IF <fs_details>-uom IS INITIAL.
          message = |Unit of measurement is missing , { message } |.
          CONTINUE.
        ENDIF.
******************Storage location***************
        IF <fs_details>-strg_loc IS INITIAL.
          message = |Storage Loc. is missing , { message } |.
          CONTINUE.
        ENDIF.

        lv_charg = <fs_details>-batch.
        AT NEW batch.
          CLEAR : lv_charg.
        ENDAT.

        AT NEW material.
          CLEAR : lv_matnr,lv_check.
          lv_matnr = <fs_details>-material.
          lv_check = 'X'.
        ENDAT.

        AT NEW strg_loc.
          CLEAR : lv_strg.
          lv_strg = <fs_details>-strg_loc.
          lv_check = 'X'.
        ENDAT.
************check given return order material & batch repeated**************
        IF lv_check = abap_false.
          IF lv_matnr = <fs_details>-material AND lv_charg EQ <fs_details>-batch.
            message = | material - { <fs_details>-material } & batch - { <fs_details>-batch } repeated more than once , { message } |.
            CONTINUE.
          ENDIF.
        ENDIF.

        CLEAR : lv_check.

      ENDLOOP.

*********fill the datas for bapi**********
      IF message IS NOT INITIAL.
        type = 'E'.
        EXIT.
      ENDIF.
    ENDIF.

*****************create purchase return order************
    IF gs_input IS NOT INITIAL AND lv_purord IS INITIAL.

      CALL METHOD create_purchase_order
        EXPORTING
          lifnr     = gs_input-vendor
          plant     = gs_input-plant
          po_type   = gs_input-po_type
          item_tab  = gs_input-item[]
        IMPORTING
          po_number = lv_purord
          message   = message
          type      = type.

    ENDIF.

*****************create MIGO************
    IF lv_purord IS NOT INITIAL AND lv_mblnr IS INITIAL.

      CLEAR : lv_movtype.
      IF gs_input-po_type = 'PO' OR gs_input-comp_code = '1000'. "Only for comp code 1000 ( PO & PR movement type same )
        lv_movtype = '101'.
      ELSEIF gs_input-po_type = 'PR'.
        lv_movtype = '161'.
      ENDIF.

      CALL METHOD goods_reciept
        EXPORTING
          movement_type  = lv_movtype
          purchase_order = lv_purord
        IMPORTING
          mblnr          = lv_mblnr
          message        = message
          type           = type.

    ENDIF.

    IF lv_mblnr IS NOT INITIAL AND po_docno IS INITIAL.

      CALL METHOD miro_invoice
        EXPORTING
          purchase_order = lv_purord
          comp_code      = gs_input-comp_code
        IMPORTING
          docnumber      = po_docno
          fiscalyear     = fisc_yr
          message        = message
          type           = type.

    ENDIF.
**************e-invoice process**************
*    IF lv_mblnr IS NOT INITIAL AND lv_irn IS INITIAL.
**************check e-invoice process*************
*      SELECT SINGLE * FROM zdms_po_return INTO @DATA(ls_header2) WHERE dms_ordid = @gs_input-mis_id.
*      SELECT SINGLE cust_code FROM zdist_einv_dtls INTO @DATA(ls_check)
*                                                     WHERE cust_code = @gs_input-cust_code
*                                                     AND   act_date    LE @ls_header2-inv_date.
*      IF sy-subrc = 0.
*        DATA : lo_main1 TYPE REF TO zcl_dms_einvoice_process.
*        CREATE OBJECT lo_main1.
*
*        invoice_no = ls_header2-invoice_no.
************call method for create_irn_qrcode_poreturn*************
*        lo_main1->create_irn_qrcode_poreturn(
*          EXPORTING
*            cust_code_code = gs_input-cust_code " Customer Number
*            seller_plant     = gs_input-she_plant   " Plant
*            po_number        = lv_purord            " Purchasing Document Number
*            invoiceno        = ls_header2-invoice_no " Billing Document
*            invoicedt        = ls_header2-inv_date     " Billing Date
*            seller_invoiceno = gs_input-invoice_no  " Billing Document
*            bukrs            = 'DMS1'               " Company Code
*            gjahr            = ls_header2-mjahr      " Fiscal Year
*          IMPORTING
*            irn              = lv_irn               " Invoice Reference Number
*            signed_qrcode    = signed_qrcode        " Signed QR Code
*            signed_inv       = lv_signed_inv        " Signed QR Code
*            pdf              = pdf
*            ack_no           = ack_no
*            ack_date         = ack_date
*            return           = lv_msg ).
*
************fill the response***********
*        IF lv_irn IS NOT INITIAL.
*          lv_type = 'S'.
*          lv_msg  = 'E-Invoice created successfully'.
*        ELSE.
*          lv_type = 'E'.
*        ENDIF.
*      ENDIF.
***************update the log table only for e-invoice process**************
*      UPDATE zdms_po_return SET   message       = lv_msg  type          = lv_type
*                                  status        = '16'    signed_inv    = lv_signed_inv
*                                  irn           = lv_irn  signed_qrcode = signed_qrcode
*                                  pdf           = pdf     ack_date      = ack_date
*                                  ack_no        = ack_no
*                            WHERE dms_ordid = gs_input-mis_id.
*    ENDIF.

*************export the parameter**********
    po_number     = lv_purord.
*    irn           = lv_irn.

  ENDMETHOD.


  METHOD fill_po_details.
    DATA: lv_date TYPE char10.
    CONSTANTS: c_version TYPE char05 VALUE '1.1',
               c_gst     TYPE char10 VALUE  'GST',
               c_b2b     TYPE char03 VALUE  'B2B',
               c_null    TYPE char07 VALUE  'null',
               c_zero    TYPE char01 VALUE  '0'.

    DATA: v_tcs       TYPE string,
          v_assval    TYPE string,
          v_igst      TYPE string,
          v_cgst      TYPE string,
          v_sgst      TYPE string,
          v_amtval    TYPE string,
          lv_roundtot TYPE i.

    "Get the Seller Gstin--->>
    SELECT SINGLE
*             werks,
           name1,
           adrnr,stcd3 AS gstin FROM lfa1
                      INTO  @DATA(l_dealer_dtls)
*                          FOR ALL ENTRIES IN  @l_tab_item
                      WHERE lifnr EQ @vendor.
    IF l_dealer_dtls IS NOT INITIAL.

      "Get Plant address details for Seller Addr1
      SELECT SINGLE
*               a~werks,
             c~post_code1,
             c~country,
             c~region,
*               b~bezei,
             c~addrnumber,      "Seller Legal Name
             c~name1,           "Despatch from Name\
             c~house_num1,
             c~street,          "Street     &"Despatch from add1
             c~str_suppl3,      "
             c~location,        "Despatch from add1
             c~post_code1 AS postal_code,
             c~region AS regio,
             c~mc_city1
                    FROM adrc AS c WHERE c~addrnumber = @l_dealer_dtls-adrnr
                     INTO @DATA(l_dealer_addr).

*      SELECT SINGLE
*             bukrs,
*             branch,
*             gstin    "Seller GSTIN
*                   FROM j_1bbranc
*                   INTO @DATA(l_buyer_gstin)
**                   FOR ALL ENTRIES IN @lt_tab_t001w
*                   WHERE bukrs = '1000' AND
*                         branch = @l_dealer_dtls-j_1bbranch.

    ENDIF.
*----- Seller Details based on invoice plant  ------*
*        DATA(lv_werks) = CONV werks_d( VALUE #( l_tab_item[ 1 ]-werks OPTIONAL ) ).
*        IF lv_werks IS NOT INITIAL.
    SELECT SINGLE * FROM lfa1 INTO @DATA(l_dist_dtls) WHERE kunnr = @vendor.
    IF sy-subrc = 0.
      DATA(l_buyer_gstin) = l_dist_dtls-stcd3.
      SELECT SINGLE * FROM adrc INTO @DATA(l_dist_addr) WHERE addrnumber = @l_dist_dtls-adrnr.
    ENDIF.
**      ENDIF.
*      ENDIF.
*------- Data Population for Gloabal Structures ------*
    CLEAR: data_tab.
    data_tab-distributor = l_dist_dtls-kunnr. "Distributor Code.
    data_tab-body-version = c_version.
*---- Transaction Details ----*
    DATA(l_trandtls) = VALUE zeinv_trans_st( taxsch = c_gst
                                             suptyp = c_b2b
                                             regrev = 'N' ).
    data_tab-body-trandtls =  l_trandtls.
    DATA(doctyp)    = 'DBN'.

    DATA(lv_inv_date) = invoicedt.
    WRITE lv_inv_date TO lv_date DD/MM/YYYY.
    REPLACE ALL OCCURRENCES OF '.' IN lv_date WITH '/'.
    DATA(docno_tmp) = docno.
    SHIFT docno_tmp LEFT DELETING LEADING '0'.
    DATA(l_docdtls) = VALUE zirn_docdls_st( typ = doctyp
                                            no = docno_tmp
                                            dt = lv_date ).
    data_tab-docdtls = l_docdtls.
*----------------------------------------------------------------------------------------------------------
*----- Seller Details which Means Distributor ----*
    SELECT SINGLE taxnum FROM dfkkbptaxnum INTO @DATA(l_dist_gstin) WHERE partner = @l_dist_dtls-kunnr.
    IF sy-subrc = 0.
      IF sy-sysid NE 'PRD'.
        data_tab-sellerdtls-gstin = '29AABCR7796N000'.
      ELSE.
        CONDENSE l_dist_gstin.
        data_tab-sellerdtls-gstin = l_dist_gstin. "Seller GSTIN
      ENDIF.
      "Seller Name
      IF l_dist_dtls-name1 IS NOT INITIAL.
        data_tab-sellerdtls-lglnm = l_dist_dtls-name1.
        data_tab-sellerdtls-trdnm = l_dist_dtls-name1.
      ELSE.
        data_tab-sellerdtls-lglnm = c_null.
        data_tab-sellerdtls-trdnm = c_null.
      ENDIF.
      "seller Address Details
      IF l_dist_dtls-stras IS NOT INITIAL.
        data_tab-sellerdtls-addr1 = l_dist_dtls-stras.
        data_tab-sellerdtls-addr2 = l_dist_dtls-stras.
      ELSE.
        IF l_dist_addr-street IS NOT INITIAL.
          data_tab-sellerdtls-addr1 = l_dist_addr-street.
          data_tab-sellerdtls-addr2 = l_dist_addr-street.
        ENDIF.
      ENDIF.
      "Seller Location
      IF l_dist_dtls-ort01 IS NOT INITIAL.
        data_tab-sellerdtls-loc = l_dist_dtls-ort01.
      ELSE.
        IF l_dist_addr-location IS NOT INITIAL.
          data_tab-sellerdtls-loc = l_dist_addr-location.
        ENDIF.
      ENDIF.
      "Seller Postal Code

      IF sy-sysid NE 'PRD'.
        data_tab-sellerdtls-pin = '560037'.
      ELSE.
        IF l_dist_dtls-pstlz IS NOT INITIAL.
          data_tab-sellerdtls-pin = l_dist_dtls-pstlz.
        ELSE.
          IF l_dist_addr-post_code1 IS NOT INITIAL.
            data_tab-sellerdtls-pin = l_dist_addr-post_code1.
          ENDIF.
        ENDIF.
      ENDIF.
      "Seller state Code
      IF l_dist_gstin IS NOT INITIAL.
        data_tab-sellerdtls-stcd = data_tab-sellerdtls-gstin(2).
        SHIFT data_tab-sellerdtls-stcd LEFT DELETING LEADING '0'.
      ENDIF.
      "Seller Phone Number
      IF l_dist_dtls-telf1 IS NOT INITIAL.
        data_tab-sellerdtls-ph = l_dist_dtls-telf1.
      ELSE.
        data_tab-sellerdtls-ph = c_null.
      ENDIF.
      "Seller Email
      SELECT SINGLE smtp_addr FROM adr6 INTO @DATA(l_seller_mail) WHERE addrnumber = @l_dist_addr-addrnumber.
      IF sy-subrc = 0.
        data_tab-sellerdtls-em = l_seller_mail.
      ELSE.
        data_tab-sellerdtls-em = c_null.
      ENDIF.
    ELSE.
      message = | Error: GST No is not maintained for Distributor { l_dist_dtls-kunnr } |.
    ENDIF.
*--------------------------------------------------------------------------------------------------------*
    "Buyer Details
*    SELECT SINGLE taxnum FROM dfkkbptaxnum INTO @DATA(l_dealer_gstin) WHERE partner = @l_dealer_dtls-kunnr.
    IF l_buyer_gstin IS NOT INITIAL.
      IF sy-sysid NE 'PRD'.
        data_tab-buyerdtls-gstin = '29AGHPG8602R1ZR'.
      ELSE.
        CONDENSE l_buyer_gstin.
        data_tab-buyerdtls-gstin = l_buyer_gstin. "Seller GSTIN
      ENDIF.
      "Buyer Name
      IF l_dealer_dtls-name1 IS NOT INITIAL.
        data_tab-buyerdtls-lglnm = l_dealer_dtls-name1. "Buyer Name
        data_tab-buyerdtls-trdnm = l_dealer_dtls-name1. "Buyer Name
      ELSE.
        data_tab-buyerdtls-lglnm = c_null.
        data_tab-buyerdtls-trdnm = c_null.
      ENDIF.
      "Buyer Address Details
*      IF l_dealer_dtls-stras IS NOT INITIAL.
*        data_tab-buyerdtls-addr1 = l_dealer_dtls-stras.
*        data_tab-buyerdtls-addr2 = l_dealer_dtls-stras.
*      ELSE.
      IF l_dealer_addr-street IS NOT INITIAL.
        data_tab-buyerdtls-addr1 = l_dealer_addr-street.
        data_tab-buyerdtls-addr2 = l_dealer_addr-street.
      ENDIF.
*      ENDIF.
      "Buyer Location
*      IF l_dealer_dtls-ort01 IS NOT INITIAL.
*        data_tab-buyerdtls-loc = l_dealer_dtls-ort01.
*      ELSE.
      IF l_dealer_addr-location IS NOT INITIAL.
        data_tab-buyerdtls-loc = l_dealer_addr-location.
      ENDIF.
*      ENDIF.
      "Buyer Postal Code
      IF sy-sysid NE 'PRD'.
        data_tab-buyerdtls-pin = '560037'.
      ELSE.
*        IF l_dealer_dtls-pstlz IS NOT INITIAL.
*          data_tab-buyerdtls-pin = l_dealer_dtls-pstlz.
*        ELSE.
        IF l_dealer_addr-post_code1 IS NOT INITIAL.
          data_tab-buyerdtls-addr1 = l_dealer_addr-post_code1.
        ENDIF.
*        ENDIF.
      ENDIF.
      "Buyer state Code
      IF l_buyer_gstin IS NOT INITIAL.
        data_tab-buyerdtls-stcd = data_tab-buyerdtls-gstin(2).
        data_tab-buyerdtls-pos = data_tab-buyerdtls-gstin(2).
        SHIFT data_tab-buyerdtls-stcd LEFT DELETING LEADING '0'.
        SHIFT data_tab-buyerdtls-pos LEFT DELETING LEADING '0'.
      ENDIF.
    ENDIF.
*--------------------------------------------------------------------------------------------------------------------*
*    DATA(lv_condno) = VALUE #( l_tab_vbrk[ 1 ]-knumv OPTIONAL ).


    "Get the material description->>
    SELECT a~ebeln,
           a~ebelp,
           a~matnr,
           b~steuc, " Matnr HSN Code
           a~txz01 AS maktx,
           a~werks,
           a~meins, "Order Unit
           a~menge, "Ordered Qty
           a~netpr, "Net Price
           a~mwskz  "Tax code
           FROM ekpo AS a INNER JOIN marc AS b ON a~matnr = b~matnr
                                              AND a~werks = b~werks
      INTO TABLE @DATA(l_tab_item)
      WHERE a~ebeln = @po_number.
    IF sy-subrc = 0.
*** GST Variable for Calculation **
      SELECT kschl, kbetr, mwsk1 FROM konp
                                 INTO TABLE @DATA(lt_gst)
                                 FOR ALL ENTRIES IN @l_tab_item
                                 WHERE kappl = 'TX'
                                 AND kschl IN ( 'JIIG','JISG','JICG','JTC1' )
                                 AND mwsk1 EQ @l_tab_item-mwskz.
    ENDIF.
    "PO Items Details
    LOOP AT l_tab_item ASSIGNING FIELD-SYMBOL(<fs_vbrp>).
      "Material HSN code
      DATA(lv_hsncd) = <fs_vbrp>-steuc .
      REPLACE ALL OCCURRENCES OF '.' IN lv_hsncd WITH space.
      CONDENSE lv_hsncd NO-GAPS.
      IF lv_hsncd IS INITIAL.
        lv_hsncd = c_null.
      ENDIF.
*           a~netpr, "Net Price
      "Lineitem Qty
      IF <fs_vbrp>-menge IS INITIAL.
        <fs_vbrp>-menge = c_null.
      ENDIF.
      "UOM Conversion
      IF <fs_vbrp>-meins = 'EA'.         "Each
        DATA(l_meins) = 'OTH'.
      ELSEIF <fs_vbrp>-meins = 'KG'.     "Kilograms
        l_meins = 'KGS'.
      ELSEIF <fs_vbrp>-meins = 'BT'.     "Bottle
        l_meins = 'BTL'.
      ELSEIF <fs_vbrp>-meins = 'L'.      "Litre
        l_meins = 'LTR'.
      ELSEIF <fs_vbrp>-meins = 'M'.      "Metre
        l_meins = 'MTR'.
      ELSEIF <fs_vbrp>-meins = 'PAA'.    "Pairs
        l_meins = 'PRS'.
      ELSE.
        l_meins = <fs_vbrp>-meins.
      ENDIF.

      IF <fs_vbrp>-netpr IS NOT INITIAL.
        DATA(l_uprice) = CONV kbetr_kond( <fs_vbrp>-netpr DIV <fs_vbrp>-menge ).
        DATA(l_amount) = <fs_vbrp>-netpr.
        DATA(l_assamt) = <fs_vbrp>-netpr.
      ELSE.
        l_uprice = c_zero.
        l_amount = c_zero.
        l_assamt = c_zero.
      ENDIF.
**----- Condition data amount Population --------*

**********Integrated GST Calculation ************************************************************
      READ TABLE lt_gst INTO DATA(ls_gst) WITH KEY kschl = 'JIIG'
                                                   mwsk1 = <fs_vbrp>-mwskz BINARY SEARCH.
      IF sy-subrc EQ 0.
        DATA(l_itax) = ( ls_gst-kbetr / 10 ).
        DATA(l_igst) = ( <fs_vbrp>-netpr * l_itax ) / 100.
      ELSE.
        l_itax = c_zero.
        l_igst = c_zero.
      ENDIF.
************** State GST Calculation ***********************************************************
      READ TABLE lt_gst INTO DATA(ls_gst1) WITH KEY kschl = 'JICG'
                                                    mwsk1 = <fs_vbrp>-mwskz BINARY SEARCH.
      IF sy-subrc EQ 0.
        DATA(l_ctax) = ( ls_gst1-kbetr / 10 ).
        DATA(l_cgst)  = ( <fs_vbrp>-netpr * l_ctax ) / 100.
      ELSE.
        l_ctax = c_zero.
        l_cgst = c_zero.
      ENDIF.
***************** Central GST Calculation ********************************************************
      READ TABLE lt_gst INTO DATA(ls_gst2) WITH KEY kschl = 'JISG'
                                           mwsk1 = <fs_vbrp>-mwskz BINARY SEARCH.
      IF sy-subrc EQ 0.
        DATA(l_stax) = ( ls_gst1-kbetr / 10 ).
        DATA(l_sgst)  = ( <fs_vbrp>-netpr * l_stax ) / 100.
      ELSE.
        l_stax = c_zero.
        l_sgst = c_zero.
      ENDIF.


      DATA(l_total_gst) = CONV string( l_itax + l_ctax + l_stax ).
      CONDENSE l_total_gst.
      DATA(l_totalitem_amt) = CONV string( l_assamt + l_igst + l_cgst + l_sgst   ).
      CONDENSE l_totalitem_amt.

      APPEND VALUE #( slno       = <fs_vbrp>-ebelp
                      prddesc    = <fs_vbrp>-maktx
                      isservc    = 'N'
                      hsncd      = lv_hsncd
                      qty        = <fs_vbrp>-menge
                      freeqty    = c_zero
                      unit       = l_meins
                      unitprice  = l_uprice
                      totamt     = l_amount
                      assamt     = l_assamt
                      gstrt      = l_total_gst
                      igstamt    = l_igst
                      cgstamt    = l_cgst
                      sgstamt    = l_sgst
                      othchrg    = c_zero
                      totitemval = l_totalitem_amt
                    ) TO data_tab-itemlist.
      CLEAR: l_cgst,l_sgst,l_igst,l_itax,l_ctax,l_stax,
             l_uprice,l_amount,l_assamt,l_total_gst,l_totalitem_amt.
    ENDLOOP.
*---- Invoice Total Value Population ----*\
    IF data_tab-itemlist IS NOT INITIAL.
      LOOP AT data_tab-itemlist INTO DATA(lw_itemlist).
****assessable Value, Total Value & igst Value
        v_assval = v_assval + lw_itemlist-assamt.
        v_amtval = v_amtval + lw_itemlist-totamt.
        v_tcs    = v_tcs    + lw_itemlist-othchrg.
        v_igst   = v_igst   + lw_itemlist-igstamt.
        v_cgst   = v_cgst   + lw_itemlist-cgstamt.
        v_sgst   = v_sgst   + lw_itemlist-sgstamt.
      ENDLOOP.
      data_tab-valdtls-assval      = v_assval.
      data_tab-valdtls-cgstval     = v_cgst.
      data_tab-valdtls-sgstval     = v_sgst.
      data_tab-valdtls-igstval     = v_igst.
      data_tab-valdtls-cesval      = c_zero.
      data_tab-valdtls-stcesval    = c_zero.
      data_tab-valdtls-stcesval    = c_zero.
      data_tab-valdtls-othchrg     = c_zero.
      data_tab-valdtls-totinvval   = v_amtval + v_cgst + v_sgst + v_igst.
      data_tab-valdtls-totinvvalfc = c_zero.
*************rounding off process*************
      CLEAR : lv_roundtot.
      lv_roundtot                  = data_tab-valdtls-totinvval.
      data_tab-valdtls-rndoffamt   = lv_roundtot - data_tab-valdtls-totinvval.
      data_tab-valdtls-totinvval   = lv_roundtot.
    ENDIF.
*    ENDIF.
  ENDMETHOD.
ENDCLASS.
