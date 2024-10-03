class ZCL_API_PO_RETURN_DMS definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .

  types:
**********item structure************
    BEGIN OF ty_item,
        ref_invno TYPE vbeln,
        material  TYPE matnr,
        qty       TYPE menge_d,
        batch     TYPE charg_d,
        org_batch TYPE charg_d,
        uom       TYPE meins,
      END OF ty_item .
  types:
    tt_item TYPE STANDARD TABLE OF ty_item WITH NON-UNIQUE DEFAULT KEY .
  types:
    BEGIN OF ty_input,
        she_plant   TYPE  werks_d,
        distributor TYPE  kunnr,
        dms_orderid TYPE  zorder_id,
        details     TYPE  tt_item,
      END OF ty_input .
  types:
    gt_bdcdata_ty TYPE STANDARD TABLE OF bdcdata .
  types:
    gt_bdcmsgcoll_ty TYPE STANDARD TABLE OF bdcmsgcoll .

  data GS_INPUT type TY_INPUT .
  data GV_PLANT type WERKS_D .

  methods INFORECORD_PROCESS
    importing
      value(ITEM) type ZSD_TT_RETURN_ITEM_DMS
      !DIST_PLANT type WERKS_D
      !INVOICE_NO type VBELN_VF
      !SHE_PLANT type WERKS_D
    exporting
      !MSG_TYPE type BAPI_MTYPE
      !MSG type STRING .
  methods CREATE_PURCHASE_ORDER
    importing
      !LIFNR type LIFNR
      !KUNNR type KUNNR
      !DOC_TYPE type ESART
      value(ITEM_TAB) type TT_ITEM
      !DIST_PLANT type WERKS_D
      !SHE_PLANT type WERKS_D optional
    exporting
      !PO_NUMBER type EBELN
      !TYPE type BAPI_MTYPE
      !MESSAGE type STRING .
  methods GOODS_RECIEPT
    importing
      !MOVEMENT_TYPE type BWART
      !PURCHASE_ORDER type EBELN
    exporting
      !MBLNR type MBLNR
      !MJAHR type MJAHR
      !TYPE type BAPI_MTYPE
      !MESSAGE type STRING
      !INVOICE_NO type BELNR_D .
  methods MIRO_INVOICE
    importing
      !PURCHASE_ORDER type EBELN
    exporting
      !INVOICEDOCNUMBER type BELNR_D
      !FISCALYEAR type GJAHR
      !TYPE type BAPI_MTYPE
      !MESSAGE type STRING
      !DMS_INVNO type BELNR_D .
  methods VALIDATIONS
    exporting
      !MESSAGE type STRING
      !TYPE type BAPI_MTYPE
      !INVOICE_NO type BELNR_D
      !PO_NUMBER type EBELN
      !IRN type J_1IG_IRN
      !SIGNED_QRCODE type J_1IG_SIGN_QRCODE
      !PDF type FPCONTENT
      !ACK_NO type J_1IG_ACK_NO
      !ACK_DATE type J_1IG_ACK_DATE .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_API_PO_RETURN_DMS IMPLEMENTATION.


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
          lt_logitem     TYPE TABLE OF zdms_po_ret_item.

    DATA: lv_item  TYPE ebelp.

    DATA lv_unit_price TYPE netwr.
    DATA : lv_tax  TYPE mwskz.

*------- lineitem DATA for purchase order preparation-------------------*
    IF item_tab IS NOT INITIAL.

***************fetch the sheenlac & distributor plant region***************
      SELECT SINGLE werks,regio FROM t001w INTO @DATA(ls_sheen) WHERE werks = @she_plant.
      SELECT SINGLE werks,regio FROM t001w INTO @DATA(ls_dist)  WHERE werks = @dist_plant.

*getting price from VBRP instead of VBAP
      SELECT a~knumv,
             a~fkdat,
             a~vbeln,
             b~posnr,
             b~werks,
             b~fkimg AS kwmeng,
             b~netwr,
             b~meins,
             b~vrkme,
             b~matkl,
             b~matnr FROM vbrk AS a
                     INNER JOIN vbrp AS b ON a~vbeln = b~vbeln
                     INTO TABLE @DATA(lt_vbrp)
                     FOR ALL ENTRIES IN @item_tab
                     WHERE a~vbeln EQ @item_tab-ref_invno.
      IF sy-subrc = 0.
        SORT lt_vbrp BY vbeln matnr posnr.
      ENDIF.

      SELECT knumv,kposn,kschl,kbetr,mwsk1 FROM prcd_elements INTO TABLE @DATA(lt_cond)
                                                              FOR ALL ENTRIES IN @lt_vbrp
                                                              WHERE knumv =  @lt_vbrp-knumv
                                                              AND   kschl IN ( 'JOIG' , 'JOCG' , 'JOSG' ).
      IF sy-subrc = 0.
        SORT : lt_cond BY knumv kposn.
      ENDIF.

      REFRESH: lt_poitem,lt_poitemx,lt_account,lt_accountx,lt_poschedule,lt_poschedulex,lt_itemtext.
      CLEAR lv_item.
      lv_item = '00010'.

      LOOP AT item_tab ASSIGNING FIELD-SYMBOL(<fs_item>).

***************get the old material code for validation***********
        SELECT SINGLE matnr,bismt,mtart FROM mara INTO @DATA(ls_old)
                                        WHERE bismt = @<fs_item>-material.

**************Get the material price************
        READ TABLE lt_vbrp INTO DATA(ls_vbrp) WITH KEY vbeln = <fs_item>-ref_invno
                                                       matnr = <fs_item>-material BINARY SEARCH.
        IF sy-subrc = 0.
          CLEAR : lv_unit_price.
          IF ls_vbrp-kwmeng IS NOT INITIAL AND ls_vbrp-netwr IS NOT INITIAL.
            lv_unit_price = ( ls_vbrp-netwr / ls_vbrp-kwmeng ).
          ENDIF.
        ELSE.
          CLEAR : ls_vbrp.
          CLEAR : lv_unit_price.
          READ TABLE lt_vbrp INTO ls_vbrp WITH KEY vbeln = <fs_item>-ref_invno
                                                   matnr = ls_old-matnr BINARY SEARCH.
          IF sy-subrc = 0.
            IF ls_vbrp-kwmeng IS NOT INITIAL AND ls_vbrp-netwr IS NOT INITIAL.
              lv_unit_price = ( ls_vbrp-netwr / ls_vbrp-kwmeng ).
            ENDIF.
          ENDIF.
        ENDIF.

****************check the gst is applicable or not*************
*        SELECT SINGLE * FROM zgst_fiscyr_chk INTO @DATA(ls_gst)
*                                             WHERE fisc_frm LE @ls_vbrp-fkdat
*                                             AND   fisc_to  GE @ls_vbrp-fkdat.
*        IF ls_gst-valid_upto IS INITIAL.
*          message = |GST Period is not maintained - ZGST_FISCYR_CHK|.
*          type    = 'E'.
*          EXIT.
*        ENDIF.
*        IF ls_gst-valid_upto LE sy-datum.
*          lv_tax = 'I0'.
*        ELSE.
**************check the taxcode************
          READ TABLE lt_cond INTO DATA(ls_cond) WITH KEY knumv = ls_vbrp-knumv
                                                         kposn = ls_vbrp-posnr BINARY SEARCH.
          IF sy-subrc = 0.
            IF ls_cond-mwsk1 = 'GD' OR ls_cond-mwsk1 = 'GE'
              OR ls_cond-mwsk1 = 'GA' OR ls_cond-mwsk1 = 'GB'.
              IF ls_sheen-regio = ls_dist-regio.
                lv_tax = 'PD'.          "CGST / SGCT - 18
              ELSE.
                lv_tax = 'PE'.          "IGST
              ENDIF.
            ELSEIF ls_cond-mwsk1 = 'GG' OR ls_cond-mwsk1 = 'GH'.
              IF ls_sheen-regio = ls_dist-regio.
                lv_tax = 'PG'.                       "CGST / SGCT - 12
              ELSE.
                lv_tax = 'PH'.                       "IGST
              ENDIF.
            ENDIF.
          ENDIF.
*        ENDIF.

        IF lv_tax IS INITIAL.
          message = |Tax code is missing { <fs_item>-material } |.
          type    = 'E'.
          EXIT.
        ENDIF.

***********fill item data for log table*******
        APPEND VALUE #( dms_orderid  = gs_input-dms_orderid
                        posnr        = lv_item
                        ref_invno    = ls_vbrp-vbeln
                        billing_date = ls_vbrp-fkdat
                        material     = <fs_item>-material
                        comp_code    = 'DMS1'
                        batch        = <fs_item>-batch
                        org_batch    = <fs_item>-org_batch
                        qty          = <fs_item>-qty
                        uom          = <fs_item>-uom
                        erdat        = sy-datum
                        erzet        = sy-uzeit
                        price        = lv_unit_price
                        taxcode      = lv_tax ) TO  lt_logitem.

*************if original batch number is initial means get the given batch no**********
        IF <fs_item>-org_batch IS INITIAL.
          <fs_item>-org_batch = <fs_item>-batch.
        ENDIF.

********************filling item data***************
        APPEND VALUE #( po_item        = lv_item
                        material       = <fs_item>-material
                        batch          = <fs_item>-batch
                        stge_loc       = 'D1' "Storage Location
                        plant          = dist_plant
                        quantity       = <fs_item>-qty
                        po_unit        = <fs_item>-uom
                        unlimited_dlv  = ''
                        ret_item       = abap_true
                        acknowl_no     = ls_vbrp-vbeln
                        net_price      = lv_unit_price
                        tax_code       = lv_tax
                        info_upd       = abap_false
                        ext_rfx_number = ls_vbrp-vbeln
                        ext_rfx_item   = <fs_item>-org_batch ) TO lt_poitem.

        APPEND VALUE #( po_item        = lv_item
                        po_itemx       = abap_true
                        material       = abap_true
                        plant          = abap_true
                        stge_loc       = abap_true
                        ret_item       = abap_true
                        quantity       = abap_true
                        batch          = abap_true
                        po_unit        = abap_true
                        unlimited_dlv  = abap_true
                        acknowl_no     = abap_true
                        net_price      = abap_true
                        tax_code       = abap_true
                        ext_rfx_number = abap_true
                        ext_rfx_item   = abap_true
                        info_upd       = abap_true ) TO lt_poitemx.

        APPEND VALUE #( po_item  = lv_item
                        quantity = <fs_item>-qty ) TO lt_poschedule.

        APPEND VALUE #( po_item  = lv_item
                        po_itemx = abap_true
                        quantity = abap_true ) TO lt_poschedulex.

        APPEND VALUE #( po_item   = lv_item
                        text_id   = 'F01'
                        text_form = '*'
                        text_line = <fs_item>-org_batch ) TO lt_itemtext.

        lv_item = lv_item + 10.

        CLEAR : lv_tax,ls_vbrp,ls_old,ls_cond.

      ENDLOOP.

    ENDIF.

************if error means exit from method***************
    IF type = 'E'.
      EXIT.
    ENDIF.

*------- Header DATA for purchase order preparation-------------------*
    DATA(l_lifnr) = CONV elifn( |{ lifnr ALPHA = IN }| ).
    SELECT SINGLE * FROM lfb1 INTO @DATA(ls_lfb1) WHERE lifnr = @l_lifnr.
    IF sy-subrc = 0.
      DATA(l_payterms) = ls_lfb1-zterm.
    ENDIF.

    DATA(ls_header) = VALUE bapimepoheader( comp_code  = 'DMS1'
                                            doc_type   = doc_type
                                            creat_date = sy-datum
                                            vendor     = l_lifnr
                                            langu      = sy-langu
                                            pmnttrms   = l_payterms
                                            purch_org  = 'DMS1'
                                            pur_group  = 'DMS'
                                            currency   = 'INR'
                                            doc_date   = sy-datum
                                            sales_pers = gs_input-dms_orderid
                                            ext_ref    = gs_input-dms_orderid
                                            our_ref    = gs_input-she_plant ) .

    DATA(ls_headerx) = VALUE bapimepoheaderx( comp_code  = abap_true
                                              doc_type   = abap_true
                                              creat_date = abap_true
                                              vendor     = abap_true
                                              langu      = abap_true
                                              pmnttrms   = abap_true
                                              purch_org  = abap_true
                                              pur_group  = abap_true
                                              currency   = abap_true
                                              doc_date   = abap_true
                                              sales_pers = abap_true
                                              ext_ref    = abap_true
                                              our_ref    = abap_true ) .
*------- Header Text for Reference ---------*
    REFRESH lt_headertext.
    APPEND VALUE #(  text_id = 'F03'
                     text_form = '*'
                     text_line = gs_input-dms_orderid ) TO lt_headertext.

*** Function module which creates purchase order ---------------*
    CLEAR: po_number.
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
    IF sy-subrc = 0.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
      message   = | Purchase return order created - { po_number } |.
      type      = 'S'.

    ELSE.

      LOOP AT lt_return INTO DATA(lw_return) WHERE type = 'E' OR type = 'A'.
        lv_msg = | { lw_return-message } , { lv_msg } |.
      ENDLOOP.
      message = lv_msg.
      type    = 'E'.
    ENDIF.
    "Updating Log table
    IF gs_input-dms_orderid IS NOT INITIAL.
*************data store to log table header & item*************
      DATA(ls_loghead) = VALUE zdms_po_return( dms_ordid   = gs_input-dms_orderid
                                               distributor = gs_input-distributor
                                               dist_plant  = dist_plant
                                               she_plant   = gs_input-she_plant
                                               status      = '13'
                                               ernam       = sy-uname
                                               erdat       = sy-datum
                                               erzet       = sy-uzeit
                                               message     = message
                                               type        = type
                                               ebeln       = po_number ) .
      MODIFY zdms_po_return FROM ls_loghead.
      MODIFY zdms_po_ret_item FROM TABLE lt_logitem.
      IF sy-subrc = 0.
        COMMIT WORK.
      ELSE.
        ROLLBACK WORK.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD goods_reciept.
    "Created by: Pandiarajan
    "Created on: 13.02.2024
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
          lv_inv          TYPE belnr_d,
          lv_date         TYPE erdat.
    DATA: lv_item TYPE i.

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

    DELETE lt_item_text WHERE text_line = space.
*------- Material Document Which Means Goods Receipt -----------*
*-- Header DATA for grn PROCESS ----*
    CLEAR: lw_goodsmvt_header.
    goodsmvt_code_tmp                 = '01'.
    lw_goodsmvt_header-pstng_date     = sy-datum.
    lw_goodsmvt_header-doc_date       = sy-datum.
    lw_goodsmvt_header-pr_uname       = sy-uname.
    lw_goodsmvt_header-header_txt     = VALUE #( lt_po_htxt[ 1 ]-text_line OPTIONAL ).
    lw_goodsmvt_header-bill_of_lading = po_header-po_number.
    lw_goodsmvt_header-ref_doc_no     = gs_input-dms_orderid.
*---- Material Document Item Data -----*
    REFRESH lt_goodsmvt_item.
    LOOP AT lt_po_lineitems INTO DATA(lw_po_lineitems).

      APPEND VALUE #( move_type            = movement_type "Movement Type
                      mvt_ind              = 'B'
                      plant                = lw_po_lineitems-plant "Plant
                      material             = lw_po_lineitems-material "Material
                      entry_qnt            = lw_po_lineitems-quantity "Batch Number "PO Quantity OR GRN Quantity
                      po_number            = lw_po_lineitems-po_number "PO Number
                      po_item              = lw_po_lineitems-po_item ) TO lt_goodsmvt_item.

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

    IF lw_goodsmvt_headret-mat_doc IS NOT INITIAL AND lw_goodsmvt_headret-doc_year IS NOT INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait   = 'X'
        IMPORTING
          return = l_return.

      mblnr   = lw_goodsmvt_headret-mat_doc. "Material Document Number Or GRN Number
      mjahr   = lw_goodsmvt_headret-doc_year. "Material Document Year
      message = | Purchase return order delivery created - { mblnr } - { mjahr } |.
      type    = 'S'.
      lv_date = sy-datum.

*************Invoice number range generation*************

*get the distributor wise number range**************
      SELECT SINGLE * FROM zdms_po_re_seris INTO @DATA(ls_series) WHERE distributor = @gs_input-distributor.
      IF sy-subrc = 0.
        IF ls_series-num_range EQ '999999'.
          message = | { message } , Distributor Number range going to exceed |.
        ENDIF.
      ELSE.
        ls_series-distributor = gs_input-distributor.
        ls_series-plant       = gv_plant.
        ls_series-num_range   = '000001'.
        ls_series-mandt       = sy-mandt.
      ENDIF.
      lv_inv     = |{ ls_series-plant }{ ls_series-num_range }|.
      invoice_no = lv_inv.
************update the po return log table**************
      ls_series-num_range = ls_series-num_range + 1.
      MODIFY zdms_po_re_seris FROM ls_series.
      CLEAR : ls_series,lv_inv.

    ELSE.

      LOOP AT lt_return INTO DATA(lw_return) WHERE type = 'E' OR type = 'A'.
        lv_msg = | { lw_return-message } , { lv_msg } |.
      ENDLOOP.
      message = lv_msg.
      type    = 'E'.
    ENDIF.
    IF gs_input-dms_orderid IS NOT INITIAL.
      UPDATE zdms_po_return SET   message    = message     type  = type
                                  invoice_no = invoice_no  mjahr = mjahr
                                  status     = '14'        mblnr = mblnr
                                  inv_date   = lv_date
                            WHERE dms_ordid = gs_input-dms_orderid.
    ENDIF.
  ENDMETHOD.


  METHOD if_http_extension~handle_request.
    "Created by: Pandiarajan
    "Created on: 13.02.2024
    "Reference by: Ramakrishnan J
    "Purpose : Create Purchase Return order for DMS Process
*-------------------------------------------------------------*

    TYPES: BEGIN OF ty_msg,
             she_plant   TYPE werks_d,
             dms_orderid TYPE zorder_id,
             distributor TYPE kunnr,
             po_number   TYPE belnr_d,
             po_invno    TYPE belnr_d,
             irn         TYPE j_1ig_irn,
             qr_code     TYPE j_1ig_sign_qrcode,
             pdf         TYPE fpcontent,
             ack_no      TYPE j_1ig_ack_no,
             ack_date    TYPE j_1ig_ack_date,
             type        TYPE bapi_mtype,
             message     TYPE string,
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
***************input alpha conversion************
    gs_input-distributor = |{ gs_input-distributor ALPHA = IN }|.
****************contains the all validations process****************
    CLEAR : ls_response.
    CALL METHOD validations
      IMPORTING
        message       = ls_response-message
        invoice_no    = ls_response-po_invno
        po_number     = ls_response-po_number
        type          = ls_response-type
        irn           = ls_response-irn
        pdf           = ls_response-pdf
        signed_qrcode = ls_response-qr_code
        ack_no        = ls_response-ack_no
        ack_date      = ls_response-ack_date.
***************fill the response********
    IF ls_response-type IS INITIAL.
      ls_response-type = 'E'.
    ENDIF.
    ls_response-she_plant    = gs_input-she_plant.
    ls_response-dms_orderid  = gs_input-dms_orderid.
    ls_response-distributor  = gs_input-distributor.
*****************generate PDF**************
    IF ls_response-po_invno IS NOT INITIAL AND ls_response-type = 'S'
      AND ls_response-pdf IS INITIAL.

      SELECT SINGLE * FROM zdms_po_return INTO @DATA(ls_header) WHERE dms_ordid = @gs_input-dms_orderid.
* api call
      SELECT SINGLE * FROM  zdms_invoice_irn INTO @DATA(lw_irn)
                                             WHERE docno    = @ls_response-po_invno.
      CALL METHOD lo_main->fill_poreturn_details
        EXPORTING
          distributor_code = gs_input-distributor
          seller_plant     = gs_input-she_plant
          po_number        = ls_response-po_number
          invoiceno        = ls_response-po_invno
          invoicedt        = ls_header-inv_date
          seller_invoiceno = ls_response-po_invno
          bukrs            = 'DMS1'
          gjahr            = ls_header-mjahr
        IMPORTING
          doctyp           = DATA(doctyp)
          data_tab         = DATA(lw_invoice_tab).
*        message          = return.
      CALL METHOD lo_main->generate_pdf
        EXPORTING
          inv_details      = lw_invoice_tab
          inv_no           = ls_response-po_invno
          inv_date         = ls_header-inv_date
          irn              = lw_irn-irn
          signed_inv       = lw_irn-signed_inv
          signed_qrcode    = lw_irn-signed_qrcode
          po_number        = ls_response-po_number
          seller_invoiceno = ls_response-po_invno
        IMPORTING
          pdf              = ls_response-pdf.
*        return           = return.
    ENDIF.

*********************error log***********
    IF ls_response-type = 'E'.
      lo_errorlog_dms->log_entry_store(
        EXPORTING
          type                = 24
          status              = 10
          dms_orderid         = ls_response-dms_orderid
          distributor         = ls_response-distributor
          plant               = gv_plant
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
        apiname         = 'DMS_PO_RETURN'
        ijson           = lv_data
        ojson           = lv_body
        distributor     = gs_input-distributor
      EXCEPTIONS
        apiname_missing = 1
        json_missing    = 2
        OTHERS          = 3.
*Set JSON Content-Type
    CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
    CALL METHOD server->response->set_cdata( data = lv_body ).
    DELETE FROM zsd_invoice_dms WHERE invoice_no = gs_input-dms_orderid.

  ENDMETHOD.


  METHOD inforecord_process.
    "Created by: Pandiarajan
    "Created on: 13.02.2024
    "Reference by: Ramakrishnan J
    "Purpose: Info record Creation and Updation for DMS Purchasing
*----------------------------------------------------------------------*
    DATA lv_unit_price TYPE netwr.
    DATA lv_price TYPE char20.
    CONSTANTS : lv_lifnr TYPE lifnr VALUE '0010002452'.
    DATA : lv_tax  TYPE mwskz,
           lv_msg  TYPE string,
           lv_type TYPE bapi_mtype.
**********fetch the invoice details***********

    SELECT SINGLE vbeln,fkdat, knumv FROM vbrk INTO @DATA(ls_vbrk) WHERE vbeln = @invoice_no.



    SELECT SINGLE vbeln,aubel,erdat FROM vbrp INTO @DATA(ls_vbrp) WHERE vbeln = @invoice_no.
*    SELECT vbeln,
*           posnr,
*           werks,
*           kwmeng,
*           netwr,
*           meins,
*           vrkme,
*           matkl,
*           matnr,
*           knumv_ana FROM vbap INTO TABLE @DATA(lt_vbap)
*                               WHERE vbeln EQ @ls_vbrp-aubel.
*getting price from VBRP instead of VBAP
    SELECT vbeln,
           posnr,
           werks,
           fkimg AS kwmeng,
           netwr,
           meins,
           vrkme,
           matkl,
           matnr,
           knumv_ana FROM vbrp INTO TABLE @DATA(lt_vbap)
                               WHERE vbeln EQ @invoice_no.

    IF sy-subrc = 0.
      SORT lt_vbap BY matnr posnr.
    ENDIF.

*----- Inforecord Details based on Invoice data --*

    SELECT a~infnr,a~matnr,a~lifnr,b~ekorg,b~werks FROM eina AS a INNER JOIN eine AS b ON a~infnr = b~infnr
                                                   INTO TABLE @DATA(lt_inforecord)
*                                                   FOR ALL ENTRIES IN @item
                                                   WHERE "a~matnr = @item-material
                                                         a~lifnr = @lv_lifnr
                                                   AND   b~ekorg = 'DMS1'
                                                   AND   b~werks = @dist_plant.
    IF sy-subrc = 0.
      SORT lt_inforecord[] BY matnr werks.
    ENDIF.
***************fetch the sheenlac & distributor plant region***************
    SELECT SINGLE werks,regio FROM t001w INTO @DATA(ls_sheen) WHERE werks = @she_plant.
    SELECT SINGLE werks,regio FROM t001w INTO @DATA(ls_dist)  WHERE werks = @dist_plant.
******************fetch the pricing elements************
*    SELECT knumv,kposn,kschl,kbetr,mwsk1 FROM prcd_elements INTO TABLE @DATA(lt_cond)
*                                                            FOR ALL ENTRIES IN @lt_vbap
*                                                            WHERE knumv =  @lt_vbap-knumv_ana
*                                                            AND   kschl IN ( 'JOIG' , 'JOCG' , 'JOSG' ).
    SELECT knumv,kposn,kschl,kbetr,mwsk1 FROM prcd_elements INTO TABLE @DATA(lt_cond)
*                                                            FOR ALL ENTRIES IN @lt_vbap
                                                            WHERE knumv =  @ls_vbrk-knumv
                                                            AND   kschl IN ( 'JOIG' , 'JOCG' , 'JOSG' ).
    IF sy-subrc = 0.
      SORT : lt_cond BY knumv kposn.
    ENDIF.
*----- Inforecord Creation and Updation Process -----*
    SORT : item BY material.
    LOOP AT item ASSIGNING FIELD-SYMBOL(<fs_item>).

***************get the old material code for validation***********
      SELECT SINGLE matnr,bismt,mtart FROM mara INTO @DATA(ls_old)
                                      WHERE bismt = @<fs_item>-material.

      AT NEW material.
**************Get the material price************
        READ TABLE lt_vbap INTO DATA(ls_vbap) WITH KEY matnr = <fs_item>-material BINARY SEARCH.
        IF sy-subrc = 0.
          CLEAR : lv_unit_price,lv_price.
          IF ls_vbap-kwmeng IS NOT INITIAL AND ls_vbap-netwr IS NOT INITIAL.
            lv_unit_price = ( ls_vbap-netwr / ls_vbap-kwmeng ).
            WRITE lv_unit_price TO lv_price.
            CONDENSE lv_price.
          ENDIF.
        ELSE.
          CLEAR : ls_vbap.
          CLEAR : lv_unit_price,lv_price.
          READ TABLE lt_vbap INTO ls_vbap WITH KEY matnr = ls_old-matnr BINARY SEARCH.
          IF sy-subrc = 0.
            IF ls_vbap-kwmeng IS NOT INITIAL AND ls_vbap-netwr IS NOT INITIAL.
              lv_unit_price = ( ls_vbap-netwr / ls_vbap-kwmeng ).
              WRITE lv_unit_price TO lv_price.
              CONDENSE lv_price.
            ENDIF.
          ENDIF.
        ENDIF.
**************check the taxcode************
        READ TABLE lt_cond INTO DATA(ls_cond) WITH KEY knumv = ls_vbrk-knumv
                                              kposn = ls_vbap-posnr BINARY SEARCH.
        IF sy-subrc = 0.
          IF ls_cond-mwsk1 = 'GD' OR ls_cond-mwsk1 = 'GE' OR ls_cond-mwsk1 = 'GA'.
            IF ls_sheen-regio = ls_dist-regio.
              lv_tax = 'PD'.          "CGST / SGCT - 18
            ELSE.
              lv_tax = 'PE'.          "IGST
            ENDIF.
          ELSEIF ls_cond-mwsk1 = 'GG' OR ls_cond-mwsk1 = 'GH'.
            IF ls_sheen-regio = ls_dist-regio.
              lv_tax = 'PG'.                       "CGST / SGCT - 12
            ELSE.
              lv_tax = 'PH'.                       "IGST
            ENDIF.
          ENDIF.
        ENDIF.

        IF lv_tax IS INITIAL.
          msg      = | Tax code is missing { <fs_item>-material } |.
          msg_type = 'E'.
          EXIT.
        ENDIF.
**************Inforecord creation process***********
        DATA(l_data) = VALUE zdms_inforecord_st( matnr      = <fs_item>-material
                                                 lifnr      = lv_lifnr
                                                 werks      = dist_plant
                                                 vrkme      = ls_vbap-vrkme
                                                 mwskz      = lv_tax
                                                 telf1      = '04443949900'
                                                 unit_price = lv_price
                                                 valid_from = sy-datum
                                                 ekorg      = 'DMS1'
                                                 ekgrp      = 'DMS' ).
        "Info Record Existence Checks
        DATA(ls_inforecord) = VALUE #( lt_inforecord[ matnr = <fs_item>-material
                                                      werks = dist_plant ] OPTIONAL ).
        IF ls_inforecord IS INITIAL.
******************Inforecord Creation
          CALL FUNCTION 'ZDMS_INFORECORD_CREATION2'
            EXPORTING
              im_input = l_data
            IMPORTING
              msg_type = lv_type
              msg      = lv_msg.
        ELSE.
******************Inforecord Updation
          CALL FUNCTION 'ZDMS_INFORECORD_UPDATION2'
            EXPORTING
              im_input = l_data
            IMPORTING
              msg_type = lv_type
              msg      = lv_msg.
        ENDIF.
        msg      = | { msg } , { lv_msg } |.
        msg_type = lv_type.
**************Updating Item Log table
        IF gs_input-dms_orderid IS NOT INITIAL.
          UPDATE zdms_po_ret_item SET taxcode = lv_tax price = lv_unit_price
                                  WHERE dms_orderid = gs_input-dms_orderid
                                  AND   material    = <fs_item>-material.
        ENDIF.
        CLEAR : ls_inforecord,lv_unit_price,l_data,lv_tax.
      ENDAT.
      IF msg_type = 'E'.
        EXIT.
      ENDIF.
      CLEAR : ls_old.
    ENDLOOP.

    IF gs_input-dms_orderid IS NOT INITIAL.
      "Updating Log table
      UPDATE zdms_po_return SET   message   = msg type = msg_type status  = '12'
                            WHERE dms_ordid = gs_input-dms_orderid.
    ENDIF.

  ENDMETHOD.


  METHOD miro_invoice.
    "Created by: Pandiarajan
    "Created on: 13.02.2024
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

      IF lt_po_lineitems IS NOT INITIAL.
        REFRESH: lt_item.
        CLEAR: lv_gross_amt,lv_item.
        lv_item = '000001'.

        LOOP AT lt_po_lineitems INTO DATA(lw_po_lineitems).
          APPEND VALUE #( invoice_doc_item = lv_item
                          po_number        = lw_po_lineitems-po_number
                          po_item          = lw_po_lineitems-po_item
                          tax_code         = lw_po_lineitems-tax_code
                          quantity         = lw_po_lineitems-quantity
                          item_amount      = lw_po_lineitems-net_value
                          po_unit          = lw_po_lineitems-unit ) TO lt_item.
          lv_item = lv_item + 1.
          CLEAR lw_po_lineitems.
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
              i_bukrs           = 'DMS1'
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
*        lw_header-invoice_ind    = 'X'.
        lw_header-doc_date       = sy-datum. "Enter the document date
        lw_header-pstng_date     = sy-datum. "Enter the posting date
        lw_header-ref_doc_no     = purchase_order.
        lw_header-comp_code      = 'DMS1'.
        lw_header-gross_amount   = lv_gross_amt.  "Enter the gross amount(aft. tax) for the invoice
        lw_header-calc_tax_ind   = 'X'.
        lw_header-business_place = 'TN01'.
        lw_header-currency       = 'INR'.
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

        IF lt_return IS INITIAL.
          CLEAR l_return.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait   = 'X'
            IMPORTING
              return = l_return.

          invoicedocnumber = l_invno.
          fiscalyear       = l_fyear.
          message = |Purchase Return Invoice Created|.
          type    = 'S'.

**------- Condition Records for checking TCS Existence -----*
*          SELECT a~vbeln,
*                 a~knumv,
*                 b~kschl,
*                 b~kbetr INTO TABLE @DATA(lt_cond)
*                         FROM vbrk AS a INNER JOIN prcd_elements AS b
*                         ON a~knumv = b~knumv
*                         WHERE a~vbeln = @gs_input-invoice_no
*                         AND   b~kschl = 'JTC1'.
*          IF sy-subrc = 0.
*
*          ENDIF.

        ELSE.

          LOOP AT lt_return INTO DATA(lw_return) WHERE type = 'E' OR type = 'A'.
            lv_msg = | { lw_return-message } , { lv_msg } |.
          ENDLOOP.
          message = lv_msg.
          type    = 'E'.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD validations.

**********local variable data dec***********
    DATA : lv_msg     TYPE string,
           lv_type    TYPE bapi_mtype,
           lv_purord  TYPE ebeln,
           lv_matnr   TYPE matnr,
           lv_mblnr   TYPE mblnr,
           lv_mjahr   TYPE mjahr,
           lv_belnr   TYPE belnr_d,
           lv_fiscal  TYPE gjahr,
           lv_posnr   TYPE posnr,
           lv_charg   TYPE charg_d,
           lv_check   TYPE char1,
           lv_qty     TYPE fkimg,       "return qty
           lv_inv_qty TYPE fkimg.       "invoice qty

    DATA : lv_irn        TYPE  j_1ig_irn,
           lv_signed_inv TYPE  j_1ig_sign_inv,
           so_labnr      TYPE RANGE OF labnr,
           ls_labnr      LIKE LINE OF so_labnr.

    CONSTANTS : lv_lifnr TYPE lifnr VALUE '10002452'.
    CLEAR : lv_msg,
            lv_type,
            lv_purord,
            lv_mblnr,
            lv_belnr,
            lv_posnr,
            lv_matnr,
            lv_charg,
            lv_check,
            lv_qty,
            lv_inv_qty,
            gv_plant.
    IF gs_input-dms_orderid IS INITIAL.
      message = | Please fill dms orderid|.
      EXIT.
    ENDIF.
*Multiple click on front end so avoiding duplicates
    SELECT SINGLE * FROM zsd_invoice_dms
      INTO @DATA(l_dupl_ordid)
      WHERE invoice_no = @gs_input-dms_orderid.
    IF sy-subrc = 0.
      message = | Already Purchase return order In Processing { gs_input-dms_orderid }; { lv_msg }|.
      EXIT.
    ELSE.
      DATA(l_orderid_chk) = VALUE zsd_invoice_dms( mandt      = sy-mandt
                                                   invoice_no = gs_input-dms_orderid ).
      INSERT zsd_invoice_dms FROM l_orderid_chk.
    ENDIF.
*************check the duplicate purchase return*********
    SELECT SINGLE * FROM zdms_po_return INTO @DATA(ls_header) WHERE dms_ordid = @gs_input-dms_orderid.
    IF sy-subrc = 0 AND ls_header-status = 16 AND ls_header-type = 'S'.
      message       = |Already PO Return Completed|.
      type          = 'S'.
      invoice_no    = ls_header-invoice_no.
      po_number     = ls_header-ebeln.
      signed_qrcode = ls_header-signed_qrcode.
      irn           = ls_header-irn.
*      pdf           = ls_header-pdf.
      ack_date      = ls_header-ack_date.
      ack_no        = ls_header-ack_no.
      EXIT.
    ELSE.
      lv_purord = ls_header-ebeln.
      lv_mblnr  = ls_header-mblnr.
      lv_belnr  = ls_header-belnr.
      lv_irn    = ls_header-irn.
      gv_plant  = ls_header-dist_plant.
    ENDIF.

    IF lv_purord IS INITIAL.
***********************distributor check***************
      SELECT SINGLE kunnr,werks FROM kna1 INTO @DATA(ls_kna1) WHERE kunnr = @gs_input-distributor.
      IF sy-subrc NE 0.
        message = | Distributor is not found - { gs_input-distributor }|.
        EXIT.
      ELSE.
        IF ls_kna1-werks IS INITIAL.
          message = | Distributor Plant not Maintained - { gs_input-distributor }|.
          EXIT.
        ELSE.
          gv_plant = ls_kna1-werks.
        ENDIF.
      ENDIF.
***************check the sheenlac plant code***************
      SELECT SINGLE werks FROM t001w INTO @DATA(ls_sheen) WHERE werks = @gs_input-she_plant.
      IF sy-subrc NE 0.
        message = | Incorrect sheenlac Plant code - { gs_input-she_plant }|.
        EXIT.
      ENDIF.

*****************************Block the process for distributor or customer*****************
      DATA : lobj_check TYPE REF TO zcl_common_check.
      CREATE OBJECT lobj_check.
********************for distributor checking****************
      lobj_check->dms_process_stop(
        EXPORTING
          kunnr        = gs_input-distributor
          process_type = 'PR'
        IMPORTING
          message      = message
          type         = type ).
      IF type = 'E'.
        EXIT.
      ENDIF.

**************alpha conversion*************
      LOOP AT gs_input-details[] ASSIGNING FIELD-SYMBOL(<fs_details>).

********************invoice no conversion*************
        TRANSLATE <fs_details>-ref_invno TO UPPER CASE.
        <fs_details>-ref_invno  = |{ <fs_details>-ref_invno ALPHA = IN }|.

************select option range filling***************
        ls_labnr-sign   = 'I'.
        ls_labnr-option = 'EQ'.
        ls_labnr-low    = <fs_details>-ref_invno.
        APPEND ls_labnr TO so_labnr.
        CLEAR : ls_labnr.

**************material conversion***************
        CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
          EXPORTING
            input  = <fs_details>-material
          IMPORTING
            output = <fs_details>-material.

******************material unit price**********

        IF <fs_details>-uom <> 'EA'.

          CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
            EXPORTING
              i_matnr  = <fs_details>-material
              i_in_me  = <fs_details>-uom
              i_out_me = 'EA'
              i_menge  = <fs_details>-qty
            IMPORTING
              e_menge  = <fs_details>-qty.

          <fs_details>-uom = 'EA'.
        ENDIF.

        IF <fs_details>-qty IS INITIAL.
          message = |Material Quantity is Zero - { <fs_details>-material }, { message } |.
          CONTINUE.
        ENDIF.

***********get the new material code*************
        CONDENSE <fs_details>-material.
        IF <fs_details>-material(1) NE 'Z'.

          SELECT SINGLE matnr,bismt,mtart FROM mara INTO @DATA(ls_mara)
                                          WHERE matnr = @<fs_details>-material.
          IF ls_mara-mtart NE 'HAWA'.
            IF ls_mara-bismt IS INITIAL.
              message = |New material is not maintained in old material code - { <fs_details>-material }, { message } |.
              CONTINUE.
            ELSE.
              CONDENSE ls_mara-bismt.
              <fs_details>-material = ls_mara-bismt.
            ENDIF.
            CLEAR : ls_mara.
          ENDIF.
        ENDIF.

      ENDLOOP.

*********if error means exit the method**********
      IF message IS NOT INITIAL.
        type = 'E'.
        EXIT.
      ENDIF.
**********fetch the invoice details***********
      SELECT vbeln,matnr,fkimg,kunag_ana FROM vbrp
                   INTO TABLE @DATA(lt_vbrp)
                   FOR ALL ENTRIES IN @gs_input-details[]
                   WHERE vbeln   EQ @gs_input-details-ref_invno.
***********check already return order details***********
      SELECT matnr,
             menge,
             labnr FROM ekpo
                   INTO TABLE @DATA(lt_returned)
                   WHERE labnr IN @so_labnr
                   AND   elikz = 'X'.
      IF sy-subrc = 0.
        SORT lt_returned BY labnr matnr.
        DATA(lt_returned2) = lt_returned.
***********add the qty based on duplicate invoice wise material************
        DELETE ADJACENT DUPLICATES FROM lt_returned COMPARING labnr matnr.
        IF sy-subrc = 0.

          SORT lt_returned2 BY labnr matnr.

          LOOP AT lt_returned2 INTO DATA(ls_returned).

            AT NEW matnr.
              CONTINUE.
            ENDAT.

            READ TABLE lt_returned ASSIGNING FIELD-SYMBOL(<ls_returned>)
                                   WITH KEY labnr = ls_returned-labnr
                                            matnr = ls_returned-matnr BINARY SEARCH.
            IF sy-subrc = 0.
              <ls_returned>-menge = <ls_returned>-menge + ls_returned-menge.
            ENDIF.

            CLEAR : ls_returned.

          ENDLOOP.
        ENDIF.
      ENDIF.

****************fetch the distributor stock details************
      SELECT matnr,
             werks,
             lgort,
             charg,
             clabs FROM mchb INTO TABLE @DATA(lt_mchb)
                   WHERE werks = @gv_plant.
      IF sy-subrc = 0.
        SORT lt_mchb BY matnr werks lgort charg.
      ENDIF.

      SORT : gs_input-details[] BY ref_invno material batch,
             lt_vbrp BY kunag_ana vbeln matnr.

      LOOP AT gs_input-details[] ASSIGNING <fs_details>.

***************get the old material code for validation***********
        SELECT SINGLE matnr,bismt,mtart FROM mara INTO @DATA(ls_old)
                                        WHERE bismt = @<fs_details>-material.

******************check the given invoice no created or not*************
        AT NEW ref_invno.

          READ TABLE lt_vbrp TRANSPORTING NO FIELDS WITH KEY kunag_ana = gs_input-distributor
                                                             vbeln = <fs_details>-ref_invno BINARY SEARCH.
          IF sy-subrc NE 0.
            message = | Invoice is not created - { <fs_details>-ref_invno } for - { gs_input-distributor } , { message } |.
          ENDIF.

        ENDAT.

        lv_charg = <fs_details>-batch.
        AT NEW batch.
**********Check the stock availablity*********
          READ TABLE lt_mchb INTO DATA(ls_mchb) WITH KEY matnr = <fs_details>-material
                                                         werks = gv_plant
                                                         lgort = 'D1'
                                                         charg = <fs_details>-batch BINARY SEARCH.
          IF ls_mchb-clabs LT <fs_details>-qty.
            message = | Stock not available in material - { <fs_details>-material } batch - { <fs_details>-batch } , { message } |.
          ENDIF.
          CLEAR : lv_charg,ls_mchb.
        ENDAT.

        AT NEW material.
          CLEAR : lv_matnr,lv_check,lv_qty,lv_inv_qty.
          lv_matnr = <fs_details>-material.
          lv_check = 'X'.
***********calculated the already take the sales return for ref. invoice*********
          READ TABLE lt_returned INTO DATA(ls_returned2)
                                 WITH KEY labnr = <fs_details>-ref_invno
                                          matnr = <fs_details>-material BINARY SEARCH.
          IF sy-subrc = 0.
            lv_qty   = ls_returned2-menge.
            CLEAR ls_returned2.
          ELSE.
            CLEAR ls_returned2.
            READ TABLE lt_returned INTO ls_returned2 WITH KEY labnr = <fs_details>-ref_invno
                                                              matnr = ls_old-matnr BINARY SEARCH.
            IF sy-subrc = 0.
              lv_qty   = ls_returned2-menge.
            ENDIF.
          ENDIF.

**********Check the given material to invoice material*********
          READ TABLE lt_vbrp TRANSPORTING NO FIELDS WITH KEY vbeln = <fs_details>-ref_invno
                                                             matnr = <fs_details>-material BINARY SEARCH.
          IF sy-subrc NE 0.
            READ TABLE lt_vbrp TRANSPORTING NO FIELDS WITH KEY vbeln = <fs_details>-ref_invno
                                                               matnr = ls_old-matnr BINARY SEARCH.
            IF sy-subrc NE 0.
              message = | given return material - { <fs_details>-material } is not found in ref. invoice { message } |.
            ENDIF.
          ENDIF.

        ENDAT.

        lv_qty   = lv_qty + <fs_details>-qty.
************check given return order material & batch repeated**************
        IF lv_check = abap_false.
          IF lv_matnr = <fs_details>-material AND lv_charg EQ <fs_details>-batch.
            message = | material - { <fs_details>-material } & batch - { <fs_details>-batch } repeated more than once , { message } |.
          ENDIF.
        ENDIF.
        CLEAR : lv_check.

************check the return material qty & invoice qty***************
        AT END OF material.

********read the actual invoiced quantity***************
          CLEAR : lv_inv_qty.
          READ TABLE lt_vbrp INTO DATA(ls_vbrp) WITH KEY vbeln = <fs_details>-ref_invno
                                                         matnr = <fs_details>-material BINARY SEARCH.
          IF sy-subrc = 0.
            lv_inv_qty = ls_vbrp-fkimg.
          ELSE.
            READ TABLE lt_vbrp INTO ls_vbrp WITH KEY vbeln = <fs_details>-ref_invno
                                                     matnr = ls_old-matnr BINARY SEARCH.
            IF sy-subrc = 0.
              lv_inv_qty = ls_vbrp-fkimg.
            ENDIF.
          ENDIF.
          IF lv_qty GT lv_inv_qty.
            message = | Return material qty - { <fs_details>-material } Exceed the invoice qty - { <fs_details>-ref_invno }, { message } |.
          ENDIF.
          CLEAR : ls_vbrp.
        ENDAT.

        CLEAR : ls_old.

      ENDLOOP.

*********if error means exit the method**********
      IF message IS NOT INITIAL.
        type = 'E'.
        EXIT.
      ENDIF.
    ENDIF.

*****************create purchase return order************
    IF gs_input IS NOT INITIAL AND lv_purord IS INITIAL.
      CALL METHOD create_purchase_order
        EXPORTING
          lifnr      = lv_lifnr
          kunnr      = gs_input-distributor
          doc_type   = 'ZDRV'
          dist_plant = gv_plant
          she_plant  = gs_input-she_plant
          item_tab   = gs_input-details[]
        IMPORTING
          po_number  = lv_purord
          message    = lv_msg
          type       = lv_type.
    ENDIF.

*****************create MIGO************
    IF lv_purord IS NOT INITIAL AND lv_mblnr IS INITIAL.
      CALL METHOD goods_reciept
        EXPORTING
          movement_type  = '161'
          purchase_order = lv_purord
        IMPORTING
          mblnr          = lv_mblnr
          invoice_no     = invoice_no
          message        = lv_msg
          type           = lv_type.
    ENDIF.

*    CALL METHOD miro_invoice
*      EXPORTING
*        purchase_order   = lv_purord
*      IMPORTING
*        invoicedocnumber = lv_belnr
*        dms_invno        = invoice_no
*        message          = lv_msg
*        type             = lv_type.
*************e-invoice process**************
    IF lv_mblnr IS NOT INITIAL AND lv_irn IS INITIAL.
*************check e-invoice process*************
      SELECT SINGLE * FROM zdms_po_return INTO @DATA(ls_header2) WHERE dms_ordid = @gs_input-dms_orderid.
      SELECT SINGLE distributor FROM zdist_einv_dtls INTO @DATA(ls_check)
                                                     WHERE distributor = @gs_input-distributor
                                                     AND   act_date    LE @ls_header2-inv_date.
      IF sy-subrc = 0.
        DATA : lo_main1 TYPE REF TO zcl_dms_einvoice_process.
        CREATE OBJECT lo_main1.

        invoice_no = ls_header2-invoice_no.
***********call method for create_irn_qrcode_poreturn*************
        CLEAR lv_msg.
        lo_main1->create_irn_qrcode_poreturn(
          EXPORTING
            distributor_code = gs_input-distributor " Customer Number
            seller_plant     = gs_input-she_plant   " Plant
            po_number        = lv_purord            " Purchasing Document Number
            invoiceno        = ls_header2-invoice_no " Billing Document
            invoicedt        = ls_header2-inv_date     " Billing Date
            seller_invoiceno = ls_header2-invoice_no  " Billing Document
            bukrs            = 'DMS1'               " Company Code
            gjahr            = ls_header2-mjahr      " Fiscal Year
          IMPORTING
            irn              = lv_irn               " Invoice Reference Number
            signed_qrcode    = signed_qrcode        " Signed QR Code
            signed_inv       = lv_signed_inv        " Signed QR Code
            pdf              = pdf
            ack_no           = ack_no
            ack_date         = ack_date
            return           = lv_msg ).

***********fill the response***********
        IF lv_irn IS NOT INITIAL.
          lv_type = 'S'.
          lv_msg  = 'E-Invoice created successfully'.
        ELSE.
          lv_type = 'E'.
        ENDIF.
      ENDIF.
**************update the log table only for e-invoice process**************
      UPDATE zdms_po_return SET   message       = lv_msg  type          = lv_type
                                  status        = '16'    signed_inv    = lv_signed_inv
                                  irn           = lv_irn  signed_qrcode = signed_qrcode
                                  pdf           = pdf     ack_date      = ack_date
                                  ack_no        = ack_no
                            WHERE dms_ordid = gs_input-dms_orderid.
    ENDIF.

*************export the parameter**********
    message       = lv_msg.
    type          = lv_type.
    po_number     = lv_purord.
    irn           = lv_irn.

  ENDMETHOD.
ENDCLASS.
