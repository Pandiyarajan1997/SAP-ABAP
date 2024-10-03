CLASS zcl_api_sales_return_shn DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_http_extension .

    TYPES:
      BEGIN OF ty_po_data,
        vbeln       TYPE vbeln,
        posnr       TYPE posnr,
        ebeln       TYPE ebeln,
        distributor TYPE kunnr,
        mblnr       TYPE mblnr,
        belnr       TYPE belnr_d,
        she_plant   TYPE werks_d,
        material    TYPE matnr,
        batch       TYPE charg_d,
        qty         TYPE menge_d,
        uom         TYPE meins,
        batch_count TYPE i,
      END OF ty_po_data .
    TYPES:
      tt_po_data TYPE TABLE OF ty_po_data .
    TYPES:
      BEGIN OF ty_msg_res,
        message    TYPE string,
        type       TYPE char1,
        Invoice_no TYPE string,
        Sord_no    TYPE string,
*        dms_ord_id TYPE char20,
*        Purchase_Order_no TYPE ebeln,
*        material    type matnr,
      END OF ty_msg_res .
    TYPES:
      tt_msg_res TYPE TABLE OF ty_msg_res WITH DEFAULT KEY .

    DATA gs_input TYPE zsd_st_return_input_shn .

    METHODS return_order
      IMPORTING
        !LS_po_data     TYPE ty_po_data
        !ls_dmsid       TYPE char20
        !gs_vbrp        TYPE ty_po_data OPTIONAL
        !gs_items       TYPE zsd_dms_spl_sor OPTIONAL
        !gt_vbrp        TYPE tt_po_data OPTIONAL
      EXPORTING
        !return_orderno TYPE vbeln
        !message        TYPE string
        !type           TYPE bapi_mtype .
    METHODS delivery
      IMPORTING
        !return_orderno TYPE vbeln
        !dms_orderid    TYPE zorder_id OPTIONAL
      EXPORTING
        !delivery_no    TYPE vbeln_vl
        !message        TYPE string
        !type           TYPE bapi_mtype .
    METHODS pgi
      IMPORTING
        !delivery_no TYPE vbeln_vl
        !dms_orderid TYPE zorder_id OPTIONAL
      EXPORTING
        !message     TYPE string
        !type        TYPE bapi_mtype
        !pgi_no      TYPE mblnr .
    METHODS invoice
      IMPORTING
        !delivery_no   TYPE vbeln_vl
        !dms_orderid   TYPE zorder_id OPTIONAL
      EXPORTING
        !company_code  TYPE bukrs
        !invoice_no    TYPE vbeln_vf
        !message       TYPE string
        !type          TYPE bapi_mtype
        !year          TYPE gjahr
        !accounting_no TYPE mblnr .
    METHODS validations_inv
      EXPORTING
        !message        TYPE string
        !type           TYPE bapi_mtype
        !invoice_no     TYPE vbeln_vf
        !sord_no        TYPE vbeln
        !et_response    TYPE tt_msg_res
        !irn            TYPE j_1ig_irn
        !signed_qrcode  TYPE j_1ig_sign_qrcode
        !ack_no         TYPE j_1ig_ack_no
        !ack_date       TYPE j_1ig_ack_date
        !gt_vbrp        TYPE tt_po_data
        !ls_distributor TYPE kunnr .
*      !ACK_NO type J_1IG_ACK_NO
*      !ACK_DATE type J_1IG_ACK_DATE
    METHODS overall_process
      IMPORTING
        VALUE(input)          TYPE zsd_st_return_input_shn
        VALUE(gt_vbrp)        TYPE tt_po_data OPTIONAL
        VALUE(gt_items)       TYPE zsd_dms_spl_sor OPTIONAL
        VALUE(lv_pgi)         TYPE mblnr
        VALUE(lv_invoice)     TYPE vbeln_vf
        VALUE(lv_returnorder) TYPE vbeln
        VALUE(lv_delivery)    TYPE vbeln_vl
        VALUE(year)           TYPE gjahr
      EXPORTING
        !signed_qrcode        TYPE j_1ig_sign_qrcode
        !irn                  TYPE j_1ig_irn
        !message              TYPE string
        !type                 TYPE bapi_mtype
        !invoice_no           TYPE vbeln_vf
        !sord_no              TYPE vbeln
        !et_response          TYPE tt_msg_res .
    METHODS common_validations
      IMPORTING
        VALUE(gt_vbrp) TYPE tt_po_data
      EXPORTING
        !lv_msg        TYPE string .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_api_sales_return_shn IMPLEMENTATION.


  METHOD common_validations.

    TYPES:BEGIN OF lt_qty,
            vbeln TYPE vbeln,
            matnr TYPE matnr,
            charg TYPE charg_d,
            fkimg TYPE fkimg,
*         uom TYPE meins,
          END OF lt_qty,
          BEGIN OF ty_po_itms,
            vbeln TYPE vbeln,
            matnr TYPE matnr,
            fkimg TYPE fkimg,
          END OF ty_po_itms.

    DATA: lt_bill     TYPE TABLE OF lt_qty,
          lt_ret_itms TYPE TABLE OF ty_po_itms.

    DATA(ls_distributor) = VALUE #( gt_vbrp[ 1 ]-distributor OPTIONAL ).
***********************distributor check***************
    SELECT SINGLE kunnr,werks,regio,stcd3 FROM kna1 INTO @DATA(ls_dist) WHERE kunnr = @ls_distributor.
    IF sy-subrc NE 0.
      lv_msg = | Distributor is not found - { gs_input-invoice_no }|.
      EXIT.
    ELSE.
*********************Distributor Code(gst) & region check*****************************
      IF ls_dist-stcd3 IS NOT INITIAL.
        IF ls_dist-stcd3(1) = space.
          lv_msg = |{ lv_msg } ; Distributor { gs_input-distributor } GST is incorrect|.
        ENDIF.
        IF ls_dist-regio IS NOT INITIAL.
          SELECT SINGLE region,gst_key FROM zsd_gst_reg INTO @DATA(gs_gst) WHERE region = @ls_dist-regio.
          DATA(lv_gstkey) = CONV char02( gs_gst-gst_key ).
          IF ls_dist-stcd3(2) <> lv_gstkey.
            lv_msg = |{ lv_msg } ; Distributor { gs_input-distributor } GST and Region not matching|.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

***********************dealer check***************
    SELECT SINGLE kunnr,regio,stcd3 FROM kna1 INTO @DATA(ls_cust) WHERE kunnr = @ls_distributor.
    IF sy-subrc NE 0.
      lv_msg = | Dealer is not found - { gs_input-dms_orderid }|.
      EXIT.
    ENDIF.
*************fetch the gst condition*******
    SELECT a~kschl,a~wkreg,a~regio,a~matnr,a~knumh,b~loevm_ko FROM a709 AS a
                                                              INNER JOIN konp AS b ON a~knumh EQ b~knumh
                                                              INTO TABLE @DATA(lt_cond)
                                                              FOR ALL ENTRIES IN @gt_vbrp
                                                              WHERE wkreg = @ls_dist-regio
                                                              AND   regio = @ls_cust-regio
                                                              AND   matnr = @gt_vbrp-material
                                                              AND   datbi GE @sy-datum.


    SELECT FROM vbrp
         FIELDS vbeln ,  matnr , charg,  fkimg ", meins
         FOR ALL ENTRIES IN @gt_vbrp
         WHERE vbeln = @gt_vbrp-vbeln AND
               matnr = @gt_vbrp-material "and
*               charg = @lt_vbrp-batch
         INTO TABLE @DATA(lt_po_itm).

    IF lt_po_itm IS NOT INITIAL.
      SELECT FROM zdms_po_return AS Ret_HDR
   INNER JOIN zdms_po_ret_item AS Ret_ITM
   ON Ret_ITM~dms_orderid = Ret_HDR~dms_ordid
   FIELDS Ret_ITM~ref_invno , Ret_ITM~material,  Ret_ITM~qty
   WHERE Ret_HDR~dms_ordid = @gs_input-dms_orderid
   ORDER BY material ,qty
   INTO TABLE @DATA(gt_po_rt).

      IF gt_po_rt IS NOT INITIAL.
        SORT gt_po_rt BY material.
        LOOP AT gt_po_rt INTO DATA(po_items).
          COLLECT po_items INTO lt_ret_itms.         " material quantity of entire po return based on Ref.Invoice
        ENDLOOP.
      ENDIF.

      LOOP AT lt_po_itm INTO DATA(ls_itm).
        COLLECT ls_itm INTO lt_bill.
      ENDLOOP.
      SORT lt_bill BY matnr.
***************************Validating Material quantity for history of current po return items qty and invoice quantity based on Ref.Invoice
      LOOP AT lt_ret_itms INTO DATA(ls_ret_itms).  "Po ret items
        READ TABLE lt_bill INTO DATA(ls_bill) WITH KEY vbeln = ls_ret_itms-vbeln matnr = ls_ret_itms-matnr.
        IF  sy-subrc = 0 AND ls_bill-fkimg < ls_ret_itms-fkimg .
          lv_msg = | Po Return Quantity { ls_ret_itms-fkimg } for the Ref. Invoice is greater than invoice quantity { ls_bill-fkimg } for material { ls_ret_itms-matnr } , { lv_msg }|.
        ENDIF.
      ENDLOOP.
***************************Validating Material quantity in current po return qty and invoice quantity
      LOOP AT gt_vbrp INTO DATA(ls_vbrp).
        READ TABLE lt_bill ASSIGNING FIELD-SYMBOL(<fs_bill>)  WITH KEY vbeln = ls_vbrp-vbeln
                                                       matnr = ls_vbrp-material.
        IF <fs_bill> IS ASSIGNED.
          IF <fs_bill>-fkimg < ls_vbrp-qty.
            lv_msg = |Po Return Quantity { ls_vbrp-qty } is greater than Invoice quantity { <fs_bill>-fkimg } { ls_vbrp-material } , { lv_msg }|.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

***************get the material details************
    SELECT a~matnr,a~zterm,a~tragr,b~werks,b~ladgr,b~steuc,b~prctr,c~mvgr1,c~vkorg,c~vtweg FROM mara AS a
                                                           INNER JOIN marc AS b ON a~matnr = b~matnr
                                                           INNER JOIN mvke AS c ON a~matnr = c~matnr
                                                           INTO TABLE @DATA(lt_material)
                                                           FOR ALL ENTRIES IN @gt_vbrp
                                                           WHERE a~matnr = @gt_vbrp-material.
    IF sy-subrc = 0.
      SORT lt_material BY matnr werks.
    ENDIF.
**********Looping process**************
    LOOP AT gt_vbrp ASSIGNING FIELD-SYMBOL(<fs_details>).

      AT NEW material.
***********Transportation group check*************
        READ TABLE lt_material INTO DATA(ls_material) WITH KEY matnr = <fs_details>-material BINARY SEARCH.
        IF ls_material-tragr IS INITIAL.
          lv_msg = |Transportation Group Missing { <fs_details>-material } , { lv_msg }|.
        ELSE.
          IF ls_material-tragr <> '0001'.
            lv_msg = |Transportation Group Incorrect{ <fs_details>-material }  , { lv_msg }|.
          ENDIF.
        ENDIF.
*********plant wise check****************
        READ TABLE lt_material INTO ls_material WITH KEY matnr = <fs_details>-material
                                                         werks = <fs_details>-she_plant BINARY SEARCH.
        IF sy-subrc <> 0.

          lv_msg = |Material { <fs_details>-material } Not Maintained in Plant { ls_dist-werks } , { lv_msg }|.
        ELSE.
*********************Loading group check*************
          IF ls_material-ladgr IS INITIAL.
            lv_msg = |Loading Group Missing { <fs_details>-material } , { lv_msg }|.
          ELSE.
            IF ls_material-ladgr <> '0004'.
              lv_msg = |Loading Group Incorrect { <fs_details>-material } , { lv_msg }|.
            ENDIF.
          ENDIF.
*********************HSN & Profit Center Missing Check*****************************
          IF ls_material-steuc IS INITIAL.
            lv_msg = |{ lv_msg } ; HSN Missing for Material { <fs_details>-material }|.
          ENDIF.
          IF ls_material-prctr IS INITIAL.
            lv_msg = |{ lv_msg } ; Profit Center Missing for Material { <fs_details>-material }|.
          ENDIF.
        ENDIF.
************************material group1 check***********************************
        READ TABLE lt_material INTO ls_material WITH KEY matnr = <fs_details>-material
                                                         vkorg = 1000
                                                         vtweg = '20'.
        IF sy-subrc <> 0.
          lv_msg = |Material { <fs_details>-material } Not Maintained in Sales org 1000 & Dis.Channel 20 , { lv_msg }|.
        ELSE.
          IF ls_material-mvgr1 IS INITIAL.
            lv_msg = |Material GRP 1 Missing - { <fs_details>-material } , { lv_msg }|.
          ENDIF.
        ENDIF.
*----------------------------------------------------------------------------------
**********gst check***********
        IF ls_dist-regio = ls_cust-regio.
          READ TABLE lt_cond TRANSPORTING NO FIELDS WITH KEY kschl    = 'JOCG'
                                                             wkreg    = ls_dist-regio
                                                             regio    = ls_cust-regio
                                                             matnr    = <fs_details>-material
                                                             loevm_ko = abap_false.
          IF sy-subrc NE 0.
            lv_msg = |Material JOCG is not maintained { <fs_details>-material } , { lv_msg }|.
          ENDIF.
          READ TABLE lt_cond TRANSPORTING NO FIELDS WITH KEY kschl    = 'JOSG'
                                                             wkreg    = ls_dist-regio
                                                             regio    = ls_cust-regio
                                                             matnr    = <fs_details>-material
                                                             loevm_ko = abap_false.
          IF sy-subrc NE 0.
            lv_msg = |Material JOSG is not maintained { <fs_details>-material } , { lv_msg }|.
          ENDIF.
        ELSE.
          READ TABLE lt_cond TRANSPORTING NO FIELDS WITH KEY kschl    = 'JOIG'
                                                             wkreg    = ls_dist-regio
                                                             regio    = ls_cust-regio
                                                             matnr    = <fs_details>-material
                                                             loevm_ko = abap_false.
          IF sy-subrc NE 0.
            lv_msg = |Material JOIG is not maintained { <fs_details>-material } , { lv_msg }|.
          ENDIF.
        ENDIF.
      ENDAT.
***********check material qty and uom missing************
      IF <fs_details>-qty IS INITIAL.
        lv_msg = |Quantity is missing - { <fs_details>-material } , { lv_msg } |.
      ENDIF.
      IF <fs_details>-uom IS INITIAL.
        lv_msg = |UOM is missing - { <fs_details>-material } , { lv_msg } |.
      ENDIF.

      CLEAR : ls_material.
    ENDLOOP.

  ENDMETHOD.


  METHOD delivery.

    DATA: lt_return    TYPE TABLE OF bapiret2,
          lt_salesitem TYPE TABLE OF bapidlvreftosalesorder.
    DATA:lv_returnorder  TYPE vbeln.

*     IMPORT lv_returnorder = lv_returnorder FROM MEMORY ID 'SHN'.
    IF return_orderno IS NOT INITIAL.
**************get the sales return details************
      SELECT vbeln,posnr,kwmeng,vrkme FROM vbap INTO TABLE @DATA(lt_vbap)
                                                WHERE vbeln = @return_orderno.
      LOOP AT lt_vbap ASSIGNING FIELD-SYMBOL(<fs_vbap>).
        APPEND VALUE #( ref_doc    = <fs_vbap>-vbeln

                        ref_item   = <fs_vbap>-posnr
                        dlv_qty    = <fs_vbap>-kwmeng
                        sales_unit = <fs_vbap>-vrkme ) TO lt_salesitem.
      ENDLOOP.
      "Bapi to create Delivery alone
      CALL FUNCTION 'BAPI_OUTB_DELIVERY_CREATE_SLS'
        IMPORTING
          delivery          = delivery_no
        TABLES
          sales_order_items = lt_salesitem
          return            = lt_return.
      READ TABLE lt_return INTO DATA(ls_return) WITH KEY type = 'E'.
      IF sy-subrc = 0.
        message = ls_return-message.
        type    = 'E'.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
        message = | delivery is completed - { delivery_no } |.
        type    = 'S'.
      ENDIF.
      IF dms_orderid IS NOT INITIAL.
        UPDATE zsd_dms_spl_sor SET   so_msg = message so_type = type so_stat = '12' so_delivery = delivery_no
                                WHERE dms_ordid = dms_orderid AND return_orderno = return_orderno.
        IF sy-subrc = 0.
          COMMIT WORK.
        ELSE.
          ROLLBACK WORK.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD if_http_extension~handle_request.
*Created By: Zakir hussain
*Created On: 09.04.2024
*Reference: Ramakrishnan J
*Purpose: Sales return from Distributor to Sheenlac
*-------------------------------------------------------------*
    TYPES: tt_ret_doc TYPE TABLE OF ty_msg_res WITH DEFAULT KEY.


    TYPES: BEGIN OF ty_msg,
             orderid     TYPE zorder_id,       "dms ord id
             distributor TYPE kunnr,           "distributor code
*             Ref_invoice_no     TYPE vbeln,           "ref invoice no
             Return_Msgs TYPE tt_ret_doc,         " Return msgs
           END OF ty_msg.


    DATA:lt_vbrp TYPE TABLE OF ty_po_data .
    DATA:lt_ret_doc TYPE STANDARD TABLE OF ty_msg_res .
****************************************************************
    DATA : ls_response TYPE ty_msg,
           gs_response TYPE ty_msg,
           lv_batch    TYPE char1,
           lv_message  TYPE string,
           lv_type     TYPE bapi_mtype.

    DATA: lv_body TYPE string.
    DATA: lv_data TYPE string.
************API log**********
    DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
    CREATE OBJECT lo_log_upd.
************dms error log**********
    DATA: lo_errorlog_dms TYPE REF TO zcl_api_dms_error_log_entries.
    CREATE OBJECT lo_errorlog_dms.

    CALL METHOD server->request->get_cdata RECEIVING data = lv_data.

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
    gs_input-invoice_no  = |{ gs_input-invoice_no ALPHA = IN }|.
*************check the distributor check status*************

    CALL METHOD validations_inv(
      IMPORTING
        et_response    = lt_ret_doc
        ls_distributor = DATA(ws_distributor)
    ).


    gs_response-orderid      = gs_input-dms_orderid.
    gs_response-distributor  = ws_distributor.
*    gs_response-Ref_invoice_no   = gs_input-invoice_no.
    gs_response-return_msgs = lt_ret_doc.

    IF gs_response IS NOT INITIAL.       " Check response is not initial
*********************error log***********
      SELECT SINGLE kunnr,werks FROM kna1 INTO @DATA(ls_kna1) WHERE kunnr = @gs_input-distributor.
      IF gs_response-return_msgs IS NOT INITIAL.
        "Duplicate Return order from Return order table error log
        READ TABLE lt_ret_doc  WITH KEY type = 'E' TRANSPORTING NO FIELDS.    " when error
        IF sy-subrc = 0.
          LOOP AT lt_ret_doc INTO DATA(ls_err_rep) WHERE type = 'E'.
            lo_errorlog_dms->log_entry_store(
              EXPORTING
                type                = 18
                status              = 10
                dms_orderid         = gs_response-orderid
                distributor         = gs_response-distributor
                plant               = ls_kna1-werks
*            dealer              = ls_response-dealer
                msg                 = ls_err_rep-message ).
** serialize the output for response ***
            /ui2/cl_json=>serialize(
            EXPORTING
             data         =  gs_response
             pretty_name  = /ui2/cl_json=>pretty_mode-user
            RECEIVING
             r_json         = lv_body ).

*Output Entry in Log Table
            CALL METHOD lo_log_upd->log_entry_store
              EXPORTING
                apiname         = 'SPL_RETURN_ORDER'
                ijson           = lv_data
                ojson           = lv_body
                distributor     = gs_input-distributor
*               retailer        = gs_input-dealer
              EXCEPTIONS
                apiname_missing = 1
                json_missing    = 2
                OTHERS          = 3.
          ENDLOOP.
        ENDIF.

        READ TABLE lt_ret_doc  WITH KEY type = 'S' TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          LOOP AT lt_ret_doc INTO DATA(gs_ret_doc) WHERE type = 'S'.         " when Success
            lo_errorlog_dms->log_entry_store(
             EXPORTING
               type                = 18
               status              = 10
               dms_orderid         = gs_response-orderid
               distributor         = gs_response-distributor
               plant               = ls_kna1-werks
*            dealer              = ls_response-dealer
               msg                 = gs_ret_doc-message ).
** serialize the output for response ***
            /ui2/cl_json=>serialize(
            EXPORTING
             data         =  gs_response
             pretty_name  = /ui2/cl_json=>pretty_mode-user
            RECEIVING
             r_json         = lv_body ).

*Output Entry in Log Table
            CALL METHOD lo_log_upd->log_entry_store
              EXPORTING
                apiname         = 'SPL_RETURN_ORDER'
                ijson           = lv_data
                ojson           = lv_body
                distributor     = gs_input-distributor
*               retailer        = gs_input-dealer
              EXCEPTIONS
                apiname_missing = 1
                json_missing    = 2
                OTHERS          = 3.
          ENDLOOP.
        ENDIF.

      ENDIF.
*Set JSON Content-Type
            CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
            CALL METHOD server->response->set_cdata( data = lv_body ).
            DELETE FROM zsd_invoice_dms WHERE invoice_no = gs_input-dms_orderid.
    ENDIF.

  ENDMETHOD.


  METHOD invoice.
    DATA: vbsk_i TYPE vbsk.
    DATA: d_success TYPE c.
    DATA: xkomfk    TYPE STANDARD TABLE OF komfk,
          w_xkomfk  TYPE komfk,
          xkomv     TYPE STANDARD TABLE OF komv,
          xthead    TYPE STANDARD TABLE OF theadvb,
          xvbfs     TYPE STANDARD TABLE OF vbfs,
          xvbpa     TYPE STANDARD TABLE OF vbpavb,
          xvbrk     TYPE STANDARD TABLE OF vbrkvb,
          xvbrp     TYPE STANDARD TABLE OF vbrpvb,
          xvbss     TYPE STANDARD TABLE OF vbss,
          xkomfkgn  TYPE STANDARD TABLE OF komfkgn,
          ls_header TYPE zsd_retrn_hd_dms.

    DATA:lv_returnorder  TYPE vbeln.

    IMPORT lv_returnorder = lv_returnorder FROM MEMORY ID 'SHN'.

    IF delivery_no IS NOT INITIAL.
      REFRESH: xkomfk, xkomv,
      xthead, xvbfs,
      xvbpa, xvbrk,
      xvbrp, xvbss.

      CLEAR : xkomfk, xkomv,
      xthead, xvbfs,
      xvbpa, xvbrk,
      xvbrp, xvbss,
      vbsk_i.

      vbsk_i-smart = 'F'.
      w_xkomfk-vbeln = delivery_no.
      w_xkomfk-vbtyp = 'J'.

      APPEND w_xkomfk TO xkomfk.

      CALL FUNCTION 'RV_INVOICE_CREATE'
        EXPORTING
          vbsk_i       = vbsk_i
          with_posting = 'C'
        TABLES
          xkomfk       = xkomfk
          xkomv        = xkomv
          xthead       = xthead
          xvbfs        = xvbfs
          xvbpa        = xvbpa
          xvbrk        = xvbrk
          xvbrp        = xvbrp
          xvbss        = xvbss.

      IF sy-subrc EQ 0.
        COMMIT WORK.
        SELECT SINGLE vbeln FROM vbfa INTO invoice_no
                            WHERE vbelv = delivery_no AND vbtyp_n = 'O'.
        SELECT SINGLE belnr
                      bukrs
                      gjahr FROM vbrk INTO ( accounting_no, company_code, year )
                            WHERE vbeln = invoice_no.
        message = | Invoice is created - { invoice_no } - { year } |.
        type    = 'S'.
      ELSE.
        message = | Invoice is not created|.
        type    = 'E'.
      ENDIF.
      IF dms_orderid IS NOT INITIAL.
        UPDATE zsd_dms_spl_sor SET   so_msg = message so_type = type so_stat = '14' gjahr = year
                                      so_invno = invoice_no so_acnt_doc = accounting_no
                                WHERE dms_ordid = dms_orderid AND return_orderno = lv_returnorder.
        IF sy-subrc = 0.
          COMMIT WORK.
        ELSE.
          ROLLBACK WORK.
        ENDIF.
      ENDIF.
    ELSE.
      message = | Fill delivery no |.
      type    = 'E'.
    ENDIF.
  ENDMETHOD.


  METHOD overall_process.
**********e invoice class data dec**********
    DATA : lo_main1 TYPE REF TO zcl_dms_einvoice_process.
*    CREATE OBJECT lo_main1.
    DATA : lv_bukrs    TYPE bukrs,
           lv_response TYPE string,
           signed_inv  TYPE j_1ig_sign_inv,
           pdf         TYPE fpcontent.
    DATA : ls_vbrp LIKE LINE OF gt_vbrp.
    DATA : lv_posnr TYPE posnr,
           lv_count TYPE int4.


    IF  gt_items IS NOT INITIAL.
      IF lv_delivery IS  INITIAL.   "When Duplicate DMS Order present and in Error Status=> Reprocessing takes place
        CALL METHOD delivery(
          EXPORTING
            return_orderno = gt_items-return_orderno
            dms_orderid    = gs_input-dms_orderid
          IMPORTING
            delivery_no    = lv_delivery
            message        = message
            type           = type
        ).
      ENDIF.
      IF lv_delivery IS NOT INITIAL AND lv_pgi IS  INITIAL .
        EXPORT lv_returnorder = lv_returnorder TO MEMORY ID 'SHN'.
        CALL METHOD pgi(
          EXPORTING
            delivery_no = lv_delivery
            dms_orderid = gs_input-dms_orderid
          IMPORTING
            message     = message
            type        = type
            pgi_no      = lv_pgi
        ).
      ENDIF.
      IF lv_delivery IS NOT INITIAL AND lv_invoice IS INITIAL AND lv_pgi IS NOT INITIAL.
        EXPORT lv_returnorder = lv_returnorder TO MEMORY ID 'SHN'.
        CALL METHOD invoice(
          EXPORTING
            delivery_no = lv_delivery
            dms_orderid = gs_input-dms_orderid
          IMPORTING
            invoice_no  = lv_invoice
            message     = message
            type        = type
            year        = year
        ).
      ENDIF.
    ENDIF.

    IF gt_vbrp IS NOT INITIAL.
      LOOP AT gt_vbrp INTO DATA(ws_vbrp).
        ADD 1 TO lv_count.
        EXPORT lv_count = lv_count  TO MEMORY ID 'CNT'.
*****************create return order************
        IF input IS NOT INITIAL." AND lv_returnorder IS INITIAL.
          CALL METHOD return_order
            EXPORTING
              ls_po_data     = ws_vbrp           " Structure for Direct Sales Order from SHN to all
              ls_dmsid       = input-dms_orderid
              gs_vbrp        = ls_vbrp                 " Return PO HDR and ITM Structure
              gt_vbrp        = gt_vbrp
            IMPORTING
              return_orderno = lv_returnorder
              message        = message
              type           = type.              " Message type: S Success, E Error, W Warning, I Info, A Abort
        ENDIF.
        EXPORT lv_returnorder = lv_returnorder TO MEMORY ID 'SHN'.
*****************create delivery************
        IF lv_returnorder IS NOT INITIAL AND lv_delivery IS INITIAL.
          CALL METHOD delivery(
            EXPORTING
              return_orderno = lv_returnorder
              dms_orderid    = gs_input-dms_orderid
            IMPORTING
              delivery_no    = lv_delivery
              message        = message
              type           = type
          ).
        ENDIF.
*****************create PGI************
        IF lv_delivery IS NOT INITIAL AND lv_pgi IS INITIAL.
          CALL METHOD pgi(
            EXPORTING
              delivery_no = lv_delivery
              dms_orderid = gs_input-dms_orderid
            IMPORTING
              message     = message
              type        = type
              pgi_no      = lv_pgi
          ).
        ENDIF.
*****************create invoice************
        IF lv_delivery IS NOT INITIAL AND lv_invoice IS INITIAL AND lv_pgi IS NOT INITIAL.
          CALL METHOD invoice(
            EXPORTING
              delivery_no = lv_delivery
              dms_orderid = gs_input-dms_orderid
            IMPORTING
              invoice_no  = lv_invoice
              message     = message
              type        = type
              year        = year
          ).

        ENDIF.
        IF lv_invoice IS NOT INITIAL AND message IS NOT INITIAL.
          UPDATE zsd_dms_spl_sor SET   so_msg = message  so_type = type so_stat = '17'
*                                       = irn      signed_inv = signed_inv
*                                      pdf = pdf      signed_qrcode = signed_qrcode
                                  WHERE dms_ordid = gs_input-dms_orderid AND return_orderno = lv_returnorder.

        ENDIF.

        DATA(gs_response) = VALUE me->ty_msg_res( message = message type = type sord_no = lv_returnorder invoice_no = lv_invoice  ).
        APPEND gs_response TO et_response.
        DELETE gt_vbrp WHERE batch_count = lv_count.

        CLEAR:lv_returnorder,message,lv_delivery,lv_pgi,lv_invoice.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD pgi.

    DATA : ls_vbkok TYPE vbkok,
           lt_prott TYPE TABLE OF prott,
           lv_msg   TYPE string.

    DATA:lv_returnorder  TYPE vbeln.

    IMPORT lv_returnorder = lv_returnorder FROM MEMORY ID 'SHN'.
    IF delivery_no IS NOT INITIAL.
      ls_vbkok-vbeln_vl  = delivery_no.
      ls_vbkok-wadat_ist = sy-datum.
      ls_vbkok-wabuc     = 'X'.       "<- automatic pgi
      ls_vbkok-komue     = 'X'.       "<- automatic pgi
      CALL FUNCTION 'WS_DELIVERY_UPDATE'
        EXPORTING
          vbkok_wa                 = ls_vbkok
          synchron                 = 'X'
          no_messages_update       = ' '
          commit                   = 'X'
          delivery                 = delivery_no
          update_picking           = 'X'
          nicht_sperren            = 'X'
          if_database_update       = '1'
          if_error_messages_send_0 = 'X'
        TABLES
          prot                     = lt_prott.
      LOOP AT lt_prott INTO DATA(lw_prot) WHERE msgty = 'A' OR msgty = 'E'.
        CALL FUNCTION 'MESSAGE_TEXT_BUILD'
          EXPORTING
            msgid               = lw_prot-msgid
            msgnr               = lw_prot-msgno
            msgv1               = lw_prot-msgv1
            msgv2               = lw_prot-msgv2
            msgv3               = lw_prot-msgv3
            msgv4               = lw_prot-msgv3
          IMPORTING
            message_text_output = lv_msg.
        message = | { message } , { lv_msg } |.
        type    = 'E'.
      ENDLOOP.
      IF sy-subrc <> 0.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
      ENDIF.
**********get the pgi no*********
      SELECT SINGLE mblnr FROM mseg
        INTO pgi_no
        WHERE vbeln_im = delivery_no.
      IF sy-subrc = 0.
        message = |Pgi completed - { pgi_no }|.
        type    = 'S'.
      ENDIF.
      IF dms_orderid IS NOT INITIAL.
        UPDATE zsd_dms_spl_sor SET   so_msg = message so_type = type so_stat = '13' so_pgi_no = pgi_no
                                WHERE dms_ordid = dms_orderid AND return_orderno = lv_returnorder .
        IF sy-subrc = 0.
          COMMIT WORK.
        ELSE.
          ROLLBACK WORK.
        ENDIF.
      ENDIF.
    ELSE.
      message = | Fill delivery no |.
      type    = 'E'.
    ENDIF.

  ENDMETHOD.


  METHOD return_order.

    DATA : lt_return       TYPE TABLE OF  bapiret2,
           lt_item         TYPE TABLE OF  bapisditm,
           lt_itemx        TYPE TABLE OF  bapisditmx,
           lt_partners     TYPE TABLE OF  bapiparnr,
           lt_cond         TYPE TABLE OF bapicond,
           ls_cond         TYPE bapicond,
           lt_oldcondition TYPE TABLE OF bapisdcond,
           ls_oldcond      TYPE bapisdcond,
           lt_olditem      TYPE TABLE OF bapisditbos,
           ls_olditem      TYPE bapisditbos.
    DATA : lt_schedules_inx TYPE STANDARD TABLE OF bapischdlx.
    DATA : lt_schedules_in  TYPE STANDARD TABLE OF bapischdl.
    DATA : lv_posnr   TYPE posnr.
    DATA : ls_header  TYPE bapisdhd1,
           ls_headerx TYPE bapisdhd1x,
           ls_item    TYPE bapisditm,
           ls_itemx   TYPE bapisditmx,
           ls_loghead TYPE zsd_dms_spl_sor,
           lt_logitem TYPE TABLE OF zsd_dms_spl_sor,
           ls_log     TYPE zsd_dms_spl_sor.
    FIELD-SYMBOLS: <fs_details_1> LIKE gs_items.
    DATA:ls_qty TYPE fkimg.
    DATA:lv_count TYPE int4,
         gs_posnr TYPE posnr.


*    IMPORT lv_posnr = lv_posnr FROM MEMORY ID 'SPL'.
    IMPORT lv_count = lv_count FROM MEMORY ID 'CNT'.
**************Get the sales order details******************
    IF  ls_po_data  IS NOT INITIAL.
      SELECT FROM vbrp
  FIELDS vbeln,posnr,matnr,charg ,fkimg
  WHERE vbeln =  @ls_po_data-vbeln  "InvoiceNo
  INTO TABLE @DATA(gt_bill).

      SELECT SINGLE aubel,erdat FROM vbrp INTO @DATA(ls_vbrp) WHERE vbeln = @ls_po_data-vbeln."InvoiceNo

      IF ls_vbrp IS NOT INITIAL.
        CALL FUNCTION 'BAPI_SALESORDER_GETDETAILBOS'
          EXPORTING
            salesdocument   = ls_vbrp-aubel
          TABLES
            orderconditions = lt_oldcondition
            orderitems      = lt_olditem.
      ENDIF.
    ENDIF.

*-----------------------------------------------------------------*
* Build order item data & flags
*-----------------------------------------------------------------*
*IF gt_vbrp is NOT INITIAL.
*      add 1 to count.
*    LOOP AT gt_vbrp ASSIGNING FIELD-SYMBOL(<fs_details>) where vbeln = ls_invoice." WHERE batch_count = lv_count.
*****************item data filling**************
*data(gs_bill) = VALUE #( gt_bill[ vbeln = <fs_details>-vbeln  matnr = <fs_details>-material  ]  ).
*READ TABLE gt_bill INTO data(gs_bill) WITH KEY vbeln = ls_po_data-vbeln  matnr = ls_po_data-material.
*if sy-subrc = 0.
*data(lv_index) = sy-tabix.
*lv_posnr = gs_bill-posnr.
    lv_posnr = lv_posnr + 1.
    CLEAR : ls_item,ls_itemx.
    ls_item-itm_number = lv_posnr.
*    if <fs_details> is ASSIGNED.
    ls_item-material   = ls_po_data-material.
    ls_item-target_qty = ls_po_data-qty.
    ls_item-target_qu  = ls_po_data-uom.
    ls_item-batch      = ls_po_data-batch.
    ls_item-plant      = ls_po_data-she_plant.  "sheenlac plant
    ls_qty             = ls_po_data-qty.
*    ELSEIF <fs_details_1> is ASSIGNED.
*     ls_item-material   = <fs_details_1>-material.
*    ls_item-target_qty = <fs_details_1>-qty.
*    ls_item-target_qu  = <fs_details_1>-uom.
*    ls_item-batch      = <fs_details_1>-batch.
**    ls_item-plant      = <fs_details_1>-she_plant.  "sheenlac plant
*    ls_qty             = <fs_details_1>-qty.
*    endif.
    ls_item-store_loc  = '0006'. "<fs_details>-lgort.         "  0006 Stor. loc. maintained
    IF ls_po_data IS NOT INITIAL.
      ls_item-ref_doc     = ls_po_data-vbeln.
      ls_item-ref_doc_ca  = 'M'.
      ls_item-ref_doc_it  = lv_posnr.
      ls_itemx-ref_doc    = abap_true.
      ls_itemx-ref_doc_ca = abap_true.
      ls_itemx-ref_doc_it = abap_true.
***********pass the pricing condition**********
      CLEAR : ls_olditem.
      READ TABLE lt_olditem INTO ls_olditem WITH KEY material = ls_po_data-material.
******************************Z008***************
*        CLEAR : ls_cond,ls_oldcond.
*        READ TABLE lt_oldcondition INTO ls_oldcond WITH KEY itm_number = ls_olditem-itm_number
*                                                            cond_type  = 'Z008'.
*        IF sy-subrc = 0.
*          ls_cond-itm_number = lv_posnr.
*          ls_cond-cond_count = lv_posnr.
*          ls_cond-cond_type  = 'Z008'.
*          IF ls_oldcond-cond_value LT 0.
*            ls_cond-cond_value = ls_oldcond-cond_value * -10.
*          ENDIF.
*          ls_cond-currency   = 'INR'.
*          APPEND ls_cond TO lt_cond.
*        ENDIF.
******************************Z009***************
*  CLEAR : ls_cond,ls_oldcond.
*        READ TABLE lt_oldcondition INTO ls_oldcond WITH KEY itm_number = ls_olditem-itm_number
*                                                            cond_type  = 'Z009'.
*        IF sy-subrc = 0.
*          ls_cond-itm_number = lv_posnr.
*          ls_cond-cond_count = lv_posnr.
*          ls_cond-cond_type  = 'Z009'.
*          IF ls_oldcond-cond_value LT 0.
*            ls_cond-cond_value = ls_oldcond-cond_value * -10.
*          ENDIF.
*          ls_cond-currency   = 'INR'.
*          APPEND ls_cond TO lt_cond.
*        ENDIF.
*****************************Z011***************
      CLEAR : ls_cond,ls_oldcond.
      READ TABLE lt_oldcondition INTO ls_oldcond WITH KEY itm_number = ls_olditem-itm_number
                                                          cond_type  = 'Z011'.
      IF sy-subrc = 0.
        ls_cond-itm_number = lv_posnr.
        ls_cond-cond_count = lv_posnr.
        ls_cond-cond_type  = 'Z011'.
        IF ls_oldcond-cond_value LT 0.
          ls_cond-cond_value = ls_oldcond-cond_value * -1.
          ls_cond-cond_value = ls_cond-cond_value / ls_olditem-req_qty.
          ls_cond-cond_value = ls_po_data-qty * ls_cond-cond_value.
        ENDIF.
        ls_cond-currency   = 'INR'.
        APPEND ls_cond TO lt_cond.
      ENDIF.
*****************************Z012***************
      CLEAR : ls_cond,ls_oldcond.
      READ TABLE lt_oldcondition INTO ls_oldcond WITH KEY itm_number = ls_olditem-itm_number
                                                          cond_type  = 'Z012'.
      IF sy-subrc = 0.
        ls_cond-itm_number = lv_posnr.
        ls_cond-cond_count = lv_posnr.
        ls_cond-cond_type  = 'Z012'.
        IF ls_oldcond-cond_value LT 0.
          ls_cond-cond_value = ls_oldcond-cond_value * -1.
          ls_cond-cond_value = ls_cond-cond_value / ls_olditem-req_qty.
          ls_cond-cond_value = ls_po_data-qty * ls_cond-cond_value.
        ENDIF.
        ls_cond-currency   = 'INR'.
        APPEND ls_cond TO lt_cond.
      ENDIF.
    ENDIF.
*        ENDIF.
    APPEND ls_item TO lt_item.
    ls_itemx-itm_number = lv_posnr.
    ls_itemx-material   = abap_true.
    ls_itemx-target_qty = abap_true.
    ls_itemx-target_qu  = abap_true.
    ls_itemx-batch      = abap_true.
    ls_itemx-plant      = abap_true.
    ls_itemx-store_loc  = abap_true.
    ls_itemx-updateflag = 'I'.
    APPEND ls_itemx TO lt_itemx.
**************Fill schedule line data & flags*************
    APPEND VALUE #( itm_number = lv_posnr
                    sched_line = lv_posnr
                    req_qty    = ls_qty ) TO lt_schedules_in.
    APPEND VALUE #( itm_number = lv_posnr
                    sched_line = abap_true
                    req_qty    = abap_true
                    updateflag = abap_true ) TO  lt_schedules_inx.
***********fill item data for log table*******
    APPEND VALUE #( mandt = sy-mandt
                    dms_ordid  = ls_dmsid
                    distributor = ls_po_data-distributor
                    vbeln     = ls_po_data-vbeln
                    material     = ls_po_data-material
                    posnr        = lv_posnr
                    batch        = ls_po_data-batch
                    qty          = ls_po_data-qty
                    uom          = ls_po_data-uom
                    created_by   = sy-uname
                    erdat        = sy-datum
                    erzet        = sy-uzeit ) TO  lt_logitem.

*    DELETE gt_bill INDEX lv_index.
*    ENDIF.
*  ENDLOOP.
*  ENDIF.

    IF message IS INITIAL.
**------------------------------------------------------------------
** Build order header data & flag
**------------------------------------------------------------------*
      CLEAR : ls_header,ls_headerx.
      ls_header-doc_type   = 'YBRE'.
      ls_header-ord_reason = '101'.
      ls_header-sd_doc_cat = 'H'.
      ls_header-comp_cde_b = '1000'."ls_input-compcode.
      ls_header-sales_org  = '1000'."ls_input-salesorg'.
      ls_header-distr_chan = '20'."ls_input-distchnl.
      ls_header-division   = '10'."ls_input-division.
      ls_header-business_partner_no = ls_po_data-distributor." soldtoparty.
**********check for inv ref.*********
      IF ls_po_data-vbeln IS NOT INITIAL.
        ls_header-refdoc_cat  = 'M'.
        ls_headerx-refdoc_cat = abap_true.
      ENDIF.
      ls_headerx-doc_type   = abap_true.
      ls_headerx-ord_reason = abap_true.
      ls_headerx-sd_doc_cat = abap_true.
      ls_headerx-comp_cde_b = abap_true.
      ls_headerx-sales_org  = abap_true.
      ls_headerx-distr_chan = abap_true.
      ls_headerx-division   = abap_true.
      ls_headerx-business_partner_no = abap_true.
*------------------------------------------------------------------
* Build partner information
*------------------------------------------------------------------
      lt_partners = VALUE #( ( partn_role = 'AG'
                               partn_numb = ls_po_data-distributor )
                             ( partn_role = 'WE'
                               partn_numb = ls_po_data-distributor ) ).
*------------------------------------------------------------------
*call the bapi to CREATE the RETURN Order
      CALL FUNCTION 'BAPI_CUSTOMERRETURN_CREATE'
        EXPORTING
          return_header_in     = ls_header
          return_header_inx    = ls_headerx
        IMPORTING
          salesdocument        = return_orderno
        TABLES
          return               = lt_return
          return_items_in      = lt_item
          return_items_inx     = lt_itemx
          return_partners      = lt_partners
          return_schedules_inx = lt_schedules_inx
          return_schedules_in  = lt_schedules_in
          return_conditions_in = lt_cond.

      READ TABLE lt_return INTO DATA(ls_return) WITH KEY type = 'E'.

      IF sy-subrc = 0.
        message = ls_return-message.
        type    = 'E'.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
        message = | Return sales order created - { return_orderno } |.
        type    = 'S'.
*************data store to log table header & item*************
        ls_log = VALUE #(   "dms_ordid = ls_dmsid
                            return_orderno = return_orderno
                            so_type        = 'S'
                            so_stat         = 11
                            so_msg           = message    ).

        MODIFY lt_logitem FROM ls_log TRANSPORTING return_orderno so_type so_stat so_msg WHERE dms_ordid =  ls_dmsid AND
                                          vbeln = ls_po_data-vbeln.
        MODIFY zsd_dms_spl_sor FROM TABLE lt_logitem.
*        MODIFY zsd_dms_spl_sor from ls_log.

        IF sy-subrc = 0.
          COMMIT WORK.
        ELSE.
          ROLLBACK WORK.
        ENDIF.
      ENDIF.
    ELSE.
      type = 'E'.
      EXIT.
    ENDIF.
  ENDMETHOD.




  METHOD validations_inv.
**********local variable data dec***********
    TYPES:BEGIN OF inv_so,
            invoice_no TYPE string,
            sord_no    TYPE string,
          END OF inv_so.
    DATA : lv_msg         TYPE string,
           lv_type        TYPE bapi_mtype,
           lv_returnorder TYPE vbeln,
           lv_delivery    TYPE vbeln,
           lv_pgi         TYPE mblnr,
           lv_invoice     TYPE vbeln,
           lv_gjahr       TYPE gjahr,
           lv_posnr       TYPE posnr,
*           lv_matnr       TYPE matnr,
           lv_charg       TYPE charg_d,
           lv_check       TYPE char1,
           lv_qty         TYPE fkimg,       "return qty
           lv_inv_qty     TYPE fkimg.       "invoice qty

    DATA : ls_input TYPE zsd_st_return_input_shn.
    CLEAR : lv_msg,
            lv_type,
            lv_returnorder,
            lv_delivery,
            lv_pgi,
            lv_invoice,
            lv_posnr,
*            lv_matnr,
            lv_charg,
            lv_check,
            lv_qty,
            lv_inv_qty.
    IF gs_input-dms_orderid IS INITIAL.
      message = | DMS order id is missing |.
      EXIT.
    ENDIF.

**********fetch the invoice details***********
    SELECT SINGLE ebeln FROM zdms_po_return
                INTO @DATA(lt_po_hd)
                WHERE dms_ordid   = @gs_input-dms_orderid
*                AND   vbeln       = @gs_input-invoice_no
*                AND   distributor = @gs_input-distributor
                AND   status      = 16
                AND   type        = 'S'.
    IF sy-subrc NE 0.
      message = | Po Return is not done for the DMS Order ID { gs_input-dms_orderid }; { lv_msg }|.
      DATA(err_1) = VALUE ty_msg_res( message = message type = 'E'  ).
      APPEND err_1 TO et_response.
      EXIT.
    ENDIF.
*Multiple click on front end so avoiding duplicates
    SELECT SINGLE * FROM zsd_invoice_dms
      INTO @DATA(l_dupl_ordid)
      WHERE invoice_no = @gs_input-dms_orderid.
    IF sy-subrc = 0.
      message = | Already sales return order In Processing { gs_input-dms_orderid }; { lv_msg }|.
      DATA(err_2) = VALUE ty_msg_res( message = message type = 'E'  ).
      APPEND err_2 TO et_response.
      DELETE FROM zsd_invoice_dms WHERE invoice_no = gs_input-dms_orderid.
      EXIT.
    ELSE.
      DATA(l_orderid_chk) = VALUE zsd_invoice_dms( mandt      = sy-mandt
                                                   invoice_no = gs_input-dms_orderid ).
      INSERT zsd_invoice_dms FROM l_orderid_chk.
    ENDIF.
*************check the duplicate sales return*********
    SELECT * FROM zsd_dms_spl_sor INTO TABLE @DATA(lt_header)  WHERE dms_ordid = @gs_input-dms_orderid.
    IF sy-subrc = 0 AND lt_header IS NOT INITIAL .
      LOOP AT lt_header INTO DATA(ls_itms) .
        CASE ls_itms-so_type.
          WHEN 'E'.                                  "Reprocess the Error document again in error type
            lv_returnorder = ls_itms-return_orderno.
            lv_delivery    = ls_itms-so_delivery.
            lv_pgi         = ls_itms-so_pgi_no.
            lv_invoice     = ls_itms-so_invno.
            lv_gjahr       = ls_itms-gjahr.
            CALL METHOD overall_process
              EXPORTING
                input          = ls_input
                gt_items       = ls_itms
                lv_returnorder = lv_returnorder
                lv_delivery    = lv_delivery
                lv_pgi         = lv_pgi
                lv_invoice     = lv_invoice
                year           = lv_gjahr
              IMPORTING
                message        = message
                type           = type
                invoice_no     = lv_invoice
                sord_no        = lv_returnorder
              .

    DATA(err_resp) = VALUE ty_msg_res( message = message type = type invoice_no = lv_invoice sord_no = lv_returnorder ).
    APPEND err_resp TO et_response.
    IF lv_returnorder IS NOT INITIAL AND lv_invoice IS NOT INITIAL AND type = 'S'.         " when reprocessed document is success, then delete the line of which has Error before.
      DELETE FROM zsd_dms_spl_sor WHERE dms_ordid = ls_itms-dms_ordid AND return_orderno = ls_itms-return_orderno
                                                                AND material = ls_itms-material AND so_type = ls_itms-so_type.
    ENDIF.
          WHEN 'S'.      " Already Invoice is created for order id
            IF ls_itms-so_stat = 17.

              DATA(inv_so) = REDUCE inv_so( INIT Ls_INV TYPE inv_so
                                            lv_sep = ' '
                                    FOR ls_data IN lt_header WHERE ( so_type = 'S' )
                                    NEXT Ls_INV-invoice_no = Ls_INV-invoice_no && lv_sep &&  ls_data-vbeln
                                         ls_inv-sord_no = ls_inv-sord_no && lv_sep && ls_data-return_orderno
                                    lv_sep = ','  ).
              DATA(gs_response) = VALUE ty_msg_res( message = | Already sales return order completed | type = 'S' invoice_no = inv_so-invoice_no
                                                                 sord_no = inv_so-sord_no ).

              APPEND gs_response TO et_response.
              EXIT.
            ENDIF.
        ENDCASE.
      ENDLOOP.

      IF et_response IS NOT INITIAL.
        ls_distributor = ls_itms-distributor.
        EXIT.
      ENDIF.

    ENDIF.

    IF lv_returnorder IS INITIAL.
*************Distributor maintanence table check************
*      SELECT SINGLE * FROM zsd_re_statu_dms INTO @DATA(lt_check) WHERE distributor = @gs_input-distributor.
*      IF sy-subrc NE 0.
*        message = |sales return config for distributor missing|.
*        EXIT.
*      ENDIF.
**********fetch the invoice details***********
      SELECT FROM zdms_po_return AS a
    INNER JOIN zdms_po_ret_item AS c
     ON a~dms_ordid = c~dms_orderid
    FIELDS c~ref_invno AS vbeln,
           c~posnr,
           a~ebeln,
          a~distributor,
           a~mblnr,
           a~belnr,
           a~she_plant,
           c~material,
           CASE WHEN c~org_batch IS NOT INITIAL THEN c~org_batch ELSE c~batch END AS batch,
*           COALESCE( c~org_batch, c~batch, ' ' )  as batch,
           c~qty,
           c~uom
*           DENSE_RANK( ) OVER( PARTITION BY c~material ORDER BY batch ASCENDING  ) as batch_count
           WHERE a~dms_ordid EQ @gs_input-dms_orderid "AND
*                 a~distributor EQ @gs_input-distributor
                 ORDER BY ref_invno "batch
                 INTO TABLE @Gt_vbrp .

      IF sy-subrc NE 0.
        message = | Invoice is not created - { gs_input-invoice_no } |.
        EXIT.
      ENDIF.
*       Sort Gt_vbrp by batch.
*      DATA : ls_vbrp LIKE LINE OF gt_vbrp.
**************Call the common validation***********
      CALL METHOD common_validations
        EXPORTING
          gt_vbrp = gt_vbrp                 " Return PO HDR and ITM Structure
        IMPORTING
          lv_msg  = message.
      IF message IS NOT INITIAL.
        DATA(msg) = VALUE ty_msg_res( message = message type = 'E' ).
        APPEND msg TO et_response.
        EXIT.
      ENDIF.

*********fill the datas for bapi**********
      IF message IS INITIAL.
        ls_input-dms_orderid    = gs_input-dms_orderid.
        ls_input-distributor = gs_input-distributor.
*        ls_input-invoice_no   = gs_input-invoice_no.
        MOVE-CORRESPONDING gs_input TO ls_input.
      ELSE.
        type = 'E'.
        EXIT.
      ENDIF.
    ENDIF.
***********call method for sales order to e-invoice***********
    CALL METHOD overall_process
      EXPORTING
        input          = ls_input
        gt_vbrp        = gt_vbrp
        lv_returnorder = lv_returnorder
        lv_delivery    = lv_delivery
        lv_pgi         = lv_pgi
        lv_invoice     = lv_invoice
        year           = lv_gjahr
      IMPORTING
        et_response    = et_response.
    ls_distributor = VALUE #( gt_vbrp[ 1 ]-distributor OPTIONAL ).
  ENDMETHOD.
ENDCLASS.
