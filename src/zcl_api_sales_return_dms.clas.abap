class ZCL_API_SALES_RETURN_DMS definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .

  data GS_INPUT type ZSD_ST_RETURN_INPUT_DMS .

  methods RETURN_ORDER
    importing
      value(LS_INPUT) type ZSD_ST_RETURN_ORDER_DMS
    exporting
      !RETURN_ORDERNO type VBELN
      !MESSAGE type STRING
      !TYPE type BAPI_MTYPE .
  methods DELIVERY
    importing
      !RETURN_ORDERNO type VBELN
      !DMS_ORDERID type ZORDER_ID optional
    exporting
      !DELIVERY_NO type VBELN_VL
      !MESSAGE type STRING
      !TYPE type BAPI_MTYPE .
  methods PGI
    importing
      !DELIVERY_NO type VBELN_VL
      !DMS_ORDERID type ZORDER_ID optional
    exporting
      !MESSAGE type STRING
      !TYPE type BAPI_MTYPE
      !PGI_NO type MBLNR .
  methods INVOICE
    importing
      !DELIVERY_NO type VBELN_VL
      !DMS_ORDERID type ZORDER_ID optional
    exporting
      !COMPANY_CODE type BUKRS
      !INVOICE_NO type VBELN_VF
      !MESSAGE type STRING
      !TYPE type BAPI_MTYPE
      !YEAR type GJAHR
      !ACCOUNTING_NO type MBLNR .
  methods VALIDATIONS_INV
    exporting
      !MESSAGE type STRING
      !TYPE type BAPI_MTYPE
      !INVOICE_NO type VBELN_VF
      !SORD_NO type VBELN
      !IRN type J_1IG_IRN
      !SIGNED_QRCODE type J_1IG_SIGN_QRCODE
      !ACK_NO type J_1IG_ACK_NO
      !ACK_DATE type J_1IG_ACK_DATE .
  methods VALIDATIONS_WOUT_INV
    exporting
      !MESSAGE type STRING
      !TYPE type BAPI_MTYPE
      !INVOICE_NO type VBELN_VF
      !SORD_NO type VBELN
      !IRN type J_1IG_IRN
      !SIGNED_QRCODE type J_1IG_SIGN_QRCODE
      !ACK_NO type J_1IG_ACK_NO
      !ACK_DATE type J_1IG_ACK_DATE .
  methods OVERALL_PROCESS
    importing
      value(INPUT) type ZSD_ST_RETURN_ORDER_DMS
      value(LV_PGI) type MBLNR
      value(LV_INVOICE) type VBELN_VF
      value(LV_RETURNORDER) type VBELN
      value(LV_DELIVERY) type VBELN_VL
      value(YEAR) type GJAHR
    exporting
      !SIGNED_QRCODE type J_1IG_SIGN_QRCODE
      !IRN type J_1IG_IRN
      !MESSAGE type STRING
      !TYPE type BAPI_MTYPE
      !INVOICE_NO type VBELN_VF
      !SORD_NO type VBELN
      !ACK_NO type J_1IG_ACK_NO
      !ACK_DATE type J_1IG_ACK_DATE .
  methods COMMON_VALIDATIONS
    exporting
      !LV_MSG type STRING .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_API_SALES_RETURN_DMS IMPLEMENTATION.


  METHOD common_validations.

    IF gs_input-inv_date IS INITIAL.
      lv_msg = |Invoice Date Missing- { gs_input-orderid }|.
      EXIT.
    ENDIF.
***********************distributor check***************
    SELECT SINGLE kunnr,werks,regio,stcd3 FROM kna1 INTO @DATA(ls_dist) WHERE kunnr = @gs_input-distributor.
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
***********check plant & Valuation area********
    SELECT SINGLE bwkey FROM t134h INTO @DATA(ls_area) WHERE bwkey = @ls_dist-werks
                                                       AND   gsber = @ls_dist-werks.
    IF sy-subrc NE 0.
      lv_msg = |Plant - { ls_dist-werks } & Valuation area Mismatch , { lv_msg }|.
      EXIT.
    ENDIF.
***********************dealer check***************
    SELECT SINGLE kunnr,regio,stcd3 FROM kna1 INTO @DATA(ls_cust) WHERE kunnr = @gs_input-dealer.
    IF sy-subrc NE 0.
      lv_msg = | Dealer is not found - { gs_input-dealer }|.
      EXIT.
    ENDIF.
**************check the GST active or not****************
    IF ls_cust-stcd3 IS NOT INITIAL.

      IF ls_cust-stcd3(1) = space.
        lv_msg = |{ lv_msg } ; Dealer { gs_input-dealer } GST is incorrect|.
      ENDIF.
      CLEAR : gs_gst.
      SELECT SINGLE region,gst_key FROM zsd_gst_reg INTO @gs_gst WHERE region = @ls_cust-regio.
      lv_gstkey = CONV char02( gs_gst-gst_key ).
      IF ls_cust-stcd3(2) <> lv_gstkey.
        lv_msg = |{ lv_msg } ; Dealer { gs_input-dealer } GST and Region not matching|.
      ENDIF.

      SELECT SINGLE * FROM zdist_einv_dtls INTO @DATA(ls_einv)
                                           WHERE distributor = @gs_input-distributor.
      IF sy-subrc = 0.
        SELECT SINGLE * FROM zcust_gst_chk INTO @DATA(ls_gst)
                                           WHERE kunnr  = @gs_input-dealer.
        IF sy-subrc EQ 0.
          CONDENSE : ls_gst-status.
          IF ls_gst-status NE 'ACT'.
            IF ls_gst-status = 'CNL'.
              lv_msg = |Customer - { gs_input-dealer } GST No - { ls_cust-stcd3 } is Cancelled, { lv_msg }|.
            ELSEIF ls_gst-status = 'SUS'.
*                lv_msg = |Customer - { gs_input-dealer } GST No - { ls_cust-stcd3 } is Suspended, { lv_msg }|.
            ELSE.
              lv_msg = |Customer - { gs_input-dealer } GST No - { ls_cust-stcd3 } is invalid, { lv_msg }|.
            ENDIF.
          ENDIF.
        ELSE.
          lv_msg = |Customer - { gs_input-dealer } GST Check not Maintained, { lv_msg }|.
        ENDIF.
      ENDIF.
    ENDIF.

****************************customer block check*****************
    SELECT SINGLE * FROM zcust_blk_chk INTO @DATA(ls_blk)
                                       WHERE bukrs = 'DMS1'
                                       AND   vkorg = 'SDMS'
                                       AND   kunnr = @gs_input-dealer
                                       AND   block = @abap_true.
    IF sy-subrc = 0.
      lv_msg = |Customer - { gs_input-dealer } is blocked, { lv_msg }|.
    ENDIF.
*****************************Block the process for distributor or customer*****************
    DATA : lobj_check TYPE REF TO zcl_common_check.
    CREATE OBJECT lobj_check.
********************for distributor checking****************
    lobj_check->dms_process_stop(
      EXPORTING
        kunnr        = gs_input-distributor
        process_type = 'SR'
      IMPORTING
        message      = lv_msg
        type         = DATA(lv_type) ).
    IF lv_type = 'E'.
      EXIT.
    ENDIF.

********************for customer checking****************
    CLEAR : lv_type.
    lobj_check->dms_process_stop(
      EXPORTING
        kunnr        = gs_input-dealer
        process_type = 'SR'
      IMPORTING
        message      = lv_msg
        type         = lv_type ).
    IF lv_type = 'E'.
      EXIT.
    ENDIF.

****************get the marc details************
    SELECT a~matnr,a~zterm,a~tragr,a~bismt,a~mtart,b~werks,b~ladgr,b~steuc,b~prctr
                                  FROM mara AS a
                                  INNER JOIN marc AS b ON a~matnr = b~matnr
                                  INTO TABLE @DATA(lt_marc)
                                  WHERE b~werks = @ls_dist-werks.
    IF sy-subrc = 0.
      SORT lt_marc BY matnr werks.
    ENDIF.
*********************plant region check****************
    SELECT SINGLE werks,regio FROM t001w INTO @DATA(t001w) WHERE werks = @ls_dist-werks
                                                           AND   regio = @ls_dist-regio.
    IF sy-subrc NE 0.
      lv_msg = | Distributor & Plant Region is mismatch - { gs_input-invoice_no }|.
      EXIT.
    ENDIF.
*************fetch the gst condition*******
    SELECT a~kschl,a~wkreg,a~regio,a~matnr,a~knumh,b~loevm_ko FROM a709 AS a
                                                              INNER JOIN konp AS b ON a~knumh EQ b~knumh
                                                              INTO TABLE @DATA(lt_cond)
                                                              FOR ALL ENTRIES IN @lt_marc
                                                              WHERE wkreg = @ls_dist-regio
                                                              AND   regio = @ls_cust-regio
                                                              AND   matnr = @lt_marc-matnr
                                                              AND   datbi GE @sy-datum.
*    IF ls_cust-regio = 'KL'.
******************fetch the ZPRO price condition( only for kerala )************
*      SELECT a~matnr,b~loevm_ko FROM a832 AS a
*                                INNER JOIN konp AS b ON a~knumh = b~knumh
*                                INTO TABLE @DATA(lt_zpr0)
*                                FOR ALL ENTRIES IN @gs_input-details
*                                WHERE a~kschl    = 'ZPR0'
*                                AND   a~regio    = @ls_cust-regio
*                                AND   a~matnr    = @gs_input-details-material
*                                AND   a~datbi    GE @sy-datum
*                                AND   b~loevm_ko = @abap_false.
*      IF sy-subrc = 0.
*        SORT : lt_zpr0 BY matnr.
*      ENDIF.
*    ELSE.
*****************fetch the ZPRO price condition others************
    SELECT a~matnr,b~loevm_ko FROM a304 AS a
                              INNER JOIN konp AS b ON a~knumh = b~knumh
                              INTO TABLE @DATA(lt_zpr0)
                              FOR ALL ENTRIES IN @lt_marc
                              WHERE a~kschl = 'ZPR0'
                              AND   a~vkorg = 'SDMS'
                              AND   a~matnr = @lt_marc-matnr
                              AND   a~datbi GE @sy-datum
                              AND   b~loevm_ko = @abap_false.
    IF sy-subrc = 0.
      SORT : lt_zpr0 BY matnr.
    ENDIF.
*    ENDIF.
****************get the mvke details************
    SELECT matnr,mvgr1,vkorg,vtweg  FROM mvke
                                    INTO TABLE @DATA(lt_mvke)
                                    FOR ALL ENTRIES IN @lt_marc
                                    WHERE matnr = @lt_marc-matnr.
    IF sy-subrc = 0.
      SORT lt_mvke BY matnr vkorg vtweg.
    ENDIF.
***************fetch the batch details***********
    SELECT matnr,charg FROM mch1 INTO TABLE @DATA(lt_mch1) FOR ALL ENTRIES IN @lt_marc
                                                           WHERE matnr = @lt_marc-matnr.
    IF sy-subrc = 0.
      SORT lt_mch1 BY matnr charg.
    ENDIF.
***********************check the internal price************
    SELECT matnr,bwkey,stprs FROM mbew INTO TABLE @DATA(lt_mbew) WHERE bwkey = @ls_dist-werks.
    IF sy-subrc = 0.
      SORT : lt_mbew BY matnr.
    ENDIF.
***********Looping process**************
    LOOP AT gs_input-details ASSIGNING FIELD-SYMBOL(<fs_details>).

*      AT NEW material.
***********************Old material code conversion************
      CONDENSE <fs_details>-material.
      IF <fs_details>-material(1) NE 'Z'.
***********Transportation group check*************
        READ TABLE lt_marc INTO DATA(ls_old) WITH KEY matnr = <fs_details>-material BINARY SEARCH.
        IF ls_old-mtart NE 'HAWA'.
          IF ls_old-bismt IS INITIAL.
            lv_msg = | New material is not maintained in old material code - { <fs_details>-material }, { lv_msg } |.
            CONTINUE.
          ELSE.
            CONDENSE ls_old-bismt.
            <fs_details>-material = ls_old-bismt.
          ENDIF.
        ENDIF.
      ENDIF.
***********Transportation group check*************
      READ TABLE lt_marc INTO DATA(ls_marc) WITH KEY matnr = <fs_details>-material BINARY SEARCH.
      IF ls_marc-tragr IS INITIAL.
        lv_msg = |Transportation Group Missing { <fs_details>-material } , { lv_msg }|.
      ELSE.
        IF ls_marc-tragr <> '0001'.
          lv_msg = |Transportation Group Incorrect{ <fs_details>-material }  , { lv_msg }|.
        ENDIF.
      ENDIF.
      CLEAR : ls_marc.
*********plant wise check****************
      READ TABLE lt_marc INTO ls_marc WITH KEY matnr = <fs_details>-material
                                               werks = ls_dist-werks BINARY SEARCH.
      IF sy-subrc <> 0.
        lv_msg = |Maretial { <fs_details>-material } Not Maintained in Plant { ls_dist-werks } , { lv_msg }|.
      ELSE.
*********************Loading group check*************
        IF ls_marc-ladgr IS INITIAL.
          lv_msg = |Loading Group Missing { <fs_details>-material } , { lv_msg }|.
        ELSE.
          IF ls_marc-ladgr <> '0004'.
            lv_msg = |Loading Group Incorrect { <fs_details>-material } , { lv_msg }|.
          ENDIF.
        ENDIF.
*********************HSN & Profit Center Missing Check*****************************
        IF ls_marc-steuc IS INITIAL.
          lv_msg = |{ lv_msg } ; HSN Missing for Material { <fs_details>-material }|.
        ENDIF.
        IF ls_marc-prctr IS INITIAL.
          lv_msg = |{ lv_msg } ; Profit Center Missing for Material { <fs_details>-material }|.
        ENDIF.
      ENDIF.
************************material group1 check***********************************
      READ TABLE lt_mvke INTO DATA(ls_mvke) WITH KEY matnr = <fs_details>-material
                                               vkorg = 'SDMS'
                                               vtweg = '20' BINARY SEARCH.
      IF sy-subrc <> 0.
        lv_msg = |Maretial { <fs_details>-material } Not Maintained in Sales org SDMS & Dis.Channel 20 , { lv_msg }|.
      ELSE.
        IF ls_mvke-mvgr1 IS INITIAL.
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
**********************ZPR0 checking*************
      READ TABLE lt_zpr0 TRANSPORTING NO FIELDS WITH KEY matnr = <fs_details>-material BINARY SEARCH.
      IF sy-subrc NE 0.
        lv_msg = |Material ZPR0 is not maintained { <fs_details>-material } , { lv_msg }|.
      ENDIF.
*      ENDAT.
***********check batch is present or not**************
      READ TABLE lt_mch1 TRANSPORTING NO FIELDS WITH KEY matnr = <fs_details>-material
                                                         charg = <fs_details>-batch BINARY SEARCH.
      IF sy-subrc NE 0.
************check batch is present or not**************
        READ TABLE lt_mch1 TRANSPORTING NO FIELDS WITH KEY matnr = ls_old-matnr
                                                           charg = <fs_details>-batch BINARY SEARCH.
        IF sy-subrc <> 0.
          lv_msg = | Given batch - { <fs_details>-batch }  is not found in material - { <fs_details>-material } , { lv_msg }|.
        ENDIF.
      ENDIF.
***********check material qty and uom missing************
      IF <fs_details>-qty IS INITIAL.
        lv_msg = |Quantity is missing - { <fs_details>-material } , { lv_msg } |.
      ENDIF.
      IF <fs_details>-uom IS INITIAL.
        lv_msg = |UOM is missing - { <fs_details>-material } , { lv_msg } |.
      ENDIF.

***********************check the internal price************
      READ TABLE lt_mbew INTO DATA(ls_mbew) WITH KEY matnr = <fs_details>-material BINARY SEARCH.
      IF ls_mbew-stprs IS INITIAL.
        lv_msg = |Material internal price - VPRS is not maintained { <fs_details>-material } , { lv_msg }|.
      ENDIF.

      CLEAR : ls_marc,ls_mvke,ls_mbew,ls_old.
    ENDLOOP.

  ENDMETHOD.


  METHOD delivery.

    DATA: lt_return    TYPE TABLE OF bapiret2,
          lt_salesitem TYPE TABLE OF bapidlvreftosalesorder.

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
        UPDATE zsd_retrn_hd_dms SET   msg = message status = type sostat = '12' delivery_no = delivery_no
                                WHERE dms_orderid = dms_orderid.
        IF sy-subrc = 0.
          COMMIT WORK.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD if_http_extension~handle_request.
    "Created by: Pandiarajan
    "Created on: 18.12.2023
    "Reference by: Ramakrishnan J
    "Purpose : Create Sales Return for DMS ( sub dealer to distributor )
*-------------------------------------------------------------*
    TYPES: BEGIN OF ty_msg,
             orderid        TYPE zorder_id,       "dms ord id
             distributor    TYPE kunnr,           "distributor code
             dealer         TYPE kunnr,           "dealer code
             invoice_no     TYPE vbeln,           "ref invoice no
             return_orderno TYPE vbeln,           "return sale order no
             return_invno   TYPE vbeln,           "return invoice no
             irn            TYPE j_1ig_irn,
             signed_qrcode  TYPE j_1ig_sign_inv,
             ack_no         TYPE j_1ig_ack_no,
             ack_date       TYPE j_1ig_ack_date,
             message        TYPE string,
             type           TYPE bapi_mtype,
             form           TYPE fpcontent,
           END OF ty_msg.
    DATA : ls_response TYPE ty_msg,
           lv_batch    TYPE char1,
           lv_message  TYPE string,
           lv_type     TYPE bapi_mtype,
           lv_vbeln    TYPE vbeln.

    DATA: lv_body TYPE string.
    DATA: lv_data TYPE string.
************API log**********
    DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
    CREATE OBJECT lo_log_upd.
************dms error log**********
    DATA: lo_errorlog_dms TYPE REF TO zcl_api_dms_error_log_entries.
    CREATE OBJECT lo_errorlog_dms.
    DATA : lo_main1 TYPE REF TO zcl_dms_einvoice_process.
    CREATE OBJECT lo_main1.

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
    gs_input-dealer      = |{ gs_input-dealer ALPHA = IN }|.
****************invoice no alpha conversion***********
    TRANSLATE gs_input-invoice_no TO UPPER CASE.
    CONDENSE gs_input-invoice_no.
    gs_input-invoice_no  = |{ gs_input-invoice_no ALPHA = IN }|.
**********fetch the invoice details***********
    SELECT SINGLE vbeln FROM vbrk
                   INTO @DATA(lt_vbrp)
                   WHERE vbeln EQ @gs_input-invoice_no.
*                   AND   fkdat EQ @gs_input-inv_date.
    IF sy-subrc EQ 0.
      CALL METHOD validations_inv(
        IMPORTING
          message       = lv_message
          type          = lv_type
          invoice_no    = ls_response-return_invno
          sord_no       = ls_response-return_orderno
          irn           = ls_response-irn
          signed_qrcode = ls_response-signed_qrcode
          ack_no        = ls_response-ack_no
          ack_date      = ls_response-ack_date ).
    ELSE.
      CALL METHOD validations_wout_inv(
        IMPORTING
          message       = lv_message
          type          = lv_type
          invoice_no    = ls_response-return_invno
          sord_no       = ls_response-return_orderno
          irn           = ls_response-irn
          signed_qrcode = ls_response-signed_qrcode
          ack_no        = ls_response-ack_no
          ack_date      = ls_response-ack_date ).
    ENDIF.
***fill the response********
    IF lv_type IS INITIAL.
      lv_type = 'E'.
    ENDIF.
    ls_response-dealer       = gs_input-dealer.
    ls_response-orderid      = gs_input-orderid.
    ls_response-distributor  = gs_input-distributor.
    ls_response-invoice_no   = gs_input-invoice_no.
    ls_response-message      = lv_message.
    ls_response-type         = lv_type.
**********************get the pdf***************
    IF ls_response-type = 'S' AND ls_response-return_invno IS NOT INITIAL.
      CALL METHOD lo_main1->generate_pdf_dms
        EXPORTING
          inv_no   = ls_response-return_invno
          customer = ls_response-dealer
        IMPORTING
          pdf      = ls_response-form.
*          return   = return.
    ENDIF.
    IF ls_response IS NOT INITIAL.
*********************error log***********
      IF ls_response-type = 'E'.
        SELECT SINGLE kunnr,werks FROM kna1 INTO @DATA(ls_kna1) WHERE kunnr = @gs_input-distributor.
        lo_errorlog_dms->log_entry_store(
          EXPORTING
            type                = 18
            status              = 10
            dms_orderid         = ls_response-orderid
            distributor         = ls_response-distributor
            plant               = ls_kna1-werks
            dealer              = ls_response-dealer
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
          apiname         = 'DMS_RETURN_ORDER'
          ijson           = lv_data
          ojson           = lv_body
          distributor     = gs_input-distributor
          retailer        = gs_input-dealer
        EXCEPTIONS
          apiname_missing = 1
          json_missing    = 2
          OTHERS          = 3.
*Set JSON Content-Type
      CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
      CALL METHOD server->response->set_cdata( data = lv_body ).
      DELETE FROM zsd_invoice_dms WHERE invoice_no = gs_input-orderid.
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
          xkomfkgn  TYPE STANDARD TABLE OF komfkgn.

*******************debit note creation data dec*********
    DATA: lv_msg_text TYPE string.
    DATA: lv_objtyp TYPE bapiache09-obj_type.
    DATA: lv_objkey TYPE bapiache09-obj_key.
    DATA: lv_objsys TYPE bapiache09-obj_sys.
    DATA: lt_glaccount  TYPE TABLE OF bapiacgl09,
          lt_payable    TYPE TABLE OF bapiacap09,
          lt_recievable TYPE TABLE OF bapiacar09,
          lt_curramnt   TYPE TABLE OF bapiaccr09,
          lt_return     TYPE TABLE OF bapiret2,
          inv_amnt      TYPE netwr.
*          lv_plant      TYPE werks_d.

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

      vbsk_i-smart   = 'F'.
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
                      gjahr
                      netwr FROM vbrk
*                              INNER JOIN vbrp AS b ON a~vbeln = b~vbeln
                              INTO ( accounting_no, company_code, year , inv_amnt )
                              WHERE vbeln = invoice_no.
        IF sy-subrc EQ 0.

*          message = | Invoice is created - { invoice_no } - { year } |.
*************************check the autocd created for original inv*************
*          SELECT SINGLE invno,dgper,gsber,hkont,kunnr FROM zdms_crnote_log
*                                    INTO @DATA(ls_cdchk)
*                                    WHERE invno = @gs_input-invoice_no.
*          IF sy-subrc <> 0.
*            ls_cdchk-dgper = '2.5'.
*            ls_cdchk-hkont = '0042002012'.
*          ENDIF.
*
*          CONDENSE : ls_cdchk-dgper.
*
*          IF ls_cdchk-dgper = '2.5' OR ls_cdchk-dgper = '1.5'.
*
*****************header data filling******************
*            DATA(l_headers) = VALUE bapiache09( bus_act    = 'RFBU'
*                                                username   = sy-uname
*                                                comp_code  = company_code
*                                                doc_date   = sy-datum
*                                                pstng_date = sy-datum
*                                                ref_doc_no = invoice_no
*                                                fisc_year  = year
*                                                doc_type   = 'DR' ).
*
*            REFRESH: lt_glaccount,lt_curramnt,lt_return,lt_recievable.
*
*            lt_recievable = VALUE #( ( itemno_acc = '1'
*                                       customer   = gs_input-dealer
*                                       item_text  = |Sales return DR - { gs_input-invoice_no }|
*                                       bus_area   = lv_plant
*                                       alloc_nmbr = gs_input-invoice_no ) ).
*
*            lt_glaccount = VALUE #( ( itemno_acc = '2'
*                                      gl_account = ls_cdchk-hkont
*                                      item_text  = |Sales return DR - { gs_input-invoice_no }|
*                                      bus_area   = lv_plant
*                                      alloc_nmbr = gs_input-invoice_no ) ).
*
*            lt_curramnt = VALUE #( ( itemno_acc = '1'
*                                     currency   = 'INR'
*                                     amt_doccur = ( inv_amnt * ls_cdchk-dgper ) / 100 )
*                                   ( itemno_acc = '2'
*                                     currency   = 'INR'
*                                     amt_doccur = ( ( inv_amnt * ls_cdchk-dgper ) / 100 ) * -1 ) ).
*
*            CLEAR : lv_objkey,lv_objsys,lv_objtyp,ls_cdchk.
*
*            CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
*              EXPORTING
*                documentheader    = l_headers
*              IMPORTING
*                obj_type          = lv_objtyp
*                obj_key           = lv_objkey
*                obj_sys           = lv_objsys
*              TABLES
*                accountgl         = lt_glaccount
*                accountreceivable = lt_recievable
*                accountpayable    = lt_payable
*                currencyamount    = lt_curramnt
*                return            = lt_return.
*
*            READ TABLE lt_return TRANSPORTING NO FIELDS WITH KEY type = 'S'.
*            IF sy-subrc EQ 0.
*              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*                EXPORTING
*                  wait = 'X'.
*
*              message = | Invoice is created - { invoice_no } - { year } |.
*              type    = 'S'.
*            ELSE.
*              CLEAR : lv_msg_text,lv_objkey.
*              LOOP AT lt_return INTO DATA(l_ret) WHERE ( type = 'E' OR type = 'A' ).
*                message = |{ message },{ l_ret-message }|.
*              ENDLOOP.
*              type    = 'E'.
*            ENDIF.
*
*          ELSE.
            message = | Invoice is created - { invoice_no } - { year } |.
            type    = 'S'.

        ELSE.
          message = | Invoice is not created|.
          type    = 'E'.
        ENDIF.

      ELSE.
        message = | Invoice is not created|.
        type    = 'E'.
      ENDIF.
      IF dms_orderid IS NOT INITIAL.
        UPDATE zsd_retrn_hd_dms SET   msg = message status = type sostat = '14' gjahr = year
                                      invoice_no = invoice_no accounting_doc = accounting_no
                                WHERE dms_orderid = dms_orderid.
        IF sy-subrc = 0.
          COMMIT WORK.
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
    CREATE OBJECT lo_main1.
    DATA : lv_bukrs    TYPE bukrs,
           lv_response TYPE string,
           signed_inv  TYPE j_1ig_sign_inv,
           pdf         TYPE fpcontent.
    lv_bukrs = 'DMS1'.
*****************create return order************
    IF input IS NOT INITIAL AND lv_returnorder IS INITIAL.
      CALL METHOD return_order(
        EXPORTING
          ls_input       = input
        IMPORTING
          return_orderno = lv_returnorder
          message        = message
          type           = type
      ).
    ENDIF.
*****************create delivery************
    IF lv_returnorder IS NOT INITIAL AND lv_delivery IS INITIAL.
      CALL METHOD delivery(
        EXPORTING
          return_orderno = lv_returnorder
          dms_orderid    = gs_input-orderid
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
          dms_orderid = gs_input-orderid
        IMPORTING
          message     = message
          type        = type
          pgi_no      = lv_pgi
      ).
    ENDIF.
*****************create invoice************
    IF lv_delivery IS NOT INITIAL  AND lv_pgi IS NOT INITIAL
      AND lv_invoice IS INITIAL.
      CALL METHOD invoice(
        EXPORTING
          delivery_no = lv_delivery
          dms_orderid = gs_input-orderid
        IMPORTING
          invoice_no  = lv_invoice
          message     = message
          type        = type
          year        = year
      ).
    ENDIF.
***************e invoice generation***********
    IF lv_invoice IS NOT INITIAL.
      invoice_no = lv_invoice.
      SELECT SINGLE vbeln,fkdat,gjahr FROM vbrk INTO @DATA(ls_vbrk) WHERE vbeln = @invoice_no.
*************check e-invoice process*************
      SELECT SINGLE distributor FROM zdist_einv_dtls INTO @DATA(ls_check)
                                                     WHERE distributor = @gs_input-distributor
                                                     AND   act_date LE @ls_vbrk-fkdat.
      IF sy-subrc = 0.
        SELECT SINGLE kunnr,stcd3 FROM kna1 INTO @DATA(ls_kna1) WHERE kunnr = @gs_input-dealer.
        IF ls_kna1-stcd3 IS NOT INITIAL.
*********call the irn generated method*************
          CALL METHOD lo_main1->create_irn_qrcode
            EXPORTING
              distributor_code = gs_input-distributor
              vbeln            = lv_invoice
              bukrs            = lv_bukrs
              gjahr            = ls_vbrk-gjahr
            IMPORTING
              irn              = irn
              signed_inv       = signed_inv
              signed_qrcode    = signed_qrcode
              ack_no           = ack_no
              ack_date         = ack_date
              return           = lv_response.
          IF irn IS INITIAL.
            message = lv_response.
            type    = 'E'.
          ELSE.
            message = | IRN is generated successfully |.
            type    = 'S'.
          ENDIF.
        ELSE.
          message = | { message } , Subdealer GST code is not found |.
          type    = 'S'.
****************************add the irn table********************
          DATA  : ls_irn TYPE zdms_invoice_irn.
          CLEAR : ls_irn.
          ls_irn-bukrs  = 'DMS1'.
          ls_irn-dealer = gs_input-dealer.
          ls_irn-distrb = gs_input-distributor.
          ls_irn-docno  = lv_invoice.
          ls_irn-doc_year = ls_vbrk-gjahr.
          ls_irn-ernam  = sy-uname.
          ls_irn-erdat  = sy-datum.
          ls_irn-erzet  = sy-uzeit.
          MODIFY zdms_invoice_irn FROM ls_irn.
          IF sy-subrc = 0.
            COMMIT WORK.
          ENDIF.
        ENDIF.
      ELSE.
****************************add the irn table********************
        CLEAR : ls_irn.
        ls_irn-bukrs  = 'DMS1'.
        ls_irn-dealer = gs_input-dealer.
        ls_irn-distrb = gs_input-distributor.
        ls_irn-docno  = lv_invoice.
        ls_irn-doc_year = ls_vbrk-gjahr.
        ls_irn-ernam  = sy-uname.
        ls_irn-erdat  = sy-datum.
        ls_irn-erzet  = sy-uzeit.
        MODIFY zdms_invoice_irn FROM ls_irn.
        IF sy-subrc = 0.
          COMMIT WORK.
        ENDIF.
      ENDIF.
**************update the log table only for invoice process**************
      IF message IS NOT INITIAL.
        UPDATE zsd_retrn_hd_dms SET   msg = message  status = type sostat = '17'
                                      irn = irn      signed_inv = signed_inv
                                      pdf = pdf      signed_qrcode = signed_qrcode
                                WHERE dms_orderid = gs_input-orderid.
        IF sy-subrc = 0.
          COMMIT WORK.
        ENDIF.
      ENDIF.

    ENDIF.
    sord_no = lv_returnorder.

  ENDMETHOD.


  METHOD pgi.

    DATA : ls_vbkok TYPE vbkok,
           lt_prott TYPE TABLE OF prott,
           lv_msg   TYPE string.

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
      ELSE.
        message = |Pgi not created |.
        type    = 'E'.
      ENDIF.
      IF dms_orderid IS NOT INITIAL.
        UPDATE zsd_retrn_hd_dms SET   msg = message status = type sostat = '13' pgi_no = pgi_no
                                WHERE dms_orderid = dms_orderid.
        IF sy-subrc = 0.
          COMMIT WORK.
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
           ls_loghead TYPE zsd_retrn_hd_dms,
           lt_logitem TYPE TABLE OF zsd_retrn_it_dms.

********************************************************************** Pricing Update Attributes
    DATA: logic_switch      TYPE bapisdls,
          order_header_inx  TYPE bapisdh1x,
          lt_order_item_in  TYPE TABLE OF  bapisditm,
          ls_order_item_in  TYPE  bapisditm,
          lt_order_item_inx TYPE TABLE OF bapisditmx,
          ls_order_item_inx TYPE  bapisditmx,
          gt_return         TYPE TABLE OF bapiret2.
**************Get the sales order details******************
    IF ls_input-invoiceno IS NOT INITIAL.

      SELECT SINGLE aubel,erdat FROM vbrp INTO @DATA(ls_vbrp) WHERE vbeln = @ls_input-invoiceno.

      CALL FUNCTION 'BAPI_SALESORDER_GETDETAILBOS'
        EXPORTING
          salesdocument   = ls_vbrp-aubel
        TABLES
          orderconditions = lt_oldcondition
          orderitems      = lt_olditem.

**************get the ship & sold to party details************
      SELECT SINGLE kunnr_ana,kunwe_ana FROM vbap INTO @DATA(ls_vbap) WHERE vbeln = @ls_vbrp-aubel.
      IF sy-subrc = 0.
        ls_input-soldtoparty = ls_vbap-kunnr_ana.
        ls_input-shiptoparty = ls_vbap-kunwe_ana.
      ENDIF.

    ENDIF.

*-----------------------------------------------------------------*
* Build order item data & flags
*-----------------------------------------------------------------*
    LOOP AT ls_input-item ASSIGNING FIELD-SYMBOL(<fs_details>).
*****************item data filling**************
      lv_posnr = lv_posnr + 1.
      CLEAR : ls_item,ls_itemx.
      ls_item-itm_number = lv_posnr.
      ls_item-material   = <fs_details>-material.
      ls_item-target_qty = <fs_details>-qty.
      ls_item-target_qu  = <fs_details>-uom.
      ls_item-batch      = <fs_details>-batch.
      ls_item-plant      = ls_input-plant.
      ls_item-store_loc  = ls_input-storage_loc.
********************only for invoice refernce **************
      IF ls_input-invoiceno IS NOT INITIAL.
        ls_item-ref_doc     = ls_input-invoiceno.
        ls_item-ref_doc_ca  = 'M'.
        ls_itemx-ref_doc    = abap_true.
        ls_itemx-ref_doc_ca = abap_true.
        ls_itemx-ref_doc_it = abap_true.       "TR: DEVK935586
***********pass the pricing condition**********
        CLEAR : ls_olditem.
        READ TABLE lt_olditem INTO ls_olditem WITH KEY material = <fs_details>-material.
        IF sy-subrc <> 0.
          CLEAR : ls_olditem.
          SELECT SINGLE matnr,bismt FROM mara INTO @DATA(ls_mara)
                                    WHERE bismt = @<fs_details>-material.
          READ TABLE lt_olditem INTO ls_olditem WITH KEY material = ls_mara-matnr.
          CLEAR : ls_mara.
        ENDIF.
*        ls_item-itm_number = ls_olditem-itm_number.
        ls_item-ref_doc_it = ls_olditem-itm_number.        "TR: DEVK935586
*****************************Z013***************
        CLEAR : ls_cond,ls_oldcond.
        READ TABLE lt_oldcondition INTO ls_oldcond WITH KEY itm_number = ls_olditem-itm_number
                                                            cond_type  = 'Z013'.
        IF sy-subrc = 0.
          ls_cond-itm_number = lv_posnr.
          ls_cond-cond_count = lv_posnr.
          ls_cond-cond_type  = 'Z013'.
          IF ls_oldcond-cond_value LT 0.
            ls_cond-cond_value = ls_oldcond-cond_value * -10.
          ENDIF.
          ls_cond-currency   = 'INR'.
          APPEND ls_cond TO lt_cond.
        ENDIF.
*****************************Z014***************
        CLEAR : ls_cond,ls_oldcond.
        READ TABLE lt_oldcondition INTO ls_oldcond WITH KEY itm_number = ls_olditem-itm_number
                                                            cond_type  = 'Z014'.
        IF sy-subrc = 0.
          ls_cond-itm_number = lv_posnr.
          ls_cond-cond_count = lv_posnr.
          ls_cond-cond_type  = 'Z014'.
          IF ls_oldcond-cond_value LT 0.
            ls_cond-cond_value = ls_oldcond-cond_value * -10.
          ENDIF.
          ls_cond-currency   = 'INR'.
          APPEND ls_cond TO lt_cond.
        ENDIF.
*****************************Z015***************
        CLEAR : ls_cond,ls_oldcond.
        READ TABLE lt_oldcondition INTO ls_oldcond WITH KEY itm_number = ls_olditem-itm_number
                                                            cond_type  = 'Z015'.
        IF sy-subrc = 0.
          ls_cond-itm_number = lv_posnr.
          ls_cond-cond_count = lv_posnr.
          ls_cond-cond_type  = 'Z015'.
          IF ls_oldcond-cond_value LT 0.
            ls_cond-cond_value = ls_oldcond-cond_value * -1.
            ls_cond-cond_value = ls_cond-cond_value / ls_olditem-req_qty.
            ls_cond-cond_value = <fs_details>-qty * ls_cond-cond_value.
          ENDIF.
          ls_cond-currency   = 'INR'.
          APPEND ls_cond TO lt_cond.
        ENDIF.
*****************************Z016***************
        CLEAR : ls_cond,ls_oldcond.
        READ TABLE lt_oldcondition INTO ls_oldcond WITH KEY itm_number = ls_olditem-itm_number
                                                            cond_type  = 'Z016'.
        IF sy-subrc = 0.
          ls_cond-itm_number = lv_posnr.
          ls_cond-cond_count = lv_posnr.
          ls_cond-cond_type  = 'Z016'.
          IF ls_oldcond-cond_value LT 0.
            ls_cond-cond_value = ls_oldcond-cond_value * -1.
            ls_cond-cond_value = ls_cond-cond_value / ls_olditem-req_qty.
            ls_cond-cond_value = <fs_details>-qty * ls_cond-cond_value.
          ENDIF.
          ls_cond-currency   = 'INR'.
          APPEND ls_cond TO lt_cond.
        ENDIF.
********************only for invoice refernce **************
      ELSE.
***********pass the pricing condition**********
*****************************Z013***************
        IF <fs_details>-disper1 IS NOT INITIAL.
          CLEAR : ls_cond.
          ls_cond-itm_number = lv_posnr.
          ls_cond-cond_count = lv_posnr.
          ls_cond-cond_type  = 'Z013'.
          ls_cond-cond_value = <fs_details>-disper1 * 10.
          ls_cond-currency   = 'INR'.
          APPEND ls_cond TO lt_cond.
        ENDIF.
*****************************Z014***************
        IF <fs_details>-disper2 IS NOT INITIAL.
          CLEAR : ls_cond.
          ls_cond-itm_number = lv_posnr.
          ls_cond-cond_count = lv_posnr.
          ls_cond-cond_type  = 'Z014'.
          ls_cond-cond_value = <fs_details>-disper2 * 10.
          ls_cond-currency   = 'INR'.
          APPEND ls_cond TO lt_cond.
        ENDIF.
*****************************Z015***************
        IF <fs_details>-disvalue1 IS NOT INITIAL.
          CLEAR : ls_cond.
          ls_cond-itm_number = lv_posnr.
          ls_cond-cond_count = lv_posnr.
          ls_cond-cond_type  = 'Z015'.
          ls_cond-cond_value = <fs_details>-disvalue1.
          ls_cond-currency   = 'INR'.
          APPEND ls_cond TO lt_cond.
        ENDIF.
*****************************Z016***************
        IF <fs_details>-disvalue2 IS NOT INITIAL.
          CLEAR : ls_cond.
          ls_cond-itm_number = lv_posnr.
          ls_cond-cond_count = lv_posnr.
          ls_cond-cond_type  = 'Z016'.
          ls_cond-cond_value = <fs_details>-disvalue2.
          ls_cond-currency   = 'INR'.
          APPEND ls_cond TO lt_cond.
        ENDIF.
      ENDIF.

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
                      req_qty    = <fs_details>-qty ) TO lt_schedules_in.
      APPEND VALUE #( itm_number = lv_posnr
                      sched_line = abap_true
                      req_qty    = abap_true
                      updateflag = abap_true ) TO  lt_schedules_inx.
***********fill item data for log table*******
      APPEND VALUE #( dms_orderid  = ls_input-orderid
                      posnr        = lv_posnr
                      material     = <fs_details>-material
                      batch        = <fs_details>-batch
                      qty          = <fs_details>-qty
                      uom          = <fs_details>-uom
                      erdat        = sy-datum
                      erzet        = sy-uzeit ) TO  lt_logitem.
    ENDLOOP.

**------------------------------------------------------------------
** Build order header data & flag
**------------------------------------------------------------------*
    CLEAR : ls_header,ls_headerx.
    ls_header-doc_type   = 'YRMS'.
    ls_header-ord_reason = '101'.
    ls_header-sd_doc_cat = 'H'.
    ls_header-comp_cde_b = ls_input-compcode.
    ls_header-sales_org  = ls_input-salesorg.
    ls_header-distr_chan = ls_input-distchnl.
    ls_header-division   = ls_input-division.
    ls_header-sales_off  = ls_input-plant.
    ls_header-purch_no_c = gs_input-orderid.
    ls_header-business_partner_no = ls_input-soldtoparty.
**************************invoice pricing date fetching*************
    ls_header-price_date  = ls_input-inv_date.
    ls_headerx-price_date = abap_true.

**********check for inv ref.*********
    IF ls_input-invoiceno IS NOT INITIAL.
      ls_header-ref_doc     = ls_input-invoiceno.
      ls_header-refdoc_cat  = 'M'.
      ls_headerx-refdoc_cat = abap_true.
      ls_headerx-ref_doc    = abap_true.
    ENDIF.
    ls_headerx-doc_type   = abap_true.
    ls_headerx-ord_reason = abap_true.
    ls_headerx-sd_doc_cat = abap_true.
    ls_headerx-comp_cde_b = abap_true.
    ls_headerx-sales_org  = abap_true.
    ls_headerx-sales_off  = abap_true.
    ls_headerx-distr_chan = abap_true.
    ls_headerx-division   = abap_true.
    ls_header-purch_no_c  = abap_true.
    ls_headerx-business_partner_no = abap_true.
*------------------------------------------------------------------
* Build partner information
*------------------------------------------------------------------

    lt_partners = VALUE #( ( partn_role = 'AG'
                             partn_numb = ls_input-soldtoparty )
                           ( partn_role = 'WE'
                             partn_numb =  ls_input-shiptoparty ) ).
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

*****************Read the response.***********
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
********************************************************************** Pricing update Begin of  "TR: DEVK935586
*      logic_switch-pricing = 'B'.
*      order_header_inx-updateflag = 'U'.
*
*      DATA: lw_conditionx TYPE  bapicondx,
*            lt_conditionx TYPE STANDARD TABLE OF bapicondx.
*
*      LOOP AT lt_item INTO DATA(gs_item).
*        ls_order_item_in-itm_number = gs_item-itm_number.
*        APPEND ls_order_item_in TO lt_order_item_in.
*
*        CLEAR : lw_conditionx.
*        lw_conditionx-itm_number = gs_item-itm_number.
*        lw_conditionx-updateflag = 'U'.
*        lw_conditionx-cond_count = abap_true.
*        lw_conditionx-cond_type  = abap_true.
*        lw_conditionx-cond_value = abap_true.
*        lw_conditionx-currency   = abap_true.
*        APPEND lw_conditionx TO lt_conditionx.
*
*      ENDLOOP.
*
*      LOOP AT lt_itemx INTO DATA(gs_item_x).
*        ls_order_item_inx-itm_number = gs_item_x-itm_number.
*        ls_order_item_inx-updateflag = 'U'.
*        APPEND ls_order_item_inx TO lt_order_item_inx.
*
*      ENDLOOP.
*      CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
*        EXPORTING
*          salesdocument    = return_orderno
**         order_header_in  =
*          order_header_inx = order_header_inx
**         simulation       =
**         behave_when_error     = space
**         int_number_assignment = space
*          logic_switch     = logic_switch
**         no_status_buf_init    = space
*        TABLES
*          return           = gt_return
*          order_item_in    = lt_order_item_in
*          order_item_inx   = lt_order_item_inx
*          CONDITIONS_IN    = lt_cond
*          CONDITIONS_INX   = lt_conditionx.
*
*      READ TABLE gt_return INTO DATA(gs_return) WITH KEY type = 'E'.
*      IF sy-subrc = 0.
*        message = ls_return-message.
*        type    = 'E'.
*      ELSE.
*        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*          EXPORTING
*            wait = 'X'.
**     message = | Return sales order created - { return_orderno } |.
**      type    = 'S'.
*      ENDIF.
********************************************************************** End of Pricing Update"TR: DEVK935586
*************data store to log table header & item*************
      ls_loghead-dms_orderid    = ls_input-orderid.
      ls_loghead-distributor    = ls_input-distributor.
      ls_loghead-dealer         = ls_input-soldtoparty.
      ls_loghead-ref_invoiceno  = gs_input-invoice_no.
      ls_loghead-ref_invdate    = gs_input-inv_date.
      ls_loghead-return_orderno = return_orderno.
      ls_loghead-erdat          = sy-datum.
      ls_loghead-erzet          = sy-uzeit.
      ls_loghead-status         = 'S'.
      ls_loghead-sostat         = 11.
      ls_loghead-msg            = message.
      MODIFY zsd_retrn_hd_dms FROM ls_loghead.
      MODIFY zsd_retrn_it_dms FROM TABLE lt_logitem.
      IF sy-subrc = 0.
        COMMIT WORK.
      ELSE.
        ROLLBACK WORK.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD validations_inv.
**********local variable data dec***********
    DATA : lv_type        TYPE bapi_mtype,
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

    DATA : ls_input TYPE zsd_st_return_order_dms.
    CLEAR : lv_type,
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
    IF gs_input-orderid IS INITIAL.
      message = | DMS order id is missing |.
      EXIT.
    ENDIF.
*Multiple click on front end so avoiding duplicates
    SELECT SINGLE * FROM zsd_invoice_dms
      INTO @DATA(l_dupl_ordid)
      WHERE invoice_no = @gs_input-orderid.
    IF sy-subrc = 0.
      message = | Already sales return order In Processing { gs_input-orderid }|.
      EXIT.
    ELSE.
      DATA(l_orderid_chk) = VALUE zsd_invoice_dms( mandt      = sy-mandt
                                                   invoice_no = gs_input-orderid ).
      INSERT zsd_invoice_dms FROM l_orderid_chk.
    ENDIF.
*************check the duplicate sales return*********
    SELECT SINGLE * FROM zsd_retrn_hd_dms INTO @DATA(ls_header) WHERE dms_orderid = @gs_input-orderid.
    IF sy-subrc = 0 AND ls_header-sostat = 17 AND ls_header-status = 'S'.
      "IRN Details Fetch
      SELECT SINGLE * FROM zdms_invoice_irn INTO @DATA(l_irn)
                      WHERE bukrs    = 'DMS1'
                      AND   docno    = @ls_header-invoice_no.
*                      AND   doc_year = @ls_header-gjahr.
      message       = | Already sales return order completed |.
      type          = 'S'.
      invoice_no    = ls_header-invoice_no.
      sord_no       = ls_header-return_orderno.
      irn           = l_irn-irn.
      signed_qrcode = l_irn-signed_qrcode.
      ack_no        = l_irn-ack_no.
      ack_date      = l_irn-ack_date.
      EXIT.
    ELSE.
      lv_returnorder = ls_header-return_orderno.
      lv_delivery    = ls_header-delivery_no.
      lv_pgi         = ls_header-pgi_no.
      lv_invoice     = ls_header-invoice_no.
      lv_gjahr       = ls_header-gjahr.
    ENDIF.
    IF lv_returnorder IS INITIAL.
**********fetch the invoice details***********
      SELECT a~vbeln,
             a~kunag,
             a~vkorg,
             a~vtweg,
             a~spart,
             a~kunwe,
             b~posnr,
             b~fkimg,
             b~meins,
             b~matnr,
             b~vgbel,
             b~werks FROM vbrk AS a
                     INNER JOIN vbrp AS b
                     ON a~vbeln EQ b~vbeln
                     INNER JOIN kna1 AS c
                     ON b~werks EQ c~werks
                     INTO TABLE @DATA(lt_vbrp)
                     WHERE a~vbeln EQ @gs_input-invoice_no
                     AND   a~fkdat EQ @gs_input-inv_date
                     AND   a~fkart EQ 'YDMS'
                     AND   a~kunag EQ @gs_input-dealer
                     AND   c~kunnr EQ @gs_input-distributor.
      IF sy-subrc NE 0.
        message = | Invoice is not created - { gs_input-invoice_no } |.
        EXIT.
      ENDIF.
      DATA : ls_vbrp LIKE LINE OF lt_vbrp.
**************Call the common validation***********
      CALL METHOD common_validations
        IMPORTING
          lv_msg = message.
      IF message IS NOT INITIAL.
        EXIT.
      ENDIF.
***********fetch the delivery details***********
*      SELECT matnr,
*             werks,
*             lgort,
*             charg,
*             lfimg FROM lips
*                   INTO TABLE @DATA(lt_lips)
*                   WHERE vbeln EQ @ls_vbrk-vgbel
*                   AND   charg NE @space.
**********check already return order details***********
      SELECT vgbel,
             matnr,
             kwmeng FROM vbap
                    INTO TABLE @DATA(lt_returned)
                    WHERE vgbel     EQ @gs_input-invoice_no
                    AND   auart_ana EQ 'YRMS'
                    AND   charg     NE @space.
      IF sy-subrc = 0.
        SORT lt_returned BY matnr.
        DATA(lt_returned2) = lt_returned.
***********add the qty based on duplicate material & batch ************
        DELETE ADJACENT DUPLICATES FROM lt_returned COMPARING matnr.
        IF sy-subrc = 0.
          SORT lt_returned2 BY matnr.
          LOOP AT lt_returned2 INTO DATA(ls_returned).
            AT NEW matnr.
              CONTINUE.
            ENDAT.
            READ TABLE lt_returned ASSIGNING FIELD-SYMBOL(<ls_returned>) WITH KEY matnr = ls_returned-matnr BINARY SEARCH.
            IF sy-subrc = 0.
              <ls_returned>-kwmeng = <ls_returned>-kwmeng + ls_returned-kwmeng.
            ENDIF.
            CLEAR : ls_returned.
          ENDLOOP.
        ENDIF.
      ENDIF.

      SORT : gs_input-details[] BY material batch,
             lt_vbrp BY matnr.

      LOOP AT gs_input-details[] ASSIGNING FIELD-SYMBOL(<fs_details>).
*        lv_charg = <fs_details>-batch.
*        AT NEW batch.
**          IF batch_check = abap_true.
************Check the given batch to invoice material*********
**            READ TABLE lt_lips TRANSPORTING NO FIELDS WITH KEY matnr = <fs_details>-material
**                                                               charg = <fs_details>-batch BINARY SEARCH.
**            IF sy-subrc NE 0.
**              message = | given return material - { <fs_details>-material } batch - { <fs_details>-batch } is not found in ref. invoice { message } |.
**              CONTINUE.
**            ENDIF.
**          ENDIF.
*          CLEAR : lv_charg.
*        ENDAT.
        CLEAR : lv_qty,lv_inv_qty.
*          lv_matnr = <fs_details>-material.
*          lv_check = 'X'.
***********calculated the already take the sales return for ref. invoice*********
        CLEAR ls_returned.
        READ TABLE lt_returned INTO ls_returned WITH KEY matnr = <fs_details>-material BINARY SEARCH.
        IF sy-subrc = 0.
          lv_qty   = lv_qty + ls_returned-kwmeng.
        ELSE.
          SELECT SINGLE matnr,bismt FROM mara INTO @DATA(ls_mara) WHERE bismt = @<fs_details>-material.
          IF sy-subrc = 0.
            READ TABLE lt_returned INTO ls_returned WITH KEY matnr = ls_mara-matnr BINARY SEARCH.
            IF sy-subrc = 0.
              lv_qty   = lv_qty + ls_returned-kwmeng.
            ENDIF.
          ENDIF.
          CLEAR : ls_mara.
        ENDIF.
**********Check the given material to invoice material*********
        CLEAR : ls_vbrp.
        READ TABLE lt_vbrp INTO ls_vbrp WITH KEY matnr = <fs_details>-material BINARY SEARCH.
        IF sy-subrc = 0.
          lv_inv_qty = lv_inv_qty + ls_vbrp-fkimg.
        ELSE.
          CLEAR : ls_mara.
          SELECT SINGLE matnr bismt FROM mara INTO ls_mara
                                    WHERE bismt = <fs_details>-material.
          READ TABLE lt_vbrp INTO ls_vbrp WITH KEY matnr = ls_mara-matnr BINARY SEARCH.
          IF sy-subrc = 0.
            lv_inv_qty = lv_inv_qty + ls_vbrp-fkimg.
          ELSE.
            message = |Given Material { <fs_details>-material }  not Found in Ref. Invoice - { gs_input-invoice_no } , { message }|.
          ENDIF.
        ENDIF.

        lv_qty   = lv_qty + <fs_details>-qty.
*************check given return order material & batch repeated**************
*        IF lv_check = abap_false.
*          IF lv_matnr = <fs_details>-material AND lv_charg EQ <fs_details>-batch.
*            message = | material - { <fs_details>-material } & batch - { <fs_details>-batch } repeated more than once , { message } |.
*            CONTINUE.
*          ENDIF.
*        ENDIF.
*        CLEAR : lv_check.
************check the return material qty & invoice qty***************
        IF lv_qty GT lv_inv_qty.
          message = | Return material qty - { <fs_details>-material } Exceed the invoice qty , { message } |.
        ENDIF.

      ENDLOOP.

*********fill the datas for bapi**********
      IF message IS INITIAL.
        ls_input-orderid     = gs_input-orderid.
        ls_input-compcode    = 'DMS1'.
        ls_input-distchnl    = ls_vbrp-vtweg.
        ls_input-division    = ls_vbrp-spart.
        ls_input-shiptoparty = gs_input-dealer.
        ls_input-soldtoparty = gs_input-dealer.
        ls_input-invoiceno   = gs_input-invoice_no.
        ls_input-inv_date    = gs_input-inv_date.
        ls_input-distributor = gs_input-distributor.
        ls_input-plant       = ls_vbrp-werks.
        ls_input-storage_loc = 'D1'.
        ls_input-salesorg    = ls_vbrp-vkorg.
        MOVE-CORRESPONDING gs_input-details[] TO ls_input-item[].
      ELSE.
        type = 'E'.
        EXIT.
      ENDIF.
    ENDIF.
***********call method for sales order to e-invoice***********
    CALL METHOD overall_process
      EXPORTING
        input          = ls_input
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
        irn            = irn
        signed_qrcode  = signed_qrcode
        ack_no         = ack_no
        ack_date       = ack_date.
***export parameter
    invoice_no = lv_invoice.
    sord_no    = lv_returnorder.

  ENDMETHOD.


  METHOD validations_wout_inv.
**********local variable data dec***********
    DATA : lv_returnorder TYPE vbeln,
           lv_delivery    TYPE vbeln,
           lv_pgi         TYPE mblnr,
           lv_invoice     TYPE vbeln,
           lv_gjahr       TYPE gjahr.
    DATA : ls_input TYPE zsd_st_return_order_dms.

    CLEAR : lv_returnorder,
            lv_delivery,
            lv_pgi,
            lv_gjahr,
            lv_invoice.
    IF gs_input-orderid IS INITIAL.
      message = | DMS order id is missing |.
      EXIT.
    ENDIF.
*Multiple click on front end so avoiding duplicates
    SELECT SINGLE * FROM zsd_invoice_dms
      INTO @DATA(l_dupl_ordid)
      WHERE invoice_no = @gs_input-orderid.
    IF sy-subrc = 0.
      message = | Already sales return order In Processing { gs_input-orderid }|.
      EXIT.
    ELSE.
      DATA(l_orderid_chk) = VALUE zsd_invoice_dms( mandt      = sy-mandt
                                                   invoice_no = gs_input-orderid ).
      INSERT zsd_invoice_dms FROM l_orderid_chk.
    ENDIF.
*************check the duplicate sales return*********
    SELECT SINGLE * FROM zsd_retrn_hd_dms INTO @DATA(ls_header) WHERE dms_orderid = @gs_input-orderid.
    IF sy-subrc = 0 AND ls_header-sostat = 17 AND ls_header-status = 'S'.
      "IRN Details Fetch
      SELECT SINGLE * FROM zdms_invoice_irn INTO @DATA(l_irn)
                      WHERE bukrs    = 'DMS1'
                      AND   docno    = @ls_header-invoice_no.
*                      AND   doc_year = @ls_header-gjahr.
      message       = |Already sales return order completed |.
      type          = 'S'.
      invoice_no    = ls_header-invoice_no.
      sord_no       = ls_header-return_orderno.
      irn           = l_irn-irn.
      signed_qrcode = l_irn-signed_qrcode.
      ack_no        = l_irn-ack_no.
      ack_date      = l_irn-ack_date.
      EXIT.
    ELSE.
      lv_returnorder = ls_header-return_orderno.
      lv_delivery    = ls_header-delivery_no.
      lv_pgi         = ls_header-pgi_no.
      lv_invoice     = ls_header-invoice_no.
      lv_gjahr       = ls_header-gjahr.
    ENDIF.
    IF lv_returnorder IS INITIAL.
***************Call the common validation***********
      CALL METHOD common_validations
        IMPORTING
          lv_msg = message.

      IF message IS NOT INITIAL.
        EXIT.
      ENDIF.

******************header & item data filling****************
      ls_input-compcode      = 'DMS1'.
      ls_input-distchnl      = '20'.
      ls_input-division      = '10'.
      ls_input-storage_loc   = 'D1'.
      ls_input-salesorg      = 'SDMS'.
      ls_input-soldtoparty   = gs_input-dealer.
      ls_input-shiptoparty   = gs_input-dealer.
      ls_input-orderid       = gs_input-orderid.
      ls_input-distributor   = gs_input-distributor.
      ls_input-inv_date      = gs_input-inv_date.
      MOVE-CORRESPONDING gs_input-details[] TO ls_input-item[].
********fetch distributor plant*********
      SELECT SINGLE werks FROM kna1 INTO ls_input-plant WHERE kunnr = gs_input-distributor.
    ENDIF.
***********call method for sales order to e-invoice***********
    CALL METHOD overall_process
      EXPORTING
        input          = ls_input
        lv_pgi         = lv_pgi
        lv_invoice     = lv_invoice
        lv_returnorder = lv_returnorder
        lv_delivery    = lv_delivery
        year           = lv_gjahr
      IMPORTING
        message        = message
        type           = type
        invoice_no     = lv_invoice
        sord_no        = lv_returnorder
        irn            = irn
        signed_qrcode  = signed_qrcode
        ack_no         = ack_no
        ack_date       = ack_date.
***export parameter
    invoice_no = lv_invoice.
    sord_no    = lv_returnorder.
  ENDMETHOD.
ENDCLASS.
