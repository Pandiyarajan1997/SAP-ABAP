class ZCL_API_SCM_INVOICE_IRN_DMS definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_SCM_INVOICE_IRN_DMS IMPLEMENTATION.


  METHOD if_http_extension~handle_request.
*----------------------------------------------------------------------------------
*&Created by: Samsudeen M
*&Created On: 13.06.2023
*&Purpose : For Invoice Process & E-invoice Process
*&Reference: Ramakrishnan J & Praveen Kumar & Gopal raja
*-------------------------------------------------------------------------------------
    TYPES: BEGIN OF ty_input,
             orderid  TYPE zorder_id,
             salesdoc TYPE vbeln_va,
           END OF ty_input.

    DATA: gt_input  TYPE TABLE OF ty_input,
          gt_update TYPE TABLE OF ty_input,
          gs_input  TYPE ty_input,
          ls_inv    TYPE zdms_inv_credit.

    TYPES: BEGIN OF output,
             order_id TYPE zorder_id,
             sales_no TYPE vbeln_va,
             invno    TYPE vbeln_vf,
             inv_date TYPE audat,
             irn      TYPE j_1ig_irn,
             sqrcode  TYPE j_1ig_sign_inv,
             ack_no   TYPE  j_1ig_ack_no,
             ack_date TYPE  j_1ig_ack_date,
             type     TYPE bapi_mtype,
             msg      TYPE string,
             form     TYPE fpcontent,
           END OF output.
    DATA: gt_output TYPE TABLE OF output.

    DATA: lv_data TYPE string.
    DATA: lr_data TYPE REF TO data.
    DATA:lv_belnr   TYPE belnr_d,
         lv_invoice TYPE vbeln_vf,
         lv_bukrs   TYPE bukrs,
         lv_gjahr   TYPE gjahr,
         l_einv     TYPE zst_irn_out_dms,
         l_eway     TYPE j_1ig_ewaybill,
         lt_return  TYPE bapiret2_t.
    DATA: lv_msg TYPE string.

    DATA: lv_body    TYPE string,
          v_jsonload TYPE string.
    DATA:  lv_errstr TYPE string.

    DATA: lo_object_cls TYPE REF TO zcl_scm_sales_to_invoice_dms.
    CREATE OBJECT lo_object_cls.

    DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
    CREATE OBJECT lo_log_upd.

    DATA: lo_common_check TYPE REF TO zcl_common_check.       "obj dec for global class common check
    CREATE OBJECT lo_common_check.
************dms error log**********
    DATA: lo_errorlog_dms TYPE REF TO zcl_api_dms_error_log_entries.
    CREATE OBJECT lo_errorlog_dms.

*************IRN data dec**************
    DATA :irn           TYPE  j_1ig_irn,
          signed_inv    TYPE  j_1ig_sign_inv,
          signed_qrcode TYPE  j_1ig_sign_qrcode,
          ack_no        TYPE  j_1ig_ack_no,
          ack_date      TYPE  j_1ig_ack_date,
          pdf           TYPE  fpcontent,
          return        TYPE string,
          lv_double     TYPE char1.

    CONSTANTS: c_invoice TYPE char02 VALUE '15',
               c_pgi     TYPE char02 VALUE '13',
               c_lgort   TYPE lgort_d VALUE 'D1',
               c_irn     TYPE char02 VALUE '17',
               c_comp    TYPE char02 VALUE '18'.

    "Input of API Request
    CLEAR lv_data.
    CALL METHOD server->request->get_cdata RECEIVING data = lv_data.
    DATA(lv_data_tmp) = |[ { lv_data } ]|.
*    CONDENSE lv_data_tmp NO-GAPS.
    SHIFT lv_data_tmp LEFT DELETING LEADING ''.

** Deserialize the input our required input ***
    /ui2/cl_json=>deserialize(
   EXPORTING
     json         = lv_data_tmp
     pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
   CHANGING
     data         = gt_input ).

    IF gt_input IS NOT INITIAL.
      REFRESH: gt_output.
      CLEAR  : lv_double.
      gs_input = VALUE #( gt_input[ 1 ] OPTIONAL ).

*Multiple click on front end so avoiding duplicates
      SELECT SINGLE * FROM zsd_dms_inv_chk INTO @DATA(ls_dublict) WHERE orderid = @gs_input-orderid.
      IF sy-subrc = 0.
        APPEND VALUE #( order_id = gs_input-orderid
                        sales_no = gs_input-salesdoc
                        type     = 'E'
                        msg      = |Already Order ID in Processing| ) TO gt_output.
        lv_double = 'X'.
      ELSE.
        DATA(l_orderid_chk) = VALUE zsd_dms_inv_chk( mandt   = sy-mandt
                                                     orderid = gs_input-orderid ).
        INSERT zsd_dms_inv_chk FROM l_orderid_chk.
        "alpha conversion***********
        DATA(l_vbeln) = CONV vbeln_va( |{ gs_input-salesdoc  ALPHA = IN }| ).
********************get the sales details****************
        SELECT SINGLE * FROM zsd_sale_hd_dms INTO @DATA(l_salelog) WHERE ordid = @gs_input-orderid
                                                                   AND   vbeln =   @l_vbeln.
*        Orderid Checks
        SELECT SINGLE * FROM zsd_scm_hd_dms INTO @DATA(lw_header) WHERE order_id = @gs_input-orderid
                                                                  AND   vbeln    = @l_vbeln.
        IF sy-subrc = 0.
          "Delivery Completed Means only Process for Invoice
          IF lw_header-dcomp EQ abap_true.
            "First Check whether Invoice is Generated Already or Not
            IF lw_header-icomp EQ abap_true.
**********call e inv process for particular distributor************
              DATA : lo_main2 TYPE REF TO zcl_dms_einvoice_process.
              CREATE OBJECT lo_main2.
*              SELECT SINGLE * FROM zsd_sale_hd_dms INTO @DATA(l_salelog1) WHERE ordid = @gs_input-orderid
*                                             AND   vbeln =   @l_vbeln.
***********************get the pdf***************
              CALL METHOD lo_main2->generate_pdf_dms
                EXPORTING
                  inv_no   = lw_header-invoice_no
                  customer = l_salelog-kunag
                IMPORTING
                  pdf      = pdf
                  return   = return.
              "IRN Details Fetch
              SELECT SINGLE * FROM zdms_invoice_irn INTO @DATA(l_irn)
                              WHERE bukrs    = @lw_header-bukrs
                              AND   docno    = @lw_header-invoice_no.
*                              AND   doc_year = @lw_header-gjahr.
              IF sy-subrc NE 0.
****************get the invoice date****************
                SELECT SINGLE vbeln,fkdat FROM vbrk INTO @DATA(ls_vbrk)
                                          WHERE vbeln = @lw_header-invoice_no.
*************check e-invoice process*************
                SELECT SINGLE distributor FROM zdist_einv_dtls
                                          INTO @DATA(ls_check)
                                          WHERE distributor = @l_salelog-distributor
                                          AND   act_date LE @ls_vbrk-fkdat.
                IF sy-subrc NE 0.
                  APPEND VALUE #( order_id = gs_input-orderid
                                  sales_no = l_vbeln
                                  invno    = lw_header-invoice_no
                                  inv_date = lw_header-inv_date
                                  irn      = l_irn-irn
                                  sqrcode  = l_irn-signed_qrcode
                                  type     = 'S'
                                  msg      = |Invoice Already Completed|
                                  form     = pdf ) TO gt_output.
                ELSE.
                  SELECT SINGLE kunnr,stcd3  FROM kna1 INTO @DATA(ls_kna2) WHERE kunnr = @l_salelog-kunag.
                  IF ls_kna2-stcd3 IS NOT INITIAL.
                    CALL METHOD lo_main2->create_irn_qrcode
                      EXPORTING
                        distributor_code = l_salelog-distributor
                        vbeln            = lw_header-invoice_no
                        bukrs            = lw_header-bukrs
                        gjahr            = lw_header-gjahr
*                       DATE             = <lw_header>
                      IMPORTING
                        irn              = irn
                        signed_inv       = signed_inv
                        signed_qrcode    = signed_qrcode
                        ack_date         = ack_date
                        ack_no           = ack_no
                        return           = return.

                    IF irn IS NOT INITIAL.
                      lw_header-irn    = abap_true.
                      lw_header-status = c_irn.
                      lw_header-icomp  = abap_true.
                      lw_header-msgtyp = 'S'.
                      lw_header-msg    = |Sales to E-invoice Process Completed|.
                    ELSE.
                      lw_header-msgtyp = 'E'.
                      lw_header-msg = | Billing Document Created , { return } |.
                    ENDIF.
                  ELSE.
                    lw_header-msgtyp = 'S'.
                    lw_header-msg    = |Sales to invoice Process Completed , GST is missing|.
                    lw_header-status = '17'.
                    lw_header-icomp  = abap_true.
                  ENDIF.
***********************get the pdf***************
                  CALL METHOD lo_main2->generate_pdf_dms
                    EXPORTING
                      inv_no   = lw_header-invoice_no
                      customer = l_salelog-kunag
                    IMPORTING
                      pdf      = pdf
                      return   = return.
*****************Send the response***************
                  MODIFY zsd_scm_hd_dms FROM lw_header.
                  APPEND VALUE #( order_id = lw_header-order_id
                                  sales_no = lw_header-vbeln
                                  invno    = lw_header-invoice_no
                                  inv_date = lw_header-inv_date
                                  irn      = irn
                                  sqrcode  = signed_qrcode
                                  type     = lw_header-msgtyp
                                  ack_no   = ack_no
                                  ack_date = ack_date
                                  msg      = lw_header-msg
                                  form     = pdf ) TO gt_output.
                ENDIF.
              ELSE.
                APPEND VALUE #( order_id = gs_input-orderid
                                sales_no = l_vbeln
                                invno    = lw_header-invoice_no
                                inv_date = lw_header-inv_date
                                irn      = l_irn-irn
                                sqrcode  = l_irn-signed_qrcode
                                ack_no   = l_irn-ack_no
                                ack_date = l_irn-ack_date
                                type     = 'S'
                                msg      = |Invoice Already Completed|
                                form     = pdf ) TO gt_output.
              ENDIF.
            ELSE.
*----------------------------------------------------------------------------------
              IF l_salelog-bukrs IS NOT INITIAL.
                lo_common_check->fi_period_check(
                  EXPORTING
                    posting_date =  sy-datum          " Field of type DATS
                    acct_type    =  'D'               " Account type
                    com_code     =  l_salelog-bukrs   " Company Code
                  IMPORTING
                    type         =  DATA(gv_type)                  " Single-Character Flag
                  EXCEPTIONS
                    mandatory    = 1                " Fill POSTING_DATE , ACCT_TYPE , COM_CODE
                    OTHERS       = 2
        ).
                IF sy-subrc = 0 AND gv_type = 'E'.
                  lv_msg = |{ lv_msg } ; FI Posting Period Not Opened|.
                  CLEAR : gv_type.
                ENDIF.
*********************Customer not extended to company code Check*************************
                "Primary Order ID and Sales Order Combination Check
                SELECT SINGLE kunnr,bukrs FROM knb1 INTO @DATA(gs_knb1) WHERE bukrs = @l_salelog-bukrs AND
                                                                              kunnr = @l_salelog-kunag.
                IF sy-subrc <> 0.
                  lv_msg = |{ lv_msg } ; Customer Code { l_salelog-kunag } not Extended to Company code { l_salelog-bukrs }|.
                ENDIF.
              ENDIF.
****************************customer block check*****************
              SELECT SINGLE * FROM zcust_blk_chk INTO @DATA(ls_blk)
                                                 WHERE bukrs = @l_salelog-bukrs
                                                 AND   vkorg = @l_salelog-vkorg
                                                 AND   kunnr = @l_salelog-kunag
                                                 AND   block = @abap_true.
              IF sy-subrc = 0.
                lv_msg = |Customer - { l_salelog-kunag } is blocked, { lv_msg }|.
              ENDIF.
*---------------------------------------------------------------------------------
              IF lv_msg IS INITIAL.
*** Billing Document Creation Process
                IF lw_header-status EQ c_pgi.
                  REFRESH: lt_return. CLEAR: lv_belnr,lv_invoice,lv_bukrs,lv_gjahr.
                  CALL METHOD lo_object_cls->create_invoice
                    EXPORTING
                      delivery_no  = lw_header-delivery_no
                    IMPORTING
                      bill_invoice = lv_invoice
                      acco_invoice = lv_belnr
                      bukrs        = lv_bukrs
                      gjahr        = lv_gjahr
                      return       = lt_return.

                  IF lv_invoice IS NOT INITIAL.
                    lw_header-invoice_no = lv_invoice.
                    lw_header-inv_date   = sy-datum.
                    lw_header-belnr      = lv_belnr.
                    lw_header-bukrs      = lv_bukrs.
                    lw_header-gjahr      = lv_gjahr.
                    lw_header-status     = c_invoice.
                    lw_header-msgtyp     = 'S'.
                    lw_header-msg        = |Billing Document Created|.
*** If Invoice is Generated But Accounting Document is Not Generated ***
                    IF lv_belnr IS INITIAL.
                      INSERT INTO zsd_accdoc_log VALUES @( VALUE #( mandt      = sy-mandt
                                                                    sonumber   = l_vbeln
                                                                    billnumber = lv_invoice
                                                                    message    = |Account Document Not Generated| ) ).
                    ENDIF.

********************update the invoice details for automatic credit note******************
                    SELECT SINGLE netwr FROM vbrk INTO @DATA(lv_netwr) WHERE vbeln = @lv_invoice.
                    IF sy-subrc = 0.
                      ls_inv-invoice_no   = lv_invoice.
                      ls_inv-inv_type     = 'I'.
                      ls_inv-inv_date     = sy-datum.
                      ls_inv-ac_doc       = lv_belnr.
                      ls_inv-fisc_year    = lv_gjahr.
                      ls_inv-distributor  = l_salelog-distributor.
                      ls_inv-dist_plant   = l_salelog-plant.
                      ls_inv-dealer       = l_salelog-kunag.
                      ls_inv-created_on   = sy-datum.
                      ls_inv-created_by   = sy-uname.
                      ls_inv-created_time = sy-uzeit.
                      ls_inv-status       = '10'.
                      ls_inv-net_amount   = lv_netwr.
                      MODIFY zdms_inv_credit FROM ls_inv.
                      CLEAR ls_inv.
                    ENDIF.
                  ELSE.
                    lw_header-msgtyp = 'E'.
                    lw_header-msg = |Invoice is Not Generated|.
                  ENDIF.
                ENDIF.
*** E-invoice Generation
                IF lw_header-status = c_invoice AND lw_header-invoice_no IS NOT INITIAL.
**********call e inv process for particular distributor************
                  DATA : lo_main1 TYPE REF TO zcl_dms_einvoice_process.
                  CREATE OBJECT lo_main1.
*                IF ( sy-sysid EQ 'DEV' OR sy-sysid EQ 'QAS' ).
*                  SELECT SINGLE * FROM j_1ig_invrefnum INTO @l_irn
*                    WHERE irn NE @space.
*                  IF sy-subrc EQ 0.
*
*                  ENDIF.
*                ELSE.
*                CLEAR: lt_return,l_einv,l_eway.
*************check e-invoice process*************
                  SELECT SINGLE distributor FROM zdist_einv_dtls INTO ls_check
                                                                 WHERE distributor = l_salelog-distributor
                                                                 AND   act_date LE lw_header-inv_date.
                  IF sy-subrc NE 0.
                    lw_header-msgtyp = 'S'.
                    lw_header-msg    = |Sales to invoice Process Completed|.
                    lw_header-status = '17'.
                    lw_header-icomp  = abap_true.
****************************add the irn table********************
                    DATA : ls_irn TYPE zdms_invoice_irn.
                    CLEAR : ls_irn.
                    ls_irn-bukrs  = l_salelog-bukrs. "'DMS1'.
                    ls_irn-dealer = l_salelog-kunag.
                    ls_irn-distrb = l_salelog-distributor.
                    ls_irn-docno  = lv_invoice.
                    ls_irn-ernam  = sy-uname.
                    ls_irn-erdat  = sy-datum.
                    ls_irn-erzet  = sy-uzeit.
                    MODIFY zdms_invoice_irn FROM ls_irn.
                  ELSE.
                    SELECT SINGLE kunnr,stcd3  FROM kna1 INTO @DATA(ls_kna1) WHERE kunnr = @l_salelog-kunag.
                    IF ls_kna1-stcd3 IS NOT INITIAL.
                      CALL METHOD lo_main1->create_irn_qrcode
                        EXPORTING
                          distributor_code = l_salelog-distributor
                          vbeln            = lw_header-invoice_no
                          bukrs            = lw_header-bukrs
                          gjahr            = lw_header-gjahr
*                         DATE             = <lw_header>
                        IMPORTING
                          irn              = irn
                          signed_inv       = signed_inv
                          signed_qrcode    = signed_qrcode
                          ack_no           = ack_no
                          ack_date         = ack_date
                          return           = return.
                      IF irn IS NOT INITIAL.
**  ***einvoice irn invrefnum details.--->>
                        lw_header-irn    = abap_true.
                        lw_header-status = c_irn.
                        lw_header-icomp  = abap_true.
                        lw_header-msgtyp = 'S'.
                        lw_header-msg    = |Sales to E-invoice Process Completed|.
                      ELSE.
                        lw_header-msgtyp = 'E'.
                        lw_header-msg = | Billing Document Created ,{ return } |.
                      ENDIF.

                    ELSE.
                      lw_header-msgtyp = 'S'.
                      lw_header-msg    = |Sales to invoice Process Completed , GST is missing|.
                      lw_header-status = '17'.
                      lw_header-icomp  = abap_true.
****************************add the irn table********************
                      CLEAR : ls_irn.
                      ls_irn-bukrs  = l_salelog-bukrs."'DMS1'.
                      ls_irn-dealer = l_salelog-kunag.
                      ls_irn-distrb = l_salelog-distributor.
                      ls_irn-docno  = lv_invoice.
                      ls_irn-ernam  = sy-uname.
                      ls_irn-erdat  = sy-datum.
                      ls_irn-erzet  = sy-uzeit.
                      MODIFY zdms_invoice_irn FROM ls_irn.
                    ENDIF.
                  ENDIF.
                ENDIF.
                MODIFY zsd_scm_hd_dms FROM lw_header.
                IF lw_header-msgtyp = 'S'.
***********************get the pdf***************
                  CALL METHOD lo_main1->generate_pdf_dms
                    EXPORTING
                      inv_no   = lw_header-invoice_no
                      customer = l_salelog-kunag
                    IMPORTING
                      pdf      = pdf
                      return   = return.
                ENDIF.
                APPEND VALUE #( order_id = lw_header-order_id
                                sales_no = lw_header-vbeln
                                invno    = lw_header-invoice_no
                                inv_date = lw_header-inv_date
                                irn      = irn
                                sqrcode  = signed_qrcode
                                type     = lw_header-msgtyp
                                ack_no   = ack_no
                                ack_date = ack_date
                                msg      = lw_header-msg
                                form     = pdf ) TO gt_output.

              ELSE.
                APPEND VALUE #( order_id = gs_input-orderid
                                sales_no = l_vbeln
                                type     = 'E'
                                msg      = lv_msg ) TO gt_output.
              ENDIF.

            ENDIF.

            "If Delivery is not Processed Means Invoice Process Not allowed
          ELSE.
            APPEND VALUE #( order_id = gs_input-orderid
                            sales_no = l_vbeln
                            type     = 'E'
                            msg      = |Delivery is not Completed| ) TO gt_output.
          ENDIF.
        ELSE.
          APPEND VALUE #( order_id = gs_input-orderid
                          sales_no = l_vbeln
                          type     = 'E'
                          msg      = |Order ID is Not Present in log table| ) TO gt_output.
        ENDIF.
      ENDIF.
    ENDIF.

    IF gt_output[] IS NOT INITIAL.
      READ TABLE gt_output INTO DATA(ls_output) WITH KEY type = 'E'.
      IF sy-subrc = 0.
*********************error log***********
        lo_errorlog_dms->log_entry_store(
          EXPORTING
            type                = 14
            status              = 10
            dms_orderid         = gs_input-orderid
            distributor         = l_salelog-distributor
            plant               = l_salelog-plant
            dealer              = l_salelog-kunag
            msg                 = ls_output-msg ).

      ENDIF.
      CLEAR: lv_body,v_jsonload.
** serialize the output for response ***
      /ui2/cl_json=>serialize(
      EXPORTING
       data         =  gt_output
       pretty_name  = /ui2/cl_json=>pretty_mode-user
      RECEIVING
       r_json         = lv_body ).

*Output Entry in Log Table
      CALL METHOD lo_log_upd->log_entry_store
        EXPORTING
          apiname         = 'DMS_BILLINVOICE'
          ijson           = lv_data
          ojson           = lv_body
          distributor     = l_salelog-distributor
          retailer        = l_salelog-kunag
        EXCEPTIONS
          apiname_missing = 1
          json_missing    = 2
          OTHERS          = 3.
      IF sy-subrc = 0.

      ENDIF.

      IF lv_double = 'X'.
        WAIT UP TO 5 SECONDS.
      ENDIF.
*once successfully processed deleting the Order ID from duplicaton CHECK table
      DELETE FROM zsd_dms_inv_chk WHERE orderid = gs_input-orderid.

      v_jsonload = | { '[' } { lv_body } { ']' } |.
*Set JSON Content-Type
      CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
      CALL METHOD server->response->set_cdata( data = v_jsonload ).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
