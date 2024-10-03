class ZCL_API_SCM_DELIVERY_PGI_DMS definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_SCM_DELIVERY_PGI_DMS IMPLEMENTATION.


  METHOD if_http_extension~handle_request.
*----------------------------------------------------------------------------------
*&Created by: Samsudeen M
*&Created On: 12.04.2023
*&Purpose : For Filling Log table of SCM Sales order to Invoice Process
*&Reference: Ramakrishnan J & Praveen Kumar & Gopal raja
*    CONSTANTS: c_initiated TYPE char02 VALUE '11',
*               c_delivery  TYPE char02 VALUE '12',
*               c_pgi       TYPE char02 VALUE '13',
*               c_lgort     TYPE lgort_d VALUE 'D1'.
*-------------------------------------------------------------------------------------
    TYPES: BEGIN OF ty_input,
             orderid  TYPE zorder_id,
             salesdoc TYPE vbeln_va,
           END OF ty_input.

    DATA: gt_input  TYPE TABLE OF ty_input,
          gt_update TYPE TABLE OF ty_input,
          gs_input  TYPE ty_input.

    TYPES: BEGIN OF output,
             order_id TYPE zorder_id,
             sales_no TYPE vbeln_va,
             delno    TYPE vbeln_vl,
             pgino    TYPE mblnr,
             type     TYPE bapi_mtype,
             msg      TYPE string,
*             form     TYPE string,
           END OF output.
    DATA: gt_output TYPE TABLE OF output.

    DATA: lv_data TYPE string.
    DATA: lr_data TYPE REF TO data.
    DATA: lv_vbeln TYPE vbeln_vl,
          lv_mblnr TYPE mblnr.
    DATA: lv_msg TYPE string.

    DATA: lv_body    TYPE string,
          v_jsonload TYPE string.
    DATA: lv_opendel_qty TYPE lfimg,
          lv_pend_qty    TYPE lfimg.
    DATA: lt_ret TYPE bapireturn.
    DATA: lt_table1 TYPE TABLE OF bapiwmdvs,
          lt_table2 TYPE TABLE OF bapiwmdve.
*    DATA: "lv_string TYPE string,
*          lv_errstr TYPE string.

    DATA: lo_object_cls TYPE REF TO zcl_scm_sales_to_invoice_dms.
    CREATE OBJECT lo_object_cls.

    DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
    CREATE OBJECT lo_log_upd.

    DATA: lo_common_check TYPE REF TO zcl_common_check.       "obj dec for global class common check
    CREATE OBJECT lo_common_check.

    DATA : gt_condition TYPE TABLE OF bapisdcond,
           gt_item      TYPE TABLE OF bapisditbos.
************dms error log**********
    DATA: lo_errorlog_dms TYPE REF TO zcl_api_dms_error_log_entries.
    CREATE OBJECT lo_errorlog_dms.


    CONSTANTS: c_initiated TYPE char02 VALUE '11',
               c_delivery  TYPE char02 VALUE '12',
               c_pgi       TYPE char02 VALUE '13',
               c_lgort     TYPE lgort_d VALUE 'D1'.

    "Input of API Request
    CLEAR lv_data.
    CALL METHOD server->request->get_cdata RECEIVING data = lv_data.
    DATA(lv_data_tmp) = |[ { lv_data } ]|.
    CONDENSE lv_data_tmp NO-GAPS.
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
      gs_input = VALUE #( gt_input[ 1 ] OPTIONAL ).

      DATA(l_vbeln) = CONV vbeln_va( |{ gs_input-salesdoc  ALPHA = IN }| ).
      "Primary Order ID and Sales Order Combination Check
      SELECT SINGLE * FROM zsd_sale_hd_dms
        INTO @DATA(l_salelog)
        WHERE ordid = @gs_input-orderid
        AND vbeln = @l_vbeln.
      IF sy-subrc NE 0.
        APPEND VALUE #( order_id = gs_input-orderid
                        sales_no = l_vbeln
                        type     = 'E'
                        msg      = |Incorrect Sales Order Number| ) TO gt_output.
*********************error log***********
        lv_msg = |Incorrect Sales Order Number|.
        lo_errorlog_dms->log_entry_store(
          EXPORTING
            type                = 12
            status              = 10
            dms_orderid         = gs_input-orderid
            distributor         = l_salelog-distributor
            plant               = l_salelog-plant
            dealer              = l_salelog-kunag
            msg                 = lv_msg ).
*      ENDIF.
      ELSE.
        IF l_salelog-sostat = '12'.
          APPEND VALUE #( order_id = gs_input-orderid
                          sales_no = l_vbeln
                          type     = 'E'
                          msg      = 'Sales order is closed' ) TO gt_output.
        ELSE.
          "Orderid Checks
          SELECT SINGLE * FROM zsd_scm_hd_dms INTO @DATA(lw_header2)
                                              WHERE order_id = @gs_input-orderid
                                              AND   dcomp    = @abap_true.
*        ENDIF.
          IF sy-subrc = 0 AND lw_header2-gdsmvt_no IS NOT INITIAL.
            APPEND VALUE #( order_id = gs_input-orderid
                            sales_no = l_vbeln
                            delno    = lw_header2-delivery_no
                            pgino    = lw_header2-gdsmvt_no
                            type     = lw_header2-msgtyp
                            msg      = lw_header2-msg ) TO gt_output.
          ELSE.
*----------------------------------------------------------------------------------
*      Changed by - pandiarajan
*      changed on date - 12.09.2023
*      Reference by: Ramakrishnan J
*********************MM Posting Period Open Check*********************************
            CLEAR lv_msg.
            IF l_salelog-bukrs IS NOT INITIAL.
              lo_common_check->mm_period_check(
                EXPORTING
                  com_code  = l_salelog-bukrs               " Company Code
                  date      = sy-datum                      " Posting Date in the Document
                IMPORTING
                  type      = DATA(gv_type)                 " Single-Character Flag
*              message  =               " Character 100
                EXCEPTIONS
                  mandatory = 1                " Fill Com_code & Date
                  OTHERS    = 2 ).
              IF sy-subrc = 0 AND gv_type = 'E'.
                lv_msg = |{ lv_msg } ; MM Posting Period Not Opened|.
              ENDIF.
*********************FI Posting Period Open Check*********************************
              lo_common_check->fi_period_check(
                EXPORTING
                  posting_date =  sy-datum          " Field of type DATS
                  acct_type    =  'D'                  " Account type
                  com_code     =  l_salelog-bukrs   " Company Code
                IMPORTING
                  type         =  gv_type                  " Single-Character Flag
*      message      =                  " Character 100
                EXCEPTIONS
                  mandatory    = 1                " Fill POSTING_DATE , ACCT_TYPE , COM_CODE
                OTHERS         = 2
      ).
              IF sy-subrc = 0 AND gv_type = 'E'.
                lv_msg = |{ lv_msg } ; FI Posting Period Not Opened|.
                CLEAR : gv_type.
              ENDIF.
            ENDIF.

*******************check the partner function for mapped distributor***********
            SELECT SINGLE * FROM knvp INTO @DATA(ls_knvp) WHERE kunnr = @l_salelog-kunag
                                                          AND   vkorg = @l_salelog-vkorg
                                                          AND   vtweg = @l_salelog-vtweg
                                                          AND   spart = @l_salelog-spart
                                                          AND   parvw = 'SK'.
            IF l_salelog-distributor <> ls_knvp-kunn2.
              lv_msg = |Given distributor { l_salelog-distributor } is mismatch with partner function SK - { ls_knvp-kunn2 }|.
            ENDIF.

*********************Pan No Missing for Sundaram Finance Customer******************
            SELECT SINGLE kunnr,zcustype,j_1ipanno,regio,stcd3 FROM kna1 INTO @DATA(gs_kna1) WHERE kunnr = @l_salelog-kunag.
            IF sy-subrc = 0 AND gs_kna1-zcustype = 'SF'.
              SELECT SINGLE * FROM zsd_cus_pan INTO @DATA(gs_cus_pan) WHERE cus_no = @l_salelog-kunag.
              IF sy-subrc NE 0.
                lv_msg = |{ lv_msg } ; SF Customer { l_salelog-kunag } Pan No Missing|.
              ENDIF.
            ENDIF.
*********************Customer Code(gst) & region check*****************************
            IF gs_kna1-stcd3 IS NOT INITIAL.
              CONDENSE : gs_kna1-stcd3.
              IF gs_kna1-stcd3(1) = space.
                lv_msg = |{ lv_msg } ; Customer { l_salelog-kunag } GST is incorrect|.
              ENDIF.
              IF gs_kna1-regio IS NOT INITIAL.
                SELECT SINGLE region,gst_key FROM zsd_gst_reg INTO @DATA(gs_gst) WHERE region = @gs_kna1-regio.
                DATA(lv_gstkey) = CONV char02( gs_gst-gst_key ).
                IF gs_kna1-stcd3(2) <> lv_gstkey.
                  lv_msg = |{ lv_msg } ; Customer { l_salelog-kunag } GST and Region not matching|.
                ENDIF.
              ENDIF.
            ENDIF.
*****************************Block the process for distributor or customer*****************
            DATA : lobj_check TYPE REF TO zcl_common_check.
            CREATE OBJECT lobj_check.
********************for distributor checking****************
            lobj_check->dms_process_stop(
              EXPORTING
                kunnr        =  l_salelog-distributor
                process_type = 'DR'
              IMPORTING
                message      = lv_msg ).
********************for customer checking****************
            lobj_check->dms_process_stop(
              EXPORTING
                kunnr        =  l_salelog-kunag
                process_type = 'DR'
              IMPORTING
                message      = lv_msg ).
**************check the GST active or not****************
            IF gs_kna1-stcd3 IS NOT INITIAL.
              SELECT SINGLE * FROM zdist_einv_dtls INTO @DATA(ls_einv)
                                                   WHERE distributor = @l_salelog-distributor.
              IF sy-subrc = 0.
                DATA : lo_gstchk TYPE REF TO zcl_api_salesorder_simulation,
                       lv_gstchk TYPE char10,
                       lv_return TYPE string.

                CREATE OBJECT : lo_gstchk.

                CLEAR : lv_return.
                lo_gstchk->gst_check(
                  EXPORTING
                    distributor = l_salelog-distributor
                    customer    = l_salelog-kunag
                    cust_gst    = gs_kna1-stcd3
                  IMPORTING
                    status      = lv_gstchk
                    return      = lv_return ).

                IF lv_return IS INITIAL.
                  IF lv_gstchk NE 'ACT'.
                    IF lv_gstchk = 'CNL'.
                      lv_msg = |Customer - { l_salelog-kunag } GST No - { gs_kna1-stcd3 } is Cancelled, { lv_msg }|.
                    ELSEIF lv_gstchk = 'SUS'.
*                lv_msg = |Customer - { l_salelog-kunag } GST No - { l_kunag-stcd3 } is Suspended, { lv_msg }|.
                    ELSE.
                      lv_msg = |Customer - { l_salelog-kunag } GST No - { gs_kna1-stcd3 } is invalid, { lv_msg }|.
                    ENDIF.
                  ENDIF.
                ELSE.
                  lv_msg = | { lv_return } , { lv_msg }|.
                ENDIF.
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
*----------------------------------------------------------------------------------
***********pricing condition base on company code*************
            IF l_salelog-bukrs = 'DMS1'.
              DATA(lv_price) = 'ZPR0'.
            ELSEIF l_salelog-bukrs = 'ALPN'.
              lv_price = 'PRAP'.
            ELSEIF l_salelog-bukrs = 'F001' OR l_salelog-bukrs = 'F006'
                OR l_salelog-bukrs = 'F002' OR l_salelog-bukrs = 'F003'
                OR l_salelog-bukrs = 'F004' OR l_salelog-bukrs = 'F005'.
              lv_price = 'ZMRP'.
            ENDIF.

            IF l_vbeln IS NOT INITIAL.
* bapi call get data for order conditions
              CALL FUNCTION 'BAPI_SALESORDER_GETDETAILBOS'
                EXPORTING
                  salesdocument   = l_vbeln
                TABLES
                  orderitems      = gt_item
                  orderconditions = gt_condition.
              SORT gt_condition BY itm_number cond_type.
              SORT gt_item BY material .
            ENDIF.
*Sale Order Item Details *
            SELECT matnr,
                   lfimg,
                   meins FROM zsd_sale_it_dms
                         INTO TABLE @DATA(lt_sale_itm)
                         WHERE ordid = @gs_input-orderid.
            IF sy-subrc EQ 0.
              SORT  lt_sale_itm[] BY matnr ASCENDING.
*Getting Delivery Details *
              SELECT vbeln,
                     posnr,
                     matnr,
                     werks,
                     lfimg FROM lips
                           INTO TABLE @DATA(lt_delivery)
                           WHERE wbsta EQ 'A'.

              LOOP AT lt_sale_itm ASSIGNING FIELD-SYMBOL(<fs_sale_itm>).
                CLEAR: lv_opendel_qty.
                LOOP AT lt_delivery ASSIGNING FIELD-SYMBOL(<fls_delv>) WHERE matnr = <fs_sale_itm>-matnr
                                                                       AND werks = l_salelog-plant.
                  lv_opendel_qty = ( lv_opendel_qty  + <fls_delv>-lfimg ).
                ENDLOOP.
                DATA(l_stock) = CONV bapicm61v-wkbst( 0 ).
                DATA(matnr) = CONV matnr18( <fs_sale_itm>-matnr ).
                "Stock Availabity Check for Material
                CLEAR: lt_ret,lt_table1,lt_table2.
                CALL FUNCTION 'BAPI_MATERIAL_AVAILABILITY'
                  EXPORTING
                    plant      = l_salelog-plant
                    material   = matnr
                    unit       = <fs_sale_itm>-meins
                  IMPORTING
                    av_qty_plt = l_stock
                    return     = lt_ret
                  TABLES
                    wmdvsx     = lt_table1
                    wmdvex     = lt_table2.
                IF lt_table2 IS NOT INITIAL.
                  l_stock = VALUE #( lt_table2[ 1 ]-com_qty OPTIONAL ).
                ENDIF.
                IF l_stock NE 0.
                  IF l_stock LT <fs_sale_itm>-lfimg.
*                CLEAR lv_msg.
                    lv_msg = |{ lv_msg } ; Material Stock { <fs_sale_itm>-matnr } is Less than Order Quantity|.
                    CONTINUE.
                  ENDIF.
                  CLEAR lv_pend_qty.
                  lv_pend_qty = ( l_stock - lv_opendel_qty ).
                  IF lv_pend_qty LT <fs_sale_itm>-lfimg.
                    lv_msg = |{ lv_msg } ; Open Deliveries Available for Material { <fs_sale_itm>-matnr }|.
                  ENDIF.
                ELSE.
*              CLEAR lv_msg.
                  lv_msg = |{ lv_msg } ; No Stock Available for Material { <fs_sale_itm>-matnr }|.
                  CONTINUE.
                ENDIF.
*----------------------------------------------------------------------------------
*      Changed by - Pandiarajan
*      changed on date - 12.09.2023
*      Reference by: Ramakrishnan J
*********************HSN & Profit Center Missing Check*****************************
                SELECT SINGLE matnr,steuc,prctr FROM marc INTO @DATA(gs_marc) WHERE matnr = @<fs_sale_itm>-matnr
                                                                              AND   werks = @l_salelog-plant.
                IF sy-subrc = 0.
                  IF gs_marc-steuc IS INITIAL.
                    lv_msg = |{ lv_msg } ; HSN Missing for Material { <fs_sale_itm>-matnr }|.
                  ENDIF.
                  IF gs_marc-prctr IS INITIAL.
                    lv_msg = |{ lv_msg } ; Profit Center Missing for Material { <fs_sale_itm>-matnr }|.
                  ENDIF.
                ENDIF.
*----------------------------------------------------------------------------------
****************************PR00 & VPRS Missing Check*****************************
                READ TABLE gt_item INTO DATA(gs_item) WITH KEY material = <fs_sale_itm>-matnr BINARY SEARCH.
**********************check the netvalue*******************
                IF gs_item-net_price LE 0.
                  lv_msg = |{ lv_msg } ; Net Value is zero for Materials { <fs_sale_itm>-matnr }|.
                ENDIF.
                IF gs_item-itm_number IS NOT INITIAL.

                  READ TABLE gt_condition TRANSPORTING cond_value INTO DATA(gv_condvalue) WITH KEY itm_number = gs_item-itm_number
                                                                                                   cond_type = lv_price BINARY SEARCH.
                  IF sy-subrc <> 0 OR gv_condvalue IS INITIAL.
                    lv_msg = |{ lv_msg } ; Selling Price { lv_price } not Maintained for Materials { <fs_sale_itm>-matnr }|.
                  ENDIF.
                  READ TABLE gt_condition TRANSPORTING cond_value INTO gv_condvalue WITH KEY itm_number = gs_item-itm_number
                                                                                            cond_type = 'VPRS' BINARY SEARCH.
                  IF sy-subrc <> 0 OR gv_condvalue IS INITIAL.
                    lv_msg = |{ lv_msg } ; Internal Price (VPRS) not Maintained for Materials { <fs_sale_itm>-matnr }|.
                  ENDIF.
**********************Cgst & Sgst value Mismatch Check*****************************
                  READ TABLE gt_condition TRANSPORTING cond_value INTO DATA(lv_igst) WITH KEY itm_number = gs_item-itm_number
                                                                                              cond_type = 'JOIG' BINARY SEARCH.
                  IF sy-subrc NE 0.
                    READ TABLE gt_condition TRANSPORTING cond_value INTO DATA(lv_cgst) WITH KEY itm_number = gs_item-itm_number
                                                                                                cond_type = 'JOCG' BINARY SEARCH.
                    IF sy-subrc = 0.
                      READ TABLE gt_condition TRANSPORTING cond_value INTO DATA(lv_sgst) WITH KEY itm_number = gs_item-itm_number
                                                                                                  cond_type = 'JOSG' BINARY SEARCH.
                      IF sy-subrc = 0.
                        IF lv_cgst-cond_value <> lv_sgst-cond_value.
                          lv_msg = |{ lv_msg } ; Cgst/Sgst Values are different in Sales Order for Material { <fs_sale_itm>-matnr }|.
                        ENDIF.
                      ENDIF.
                    ELSE.
                      lv_msg = |{ lv_msg } ; IGST or Cgst/Sgst not Maintained for Materials { <fs_sale_itm>-matnr }|.
                    ENDIF.
                  ENDIF.
                ENDIF.
*----------------------------------------------------------------------------------
              ENDLOOP.
            ENDIF.
*** If Error Messages is Present stoping the deliveries ***
            IF lv_msg IS NOT INITIAL.
              SHIFT lv_msg LEFT DELETING LEADING ','.
              APPEND VALUE #( order_id = gs_input-orderid
                              sales_no = l_vbeln
                              type     = 'E'
                              msg      = lv_msg ) TO gt_output.
*********************error log***********
              lo_errorlog_dms->log_entry_store(
                EXPORTING
                  type                = 12
                  status              = 10
                  dms_orderid         = gs_input-orderid
                  distributor         = l_salelog-distributor
                  plant               = l_salelog-plant
                  dealer              = l_salelog-kunag
                  msg                 = lv_msg ).
            ELSE.
              "Orderid Checks
              SELECT SINGLE * FROM zsd_scm_hd_dms INTO @DATA(lw_header) WHERE order_id = @gs_input-orderid.
              IF sy-subrc <> 0.
*delivery and PGI in single stretch
                lw_header-mandt      = sy-mandt.
                lw_header-order_id   = gs_input-orderid.
                lw_header-vbeln      = l_vbeln.
                lw_header-erdat      = sy-datum.
                lw_header-ernam      = sy-uname.
                lw_header-entry_time = sy-uzeit.
                lw_header-status     = c_initiated.
                lw_header-distributor = l_salelog-distributor.
                lw_header-dealer     = l_salelog-kunag.
*Delivery Process
                IF lw_header-status EQ c_initiated.
*sales Order lock checks
                  CALL FUNCTION 'ENQUEUE_EVVBAKE'
                    EXPORTING
                      mode_vbak      = 'E'
                      mandt          = sy-mandt
                      vbeln          = l_vbeln
                    EXCEPTIONS
                      foreign_lock   = 1
                      system_failure = 2
                      OTHERS         = 3.
                  IF sy-subrc <> 0.
                    lw_header-msgtyp = 'E'.
                    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
                      EXPORTING
                        msgid               = sy-msgid
                        msgnr               = sy-msgno
                        msgv1               = sy-msgv1
                        msgv2               = sy-msgv2
                        msgv3               = sy-msgv3
                        msgv4               = sy-msgv4
                      IMPORTING
                        message_text_output = lw_header-msg.
                  ELSE.
                    CLEAR: lv_vbeln,lv_msg.
                    CALL METHOD lo_object_cls->create_ob_delivery
                      EXPORTING
                        sales_document = l_vbeln
                        lgort          = c_lgort
                      IMPORTING
                        delivery_no    = lv_vbeln
                        return         = DATA(lt_return)
                        msg            = lv_msg.
                    IF lv_vbeln IS NOT INITIAL.
                      lw_header-delivery_no = lv_vbeln.
                      lw_header-status      = c_delivery.
                      lw_header-msg         = |Delivery Completed|.
** Delivery Picklist Smartforms
*                  CLEAR: lv_string,lv_errstr.
*                  CALL FUNCTION 'ZSD_PICKLIST_PDF_GENERATE_DMS'
*                    EXPORTING
*                      im_vbeln       = lv_vbeln
*                    IMPORTING
*                      ep_xstring     = lv_string
*                      error_message  = lv_errstr
*                    EXCEPTIONS
*                      so_num_missing = 1
*                      OTHERS         = 2.
*                  IF sy-subrc = 0.
*
*                  ENDIF.
                    ELSE.
                      lw_header-msgtyp = 'E'.
                      lw_header-msg = lv_msg.
*********************error log***********
                      lo_errorlog_dms->log_entry_store(
                        EXPORTING
                          type                = 12
                          status              = 10
                          dms_orderid         = gs_input-orderid
                          distributor         = l_salelog-distributor
                          plant               = l_salelog-plant
                          dealer              = l_salelog-kunag
                          msg                 = lv_msg ).
                    ENDIF.
                  ENDIF.
                ENDIF.
*Post Goods Issue Process
                IF lw_header-status EQ c_delivery AND lw_header-delivery_no IS NOT INITIAL.
                  REFRESH: lt_return. CLEAR: lv_mblnr,lv_msg.
                  CALL METHOD lo_object_cls->post_goods_issue
                    EXPORTING
                      delivery_no = lw_header-delivery_no
                      lgort       = c_lgort
                    IMPORTING
                      pgi_no      = lv_mblnr
                      return      = lt_return
                      msg         = lv_msg.
                  IF lv_mblnr IS NOT INITIAL.
                    lw_header-gdsmvt_no = lv_mblnr.
                    lw_header-dcomp     = abap_true.
                    lw_header-status    = c_pgi.
                    lw_header-msgtyp    = 'S'.
                    lw_header-msg       = |PGI is Created|.
                  ELSE.
*********************error log***********
                    lo_errorlog_dms->log_entry_store(
                      EXPORTING
                        type                = 13                 " Apiname
                        status              = 10                 " Long String to Store Big XML Documents
                        dms_orderid         = gs_input-orderid             " Long String to Store Big XML Documents
                        distributor         = l_salelog-distributor                 " Customer Number
                        plant               = l_salelog-plant           " Plant
                        dealer              = l_salelog-kunag                 " Customer Number
                        msg                 = lv_msg ).
                    lw_header-msgtyp = 'E'.
                    lw_header-msg = lv_msg.
                  ENDIF.
                ENDIF.
                MODIFY zsd_scm_hd_dms FROM lw_header.
                APPEND VALUE #( order_id = gs_input-orderid
                                sales_no = l_vbeln
                                delno    = lw_header-delivery_no
                                pgino    = lw_header-gdsmvt_no
                                type     = lw_header-msgtyp
                                msg      = lw_header-msg ) TO gt_output.
** Already Entries Present in log will check the status **
              ELSE.
*Already Delivery Completed Flag is set
                IF lw_header-dcomp EQ abap_true.
** Delivery Picklist Smartforms
*              CLEAR: lv_errstr.
*              CALL FUNCTION 'ZSD_PICKLIST_PDF_GENERATE_DMS'
*                EXPORTING
*                  im_vbeln       = lw_header-delivery_no
*                IMPORTING
*                  ep_xstring     = lv_string
*                  error_message  = lv_errstr
*                EXCEPTIONS
*                  so_num_missing = 1
*                  OTHERS         = 2.
*              IF sy-subrc = 0.
*
*              ENDIF.
                  APPEND VALUE #( order_id = lw_header-order_id
                                  sales_no = lw_header-vbeln
                                  delno    = lw_header-delivery_no
                                  pgino    = lw_header-gdsmvt_no
                                  type     = lw_header-msgtyp
                                  msg      = lw_header-msg ) TO gt_output.
                ELSE.
*Delivery Process
                  IF lw_header-status EQ c_initiated.
*sales Order lock checks
                    CALL FUNCTION 'ENQUEUE_EVVBAKE'
                      EXPORTING
                        mode_vbak      = 'E'
                        mandt          = sy-mandt
                        vbeln          = l_vbeln
                      EXCEPTIONS
                        foreign_lock   = 1
                        system_failure = 2
                        OTHERS         = 3.
                    IF sy-subrc <> 0.
                      lw_header-msgtyp = 'E'.
                      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
                        EXPORTING
                          msgid               = sy-msgid
                          msgnr               = sy-msgno
                          msgv1               = sy-msgv1
                          msgv2               = sy-msgv2
                          msgv3               = sy-msgv3
                          msgv4               = sy-msgv4
                        IMPORTING
                          message_text_output = lw_header-msg.
                    ELSE.
                      CLEAR: lv_vbeln,lv_msg.
                      CALL METHOD lo_object_cls->create_ob_delivery
                        EXPORTING
                          sales_document = l_vbeln
                          lgort          = c_lgort
                        IMPORTING
                          delivery_no    = lv_vbeln
                          return         = lt_return
                          msg            = lv_msg.
                      IF lv_vbeln IS NOT INITIAL.
                        lw_header-delivery_no = lv_vbeln.
                        lw_header-status = c_delivery.
                        lw_header-msg = |Delivery Completed|.
** Delivery Picklist Smartforms
*                    CLEAR: lv_errstr.
*                    CALL FUNCTION 'ZSD_PICKLIST_PDF_GENERATE_DMS'
*                      EXPORTING
*                        im_vbeln       = lv_vbeln
*                      IMPORTING
*                        ep_xstring     = lv_string
*                        error_message  = lv_errstr
*                      EXCEPTIONS
*                        so_num_missing = 1
*                        OTHERS         = 2.
*                    IF sy-subrc = 0.
*
*                    ENDIF.
                      ELSE.
                        lw_header-msgtyp = 'E'.
                        lw_header-msg = lv_msg.
*********************error log***********
                        lo_errorlog_dms->log_entry_store(
                          EXPORTING
                            type                = 12
                            status              = 10
                            dms_orderid         = gs_input-orderid
                            distributor         = l_salelog-distributor
                            plant               = l_salelog-plant
                            dealer              = l_salelog-kunag
                            msg                 = lv_msg ).
                      ENDIF.
                    ENDIF.
                  ENDIF.
*Post Goods Issue Process
                  IF lw_header-status EQ c_delivery AND lw_header-delivery_no IS NOT INITIAL.
                    REFRESH: lt_return. CLEAR: lv_mblnr,lv_msg.
                    CALL METHOD lo_object_cls->post_goods_issue
                      EXPORTING
                        delivery_no = lw_header-delivery_no
                        lgort       = c_lgort
                      IMPORTING
                        pgi_no      = lv_mblnr
                        return      = lt_return
                        msg         = lv_msg.
                    IF lv_mblnr IS NOT INITIAL.
                      lw_header-gdsmvt_no = lv_mblnr.
                      lw_header-dcomp = abap_true.
                      lw_header-status = c_pgi.
                      lw_header-msgtyp = 'S'.
                      lw_header-msg   = |PGI is Created|.
                    ELSE.
                      lw_header-msgtyp = 'E'.
                      lw_header-msg = lv_msg.
*********************error log***********
                      lo_errorlog_dms->log_entry_store(
                        EXPORTING
                          type                = 13
                          status              = 10
                          dms_orderid         = gs_input-orderid
                          distributor         = l_salelog-distributor
                          plant               = l_salelog-plant
                          dealer              = l_salelog-kunag
                          msg                 = lv_msg ).
                    ENDIF.
                  ENDIF.
                  MODIFY zsd_scm_hd_dms FROM lw_header.
                  APPEND VALUE #( order_id = gs_input-orderid
                                  sales_no = l_vbeln
                                  delno    = lw_header-delivery_no
                                  pgino    = lw_header-gdsmvt_no
                                  type     = lw_header-msgtyp
                                  msg      = lw_header-msg ) TO gt_output.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
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

*Input Entry in Log Table
      CALL METHOD lo_log_upd->log_entry_store
        EXPORTING
          apiname         = 'DMS_DELIVERY'
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

      v_jsonload = | { '[' } { lv_body } { ']' } |.
*Set JSON Content-Type
      CALL METHOD server->response->set_header_field( name = 'Content-Type' value = 'application/json; charset=UTF-8' ).
*Set Response Data
      CALL METHOD server->response->set_cdata( data = v_jsonload ).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
