class ZCL_API_SALESORDER_SIMULATION definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .

  methods GST_CHECK
    importing
      !DISTRIBUTOR type ZDIST
      !CUSTOMER type KUNNR
      !CUST_GST type STCD3
    exporting
      !STATUS type CHAR10
      !RETURN type STRING .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_SALESORDER_SIMULATION IMPLEMENTATION.


  METHOD gst_check.
    DATA : lo_main TYPE REF TO zcl_dms_einvoice_process.
    CREATE OBJECT lo_main.
    CALL METHOD lo_main->get_gst_token
      EXPORTING
        distributor_code     = distributor
      IMPORTING
        authentication_token = DATA(authentication_token)
        subscription_id      = DATA(subscription_id)
        auth_token           = DATA(auth_token)
        session_key          = DATA(session_key)
        user_name            = DATA(user_name)
        gstin                = DATA(gstin)
        return               = return.

    IF return IS INITIAL.
      CALL METHOD lo_main->get_gst_details
        EXPORTING
          distributor_code     = distributor
          costomer_code        = customer
          authentication_token = authentication_token
          subscription_id      = subscription_id
          auth_token           = auth_token
          session_key          = session_key
          user_name            = user_name
          gstin                = gstin
          customer_gstin       = cust_gst
        IMPORTING
          zgst_msg             = DATA(lw_msg)
          return               = return.

      CONDENSE : lw_msg-status.
      status = lw_msg-status.
    ENDIF.

  ENDMETHOD.


  METHOD if_http_extension~handle_request.

***** Purpose of this API is for Sales Order simulation
*         from distributor to Dealers - DMS*****
    "Created by: pandiarajan
    "Created on: 25.09.2024
    "Reference by: Ramakrishnan J & Praveen Kumar
    "-----------------------------------------------------------------------------------*
***** Changing the customer Payment terms fetching logic from KNVV instead of KNB1


    TYPES: BEGIN OF ty_msg,
             ordid       TYPE zorder_id,
             distributor TYPE kunnr,
             dealer      TYPE kunnr,
             salesorder  TYPE vbeln,
             status      TYPE bapi_mtype,
             message     TYPE string,
             creditstat  TYPE cmgst,
*             details    TYPE zsd_st_direct_so_res_dms,
*             form       TYPE string,
           END OF ty_msg.
    DATA: ls_details TYPE zsd_st_direct_so_res_dms.
    DATA: gs_input TYPE zsd_st_direct_sales_order_dms.
    DATA: lv_string    TYPE string,
          lv_errstring TYPE string.
    DATA: gt_response TYPE TABLE OF ty_msg.

    DATA: lv_body TYPE string.
    DATA: v_jsonload TYPE string.
    DATA: lv_msgtxt TYPE string.
    DATA: lv_data TYPE string.
    DATA: lv_type TYPE char1.
    DATA: lv_msg TYPE string.
    DATA: lr_data TYPE REF TO data.
    DATA: lt_sale_itm TYPE STANDARD TABLE OF zsd_sale_it_dms,
          lw_sale_itm TYPE zsd_sale_it_dms,
          lw_sale_hdr TYPE zsd_sale_hd_dms.
**********normal api log**********
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
     data         = gs_input ). "lr_data ).


    IF gs_input IS NOT INITIAL.
      CLEAR: lw_sale_hdr.
      lw_sale_hdr-ordid    = gs_input-orderid. "Order ID
*Multiple click on front end so avoiding duplicates
      SELECT SINGLE * FROM zsd_splordid_dms
        INTO @DATA(l_dupl_ordid)
        WHERE orderid = @lw_sale_hdr-ordid.
      IF sy-subrc = 0.
        lv_msg = |Already Order ID in Processing , { lv_msg }|.
      ENDIF.
      CONDENSE : gs_input-distance.
      lw_sale_hdr-auart    = gs_input-ordertype. "Order Type
      lw_sale_hdr-bukrs    = gs_input-compcode. "Company Code
      lw_sale_hdr-vkorg    = gs_input-salesorg. "Sales Organization  SDMS
      lw_sale_hdr-vtweg    = gs_input-distchnl. " DIstrubtion CHannel
      lw_sale_hdr-spart    = gs_input-division. " Division
      lw_sale_hdr-distributor = gs_input-distributor. " distributor
      lw_sale_hdr-plant       = gs_input-plant. " distributor
      lw_sale_hdr-sodate      = gs_input-sodate. " sales order created date
      lw_sale_hdr-kunag       = gs_input-soldtoparty. "Distributor OR Direct Dealer
      lw_sale_hdr-kunwg       = gs_input-shiptoparty. "Ship to Party
      lw_sale_hdr-vkbur       = gs_input-salesoffice. "Sales Office
      lw_sale_hdr-distance    = gs_input-distance.
      lw_sale_hdr-xblnr       =  gs_input-refdocno." VALUE #( gt_input[ 1 ]-ref_doc_no  OPTIONAL ).
      lw_sale_hdr-erdat       =  sy-datum.
      lw_sale_hdr-erzet       =  sy-uzeit.
      "Order ID Existence Checks
      DATA(l_orderid) = |{ gs_input-orderid }|.
      SELECT SINGLE ordid,vbeln FROM zsd_sale_hd_dms
                          INTO @DATA(l_sales_hdr)
                          WHERE ordid = @l_orderid
                          AND   status = 'S'.
      IF sy-subrc = 0.
        lv_msg = |Already DMS Order ID Available , { lv_msg }|.
        APPEND VALUE #( ordid       = lw_sale_hdr-ordid
                        status      = 'S'
                        distributor = gs_input-distributor
                        dealer      = gs_input-soldtoparty
                        message     = | { lv_msg } |
                        salesorder  = l_sales_hdr-vbeln ) TO gt_response.
      ELSE.
        "Sales Order type Check
        SELECT SINGLE auart FROM tvak
           INTO @DATA(l_auart)
           WHERE auart = @lw_sale_hdr-auart.
        IF sy-subrc <> 0.
          lv_msg = |Order Type is Incorrect , { lv_msg }|.
        ENDIF.

*******************check the distance only numeric*************

        DATA : c_numeric TYPE string VALUE ' .,0123456789'.
        IF gs_input-distance IS NOT INITIAL.
          IF gs_input-distance CN c_numeric.
            lv_msg = |Invalid distance field , { lv_msg }|.
          ENDIF.
        ENDIF.
*      IF lw_sale_hdr-auart NE 'YDMS'.
*        lv_msg = |Order Type is Incorrect , { lv_msg }|.
*      ENDIF.
        "Company Code Checks
        SELECT SINGLE bukrs FROM t001
                            INTO @DATA(l_bukrs)
                            WHERE bukrs = @lw_sale_hdr-bukrs.
        IF sy-subrc <> 0.
          lv_msg = |Company Code is Incorrect , { lv_msg }|.
        ENDIF.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lw_sale_hdr-kunag
          IMPORTING
            output = lw_sale_hdr-kunag.

        "Sold to Party Code Check
        SELECT SINGLE kunnr,regio,stcd3 FROM kna1
          INTO @DATA(l_kunag)
          WHERE kunnr = @lw_sale_hdr-kunag.
        IF sy-subrc <> 0.
          lv_msg = |Sold-to-party is Incorrect , { lv_msg }|.
        ENDIF.
        "Sold to Party Code Check & Payment terms checks
        SELECT SINGLE kunnr,zterm FROM knvv
          INTO @DATA(l_payterms)
          WHERE kunnr = @lw_sale_hdr-kunag
          AND vkorg = @lw_sale_hdr-vkorg
          AND vtweg = @lw_sale_hdr-vtweg
          AND spart = @lw_sale_hdr-spart.
        IF sy-subrc <> 0.
          lv_msg = |Sold-to-party is Not extended to Sales Area { lw_sale_hdr-vkorg } - { lw_sale_hdr-vtweg } - { lw_sale_hdr-spart } , { lv_msg }|.
        ENDIF.
        IF lw_sale_hdr-plant NE lw_sale_hdr-vkbur.
          lv_msg = |Sales office & plant is mismatch , { lv_msg }|.
        ENDIF.
***************customer block check***********
        SELECT SINGLE a~loevm,a~nodel,b~aufsd,b~loevm AS loevm2
          FROM kna1 AS a
          INNER JOIN knvv AS b
          ON a~kunnr = b~kunnr
          INTO @DATA(ls_block)
          WHERE a~kunnr = @lw_sale_hdr-kunag
          AND b~vkorg   = @lw_sale_hdr-vkorg
          AND b~vtweg   = @lw_sale_hdr-vtweg
          AND b~spart   = @lw_sale_hdr-spart.
        IF ls_block-loevm = 'X' OR ls_block-nodel = 'X' OR ls_block-aufsd = 'X' OR ls_block-loevm2 = 'X'.
          lv_msg = |{ lw_sale_hdr-kunag } - Customer is blocked , { lv_msg }|.
        ENDIF.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lw_sale_hdr-distributor
          IMPORTING
            output = lw_sale_hdr-distributor.
        "distributor code check
        IF lw_sale_hdr-distributor IS INITIAL.
          lv_msg = |distributor is missing , { lv_msg }|.
        ENDIF.
        SELECT SINGLE kunnr,regio FROM kna1
          INTO @DATA(l_distr)
          WHERE kunnr = @lw_sale_hdr-distributor
          AND   werks = @lw_sale_hdr-plant.
        IF sy-subrc <> 0.
          lv_msg = |distributor & plant is mismatch , { lv_msg }|.
        ENDIF.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lw_sale_hdr-kunwg
          IMPORTING
            output = lw_sale_hdr-kunwg.
        "Ship to Party Code Check
        SELECT SINGLE kunnr FROM kna1
          INTO @DATA(l_kunwg)
          WHERE kunnr = @lw_sale_hdr-kunwg.
        IF sy-subrc <> 0.
          lv_msg = |Ship-to-party is Incorrect , { lv_msg }|.
        ENDIF.
        "Ship to Party Code Check
        SELECT SINGLE kunnr FROM knvv
          INTO @DATA(l_kunwe)
          WHERE kunnr = @lw_sale_hdr-kunwg
          AND vkorg = @lw_sale_hdr-vkorg
          AND vtweg = @lw_sale_hdr-vtweg
          AND spart = @lw_sale_hdr-spart.
        IF sy-subrc <> 0.
          lv_msg = |Ship-to-party is Not extended to Sales Area { lw_sale_hdr-vkorg } - { lw_sale_hdr-vtweg } - { lw_sale_hdr-spart } , { lv_msg }|.
        ENDIF.
        "Sales Organization Check
        SELECT SINGLE vkorg
          FROM tvko INTO @DATA(l_saleorg)
          WHERE vkorg =  @lw_sale_hdr-vkorg.
        IF sy-subrc <> 0.
          lv_msg = |Sales Organisation is Incorrect , { lv_msg }|.
        ENDIF.
        "Distribution Channel Check
        SELECT SINGLE vtweg
          FROM tvtw INTO @DATA(l_distchnl)
          WHERE  vtweg = @lw_sale_hdr-vtweg.
        IF sy-subrc <> 0.
          lv_msg = |Distribution Channel is Incorrect , { lv_msg }|.
        ENDIF.
        "Division Check
        SELECT SINGLE spart FROM tspa
          INTO @DATA(l_division)
          WHERE spart = @lw_sale_hdr-spart.
        IF sy-subrc <> 0.
          lv_msg = |Division is Incorrect , { lv_msg }|.
        ENDIF.
        lw_sale_hdr-plant = gs_input-plant.

        "Sales Office
        SELECT SINGLE vkbur FROM tvbur
                            INTO @DATA(l_salesoff)
                            WHERE vkbur = @lw_sale_hdr-vkbur.
        IF sy-subrc NE 0.
          lv_msg = |Sales Office is Incorrect , { lv_msg }|.
        ENDIF.
        "Plant Check
        SELECT SINGLE werks,regio FROM t001w
          INTO @DATA(l_plant)
          WHERE werks = @lw_sale_hdr-plant.
        IF sy-subrc <> 0.
          lv_msg = |Plant Code is Incorrect , { lv_msg }|.
        ENDIF.

**********************only for DMS1 company code*********
        IF lw_sale_hdr-bukrs = 'DMS1'.
***********check plant & business area********
          SELECT SINGLE werks FROM t134g INTO @DATA(ls_area) WHERE werks = @lw_sale_hdr-plant
                                                             AND   gsber = @lw_sale_hdr-plant.
          IF sy-subrc NE 0.
            lv_msg = |Plant - { lw_sale_hdr-plant } & business area Mismatch , { lv_msg }|.
          ENDIF.
***********check plant & Valuation area********
          SELECT SINGLE bwkey FROM t134h INTO ls_area WHERE bwkey = lw_sale_hdr-plant
                                                      AND   gsber = lw_sale_hdr-plant.
          IF sy-subrc NE 0.
            lv_msg = |Plant - { lw_sale_hdr-plant } & Valuation area Mismatch , { lv_msg }|.
          ENDIF.
********************check sales office config for every distributor - business area**************
          SELECT SINGLE gsber FROM zsku_ba
                              INTO @DATA(gsber)
                              WHERE vkorg = @lw_sale_hdr-vkorg
                              AND   vkbur = @lw_sale_hdr-plant.
          IF sy-subrc NE 0.
            lv_msg = |Distributor sales office not maintained in table - ZSKU_BA - { lw_sale_hdr-distributor }-{ lw_sale_hdr-plant } ,{ lv_msg }|.
          ENDIF.
        ENDIF.
****************check region of plant & distributor***********
        IF l_plant-regio NE l_distr-regio.
          lv_msg = |Distributor { lw_sale_hdr-distributor } & Plant - { lw_sale_hdr-plant } region Mismatch , { lv_msg }|.
        ENDIF.

*******************check the partner function for mapped distributor***********
        SELECT SINGLE * FROM knvp INTO @DATA(ls_knvp) WHERE kunnr = @lw_sale_hdr-kunag
                                                      AND   vkorg = @lw_sale_hdr-vkorg
                                                      AND   vtweg = @lw_sale_hdr-vtweg
                                                      AND   spart = @lw_sale_hdr-spart
                                                      AND   parvw = 'SK'.
        IF lw_sale_hdr-distributor <> ls_knvp-kunn2.
          lv_msg = |Given distributor { lw_sale_hdr-distributor } is mismatch with partner function SK - { ls_knvp-kunn2 }|.
        ENDIF.

****************************customer block check*****************
        SELECT SINGLE * FROM zcust_blk_chk INTO @DATA(ls_blk)
                                           WHERE bukrs = @lw_sale_hdr-bukrs
                                           AND   vkorg = @lw_sale_hdr-vkorg
                                           AND   kunnr = @lw_sale_hdr-kunag
                                           AND   block = @abap_true.
        IF sy-subrc = 0.
          lv_msg = |Customer - { lw_sale_hdr-kunag } is blocked, { lv_msg }|.
        ENDIF.

*************fetch the gst condition*******
        SELECT a~kschl,a~wkreg,a~regio,a~matnr,a~knumh,b~loevm_ko
                                             FROM a709 AS a
                                             INNER JOIN konp AS b ON a~knumh EQ b~knumh
                                             INTO TABLE @DATA(lt_cond)
                                             FOR ALL ENTRIES IN @gs_input-item
                                             WHERE wkreg = @l_plant-regio
                                             AND   regio = @l_kunag-regio
                                             AND   matnr = @gs_input-item-material
                                             AND   datbi GE @sy-datum.
*      IF l_kunag-regio = 'KL'.
******************fetch the ZPRO price condition( only for kerala )************
*        SELECT a~matnr,b~loevm_ko FROM a832 AS a
*                                  INNER JOIN konp AS b ON a~knumh = b~knumh
*                                  INTO TABLE @DATA(lt_zpr0)
*                                  FOR ALL ENTRIES IN @gs_input-item
*                                  WHERE a~kschl    = 'ZPR0'
*                                  AND   a~regio    = @l_kunag-regio
*                                  AND   a~matnr    = @gs_input-item-material
*                                  AND   a~datbi    GE @sy-datum
*                                  AND   b~loevm_ko = @abap_false.
*        IF sy-subrc = 0.
*          SORT : lt_zpr0 BY matnr.
*        ENDIF.
*      ELSE.
***********pricing condition base on company code*************
        IF lw_sale_hdr-bukrs = 'DMS1'.
          DATA(lv_price) = 'ZPR0'.
        ELSE.
          lv_price = 'PRAP'.
        ENDIF.
*****************fetch the ZPRO price condition others************
        SELECT a~matnr,b~loevm_ko FROM a304 AS a
                               INNER JOIN konp AS b ON a~knumh = b~knumh
                               INTO TABLE @DATA(lt_zpr0)
                               FOR ALL ENTRIES IN @gs_input-item
                               WHERE a~kschl = @lv_price
                               AND   a~vkorg = @lw_sale_hdr-vkorg
                               AND   a~matnr = @gs_input-item-material
                               AND   a~datbi GE @sy-datum
                               AND   b~loevm_ko = @abap_false.
        IF sy-subrc = 0.
          SORT : lt_zpr0 BY matnr.
        ENDIF.
*      ENDIF.

***********************check the internal price************
        SELECT matnr,bwkey,stprs FROM mbew INTO TABLE @DATA(lt_mbew) WHERE bwkey = @gs_input-plant.
        IF sy-subrc = 0.
          SORT : lt_mbew BY matnr.
        ENDIF.

**************check the GST active or not****************
*        IF l_kunag-stcd3 IS NOT INITIAL.
*          SELECT SINGLE * FROM zdist_einv_dtls INTO @DATA(ls_einv)
*                                               WHERE distributor = @lw_sale_hdr-distributor.
*          IF sy-subrc = 0.
*            SELECT SINGLE * FROM zcust_gst_chk INTO @DATA(ls_gst)
*                                               WHERE kunnr  = @lw_sale_hdr-kunag.
*            IF sy-subrc EQ 0.
*              CONDENSE : gst_status.
*              IF gst_status NE 'ACT'.
*                IF gst_status = 'CNL'.
*                  lv_msg = |Customer - { lw_sale_hdr-kunag } GST No - { l_kunag-stcd3 } is Cancelled, { lv_msg }|.
*                ELSEIF gst_status = 'SUS'.
**                lv_msg = |Customer - { lw_sale_hdr-kunag } GST No - { l_kunag-stcd3 } is Suspended, { lv_msg }|.
*                ELSE.
*                  lv_msg = |Customer - { lw_sale_hdr-kunag } GST No - { l_kunag-stcd3 } is invalid, { lv_msg }|.
*                ENDIF.
*              ENDIF.
*            ELSE.
*              lv_msg = |Customer - { lw_sale_hdr-kunag } GST Check not Maintained, { lv_msg }|.
*            ENDIF.
*          ENDIF.
*        ENDIF.

*****************************Block the process for distributor or customer*****************
        DATA : lobj_check TYPE REF TO zcl_common_check.
        CREATE OBJECT lobj_check.
********************for distributor checking****************
        lobj_check->dms_process_stop(
          EXPORTING
            kunnr        = lw_sale_hdr-distributor
            process_type = 'SO'
          IMPORTING
            message      = lv_msg ).
********************for customer checking****************
        lobj_check->dms_process_stop(
          EXPORTING
            kunnr        = lw_sale_hdr-kunag
            process_type = 'SO'
          IMPORTING
            message      = lv_msg ).

******************GST validations***********
        IF l_kunag-stcd3 IS NOT INITIAL.
          SELECT SINGLE * FROM zdist_einv_dtls INTO @DATA(ls_einv)
                                               WHERE distributor = @lw_sale_hdr-distributor.
          IF sy-subrc = 0.
            CALL METHOD gst_check
              EXPORTING
                distributor = lw_sale_hdr-distributor
                customer    = lw_sale_hdr-kunag
                cust_gst    = l_kunag-stcd3
              IMPORTING
                status      = DATA(gst_status)
                return      = DATA(lv_return).
            CONDENSE : gst_status.
            IF lv_return IS INITIAL.
              IF gst_status NE 'ACT'.
                IF gst_status = 'CNL'.
                  lv_msg = |Customer - { lw_sale_hdr-kunag } GST No - { l_kunag-stcd3 } is Cancelled, { lv_msg }|.
                ELSEIF gst_status = 'SUS'.
*                lv_msg = |Customer - { l_salelog-kunag } GST No - { l_kunag-stcd3 } is Suspended, { lv_msg }|.
                ELSE.
                  lv_msg = |Customer - { lw_sale_hdr-kunag } GST No - { l_kunag-stcd3 } is invalid, { lv_msg }|.
                ENDIF.
              ENDIF.
            ELSE.
              lv_msg = | { lv_return } , { lv_msg }|.
            ENDIF.
          ENDIF.
        ENDIF.

************************Item data checking process*****************

        LOOP AT gs_input-item INTO DATA(lw_data).
          DATA(lt_matnr) = gs_input-item.
          DELETE lt_matnr WHERE material NE lw_data-material.
          DESCRIBE TABLE lt_matnr LINES DATA(lv_lines).
          IF lv_lines GT 1.
            lv_msg = |Same Material { lw_data-material } is repeated More than Once , { lv_msg }|.
          ENDIF.
          lw_sale_itm-ordid     = gs_input-orderid.
          lw_sale_itm-matnr     = lw_data-material.
          lw_sale_itm-lfimg     = lw_data-qty.
          lw_sale_itm-meins     = lw_data-uom.
          lw_sale_itm-werks     = lw_sale_hdr-plant.
          lw_sale_itm-disvalue1 = lw_data-disvalue1.
          lw_sale_itm-disvalue2 = lw_data-disvalue2.
          lw_sale_itm-disper1   = lw_data-disper1.
          lw_sale_itm-disper2   = lw_data-disper2.
          lw_sale_itm-cdate     = sy-datum.
          lw_sale_itm-ctime     = sy-uzeit.
          APPEND lw_sale_itm TO lt_sale_itm.

          SELECT SINGLE matnr, zterm,tragr FROM mara INTO @DATA(l_mara)
                WHERE matnr = @lw_sale_itm-matnr.
          IF sy-subrc <> 0.
            lv_msg = |Material { lw_sale_itm-matnr } is not found , { lv_msg }|.
          ELSE.
            IF lw_sale_itm-lfimg  = 0.
              lv_msg = |Maretial { lw_sale_itm-matnr } quantity is zero , { lv_msg }|.
            ENDIF.

            IF l_mara-tragr IS INITIAL.
              lv_msg = |Transportation Group Missing - Check with R&D Team { lw_sale_itm-matnr } , { lv_msg }|.
            ELSE.
              IF l_mara-tragr <> '0001'.
                lv_msg = |Transportation Group Incorrect - Check with R&D Team { lw_sale_itm-matnr }  , { lv_msg }|.
              ENDIF.
            ENDIF.
          ENDIF.
*** -------------------------------------------------------------------------------
          SELECT SINGLE matnr ,ladgr FROM marc INTO @DATA(l_material) WHERE matnr = @lw_sale_itm-matnr
                                                                      AND werks   = @lw_sale_itm-werks.
          IF sy-subrc <> 0.
            lv_msg = |Maretial { lw_sale_itm-matnr } Not Maintained in Plant { lw_sale_itm-werks } , { lv_msg }|.
          ELSE.
*********************Loading group check*******************************************
            IF l_material-ladgr IS INITIAL.
              lv_msg = |Loading Group Missing - Check with R&D Team { lw_sale_itm-matnr } , { lv_msg }|.
            ELSE.
              IF l_material-ladgr <> '0004'.
                lv_msg = |Loading Group Incorrect - Check with R&D Team { lw_sale_itm-matnr } , { lv_msg }|.
              ENDIF.
            ENDIF.
          ENDIF.
************************material group1 check***********************************
          SELECT SINGLE matnr ,mvgr1 FROM mvke INTO @DATA(l_mvke) WHERE matnr = @lw_sale_itm-matnr
                                                                  AND   vkorg = @lw_sale_hdr-vkorg
                                                                  AND   vtweg = @lw_sale_hdr-vtweg.
          IF sy-subrc <> 0.
            lv_msg = |Maretial { lw_sale_itm-matnr } Not Maintained in Sales org { lw_sale_hdr-vkorg }  & Dis.Channel { lw_sale_hdr-vtweg } , { lv_msg }|.
          ELSE.
            IF l_mvke-mvgr1 IS INITIAL.
              lv_msg = |Material GRP 1 Missing - { lw_sale_itm-matnr } , { lv_msg }|.
            ENDIF.
          ENDIF.
*----------------------------------------------------------------------------------
**********gst check***********
          IF l_distr-regio = l_kunag-regio.
            READ TABLE lt_cond TRANSPORTING NO FIELDS WITH KEY kschl = 'JOCG'
                                                               wkreg = l_plant-regio
                                                               regio = l_kunag-regio
                                                               matnr = lw_sale_itm-matnr
                                                               loevm_ko = abap_false.
            IF sy-subrc NE 0.
              lv_msg = |Material JOCG is not maintained { lw_sale_itm-matnr } , { lv_msg }|.

            ENDIF.
            READ TABLE lt_cond TRANSPORTING NO FIELDS WITH KEY kschl = 'JOSG'
                                                               wkreg = l_plant-regio
                                                               regio = l_kunag-regio
                                                               matnr = lw_sale_itm-matnr
                                                               loevm_ko = abap_false.
            IF sy-subrc NE 0.
              lv_msg = |Material JOSG is not maintained { lw_sale_itm-matnr } , { lv_msg }|.
            ENDIF.
          ELSE.
            READ TABLE lt_cond TRANSPORTING NO FIELDS WITH KEY kschl = 'JOIG'
                                                               wkreg = l_plant-regio
                                                               regio = l_kunag-regio
                                                               matnr = lw_sale_itm-matnr
                                                               loevm_ko = abap_false.
            IF sy-subrc NE 0.
              lv_msg = |Material JOIG is not maintained { lw_sale_itm-matnr } , { lv_msg }|.
            ENDIF.
          ENDIF.
** *********************ZPR0 checking*************
          READ TABLE lt_zpr0 TRANSPORTING NO FIELDS WITH KEY matnr = lw_sale_itm-matnr BINARY SEARCH.
          IF sy-subrc NE 0.
            lv_msg = |Material { lv_price } is not maintained { lw_sale_itm-matnr } , { lv_msg }|.
          ENDIF.

***********************check the internal price************
          READ TABLE lt_mbew INTO DATA(ls_mbew) WITH KEY matnr = lw_sale_itm-matnr BINARY SEARCH.
          IF sy-subrc = 0 AND ls_mbew-stprs IS INITIAL.
            lv_msg = |Material internal price - VPRS is not maintained { lw_sale_itm-matnr } , { lv_msg }|.
          ENDIF.

        ENDLOOP.

        IF lv_msg IS INITIAL.
          DATA : v_vbeln TYPE vbeln_va.
          DATA: lt_return TYPE STANDARD TABLE OF bapiret2.
          DATA(l_orderid_chk) = VALUE zsd_splordid_dms( mandt   = sy-mandt
                                                orderid = lw_sale_hdr-ordid ).
          INSERT zsd_splordid_dms FROM l_orderid_chk.
          CALL FUNCTION 'ZSD_SALES_ORDER_SIMULATE_DMS'
            EXPORTING
              im_header     = lw_sale_hdr
              im_item       = gs_input-item
            IMPORTING
              salesorder_no = v_vbeln
              message       = lv_msgtxt
              msg_type      = lv_type.

          IF lv_type = 'S'.

            lw_sale_hdr-vbeln = v_vbeln.
            lw_sale_hdr-status = 'S'.
            lw_sale_hdr-msg = |Proceed to Sales Order creation|.
            CLEAR ls_details.
            SELECT SINGLE kunnr,
                          auart,
                          erdat,
                          netwr,
                          knumv,
                          cmgst AS condition_no FROM vbak INTO @ls_details-header WHERE vbeln = @lw_sale_hdr-vbeln.
            SELECT posnr,
                   matnr,
                   arktx,
                   kwmeng,
                   meins,
                   netwr,
                   mwsbp FROM vbap
                         INTO TABLE @ls_details-item
                         WHERE vbeln = @lw_sale_hdr-vbeln.
            SELECT kposn AS posnr,
                   kschl,
                   kwert AS value FROM prcd_elements
                                  INTO TABLE @ls_details-condition
                                  WHERE knumv = @ls_details-header-condition_no.
            DELETE ls_details-condition WHERE value = '0'.
*** Smartform Output for Created Sales Order ****
            CLEAR: lv_string,lv_errstring.
            APPEND VALUE #( ordid       = lw_sale_hdr-ordid
                            salesorder  = lw_sale_hdr-vbeln
                            distributor = gs_input-distributor
                            dealer      = gs_input-soldtoparty
                            status      = 'S'
                            message     = lw_sale_hdr-msg
                            creditstat  = ls_details-header-cmgst
*                          details    = ls_details
*                          form       = lv_string
                             ) TO gt_response.
          ELSE.
            lw_sale_hdr-status = 'E'.
            lw_sale_hdr-msg = lv_msgtxt.
            APPEND VALUE #( ordid       = lw_sale_hdr-ordid
                            salesorder  = lw_sale_hdr-vbeln
                            distributor = gs_input-distributor
                            dealer      = gs_input-soldtoparty
                            status      = 'E'
                            message     = lw_sale_hdr-msg ) TO gt_response.
*********************error log***********
            lo_errorlog_dms->log_entry_store(
              EXPORTING
                type                = 11                 " Apiname
                status              = 10                 " Long String to Store Big XML Documents
                dms_orderid         = gs_input-orderid                 " Long String to Store Big XML Documents
                distributor         = gs_input-distributor                 " Customer Number
                plant               = gs_input-plant                 " Plant
                dealer              = gs_input-soldtoparty                 " Customer Number
                msg                 = lv_msgtxt ).                " status of the dms sales order to eway bill ).
          ENDIF.
*once successfully processed deleting the Order ID from duplicaton CHECK table
          DELETE FROM zsd_splordid_dms WHERE orderid = lw_sale_hdr-ordid.
*          MODIFY zsd_sale_hd_dms FROM lw_sale_hdr .
*          MODIFY zsd_sale_it_dms FROM TABLE lt_sale_itm .
        ELSE.
          APPEND VALUE #( ordid       = lw_sale_hdr-ordid
                          status      = 'E'
                          distributor = gs_input-distributor
                          dealer      = gs_input-soldtoparty
                          message     = | { lv_msg } | ) TO gt_response.
*********************error log***********
          lo_errorlog_dms->log_entry_store(
            EXPORTING
              type                = 11                 " Apiname
              status              = 10                 " Long String to Store Big XML Documents
              dms_orderid         = gs_input-orderid                 " Long String to Store Big XML Documents
              distributor         = gs_input-distributor                 " Customer Number
              plant               = gs_input-plant                 " Plant
              dealer              = gs_input-soldtoparty                 " Customer Number
              msg                 = lv_msg ).                " status of the dms sales order to eway bill ).
        ENDIF.
      ENDIF.
    ENDIF.
    IF gt_response IS NOT INITIAL.
** serialize the output for response ***
      /ui2/cl_json=>serialize(
      EXPORTING
       data         =  gt_response
       pretty_name  = /ui2/cl_json=>pretty_mode-user
      RECEIVING
       r_json         = lv_body ).

*Output Entry in Log Table
      CALL METHOD lo_log_upd->log_entry_store
        EXPORTING
          apiname         = 'DMS_SORD_SIMULATION'
          ijson           = lv_data
          ojson           = lv_body
          distributor     = gs_input-distributor
          retailer        = gs_input-soldtoparty
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
