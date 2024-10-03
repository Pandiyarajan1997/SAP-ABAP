class ZCL_API_DIRECT_SALESORDER definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_API_DIRECT_SALESORDER IMPLEMENTATION.


  METHOD if_http_extension~handle_request.

***** Purpose of this API is for Creating Sales Order
*         from SPL to Direct Dealers and Distributors *****
    "Created by: Samsudeen M
    "Created on: 25.03.2023
    "Reference by: Ramakrishnan J & Praveen Kumar
    "-----------------------------------------------------------------------------------*
***** Changing the customer Payment terms fetching logic from KNVV instead of KNB1


    TYPES: BEGIN OF ty_msg,
             ordid      TYPE zorder_id,
             salesorder TYPE vbeln,
             status     TYPE bapi_mtype,
             message    TYPE string,
             creditstat TYPE cmgst,
             details    TYPE zsd_st_direct_sales_order_res,
             form       TYPE string,
           END OF ty_msg.
    DATA: ls_details TYPE zsd_st_direct_sales_order_res.
    DATA: gs_input TYPE zsd_st_direct_sales_order.
    DATA: lv_string    TYPE string,
          lv_errstring TYPE string.
    DATA: gt_response TYPE TABLE OF ty_msg.

    DATA: lv_body TYPE string.
    DATA: v_jsonload TYPE string.
    DATA: lv_msgtxt TYPE string.
    DATA: lv_data TYPE string.
    DATA: lr_data TYPE REF TO data.
    DATA: lt_sale_itm TYPE STANDARD TABLE OF zsd_spl_sale_it,
          lw_sale_itm TYPE zsd_spl_sale_it,
          lw_sale_hdr TYPE zsd_spl_sale_hd.

    DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
    CREATE OBJECT lo_log_upd.


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
      SELECT SINGLE * FROM zsd_splordid_chk
        INTO @DATA(l_dupl_ordid)
        WHERE orderid = @lw_sale_hdr-ordid.
      IF sy-subrc = 0.
        APPEND VALUE #( ordid = lw_sale_hdr-ordid
                        status = 'E'
                        message = |Already Order ID is Processing| ) TO gt_response.
      ENDIF.

      lw_sale_hdr-auart    = gs_input-ordertype. "Order Type
      lw_sale_hdr-bukrs    = gs_input-compcode. "Company Code
      lw_sale_hdr-vkorg    = gs_input-salesorg. "Sales Organization  SDMS
      lw_sale_hdr-vtweg    = gs_input-distchnl. " DIstrubtion CHannel
      lw_sale_hdr-spart    = gs_input-division. " Division
      lw_sale_hdr-kunag    = gs_input-soldtoparty. "Distributor OR Direct Dealer
      lw_sale_hdr-kunwg    = gs_input-shiptoparty. "Ship to Party
      lw_sale_hdr-vkbur    = gs_input-salesoffice. "Sales Office
      lw_sale_hdr-strg_loc = gs_input-strg_loc. "Storage loc
      lw_sale_hdr-booster  = gs_input-booster. "Booster enabled
      "Order ID Existence Checks
      DATA(l_orderid) = |{ gs_input-orderid }|.
      SELECT SINGLE ordid FROM zsd_spl_sale_hd
                          INTO @DATA(l_sales_hdr)
                          WHERE ordid = @l_orderid
                          AND   vbeln NE @abap_false.
      IF sy-subrc = 0.
        APPEND VALUE #( ordid = l_orderid
                        status = 'E'
                        message = |Already Order ID Available| ) TO gt_response.
      ENDIF.
      "Sales Order type Check
      SELECT SINGLE auart FROM tvak
         INTO @DATA(l_auart)
         WHERE auart = @lw_sale_hdr-auart.
      IF sy-subrc <> 0.
        APPEND VALUE #( ordid = l_orderid
                        status = 'E'
                        message = |Order Type is Incorrect| ) TO gt_response.
      ENDIF.
      "Company Code Checks
      SELECT SINGLE bukrs FROM t001
                          INTO @DATA(l_bukrs)
                          WHERE bukrs = @lw_sale_hdr-bukrs.
      IF sy-subrc <> 0.
        APPEND VALUE #( ordid = l_orderid
                        status = 'E'
                        message = |Company Code is Incorrect| ) TO gt_response.
      ENDIF.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lw_sale_hdr-kunag
        IMPORTING
          output = lw_sale_hdr-kunag.

      "Sold to Party Code Check
      SELECT SINGLE kunnr FROM kna1
        INTO @DATA(l_kunag)
        WHERE kunnr = @lw_sale_hdr-kunag.
      IF sy-subrc <> 0.
        APPEND VALUE #( ordid = l_orderid
                        status = 'E'
                        message = |Sold-to-party is Incorrect| ) TO gt_response.
      ENDIF.
      "Sold to Party Code Check & Payment terms checks

      SELECT SINGLE kunnr,zterm FROM knvv
        INTO @DATA(l_payterms)
        WHERE kunnr = @lw_sale_hdr-kunag
        AND vkorg = @lw_sale_hdr-vkorg
        AND vtweg = @lw_sale_hdr-vtweg
        AND spart = @lw_sale_hdr-spart.
      IF sy-subrc <> 0.
        APPEND VALUE #( ordid = l_orderid
                        status = 'E'
                        message = |Sold-to-party is Not extended to Sales Area { lw_sale_hdr-vkorg } - { lw_sale_hdr-vtweg } - { lw_sale_hdr-spart } - Please contact Sales Co-ordinators Team | ) TO gt_response.
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
        APPEND VALUE #( ordid = l_orderid
                        status = 'E'
                        message = |Ship-to-party is Incorrect| ) TO gt_response.
      ENDIF.

      "Ship to Party Code Check
      SELECT SINGLE kunnr FROM knvv
        INTO @DATA(l_kunwe)
        WHERE kunnr = @lw_sale_hdr-kunwg
        AND vkorg = @lw_sale_hdr-vkorg
        AND vtweg = @lw_sale_hdr-vtweg
        AND spart = @lw_sale_hdr-spart.

      IF sy-subrc <> 0.

        APPEND VALUE #( ordid = l_orderid
                        status = 'E'
                        message = |Ship-to-party is Not extended to Sales Area { lw_sale_hdr-vkorg } - { lw_sale_hdr-vtweg } - { lw_sale_hdr-spart } - Please contact Sales Co-ordinators Team | ) TO gt_response.
      ENDIF.

      "Sales Organization Check
      SELECT SINGLE vkorg
        FROM tvko INTO @DATA(l_saleorg)
        WHERE vkorg =  @lw_sale_hdr-vkorg.
      IF sy-subrc <> 0.
        APPEND VALUE #( ordid = l_orderid
                        status = 'E'
                        message = |Sales Organisation is Incorrect| ) TO gt_response.
      ENDIF.

      "Distribution Channel Check
      SELECT SINGLE vtweg
        FROM tvtw INTO @DATA(l_distchnl)
        WHERE  vtweg = @lw_sale_hdr-vtweg.
      IF sy-subrc <> 0.
        APPEND VALUE #( ordid = l_orderid
                        status = 'E'
                        message = |Distribution Channel is Incorrect| ) TO gt_response.
      ENDIF.

      "Division Check
      SELECT SINGLE spart FROM tspa
        INTO @DATA(l_division)
        WHERE spart = @lw_sale_hdr-spart.
      IF sy-subrc <> 0.
        APPEND VALUE #( ordid = l_orderid
                        status = 'E'
                        message = |Division is Incorrect| ) TO gt_response.
      ENDIF.
      lw_sale_hdr-plant = gs_input-plant.

      "Sales Office
      SELECT SINGLE vkbur FROM tvbur
                          INTO @DATA(l_salesoff)
                          WHERE vkbur = @lw_sale_hdr-vkbur.
      IF sy-subrc NE 0.
        APPEND VALUE #( ordid = l_orderid
                        status = 'E'
                        message = |Sales Office is Incorrect| ) TO gt_response.
      ENDIF.

      "Plant Check
      SELECT SINGLE werks FROM t001w
        INTO @DATA(l_plant)
        WHERE werks = @lw_sale_hdr-plant.
      IF sy-subrc <> 0.
        APPEND VALUE #( ordid = l_orderid
                        status = 'E'
                        message = |Plant Code is Incorrect| ) TO gt_response.
      ENDIF.

      lw_sale_hdr-xblnr    =  gs_input-refdocno." VALUE #( gt_input[ 1 ]-ref_doc_no  OPTIONAL ).
      lw_sale_hdr-erdat    =  sy-datum.
      lw_sale_hdr-erzet    =  sy-uzeit.

*Varibale for Check exclusion of specific Ordertype
      SELECT * FROM tvarvc INTO TABLE @DATA(lt_tvarvc) WHERE name = 'ORDERTYPE_CHECK_EXC'
                                                       AND   type = 'S'.
      IF sy-subrc = 0.

      ENDIF.

      LOOP AT gs_input-item INTO DATA(lw_data).
        DATA(lt_matnr) = gs_input-item.
        DELETE lt_matnr WHERE material NE lw_data-material.
        DESCRIBE TABLE lt_matnr LINES DATA(lv_lines).
        IF lv_lines GT 1 AND lw_sale_hdr-bukrs = '1000'.
          APPEND VALUE #( ordid = l_orderid
                          status = 'E'
                          message = |Same Material { lw_data-material } is repeated More than Once| ) TO gt_response.
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
          APPEND VALUE #( ordid = l_orderid
                          status = 'E'
                          message = |Material { lw_sale_itm-matnr } is not found| ) TO gt_response.
        ELSE.
          "Check not applicable for specifice Ordertype
          READ TABLE lt_tvarvc INTO DATA(ls_tvarvc) WITH KEY low = gs_input-ordertype.
          IF sy-subrc NE 0.
            IF l_payterms-zterm NE l_mara-zterm.
              APPEND VALUE #( ordid = l_orderid
                              status = 'E'
                              message = |Material and customer payment terms not matching| ) TO gt_response.
            ENDIF.
          ENDIF.
          IF lw_sale_itm-lfimg  = 0.
            APPEND VALUE #( ordid = l_orderid
                            status = 'E'
                            message = |Maretial { lw_sale_itm-matnr } quantity is zero| ) TO gt_response.
          ENDIF.
*----------------------------------------------------------------------------------
*      Changed by - pandiarajan
*      changed on date - 11.09.2023
*      Reference by: Ramakrishnan J

*******************transportation group check*************************************
          IF l_mara-tragr IS INITIAL.
            APPEND VALUE #( ordid = l_orderid
                  status = 'E'
                  message = |Transportation Group Missing - Check with R&D Team { lw_sale_itm-matnr } | ) TO gt_response.
          ELSE.
            IF l_mara-tragr <> '0001'.
              APPEND VALUE #( ordid = l_orderid
                    status = 'E'
                    message = |Transportation Group Incorrect - Check with R&D Team { lw_sale_itm-matnr } | ) TO gt_response.
            ENDIF.
          ENDIF.
        ENDIF.
*** -------------------------------------------------------------------------------
        SELECT SINGLE matnr ,ladgr FROM marc INTO @DATA(l_material) WHERE matnr = @lw_sale_itm-matnr
                                                                    AND werks   = @lw_sale_itm-werks.
        IF sy-subrc <> 0.
          APPEND VALUE #( ordid = l_orderid
                          status = 'E'
                          message = |Maretial { lw_sale_itm-matnr } Not Maintained in Plant { lw_sale_itm-werks }| ) TO gt_response.
        ELSE.
*----------------------------------------------------------------------------------
*      Changed by - pandiarajan
*      changed on date - 11.09.2023
*      Reference by: Ramakrishnan J

*********************Loading group check*******************************************
          IF l_material-ladgr IS INITIAL.
            APPEND VALUE #( ordid = l_orderid
                  status = 'E'
                  message = |Loading Group Missing - Check with R&D Team { lw_sale_itm-matnr } | ) TO gt_response.
          ELSE.
            IF l_material-ladgr <> '0004'.

              APPEND VALUE #( ordid = l_orderid
                    status = 'E'
                    message = |Loading Group Incorrect - Check with R&D Team { lw_sale_itm-matnr } | ) TO gt_response.
            ENDIF.
          ENDIF.
        ENDIF.
************************material group1 check***********************************
        SELECT SINGLE matnr ,mvgr1 FROM mvke INTO @DATA(l_mvke) WHERE matnr = @lw_sale_itm-matnr
                                                                AND   vkorg = @lw_sale_hdr-vkorg
                                                                AND   vtweg = @lw_sale_hdr-vtweg.
        IF sy-subrc <> 0.

          APPEND VALUE #( ordid = l_orderid
                  status = 'E'
                  message = |Maretial { lw_sale_itm-matnr } Not Maintained in Sales org { lw_sale_hdr-vkorg }  & Dis.Channel { lw_sale_hdr-vtweg } - Check with R&D Team | ) TO gt_response.

        ELSE.

          IF l_mvke-mvgr1 IS INITIAL.

            APPEND VALUE #( ordid = l_orderid
                  status = 'E'
                  message = |Material GRP 1 Missing - Check with R&D Team { lw_sale_itm-matnr } | ) TO gt_response.

          ENDIF.

        ENDIF.
      ENDLOOP.
*----------------------------------------------------------------------------------

      IF gt_response IS INITIAL.
        DATA : v_vbeln TYPE vbeln_va.
        DATA: lt_return TYPE STANDARD TABLE OF bapiret2.
        DATA(l_orderid_chk) = VALUE zsd_splordid_chk( mandt   = sy-mandt
                                              orderid = lw_sale_hdr-ordid ).
        INSERT zsd_splordid_chk FROM l_orderid_chk.
        CALL FUNCTION 'ZSD_SPL_SALES_ORDER_CREATE'
          EXPORTING
            im_header     = lw_sale_hdr
            im_item       = gs_input-item
          IMPORTING
            salesorder_no = v_vbeln
          TABLES
            return        = lt_return.
        IF v_vbeln IS NOT INITIAL.
          lw_sale_hdr-vbeln = v_vbeln.
          lw_sale_hdr-status = 'S'.
          lw_sale_hdr-msg = |Sales Order { lw_sale_hdr-vbeln } is Created|.
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
          CALL FUNCTION 'ZSD_SALES_ORDER_PDF_GENERATE'
            EXPORTING
              im_vbeln       = v_vbeln
            IMPORTING
              ep_xstring     = lv_string
              error_message  = lv_errstring
            EXCEPTIONS
              so_num_missing = 1
              OTHERS         = 2.
          IF sy-subrc = 0.

          ENDIF.

          APPEND VALUE #( ordid      = lw_sale_hdr-ordid
                          salesorder = lw_sale_hdr-vbeln
                          status     = 'S'
                          message    = lw_sale_hdr-msg
                          creditstat = ls_details-header-cmgst
                          details    = ls_details
                          form       = lv_string ) TO gt_response.
        ELSE.
          CLEAR lv_msgtxt.
          LOOP AT lt_return INTO DATA(lw_ret).
            lv_msgtxt = |{ lw_ret-message },{ lv_msgtxt }|.
          ENDLOOP.
          lw_sale_hdr-status = 'E'.
          lw_sale_hdr-msg = lv_msgtxt.
          APPEND VALUE #( ordid = lw_sale_hdr-ordid
                          salesorder = lw_sale_hdr-vbeln
                          status = 'E'
                          message = lw_sale_hdr-msg ) TO gt_response.
        ENDIF.
*once successfully processed deleting the Order ID from duplicaton CHECK table
        DELETE FROM zsd_splordid_chk WHERE orderid = lw_sale_hdr-ordid.
        MODIFY zsd_spl_sale_hd FROM lw_sale_hdr .
        MODIFY zsd_spl_sale_it FROM TABLE lt_sale_itm .
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
          apiname         = 'SALESORDER'
          ijson           = lv_data
          ojson           = lv_body
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
