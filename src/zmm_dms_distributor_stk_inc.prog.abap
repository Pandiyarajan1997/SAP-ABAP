*&---------------------------------------------------------------------*
*& Include          ZMM_DMS_DISTRIBUTOR_STK_INC
*&---------------------------------------------------------------------*
CLASS lcl_distributor_stk DEFINITION.

  PUBLIC SECTION.
    CLASS-DATA: gr_salv_1 TYPE REF TO cl_salv_table,
                gr_salv_2 TYPE REF TO cl_salv_table.

    METHODS: constructor IMPORTING distributor TYPE kunnr
                                   lifnr       TYPE elifn,
      f_fetch_dstock_api,
      f_data_selection,
      f_purchase_order,
      f_goods_receipt,
*      f_invoice,
      f_inward_process,
      alv_display.

    METHODS:
      on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function.

  PRIVATE SECTION.
    " Structure for API response
    TYPES: BEGIN OF ty_dist_stk,
             distributor TYPE string.
    TYPES: lt_sub_stk TYPE zdist_stock_tt.
    TYPES END OF ty_dist_stk.

    DATA: gt_dist_stk TYPE TABLE OF ty_dist_stk,
          gs_dist_stk TYPE ty_dist_stk.

    "ALV Structure
    TYPES: BEGIN OF alv,
             distributor TYPE kunnr,
             vbeln       TYPE vbeln_vf,
             fkdat       TYPE fkdat,
             matnr       TYPE matnr,
             qty         TYPE fkimg,
           END OF alv.

    DATA: gt_alv_disp1 TYPE TABLE OF alv.
*----- Invoice Structure -------*
    TYPES: BEGIN OF invoice,
             distributor TYPE kunnr,
             vbeln       TYPE vbeln_vf,
             fkdat       TYPE fkdat,
             knumv       TYPE knumv,
             kunag       TYPE kunag,
             posnr       TYPE posnr_vf,
             fkimg       TYPE fkimg,
             vrkme       TYPE vrkme,
             netwr       TYPE netwr_fp,
             matnr       TYPE matnr,
             werks       TYPE werks_d,
             matkl       TYPE matkl,
             vgbel       TYPE vgbel,
             aubel       TYPE vbeln_va,
             aupos       TYPE posnr_va,
             regio       TYPE regio,
           END OF invoice.
    DATA:gt_invoice TYPE TABLE OF invoice.
*------ Info record Structure -------*
    TYPES: BEGIN OF info_record,
             lifnr TYPE lifnr,
             matnr TYPE matnr,
             ekorg TYPE ekorg,
             werks TYPE werks_d,
           END OF info_record.
    DATA: gt_inforecord TYPE TABLE OF info_record.

    DATA: gt_alv_disp2 TYPE STANDARD TABLE OF zdist_inward_dls.

    DATA:
      go_splitter_main TYPE REF TO cl_gui_splitter_container,
      go_container_1   TYPE REF TO cl_gui_container,
      go_container_2   TYPE REF TO cl_gui_container,
      go_salv_func_1   TYPE REF TO cl_salv_functions,
      go_salv_func_2   TYPE REF TO cl_salv_functions.

    DATA: gv_distributor TYPE kunnr,
          gv_supplier    TYPE elifn.
    DATA: lv_taxcode TYPE mwskz.
    DATA: gs_kna1 TYPE kna1.
    DATA: gt_item TYPE zdms_poitem_tt.
ENDCLASS.

CLASS lcl_distributor_stk IMPLEMENTATION.

  METHOD constructor.
    CLEAR: gv_distributor,gv_supplier.
    gv_distributor = distributor.
    gv_supplier = lifnr.
  ENDMETHOD.

  METHOD f_fetch_dstock_api.
    DATA: lo_http_client TYPE REF TO if_http_client.
    DATA: v_jsonload TYPE string.
    "Fetching the Variable from stvarv table
    SELECT SINGLE low FROM tvarvc
                      INTO @DATA(l_url)
                      WHERE name = 'DISTRIBUTOR_STOCK_URL'
                      AND type = 'P'.
    IF sy-subrc = 0.
      IF l_url IS INITIAL.
        l_url = 'https://webdevqas.sheenlac.com:44305/sap/zapi_service/ZMM_ASN_TEST?sap-client=600'.
      ENDIF.
      DATA(create_url) = CONV string( l_url ).
    ENDIF.
    cl_http_client=>create_by_url(
               EXPORTING
               url = create_url
               IMPORTING
               client = lo_http_client
               EXCEPTIONS
               argument_not_found = 1
               plugin_not_active = 2
               internal_error = 3
               OTHERS = 4 ).

    CHECK lo_http_client IS BOUND.
    lo_http_client->propertytype_logon_popup = if_http_client=>co_disabled.


    lo_http_client->authenticate(
    EXPORTING
     username = '366722'
     password = 'KMpSV12345' ).

    lo_http_client->request->set_method(
     EXPORTING
     method = if_http_entity=>co_request_method_post ).


    lo_http_client->request->set_content_type(
     EXPORTING
     content_type = if_rest_media_type=>gc_appl_json ).


    lo_http_client->request->set_cdata(
       EXPORTING
       data = '{ JSON_Payload }' ).

    CLEAR v_jsonload.
    CONCATENATE '{'
                '"distributor": "0010000100"'
                '}'  INTO v_jsonload.
*    CONDENSE v_jsonload NO-GAPS.

    lo_http_client->request->set_cdata(
    EXPORTING
     data = v_jsonload ).

    lo_http_client->send(
     EXCEPTIONS
     http_communication_failure = 1
     http_invalid_state = 2 ).


    CHECK sy-subrc = 0.
    lo_http_client->receive(
     EXCEPTIONS
     http_communication_failure = 1
     http_invalid_state = 2
     http_processing_failed = 3 ).


    lo_http_client->response->get_status(
    IMPORTING
      code = DATA(lv_codes) ).

    lo_http_client->response->get_status(
    IMPORTING
      reason = DATA(lv_http_error) ).
    "Actual API Response If success
    IF lv_codes = 200.
      DATA(lv_response) = lo_http_client->response->get_cdata( ).
    ENDIF.
    IF lv_response IS NOT INITIAL.
      REFRESH: gt_dist_stk. CLEAR: gs_dist_stk.
      CALL METHOD /ui2/cl_json=>deserialize
        EXPORTING
          json         = lv_response
          pretty_name  = /ui2/cl_json=>pretty_mode-user
          assoc_arrays = abap_true
        CHANGING
          data         = gs_dist_stk.
    ENDIF.
    IF gs_dist_stk IS NOT INITIAL.
      SORT gs_dist_stk-lt_sub_stk[] BY invoice_no material.
      REFRESH: gt_alv_disp1.

      LOOP AT gs_dist_stk-lt_sub_stk ASSIGNING FIELD-SYMBOL(<fs_stock>).
        DATA(l_vbeln) = |{ <fs_stock>-invoice_no ALPHA = IN }|. "Invocie Number
        DATA(l_matnr) = |{ <fs_stock>-material ALPHA = IN }|. "Material Number

        REPLACE ALL OCCURRENCES OF '-' IN <fs_stock>-invoice_date WITH ''.
        DATA(l_fkdat) = CONV fkdat( <fs_stock>-invoice_date ). "Invoice Date

        APPEND VALUE #( distributor = gs_dist_stk-distributor
                        vbeln = l_vbeln
                        fkdat = l_fkdat
                        matnr = l_matnr
                        qty = <fs_stock>-qty ) TO me->gt_alv_disp1.

        DATA(ls_table) = VALUE zdist_inward_dls( kunnr = gs_dist_stk-distributor
                                                 vbeln = l_vbeln
                                                 fkdat = l_fkdat
                                                 matnr = l_matnr
                                                 qty = <fs_stock>-qty ).
        MODIFY zdist_inward_dls FROM ls_table.
      ENDLOOP.

      DATA(lt_inward) = me->gt_alv_disp1[].
      DELETE ADJACENT DUPLICATES FROM lt_inward[] COMPARING distributor vbeln matnr.
      "Second Internal which is Going to Process
      REFRESH: gt_alv_disp2.
      LOOP AT lt_inward INTO DATA(lw_inward).
        APPEND VALUE #( kunnr  = lw_inward-distributor
                        vbeln  = lw_inward-vbeln
                        fkdat  = lw_inward-fkdat
                        matnr  = lw_inward-matnr
                        qty    = lw_inward-qty
                        status = '11' ) TO gt_alv_disp2.
      ENDLOOP.

    ENDIF.
  ENDMETHOD.

  METHOD f_data_selection.
    REFRESH: gt_invoice,gt_inforecord.
*--- Fetching Invoice Data from Billing Documents ----------------*
    SELECT a~vbeln
           a~fkdat
           a~knumv
           a~kunag
           b~posnr
           b~fkimg
           b~vrkme
           b~netwr
           b~matnr
           b~werks
           b~matkl
           b~vgbel
           b~aubel
           b~aupos
           c~regio INTO CORRESPONDING FIELDS OF TABLE me->gt_invoice
                   FROM vbrk AS a INNER JOIN vbrp AS b
                   ON a~vbeln = b~vbeln
                   INNER JOIN t001w AS c
                   ON c~werks = b~werks
                   FOR ALL ENTRIES IN me->gt_alv_disp2
                   WHERE a~vbeln = me->gt_alv_disp2-vbeln
                   AND a~fkart = 'YBBR'
                   AND a~fkdat = me->gt_alv_disp2-fkdat
                   AND a~fksto NE 'X'.
    IF sy-subrc = 0.
      SORT me->gt_invoice[] BY vbeln matnr posnr.
*----- Inforecord Details based on Invoice data --*
      SELECT lifnr
             matnr
             ekorg
             werks FROM a017
                   INTO TABLE me->gt_inforecord
                   FOR ALL ENTRIES IN me->gt_invoice
                   WHERE lifnr = p_lifnr
                   AND matnr = me->gt_invoice-matnr
                   AND ekorg = 'DMS1'
                   AND datbi GE sy-datum
                   AND datab LE sy-datum.
      IF sy-subrc = 0.
        SORT me->gt_inforecord[] BY lifnr matnr.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  "Purchase Order Creation
  METHOD f_purchase_order.

    CALL METHOD lo_main_cls->create_purchase_order
      EXPORTING
        lifnr     = p_lifnr
        kunnr     = <fs_inward>-kunnr
        vbeln     = <fs_inward>-vbeln
        taxcode   = lv_taxcode
        flag      = 'X'
        item_tab  = gt_item
      IMPORTING
        po_number = DATA(l_ponumber)
        return    = DATA(lt_ret).
    IF l_ponumber IS NOT INITIAL.
      <fs_inward>-ebeln = l_ponumber.
      <fs_inward>-status = '13'.
      <fs_inward>-type = 'S'.
      <fs_inward>-msg = |Purchase Order Created|.
    ELSE.
      IF lt_ret IS NOT INITIAL.
        CLEAR: lv_msg_text.
        LOOP AT lt_ret INTO DATA(lw_ret) WHERE type = 'E'.
          lv_msg_text = |{ lv_msg_text }, { lw_ret-message }|.
        ENDLOOP.
        <fs_inward>-type = 'E'.
        <fs_inward>-msg = lv_msg_text.
      ENDIF.
    ENDIF.
    MODIFY zdist_inward_dls FROM <fs_inward>.

  ENDMETHOD.
  "Goods Reciept (GRN) Process
  METHOD f_goods_receipt.
    CALL METHOD lo_main_cls->goods_reciept
      EXPORTING
        movement_type  = '101'
        purchase_order = <fs_inward>-ebeln
      IMPORTING
        mblnr          = DATA(l_mblnr)
        mjahr          = DATA(l_mjahr)
        return         = DATA(lt_ret).
    IF l_mblnr IS NOT INITIAL AND l_mjahr IS NOT INITIAL.
      <fs_inward>-mblnr = l_mblnr.
      <fs_inward>-mjahr = l_mjahr.
      <fs_inward>-type = 'S'.
      <fs_inward>-msg = |Goods Receipt Created|.
    ELSE.
      IF lt_ret IS NOT INITIAL.
        CLEAR: lv_msg_text.
        LOOP AT lt_ret INTO DATA(lw_ret) WHERE type = 'E'.
          lv_msg_text = |{ lv_msg_text }, { lw_ret-message }|.
        ENDLOOP.
        <fs_inward>-type = 'E'.
        <fs_inward>-msg = lv_msg_text.
      ENDIF.
    ENDIF.
    MODIFY zdist_inward_dls FROM <fs_inward>.
  ENDMETHOD.

  METHOD f_inward_process.

    DATA lv_unit_price TYPE char15.
    DATA: lv_type TYPE bapi_mtype,
          lv_msg  TYPE string.

    LOOP AT me->gt_alv_disp2 ASSIGNING <fs_inward>.
*------ Tax code Fixing Based on Sales invoice ------*
      CLEAR lv_taxcode.
      DATA(ls_cond_no) = VALUE #( me->gt_invoice[ vbeln = <fs_inward>-vbeln ] OPTIONAL ).

      "Plant code and Region of Distributor
      CLEAR gs_kna1.
      SELECT SINGLE * FROM kna1 INTO gs_kna1 WHERE kunnr = <fs_inward>-kunnr.
      IF sy-subrc = 0.

      ENDIF.

      "If Invoice is between same State without TCS
      IF ls_cond_no-regio = gs_kna1-regio.
        lv_taxcode = 'PD'.
      ELSE."If Invoice is between Different State without TCS
        lv_taxcode = 'PE'.
      ENDIF.

      "Info Record Creation and Updation Process
      IF <fs_inward>-status = '11'.
        REFRESH: gt_item.
*----- Inforecord Creation and Updation Process -----*
        LOOP AT me->gt_alv_disp1 ASSIGNING FIELD-SYMBOL(<fs_alv>) WHERE vbeln = <fs_inward>-vbeln.
          APPEND VALUE #( material = <fs_alv>-matnr
                          quantity = <fs_alv>-qty ) TO gt_item.
          LOOP AT me->gt_invoice ASSIGNING FIELD-SYMBOL(<fs_invoice>) WHERE vbeln = <fs_inward>-vbeln
                                                                      AND matnr = <fs_alv>-matnr.
            CLEAR: lv_unit_price.
            lv_unit_price = ( <fs_invoice>-netwr DIV <fs_invoice>-fkimg ).

            "Info Record Existence Checks
            DATA(ls_inforecord) = VALUE #( me->gt_inforecord[ matnr = <fs_invoice>-matnr
                                                              werks = gs_kna1-werks ] OPTIONAL ).
            IF ls_inforecord IS INITIAL.
              DATA(l_data) = VALUE zdms_inforecord_st( matnr = <fs_invoice>-matnr
                                                       lifnr = p_lifnr
                                                       werks = gs_kna1-werks
                                                       vrkme = <fs_invoice>-vrkme
                                                       mwskz = lv_taxcode
                                                       unit_price = lv_unit_price
                                                       valid_from = <fs_invoice>-fkdat ).
              DATA(lv_flag) = abap_false.
              PERFORM f_inforecord USING lv_flag l_data CHANGING lv_type lv_msg.
              IF lv_type = 'E'.
                <fs_inward>-type = 'E'.
                <fs_inward>-msg = lv_msg.
              ELSE.
                <fs_inward>-i_flag = abap_true.
                <fs_inward>-type = 'S'.
                <fs_inward>-msg = lv_msg.
              ENDIF.
            ELSE.
              l_data = VALUE zdms_inforecord_st( matnr = <fs_invoice>-matnr
                                                 lifnr = p_lifnr
                                                 werks = gs_kna1-werks
                                                 vrkme = <fs_invoice>-vrkme
                                                 mwskz = lv_taxcode
                                                 unit_price = lv_unit_price
                                                 valid_from = <fs_invoice>-fkdat ).
              lv_flag = abap_true.
              PERFORM f_inforecord USING lv_flag l_data CHANGING lv_type lv_msg.
              IF lv_type = 'E'.
                <fs_inward>-type = 'E'.
                <fs_inward>-msg = lv_msg.
              ELSE.
                <fs_inward>-i_flag = abap_true.
                <fs_inward>-type = 'S'.
                <fs_inward>-msg = lv_msg.
              ENDIF.
            ENDIF.
            MODIFY zdist_inward_dls FROM <fs_inward>.
          ENDLOOP.
        ENDLOOP.
      ENDIF.

      "Purchase Order Process
      IF <fs_inward>-i_flag IS NOT INITIAL.
        me->f_purchase_order( ).
      ENDIF.
      "Goods Reciept Process
      IF <fs_inward>-ebeln IS NOT INITIAL.
        me->f_goods_receipt( ).
      ELSE.
        CONTINUE.
      ENDIF.
      "overall Process Completed
      IF <fs_inward>-mblnr IS NOT INITIAL.
        <fs_inward>-status = '16'.
        <fs_inward>-type = 'S'.
        <fs_inward>-msg = |All Process Completed Successfully|.
        <fs_inward>-ernam = sy-uname.
        <fs_inward>-erdat = sy-datum.
        MODIFY zdist_inward_dls FROM <fs_inward>.
      ENDIF.
      CLEAR: ls_cond_no."lv_cond_typ.
    ENDLOOP.

  ENDMETHOD.

  METHOD alv_display.

    DATA: lr_column_1 TYPE REF TO cl_salv_column_list,
          lr_column_2 TYPE REF TO cl_salv_column_list.
    DATA: lo_events TYPE REF TO cl_salv_events_table.

    CHECK gt_alv_disp1 IS NOT INITIAL.
    TRY.
        go_splitter_main = NEW #( parent                    = cl_gui_container=>default_screen
                                  no_autodef_progid_dynnr   = abap_true
                                  rows                      = 2
                                  columns                   = 1 ).
* Container 1 for display

        go_container_1 = go_splitter_main->get_container(  row  = 1
                                                      column = 1 ).
* Container after button press whole process display

        go_container_2 = go_splitter_main->get_container( row  = 2
                                                     column = 1 ).

        cl_salv_table=>factory( EXPORTING r_container   = go_container_1
                                IMPORTING r_salv_table  = gr_salv_1
                                CHANGING  t_table       = gt_alv_disp1 ).
        INCLUDE <icon>.
        TRY.
            go_salv_func_1->add_function(
              name     = 'INWARD'
              icon     = CONV string( icon_complete )
              text     = `INWARD`
              tooltip  = `INWARD`
              position = if_salv_c_function_position=>right_of_salv_functions ).
          CATCH cx_salv_existing cx_salv_wrong_call.
        ENDTRY.

* Set ALV functions - should you wish to include any

        go_salv_func_1 = gr_salv_1->get_functions( ).
        go_salv_func_1->set_all( abap_true ).

        gr_salv_1->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>multiple ).
* Layout - set as usual

        DATA(ls_key_1) = VALUE salv_s_layout_key( report = sy-cprog
                                                  handle = '0001' ).
        DATA(lr_layout_1) = gr_salv_1->get_layout( ).
        lr_layout_1->set_key( ls_key_1 ).

        cl_salv_table=>factory( EXPORTING r_container   = go_container_2
                                IMPORTING r_salv_table  = gr_salv_2
                                CHANGING  t_table       = gt_alv_disp2 ).
* ALV functions

        go_salv_func_2 = gr_salv_2->get_functions( ).
        go_salv_func_2->set_all( abap_true ).

        DATA(ls_key_2) = VALUE salv_s_layout_key( report = sy-cprog
                                                  handle = '0002' ).

        DATA(lr_columns_1) = gr_salv_1->get_columns( ).
        lr_columns_1->set_optimize( abap_true ).
        lr_column_1 ?= lr_columns_1->get_column('DISTRIBUTOR').
        lr_column_1->set_long_text( 'Distributor Code' ).
        lr_column_1->set_medium_text( 'Distributor Code' ).
        lr_column_1->set_short_text( 'Distcode' ).

        DATA(lr_columns_2) = gr_salv_2->get_columns( ).
        lr_columns_2->set_optimize( abap_true ).
        lr_column_2 ?= lr_columns_1->get_column('KUNNR').
        lr_column_2->set_visible( abap_false ).

        lr_column_2 ?= lr_columns_1->get_column('MANDT').
        lr_column_2->set_visible( abap_false ).

        lr_column_2 ?= lr_columns_2->get_column('TYPE').
        lr_column_2->set_long_text( 'Msgtyp' ).
        lr_column_2->set_medium_text( 'Msgtyp' ).
        lr_column_2->set_short_text( 'Msgtyp' ).

        lr_column_2 ?= lr_columns_2->get_column('I_FLAG').
        lr_column_2->set_long_text( 'PIR Flag' ).
        lr_column_2->set_medium_text( 'PIR Flag' ).
        lr_column_2->set_short_text( 'PIR Flag' ).

        lr_column_2 ?= lr_columns_2->get_column('MESSAGE').
        lr_column_2->set_long_text( 'Message' ).
        lr_column_2->set_medium_text( 'Message' ).
        lr_column_2->set_short_text( 'Message' ).

        lo_events = gr_salv_2->get_event( ).
        SET HANDLER on_user_command FOR lo_events.

        WRITE: space.

      CATCH cx_salv_msg cx_salv_not_found cx_salv_data_error cx_salv_access_error INTO DATA(lx_alv).
        MESSAGE lx_alv->get_text( ) TYPE 'I' DISPLAY LIKE 'E'.

    ENDTRY.
  ENDMETHOD.
  METHOD on_user_command.

    CASE e_salv_function.
      WHEN 'INWARD'.
        "First Data fetching Based on Invoice from MIS
        me->f_data_selection( ).
        "Overall Process
        me->f_inward_process( ).

        gr_salv_2->refresh( ).
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
*&---------------------------------------------------------------------*
FORM f_inforecord  USING    p_l_flag TYPE flag
                            p_l_data TYPE zdms_inforecord_st
                   CHANGING p_lv_type TYPE bapi_mtype
                            p_lv_msg TYPE string.

  CALL METHOD lo_main_cls->inforecord_process
    EXPORTING
      data     = p_l_data
      flag     = p_l_flag
    IMPORTING
      msg_type = p_lv_type
      msg      = p_lv_msg.
ENDFORM.
