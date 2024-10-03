*&---------------------------------------------------------------------*
*& Include          ZMM_PO_CREATE_FORMS
*&---------------------------------------------------------------------*
CLASS lcl_po_create DEFINITION.

  PUBLIC SECTION.
    METHODS:
      f_get_purchase_order,
      f_create_po,
      f_display_alv.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_eket,
             ebeln TYPE ebeln,
             ebelp TYPE ebelp,
             eindt TYPE eindt,
             menge TYPE menge_d,
           END OF ty_eket.
    DATA: gt_ekko TYPE STANDARD TABLE OF ekko.
    DATA: gt_ekpo TYPE STANDARD TABLE OF ekpo.
    DATA: gt_eket TYPE TABLE OF ty_eket.
    DATA: lr_bsart TYPE RANGE OF ekko-bsart.
    DATA: gt_log TYPE STANDARD TABLE OF zmm_po_log.
    DATA: lt_poitem      TYPE TABLE OF bapimepoitem,
          lt_poitemx     TYPE TABLE OF bapimepoitemx,
          lt_poschedule  TYPE TABLE OF bapimeposchedule,
          lt_poschedulex TYPE TABLE OF bapimeposchedulx,
          lt_account     TYPE TABLE OF bapimepoaccount,
          lt_accountx    TYPE TABLE OF bapimepoaccountx,
          lt_headertext  TYPE TABLE OF bapimepotext,
          return         TYPE bapiret2_t.
    DATA: ponumber TYPE ebeln.
    DATA: lv_msg TYPE string.
ENDCLASS.
CLASS lcl_po_create IMPLEMENTATION.
  METHOD f_get_purchase_order.
    REFRESH: gt_ekko,gt_ekpo,gt_eket,lr_bsart.
*------------- Fetching the Document which needs to close -----------------*
    SELECT * FROM tvarvc INTO TABLE @DATA(lt_tvarvc) WHERE name = 'OPEN_PO_CLOSE_BSART'
    AND type = 'S'.
    IF sy-subrc = 0.
      LOOP AT lt_tvarvc INTO DATA(ls_tvarvc).
        APPEND VALUE #( sign = 'I'
                        option = 'EQ'
                        low = ls_tvarvc-low ) TO lr_bsart.
      ENDLOOP.
    ELSE.
      MESSAGE 'Please Enter Document type in tcode STVARV' TYPE 'E'.
      LEAVE TO SCREEN 0.
    ENDIF.
*------- Fetching ZSTO Purchase order for reference ---------------------*
    SELECT * FROM ekko INTO TABLE gt_ekko
             WHERE ebeln IN s_ebeln
             AND bstyp = 'F'
             AND bsart IN lr_bsart
             AND bukrs IN s_bukrs
             AND ekorg IN s_ekorg
             AND aedat IN s_aedat.
    IF sy-subrc = 0.
      SORT gt_ekko[] BY ebeln.
*--- Purchase Order Item Details for fetched open po --------------------*
      SELECT * FROM ekpo INTO TABLE gt_ekpo
               FOR ALL ENTRIES IN gt_ekko
               WHERE ebeln = gt_ekko-ebeln
               AND werks IN s_plant.
      IF sy-subrc = 0.
        SORT gt_ekpo[] BY ebeln ebelp.
      ENDIF.
*----- Delivery Details for Lineitems-------------------------------------*
      SELECT ebeln
             ebelp
             eindt
             menge FROM eket
                   INTO TABLE gt_eket
                   FOR ALL ENTRIES IN gt_ekko
                   WHERE ebeln = gt_ekko-ebeln.
      IF sy-subrc = 0.
        SORT gt_eket[] BY ebeln ebelp.
      ENDIF.
    ELSE.
      MESSAGE 'No Open ZSTO Purchase Order' TYPE 'E'.
    ENDIF.
  ENDMETHOD.
  METHOD f_create_po.
    REFRESH gt_log.
    LOOP AT gt_ekko ASSIGNING FIELD-SYMBOL(<fs_header>).
      SELECT SINGLE * INTO @DATA(ls_polog) FROM zmm_po_log WHERE ebeln = @<fs_header>-ebeln.
      IF sy-subrc = 0.
        APPEND VALUE #( ebeln = ls_polog-ebeln
                        ebelp = ls_polog-ebelp
                        bsart = ls_polog-bsart
                        type = 'E'
                        message = |Already Purchase Order Created Based on the PO { ls_polog-ebeln } | ) TO gt_log.
        CONTINUE.
      ENDIF.
*------- Header data for purchase order preparation-------------------*
      DATA(ls_header) = VALUE bapimepoheader( comp_code = <fs_header>-bukrs
                                              doc_type = p_bsart
                                              creat_date = <fs_header>-aedat
                                              vendor = p_lifnr
                                              langu = sy-langu
                                              pmnttrms = <fs_header>-zterm
                                              purch_org = <fs_header>-ekorg
                                              pur_group = <fs_header>-ekgrp
                                              currency = <fs_header>-waers
                                              doc_date = <fs_header>-bedat
                                              ref_1 = <fs_header>-ebeln ) .

      DATA(ls_headerx) = VALUE bapimepoheaderx( comp_code = abap_true
                                                doc_type = abap_true
                                                creat_date = abap_true
                                                vendor = abap_true
                                                langu = abap_true
                                                pmnttrms = abap_true
                                                purch_org = abap_true
                                                pur_group = abap_true
                                                currency = abap_true
                                                doc_date = abap_true
                                                ref_1 = abap_true ) .
*------- Lineitem data for purchase order preparation-------------------*
      REFRESH: lt_poitem,lt_poitemx,lt_account,lt_accountx,lt_poschedule,lt_poschedulex.
      LOOP AT gt_ekpo ASSIGNING FIELD-SYMBOL(<fs_lineitems>) WHERE ebeln = <fs_header>-ebeln.
        APPEND VALUE #( po_item = <fs_lineitems>-ebelp
                        material = <fs_lineitems>-matnr
                        plant = <fs_lineitems>-werks
                        matl_group = <fs_lineitems>-matkl
                        quantity = <fs_lineitems>-menge
                        po_unit = <fs_lineitems>-meins
                        net_price = <fs_lineitems>-netpr
                        tax_code = <fs_lineitems>-mwskz
                        unlimited_dlv = ''
                      ) TO lt_poitem.
        APPEND VALUE #( po_item = <fs_lineitems>-ebelp
                        po_itemx = abap_true
                        material = abap_true
                        plant = abap_true
                        matl_group = abap_true
                        quantity = abap_true
                        po_unit = abap_true
                        net_price = abap_true
                        tax_code = abap_true
                        unlimited_dlv = abap_true
                      ) TO lt_poitemx.
        APPEND VALUE #( po_item = <fs_lineitems>-ebelp
                        delivery_date = VALUE #( gt_eket[ ebeln = <fs_lineitems>-ebeln
                                                          ebelp = <fs_lineitems>-ebelp ]-eindt OPTIONAL )
                        quantity = VALUE #( gt_eket[ ebeln = <fs_lineitems>-ebeln
                                                     ebelp = <fs_lineitems>-ebelp ]-menge OPTIONAL )
                       ) TO lt_poschedule.
        APPEND VALUE #( po_item = <fs_lineitems>-ebelp
                        po_itemx = abap_true
                        delivery_date = abap_true
                        quantity = abap_true
                       ) TO lt_poschedulex.
      ENDLOOP.
      REFRESH lt_headertext.
      APPEND VALUE #(  text_id = 'F03'
                       text_form = '*'
                       text_line = <fs_header>-ebeln ) TO lt_headertext.
*** Function module which creates new service purchase order ---------------*
      CLEAR: ponumber,return.
      CALL FUNCTION 'BAPI_PO_CREATE1'
        EXPORTING
          poheader         = ls_header
          poheaderx        = ls_headerx
        IMPORTING
          exppurchaseorder = ponumber
        TABLES
          return           = return
          poitem           = lt_poitem
          poitemx          = lt_poitemx
          poschedule       = lt_poschedule
          poschedulex      = lt_poschedulex
          potextheader     = lt_headertext.
      READ TABLE return INTO DATA(ls_ret) WITH KEY type = 'E'.
      IF sy-subrc = 0.
        CLEAR lv_msg.
        LOOP AT return INTO DATA(lw_ret) WHERE type = 'E'.
          lv_msg = | { lv_msg } { lw_ret-message } |.
        ENDLOOP.
        LOOP AT lt_poitem INTO DATA(ls_poitem).
          APPEND VALUE #( ebeln = <fs_header>-ebeln
                          ebelp = ls_poitem-po_item
                          bsart = <fs_header>-bsart
                          erdat = sy-datum
                          ernam = sy-uname
                          type = 'E'
                          message = lv_msg ) TO gt_log.
        ENDLOOP.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
        LOOP AT lt_poitem INTO DATA(ls_item).
          DATA(ls_log) = VALUE zmm_po_log( mandt = sy-mandt
                                           ebeln = <fs_header>-ebeln
                                           ebelp = ls_item-po_item
                                           bsart = <fs_header>-bsart
                                           ebeln_new = ponumber
                                           ebelp_new = ls_item-po_item
                                           bsart_new = 'ZNB'
                                           erdat = sy-datum
                                           ernam = sy-uname
                                           type = 'S'
                                           message = |Purchase Order { ponumber } cretaed for Vendor { p_lifnr }| ).
          INSERT zmm_po_log FROM ls_log.
          APPEND VALUE #( ebeln = <fs_header>-ebeln
                          ebelp = ls_item-po_item
                          bsart = <fs_header>-bsart
                          ebeln_new = ponumber
                          ebelp_new = ls_item-po_item
                          bsart_new = 'ZNB'
                          erdat = sy-datum
                          ernam = sy-uname
                          type = 'S'
                          message = |Purchase Order { ponumber } cretaed for Vendor { p_lifnr }| ) TO gt_log.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD f_display_alv.
    DATA: lo_gr_alv       TYPE REF TO cl_salv_table, " Variables for ALV properties
          lo_gr_functions TYPE REF TO cl_salv_functions_list.

    DATA: lo_grid        TYPE REF TO cl_salv_form_layout_grid, " Variables for header
          lo_layout_logo TYPE REF TO cl_salv_form_layout_logo,
          lo_content     TYPE REF TO cl_salv_form_element,
          lv_title       TYPE string,
          lv_rows        TYPE string.

    DATA: lo_layout TYPE REF TO cl_salv_layout, " Variables for enabling Save button
          lv_key    TYPE salv_s_layout_key.

    DATA: lo_display TYPE REF TO cl_salv_display_settings. " Variable for layout settings

    DATA: lo_selections TYPE REF TO cl_salv_selections, " Variables for selection mode and column properties
          lo_columns    TYPE REF TO cl_salv_columns,
          lo_column     TYPE REF TO cl_salv_column_table.

    IF gt_log IS NOT INITIAL.
* Create the ALV object
      TRY.
          CALL METHOD cl_salv_table=>factory
            IMPORTING
              r_salv_table = lo_gr_alv
            CHANGING
              t_table      = gt_log.
        CATCH cx_salv_msg.
      ENDTRY.
    ENDIF.

    lo_gr_functions = lo_gr_alv->get_functions( ).
    lo_gr_functions->set_all( abap_true ).

* Fit the columns
    lo_columns = lo_gr_alv->get_columns( ).
    lo_columns->set_optimize( 'X' ).
* Apply zebra style to lv_rows
    lo_display = lo_gr_alv->get_display_settings( ).
    lo_display->set_striped_pattern( cl_salv_display_settings=>true ).

    TRY.
        lo_column ?= lo_columns->get_column( 'MANDT' ).
        lo_column->set_visible( if_salv_c_bool_sap=>false ).
        lo_column->set_long_text( 'Reference Po' ).
        lo_column->set_medium_text( 'Reference Po' ).
        lo_column->set_short_text( 'Refer Po' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'EBELN' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Reference Po' ).
        lo_column->set_medium_text( 'Reference Po' ).
        lo_column->set_short_text( 'Refer Po' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'EBELN_NEW' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'New Purchase order' ).
        lo_column->set_medium_text( 'New PO' ).
        lo_column->set_short_text( 'New PO' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'TYPE' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Message type' ).
        lo_column->set_medium_text( 'Message typ' ).
        lo_column->set_short_text( 'Msgtyp' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.


    TRY.
        lo_column ?= lo_columns->get_column( 'MESSAGE' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Message' ).
        lo_column->set_medium_text( 'Message' ).
        lo_column->set_short_text( 'Message' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    lo_gr_alv->display( ).
  ENDMETHOD.
ENDCLASS.
