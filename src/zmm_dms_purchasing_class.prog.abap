*&---------------------------------------------------------------------*
*& Include          ZMM_DMS_PURCHASING_CLASS
*&---------------------------------------------------------------------*
CLASS lc_exception DEFINITION INHERITING FROM cx_static_check.

ENDCLASS.
CLASS  lcl_purchasing DEFINITION.
  PUBLIC SECTION.

    METHODS:
      f_data_selection,
      f_overall_process,
      f_purchase_order,
      f_goods_reciept,
      f_invoice,
      f_display_alv.

    METHODS: on_double_click FOR EVENT double_click OF cl_salv_events_table
      IMPORTING row column. "Event for Double click on alv display

  PRIVATE SECTION.
    DATA: gt_log     TYPE STANDARD TABLE OF zmm_dms_purchase.
*----- Invoice Structure -------*
    TYPES: BEGIN OF invoice,
             vbeln TYPE vbeln_vf,
             fkdat TYPE fkdat,
             knumv TYPE knumv,
             kunag TYPE kunag,
             posnr TYPE posnr_vf,
             fkimg TYPE fkimg,
             vrkme TYPE vrkme,
             netwr TYPE netwr_fp,
             matnr TYPE matnr,
             werks TYPE werks_d,
             matkl TYPE matkl,
             vgbel TYPE vgbel,
             aubel TYPE vbeln_va,
             aupos TYPE posnr_va,
             regio TYPE regio,
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
*------- Condition type fetching from prcd_elements -------*
    TYPES: BEGIN OF condition,
             knumv TYPE knumv,
             kschl TYPE kschl,
           END OF condition.
    DATA: gt_condition TYPE TABLE OF condition.
    DATA: po_header       TYPE bapiekkol,
          gt_po_lineitems TYPE TABLE OF bapiekpo,
          gt_ret          TYPE TABLE OF bapireturn.
    DATA: gs_kna1 TYPE kna1.
ENDCLASS.

CLASS lcl_purchasing IMPLEMENTATION.

  METHOD on_double_click.

    IF column EQ 'BELNR'.
      READ TABLE gt_log INTO DATA(wa_final) INDEX  row.
      SELECT SINGLE * FROM rbkp INTO @DATA(ls_bkpf) WHERE bukrs EQ 'DMS1'
                                                  AND belnr EQ @wa_final-belnr
                                                  AND gjahr EQ @wa_final-gjahr.
      IF sy-subrc = 0.
        SET PARAMETER ID 'RBN' FIELD wa_final-belnr.

        SET PARAMETER ID 'GJR' FIELD wa_final-gjahr.

        CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.
      ENDIF.
    ENDIF.

    IF column EQ 'EBELN'.
      READ TABLE gt_log INTO DATA(wa_final1) INDEX  row.
      SELECT SINGLE * FROM ekko INTO @DATA(ls_ekko) WHERE ebeln = @wa_final1-ebeln.
      IF sy-subrc = 0.
        SET PARAMETER ID 'BES' FIELD wa_final1-ebeln.

        CALL TRANSACTION 'ME23N'.
      ENDIF.
    ENDIF.


  ENDMETHOD.

  METHOD f_data_selection.

    REFRESH: gt_log,gt_invoice,gt_inforecord,gt_condition.
*----- Fetching Log Table data for which we have to Process -----*
    SELECT * FROM zmm_dms_purchase
             INTO TABLE me->gt_log
             WHERE vbeln IN s_vbeln
             AND fkdat IN s_fkdat
             AND status NE '16'.
    IF sy-subrc = 0.
      SORT me->gt_log[] BY vbeln.
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
                     FOR ALL ENTRIES IN me->gt_log
                     WHERE a~vbeln = me->gt_log-vbeln
                     AND a~fkart = 'YBBR'
                     AND a~fkdat = me->gt_log-fkdat
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
*------- Condition Records for checking TCS Existence -----*
        SELECT knumv
               kschl FROM prcd_elements
                     INTO TABLE gt_condition
                     FOR ALL ENTRIES IN me->gt_invoice
                     WHERE knumv = me->gt_invoice-knumv
                     AND kschl = 'JTC1' AND kbetr NE 0.
        IF sy-subrc = 0.
          SORT me->gt_condition BY knumv.
        ENDIF.

      ENDIF.
    ELSE.
      MESSAGE 'No Data for Process' TYPE 'E'.
    ENDIF.

  ENDMETHOD.

  METHOD f_overall_process.

    LOOP AT gt_log ASSIGNING <fs_log>.
*------ Tax code Fixing Based on Sales invoice ------*
      CLEAR lv_taxcode.
      DATA(ls_cond_no) = VALUE #( gt_invoice[ vbeln = <fs_log>-vbeln ] OPTIONAL ).
*---- Checks Whether TCS is Present in Sales Invoice ------*
      IF ls_cond_no IS NOT INITIAL.
        DATA(lv_cond_typ) = VALUE #( gt_condition[ knumv = ls_cond_no-knumv ] OPTIONAL ).
      ENDIF.
      "Plant code and Region of Distributor
      CLEAR gs_kna1.
      SELECT SINGLE * FROM kna1 INTO gs_kna1 WHERE kunnr = <fs_log>-kunnr.
      IF sy-subrc = 0.

      ENDIF.

      IF lv_cond_typ IS NOT INITIAL.
        "If Invoice is between same State with TCS
        IF ls_cond_no-regio = gs_kna1-regio.
          lv_taxcode = 'H5'.
        ELSE."If Invoice is between Different State with TCS
          lv_taxcode = 'H6'.
        ENDIF.
      ELSE.
        "If Invoice is between same State without TCS
        IF ls_cond_no-regio = gs_kna1-regio.
          lv_taxcode = 'PD'.
        ELSE."If Invoice is between Different State without TCS
          lv_taxcode = 'PE'.
        ENDIF.
      ENDIF.
      "Info Record Creation and Updation Process
      IF <fs_log>-status = '11'.
*----- Inforecord Creation and Updation Process -----*
        LOOP AT me->gt_invoice ASSIGNING FIELD-SYMBOL(<fs_invoice>) WHERE vbeln = <fs_log>-vbeln.
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
                                                     valid_from = sy-datum ).
            DATA(lv_flag) = abap_false.
            PERFORM f_inforecord USING lv_flag l_data CHANGING lv_type lv_msg.
            IF lv_type = 'E'.
              <fs_log>-type = 'E'.
              <fs_log>-message = lv_msg.
              "Updating Log table
              UPDATE zmm_dms_purchase SET type = <fs_log>-type
                                          message = <fs_log>-message
                                          WHERE vbeln = <fs_log>-vbeln
                                          AND fkdat = <fs_log>-fkdat .
            ELSE.
              <fs_log>-i_flag = abap_true.
              <fs_log>-type = 'S'.
              <fs_log>-message = lv_msg.
              "Updating Log table
              UPDATE zmm_dms_purchase SET i_flag = abap_true
                                          status = '12'
                                          WHERE vbeln = <fs_log>-vbeln
                                          AND fkdat = <fs_log>-fkdat .
            ENDIF.
          ELSE.
            l_data = VALUE zdms_inforecord_st( matnr = <fs_invoice>-matnr
                                                    lifnr = p_lifnr
                                                    werks = gs_kna1-werks
                                                    vrkme = <fs_invoice>-vrkme
                                                    mwskz = lv_taxcode
                                                    unit_price = lv_unit_price
                                                    valid_from = sy-datum ).
            lv_flag = abap_true.
            PERFORM f_inforecord USING lv_flag l_data CHANGING lv_type lv_msg.
            IF lv_type = 'E'.
              <fs_log>-type = 'E'.
              <fs_log>-message = lv_msg.
              "Updating Log table
              UPDATE zmm_dms_purchase SET type = <fs_log>-type
                                          message = <fs_log>-message
                                          WHERE vbeln = <fs_log>-vbeln
                                          AND fkdat = <fs_log>-fkdat .
            ELSE.
              <fs_log>-i_flag = abap_true.
              <fs_log>-type = 'S'.
              <fs_log>-message = lv_msg.
              "Updating Log table
              UPDATE zmm_dms_purchase SET i_flag = abap_true
                                          status = '12'
                                          WHERE vbeln = <fs_log>-vbeln
                                          AND fkdat = <fs_log>-fkdat .
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.

      "Purchase Order Process
      IF <fs_log>-i_flag IS NOT INITIAL.
        me->f_purchase_order( ).
      ENDIF.
      "Goods Reciept Process
      IF <fs_log>-ebeln IS NOT INITIAL.
        me->f_goods_reciept( ).
      ELSE.
        CONTINUE.
      ENDIF.
      "MIRO Invoice Process
      IF <fs_log>-mblnr IS NOT INITIAL.
        me->f_invoice( ).
      ELSE.
        CONTINUE.
      ENDIF.
      "overall Process Completed
      IF <fs_log>-belnr IS NOT INITIAL.
        <fs_log>-status = '16'.
        <fs_log>-type = 'S'.
        <fs_log>-message = |All Process Completed Successfully|.
        <fs_log>-ernam = sy-uname.
        <fs_log>-erdat = sy-datum.
        "Updating the Log table
        UPDATE zmm_dms_purchase SET status = <fs_log>-status
                                    ernam = <fs_log>-ernam
                                    erdat = <fs_log>-erdat
                                    type =  <fs_log>-type
                                    message = <fs_log>-message
                                    WHERE vbeln = <fs_log>-vbeln
                                    AND fkdat = <fs_log>-fkdat.
      ENDIF.
      CLEAR: ls_cond_no,lv_cond_typ.
    ENDLOOP.

  ENDMETHOD.
  "Purchase Order Creation
  METHOD f_purchase_order.
    CALL METHOD lo_main->create_purchase_order
      EXPORTING
        lifnr     = p_lifnr
        kunnr     = <fs_log>-kunnr
        vbeln     = <fs_log>-vbeln
        taxcode   = lv_taxcode
      IMPORTING
        po_number = DATA(l_ponumber)
        return    = DATA(lt_ret).
    IF l_ponumber IS NOT INITIAL.
      <fs_log>-ebeln = l_ponumber.
      <fs_log>-status = '13'.
      <fs_log>-type = 'S'.
      <fs_log>-message = |Purchase Order Created|.
      "Update Log table
      UPDATE zmm_dms_purchase SET ebeln = <fs_log>-ebeln
                                  status = '13'
                                  type = 'S'
                                  message = <fs_log>-message
                                  WHERE vbeln = <fs_log>-vbeln
                                  AND fkdat = <fs_log>-fkdat.
    ELSE.
      IF lt_ret IS NOT INITIAL.
        CLEAR: lv_msg_text.
        LOOP AT lt_ret INTO DATA(lw_ret) WHERE type = 'E'.
          lv_msg_text = |{ lv_msg_text }, { lw_ret-message }|.
        ENDLOOP.
        <fs_log>-type = 'E'.
        <fs_log>-message = lv_msg_text.
        "Updating Log table
        UPDATE zmm_dms_purchase SET type = <fs_log>-type
                                    message = <fs_log>-message
                                    WHERE vbeln = <fs_log>-vbeln
                                    AND fkdat = <fs_log>-fkdat.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  "Goods Reciept (GRN) Process
  METHOD f_goods_reciept.
    CALL METHOD lo_main->goods_reciept
      EXPORTING
        movement_type  = '101'
        purchase_order = <fs_log>-ebeln
      IMPORTING
        mblnr          = DATA(l_mblnr)
        mjahr          = DATA(l_mjahr)
        return         = DATA(lt_ret).
    IF l_mblnr IS NOT INITIAL AND l_mjahr IS NOT INITIAL.
      <fs_log>-mblnr = l_mblnr.
      <fs_log>-mjahr = l_mjahr.
      <fs_log>-type = 'S'.
      <fs_log>-message = |Goods Receipt Created|.
      "Updating Log table
      UPDATE zmm_dms_purchase SET mblnr = <fs_log>-mblnr
                                  mjahr = <fs_log>-mjahr
                                  status = '14'
                                  type = 'S'
                                  message = <fs_log>-message
                                  WHERE vbeln = <fs_log>-vbeln
                                  AND fkdat = <fs_log>-fkdat.
    ELSE.
      IF lt_ret IS NOT INITIAL.
        CLEAR: lv_msg_text.
        LOOP AT lt_ret INTO DATA(lw_ret) WHERE type = 'E'.
          lv_msg_text = |{ lv_msg_text }, { lw_ret-message }|.
        ENDLOOP.
        <fs_log>-type = 'E'.
        <fs_log>-message = lv_msg_text.
        "Updating Log table
        UPDATE zmm_dms_purchase SET type = <fs_log>-type
                                    message = <fs_log>-message
                                    WHERE vbeln = <fs_log>-vbeln
                                    AND fkdat = <fs_log>-fkdat.
      ENDIF.
    ENDIF.

  ENDMETHOD.
*------ MIRO Invoice Booking ------*
  METHOD f_invoice.
    CALL METHOD lo_main->miro_invoice
      EXPORTING
        purchase_order   = <fs_log>-ebeln
      IMPORTING
        invoicedocnumber = DATA(l_invno)
        fiscalyear       = DATA(l_year)
        return           = DATA(lt_ret).
    IF l_invno IS NOT INITIAL AND l_year IS NOT INITIAL.
      <fs_log>-belnr = l_invno.
      <fs_log>-gjahr = l_year.
      <fs_log>-type = 'S'.
      <fs_log>-message = |Miro Invoice Created|.
      "Updating Log table
      UPDATE zmm_dms_purchase SET belnr = <fs_log>-belnr
                                  gjahr = <fs_log>-gjahr
                                  status = '15'
                                  type = 'S'
                                  message = <fs_log>-message
                                  WHERE vbeln = <fs_log>-vbeln
                                  AND fkdat = <fs_log>-fkdat.
    ELSE.
      IF lt_ret IS NOT INITIAL.
        CLEAR: lv_msg_text.
        LOOP AT lt_ret INTO DATA(lw_ret) WHERE type = 'E'.
          lv_msg_text = |{ lv_msg_text }, { lw_ret-message }|.
        ENDLOOP.
        <fs_log>-type = 'E'.
        <fs_log>-message = lv_msg_text.
        "Updating Log table
        UPDATE zmm_dms_purchase SET type = <fs_log>-type
                                    message = <fs_log>-message
                                    WHERE vbeln = <fs_log>-vbeln
                                    AND fkdat = <fs_log>-fkdat.
      ENDIF.
    ENDIF.

  ENDMETHOD.
  "Displaying ALV
  METHOD f_display_alv.
    DATA: lo_gr_alv       TYPE REF TO cl_salv_table, " Variables for ALV properties
          lo_gr_functions TYPE REF TO cl_salv_functions_list.

    DATA: lo_grid        TYPE REF TO cl_salv_form_layout_grid, " Variables for header
          lo_layout_logo TYPE REF TO cl_salv_form_layout_logo,
          lo_content     TYPE REF TO cl_salv_form_element,

          lv_title       TYPE string,
          lv_rows        TYPE string.

    DATA: lo_event_handler TYPE REF TO lcl_purchasing, " Variables for events
          lo_events        TYPE REF TO cl_salv_events_table.

    DATA: lo_layout TYPE REF TO cl_salv_layout, " Variables for enabling Save button
          lv_key    TYPE salv_s_layout_key.

    DATA: lo_display TYPE REF TO cl_salv_display_settings. " Variable for layout settings

    DATA: lo_selections TYPE REF TO cl_salv_selections, " Variables for selection mode and column properties
          lo_columns    TYPE REF TO cl_salv_columns,
          lo_column     TYPE REF TO cl_salv_column_table.
    DATA: lr_aggregations TYPE REF TO cl_salv_aggregations.
    DATA: lr_groups TYPE REF TO cl_salv_sorts .
    DATA: toolbar TYPE REF TO cl_salv_functions_list .
* create the alv object
    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = lo_gr_alv
          CHANGING
            t_table      = gt_log.
      CATCH cx_salv_msg.
    ENDTRY.
* Fit the columns
    lo_columns = lo_gr_alv->get_columns( ).
    lo_columns->set_optimize( 'X' ).
* Apply zebra style to lv_rows
    lo_display = lo_gr_alv->get_display_settings( ).
    lo_display->set_striped_pattern( cl_salv_display_settings=>true ).

* Register events
    lo_events = lo_gr_alv->get_event( ).
    CREATE OBJECT lo_event_handler.
    SET HANDLER lo_event_handler->on_double_click FOR lo_events.

    TRY.
        lo_column ?= lo_columns->get_column( 'MANDT' ).
        lo_column->set_visible( if_salv_c_bool_sap=>false ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.
    TRY.
        lo_column ?= lo_columns->get_column( 'TYPE' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Msg type' ).
        lo_column->set_medium_text( 'Msg type' ).
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
*&---------------------------------------------------------------------*
FORM f_inforecord  USING    p_l_flag TYPE flag
                            p_l_data TYPE zdms_inforecord_st
                   CHANGING p_lv_type TYPE bapi_mtype
                            p_lv_msg TYPE string.

  CALL METHOD lo_main->inforecord_process
    EXPORTING
      data     = p_l_data
      flag     = p_l_flag
    IMPORTING
      msg_type = p_lv_type
      msg      = p_lv_msg.
ENDFORM.
