*&---------------------------------------------------------------------*
*& Include          ZMM_STOCK_B2B_TRANSFER_FORMS
*&---------------------------------------------------------------------*
CLASS lcl_handle_events DEFINITION. " Variables for events
  PUBLIC SECTION.

    METHODS:
      on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function. "Event for User command

*    METHODS: on_double_click FOR EVENT double_click OF cl_salv_events_table
*      IMPORTING row column. "Event for Double click on alv display


ENDCLASS.
CLASS lcl_handle_events IMPLEMENTATION.

  METHOD on_user_command.
    PERFORM handle_user_command USING e_salv_function.
  ENDMETHOD.
ENDCLASS.
*&---------------------------------------------------------------------*
*& Form f4_help
*&---------------------------------------------------------------------*
FORM f4_help  USING    p_p_fname TYPE rlgrap-filename.
  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    EXPORTING
      program_name  = syst-repid
      dynpro_number = syst-dynnr
      field_name    = ' '
      static        = ' '
      mask          = ' '
    CHANGING
      file_name     = p_p_fname.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form excel_conversion
*&---------------------------------------------------------------------*
FORM excel_conversion  USING    p_p_fname TYPE rlgrap-filename.
  DATA: lt_type  TYPE truxs_t_text_data.
  IF p_p_fname IS NOT INITIAL.
*** Function Module to Convert excel data to SAP ***
    CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
      EXPORTING
        i_line_header        = 'X'
        i_tab_raw_data       = lt_type
        i_filename           = p_p_fname
      TABLES
        i_tab_converted_data = gt_excel
      EXCEPTIONS
        conversion_failed    = 1
        OTHERS               = 2.
    IF sy-subrc <> 0.
      MESSAGE 'Conversion of excel failed' TYPE 'E'.
    ENDIF.
  ELSE.
    MESSAGE 'File is Mandatory' TYPE 'E'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form initial_checks
*&---------------------------------------------------------------------*
FORM initial_checks .
  REFRESH: gt_alv.
  LOOP AT gt_excel ASSIGNING FIELD-SYMBOL(<fs_excel>).

    IF <fs_excel>-from_plant IS INITIAL.
      MOVE-CORRESPONDING <fs_excel> TO gs_alv.
      gs_alv-type = 'E'.
      gs_alv-msg  = TEXT-011.
      APPEND gs_alv TO gt_alv.
      CLEAR gs_alv.
      CONTINUE.
    ELSEIF <fs_excel>-to_plant IS INITIAL.
      MOVE-CORRESPONDING <fs_excel> TO gs_alv.
      gs_alv-type = 'E'.
      gs_alv-msg  = TEXT-012.
      APPEND gs_alv TO gt_alv.
      CLEAR gs_alv.
      CONTINUE.
    ELSEIF <fs_excel>-from_matnr IS INITIAL.
      MOVE-CORRESPONDING <fs_excel> TO gs_alv.
      gs_alv-type = 'E'.
      gs_alv-msg  = TEXT-013.
      APPEND gs_alv TO gt_alv.
      CLEAR gs_alv.
      CONTINUE.
    ELSEIF <fs_excel>-to_matnr IS INITIAL.
      MOVE-CORRESPONDING <fs_excel> TO gs_alv.
      gs_alv-type = 'E'.
      gs_alv-msg  = TEXT-014.
      APPEND gs_alv TO gt_alv.
      CLEAR gs_alv.
      CONTINUE.
    ELSEIF <fs_excel>-from_stloc IS INITIAL.
      MOVE-CORRESPONDING <fs_excel> TO gs_alv.
      gs_alv-type = 'E'.
      gs_alv-msg  = TEXT-015.
      APPEND gs_alv TO gt_alv.
      CLEAR gs_alv.
      CONTINUE.
    ELSEIF <fs_excel>-to_stloc IS INITIAL.
      MOVE-CORRESPONDING <fs_excel> TO gs_alv.
      gs_alv-type = 'E'.
      gs_alv-msg  = TEXT-016.
      APPEND gs_alv TO gt_alv.
      CLEAR gs_alv.
      CONTINUE.
    ELSEIF <fs_excel>-from_batch IS INITIAL.
      MOVE-CORRESPONDING <fs_excel> TO gs_alv.
      gs_alv-type = 'E'.
      gs_alv-msg  = TEXT-017.
      APPEND gs_alv TO gt_alv.
      CLEAR gs_alv.
      CONTINUE.
    ELSEIF <fs_excel>-to_batch IS INITIAL.
      MOVE-CORRESPONDING <fs_excel> TO gs_alv.
      gs_alv-type = 'E'.
      gs_alv-msg  = TEXT-018.
      APPEND gs_alv TO gt_alv.
      CLEAR gs_alv.
      CONTINUE.
    ELSEIF <fs_excel>-quantity IS INITIAL.
      MOVE-CORRESPONDING <fs_excel> TO gs_alv.
      gs_alv-type = 'E'.
      gs_alv-msg  = TEXT-019.
      APPEND gs_alv TO gt_alv.
      CLEAR gs_alv.
      CONTINUE.
    ELSEIF <fs_excel>-base_uom IS INITIAL.
      MOVE-CORRESPONDING <fs_excel> TO gs_alv.
      gs_alv-type = 'E'.
      gs_alv-msg  = TEXT-020.
      APPEND gs_alv TO gt_alv.
      CLEAR gs_alv.
      CONTINUE.
    ENDIF.
*From Plant Checks
    SELECT SINGLE werks FROM t001w
      INTO @DATA(l_from_plant)
      WHERE werks = @<fs_excel>-from_plant.
    IF sy-subrc NE 0.
      MOVE-CORRESPONDING <fs_excel> TO gs_alv.
      gs_alv-type = 'E'.
      gs_alv-msg  = TEXT-000.
      APPEND gs_alv TO gt_alv.
      CLEAR gs_alv.
      CONTINUE.
    ENDIF.
*To Plant Checks
    SELECT SINGLE werks FROM t001w
      INTO @DATA(l_to_plant)
      WHERE werks = @<fs_excel>-to_plant.
    IF sy-subrc NE 0.
      MOVE-CORRESPONDING <fs_excel> TO gs_alv.
      gs_alv-type = 'E'.
      gs_alv-msg  = TEXT-001.
      APPEND gs_alv TO gt_alv.
      CLEAR gs_alv.
      CONTINUE.
    ENDIF.
*From Material Number Checks
    DATA(lv_matnr_from) = CONV matnr18( <fs_excel>-from_matnr ).
    lv_matnr_from = |{ lv_matnr_from ALPHA = IN }|.
    SELECT SINGLE matnr FROM marc
      INTO @DATA(l_frm_matnr)
      WHERE matnr = @lv_matnr_from
      AND werks = @l_from_plant.
    IF sy-subrc NE 0.
      MOVE-CORRESPONDING <fs_excel> TO gs_alv.
      gs_alv-type = 'E'.
      gs_alv-msg  = TEXT-002.
      APPEND gs_alv TO gt_alv.
      CLEAR gs_alv.
      CONTINUE.
    ENDIF.
*From Material Number Checks
    DATA(lv_matnr_to) = CONV matnr18( <fs_excel>-to_matnr ).
    lv_matnr_to = |{ lv_matnr_to ALPHA = IN }|.
    SELECT SINGLE matnr FROM marc
      INTO @DATA(l_to_matnr)
      WHERE matnr = @lv_matnr_to
      AND werks = @l_to_plant.
    IF sy-subrc NE 0.
      MOVE-CORRESPONDING <fs_excel> TO gs_alv.
      gs_alv-type = 'E'.
      gs_alv-msg  = TEXT-003.
      APPEND gs_alv TO gt_alv.
      CLEAR gs_alv.
      CONTINUE.
    ENDIF.
*From Storage Location Checks
    SELECT SINGLE lgort FROM mard
      INTO @DATA(l_from_stloc)
      WHERE matnr = @lv_matnr_from
      AND werks   = @l_from_plant
      AND lgort   = @<fs_excel>-from_stloc.
    IF sy-subrc NE 0.
      MOVE-CORRESPONDING <fs_excel> TO gs_alv.
      gs_alv-type = 'E'.
      gs_alv-msg  = TEXT-004.
      APPEND gs_alv TO gt_alv.
      CLEAR gs_alv.
      CONTINUE.
    ENDIF.
*To Storage Location Checks
    SELECT SINGLE lgort FROM mard
      INTO @DATA(l_to_stloc)
      WHERE matnr = @lv_matnr_to
      AND werks   = @l_to_plant
      AND lgort   = @<fs_excel>-to_stloc.
    IF sy-subrc NE 0.
      MOVE-CORRESPONDING <fs_excel> TO gs_alv.
      gs_alv-type = 'E'.
      gs_alv-msg  = TEXT-008.
      APPEND gs_alv TO gt_alv.
      CLEAR gs_alv.
      CONTINUE.
    ENDIF.
*From Batch Number Checks
    SELECT SINGLE charg FROM mcha
      INTO @DATA(l_from_batch)
      WHERE matnr = @lv_matnr_from
      AND werks   = @l_from_plant
      AND charg   = @<fs_excel>-from_batch.
    IF sy-subrc NE 0.
      MOVE-CORRESPONDING <fs_excel> TO gs_alv.
      gs_alv-type = 'E'.
      gs_alv-msg  = TEXT-005.
      APPEND gs_alv TO gt_alv.
      CLEAR gs_alv.
      CONTINUE.
    ENDIF.
*To Batch Number Checks
    SELECT SINGLE charg FROM mcha
      INTO @DATA(l_to_batch)
      WHERE matnr = @lv_matnr_to
      AND werks   = @l_to_plant
      AND charg   = @<fs_excel>-to_batch.
    IF sy-subrc NE 0.
      MOVE-CORRESPONDING <fs_excel> TO gs_alv.
      gs_alv-type = 'E'.
      gs_alv-msg  = TEXT-006.
      APPEND gs_alv TO gt_alv.
      CLEAR gs_alv.
      CONTINUE.
    ENDIF.
*Quantity Checks
    SELECT SINGLE clabs FROM mchb
      INTO @DATA(l_batch_stk)
      WHERE matnr = @lv_matnr_from
      AND werks   = @l_from_plant
      AND lgort   = @l_from_stloc
      AND charg   = @<fs_excel>-from_batch.
    IF sy-subrc = 0.
      IF l_batch_stk LT <fs_excel>-quantity.
        MOVE-CORRESPONDING <fs_excel> TO gs_alv.
        gs_alv-type = 'E'.
        gs_alv-msg  = TEXT-007.
        APPEND gs_alv TO gt_alv.
        CLEAR gs_alv.
        CONTINUE.
      ENDIF.
    ELSE.
      MOVE-CORRESPONDING <fs_excel> TO gs_alv.
      gs_alv-type = 'E'.
      gs_alv-msg  = TEXT-007.
      APPEND gs_alv TO gt_alv.
      CLEAR gs_alv.
      CONTINUE.
    ENDIF.
*Success Entry
    MOVE-CORRESPONDING <fs_excel> TO gs_alv.
    gs_alv-from_matnr = lv_matnr_from.
    gs_alv-to_matnr   = lv_matnr_to.
    gs_alv-type       = 'S'.
    gs_alv-msg        = TEXT-021.
    APPEND gs_alv TO gt_alv.
    CLEAR gs_alv.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form alv_display
*&---------------------------------------------------------------------*
FORM alv_display .
  DATA: "lo_gr_alv       TYPE REF TO cl_salv_table, " Variables for ALV properties
        lo_gr_functions TYPE REF TO cl_salv_functions_list.

  DATA: lo_event_handler TYPE REF TO lcl_handle_events, " Variables for events
        lo_events        TYPE REF TO cl_salv_events_table.

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
  DATA: lr_aggregations TYPE REF TO cl_salv_aggregations.
  DATA: lr_groups TYPE REF TO cl_salv_sorts .
  DATA: toolbar TYPE REF TO cl_salv_functions_list .
* create the alv object
  TRY.
      CALL METHOD cl_salv_table=>factory
        IMPORTING
          r_salv_table = lo_gr_alv
        CHANGING
          t_table      = gt_alv.
    CATCH cx_salv_msg.
  ENDTRY.


  lo_gr_alv->set_screen_status(
    pfstatus      =  'GRN_STATUS'
    report        =  sy-repid
    set_functions = lo_gr_alv->c_functions_all ).

  lr_aggregations = lo_gr_alv->get_aggregations( ).
* Let's show all default buttons of ALV
  lo_gr_functions = lo_gr_alv->get_functions( ).
  lo_gr_functions->set_all( value  = if_salv_c_bool_sap=>true ).

* Fit the columns
  lo_columns = lo_gr_alv->get_columns( ).
  lo_columns->set_optimize( 'X' ).


* Apply zebra style to lv_rows
  lo_display = lo_gr_alv->get_display_settings( ).
  lo_display->set_striped_pattern( cl_salv_display_settings=>true ).

* Register events
  lo_events = lo_gr_alv->get_event( ).
  CREATE OBJECT lo_event_handler.
  SET HANDLER lo_event_handler->on_user_command FOR lo_events.
*  SET HANDLER lo_event_handler->on_double_click FOR lo_events.
  TRY.
      lo_column ?= lo_columns->get_column( 'FROM_MATNR' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'From Material' ).
      lo_column->set_medium_text( 'From Material' ).
      lo_column->set_short_text( 'Fm matnr' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'TO_MATNR' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'To Material' ).
      lo_column->set_medium_text( 'To Material' ).
      lo_column->set_short_text( 'To Matnr' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'FROM_PLANT' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'From Plant' ).
      lo_column->set_medium_text( 'From Plant' ).
      lo_column->set_short_text( 'Frm_plant' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'TO_PLANT' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'To Plant' ).
      lo_column->set_medium_text( 'To Plant' ).
      lo_column->set_short_text( 'To_plant' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'FROM_STLOC' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'From stloc' ).
      lo_column->set_medium_text( 'From stloc' ).
      lo_column->set_short_text( 'From stloc' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'TO_STLOC' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'To stloc' ).
      lo_column->set_medium_text( 'To stloc' ).
      lo_column->set_short_text( 'To stloc' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'FROM_BATCH' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'From Batch' ).
      lo_column->set_medium_text( 'From Batch' ).
      lo_column->set_short_text( 'Frm Batch' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'TO_BATCH' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'To Batch' ).
      lo_column->set_medium_text( 'To Batch' ).
      lo_column->set_short_text( 'To Batch' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'QUANTITY' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Quantity' ).
      lo_column->set_medium_text( 'Quantity' ).
      lo_column->set_short_text( 'Quantity' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'BASE_UOM' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Base UOM' ).
      lo_column->set_medium_text( 'Base UOM' ).
      lo_column->set_short_text( 'Base UOM' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'TYPE' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Msgtyp' ).
      lo_column->set_medium_text( 'Msgtyp' ).
      lo_column->set_short_text( 'Msgtyp' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'MSG' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Message' ).
      lo_column->set_medium_text( 'Message' ).
      lo_column->set_short_text( 'Message' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  lo_gr_alv->display( ).
ENDFORM.
FORM handle_user_command USING i_ucomm TYPE salv_de_function.
* Structures for BAPI
  DATA: goodsmvt_header   LIKE bapi2017_gm_head_01.
  DATA: goodsmvt_code LIKE bapi2017_gm_code.
  DATA: goodsmvt_headret LIKE bapi2017_gm_head_ret.
  DATA: goodsmvt_item LIKE bapi2017_gm_item_create
                         OCCURS 1 WITH HEADER LINE.
  DATA: return   LIKE bapiret2 OCCURS 0,
        l_return TYPE bapiret2.
  DATA: matdoc TYPE mblnr.
  DATA: l_msg TYPE string.

  CASE i_ucomm.
    WHEN '&GI_POST'.
      LOOP AT gt_alv ASSIGNING <fs_alv> WHERE type NE 'E'.
        CLEAR: goodsmvt_item,
               goodsmvt_header,
               goodsmvt_code,
               goodsmvt_headret.

        goodsmvt_header-pstng_date = sy-datum.
        goodsmvt_header-header_txt = <fs_alv>-header_txt.
        goodsmvt_code-gm_code = '06'.

* Add "309" movement to table.
        CLEAR goodsmvt_item.
        goodsmvt_item-move_type  = '309'.
        goodsmvt_item-material   = <fs_alv>-from_matnr.
        goodsmvt_item-move_mat   = <fs_alv>-to_matnr.


        goodsmvt_item-entry_qnt = <fs_alv>-quantity.
        goodsmvt_item-entry_uom = <fs_alv>-base_uom.

        goodsmvt_item-stge_loc   = <fs_alv>-from_stloc.
        goodsmvt_item-move_stloc = <fs_alv>-to_stloc.

        goodsmvt_item-plant      = <fs_alv>-from_plant.
        goodsmvt_item-move_plant = <fs_alv>-to_plant.



        goodsmvt_item-batch = <fs_alv>-from_batch.
        goodsmvt_item-move_batch = <fs_alv>-to_batch.
        APPEND goodsmvt_item.

        CLEAR: return,matdoc.
        CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
          EXPORTING
            goodsmvt_header  = goodsmvt_header
            goodsmvt_code    = goodsmvt_code
            testrun          = p_run
          IMPORTING
            goodsmvt_headret = goodsmvt_headret
            materialdocument = matdoc
          TABLES
            goodsmvt_item    = goodsmvt_item
            return           = return.
        IF return IS INITIAL.
          IF p_run EQ abap_true.
            <fs_alv>-type = 'S'.
            <fs_alv>-msg  = TEXT-022.
          ELSE.
            CLEAR l_return.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait   = 'X'
              IMPORTING
                return = l_return.
            IF l_return IS INITIAL.
              <fs_alv>-goodsmvt_no = matdoc.
              <fs_alv>-type        = 'S'.
              <fs_alv>-msg         = TEXT-023.
            ENDIF.
          ENDIF.
        ELSE.
          LOOP AT return INTO DATA(lw_ret).
            CALL FUNCTION 'MESSAGE_TEXT_BUILD'
              EXPORTING
                msgid               = lw_ret-id
                msgnr               = lw_ret-number
                msgv1               = lw_ret-message_v1
                msgv2               = lw_ret-message_v2
                msgv3               = lw_ret-message_v3
                msgv4               = lw_ret-message_v4
              IMPORTING
                message_text_output = l_msg.
            l_msg = |{ l_msg }/{ l_msg }|.
          ENDLOOP.
          <fs_alv>-type        = 'E'.
          <fs_alv>-msg         = l_msg.
        ENDIF.
        CLEAR l_msg.
      ENDLOOP.
      lo_gr_alv->refresh( ).
  ENDCASE.
ENDFORM.
