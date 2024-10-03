*&---------------------------------------------------------------------*
*& Include          ZAPI_LOGS_DISPLAY_CLS
*&---------------------------------------------------------------------*
CLASS lcl_logdisplay DEFINITION.
  PUBLIC SECTION.
    METHODS: f4_shlp,data_population,build_alv.
    METHODS: on_double_click FOR EVENT double_click OF cl_salv_events_table
      IMPORTING row column. "Event for Double click on alv display
ENDCLASS.
CLASS lcl_logdisplay IMPLEMENTATION.
  METHOD f4_shlp.
    SELECT low FROM tvarvc INTO TABLE @DATA(lt_apiname)
      WHERE name = 'APINAME_SHLP'
      AND type = 'S'.
    IF sy-subrc EQ 0.
      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
          retfield    = 'APINAME'
          dynpprog    = sy-cprog
          dynpnr      = sy-dynnr
          dynprofield = 'S_NAME'
          value_org   = 'S'
        TABLES
          value_tab   = lt_apiname.
    ENDIF.
  ENDMETHOD.
  METHOD data_population.
    REFRESH: gt_api_logs.
    SELECT * FROM zapi_logs
      INTO TABLE gt_api_logs
      WHERE refid IN s_refid
      AND apiname IN s_name
      AND erdat IN s_erdat
      AND distributor IN s_dist
      AND retailer    IN s_reta.
    IF sy-subrc EQ 0.
      SORT gt_api_logs[] BY refid.
      LOOP AT gt_api_logs ASSIGNING FIELD-SYMBOL(<fs1>) WHERE apiservice = ''.
        SELECT SINGLE
            apiservice,
            classname
          FROM zapi_service INTO @DATA(lw_api_service) WHERE apiname = @<fs1>-apiname.
        IF sy-subrc = 0.
          <fs1>-apiservice = lw_api_service-apiservice.
          <fs1>-classname = lw_api_service-classname.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
  METHOD build_alv.
    DATA: lo_gr_alv       TYPE REF TO cl_salv_table, " Variables for ALV properties
          lo_gr_functions TYPE REF TO cl_salv_functions_list.

    DATA: lo_grid          TYPE REF TO cl_salv_form_layout_grid, " Variables for header
          lo_layout_logo   TYPE REF TO cl_salv_form_layout_logo,
          lo_display       TYPE REF TO cl_salv_display_settings,
          lo_columns       TYPE REF TO cl_salv_columns,
          lo_column        TYPE REF TO cl_salv_column_table,
          lo_event_handler TYPE REF TO lcl_logdisplay, " Variables for events
          lo_events        TYPE REF TO cl_salv_events_table.

* create the alv object
    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = lo_gr_alv
          CHANGING
            t_table      = gt_api_logs.
      CATCH cx_salv_msg.
    ENDTRY.
* Let's show all default buttons of ALV
    lo_gr_functions = lo_gr_alv->get_functions( ).
    lo_gr_functions->set_all( value  = if_salv_c_bool_sap=>true ).

* Fit the columns
    lo_columns = lo_gr_alv->get_columns( ).
    lo_columns->set_optimize( 'X' ).


* Apply zebra style to lv_rows
    lo_display = lo_gr_alv->get_display_settings( ).
    lo_display->set_striped_pattern( cl_salv_display_settings=>true ).
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
        lo_column->set_long_text( 'Employeeno' ).
        lo_column->set_medium_text( 'Employeeno' ).
        lo_column->set_short_text( 'Employeeno' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'REFID' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Reference ID' ).
        lo_column->set_medium_text( 'Reference ID' ).
        lo_column->set_short_text( 'RefID' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'APINAME' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Apiname' ).
        lo_column->set_medium_text( 'Apiname' ).
        lo_column->set_short_text( 'Apiname' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'ERDAT' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Created On' ).
        lo_column->set_medium_text( 'Created On' ).
        lo_column->set_short_text( 'CreateOn' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'ERZET' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Created Time' ).
        lo_column->set_medium_text( 'Created Time' ).
        lo_column->set_short_text( 'Entrytime' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'UNAME' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Username' ).
        lo_column->set_medium_text( 'Username' ).
        lo_column->set_short_text( 'Username' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'JSON' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'JSON String' ).
        lo_column->set_medium_text( 'JSON' ).
        lo_column->set_short_text( 'JSON' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    lo_gr_alv->display( ).
  ENDMETHOD.
  METHOD on_double_click.
    IF column EQ 'IJSON' OR column EQ 'OJSON'.
      READ TABLE gt_api_logs ASSIGNING FIELD-SYMBOL(<fs_logs>) INDEX row.
      IF sy-subrc = 0.
        CLEAR: gv_string_in,gv_string_ot,gt_text,gt_text1.
        gv_string_in = <fs_logs>-ijson.
        gv_string_ot = <fs_logs>-ojson.
        DATA(ls_text_in) = VALUE ty_text( value = gv_string_in ).
        CONDENSE ls_text_in.
        APPEND ls_text_in TO gt_text.
        DATA(ls_text_ot) = VALUE ty_text( value = gv_string_ot ).
        CONDENSE ls_text_ot.
        APPEND ls_text_ot TO gt_text1.
        CALL SCREEN 0101.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
MODULE status_0101 OUTPUT.
  SET PF-STATUS 'ZDISP_LOG'.
  SET TITLEBAR 'ZLOGDISP'.

  IF geditor_container1 IS NOT INITIAL.
    CALL METHOD geditor_container1->free
      EXCEPTIONS
        OTHERS = 1.
    IF sy-subrc <> 0.
    ENDIF.
    FREE geditor_container1.
  ENDIF.

  IF gtext_editor1 IS NOT INITIAL.
    CALL METHOD gtext_editor1->free.
    FREE gtext_editor1.
  ENDIF.

  IF geditor_container1 IS INITIAL.
    CREATE OBJECT geditor_container1
      EXPORTING
        container_name              = 'TEXT_BOX1'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    CREATE OBJECT gtext_editor1
      EXPORTING
        parent                     = geditor_container1
        wordwrap_mode              = cl_gui_textedit=>wordwrap_at_fixed_position
        wordwrap_to_linebreak_mode = cl_gui_textedit=>true.

    CALL METHOD gtext_editor1->set_selected_text_as_r3table
      EXPORTING
        table                         = gt_text
        enable_editing_protected_text = 1
      EXCEPTIONS
        error_dp                      = 1
        error_dp_create               = 2
        OTHERS                        = 3.
*
    CALL METHOD gtext_editor1->set_toolbar_mode
      EXPORTING
        toolbar_mode = cl_gui_textedit=>true.

    CALL METHOD gtext_editor1->set_statusbar_mode
      EXPORTING
        statusbar_mode = cl_gui_textedit=>true.

    CALL METHOD gtext_editor1->set_readonly_mode
      EXPORTING
        readonly_mode          = 1 "display mode
      EXCEPTIONS
        error_cntl_call_method = 1
        invalid_parameter      = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
    ENDIF.
  ENDIF.

  IF geditor_container2 IS NOT INITIAL.
    CALL METHOD geditor_container2->free
      EXCEPTIONS
        OTHERS = 1.
    IF sy-subrc <> 0.
    ENDIF.
    FREE geditor_container2.
  ENDIF.

  IF gtext_editor2 IS NOT INITIAL.
    CALL METHOD gtext_editor2->free.
    FREE gtext_editor1.
  ENDIF.

  IF geditor_container2 IS INITIAL.
    CREATE OBJECT geditor_container2
      EXPORTING
        container_name              = 'TEXT_BOX2'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    CREATE OBJECT gtext_editor2
      EXPORTING
        parent                     = geditor_container2
        wordwrap_mode              = cl_gui_textedit=>wordwrap_at_fixed_position
        wordwrap_to_linebreak_mode = cl_gui_textedit=>true.

    CALL METHOD gtext_editor2->set_selected_text_as_r3table
      EXPORTING
        table                         = gt_text1
        enable_editing_protected_text = 1
      EXCEPTIONS
        error_dp                      = 1
        error_dp_create               = 2
        OTHERS                        = 3.
*
    CALL METHOD gtext_editor2->set_toolbar_mode
      EXPORTING
        toolbar_mode = cl_gui_textedit=>true.

    CALL METHOD gtext_editor2->set_statusbar_mode
      EXPORTING
        statusbar_mode = cl_gui_textedit=>true.

    CALL METHOD gtext_editor2->set_readonly_mode
      EXPORTING
        readonly_mode          = 1 "display mode
      EXCEPTIONS
        error_cntl_call_method = 1
        invalid_parameter      = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
    ENDIF.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101  INPUT
*----------------------------------------------------------------------*
MODULE user_command_0101 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      SET SCREEN 0.
    WHEN 'CANC'.
      SET SCREEN 0.
  ENDCASE.
ENDMODULE.
