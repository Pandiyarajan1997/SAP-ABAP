FUNCTION z_popup_alv .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_REPID) TYPE  SY-REPID OPTIONAL
*"     REFERENCE(I_START_COLUMN) TYPE  I DEFAULT 25
*"     REFERENCE(I_START_LINE) TYPE  I DEFAULT 6
*"     REFERENCE(I_END_COLUMN) TYPE  I DEFAULT 200
*"     REFERENCE(I_END_LINE) TYPE  I DEFAULT 20
*"     REFERENCE(I_TITLE) TYPE  LVC_TITLE DEFAULT 'List Display'
*"     REFERENCE(I_STATUS_FIELD_NAME) TYPE  FIELDNAME DEFAULT ''
*"     REFERENCE(I_HYPERLINK_COLUMN) TYPE  FIELDNAME OPTIONAL
*"     REFERENCE(I_HYPERLINK_DATA) TYPE  FIELDNAME OPTIONAL
*"     REFERENCE(I_HIDE_COLUMN) TYPE  FIELDNAME OPTIONAL
*"     REFERENCE(I_HIDE_COLUMN_EXT) TYPE  FIELDNAME OPTIONAL
*"     REFERENCE(I_POPUP) TYPE  FLAG OPTIONAL
*"     REFERENCE(I_LAYOUT) TYPE  DISVARIANT-VARIANT OPTIONAL
*"  TABLES
*"      IT_ALV TYPE  STANDARD TABLE
*"----------------------------------------------------------------------
  ASSIGN it_alv[] TO <fst_alv_tab>.

  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = go_alv
        CHANGING
          t_table      = it_alv[] ).

    CATCH cx_salv_msg.
  ENDTRY.

  DATA: lr_functions TYPE REF TO cl_salv_functions_list.

  lr_functions = go_alv->get_functions( ).
  lr_functions->set_all( 'X' ).

  " Document Status color " Error Success Warning
  IF i_status_field_name IS NOT INITIAL.
    LOOP AT it_alv ASSIGNING FIELD-SYMBOL(<fs_data>).
      ASSIGN COMPONENT i_status_field_name  OF STRUCTURE <fs_data> TO <fs_field>.
      IF <fs_field> IS ASSIGNED.
        CASE <fs_field>.
          WHEN 'E'. <fs_field> = 1.
          WHEN 'W'. <fs_field> = 2.
          WHEN 'S'. <fs_field> = 3.
          WHEN OTHERS.
        ENDCASE.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF go_alv IS BOUND.
    IF i_popup = 'X'.
      go_alv->set_screen_popup(
        start_column = i_start_column
        end_column  = i_end_column
        start_line  = i_start_line
        end_line    = i_end_line ).
    ELSE.
      gr_layout = go_alv->get_layout( ).
      gs_key-report = i_repid.
      gr_layout->set_key( gs_key ).
      gr_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
      IF i_layout IS NOT INITIAL.
        gr_layout->set_initial_layout( value = i_layout  ).
      ENDIF.
*
*      gr_layout = go_alv->get_layout( ).
*      gs_key-report = sy-repid.
*      gr_layout->set_key( gs_key ).
** You can pass the folling values to the SET_SAVE_RESTRICTION method.
**RESTRICT_NONE
**RESTRICT_USER_DEPENDANT
**RESTRICT_USER_INDEPENDANT
*      gr_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
    ENDIF.


    " Set Title
    IF i_title IS NOT INITIAL.
      gr_display = go_alv->get_display_settings( ).
      gr_display->set_list_header( i_title ).
    ENDIF.

    " Document Status color " Error Success Warning
    PERFORM f_set_coloumns USING i_status_field_name.


    " Hide Columns
    IF i_hide_column IS NOT INITIAL.
      PERFORM f_hide_coloumns USING i_hide_column.
    ENDIF.
    IF i_hide_column_ext IS NOT INITIAL.
      PERFORM f_hide_coloumns USING i_hide_column_ext.
    ENDIF.

    " Set Hyper-Link Column
    IF i_hyperlink_column IS NOT INITIAL.
      PERFORM f_hyperlink_column USING i_hyperlink_column.

      ASSIGN i_hyperlink_data TO <fs_hyperlink_data>.
*   Add Hotspot Handlers
      CREATE OBJECT gr_event_handler. "type ref to lcl_events.
      TRY.
          CALL METHOD go_alv->get_event
            RECEIVING
              value = DATA(lr_event).
        CATCH cx_salv_data_error.
      ENDTRY.
      SET HANDLER gr_event_handler->on_link_click  FOR lr_event.

    ENDIF.
    go_alv->display( ).
  ENDIF.



ENDFUNCTION.
