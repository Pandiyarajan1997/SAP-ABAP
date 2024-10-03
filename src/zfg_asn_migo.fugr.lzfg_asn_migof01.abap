
*----------------------------------------------------------------------*
***INCLUDE LZFG_ASN_MIGOF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form f_set_coloumns
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_set_coloumns USING i_status_field_name.

  DATA: lr_cols TYPE REF TO cl_salv_columns_list,
        lr_col  TYPE REF TO cl_salv_column_list.

  lr_cols = go_alv->get_columns( ).
  IF i_status_field_name IS NOT INITIAL.
    TRY.
        lr_cols->set_exception_column( i_status_field_name ).
        lr_cols->set_color_column( 'TABCOL' ).
      CATCH cx_salv_data_error.                         "#EC NO_HANDLER
    ENDTRY.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_hide_coloumns
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> I_STATUS_FIELD_NAME
*&---------------------------------------------------------------------*
FORM f_hide_coloumns  USING  i_hide_column.
*
*  DATA: lr_col  TYPE REF TO cl_salv_column_list.
*
*  DATA(lr_columns) = go_alv->get_columns( ).
*  lr_col ?= lr_columns->get_column( i_hide_column ).
*  lr_col->set_visible( abap_false ).

  DATA: lr_col  TYPE REF TO cl_salv_column_list.
  DATA: l_hide_colunm TYPE fieldname.
  SPLIT i_hide_column AT ';' INTO TABLE DATA(lt_fields).
  DATA(lr_columns) = go_alv->get_columns( ).
  LOOP AT lt_fields INTO DATA(lw_field).
    l_hide_colunm = lw_field.
    lr_col ?= lr_columns->get_column( l_hide_colunm ).
    lr_col->set_visible( abap_false ).
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_hyperlink_column
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> I_HYPERLINK_COLUMN
*&---------------------------------------------------------------------*
FORM f_hyperlink_column  USING    i_hyperlink_column.
  DATA: lr_col  TYPE REF TO cl_salv_column_list.

  DATA(lr_columns) = go_alv->get_columns( ).
  lr_col ?= lr_columns->get_column( i_hyperlink_column ).
  lr_col->set_cell_type( if_salv_c_cell_type=>hotspot ).

ENDFORM.
