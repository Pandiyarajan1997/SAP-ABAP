class ZCL_EXCEL_UPLOADER definition
  public
  final
  create public .

public section.

  data HEADER_ROWS_COUNT type I .
  data MAX_ROWS type I .
  data FILENAME type LOCALFILE .

  methods F4_HELP
    exporting
      !P_FNAME type LOCALFILE .
  methods UPLOAD
    changing
      !CT_DATA type ANY TABLE .
**********************************************************************Customer block/unblock SALV
  methods BUILD_ALV_CB_UB
    changing
      !IV_DATA type ANY TABLE .
  methods CONSTRUCTOR .
  PROTECTED SECTION.
private section.

  data LV_TOT_COMPONENTS type I .

  methods DO_UPLOAD
    importing
      !IV_BEGIN type I
      !IV_END type I
    exporting
      !RV_EMPTY type FLAG
    changing
      !CT_DATA type STANDARD TABLE .
ENDCLASS.



CLASS ZCL_EXCEL_UPLOADER IMPLEMENTATION.


  METHOD upload.
    DATA(lo_table) = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( p_data = ct_data  ) ).
    DATA(lo_struct) = CAST cl_abap_structdescr( lo_table->get_table_line_type( ) )->components.
**
    lv_tot_components = lines( lo_struct ).
*
    DATA: lv_empty TYPE flag,
          lv_begin TYPE i,
          lv_end   TYPE i.
*
    lv_begin = header_rows_count + 1.
    lv_end   = max_rows.
    WHILE lv_empty IS INITIAL.
      do_upload(
        EXPORTING
            iv_begin = lv_begin
            iv_end   = lv_end
        IMPORTING
            rv_empty = lv_empty
        CHANGING
            ct_data  = ct_data
      ).
      lv_begin = lv_end + 1.
      lv_end   = lv_begin + max_rows.
    ENDWHILE.
  ENDMETHOD.


  METHOD do_upload.
    DATA: li_exceldata  TYPE STANDARD TABLE OF alsmex_tabline.
    DATA: ls_exceldata  LIKE LINE OF li_exceldata.
    DATA: lv_tot_rows   TYPE i.
    DATA: lv_packet     TYPE i.
    FIELD-SYMBOLS: <struc> TYPE any,
                   <field> TYPE any.

*   Upload this packet
    CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
      EXPORTING
        filename                = filename
        i_begin_col             = 1
        i_begin_row             = iv_begin
        i_end_col               = lv_tot_components
        i_end_row               = iv_end
      TABLES
        intern                  = li_exceldata
      EXCEPTIONS
        inconsistent_parameters = 1
        upload_ole              = 2
        OTHERS                  = 3.
*   something wrong, exit
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      rv_empty = 'X'.
      EXIT.
    ENDIF.

*   No rows uploaded, exit
    IF li_exceldata IS INITIAL.
      rv_empty = 'X'.
      EXIT.
    ENDIF.

*   Move from Row, Col to Flat Structure
    LOOP AT li_exceldata INTO ls_exceldata.
      " Append new row
      AT NEW row.
        APPEND INITIAL LINE TO ct_data ASSIGNING <struc>.
      ENDAT.

      " component and its value
      ASSIGN COMPONENT ls_exceldata-col OF STRUCTURE <struc> TO <field>.
      IF sy-subrc EQ 0.
        <field> = ls_exceldata-value.
      ENDIF.

      " add the row count
      AT END OF row.
        IF <struc> IS NOT INITIAL.
          lv_tot_rows = lv_tot_rows + 1.
        ENDIF.
      ENDAT.
    ENDLOOP.

*   packet has more rows than uploaded rows,
*   no more packet left. Thus exit
    lv_packet = iv_end - iv_begin.
    IF lv_tot_rows LT lv_packet.
      rv_empty = 'X'.
    ENDIF.
  ENDMETHOD.


  METHOD f4_help.
    CALL FUNCTION 'F4_FILENAME'
      EXPORTING
        program_name  = syst-cprog
        dynpro_number = syst-dynnr
        field_name    = 'p_fname'
      IMPORTING
        file_name     = p_fname.
  ENDMETHOD.


  METHOD build_alv_cb_ub.
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

* Create the ALV object
    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = lo_gr_alv
          CHANGING
            t_table      = iv_data.
      CATCH cx_salv_msg.
    ENDTRY.
* Let's show all default buttons of ALV
    lo_gr_functions = lo_gr_alv->get_functions( ).
    lo_gr_functions->set_all( abap_true ).

* Fit the columns
    lo_columns = lo_gr_alv->get_columns( ).
    lo_columns->set_optimize( 'X' ).
* Apply zebra style to lv_rows
    lo_display = lo_gr_alv->get_display_settings( ).
    lo_display->set_striped_pattern( cl_salv_display_settings=>true ).

    TRY.
        lo_column ?= lo_columns->get_column( 'CUSTOMER' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Customer' ).
        lo_column->set_medium_text( 'Customer' ).
        lo_column->set_short_text( 'Customer' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'CUSTOMER_TYPE' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Customer Type' ).
        lo_column->set_medium_text( 'CusType' ).
        lo_column->set_short_text( 'CusType' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'BLOCK_INDICATOR' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Status' ).
        lo_column->set_medium_text( 'Status ' ).
        lo_column->set_short_text( 'Status' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.
    TRY.
        lo_column ?= lo_columns->get_column( 'MESSAGE' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Message' ).
        lo_column->set_medium_text( 'Message ' ).
        lo_column->set_short_text( 'Message' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.
        TRY.
        lo_column ?= lo_columns->get_column( 'TYPE' ).
        lo_column->set_visible( if_salv_c_bool_sap=>false ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.
    lo_gr_alv->display( ).
  ENDMETHOD.


  METHOD constructor.
    max_rows = 9999.
  ENDMETHOD.
ENDCLASS.
