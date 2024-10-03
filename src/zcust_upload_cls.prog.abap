*&---------------------------------------------------------------------*
*& Include          ZCUST_UPLOAD_CLS
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-003.
  PARAMETERS: p_fname TYPE rlgrap-filename OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

CLASS lcl_upload DEFINITION.
  PUBLIC SECTION.
    METHODS: f4_help, convert_xls_to_sap, actual_upload, build_alv.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_str,
             customer_no TYPE kunnr,
             status      TYPE z_status,
           END OF ty_str,
           BEGIN OF lty_disp,
             customer_no  TYPE kunnr,
             cusname      TYPE name1_gp,
             status       TYPE z_status,
             type         TYPE bapi_mtype,
             message(100) TYPE c,
           END OF lty_disp.
    DATA: gt_type  TYPE truxs_t_text_data,
          gt_excel TYPE TABLE OF ty_str,
          gt_alv   TYPE TABLE OF lty_disp.
ENDCLASS.
CLASS lcl_upload IMPLEMENTATION.
  METHOD f4_help.
    CALL FUNCTION 'F4_FILENAME'
      EXPORTING
        program_name  = syst-cprog
        dynpro_number = syst-dynnr
        field_name    = 'p_fname'
      IMPORTING
        file_name     = p_fname.
  ENDMETHOD.

  METHOD convert_xls_to_sap.
    REFRESH: gt_excel.
    CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
      EXPORTING
*       I_FIELD_SEPERATOR    =
        i_line_header        = 'X'
        i_tab_raw_data       = gt_type
        i_filename           = p_fname
      TABLES
        i_tab_converted_data = gt_excel
      EXCEPTIONS
        conversion_failed    = 1
        OTHERS               = 2.
    IF sy-subrc <> 0.
      MESSAGE 'Conversion error' TYPE 'E'.
    ENDIF.
  ENDMETHOD.

  METHOD actual_upload.
    REFRESH: gt_alv.
    LOOP AT gt_excel ASSIGNING FIELD-SYMBOL(<fs_excel>).

      <fs_excel>-customer_no = |{ <fs_excel>-customer_no ALPHA = IN }|.

      SELECT SINGLE * FROM kna1 INTO @DATA(l_customer)
        WHERE kunnr = @<fs_excel>-customer_no.
      IF sy-subrc NE 0.
        APPEND VALUE #( customer_no = <fs_excel>-customer_no
                        status      = <fs_excel>-status
                        type        = 'E'
                        message     = |Incorrect Customer Number| ) TO gt_alv.
        CONTINUE.
      ENDIF.

      SELECT SINGLE * FROM zmis_cust_st INTO @DATA(l_custstatus)
        WHERE customer_no = @l_customer-kunnr.
      IF sy-subrc EQ 0.
        l_custstatus-status     = <fs_excel>-status.
        l_custstatus-changed_by = sy-uname.
        l_custstatus-changed_on = sy-datum.
        MODIFY zmis_cust_st FROM l_custstatus.

        APPEND VALUE #( customer_no = l_customer-kunnr
                        cusname     = l_customer-name1
                        status      = <fs_excel>-status
                        type        = 'S'
                        message     = |Data Updated| ) TO gt_alv.

      ELSE.
        l_custstatus-customer_no = l_customer-kunnr.
        l_custstatus-status      = <fs_excel>-status.
        l_custstatus-create_on   = sy-datum.
        l_custstatus-created_by  = sy-uname.
        MODIFY zmis_cust_st FROM l_custstatus.

        APPEND VALUE #( customer_no = <fs_excel>-customer_no
        cusname     = l_customer-name1
                        status      = <fs_excel>-status
                        type        = 'S'
                        message     = |Data Created| ) TO gt_alv.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD build_alv.

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
            t_table      = gt_alv.
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
        lo_column ?= lo_columns->get_column( 'CUSTOMER_NO' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Customer' ).
        lo_column->set_medium_text( 'Customer' ).
        lo_column->set_short_text( 'Customer' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'CUSNAME' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Cusname' ).
        lo_column->set_medium_text( 'Cusname' ).
        lo_column->set_short_text( 'Cusname' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'STATUS' ).
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
    lo_gr_alv->display( ).
  ENDMETHOD.

ENDCLASS.
