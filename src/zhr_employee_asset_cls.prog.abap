*&---------------------------------------------------------------------*
*& Include          ZHR_EMPLOYEE_ASSET_CLS
*&---------------------------------------------------------------------*
CLASS lcl_asset DEFINITION.
  PUBLIC SECTION.
    METHODS: file_name,
      screen_adjust,
      excel_download,
      upload,
      update,
      display.
ENDCLASS.

CLASS lcl_asset IMPLEMENTATION.
  METHOD file_name.
    CALL FUNCTION 'F4_FILENAME'
      EXPORTING
        program_name  = syst-cprog
        dynpro_number = syst-dynnr
        field_name    = 'P_FNAME'
      IMPORTING
        file_name     = p_fname.
  ENDMETHOD.

  METHOD screen_adjust.
    LOOP AT SCREEN.
      IF p_rad1 IS NOT INITIAL.
        IF screen-group1 = 'BL3'.
          screen-input = '0'.
          screen-invisible = '1'.
          MODIFY SCREEN.
        ENDIF.

      ELSE.
        IF screen-group1 = 'BL2'.
          screen-input = '0'.
          screen-invisible = '1'.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD excel_download.
    GET REFERENCE OF gt_upload INTO lr_excel_structure.
    DATA(lo_itab_services) = cl_salv_itab_services=>create_for_table_ref( lr_excel_structure ).
    lo_source_table_descr ?= cl_abap_tabledescr=>describe_by_data_ref( lr_excel_structure ).
    lo_table_row_descripter ?= lo_source_table_descr->get_table_line_type( ).

    DATA(lo_tool_xls) = cl_salv_export_tool_ats_xls=>create_for_excel(
                                 EXPORTING r_data = lr_excel_structure ).

    DATA(lo_config) = lo_tool_xls->configuration( ).
    lo_config->add_column(
           EXPORTING
             header_text = 'Employee Number'
             field_name = 'PERNR'
             display_type = if_salv_bs_model_column=>uie_text_view ).

    lo_config->add_column(
          EXPORTING
            header_text = 'Subtype'
            field_name = 'SUBTY'
            display_type = if_salv_bs_model_column=>uie_text_view ).

    lo_config->add_column(
          EXPORTING
            header_text = 'Asset ID'
            field_name = 'LOBNR'
            display_type = if_salv_bs_model_column=>uie_text_view ).

    lo_config->add_column(
        EXPORTING
          header_text = 'Serial_no'
          field_name = 'SERIAL_NO'
          display_type = if_salv_bs_model_column=>uie_text_view ).

    lo_config->add_column(
          EXPORTING
            header_text = 'Asset_tag'
            field_name = 'ASSET_TAG'
            display_type = if_salv_bs_model_column=>uie_text_view ).

    lo_config->add_column(
        EXPORTING
          header_text = 'Company code'
          field_name = 'BUKRS'
          display_type = if_salv_bs_model_column=>uie_text_view ).

    lo_config->add_column(
        EXPORTING
          header_text = 'Plant'
          field_name = 'WERKS'
          display_type = if_salv_bs_model_column=>uie_text_view ).

    lo_config->add_column(
          EXPORTING
            header_text = 'Costcenter'
            field_name = 'KOSTL'
            display_type = if_salv_bs_model_column=>uie_text_view ).

    TRY.
        lo_tool_xls->read_result( IMPORTING content = lv_content ).
      CATCH cx_root.
    ENDTRY.

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = lv_content
      IMPORTING
        output_length = lv_length
      TABLES
        binary_tab    = lt_binary_tab.

    lv_filename1 = | { 'Employee Asset Upload' } |.
    CALL METHOD cl_gui_frontend_services=>file_save_dialog
      EXPORTING
        window_title      = 'Enter file name'
        default_extension = 'XLSX'
        default_file_name = lv_filename1
      CHANGING
        filename          = lv_filename1
        path              = lv_path
        fullpath          = lv_fullpath.

    IF lv_fullpath IS NOT INITIAL.
      CALL FUNCTION 'GUI_DOWNLOAD'
        EXPORTING
          bin_filesize            = lv_length
          filename                = lv_fullpath
          filetype                = 'BIN'
        TABLES
          data_tab                = lt_binary_tab
        EXCEPTIONS
          file_write_error        = 1
          no_batch                = 2
          gui_refuse_filetransfer = 3
          invalid_type            = 4
          no_authority            = 5
          unknown_error           = 6
          header_not_allowed      = 7
          separator_not_allowed   = 8
          filesize_not_allowed    = 9
          header_too_long         = 10
          dp_error_create         = 11
          dp_error_send           = 12
          dp_error_write          = 13
          unknown_dp_error        = 14
          access_denied           = 15
          dp_out_of_memory        = 16
          disk_full               = 17
          dp_timeout              = 18
          file_not_found          = 19
          dataprovider_exception  = 20
          control_flush_error     = 21
          OTHERS                  = 22.
      IF sy-subrc <> 0.
      ELSE.
        CALL METHOD cl_gui_frontend_services=>execute
          EXPORTING
            document               = lv_fullpath
*           application            =
*           parameter              =
*           default_directory      =
*           maximized              =
*           minimized              =
*           synchronous            =
*           operation              = 'OPEN'
          EXCEPTIONS
            cntl_error             = 1
            error_no_gui           = 2
            bad_parameter          = 3
            file_not_found         = 4
            path_not_found         = 5
            file_extension_unknown = 6
            error_execute_failed   = 7
            synchronous_failed     = 8
            not_supported_by_gui   = 9
            OTHERS                 = 10.
        IF sy-subrc <> 0.
*       Implement suitable error handling here
        ENDIF.

      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD upload.
    IF p_fname IS NOT INITIAL.
      CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
        EXPORTING
          i_line_header        = 'X'
          i_tab_raw_data       = lt_type
          i_filename           = p_fname
        TABLES
          i_tab_converted_data = gt_upload[]
        EXCEPTIONS
          conversion_failed    = 1
          OTHERS               = 2.
      IF sy-subrc <> 0.
        MESSAGE 'Conversion failed' TYPE 'E'.
      ENDIF.
    ELSE.
      MESSAGE 'Please excel file for Upload' TYPE 'E'.
    ENDIF.
  ENDMETHOD.
  METHOD update.
    DATA: ls_0040 TYPE p0040.
    DATA: ls_ret TYPE bapireturn1.

    LOOP AT gt_upload ASSIGNING FIELD-SYMBOL(<fs_upload>).
**Pernr Checks if not present  **
      IF <fs_upload>-pernr IS INITIAL.
        CLEAR gs_display.
        gs_display-pernr = <fs_upload>-pernr.
        gs_display-t_error = 'E'.
        gs_display-message = 'Enter Employee Number'.
        APPEND gs_display TO gt_display.
        CONTINUE.
      ELSE.
**Pernr Checks if present  **
        CLEAR gs_pernr.
        READ TABLE gt_pernr INTO gs_pernr WITH KEY pernr = <fs_upload>-pernr BINARY SEARCH.
        IF sy-subrc NE 0.
          CLEAR gs_display.
          gs_display-pernr = <fs_upload>-pernr.
          gs_display-t_error = 'E'.
          gs_display-message = 'Invalid Employee Number'.
          APPEND gs_display TO gt_display.
          CONTINUE.
        ENDIF.
      ENDIF.
***Check for Subtype **
      IF <fs_upload>-subty IS INITIAL.
        CLEAR gs_display.
        gs_display-pernr = <fs_upload>-pernr.
        gs_display-sname = VALUE #( gt_pernr[ pernr = <fs_upload>-pernr ]-sname OPTIONAL ).
        gs_display-t_error = 'E'.
        gs_display-message = 'Subtype is Mandatory'.
        APPEND gs_display TO gt_display.
        CONTINUE.
      ELSE.
        CLEAR gs_asset_typ.
        READ TABLE gt_asset_typ INTO gs_asset_typ WITH KEY asset_type = <fs_upload>-subty.
        IF sy-subrc NE 0.
          CLEAR gs_display.
          gs_display-pernr = <fs_upload>-pernr.
          gs_display-sname = VALUE #( gt_pernr[ pernr = <fs_upload>-pernr ]-sname OPTIONAL ).
          gs_display-t_error = 'E'.
          gs_display-message = 'Invalid Subtype'.
          APPEND gs_display TO gt_display.
          CONTINUE.
        ENDIF.
      ENDIF.
*--------- Costcenter Checks based on excel costcenter ------------------------------------*
      IF <fs_upload>-kostl IS NOT INITIAL.
        DATA(lv_cc) = VALUE #( gt_kostl[ kostl = <fs_upload>-kostl ]-kostl OPTIONAL ).
        IF lv_cc IS INITIAL.
          CLEAR gs_display.
          gs_display-pernr = <fs_upload>-pernr.
          gs_display-sname = VALUE #( gt_pernr[ pernr = <fs_upload>-pernr ]-sname OPTIONAL ).
          gs_display-t_error = 'E'.
          gs_display-message = 'Invalid Costcenter'.
          APPEND gs_display TO gt_display.
          CONTINUE.
        ENDIF.
      ENDIF.
*----------- Plant Checks Based on excel Plant ----------------------------------------*
      IF <fs_upload>-werks IS NOT INITIAL.
        SELECT SINGLE werks FROM t001w INTO @DATA(lv_werks) WHERE werks = @<fs_upload>-werks.
        IF sy-subrc NE 0.
          CLEAR gs_display.
          gs_display-pernr = <fs_upload>-pernr.
          gs_display-sname = VALUE #( gt_pernr[ pernr = <fs_upload>-pernr ]-sname OPTIONAL ).
          gs_display-t_error = 'E'.
          gs_display-message = 'Invalid Plant'.
          APPEND gs_display TO gt_display.
          CONTINUE.
        ENDIF.
      ENDIF.
* Same Check
      SELECT SINGLE * FROM pa0040 INTO @DATA(l_pa0040)
        WHERE pernr = @<fs_upload>-pernr
        AND subty = @<fs_upload>-subty
        AND begda LE @sy-datum
        AND endda GE @sy-datum.
      IF sy-subrc EQ 0.
        IF l_pa0040-lobnr = <fs_upload>-lobnr AND l_pa0040-serial_no = <fs_upload>-serial_no AND
           l_pa0040-asset_tag = <fs_upload>-asset_tag AND l_pa0040-bukrs = <fs_upload>-bukrs AND
           l_pa0040-werks = <fs_upload>-werks AND l_pa0040-kostl = <fs_upload>-kostl.
          CLEAR gs_display.
          gs_display-pernr = <fs_upload>-pernr.
          gs_display-sname = VALUE #( gt_pernr[ pernr = <fs_upload>-pernr ]-sname OPTIONAL ).
          gs_display-t_error = 'S'.
          gs_display-message = 'Data is already Same'.
          APPEND gs_display TO gt_display.
          CONTINUE.
        ENDIF.
      ENDIF.
*** Already assigned to any other employee Checks ***
      SELECT SINGLE * FROM pa0040 INTO @DATA(l_duplicate_chk)
        WHERE lobnr = @<fs_upload>-lobnr
        AND serial_no = @<fs_upload>-serial_no
        AND asset_tag = @<fs_upload>-asset_tag
        AND begda LE @sy-datum
        AND endda GE @sy-datum.
      IF sy-subrc EQ 0.
        CLEAR gs_display.
        gs_display-pernr = <fs_upload>-pernr.
        gs_display-sname = VALUE #( gt_pernr[ pernr = <fs_upload>-pernr ]-sname OPTIONAL ).
        gs_display-t_error = 'E'.
        gs_display-message = 'Already assets are assigned to someone'.
        APPEND gs_display TO gt_display.
        CONTINUE.
      ENDIF.
*** Insertion Of Record In Infotype 0040 ***
      CLEAR ls_ret.
***Lock pernr For Data update ***
      CALL FUNCTION 'BAPI_EMPLOYEE_ENQUEUE'
        EXPORTING
          number = <fs_upload>-pernr
        IMPORTING
          return = ls_ret.
      IF ( ls_ret-type NE 'E' AND ls_ret-type NE 'A' ).
        CLEAR:ls_0040, ls_ret.
        ls_0040-pernr = <fs_upload>-pernr.
        ls_0040-subty = <fs_upload>-subty.
        READ TABLE gt_hire_date INTO gs_hire_date WITH KEY pernr = <fs_upload>-pernr.
        IF sy-subrc EQ 0.
          ls_0040-begda = gs_hire_date-dat01.
        ENDIF.
        ls_0040-endda = '99991231'.
        ls_0040-lobnr = <fs_upload>-lobnr. " Asset ID
        ls_0040-serial_no = <fs_upload>-serial_no. "Device Serial Number
        ls_0040-asset_tag = <fs_upload>-asset_tag. "Asset Tag
        ls_0040-bukrs = <fs_upload>-bukrs. "Company Code
        ls_0040-werks = <fs_upload>-werks. "Plant
        ls_0040-kostl = <fs_upload>-kostl. "Costcenter
**Function Module To update Infotype **
        CLEAR: ls_ret.
        CALL FUNCTION 'HR_INFOTYPE_OPERATION'
          EXPORTING
            infty         = '0040'
            number        = ls_0040-pernr
            subtype       = ls_0040-subty
            validityend   = ls_0040-endda
            validitybegin = ls_0040-begda
            record        = ls_0040
            operation     = 'INS'
          IMPORTING
            return        = ls_ret.
        IF ( ls_ret-type NE 'E' AND ls_ret-type NE 'A' ).
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.
          CALL FUNCTION 'HR_PSBUFFER_INITIALIZE'.
          CLEAR gs_display.
          gs_display-pernr = <fs_upload>-pernr.
          gs_display-sname = VALUE #( gt_pernr[ pernr = <fs_upload>-pernr ]-sname OPTIONAL ).
          gs_display-t_error = 'S'.
          gs_display-message = 'Data Inserted Successfully'.
          APPEND gs_display TO gt_display.
        ELSE.
          CLEAR gs_display.
          gs_display-pernr = <fs_upload>-pernr.
          gs_display-sname = VALUE #( gt_pernr[ pernr = <fs_upload>-pernr ]-sname OPTIONAL ).
          gs_display-t_error = 'E'.
          gs_display-message = ls_ret-message.
          APPEND gs_display TO gt_display.
        ENDIF.
        CALL FUNCTION 'BAPI_EMPLOYEE_DEQUEUE'
          EXPORTING
            number = <fs_upload>-pernr
          IMPORTING
            return = ls_ret.
      ELSE.
        CLEAR gs_display.
        gs_display-pernr = <fs_upload>-pernr.
        gs_display-sname = VALUE #( gt_pernr[ pernr = <fs_upload>-pernr ]-sname OPTIONAL ).
        gs_display-t_error = 'E'.
        gs_display-message = 'Employee id already locked in SAP'.
        APPEND gs_display TO gt_display.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD display.
    DATA: lr_columns    TYPE REF TO cl_salv_columns_table,
          lr_column     TYPE REF TO cl_salv_column_table,
          lo_column     TYPE REF TO cl_salv_column,
          lr_selections TYPE REF TO cl_salv_selections.
    IF gt_display[] IS NOT INITIAL.
      TRY.
          cl_salv_table=>factory(
            IMPORTING
              r_salv_table = gr_table
            CHANGING
              t_table      = gt_display ).
        CATCH cx_salv_msg.                              "#EC NO_HANDLER
      ENDTRY.

      gr_table->set_screen_status(
      pfstatus      =  'STANDARD_FULLSCREEN'
      report        =  'SAPLKKBL'
      set_functions = gr_table->c_functions_all ).

*Columns Optimize
      lr_columns = gr_table->get_columns( ).
      lr_columns->set_optimize( abap_true ).
*multiple row SELECTION
      lr_selections = gr_table->get_selections( ).
      lr_selections->set_selection_mode( if_salv_c_selection_mode=>cell ).

*Column description change
      TRY.
          lo_column = lr_columns->get_column( 'T_ERROR' ).
          lo_column->set_long_text( 'Error type' ).
          lo_column->set_medium_text( 'Error Type' ).
          lo_column->set_short_text( 'Error Type' ).
        CATCH cx_salv_not_found.
        CATCH cx_salv_existing.
        CATCH cx_salv_data_error.
      ENDTRY.
*Column description change
      TRY.
          lo_column = lr_columns->get_column( 'SNAME' ).
          lo_column->set_long_text( 'Empname' ).
          lo_column->set_medium_text( 'Empname' ).
          lo_column->set_short_text( 'Empname' ).
        CATCH cx_salv_not_found.
        CATCH cx_salv_existing.
        CATCH cx_salv_data_error.
      ENDTRY.
*Column description change
      TRY.
          lo_column = lr_columns->get_column( 'MESSAGE' ).
          lo_column->set_long_text( 'Message' ).
          lo_column->set_medium_text( 'Message' ).
          lo_column->set_short_text( 'Message' ).
        CATCH cx_salv_not_found.
        CATCH cx_salv_existing.
        CATCH cx_salv_data_error.
      ENDTRY.
    ENDIF.

    gr_table->display( ).
  ENDMETHOD.
ENDCLASS.
