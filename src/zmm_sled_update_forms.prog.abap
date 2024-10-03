*&---------------------------------------------------------------------*
*& Include zmm_sled_update_forms
*&---------------------------------------------------------------------*
*--------------Upload_Request-------------------*
FORM upload_requests USING  p_fname.
  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name = syst-cprog
    IMPORTING
      file_name    = p_fname.
ENDFORM.
FORM SLED_Process CHANGING lt_upload LIKE gt_upload
                           it_rows   LIKE lt_rows
                           lt_sled_logs LIKE gt_sled_logs.
  READ TABLE lt_sled_logs TRANSPORTING NO FIELDS WITH KEY check = abap_true.
  IF sy-subrc NE 0.
    MESSAGE 'Please select a checkbox' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.
    LOOP AT lt_sled_logs INTO DATA(is_log) WHERE check IS NOT INITIAL.
    DATA(lv_ind) = sy-tabix.
    READ TABLE lt_sled_logs ASSIGNING FIELD-SYMBOL(<fs_sled>) INDEX lv_ind.
    IF sy-subrc = 0 AND <fs_sled>-msgtype = '@0A@'.
      MESSAGE |{ <fs_sled>-msg }| TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
  ENDLOOP.
  clear lv_ind.
  LOOP AT lt_sled_logs INTO DATA(is_row) WHERE check IS NOT INITIAL.
   lv_ind = sy-tabix.
    READ TABLE lt_upload ASSIGNING FIELD-SYMBOL(<fs_upl>) INDEX lv_ind.
    IF sy-subrc = 0.
      READ TABLE lt_sled_logs ASSIGNING FIELD-SYMBOL(<fs_data>) INDEX lv_ind.
      IF sy-subrc = 0 AND <fs_data>-msg CS 'Successfully'.
        DATA(lv_matnr) = <fs_upl>-matnr.
        SHIFT lv_matnr LEFT DELETING LEADING '0'.
        MESSAGE |SLED has been Already Extended | TYPE 'S' DISPLAY LIKE 'E'.
        "For Material { lv_matnr },Plant { <fs_upl>-plant },Storage Loc. { <fs_upl>-stor_loc },Batch { <fs_upl>-batch }| TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ELSEIF  <fs_data>-msgtype = '@0A@'.
        lv_matnr = <fs_upl>-matnr.
        SHIFT lv_matnr LEFT DELETING LEADING '0'.
        MESSAGE |{ <fs_data>-msg }| TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
      IF <fs_upl>-sled IS INITIAL.
        CONTINUE.
      ENDIF.
      DATA(i_batch_attr) = VALUE bapibatchatt( expirydate = |{ <fs_upl>-sled+6(4) }{ <fs_upl>-sled+3(2) }{ <fs_upl>-sled+0(2) }| ).
      DATA(i_batch_attrX) = VALUE bapibatchattx( expirydate = abap_true ).
      REFRESH gt_return.
      CALL FUNCTION 'BAPI_BATCH_CHANGE'
        EXPORTING
          material         = <fs_upl>-matnr+0(18)
          batch            = <fs_upl>-batch
          plant            = <fs_upl>-plant
          batchattributes  = i_batch_attr
          batchattributesx = i_batch_attrX
        IMPORTING
          batchattributes  = gt_batch_details
        TABLES
          return           = gt_return.

      IF line_exists( gt_return[ type = 'E' ] ) OR line_exists( gt_return[ type = 'A' ] ).
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        READ TABLE gt_return INTO DATA(ls_return) INDEX 1.
        IF sy-subrc = 0.
          READ TABLE lt_sled_logs ASSIGNING FIELD-SYMBOL(<ls_data>) WITH KEY matnr = <fs_upl>-matnr
                                                                            plant  = <fs_upl>-plant
                                                                            lgort  = <fs_upl>-stor_loc
                                                                            charg  = <fs_upl>-batch.
          IF sy-subrc = 0.
            <ls_data>-msgtype = 1.
            <ls_data>-msg = ls_return-message.
          ENDIF.
        ENDIF.
      ELSE.
        REFRESH gt_return.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
        READ TABLE lt_sled_logs ASSIGNING FIELD-SYMBOL(<gs_data>) WITH KEY matnr = <fs_upl>-matnr
                                                                         plant  = <fs_upl>-plant
                                                                         lgort  = <fs_upl>-stor_loc
                                                                         charg  = <fs_upl>-batch.
        IF sy-subrc = 0.
          <gs_data>-msg = |SLED Extended Successfully |.
        ENDIF.
        MODIFY zsled_log_table FROM @( VALUE #( mandt = sy-mandt
                                                matnr = <fs_upl>-matnr
                                                plant = <fs_upl>-plant
                                                lgort = <fs_upl>-stor_loc
                                                charg = <fs_upl>-batch
                                            curr_sled = i_batch_attr-expirydate
                                            act_sled  = <gs_data>-act_sled
                                                erdat = sy-datum
                                                ernam = sy-uname
                                                erzet = sy-uzeit
                                                 ) ).
        COMMIT WORK.

      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.
FORM Get_Num_range USING p1 p2 CHANGING p3.
  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = p1
      object                  = p2
    IMPORTING
      number                  = p3
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form Excel_download
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM Excel_download .
  GET REFERENCE OF gt_upload INTO lr_excel_structure.
  DATA(lo_itab_services) = cl_salv_itab_services=>create_for_table_ref( lr_excel_structure ).
  lo_source_table_descr ?= cl_abap_tabledescr=>describe_by_data_ref( lr_excel_structure ).
  lo_table_row_descripter ?= lo_source_table_descr->get_table_line_type( ).

  DATA(lo_tool_xls) = cl_salv_export_tool_ats_xls=>create_for_excel(
                               EXPORTING r_data = lr_excel_structure ).

  DATA(lo_config) = lo_tool_xls->configuration( ).
  IF sy-repid = 'ZMM_SLED_UPDATE_REP'.
    lo_config->add_column(
           EXPORTING
             header_text = 'Material Number'
             field_name = 'MATNR'
             display_type = if_salv_bs_model_column=>uie_text_view ).

    lo_config->add_column(
          EXPORTING
            header_text = 'Plant'
            field_name = 'PLANT'
            display_type = if_salv_bs_model_column=>uie_text_view ).

    lo_config->add_column(
          EXPORTING
            header_text = 'Storage Location'
            field_name = 'STOR_LOC'
            display_type = if_salv_bs_model_column=>uie_text_view ).

    lo_config->add_column(
        EXPORTING
          header_text = 'Batch'
          field_name = 'BATCH'
          display_type = if_salv_bs_model_column=>uie_text_view ).

    lo_config->add_column(
          EXPORTING
            header_text = 'SLED'
            field_name = 'SLED'
            display_type = if_salv_bs_model_column=>uie_text_view ).
  ELSE.
    lo_config->add_column(
           EXPORTING
             header_text = 'Material Number'
             field_name = 'MATNR'
             display_type = if_salv_bs_model_column=>uie_text_view ).

    lo_config->add_column(
          EXPORTING
            header_text = 'Plant'
            field_name = 'PLANT'
            display_type = if_salv_bs_model_column=>uie_text_view ).

    lo_config->add_column(
          EXPORTING
            header_text = 'Storage Location'
            field_name = 'STOR_LOC'
            display_type = if_salv_bs_model_column=>uie_text_view ).

    lo_config->add_column(
        EXPORTING
          header_text = 'Batch'
          field_name = 'BATCH'
          display_type = if_salv_bs_model_column=>uie_text_view ).

*  lo_config->add_column(
*        EXPORTING
*          header_text = 'SLED'
*          field_name = 'SLED'
*          display_type = if_salv_bs_model_column=>uie_text_view ).

    lo_config->add_column(
        EXPORTING
          header_text = 'Quantity'
          field_name = 'QTY'
          display_type = if_salv_bs_model_column=>uie_text_view ).

  ENDIF.


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

  lv_filename1 = SWITCH #( sy-repid WHEN 'ZMM_SLED_UPDATE_REP' THEN 'Material SLED Data' ELSE 'Transfer Post Data' ).
  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      window_title      = 'Enter file name'
      default_extension = 'xlsx'
      default_file_name = lv_filename1
      file_filter       = 'Excel files (*.XLSX)|*.XLSX|Excel files (*.XLS)|*.XLS|Text files (*.txt)|*.txt|All Files (*.*)|*.*|'
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
*         application            =
*         parameter              =
*         default_directory      =
*         maximized              =
*         minimized              =
*         synchronous            =
*         operation              = 'OPEN'
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
ENDFORM.
FORM SLED_validation USING lt_upload LIKE gt_upload
                      CHANGING lt_logs LIKE gt_sled_logs.
  LOOP AT lt_upload ASSIGNING FIELD-SYMBOL(<ls_upload>).
    matnr = |{  <ls_upload>-matnr ALPHA = IN }|.
    iv_sled = |{ <ls_upload>-sled+6(4) }{ <ls_upload>-sled+3(2) }{ <ls_upload>-sled+0(2) }|.
    IF <ls_upload>-sled IS NOT INITIAL.
      SELECT SINGLE FROM mch1 FIELDS vfdat WHERE matnr = @matnr AND charg = @( CONV #( <ls_upload>-batch ) )
                                                          INTO @DATA(iv_vfdat).
      IF sy-subrc = 0.
        DATA(act_sled) = |{ iv_vfdat+6(2) }.{ iv_vfdat+4(2) }.{ iv_vfdat+0(4) }|.
      ENDIF.
    ENDIF.
*-----Check Material-----*
    IF <ls_upload>-matnr IS NOT INITIAL.
      matnr = |{  <ls_upload>-matnr ALPHA = IN }|.
      SELECT SINGLE FROM mara FIELDS matnr, lvorm WHERE matnr  = @matnr INTO ( @DATA(lv_matnr), @DATA(lv_lvorm) ).
      IF Sy-subrc NE 0.
        APPEND VALUE #( msgtype = '@0A@'
                 matnr = <ls_upload>-matnr plant = <ls_upload>-plant lgort = <Ls_upload>-stor_loc
                 charg = <ls_upload>-batch  curr_sled = iv_sled  act_sled = iv_vfdat
                 msg = |Material is incorrect| ) TO lt_logs.
        CONTINUE.
      ELSEIF lv_lvorm = abap_true. "*-----Check Client Deletion for material-----*
        APPEND VALUE #( msgtype = 1
               matnr = <ls_upload>-matnr plant = <ls_upload>-plant lgort = <Ls_upload>-stor_loc
               charg = <ls_upload>-batch  curr_sled = iv_sled  act_sled = iv_vfdat
               msg = |Material is Blocked at client level| ) TO lt_logs.
        CONTINUE.
      ENDIF.
      <ls_upload>-matnr = matnr.
    ENDIF.
*-----Check Plant-----*
    IF <ls_upload>-plant IS NOT INITIAL.
      SELECT SINGLE FROM marc FIELDS werks WHERE matnr = @lv_matnr AND werks = @<ls_upload>-plant INTO @DATA(lv_werks).
      IF sy-subrc NE 0.
        APPEND VALUE #( msgtype = '@0A@'
                       matnr = <ls_upload>-matnr plant = <ls_upload>-plant lgort = <Ls_upload>-stor_loc
                       charg = <ls_upload>-batch  curr_sled = iv_sled  act_sled = iv_vfdat
                       msg = |Plant is incorrect| ) TO lt_logs.
        CONTINUE.
      ENDIF.
    ENDIF.
*-----Check Storage Location-----*
    IF <ls_upload>-stor_loc IS NOT INITIAL.
      SELECT SINGLE FROM mard FIELDS lgort WHERE matnr = @lv_matnr AND werks = @lv_werks AND lgort = @<ls_upload>-stor_loc  INTO @DATA(ls_lgort).
      IF sy-subrc NE 0.
        APPEND VALUE #( msgtype = '@0A@'
                             matnr = <ls_upload>-matnr plant = <ls_upload>-plant lgort = <Ls_upload>-stor_loc
                             charg = <ls_upload>-batch  curr_sled = iv_sled  act_sled = iv_vfdat
                             msg = |Storage Loc. is incorrect| ) TO lt_logs.
        CONTINUE.
      ENDIF.
    ENDIF.
*-----Check Batch-----*
    IF <ls_upload>-batch IS NOT INITIAL.
      SELECT SINGLE FROM mchb FIELDS charg,cspem WHERE matnr = @lv_matnr
                                       AND werks = @lv_werks
                                       AND lgort = @ls_lgort
                                       AND charg = @<ls_upload>-batch INTO @DATA(lv_batch).
      IF sy-subrc NE 0.
        APPEND VALUE #( msgtype = '@0A@'
                              matnr = <ls_upload>-matnr plant = <ls_upload>-plant lgort = <Ls_upload>-stor_loc
                              charg = <ls_upload>-batch  curr_sled = iv_sled  act_sled = iv_vfdat
                              msg = |Batch is incorrect| ) TO lt_logs.
        CONTINUE.
      ENDIF.
    ENDIF.
*------SLED------*
    IF <ls_upload>-sled IS NOT INITIAL.
      IF sy-subrc = 0.
        IF iv_vfdat IS NOT INITIAL.
          IF  iv_sled LT iv_vfdat .
            APPEND VALUE #( msgtype = '@0A@'
                            matnr = <ls_upload>-matnr plant = <ls_upload>-plant lgort = <Ls_upload>-stor_loc
                            charg = <ls_upload>-batch  curr_sled = iv_sled  act_sled = iv_vfdat
                            msg = |Current SLED is lesser than Actual SLED| )
                               TO lt_logs.
            CONTINUE.
          ELSE.
            APPEND VALUE #( msgtype = '@08@'
                            matnr = <ls_upload>-matnr plant = <ls_upload>-plant lgort = <Ls_upload>-stor_loc
                            charg = <ls_upload>-batch  curr_sled = iv_sled  act_sled = iv_vfdat
                            msg = '' )
                               TO lt_logs.
          ENDIF.
        ELSE.
          APPEND VALUE #( msgtype = '@08@'
                           matnr = <ls_upload>-matnr plant = <ls_upload>-plant lgort = <Ls_upload>-stor_loc
                           charg = <ls_upload>-batch  curr_sled = iv_sled  act_sled = iv_vfdat
                           msg = '' )
                              TO lt_logs.
        ENDIF.
      ENDIF.
      CLEAR:iv_vfdat,matnr.
    ENDIF.


  ENDLOOP.

ENDFORM.
FORM build_alv_sled USING gt_sled_logs.
  DATA: lo_event_handler TYPE REF TO lcl_handle_events , " Variables for events
        lo_events        TYPE REF TO cl_salv_events_table.
* Create the ALV object
  TRY.
      CALL METHOD cl_salv_table=>factory
        IMPORTING
          r_salv_table = lo_gr_alv
        CHANGING
          t_table      = gt_sled_logs.
    CATCH cx_salv_msg.
  ENDTRY.
* Let's show all default buttons of ALV
  lo_gr_functions = lo_gr_alv->get_functions( ).
  lo_gr_functions->set_all( abap_true ).

* Fit the columns
  lo_columns = lo_gr_alv->get_columns( ).
  lo_columns->set_optimize( 'X' ).

*   Apply zebra style to lv_rows
  lo_display = lo_gr_alv->get_display_settings( ).
  lo_display->set_striped_pattern( cl_salv_display_settings=>true ).

  CALL METHOD lo_gr_alv->get_functions
    RECEIVING
      value = lo_gr_functions.

  CALL METHOD lo_gr_functions->set_default
    EXPORTING
      value = if_salv_c_bool_sap=>true.

  lo_gr_alv->set_screen_status(
    pfstatus      =  'SLED_STATUS'
    report        =  sy-repid
    set_functions = lo_gr_alv->c_functions_all ).

* Register events
  lo_events = lo_gr_alv->get_event( ).
  CREATE OBJECT lo_event_handler.
  SET HANDLER lo_event_handler->on_user_command FOR lo_events.
  SET HANDLER lo_event_handler->on_link_click FOR lo_events.


*  lo_selections = lo_gr_alv->get_selections( ).
*  lo_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).
  TRY.

      lo_column ?= lo_columns->get_column( 'CHECK' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_cell_type( value = if_salv_c_cell_type=>checkbox_hotspot ).

      lo_column ?= lo_columns->get_column( 'MSGTYPE' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Status' ).
      lo_column->set_medium_text( 'Status' ).
      lo_column->set_short_text( 'Status' ).

      lo_column ?= lo_columns->get_column( 'PLANT' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Plant' ).
      lo_column->set_medium_text( 'Plant' ).
      lo_column->set_short_text( 'Plant' ).

      lo_column ?= lo_columns->get_column( 'MATNR' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Material' ).
      lo_column->set_medium_text( 'Material' ).
      lo_column->set_short_text( 'Material' ).

      lo_column ?= lo_columns->get_column( 'CURR_SLED' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Current SLED' ).
      lo_column->set_medium_text( 'Current SLED' ).
      lo_column->set_short_text( 'Curr.SLED' ).

      lo_column ?= lo_columns->get_column( 'ACT_SLED' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Actual SLED' ).
      lo_column->set_medium_text( 'Actual SLED' ).
      lo_column->set_short_text( 'Act.SLED' ).

      lo_column ?= lo_columns->get_column( 'MSG' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Message' ).
      lo_column->set_medium_text( '' ).
      lo_column->set_short_text( '' ).
      lo_column->set_output_length( value = 50 ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      CALL METHOD lo_columns->get_column
        EXPORTING
          columnname = 'MSGTYPE'
        RECEIVING
          value      = lo_col_icon.

      lo_icon ?= lo_col_icon.
    CATCH cx_salv_not_found.

      CALL METHOD lo_icon->set_icon
        EXPORTING
          value = if_salv_c_bool_sap=>true.

*      CALL METHOD lo_icon->set_long_text
*        EXPORTING
*          value = 'Status'.
  ENDTRY.

  TRY.
      CALL METHOD lo_columns->set_column_position
        EXPORTING
          columnname = 'CHECK'
          position   = 1.
    CATCH cx_salv_data_error.
  ENDTRY.

  lo_gr_alv->display( ).
ENDFORM.
FORM Refresh_display.
  lo_gr_alv->refresh( ).
  lo_columns->set_optimize( 'X' ).

ENDFORM.
FORM Checkbox_selection  USING i_row TYPE salv_de_row flag TYPE xfeld CHANGING lt_sled_logs LIKE gt_sled_logs.

  IF i_row IS INITIAL.
    IF flag = abap_true.
      LOOP AT lt_sled_logs ASSIGNING FIELD-SYMBOL(<fs_sled>).
        <fs_sled>-check = abap_true.
      ENDLOOP.
    ELSE.
      LOOP AT lt_sled_logs ASSIGNING FIELD-SYMBOL(<gs_sled>).
        <gs_sled>-check = abap_false.
      ENDLOOP.
    ENDIF.
  ELSE.
    READ TABLE lt_sled_logs ASSIGNING FIELD-SYMBOL(<fs_row>) INDEX i_row.
    IF sy-subrc = 0 .
      <fs_row>-check = SWITCH #( <fs_row>-check WHEN abap_false THEN abap_true
                                                WHEN abap_false THEN abap_true ).
    ENDIF.
  ENDIF.
  lo_gr_alv->refresh( ).
  lo_columns->set_optimize( 'X' ).


ENDFORM.
FORM handle_user_command USING i_ucomm TYPE salv_de_function .
  CASE i_ucomm.
    WHEN '&SLED'.
*      lt_rows = lo_selections->get_selected_rows( ).
      PERFORM SLED_Process USING gt_upload lt_rows CHANGING gt_sled_logs.
      PERFORM Refresh_display.
    WHEN '&SEL_ALL'.
      flag = 'X'.
      PERFORM Checkbox_selection USING ' ' flag CHANGING gt_sled_logs.
    WHEN '&DSEL_ALL'.
      CLEAR flag.
      PERFORM Checkbox_selection USING ' ' flag CHANGING gt_sled_logs.
  ENDCASE.
ENDFORM.
FORM handle_on_link_click USING row TYPE salv_de_row  column TYPE salv_de_column.
  IF row IS NOT INITIAL.
    PERFORM Checkbox_selection USING row flag CHANGING gt_sled_logs.
  ENDIF.

ENDFORM.
