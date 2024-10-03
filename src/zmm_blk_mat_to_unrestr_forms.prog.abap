*&---------------------------------------------------------------------*
*& Include zmm_blk_mat_to_unrestr_forms
*&---------------------------------------------------------------------*
*--------------Upload_Request-------------------*
FORM upload_request USING  p_fname.
  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name = syst-cprog
    IMPORTING
      file_name    = p_fname.
ENDFORM.
*--------------Validations-------------------*
FORM validations USING lt_upload LIKE lt_upload
                       lt_logs LIKE gt_trpost.
  DATA: lv_gjahr  TYPE gjahr,
        lv_period TYPE monat,
        lv_frper  TYPE frper.
  DATA: lo_common_check TYPE REF TO zcl_common_check.       "obj dec for global class common check
  CREATE OBJECT lo_common_check.
  SELECT SINGLE FROM t001k FIELDS bukrs WHERE bwkey = @( lt_upload[ 1 ]-plant  ) INTO @DATA(ls_bukrs).
***************************FI period check*******************
  lo_common_check->fi_period_check(
    EXPORTING
      posting_date =  sy-datum          " Field of type DATS
      acct_type    =  'D'               " Account type
      com_code     =  ls_bukrs   " Company Code
    IMPORTING
      type         =  DATA(lv_msgtype)
    EXCEPTIONS
      mandatory    = 1
      OTHERS       = 2
).
  IF lv_msgtype = 'E'.
    MESSAGE |FI Posting Period Not Opened| TYPE 'E' DISPLAY LIKE 'I'.
    EXIT.
  ENDIF.
***************************MM period check*******************
  CLEAR : lv_msgtype.
  lo_common_check->mm_period_check(
    EXPORTING
      com_code  = ls_bukrs              " Company Code
      date      = sy-datum                      " Posting Date in the Document
    IMPORTING
      type      = lv_msgtype
    EXCEPTIONS
      mandatory = 1                " Fill Com_code & Date
      OTHERS    = 2 ).
  IF lv_msgtype = 'E'.
    MESSAGE |MM Posting Period Not Opened| TYPE 'E' DISPLAY LIKE 'I'.
    EXIT.
  ENDIF.
  LOOP AT lt_upload ASSIGNING FIELD-SYMBOL(<ls_upload>).
*    DATA(lv_ind) = sy-tabix + 1.
*IF sy-subrc = 0.

*-----Check Material-----*
    IF <ls_upload>-matnr IS NOT INITIAL.
      matnr = |{  <ls_upload>-matnr ALPHA = IN }|.
      SELECT SINGLE FROM mara FIELDS matnr, meins,lvorm  WHERE matnr  = @matnr INTO ( @DATA(lv_matnr), @DATA(lv_meins), @DATA(lv_lvorm) ).
      IF Sy-subrc NE 0.
        APPEND VALUE #(  msgtype = 1  matnr = matnr plant = <ls_upload>-plant lgort = <ls_upload>-stor_loc
                         charg  = <ls_upload>-batch qty = <ls_upload>-qty msg = |Material Incorrect| ) TO gt_trpost.
        CONTINUE.
      ELSEIF lv_lvorm = abap_true. "*-----Check Client Deletion for material-----*
        APPEND VALUE #( msgtype = '@0A@'
               matnr = <ls_upload>-matnr plant = <ls_upload>-plant lgort = <Ls_upload>-stor_loc
               charg = <ls_upload>-batch  qty = <ls_upload>-qty
               msg = |Material is Blocked at client level| ) TO gt_trpost.

        CONTINUE.
      ELSE.

      ENDIF.
      <ls_upload>-matnr = matnr.
      <ls_upload>-uom = lv_meins.
    ENDIF.
**-----Check Plant-----*
    IF <ls_upload>-plant IS NOT INITIAL.
      SELECT SINGLE FROM marc FIELDS werks WHERE matnr = @lv_matnr AND werks = @<ls_upload>-plant INTO @DATA(lv_werks).
      IF sy-subrc NE 0.
        APPEND VALUE #(  msgtype = '@0A@'  matnr = matnr plant = <ls_upload>-plant lgort = <ls_upload>-stor_loc
                      charg  = <ls_upload>-batch qty = <ls_upload>-qty msg = |Plant Incorrect| ) TO gt_trpost.
        CONTINUE.
      ENDIF.
    ENDIF.
**-----Check Storage Location-----*
    IF <ls_upload>-stor_loc IS NOT INITIAL.
      SELECT SINGLE FROM mard FIELDS lgort WHERE matnr = @lv_matnr AND werks = @lv_werks AND lgort = @<ls_upload>-stor_loc  INTO @DATA(ls_lgort).
      IF sy-subrc NE 0.
        APPEND VALUE #(  msgtype = '@0A@'  matnr = matnr plant = <ls_upload>-plant lgort = <ls_upload>-stor_loc
                       charg  = <ls_upload>-batch qty = <ls_upload>-qty msg = |Storage Location Incorrect| ) TO gt_trpost.
        CONTINUE.
      ENDIF.
    ENDIF.
*-----Check Batch-----*
    IF <ls_upload>-batch IS NOT INITIAL.
      SELECT SINGLE FROM mchb FIELDS charg,cspem WHERE matnr = @lv_matnr
                                       AND werks = @lv_werks
                                       AND lgort = @ls_lgort
                                       AND charg = @( CONV #( <ls_upload>-batch ) )  INTO @DATA(lv_batch).
      IF sy-subrc NE 0.
        APPEND VALUE #(  msgtype = '@0A@'  matnr = matnr plant = <ls_upload>-plant lgort = <ls_upload>-stor_loc
                       charg  = <ls_upload>-batch qty = <ls_upload>-qty msg = |Batch Incorrect| ) TO gt_trpost.
        CONTINUE.
      ENDIF.
    ENDIF.
*-----Quantity-----*
    IF <ls_upload>-qty IS NOT INITIAL.
      IF <ls_upload>-qty GT lv_batch-cspem .
        APPEND VALUE #(  msgtype = '@0A@'  matnr = matnr plant = <ls_upload>-plant lgort = <ls_upload>-stor_loc
                        charg  = <ls_upload>-batch qty = CONV #( <ls_upload>-qty ) msg = |Given Quantity { <ls_upload>-qty } is greater than blocked stock quantity { lv_batch-cspem } | ) TO gt_trpost.
        CONTINUE.
      ELSE.
        APPEND VALUE #(  msgtype = '@08@'  matnr = matnr plant = <ls_upload>-plant lgort = <ls_upload>-stor_loc
                          charg  = <ls_upload>-batch qty = CONV #( <ls_upload>-qty ) msg = '' ) TO gt_trpost.
        CONTINUE.
      ENDIF.
    ELSE.
      APPEND VALUE #(  msgtype = '@0A@'  matnr = matnr plant = <ls_upload>-plant lgort = <ls_upload>-stor_loc
                      charg  = <ls_upload>-batch qty = CONV #( <ls_upload>-qty ) msg = | Enter Quantity!!! | ) TO gt_trpost.
      CONTINUE.
    ENDIF.
*-----Material Slip-----*
*    IF <ls_upload>-mat_slip IS INITIAL.
*      APPEND VALUE #( msgid  = 'CX100'
*                         msgty  = 'E'
*                         msgno  = '002'
*                         msgv1  = |Please enter Material Slip, Line { lv_ind }|
*                         lineno = lv_ind ) TO lt_msg_tab.
*    ENDIF.
**-----Document Header Text-----*
*    IF <ls_upload>-doc_header_text IS INITIAL.
*      APPEND VALUE #( msgid  = 'CX100'
*                         msgty  = 'E'
*                         msgno  = '002'
*                         msgv1  = |Please enter Document Header Text, Line { lv_ind }|
*                         lineno = lv_ind ) TO lt_msg_tab.
*    ENDIF.

  ENDLOOP.
ENDFORM.

FORM Transfer_post_343 USING lt_upload LIKE lt_upload
                             it_rows TYPE salv_t_row
                  CHANGING lt_logs LIKE gt_logs
                           lt_trpost LIKE gt_trpost.
*  LOOP AT gt_upload INTO DATA(ps_upl).
  REFRESH:lt_logs,gt_return.
  REFRESH :gt_gsmvt_itm,gt_return.

  READ TABLE lt_trpost TRANSPORTING NO FIELDS WITH KEY check = abap_true.
  IF sy-subrc NE 0.
    MESSAGE 'Please select a checkbox' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.
  LOOP AT lt_trpost INTO DATA(is_post) WHERE check IS NOT INITIAL.
    DATA(lv_ind) = sy-tabix.
    READ TABLE lt_trpost ASSIGNING FIELD-SYMBOL(<fs_trpost>) INDEX lv_ind.
    IF sy-subrc = 0 AND <fs_trpost>-msgtype = '@0A@'.
      MESSAGE |{ <fs_trpost>-msg }| TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
  ENDLOOP.
  CLEAR lv_ind.
  LOOP AT lt_trpost INTO DATA(is_row) WHERE check IS NOT INITIAL.
    lv_ind = sy-tabix.
    READ TABLE lt_upload ASSIGNING FIELD-SYMBOL(<ps_upl>) INDEX lv_ind.
    IF sy-subrc = 0.
      ASSIGN lt_trpost[ lv_ind ]-msg TO FIELD-SYMBOL(<fs_post>).
      IF <fs_post> IS ASSIGNED AND <fs_post> CS 'Transfer Posting Done'.
        MESSAGE 'Already transfer Posting done for the records' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.
      APPEND VALUE #( material = <ps_upl>-matnr plant = <ps_upl>-plant
                      stge_loc = <ps_upl>-stor_loc batch = <ps_upl>-batch
                      move_type = '343'
                      entry_qnt = <ps_upl>-qty
                      entry_uom = <ps_upl>-uom
                       ) TO gt_gsmvt_itm.
    ENDIF.
  ENDLOOP.

  PERFORM Get_Num_range USING '01' 'ZMM_MSR' CHANGING lv_refno.

  DATA(gmvt_header) = VALUE  bapi2017_gm_head_01(
           pstng_date          = p_budat
           doc_date            = sy-datum
           ref_doc_no          = lv_refno
           header_txt          = lv_refno
       ).
  """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  REFRESH gt_return.
  CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
    EXPORTING
      goodsmvt_header  = gmvt_header
      goodsmvt_code    = CONV bapi2017_gm_code( '04' ) "Movement code for Transfer Posting
*     testrun          = SWITCH #( p_chk WHEN abap_true THEN 'X' ELSE space )
    IMPORTING
      goodsmvt_headret = gmsvt_ret
    TABLES
      goodsmvt_item    = gt_gsmvt_itm
      return           = gt_return.
  IF line_exists( gt_return[ type = 'E' ] ) OR line_exists( gt_return[ type = 'A' ] ).
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    LOOP AT gt_return INTO DATA(ws_return) WHERE type <> 'S'.
      APPEND VALUE #( msgtype = SWITCH #( ws_return-type WHEN 'E' OR 'A' THEN '@0A@' )
                      mblnr = ' '
                      mjahr = ''
                      msg = ws_return-message ) TO lt_logs.
    ENDLOOP.
    REFRESH gt_return.
  ELSE.
*    IF p_chk IS INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
*    ENDIF.
    APPEND VALUE #( msgtype = '@08@'
                      mblnr = gmsvt_ret-mat_doc
                      mjahr = gmsvt_ret-doc_year
                      msg = 'Blocked Stock has been moved to Unrestricted Stock' ) TO lt_logs.
*    IF p_chk IS INITIAL.
    "Header TABLE UPDATE
    MODIFY zmm_tr_post_head FROM @( VALUE #( mandt   = sy-mandt
                                            mblnr     = gmsvt_ret-mat_doc
                                            mjahr     = gmsvt_ret-doc_year
                                            mat_slip  = lv_refno
                                            bldat     = sy-datum
                                            budat     = p_budat
                                            created_by = sy-uname
                                            created_at = sy-uzeit ) ).
    COMMIT WORK.
    "Item Table update
    LOOP AT lt_trpost INTO is_row WHERE check IS NOT INITIAL.
      CLEAR lv_ind.
      lv_ind = sy-tabix.
      READ TABLE lt_trpost ASSIGNING FIELD-SYMBOL(<gs_upl>) INDEX lv_ind.
      IF sy-subrc = 0.
        ADD 1 TO mblpo.
        APPEND VALUE #( mandt = sy-mandt
                        mblnr  = gmsvt_ret-mat_doc
                        mjahr = gmsvt_ret-doc_year
                        mblpo = mblpo
                        matnr = <gs_upl>-matnr
                        plant = <gs_upl>-plant
                        stor_loc = <gs_upl>-lgort
                        batch = <gs_upl>-charg
                        qty = <gs_upl>-qty
*                      vfdat = |{ ws_upl-sled+6(4) }{ ws_upl-sled+3(2) }{ ws_upl-sled+0(2) }|
                       uom = lt_upload[ lv_ind ]-uom
                         ) TO gt_log_itm.

        <gs_upl>-msg = 'Transfer Posting Done'.
      ENDIF.
    ENDLOOP.
    MODIFY zmm_sled_343_log FROM TABLE gt_log_itm.
    COMMIT WORK.
    CLEAR: gmvt_header,gmsvt_ret.
*    ENDIF.
  ENDIF.
ENDFORM.
*-----Build ALV-----*
FORM build_alv USING gt_logs.
  DATA: lo_event_handler TYPE REF TO lcl_handle_events , " Variables for events
        lo_events        TYPE REF TO cl_salv_events_table.
* Create the ALV object
  TRY.
      CALL METHOD cl_salv_table=>factory
        IMPORTING
          r_salv_table = lo_gr_alv
        CHANGING
          t_table      = gt_logs.
    CATCH cx_salv_msg.
  ENDTRY.
* Let's show all default buttons of ALV
  lo_gr_functions = lo_gr_alv->get_functions( ).
  lo_gr_functions->set_all( abap_true ).

* Fit the columns
  lo_columns = lo_gr_alv->get_columns( ).
  lo_columns->set_optimize( 'X' ).

*  lo_columns->set_column_position( columnname = 'MSGTYP' position =  1 ).
*  lo_columns->set_column_position( columnname = 'MBLNR'  position =  2 ).
*  lo_columns->set_column_position( columnname = 'MJAHR'  position =  3 ).
*  lo_columns->set_column_position( columnname = 'MSG' position =  4 ).

* Apply zebra style to lv_rows
  lo_display = lo_gr_alv->get_display_settings( ).
  lo_display->set_striped_pattern( cl_salv_display_settings=>true ).

  CALL METHOD lo_gr_alv->get_functions
    RECEIVING
      value = lo_gr_functions.

  CALL METHOD lo_gr_functions->set_default
    EXPORTING
      value = if_salv_c_bool_sap=>true.

  lo_gr_alv->set_screen_status(
    pfstatus      =  'TRP_STATUS'
    report        =  sy-repid
    set_functions = lo_gr_alv->c_functions_all ).

* Register events
  lo_events = lo_gr_alv->get_event( ).
  CREATE OBJECT lo_event_handler.
  SET HANDLER lo_event_handler->on_user_command FOR lo_events.
  SET HANDLER lo_event_handler->on_link_click FOR lo_events.


*  lo_selections = lo_gr_alv->get_selections( ).
*  lo_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).
*  lt_rows = lo_selections->get_selected_rows( ).
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

      lo_column ?= lo_columns->get_column( 'QTY' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Quantity' ).
      lo_column->set_medium_text( 'Quantity' ).
      lo_column->set_short_text( 'Quantity' ).

      lo_column ?= lo_columns->get_column( 'MSG' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Message' ).
      lo_column->set_medium_text( '' ).
      lo_column->set_short_text( '' ).

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
  ENDTRY.

  lo_gr_alv->display( ).
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

*    lo_config->add_column(
*          EXPORTING
*            header_text = 'Material Slip'
*            field_name = 'MAT_SLIP'
*            display_type = if_salv_bs_model_column=>uie_text_view ).
*
*    lo_config->add_column(
*          EXPORTING
*            header_text = 'Posting date'
*            field_name = 'POST_DATE'
*            display_type = if_salv_bs_model_column=>uie_text_view ).

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

  lv_filename1 = | { 'Material Transfer Post data' } |.
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
    DATA(lv_ind) = sy-tabix + 1.
    iv_sled = |{ <ls_upload>-sled+6(4) }{ <ls_upload>-sled+3(2) }{ <ls_upload>-sled+0(2) }|.
    IF <ls_upload>-sled IS NOT INITIAL.
      SELECT SINGLE FROM mch1 FIELDS vfdat WHERE matnr = @<ls_upload>-matnr AND charg = @<ls_upload>-batch
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
        APPEND VALUE #( msgtype = 1
                 matnr = <ls_upload>-matnr plant = <ls_upload>-plant lgort = <Ls_upload>-stor_loc
                 charg = <ls_upload>-batch  curr_sled = iv_sled  act_sled = iv_vfdat
                 msg = |Material is incorrect| ) TO lt_logs.
*        APPEND VALUE #( msgtype = 1 mblnr = ' ' mjahr = '' msg = |Material{ <ls_upload>-matnr } not present in material masters, Line { lv_ind }| ) TO gt_logs.
        <ls_upload>-err_flag = 'X'.
      ELSEIF lv_lvorm = abap_true.
        APPEND VALUE #( msgtype = 1
               matnr = <ls_upload>-matnr plant = <ls_upload>-plant lgort = <Ls_upload>-stor_loc
               charg = <ls_upload>-batch  curr_sled = iv_sled  act_sled = iv_vfdat
               msg = |Material is Blocked at client level| ) TO lt_logs.
        <ls_upload>-err_flag = 'X'.
      ENDIF.
      <ls_upload>-matnr = matnr.
*      <ls_upload>-uom = lv_meins.
    ENDIF.
*-----Check Plant-----*
    IF <ls_upload>-plant IS NOT INITIAL.
      SELECT SINGLE FROM marc FIELDS werks WHERE matnr = @lv_matnr AND werks = @<ls_upload>-plant INTO @DATA(lv_werks).
      IF sy-subrc NE 0.
        APPEND VALUE #( msgtype = 1
                       matnr = <ls_upload>-matnr plant = <ls_upload>-plant lgort = <Ls_upload>-stor_loc
                       charg = <ls_upload>-batch  curr_sled = iv_sled  act_sled = iv_vfdat
                       msg = |Plant is incorrect| ) TO lt_logs.
*        APPEND VALUE #( msgtype = 1 mblnr = ' ' mjahr = ''
*            msg = |Plant is incorrect/unavailable for Material { lv_matnr } given, Line { lv_ind }| ) TO gt_logs.
        <ls_upload>-err_flag = 'X'.
      ENDIF.
    ENDIF.
*-----Check Storage Location-----*
    IF <ls_upload>-stor_loc IS NOT INITIAL.
      SELECT SINGLE FROM mard FIELDS lgort WHERE matnr = @lv_matnr AND werks = @lv_werks AND lgort = @<ls_upload>-stor_loc  INTO @DATA(ls_lgort).
      IF sy-subrc NE 0.
        APPEND VALUE #( msgtype = 1
                             matnr = <ls_upload>-matnr plant = <ls_upload>-plant lgort = <Ls_upload>-stor_loc
                             charg = <ls_upload>-batch  curr_sled = iv_sled  act_sled = iv_vfdat
                             msg = |Storage Loc. is incorrect| ) TO lt_logs.
*        APPEND VALUE #( msgtype = 1 mblnr = ' ' mjahr = ''
*            msg = |Storage Loc. is incorrect/unavailable for Material { lv_matnr } in Plant { lv_werks } given, Line { lv_ind }| ) TO gt_logs.
        <ls_upload>-err_flag = 'X'.
      ENDIF.
    ENDIF.
*-----Check Batch-----*
    IF <ls_upload>-batch IS NOT INITIAL.
      SELECT SINGLE FROM mchb FIELDS charg,cspem WHERE matnr = @lv_matnr
                                       AND werks = @lv_werks
                                       AND lgort = @ls_lgort
                                       AND charg = @<ls_upload>-batch INTO @DATA(lv_batch).
      IF sy-subrc NE 0.
        APPEND VALUE #( msgtype = 1
                              matnr = <ls_upload>-matnr plant = <ls_upload>-plant lgort = <Ls_upload>-stor_loc
                              charg = <ls_upload>-batch  curr_sled = iv_sled  act_sled = iv_vfdat
                              msg = |Batch is incorrect| ) TO lt_logs.
        <ls_upload>-err_flag = 'X'.
*        APPEND VALUE #( msgtype = 1 mblnr = ' ' mjahr = ''
*               msg = |Batch is incorrect/unavailable for Material { lv_matnr } in Plant { lv_werks } of Storage Loc. { ls_lgort } given| ) TO gt_logs.
      ENDIF.
    ENDIF.
*------SLED------*
    IF <ls_upload>-sled IS NOT INITIAL.
*      SELECT SINGLE FROM mch1 FIELDS vfdat WHERE matnr = @<ls_upload>-matnr AND charg = @<ls_upload>-batch
*                                                          INTO @DATA(iv_vfdat).
      IF sy-subrc = 0.
        IF iv_vfdat IS NOT INITIAL.
*          iv_sled = |{ <ls_upload>-sled+6(4) }{ <ls_upload>-sled+3(2) }{ <ls_upload>-sled+0(2) }|.
          IF iv_sled LT iv_vfdat.
*            DATA(act_sled) = |{ iv_vfdat+6(2) }.{ iv_vfdat+4(2) }.{ iv_vfdat+0(4) }|.
            APPEND VALUE #( msgtype = 1
                            matnr = <ls_upload>-matnr plant = <ls_upload>-plant lgort = <Ls_upload>-stor_loc
                            charg = <ls_upload>-batch  curr_sled = iv_sled  act_sled = iv_vfdat
                            msg = |Current SLED is lesser than Actual SLED| )
                               TO lt_logs.
            <ls_upload>-err_flag = 'X'.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDIF.


  ENDLOOP.

ENDFORM.
FORM Checkbox_selection  USING i_row TYPE salv_de_row flag TYPE xfeld CHANGING lt_trpost LIKE gt_trpost.

  IF i_row IS INITIAL.
    IF flag = abap_true.
      LOOP AT lt_trpost ASSIGNING FIELD-SYMBOL(<fs_post>).
        <fs_post>-check = abap_true.
      ENDLOOP.
    ELSE.
      LOOP AT lt_trpost ASSIGNING FIELD-SYMBOL(<gs_post>).
        <gs_post>-check = abap_false.
      ENDLOOP.
    ENDIF.
  ELSE.
    READ TABLE lt_trpost ASSIGNING FIELD-SYMBOL(<fs_row>) INDEX i_row.
    IF sy-subrc = 0 .
      <fs_row>-check = SWITCH #( <fs_row>-check WHEN abap_false THEN abap_true
                                                WHEN abap_false THEN abap_true ).
    ENDIF.
  ENDIF.
  lo_gr_alv->refresh( ).
  lo_columns->set_optimize( 'X' ).


ENDFORM.
*&---------------------------------------------------------------------*
*& Form handle_user_command
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_SALV_FUNCTION
*&---------------------------------------------------------------------*
FORM handle_user_command USING i_ucomm TYPE  salv_de_function.
  CASE i_ucomm.
    WHEN '&TR_POST'.
      PERFORM Transfer_post_343 USING lt_upload lt_rows CHANGING gt_logs gt_trpost.
      lo_gr_alv->refresh(  ).
      lo_gr_alv->display(  ).
      IF gt_logs IS NOT INITIAL.
        PERFORM build_alv_popup USING gt_logs.
      ENDIF.
    WHEN '&SEL_ALL'.
      flag = 'X'.
      PERFORM Checkbox_selection USING ' ' flag CHANGING gt_trpost.
    WHEN '&DSEL_ALL'.
      CLEAR flag.
      PERFORM Checkbox_selection USING ' ' flag CHANGING gt_trpost.
  ENDCASE.
ENDFORM.
FORM handle_on_link_click USING row TYPE salv_de_row  column TYPE salv_de_column.
  IF row IS NOT INITIAL.
    PERFORM Checkbox_selection USING row flag CHANGING gt_trpost.
  ENDIF.

ENDFORM.
FORM build_alv_popup USING it_logs LIKE gt_logs.
  CLEAR:lo_columns.
  TRY.
      CALL METHOD cl_salv_table=>factory
        IMPORTING
          r_salv_table = DATA(gr_alv)
        CHANGING
          t_table      = gt_logs.
    CATCH cx_salv_msg.
  ENDTRY.

  gr_alv->set_screen_popup(
    EXPORTING
      start_column = 25
      end_column   = 90
      start_line   = 6
      end_line     = 10
  ).

* Fit the columns
  lo_columns = gr_alv->get_columns( ).
  lo_columns->set_optimize( 'X' ).

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
  ENDTRY.
  gr_alv->display( ).
ENDFORM.
