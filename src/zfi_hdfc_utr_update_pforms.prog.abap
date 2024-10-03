*&---------------------------------------------------------------------*
*& Include          ZFI_HDFC_UTR_UPDATE_PFORMS
*&---------------------------------------------------------------------*
CLASS lcl_handle_events DEFINITION.
  PUBLIC SECTION.
    METHODS: on_double_click FOR EVENT double_click OF cl_salv_events_table
      IMPORTING row column. "Event for Double click on alv display

ENDCLASS.
CLASS lcl_handle_events IMPLEMENTATION.
  METHOD on_double_click.
    IF column EQ 'DOC_NO'.
      READ TABLE gt_alv INTO DATA(wa_alv) INDEX  row.
      SELECT SINGLE * FROM bkpf INTO @DATA(ls_bkpf) WHERE bukrs EQ @wa_alv-bukrs
                                                    AND belnr EQ @wa_alv-doc_no
                                                    AND gjahr EQ @wa_alv-gjahr.
      IF sy-subrc = 0. " Exists?
        SET PARAMETER ID 'BLN' FIELD wa_alv-doc_no. "Document Number

        SET PARAMETER ID 'BUK' FIELD wa_alv-bukrs. "Company Code

        SET PARAMETER ID 'GJR' FIELD wa_alv-gjahr. "Fiscal Year

        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
FORM f4_help USING p_fname TYPE rlgrap-filename.
  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    EXPORTING
      program_name  = syst-repid
      dynpro_number = syst-dynnr
      field_name    = ' '
      static        = ' '
      mask          = ' '
    CHANGING
      file_name     = p_fname.
ENDFORM.
FORM f_excel_download .
  PERFORM f_excel_instantiate.
  lv_filename1 = | { 'HDFC Utr Updation' } |.
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
FORM f_excel_instantiate .

  GET REFERENCE OF gt_excel INTO lr_excel_structure.
  DATA(lo_itab_services) = cl_salv_itab_services=>create_for_table_ref( lr_excel_structure ).
  lo_source_table_descr ?= cl_abap_tabledescr=>describe_by_data_ref( lr_excel_structure ).
  lo_table_row_descripter ?= lo_source_table_descr->get_table_line_type( ).

  DATA(lo_tool_xls) = cl_salv_export_tool_ats_xls=>create_for_excel(
                               EXPORTING r_data = lr_excel_structure ).

  DATA(lo_config) = lo_tool_xls->configuration( ).
  lo_config->add_column(
         EXPORTING
           header_text = 'Company Code'
           field_name = 'COMP_CODE'
           display_type = if_salv_bs_model_column=>uie_text_view ).

  lo_config->add_column(
        EXPORTING
          header_text = 'Document Number'
          field_name = 'DOCNO'
          display_type = if_salv_bs_model_column=>uie_text_view ).

  lo_config->add_column(
        EXPORTING
          header_text = 'Fiscal Year'
          field_name = 'FISYEAR'
          display_type = if_salv_bs_model_column=>uie_text_view ).

  lo_config->add_column(
      EXPORTING
        header_text = 'Utr Number'
        field_name = 'UTRNO'
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

ENDFORM.
FORM excel_conversion USING p_fname TYPE rlgrap-filename.
  IF p_fname IS NOT INITIAL.
*** Function Module to Convert excel data to SAP ***
    REFRESH: gt_type,gt_excel.
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
      MESSAGE 'Conversion of excel failed' TYPE 'E'.
    ENDIF.
  ELSE.
    MESSAGE 'Filename is Mandatory' TYPE 'E'.
  ENDIF.
ENDFORM.
FORM actual_process.
  SELECT bukrs,
          belnr,
          gjahr FROM bkpf INTO TABLE @DATA(lt_bkpf)
                FOR ALL ENTRIES IN @gt_excel
                WHERE belnr = @gt_excel-docno
                AND gjahr = @gt_excel-fisyear
                AND blart = 'KZ'.
  IF sy-subrc = 0.
    SORT lt_bkpf[] BY belnr.
  ENDIF.

  REFRESH: gt_alv.
  LOOP AT gt_excel ASSIGNING FIELD-SYMBOL(<fs_excel>).
*------ Company Code existence check ---------*
    SELECT SINGLE bukrs FROM t001 INTO @DATA(l_bukrs) WHERE bukrs = @<fs_excel>-comp_code.
    IF sy-subrc NE 0.
      APPEND VALUE #( bukrs = <fs_excel>-comp_code
                      doc_no = <fs_excel>-docno
                      gjahr = <fs_excel>-fisyear
                      utrno = <fs_excel>-utrno
                      type = 'E'
                      msg = |Incorrect Company Code| ) TO gt_alv.
      CONTINUE.
    ENDIF.
*----- Document Number existence Check --------*
    lv_belnr = VALUE #( lt_bkpf[ bukrs = <fs_excel>-comp_code
                                 belnr = <fs_excel>-docno
                                 gjahr = <fs_excel>-fisyear ]-belnr OPTIONAL ).
    IF lv_belnr IS NOT INITIAL.
      REFRESH: gt_bseg.
      DATA(lv_awref) = CONV awref( lv_belnr ).
      DATA(lv_aworg) = CONV aworg( |{ <fs_excel>-comp_code }{ <fs_excel>-fisyear }| ).
      CALL FUNCTION 'FI_DOCUMENT_READ'
        EXPORTING
          i_bukrs     = l_bukrs
          i_belnr     = lv_belnr
          i_gjahr     = <fs_excel>-fisyear
        TABLES
          t_bseg      = gt_bseg
        EXCEPTIONS
          wrong_input = 1
          not_found   = 2
          OTHERS      = 3.
      IF sy-subrc = 0.
        LOOP AT gt_bseg INTO DATA(lw_bseg).
          REFRESH: gt_accchg.
          DATA(lw_accchg) = VALUE accchg( fdname = gv_utr_field
                                          oldval = lw_bseg-kidno
                                          newval = <fs_excel>-utrno ).
          APPEND lw_accchg TO gt_accchg.
          "Function Module to update the Payment reference
          CALL FUNCTION 'FI_DOCUMENT_CHANGE'
            EXPORTING
              i_awtyp              = 'BKPF'
              i_awref              = lv_awref
              i_aworg              = lv_aworg
              i_buzei              = lw_bseg-buzei
            TABLES
              t_accchg             = gt_accchg
            EXCEPTIONS
              no_reference         = 1
              no_document          = 2
              many_documents       = 3
              wrong_input          = 4
              overwrite_creditcard = 5
              OTHERS               = 6.
          IF sy-subrc NE 0.
            CASE sy-subrc.
              WHEN '1'.
                DATA(lv_msg) = 'no_reference'.
                EXIT.
              WHEN '2'.
                lv_msg = 'no_document'.
                EXIT.
              WHEN '3'.
                lv_msg = 'many_documents'.
                EXIT.
              WHEN '4'.
                lv_msg = 'wrong_input'.
                EXIT.
              WHEN '5'.
                lv_msg = 'overwrite_creditcard'.
                EXIT.
              WHEN '6'.
                lv_msg = 'Others'.
                EXIT.
            ENDCASE.
          ENDIF.
        ENDLOOP.
        IF lv_msg IS INITIAL.
          APPEND VALUE #( bukrs = <fs_excel>-comp_code
                          doc_no = <fs_excel>-docno
                          gjahr = <fs_excel>-fisyear
                          utrno = <fs_excel>-utrno
                          type = 'S'
                          msg = |Payment Reference updated| ) TO gt_alv.
        ELSE.
          APPEND VALUE #( bukrs = <fs_excel>-comp_code
                          doc_no = <fs_excel>-docno
                          gjahr = <fs_excel>-fisyear
                          utrno = <fs_excel>-utrno
                          type = 'E'
                          msg = lv_msg ) TO gt_alv.
        ENDIF.
      ENDIF.
    ELSE.
      APPEND VALUE #( bukrs = <fs_excel>-comp_code
                      doc_no = <fs_excel>-docno
                      gjahr = <fs_excel>-fisyear
                      utrno = <fs_excel>-utrno
                      type = 'E'
                      msg = |Incorrect Clearing Document Number| ) TO gt_alv.
    ENDIF.
    CLEAR: lv_msg,lv_awref,lv_aworg.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form alv_display
FORM alv_display .
  DATA: lo_event_handler TYPE REF TO lcl_handle_events, " Variables for events
        lo_events        TYPE REF TO cl_salv_events_table.
  IF gt_alv IS NOT INITIAL.
* create the alv object
    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = lo_gr_alv
          CHANGING
            t_table      = gt_alv.
      CATCH cx_salv_msg.
    ENDTRY.
* register events
    lo_events = lo_gr_alv->get_event( ).
    CREATE OBJECT lo_event_handler.
    SET HANDLER lo_event_handler->on_double_click FOR lo_events.
    lo_gr_alv->display( ).
  ENDIF.
ENDFORM.
