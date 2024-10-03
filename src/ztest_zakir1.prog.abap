*&---------------------------------------------------------------------*
*& Report ztest_zakir1
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztest_zakir1.

*PARAMETERS : p_file TYPE string.
*DATA : len TYPE i.
*DATA : gt_content TYPE SOLI_TAB.
*DATA : xstr_content TYPE xstring.
*DATA : lt_file_table TYPE filetable.
*DATA :  lv_rc TYPE i.
*DATA :   lv_filename TYPE RLGRAP-FILENAME.
*DATA :  lv_filelength TYPE i.
*DATA : lt_rawtab TYPE TABLE OF char255.
*
*DATA: d_file   TYPE rlgrap-filename,
*      d_objkey TYPE borident-objkey.
*
*DATA : ls_object TYPE borident.
*DATA : ls_fol_id TYPE soodk.
*DATA : ls_obj_data TYPE sood1.
*DATA : lv_offset TYPE char20.
*DATA : ls_obj_id TYPE soodk.
*DATA : it_objhead TYPE TABLE OF soli.
*DATA : ls_note TYPE borident.
*
*DATA: twebpdf         TYPE string,
*fic_binario     TYPE xstring,
*gs_fol_id       TYPE soodk,
*gv_ext          TYPE sood1-file_ext,
*gv_fname        TYPE sood1-objdes,
*gs_obj_id       TYPE soodk,
*lv_solix_length TYPE i,
*lt_solixtab     TYPE STANDARD TABLE OF solix,
*wa_soli         TYPE soli,
*wa_solix        TYPE solix,
*lt_mappe        TYPE STANDARD TABLE OF soli,
*lt_objhead      TYPE STANDARD TABLE OF soli,
*ls_folmem_k     TYPE sofmk,
*lv_ep_note      TYPE borident-objkey.
*DATA: iv_len type i.
*FIELD-SYMBOLS: <ptr_text> TYPE soli,
*         <ptr_x>    TYPE any,
*         <ptr_hex>  TYPE solix,
*         <table>.
*
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
*  PERFORM f4_help_for_file.
*
*START-OF-SELECTION.
*  CALL FUNCTION 'GUI_UPLOAD'
*    EXPORTING
*      filename                = p_file
*      filetype                = 'BIN'
*    IMPORTING
*      filelength              = len
*    TABLES
*      data_tab                = gt_content
*    EXCEPTIONS
*      file_open_error         = 1
*      file_read_error         = 2
*      no_batch                = 3
*      gui_refuse_filetransfer = 4
*      invalid_type            = 5
*      no_authority            = 6
*      unknown_error           = 7
*      bad_data_format         = 8
*      header_not_allowed      = 9
*      separator_not_allowed   = 10
*      header_too_long         = 11
*      unknown_dp_error        = 12
*      access_denied           = 13
*      dp_out_of_memory        = 14
*      disk_full               = 15
*      dp_timeout              = 16
*      OTHERS                  = 17.
*  IF sy-subrc <> 0.
*
*  ENDIF.
*
**  CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
**    EXPORTING
**      input_length = len
**    IMPORTING
**      buffer       = xstr_content
**    TABLES
**      binary_tab   = gt_content
**    EXCEPTIONS
**      failed       = 1
**      OTHERS       = 2.
**  IF sy-subrc <> 0.
**
**  ENDIF.
*
*
*
**IF wa_thdr-fi_mm_flg EQ 'FI'.
*  ls_object-objtype = 'BKPF'.
**  CONCATENATE wa_thdr-comp_code  wa_thdr-sap_doc_no INTO ls_object-objkey SEPARATED BY space.
*  ls_object-objkey = '100015000000062022'.
*
**ELSEIF wa_thdr-fi_mm_flg EQ 'MM'.
**  ls_object-objtype = 'BUS2081'.
**  ls_object-objkey  = wa_thdr-sap_doc_no.
**ENDIF.
**
**  CALL FUNCTION 'WS_UPLOAD'
**    EXPORTING
**      filename            = lv_filename
**      filetype            = 'BIN'
**    TABLES
**      data_tab            = gt_content
**    EXCEPTIONS
**      conversion_error    = 1
**      file_open_error     = 2
**      file_read_error     = 3
**      invalid_table_width = 4
**      invalid_type        = 5
**      no_batch            = 6
**      unknown_error       = 7
**      OTHERS              = 8.
*
*  CALL FUNCTION 'SO_CONVERT_CONTENTS_BIN'
*    EXPORTING
*      it_contents_bin = gt_content[]
*    IMPORTING
*      et_contents_bin = gt_content[].
*
*  CALL FUNCTION 'SO_FOLDER_ROOT_ID_GET'
*    EXPORTING
*      region    = 'B' "'Q'
*    IMPORTING
*      folder_id = ls_fol_id
*    EXCEPTIONS
*      OTHERS    = 1.
*
**CONCATENATE i_obj_key+10(4) i_obj_key+0(10) i_obj_key+14(4) '.PDF' INTO gv_fname.
**CONDENSE gv_fname NO-GAPS.
*
*  ls_obj_data-objsns = 'O'.
*  ls_obj_data-objla = sy-langu.
**  CONCATENATE 'Anexo - ' space
**              wa_thdr-zeic_data_dig+6(2) '.'
**              wa_thdr-zeic_data_dig+4(2) '.'
**              wa_thdr-zeic_data_dig(4)
**              INTO ls_obj_data-objdes.
*
*  ls_obj_data-objdes = 'data.pdf'.
*  ls_obj_data-file_ext = 'PDF'.
*
**  lv_offset = strlen( lv_filename ) - 3.
**  ls_obj_data-file_ext = lv_filename+lv_offset(3).
*  ls_obj_data-objlen = lines( gt_content ) * 255.
*
*  CALL FUNCTION 'SO_OBJECT_INSERT'
*    EXPORTING
*      folder_id             = ls_fol_id
*      object_type           = 'EXT'
*      object_hd_change      = ls_obj_data
*    IMPORTING
*      object_id             = ls_obj_id
*    TABLES
*      objhead               = it_objhead
*      objcont               = gt_content
*    EXCEPTIONS
*      active_user_not_exist = 35
*      folder_not_exist      = 6
*      object_type_not_exist = 17
*      owner_not_exist       = 22
*      parameter_error       = 23
*      OTHERS                = 1000.
*
**  IF sy-subrc = 0 AND ls_object-objkey IS NOT INITIAL.
**    ls_folmem_k-foltp = ls_fol_id-objtp.
**    ls_folmem_k-folyr = ls_fol_id-objyr.
**    ls_folmem_k-folno = ls_fol_id-objno.
**    ls_folmem_k-doctp = ls_obj_id-objtp.
**    ls_folmem_k-docyr = ls_obj_id-objyr.
**    ls_folmem_k-docno = ls_obj_id-objno.
**    lv_ep_note = ls_folmem_k.
*    ls_note-objtype = 'BKPF'.
*    ls_note-objkey = ls_object-objkey.
*  CALL FUNCTION 'BINARY_RELATION_CREATE_COMMIT'
*    EXPORTING
*      obj_rolea    = ls_object
*      obj_roleb    = ls_note
*      relationtype = 'ATTA'
*    EXCEPTIONS
*      OTHERS       = 1.
*
*  BREAK-POINT.
**    DELETE FROM  zeic_enviar_wf WHERE invoice_guid = wa_wk-invoice_guid.
**  ELSE.
**    wa_wk-erro_img_doc = 'X'.
**    MODIFY it_wk FROM wa_wk INDEX d_tabix.
**  ENDIF.
*
*FORM f4_help_for_file.
*  CALL METHOD cl_gui_frontend_services=>file_open_dialog
*    CHANGING
*      file_table              = lt_file_table
*      rc                      = lv_rc
*    EXCEPTIONS
*      file_open_dialog_failed = 1
*      cntl_error              = 2
*      error_no_gui            = 3
*      not_supported_by_gui    = 4
*      OTHERS                  = 5.
*  IF sy-subrc <> 0.
*
*  ENDIF.
*
*  READ TABLE lt_file_table
*      INTO p_file
*      INDEX 1.
*  IF sy-subrc EQ 0.
*    lv_filename = p_file.
*  ENDIF.
*
*
*
**  cl_gui_frontend_services=>gui_upload(
**    EXPORTING
**      filename = lv_filename
**      filetype = 'BIN' "Binary
**    IMPORTING
**      filelength = lv_filelength
**    CHANGING
**      data_tab = lt_rawtab
**    EXCEPTIONS
**      file_open_error = 1
**      file_read_error = 2
**      no_batch = 3
**      gui_refuse_filetransfer = 4
**      invalid_type = 5
**      no_authority = 6
**      unknown_error = 7
**      bad_data_format = 8
**      header_not_allowed = 9
**      separator_not_allowed = 10
**      header_too_long = 11
**      unknown_dp_error = 12
**      access_denied = 13
**      dp_out_of_memory = 14
**      disk_full = 15
**      dp_timeout = 16
**      not_supported_by_gui = 17
**      error_no_gui = 18
**      OTHERS = 19 ).
*ENDFORM.

*******************************
*   Data Object declaration   *
*******************************

DATA: lo_excel        TYPE REF TO zcl_excel,
      lo_excel_writer TYPE REF TO zif_excel_writer,
      lo_worksheet    TYPE REF TO zcl_excel_worksheet.

DATA: lo_style_title      TYPE REF TO zcl_excel_style,
      lo_drawing          TYPE REF TO zcl_excel_drawing,
      lo_range            TYPE REF TO zcl_excel_range,
      lo_data_validation  TYPE REF TO zcl_excel_data_validation,
      lo_column           TYPE REF TO zcl_excel_column,
      lv_style_title_guid TYPE zexcel_cell_style,
      ls_key              TYPE wwwdatatab.

DATA: lv_file      TYPE xstring,
      lv_bytecount TYPE i,
      lt_file_tab  TYPE solix_tab.

DATA: lv_full_path      TYPE string,
      lv_workdir        TYPE string,
      lv_file_separator TYPE c.

CONSTANTS: lv_default_file_name TYPE string VALUE 'TechEd01.xlsx'.

*******************************
* Selection screen management *
*******************************

PARAMETERS: p_path TYPE zexcel_export_dir.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
  lv_workdir = p_path.
  cl_gui_frontend_services=>directory_browse( EXPORTING initial_folder  = lv_workdir
                                              CHANGING  selected_folder = lv_workdir ).
  p_path = lv_workdir.

INITIALIZATION.
  cl_gui_frontend_services=>get_sapgui_workdir( CHANGING sapworkdir = lv_workdir ).
  cl_gui_cfw=>flush( ).
  p_path = lv_workdir.

START-OF-SELECTION.

  IF p_path IS INITIAL.
    p_path = lv_workdir.
  ENDIF.
  cl_gui_frontend_services=>get_file_separator( CHANGING file_separator = lv_file_separator ).
  CONCATENATE p_path lv_file_separator lv_default_file_name INTO lv_full_path.

*******************************
*    abap2xlsx create XLSX    *
*******************************

  " Create excel instance
  CREATE OBJECT lo_excel.

  " Styles
  lo_style_title                   = lo_excel->add_new_style( ).
  lo_style_title->font->bold       = abap_true.
  lo_style_title->font->color-rgb  = zcl_excel_style_color=>c_blue.
  lv_style_title_guid              = lo_style_title->get_guid( ).

  " Get active sheet
  lo_worksheet        = lo_excel->get_active_worksheet( ).
  lo_worksheet->set_title( ip_title = 'Demo TechEd' ).
  lo_worksheet->set_cell( ip_column = 'B' ip_row = 5 ip_value = 'TechEd demo' ip_style = lv_style_title_guid ).
  lo_worksheet->set_cell( ip_column = 'B' ip_row = 7 ip_value = 'Is abap2xlsx simple' ).
  lo_worksheet->set_cell( ip_column = 'B' ip_row = 8 ip_value = 'Is abap2xlsx CooL' ).

  lo_worksheet->set_cell( ip_column = 'B' ip_row = 10 ip_value = 'Total score' ).
  lo_worksheet->set_cell( ip_column = 'C' ip_row = 10 ip_formula = 'SUM(C7:C8)' ).

  " add logo from SMWO
  lo_drawing = lo_excel->add_new_drawing( ).
  lo_drawing->set_position( ip_from_row = 2
                            ip_from_col = 'B' ).

  ls_key-relid = 'MI'.
  ls_key-objid = 'WBLOGO'.
  lo_drawing->set_media_www( ip_key = ls_key
                             ip_width = 140
                             ip_height = 64 ).

  " assign drawing to the worksheet
  lo_worksheet->add_drawing( lo_drawing ).

  " Add new sheet
  lo_worksheet = lo_excel->add_new_worksheet( ).
  lo_worksheet->set_title( ip_title = 'Values' ).

  " Set values for range
  lo_worksheet->set_cell( ip_row = 4 ip_column = 'A' ip_value = 1 ).
  lo_worksheet->set_cell( ip_row = 5 ip_column = 'A' ip_value = 2 ).
  lo_worksheet->set_cell( ip_row = 6 ip_column = 'A' ip_value = 3 ).
  lo_worksheet->set_cell( ip_row = 7 ip_column = 'A' ip_value = 4 ).
  lo_worksheet->set_cell( ip_row = 8 ip_column = 'A' ip_value = 5 ).

  lo_range            = lo_excel->add_new_range( ).
  lo_range->name      = 'Values'.
  lo_range->set_value( ip_sheet_name    = 'Values'
                       ip_start_column  = 'A'
                       ip_start_row     = 4
                       ip_stop_column   = 'A'
                       ip_stop_row      = 8 ).

  lo_excel->set_active_sheet_index( 1 ).

  " add data validation
  lo_worksheet        = lo_excel->get_active_worksheet( ).

  lo_data_validation              = lo_worksheet->add_new_data_validation( ).
  lo_data_validation->type        = zcl_excel_data_validation=>c_type_list.
  lo_data_validation->formula1    = 'Values'.
  lo_data_validation->cell_row    = 7.
  lo_data_validation->cell_column = 'C'.
  lo_worksheet->set_cell( ip_row = 7 ip_column = 'C' ip_value = 'Select a value' ).


  lo_data_validation              = lo_worksheet->add_new_data_validation( ).
  lo_data_validation->type        = zcl_excel_data_validation=>c_type_list.
  lo_data_validation->formula1    = 'Values'.
  lo_data_validation->cell_row    = 8.
  lo_data_validation->cell_column = 'C'.
  lo_worksheet->set_cell( ip_row = 8 ip_column = 'C' ip_value = 'Select a value' ).

  " add autosize (column width)
  lo_column = lo_worksheet->get_column( ip_column = 'B' ).
  lo_column->set_auto_size( ip_auto_size = abap_true ).
  lo_column = lo_worksheet->get_column( ip_column = 'C' ).
  lo_column->set_auto_size( ip_auto_size = abap_true ).

  " Create xlsx stream
  CREATE OBJECT lo_excel_writer TYPE zcl_excel_writer_2007.
  lv_file = lo_excel_writer->write_file( lo_excel ).

*******************************
*            Output           *
*******************************

  " Convert to binary
  lt_file_tab = cl_bcs_convert=>xstring_to_solix( iv_xstring  = lv_file ).
  lv_bytecount = xstrlen( lv_file ).

  " Save the file
  cl_gui_frontend_services=>gui_download( EXPORTING bin_filesize = lv_bytecount
                                                    filename     = lv_full_path
                                                    filetype     = 'BIN'
                                           CHANGING data_tab     = lt_file_tab ).
