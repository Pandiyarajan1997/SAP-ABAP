*&---------------------------------------------------------------------*
*& Include          ZFI_ATTACHMENT_ARCHIVE_FORMS
*&---------------------------------------------------------------------*
CLASS lcl_attachment DEFINITION.

  PUBLIC SECTION.
    METHODS:
      restrict_select_option,
      screen_adjustment,
      get_folder_details,
      data_selection,
      file_exist_check,
      display_alv.

    METHODS: on_user_command FOR EVENT added_function OF cl_salv_events
      IMPORTING e_salv_function.

    METHODS: on_double_click FOR EVENT double_click OF cl_salv_events_table
      IMPORTING row column. "Event for Double click on alv display

ENDCLASS.

CLASS lcl_attachment IMPLEMENTATION.
  "Restricting the select options
  METHOD restrict_select_option.
    DATA: ls_restrict TYPE sscr_restrict,
          gv_class    TYPE klasse_d.

    DATA(ls_opt_list) = VALUE sscr_opt_list( name = 'EQUAL'
                                             options-eq = 'X' ).
    APPEND ls_opt_list TO ls_restrict-opt_list_tab.

    DATA(ls_ass) = VALUE sscr_ass( kind = 'S'
                                   name = 'S_BELNR'
                                   sg_main = 'I'
                                   sg_addy = ''
                                   op_main = 'EQUAL' ).
    APPEND ls_ass TO ls_restrict-ass_tab.

    CALL FUNCTION 'SELECT_OPTIONS_RESTRICT'
      EXPORTING
        restriction            = ls_restrict
      EXCEPTIONS
        too_late               = 1
        repeated               = 2
        selopt_without_options = 3
        selopt_without_signs   = 4
        invalid_sign           = 5
        empty_option_list      = 6
        invalid_kind           = 7
        repeated_kind_a        = 8
        OTHERS                 = 9.
    IF sy-subrc <> 0.
      WRITE 'error'.
    ENDIF.
  ENDMETHOD.
  "Selection Screen Adjustments
  METHOD screen_adjustment.
    LOOP AT SCREEN.
      IF screen-name = 'S_BELNR-HIGH'.
        screen-input = 1.
        screen-value_help = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  "F4 Help for Choosing the folder of Documents
  METHOD get_folder_details.
    CLEAR gv_path.
    CALL METHOD cl_gui_frontend_services=>directory_browse
      CHANGING
        selected_folder      = gv_path
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4.
    IF sy-subrc = 0.
      p_path = gv_path.
    ENDIF.
  ENDMETHOD.
  "Initial Selection to get the Document details from header
  METHOD data_selection.
    REFRESH: gt_belnr,gt_bkpf,gt_rbkp.
    IF s_belnr[] IS NOT INITIAL.
      LOOP AT s_belnr ASSIGNING FIELD-SYMBOL(<fs_belnr>).
        APPEND VALUE #( belnr = <fs_belnr>-low ) TO gt_belnr.
      ENDLOOP.
    ENDIF.
    IF gt_belnr[] IS NOT INITIAL.
*-------- Account Document Fetching --------------------------*
      SELECT bukrs
             belnr
             gjahr FROM bkpf INTO TABLE gt_bkpf
                   FOR ALL ENTRIES IN gt_belnr
                   WHERE bukrs = p_bukrs
                   AND belnr = gt_belnr-belnr
                   AND gjahr = p_gjahr.
      IF sy-subrc = 0.
        SORT gt_bkpf[] BY belnr.
      ENDIF.
*----- Miro Invoice Document Fetching -------------------------*
      SELECT belnr
             gjahr
             bukrs FROM rbkp INTO TABLE gt_rbkp
                   FOR ALL ENTRIES IN gt_belnr
                   WHERE bukrs = p_bukrs
                   AND belnr = gt_belnr-belnr
                   AND gjahr = p_gjahr.
      IF sy-subrc = 0.
        SORT gt_rbkp[] BY belnr.
      ENDIF.
    ELSE.
      MESSAGE 'Enter Document Number' TYPE 'E'.
    ENDIF.
  ENDMETHOD.
  "File Existence Check
  METHOD file_exist_check.
    TYPES: BEGIN OF ty_file,
             path(60) TYPE c,
           END OF ty_file.
    DATA: lv_fname     TYPE string,
          lv_fname_tmp TYPE string,
          lv_flag      TYPE flag.
    DATA: lt_list_files TYPE TABLE OF ty_file,
          lv_count      TYPE i.
    REFRESH lt_list_files.
    CLEAR lv_fname_tmp.
    lv_fname_tmp = p_path.
    CALL METHOD cl_gui_frontend_services=>directory_list_files
      EXPORTING
        directory                   = lv_fname_tmp
      CHANGING
        file_table                  = lt_list_files
        count                       = lv_count
      EXCEPTIONS
        cntl_error                  = 1
        directory_list_files_failed = 2
        wrong_parameter             = 3
        error_no_gui                = 4
        not_supported_by_gui        = 5
        OTHERS                      = 6.
    IF sy-subrc = 0.
    LOOP AT lt_list_files ASSIGNING FIELD-SYMBOL(<fs_files>).
      TRANSLATE <fs_files>-path TO LOWER CASE.
    ENDLOOP.
    ENDIF.

    REFRESH gt_alv.
    LOOP AT gt_belnr ASSIGNING FIELD-SYMBOL(<fgs_belnr>).
*--------------------- Checks in Accounting Document Number -------------------------------- *
      DATA(ls_docno) = VALUE #( gt_bkpf[ bukrs = p_bukrs
                                          belnr = <fgs_belnr>-belnr
                                          gjahr = p_gjahr ] OPTIONAL ).
*-------------------------- Checks in Miro Invoice Table ----------------------------------- *
      DATA(ls_miro_docno) = VALUE #( gt_rbkp[ bukrs = p_bukrs
                                              belnr = <fgs_belnr>-belnr
                                              gjahr = p_gjahr ] OPTIONAL ).
*---- If the Document Number Does not exists in both the Tables ---------------------------- *
      IF ls_docno IS INITIAL AND ls_miro_docno IS INITIAL.
        APPEND VALUE #( belnr = <fgs_belnr>-belnr
                        type = 'E'
                        message = |Document Number { <fgs_belnr>-belnr } does not exists| ) TO gt_alv.
        CONTINUE.
      ENDIF.
*-------- File Name conversion and File Existence Checks ------------------------------------ *
      CLEAR lv_flag.
      IF ls_docno IS NOT INITIAL.
        CLEAR lv_fname.
*        lv_fname = | { lv_fname_tmp } { '\' } { ls_docno-belnr } .pdf|.
          lv_fname = |{ lv_fname_tmp }{ '\' }{ ls_docno-belnr }.pdf|.
*        CONDENSE lv_fname NO-GAPS.
        DATA(lv_docno) = |{ ls_docno-belnr }.pdf|.
*        CONDENSE lv_docno NO-GAPS.
        lv_flag = VALUE #( lt_list_files[ path = lv_docno ]-path OPTIONAL ).
        IF lv_flag IS INITIAL.
          APPEND VALUE #( bukrs = ls_docno-bukrs
                          belnr = ls_docno-belnr
                          gjahr = ls_docno-gjahr
                          type = 'E'
                          fname = lv_fname
                          message = |File does not exists| ) TO gt_alv.
          CONTINUE.
        ELSE.
          APPEND VALUE #( bukrs = ls_docno-bukrs
                          belnr = ls_docno-belnr
                          gjahr = ls_docno-gjahr
                          type = 'S'
                          fname = lv_fname
                          sobj = 'BKPF'
                          dtype = 'ZVENINV'
                          message = |File ready to upload| ) TO gt_alv.
          CONTINUE.
        ENDIF.
      ELSE.
        IF ls_miro_docno IS NOT INITIAL.
          CLEAR lv_fname.
*          lv_fname = | { lv_fname_tmp } { '\' } { ls_miro_docno-belnr } .pdf|.
          lv_fname = |{ lv_fname_tmp }{ '\' }{ ls_miro_docno-belnr }.pdf|.
*          CONDENSE lv_fname NO-GAPS.
          lv_docno = |{ ls_miro_docno-belnr }.pdf|.
*          CONDENSE lv_docno NO-GAPS.
          lv_flag = VALUE #( lt_list_files[ path = lv_docno ]-path OPTIONAL ).
          IF lv_flag IS INITIAL.
            APPEND VALUE #( bukrs = ls_miro_docno-bukrs
                            belnr = ls_miro_docno-belnr
                            gjahr = ls_miro_docno-gjahr
                            type = 'E'
                            fname = lv_fname
                            message = |File does not exists| ) TO gt_alv.
            CONTINUE.
          ELSE.
            APPEND VALUE #( bukrs = ls_miro_docno-bukrs
                            belnr = ls_miro_docno-belnr
                            gjahr = ls_miro_docno-gjahr
                            type = 'S'
                            fname = lv_fname
                            sobj = 'BUS2081'
                            dtype = 'ZINVOICE'
                            message = |File ready to upload| ) TO gt_alv.
            CONTINUE.
          ENDIF.
        ENDIF.
      ENDIF.
      CLEAR: ls_docno,ls_miro_docno,lv_docno.
    ENDLOOP.
  ENDMETHOD.
  "ALV Display
  METHOD display_alv.
    DATA: lo_gr_functions TYPE REF TO cl_salv_functions_list.

    DATA: lo_event_handler TYPE REF TO lcl_attachment, " Variables for events
          lo_events        TYPE REF TO cl_salv_events_table.

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
    DATA: lr_aggregations TYPE REF TO cl_salv_aggregations.
    DATA: lr_groups TYPE REF TO cl_salv_sorts .
    DATA: toolbar TYPE REF TO cl_salv_functions_list .

* create the alv object
    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = lo_gr_alv
          CHANGING
            t_table      = gt_alv.
      CATCH cx_salv_msg.
    ENDTRY.


    lo_gr_alv->set_screen_status(
      pfstatus      =  'SAL_STATUS'
      report        =  sy-repid
      set_functions = lo_gr_alv->c_functions_all ).

* Let's show all default buttons of ALV
    lo_gr_functions = lo_gr_alv->get_functions( ).
    lo_gr_functions->set_all( value  = if_salv_c_bool_sap=>true ).

* Fit the columns
    lo_columns = lo_gr_alv->get_columns( ).
    lo_columns->set_optimize( 'X' ).


* Apply zebra style to lv_rows
    lo_display = lo_gr_alv->get_display_settings( ).
    lo_display->set_striped_pattern( cl_salv_display_settings=>true ).

* Register events
    lo_events = lo_gr_alv->get_event( ).
    CREATE OBJECT lo_event_handler.
    SET HANDLER lo_event_handler->on_user_command FOR lo_events.
    SET HANDLER lo_event_handler->on_double_click FOR lo_events.

    TRY.
        lo_column ?= lo_columns->get_column( 'FNAME' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Filename' ).
        lo_column->set_medium_text( 'Filename' ).
        lo_column->set_short_text( 'File' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'TYPE' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Msgtyp' ).
        lo_column->set_medium_text( 'Msgtyp' ).
        lo_column->set_short_text( 'Msgtyp' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'MESSAGE' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Message' ).
        lo_column->set_medium_text( 'Message' ).
        lo_column->set_short_text( 'Message' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.
    lo_gr_alv->display( ).
  ENDMETHOD.
  "User Command on ALV Display
  METHOD on_user_command.
    PERFORM handle_user_command USING e_salv_function.
  ENDMETHOD.
  "Double Click events
  METHOD on_double_click.
    IF column EQ 'BELNR'.
      READ TABLE gt_alv INTO DATA(wa_alv) INDEX  row.
      SELECT SINGLE * FROM bkpf INTO @DATA(ls_bkpf) WHERE bukrs EQ @wa_alv-bukrs
                                                  AND belnr EQ @wa_alv-belnr
                                                  AND gjahr EQ @wa_alv-gjahr.
      IF sy-subrc = 0. " Exists?
        SET PARAMETER ID 'BLN' FIELD wa_alv-belnr. "Document Number

        SET PARAMETER ID 'BUK' FIELD wa_alv-bukrs. "Company Code

        SET PARAMETER ID 'GJR' FIELD wa_alv-gjahr. "Fiscal Year

        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ELSE.
        SELECT SINGLE * FROM rbkp INTO @DATA(ls_rbkp) WHERE belnr EQ @wa_alv-belnr
                                                      AND gjahr EQ @wa_alv-gjahr
                                                      AND bukrs EQ @wa_alv-bukrs.
        IF sy-subrc = 0.
          SET PARAMETER ID 'RBN' FIELD wa_alv-belnr.

          SET PARAMETER ID 'GJR' FIELD wa_alv-gjahr.

          CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

FORM handle_user_command USING i_ucomm TYPE salv_de_function.
  DATA: lv_error TYPE flag.
  CASE i_ucomm.
    WHEN '&UPLOAD'. "Upload the File One by one Archive server
      LOOP AT gt_alv ASSIGNING FIELD-SYMBOL(<fgs_alv>) WHERE type = 'S'.
        PERFORM upload_document USING <fgs_alv> CHANGING lv_error.
        IF lv_error NE abap_true.
          <fgs_alv>-type = 'E'.
          <fgs_alv>-message = |File Not uploaded in Document { <fgs_alv>-belnr } |.
        ELSE.
          <fgs_alv>-type = 'S'.
          <fgs_alv>-message = |File Uploaded Successfully for Document { <fgs_alv>-belnr } |.
        ENDIF.
      ENDLOOP.
      lo_gr_alv->refresh( ). "Refresh ALV
  ENDCASE.
ENDFORM.
*-------------- Class to Upload the Documents in SAP Business Documents --------------------------*
FORM upload_document USING p_alv TYPE ty_alv CHANGING p_error TYPE flag.

  DATA: lo_attach TYPE REF TO zcl_attach_document.
  CREATE OBJECT lo_attach.
  DATA: lv_belnr TYPE belnr_d,
        lv_mblnr TYPE mblnr,
        lv_error TYPE flag.

  CLEAR: lv_belnr,lv_mblnr.
  IF p_alv-dtype = 'ZVENINV'.
    lv_belnr = p_alv-belnr.
  ELSE.
    lv_mblnr = p_alv-belnr.
  ENDIF.
*------- Globel Class for Document Upload in Business Documents ------------------*
  CALL METHOD lo_attach->upload_file
    EXPORTING
      bukrs                     = p_alv-bukrs
      gjahr                     = p_alv-gjahr
      belnr                     = lv_belnr
      mblnr                     = lv_mblnr
      objecttype                = p_alv-sobj
      documenttype              = p_alv-dtype
      fname                     = p_alv-fname
    IMPORTING
      upload_status             = lv_error
    EXCEPTIONS
      error_archive             = 1
      error_communicationtable  = 2
      error_connectiontable     = 3
      error_kernel              = 4
      error_parameter           = 5
      document_number_not_found = 6
      OTHERS                    = 7.
  IF sy-subrc = 0.
    p_error = lv_error.
  ENDIF.
ENDFORM.
