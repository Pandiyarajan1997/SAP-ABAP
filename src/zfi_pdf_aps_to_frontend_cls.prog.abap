*&---------------------------------------------------------------------*
*& Include          ZFI_PDF_APS_TO_FRONTEND_CLS
*&---------------------------------------------------------------------*
CLASS lcl_pdf_attach DEFINITION.
  PUBLIC SECTION.
    METHODS: list_of_files,
      actual_process, build_alv.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_fidoc,
             bukrs TYPE bukrs,
             belnr TYPE belnr_d,
             gjahr TYPE gjahr,
           END OF ty_fidoc.
    DATA: ls_fidocno TYPE ty_fidoc.
    TYPES: BEGIN OF ty_midoc,
             belnr TYPE belnr_d,
             gjahr TYPE gjahr,
           END OF ty_midoc.
    DATA: ls_midocno TYPE ty_midoc.

    TYPES: BEGIN OF alv,
             docno TYPE belnr_d,
             gjahr TYPE gjahr,
             fname TYPE epsfilnam,
             msg   TYPE string,
           END OF alv.
    DATA: lt_alv TYPE TABLE OF alv.

    DATA: lv_directory   TYPE epsf-epsdirnam VALUE '/sapmnt/vportal',
          lv_dirname     TYPE epsf-epsdirnam,
          gv_dirname     TYPE epsf-epsdirnam,
          lv_filecntr    TYPE epsf-epsfilsiz,
          lv_errorcntr   TYPE epsf-epsfilsiz,
          lt_file_counts TYPE TABLE OF epsfili.
    DATA: lv_filesize TYPE epsf-epsfilsiz,
          lv_filetype TYPE epsf-epsfiltyp.
    DATA: lv_window   TYPE string,
          lv_filepath TYPE string.
    DATA: lv_flag TYPE flag,
          lv_msg  TYPE string.

ENDCLASS.
CLASS lcl_pdf_attach IMPLEMENTATION.
  METHOD list_of_files.
    REFRESH: lt_file_counts.
    CLEAR: lv_dirname,lv_filecntr,lv_errorcntr.
    CALL FUNCTION 'EPS_GET_DIRECTORY_LISTING'
      EXPORTING
        dir_name               = lv_directory
        file_mask              = '*'
      IMPORTING
        dir_name               = lv_dirname
        file_counter           = lv_filecntr
        error_counter          = lv_errorcntr
      TABLES
        dir_list               = lt_file_counts
      EXCEPTIONS
        invalid_eps_subdir     = 1
        sapgparam_failed       = 2
        build_directory_failed = 3
        no_authorization       = 4
        read_directory_failed  = 5
        too_many_read_errors   = 6
        empty_directory_list   = 7
        OTHERS                 = 8.
    IF sy-subrc = 0.
      CLEAR gv_dirname.
      gv_dirname = lv_dirname.
    ENDIF.
  ENDMETHOD.
  METHOD actual_process.
    IF lt_file_counts IS NOT INITIAL.
      REFRESH: lt_alv.
      LOOP AT me->lt_file_counts ASSIGNING FIELD-SYMBOL(<fs_list>).
*Splitting the Document Number Details
        DATA(lv_docno) = |{ <fs_list>-name  }|.
        REPLACE ALL OCCURRENCES OF '.pdf' IN lv_docno WITH ''.
        DATA(str_length) = strlen( lv_docno ).

        IF str_length = 18.
          CLEAR ls_fidocno.
          ls_fidocno = lv_docno.
*Check whether the Fi Document or Miro Invoices
          SELECT SINGLE * FROM bkpf
            INTO @DATA(l_fidocs)
            WHERE bukrs = @ls_fidocno-bukrs
            AND belnr = @ls_fidocno-belnr
            AND gjahr = @ls_fidocno-gjahr.
        ELSEIF str_length = 14.
          CLEAR ls_midocno.
          ls_midocno = lv_docno.
          SELECT SINGLE * FROM rbkp INTO @DATA(l_mmdocs)
            WHERE belnr = @ls_midocno-belnr
            AND gjahr = @ls_midocno-gjahr.
        ENDIF.

        DATA(lv_fname) = CONV text100( |{ gv_dirname }/{ <fs_list>-name  }| ).
*        CONDENSE lv_fname NO-GAPS.
        IF l_fidocs IS NOT INITIAL.
          CLEAR: lv_flag,lv_msg.
          CALL FUNCTION 'ZATTACH_PDF_APPL_SERVER'
            EXPORTING
              fname             = lv_fname
              belnr             = l_fidocs-belnr
              mjahr             = l_fidocs-gjahr
              bukrs             = l_fidocs-bukrs
              doctyp            = 'ZVENINV'
              objecttype        = 'BKPF'
            IMPORTING
              status            = lv_flag
              msg               = lv_msg
            EXCEPTIONS
              filename_missing  = 1
              opendata_seterror = 2
              docno_missing     = 3
              OTHERS            = 4.
          IF sy-subrc <> 0.
            CASE sy-subrc.
              WHEN '1'.
                lv_msg = |filename_missing|.
              WHEN '2'.
                lv_msg = |opendata_seterror|.
              WHEN '3'.
                lv_msg = |docno_missing|.
              WHEN OTHERS.
                lv_msg = |Error Not Listed|.
            ENDCASE.
          ENDIF.
          APPEND VALUE #( docno = l_fidocs-belnr
                          gjahr = l_fidocs-gjahr
                          fname = lv_fname
                          msg   = lv_msg ) TO lt_alv.
        ELSEIF l_mmdocs IS NOT INITIAL.
          CLEAR: lv_flag,lv_msg.
          CALL FUNCTION 'ZATTACH_PDF_APPL_SERVER'
            EXPORTING
              fname             = lv_fname
              mblnr             = l_mmdocs-belnr
              mjahr             = l_mmdocs-gjahr
              doctyp            = 'ZINVOICE'
              objecttype        = 'BUS2081'
            IMPORTING
              status            = lv_flag
              msg               = lv_msg
            EXCEPTIONS
              filename_missing  = 1
              opendata_seterror = 2
              docno_missing     = 3
              OTHERS            = 4.
          IF sy-subrc <> 0.
            CASE sy-subrc.
              WHEN '1'.
                lv_msg = |filename_missing|.
              WHEN '2'.
                lv_msg = |opendata_seterror|.
              WHEN '3'.
                lv_msg = |docno_missing|.
              WHEN OTHERS.
                lv_msg = |Error Not Listed|.
            ENDCASE.
          ENDIF.
          APPEND VALUE #( docno = l_mmdocs-belnr
                          gjahr = l_mmdocs-gjahr
                          fname = lv_fname
                          msg   = lv_msg ) TO lt_alv.
        ELSE.

        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
  METHOD build_alv.
    DATA: lo_gr_alv       TYPE REF TO cl_salv_table, " Variables for ALV properties
          lo_gr_functions TYPE REF TO cl_salv_functions_list.

    DATA: lo_grid        TYPE REF TO cl_salv_form_layout_grid, " Variables for header
          lo_layout_logo TYPE REF TO cl_salv_form_layout_logo,
          lo_display     TYPE REF TO cl_salv_display_settings,
          lo_columns     TYPE REF TO cl_salv_columns,
          lo_column      TYPE REF TO cl_salv_column_table.

* create the alv object
    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = lo_gr_alv
          CHANGING
            t_table      = lt_alv.
      CATCH cx_salv_msg.
    ENDTRY.
* Let's show all default buttons of ALV
    lo_gr_functions = lo_gr_alv->get_functions( ).
    lo_gr_functions->set_all( value  = if_salv_c_bool_sap=>true ).

* Fit the columns
    lo_columns = lo_gr_alv->get_columns( ).
    lo_columns->set_optimize( 'X' ).


* Apply zebra style to lv_rows
    lo_display = lo_gr_alv->get_display_settings( ).
    lo_display->set_striped_pattern( cl_salv_display_settings=>true ).
* Apply zebra style to lv_rows
    lo_display = lo_gr_alv->get_display_settings( ).
    lo_display->set_striped_pattern( cl_salv_display_settings=>true ).

    TRY.
        lo_column ?= lo_columns->get_column( 'DOCNO' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'DocNumber' ).
        lo_column->set_medium_text( 'DocNum' ).
        lo_column->set_short_text( 'Docno' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'GJAHR' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Fisyear' ).
        lo_column->set_medium_text( 'Fisyear' ).
        lo_column->set_short_text( 'Fisyear' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'FNAME' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Filename' ).
        lo_column->set_medium_text( 'Filename' ).
        lo_column->set_short_text( 'Filename' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'MSG' ).
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
ENDCLASS.
