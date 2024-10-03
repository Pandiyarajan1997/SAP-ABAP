*&---------------------------------------------------------------------*
*&  Include           ZDEMO_EXCEL_OUTPUTOPT_INCL
*&---------------------------------------------------------------------*
CLASS lcl_output DEFINITION .
  PUBLIC SECTION.
   CLASS-METHODS:
      format_download  IMPORTING cl_excel            TYPE REF TO zcl_excel
                                 iv_writerclass_name TYPE clike OPTIONAL
                                 iv_info_message     TYPE abap_bool DEFAULT abap_true
                       RAISING   zcx_excel.

  PRIVATE SECTION.
    METHODS:
      download_frontend
        RAISING
          zcx_excel.

    DATA: xdata     TYPE xstring,             " Will be used for sending as email
          t_rawdata TYPE solix_tab,           " Will be used for downloading or open directly
          bytecount TYPE i.                   " Will be used for downloading or open directly
ENDCLASS.                    "lcl_output DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_output IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_output IMPLEMENTATION.               "output

  METHOD: download_frontend.
    DATA: filename TYPE string,
          message  TYPE string,
          lv_path type string,
          lv_fullpath TYPE string.
* I don't like p_path here - but for this include it's ok
    filename = 'Material Master Data'."p_path.
    CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      window_title      = 'Enter file name'
      default_extension = 'xlsx'
      default_file_name = filename
      file_filter       = 'Excel files (*.XLSX)|*.XLSX|Excel files (*.XLS)|*.XLS|Text files (*.txt)|*.txt|All Files (*.*)|*.*|'
    CHANGING
      filename          = filename
      path              = lv_path
      fullpath          = lv_fullpath.
* Add trailing "\" or "/"
    IF filename CA '/'.
      REPLACE REGEX '([^/])\s*$' IN filename WITH '$1/' .
    ELSE.
      REPLACE REGEX '([^\\])\s*$' IN filename WITH '$1\\'.
    ENDIF.

*    CONCATENATE filename gc_save_file_name INTO filename.
* Get trailing blank
    cl_gui_frontend_services=>gui_download( EXPORTING bin_filesize = bytecount
                                                      filename     = lv_fullpath
                                                      filetype     = 'BIN'
                                             CHANGING data_tab     = t_rawdata
                                           EXCEPTIONS OTHERS       = 1 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO message.
      RAISE EXCEPTION TYPE zcx_excel EXPORTING error = message.
      ELSE.
      CALL METHOD cl_gui_frontend_services=>execute
        EXPORTING
          document               = lv_fullpath
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
  ENDMETHOD.                    "download_frontend
  METHOD Format_download.
    DATA: cl_output TYPE REF TO lcl_output,
          cl_writer TYPE REF TO zif_excel_writer,
          cl_error  TYPE REF TO zcx_excel.

    TRY.

        IF iv_writerclass_name IS INITIAL.
          CREATE OBJECT cl_output.
          CREATE OBJECT cl_writer TYPE zcl_excel_writer_2007.
        ELSE.
          CREATE OBJECT cl_output.
          CREATE OBJECT cl_writer TYPE (iv_writerclass_name).
        ENDIF.
        cl_output->xdata = cl_writer->write_file( cl_excel ).

        cl_output->t_rawdata = cl_bcs_convert=>xstring_to_solix( iv_xstring  = cl_output->xdata ).
        cl_output->bytecount = xstrlen( cl_output->xdata ).
    ENDTRY.
    IF sy-batch IS INITIAL.
      cl_output->download_frontend( ).
    ELSE.
      MESSAGE e802(zabap2xlsx).
    ENDIF.

  ENDMETHOD.

ENDCLASS.                    "lcl_output IMPLEMENTATION
