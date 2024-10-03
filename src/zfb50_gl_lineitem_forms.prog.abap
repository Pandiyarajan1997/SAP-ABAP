*&---------------------------------------------------------------------*
*& Include          ZFB50_GL_LINEITEM_FORMS
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form f_excel_download
*&---------------------------------------------------------------------*
FORM f_excel_download .
  PERFORM f_excel_instantiate.
  lv_filename1 = | { 'JV Posting Template' } |.
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
*&---------------------------------------------------------------------*
*& Form f_excel_instantiate
*&---------------------------------------------------------------------*
FORM f_excel_instantiate .

  GET REFERENCE OF lt_excel INTO lr_excel_structure.
  DATA(lo_itab_services) = cl_salv_itab_services=>create_for_table_ref( lr_excel_structure ).
  lo_source_table_descr ?= cl_abap_tabledescr=>describe_by_data_ref( lr_excel_structure ).
  lo_table_row_descripter ?= lo_source_table_descr->get_table_line_type( ).

  DATA(lo_tool_xls) = cl_salv_export_tool_ats_xls=>create_for_excel(
                               EXPORTING r_data = lr_excel_structure ).

  DATA(lo_config) = lo_tool_xls->configuration( ).
  lo_config->add_column(
       EXPORTING
         header_text = 'Serial No'
         field_name = 'SNO'
         display_type = if_salv_bs_model_column=>uie_text_view ).
  lo_config->add_column(
       EXPORTING
         header_text = 'Reference Document'
         field_name = 'REFDOC'
         display_type = if_salv_bs_model_column=>uie_text_view ).
  lo_config->add_column(
     EXPORTING
       header_text = 'Posting date'
       field_name = 'PDATE'
       display_type = if_salv_bs_model_column=>uie_text_view ).
  lo_config->add_column(
         EXPORTING
           header_text = 'Posting Key'
           field_name = 'KEY'
           display_type = if_salv_bs_model_column=>uie_text_view ).

  lo_config->add_column(
        EXPORTING
          header_text = 'G/L Account'
          field_name = 'GL_ACCOUNT'
          display_type = if_salv_bs_model_column=>uie_text_view ).

  lo_config->add_column(
        EXPORTING
          header_text = 'G/L text'
          field_name = 'GL_ACCOUNT_TXT'
          display_type = if_salv_bs_model_column=>uie_text_view ).

  lo_config->add_column(
        EXPORTING
          header_text = 'Amount'
          field_name = 'AMT'
          display_type = if_salv_bs_model_column=>uie_text_view ).

  lo_config->add_column(
        EXPORTING
          header_text = 'Item text'
          field_name = 'ITEM_TXT'
          display_type = if_salv_bs_model_column=>uie_text_view ).

  lo_config->add_column(
        EXPORTING
          header_text = 'Costcenter'
          field_name = 'COSTCENTER'
          display_type = if_salv_bs_model_column=>uie_text_view ).

  lo_config->add_column(
        EXPORTING
          header_text = 'Plant'
          field_name = 'PLANT'
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
*&---------------------------------------------------------------------*
*& Form f_jv_posting
*&---------------------------------------------------------------------*
FORM f_jv_posting .
  DATA(lt_excel1) = lt_excel[].
  DATA(lt_glpost_dt) = lt_excel[].
  DELETE lt_glpost_dt WHERE key = '50'.
  DESCRIBE TABLE lt_glpost_dt LINES DATA(lv_glpost_dt).
  DATA(lt_glpost_ct) = lt_excel[].
  DELETE lt_glpost_ct WHERE key = '40'.
  DESCRIBE TABLE lt_glpost_ct LINES DATA(lv_glpost_ct).
  DELETE ADJACENT DUPLICATES FROM lt_excel COMPARING sno.

  LOOP AT lt_excel ASSIGNING FIELD-SYMBOL(<fs_post>).
    REFRESH: lt_glaccount,lt_curramnt.
*** Header content of the document ***
    CLEAR: ls_header,lv_fisyear,lv_month,l_return.
*** FUNCTION module to GET fiscal year ***
    CALL FUNCTION 'BAPI_COMPANYCODE_GET_PERIOD'
      EXPORTING
        companycodeid = p_bukrs
        posting_date  = <fs_post>-pdate
      IMPORTING
        fiscal_year   = lv_fisyear
        fiscal_period = lv_month
        return        = l_return.
    ls_header = VALUE bapiache09( bus_act = 'RFBU'
                                  username = sy-uname
                                  comp_code = p_bukrs
                                  doc_date = <fs_post>-pdate
                                  pstng_date = <fs_post>-pdate
                                  ref_doc_no = <fs_post>-refdoc
                                  fisc_year = lv_fisyear
                                  doc_type = p_blart ).
    CLEAR lv_fund.
    CALL FUNCTION 'ZFUND_GET'
      EXPORTING
        date       = <fs_post>-pdate
      IMPORTING
        lv_fund    = lv_fund
      EXCEPTIONS
        enter_date = 1
        OTHERS     = 2.
    IF sy-subrc <> 0.

    ENDIF.

*** Debit table for Posting ***
    CLEAR lv_debit.
    LOOP AT lt_glpost_dt ASSIGNING FIELD-SYMBOL(<fls_post_dt>) WHERE sno = <fs_post>-sno.
      lv_debit = lv_debit + <fls_post_dt>-amt.
    ENDLOOP.
*** credit table for posting ***
    CLEAR lv_credit.
    LOOP AT lt_glpost_ct ASSIGNING FIELD-SYMBOL(<fls_post_ct>) WHERE sno = <fs_post>-sno. "WHERE item_txt = <fls_post_dt>-item_txt.
      lv_credit = lv_credit + <fls_post_ct>-amt.
    ENDLOOP.
** If difference is not equal to zero then knock off is wrong ***
    CLEAR lv_difference.
    lv_difference = lv_debit + lv_credit.
** If difference is not equal to zero then knock off is wrong ***
    IF lv_difference <> '0.00'.
      APPEND VALUE #( sno = <fs_post>-sno
                      type = 'E'
                      d_amt = lv_debit
                      c_amt = lv_credit
                      message = |Difference of D/C Amount not Matching| ) TO gt_alv.
      CONTINUE.
    ELSE.
      DATA:  lv_itmcnt TYPE i.
      lv_itmcnt = '1'.
      LOOP AT lt_excel1 ASSIGNING FIELD-SYMBOL(<fs_posting>).
        DATA(lv_glacc) = VALUE #( gt_skat[ saknr = <fs_posting>-gl_account ]-saknr OPTIONAL ).
        IF lv_glacc IS INITIAL.
          APPEND VALUE #( sno = <fs_post>-sno
                          type = 'E'
                          d_amt = lv_debit
                          c_amt = lv_credit
                          message = |G/L Account { <fs_posting>-gl_account } does not exists | ) TO gt_alv.
          CONTINUE.
        ELSE.
          SHIFT lv_glacc LEFT DELETING LEADING '0'.
        ENDIF.
        IF <fs_posting>-costcenter IS NOT INITIAL.
          SELECT SINGLE kostl FROM csks INTO @DATA(lv_costcenter) WHERE kostl = @<fs_posting>-costcenter.
          IF sy-subrc NE 0.
            APPEND VALUE #( sno = <fs_post>-sno
                            type = 'E'
                            d_amt = lv_debit
                            c_amt = lv_credit
                            message = |Costcenter { <fs_posting>-costcenter } does not exists | ) TO gt_alv.
            CONTINUE.
          ENDIF.
        ENDIF.
*** G/L account posting entries ***
        APPEND VALUE #( itemno_acc = lv_itmcnt
                        gl_account = <fs_posting>-gl_account
                        costcenter = <fs_posting>-costcenter
*                      funds_ctr = <fls_gl_post>-costcenter
                        fund = lv_fund
                        cmmt_item = lv_glacc
                        item_text = <fs_posting>-item_txt
                        plant = <fs_posting>-plant  ) TO lt_glaccount.
        CLEAR lv_amount.
        lv_amount = <fs_posting>-amt.
        CONDENSE lv_amount NO-GAPS.
*** Currency lineitems entries ***
        APPEND VALUE #( itemno_acc = lv_itmcnt
                        currency = 'INR'
                        amt_doccur = lv_amount
                        amt_base = lv_amount  ) TO lt_curramnt.
        lv_itmcnt = lv_itmcnt + 1.
      ENDLOOP.
      REFRESH: return.
      PERFORM f_document_check USING ls_header. " Check Document Before Posting
      SORT return BY type.
      READ TABLE return WITH KEY type = 'E'.
      IF sy-subrc EQ 0.
        CLEAR lv_msg.
        LOOP AT return INTO DATA(ls_ret) WHERE type = 'E'.
          lv_msg = |{ lv_msg } { ls_ret-message }|.
        ENDLOOP.
        APPEND VALUE #( sno = <fs_post>-sno
                        type = 'E'
                        d_amt = lv_debit
                        c_amt = lv_credit
                        message = |{ lv_msg }| ) TO gt_alv.
      ELSE.
        IF p_run EQ abap_true.
          APPEND VALUE #( sno = <fs_post>-sno
                          type = 'S'
                          d_amt = lv_debit
                          c_amt = lv_credit
                          message = |Serial Number { <fs_post>-sno } is ready to Post| ) TO gt_alv.
        ELSE.
          REFRESH return.
          PERFORM f_document_post.  " Posting Happens Here
          READ TABLE return WITH KEY type = 'E'.
          IF sy-subrc = 0.
            LOOP AT return INTO DATA(ls_ret1) WHERE type = 'E'.
              lv_msg = |{ lv_msg } { ls_ret1-message }|.
            ENDLOOP.
            APPEND VALUE #( sno = <fs_post>-sno
                            type = 'E'
                            d_amt = lv_debit
                            c_amt = lv_credit
                            message = |{ lv_msg }| ) TO gt_alv.
          ELSE.
            APPEND VALUE #( sno = <fs_post>-sno
                            type = 'S'
                            d_amt = lv_debit
                            c_amt = lv_credit
                            docno = lv_belnr
                            message = |Document Number { lv_belnr } is Posted Successfully| ) TO gt_alv.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.
FORM f_document_check USING p_header TYPE bapiache09.
*** document CHECK before posting ***
  CALL FUNCTION 'BAPI_ACC_DOCUMENT_CHECK'
    EXPORTING
      documentheader = p_header
    TABLES
      accountgl      = lt_glaccount
      currencyamount = lt_curramnt
      return         = return.
ENDFORM.
FORM f_document_post.
  CLEAR lv_belnr.
  REFRESH: return.
  CLEAR: lv_objkey,lv_objsys,lv_objtyp.
*** Function Module to create Debit note ***
  CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
    EXPORTING
      documentheader = ls_header
    IMPORTING
      obj_type       = lv_objtyp
      obj_key        = lv_objkey
      obj_sys        = lv_objsys
    TABLES
      accountgl      = lt_glaccount
      currencyamount = lt_curramnt
      return         = return.
  COMMIT WORK AND WAIT.
  lv_belnr = lv_objkey+0(10).
ENDFORM.
*---------------ALV Display -------------------------------------------------------*
FORM f_alv_display .
  DATA: "lo_gr_alv       TYPE REF TO cl_salv_table, " Variables for ALV properties
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
  DATA: lr_aggregations TYPE REF TO cl_salv_aggregations.
  DATA: lr_groups TYPE REF TO cl_salv_sorts .
  DATA: toolbar TYPE REF TO cl_salv_functions_list .
* Create the ALV object
  TRY.
      CALL METHOD cl_salv_table=>factory
        IMPORTING
          r_salv_table = lo_gr_alv
        CHANGING
          t_table      = gt_alv.
    CATCH cx_salv_msg.
  ENDTRY.

*  lr_aggregations = lo_gr_alv->get_aggregations( ).
* Let's show all default buttons of ALV
  lo_gr_functions = lo_gr_alv->get_functions( ).
  lo_gr_functions->set_all( value  = if_salv_c_bool_sap=>true ).

* Fit the columns
  lo_columns = lo_gr_alv->get_columns( ).
  lo_columns->set_optimize( 'X' ).

* Apply zebra style to lv_rows
  lo_display = lo_gr_alv->get_display_settings( ).
  lo_display->set_striped_pattern( cl_salv_display_settings=>true ).

* Enable cell selection mode
  lo_selections = lo_gr_alv->get_selections( ).
  lo_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).

  TRY.
      lo_column ?= lo_columns->get_column( 'SNO' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Serial Number' ).
      lo_column->set_medium_text( 'Serial Number' ).
      lo_column->set_short_text( 'SerialNo' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'TYPE' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Message Type' ).
      lo_column->set_medium_text( 'Msgtyp' ).
      lo_column->set_short_text( 'Msgtyp' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.
  TRY.
      lo_column ?= lo_columns->get_column( 'DOCNO' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Document Number' ).
      lo_column->set_medium_text( 'Document No' ).
      lo_column->set_short_text( 'Doc No' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.
  TRY.
      lo_column ?= lo_columns->get_column( 'D_AMT' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Debit Amount' ).
      lo_column->set_medium_text( 'DebitAmnt' ).
      lo_column->set_short_text( 'Debit' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.
  TRY.
      lo_column ?= lo_columns->get_column( 'C_AMT' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Credit Amount' ).
      lo_column->set_medium_text( 'CreditAmnt' ).
      lo_column->set_short_text( 'Credit' ).
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
ENDFORM.
