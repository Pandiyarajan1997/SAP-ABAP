*&---------------------------------------------------------------------*
*& Report ZMM_SERVICE_PR_CREATION
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmm_service_pr_creation.
INCLUDE zmm_service_pr_creation_cls.
TYPES: BEGIN OF ty_file,
         sno(2)  TYPE c,
         htext   TYPE text80,
         knttp   TYPE knttp,
         epstp   TYPE epstp,
         txz01   TYPE txz01,
         menge   TYPE bamng,
         wgbez   TYPE wgbez,
         werks   TYPE werks_d,
         ekgrp   TYPE ekgrp,
         afnam   TYPE afnam,
         ktext1  TYPE ktext1,
         menge_s TYPE bamng,
         meins   TYPE bamei,
         tbtwr   TYPE sbrtwr,
         sakto   TYPE sakto,
         kostl   TYPE kostl,
*         message TYPE text255,
       END OF ty_file.
TYPES: BEGIN OF ty_output,
         msgtype TYPE msgty,
         message TYPE bapi_msg,
       END OF ty_output.
DATA : lt_up_file    TYPE TABLE OF ty_file,
       ls_up_file    TYPE ty_file,
       lt_popup_file TYPE TABLE OF ty_output,
       ls_popup_file TYPE ty_output.
SELECTION-SCREEN BEGIN OF BLOCK a WITH FRAME TITLE TEXT-001.
  PARAMETERS: fname TYPE rlgrap-filename OBLIGATORY.
  PARAMETERS: p_htext TYPE text80.
*  PARAMETERS: p_keydt TYPE sy-datum DEFAULT sy-datum.
SELECTION-SCREEN END OF BLOCK a.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR fname.

  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    EXPORTING
      program_name  = syst-repid
      dynpro_number = syst-dynnr
      field_name    = ' '
      static        = ' '
      mask          = ' '
    CHANGING
      file_name     = fname.

START-OF-SELECTION.

  PERFORM f_read_uploadfile.

  PERFORM f_display_output.
*&---------------------------------------------------------------------*
*& Form f_read_uploadfile
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_read_uploadfile .
  DATA: mtable TYPE TABLE OF alsmex_tabline WITH HEADER LINE.
  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename    = fname
      i_begin_col = 1
      i_begin_row = 2
      i_end_col   = 15
      i_end_row   = 99999
    TABLES
      intern      = mtable.
  LOOP AT mtable.
    CASE mtable-col.
      WHEN 1.
        ls_up_file-sno = mtable-value.
*      WHEN 2.
*        ls_up_file-htext = mtable-value.
      WHEN 2.
        ls_up_file-knttp = mtable-value.
      WHEN 3.
        ls_up_file-epstp = mtable-value.
      WHEN 4.
        ls_up_file-txz01 = mtable-value.
      WHEN 5.
        ls_up_file-menge = mtable-value.
      WHEN 6.
        ls_up_file-wgbez = mtable-value.
      WHEN 7.
        ls_up_file-werks = mtable-value.
      WHEN 8.
        ls_up_file-ekgrp = mtable-value.
      WHEN 9.
        ls_up_file-afnam = mtable-value.
      WHEN 10.
        ls_up_file-ktext1 = mtable-value.
      WHEN 11.
        ls_up_file-menge_s = mtable-value.
      WHEN 12.
        ls_up_file-meins = mtable-value.
        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
          EXPORTING
            input          = ls_up_file-meins
            language       = sy-langu
          IMPORTING
            output         = ls_up_file-meins
          EXCEPTIONS
            unit_not_found = 1
            OTHERS         = 2.
        IF sy-subrc <> 0.
* Implement suitable error handling here
        ENDIF.


      WHEN 13.
        ls_up_file-tbtwr = mtable-value.
      WHEN 14.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = mtable-value
          IMPORTING
            output = ls_up_file-sakto.
      WHEN 15.
        ls_up_file-kostl = mtable-value.
      WHEN OTHERS.
    ENDCASE.
    AT END OF row.
      APPEND ls_up_file TO lt_up_file.
      CLEAR :ls_up_file,mtable.
    ENDAT.
  ENDLOOP.
  IF lt_up_file[] IS INITIAL.
    DATA(lv_err) = 'No data found'.
    MESSAGE lv_err TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ELSE.
    LOOP AT lt_up_file ASSIGNING FIELD-SYMBOL(<fs_file>).
      IF <fs_file>-sno IS INITIAL.
        ls_popup_file-msgtype = 'E'.
        ls_popup_file-message = |Line no{ sy-tabix } Serial No is empty|.
        APPEND ls_popup_file TO lt_popup_file.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_display_output
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_display_output .
  DATA: "lo_gr_alv       TYPE REF TO cl_salv_table, " Variables for ALV properties
        lo_gr_functions TYPE REF TO cl_salv_functions_list.

  DATA: lo_event_handler TYPE REF TO lcl_handle_events, " Variables for events
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
  DATA lo_gr_alv       TYPE REF TO cl_salv_table. " Variables for ALV properties
* Create the ALV object
  TRY.
      CALL METHOD cl_salv_table=>factory
        IMPORTING
          r_salv_table = lo_gr_alv
        CHANGING
          t_table      = lt_up_file.
    CATCH cx_salv_msg.
  ENDTRY.

  lo_gr_alv->set_screen_status(
    pfstatus      =  'SPR_STATUS'
    report        =  sy-repid
    set_functions = lo_gr_alv->c_functions_all ).

  lr_aggregations = lo_gr_alv->get_aggregations( ).
* Let's show all default buttons of ALV
  lo_gr_functions = lo_gr_alv->get_functions( ).
  lo_gr_functions->set_all( value  = if_salv_c_bool_sap=>true ).

  lr_aggregations->clear( ).
  lr_groups = lo_gr_alv->get_sorts( ) .
  lr_groups->clear( ).
*
*  TRY.
*      lr_groups->add_sort(
*     columnname = 'DOC_NO'
*     position   = 1
*     subtotal   = abap_true
*     sequence   = if_salv_c_sort=>sort_up ).
*
*    CATCH cx_salv_not_found cx_salv_data_error cx_salv_existing.
*  ENDTRY.
*  TRY.
*      lr_groups->add_sort(
*     columnname = 'COSTCENTER'
*     position   = 2
*     subtotal   = abap_true
*     sequence   = if_salv_c_sort=>sort_up ).
*
*    CATCH cx_salv_not_found cx_salv_data_error cx_salv_existing.
*  ENDTRY.
*  TRY.
*      lr_aggregations->add_aggregation( columnname = 'DB_AMT_DOCCUR' ).
*    CATCH cx_salv_not_found cx_salv_data_error cx_salv_existing.
*  ENDTRY.
*  TRY.
*      lr_aggregations->add_aggregation( columnname = 'CR_AMT_DOCCUR' ).
*    CATCH cx_salv_not_found cx_salv_data_error cx_salv_existing.
*  ENDTRY.

* Fit the columns
  lo_columns = lo_gr_alv->get_columns( ).
  lo_columns->set_optimize( 'X' ).

* Create header
  DESCRIBE TABLE lt_up_file LINES lv_rows.
  CONCATENATE 'Number of records: ' lv_rows INTO lv_title SEPARATED BY space.

  CREATE OBJECT lo_grid.
  CREATE OBJECT lo_layout_logo.
  lo_grid->create_label( row = 1 column = 1 text = lv_title tooltip = lv_title ).
  lo_layout_logo->set_left_content( lo_grid ).
  lo_content = lo_layout_logo.
  lo_gr_alv->set_top_of_list( lo_content ).

* Apply zebra style to lv_rows
  lo_display = lo_gr_alv->get_display_settings( ).
  lo_display->set_striped_pattern( cl_salv_display_settings=>true ).

* Enable the save layout buttons
  lv_key-report = sy-repid.
  lo_layout = lo_gr_alv->get_layout( ).
  lo_layout->set_key( lv_key ).
  lo_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
  lo_layout->set_default( abap_true ).

* Register events
  lo_events = lo_gr_alv->get_event( ).
  CREATE OBJECT lo_event_handler.
  SET HANDLER lo_event_handler->on_user_command FOR lo_events.

* Enable cell selection mode
  lo_selections = lo_gr_alv->get_selections( ).
  lo_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).

  TRY.
      lo_column ?= lo_columns->get_column( 'HTEXT' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Header Text' ).
      lo_column->set_medium_text( 'HeaderText' ).
      lo_column->set_short_text( 'H.Text' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'TXZ01' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Item Text' ).
      lo_column->set_medium_text( 'Item Text' ).
      lo_column->set_short_text( 'Item Text' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.
  TRY.
      lo_column ?= lo_columns->get_column( 'KTEXT1' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Service Short Text' ).
      lo_column->set_medium_text( 'Service Text' ).
      lo_column->set_short_text( 'Serv.Text' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.
  TRY.
      lo_column ?= lo_columns->get_column( 'SAKTO' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'G/L Account' ).
      lo_column->set_medium_text( 'G/L Account' ).
      lo_column->set_short_text( 'G/L Acc' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'MESSAGE' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Remark' ).
      lo_column->set_medium_text( 'Remark' ).
      lo_column->set_short_text( 'Remark' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  lo_gr_alv->display( ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form handle_user_command
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_SALV_FUNCTION
*&---------------------------------------------------------------------*
FORM handle_user_command  USING i_ucomm TYPE  salv_de_function.

  CASE i_ucomm.
    WHEN '&SAL_POST'.
      IF lt_popup_file IS INITIAL.
        PERFORM f_create_purchase_req.
      ENDIF.
      IF lt_popup_file IS NOT INITIAL.
        CALL FUNCTION 'Z_POPUP_ALV'
          EXPORTING
            i_start_column      = 25
            i_start_line        = 6
            i_end_column        = 90
            i_end_line          = 20
            i_title             = 'File Status Display'
            i_status_field_name = 'MSGTYPE'
            i_popup             = 'X'
          TABLES
            it_alv              = lt_popup_file.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_create_purchase_req
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_create_purchase_req .
*&---------------------------------------------------------------------*
*& Report ZRAM_ABAP_TEST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*

  DATA lv_sno TYPE c.
  DATA lv_preq_item TYPE bnfpo VALUE '00000'.
  DATA lv_srv_line TYPE extrow.
  DATA lv_serial_no  TYPE dzekkn.
  CONSTANTS: c_outline_no TYPE outline_no VALUE '0000000001'.
  DATA : lw_header TYPE thead.
  DATA: lw_tline TYPE tline,
        lt_tline TYPE STANDARD TABLE OF tline.
  DATA: ls_header    TYPE bapimereqheader,
        ls_headerx   TYPE bapimereqheaderx,
        lt_pritem    TYPE TABLE OF bapimereqitemimp,
        lt_pritemx   TYPE TABLE OF bapimereqitemx,
        lt_account   TYPE TABLE OF bapimereqaccount,
        lt_accountx  TYPE TABLE OF bapimereqaccountx,
        lt_service   TYPE TABLE OF bapi_srv_service_line,
        lt_service_x TYPE TABLE OF bapi_srv_service_linex,
        lt_seracc    TYPE TABLE OF bapi_srv_acc_data,
        lt_seraccx   TYPE TABLE OF bapi_srv_acc_datax,
        lt_return    TYPE TABLE OF bapiret2,
        lv_package   TYPE num15.

  DATA: lv_prnumber TYPE bapimereqheader-preq_no.
  DATA: lv_fund TYPE  char10.
  CLEAR ls_header.
  ls_header-pr_type = 'ZSR'.

  CLEAR ls_headerx.
  ls_headerx-pr_type = abap_true.

  SORT lt_up_file BY sno.
  CALL FUNCTION 'ZFUND_GET'
    EXPORTING
      date       = sy-datum
    IMPORTING
      lv_fund    = lv_fund
    EXCEPTIONS
      enter_date = 1
      OTHERS     = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  LOOP AT lt_up_file ASSIGNING FIELD-SYMBOL(<fs_data>).
    IF lv_sno <> <fs_data>-sno.
      lv_srv_line = '0000000010'.
      lv_serial_no = 0.
      lv_preq_item = lv_preq_item + 10.
      lv_sno = <fs_data>-sno.
      CLEAR: lv_package.
      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr             = '01'
          object                  = 'SERVICE'
        IMPORTING
          number                  = lv_package
        EXCEPTIONS
          interval_not_found      = 1
          number_range_not_intern = 2
          object_not_found        = 3
          quantity_is_0           = 4
          quantity_is_not_1       = 5
          interval_overflow       = 6
          buffer_overflow         = 7
          OTHERS                  = 8.
    ELSE.
      lv_srv_line = lv_srv_line + 10.
    ENDIF.
    lv_serial_no = lv_serial_no + 1.

    APPEND VALUE #( doc_item  = lv_preq_item
                    outline = c_outline_no
                    srv_line  = lv_srv_line
                    short_text  = <fs_data>-ktext1
                    quantity  = <fs_data>-menge_s
                    uom = <fs_data>-meins
                    gross_price = <fs_data>-tbtwr
                    currency  = 'INR')  TO lt_service.


    APPEND VALUE #( doc_item    = lv_preq_item
                    outline     = c_outline_no
                    srv_line    = lv_srv_line
                    short_text  = abap_true
                    quantity    = abap_true
                    uom = abap_true
                    gross_price = abap_true
                    currency    = abap_true
                    userf1_txt  = abap_true
                    userf2_txt  = abap_true ) TO lt_service_x.

    APPEND VALUE #( doc_item  = lv_preq_item
                    outline   = c_outline_no
                    srv_line  = lv_srv_line
                    serial_no = lv_serial_no
                    serial_no_item  = lv_serial_no
                    percent = '100' ) TO lt_seracc.


    APPEND VALUE #( doc_item  = lv_preq_item
                    outline   = c_outline_no
                    srv_line  = lv_srv_line
                    serial_no = lv_serial_no
                    serial_no_item  = abap_true
                    percent   = abap_true ) TO lt_seraccx.

    APPEND VALUE #( preq_item   = lv_preq_item
                    serial_no   = lv_serial_no
                    gl_account  = <fs_data>-sakto
                    costcenter  = <fs_data>-kostl
                    fund        =  lv_fund ) TO lt_account.

    APPEND VALUE #( preq_item   = lv_preq_item
                    serial_no   = lv_serial_no
                    preq_itemx  = abap_true
                    serial_nox  = abap_true
                    fund        = abap_true
                    gl_account  = abap_true
                    costcenter  = abap_true ) TO lt_accountx.
    AT END OF sno.
      APPEND VALUE #( preq_item   = lv_preq_item
                      pur_group   = <fs_data>-ekgrp
                      preq_name   = <fs_data>-afnam
                      short_text  = <fs_data>-txz01
                      plant       = <fs_data>-werks
                      matl_group  = <fs_data>-wgbez
                      item_cat    = <fs_data>-epstp
                      acctasscat  = <fs_data>-knttp
                      pckg_no     = lv_package ) TO lt_pritem.

      APPEND VALUE #( preq_item  = lv_preq_item
                      pur_group = abap_true
                      preq_name = abap_true
                      short_text  = abap_true
                      plant = abap_true
                      matl_group  = abap_true
                      item_cat  = abap_true
                      acctasscat  = abap_true
                      pckg_no = abap_true ) TO lt_pritemx.
    ENDAT.
  ENDLOOP.

  REFRESH: lt_return.
  CALL FUNCTION 'BAPI_PR_CREATE'
    EXPORTING
      prheader        = ls_header
      prheaderx       = ls_headerx
    IMPORTING
      number          = lv_prnumber
    TABLES
      return          = lt_return
      pritem          = lt_pritem
      pritemx         = lt_pritemx
      praccount       = lt_account
      praccountx      = lt_accountx
      servicelines    = lt_service
      servicelinesx   = lt_service_x
      serviceaccount  = lt_seracc
      serviceaccountx = lt_seraccx.
  LOOP AT lt_return INTO DATA(lw_ret) WHERE type = 'E' OR type = 'A'.
    ls_popup_file-msgtype = lw_ret-type.
    ls_popup_file-message = lw_ret-message.
    APPEND ls_popup_file TO lt_popup_file.
  ENDLOOP.
  IF sy-subrc NE 0.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
    ls_popup_file-msgtype = 'S'.
    ls_popup_file-message = |Document has been posted successfully : { lv_prnumber }|.
    APPEND ls_popup_file TO lt_popup_file.

*to populate the data into SAVE_TEXT,you should know the values for the below fields.
    lw_header-tdobject = 'EBANH'.
    lw_header-tdname   = lv_prnumber .
    lw_header-tdid     = 'B01'.
    lw_header-tdspras  = 'E'.

    lw_tline-tdformat = '*'.
    lw_tline-tdline = p_htext.
    APPEND lw_tline TO lt_tline.

    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
        client          = sy-mandt
        header          = lw_header
        savemode_direct = 'X'
      TABLES
        lines           = lt_tline
      EXCEPTIONS
        id              = 1
        language        = 2
        name            = 3
        object          = 4
        OTHERS          = 5.
  ENDIF.
  REFRESH: lt_pritem,lt_pritemx,lt_account,lt_accountx,lt_service,lt_service_x,lt_seracc,lt_seraccx.
ENDFORM.
