*&---------------------------------------------------------------------*
*& Report ZFI_SALARY_POSTING
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfi_salary_posting.
INCLUDE zfi_salary_posting_top.
*&---------------------------------------------------------------------*
*& Report  ZFB50_BAPI
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
TABLES: sscrfields.
TYPES: BEGIN OF ty_str,
         doc_no        TYPE belnr_d,
         costcenter    TYPE bapiacgl09-costcenter,
         compcode      TYPE bapiacgl09-comp_code,  " Added on 09.05.2018
         gl_account    TYPE bapiacgl09-gl_account,
         itemno_acc    TYPE bapiacgl09-itemno_acc,
         header_txt    TYPE bapiache09-header_txt,
         ref_doc_no    TYPE bapiache09-ref_doc_no,
         doc_date      TYPE bldat,
         pstng_date    TYPE budat,
         doc_type      TYPE bapiache09-doc_type,
         item_text     TYPE bapiacgl09-item_text,
         fisc_year     TYPE bapiacgl09-fisc_year,
         tax_code      TYPE bapiacgl09-tax_code,
         fund          TYPE bapiacgl09-fund, ""ADDED ON 18/06/2018
         profit_ctr    TYPE bapiacgl09-profit_ctr,
         currency      TYPE bapiaccr09-currency,
         cr_amt_doccur TYPE bapiaccr09-amt_doccur,
         db_amt_doccur TYPE bapiaccr09-amt_doccur,
       END OF ty_str.
TYPES : BEGIN OF ty_message,
          type    TYPE msgtype,
          doc_no  TYPE belnr_d,
          message TYPE text255,
        END OF ty_message.


DATA : itab   TYPE TABLE OF ty_str,
       mtable TYPE TABLE OF alsmex_tabline WITH HEADER LINE,
       wa     TYPE ty_str.

DATA: wa_documentheader TYPE bapiache09,
      i_accountgl       TYPE STANDARD TABLE OF bapiacgl09
                                                       WITH HEADER LINE,
      i_currencyamount  TYPE STANDARD TABLE OF bapiaccr09
                                                       WITH HEADER LINE,
      return            TYPE STANDARD TABLE OF  bapiret2
                                                       WITH HEADER LINE.

DATA : it_message     TYPE TABLE OF ty_message,
       wa_message     TYPE          ty_message,
       l_cost_cnt     TYPE kostl,
       l_cost_change  TYPE c,
       l_cr_amt_total TYPE bapiaccr09-amt_doccur,
       l_dr_amt_total TYPE bapiaccr09-amt_doccur,
       lv_fund        TYPE bapiacgl09-fund.

*DATA lv_total_prev TYPE bapiaccr09-amt_doccur.
*DATA lv_total TYPE bapiaccr09-amt_doccur.

DATA:"l_tot_flag TYPE c,
  "l_doc_flag TYPE c,
  lv_count  TYPE posnr_acc,
  lv_doc_no TYPE posnr_acc VALUE 1.

DATA lo_gr_alv       TYPE REF TO cl_salv_table. " Variables for ALV properties
FIELD-SYMBOLS : <wa> TYPE ty_str.
*SELECTION-SCREEN FUNCTION KEY 1.
SELECTION-SCREEN BEGIN OF BLOCK a WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_bukrs TYPE bukrs OBLIGATORY.
  PARAMETERS: p_month TYPE isellist-month OBLIGATORY.
  PARAMETERS: p_ddate TYPE sy-datum DEFAULT sy-datum.
  PARAMETERS: p_pdate TYPE sy-datum DEFAULT sy-datum.
  PARAMETERS: fname TYPE rlgrap-filename OBLIGATORY.
  PARAMETERS: p_itmes TYPE numc3 DEFAULT 900 NO-DISPLAY.
  PARAMETERS: ck_test TYPE c AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK a.

*INITIALIZATION.
*MOVE 'UploadFileFormat' TO sscrfields-functxt_01.
*
*AT SELECTION-SCREEN.
*
*  IF sscrfields-ucomm = 'FC01'.
*   PERFORM f_download_file_format.
*  ENDIF.
*&---------------------------------------------------------------------*
*&      At Selection Screen Event
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_month.

  CALL FUNCTION 'POPUP_TO_SELECT_MONTH'
    EXPORTING
      actual_month               = sy-datum(6)
    IMPORTING
      selected_month             = p_month
    EXCEPTIONS
      factory_calendar_not_found = 1
      holiday_calendar_not_found = 2
      month_not_found            = 3
      OTHERS                     = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

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
  " Document Date
  DATA(lv_doc_date) = p_ddate.
  " Posting Date
  DATA(lv_pstng_date) = p_pdate.
  " Fund text
  DATA lv_ref_doc_no TYPE text15.
  " Header text
  DATA lv_header_text TYPE text15.
  " Fiscal Year
  DATA lv_fiscal_yr TYPE text15.
  DATA(lv_year) = p_month(4).
  DATA: lv_fisyear TYPE bapi0002_4-fiscal_year,
        lv_month   TYPE bapi0002_4-fiscal_period,
        l_return   TYPE bapireturn1.
  DATA: lv_msg TYPE string.
  DATA: lt_budget TYPE STANDARD TABLE OF zstr_budget.
*** function module to get fiscal year ***
  CALL FUNCTION 'BAPI_COMPANYCODE_GET_PERIOD'
    EXPORTING
      companycodeid = p_bukrs
      posting_date  = p_pdate
    IMPORTING
      fiscal_year   = lv_fisyear
      fiscal_period = lv_month
      return        = l_return.
  IF l_return IS INITIAL.
    DATA(lv_period) = CONV num03( lv_month ).
    CALL FUNCTION 'FI_PERIOD_CHECK'
      EXPORTING
        i_bukrs          = p_bukrs
        i_gjahr          = lv_fisyear
        i_koart          = 'S'
        i_monat          = lv_period
        i_glvor          = 'RFBU'
      EXCEPTIONS
        error_period     = 1
        error_period_acc = 2
        invalid_input    = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.

      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          msgid               = sy-msgid
          msgnr               = sy-msgno
          msgv1               = sy-msgv1
          msgv2               = sy-msgv1
          msgv3               = sy-msgv1
          msgv4               = sy-msgv1
        IMPORTING
          message_text_output = lv_msg.

      MESSAGE 'FI Posting Period Not Opened - Check with Accounts Team' TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.
*  Get Buget for GL and CostCenter
  CALL FUNCTION 'ZBAPI_BUDGET_CALCULATION_MIS'
    EXPORTING
      company_code = p_bukrs
      fiscal_year  = lv_fisyear
      month        = lv_month
    TABLES
      lt_budget    = lt_budget.

  CASE p_month+4(2).
    WHEN 4.  lv_fund = 'FUNDI'.    lv_ref_doc_no = |{ p_month+2(2) }-{ p_month+2(2) + 1 } SAL1|. lv_header_text = |Salary Apr { lv_year+2(2) }|.
    WHEN 5.  lv_fund = 'FUNDII'.   lv_ref_doc_no = |{ p_month+2(2) }-{ p_month+2(2) + 1 } SAL2|. lv_header_text = |Salary May { lv_year+2(2) }|.
    WHEN 6.  lv_fund = 'FUNDIII'.  lv_ref_doc_no = |{ p_month+2(2) }-{ p_month+2(2) + 1 } SAL3|. lv_header_text = |Salary Jun { lv_year+2(2) }|.
    WHEN 7.  lv_fund = 'FUNDIV'.   lv_ref_doc_no = |{ p_month+2(2) }-{ p_month+2(2) + 1 } SAL4|. lv_header_text = |Salary Jul { lv_year+2(2) }|.
    WHEN 8.  lv_fund = 'FUNDV'.    lv_ref_doc_no = |{ p_month+2(2) }-{ p_month+2(2) + 1 } SAL5|. lv_header_text = |Salary Aug { lv_year+2(2) }|.
    WHEN 9.  lv_fund = 'FUNDVI'.  lv_ref_doc_no = |{ p_month+2(2) }-{ p_month+2(2) + 1 } SAL6|. lv_header_text =  |Salary Sep { lv_year+2(2) }|.
    WHEN 10. lv_fund = 'FUNDVII'. lv_ref_doc_no = |{ p_month+2(2) }-{ p_month+2(2) + 1 } SAL7|.  lv_header_text = |Salary Oct { lv_year+2(2) }|.
    WHEN 11. lv_fund = 'FUNDVIII'.lv_ref_doc_no = |{ p_month+2(2) }-{ p_month+2(2) + 1 } SAL8|.  lv_header_text = |Salary Nov { lv_year+2(2) }|.
    WHEN 12. lv_fund = 'FUNDIX'.   lv_ref_doc_no = |{ p_month+2(2) }-{ p_month+2(2) + 1 } SAL9|.  lv_header_text =  |Salary Dec { lv_year+2(2) }|.
    WHEN 1.  lv_fund = 'FUNDX'.   lv_ref_doc_no = |{ p_month+2(2) - 1 }-{ p_month+2(2) } SAL10|. lv_header_text =   |Salary Jan { lv_year+2(2) }|.
    WHEN 2.  lv_fund = 'FUNDXI'.  lv_ref_doc_no = |{ p_month+2(2) - 1 }-{ p_month+2(2) } SAL11|. lv_header_text =   |Salary Feb { lv_year+2(2) }|.
    WHEN 3.  lv_fund = 'FUNDXII'. lv_ref_doc_no = |{ p_month+2(2) - 1 }-{ p_month+2(2) } SAL12|. lv_header_text =   |Salary Mar { lv_year+2(2) }|.
    WHEN OTHERS.
  ENDCASE.
  IF p_month+4(2) LE 3.
    lv_fiscal_yr = lv_year - 1.
  ELSE.
    lv_fiscal_yr = lv_year.
  ENDIF.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename    = fname
      i_begin_col = 1
      i_begin_row = 2
      i_end_col   = 5
      i_end_row   = 99999
    TABLES
      intern      = mtable.
*  DELETE mtable WHERE row = 1.

  SORT mtable BY row col.
  DESCRIBE TABLE mtable LINES DATA(lv_line).
  DATA(lv_row) = VALUE #( mtable[ lv_line ]-row OPTIONAL ).
  DELETE mtable WHERE row = lv_row.
  DESCRIBE TABLE mtable LINES DATA(l_lines).
  lv_row = VALUE #( mtable[ 1 ]-row OPTIONAL ).
  LOOP AT mtable.
    CASE mtable-col.
      WHEN 1.
        wa-costcenter = mtable-value.
        TRANSLATE wa-costcenter TO UPPER CASE.
        READ TABLE itab TRANSPORTING NO FIELDS WITH KEY costcenter = wa-costcenter.
        IF sy-subrc <> 0.
          SELECT SINGLE kostl, prctr FROM csks
          INTO @DATA(l_csks)
          WHERE kokrs = '1000' AND
                kostl = @wa-costcenter AND
                datbi = '99991231'.
          IF l_csks-kostl IS INITIAL.
            wa_message-type = 'E'.
            wa_message-doc_no = ''.
            wa_message-message = |01 CostCenter({ wa-costcenter }) is not found|.
            APPEND wa_message TO it_message.
          ENDIF.
          IF l_csks-prctr IS INITIAL.
            wa_message-type = 'E'.
            wa_message-doc_no = ''.
            wa_message-message = |01 Profit center missing for Cost center({ wa-costcenter })|.
            APPEND wa_message TO it_message.
          ENDIF.
          CLEAR l_csks.
          CASE sy-sysid.
            WHEN 'DEV'.
              DATA: lv_table TYPE tabname.
              DATA: l_fundcenter TYPE fistl.
              lv_table = 'FMFMOADEV7000031'.

              SELECT SINGLE target1 FROM (lv_table)
                INTO l_fundcenter
                WHERE sour1_from = wa-costcenter.
              IF sy-subrc <> 0.
                CLEAR l_fundcenter.
              ENDIF.

            WHEN 'QAS' OR 'PRD'.
              lv_table = 'FMFMOAPRD5000011'.
              SELECT SINGLE target1 FROM (lv_table)
                INTO l_fundcenter
                WHERE sour1_from = wa-costcenter.
              IF sy-subrc <> 0.
                CLEAR l_fundcenter.
              ENDIF.
            WHEN OTHERS.
          ENDCASE.
          IF l_fundcenter IS INITIAL.
            wa_message-type = 'E'.
            wa_message-doc_no = ''.
            wa_message-message = |01 Fund Center mapping missing for Cost Center({ wa-costcenter })|.
            APPEND wa_message TO it_message.
          ENDIF.
        ENDIF.
        IF l_cost_cnt IS INITIAL OR
           l_cost_cnt = wa-costcenter.

          l_cost_cnt = wa-costcenter.
          wa-doc_no = lv_doc_no.
          lv_count = lv_count + 1.
        ELSEIF l_cost_cnt <> wa-costcenter.
          IF lv_count > p_itmes.
            lv_doc_no = lv_doc_no + 1.
*            l_tot_flag = abap_true.
            lv_count = 0.
          ENDIF.
          wa-doc_no = lv_doc_no.
          lv_count = lv_count + 1.
*          l_cost_cnt = wa-costcenter.
          l_cost_change = abap_true.
        ENDIF.
        wa-itemno_acc = lv_count.
      WHEN 2.
        wa-header_txt = lv_header_text.
        wa-item_text = mtable-value.
      WHEN 3.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = mtable-value
          IMPORTING
            output = wa-gl_account.
      WHEN 4.
        wa-db_amt_doccur = mtable-value.
        IF wa-db_amt_doccur > 0.
          READ TABLE lt_budget INTO DATA(lw_budget) WITH KEY company_code = p_bukrs
                                                             costcenter = wa-costcenter
                                                             gl_account_no = wa-gl_account.
          IF sy-subrc <> 0 .
            wa_message-type = 'E'.
            wa_message-doc_no = ''.
            wa_message-message = |02 Budget not available for FundCenter/GL/Costcenter-({ l_fundcenter }/{ wa-gl_account }/{ wa-costcenter })|.
            APPEND wa_message TO it_message.
          ELSE.
            IF lw_budget-remaining_budget < wa-db_amt_doccur.
              wa_message-type = 'E'.
              wa_message-doc_no = ''.
              wa_message-message = |02 Insufficient Budget for FundCenter/GL/Costcenter-({ l_fundcenter }/{ wa-gl_account }/{ wa-costcenter })|.
              APPEND wa_message TO it_message.
            ENDIF.
          ENDIF.
        ENDIF.
      WHEN 5.
        wa-cr_amt_doccur = mtable-value  * -1.
        wa-compcode = p_bukrs.
        wa-fisc_year = lv_fiscal_yr.
        wa-tax_code = ''.
        wa-fund = lv_fund.
        wa-profit_ctr = ''.
        wa-currency = 'INR'.
        wa-doc_type = 'SA'.
        wa-ref_doc_no = |{ lv_ref_doc_no }/{ lv_doc_no+8(2) }  |.
        wa-doc_date = lv_doc_date.
        wa-pstng_date = lv_pstng_date.
        IF l_cost_change IS INITIAL.
          l_cr_amt_total = l_cr_amt_total + wa-cr_amt_doccur.
          l_dr_amt_total = l_dr_amt_total + wa-db_amt_doccur.
        ENDIF.

*        IF l_tot_flag = abap_true .
*          lv_total_prev = lv_total.
*          lv_total =  wa-cr_amt_doccur + wa-db_amt_doccur.
*        ELSE.
*          lv_total = lv_total + wa-cr_amt_doccur + wa-db_amt_doccur.
*        ENDIF.
*        IF sy-tabix = l_lines.
*          l_doc_flag = abap_true.
*        ENDIF.
      WHEN OTHERS.
    ENDCASE.
*    IF lv_row NE mtable-row.
    AT END OF row.
*      IF l_tot_flag = abap_true.
*        IF lv_total_prev <> '0.00'.
*          wa_message-type = 'E'.
*          wa_message-doc_no = ''.
*          wa_message-message = |Differences in total amount for Document No: { lv_doc_no - 1 }| .
*          APPEND wa_message TO it_message.
*        ENDIF.
*        CLEAR:l_tot_flag, lv_total_prev.
*      ENDIF.
*      IF l_doc_flag = abap_true.
*        IF lv_total <> '0.00'.
*          wa_message-type = 'E'.
*          wa_message-doc_no = ''.
*          SHIFT lv_doc_no LEFT DELETING LEADING '0'.
*          wa_message-message = |Differences in total amount for Document No: { lv_doc_no }| .
*          APPEND wa_message TO it_message.
*        ENDIF.
*      ENDIF.
      IF l_cost_change = abap_true.
        IF ( l_cr_amt_total + l_dr_amt_total ) NE 0.
          wa_message-type = 'E'.
          wa_message-doc_no = ''.
          wa_message-message = |01 CostCenter: { l_cost_cnt } Debit/Credit amount is not matched for | .
          APPEND wa_message TO it_message.
        ENDIF.
        l_cr_amt_total =  wa-cr_amt_doccur.
        l_dr_amt_total =  wa-db_amt_doccur.
        l_cost_cnt = wa-costcenter.
        CLEAR: l_cost_change.
      ELSEIF sy-tabix = l_lines.
        IF ( l_cr_amt_total + l_dr_amt_total ) NE 0.
          wa_message-type = 'E'.
          wa_message-doc_no = ''.
          wa_message-message = |01 CostCenter: { wa-costcenter } Debit/Credit amount is not matched | .
          APPEND wa_message TO it_message.
        ENDIF.
      ENDIF.

      APPEND wa TO itab.
      CLEAR :wa,mtable.
    ENDAT.
  ENDLOOP.
  IF itab[] IS INITIAL.
    DATA(lv_err) = 'No data found'.
    MESSAGE lv_err TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.


  PERFORM f_display_output.

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
* Create the ALV object
  TRY.
      CALL METHOD cl_salv_table=>factory
        IMPORTING
          r_salv_table = lo_gr_alv
        CHANGING
          t_table      = itab.
    CATCH cx_salv_msg.
  ENDTRY.

  lo_gr_alv->set_screen_status(
    pfstatus      =  'SAL_STATUS'
    report        =  sy-repid
    set_functions = lo_gr_alv->c_functions_all ).

  lr_aggregations = lo_gr_alv->get_aggregations( ).
* Let's show all default buttons of ALV
  lo_gr_functions = lo_gr_alv->get_functions( ).
  lo_gr_functions->set_all( value  = if_salv_c_bool_sap=>true ).

  lr_aggregations->clear( ).
  lr_groups = lo_gr_alv->get_sorts( ) .
  lr_groups->clear( ).

  TRY.
      lr_groups->add_sort(
     columnname = 'DOC_NO'
     position   = 1
     subtotal   = abap_true
     sequence   = if_salv_c_sort=>sort_up ).

    CATCH cx_salv_not_found cx_salv_data_error cx_salv_existing.
  ENDTRY.
  TRY.
      lr_groups->add_sort(
     columnname = 'COSTCENTER'
     position   = 2
     subtotal   = abap_true
     sequence   = if_salv_c_sort=>sort_up ).

    CATCH cx_salv_not_found cx_salv_data_error cx_salv_existing.
  ENDTRY.
  TRY.
      lr_aggregations->add_aggregation( columnname = 'DB_AMT_DOCCUR' ).
    CATCH cx_salv_not_found cx_salv_data_error cx_salv_existing.
  ENDTRY.
  TRY.
      lr_aggregations->add_aggregation( columnname = 'CR_AMT_DOCCUR' ).
    CATCH cx_salv_not_found cx_salv_data_error cx_salv_existing.
  ENDTRY.

* Fit the columns
  lo_columns = lo_gr_alv->get_columns( ).
  lo_columns->set_optimize( 'X' ).

* Create header
  DESCRIBE TABLE itab LINES lv_rows.
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
      lo_column ?= lo_columns->get_column( 'COMPCODE' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Company Code' ).
      lo_column->set_medium_text( 'Company Code' ).
      lo_column->set_short_text( 'CCode' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.
  TRY.
      lo_column ?= lo_columns->get_column( 'CR_AMT_DOCCUR' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Credit Amount' ).
      lo_column->set_medium_text( 'Credit Amount' ).
      lo_column->set_short_text( 'Credit' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.
  TRY.
      lo_column ?= lo_columns->get_column( 'DB_AMT_DOCCUR' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Debit Amount' ).
      lo_column->set_medium_text( 'Debit Amount' ).
      lo_column->set_short_text( 'Debit' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.
  TRY.
      lo_column ?= lo_columns->get_column( 'CURRENCY' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Currency' ).
      lo_column->set_medium_text( 'Currency' ).
      lo_column->set_short_text( 'Currency' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.
  TRY.
      lo_column ?= lo_columns->get_column( 'PROFIT_CTR' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Profit Center' ).
      lo_column->set_medium_text( 'Profit Center' ).
      lo_column->set_short_text( 'Pr.Center' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.
  TRY.
      lo_column ?= lo_columns->get_column( 'FUND' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Fund' ).
      lo_column->set_medium_text( 'Fund' ).
      lo_column->set_short_text( 'Fund' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.
  TRY.
      lo_column ?= lo_columns->get_column( 'COSTCENTER' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Cost Center' ).
      lo_column->set_medium_text( 'Cost Center' ).
      lo_column->set_short_text( 'CCenter' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.
  TRY.
      lo_column ?= lo_columns->get_column( 'TAX_CODE' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Tax Code' ).
      lo_column->set_medium_text( 'Tax Code' ).
      lo_column->set_short_text( 'TaxCode' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.
  TRY.
      lo_column ?= lo_columns->get_column( 'FISC_YEAR' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Fiscal Year' ).
      lo_column->set_medium_text( 'Fiscal Year' ).
      lo_column->set_short_text( 'FisYear' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.
  TRY.
      lo_column ?= lo_columns->get_column( 'ITEM_TEXT' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Item Text' ).
      lo_column->set_medium_text( 'Item Text' ).
      lo_column->set_short_text( 'Item' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.
  TRY.
      lo_column ?= lo_columns->get_column( 'DOC_TYPE' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Document Type' ).
      lo_column->set_medium_text( 'Document Type' ).
      lo_column->set_short_text( 'DocType' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.
  TRY.
      lo_column ?= lo_columns->get_column( 'PSTNG_DATE' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Posting Date' ).
      lo_column->set_medium_text( 'Posting Date' ).
      lo_column->set_short_text( 'PostDate' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.
  TRY.
      lo_column ?= lo_columns->get_column( 'DOC_DATE' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Document Date' ).
      lo_column->set_medium_text( 'Document Date' ).
      lo_column->set_short_text( 'DocDate' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.
  TRY.
      lo_column ?= lo_columns->get_column( 'REF_DOC_NO' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Reference' ).
      lo_column->set_medium_text( 'Reference' ).
      lo_column->set_short_text( 'Reference' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.
  TRY.
      lo_column ?= lo_columns->get_column( 'HEADER_TXT' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Header Text' ).
      lo_column->set_medium_text( 'Header Text' ).
      lo_column->set_short_text( 'HText' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.
  TRY.
      lo_column ?= lo_columns->get_column( 'ITEMNO_ACC' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Item No' ).
      lo_column->set_medium_text( 'Item No' ).
      lo_column->set_short_text( 'ItemNo' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.
  TRY.
      lo_column ?= lo_columns->get_column( 'DOC_NO' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Doc NO' ).
      lo_column->set_medium_text( 'Doc NO' ).
      lo_column->set_short_text( 'Doc NO' ).
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
FORM handle_user_command USING i_ucomm TYPE salv_de_function.

  CASE i_ucomm.
    WHEN '&SAL_POST'.
      PERFORM f_post_salary.
    WHEN '&ERR_DISP'.
*      TYPE-POOLS: esp1.
*      DATA: lt_tab TYPE esp1_message_tab_type.
*      DATA: ls_tab TYPE esp1_message_wa_type.
*      SORT return_final BY type.
**      cl_demo_output=>display( return_final ).
*      LOOP AT return_final INTO DATA(lw_ret).
*        ls_tab-lineno = lw_ret-row.
*        ls_tab-msgid  = lw_ret-id.
*        ls_tab-msgno  = lw_ret-number.
*        ls_tab-msgty  = lw_ret-type.
*        ls_tab-msgv1  = lw_ret-message_v1.
*        ls_tab-msgv2  = lw_ret-message.
*        APPEND ls_tab TO lt_tab.
*      ENDLOOP.
*
*      CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
*        TABLES
*          i_message_tab = lt_tab.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_post_salary
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_post_salary .

  IF it_message IS NOT INITIAL.
    CALL FUNCTION 'Z_POPUP_ALV'
      EXPORTING
*       I_START_COLUMN      = 25
*       I_START_LINE        = 6
*       I_END_COLUMN        = 200
*       I_END_LINE          = 20
        i_title             = 'Salary Posting'
        i_status_field_name = 'TYPE'
        i_popup             = 'X'
      TABLES
        it_alv              = it_message.
  ELSE.
    LOOP AT itab ASSIGNING <wa>.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = <wa>-gl_account
        IMPORTING
          output = <wa>-gl_account.
      i_accountgl-itemno_acc = <wa>-itemno_acc.
      i_accountgl-item_text = <wa>-item_text.
      i_accountgl-fisc_year = lv_fiscal_yr.
      i_accountgl-tax_code = <wa>-tax_code.


      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = <wa>-costcenter
        IMPORTING
          output = i_accountgl-costcenter.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = <wa>-profit_ctr
        IMPORTING
          output = i_accountgl-profit_ctr.
      i_accountgl-gl_account = <wa>-gl_account.
      i_accountgl-itemno_tax = <wa>-itemno_acc.

      IF <wa>-cr_amt_doccur < 0. " No CostCenter for Debit Item
        IF <wa>-gl_account(3) <> '004'.
          CLEAR i_accountgl-costcenter.
        ENDIF.
      ELSE.
        i_accountgl-fund       = lv_fund.
        i_accountgl-funds_ctr  = i_accountgl-costcenter.
      ENDIF.
      APPEND i_accountgl.
      CLEAR i_accountgl.

      i_currencyamount-itemno_acc = <wa>-itemno_acc.
      i_currencyamount-currency = 'INR'.
      IF <wa>-cr_amt_doccur < 0.
        i_currencyamount-amt_doccur = <wa>-cr_amt_doccur.
        i_currencyamount-amt_base = <wa>-cr_amt_doccur.
      ELSE.
        i_currencyamount-amt_doccur = <wa>-db_amt_doccur.
        i_currencyamount-amt_base = <wa>-db_amt_doccur.
      ENDIF.
      APPEND i_currencyamount.
      CLEAR i_currencyamount.

      wa_documentheader-header_txt = <wa>-header_txt.
      wa_documentheader-ref_doc_no = <wa>-ref_doc_no.
      wa_documentheader-bus_act = 'RFBU' .
      wa_documentheader-username = sy-uname.
      wa_documentheader-fisc_year = lv_fiscal_yr.
*    wa_documentheader-comp_code = '1000'.        " Commented on 09.05.2018
      wa_documentheader-comp_code = p_bukrs.    " Added on 09.05.2018
      wa_documentheader-doc_date = lv_doc_date.
      wa_documentheader-pstng_date = lv_pstng_date.
      wa_documentheader-doc_type = 'SA'.

      AT END OF doc_no.
        SELECT SINGLE belnr INTO @DATA(l_belnr) FROM bkpf WHERE bukrs = @p_bukrs
                                        AND gjahr = @lv_fiscal_yr
                                        AND xblnr = @wa_documentheader-ref_doc_no.
        IF sy-subrc = 0.
          wa_message-type = 'E'.
          wa_message-doc_no = ''.
          wa_message-message = |Document No-{ l_belnr } is already created for { wa_documentheader-ref_doc_no }|.
          APPEND wa_message TO it_message.
        ELSE.
          CLEAR return[].
          IF ck_test = abap_true.
            CALL FUNCTION 'BAPI_ACC_DOCUMENT_CHECK' "#EC CI_USAGE_OK[2628704]
              "#EC CI_USAGE_OK[2438131]"Added by SPLABAP during code remediation
              EXPORTING
                documentheader = wa_documentheader
              TABLES
                accountgl      = i_accountgl
                currencyamount = i_currencyamount
                return         = return.
            SORT return BY type.
            DELETE return WHERE type = 'E' AND id = 'RW'  AND number = 609.
          ENDIF.
          LOOP AT return WHERE type = 'E'.
            wa_message-type = 'E'.
            wa_message-doc_no = <wa>-doc_no.
            wa_message-message = |{ return-message }{ return-message_v3 }|.
            APPEND wa_message TO it_message.
          ENDLOOP.
          IF sy-subrc <> 0.
            IF ck_test = 'X'.
              wa_message-type = 'S'.
              wa_message-doc_no = <wa>-doc_no.
              wa_message-message = |Document is ready to posted without error|.
              APPEND wa_message TO it_message.
            ELSE.
              CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST' "#EC CI_USAGE_OK[2628704]
                "#EC CI_USAGE_OK[2438131]"Added by SPLABAP during code remediation
                EXPORTING
                  documentheader = wa_documentheader
                IMPORTING
                  obj_type       = wa_documentheader-obj_type
                  obj_key        = wa_documentheader-obj_key
                  obj_sys        = wa_documentheader-obj_sys
                TABLES
                  accountgl      = i_accountgl
                  currencyamount = i_currencyamount
                  return         = return.

              COMMIT WORK AND WAIT.

              READ TABLE return WITH KEY type = 'S'.
              IF sy-subrc = 0.
                wa_message-type = 'S'.
                wa_message-doc_no = wa_documentheader-obj_key(10).
                wa_message-message = |Document is successfully posted - { wa_documentheader-obj_key(10) }|.
                APPEND wa_message TO it_message.
              ENDIF.
            ENDIF.
            CLEAR wa_documentheader.
          ENDIF.
        ENDIF.
        REFRESH: i_accountgl,i_currencyamount.
      ENDAT.
    ENDLOOP.
    CALL FUNCTION 'Z_POPUP_ALV'
      EXPORTING
*       I_START_COLUMN      = 25
*       I_START_LINE        = 6
*       I_END_COLUMN        = 200
*       I_END_LINE          = 20
        i_title             = 'Salary Posting'
        i_status_field_name = 'TYPE'
        i_popup             = 'X'
      TABLES
        it_alv              = it_message.
  ENDIF.

ENDFORM.
