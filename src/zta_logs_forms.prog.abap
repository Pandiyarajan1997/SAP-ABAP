*&---------------------------------------------------------------------*
*& Include          ZTA_LOGS_FORMS
*&---------------------------------------------------------------------*
CLASS lcl_logs DEFINITION.
  PUBLIC SECTION.
    METHODS: fetch_data, build_alv.
    METHODS: on_double_click FOR EVENT double_click OF cl_salv_events_table
      IMPORTING row column. "Event for Double click on alv display
ENDCLASS.
CLASS lcl_logs IMPLEMENTATION.
  METHOD fetch_data.
    REFRESH: gt_logs_disp.

    SELECT a~employee_no,
           b~sname AS empname,
           a~emp_vendor,
           c~name1 AS venname,
           a~xblnr AS reference,
           a~amount,
           a~bldat,
           a~budat,
           a~overall_kms,
           a~bukrs,
           a~gjahr,
           a~belnr,
           a~erdat,
           a~er_time,
           a~invtype,
           CASE
           	WHEN invtype = 'T' THEN 'TRAVEL'
            WHEN invtype = 'I' THEN 'INCENTIVE'
             END AS inv_cat,
          a~kostl,
          a~saknr,
          a~type,
          a~msg
            INTO CORRESPONDING FIELDS OF TABLE @gt_logs_disp
            FROM zfi_travel_log AS a
            INNER JOIN pa0001 AS b
            ON a~employee_no = b~pernr
            INNER JOIN lfa1 AS c
            ON a~emp_vendor = c~lifnr
            WHERE a~employee_no IN @s_pernr
            AND a~emp_vendor IN @s_lifnr
            AND a~xblnr IN @s_ref
            AND a~bldat IN @s_bldat
            AND a~budat IN @s_budat
            AND a~gjahr IN @s_gjahr
            AND a~bukrs IN @s_bukrs
            AND a~belnr IN @s_belnr
            AND a~erdat IN @s_erdat
            AND a~invtype IN @s_type
            AND a~type IN @s_etyp.
    IF sy-subrc EQ 0.
      SORT gt_logs_disp[] BY employee_no.
    ENDIF.
  ENDMETHOD.
  METHOD build_alv.
    DATA: lo_gr_alv       TYPE REF TO cl_salv_table, " Variables for ALV properties
          lo_gr_functions TYPE REF TO cl_salv_functions_list.

    DATA: lo_grid          TYPE REF TO cl_salv_form_layout_grid, " Variables for header
          lo_layout_logo   TYPE REF TO cl_salv_form_layout_logo,
          lo_display       TYPE REF TO cl_salv_display_settings,
          lo_columns       TYPE REF TO cl_salv_columns,
          lo_column        TYPE REF TO cl_salv_column_table,
          lo_event_handler TYPE REF TO lcl_logs, " Variables for events
          lo_events        TYPE REF TO cl_salv_events_table.

* create the alv object
    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = lo_gr_alv
          CHANGING
            t_table      = gt_logs_disp.
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

* Register events
    lo_events = lo_gr_alv->get_event( ).
    CREATE OBJECT lo_event_handler.
    SET HANDLER lo_event_handler->on_double_click FOR lo_events.

    TRY.
        lo_column ?= lo_columns->get_column( 'EMPLOYEE_NO' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Employeeno' ).
        lo_column->set_medium_text( 'Employeeno' ).
        lo_column->set_short_text( 'Employeeno' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'EMPNAME' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Empname' ).
        lo_column->set_medium_text( 'Empname' ).
        lo_column->set_short_text( 'Empname' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'EMP_VENDOR' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Employee vendor' ).
        lo_column->set_medium_text( 'Empvendor' ).
        lo_column->set_short_text( 'Empvendor' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'VENNAME' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Venname' ).
        lo_column->set_medium_text( 'Venname' ).
        lo_column->set_short_text( 'Venname' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'XBLNR' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Reference' ).
        lo_column->set_medium_text( 'Reference' ).
        lo_column->set_short_text( 'Reference' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'AMOUNT' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Amount' ).
        lo_column->set_medium_text( 'Amount' ).
        lo_column->set_short_text( 'Amount' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'BLDAT' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Document Date' ).
        lo_column->set_medium_text( 'Document Date' ).
        lo_column->set_short_text( 'Doc Date' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'BUDAT' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Posting Date' ).
        lo_column->set_medium_text( 'Posting Date' ).
        lo_column->set_short_text( 'Post Date' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'OVERALL_KMS' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Overall Kms' ).
        lo_column->set_medium_text( 'OverallKms' ).
        lo_column->set_short_text( 'OverallKms' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'BUKRS' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Company Code' ).
        lo_column->set_medium_text( 'Company Code' ).
        lo_column->set_short_text( 'Comp Code' ).
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
        lo_column ?= lo_columns->get_column( 'BELNR' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Doc Number' ).
        lo_column->set_medium_text( 'Doc Number' ).
        lo_column->set_short_text( 'Doc Number' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'ERDAT' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Created date' ).
        lo_column->set_medium_text( 'Created Date' ).
        lo_column->set_short_text( 'Create_on' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'ER_TIME' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Entry Time' ).
        lo_column->set_medium_text( 'Entry Time' ).
        lo_column->set_short_text( 'Entry Time' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'INVTYPE' ).
        lo_column->set_visible( if_salv_c_bool_sap=>false ).
        lo_column->set_long_text( 'Identify' ).
        lo_column->set_medium_text( 'Identify' ).
        lo_column->set_short_text( 'Identify' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'INV_CAT' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Category' ).
        lo_column->set_medium_text( 'Category' ).
        lo_column->set_short_text( 'Category' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'KOSTL' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Costcenter' ).
        lo_column->set_medium_text( 'Costcenter' ).
        lo_column->set_short_text( 'Costctr' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'SAKNR' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'GL Account' ).
        lo_column->set_medium_text( 'GL Account' ).
        lo_column->set_short_text( 'GL Account' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'TYPE' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Type' ).
        lo_column->set_medium_text( 'Type' ).
        lo_column->set_short_text( 'Type' ).
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
  METHOD on_double_click.
    IF column EQ 'BELNR'.
      READ TABLE gt_logs_disp INTO DATA(wa_disp) INDEX  row.
      IF sy-subrc EQ 0.
        SET PARAMETER ID 'BLN' FIELD wa_disp-belnr. "Document Number

        SET PARAMETER ID 'BUK' FIELD wa_disp-bukrs. "Company Code

        SET PARAMETER ID 'GJR' FIELD wa_disp-gjahr. "Fiscal Year

        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
