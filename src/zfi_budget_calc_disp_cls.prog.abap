*&---------------------------------------------------------------------*
*& Include          ZFI_BUDGET_CALC_DISP_CLS
*&---------------------------------------------------------------------*
CLASS lcl_budget DEFINITION.

  PUBLIC SECTION.
    METHODS: initialization, budget_calculation, build_alv.
    METHODS: set_sort CHANGING co_alv TYPE REF TO cl_salv_table,
      set_agreegations CHANGING co_alv TYPE REF TO cl_salv_table.

    DATA: lr_month TYPE RANGE OF zmonths,
          lv_month TYPE zmonths.
    DATA: gt_budget TYPE STANDARD TABLE OF zstr_budget.
    DATA: lt_budget TYPE STANDARD TABLE OF zstr_budget.
    DATA: o_alv TYPE REF TO cl_salv_table.
ENDCLASS.
CLASS lcl_budget IMPLEMENTATION.
  METHOD initialization.
    CLEAR: lr_month,lv_month.
    IF p_month IS INITIAL.
      lv_month = '01'.
      DO .
        IF lv_month EQ '12'.
          EXIT.
        ENDIF.
        APPEND VALUE #( sign   = 'I' option = 'EQ' low = lv_month ) TO lr_month.
        lv_month = lv_month + 1.
      ENDDO.
    ELSE.
      APPEND VALUE #( sign   = 'I' option = 'EQ' low = p_month ) TO lr_month.
    ENDIF.
  ENDMETHOD.
  METHOD budget_calculation.

    LOOP AT lr_month ASSIGNING FIELD-SYMBOL(<fs_month>).
      REFRESH: lt_budget.
      CALL FUNCTION 'ZBAPI_BUDGET_CALCULATION_MIS'
        EXPORTING
          company_code = p_bukrs
          fiscal_year  = p_gjahr
          month        = <fs_month>-low
        TABLES
          lt_budget    = lt_budget.
      APPEND LINES OF lt_budget TO gt_budget.
    ENDLOOP.

    IF gt_budget[] IS NOT INITIAL.
      SORT gt_budget[] BY zyear zmonth company_code ASCENDING.
    ENDIF.
  ENDMETHOD.
  METHOD build_alv.

    DATA: lo_selections   TYPE REF TO cl_salv_selections, " Variables for selection mode and column properties
          lo_columns      TYPE REF TO cl_salv_columns,
          lo_column       TYPE REF TO cl_salv_column_table,
          lo_gr_functions TYPE REF TO cl_salv_functions_list.
    DATA: lx_msg TYPE REF TO cx_salv_msg.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = o_alv
          CHANGING
            t_table      = gt_budget ).
      CATCH cx_salv_msg INTO lx_msg.
    ENDTRY.

    lo_gr_functions = o_alv->get_functions( ).
    lo_gr_functions->set_all( value  = if_salv_c_bool_sap=>true ).

* Fit the columns
    lo_columns = o_alv->get_columns( ).
    lo_columns->set_optimize( 'X' ).

    TRY.
        lo_column ?= lo_columns->get_column( 'ZMONTH' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Period' ).
        lo_column->set_medium_text( 'Period' ).
        lo_column->set_short_text( 'Period' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'ASSIGNED_BUDGET' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Assigned Budget' ).
        lo_column->set_medium_text( 'Assigned Budget' ).
        lo_column->set_short_text( 'ABudgt' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'USED_BUGET' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Used budget' ).
        lo_column->set_medium_text( 'Used budget' ).
        lo_column->set_short_text( 'UBudget' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'REMAINING_BUDGET' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Remaining budget' ).
        lo_column->set_medium_text( 'Remaining budget' ).
        lo_column->set_short_text( 'RBudget' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.

*   Set SORT
    CALL METHOD set_sort
      CHANGING
        co_alv = o_alv.
*
*   Set the Aggregations
    CALL METHOD set_agreegations
      CHANGING
        co_alv = o_alv.

    o_alv->display( ).
  ENDMETHOD.
  METHOD set_sort.
    DATA: lo_sort TYPE REF TO cl_salv_sorts.
*
*   get Sort object
    lo_sort = co_alv->get_sorts( ).

*   Set the SORT on the AUART with Subtotal
    TRY.
        CALL METHOD lo_sort->add_sort
          EXPORTING
            columnname = 'GL_ACCOUNT_NO'
            subtotal   = if_salv_c_bool_sap=>true.
      CATCH cx_salv_not_found .                         "#EC NO_HANDLER
      CATCH cx_salv_existing .                          "#EC NO_HANDLER
      CATCH cx_salv_data_error .                        "#EC NO_HANDLER
    ENDTRY.
  ENDMETHOD.
  METHOD set_agreegations.
    DATA: lo_aggrs TYPE REF TO cl_salv_aggregations.
*
    lo_aggrs = co_alv->get_aggregations( ).
*
*   Add TOTAL for COLUMN NETWR
    TRY.
        CALL METHOD lo_aggrs->add_aggregation
          EXPORTING
            columnname  = 'ASSIGNED_BUDGET'
            aggregation = if_salv_c_aggregation=>total.
      CATCH cx_salv_data_error .                        "#EC NO_HANDLER
      CATCH cx_salv_not_found .                         "#EC NO_HANDLER
      CATCH cx_salv_existing .                          "#EC NO_HANDLER
    ENDTRY.
*   Add TOTAL for COLUMN NETWR
    TRY.
        CALL METHOD lo_aggrs->add_aggregation
          EXPORTING
            columnname  = 'USED_BUGET'
            aggregation = if_salv_c_aggregation=>total.
      CATCH cx_salv_data_error .                        "#EC NO_HANDLER
      CATCH cx_salv_not_found .                         "#EC NO_HANDLER
      CATCH cx_salv_existing .                          "#EC NO_HANDLER
    ENDTRY.
*   Add TOTAL for COLUMN NETWR
    TRY.
        CALL METHOD lo_aggrs->add_aggregation
          EXPORTING
            columnname  = 'REMAINING_BUDGET'
            aggregation = if_salv_c_aggregation=>total.
      CATCH cx_salv_data_error .                        "#EC NO_HANDLER
      CATCH cx_salv_not_found .                         "#EC NO_HANDLER
      CATCH cx_salv_existing .                          "#EC NO_HANDLER
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
