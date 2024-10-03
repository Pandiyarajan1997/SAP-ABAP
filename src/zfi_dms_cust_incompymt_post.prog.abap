*&---------------------------------------------------------------------*
*& Report ZFI_CUSTOMER_INCOMPYMT_POST
*&---------------------------------------------------------------------*
*& Created by: Puratchiveeran
*& Created on: 07.02.2024
*& Purpose: Program to creating incoming payment posting for customers
*& Reference by: Ramakrishnan J
*&---------------------------------------------------------------------*
REPORT zfi_dms_cust_incompymt_post.

DATA: lv_kunnr TYPE zfi_dms_cust_pay-kunnr,
      lv_xblnr TYPE zfi_dms_cust_pay-xblnr.
DATA: lt_seltab TYPE TABLE OF rsparams.
DATA: lv_fisyear TYPE bapi0002_4-fiscal_year,
      lv_month   TYPE bapi0002_4-fiscal_period,
      l_return   TYPE bapireturn1.
DATA: lv_bukrs TYPE bukrs,
      lv_glacc TYPE saknr.
DATA: gr_table TYPE REF TO cl_salv_table.
*** selection screen Design ***
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_kunnr FOR lv_kunnr,
                  s_xblnr FOR lv_xblnr.
SELECTION-SCREEN END OF BLOCK b1.

START-OF-SELECTION.

  CALL FUNCTION 'ZCHECK_ACTIVE_BACKGRD_JOB'
    EXPORTING
      program_name         = sy-cprog
    EXCEPTIONS
      program_name_missing = 1
      excess_job           = 2
      OTHERS               = 3.
  IF sy-subrc <> 0.
    CASE sy-subrc.
      WHEN '1'.
        DATA(l_msg) = CONV string( |Program Name is Missing| ).
      WHEN '2'.
        l_msg = |More than one Background job is running|.
      WHEN OTHERS.
    ENDCASE.
    WRITE : / 'Error:',l_msg.
  ELSE.

    CLEAR lv_bukrs.
    lv_bukrs = 'DMS1'.

*** function module to get fiscal year ***
    CLEAR: lv_fisyear,lv_month,l_return.
    CALL FUNCTION 'BAPI_COMPANYCODE_GET_PERIOD'
      EXPORTING
        companycodeid = lv_bukrs
        posting_date  = sy-datum
      IMPORTING
        fiscal_year   = lv_fisyear
        fiscal_period = lv_month
        return        = l_return.

**** Selecting the payments which or not processed from Log table ***
    SELECT * FROM zfi_dms_cust_pay INTO TABLE @DATA(lt_paymnt_post) WHERE bukrs EQ @lv_bukrs AND kunnr IN @s_kunnr
       AND xblnr IN @s_xblnr AND status EQ '10'.
    IF sy-subrc EQ 0.
      SORT lt_paymnt_post[] BY bukrs kunnr budat process_on process_time.
** Processing One by one Entries through Incoming paymen program ***
      LOOP AT lt_paymnt_post ASSIGNING FIELD-SYMBOL(<lw_paymnt_post>).
*Check for mandatory entries
        IF <lw_paymnt_post>-hkont IS INITIAL.
          <lw_paymnt_post>-status = '80'.
          <lw_paymnt_post>-remarks = |Missing Incoming Bank A/C Details for { <lw_paymnt_post>-distributor }|.
          MODIFY zfi_dms_cust_pay FROM <lw_paymnt_post>.
          CONTINUE.
        ENDIF.

        IF <lw_paymnt_post>-gsber IS INITIAL.
          <lw_paymnt_post>-status = '80'.
          <lw_paymnt_post>-remarks = |Missing Business Area for { <lw_paymnt_post>-distributor }|.
          MODIFY zfi_dms_cust_pay FROM <lw_paymnt_post>.
          CONTINUE.
        ENDIF.

        IF <lw_paymnt_post>-reference_id IS INITIAL.
          <lw_paymnt_post>-status = '80'.
          <lw_paymnt_post>-remarks = |Missing Payment Ref ID { <lw_paymnt_post>-distributor }|.
          MODIFY zfi_dms_cust_pay FROM <lw_paymnt_post>.
          CONTINUE.
        ENDIF.

        IF <lw_paymnt_post>-xblnr IS INITIAL.
          <lw_paymnt_post>-status = '80'.
          <lw_paymnt_post>-remarks = |Missing MIS XBLNR { <lw_paymnt_post>-distributor }|.
          MODIFY zfi_dms_cust_pay FROM <lw_paymnt_post>.
          CONTINUE.
        ENDIF.

*     Duplicate Check 1
        SELECT SINGLE * FROM zfi_dms_cust_pay INTO @DATA(ls_dup_chk) WHERE bukrs = @<lw_paymnt_post>-bukrs
           AND kunnr = @<lw_paymnt_post>-kunnr AND xblnr = @<lw_paymnt_post>-xblnr
           AND reference_id NE @<lw_paymnt_post>-reference_id.
        IF sy-subrc = 0.
*     Duplicate payment
          <lw_paymnt_post>-status = '50'.
          <lw_paymnt_post>-remarks = |Payment Document is Duplicate|.
          MODIFY zfi_dms_cust_pay FROM <lw_paymnt_post>.
          CONTINUE.
        ENDIF.
        "Duplicate Check 2
        SELECT SINGLE * FROM zdms_deb_ref_mis INTO @DATA(l_alternate_chk) WHERE bukrs = @<lw_paymnt_post>-bukrs
           AND account_type = 'D' AND account = @<lw_paymnt_post>-kunnr AND invoice_refno = @<lw_paymnt_post>-xblnr.
        IF sy-subrc NE 0.
          REFRESH lt_seltab.
          lt_seltab = VALUE #(
                           ( selname = 'BLDAT' kind = 'P' sign = 'I' option = 'EQ' low = <lw_paymnt_post>-bldat )
                           ( selname = 'BUDAT' kind = 'P' sign = 'I' option = 'EQ' low = <lw_paymnt_post>-budat )
                           ( selname = 'XBLNR' kind = 'P' sign = 'I' option = 'EQ' low = <lw_paymnt_post>-xblnr )
                           ( selname = 'NEWKO' kind = 'P' sign = 'I' option = 'EQ' low = <lw_paymnt_post>-hkont )
                           ( selname = 'XTEXT' kind = 'P' sign = 'I' option = 'EQ' low = <lw_paymnt_post>-sgtxt )
                           ( selname = 'BUKRS' kind = 'P' sign = 'I' option = 'EQ' low = <lw_paymnt_post>-bukrs )
                           ( selname = 'WAERS' kind = 'P' sign = 'I' option = 'EQ' low = 'INR' )
                           ( selname = 'GJAHR' kind = 'P' sign = 'I' option = 'EQ' low = lv_fisyear )
                           ( selname = 'MONAT' kind = 'P' sign = 'I' option = 'EQ' low = lv_month )
                           ( selname = 'KUNNR' kind = 'P' sign = 'I' option = 'EQ' low = <lw_paymnt_post>-kunnr )
                           ( selname = 'WRBTR' kind = 'P' sign = 'I' option = 'EQ' low = <lw_paymnt_post>-amount )
                           ( selname = 'REFID' kind = 'P' sign = 'I' option = 'EQ' low = <lw_paymnt_post>-reference_id )
                           ( selname = 'DISTB' kind = 'P' sign = 'I' option = 'EQ' low = <lw_paymnt_post>-distributor )
                           ( selname = 'GSBER' kind = 'P' sign = 'I' option = 'EQ' low = <lw_paymnt_post>-gsber )
                           ( selname = 'CHBOX' kind = 'P' sign = 'I' option = 'EQ' low = 'X' ) ).
** Actual Program doing the posting Entries in FB05 ***
          SUBMIT zfi_dms_incoming_payment_new WITH SELECTION-TABLE lt_seltab AND RETURN.
        ELSE.
          <lw_paymnt_post>-status = '50'.
          <lw_paymnt_post>-remarks = |Payment Document is Duplicate|.
          MODIFY zfi_dms_cust_pay FROM <lw_paymnt_post>.
        ENDIF.
      ENDLOOP.
*** After Program submission then again going to log table to check remarks ***
      SELECT * FROM zfi_dms_cust_pay
               INTO TABLE @DATA(lt_paymnt_post1)
               FOR ALL ENTRIES IN @lt_paymnt_post
               WHERE bukrs EQ @lt_paymnt_post-bukrs
               AND kunnr EQ @lt_paymnt_post-kunnr
               AND xblnr EQ @lt_paymnt_post-xblnr.
      IF sy-subrc = 0.
        SORT lt_paymnt_post1[] BY bukrs kunnr.
**** Alv display ***
        PERFORM alv_display.
      ENDIF.
    ELSE.
      MESSAGE 'No table entries with initiated status' TYPE 'S'.
    ENDIF.
  ENDIF.


*&---------------------------------------------------------------------*
*& Form alv_display
*&---------------------------------------------------------------------*
FORM alv_display .
  DATA: lr_columns    TYPE REF TO cl_salv_columns_table,
        lr_column     TYPE REF TO cl_salv_column_table,
        lo_column     TYPE REF TO cl_salv_column,
        lr_selections TYPE REF TO cl_salv_selections,
        l_text        TYPE string.

  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = gr_table
        CHANGING
          t_table      = lt_paymnt_post1 ).
    CATCH cx_salv_msg.                                  "#EC NO_HANDLER
  ENDTRY.

  gr_table->set_screen_status(
    pfstatus      =  'STANDARD_FULLSCREEN'
    report        =  'SAPLKKBL'
    set_functions = gr_table->c_functions_all ).
*Columns Optimize
  lr_columns = gr_table->get_columns( ).
  lr_columns->set_optimize( abap_true ).
*Column hide **
  TRY.
      lr_column ?= lr_columns->get_column( columnname = 'MANDT' ).
    CATCH cx_salv_not_found.
  ENDTRY.
  lr_column->set_visible( value  = if_salv_c_bool_sap=>false ).
*Multiple row selection
  lr_selections = gr_table->get_selections( ).
  lr_selections->set_selection_mode( if_salv_c_selection_mode=>cell ).

  gr_table->display( ).
ENDFORM.
