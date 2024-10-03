*&---------------------------------------------------------------------*
*& Include          ZPAYMENT_ADJUST_LOG_DISP_FORMS
*&---------------------------------------------------------------------*
*** Layout Initialization ****
FORM get_default_layout CHANGING cv_layout TYPE disvariant-variant.
  DATA:
    ls_variant TYPE disvariant.

  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
    EXPORTING
      i_save        = 'A'
    CHANGING
      cs_variant    = ls_variant
    EXCEPTIONS
      wrong_input   = 1
      not_found     = 2
      program_error = 3
      OTHERS        = 4.
  IF sy-subrc = 0.
    cv_layout = ls_variant-variant.
  ENDIF.
ENDFORM.
FORM select_alv_variant CHANGING cv_layout TYPE disvariant-variant.
  DATA:
    ls_variant TYPE disvariant.

  ls_variant-report = sy-repid.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant    = ls_variant
      i_save        = 'A'
    IMPORTING
      es_variant    = ls_variant
    EXCEPTIONS
      not_found     = 1
      program_error = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    cv_layout = ls_variant-variant.
  ENDIF.
ENDFORM.
*** Selection screen value help ****
FORM f4_valuehelp USING p_name TYPE rsrestrict-objectname.

  DATA: lv_field TYPE help_info-dynprofld.

  IF p_name = 'S_BELNR-LOW' OR p_name = 'S_BELNR-HIGH'.
    SELECT doc_no, doc_type FROM zdebit_ref_mis INTO TABLE @DATA(lt_doc_value).
    IF sy-subrc = 0.
      SORT lt_doc_value[] BY doc_no.
    ENDIF.
    CLEAR lv_field.
    lv_field = p_name.
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'DOC_NO'
        dynpprog        = sy-repid
        dynpnr          = sy-dynnr
        dynprofield     = lv_field
        value_org       = 'S'
      TABLES
        value_tab       = lt_doc_value
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.
    IF sy-subrc EQ 0.
    ENDIF.
  ELSEIF p_name = 'S_DOCTY-LOW' OR p_name = 'S_DOCTY-HIGH'.
    SELECT blart, ltext FROM t003t INTO TABLE @DATA(lt_doctyp) WHERE spras = @sy-langu.
    IF sy-subrc = 0.
      SORT lt_doctyp[] BY blart.
    ENDIF.
    CLEAR lv_field.
    lv_field = p_name.
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'DOC_TYPE'
        dynpprog        = sy-repid
        dynpnr          = sy-dynnr
        dynprofield     = lv_field
        value_org       = 'S'
      TABLES
        value_tab       = lt_doctyp
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.
    IF sy-subrc EQ 0.
    ENDIF.
  ENDIF.
ENDFORM.
FORM f_prepare_data.
  DATA: lt_domain TYPE TABLE OF dd07v.
  REFRESH: lt_domain.
  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      domname         = 'KOART'
    TABLES
      values_tab      = lt_domain
    EXCEPTIONS
      no_values_found = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
  ENDIF.
**** Fetching data based on input ***
  SELECT * FROM zdebit_ref_mis
           INTO TABLE @DATA(lt_log_disp)
           WHERE bukrs IN @s_bukrs
           AND doc_no IN @s_belnr
           AND gjahr IN @s_year
           AND account_type IN @s_actyp
           AND doc_type IN @s_docty
           AND account IN @s_acc
  AND erdat IN @s_date.
  IF sy-subrc = 0.
    SORT lt_log_disp[] BY bukrs gjahr doc_no account.
    SELECT lifnr, name1 FROM lfa1 INTO TABLE @DATA(lt_lfa1).
    IF sy-subrc = 0.
      SORT lt_lfa1[] BY lifnr.
    ENDIF.
    SELECT kunnr, name1 FROM kna1 INTO TABLE @DATA(lt_kna1).
    IF sy-subrc = 0.
      SORT lt_kna1[] BY kunnr.
    ENDIF.
    SELECT saknr, txt50 FROM skat INTO TABLE @DATA(lt_skat) WHERE ktopl = 'YAIN'.
    IF sy-subrc = 0.
      SORT lt_skat[] BY saknr.
    ENDIF.
  ENDIF.
*** Final Display preparation ***
  REFRESH gt_final.
  LOOP AT lt_log_disp ASSIGNING FIELD-SYMBOL(<fls_log_disp>).
    CLEAR gs_final.
    MOVE-CORRESPONDING <fls_log_disp> TO gs_final.
    IF gs_final-account_type = 'K'.
      gs_final-venname = VALUE #( lt_lfa1[ lifnr = gs_final-account ]-name1 OPTIONAL ).
    ELSEIF gs_final-account_type = 'D'.
      gs_final-cusname = VALUE #( lt_kna1[ kunnr = gs_final-account ]-name1 OPTIONAL ).
    ELSEIF gs_final-account_type = 'S'.
      gs_final-gldesc = VALUE #( lt_skat[ saknr = gs_final-account ]-txt50 OPTIONAL ).
    ENDIF.
    gs_final-acctyp_des = VALUE #( lt_domain[ domvalue_l = gs_final-account_type ]-ddtext OPTIONAL ).
    APPEND gs_final TO gt_final.
  ENDLOOP.
ENDFORM.
FORM f_alv_display.
  DATA: lo_gr_alv       TYPE REF TO cl_salv_table, " Variables for ALV properties
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

* create the alv object
  TRY.
      CALL METHOD cl_salv_table=>factory
        IMPORTING
          r_salv_table = lo_gr_alv
        CHANGING
          t_table      = gt_final.
    CATCH cx_salv_msg.
  ENDTRY.
* Let's show all default buttons of ALV
  lo_gr_functions = lo_gr_alv->get_functions( ).
  lo_gr_functions->set_all( abap_true ).

* Fit the columns
  lo_columns = lo_gr_alv->get_columns( ).
  lo_columns->set_optimize( 'X' ).

* Create header
  DESCRIBE TABLE gt_final LINES lv_rows.
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


* Enable cell selection mode
  lo_selections = lo_gr_alv->get_selections( ).
  lo_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).

  TRY.
      lo_column ?= lo_columns->get_column( 'MANDT' ).
      lo_column->set_visible( if_salv_c_bool_sap=>false ).
      lo_column->set_long_text( 'Vendor/Customer' ).
      lo_column->set_medium_text( 'Vendor/Customer' ).
      lo_column->set_short_text( 'Vend/Cust'  ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'ACCOUNT' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Vendor/Customer' ).
      lo_column->set_medium_text( 'Vendor/Customer' ).
      lo_column->set_short_text( 'Vend/Cust'  ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'CUSNAME' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Customer Name' ).
      lo_column->set_medium_text( 'Customer Name' ).
      lo_column->set_short_text( 'Custname'  ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'VENNAME' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Vendor Name' ).
      lo_column->set_medium_text( 'Vendor Name' ).
      lo_column->set_short_text( 'venname'  ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'ACCTYP_DES' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Acc typ Desc' ).
      lo_column->set_medium_text( 'Acc typ Desc' ).
      lo_column->set_short_text( 'Atyp_des'  ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

    TRY.
      lo_column ?= lo_columns->get_column( 'AMOUNT' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Gross Amount' ).
      lo_column->set_medium_text( 'Gross Amount' ).
      lo_column->set_short_text( 'Gross'  ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

    TRY.
      lo_column ?= lo_columns->get_column( 'SGST' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'SGST Amount' ).
      lo_column->set_medium_text( 'SGST Amount' ).
      lo_column->set_short_text( 'SGST'  ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

    TRY.
      lo_column ?= lo_columns->get_column( 'CGST' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'CGST Amount' ).
      lo_column->set_medium_text( 'CGST Amount' ).
      lo_column->set_short_text( 'CGST'  ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

    TRY.
      lo_column ?= lo_columns->get_column( 'IGST' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'IGST Amount' ).
      lo_column->set_medium_text( 'IGST Amount' ).
      lo_column->set_short_text( 'IGST'  ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  lo_gr_alv->get_columns( )->set_column_position( columnname = 'ACCTP_DES' position = 1 ).
  lo_gr_alv->get_columns( )->set_column_position( columnname = 'ACCOUNT' position = 2 ).
  lo_gr_alv->display( ).
ENDFORM.
