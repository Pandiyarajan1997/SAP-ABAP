*&---------------------------------------------------------------------*
*& Include          ZSD_POSVENDOR_PF_UPFORMS
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form f_get_f4_help
*&---------------------------------------------------------------------*
FORM f_get_f4_help  USING    p_p_fname TYPE rlgrap-filename .
  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    EXPORTING
      program_name  = syst-repid
      dynpro_number = syst-dynnr
      field_name    = ' '
      static        = ' '
      mask          = ' '
    CHANGING
      file_name     = p_p_fname.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_convert_excel_data
*&---------------------------------------------------------------------*
FORM f_convert_excel_data  USING    p_p_fname TYPE rlgrap-filename.
  IF p_p_fname IS NOT INITIAL.
    REFRESH: gt_raw,gt_excel.
    CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
      EXPORTING
        i_line_header        = 'X'
        i_tab_raw_data       = gt_raw
        i_filename           = p_p_fname
      TABLES
        i_tab_converted_data = gt_excel
      EXCEPTIONS
        conversion_failed    = 1
        OTHERS               = 2.
    IF sy-subrc <> 0.
      MESSAGE 'Error in excel conversion' TYPE 'E'.
    ELSE.
      SORT gt_excel[] BY kunnr vkorg vtweg spart.
    ENDIF.
  ELSE.
    MESSAGE 'File is Mandatory' TYPE 'E'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_validate_excel_data
*&---------------------------------------------------------------------*
FORM f_validate_process_data .
  DATA: return TYPE bapiret1.
  DATA: lt_roles  TYPE TABLE OF bapibus1006_roles,
        lt_return TYPE TABLE OF bapiret2.
  REFRESH gt_alv.
  LOOP AT gt_excel ASSIGNING FIELD-SYMBOL(<fs_excel>).

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = <fs_excel>-kunnr
      IMPORTING
        output = <fs_excel>-kunnr.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = <fs_excel>-lifnr
      IMPORTING
        output = <fs_excel>-lifnr.
*--- Vendor group checking ------------------------------*
    SELECT SINGLE * FROM but000 INTO @DATA(ls_but000) WHERE partner = @<fs_excel>-lifnr.
    IF sy-subrc = 0.
      IF ls_but000-bu_group NE 'YEVP'.
        APPEND VALUE #( kunnr = <fs_excel>-kunnr
                        vkorg = <fs_excel>-vkorg
                        vtweg = <fs_excel>-vtweg
                        spart = <fs_excel>-spart
                        parvw = <fs_excel>-parvw
                        lifnr = <fs_excel>-lifnr
                        type = 'E'
                        message = |Vendor { <fs_excel>-lifnr } is not a position vendor | ) TO gt_alv.
        CONTINUE.
      ENDIF.
    ENDIF.
*** Vendor Existense check and roles check *-------------*
    CLEAR return.
    CALL FUNCTION 'BAPI_VENDOR_EXISTENCECHECK'
      EXPORTING
        vendorno = <fs_excel>-lifnr
      IMPORTING
        return   = return.
    IF return-type = 'E'.
      APPEND VALUE #( kunnr = <fs_excel>-kunnr
                      vkorg = <fs_excel>-vkorg
                      vtweg = <fs_excel>-vtweg
                      spart = <fs_excel>-spart
                      parvw = <fs_excel>-parvw
                      lifnr = <fs_excel>-lifnr
                      type = return-type
                      message = return-message ) TO gt_alv.
      CONTINUE.
    ELSE.
      CALL FUNCTION 'BAPI_BUPA_ROLES_GET'
        EXPORTING
          businesspartner      = <fs_excel>-lifnr
        TABLES
          businesspartnerroles = lt_roles
          return               = lt_return.
      IF lt_return IS NOT INITIAL.
        READ TABLE lt_return INTO DATA(ls_ret) WITH KEY type = 'E'.
        IF sy-subrc = 0.
          LOOP AT lt_return INTO DATA(ls_msg) WHERE type = 'E'.
            DATA(lv_msg) = | { ls_ret-message } |.
            lv_msg = | { lv_msg } { ls_ret-message } |.
          ENDLOOP.
          APPEND VALUE #( kunnr = <fs_excel>-kunnr
                          vkorg = <fs_excel>-vkorg
                          vtweg = <fs_excel>-vtweg
                          spart = <fs_excel>-spart
                          parvw = <fs_excel>-parvw
                          lifnr = <fs_excel>-lifnr
                          type = 'E'
                          message = lv_msg ) TO gt_alv.
          CONTINUE.
        ENDIF.
      ELSE.
        DATA(lv_role) = VALUE #( lt_roles[ partnerrole = 'ZPVBPV' ]-partnerrole OPTIONAL ).
        IF lv_role IS INITIAL.
          APPEND VALUE #( kunnr = <fs_excel>-kunnr
                          vkorg = <fs_excel>-vkorg
                          vtweg = <fs_excel>-vtweg
                          spart = <fs_excel>-spart
                          parvw = <fs_excel>-parvw
                          lifnr = <fs_excel>-lifnr
                          type = 'E'
                          message = |Roles 'YEVP' is not extended for position vendor { <fs_excel>-lifnr } | ) TO gt_alv.
          CONTINUE.
        ENDIF.
      ENDIF.
    ENDIF.
*** Customer and sales area Existense check  *-------------*
    SELECT SINGLE bukrs FROM tvko INTO @DATA(lv_bukrs) WHERE vkorg = @<fs_excel>-vkorg.
    IF sy-subrc = 0.
      CLEAR return.
      CALL FUNCTION 'BAPI_CUSTOMER_EXISTENCECHECK'
        EXPORTING
          customerno           = <fs_excel>-kunnr
          companycode          = lv_bukrs
          sales_organization   = <fs_excel>-vkorg
          distribution_channel = <fs_excel>-vtweg
          division             = <fs_excel>-spart
        IMPORTING
          return               = return.
      IF return-type = 'E'.
        APPEND VALUE #( kunnr = <fs_excel>-kunnr
                        vkorg = <fs_excel>-vkorg
                        vtweg = <fs_excel>-vtweg
                        spart = <fs_excel>-spart
                        parvw = <fs_excel>-parvw
                        lifnr = <fs_excel>-lifnr
                        type = return-type
                        message = return-message ) TO gt_alv.
        CONTINUE.
      ENDIF.
    ELSE.
      APPEND VALUE #( kunnr = <fs_excel>-kunnr
                      vkorg = <fs_excel>-vkorg
                      vtweg = <fs_excel>-vtweg
                      spart = <fs_excel>-spart
                      parvw = <fs_excel>-parvw
                      lifnr = <fs_excel>-lifnr
                      type = 'E'
                      message = |No Sales Organisation available in SAP { <fs_excel>-vkorg  }| ) TO gt_alv.
      CONTINUE.
    ENDIF.
*----Partner function existence check -----------------------*
    SELECT SINGLE parvw FROM tpar INTO @DATA(lv_pf) WHERE parvw = @<fs_excel>-parvw.
    IF sy-subrc NE 0.
      APPEND VALUE #( kunnr = <fs_excel>-kunnr
                      vkorg = <fs_excel>-vkorg
                      vtweg = <fs_excel>-vtweg
                      spart = <fs_excel>-spart
                      parvw = <fs_excel>-parvw
                      lifnr = <fs_excel>-lifnr
                      type = 'E'
                      message = |Invalid Partner Function { <fs_excel>-parvw  }| ) TO gt_alv.
      CONTINUE.
    ENDIF.
*--- New Partner function Mapping checks ----------------------------*
    SELECT SINGLE new_pf FROM zsd_pf_old_new INTO @DATA(ls_pf_tab) WHERE new_pf = @<fs_excel>-parvw.
    IF sy-subrc NE 0.
      APPEND VALUE #( kunnr = <fs_excel>-kunnr
                      vkorg = <fs_excel>-vkorg
                      vtweg = <fs_excel>-vtweg
                      spart = <fs_excel>-spart
                      parvw = <fs_excel>-parvw
                      lifnr = <fs_excel>-lifnr
                      type = 'E'
                      message = |This Partner Function { <fs_excel>-parvw  } is not allowed for Mapping| ) TO gt_alv.
      CONTINUE.
    ENDIF.
*------ Actual Process for partner function mapping -----------------*
    PERFORM f_process_update_data USING <fs_excel>.
  ENDLOOP.
*----------------------------------------------------*
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_process_excel_data
*------------------------------------------------------------------------*
FORM f_process_update_data USING p_input TYPE excel.
  DATA is_master_data           TYPE cmds_ei_main. " Ext. Interface: Total Customer Data
  DATA es_master_data_correct   TYPE cmds_ei_main. " Ext. Interface: Total Customer Data
  DATA es_message_correct       TYPE cvis_message. " Error Indicator and System Messages
  DATA es_master_data_defective TYPE cmds_ei_main. " Ext. Interface: Total Customer Data
  DATA es_message_defective     TYPE cvis_message. " Error Indicator and System Messages

  DATA: gs_alv TYPE ty_alv.
  DATA: lt_customers    TYPE cmds_ei_extern_t,
        ls_customers    TYPE cmds_ei_extern,         " Complex External Interface for Customers
        ls_address      TYPE cvis_ei_address1,       " Ext. Interface: Address of Organization
        ls_sales_data   TYPE cmds_ei_cmd_sales,      " Ext. Interface: Sales Data
        ls_central_data TYPE cmds_ei_central_data,   " External Interface: Central Data
        lt_sales        TYPE cmds_ei_sales_t,
        ls_sales        TYPE cmds_ei_sales,          " Ext. Interface: Sales Data
        lt_sales_func   TYPE cmds_ei_functions_t,
        ls_sales_func   TYPE cmds_ei_functions,      " Ext. Interface: Partner Roles
        ls_tax_data     TYPE cmds_ei_tax_ind,        " Ext. Interface: Tax Indicators
        lt_tax_data     TYPE cmds_ei_tax_ind_t,
        ls_message      TYPE cvis_message,           " Error Indicator and System Messages
        lv_contactid    TYPE bapicontact_01-contact. " Number of contact person
  DATA: lv_parza TYPE parza.

  REFRESH: lt_sales_func,lt_sales,lt_customers.
  CLEAR: is_master_data,ls_customers,ls_sales-data_key,ls_sales_func.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_input-kunnr
    IMPORTING
      output = p_input-kunnr.
  CLEAR: ls_customers,lv_parza.
  ls_customers-header-object_instance-kunnr = p_input-kunnr.
  SELECT kunnr, vkorg, vtweg, spart, parvw, parza FROM knvp
                                                  INTO TABLE @DATA(gt_knvp)
                                                  WHERE kunnr = @p_input-kunnr
                                                  AND  vkorg = @p_input-vkorg
                                                  AND  vtweg = @p_input-vtweg
                                                  AND spart = @p_input-spart
                                                 AND parvw = @p_input-parvw.
  IF sy-subrc = 0.
    ls_sales_func-task = 'U'.
  ELSE.
    ls_sales_func-task = 'I'.
  ENDIF.
  ls_customers-header-object_task = 'U'.
  ls_sales-task = 'U'.

  ls_sales-data_key-vkorg = p_input-vkorg.
  ls_sales-data_key-vtweg = p_input-vtweg.
  ls_sales-data_key-spart = p_input-spart.

  ls_sales_func-data_key-parvw = p_input-parvw.
  ls_sales_func-data_key-parza = lv_parza.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_input-lifnr
    IMPORTING
      output = p_input-lifnr.
  ls_sales_func-data-partner   = p_input-lifnr.
  ls_sales_func-datax-partner  = 'X'.
  APPEND ls_sales_func TO lt_sales_func.
  CLEAR ls_sales_func.

  ls_sales-functions-functions = lt_sales_func[].
  APPEND ls_sales TO lt_sales.

*   Set the Sales Data
  ls_sales_data-sales = lt_sales[].

  ls_customers-sales_data = ls_sales_data.
  APPEND ls_customers TO lt_customers.
  is_master_data-customers = lt_customers[].
  CLEAR es_message_defective.
  cmd_ei_api=>initialize( ).
  CALL METHOD cmd_ei_api=>maintain_bapi
    EXPORTING
      iv_test_run              = p_run
*     IV_COLLECT_MESSAGES      = SPACE
      is_master_data           = is_master_data
    IMPORTING
      es_master_data_correct   = es_master_data_correct
      es_message_correct       = es_message_correct
      es_master_data_defective = es_master_data_defective
      es_message_defective     = es_message_defective.
  IF es_message_defective-is_error IS INITIAL..
    IF p_run = abap_true.
*** successfully CHECK executed means ***
      APPEND VALUE #( kunnr = p_input-kunnr
                      vkorg = p_input-vkorg
                      vtweg = p_input-vtweg
                      spart = p_input-spart
                      parvw = p_input-parvw
                      lifnr = p_input-lifnr
                      type = 'S'
                      message = |No Errors in lineitems| ) TO gt_alv.
    ELSE.
      COMMIT WORK.
      WAIT UP TO 1 SECONDS.
      READ TABLE gt_excel ASSIGNING FIELD-SYMBOL(<fs_upd_data>) WITH KEY kunnr = p_input-kunnr
                                                                         vkorg = p_input-vkorg
                                                                         vtweg = p_input-vtweg
                                                                         spart = p_input-spart
                                                                         parvw = p_input-parvw
                                                                         lifnr = p_input-lifnr.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING <fs_upd_data> TO gs_alv.
        gs_alv-type = 'S'.
        gs_alv-message = |Partner Function updated Successfully for { <fs_upd_data>-kunnr } |.
        APPEND gs_alv TO gt_alv.
        CLEAR gs_alv.
      ENDIF.
    ENDIF.
  ELSE.
    LOOP AT es_message_defective-messages INTO DATA(lw_msg) WHERE type = 'E'.
      DATA(lv_error) = lw_msg-message.
      lv_error = | { lv_error } { lw_msg-message }|.
    ENDLOOP.
    READ TABLE gt_excel ASSIGNING FIELD-SYMBOL(<fs_upd_data1>) WITH KEY kunnr = p_input-kunnr
                                                                        vkorg = p_input-vkorg
                                                                        vtweg = p_input-vtweg
                                                                        spart = p_input-spart
                                                                        parvw = p_input-parvw
                                                                        lifnr = p_input-lifnr.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING  <fs_upd_data1> TO gs_alv.
      gs_alv-type = 'E'.
      gs_alv-message = |{ lv_error } { <fs_upd_data1>-kunnr } |.
      APPEND gs_alv TO gt_alv.
      CLEAR gs_alv.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_display_alv
*&---------------------------------------------------------------------*
FORM f_display_alv .
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

  IF gt_alv IS NOT INITIAL.
* Create the ALV object
    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = lo_gr_alv
          CHANGING
            t_table      = gt_alv.
      CATCH cx_salv_msg.
    ENDTRY.
  ENDIF.

  lo_gr_functions = lo_gr_alv->get_functions( ).
  lo_gr_functions->set_all( abap_true ).

* Fit the columns
  lo_columns = lo_gr_alv->get_columns( ).
  lo_columns->set_optimize( 'X' ).
* Apply zebra style to lv_rows
  lo_display = lo_gr_alv->get_display_settings( ).
  lo_display->set_striped_pattern( cl_salv_display_settings=>true ).
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
