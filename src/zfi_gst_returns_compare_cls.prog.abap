*&---------------------------------------------------------------------*
*& Include          ZFI_GST_RETURNS_COMPARE_CLS
*&---------------------------------------------------------------------*
CLASS lcl_gst_returns DEFINITION.
  PUBLIC SECTION.
    METHODS: "listbox_values,
      screen_adjustments, f4_helpvalues,
      excel_upload, display_oldvalues, build_alv.
    TYPES: BEGIN OF ty_excel,
             month   TYPE char10,
             state   TYPE char5,
             gstin   TYPE jv_gstno,
             name    TYPE name1,
             formula TYPE text40,
             invno   TYPE xblnr1,
             invtype TYPE char20,
             invdate TYPE bldat,
             invamt  TYPE dmbtr,
             pos     TYPE char30,
             gstrate TYPE rates,
             taxable TYPE dmbtr,
             igst    TYPE dmbtr,
             cgst    TYPE dmbtr,
             sgst    TYPE dmbtr,
             totaltax TYPE dmbtr,
           END OF ty_excel.
*    TYPES: BEGIN OF ty_excel,
*             gstin   TYPE jv_gstno,
*             invno   TYPE xblnr1,
*             invdate TYPE bldat,
*             invtype TYPE char20,
*             invamt  TYPE dmbtr,
*             pos     TYPE char30,
*             gstrate TYPE rates,
*             igst    TYPE dmbtr,
*             cgst    TYPE dmbtr,
*             sgst    TYPE dmbtr,
*           END OF ty_excel.
    TYPES: BEGIN OF ty_alv,
             vendor    TYPE lifnr,
             name      TYPE name1_gp,
             gstin     TYPE jv_gstno,
             invno     TYPE xblnr1,
             invdate   TYPE bldat,
             invtype   TYPE char20,
             invamt    TYPE dmbtr,
             pos       TYPE char30,
             gstrate   TYPE rates,
             igst      TYPE dmbtr,
             cgst      TYPE dmbtr,
             sgst      TYPE dmbtr,
             taxwt     TYPE dmbtr,
             tottax    TYPE dmbtr,
             indicator TYPE char20,
           END OF ty_alv.
    DATA: gt_excel TYPE TABLE OF ty_excel,
          gt_alv   TYPE TABLE OF ty_alv.
    DATA: gt_oldvalues TYPE STANDARD TABLE OF zfi_gst_compare.

ENDCLASS.
CLASS lcl_gst_returns IMPLEMENTATION.
**  METHOD listbox_values.
*    DATA: vrm_id     TYPE vrm_id,
*          vrm_values TYPE vrm_values,
*          vrm_value  LIKE LINE OF vrm_values.
*
*    CLEAR vrm_value.
*    APPEND vrm_value TO vrm_values.
*
*    vrm_value-key = 'E'.
*    vrm_value-text = TEXT-100.
*    APPEND vrm_value TO vrm_values.
*
*    vrm_value-key = 'D'.
*    vrm_value-text = TEXT-101.
*    APPEND vrm_value TO vrm_values.
*
*    vrm_id = 'P_LIST'.
*
*    CALL FUNCTION 'VRM_SET_VALUES'
*      EXPORTING
*        id              = vrm_id
*        values          = vrm_values
*      EXCEPTIONS
*        id_illegavrm_id = 1
*        OTHERS          = 2.
*    IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
*  ENDMETHOD.
  METHOD screen_adjustments.
    IF p_excel EQ abap_true.
      LOOP AT SCREEN.
        IF screen-group1 = 'B1'. "Disables Filename
          screen-input = 1.
          screen-active = '1'.
          screen-invisible = '0'.
        ENDIF.
        IF screen-group1 = 'B2'. "Disabels Document Date
          screen-input = 0.
        ENDIF.
        MODIFY SCREEN.
      ENDLOOP.
    ELSEIF p_disp EQ abap_true OR  p_itc EQ abap_true..
      LOOP AT SCREEN.
        IF screen-group1 = 'B2'.  "Enables Document Date
          screen-input = 1.
          screen-invisible = '0'.
        ENDIF.
        IF screen-group1 = 'B1'. "Disables Filename
          screen-input = 0.
        ENDIF.
        MODIFY SCREEN.
      ENDLOOP.
    ELSE.
      LOOP AT SCREEN.
        IF screen-group1 = 'B2'.  "Enables Document Date
          screen-input = 0.
          screen-active = '1'.
          screen-invisible = '0'.
        ENDIF.
        MODIFY SCREEN.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
  METHOD f4_helpvalues.
    CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
      EXPORTING
        program_name  = syst-repid
        dynpro_number = syst-dynnr
        field_name    = ' '
        static        = ' '
        mask          = ' '
      CHANGING
        file_name     = p_fname.
  ENDMETHOD.
  METHOD excel_upload.
    DATA: lt_type  TYPE truxs_t_text_data.
    DATA: lv_tax_wt TYPE dmbtr,
          lv_tottax TYPE dmbtr.
    REFRESH: gt_excel, gt_alv.
    CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
      EXPORTING
        i_line_header        = 'X'
        i_tab_raw_data       = lt_type
        i_filename           = p_fname
      TABLES
        i_tab_converted_data = gt_excel
      EXCEPTIONS
        conversion_failed    = 1
        OTHERS               = 2.
    IF sy-subrc <> 0.
      MESSAGE 'Conversion of excel failed' TYPE 'E'.
    ENDIF.

    IF gt_excel[] IS NOT INITIAL.
      LOOP AT gt_excel ASSIGNING FIELD-SYMBOL(<fs_excel>).
        TRANSLATE <fs_excel>-invno TO UPPER CASE.
        SELECT SINGLE * FROM lfa1 INTO @DATA(ls_lfa1) WHERE stcd3 = @<fs_excel>-gstin.
        IF sy-subrc EQ 0.

        ENDIF.
        lv_tottax = <fs_excel>-igst + <fs_excel>-cgst + <fs_excel>-sgst.
        lv_tax_wt = <fs_excel>-invamt - lv_tottax.

        DATA(ls_table) = VALUE zfi_gst_compare( mandt   = sy-mandt
                                                vendor  = ls_lfa1-lifnr
                                                gstin   = <fs_excel>-gstin
                                                invno   = <fs_excel>-invno
                                                invdate = <fs_excel>-invdate
                                                invtype = <fs_excel>-invtype
                                                invamt  = <fs_excel>-invamt
                                                pos     = <fs_excel>-pos
                                                taxexmp = lv_tax_wt
                                                gstperc = <fs_excel>-gstrate
                                                igst    = <fs_excel>-igst
                                                cgst    = <fs_excel>-cgst
                                                sgst    = <fs_excel>-sgst
                                                tottax  = lv_tottax
                                                ernam   = sy-uname
                                                erdat   = sy-datum
                                                erzet   = sy-uzeit ).
        MODIFY zfi_gst_compare FROM ls_table.
**** Miro invoices Checking ***********
        SELECT SINGLE * FROM rbkp INTO @DATA(ls_miro)
          WHERE bldat = @<fs_excel>-invdate
          AND bukrs = @p_bukrs
          AND lifnr = @ls_lfa1-lifnr
          AND xblnr = @<fs_excel>-invno.
        IF sy-subrc EQ 0.

        ENDIF.
**** Accounting Document Checking *****
        SELECT SINGLE * FROM bkpf INTO @DATA(ls_accdoc)
          WHERE bukrs = @p_bukrs
          AND xblnr = @<fs_excel>-invno
          AND bldat = @<fs_excel>-invdate.
        IF sy-subrc EQ 0.

        ENDIF.

        IF ls_miro IS INITIAL AND ls_accdoc IS INITIAL.
          DATA(lv_indicator) = 'UNMATCHED'.
        ELSE.
          lv_indicator = 'MATCHED'.
        ENDIF.

        DATA(l_alv) = VALUE ty_alv( ).
        l_alv = CORRESPONDING #( BASE ( l_alv ) <fs_excel> ).
        l_alv-vendor    = ls_lfa1-lifnr.
        l_alv-name      = ls_lfa1-name1.
        l_alv-taxwt     = lv_tax_wt.
        l_alv-tottax    = lv_tottax.
        l_alv-indicator = lv_indicator.
        APPEND l_alv TO gt_alv.
        CLEAR: lv_indicator,ls_miro,ls_accdoc,ls_lfa1.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
  METHOD display_oldvalues.
    REFRESH: gt_oldvalues.
    SELECT * FROM zfi_gst_compare INTO TABLE gt_oldvalues
      WHERE invdate IN s_bldat.
    IF sy-subrc EQ 0.
      REFRESH: gt_alv.
      LOOP AT gt_oldvalues ASSIGNING FIELD-SYMBOL(<fs_oldvalues>).
**** Miro invoices Checking ***********
        SELECT SINGLE * FROM rbkp INTO @DATA(ls_miro)
          WHERE bldat = @<fs_oldvalues>-invdate
          AND bukrs = @p_bukrs
          AND lifnr = @<fs_oldvalues>-vendor
          AND xblnr = @<fs_oldvalues>-invno.
        IF sy-subrc EQ 0.

        ENDIF.
**** Accounting Document Checking *****
        SELECT SINGLE * FROM bkpf INTO @DATA(ls_accdoc)
          WHERE bukrs = @p_bukrs
          AND xblnr = @<fs_oldvalues>-invno
          AND bldat = @<fs_oldvalues>-invdate.
        IF sy-subrc EQ 0.

        ENDIF.

        IF ls_miro IS INITIAL AND ls_accdoc IS INITIAL.
          DATA(lv_indicator) = 'UNMATCHED'.
        ELSE.
          lv_indicator = 'MATCHED'.
        ENDIF.

        DATA(l_alv) = VALUE ty_alv( ).
        l_alv-vendor    = <fs_oldvalues>-vendor.
        SELECT SINGLE name1 FROM lfa1 INTO @DATA(lv_name) WHERE lifnr = @<fs_oldvalues>-vendor.
        IF sy-subrc EQ 0.
          l_alv-name      = lv_name.
        ENDIF.
        l_alv-gstin     = <fs_oldvalues>-gstin.
        l_alv-invno     = <fs_oldvalues>-invno.
        l_alv-invtype   = <fs_oldvalues>-invtype.
        l_alv-invdate   = <fs_oldvalues>-invtype.
        l_alv-invamt    = <fs_oldvalues>-invamt.
        l_alv-gstrate   = <fs_oldvalues>-gstperc.
        l_alv-igst      = <fs_oldvalues>-igst.
        l_alv-cgst      = <fs_oldvalues>-cgst.
        l_alv-sgst      = <fs_oldvalues>-sgst.
        l_alv-taxwt     = <fs_oldvalues>-taxexmp.
        l_alv-tottax    = <fs_oldvalues>-tottax.
        l_alv-indicator = lv_indicator.
        APPEND l_alv TO gt_alv.
        CLEAR: lv_indicator,lv_name,ls_miro,ls_accdoc.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
  METHOD build_alv.
    DATA: lo_gr_alv       TYPE REF TO cl_salv_table, " Variables for ALV properties
          lo_gr_functions TYPE REF TO cl_salv_functions_list.

    DATA: lo_layout TYPE REF TO cl_salv_layout, " Variables for enabling Save button
          lv_key    TYPE salv_s_layout_key.

    DATA: lo_display TYPE REF TO cl_salv_display_settings. " Variable for layout settings

    DATA: lo_selections TYPE REF TO cl_salv_selections, " Variables for selection mode and column properties
          lo_columns    TYPE REF TO cl_salv_columns,
          lo_column     TYPE REF TO cl_salv_column_table.

    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = lo_gr_alv
          CHANGING
            t_table      = gt_alv.
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

    TRY.
        lo_column ?= lo_columns->get_column( 'VENDOR' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Vendor' ).
        lo_column->set_medium_text( 'Vendor' ).
        lo_column->set_short_text( 'Vendor' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.
    TRY.
        lo_column ?= lo_columns->get_column( 'NAME' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'VenName' ).
        lo_column->set_medium_text( 'VenName' ).
        lo_column->set_short_text( 'VenName' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.
    TRY.
        lo_column ?= lo_columns->get_column( 'GSTIN' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'GST Number' ).
        lo_column->set_medium_text( 'GST Number' ).
        lo_column->set_short_text( 'GSTNumber' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.
    TRY.
        lo_column ?= lo_columns->get_column( 'INVNO' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Vendor Invoice' ).
        lo_column->set_medium_text( 'Vendor Invoice' ).
        lo_column->set_short_text( 'VendorInv' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.
    TRY.
        lo_column ?= lo_columns->get_column( 'INVDATE' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Invoice Date' ).
        lo_column->set_medium_text( 'Invoice Date' ).
        lo_column->set_short_text( 'Invdate' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.
    TRY.
        lo_column ?= lo_columns->get_column( 'INVTYPE' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Invoice Type' ).
        lo_column->set_medium_text( 'Invoice Type' ).
        lo_column->set_short_text( 'Invtype' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.
    TRY.
        lo_column ?= lo_columns->get_column( 'INVAMT' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Invoice Amount' ).
        lo_column->set_medium_text( 'Invoice Amount' ).
        lo_column->set_short_text( 'Invamount' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.
    TRY.
        lo_column ?= lo_columns->get_column( 'POS' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Place Of Supply' ).
        lo_column->set_medium_text( 'Place Of Supply' ).
        lo_column->set_short_text( 'Place' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.
    TRY.
        lo_column ?= lo_columns->get_column( 'GSTPERC' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'GST percentage' ).
        lo_column->set_medium_text( 'GST percentage' ).
        lo_column->set_short_text( 'GST Rate' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.
    TRY.
        lo_column ?= lo_columns->get_column( 'IGST' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'IGST Amount' ).
        lo_column->set_medium_text( 'IGST Amount' ).
        lo_column->set_short_text( 'IGST' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.
    TRY.
        lo_column ?= lo_columns->get_column( 'CGST' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'CGST Amount' ).
        lo_column->set_medium_text( 'CGST Amount' ).
        lo_column->set_short_text( 'CGST' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.
    TRY.
        lo_column ?= lo_columns->get_column( 'SGST' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'CGST Amount' ).
        lo_column->set_medium_text( 'CGST Amount' ).
        lo_column->set_short_text( 'CGST' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.
    TRY.
        lo_column ?= lo_columns->get_column( 'TAXWT' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Without Taxamnt' ).
        lo_column->set_medium_text( 'Without Taxamnt' ).
        lo_column->set_short_text( 'W/O Taxamt' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.
    TRY.
        lo_column ?= lo_columns->get_column( 'TOTTAX' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Total Taxamnt' ).
        lo_column->set_medium_text( 'Total Taxamnt' ).
        lo_column->set_short_text( 'Tottax' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.
    TRY.
        lo_column ?= lo_columns->get_column( 'INDICATOR' ).
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Indicator' ).
        lo_column->set_medium_text( 'Indicator' ).
        lo_column->set_short_text( 'Indicator' ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_existing.
      CATCH cx_salv_data_error.
    ENDTRY.
    lo_gr_alv->display( ).
  ENDMETHOD.
ENDCLASS.
