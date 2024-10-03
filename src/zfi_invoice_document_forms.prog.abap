*&---------------------------------------------------------------------*
*& Include          ZFI_INVOICE_DOCUMENT_FORMS
*&---------------------------------------------------------------------*
CLASS lcl_handle_events DEFINITION. " Variables for events
  PUBLIC SECTION.

    METHODS:
      on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function. "Event for User command

    METHODS: on_double_click FOR EVENT double_click OF cl_salv_events_table
      IMPORTING row column. "Event for Double click on alv display


ENDCLASS.
CLASS lcl_handle_events IMPLEMENTATION.

  METHOD on_user_command.
    PERFORM handle_user_command USING e_salv_function.
  ENDMETHOD.

  METHOD on_double_click.
    IF column EQ 'DOCNO'.
      READ TABLE gt_final INTO DATA(wa_final) INDEX  row.
      SELECT SINGLE * FROM bkpf INTO @DATA(ls_bkpf) WHERE bukrs EQ @wa_final-bukrs
                                                  AND belnr EQ @wa_final-docno
                                                  AND gjahr EQ @wa_final-gjahr.
      IF sy-subrc = 0. " Exists?
        SET PARAMETER ID 'BLN' FIELD wa_final-docno. "Document Number

        SET PARAMETER ID 'BUK' FIELD wa_final-bukrs. "Company Code

        SET PARAMETER ID 'GJR' FIELD wa_final-gjahr. "Fiscal Year

        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

FORM f_excel_download .
  PERFORM f_excel_instantiate.
  lv_filename1 = | { 'Vendor Invoice Posting' } |.
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

  GET REFERENCE OF gt_excel INTO lr_excel_structure.
  DATA(lo_itab_services) = cl_salv_itab_services=>create_for_table_ref( lr_excel_structure ).
  lo_source_table_descr ?= cl_abap_tabledescr=>describe_by_data_ref( lr_excel_structure ).
  lo_table_row_descripter ?= lo_source_table_descr->get_table_line_type( ).

  DATA(lo_tool_xls) = cl_salv_export_tool_ats_xls=>create_for_excel(
                               EXPORTING r_data = lr_excel_structure ).

  DATA(lo_config) = lo_tool_xls->configuration( ).
  lo_config->add_column(
         EXPORTING
           header_text = 'Vendor Number'
           field_name = 'VENDOR'
           display_type = if_salv_bs_model_column=>uie_text_view ).

  lo_config->add_column(
        EXPORTING
          header_text = 'Invoice Date'
          field_name = 'INV_DATE'
          display_type = if_salv_bs_model_column=>uie_text_view ).

  lo_config->add_column(
        EXPORTING
          header_text = 'Reference Document'
          field_name = 'REFDOC'
          display_type = if_salv_bs_model_column=>uie_text_view ).

  lo_config->add_column(
      EXPORTING
        header_text = 'Posting Date'
        field_name = 'BUDAT'
        display_type = if_salv_bs_model_column=>uie_text_view ).

  lo_config->add_column(
        EXPORTING
          header_text = 'Amount'
          field_name = 'AMOUNT'
          display_type = if_salv_bs_model_column=>uie_text_view ).

  lo_config->add_column(
      EXPORTING
        header_text = 'Taxcode'
        field_name = 'TAXCODE'
        display_type = if_salv_bs_model_column=>uie_text_view ).

  lo_config->add_column(
      EXPORTING
        header_text = 'Business Place'
        field_name = 'BUPLA'
        display_type = if_salv_bs_model_column=>uie_text_view ).

  lo_config->add_column(
        EXPORTING
          header_text = 'Item text'
          field_name = 'TEXT'
          display_type = if_salv_bs_model_column=>uie_text_view ).

  lo_config->add_column(
      EXPORTING
        header_text = 'G/L Account'
        field_name = 'GLACC'
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

  lo_config->add_column(
      EXPORTING
        header_text = 'Tds calc'
        field_name = 'TDS'
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
*& Form f4_help_filename
*&---------------------------------------------------------------------*
FORM f4_help_filename USING fname TYPE localfile.
  fname = p_fname.
  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    EXPORTING
      program_name  = syst-repid
      dynpro_number = syst-dynnr
      field_name    = ' '
      static        = ' '
      mask          = ' '
    CHANGING
      file_name     = fname.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_excel_conversion
*&---------------------------------------------------------------------*
FORM f_excel_conversion .
  IF p_fname IS NOT INITIAL.
*** Function Module to Convert excel data to SAP ***
    CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
      EXPORTING
*       I_FIELD_SEPERATOR    =
        i_line_header        = 'X'
        i_tab_raw_data       = gt_type
        i_filename           = p_fname
      TABLES
        i_tab_converted_data = gt_excel
      EXCEPTIONS
        conversion_failed    = 1
        OTHERS               = 2.
    IF sy-subrc <> 0.
      MESSAGE 'Conversion of excel failed' TYPE 'E'.
    ENDIF.
  ELSE.
    MESSAGE 'File is Mandatory' TYPE 'E'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_initial_selection
*&---------------------------------------------------------------------*
FORM f_initial_selection.
  DATA: lr_kschl TYPE RANGE OF konp-kschl.
*****getting data for withholding tax data ****
  LOOP AT gt_excel ASSIGNING FIELD-SYMBOL(<fgs_excel>).
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = <fgs_excel>-vendor
      IMPORTING
        output = <fgs_excel>-vendor.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = <fgs_excel>-glacc
      IMPORTING
        output = <fgs_excel>-glacc.
    TRANSLATE <fgs_excel>-costcenter TO UPPER CASE.
    TRANSLATE <fgs_excel>-tds TO UPPER CASE.
    TRANSLATE <fgs_excel>-refdoc TO UPPER CASE.
  ENDLOOP.
*------- Fetching the Vendor With holding tax Data --------------------------*
  REFRESH: gt_withtax,gt_konp,gt_glget,gt_gldes,gt_kostl.
  SELECT a~lifnr
         b~witht
         b~wt_withcd
         b~qproz
         b~qsatz
         c~konth
         INTO CORRESPONDING FIELDS OF TABLE gt_withtax
         FROM lfbw AS a
         INNER JOIN t059z AS b ON b~witht = a~witht
                               AND b~wt_withcd = a~wt_withcd
         INNER JOIN t030 AS c ON  c~bwmod  = b~witht
                               AND c~komok = b~wt_withcd
         FOR ALL ENTRIES IN gt_excel
         WHERE a~lifnr EQ gt_excel-vendor AND
               a~bukrs EQ p_bukrs AND
               a~wt_subjct = 'X' AND
               b~land1 = 'IN' AND
               c~ktopl = 'YAIN' AND
               c~ktosl = 'WIT'.
  SORT gt_withtax[] BY lifnr.
****** GST G/L Accounts *****
** SELECT g/l account from yain table ***
  SELECT * FROM j_1it030k  INTO TABLE gt_glget
           WHERE ktopl = 'YAIN'.
  IF sy-subrc = 0.
    SORT gt_glget[] BY bupla.
  ENDIF.
*** G/L account description ***
  REFRESH: gt_gldes.
  SELECT * FROM skat
           INTO TABLE gt_gldes
           WHERE spras = sy-langu
           AND ktopl = 'YAIN'.
  IF sy-subrc = 0.
    SORT gt_gldes[] BY saknr.
  ENDIF.
****** COndition Type for GST percentage pickup --------------------*
  SELECT * FROM tvarvc INTO TABLE @DATA(lt_tvarvc) WHERE name = 'INVOICE_POSTING_FB60'
                                                   AND type = 'S'.
  IF sy-subrc = 0.
    REFRESH lr_kschl.
    LOOP AT lt_tvarvc INTO DATA(ls_tvarvc).
      APPEND VALUE #( sign = 'I'
                      option = 'EQ'
                      low = ls_tvarvc-low ) TO lr_kschl.
    ENDLOOP.
  ELSE.
    MESSAGE 'Condition type Variable in STVARV tcode' TYPE 'E'.
  ENDIF.
****  Fetching GST Amount for Calculation ***
  SELECT kschl
         kbetr
         mwsk1 FROM konp
               INTO TABLE gt_konp
               FOR ALL ENTRIES IN gt_excel
               WHERE mwsk1 = gt_excel-taxcode
               AND kschl IN lr_kschl
               AND kappl = 'TX'.
  IF sy-subrc = 0.
    SORT gt_konp[] BY kschl.
  ENDIF.
**** Fetching Costcenter Master for checks ***
  SELECT kostl FROM csks INTO TABLE gt_kostl
                WHERE kokrs = '1000'.
  IF sy-subrc = 0.
    SORT gt_kostl[] BY kostl.
  ENDIF.
ENDFORM.
*** Calculation for document posting data making ready ***
FORM f_validate_calculation.
  REFRESH: gt_final.
  DATA: lv_ven_amount TYPE wrbtr.
  DATA: lv_tds TYPE wrbtr.
  LOOP AT gt_excel ASSIGNING FIELD-SYMBOL(<fs_excel>).
*** Vendor code checks **
    SELECT SINGLE lifnr, name1 FROM lfa1 INTO @DATA(ls_lifnr) WHERE lifnr = @<fs_excel>-vendor.
    IF sy-subrc NE 0.
      APPEND VALUE #( vendor = <fs_excel>-vendor
                      xblnr = <fs_excel>-refdoc
                      msgtyp = 'E'
                      message = 'Incorrect Vendor Code' ) TO gt_final.
      CONTINUE.
    ENDIF.
*** function module to get fiscal year ***
    CLEAR: lv_fisyear,lv_month,l_return.
    CALL FUNCTION 'BAPI_COMPANYCODE_GET_PERIOD'
      EXPORTING
        companycodeid = p_bukrs
        posting_date  = <fs_excel>-budat
      IMPORTING
        fiscal_year   = lv_fisyear
        fiscal_period = lv_month
        return        = l_return.
**** Duplicate invoice checks ***
    SELECT SINGLE * FROM bsip INTO @DATA(ls_bsip) WHERE bukrs = @p_bukrs
                                                  AND lifnr = @<fs_excel>-vendor
                                                  AND waers = 'INR'
                                                  AND xblnr = @<fs_excel>-refdoc
                                                  AND gjahr = @lv_fisyear
                                                  AND shkzg = 'H'.
    IF sy-subrc = 0.
      APPEND VALUE #( vendor = <fs_excel>-vendor
                      venname = ls_lifnr-name1
                      bukrs = p_bukrs
                      erdat = <fs_excel>-inv_date
                      budat = <fs_excel>-budat
                      xblnr = <fs_excel>-refdoc
                      msgtyp = 'E'
                      message = 'Already invoice with same reference available' ) TO gt_final.
      CONTINUE.
    ENDIF.

**** G/L Checks *****
    DATA(lv_gl) = VALUE #( gt_gldes[ saknr = <fs_excel>-glacc ]-saknr OPTIONAL ).
    IF lv_gl IS INITIAL.
      APPEND VALUE #( vendor = <fs_excel>-vendor
                      venname = ls_lifnr-name1
                      bukrs = p_bukrs
                      erdat = <fs_excel>-inv_date
                      budat = <fs_excel>-budat
                      xblnr = <fs_excel>-refdoc
                      msgtyp = 'E'
                      message = 'G/L Account does not exists' ) TO gt_final.
      CONTINUE.
    ENDIF.
*-------------- Costecenter Validatin checks from excel -------------------------------*
    DATA(lv_cc) = VALUE #( gt_kostl[ kostl = <fs_excel>-costcenter ]-kostl OPTIONAL ).
    IF lv_cc IS INITIAL.
      APPEND VALUE #( vendor = <fs_excel>-vendor
                      venname = ls_lifnr-name1
                      bukrs = p_bukrs
                      erdat = <fs_excel>-inv_date
                      budat = <fs_excel>-budat
                      xblnr = <fs_excel>-refdoc
                      msgtyp = 'E'
                      message = 'Costcenter does not exists' ) TO gt_final.
      CONTINUE.
    ENDIF.
*----- Plant Code Validation from excel ---------------------------------------------*
    IF <fs_excel>-plant IS NOT INITIAL.
      SELECT SINGLE werks FROM t001w INTO @DATA(lv_werks) WHERE werks = @<fs_excel>-plant.
      IF sy-subrc NE 0.
        APPEND VALUE #( vendor = <fs_excel>-vendor
                        venname = ls_lifnr-name1
                        bukrs = p_bukrs
                        erdat = <fs_excel>-inv_date
                        budat = <fs_excel>-budat
                        xblnr = <fs_excel>-refdoc
                        msgtyp = 'E'
                        message = 'Plant code does not exists' ) TO gt_final.
        CONTINUE.
      ENDIF.
    ENDIF.
*-----Business Place validation from excel ------------------------------------------*
    IF <fs_excel>-bplace IS NOT INITIAL.
      SELECT SINGLE * FROM pbusinessplace INTO @DATA(ls_bplace) WHERE bukrs = @p_bukrs
                                                                AND branch = @<fs_excel>-bplace.
      IF sy-subrc NE 0.
        APPEND VALUE #( vendor = <fs_excel>-vendor
                        venname = ls_lifnr-name1
                        bukrs = p_bukrs
                        erdat = <fs_excel>-inv_date
                        budat = <fs_excel>-budat
                        xblnr = <fs_excel>-refdoc
                        msgtyp = 'E'
                        message = 'Business Place does not exists' ) TO gt_final.
        CONTINUE.
      ENDIF.
    ENDIF.
*------- Tax code Validation from excel --------------------------------------------------*
    IF <fs_excel>-taxcode IS NOT INITIAL.
      SELECT SINGLE * FROM t007a INTO @DATA(ls_taxcode) WHERE kalsm = 'TAXINN'
                                                        AND  mwskz = @<fs_excel>-taxcode.
      IF sy-subrc NE 0.
        APPEND VALUE #( vendor = <fs_excel>-vendor
                        venname = ls_lifnr-name1
                        bukrs = p_bukrs
                        erdat = <fs_excel>-inv_date
                        budat = <fs_excel>-budat
                        xblnr = <fs_excel>-refdoc
                        msgtyp = 'E'
                        message = 'Tax Code does not exists' ) TO gt_final.
        CONTINUE.
      ENDIF.
    ENDIF.
**** If no error before process starts ****
    gs_final-vendor = <fs_excel>-vendor.
    gs_final-venname = ls_lifnr-name1.
    gs_final-bukrs = p_bukrs.
    gs_final-erdat = <fs_excel>-inv_date.
    gs_final-budat = <fs_excel>-budat.
    gs_final-xblnr = <fs_excel>-refdoc.
    gs_final-blart = p_blart.
    gs_final-bplace = <fs_excel>-bplace.
    gs_final-gjahr = lv_fisyear.
    gs_final-text = <fs_excel>-text.
    gs_final-saknr = <fs_excel>-glacc.
    gs_final-gltxt = VALUE #( gt_gldes[ saknr = <fs_excel>-glacc ]-txt50 OPTIONAL ).
    gs_final-tax = <fs_excel>-taxcode.
    gs_final-plant = <fs_excel>-plant.
    gs_final-costcntr = <fs_excel>-costcenter.

    IF <fs_excel>-taxcode IS NOT INITIAL.

      DATA(lv_perc) = 100.

      DATA(lv_cgst_per) = VALUE #( gt_konp[ kschl = 'JICG'
                                            mwsk1 = <fs_excel>-taxcode  ]-kbetr OPTIONAL ). "CGST Percentage
      lv_cgst_per = CONV msatz_f05l( lv_cgst_per DIV 10 ).

      DATA(lv_sgst_per) = VALUE #( gt_konp[ kschl = 'JISG'
                                            mwsk1 = <fs_excel>-taxcode  ]-kbetr OPTIONAL ). "SGST Percentage
      lv_sgst_per = CONV msatz_f05l( lv_sgst_per DIV 10 ).

      IF lv_cgst_per IS NOT INITIAL AND lv_sgst_per IS NOT INITIAL.
        lv_perc =  lv_perc + lv_cgst_per + lv_sgst_per .
        DATA(lv_cgst) = CONV wrbtr( <fs_excel>-amount / lv_perc * lv_cgst_per ).
        DATA(lv_sgst) = CONV wrbtr( <fs_excel>-amount / lv_perc * lv_sgst_per ).
        gs_final-crate = lv_cgst_per.
        gs_final-srate = lv_sgst_per.
        gs_final-ccon = 'JICG'.
        gs_final-ccond = 'JIC'.
        gs_final-scon = 'JISG'.
        gs_final-scond = 'JIS'.
        SELECT SINGLE * FROM j_1it030k INTO @DATA(ls_glacc) WHERE ktopl = 'YAIN'
                                             AND ktosl = @gs_final-ccond
                                             AND bupla = @gs_final-bplace.
        IF sy-subrc = 0.
          gs_final-cgl = ls_glacc-konth. "Cgst G/L Account Based on Business place
        ENDIF.
        SELECT SINGLE * FROM j_1it030k INTO @DATA(ls_glacc1) WHERE ktopl = 'YAIN'
                                             AND ktosl = @gs_final-scond
                                             AND bupla = @gs_final-bplace.
        IF sy-subrc = 0.
          gs_final-sgl = ls_glacc1-konth. "Sgst G/L Account Based on Business place
        ENDIF.
      ENDIF.

      DATA(lv_igst_per) = VALUE #( gt_konp[ kschl = 'JIIG'
                                            mwsk1 = <fs_excel>-taxcode  ]-kbetr OPTIONAL ). "IGST Percentage
      lv_igst_per = lv_igst_per DIV 10.
      IF lv_igst_per IS NOT INITIAL.
        lv_perc = lv_perc + lv_igst_per.
        DATA(lv_igst) = CONV wrbtr( <fs_excel>-amount / lv_perc * lv_igst_per ).
        gs_final-irate = lv_igst_per.
        gs_final-icon = 'JIIG'.
        gs_final-icond = 'JII'.
        SELECT SINGLE * FROM j_1it030k INTO @DATA(ls_glacc2) WHERE ktopl = 'YAIN'
                                    AND ktosl = @gs_final-icond
                                    AND bupla = @gs_final-bplace.
        IF sy-subrc = 0.
          gs_final-igl = ls_glacc2-konth. "Igst G/L Account Based on Business place
        ENDIF.
      ENDIF.

    ENDIF.
*********GST Calculation for Net Amount *****
    IF lv_cgst IS NOT INITIAL.
      gs_final-cgst = lv_cgst.
    ENDIF.
    IF lv_sgst IS NOT INITIAL.
      gs_final-sgst = lv_sgst.
    ENDIF.

    IF lv_igst IS NOT INITIAL.
      gs_final-igst = lv_igst.
      CLEAR lv_amount.
      lv_amount = <fs_excel>-amount - lv_igst.
    ELSE.
      CLEAR lv_amount.
      lv_amount = <fs_excel>-amount - lv_sgst - lv_cgst.
    ENDIF.
*  ENDIF.

    IF lv_amount IS NOT INITIAL.
      gs_final-net_amnt = lv_amount.
    ELSE.
      gs_final-net_amnt = <fs_excel>-amount.
    ENDIF.
    IF <fs_excel>-tds = 'YES'.
***** TDS Calculation for posting ***
      READ TABLE gt_withtax ASSIGNING FIELD-SYMBOL(<fs_tdstax>) WITH KEY lifnr = <fs_excel>-vendor.
      IF sy-subrc = 0.
        gs_final-ventax = <fs_tdstax>-witht.
        gs_final-ventax1 = <fs_tdstax>-wt_withcd.
        gs_final-tds_gl = <fs_tdstax>-konth.
        gs_final-tds = ( gs_final-net_amnt * <fs_tdstax>-qsatz ) / <fs_tdstax>-qproz.
        gs_final-tds_amt = gs_final-net_amnt.
      ENDIF.
    ENDIF.

    gs_final-vend_amnt = gs_final-net_amnt + lv_cgst + lv_sgst + lv_igst.
    gs_final-msgtyp = 'S'.
    gs_final-message = 'No error in lieitems'.
    APPEND gs_final TO gt_final.
    CLEAR gs_final.
    CLEAR: gs_final,lv_cgst,lv_sgst,lv_igst,ls_glacc1,ls_glacc2,ls_glacc.
  ENDLOOP.
*---- Appending Vendor Lineitems for each One Document Number ---------------------*
  DATA(lt_final_tmp) = gt_final[].
  DELETE ADJACENT DUPLICATES FROM lt_final_tmp[] COMPARING vendor xblnr.
  LOOP AT lt_final_tmp ASSIGNING FIELD-SYMBOL(<fs_final_tmp>).
    LOOP AT gt_final INTO DATA(ls_final) WHERE vendor = <fs_final_tmp>-vendor AND xblnr = <fs_final_tmp>-xblnr .
      lv_ven_amount = lv_ven_amount + ls_final-net_amnt + ls_final-igst + ls_final-cgst + ls_final-sgst .
      lv_tds = lv_tds + ls_final-tds.
      gv_tds_famnt = gv_tds_famnt + ls_final-tds_amt.
    ENDLOOP.
    APPEND VALUE #( vendor = <fs_final_tmp>-vendor
                    venname = <fs_final_tmp>-venname
                    bukrs = p_bukrs
                    gjahr = <fs_final_tmp>-gjahr
                    erdat = <fs_final_tmp>-erdat
                    budat = <fs_final_tmp>-budat
                    xblnr = <fs_final_tmp>-xblnr
                    vend_amnt = lv_ven_amount
                    tds = lv_tds
                    tds_amt = gv_tds_famnt
                    msgtyp = 'V' ) TO gt_final.

    CLEAR: lv_ven_amount,ls_final,lv_tds,gv_tds_famnt.
  ENDLOOP.
  SORT gt_final[] BY vendor xblnr msgtyp.
ENDFORM.
FORM f_display_alv.
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
* create the alv object
  TRY.
      CALL METHOD cl_salv_table=>factory
        IMPORTING
          r_salv_table = lo_gr_alv
        CHANGING
          t_table      = gt_final.
    CATCH cx_salv_msg.
  ENDTRY.


  lo_gr_alv->set_screen_status(
    pfstatus      =  'INV_STATUS'
    report        =  sy-repid
    set_functions = lo_gr_alv->c_functions_all ).

  lr_aggregations = lo_gr_alv->get_aggregations( ).
* Let's show all default buttons of ALV
  lo_gr_functions = lo_gr_alv->get_functions( ).
  lo_gr_functions->set_all( value  = if_salv_c_bool_sap=>true ).

* Fit the columns
  lo_columns = lo_gr_alv->get_columns( ).
  lo_columns->set_optimize( 'X' ).


* Apply zebra style to lv_rows
  lo_display = lo_gr_alv->get_display_settings( ).
  lo_display->set_striped_pattern( cl_salv_display_settings=>true ).

* Register events
  lo_events = lo_gr_alv->get_event( ).
  CREATE OBJECT lo_event_handler.
  SET HANDLER lo_event_handler->on_user_command FOR lo_events.
  SET HANDLER lo_event_handler->on_double_click FOR lo_events.


  TRY.
      lo_column ?= lo_columns->get_column( 'VENDOR' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Vendor Code' ).
      lo_column->set_medium_text( 'Vendor' ).
      lo_column->set_short_text( 'Vendor' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'VENNAME' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Vendor Name' ).
      lo_column->set_medium_text( 'Posting Key' ).
      lo_column->set_short_text( 'Key' ).
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
      lo_column ?= lo_columns->get_column( 'ERDAT' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Invoice Date' ).
      lo_column->set_medium_text( 'Invoice Date' ).
      lo_column->set_short_text( 'Inv Date' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'BUDAT' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Posting date' ).
      lo_column->set_medium_text( 'Posting date' ).
      lo_column->set_short_text( 'Postdate' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'XBLNR' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Reference Document' ).
      lo_column->set_medium_text( 'Reference Doc' ).
      lo_column->set_short_text( 'Refdoc' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'BLART' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Document Type' ).
      lo_column->set_medium_text( 'Document Type' ).
      lo_column->set_short_text( 'Doctype' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'GJAHR' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Fiscal Year' ).
      lo_column->set_medium_text( 'Fiscal Year' ).
      lo_column->set_short_text( 'Fisyear' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'BPLACE' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Business Place' ).
      lo_column->set_medium_text( 'Business Place' ).
      lo_column->set_short_text( 'Bplace' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'TEXT' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Item Text' ).
      lo_column->set_medium_text( 'Item Text' ).
      lo_column->set_short_text( 'Text' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'GLTXT' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'G/L Description' ).
      lo_column->set_medium_text( 'G/L Description' ).
      lo_column->set_short_text( 'G/L Desc' ).
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
      lo_column ?= lo_columns->get_column( 'VEND_AMNT' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Vendor Net amount' ).
      lo_column->set_medium_text( 'Vendor Net amount' ).
      lo_column->set_short_text( 'Netamnt' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'NET_AMNT' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Gross Amount' ).
      lo_column->set_medium_text( 'Gross Amount' ).
      lo_column->set_short_text( 'GrossAmt' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'IGL' ).
      lo_column->set_visible( if_salv_c_bool_sap=>false ).
      lo_column->set_long_text( 'IGST tax G/L Account' ).
      lo_column->set_medium_text( 'IGST tax G/L' ).
      lo_column->set_short_text( 'ITAX G/L' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'IGST' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'IGST Tax Amount' ).
      lo_column->set_medium_text( 'IGST Tax Amt' ).
      lo_column->set_short_text( 'Itax_amt' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'SGL' ).
      lo_column->set_visible( if_salv_c_bool_sap=>false ).
      lo_column->set_long_text( 'SGST tax G/L Account' ).
      lo_column->set_medium_text( 'SGST tax G/L' ).
      lo_column->set_short_text( 'STAX G/L' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.
  TRY.
      lo_column ?= lo_columns->get_column( 'SGST' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'SGST Tax Amount' ).
      lo_column->set_medium_text( 'SGST Tax Amt' ).
      lo_column->set_short_text( 'Stax_amt' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'CGST' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'CGST Tax Amount' ).
      lo_column->set_medium_text( 'CGST Tax Amt' ).
      lo_column->set_short_text( 'Ctax_amt' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'CGL' ).
      lo_column->set_visible( if_salv_c_bool_sap=>false ).
      lo_column->set_long_text( 'CGST tax G/L Account' ).
      lo_column->set_medium_text( 'CGST tax G/L' ).
      lo_column->set_short_text( 'CTAX G/L' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.


  TRY.
      lo_column ?= lo_columns->get_column( 'TDS_GL' ).
      lo_column->set_visible( if_salv_c_bool_sap=>false ).
      lo_column->set_long_text( 'TDS G/L Account' ).
      lo_column->set_medium_text( 'TDS G/L Account' ).
      lo_column->set_short_text( 'TDS G/LAcc' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'TDS' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'TDS Amount' ).
      lo_column->set_medium_text( 'TDS Amount' ).
      lo_column->set_short_text( 'TDS Amnt' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.
  TRY.
      lo_column ?= lo_columns->get_column( 'MSGTYP' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Msg type' ).
      lo_column->set_medium_text( 'Msg type' ).
      lo_column->set_short_text( 'Msgtyp' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'DOCNO' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
*      lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
      lo_column->set_long_text( 'Document Number' ).
      lo_column->set_medium_text( 'Document No' ).
      lo_column->set_short_text( 'DocNo' ).
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

  TRY.
      lo_column ?= lo_columns->get_column( 'FLAG' ).
      lo_column->set_visible( if_salv_c_bool_sap=>false ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.


  TRY.
      lo_column ?= lo_columns->get_column( 'ICON' ).
      lo_column->set_visible( if_salv_c_bool_sap=>false ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'ICOND' ).
      lo_column->set_visible( if_salv_c_bool_sap=>false ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'IRATE' ).
      lo_column->set_visible( if_salv_c_bool_sap=>false ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'SCON' ).
      lo_column->set_visible( if_salv_c_bool_sap=>false ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'SCOND' ).
      lo_column->set_visible( if_salv_c_bool_sap=>false ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'SRATE' ).
      lo_column->set_visible( if_salv_c_bool_sap=>false ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'CCON' ).
      lo_column->set_visible( if_salv_c_bool_sap=>false ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'VENTAX' ).
      lo_column->set_visible( if_salv_c_bool_sap=>false ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'VENTAX1' ).
      lo_column->set_visible( if_salv_c_bool_sap=>false ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'CCOND' ).
      lo_column->set_visible( if_salv_c_bool_sap=>false ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'CRATE' ).
      lo_column->set_visible( if_salv_c_bool_sap=>false ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'FUND' ).
      lo_column->set_visible( if_salv_c_bool_sap=>false ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  lo_gr_alv->display( ).
ENDFORM.
FORM handle_user_command USING i_ucomm TYPE salv_de_function.

  CASE i_ucomm.
    WHEN '&INV_POST'.
      PERFORM f_invoice_posting.
  ENDCASE.
ENDFORM.
FORM f_invoice_posting.
  DATA(lt_posting) = gt_final[].
  DATA: lv_lineitem TYPE i,
        lv_taxiden  TYPE i,
        lv_titem    TYPE i.
  DELETE ADJACENT DUPLICATES FROM lt_posting COMPARING vendor xblnr.
  LOOP AT lt_posting ASSIGNING FIELD-SYMBOL(<fs_posting>) WHERE msgtyp = 'S'.
    REFRESH: lt_payable,lt_accountgl,lt_tax,lt_currency,lt_withtax.
*** FUNCTION module to GET fiscal year ***
    CLEAR: lv_fisyear,lv_month,l_return.
    CALL FUNCTION 'BAPI_COMPANYCODE_GET_PERIOD'
      EXPORTING
        companycodeid = p_bukrs
        posting_date  = <fs_posting>-budat
      IMPORTING
        fiscal_year   = lv_fisyear
        fiscal_period = lv_month
        return        = l_return.
*** Actual Process starts Here for Posting Invoice ****
    DATA(ls_header) = VALUE bapiache09( bus_act = 'RFBU'
                                        username = sy-uname
                                        comp_code = p_bukrs
                                        doc_date = <fs_posting>-erdat
                                        pstng_date = <fs_posting>-budat
                                        fisc_year = lv_fisyear
                                        doc_type = p_blart
                                        ref_doc_no = <fs_posting>-xblnr ).
**** Vendor Line items to be passed ***
    lt_payable = VALUE #(
                 ( itemno_acc = '1'
                   vendor_no = <fs_posting>-vendor
                   bline_date = <fs_posting>-erdat
                   item_text = <fs_posting>-text
*                   tax_code =  VALUE #( gt_withtax[ lifnr = <fs_posting>-vendor ]-witht OPTIONAL )
                   businessplace = <fs_posting>-bplace ) ).

    CLEAR lv_fund.
    CALL FUNCTION 'ZFUND_GET'
      EXPORTING
        date       = <fs_posting>-budat
      IMPORTING
        lv_fund    = lv_fund
      EXCEPTIONS
        enter_date = 1
        OTHERS     = 2.
    IF sy-subrc <> 0.

    ENDIF.
    lv_lineitem = '2'.
    lv_taxiden = '1'.
    lv_titem = '000001'.
    LOOP AT gt_final ASSIGNING FIELD-SYMBOL(<fs_final>) WHERE vendor = <fs_posting>-vendor AND xblnr = <fs_posting>-xblnr
                                                        AND msgtyp NE 'V'.
      APPEND VALUE #( itemno_acc = lv_lineitem
                      gl_account = <fs_final>-saknr
                      tax_code = <fs_final>-tax
                      itemno_tax = lv_taxiden
                      fund = lv_fund
                      item_text = <fs_final>-text
                      plant =  <fs_final>-plant
                      costcenter = <fs_final>-costcntr ) TO lt_accountgl.
      APPEND VALUE #( itemno_acc = lv_lineitem
                      currency = 'INR'
                      amt_doccur = <fs_final>-net_amnt
                      amt_base =  <fs_final>-net_amnt
                    ) TO lt_currency . "G/L line Items
      IF <fs_final>-cgst IS NOT INITIAL.
        lv_lineitem = lv_lineitem + 1.
        DATA(ls_curramnt) = VALUE bapiaccr09( itemno_acc = lv_lineitem
                                              currency = 'INR'
                                              amt_doccur = <fs_final>-cgst
                                              amt_base = <fs_final>-net_amnt
                                              tax_amt = <fs_final>-cgst
                                             ).
        APPEND ls_curramnt TO lt_currency.
*** tax table filling ***
        CLEAR ls_tax.
        ls_tax-itemno_acc = lv_lineitem.
        ls_tax-cond_key = <fs_final>-ccon.
        ls_tax-itemno_tax = lv_titem.
        ls_tax-acct_key = <fs_final>-ccond.
        ls_tax-tax_rate = <fs_final>-crate.
        ls_tax-tax_code = <fs_final>-tax.
        ls_tax-direct_tax = 'X'.
        ls_tax-gl_account = <fs_final>-cgl.
        APPEND ls_tax TO lt_tax.
      ENDIF.
      IF <fs_final>-sgst IS NOT INITIAL.
        lv_lineitem = lv_lineitem + 1.
        DATA(ls_curramnt1) = VALUE bapiaccr09( itemno_acc = lv_lineitem
                                              currency = 'INR'
                                              amt_doccur = <fs_final>-sgst
                                              amt_base = <fs_final>-net_amnt
                                              tax_amt = <fs_final>-sgst
                                             ).
        APPEND ls_curramnt1 TO lt_currency.
*** tax table filling ***
        CLEAR ls_tax.
        ls_tax-itemno_acc = lv_lineitem.
        ls_tax-cond_key = <fs_final>-scon.
        ls_tax-itemno_tax = lv_titem.
        ls_tax-acct_key = <fs_final>-scond.
        ls_tax-tax_rate = <fs_final>-srate.
        ls_tax-tax_code = <fs_final>-tax.
        ls_tax-direct_tax = 'X'.
        ls_tax-gl_account = <fs_final>-sgl.
        APPEND ls_tax TO lt_tax.
      ENDIF.
*----- IGST Calculation item --------------------------------------------------*
      IF <fs_final>-igst IS NOT INITIAL.
        lv_lineitem = lv_lineitem + 1.
        DATA(ls_curramnt2) = VALUE bapiaccr09( itemno_acc = lv_lineitem
                                              currency = 'INR'
                                              amt_doccur = <fs_final>-igst
                                              amt_base = <fs_final>-net_amnt
                                              tax_amt = <fs_final>-igst
                                             ).
        APPEND ls_curramnt2 TO lt_currency.
*** tax table filling ***
        CLEAR ls_tax.
        ls_tax-itemno_acc = lv_lineitem.
        ls_tax-cond_key = <fs_final>-icon.
        ls_tax-itemno_tax = lv_titem.
        ls_tax-acct_key = <fs_final>-icond.
        ls_tax-tax_rate = <fs_final>-irate.
        ls_tax-tax_code = <fs_final>-tax.
        ls_tax-direct_tax = 'X'.
        ls_tax-gl_account = <fs_final>-igl.
        APPEND ls_tax TO lt_tax.
      ENDIF.
      lv_lineitem = lv_lineitem + 1.
      lv_taxiden = lv_taxiden.
    ENDLOOP.
    DATA(ls_vendor) = VALUE #( gt_final[ vendor = <fs_posting>-vendor
                                         xblnr = <fs_posting>-xblnr
                                         msgtyp = 'V' ] OPTIONAL ).
    IF ls_vendor IS NOT INITIAL.
      APPEND VALUE #( itemno_acc = '1'
                      currency = 'INR'
                      amt_doccur = ls_vendor-vend_amnt * -1
                      amt_base = ls_vendor-vend_amnt * -1 ) TO lt_currency."Vendor Line item
      IF ls_vendor-tds IS NOT INITIAL.
        lv_lineitem = lv_lineitem + 1.
        DATA(ls_curramnt3) = VALUE bapiaccr09( itemno_acc = lv_lineitem
                                               currency = 'INR'
                                               amt_doccur = ls_vendor-tds * -1
                                               amt_base = ls_vendor-vend_amnt
                                               tax_amt = ls_vendor-tds * -1
                                              ).
        APPEND ls_curramnt3 TO lt_currency.
        SORT lt_currency[] BY itemno_acc.
        CLEAR ls_withtax.
        ls_withtax-itemno_acc = '1'.
        ls_withtax-wt_type = VALUE #( gt_withtax[ lifnr = ls_vendor-vendor ]-witht OPTIONAL ).
        ls_withtax-wt_code = VALUE #( gt_withtax[ lifnr = ls_vendor-vendor ]-wt_withcd OPTIONAL ).
        ls_withtax-bas_amt_tc = ls_vendor-tds_amt.
*      ls_withtax-man_amt_ind = 'X'.
        APPEND ls_withtax TO lt_withtax.
      ENDIF.
    ENDIF.
***** Performing Checks ***
    PERFORM f_document_check USING ls_header.
    SORT return BY type.
    READ TABLE return INTO DATA(ls_ret) WITH KEY type = 'E'.
    IF sy-subrc = 0 .
      LOOP AT return INTO DATA(lw_ret) WHERE type = 'E'.
        DATA(lv_msg) = lw_ret-message.
      ENDLOOP.
      LOOP AT gt_final ASSIGNING <fs_final> WHERE vendor = <fs_posting>-vendor AND xblnr = <fs_posting>-xblnr.
        <fs_final>-msgtyp = 'E'.
        <fs_final>-message = lv_msg.
      ENDLOOP.
    ELSE.
***** If test run is not enabled it will post directly ****
      IF p_run NE abap_true.
        PERFORM f_document_post USING ls_header CHANGING lv_objkey.
        LOOP AT gt_final ASSIGNING <fs_final> WHERE vendor = <fs_posting>-vendor AND xblnr = <fs_posting>-xblnr..
          <fs_final>-msgtyp = 'S'.
          <fs_final>-docno = lv_objkey(10).
          <fs_final>-message = | Doc No: { lv_objkey(10) } Posted successfully for vendor { gs_final-vendor } |.
          CONDENSE <fs_final>-message.
        ENDLOOP.
      ELSE.
*** If test run enabled means ****
        LOOP AT gt_final ASSIGNING <fs_final> WHERE vendor = <fs_posting>-vendor AND xblnr = <fs_posting>-xblnr.
          <fs_final>-msgtyp = 'S'.
          <fs_final>-message = | Invoice Document ready to Post |.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDLOOP.
  lo_gr_alv->refresh( ).
ENDFORM.
FORM f_document_check USING p_header TYPE bapiache09.
  REFRESH: return.
*** Document Check Before posting ***
  CALL FUNCTION 'BAPI_ACC_DOCUMENT_CHECK'
    EXPORTING
      documentheader = p_header
    TABLES
      accountgl      = lt_accountgl
      accountpayable = lt_payable
      accounttax     = lt_tax
      currencyamount = lt_currency
      return         = return
      accountwt      = lt_withtax.
ENDFORM.
FORM f_document_post USING p_header TYPE bapiache09 CHANGING p_objkey TYPE bapiache09-obj_key.
  REFRESH: return.
  CLEAR: lv_objkey,lv_objsys,lv_objtyp.
*** Function Module to create Debit note ***
  CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
    EXPORTING
      documentheader = p_header
    IMPORTING
      obj_type       = lv_objtyp
      obj_key        = lv_objkey
      obj_sys        = lv_objsys
    TABLES
      accountgl      = lt_accountgl
      accountpayable = lt_payable
      accounttax     = lt_tax
      currencyamount = lt_currency
      return         = return
      accountwt      = lt_withtax.
  IF sy-subrc = 0.
    COMMIT WORK AND WAIT.
    p_objkey = lv_objkey.
  ENDIF.
ENDFORM.
