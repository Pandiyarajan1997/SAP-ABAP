*&---------------------------------------------------------------------*
*& Include zmaterial_upload_class
*&---------------------------------------------------------------------*
CLASS lcl_material DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS: get_instance RETURNING VALUE(instance) TYPE REF TO lcl_material.
    METHODS:file_name ,
      upload ,
      validate,
      update ,
      display.
  PRIVATE SECTION.
    CLASS-DATA:lo_material TYPE REF TO lcl_material.
ENDCLASS.

CLASS lcl_material IMPLEMENTATION.

  METHOD get_instance.
    IF lo_material IS NOT BOUND.
      CREATE OBJECT lo_material.
    ENDIF.
    instance = lo_material.
  ENDMETHOD.

  METHOD update.
    SORT it_mattab BY matnr.
    LOOP AT it_mattab INTO DATA(is_upload).
      IF is_upload-matnr IS INITIAL.

        REFRESH it_matnr. CLEAR wa_headdata.

        CALL FUNCTION 'BAPI_STDMATERIAL_GETINTNUMBER'
          EXPORTING
            material_type    = is_upload-mtart
            industry_sector  = is_upload-mbrsh
            required_numbers = 1
          IMPORTING
            return           = wa_ret
          TABLES
            material_number  = it_matnr.

      ENDIF.
**********************************************************************Material header Data
      wa_headdata = VALUE #(  material          = COND #( WHEN is_upload-matnr IS NOT INITIAL THEN is_upload-matnr ELSE it_matnr[ 1 ]-material )
                              ind_sector        = is_upload-mbrsh
                              matl_type         = is_upload-mtart
                              basic_view        = c_x
                              sales_view        = c_x
                              purchase_view     = c_x
                              storage_view      = c_x
                              account_view      = c_x
                              inp_fld_check     = 'W' ).

**********************************************************************MARA Client Data
      wa_clientdata = VALUE #(  matl_group                    = is_upload-matl_group
                                old_mat_no                    = wa_headdata-material
                                base_uom                      = CONV #( is_upload-meins )
                                base_uom_iso                  = CONV #( is_upload-meins )
*                                po_unit                       = CONV #( is_upload-meins )
*                                po_unit_iso                   = CONV #( is_upload-meins )
                                size_dim                      = is_upload-size_dim
                                std_descr                     = is_upload-maktx
                                net_weight                    = is_upload-net_weight
                                unit_of_wt                    = is_upload-wt_unit
                                unit_of_wt_iso                = is_upload-wt_unit
                                division                      = is_upload-divison
                                batch_mgmt                    = c_x
                                minremlife                    = CONV #( is_upload-min_shelflife )
                                shelf_life                    = CONV #( is_upload-tot_shelflife )
                                item_cat                      = c_itcat
                                sled_bbd                      = c_expd
                                trans_grp                     = c_tgrp
                                zzterm                        = is_upload-payterm
                                zzgross_wt                    = is_upload-gross_wt
                                zzvolume                      = is_upload-volume
                                zzvoleh                       = is_upload-vol_unit
                                zzecom_category               = is_upload-ecom_category
                                zzecom_mat_name               = is_upload-ecom_matnr
                                zzecom_name                   = is_upload-ecom_name
                                zzref_material                = '100001982'
*                              extmatlgrp                    =
                              ).

      wa_clientdatax  = VALUE #(  matl_group                    = c_x
                                  old_mat_no                    = c_x
                                  base_uom                      = c_x
                                  base_uom_iso                  = c_x
*                                  po_unit                       = c_x
*                                  po_unit_iso                   = c_x
                                  size_dim                      = c_x
                                  std_descr                     = c_x
                                  net_weight                    = c_x
                                  unit_of_wt                    = c_x
                                  unit_of_wt_iso                = c_x
                                  division                      = c_x
                                  batch_mgmt                    = c_x
                                  minremlife                    = c_x
                                  shelf_life                    = c_x
                                  item_cat                      = c_x
                                  sled_bbd                      = c_x
                                  trans_grp                     = c_x
                                  uomusage                      = c_x
                                  zzterm                        = c_x
                                  zzgross_wt                    = c_x
                                  zzvolume                      = c_x
                                  zzvoleh                       = c_x
                                  zzecom_category               = c_x
                                  zzecom_mat_name               = c_x
                                  zzecom_name                   = c_x
                                  zzref_material                = c_x
                                ).

**********************************************************************MARC Plant Data
      wa_plantdata = VALUE #(  plant                          = is_upload-plant
                               period_ind                     = c_pind
                               fy_variant                     = 'V9'
                               batch_mgmt                     = c_x
                               availcheck                     = c_achk
                               profit_ctr                     = is_upload-prctr
                               ctrl_code                      = is_upload-cntrl_code
                               loadinggrp                     = c_lgrp
                             ).

      wa_plantdatax = VALUE #(  plant                          = is_upload-plant
                                period_ind                     = c_x
                                fy_variant                     = c_x
                                batch_mgmt                     = c_x
                                availcheck                     = c_x
                                profit_ctr                     = c_x
                                ctrl_code                      = c_x
                                loadinggrp                     = c_x
                             ).

**********************************************************************MARD Plant/Storage Data
      wa_storagelocationdata  = VALUE #( plant = is_upload-plant stge_loc = is_upload-lgort ).
      wa_storagelocationdatax = VALUE #( plant = is_upload-plant stge_loc = is_upload-lgort ).

**********************************************************************MBEW valuation Data
      "Deriving Valuation class from mtart
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      SELECT SINGLE FROM t134 FIELDS kkref WHERE mtart = @is_upload-mtart INTO @DATA(iv_catref).
      IF sy-subrc = 0 .
        SELECT SINGLE FROM t025 FIELDS bklas WHERE kkref = @iv_catref INTO @DATA(iv_valclass).
      ENDIF.
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      wa_valuationdata = VALUE #( val_area     = is_upload-plant
*                               val_type     =
                                  price_ctrl   = c_pcntrl
                                  std_price    = is_upload-std_price
                                  price_unit   = c_prunit
                                  val_class    = iv_valclass
                                ).

      wa_valuationdatax = VALUE #( val_area     = is_upload-plant
*                               val_type     =
                                  price_ctrl   = c_x
                                  std_price    = c_x
                                  price_unit   = c_x
                                  val_class    = c_x
                                ).
**********************************************************************MVKE Sales Data
      wa_salesdata  = VALUE #(  sales_org = is_upload-sales_org  distr_chan = is_upload-distr_chan  item_cat = c_itcat  ).
      wa_salesdatax = VALUE #(  sales_org = is_upload-sales_org  distr_chan = is_upload-distr_chan item_cat = c_x  ).

      """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""Table Type Data's
*!!!Material Description
      APPEND VALUE #( langu = sy-langu langu_iso = sy-langu matl_desc = is_upload-maktx ) TO ist_materialdescription.

*!!!Alternate Stock-keeping Units
      SORT it_altuom BY matnr alt_uom .
      LOOP AT it_altuom INTO DATA(is_altuom) WHERE matnr = wa_headdata-material.
        APPEND VALUE #( alt_unit = is_altuom-alt_uom denominatr = is_altuom-denominator numerator = is_altuom-numerator  gross_wt = is_upload-gross_wt
                        unit_of_wt = is_upload-wt_unit unit_of_wt_iso = is_upload-wt_unit  ) TO ist_unitsofmeasure.
        APPEND VALUE #( alt_unit = is_altuom-alt_uom  denominatr = c_x numerator = c_x  gross_wt = c_x unit_of_wt = c_x unit_of_wt_iso = c_x  ) TO ist_unitsofmeasurex.
*        AT END OF matnr.
*          IF is_altuom-matnr IS NOT INITIAL.
*            EXIT.
*          ENDIF.
*        ENDAT.
      ENDLOOP.

*!!!Tax Classifications
      APPEND VALUE #(  depcountry     = 'IN'
                       depcountry_iso = 'IN'
                       tax_type_1     = 'JTC1'
                       tax_type_2     = 'JOCG'
                       tax_type_3     = 'JOIG'
                       tax_type_4     = 'JOUG'
                       tax_type_5     = 'JCOS'
                       tax_type_6     = 'JOSG'
                       taxclass_1     = c_taxcl
                       taxclass_2     = c_taxcl
                       taxclass_3     = c_taxcl
                       taxclass_4     = c_taxcl
                       taxclass_5     = c_taxcl
                       taxclass_6     = c_taxcl ) TO ist_taxclassifications.

*!!!Finally,Material BAPI called for creation
      CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
        EXPORTING
          headdata             = wa_headdata
          clientdata           = wa_clientdata
          clientdatax          = wa_clientdatax
          plantdata            = wa_plantdata
          plantdatax           = wa_plantdatax
          storagelocationdata  = wa_storagelocationdata
          storagelocationdatax = wa_storagelocationdatax
          valuationdata        = wa_valuationdata
          valuationdatax       = wa_valuationdatax
          salesdata            = wa_salesdata
          salesdatax           = wa_salesdatax
          storagetypedata      = wa_storagetypedata
          storagetypedatax     = wa_storagetypedatax
          forecastparameters   = wa_forecastdata
          forecastparametersx  = wa_forecastdatax
        IMPORTING
          return               = wa_return
        TABLES
          materialdescription  = ist_materialdescription
          unitsofmeasure       = ist_unitsofmeasure
          unitsofmeasurex      = ist_unitsofmeasurex
          materiallongtext     = ist_materiallongtext
          taxclassifications   = ist_taxclassifications
          prtdata              = ist_prtdata
          prtdatax             = ist_prtdatax
          extensionin          = extensionin
          extensioninx         = extensioninx.

      IF wa_return-type = 'S' AND wa_return-number = '356'.
        DATA lv_matnr TYPE bapi1003_key-object.
        lv_matnr = wa_headdata-material.
        DATA : l_classnum  TYPE bapi1003_key-classnum,
               l_classtype TYPE bapi1003_key-classtype,
               l_date      TYPE bapi1003_key-keydate.

        MOVE is_upload-class TO l_classnum.
        MOVE is_upload-klart TO l_classtype.
        IF is_upload-date is NOT INITIAL.
        l_date = |{ is_upload-date+0(4) }{ is_upload-date+5(2) }{ is_upload-date+8(2) }|.
        ENDIF.
        IF  l_classnum IS NOT INITIAL.
          CALL FUNCTION 'BAPI_OBJCL_CREATE'
            EXPORTING
              objectkeynew   = lv_matnr
              objecttablenew = 'MARA'
              classnumnew    = l_classnum
              classtypenew   = l_classtype
              keydate        = l_date
            TABLES
              return         = it_return.

          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.
        ENDIF.

        APPEND VALUE #( matnr = wa_headdata-material msgty = wa_return-type message = wa_return-message  ) TO lt_msg.
      ELSE.
       CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        APPEND VALUE #( matnr = wa_headdata-material msgty = wa_return-type message = wa_return-message  ) TO lt_msg.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD display.
    DATA: lo_alv  TYPE REF TO cl_salv_table,
          lo_func TYPE REF TO cl_salv_functions_list.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table   = lo_alv
          CHANGING
            t_table        = lt_msg
        ).

        lo_func = lo_alv->get_functions( ).
        lo_func->set_all( abap_true ).

        lo_alv->display( ).
      CATCH cx_salv_msg. " ALV: General Error Class with Message
    ENDTRY.
  ENDMETHOD.

  METHOD file_name.
    CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
      EXPORTING
        program_name = syst-repid
      CHANGING
        file_name    = p_fname.

  ENDMETHOD.

  METHOD upload.
*    DATA(lo_upload) = NEW zcl_excel_uploader_new( ).
*
*    lo_upload->filename = p_fname.
*    lo_upload->header_rows_count = 1.
*    lo_upload->upload(
*      CHANGING
*        ct_data = it_upload
*    ).
    cl_gui_frontend_services=>gui_upload(
         EXPORTING
           filename = CONV #( p_fname )
           filetype = 'BIN'
         IMPORTING
           filelength = DATA(l_length)
         CHANGING
           data_tab = l_t_data
       ).

    TRY.
        DATA(l_r_xls) = NEW cl_fdt_xl_spreadsheet(
          document_name = CONV #( p_fname )
          xdocument = cl_fxs_converter=>w3mimetab_to_xstring( iv_w3mimetab = l_t_data iv_length = l_length )
        ).
      CATCH cx_fdt_excel_core.
        ASSERT 1 = 2.  "Static Assertion
    ENDTRY.

    l_r_xls->if_fdt_doc_spreadsheet~get_worksheet_names( IMPORTING worksheet_names = DATA(lt_worksheets) ).

    IF lt_worksheets IS NOT INITIAL.

      REFRESH : it_mattab,it_altuom.

      LOOP AT lt_worksheets INTO DATA(lv_woksheetname).

        DATA(lo_data_ref) = l_r_xls->if_fdt_doc_spreadsheet~get_itab_from_worksheet(
                                                 lv_woksheetname ).
        "now we have excel work sheet data in dynamic internal table
        ASSIGN lo_data_ref->* TO <gt_data>.
        IF <gt_data> IS ASSIGNED.
          DELETE <gt_data> INDEX 1.
          DELETE <gt_data> INDEX 1.
        ENDIF.

        IF it_mattab IS INITIAL.
          IF <gt_data> IS INITIAL.
            MESSAGE 'No data found' TYPE 'S' DISPLAY LIKE 'E'.
            LEAVE LIST-PROCESSING.
          ENDIF.
        it_mattab[] = <gt_data>.
        ELSEIF it_altuom IS INITIAL.

          it_altuom[] = <gt_data>.

        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
METHOD validate.
DELETE it_mattab where matnr is INITIAL.
DELETE it_altuom WHERE alt_uom is INITIAL.
LOOP AT It_mattab ASSIGNING FIELD-SYMBOL(<fs_mattab>).
<fs_mattab>-matnr = CONV matnr( <fs_mattab>-matnr ).
*** Check Plant
*    <fs_mattab>
***Gross weight conversion
<fs_mattab>-gross_wt = CONV brgew( <fs_mattab>-gross_wt ).
***Net weight conversion
<fs_mattab>-net_weight = CONV brgew( <fs_mattab>-net_weight ).
***Volume conversion
<fs_mattab>-volume = CONV volum( <fs_mattab>-volume ).
***Size/Dim Coversion
<fs_mattab>-size_dim = CONV groes( <fs_mattab>-size_dim ).
ENDLOOP.



  ENDMETHOD.

ENDCLASS.
*&---------------------------------------------------------------------*
*& Form bdc_dynpro
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      --> P_
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  bdc_dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PROGRAM    text
*      -->DYNPRO     text
*----------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.
  CLEAR bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = dynpro.
  bdcdata-dynbegin = 'X'.
  APPEND bdcdata.
ENDFORM.                    "bdc_dynpro
*&---------------------------------------------------------------------*
*&      Form  bdc_field
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->FNAM       text
*      -->FVAL       text
*----------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  APPEND bdcdata.
ENDFORM.                   "bdc_field
FORM Excel_download USING lo_excel TYPE REF TO zcl_excel.

  DATA: lo_worksheet              TYPE REF TO zcl_excel_worksheet,

        lo_hyperlink              TYPE REF TO zcl_excel_hyperlink,

        lv_tabcolor               TYPE zexcel_s_tabcolor,

        ls_header                 TYPE zexcel_s_worksheet_head_foot,
        ls_footer                 TYPE zexcel_s_worksheet_head_foot,
        lo_column                 TYPE REF TO zcl_excel_column,
        lv_style_bold_border_guid TYPE zexcel_cell_style,
        lo_style_bold_border      TYPE REF TO zcl_excel_style,
        lo_border_dark            TYPE REF TO zcl_excel_style_border,
        lo_complete               TYPE REF TO zexcel_s_cstyle_complete.

  CONSTANTS: gc_save_file_name TYPE string VALUE '04_Sheets.xlsx'.
  "zdemo_excel_outputopt_incl.

  " Creates active sheet
  CREATE OBJECT lo_excel.

  " Get active sheet
  lo_worksheet = lo_excel->get_active_worksheet( ).
  lo_worksheet->set_title( ip_title = 'Materials' ).

  CREATE OBJECT lo_border_dark.
  lo_border_dark->border_color-rgb = zcl_excel_style_color=>c_black.
  lo_border_dark->border_style = zcl_excel_style_border=>c_border_thin.

  DATA(lo_style) = lo_excel->add_new_style(  ).
  lo_style->font->bold        = abap_true.
  lo_style->font->italic      = abap_false.
  lo_style->font->name        = zcl_excel_style_font=>c_name_calibri.
  lo_style->font->scheme      = zcl_excel_style_font=>c_scheme_none.
  lo_style->font->color-rgb   = zcl_excel_style_color=>c_black.
  lo_style->fill->bgcolor-rgb = zcl_excel_style_color=>c_gray.
  lo_style->borders->allborders = lo_border_dark.
  DATA(lo_style_guid)               = lo_style->get_guid( ).
  lo_style_bold_border = lo_excel->add_new_style( ).
  lo_style_bold_border->font->name = zcl_excel_style_font=>c_name_cambria.
  lo_style_bold_border->font->size = 13.
  lo_style_bold_border->font->bold = abap_true.
  lo_style_bold_border->font->italic = abap_true.
  lo_style_bold_border->font->color-rgb = zcl_excel_style_color=>c_black.
  lo_style_bold_border->fill->bgcolor-rgb = zcl_excel_style_color=>c_yellow.
  lo_style_bold_border->alignment->horizontal = zcl_excel_style_alignment=>c_horizontal_center.
  lo_style_bold_border->borders->allborders = lo_border_dark.
  lv_style_bold_border_guid = lo_style_bold_border->get_guid( ).

  lo_worksheet->zif_excel_sheet_properties~selected = zif_excel_sheet_properties=>c_selected.
  "Material Data in SHEET 1.
************************************Initial DATA***********************************Begin of Sheet 1
  lo_worksheet->set_area(  ip_row = 1  ip_column_start = 'A' ip_column_end = 'C' ip_style = lv_style_bold_border_guid
                             ip_value = 'Initial Data' ip_merge = abap_true ).
  lo_worksheet->change_cell_style(  ip_column               = 'A'
                                    ip_row                  = 1
                                    ip_fill_fgcolor_rgb     = zcl_excel_style_color=>c_yellow
                                    ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
  lo_worksheet->change_cell_style(  ip_column               = 'B'
                                    ip_row                  = 1
                                    ip_fill_fgcolor_rgb     = zcl_excel_style_color=>c_yellow
                                    ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
  lo_worksheet->change_cell_style(  ip_column               = 'C'
                                    ip_row                  = 1
                                    ip_fill_fgcolor_rgb     = zcl_excel_style_color=>c_yellow
                                    ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).

*** Material
  lo_worksheet->set_cell( ip_column = 'A' ip_row = 2 ip_value = 'Material' ip_style = lo_style_guid  ).
  lo_worksheet->change_cell_style(  ip_column               = 'A'
                                    ip_row                  = 2
                                    ip_fill_fgcolor_rgb     = 'C3B8B6'
                                    ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
  lo_column = lo_worksheet->get_column( ip_column = 'A' ).
  lo_column->set_width( ip_width = 20 ).
  lo_worksheet->calculate_column_widths( ).

***  Industry Sector MBRSH
  lo_worksheet->set_cell( ip_column = 'B' ip_row = 2 ip_value = 'Industry Sector' ip_style = lo_style_guid  ).
  lo_worksheet->change_cell_style(  ip_column               = 'B'
                                    ip_row                  = 2
                                    ip_fill_fgcolor_rgb     = 'C3B8B6'
                                    ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).

  lo_column = lo_worksheet->get_column( ip_column = 'B' ).
  lo_column->set_auto_size( ip_auto_size = abap_true ).
  lo_worksheet->calculate_column_widths( ).
****  Material type MTART
  lo_worksheet->set_cell( ip_column = 'C' ip_row = 2 ip_value = 'Material Type' ip_style = lo_style_guid  ).
  lo_worksheet->change_cell_style(  ip_column               = 'C'
                                    ip_row                  = 2
                                    ip_fill_fgcolor_rgb     = 'C3B8B6'
                                    ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
  lo_column = lo_worksheet->get_column( ip_column = 'C' ).
  lo_column->set_width( ip_width = 20 ).
  lo_worksheet->calculate_column_widths( ).
************************************Organization Data***********************************
  lo_worksheet->set_area(  ip_row = 1  ip_column_start = 'D' ip_column_end = 'G' ip_style = lv_style_bold_border_guid
                             ip_value = 'Organization Data' ip_merge = abap_true ).
  lo_worksheet->change_cell_style(  ip_column               = 'D'
                                    ip_row                  = 1
                                    ip_fill_fgcolor_rgb     = zcl_excel_style_color=>c_yellow
                                    ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
  lo_worksheet->change_cell_style(  ip_column               = 'E'
                                    ip_row                  = 1
                                    ip_fill_fgcolor_rgb     = zcl_excel_style_color=>c_yellow
                                    ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
  lo_worksheet->change_cell_style(  ip_column               = 'F'
                                    ip_row                  = 1
                                    ip_fill_fgcolor_rgb     = zcl_excel_style_color=>c_yellow
                                    ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
  lo_worksheet->change_cell_style(  ip_column               = 'G'
                                    ip_row                  = 1
                                    ip_fill_fgcolor_rgb     = zcl_excel_style_color=>c_yellow
                                    ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
***  Plant WERKS
  lo_worksheet->set_cell( ip_column = 'D' ip_row = 2 ip_value = 'Plant' ip_style = lo_style_guid  ).
  lo_worksheet->change_cell_style(  ip_column               = 'D'
                                    ip_row                  = 2
                                    ip_fill_fgcolor_rgb     = 'C3B8B6'
                                    ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
  lo_column = lo_worksheet->get_column( ip_column = 'D' ).
  lo_column->set_auto_size( ip_auto_size = abap_true ).
  lo_worksheet->calculate_column_widths( ).
***  Sales Organization VKORG
  lo_worksheet->set_cell( ip_column = 'E' ip_row = 2 ip_value = 'Sales Org.' ip_style = lo_style_guid  ).
  lo_worksheet->change_cell_style(  ip_column               = 'E'
                                    ip_row                  = 2
                                    ip_fill_fgcolor_rgb     = 'C3B8B6'
                                    ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
  lo_column = lo_worksheet->get_column( ip_column = 'E' ).
  lo_column->set_auto_size( ip_auto_size = abap_true ).
  lo_worksheet->calculate_column_widths( ).
***  Distribution Channel VTWEG
  lo_worksheet->set_cell( ip_column = 'F' ip_row = 2 ip_value = 'Distribution Chnl' ip_style = lo_style_guid  ).
  lo_worksheet->change_cell_style(  ip_column               = 'F'
                                    ip_row                  = 2
                                    ip_fill_fgcolor_rgb     = 'C3B8B6'
                                    ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
  lo_column = lo_worksheet->get_column( ip_column = 'F' ).
  lo_column->set_auto_size( ip_auto_size = abap_true ).
  lo_worksheet->calculate_column_widths( ).
***  Storage location LGORT
  lo_worksheet->set_cell( ip_column = 'G' ip_row = 2 ip_value = 'Storage Loc.' ip_style = lo_style_guid  ).
  lo_worksheet->change_cell_style(  ip_column               = 'G'
                                    ip_row                  = 2
                                    ip_fill_fgcolor_rgb     = 'C3B8B6'
                                    ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
  lo_column = lo_worksheet->get_column( ip_column = 'G' ).
  lo_column->set_auto_size( ip_auto_size = abap_true ).
  lo_worksheet->calculate_column_widths( ).
************************************Basic Data1***********************************
  lo_worksheet->set_area(  ip_row = 1  ip_column_start = 'H' ip_column_end = 'V' ip_style = lv_style_bold_border_guid
                             ip_value = 'Basic Data' ip_merge = abap_true ).
  lo_worksheet->change_cell_style(  ip_column               = 'H'
                                    ip_row                  = 1
                                    ip_fill_fgcolor_rgb     = zcl_excel_style_color=>c_yellow
                                    ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
  lo_worksheet->change_cell_style(  ip_column               = 'I'
                                    ip_row                  = 1
                                    ip_fill_fgcolor_rgb     = zcl_excel_style_color=>c_yellow
                                    ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
  lo_worksheet->change_cell_style(  ip_column               = 'J'
                                    ip_row                  = 1
                                    ip_fill_fgcolor_rgb     = zcl_excel_style_color=>c_yellow
                                    ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
  lo_worksheet->change_cell_style(  ip_column               = 'K'
                                    ip_row                  = 1
                                    ip_fill_fgcolor_rgb     = zcl_excel_style_color=>c_yellow
                                    ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
  lo_worksheet->change_cell_style(  ip_column               = 'L'
                                   ip_row                  = 1
                                   ip_fill_fgcolor_rgb     = zcl_excel_style_color=>c_yellow
                                   ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
  lo_worksheet->change_cell_style(  ip_column               = 'M'
                                   ip_row                  = 1
                                   ip_fill_fgcolor_rgb     = zcl_excel_style_color=>c_yellow
                                   ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
  lo_worksheet->change_cell_style(  ip_column               = 'N'
                                   ip_row                  = 1
                                   ip_fill_fgcolor_rgb     = zcl_excel_style_color=>c_yellow
                                   ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
  lo_worksheet->change_cell_style(  ip_column               = 'O'
                                   ip_row                  = 1
                                   ip_fill_fgcolor_rgb     = zcl_excel_style_color=>c_yellow
                                   ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
  lo_worksheet->change_cell_style(  ip_column               = 'P'
                                    ip_row                  = 1
                                    ip_fill_fgcolor_rgb     = zcl_excel_style_color=>c_yellow
                                    ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
  lo_worksheet->change_cell_style(  ip_column               = 'Q'
                                    ip_row                  = 1
                                    ip_fill_fgcolor_rgb     = zcl_excel_style_color=>c_yellow
                                    ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
  lo_worksheet->change_cell_style(  ip_column               = 'R'
                                    ip_row                  = 1
                                    ip_fill_fgcolor_rgb     = zcl_excel_style_color=>c_yellow
                                    ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
  lo_worksheet->change_cell_style(  ip_column               = 'S'
                                    ip_row                  = 1
                                    ip_fill_fgcolor_rgb     = zcl_excel_style_color=>c_yellow
                                    ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
  lo_worksheet->change_cell_style(  ip_column               = 'T'
                                    ip_row                  = 1
                                    ip_fill_fgcolor_rgb     = zcl_excel_style_color=>c_yellow
                                    ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
  lo_worksheet->change_cell_style(  ip_column               = 'U'
                                    ip_row                  = 1
                                    ip_fill_fgcolor_rgb     = zcl_excel_style_color=>c_yellow
                                    ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
  lo_worksheet->change_cell_style(  ip_column               = 'V'
                                    ip_row                  = 1
                                    ip_fill_fgcolor_rgb     = zcl_excel_style_color=>c_yellow
                                    ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
***  Material Description MAKTX
  lo_worksheet->set_cell( ip_column = 'H' ip_row = 2 ip_value = 'Material Description' ip_style = lo_style_guid  ).
  lo_worksheet->change_cell_style(  ip_column               = 'H'
                                    ip_row                  = 2
                                    ip_fill_fgcolor_rgb     = 'C3B8B6'
                                    ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
  lo_column = lo_worksheet->get_column( ip_column = 'H' ).
  lo_column->set_width( ip_width = 50 ).
*  CATCH zcx_excel.
  lo_worksheet->calculate_column_widths( ).
***  Basic uom MEINS
  lo_worksheet->set_cell( ip_column = 'I' ip_row = 2 ip_value = 'Base UOM' ip_style = lo_style_guid  ).
  lo_worksheet->change_cell_style(  ip_column               = 'I'
                                    ip_row                  = 2
                                    ip_fill_fgcolor_rgb     = 'C3B8B6'
                                    ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
  lo_column = lo_worksheet->get_column( ip_column = 'I' ).
  lo_column->set_auto_size( ip_auto_size = abap_true ).
  lo_worksheet->calculate_column_widths( ).
***  Material Group MATKL
  lo_worksheet->set_cell( ip_column = 'J' ip_row = 2 ip_value = 'Material Group' ip_style = lo_style_guid  ).
  lo_worksheet->change_cell_style(  ip_column               = 'J'
                                    ip_row                  = 2
                                    ip_fill_fgcolor_rgb     = 'C3B8B6'
                                    ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
  lo_column = lo_worksheet->get_column( ip_column = 'J' ).
  lo_column->set_auto_size( ip_auto_size = abap_true ).
  lo_worksheet->calculate_column_widths( ).
***  Old Material BISMT
  lo_worksheet->set_cell( ip_column = 'K' ip_row = 2 ip_value = 'Old Matnr(BISMT)' ip_style = lo_style_guid  ).
  lo_worksheet->change_cell_style(  ip_column               = 'K'
                                    ip_row                  = 2
                                    ip_fill_fgcolor_rgb     = 'C3B8B6'
                                    ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
  lo_column = lo_worksheet->get_column( ip_column = 'K' ).
  lo_column->set_auto_size( ip_auto_size = abap_true ).
  lo_worksheet->calculate_column_widths( ).
***  Divison SPART
  lo_worksheet->set_cell( ip_column = 'L' ip_row = 2 ip_value = 'Divison' ip_style = lo_style_guid  ).
  lo_worksheet->change_cell_style(  ip_column               = 'L'
                                    ip_row                  = 2
                                    ip_fill_fgcolor_rgb     = 'C3B8B6'
                                    ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
  lo_column = lo_worksheet->get_column( ip_column = 'L' ).
  lo_column->set_auto_size( ip_auto_size = abap_true ).
  lo_worksheet->calculate_column_widths( ).
***  Payment Term ZTERM
  lo_worksheet->set_cell( ip_column = 'M' ip_row = 2 ip_value = 'Pay Term' ip_style = lo_style_guid  ).
  lo_worksheet->change_cell_style(  ip_column               = 'M'
                                    ip_row                  = 2
                                    ip_fill_fgcolor_rgb     = 'C3B8B6'
                                    ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
  lo_column = lo_worksheet->get_column( ip_column = 'M' ).
  lo_column->set_auto_size( ip_auto_size = abap_true ).
  lo_worksheet->calculate_column_widths( ).
***  Gross Weight
  lo_worksheet->set_cell( ip_column = 'N' ip_row = 2 ip_value = 'Gross Weight' ip_style = lo_style_guid  ).
  lo_worksheet->change_cell_style(  ip_column               = 'N'
                                    ip_row                  = 2
                                    ip_fill_fgcolor_rgb     = 'C3B8B6'
                                    ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
  lo_column = lo_worksheet->get_column( ip_column = 'N' ).
  lo_column->set_auto_size( ip_auto_size = abap_true ).
  lo_worksheet->calculate_column_widths( ).
***  Weight Unit UOM
  lo_worksheet->set_cell( ip_column = 'O' ip_row = 2 ip_value = 'Weight Unit' ip_style = lo_style_guid  ).
  lo_worksheet->change_cell_style(  ip_column               = 'O'
                                    ip_row                  = 2
                                    ip_fill_fgcolor_rgb     = 'C3B8B6'
                                    ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
  lo_column = lo_worksheet->get_column( ip_column = 'O' ).
  lo_column->set_auto_size( ip_auto_size = abap_true ).
  lo_worksheet->calculate_column_widths( ).
***  Net Weight
  lo_worksheet->set_cell( ip_column = 'P' ip_row = 2 ip_value = 'Net Weight' ip_style = lo_style_guid  ).
  lo_worksheet->change_cell_style(  ip_column               = 'P'
                                    ip_row                  = 2
                                    ip_fill_fgcolor_rgb     = 'C3B8B6'
                                    ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
  lo_column = lo_worksheet->get_column( ip_column = 'P' ).
  lo_column->set_auto_size( ip_auto_size = abap_true ).
  lo_worksheet->calculate_column_widths( ).
***  Volume
  lo_worksheet->set_cell( ip_column = 'Q' ip_row = 2 ip_value = 'Volume' ip_style = lo_style_guid  ).
  lo_worksheet->change_cell_style(  ip_column               = 'Q'
                                    ip_row                  = 2
                                    ip_fill_fgcolor_rgb     = 'C3B8B6'
                                    ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
  lo_column = lo_worksheet->get_column( ip_column = 'Q' ).
  lo_column->set_auto_size( ip_auto_size = abap_true ).
  lo_worksheet->calculate_column_widths( ).
***  Volume Unit
  lo_worksheet->set_cell( ip_column = 'R' ip_row = 2 ip_value = 'Volume Unit' ip_style = lo_style_guid  ).
  lo_worksheet->change_cell_style(  ip_column               = 'R'
                                    ip_row                  = 2
                                    ip_fill_fgcolor_rgb     = 'C3B8B6'
                                    ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
  lo_column = lo_worksheet->get_column( ip_column = 'R' ).
  lo_column->set_auto_size( ip_auto_size = abap_true ).
  lo_worksheet->calculate_column_widths( ).
***  Size/Dimensions
  lo_worksheet->set_cell( ip_column = 'S' ip_row = 2 ip_value = 'Size/Dimensions' ip_style = lo_style_guid  ).
  lo_worksheet->change_cell_style(  ip_column               = 'S'
                                    ip_row                  = 2
                                    ip_fill_fgcolor_rgb     = 'C3B8B6'
                                    ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
  lo_column = lo_worksheet->get_column( ip_column = 'S' ).
  lo_column->set_auto_size( ip_auto_size = abap_true ).
  lo_worksheet->calculate_column_widths( ).
***  Ecomm Name
  lo_worksheet->set_cell( ip_column = 'T' ip_row = 2 ip_value = 'Ecomm Name' ip_style = lo_style_guid  ).
  lo_worksheet->change_cell_style(  ip_column               = 'T'
                                    ip_row                  = 2
                                    ip_fill_fgcolor_rgb     = 'C3B8B6'
                                    ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
  lo_column = lo_worksheet->get_column( ip_column = 'T' ).
  lo_column->set_auto_size( ip_auto_size = abap_true ).
  lo_worksheet->calculate_column_widths( ).
***  Ecomm Category
  lo_worksheet->set_cell( ip_column = 'U' ip_row = 2 ip_value = 'Ecomm Category' ip_style = lo_style_guid  ).
  lo_worksheet->change_cell_style(  ip_column               = 'U'
                                    ip_row                  = 2
                                    ip_fill_fgcolor_rgb     = 'C3B8B6'
                                    ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
  lo_column = lo_worksheet->get_column( ip_column = 'U' ).
  lo_column->set_auto_size( ip_auto_size = abap_true ).
  lo_worksheet->calculate_column_widths( ).
***  Ecomm Material
  lo_worksheet->set_cell( ip_column = 'V' ip_row = 2 ip_value = 'Ecomm Material' ip_style = lo_style_guid  ).
  lo_worksheet->change_cell_style(  ip_column               = 'V'
                                    ip_row                  = 2
                                    ip_fill_fgcolor_rgb     = 'C3B8B6'
                                    ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
  lo_column = lo_worksheet->get_column( ip_column = 'V' ).
  lo_column->set_width( ip_width = 20 ).
  lo_worksheet->calculate_column_widths( ).
************************************Classification View***********************************
  lo_worksheet->set_area(  ip_row = 1  ip_column_start = 'W' ip_column_end = 'Y' ip_style = lv_style_bold_border_guid
                             ip_value = 'Classification View' ip_merge = abap_true ).
  lo_worksheet->change_cell_style(  ip_column               = 'W'
                                    ip_row                  = 1
                                    ip_fill_fgcolor_rgb     = zcl_excel_style_color=>c_yellow
                                    ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
  lo_worksheet->change_cell_style(  ip_column               = 'X'
                                    ip_row                  = 1
                                    ip_fill_fgcolor_rgb     = zcl_excel_style_color=>c_yellow
                                    ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
  lo_worksheet->change_cell_style(  ip_column               = 'Y'
                                      ip_row                  = 1
                                      ip_fill_fgcolor_rgb     = zcl_excel_style_color=>c_yellow
                                      ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
*** Class Type
  lo_worksheet->set_cell( ip_column = 'W' ip_row = 2 ip_value = 'Class Type' ip_style = lo_style_guid  ).
  lo_worksheet->change_cell_style(  ip_column               = 'W'
                                    ip_row                  = 2
                                    ip_fill_fgcolor_rgb     = 'C3B8B6'
                                    ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
  lo_column = lo_worksheet->get_column( ip_column = 'W' ).
  lo_column->set_auto_size( ip_auto_size = abap_true ).
  lo_worksheet->calculate_column_widths( ).
*** Class
  lo_worksheet->set_cell( ip_column = 'X' ip_row = 2 ip_value = 'Class' ip_style = lo_style_guid  ).
  lo_worksheet->change_cell_style(  ip_column               = 'X'
                                    ip_row                  = 2
                                    ip_fill_fgcolor_rgb     = 'C3B8B6'
                                    ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
  lo_column = lo_worksheet->get_column( ip_column = 'X' ).
  lo_column->set_width( ip_width = 20 ).
  lo_worksheet->calculate_column_widths( ).
*** Created On
  lo_worksheet->set_cell( ip_column = 'Y' ip_row = 2 ip_value = 'Created On' ip_style = lo_style_guid  ).
  lo_worksheet->change_cell_style(  ip_column               = 'Y'
                                    ip_row                  = 2
                                    ip_fill_fgcolor_rgb     = 'C3B8B6'
                                    ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
  lo_column = lo_worksheet->get_column( ip_column = 'Y' ).
  lo_column->set_auto_size( ip_auto_size = abap_true ).
  lo_worksheet->calculate_column_widths( ).
************************************ Sales: General/Plant***********************************
  lo_worksheet->set_area(  ip_row = 1  ip_column_start = 'Z' ip_style = lv_style_bold_border_guid
                            ip_value = 'Sales: General/Plant' ip_merge = abap_true ).
  lo_worksheet->change_cell_style(  ip_column               = 'Z'
                                    ip_row                  = 1
                                    ip_fill_fgcolor_rgb     = zcl_excel_style_color=>c_yellow
                                    ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
  lo_worksheet->set_column_width(
    EXPORTING
      ip_column         = 'Z'
      ip_width_fix      = 23
*    ip_width_autosize = 'X'
  ).
*** Profit Centre
  lo_worksheet->set_cell( ip_column = 'Z' ip_row = 2 ip_value = 'Profit Centre' ip_style = lo_style_guid  ).
  lo_worksheet->change_cell_style(  ip_column               = 'Z'
                                    ip_row                  = 2
                                    ip_fill_fgcolor_rgb     = 'C3B8B6'
                                    ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
*     lo_column = lo_worksheet->get_column( ip_column = 'Z' ).
*  lo_column->set_auto_size( ip_auto_size = abap_true ).
*    lo_worksheet->calculate_column_widths( ).
************************************Foreign Trade Import***********************************
  lo_worksheet->set_area(  ip_row = 1  ip_column_start = 'AA' ip_column_end = 'AA' ip_style = lv_style_bold_border_guid
                             ip_value = 'Foreign Trade Import' ip_merge = abap_true  ).
  lo_worksheet->change_cell_style(  ip_column               = 'AA'
                                      ip_row                  = 1
                                      ip_fill_fgcolor_rgb     = zcl_excel_style_color=>c_yellow
                                      ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
*** HSN Code
  lo_worksheet->set_cell( ip_column = 'AA' ip_row = 2 ip_value = 'HSN Code' ip_style = lo_style_guid  ).
  lo_worksheet->change_cell_style(  ip_column               = 'AA'
                                    ip_row                  = 2
                                    ip_fill_fgcolor_rgb     = 'C3B8B6'
                                    ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
  lo_column = lo_worksheet->get_column( ip_column = 'AA' ).
  lo_column->set_width( ip_width = 22 ).
  lo_worksheet->calculate_column_widths( ).
************************************Plant Storage data1***********************************
  lo_worksheet->set_area(  ip_row = 1  ip_column_start = 'AB' ip_column_end = 'AC' ip_style = lv_style_bold_border_guid
                             ip_value = 'Plant Storage data' ip_merge = abap_true  ).
*    CATCH zcx_excel.
  lo_worksheet->change_cell_style(  ip_column               = 'AB'
                                    ip_row                  = 1
                                    ip_fill_fgcolor_rgb     = zcl_excel_style_color=>c_yellow
                                    ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
  lo_worksheet->change_cell_style(  ip_column               = 'AC'
                                    ip_row                  = 1
                                    ip_fill_fgcolor_rgb     = zcl_excel_style_color=>c_yellow
                                    ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
*** Minimum Shelf Life
  lo_worksheet->set_cell( ip_column = 'AB' ip_row = 2 ip_value = 'Min. Shelf Life' ip_style = lo_style_guid  ).
  lo_worksheet->change_cell_style(  ip_column               = 'AB'
                                    ip_row                  = 2
                                    ip_fill_fgcolor_rgb     = 'C3B8B6'
                                    ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
  lo_column = lo_worksheet->get_column( ip_column = 'AB' ).
  lo_column->set_auto_size( ip_auto_size = abap_true ).
  lo_worksheet->calculate_column_widths( ).
*** Maximum Shelf Life
  lo_worksheet->set_cell( ip_column = 'AC' ip_row = 2 ip_value = 'Max. Shelf Life' ip_style = lo_style_guid  ).
  lo_worksheet->change_cell_style(  ip_column               = 'AC'
                                    ip_row                  = 2
                                    ip_fill_fgcolor_rgb     = 'C3B8B6'
                                    ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
  lo_column = lo_worksheet->get_column( ip_column = 'AC' ).
  lo_column->set_auto_size( ip_auto_size = abap_true ).
  lo_worksheet->calculate_column_widths( ).
************************************Accounting1***********************************  \
  lo_worksheet->set_area(  ip_row = 1  ip_column_start = 'AD' ip_column_end = 'AD' ip_style = lv_style_bold_border_guid
                             ip_value = 'Accounting' ip_merge = abap_true ).
  lo_worksheet->change_cell_style(  ip_column               = 'AD'
                                    ip_row                  = 1
                                    ip_fill_fgcolor_rgb     = zcl_excel_style_color=>c_yellow
                                    ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
*** Maximum Shelf Life
  lo_worksheet->set_cell( ip_column = 'AD' ip_row = 2 ip_value = 'Standard Price' ip_style = lo_style_guid  ).
  lo_worksheet->change_cell_style(  ip_column               = 'AD'
                                    ip_row                  = 2
                                    ip_fill_fgcolor_rgb     = 'C3B8B6'
                                    ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
  lo_column = lo_worksheet->get_column( ip_column = 'AD' ).
  lo_column->set_auto_size( ip_auto_size = abap_true ).
  lo_worksheet->calculate_column_widths( ).
*******************************************************************************************End of Sheet 1
  lo_worksheet->freeze_panes(
    EXPORTING
      ip_num_rows    = 1
  ).

  lv_tabcolor-rgb = zcl_excel_style_color=>create_new_argb( ip_red   = 'FF'
                                                            ip_green = '4C'
                                                            ip_blu   = '4C' ).
  lo_worksheet->set_tabcolor( lv_tabcolor ).

*  lo_hyperlink = zcl_excel_hyperlink=>create_internal_link( iv_location = 'Sheet2!B2' ).
*  lo_worksheet->set_cell( ip_column = 'B' ip_row = 3 ip_value = 'This is link to second sheet' ip_hyperlink = lo_hyperlink ).

  " Page printing settings
  lo_worksheet->sheet_setup->set_page_margins( ip_header = '1' ip_footer = '1' ip_unit = 'cm' ).
  lo_worksheet->sheet_setup->black_and_white   = 'X'.
  lo_worksheet->sheet_setup->fit_to_page       = 'X'.  " you should turn this on to activate fit_to_height and fit_to_width
  lo_worksheet->sheet_setup->fit_to_height     = 0.    " used only if ip_fit_to_page = 'X'
  lo_worksheet->sheet_setup->fit_to_width      = 2.    " used only if ip_fit_to_page = 'X'
  lo_worksheet->sheet_setup->orientation       = zcl_excel_sheet_setup=>c_orientation_landscape.
  lo_worksheet->sheet_setup->page_order        = zcl_excel_sheet_setup=>c_ord_downthenover.
  lo_worksheet->sheet_setup->paper_size        = zcl_excel_sheet_setup=>c_papersize_a4.
  lo_worksheet->sheet_setup->scale             = 80.   " used only if ip_fit_to_page = SPACE

  " Header and Footer
  ls_header-right_value = 'print date &D'.
  ls_header-right_font-size = 8.
  ls_header-right_font-name = zcl_excel_style_font=>c_name_arial.

  ls_footer-left_value = '&Z&F'. "Path / Filename
  ls_footer-left_font = ls_header-right_font.
  ls_footer-right_value = 'page &P of &N'. "page x of y
  ls_footer-right_font = ls_header-right_font.

  lo_worksheet->sheet_setup->set_header_footer( ip_odd_header  = ls_header
                                                ip_odd_footer  = ls_footer ).


  lo_worksheet = lo_excel->add_new_worksheet( ).
  lo_worksheet->set_title( ip_title = 'Alternate UOM' ).
* Set color to tab with sheetname   - Green
  lv_tabcolor-rgb = zcl_excel_style_color=>create_new_argb( ip_red   = '00'
                                                            ip_green = 'FF'
                                                            ip_blu   = '00' ).
  lo_worksheet->set_tabcolor( lv_tabcolor ).
  lo_worksheet->zif_excel_sheet_properties~selected = zif_excel_sheet_properties=>c_selected.
*****************************************************************************************************Begin of Sheet 2
  "Material Data: AUom in SHEET 2.
  lo_worksheet->set_area(  ip_row = 1  ip_column_start = 'A' ip_column_end = 'E' ip_style = lv_style_bold_border_guid
                           ip_value = 'Alternate Unit of Measurements' ip_merge = abap_true ).
  lo_worksheet->change_cell_style(  ip_column               = 'A'
                                     ip_row                  = 1
                                     ip_fill_fgcolor_rgb     = zcl_excel_style_color=>c_yellow
                                     ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
  lo_worksheet->change_cell_style(  ip_column               = 'B'
                                     ip_row                  = 1
                                     ip_fill_fgcolor_rgb     = zcl_excel_style_color=>c_yellow
                                     ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
  lo_worksheet->change_cell_style(  ip_column               = 'C'
                                     ip_row                  = 1
                                     ip_fill_fgcolor_rgb     = zcl_excel_style_color=>c_yellow
                                     ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
  lo_worksheet->change_cell_style(  ip_column               = 'D'
                                    ip_row                  = 1
                                    ip_fill_fgcolor_rgb     = zcl_excel_style_color=>c_yellow
                                    ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
  lo_worksheet->change_cell_style(  ip_column               = 'E'
                                    ip_row                  = 1
                                    ip_fill_fgcolor_rgb     = zcl_excel_style_color=>c_yellow
                                    ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
*** Material
  lo_worksheet->set_cell( ip_column = 'A' ip_row = 2 ip_value = 'Material' ip_style = lo_style_guid  ).
  lo_worksheet->change_cell_style(  ip_column               = 'A'
                                    ip_row                  = 2
                                    ip_fill_fgcolor_rgb     = 'C3B8B6'
                                    ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
  lo_column = lo_worksheet->get_column( ip_column = 'A' ).
  lo_column->set_width( ip_width = 20 ).
  lo_worksheet->calculate_column_widths( ).
  lo_worksheet->set_cell( ip_column = 'A' ip_row = 3 ip_value = 'Enter Material'  ).
*** Denominator
  lo_worksheet->set_cell( ip_column = 'B' ip_row = 2 ip_value = 'Denominator' ip_style = lo_style_guid  ).
  lo_worksheet->change_cell_style(  ip_column               = 'B'
                                    ip_row                  = 2
                                    ip_fill_fgcolor_rgb     = 'C3B8B6'
                                    ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
  lo_column = lo_worksheet->get_column( ip_column = 'B' ).
  lo_column->set_width( ip_width = 20 ).
  lo_worksheet->calculate_column_widths( ).

  lo_worksheet->set_cell( ip_column = 'B' ip_row = 3 ip_value = 7  ).
  lo_worksheet->set_cell( ip_column = 'B' ip_row = 4 ip_value = 2  ).
  lo_worksheet->set_cell( ip_column = 'B' ip_row = 5 ip_value = 4  ).
*** Alt. Uom
  lo_worksheet->set_cell( ip_column = 'C' ip_row = 2 ip_value = 'Alternate UOM' ip_style = lo_style_guid  ).
  lo_worksheet->change_cell_style(  ip_column               = 'C'
                                    ip_row                  = 2
                                    ip_fill_fgcolor_rgb     = 'C3B8B6'
                                    ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
  lo_column = lo_worksheet->get_column( ip_column = 'C' ).
  lo_column->set_width( ip_width = 20 ).
  lo_worksheet->calculate_column_widths( ).

  lo_worksheet->set_cell( ip_column = 'C' ip_row = 3 ip_value = 'KG'  ).
  lo_worksheet->set_cell( ip_column = 'C' ip_row = 4 ip_value = 'BT'  ).
  lo_worksheet->set_cell( ip_column = 'C' ip_row = 5 ip_value = 'L'  ).

*** Numerator
  lo_worksheet->set_cell( ip_column = 'D' ip_row = 2 ip_value = 'Numerator' ip_style = lo_style_guid  ).
  lo_worksheet->change_cell_style(  ip_column               = 'D'
                                    ip_row                  = 2
                                    ip_fill_fgcolor_rgb     = 'C3B8B6'
                                    ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
  lo_column = lo_worksheet->get_column( ip_column = 'D' ).
  lo_column->set_width( ip_width = 20 ).
  lo_worksheet->calculate_column_widths( ).

  lo_worksheet->set_cell( ip_column = 'D' ip_row = 3 ip_value = 1  ).
  lo_worksheet->set_cell( ip_column = 'D' ip_row = 4 ip_value = 1  ).
  lo_worksheet->set_cell( ip_column = 'D' ip_row = 5 ip_value = 1  ).
*** Base Unit
  lo_worksheet->set_cell( ip_column = 'E' ip_row = 2 ip_value = 'Base Unit' ip_style = lo_style_guid  ).
  lo_worksheet->change_cell_style(  ip_column               = 'E'
                                    ip_row                  = 2
                                    ip_fill_fgcolor_rgb     = 'C3B8B6'
                                    ip_fill_filltype        = zcl_excel_style_fill=>c_fill_gradient_diagonal45 ).
  lo_column = lo_worksheet->get_column( ip_column = 'E' ).
  lo_column->set_width( ip_width = 20 ).
  lo_worksheet->calculate_column_widths( ).

  lo_worksheet->set_cell( ip_column = 'E' ip_row = 3 ip_value = 'EA'  ).
  lo_worksheet->set_cell( ip_column = 'E' ip_row = 4 ip_value = 'EA'  ).
  lo_worksheet->set_cell( ip_column = 'E' ip_row = 5 ip_value = 'EA'  ).

*****************************************************************************************************End of Sheet2
  lo_excel->set_active_sheet_index_by_name( 'Materials' ).

ENDFORM.
