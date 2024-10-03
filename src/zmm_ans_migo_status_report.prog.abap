*&---------------------------------------------------------------------*
*& Report ZMM_ANS_MIGO_STATUS_REPORT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmm_ans_migo_status_report.
TABLES: zmm_ge_asn_migo.

SELECT-OPTIONS s_asnno FOR zmm_ge_asn_migo-asnno.
SELECT-OPTIONS s_lifnr FOR zmm_ge_asn_migo-lifnr.
SELECT-OPTIONS s_werks FOR zmm_ge_asn_migo-werks.
SELECT-OPTIONS s_gdate FOR zmm_ge_asn_migo-gdate OBLIGATORY.

INITIALIZATION.
  s_gdate-low = sy-datum.
  s_gdate-high = sy-datum.
  APPEND s_gdate.


START-OF-SELECTION.

  PERFORM p_display_alv.
*&---------------------------------------------------------------------*
*& Form p_display_alv
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM p_display_alv .
  DATA: lo_gr_functions TYPE REF TO cl_salv_functions_list.

  DATA lo_gr_alv       TYPE REF TO cl_salv_table. " Variables for ALV properties

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
  TYPES: BEGIN OF ty_output,
           mblnr    TYPE    mblnr,
           asnno    TYPE    zasnno,
           lifnr    TYPE    lifnr,
           werks    TYPE    werks_d,
           matnr    TYPE    matnr,
           charg    TYPE    charg_d,
           menge    TYPE    zasnqty,
           asnds    TYPE    zasndesc,
           orbyr    TYPE    zorbyr,
           vbeln    TYPE    zasninv,
           ddate    TYPE    zasndate,
           vehno    TYPE    zasnvehno,
           intim    TYPE    zvehno_intime,
           outim    TYPE    zvehno_outime,
           name1    TYPE    name1_gp,
           ort01    TYPE    ort01_gp,
*           mblnr    TYPE    mblnr,
*           asnno    TYPE    zasnno,
*           lifnr    TYPE    lifnr,
*           werks    TYPE    werks_d,
           ebeln    TYPE    ebeln,
           ebelp    TYPE    ebelp,
           matnr_po TYPE    matnr,
           charg_po TYPE    charg_d,
           lfimg_po TYPE    lfimg,
           lgort_po TYPE    lgort_d,
           bedat    TYPE    bedat,
           gdate    TYPE    zgpdate,
           gtime    TYPE    zgptime,
           usnam    TYPE    usnam,
         END OF ty_output.
  DATA lt_output TYPE STANDARD TABLE OF ty_output.
  DATA lw_output TYPE ty_output.
  SELECT * FROM zmm_ge_asn
           INTO TABLE @DATA(lt_asn)
           WHERE asnno IN @s_asnno[] AND
                 lifnr IN @s_lifnr[] AND
                 werks IN @s_werks[] AND
                 gdate BETWEEN @s_gdate-low AND @s_gdate-high.
  IF lt_asn IS NOT INITIAL.
    SELECT * FROM zmm_ge_asn_migo
             INTO TABLE @DATA(lt_migo)
             FOR ALL ENTRIES IN @lt_asn
             WHERE mblnr = @lt_asn-mblnr AND
                   asnno = @lt_asn-asnno AND
                   lifnr = @lt_asn-lifnr AND
                   werks = @lt_asn-werks AND
                   matnr = @lt_asn-matnr .

    LOOP AT lt_migo INTO DATA(lw_migo).
*        WHERE
*                                             mblnr = lw_asn-mblnr AND
*                                             asnno = lw_asn-asnno AND
*                                             lifnr = lw_asn-lifnr AND
*                                             werks = lw_asn-werks AND
*                                             matnr = lw_asn-matnr .

      READ TABLE lt_asn INTO DATA(lw_asn) WITH KEY  mblnr = lw_migo-mblnr
                                                    asnno = lw_migo-asnno
                                                    lifnr = lw_migo-lifnr
                                                    werks = lw_migo-werks
                                                    matnr = lw_migo-matnr
                                                    charg = lw_migo-charg_v.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING lw_asn TO lw_output.
      ENDIF.
      lw_output-ebeln    = lw_migo-ebeln.
      lw_output-ebelp       = lw_migo-ebelp.
      lw_output-matnr_po     = lw_migo-matnr.
      lw_output-charg_po     = lw_migo-charg.
      lw_output-lfimg_po     = lw_migo-lfimg.
      lw_output-lgort_po     = lw_migo-lgort.
      lw_output-bedat        = lw_migo-bedat.
      APPEND lw_output TO lt_output.
    ENDLOOP.
  ENDIF.
  IF lt_output IS INITIAL.
    MESSAGE 'No Data Found' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT .
  ENDIF.
* create the alv object
  TRY.
      CALL METHOD cl_salv_table=>factory
        IMPORTING
          r_salv_table = lo_gr_alv
        CHANGING
          t_table      = lt_output.


*      lo_gr_alv->set_screen_status(
*        pfstatus      =  'SAL_STATUS'
*        report        =  sy-repid
*        set_functions = lo_gr_alv->c_functions_all ).

* Let's show all default buttons of ALV
      lo_gr_functions = lo_gr_alv->get_functions( ).
      lo_gr_functions->set_all( value  = if_salv_c_bool_sap=>true ).

* Fit the columns
      lo_columns = lo_gr_alv->get_columns( ).
      lo_columns->set_optimize( 'X' ).

      lr_groups = lo_gr_alv->get_sorts( ) .
      lr_groups->clear( ).

      TRY.
          lr_groups->add_sort(
         columnname = 'MBLNR'
         position   = 1
         subtotal   = abap_true
         sequence   = if_salv_c_sort=>sort_up ).

        CATCH cx_salv_not_found cx_salv_data_error cx_salv_existing.
      ENDTRY.
      TRY.
          lr_groups->add_sort(
         columnname = 'ASNNO'
         position   = 2
         subtotal   = abap_true
         sequence   = if_salv_c_sort=>sort_up ).

        CATCH cx_salv_not_found cx_salv_data_error cx_salv_existing.
      ENDTRY.
      TRY.
          lr_groups->add_sort(
         columnname = 'LIFNR'
         position   = 3
         subtotal   = abap_true
         sequence   = if_salv_c_sort=>sort_up ).

        CATCH cx_salv_not_found cx_salv_data_error cx_salv_existing.
      ENDTRY.
      TRY.
          lr_groups->add_sort(
         columnname = 'WERKS'
         position   = 4
         subtotal   = abap_true
         sequence   = if_salv_c_sort=>sort_up ).

        CATCH cx_salv_not_found cx_salv_data_error cx_salv_existing.
      ENDTRY.
      TRY.
          lr_groups->add_sort(
         columnname = 'MATNR'
         position   = 5
         subtotal   = abap_true
         sequence   = if_salv_c_sort=>sort_up ).

        CATCH cx_salv_not_found cx_salv_data_error cx_salv_existing.
      ENDTRY.
*      TRY.
*          lr_groups->add_sort(
*         columnname = 'CHARG'
*         position   = 6
*         subtotal   = abap_true
*         sequence   = if_salv_c_sort=>sort_up ).
*
*        CATCH cx_salv_not_found cx_salv_data_error cx_salv_existing.
*      ENDTRY.
*      TRY.
*          lr_groups->add_sort(
*         columnname = 'MENGE'
*         position   = 6
*         subtotal   = abap_true
*         sequence   = if_salv_c_sort=>sort_up ).
*
*        CATCH cx_salv_not_found cx_salv_data_error cx_salv_existing.
*      ENDTRY.

* Apply zebra style to lv_rows
      lo_display = lo_gr_alv->get_display_settings( ).
      lo_display->set_striped_pattern( cl_salv_display_settings=>true ).

      TRY.
          lo_column ?= lo_columns->get_column( 'CHARG' ).
          lo_column->set_visible( if_salv_c_bool_sap=>true ).
          lo_column->set_long_text( 'Vendor Batch' ).
          lo_column->set_medium_text( 'Vendor Batch' ).
          lo_column->set_short_text( 'VendBatch' ).
        CATCH cx_salv_not_found.
        CATCH cx_salv_existing.
        CATCH cx_salv_data_error.
      ENDTRY.
*
*    TRY.
*        lo_column ?= lo_columns->get_column( 'TYPE' ).
*        lo_column->set_visible( if_salv_c_bool_sap=>true ).
*        lo_column->set_long_text( 'Msgtyp' ).
*        lo_column->set_medium_text( 'Msgtyp' ).
*        lo_column->set_short_text( 'Msgtyp' ).
*      CATCH cx_salv_not_found.
*      CATCH cx_salv_existing.
*      CATCH cx_salv_data_error.
*    ENDTRY.
*
*    TRY.
*        lo_column ?= lo_columns->get_column( 'MESSAGE' ).
*        lo_column->set_visible( if_salv_c_bool_sap=>true ).
*        lo_column->set_long_text( 'Message' ).
*        lo_column->set_medium_text( 'Message' ).
*        lo_column->set_short_text( 'Message' ).
*      CATCH cx_salv_not_found.
*      CATCH cx_salv_existing.
*      CATCH cx_salv_data_error.
*    ENDTRY.
      lo_gr_alv->display( ).
    CATCH cx_salv_msg.
  ENDTRY.
ENDFORM.
