
*       Batchinputdata of single transaction
DATA:   bdcdata LIKE bdcdata    OCCURS 0 WITH HEADER LINE.
*       messages of call transaction
DATA:   messtab LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
*       error session opened (' ' or 'X')
DATA:   e_group_opened.
*       message texts
TABLES: t100.

CLASS lcl_handle_events DEFINITION. " Variables for events
  PUBLIC SECTION.

    METHODS:
      on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function. "Event for User command
ENDCLASS.
CLASS lcl_handle_events IMPLEMENTATION.

  METHOD on_user_command.
    PERFORM handle_user_command USING e_salv_function.
  ENDMETHOD.
ENDCLASS.
FORM f4_value_help USING p_name TYPE rlgrap-filename.
  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    EXPORTING
      program_name  = syst-repid
      dynpro_number = syst-dynnr
      field_name    = ' '
      static        = ' '
      mask          = ' '
    CHANGING
      file_name     = p_name.
ENDFORM.

FORM excel_conversion USING p_name TYPE rlgrap-filename.
  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
      i_line_header        = 'X'
      i_tab_raw_data       = gt_raw
      i_filename           = p_name
    TABLES
      i_tab_converted_data = gt_excel
    EXCEPTIONS
      conversion_failed    = 1
      OTHERS               = 2.
  IF sy-subrc <> 0.
    MESSAGE 'Excel Conversion Error' TYPE 'E'.
  ENDIF.
ENDFORM.

FORM validate_excel_data.
  REFRESH gt_alv.
  LOOP AT gt_excel ASSIGNING FIELD-SYMBOL(<fgs_excel>).
*--------- Material Number Checks on Material Master ---------*
    CLEAR lv_matnr.
    SELECT SINGLE matnr FROM marc INTO lv_matnr WHERE matnr = <fgs_excel>-matnr AND werks = <fgs_excel>-werks .
    IF sy-subrc NE 0.
      APPEND VALUE #( matnr = <fgs_excel>-matnr
                      werks = <fgs_excel>-werks
                      stlal = <fgs_excel>-stlal
                      type = 'E'
                      message = |Material Number { <fgs_excel>-matnr } Does not esits| ) TO gt_alv.
      CONTINUE.
    ENDIF.
*------------- Alternate BOM Existence Checks ---------------------------------------------*
    SELECT SINGLE * FROM mast INTO @DATA(ls_mast) WHERE matnr = @<fgs_excel>-matnr AND werks = @<fgs_excel>-werks
                                                  AND stlan = '1' AND stlal = @<fgs_excel>-stlal.
    IF sy-subrc = 0.
      APPEND VALUE #( matnr = <fgs_excel>-matnr
                      werks = <fgs_excel>-werks
                      stlal = <fgs_excel>-stlal
                     type = 'E'
                     message = |For Material Number { <fgs_excel>-matnr } already Alternate BOM { <fgs_excel>-stlal } exists| ) TO gt_alv.
      CONTINUE.
    ENDIF.

*---- Material Components existence checks -----------------------------------------------*
    CLEAR: lv_matnr.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input  = <fgs_excel>-idnrk
      IMPORTING
        output = lv_material.
*--------- Component Material Checks and Deletion Indicator Checks ---------------------------------------------------------*
    SELECT SINGLE * FROM marc INTO @DATA(ls_marc) WHERE matnr = @lv_material AND werks = @<fgs_excel>-werks.
    IF sy-subrc NE 0.
      APPEND VALUE #( matnr = <fgs_excel>-matnr
                      werks = <fgs_excel>-werks
                      stlal = <fgs_excel>-stlal
                      idnrk = <fgs_excel>-idnrk
                      type = 'E'
                      message = |Componet { <fgs_excel>-idnrk } Not Maintained in Plant { <fgs_excel>-werks }| ) TO gt_alv.
      CONTINUE.
    ELSE."Deletion Indicator Checks
      IF ls_marc-lvorm = abap_true.
        APPEND VALUE #( matnr = <fgs_excel>-matnr
                        werks = <fgs_excel>-werks
                        stlal = <fgs_excel>-stlal
                        idnrk = <fgs_excel>-idnrk
                        type = 'E'
                        message = |Componet { <fgs_excel>-idnrk } is Flagged for Deletion in plant { <fgs_excel>-werks }| ) TO gt_alv.
        CONTINUE.
      ENDIF.
    ENDIF.

    SELECT SINGLE meins FROM mara INTO @DATA(lv_uom) WHERE matnr = @lv_material AND meins = @<fgs_excel>-item_uom.
    IF sy-subrc NE 0.
      SELECT SINGLE ausme FROM marc INTO lv_uom WHERE matnr = lv_material AND ausme = <fgs_excel>-item_uom.
      IF sy-subrc NE 0.
        SELECT SINGLE meinh FROM marm INTO lv_uom WHERE matnr = lv_material AND meinh = <fgs_excel>-item_uom.
        IF sy-subrc NE 0.
          APPEND VALUE #( matnr = <fgs_excel>-matnr
                          werks = <fgs_excel>-werks
                          stlal = <fgs_excel>-stlal
                          idnrk = <fgs_excel>-idnrk
                          type = 'E'
                          message = |Componet { <fgs_excel>-idnrk } Unit of Measurement { <fgs_excel>-item_uom } is incorrect| ) TO gt_alv.
          CONTINUE.
        ENDIF.
      ENDIF.
    ENDIF.

  APPEND VALUE #( matnr = <fgs_excel>-matnr
                  werks = <fgs_excel>-werks
                  stlal = <fgs_excel>-stlal
                  base_qty = <fgs_excel>-base_qty
                  idnrk = <fgs_excel>-idnrk
                  item_qty = <fgs_excel>-item_qty
                  item_uom = <fgs_excel>-item_uom
                  type = 'S'
                  message = |Ready to Use in BOM| ) TO gt_alv.
ENDLOOP.
SORT gt_alv[] BY matnr werks stlal.
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
          t_table      = gt_alv.
    CATCH cx_salv_msg.
  ENDTRY.


  lo_gr_alv->set_screen_status(
    pfstatus      =  'BOM_UPDATE'
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

  TRY.
      lo_column ?= lo_columns->get_column( 'TYPE' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Type' ).
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
FORM handle_user_command USING i_ucomm TYPE salv_de_function.

  CASE i_ucomm.
    WHEN '&BOM_UPD'.

      IF test_run EQ abap_false.
        PERFORM bom_updation.
      ELSE.
        LOOP AT gt_alv ASSIGNING FIELD-SYMBOL(<fs_final>) WHERE type = 'S'.
          <fs_final>-type = 'S'.
          <fs_final>-message = |Bom Updated Successfully|.
        ENDLOOP.
        lo_gr_alv->refresh( ).
      ENDIF.

  ENDCASE.
ENDFORM.

FORM bom_updation.
  DATA: lv_datum TYPE char10.
  DATA lv_msg_text TYPE string.
  CLEAR lv_datum.
  WRITE sy-datum TO lv_datum DD/MM/YYYY.
  DATA(lt_alv) = gt_alv[].
  DELETE ADJACENT DUPLICATES FROM lt_alv COMPARING matnr werks stlal.
  LOOP AT lt_alv ASSIGNING FIELD-SYMBOL(<fs_alv>) WHERE type = 'S'.

    READ TABLE gt_alv INTO DATA(gs_alv) WITH KEY matnr = <fs_alv>-matnr
                                                 werks = <fs_alv>-werks
                                                 stlal = <fs_alv>-stlal
                                                 type = 'E'.
    IF sy-subrc = 0.
      CONTINUE.
    ENDIF.

    PERFORM bdc_dynpro      USING 'SAPLCSDI' '0100'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'RC29N-STLAL'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM bdc_field       USING 'RC29N-MATNR'
                                  <fs_alv>-matnr.
    PERFORM bdc_field       USING 'RC29N-WERKS'
                                  <fs_alv>-werks.
    PERFORM bdc_field       USING 'RC29N-STLAN'
                                  '1'.
    PERFORM bdc_field       USING 'RC29N-STLAL'
                                  <fs_alv>-stlal.
    PERFORM bdc_field       USING 'RC29N-DATUV'
                                  lv_datum.
    PERFORM bdc_dynpro      USING 'SAPLCSDI' '0110'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM bdc_field       USING 'RC29K-BMENG'
                                  <fs_alv>-base_qty.
    PERFORM bdc_field       USING 'RC29K-STLST'
                                  '1'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'RC29K-EXSTL'.
    PERFORM bdc_field       USING 'RC29K-EXSTL'
                                  ''.
    PERFORM bdc_dynpro      USING 'SAPLCSDI' '0111'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'RC29K-LABOR'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '/00'.

*    DELETE lt_excel WHERE matnr NE <fs_excel>-matnr.
    lv_refitem = '01'.
    DATA lv_component_c TYPE char20 VALUE 'RC29P-IDNRK'.
    DATA lv_menge_c TYPE char20 VALUE 'RC29P-MENGE'.
    DATA lv_meins_c TYPE char20 VALUE 'RC29P-MEINS'.
    DATA lv_icat_c TYPE char20 VALUE 'RC29P-POSTP'.



    PERFORM bdc_dynpro      USING 'SAPLCSDI' '0140'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'RC29P-MEINS(01)'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '/00'.

    LOOP AT gt_alv ASSIGNING FIELD-SYMBOL(<fls_alv>) WHERE matnr = <fs_alv>-matnr
                                                         AND werks = <fs_alv>-werks
                                                         AND stlal = <fs_alv>-stlal
                                                         AND type = 'S'.
      DATA(lv_component) = |{ lv_component_c }({ lv_refitem })|.
      CONDENSE lv_component.

      DATA(lv_menge) = | { lv_menge_c }({ lv_refitem })|.
      CONDENSE lv_menge.

      DATA(lv_meins) = | { lv_meins_c }({ lv_refitem })|.
      CONDENSE lv_meins.

      DATA(lv_icat) = | { lv_icat_c }({ lv_refitem })|.
      CONDENSE lv_icat.

      lv_qty = <fs_alv>-item_qty.
      CONDENSE lv_qty.


      PERFORM bdc_field       USING lv_component
                                    <fls_alv>-idnrk.

      PERFORM bdc_field       USING lv_menge
                                    lv_qty.

      PERFORM bdc_field       USING lv_meins
                                    <fls_alv>-item_uom.

      PERFORM bdc_field       USING lv_icat
                                    'L'.
      lv_refitem = lv_refitem + 1.
    ENDLOOP.

    DATA l_item TYPE num4.
    l_item = 10.

    LOOP AT gt_alv ASSIGNING FIELD-SYMBOL(<fls_excel1>) WHERE matnr = <fs_alv>-matnr
                                                          AND werks = <fs_alv>-werks
                                                          AND stlal = <fs_alv>-stlal
                                                          AND type = 'S' .
      CLEAR lv_qty.
      lv_qty =  <fls_excel1>-item_qty.
      PERFORM bdc_dynpro      USING 'SAPLCSDI' '0130'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '/00'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'RC29P-POSNR'.
      PERFORM bdc_field       USING 'RC29P-POSNR'
                                    l_item.
      PERFORM bdc_field       USING 'RC29P-IDNRK'
                                    <fls_excel1>-idnrk.
      PERFORM bdc_field       USING 'RC29P-MENGE'
                                    lv_qty.
      PERFORM bdc_field       USING 'RC29P-MEINS'
                                    <fls_excel1>-item_uom.
      PERFORM bdc_dynpro      USING 'SAPLCSDI' '0131'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '/00'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'RC29P-POTX1'.
      PERFORM bdc_field       USING 'RC29P-SANKA'
                                    'X'.
      PERFORM bdc_dynpro      USING 'SAPLCSDI' '0138'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '/00'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'STPO-ZZMFGINS'.
      PERFORM bdc_dynpro      USING 'SAPLCSDI' '0140'.
*      PERFORM bdc_field       USING 'RC29P-POSTP(02)'
*                                    'L'.
      l_item = l_item + 10.
    ENDLOOP.

    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=FCBU'.
    PERFORM bdc_transaction USING 'CS01'.

    READ TABLE messtab ASSIGNING FIELD-SYMBOL(<fs_messtab>) WITH KEY msgtyp = 'E'.
    IF sy-subrc = 0.
      LOOP AT gt_alv ASSIGNING FIELD-SYMBOL(<fs_error>) WHERE matnr = <fs_alv>-matnr
                                                        AND werks = <fs_alv>-werks
                                                        AND stlal = <fs_alv>-stlal
                                                        AND type = 'S'.
        LOOP AT messtab INTO DATA(lw_messtab) WHERE msgtyp = 'E'.
          CLEAR lv_msg_text.
          CALL FUNCTION 'FORMAT_MESSAGE'
            EXPORTING
              id        = sy-msgid
              lang      = '-D'
              no        = sy-msgno
              v1        = sy-msgv1
              v2        = sy-msgv2
              v3        = sy-msgv3
              v4        = sy-msgv4
            IMPORTING
              msg       = lv_msg_text
            EXCEPTIONS
              not_found = 1
              OTHERS    = 2.
          IF sy-subrc = 0.
            <fs_error>-message = | { <fs_error>-message } { lv_msg_text }|.
          ENDIF.
        ENDLOOP.
        <fs_error>-type = 'E'.
      ENDLOOP.
    ELSE.
      LOOP AT gt_alv ASSIGNING FIELD-SYMBOL(<fs_final>) WHERE matnr = <fs_alv>-matnr
                                                        AND werks = <fs_alv>-werks
                                                        AND stlal = <fs_alv>-stlal
                                                        AND type = 'S'.
        <fs_final>-type = 'S'.
        <fs_final>-message = |Bom Updated Successfully|.
      ENDLOOP.
    ENDIF.
    REFRESH: bdcdata[],messtab[].
  ENDLOOP.
  lo_gr_alv->refresh( ).
ENDFORM.
*----------------------------------------------------------------------*
*        Start new transaction according to parameters                 *
*----------------------------------------------------------------------*
FORM bdc_transaction USING tcode.
  DATA: l_mstring(480).
  DATA: l_subrc LIKE sy-subrc.
  DATA: lx_auth_check TYPE REF TO cx_root.
  DATA: l_auth_check_text TYPE string.
  REFRESH messtab.
  TRY.
      CALL TRANSACTION tcode WITH AUTHORITY-CHECK USING bdcdata
                       MODE  'N'
                       UPDATE 'S'
                       MESSAGES INTO messtab.
    CATCH cx_sy_authorization_error INTO lx_auth_check.
*     Authorization missing for user when executing transaction
      l_auth_check_text = lx_auth_check->get_text( ).
      sy-subrc = 99.
  ENDTRY.
*  COMMIT WORK AND WAIT.
  l_subrc = sy-subrc.
ENDFORM.

*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.
  CLEAR bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = dynpro.
  bdcdata-dynbegin = 'X'.
  APPEND bdcdata.
ENDFORM.

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  APPEND bdcdata.
ENDFORM.
