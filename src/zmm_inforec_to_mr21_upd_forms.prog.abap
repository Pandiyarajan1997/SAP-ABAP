*&---------------------------------------------------------------------*
*& Include          ZMM_INFOREC_TO_MR21_UPD_FORMS
*&---------------------------------------------------------------------*
FORM f_get_open_po USING p_werks TYPE werks_d.
**** OPEN purchase order details fetching FUNCTION module ***
  DATA: lr_bsart TYPE RANGE OF ekko-bsart,
        lr_werks TYPE RANGE OF ekpo-werks.
*** For which document TYPE we need to EXTRACT open po ***
  SELECT * FROM tvarvc INTO TABLE @DATA(lt_tvarvc)
                WHERE name = 'ZMR21_DOC_TYPE'
                AND type = 'S'.
  IF sy-subrc = 0.
    LOOP AT lt_tvarvc INTO DATA(ls_tvarv).
      APPEND VALUE #(  sign = 'I'
                       option = 'EQ'
                       low = ls_tvarv-low ) TO lr_bsart.
    ENDLOOP.
  ENDIF.
  lr_werks = VALUE #( ( sign = 'I'
                        option = 'EQ'
                        low = p_from ) ).
**************************************************************
  REFRESH: gt_open_po,gt_open_pt.
  CALL FUNCTION 'ZGET_OPEN_PO_DETAILS'
    EXPORTING
      in_bsart  = lr_bsart
      in_plant  = lr_werks
      flag      = 'X'
    TABLES
      t_open_po = gt_open_po.
  IF sy-subrc = 0.
    SORT gt_open_po[] BY matnr.
    DELETE gt_open_po[] WHERE matnr IS INITIAL.
    gt_open_pt[] = gt_open_po[].
    DELETE ADJACENT DUPLICATES FROM gt_open_po[] COMPARING matnr.
  ENDIF.
ENDFORM.
FORM f_initial_selection.
  REFRESH: gt_mara,gt_marc_fr,gt_marc_to,gt_mbew.
*** Fetching material master data ***
  SELECT matnr
         mtart FROM mara
               INTO TABLE gt_mara
               WHERE matnr IN s_matnr
               AND mtart IN s_mtype.
  IF sy-subrc EQ 0.
    SORT gt_mara[] BY matnr mtart.
**** final material list for which the price is going to update ***
    SELECT * FROM zmm_mat_list
             INTO TABLE gt_mm_plant
             FOR ALL ENTRIES IN gt_mara
             WHERE werks = p_from
             AND matnr = gt_mara-matnr.
    IF sy-subrc = 0.
      SORT gt_mm_plant[] BY werks matnr.
*** Material Description ****
      SELECT * FROM makt INTO TABLE gt_makt
               FOR ALL ENTRIES IN gt_mm_plant
               WHERE matnr = gt_mm_plant-matnr
               AND spras = sy-langu.
      IF sy-subrc EQ 0.
        SORT gt_makt[] BY matnr.
      ENDIF.
*** selecting material input from plant material DATA **
      SELECT matnr
             werks FROM marc
                   INTO TABLE gt_marc_fr
                   FOR ALL ENTRIES IN gt_mm_plant
                   WHERE matnr EQ gt_mm_plant-matnr
                   AND werks EQ p_from.
      IF sy-subrc EQ 0.
        SORT gt_marc_fr[] BY matnr.
** selecting input to plant material Data ***
        SELECT matnr
               werks FROM marc
                     INTO TABLE gt_marc_to
                     FOR ALL ENTRIES IN gt_marc_fr
                     WHERE matnr EQ gt_marc_fr-matnr
                     AND werks EQ p_to.
        IF sy-subrc EQ 0.
          SORT gt_marc_to[] BY matnr.
**** Fetching the moving average price and price control ***
          SELECT matnr
                 bwkey
                 vprsv
                 verpr FROM mbew
                       INTO TABLE gt_mbew
                       FOR ALL ENTRIES IN gt_marc_to
                       WHERE matnr = gt_marc_to-matnr
                       AND bwkey = p_from.
          IF sy-subrc = 0.
            SORT gt_mbew[] BY matnr.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.
FORM f_price_calculation USING p_matnr TYPE matnr.
***** Calling fm to get as on day stock for calculation ****
  DATA: lr_matnr TYPE RANGE OF mara-matnr.
  DATA: lv_totstk  TYPE labst,
        lv_totval  TYPE salk3,
        lv_remarks TYPE string.
  DATA: lv_totqty TYPE labst.
  DATA: lv_totpr TYPE salk3.

  DATA: lv_postk TYPE menge_d,
        lv_poval TYPE netwr.
  DATA: lv_price TYPE salk3.
*** Preparing Material input table for Function module ***
  APPEND VALUE #(  sign = 'I'
                    option = 'EQ'
                    low = p_matnr ) TO lr_matnr.

*** Actual FM call for stock *****
  REFRESH: gt_stock,gt_stock_t.
  CALL FUNCTION 'ZBAPI_SAFETY_STOCK_MIS'
    EXPORTING
      plant           = p_from
    TABLES
      it_stock        = gt_stock
      it_matnr        = lr_matnr
    EXCEPTIONS
      incorrect_plant = 1
      OTHERS          = 2.
  IF sy-subrc = 0.
    SORT gt_stock[] BY material.
    DELETE gt_stock[] WHERE unrestricted_stk = '0'.
    gt_stock_t[] = gt_stock[].
    DELETE ADJACENT DUPLICATES FROM gt_stock[] COMPARING material.
    LOOP AT gt_stock INTO DATA(ls_stock).
      CLEAR: lv_totstk,lv_totval.
      LOOP AT gt_stock_t INTO DATA(ls_stock_t) WHERE material = ls_stock-material.
        lv_totstk = ( lv_totstk + ls_stock_t-unrestricted_stk ).
        lv_totval = ( lv_totval + ls_stock_t-unrestrict_value ).
      ENDLOOP.
    ENDLOOP.
  ENDIF.
  IF gt_open_po IS NOT INITIAL.
***** Final price calculation preparation ***
    LOOP AT gt_open_po INTO DATA(gs_open_po) WHERE matnr = p_matnr.
      CLEAR: lv_remarks.
      LOOP AT gt_open_pt INTO DATA(gs_open_pt) WHERE matnr = gs_open_po-matnr.
        lv_postk = ( lv_postk + gs_open_pt-po_bal_quan ).
        lv_poval = ( lv_poval + gs_open_pt-po_bal_amount ).
        lv_remarks = | { lv_remarks } { '|' } { gs_open_pt-ebeln } |. "Remarks
      ENDLOOP.
      CONDENSE lv_remarks NO-GAPS.
      CLEAR: lv_totqty,lv_totpr.
      lv_totqty = ( lv_totstk + lv_postk ). "Total Qty
      lv_totpr = ( lv_totval + lv_poval ). "Total Value
      CLEAR lv_price.
      lv_price = ( lv_totpr / lv_totqty ). "Final Price
    ENDLOOP.
  ENDIF.
  IF lv_price = '0.00'.
    CLEAR lv_price.
    lv_price = VALUE #( gt_mbew[ matnr = p_matnr ]-verpr OPTIONAL ).
  ENDIF.
*** ALV Preparation and updation ****
  APPEND VALUE #(  matnr = p_matnr
                   matdes = VALUE #( gt_makt[ matnr = p_matnr ]-maktx OPTIONAL )
                   f_werks = p_from
                   t_werks = p_to
                   vprsv = VALUE #( gt_mbew[ matnr = p_matnr ]-vprsv OPTIONAL )
                   unstk = lv_totstk
                   unval = lv_totval
                   poqty = lv_postk
                   poval = lv_poval
                   fprice = lv_price
                   update = 'X'
                   message = 'Price to Update'
                   remarks = lv_remarks  )  TO gt_cal_price.

ENDFORM.
FORM f_price_upd_or_disp USING p_type TYPE char01.
  DATA: lv_date TYPE char10.
  DATA: lv_pri_mr21 TYPE char13.
  DATA: lv_msg TYPE string.

  IF p_type = 'X'.
*** Only Display ***
    PERFORM f_display_alv.
  ELSE.
    CLEAR lv_date.
    WRITE sy-datum TO lv_date DD/MM/YYYY.
**** Updation if no display means ***
    LOOP AT gt_cal_price INTO DATA(ls_price_upd) WHERE update = 'X'.
      gv_org = '1000'.
      CLEAR lv_pri_mr21.
      lv_pri_mr21 = ls_price_upd-fprice.

      it_bdcdata = VALUE #(
        ( program = 'SAPRCKM_MR21'            dynpro = '0201'   dynbegin = 'X' )
        ( fnam = 'BDC_CURSOR'                 fval = 'MR21HEAD-WERKS' )
        ( fnam = 'BDC_OKCODE'                 fval = '=ENTR' )
        ( fnam = 'MR21HEAD-BUDAT'             fval = lv_date )
        ( fnam = 'MR21HEAD-BUKRS'             fval = gv_org )
        ( fnam = 'MR21HEAD-WERKS'             fval = ls_price_upd-t_werks )
        ( fnam = 'MR21HEAD-SCREEN_VARIANT'    fval = 'MR21_LAGERMATERIAL_0250' ) ).

      APPEND VALUE #( program = 'SAPRCKM_MR21'  dynpro = '0201'   dynbegin = 'X' ) TO it_bdcdata.
      APPEND VALUE #( fnam = 'BDC_OKCODE'                  fval = '=ENTR' ) TO it_bdcdata.
      APPEND VALUE #( fnam = 'BDC_CURSOR'                  fval = 'CKI_MR21_0250-NEWVALPR(01)' ) TO it_bdcdata.
      APPEND VALUE #( fnam = 'MR21HEAD-SCREEN_VARIANT'     fval = 'MR21_LAGERMATERIAL_BWKEY_0250' ) TO it_bdcdata.
      APPEND VALUE #( fnam = 'CKI_MR21_0250-MATNR(01)'     fval = ls_price_upd-matnr ) TO it_bdcdata.
      APPEND VALUE #( fnam = 'CKI_MR21_0250-NEWVALPR(01)'  fval = lv_pri_mr21 ) TO it_bdcdata.

      APPEND VALUE #( program = 'SAPRCKM_MR21'  dynpro = '0201'   dynbegin = 'X' ) TO it_bdcdata.
      APPEND VALUE #( fnam = 'BDC_OKCODE'                  fval = '=SAVE' ) TO it_bdcdata.
      APPEND VALUE #( fnam = 'BDC_CURSOR'                  fval = 'CKI_MR21_0250-MATNR(02)' ) TO it_bdcdata.
      APPEND VALUE #( fnam = 'MR21HEAD-SCREEN_VARIANT'     fval = 'MR21_LAGERMATERIAL_BWKEY_0250' ) TO it_bdcdata.

      APPEND VALUE #( program = 'SAPLSPO1'  dynpro = '0100'   dynbegin = 'X' ) TO it_bdcdata.
      APPEND VALUE #( fnam = 'BDC_OKCODE'      fval = '=YES' ) TO it_bdcdata.

      PERFORM bdc_transaction USING 'MR21'.
      READ TABLE gt_bdcmsg INTO DATA(gs_bdc) WITH KEY msgtyp = 'E'.
      IF sy-subrc = 0.
        LOOP AT gt_bdcmsg INTO DATA(ls_msg) WHERE msgtyp = 'E'.
          CLEAR lv_msg.
          CALL FUNCTION 'FORMAT_MESSAGE'
            EXPORTING
              id        = ls_msg-msgid
              lang      = sy-langu
              no        = ls_msg-msgnr
              v1        = ls_msg-msgv1
              v2        = ls_msg-msgv2
              v3        = ls_msg-msgv3
              v4        = ls_msg-msgv4
            IMPORTING
              msg       = lv_msg
            EXCEPTIONS
              not_found = 01.
          ls_price_upd-message = |{ ls_price_upd-message } { lv_msg }|.
        ENDLOOP.
      ELSE.
        ls_price_upd-message = 'Price Updated'.
      ENDIF.
      MODIFY gt_cal_price FROM ls_price_upd.
      CLEAR:it_bdcdata[], gt_bdcmsg[].
    ENDLOOP.
**** After Updation ALV Display *****
    PERFORM f_display_alv.
  ENDIF.
ENDFORM.
FORM bdc_dynpro  USING  program TYPE any
                        dynpro TYPE any.

  CLEAR wa_bdcdata.
  wa_bdcdata-program  = program.
  wa_bdcdata-dynpro   = dynpro.
  wa_bdcdata-dynbegin = 'X'.
  APPEND wa_bdcdata TO it_bdcdata.

ENDFORM.                    " BDC_DYNPRO
*----------------------------------------------------------------------*
FORM bdc_field  USING fnam TYPE any
                      fval TYPE any.

  CLEAR wa_bdcdata.
  wa_bdcdata-fnam = fnam.
  wa_bdcdata-fval = fval.
  APPEND wa_bdcdata TO it_bdcdata.

ENDFORM.
FORM bdc_transaction  USING  tcode TYPE any.
  DATA(ls_options) = VALUE ctu_params(  dismode = p_mode
                                        updmode = 'S'
                                        defsize = ''
                                        nobinpt = 'X'
                                        racommit = 'X' ).
  TRY.
      CALL TRANSACTION tcode WITH AUTHORITY-CHECK
                            USING it_bdcdata
                            OPTIONS FROM ls_options
                            MESSAGES INTO gt_bdcmsg.
    CATCH cx_sy_authorization_error.
  ENDTRY.
ENDFORM.
FORM f_display_alv.
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

* Create the ALV object
  TRY.
      CALL METHOD cl_salv_table=>factory
        IMPORTING
          r_salv_table = lo_gr_alv
        CHANGING
          t_table      = gt_cal_price.
    CATCH cx_salv_msg.
  ENDTRY.
* Let's show all default buttons of ALV
  lo_gr_functions = lo_gr_alv->get_functions( ).
  lo_gr_functions->set_all( abap_true ).

* Fit the columns
  lo_columns = lo_gr_alv->get_columns( ).
  lo_columns->set_optimize( 'X' ).
* Apply zebra style to lv_rows
  lo_display = lo_gr_alv->get_display_settings( ).
  lo_display->set_striped_pattern( cl_salv_display_settings=>true ).

  TRY.
      lo_column ?= lo_columns->get_column( 'F_WERKS' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'From plant' ).
      lo_column->set_medium_text( 'From plant' ).
      lo_column->set_short_text( 'From plant' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'T_WERKS' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'To plant' ).
      lo_column->set_medium_text( 'To plant' ).
      lo_column->set_short_text( 'To plant' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'FPRICE' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Price' ).
      lo_column->set_medium_text( 'Price' ).
      lo_column->set_short_text( 'Price' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'UNSTK' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Unrstd Stk' ).
      lo_column->set_medium_text( 'Unrstd Stk' ).
      lo_column->set_short_text( 'Unrstd Stk' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'UNVAL' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Unrestricted vale' ).
      lo_column->set_medium_text( 'Unrstd value' ).
      lo_column->set_short_text( 'Unstk val' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'POQTY' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Open PoQty' ).
      lo_column->set_medium_text( 'Open PoQty' ).
      lo_column->set_short_text( 'Open PoQty' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'POVAL' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Povalue' ).
      lo_column->set_medium_text( 'Povalue' ).
      lo_column->set_short_text( 'Povalue' ).
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
      lo_column ?= lo_columns->get_column( 'REMARKS' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Remarks' ).
      lo_column->set_medium_text( 'Remarks' ).
      lo_column->set_short_text( 'Remarks' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'UPDATE' ).
      lo_column->set_visible( if_salv_c_bool_sap=>false ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.
  lo_gr_alv->display( ).
ENDFORM.
