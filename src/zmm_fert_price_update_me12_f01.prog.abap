*&---------------------------------------------------------------------*
*& Include          ZMM_FERT_PRICE_UPDATE_ME12_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form f_initial_screen
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_initial_screen .
  LOOP AT SCREEN.
    IF screen-name = 'P_MTYPE'.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_popup_confirm
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_popup_confirm .
  IF p_disp IS INITIAL.
    IF r1 = abap_true.
      DATA(lv_confirm_text) = |Please confirm copying the finished product price from Plant { p_werks } to Plant { p_werks2 } |.
    ELSE.
      lv_confirm_text = |Please confirm copying the Sales Org material price from  Plant { p_werks } to Plant { p_werks2 } |.
    ENDIF.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Confirmation '
        text_question         = lv_confirm_text
        text_button_1         = 'Yes'
        text_button_2         = 'No'
        default_button        = '2'
        display_cancel_button = 'X'
      IMPORTING
        answer                = gv_return " to hold the FM's return value
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    IF gv_return EQ '1'.
      p_disp = ''.
    ELSE.
      p_disp = 'X'.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_records_from_selection
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_process_for_purchase_org.

  DATA lv_unit_price TYPE char15.
  SELECT a~matnr,
         c~maktx,
         b~werks,
         d~vprsv, "Price Control Indicator
         d~verpr, "Moving Average Price
         d~stprs  "Standard price
  INTO TABLE @DATA(lt_plant_data)
  FROM mara AS a
    INNER JOIN marc AS b ON a~matnr = b~matnr
    INNER JOIN makt AS c ON a~matnr = c~matnr
    INNER JOIN mbew AS d ON c~matnr = d~matnr AND
                            b~werks = d~bwkey
  WHERE a~matnr IN @s_matnr AND
        a~mtart = @p_mtype AND
        c~spras = @sy-langu AND
        d~bwkey = @p_werks AND
        d~vprsv IN ('S','V').
  CHECK lt_plant_data IS NOT INITIAL.
  SORT lt_plant_data BY matnr werks.


*  SELECT matnr,
*         werks,
*         lifnr,
*         ekorg,
*         meins,
*         bdatu
*          FROM eord INTO TABLE @DATA(lt_vendor_list) "Source list
*    FOR ALL ENTRIES IN @lt_plant_data
*    WHERE matnr = @lt_plant_data-matnr AND
*          werks = @p_werks2 AND
*          bdatu GE @sy-datum AND
*          vdatu LE @sy-datum.
*
*  SORT lt_vendor_list BY matnr werks lifnr ASCENDING bdatu DESCENDING.
*  DELETE ADJACENT DUPLICATES FROM lt_vendor_list COMPARING matnr werks lifnr.

  " Get Supplier from Plant

  SELECT * FROM zmm_plant_supp
    INTO TABLE gt_supplier_list WHERE plant = p_werks.

  IF gt_supplier_list IS INITIAL.
    DATA(lv_err) = `Supplier is not maintained in table ZMM_PLANT_SUPP`.
    MESSAGE lv_err TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  DATA lr_supplier TYPE RANGE OF elifn.

  lr_supplier = VALUE #(  FOR <ls_supplier> IN gt_supplier_list
                              ( sign = 'I'
                                option = 'EQ'
                                low = <ls_supplier>-lifnr )
                        ).
  " Get Inforecords from Plant, Purchase Org, Vendor and Material.
  SELECT a~infnr,
         a~matnr,
         a~lifnr,
         b~werks,
         b~ekorg,
         c~datbi,
         c~datab
  INTO TABLE @DATA(lt_inforecords)
  FROM eina AS a INNER  JOIN eine AS b ON a~infnr = b~infnr
                        JOIN a017 AS c ON a~matnr = c~matnr
*                                      AND a~lifnr IN lr_supplier
                  FOR ALL ENTRIES IN @lt_plant_data
                  WHERE a~matnr = @lt_plant_data-matnr
                      AND a~lifnr IN @lr_supplier
                      AND b~ekorg = @p_ekorg
                      AND b~werks = @p_werks2
                      AND c~kappl = 'M'
                      AND c~kschl = 'P000'
                      AND c~lifnr IN @lr_supplier
                      AND c~matnr = @lt_plant_data-matnr
                      AND c~ekorg = @p_ekorg
                      AND c~werks = @p_werks2
                      AND c~esokz = '0'.
*                      AND c~datbi GE @sy-datum
*                      AND c~datab LE @sy-datum.

  SORT lt_inforecords BY matnr lifnr ASCENDING datbi DESCENDING.
  DELETE ADJACENT DUPLICATES FROM lt_inforecords COMPARING infnr matnr lifnr werks ekorg.
  SORT lt_inforecords BY matnr lifnr werks ekorg.

  LOOP AT lt_plant_data INTO DATA(lw_plant_data).
    CLEAR: gw_output.
    MOVE-CORRESPONDING lw_plant_data TO gw_output.
    " Read Supplier ID
    gw_output-lifnr = gt_supplier_list[ 1 ]-lifnr.
    " Read Inforecord details
    gw_output-infnr = VALUE #( lt_inforecords[ matnr = lw_plant_data-matnr
                                               lifnr = gw_output-lifnr
                                               werks = p_werks2
                                               ekorg = p_ekorg ]-infnr OPTIONAL ).
    gw_output-datbi = VALUE #( lt_inforecords[ matnr = lw_plant_data-matnr
                                               lifnr = gw_output-lifnr
                                               werks = p_werks2
                                               ekorg = p_ekorg ]-datbi OPTIONAL ).
    gw_output-datab = VALUE #( lt_inforecords[ matnr = lw_plant_data-matnr
                                               lifnr = gw_output-lifnr
                                               werks = p_werks2
                                               ekorg = p_ekorg ]-datab OPTIONAL ).

    IF lw_plant_data-vprsv = 'S'. "Price Control Indicator
      lv_unit_price = lw_plant_data-stprs.  "Standard price.
    ENDIF.
    IF lw_plant_data-vprsv = 'V'.
      lv_unit_price = lw_plant_data-verpr. "Moving Average Price.
    ENDIF.
    CONDENSE lv_unit_price.
    IF lv_unit_price = '0.00'.
      gw_output-message = |Unit price is zero in MBEW. It is not applicable|.
      APPEND gw_output TO gt_output.
      CONTINUE.
    ENDIF.
    IF p_disp IS INITIAL. " Update run
      PERFORM f_process_create_inforecord
                                          USING lw_plant_data-matnr
                                                lv_unit_price.
    ELSE.
      gw_output-message = |Process for update Inforecord ME11/ME12(No Updated)|.
      APPEND gw_output TO gt_output.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_display_output
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_display_output .
  DATA: lo_gr_alv       TYPE REF TO cl_salv_table, " Variables for ALV properties
        lo_gr_functions TYPE REF TO cl_salv_functions_list.

  DATA: lo_event_handler TYPE REF TO cl_handler, " Variables for events
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

*  IF r1 = abap_true. " Purchase Org Copy Plant to Plant
* Create the ALV object
  TRY.
      CALL METHOD cl_salv_table=>factory
        IMPORTING
          r_salv_table = lo_gr_alv
        CHANGING
          t_table      = gt_output.
    CATCH cx_salv_msg.
  ENDTRY.
*
*  ELSE.  " Sales Org Copy Plant to Plant
** Create the ALV object
*    TRY.
*        CALL METHOD cl_salv_table=>factory
*          IMPORTING
*            r_salv_table = lo_gr_alv
*          CHANGING
*            t_table      = gt_output_vk11.
*      CATCH cx_salv_msg.
*    ENDTRY.
*  ENDIF.
* Let's show all default buttons of ALV
  lo_gr_functions = lo_gr_alv->get_functions( ).
  lo_gr_functions->set_all( abap_true ).

* Fit the columns
  lo_columns = lo_gr_alv->get_columns( ).
  lo_columns->set_optimize( 'X' ).

* Create header
  DESCRIBE TABLE gt_output LINES lv_rows.
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

* Register events
  lo_events = lo_gr_alv->get_event( ).
  CREATE OBJECT lo_event_handler.
  SET HANDLER lo_event_handler->on_double_click FOR lo_events.

* Enable cell selection mode
  lo_selections = lo_gr_alv->get_selections( ).
  lo_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).

  TRY.
      lo_column ?= lo_columns->get_column( 'MESSAGE' ).
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Remark' ).
      lo_column->set_medium_text( 'Remark' ).
      lo_column->set_short_text( 'Remark' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  lo_gr_alv->display( ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_process_create_inforecord
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_process_create_inforecord USING  pi_matnr pi_unit_price.
  DATA lv_msg_text TYPE string.
  DATA(ls_options) = VALUE ctu_params(  dismode = 'N'
                                        updmode = 'S'
                                        defsize = ''
                                        nobinpt = 'X'
                                        racommit = 'X' ).

  LOOP AT gt_supplier_list INTO DATA(lw_supplier).
    IF gw_output-infnr IS INITIAL.
*      PERFORM f_prepare_for_me11.
      DATA(lv_tcode) = 'ME11'.

      gt_bdcdata = VALUE #(
        ( program  = 'SAPMM06I' dynpro   = '0100' dynbegin = 'X' )
        ( fnam = 'BDC_OKCODE'       fval = '/00' )
        ( fnam = 'EINA-LIFNR'       fval = lw_supplier-lifnr )
        ( fnam = 'EINA-MATNR'       fval = pi_matnr )
        ( fnam = 'EINE-EKORG'       fval = p_ekorg )
        ( fnam = 'EINE-WERKS'       fval = p_werks2 )
        ( fnam = 'RM06I-NORMB'      fval = 'X' ) ).

      APPEND VALUE #( program  = 'SAPMM06I'  dynpro   = '0101' dynbegin = 'X' ) TO gt_bdcdata.
      APPEND VALUE #( fnam = 'BDC_OKCODE'    fval = '=EINE' )                   TO gt_bdcdata.
      APPEND VALUE #( program  = 'SAPMM06I'  dynpro   = '0102' dynbegin = 'X' ) TO gt_bdcdata.
      APPEND VALUE #( fnam = 'BDC_OKCODE'    fval = '=KO' )                     TO gt_bdcdata.
      APPEND VALUE #( fnam = 'EINE-NORBM'    fval = '1' )                       TO gt_bdcdata.
      APPEND VALUE #( fnam = 'EINE-NETPR'    fval = pi_unit_price )             TO gt_bdcdata.
      APPEND VALUE #( fnam = 'EINE-MWSKZ'    fval = 'PE' )                      TO gt_bdcdata.
      APPEND VALUE #( program  = 'SAPMV13A'  dynpro   = '0201' dynbegin = 'X' ) TO gt_bdcdata.
      APPEND VALUE #( fnam = 'BDC_OKCODE'    fval = '=SICH' )                   TO gt_bdcdata.
    ELSE.
      lv_tcode = 'ME12'.
      gt_bdcdata = VALUE #(
        ( program  = 'SAPMM06I' dynpro   = '0100' dynbegin = 'X' )
        ( fnam = 'BDC_OKCODE' fval = '/00' )
        ( fnam = 'EINA-LIFNR' fval = lw_supplier-lifnr )
        ( fnam = 'EINA-MATNR' fval = pi_matnr )
        ( fnam = 'EINE-EKORG' fval = p_ekorg )
        ( fnam = 'EINE-WERKS' fval = p_werks2 )
        ( fnam = 'RM06I-NORMB' fval = 'X' ) ).
      APPEND VALUE #( program  = 'SAPMM06I'   dynpro   = '0101'  dynbegin = 'X' ) TO gt_bdcdata.
      APPEND VALUE #( fnam = 'BDC_OKCODE'     fval = '=KO' )                      TO gt_bdcdata.
      APPEND VALUE #( program  = 'SAPLV14A'   dynpro   = '0102'  dynbegin = 'X' ) TO gt_bdcdata.
      APPEND VALUE #( fnam = 'BDC_OKCODE'     fval = '=NEWD' )                    TO gt_bdcdata.
      APPEND VALUE #( program  = 'SAPMV13A'   dynpro   = '0201'  dynbegin = 'X' ) TO gt_bdcdata.
      APPEND VALUE #( fnam = 'BDC_OKCODE'     fval = '=SICH' )                    TO gt_bdcdata.
      APPEND VALUE #( fnam = 'KONP-KBETR(01)' fval = pi_unit_price )              TO gt_bdcdata.
    ENDIF.

    TRY.
        CALL TRANSACTION lv_tcode WITH AUTHORITY-CHECK
                                USING gt_bdcdata OPTIONS FROM ls_options
                                MESSAGES INTO gt_bdcmsg.
      CATCH cx_sy_authorization_error ##NO_HANDLER.
    ENDTRY.

    COMMIT WORK AND WAIT.
    LOOP AT gt_bdcmsg INTO gw_bdcmsg WHERE ( msgtyp = 'S' OR msgtyp = 'E' ).
*                                       WHERE ( msgtyp = 'S' OR msgtyp = 'E' ) AND
*                                           ( msgnr = 312 OR
*                                             msgnr = 331 OR
*                                             msgnr = 335 ).
      CLEAR lv_msg_text.
      CALL FUNCTION 'FORMAT_MESSAGE'
        EXPORTING
          id        = gw_bdcmsg-msgid
          lang      = sy-langu
          no        = gw_bdcmsg-msgnr
          v1        = gw_bdcmsg-msgv1
          v2        = gw_bdcmsg-msgv2
          v3        = gw_bdcmsg-msgv3
          v4        = gw_bdcmsg-msgv4
        IMPORTING
          msg       = lv_msg_text
        EXCEPTIONS
          not_found = 01.
      gw_output-message = |{ gw_output-message } { lv_msg_text }|.
    ENDLOOP.
    APPEND gw_output TO gt_output.
    CLEAR:gt_bdcdata[], gt_bdcmsg[].
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form bdc_dynpro
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      --> P_
*&---------------------------------------------------------------------*
FORM bdc_dynpro  USING program TYPE any
                        dynpro TYPE any.
  CLEAR:gw_bdcdata.
  gw_bdcdata-program  = program.
  gw_bdcdata-dynpro   = dynpro.
  gw_bdcdata-dynbegin = 'X'.
  APPEND gw_bdcdata TO gt_bdcdata.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form bdc_field
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      --> P_
*&---------------------------------------------------------------------*
FORM bdc_field  USING  fnam TYPE any
                      fval TYPE any.
  CLEAR:gw_bdcdata.
  gw_bdcdata-fnam = fnam.
  gw_bdcdata-fval = fval.
  APPEND gw_bdcdata TO gt_bdcdata.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_prepare_for_me11
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
*FORM f_prepare_for_me11 .
*
*  DATA bdcdata_tab TYPE TABLE OF bdcdata WITH EMPTY KEY.
*
*  bdcdata_tab = VALUE #(
*    ( program  = 'SAPMM06I' dynpro   = '0100' dynbegin = 'X' )
*    ( fnam = 'BDC_OKCODE'       fval = '/00' )
*    ( fnam = 'EINA-LIFNR'       fval = lw_supplier-lifnr )
*    ( fnam = 'EINA-MATNR'       fval = pi_matnr )
*    ( fnam = 'EINE-EKORG'       fval = p_ekorg )
*    ( fnam = 'EINE-WERKS'       fval = p_werks2 )
*    ( fnam = 'RM06I-NORMB'      fval = 'X' ) ).
*
**    PERFORM bdc_dynpro      USING 'SAPMM06I' '0100'.
***    PERFORM bdc_field       USING 'BDC_CURSOR'
***                                  'EINE-WERKS'.
**    PERFORM bdc_field       USING 'BDC_OKCODE'
**                                  '/00'.
**    PERFORM bdc_field       USING 'EINA-LIFNR'
**                                  lw_supplier-lifnr.
**    PERFORM bdc_field       USING 'EINA-MATNR'
**                                  pi_matnr.
**    PERFORM bdc_field       USING 'EINE-EKORG'
**                                  p_ekorg.
**    PERFORM bdc_field       USING 'EINE-WERKS'
**                                  p_werks2.
**    PERFORM bdc_field       USING 'RM06I-NORMB'
**                                  'X'.
*
*  APPEND VALUE #(
*    ( program  = 'SAPMM06I' dynpro   = '0101' dynbegin = 'X' )
*    ( fnam = 'BDC_OKCODE'       fval = '=EINE' ) ) TO bdcdata_tab.
**
**    PERFORM bdc_dynpro      USING 'SAPMM06I' '0101'.
***    PERFORM bdc_field       USING 'BDC_CURSOR'
***                                  'EINE-NETPR'.
**    PERFORM bdc_field       USING 'BDC_OKCODE'
**                                  '=EINE'.
*  APPEND VALUE #(
*        ( program  = 'SAPMM06I' dynpro   = '0102' dynbegin = 'X' )
*        ( fnam = 'BDC_OKCODE'       fval = '=KO' )
*        ( fnam = 'EINE-NORBM'       fval = '1' )
*        ( fnam = 'EINE-NETPR'       fval = pi_unit_price )
*        ( fnam = 'EINE-MWSKZ'       fval = 'PE' ) ) TO bdcdata_tab.
*
**  PERFORM bdc_dynpro      USING 'SAPMM06I' '0102'.
***    PERFORM bdc_field       USING 'BDC_CURSOR'
***                                  'EINE-NETPR'.
**  PERFORM bdc_field       USING 'BDC_OKCODE'
**                                '=KO'.
**  PERFORM bdc_field       USING 'EINE-NORBM'
**                                '1'.
**
**  PERFORM bdc_field       USING 'EINE-NETPR'
**                                 pi_unit_price.
**
**  PERFORM bdc_field       USING 'EINE-MWSKZ'
**                                'PE'.
*
*  APPEND VALUE #(
*        ( program  = 'SAPMV13A' dynpro   = '0201' dynbegin = 'X' )
*        ( fnam = 'BDC_OKCODE'       fval = '=SICH' ) ) TO bdcdata_tab.
**  PERFORM bdc_dynpro      USING 'SAPMV13A' '0201'.
***    PERFORM bdc_field       USING 'BDC_CURSOR'
***                                  'RV13A-DATAB'.
**  PERFORM bdc_field       USING 'BDC_OKCODE'
**                                '=SICH'.
***    PERFORM bdc_field       USING 'RV13A-DATAB'
***                                  15.11.2022.
***
***    PERFORM bdc_field       USING 'RV13A-DATBI'
***                                  '31.12.9999'.

*ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_process_for_sales_org
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_process_for_sales_org .
  " Get Sales Org from Source Plant
  SELECT SINGLE vkorg FROM t001w INTO @DATA(l_vkorg_source) WHERE werks = @p_werks.
  " Get Sales Org from Target Plant
  SELECT SINGLE vkorg FROM t001w INTO @DATA(l_vkorg_target) WHERE werks = @p_werks2.
  IF p_vkorg NE l_vkorg_target.
    DATA(lv_err) = |Selected Target Sales Org { p_vkorg } is not matched with Target Plant { p_werks2 } Sales Org which is { l_vkorg_target }|.
    MESSAGE lv_err TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.
  " Get Source Plant Sales Org Material Price


  DATA lv_unit_price TYPE char15.
  SELECT a~matnr,
         a~meins,
         c~maktx,
         b~werks,
         d~vprsv, "Price Control Indicator
         d~verpr, "Moving Average Price
         d~stprs  "Standard price
  INTO TABLE @DATA(lt_plant_data)
  FROM mara AS a
    INNER JOIN marc AS b ON a~matnr = b~matnr
    INNER JOIN makt AS c ON a~matnr = c~matnr
    INNER JOIN mbew AS d ON c~matnr = d~matnr AND
                            b~werks = d~bwkey
  WHERE a~matnr IN @s_matnr AND
        a~mtart = @p_mtype AND
        c~spras = @sy-langu AND
        d~bwkey = @p_werks AND
        d~vprsv IN ('S','V').
  CHECK lt_plant_data IS NOT INITIAL.
  SORT lt_plant_data BY matnr werks.
  DELETE lt_plant_data WHERE vprsv = 'S' AND stprs = 0.
  DELETE lt_plant_data WHERE vprsv = 'V' AND verpr = 0.

  SELECT vkorg, " Sales Org
         vtweg, " Distrubution Channel
         matnr, " Material
         datbi, " Valid To
         datab " Valid From
*         kbetr, " Condition Amount
*         kpein, " Pricing Unit
*         kmein  " Unit of Measure
         FROM a304
         INTO TABLE @DATA(lt_a304_target)
    FOR ALL ENTRIES IN @lt_plant_data
              WHERE kappl = 'V'
                AND kschl = 'PR00'
                AND vkorg = @l_vkorg_target
                AND matnr = @lt_plant_data-matnr
                AND datab = @p_date.
*
*  SELECT vkorg, " Sales Org
*         vtweg, " Distrubution Channel
*         matnr, " Material
*         datbi, " Valid To
*         datab, " Valid From
*         kbetr, " Condition Amount
*         kpein, " Pricing Unit
*         kmein  " Unit of Measure
*         FROM a304 AS a INNER JOIN konp AS b ON a~knumh = b~knumh
*         INTO TABLE @DATA(lt_a304_source)
*              WHERE a~kappl = 'V'
*                AND a~kschl = 'PR00'
*                AND a~vkorg = @l_vkorg_source
*                AND a~matnr IN @s_matnr
*                AND a~datbi GE @sy-datum
*                AND a~datab LE @sy-datum
*                AND b~kappl = 'V'
*                AND b~kschl EQ 'PR00'
*                AND b~loevm_ko NE 'X'.
*  SORT lt_a304_source BY matnr ASCENDING datab DESCENDING.
**  " Get Target Plant Sales Org Material Price
**  SELECT vkorg, " Sales Org
**         vtweg, " Distrubution Channel
**         matnr, " Material
**         datbi, " Valid To
**         datab, " Valid From
**         kbetr, " Condition Amount
**         kpein, " Pricing Unit
**         kmein  " Unit of Measure
**         FROM a304 AS a INNER JOIN konp AS b ON a~knumh = b~knumh
**         INTO TABLE @DATA(lt_a304_target)
**              WHERE a~kappl = 'V'
**                AND a~kschl = 'PR00'
**                AND a~vkorg = @p_vkorg
**                AND a~matnr IN @s_matnr
**                AND a~datbi GE @sy-datum
**                AND a~datab LE @sy-datum
**                AND b~kappl = 'V'
**                AND b~kschl EQ 'PR00'
**                AND b~loevm_ko NE 'X'.
**  SORT lt_a304_target BY matnr ASCENDING datab DESCENDING.
*  DATA lt_a304_target_ins  LIKE lt_a304_source.
**  DATA lt_a304_target_mod  LIKE lt_a304_target.
**
**  " Modify existing Material Pricing
**  LOOP AT lt_a304_target INTO DATA(lw_target).
**    READ TABLE lt_a304_source INTO DATA(lw_source) WITH KEY matnr = lw_target-matnr.
**    IF sy-subrc = 0.
**      lw_target-kbetr = lw_source-kbetr.
**      lw_target-kpein = lw_source-kpein.
**      lw_target-kmein = lw_source-kmein.
**      lw_target-datab = sy-datum.
**      lw_target-datbi = '99991231'.
**      APPEND lw_target TO lt_a304_target_mod.
**    ENDIF.
**  ENDLOOP.
*  " Create New Metrial pricing
**  LOOP AT lt_a304_source INTO DATA(lw_target).
***    READ TABLE lt_a304_target_mod INTO lw_target  WITH KEY matnr = lw_source-matnr.
***    IF sy-subrc NE 0.
**    lw_target-vkorg = p_vkorg.
**    lw_target-vtweg = '20'.
**    lw_target-datab = p_date.
**    lw_target-datbi = '99991231'.
**    APPEND lw_target TO lt_a304_target_ins.
***    ENDIF.
**  ENDLOOP.


  DATA lv_msg_text TYPE string.
  DATA(ls_options) = VALUE ctu_params(  dismode = 'N'
                                        updmode = 'S'
                                        defsize = ''
                                        nobinpt = 'X'
                                        racommit = 'X' ).
  DATA: lv_datum     TYPE char10,
        lv_price(13) TYPE c.  "Final Price
  WRITE p_date TO lv_datum DD/MM/YYYY.
  DATA(lv_tcode) = 'VK11'.
  LOOP AT lt_plant_data INTO DATA(lw_plant_data) .
    MOVE-CORRESPONDING lw_plant_data TO gw_output.
    DATA(l_matnr) = VALUE #( lt_a304_target[ matnr = lw_plant_data-matnr ]-matnr OPTIONAL ).
    IF l_matnr IS INITIAL.

      PERFORM bdc_dynpro      USING 'SAPMV13A' '0100'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'RV13A-KSCHL'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '/00'.
      PERFORM bdc_field       USING 'RV13A-KSCHL'
                                    'PR00'.

      PERFORM bdc_dynpro      USING 'SAPLV14A' '0100'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'RV130-SELKZ(05)'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=WEIT'.
      PERFORM bdc_field       USING 'RV130-SELKZ(01)'
                                    ''.
      PERFORM bdc_field       USING 'RV130-SELKZ(05)'
                                    'X'.

      PERFORM bdc_dynpro      USING 'SAPMV13A' '1304'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'RV13A-DATBI(01)'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=SICH'.
      PERFORM bdc_field       USING 'KOMG-VKORG'
                                    p_vkorg.
      PERFORM bdc_field       USING 'KOMG-VTWEG'
                                    '40'.
      PERFORM bdc_field       USING 'KOMG-MATNR(01)'
                                    lw_plant_data-matnr.
      IF lw_plant_data-vprsv = 'S'. "Price Control Indicator
        lv_price = lw_plant_data-stprs.
      ENDIF.
      IF lw_plant_data-vprsv = 'V'.
        lv_price = lw_plant_data-verpr.
      ENDIF.
      PERFORM bdc_field       USING 'KONP-KBETR(01)'
                                    lv_price.
*    PERFORM bdc_field       USING 'KONP-KPEIN(01)'
*                                  lw_plant_data-kpein.
      PERFORM bdc_field       USING 'RV13A-DATAB(01)'
                                    lv_datum.
      IF lw_plant_data-meins EQ 'BOT'.
        PERFORM bdc_field       USING 'KONP-KMEIN(01)'
                                      'BT'.
      ELSE.
        PERFORM bdc_field       USING 'KONP-KMEIN(01)'
                                      lw_plant_data-meins.
      ENDIF.

      TRY.
          CALL TRANSACTION lv_tcode WITH AUTHORITY-CHECK
                                  USING gt_bdcdata OPTIONS FROM ls_options
                                  MESSAGES INTO gt_bdcmsg.
        CATCH cx_sy_authorization_error ##NO_HANDLER.
      ENDTRY.

      COMMIT WORK AND WAIT.
      LOOP AT gt_bdcmsg INTO gw_bdcmsg WHERE ( msgtyp = 'S' OR msgtyp = 'E' ).

        CLEAR lv_msg_text.
        CALL FUNCTION 'FORMAT_MESSAGE'
          EXPORTING
            id        = gw_bdcmsg-msgid
            lang      = sy-langu
            no        = gw_bdcmsg-msgnr
            v1        = gw_bdcmsg-msgv1
            v2        = gw_bdcmsg-msgv2
            v3        = gw_bdcmsg-msgv3
            v4        = gw_bdcmsg-msgv4
          IMPORTING
            msg       = lv_msg_text
          EXCEPTIONS
            not_found = 01.
        gw_output-message = |{ gw_output-message } { lv_msg_text }|.
      ENDLOOP.
    ELSE.
      gw_output-message = 'Price record found in A304 on Same date. Please update manually' .
      CLEAR l_matnr.
    ENDIF.
    APPEND gw_output TO gt_output.
    CLEAR:gt_bdcdata[], gt_bdcmsg[].
  ENDLOOP.
ENDFORM.
