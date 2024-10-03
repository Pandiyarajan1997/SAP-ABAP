*&---------------------------------------------------------------------*
*& Include          ZMM_PO_CLOSE_CLS
*&---------------------------------------------------------------------*
CLASS lcl_po_close DEFINITION.
  PUBLIC SECTION.
    METHODS:
      f_date_adjustment,
      f_get_open_po,
      f_close_po,
      f_display_alv.

  PRIVATE SECTION.
    TYPES: BEGIN OF open_po,
             ebeln   TYPE ebeln,
             reswk   TYPE ekko-reswk,
             ebelp   TYPE ebelp,
             matnr   TYPE matnr,
             werks   TYPE werks_d,
             dc_ind  TYPE elikz,
             type    TYPE bapi_mtype,
             message TYPE string,
           END OF open_po.
    DATA: gt_open_po TYPE TABLE OF open_po.
    DATA: gt_open_po1 TYPE TABLE OF open_po.
    DATA: lr_bsart TYPE RANGE OF ekko-bsart.
    DATA: lt_poitem  TYPE TABLE OF bapimepoitem,
          lt_poitemx TYPE TABLE OF bapimepoitemx,
          return     TYPE bapiret2_t.
    DATA: header  TYPE bapimepoheader,
          headerx TYPE bapieikp.
    DATA: lv_msg TYPE string.
ENDCLASS.

CLASS lcl_po_close IMPLEMENTATION.
*------- Selection Screen Adjustments ---------------------*
  METHOD f_date_adjustment.
    CLEAR lv_date.
    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        date      = sy-datum
        days      = 7
        months    = 0
        signum    = '-'
        years     = 0
      IMPORTING
        calc_date = lv_date.
    CLEAR lv_date1.
    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        date      = lv_date
        days      = 7
        months    = 0
        signum    = '-'
        years     = 0
      IMPORTING
        calc_date = lv_date1.
  ENDMETHOD.
*---- Fetching the Open PO for input or all --------*
  METHOD f_get_open_po.
    REFRESH: gt_open_po,lr_bsart.

    SELECT a~ebeln
           a~reswk
           b~ebelp
           b~matnr
           b~werks
           b~elikz AS dc_ind INTO CORRESPONDING FIELDS OF TABLE me->gt_open_po
                   FROM  ekko AS a
                   INNER JOIN ekpo AS b ON a~ebeln = b~ebeln
                   WHERE a~ebeln IN s_ebeln
                   AND a~bstyp = 'F'
                   AND a~bsart IN s_bsart
                   AND a~reswk IN s_plant
                   AND a~aedat IN s_aedat
                   AND b~ebelp IN s_ebelp
                   AND b~werks IN s_werks
                   AND b~elikz NE 'X'.
    IF sy-subrc = 0.
      SORT gt_open_po[] BY ebeln ebelp.
    ELSE.
      MESSAGE 'No open Purchase order to close' TYPE 'E'.
    ENDIF.
  ENDMETHOD.
*---- Closing the Open Purchase order Process --------*
  METHOD f_close_po.
    REFRESH gt_open_po1.
    gt_open_po1[] = gt_open_po[].
    DELETE ADJACENT DUPLICATES FROM gt_open_po[] COMPARING ebeln.

    LOOP AT gt_open_po ASSIGNING FIELD-SYMBOL(<fs_open_po>).
      REFRESH: lt_poitem,lt_poitemx.
      DATA(lv_ponumber) = <fs_open_po>-ebeln. "Purchase Order Number

      LOOP AT gt_open_po1 ASSIGNING FIELD-SYMBOL(<fs_open_po1>) WHERE ebeln = <fs_open_po>-ebeln.
        APPEND VALUE #( po_item = <fs_open_po1>-ebelp
                        no_more_gr  = abap_true
                        deliv_compl = abap_true ) TO lt_poitem.
        APPEND VALUE #( po_item = <fs_open_po1>-ebelp
                        po_itemx = abap_true
                        no_more_gr  = abap_true
                        deliv_compl = abap_true ) TO lt_poitemx.
      ENDLOOP.
*------ Function module which amends the po lineitems ---------------------------*
      CLEAR: header,headerx,return.
      CALL FUNCTION 'BAPI_PO_CHANGE'
        EXPORTING
          purchaseorder     = lv_ponumber
        IMPORTING
          expheader         = header
          exppoexpimpheader = headerx
        TABLES
          return            = return
          poitem            = lt_poitem
          poitemx           = lt_poitemx.
      READ TABLE return INTO DATA(ls_ret1) WITH KEY type = 'E'.
      IF sy-subrc = 0.
        CLEAR lv_msg.
        LOOP AT return INTO DATA(lw_ret1) WHERE type = 'E'.
          lv_msg = | { lv_msg } { lw_ret1-message } |.
        ENDLOOP.
        LOOP AT gt_open_po1 ASSIGNING FIELD-SYMBOL(<ls_poitem>) WHERE ebeln = <fs_open_po>-ebeln.
          <ls_poitem>-type = 'E'.
          <ls_poitem>-message = lv_msg.
        ENDLOOP.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
        LOOP AT gt_open_po1 ASSIGNING FIELD-SYMBOL(<ls_poitem1>) WHERE ebeln = <fs_open_po>-ebeln.
          <ls_poitem1>-dc_ind = 'X'.
          <ls_poitem1>-type = 'S'.
          <ls_poitem1>-message = |Purchase Order { <fs_open_po>-ebeln } is closed |.
        ENDLOOP.
      ENDIF.
      CLEAR lv_ponumber.
    ENDLOOP.
  ENDMETHOD.
*------- Display ALV finally
  METHOD f_display_alv.
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

    IF gt_open_po1 IS NOT INITIAL.
* Create the ALV object
      TRY.
          CALL METHOD cl_salv_table=>factory
            IMPORTING
              r_salv_table = lo_gr_alv
            CHANGING
              t_table      = me->gt_open_po1.
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
  ENDMETHOD.
ENDCLASS.
