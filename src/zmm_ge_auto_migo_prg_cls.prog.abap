*&---------------------------------------------------------------------*
*& Include          ZMM_AUTO_MIGO_PRG_CLS
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*       CLASS lcl_salv_buddy DEFINITION
*----------------------------------------------------------------------*
*
* This class is meant to be used with SALV objects to gain access to the GUI control that exist behind them.
* All SALV objects are descendants of the CL_SALV_MODEL_BASE class. Currently, there are the following SALV object classes:
*   CL_SALV_TABLE
*   CL_SALV_HIERSEQ_TABLE
*   CL_SALV_TREE
*
* We inherit from the abstract class CL_SAL_CONTROLLER because SALV objects have it as a FRIEND, thus
*   making us a FRIEND of SALV objects too. That allows us to have access to ALL attributes and methods of
*   SALV objects, regardless of them being PUBLIC, PROTECTED or PRIVATE. Example: R_CONTROLLER.
* Since this class only has STATIC methods, there's no need to instantiate objects of its type, thus the CRIATE PRIVATE.
* Finally, I see no need to inherit from this class, so this class is declared FINAL.
*----------------------------------------------------------------------*
CLASS lcl_salv_buddy DEFINITION INHERITING FROM cl_salv_controller CREATE PRIVATE FINAL.

  PUBLIC SECTION.

*----------------------------------------------------------------------*
* GET_CONTROL_RTTI - Returns runtime type information for the control that is behind a SALV object.
*   Parameter E_ADAPTER_TYPE returns the adapter type of a SALV object.
*     Based on this information, method GET_CONTROL will return a different control in its returrning parameter R_CONTROL.
*     You can use this runtime type information to choose the right control object to supply to the returning parameter R_CONTROL of method GET_CONTROL.
*   Parameter E_CONTROL_RTTI returns a TYPE HANDLE that you can use to create an object compatible with the returning parameter R_CONTROL of method GET_CONTROL.
* Below there is a correspondence between the adapter type returned in parameter E_ADAPTER_TYPE and
* the type of the control expected in parameter R_CONTROL of method GET_CONTROL:
*   IF_SALV_ADAPTER~C_ADAPTER_TYPE_FULLSCREEN       CL_GUI_ALV_GRID
*   IF_SALV_ADAPTER~C_ADAPTER_TYPE_GRID             CL_GUI_ALV_GRID
*   IF_SALV_ADAPTER~C_ADAPTER_TYPE_HIERSEQ          nothing (null)
*   IF_SALV_ADAPTER~C_ADAPTER_TYPE_LIST             CL_SALV_TABLE
*   IF_SALV_ADAPTER~C_ADAPTER_TYPE_TREE             CL_SALV_GUI_TREE
*   IF_SALV_ADAPTER~C_ADAPTER_TYPE_APPEND           nothing (null)
*----------------------------------------------------------------------*
    CLASS-METHODS: get_control_rtti IMPORTING i_salv         TYPE REF TO cl_salv_model_base
                                    EXPORTING e_adapter_type TYPE salv_de_adapter_type
                                              e_control_rtti TYPE REF TO cl_abap_typedescr,

*----------------------------------------------------------------------*
* GET_CONTROL - Returns the control that is behind the SALV object.
*   MUST be called after the DISPLAY method of the SALV object, so that its control gets created.
*   See method GET_CONTROL_RTTI above for a correspondence between what you supply in paramter I_SALV and what you get back in parameter R_CONTROL.
*----------------------------------------------------------------------*
      get_control IMPORTING i_salv           TYPE REF TO cl_salv_model_base
                  RETURNING VALUE(r_control) TYPE REF TO object,

*----------------------------------------------------------------------*
* SET_EDITABLE - Enables OR disables editing on a CL_SALV_TABLE object.
*   If you supply parameter I_FIELDNAME and supply it NOT INITIAL, you get that
*     field enabled or disabled for editing, depending on parameter I_EDITABLE.
*   If you do not supply parameter I_FIELDNAME or supply it INITIAL, you get
*     all fields of the table enabled or disabled for editing, depending on parameter I_EDITABLE.
*   Parameter I_SALV_TABLE is the CL_SALV_TABLE object you want to enable or disable editing for.
*   Parameter I_EDITABLE should be ABAP_TRUE or ABAP_FALSE to enable or disable editing.
*   Parameter I_REFRESH indicates whether you want the control to be refreshed or not. You'll only see the changes
*     you've made using this method AFTER you do a refresh on the CL_SALV_TABLE object.
*   NOTE: If you want field per field editing capabilities, you MUST make sure editing for the whole table is disabled.
*         You can disable editing for the whole table simply by issuing a call to this method, omitting parameter I_FIELDNAME and
*         passing parameter I_EDITABLE as ABAP_FALSE. After that you can enable or disable editing on a field per field basis.
*         The CL_SALV_TABLE is disabled for editing by default.
*----------------------------------------------------------------------*
      set_editable IMPORTING VALUE(i_fieldname) TYPE csequence OPTIONAL
                             i_salv_table       TYPE REF TO cl_salv_table
                             VALUE(i_editable)  TYPE abap_bool DEFAULT abap_true
                             VALUE(i_refresh)   TYPE abap_bool DEFAULT abap_true.

  PRIVATE SECTION.

*----------------------------------------------------------------------*
* GET_CONTROL_INTERNAL - It is the guts of method GET_CONTROL. It does all the heavy work and method GET_CONTROL gets all the credits
*----------------------------------------------------------------------*
    CLASS-METHODS: get_control_internal IMPORTING i_salv         TYPE REF TO cl_salv_model_base
                                        EXPORTING e_adapter_type TYPE salv_de_adapter_type
                                                  e_control      TYPE REF TO object.

ENDCLASS.                    "lcl_salv_buddy DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_salv_buddy IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_salv_buddy IMPLEMENTATION.


  METHOD get_control_internal.

    DATA: lo_controller            TYPE REF TO cl_salv_controller_model,
          lo_adapter               TYPE REF TO cl_salv_adapter,
          lo_fullscreen_adapter    TYPE REF TO cl_salv_fullscreen_adapter,
          lo_grid_adapter          TYPE REF TO cl_salv_grid_adapter,
          lo_table_display_adapter TYPE REF TO if_salv_table_display_adapter,
          lo_tree_adapter_base     TYPE REF TO cl_salv_tree_adapter_base.

    CHECK e_adapter_type IS REQUESTED OR
          e_control      IS REQUESTED.

    IF  e_adapter_type IS REQUESTED.
      CLEAR e_adapter_type.
    ENDIF.

    IF  e_control IS REQUESTED.
      CLEAR e_control.
    ENDIF.

    lo_controller = i_salv->r_controller.
    CHECK lo_controller IS BOUND.

    lo_adapter = lo_controller->r_adapter.
    CHECK lo_adapter IS BOUND.

    IF e_adapter_type IS REQUESTED.
      e_adapter_type = lo_adapter->type.
    ENDIF.

    CHECK e_control IS REQUESTED.

    CASE lo_adapter->type.
      WHEN lo_adapter->if_salv_adapter~c_adapter_type_fullscreen.
        lo_fullscreen_adapter ?= lo_adapter.
        e_control = lo_fullscreen_adapter->get_grid( ).

      WHEN lo_adapter->if_salv_adapter~c_adapter_type_grid.
        lo_grid_adapter ?= lo_adapter.
        e_control = lo_grid_adapter->get_grid( ).

      WHEN lo_adapter->if_salv_adapter~c_adapter_type_hierseq.

      WHEN lo_adapter->if_salv_adapter~c_adapter_type_list.
        lo_table_display_adapter ?= lo_adapter.
        e_control = lo_table_display_adapter->r_table.

      WHEN lo_adapter->if_salv_adapter~c_adapter_type_tree.
        lo_tree_adapter_base ?= lo_adapter.
        e_control = lo_tree_adapter_base->r_tree.

      WHEN lo_adapter->if_salv_adapter~c_adapter_type_append.

    ENDCASE.

  ENDMETHOD.                    "get_control_internal

  METHOD get_control_rtti.

    DATA: lv_adapter_type TYPE salv_de_adapter_type,
          lo_control      TYPE REF TO object.

    CHECK e_adapter_type IS REQUESTED OR
          e_control_rtti IS REQUESTED.

    IF  e_adapter_type IS REQUESTED.
      CLEAR e_adapter_type.
    ENDIF.

    IF  e_control_rtti IS REQUESTED.
      CLEAR e_control_rtti.
    ENDIF.

    get_control_internal( EXPORTING i_salv = i_salv IMPORTING e_adapter_type = lv_adapter_type e_control = lo_control ).

    IF e_adapter_type IS REQUESTED.
      e_adapter_type = lv_adapter_type.
    ENDIF.

    IF e_control_rtti IS REQUESTED.
      e_control_rtti = cl_abap_typedescr=>describe_by_object_ref( lo_control ).
    ENDIF.

  ENDMETHOD.                    "get_control_rtti

  METHOD get_control.

    CHECK r_control IS REQUESTED.

    get_control_internal( EXPORTING i_salv = i_salv IMPORTING e_control = r_control ).

  ENDMETHOD.                    "get_control

  METHOD set_editable.
    CONSTANTS: lc_stable TYPE lvc_s_stbl VALUE 'XX'.

    DATA: lo_grid     TYPE REF TO cl_gui_alv_grid,
          lt_fieldcat TYPE lvc_t_fcat,
          ls_layout   TYPE lvc_s_layo.

    FIELD-SYMBOLS: <fs_fieldcat> LIKE LINE OF lt_fieldcat.

    lo_grid ?= get_control( i_salv_table ).
    CHECK lo_grid IS BOUND.

    IF i_fieldname IS SUPPLIED AND
       i_fieldname IS NOT INITIAL.
      lo_grid->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = lt_fieldcat ).
      READ TABLE lt_fieldcat ASSIGNING <fs_fieldcat> WITH KEY fieldname = i_fieldname.
      CHECK sy-subrc = 0.
      <fs_fieldcat>-edit = i_editable.
      lo_grid->set_frontend_fieldcatalog( lt_fieldcat ).
    ELSE.
      lo_grid->get_frontend_layout( IMPORTING es_layout = ls_layout ).
      ls_layout-edit = i_editable.
      lo_grid->set_frontend_layout( EXPORTING is_layout = ls_layout ).
    ENDIF.

    CHECK i_refresh = abap_true.
    i_salv_table->refresh( lc_stable ).

  ENDMETHOD.                    "set_editable

ENDCLASS.                    "lcl_salv_buddy IMPLEMENTATION


CLASS lcl_auto_migo DEFINITION." INHERITING FROM cl_salv_controller CREATE PRIVATE FINAL.
  PUBLIC SECTION.
*    TYPES: ty_r_lifnr TYPE RANGE OF ekko-lifnr.

    CLASS-DATA: gv_editable TYPE abap_bool,
*                gv_editable1  TYPE abap_bool,
                gt_out_1    TYPE STANDARD TABLE OF zmm_st_asn,
                gr_salv_1   TYPE REF TO cl_salv_table,
                gr_salv_2   TYPE REF TO cl_salv_table.
*                lo_alv_custom TYPE REF TO cl_gui_custom_container,
*                lo_popup_alv  TYPE REF TO cl_salv_table.

    METHODS:
      fetch_invoice_data,
      fetch_purchase_data,
      process_migo,
      update_asn_migo_table,
      build_alv,
      fill_required_field,
*      validate_Shelf_life,
      get_field_values IMPORTING fname TYPE dynfnam,
      flush.
*      display_popup_alv.
    METHODS:
      on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function,
      on_user_command_g1 FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function.

*      on_user_command_pop_up FOR EVENT added_function OF cl_salv_events
*        IMPORTING e_salv_function.

    CLASS-METHODS: handle_double_click
      FOR EVENT double_click OF cl_salv_events_table
      IMPORTING row column,
      handle_double_click_g1
        FOR EVENT double_click OF cl_salv_events_table
        IMPORTING row column,

      on_link_click FOR EVENT link_click OF cl_salv_events_table
        IMPORTING row column.

*      handle_double_click_popup
*        FOR EVENT double_click OF cl_salv_events_table
*        IMPORTING row column.

  PRIVATE SECTION.

*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*
*--------------D-A-T-A  D-E-C-L-A-R-A-T-I-O-N -----------------------*
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*

    TYPES: BEGIN OF ty_qty,
*             ebeln TYPE ekpo-ebeln,
             matnr TYPE ekpo-matnr,
             qty   TYPE eket-menge,
           END OF ty_qty.
    TYPES: BEGIN OF ty_po_qty,
             ebeln TYPE ekpo-ebeln,
             ebelp TYPE ekpo-ebelp,
             matnr TYPE ekpo-matnr,
             qty   TYPE eket-menge,
           END OF ty_po_qty.
*    TYPES:
*      BEGIN OF ty_invoice,
*        vbeln  TYPE vbrk-vbeln,
*        posnr  TYPE vbrp-posnr,
*        fkdat  TYPE vbrk-fkdat,
*        matnr  TYPE vbrp-matnr,
*        arktx  TYPE lips-arktx,
*        werks  TYPE lips-werks,
*        lgort  TYPE lips-lgort,
*        charg  TYPE lips-charg,
*        lfimg  TYPE lips-lfimg,
*        vrkme  TYPE lips-vrkme,
*        netwr  TYPE vbrp-netwr,
*        fkimg  TYPE vbrp-fkimg,
*        uprice TYPE vbrp-netwr,
*        cmpre  TYPE vbrp-cmpre,
*        flag   TYPE flag,
*      END OF ty_invoice,
*      BEGIN OF ty_migo,
*        ebeln    TYPE ekpo-ebeln,
*        ebelp    TYPE ekpo-ebelp,
*        bedat    TYPE ekko-bedat,
*        matnr    TYPE ekpo-matnr,
*        txz01    TYPE ekpo-txz01,
*        werks    TYPE ekpo-werks,
*        lgort    TYPE lips-lgort,
*        charg    TYPE lips-charg,
*        po_qty   TYPE eket-menge,
*        del_qty  TYPE eket-wemng,
*        open_qty TYPE eket-menge,
*        asn_qty  TYPE eket-wemng,
*        migo_qty TYPE eket-wemng,
*        gravity  TYPE eket-wemng,
*        temprature  TYPE text10,
*        meins    TYPE ekpo-meins,
*        sku_qty  TYPE eket-wemng,
*        remark   TYPE text100,
*        netpr    TYPE ekpo-netpr,
*        total    TYPE ekpo-netpr,
*        flag     TYPE flag,
*      END OF ty_migo.

    DATA: gw_goodsmvt_header TYPE bapi2017_gm_head_01,
          gt_goodsmvt_item   TYPE STANDARD TABLE OF bapi2017_gm_item_create,
          gw_goodsmvt_item   TYPE bapi2017_gm_item_create,
          gt_return          TYPE STANDARD TABLE OF bapiret2.
    DATA: gw_goodsmvt_headret TYPE bapi2017_gm_head_ret,
          goodsmvt_code_tmp   TYPE bapi2017_gm_code,
          l_return            TYPE bapiret2.
    DATA : gv_inv_no       TYPE vbeln_vf,
*           gv_inv_plant    TYPE werks_d,
*           gv_rec_plant    TYPE werks_d,
*           gv_frbnr        TYPE frbnr,
*           gv_del_note     TYPE lfsnr1,
*           gv_bktxt        TYPE bktxt,
*           gv_test         TYPE flag,
*           gr_lifnr        TYPE ekko-lifnr,
*           gt_out_1        TYPE STANDARD TABLE OF zmm_st_asn,
           gv_field_flag   TYPE flag,
           wa_data         TYPE ty_migo,
           gt_out_2        TYPE STANDARD TABLE OF ty_migo,
           gt_migo         TYPE STANDARD TABLE OF ty_migo,
           gv_fname        TYPE rlgrap-filename,
           gw_inv_log      TYPE zmm_ge_asn,
           gt_inv_log      TYPE STANDARD TABLE OF zmm_ge_asn,
           gw_automigo_log TYPE zmm_ge_asn_migo,
           gt_automigo_log TYPE STANDARD TABLE OF zmm_ge_asn_migo.
    DATA: gt_matnr_po  TYPE STANDARD TABLE OF ty_qty,
          gt_matnr_inv TYPE STANDARD TABLE OF ty_qty,
          gw_matnr     TYPE ty_qty.
    DATA: gt_po_item_qty TYPE STANDARD TABLE OF ty_po_qty,
          gw_po_item_qty TYPE ty_po_qty.

    DATA: go_alv_custom  TYPE REF TO cl_gui_custom_container,
          gr_splitter    TYPE REF TO cl_gui_splitter_container,
          gr_container_1 TYPE REF TO cl_gui_container,
          gr_container_2 TYPE REF TO cl_gui_container,
          gr_salv_func_1 TYPE REF TO cl_salv_functions,
          gr_salv_func_2 TYPE REF TO cl_salv_functions.

ENDCLASS.


CLASS lcl_auto_migo IMPLEMENTATION.
  METHOD flush.
    CLEAR: gt_out_1[], gt_migo[], zmm_ge_asn,gt_matnr_inv[],gt_matnr_po[],gt_po_item_qty[].
    IF go_alv_custom IS NOT INITIAL.
      CALL METHOD go_alv_custom->free.
    ENDIF.
*
*    IF gr_salv_1 IS NOT INITIAL.
*      CALL METHOD me->gr_salv_1->free.
*    ENDIF.
  ENDMETHOD.
  METHOD  get_field_values.
    DATA: lt_values TYPE TABLE OF dynpread,
          lw_values TYPE dynpread.
    FIELD-SYMBOLS: <fs_field> TYPE any.
    CLEAR:    lw_values, lt_values.
    lw_values-fieldname = fname.
    APPEND lw_values TO lt_values.
    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        dyname             = sy-cprog
        dynumb             = sy-dynnr
        translate_to_upper = 'X'
      TABLES
        dynpfields         = lt_values.

    READ TABLE lt_values INDEX 1 INTO lw_values.
    IF sy-subrc = 0.
      REPLACE ALL OCCURRENCES OF ':' IN lw_values-fieldvalue WITH space.
      ASSIGN (fname) TO <fs_field>.
      IF fname = 'ZMM_GE_ASN-DDATE'.
        lw_values-fieldvalue = |{ lw_values-fieldvalue+6(4) }{ lw_values-fieldvalue+3(2) }{ lw_values-fieldvalue(2) }|.
      ENDIF.
      <fs_field> = lw_values-fieldvalue.
    ENDIF.
  ENDMETHOD.
  METHOD fill_required_field.
    IF zmm_ge_asn-vbeln IS INITIAL.
      MESSAGE 'Please fill the DC/invoice Number' TYPE 'S' DISPLAY LIKE 'E'.
      gv_field_flag = abap_true.
    ELSEIF zmm_ge_asn-ddate IS INITIAL.
      MESSAGE 'Please fill the DC/invoice Date' TYPE 'S' DISPLAY LIKE 'E'.
      gv_field_flag = abap_true.
    ELSEIF zmm_ge_asn-vehno IS INITIAL.
      MESSAGE 'Please fill the Vehicle Number' TYPE 'S' DISPLAY LIKE 'E'.
      gv_field_flag = abap_true.
    ELSEIF p_intime IS INITIAL.
      MESSAGE 'Please fill the Vehicle In-time' TYPE 'S' DISPLAY LIKE  'E'.
      gv_field_flag = abap_true.
    ELSEIF p_outtime IS INITIAL.
      MESSAGE 'Please fill the Vehicle Out-time' TYPE 'S' DISPLAY LIKE 'E'.
      gv_field_flag = abap_true.
    ELSEIF p_frbnr IS INITIAL.
      MESSAGE 'Please fill the Bill of Lading' TYPE 'S' DISPLAY LIKE 'E'.
      gv_field_flag = abap_true.
    ELSEIF p_d_note IS INITIAL.
      MESSAGE 'Please fill the Delivery Note' TYPE 'S' DISPLAY LIKE 'E'.
      gv_field_flag = abap_true.
    ELSEIF p_htext IS INITIAL.
      MESSAGE 'Please fill the Header Text' TYPE 'S' DISPLAY LIKE 'E'.
      gv_field_flag = abap_true.
    ELSE.
      READ TABLE gt_migo TRANSPORTING NO FIELDS WITH KEY lgort = space.
      IF sy-subrc = 0.
        MESSAGE 'Please fill the Storage Location' TYPE 'S' DISPLAY LIKE 'E'.
      ELSE.
        MESSAGE 'No Error Found' TYPE 'S' DISPLAY LIKE 'S'.
      ENDIF.
      CLEAR gv_field_flag.
    ENDIF.
      SELECT  matnr, mhdhb FROM mara
        for ALL ENTRIES IN @gt_migo
        WHERE matnr = @gt_migo-matnr
        INTO TABLE @DATA(lt_tot_shelf_life).

       LOOP AT gt_migo INTO data(ls_migo).
        READ TABLE lt_tot_shelf_life INTO data(ls_data) with key matnr = ls_migo-matnr BINARY SEARCH.
        IF sy-subrc = 0.
          IF ls_data-mhdhb > 0 and ls_migo-date_of_manufacture is INITIAL.
             MESSAGE 'Enter the Date of Manufacture' type 'S' DISPLAY LIKE 'E'.
             gv_field_flag = abap_true.
             ELSE.
           CLEAR gv_field_flag.
          ENDIF.
          ENDIF.
       ENDLOOP.
  ENDMETHOD.
  METHOD build_alv.

*<-------------------------------------------

    DATA: lr_column_1 TYPE REF TO cl_salv_column_list,
          lr_column_2 TYPE REF TO cl_salv_column_list.
    DATA: lo_events TYPE REF TO cl_salv_events_table.
    DATA: lo_events_g1 TYPE REF TO cl_salv_events_table.
    DATA: lo_columns TYPE REF TO cl_salv_columns_table,
           lo_column TYPE REF TO cl_salv_column_table.

* ------------------
* Create ALV screens
    LOOP AT gt_migo ASSIGNING FIELD-SYMBOL(<fs_migo>).
      SELECT SINGLE
            umrez,
            umren FROM marm
            INTO @DATA(lw_marm)
            WHERE matnr = @<fs_migo>-matnr AND
                  meinh = @<fs_migo>-meins.
      IF sy-subrc = 0.
        <fs_migo>-sku_qty = ( lw_marm-umrez / lw_marm-umren ) * <fs_migo>-migo_qty.
      ENDIF.
*      IF <fs_migo>-gravity NE 0.
*        <fs_migo>-sku_qty = <fs_migo>-migo_qty / <fs_migo>-gravity.
*      ENDIF.
    ENDLOOP.
*    CHECK gt_out_1 IS NOT INITIAL.
    IF go_alv_custom IS INITIAL OR
       gv_scr_100 = abap_true.
      CLEAR gv_scr_100.
      LOOP AT gt_out_1 ASSIGNING FIELD-SYMBOL(<fs_grid>).
        <fs_grid>-radio1 = icon_wd_radio_button_empty.  "Empty Radio Buttons
      ENDLOOP.
      TRY.
* Create Custo Container
          CREATE OBJECT go_alv_custom
            EXPORTING
              container_name = 'MCONTAINER'. "Name of Screen CustCtrl Name to Link

* Splitter - it is neccessary to specify the default_screen as parent

          gr_splitter = NEW #( parent = go_alv_custom
                              no_autodef_progid_dynnr   = abap_true

                              rows                      = 2

                              columns                   = 1 ).

* Container 1 for header

          gr_container_1 = gr_splitter->get_container(  row  = 1
                                                        column = 1 ).
* Container 2 for detail

          gr_container_2 = gr_splitter->get_container( row  = 2
                                                       column = 1 ).
*<< ALV1 - gt_alv_1 is the table with the data
          cl_salv_table=>factory( EXPORTING r_container             = gr_container_1
                                            container_name = ''
                                            IMPORTING r_salv_table  = gr_salv_1
                                            CHANGING t_table         = gt_out_1 ).

* Set ALV functions - should you wish to include any

          gr_salv_func_1 = gr_salv_1->get_functions( ).
          gr_salv_func_1->set_all( abap_true ).
          INCLUDE <icon>.
*          TRY.
*              gr_salv_func_1->add_function(
*                name     = 'EDIT'
*                icon     = CONV string( icon_select_all ) "icon_system_mark
*                text     = 'Select'
*                tooltip  = 'Select'
*                position = if_salv_c_function_position=>right_of_salv_functions ).
*            CATCH cx_salv_existing cx_salv_wrong_call.
*          ENDTRY.

*            CATCH cx_salv_not_found. " ALV: General Error Class (Checked in Syntax Check)

*Get the columns object
*lo_salv_columns = lo_salv_table->get_columns( ).

          TRY.
              gr_salv_func_1->add_function(
                name     = 'SPLIT'
                icon     = CONV string( icon_simulate )
                text     = 'Split'
                tooltip  = 'Split'
                position = if_salv_c_function_position=>right_of_salv_functions ).
            CATCH cx_salv_existing cx_salv_wrong_call.
          ENDTRY.
*          TRY.
*              gr_salv_func_1->add_function(
*                name     = 'OPEN'
*                icon     = CONV string( icon_open_folder )
*                text     = 'Get Open PO'
*                tooltip  = 'Get Open PO'
*                position = if_salv_c_function_position=>right_of_salv_functions ).
*            CATCH cx_salv_existing cx_salv_wrong_call.
*          ENDTRY.

* Register events
          lo_events_g1 = gr_salv_1->get_event( ).
*          gt_asn_split = gt_out_1.
          SET HANDLER on_user_command_g1 FOR lo_events_g1.
          SET HANDLER on_link_click FOR lo_events_g1.
          SET HANDLER handle_double_click_g1 FOR lo_events_g1.
** Set display settings as usual
*
*        DATA(lr_display_1) = gr_salv_1->get_display_settings( ).
* Selection - set as usual

          gr_salv_1->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>multiple ).
* Layout - set as usual

          DATA(ls_key_1) = VALUE salv_s_layout_key( report = sy-cprog
                                                    handle = '0001' ).
          DATA(lr_layout_1) = gr_salv_1->get_layout( ).
          lr_layout_1->set_key( ls_key_1 ).
*<< ALV_2 - details - gt_alv_2 is the table with data for the second ALV

          cl_salv_table=>factory( EXPORTING r_container             = gr_container_2
                                            container_name = 'MCONTAINER'
                                            IMPORTING r_salv_table  = gr_salv_2
                                            CHANGING t_table         = gt_migo ).
* ALV functions

          gr_salv_func_2 = gr_salv_2->get_functions( ).
          gr_salv_func_2->set_all( abap_true ).

          TRY.
              gr_salv_func_2->add_function(
                name     = 'TEST'
                icon     = CONV string( icon_simulate )
                text     = `Check`
                tooltip  = `Check`
                position = if_salv_c_function_position=>right_of_salv_functions ).
            CATCH cx_salv_existing cx_salv_wrong_call.
          ENDTRY.
          TRY.
              gr_salv_func_2->add_function(
                name     = 'MIGO'
                icon     = CONV string( icon_complete )
                text     = `MIGO`
                tooltip  = `MIGO`
                position = if_salv_c_function_position=>right_of_salv_functions ).
            CATCH cx_salv_existing cx_salv_wrong_call.
          ENDTRY.
          TRY.
              gr_salv_func_2->add_function(
                name     = 'EDIT'
                icon     = CONV string( icon_wd_text_edit )
                text     = `Edit`
                tooltip  = `Changes Values`
                position = if_salv_c_function_position=>right_of_salv_functions ).
            CATCH cx_salv_existing cx_salv_wrong_call.
          ENDTRY.
          TRY.
              gr_salv_func_2->add_function(
                name     = 'FILE'
                icon     = CONV string( icon_simulate )
                text     = `Attachment`
                tooltip  = `Attachment`
                position = if_salv_c_function_position=>right_of_salv_functions ).
            CATCH cx_salv_existing cx_salv_wrong_call.
          ENDTRY.

** Display settings - set as usual
*
*        DATA(lr_display_2) = gr_salv_2->get_display_settings( ).

* Layout - set as usual

          DATA(ls_key_2) = VALUE salv_s_layout_key( report = sy-cprog
                                                    handle = '0002' ).
* Set columns

* Set hotspot / Text on particular column
          DATA(lr_columns_1) = gr_salv_1->get_columns( ).
          lr_columns_1->set_optimize( abap_true ).
          lr_column_1 ?= lr_columns_1->get_column('MARK').
          lr_column_1->set_visible( abap_false ).
*          lr_column_1->set_cell_type( if_salv_c_cell_type=>checkbox ).
          lr_column_1 ?= lr_columns_1->get_column('RADIO1').
          lr_column_1->set_icon( if_salv_c_bool_sap=>true ).
          lr_column_1->set_cell_type( if_salv_c_cell_type=>hotspot ).
          lr_column_1->set_long_text( 'Select' ).
*          lr_column_1->set_visible( abap_false ).
          lr_column_1 ?= lr_columns_1->get_column('ASNNO').
          lr_column_1->set_visible( abap_false ).
          lr_column_1 ?= lr_columns_1->get_column('ASNDESC').
          lr_column_1->set_visible( abap_false ).
          lr_column_1 ?= lr_columns_1->get_column('INVOICENO').
          lr_column_1->set_visible( abap_false ).
          lr_column_1 ?= lr_columns_1->get_column('INVOICEDATE').
          lr_column_1->set_visible( abap_false ).
          lr_column_1 ?= lr_columns_1->get_column('CVEHICLENO').
          lr_column_1->set_visible( abap_false ).
****
          lr_column_1 ?= lr_columns_1->get_column( columnname = 'DATE_OF_MANUFACTURE' ).
          lr_column_1->set_visible( abap_false ).
****
*          lr_column_1 ?= lr_columns_1->get_column('MAKTX').
*          lr_column_1->set_visible( abap_false ).

          DATA(lr_columns_2) = gr_salv_2->get_columns( ).
          lr_columns_2->set_optimize( abap_true ).
          lr_column_2 ?= lr_columns_2->get_column('PO_QTY').
          lr_column_2->set_long_text( 'PO Quantity' ).
*        lr_column_2->set_medium_text( 'Open PO Qty' ).
*        lr_column_2->set_short_text( 'PO Qty' ).

          lr_column_2 ?= lr_columns_2->get_column('OPEN_QTY').
          lr_column_2->set_long_text( 'Open Qty' ).
          lr_column_2->set_medium_text( 'Open Qty' ).
          lr_column_2->set_short_text( 'Open Qty' ).

          lr_column_2 ?= lr_columns_2->get_column('DEL_QTY').
          lr_column_2->set_long_text( 'Delivered Qty' ).
          lr_column_2->set_medium_text( 'Delivered Qty' ).
          lr_column_2->set_short_text( 'Del. Qty' ).

          lr_column_2 ?= lr_columns_2->get_column('ASN_QTY').
          lr_column_2->set_long_text( 'ASN Billed Qty' ).
          lr_column_2->set_medium_text( 'Qty.Del.note' ).
          lr_column_2->set_short_text( 'Delv.Qy' ).

          lr_column_2 ?= lr_columns_2->get_column('MIGO_QTY').
          lr_column_2->set_long_text( 'Qty in Unit of Entry' ).
          lr_column_2->set_medium_text( 'Qty in UnitofEntry' ).
          lr_column_2->set_short_text( 'Qty.UOE' ).

          lr_column_2 ?= lr_columns_2->get_column('SKU_QTY').
          lr_column_2->set_long_text( 'Qty in SKU' ).
          lr_column_2->set_medium_text( 'Qty in SKU' ).
          lr_column_2->set_short_text( 'Qty.in.SKU' ).
          lr_column_2->set_output_length( '10' ).

          lr_column_2 ?= lr_columns_2->get_column('REMARK').
          lr_column_2->set_long_text( 'Item Text' ).
          lr_column_2->set_medium_text( 'Item Text' ).
          lr_column_2->set_short_text( 'Item.Text' ).
          lr_column_2->set_output_length( '50' ).

          lr_column_2 ?= lr_columns_2->get_column('GRAVITY').
          lr_column_2->set_long_text( 'Delivered Qty' ).
          lr_column_2->set_medium_text( 'Delivered Qty' ).
          lr_column_2->set_short_text( 'Gravity' ).
          lr_column_2->set_output_length( '10' ).

          lr_column_2 ?= lr_columns_2->get_column('TEMPRATURE').
          lr_column_2->set_long_text( 'Temprature' ).
          lr_column_2->set_medium_text( 'Temprature' ).
          lr_column_2->set_short_text( 'Temprature' ).

          lr_column_2 ?= lr_columns_2->get_column('NETPR').
          lr_column_2->set_long_text( 'Unit Price' ).
          lr_column_2->set_medium_text( 'Unit Price' ).
          lr_column_2->set_short_text( 'Unit Price' ).

          lr_column_2 ?= lr_columns_2->get_column('TOTAL').
          lr_column_2->set_long_text( 'Total Price' ).
          lr_column_2->set_medium_text( 'Total Price' ).
          lr_column_2->set_short_text( 'Total' ).

          lr_column_2 ?= lr_columns_2->get_column('FLAG').
          lr_column_2->set_visible( abap_false ).

          lr_column_2 ?= lr_columns_2->get_column('CHARG').
          lr_column_2->set_visible( abap_false ).

          lr_column_2 ?= lr_columns_2->get_column('LGORT').
          DATA  p_ddic TYPE salv_s_ddic_reference.
          p_ddic-table = 'T001L'.
          p_ddic-field = 'LGORT'.
          lr_column_2->set_ddic_reference( p_ddic ).
          lr_column_2->set_f4( if_salv_c_bool_sap=>true ).

          lr_column_2 ?= lr_columns_2->get_column('MEINS').
          p_ddic-table = 'MARA'.
          p_ddic-field = 'MEINS'.
          lr_column_2->set_ddic_reference( p_ddic ).
          lr_column_2->set_f4( if_salv_c_bool_sap=>true ).

          lr_column_2 ?= lr_columns_2->get_column('DATE_OF_MANUFACTURE').
          p_ddic-table = 'EKKO'.
          p_ddic-field = 'AEDAT'.
          lr_column_2->set_ddic_reference( p_ddic ).
          lr_column_2->set_f4( if_salv_c_bool_sap=>true ).
          lr_column_2->set_ddic_reference( p_ddic ).
          lr_column_2->set_long_text( 'Date of Manufacture' ).
          lr_column_2->set_medium_text( 'Date of Manufacture' ).
          lr_column_2->set_short_text( 'Manuf.date' ).
          lr_column_2->set_output_length( '10' ).


*        lr_column_1->set_cell_type( if_salv_c_cell_type=>hotspot ).
** ALV events - the implementation of the on link click event follows further in the post

* Register events
          lo_events = gr_salv_2->get_event( ).

          SET HANDLER on_user_command FOR lo_events.
          SET HANDLER handle_double_click FOR lo_events.
*
**        gr_salv_1->set_list_header( 'Invoice Details' ).
*
*          DATA : lref_structype TYPE  REF TO cl_abap_structdescr,
*                 lt_dis_comps   TYPE abap_component_tab,
*                 ls_comp        TYPE abap_componentdescr,
*                 ls_scol        TYPE lvc_s_colo,
*                 lt_colors      TYPE lvc_t_scol,
*                 l_fname        TYPE lvc_fname.
*          DATA: lo_header  TYPE REF TO cl_salv_form_layout_grid,
*                lo_h_label TYPE REF TO cl_salv_form_label,
*                lo_h_flow  TYPE REF TO cl_salv_form_layout_flow.
*
*          lref_structype ?= cl_abap_typedescr=>describe_by_data( wa_data ).
*          lt_dis_comps = lref_structype->get_components( ).
*
*          LOOP AT lt_dis_comps INTO ls_comp.
*            l_fname = ls_comp-name.
*            lr_column_2 ?= lr_columns_2->get_column( l_fname ).
*            ls_scol-col = 7.
*            ls_scol-int = 1.
*            ls_scol-inv = 0.
*            lr_column_2->set_color( value = ls_scol ).
*          ENDLOOP.
*
**   header object
*          CREATE OBJECT lo_header.
**
**   To create a Lable or Flow we have to specify the target
**     row and column number where we need to set up the output
**     text.
**
*          DATA: lo_functions TYPE REF TO cl_salv_functions_list.
**   Default Functions
*          lo_functions = gr_salv_1->get_functions( ).
*          lo_functions->set_default( abap_true ).
*
**   information in Bold
*          lo_h_label = lo_header->create_label( row = 1 column = 1 ).
*          lo_h_label->set_text( 'Invoice Details' ).
*
**   set the top of list using the header for Online.
*          gr_salv_1->set_top_of_list( lo_header ).
**
**   set the top of list using the header for Print.
*          gr_salv_1->set_top_of_list_print( lo_header ).

          gr_salv_1->display( ).

*<< Display ALV_2,

          gr_salv_2->display( ).

*<< Have to be to display ALV in full screen mode

          WRITE: space.

        CATCH cx_salv_msg cx_salv_not_found cx_salv_data_error cx_salv_access_error INTO DATA(lx_alv).
          MESSAGE lx_alv->get_text( ) TYPE 'I' DISPLAY LIKE 'E'.
*        DATA(lv_message) = lx_alv->get_text( ).

*        raise_sy( ).

      ENDTRY.
    ELSE.
      IF gr_salv_1 IS NOT INITIAL.
        gr_salv_1->refresh( ).
      ENDIF.
      IF gr_salv_2 IS NOT INITIAL.
        gr_salv_2->refresh( ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
*  METHOD display_popup_alv.
*    IF lo_alv_custom IS BOUND AND
*       gv_newasn IS INITIAL.
*      CALL METHOD lo_alv_custom->free.
*      CLEAR lo_alv_custom.
*    ENDIF.
*
*    IF lo_alv_custom IS INITIAL OR
*       gv_scr_101 IS INITIAL.
*      gv_scr_101 = abap_true.
** Create Custo Container
*      CREATE OBJECT lo_alv_custom
*        EXPORTING
*          container_name = 'MCONTAINER'. "Name of Screen CustCtrl Name to Link
*
**<< ALV1 - gt_alv_1 is the table with the data
*      cl_salv_table=>factory( EXPORTING r_container             = lo_alv_custom
*                                        container_name = ''
*                                        IMPORTING r_salv_table  = lo_popup_alv
*                                        CHANGING t_table         = gt_output ).
** Set ALV functions - should you wish to include any
*
*
**    ENDIF.
*
*      DATA: lr_functions TYPE REF TO cl_salv_functions_list,
*            lr_salv_func TYPE REF TO cl_salv_functions.
*      DATA: lr_column_1  TYPE REF TO cl_salv_column_list.
*      DATA: lo_events TYPE REF TO cl_salv_events_table.
*
*      lr_functions = lo_popup_alv->get_functions( ).
*      lr_functions->set_all( 'X' ).
*
*      DATA(lr_columns_1) = lo_popup_alv->get_columns( ).
**    lr_columns_1->set_optimize( abap_true ).
**    IF gt_output IS NOT INITIAL.
**      lr_column_1 ?= lr_columns_1->get_column('ASNNO').
**      lr_column_1->set_visible( abap_false ).
**    ENDIF.
*      lr_column_1 ?= lr_columns_1->get_column('ASNDESC').
*      lr_column_1->set_visible( abap_false ).
*      lr_column_1 ?= lr_columns_1->get_column('INVOICENO').
*      lr_column_1->set_visible( abap_false ).
*      lr_column_1 ?= lr_columns_1->get_column('INVOICEDATE').
*      lr_column_1->set_visible( abap_false ).
*      lr_column_1 ?= lr_columns_1->get_column('CVEHICLENO').
*      lr_column_1->set_visible( abap_false ).
*
*      TRY.
*          lr_functions->add_function(
*          name     = 'NEWASN'
*          icon     = CONV string( icon_wd_text_edit )
*          text     = `New Vendor Invoice`
*          tooltip  = `Create new ASN`
*          position = if_salv_c_function_position=>right_of_salv_functions ).
*        CATCH cx_salv_existing cx_salv_wrong_call.
*      ENDTRY.
*
*      TRY.
*          lr_functions->add_function(
*          name     = 'CONFIRM'
*          icon     = CONV string( icon_wd_text_edit )
*          text     = `Confirm`
*          tooltip  = `Create new ASN`
*          position = if_salv_c_function_position=>right_of_salv_functions ).
*        CATCH cx_salv_existing cx_salv_wrong_call.
*      ENDTRY.
*      lo_events = lo_popup_alv->get_event( ).
**
*      SET HANDLER on_user_command_pop_up FOR lo_events.
*      SET HANDLER handle_double_click_popup FOR lo_events.
*      lo_popup_alv->display( ).
*    ELSE.
*      IF lo_popup_alv IS NOT INITIAL.
*        lo_popup_alv->refresh( ).
*      ENDIF.
*    ENDIF.
*  ENDMETHOD.
  METHOD fetch_invoice_data.
    IF gt_asn_split IS INITIAL.
      CLEAR: gt_out_1,gt_migo,gt_matnr_inv[],gt_matnr_po[],gt_po_item_qty[].
      SELECT SINGLE mblnr
        FROM zmm_ge_asn INTO @DATA(l_mblnr)
        WHERE asnno = @zmm_ge_asn-asnno AND
              lifnr = @zmm_ge_asn-lifnr.
      IF sy-subrc = 0.
        DATA(l_asn) = zmm_ge_asn-asnno.
        CONDENSE l_asn.
        DATA(l_text) = |ASNo: { l_asn } already processed with GRN No-{ l_mblnr }|.
        MESSAGE l_text TYPE 'S' DISPLAY LIKE 'E'.
      ELSE.

        gt_out_1 = gt_output[].

        DELETE gt_out_1 WHERE asnno NE zmm_ge_asn-asnno.

        READ TABLE gt_out_1 INTO DATA(lw_out) INDEX 1.
        IF sy-subrc = 0 AND gv_newasn IS INITIAL.
          zmm_ge_asn-orbyr = 'IN'.
          zmm_ge_asn-asnds = lw_out-asndesc.
          zmm_ge_asn-vbeln = lw_out-invoiceno.
          zmm_ge_asn-ddate =  lw_out-invoicedate.
          zmm_ge_asn-vehno = lw_out-cvehicleno.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD fetch_purchase_data.
    CHECK gt_out_1 IS NOT INITIAL.
    CLEAR gt_migo[].
    SELECT  a~ebeln
            b~ebelp
            a~bedat
            b~matnr
            b~txz01
            b~werks
            c~menge AS po_qty
            b~meins
            c~wemng AS del_qty
            b~netpr
            INTO CORRESPONDING FIELDS OF TABLE me->gt_out_2
            FROM ekko AS a
            INNER JOIN ekpo AS b ON a~ebeln = b~ebeln
            INNER JOIN eket AS c ON b~ebeln = c~ebeln AND
                                    b~ebelp = c~ebelp
            FOR ALL ENTRIES IN  me->gt_out_1
            WHERE
                  a~bsart = 'ZNB' AND
                  a~frgke = '3' AND
                  a~procstat = '05' AND
                  a~lifnr = zmm_ge_asn-lifnr AND
                  b~werks = zmm_ge_asn-werks AND
                  b~matnr =  me->gt_out_1-cmaterialno AND
                  b~loekz EQ '' AND
                  b~elikz = space .

    SORT me->gt_out_1 BY cmaterialno.
    SORT me->gt_out_2 BY matnr.

*      marc-

    CLEAR: gt_automigo_log[],gt_inv_log[].
    LOOP AT gt_out_1 ASSIGNING FIELD-SYMBOL(<fs_out1>).
      CLEAR gw_inv_log.
      MOVE-CORRESPONDING zmm_ge_asn TO gw_inv_log.
      gw_inv_log-matnr   = <fs_out1>-cmaterialno.
      gw_inv_log-charg   = <fs_out1>-cbatchno.
      gw_inv_log-menge = <fs_out1>-nasnqty.
      APPEND gw_inv_log TO gt_inv_log.

      gw_matnr-matnr = <fs_out1>-cmaterialno.
      gw_matnr-qty   = <fs_out1>-nasnqty.
      COLLECT gw_matnr INTO gt_matnr_inv.
    ENDLOOP.

    LOOP AT gt_out_2 ASSIGNING FIELD-SYMBOL(<fs_out2>).
      <fs_out2>-open_qty = <fs_out2>-po_qty - <fs_out2>-del_qty.

      SELECT SINGLE lgfsb FROM marc
             INTO <fs_out2>-lgort WHERE matnr = <fs_out2>-matnr
                    AND werks = p_werks.
      gw_matnr-matnr = <fs_out2>-matnr.
      gw_matnr-qty   = <fs_out2>-open_qty.
      COLLECT gw_matnr INTO gt_matnr_po.
      gw_po_item_qty-ebeln = <fs_out2>-ebeln.
      gw_po_item_qty-ebelp = <fs_out2>-ebelp.
      gw_po_item_qty-matnr = <fs_out2>-matnr.
      gw_po_item_qty-qty   = <fs_out2>-open_qty.
      COLLECT gw_po_item_qty INTO gt_po_item_qty.
    ENDLOOP.

    DELETE gt_out_2 WHERE open_qty = 0.

    DATA(lt_inv) = me->gt_out_1[].
    DATA(lt_po) = me->gt_out_2[].

    LOOP AT lt_inv ASSIGNING FIELD-SYMBOL(<fs_inv>).
      LOOP AT lt_po ASSIGNING FIELD-SYMBOL(<fs_po>)
                                    WHERE matnr = <fs_inv>-cmaterialno AND
                                          flag = space.
        IF <fs_po>-open_qty <= 0.
          <fs_po>-flag = 'X'.
          CONTINUE.
        ENDIF.
        IF <fs_po>-open_qty >= <fs_inv>-nasnqty.
          <fs_po>-migo_qty = <fs_inv>-nasnqty.
          <fs_po>-charg = <fs_inv>-cbatchno.
          MOVE-CORRESPONDING <fs_inv> TO gw_automigo_log.
          MOVE-CORRESPONDING <fs_po> TO gw_automigo_log.
          MOVE-CORRESPONDING zmm_ge_asn TO gw_automigo_log.
          gw_automigo_log-charg = <fs_inv>-cbatchno.
          gw_automigo_log-lfimg = <fs_po>-migo_qty.
          gw_automigo_log-matnr = <fs_po>-matnr.
          <fs_po>-txz01 = <fs_inv>-maktx.
          APPEND gw_automigo_log TO gt_automigo_log. CLEAR gw_automigo_log.
          <fs_po>-total = <fs_po>-migo_qty * <fs_po>-netpr.
          <fs_po>-temprature = '30'.
          <fs_po>-asn_qty = <fs_po>-migo_qty.
          <fs_po>-date_of_manufacture = <fs_inv>-date_of_manufacture.
          APPEND <fs_po> TO gt_migo.
          <fs_po>-open_qty = <fs_po>-open_qty - <fs_inv>-nasnqty.
          READ TABLE gt_po_item_qty ASSIGNING FIELD-SYMBOL(<lf_po_item_qty>) WITH KEY ebeln = <fs_po>-ebeln
                                                                                      ebelp = <fs_po>-ebelp
                                                                                      matnr = <fs_po>-matnr.
          IF sy-subrc = 0.
            <lf_po_item_qty>-qty = <lf_po_item_qty>-qty - <fs_po>-migo_qty.
          ENDIF.

          IF <lf_po_item_qty>-qty <= 0.
            <fs_po>-flag = 'X'.
          ENDIF.
          EXIT.
        ELSE.
          <fs_po>-migo_qty = <fs_po>-open_qty.
          <fs_po>-charg = <fs_inv>-cbatchno.
          MOVE-CORRESPONDING <fs_inv> TO gw_automigo_log.
          MOVE-CORRESPONDING <fs_po> TO gw_automigo_log.
          MOVE-CORRESPONDING zmm_ge_asn TO gw_automigo_log.
          <fs_po>-txz01 = <fs_inv>-maktx.
          gw_automigo_log-charg = <fs_inv>-cbatchno.
          gw_automigo_log-lfimg = <fs_po>-migo_qty.
          gw_automigo_log-matnr = <fs_po>-matnr.
          APPEND gw_automigo_log TO gt_automigo_log. CLEAR gw_automigo_log.
          <fs_po>-total = <fs_po>-migo_qty * <fs_po>-netpr.
          <fs_po>-temprature = '30'.
          <fs_po>-asn_qty = <fs_po>-migo_qty.
          APPEND <fs_po> TO gt_migo.
          <fs_inv>-nasnqty =  <fs_inv>-nasnqty - <fs_po>-open_qty.
          <fs_po>-open_qty = <fs_po>-open_qty - <fs_inv>-nasnqty.
          READ TABLE gt_po_item_qty ASSIGNING <lf_po_item_qty> WITH KEY ebeln = <fs_po>-ebeln
                                                                        ebelp = <fs_po>-ebelp
                                                                        matnr = <fs_po>-matnr.
          IF sy-subrc = 0.
            <lf_po_item_qty>-qty = <lf_po_item_qty>-qty - <fs_po>-migo_qty.
          ENDIF.
          IF <lf_po_item_qty>-qty <= 0.
            <fs_po>-flag = 'X'.
          ENDIF.
          CONTINUE.
        ENDIF.

      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
  METHOD process_migo.

    DATA: lt_tab TYPE esp1_message_tab_type.
    DATA: ls_tab TYPE esp1_message_wa_type.
    DATA: lt_delv_note_qty TYPE STANDARD TABLE OF zmm_st_asn_delv_qty,
          lw_delv_note_qty TYPE zmm_st_asn_delv_qty,
          answer           TYPE c.

    DATA: lt_class      TYPE STANDARD TABLE OF  sclass,
          lt_objectdata TYPE STANDARD TABLE OF  clobjdat.
    DATA: lt_objectkeytable  TYPE  TABLE OF bapi1003_object_keys.
    DATA: lw_objectkeytable LIKE LINE OF lt_objectkeytable.
    DATA: lv_objectkeys TYPE bapi1003_key-object,
          lv_status     TYPE c.
* holds data for charcateristics with type CHAR/DATE
    DATA: lt_val_char TYPE TABLE OF bapi1003_alloc_values_char,
          lw_val_char TYPE bapi1003_alloc_values_char,
          lt_return   TYPE TABLE OF bapiret2,
          lv_objkey   TYPE ausp-objek.
*    LOOP AT gt_out_1 INTO DATA(w_out_1).
*      IF w_out_1-cbatchno CS ',;'.
*        DATA(l_txt) = |ASN Batch have special symbols-{ w_out_1-cbatchno }. Please Split the Batch or Ignore if its not required.|.
*        MESSAGE l_txt  TYPE 'E' DISPLAY LIKE 'I'.
*      ENDIF.
*    ENDLOOP.
    LOOP AT gt_out_1 INTO DATA(w_out_1).
      IF w_out_1-cbatchno CS ';'.
        DATA(l_spl_flag) = abap_true.
        EXIT.
      ENDIF.
      IF w_out_1-cbatchno CS ','.
        l_spl_flag  = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.
    IF l_spl_flag = abap_true.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Inward Gate Entry Confirmation'
          text_question         = 'ASN Batch have special symbols.Please Split the Batch or Ignore if its not required'
          text_button_1         = 'Confirm'(101)
          text_button_2         = 'Goback'(102)
          default_button        = '1'
          display_cancel_button = ''
          start_column          = 25
          start_row             = 6
        IMPORTING
          answer                = answer
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.
      CHECK  answer = '1' .
      CLEAR l_spl_flag.
    ENDIF.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Inward Gate Entry Confirmation'
        text_question         = 'Do you want to Confirm for Create GRN'
        text_button_1         = 'Confirm'(101)
        text_button_2         = 'Goback'(102)
        default_button        = '1'
        display_cancel_button = ''
        start_column          = 25
        start_row             = 6
      IMPORTING
        answer                = answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.
    CHECK  answer = '1' .

    LOOP AT gt_matnr_inv INTO gw_matnr.
      READ TABLE gt_matnr_po ASSIGNING FIELD-SYMBOL(<fs_matnr_po>) WITH KEY matnr = gw_matnr-matnr.
      IF sy-subrc = 0 .
        IF <fs_matnr_po>-qty < gw_matnr-qty .
          ls_tab-lineno = sy-tabix.
          ls_tab-msgid  = 'ZSD'.
          ls_tab-msgno  = 004.
          ls_tab-msgty  = 'E'.
          ls_tab-msgv1  = gw_matnr-matnr.
*          ls_tab-msgv2  =  'Open quantities is less than Invoice quantities'.
          APPEND ls_tab TO lt_tab.
        ENDIF.
      ELSE.
        ls_tab-lineno = sy-tabix.
        ls_tab-msgid  = 'ZSD'.
        ls_tab-msgno  = 005.
        ls_tab-msgty  = 'E'.
        ls_tab-msgv1  = gw_matnr-matnr.
*        ls_tab-msgv2  =  'No Open quantities found in PO''s'.
        APPEND ls_tab TO lt_tab.
      ENDIF.
    ENDLOOP.
    IF lt_tab IS NOT INITIAL.
      CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
        TABLES
          i_message_tab = lt_tab.
      EXIT .
    ENDIF.
    DATA: l_charg     TYPE charg_d,
          l_charg_new TYPE charg_d,
          l_matnr18   TYPE matnr18.

    CLEAR: gw_goodsmvt_item, gt_goodsmvt_item.

    LOOP AT gt_migo INTO wa_data.
      IF l_charg IS INITIAL.
        l_charg = wa_data-charg.
        l_matnr18 = wa_data-matnr.
        IF p_test = abap_true.
*          gw_goodsmvt_item-batch = 'TEST123'.
          l_charg_new = 'TEST123'.
        ELSE.
          SELECT SINGLE mtart FROM mara INTO @DATA(l_mtype) WHERE matnr = @l_matnr18.
          IF l_mtype = 'FERT' OR l_mtype = 'HAWA'.
            l_charg_new = wa_data-charg.
          ELSE.
            CALL FUNCTION 'BAPI_BATCH_CREATE'
              EXPORTING
                material = l_matnr18
                plant    = p_werks
              IMPORTING
                batch    = l_charg_new.
          ENDIF.
          CLEAR l_mtype.
        ENDIF.
      ENDIF.

*<<<<<BAPI Communication Structure: Material Document Item Data>>>>>>>>>
      gw_goodsmvt_item-move_type            = '101'.
      gw_goodsmvt_item-mvt_ind              = 'B'.
      gw_goodsmvt_item-plant                = wa_data-werks.
      gw_goodsmvt_item-material             = wa_data-matnr.
      gw_goodsmvt_item-entry_qnt            = wa_data-migo_qty.

      gw_goodsmvt_item-move_stloc           = wa_data-lgort.
      gw_goodsmvt_item-stge_loc             = wa_data-lgort.
      gw_goodsmvt_item-entry_uom            = wa_data-meins.
      gw_goodsmvt_item-vendrbatch           = wa_data-charg.
      IF l_charg <> wa_data-charg OR
         l_matnr18 <> wa_data-matnr.
        IF l_matnr18 <> wa_data-matnr.
          l_matnr18 = wa_data-matnr.
        ENDIF.
        IF l_charg <> wa_data-charg.
          l_charg = wa_data-charg.
        ENDIF.
        IF p_test = abap_true.
          gw_goodsmvt_item-batch = 'TEST123'.
        ELSE.
          SELECT SINGLE mtart FROM mara INTO @l_mtype WHERE matnr = @l_matnr18.
          IF l_mtype = 'FERT' OR l_mtype = 'HAWA'.
            gw_goodsmvt_item-batch = wa_data-charg.
          ELSE.
            CALL FUNCTION 'BAPI_BATCH_CREATE'
              EXPORTING
                material = l_matnr18
                plant    = p_werks
              IMPORTING
                batch    = l_charg_new.
            gw_goodsmvt_item-batch = l_charg_new.
          ENDIF.
          CLEAR l_mtype.
        ENDIF.
      ELSE.
        gw_goodsmvt_item-batch = l_charg_new .
      ENDIF.
      gw_goodsmvt_item-po_number            = wa_data-ebeln.
      gw_goodsmvt_item-po_item              = wa_data-ebelp.
*      CONCATENATE pcitab-del_no pcitab-del_item INTO itab-item_text.
*      itab-move_reas  = pcitab-scrap_reason.
      gw_goodsmvt_item-item_text            = wa_data-remark."wa_data-txz01.
*      gw_goodsmvt_item-no_more_gr           = 'X'.
      data(ls_flag) = VALUE xfeld( ).
*      PERFORM Shelf_life_validation USING wa_data CHANGING gw_goodsmvt_item.
*      SELECT SINGLE mhdhb FROM mara
*        INTO @DATA(l_tot_self_life)
*        WHERE matnr = @wa_data-matnr.
*      IF l_tot_self_life GT 0.
**        gw_goodsmvt_item-prod_date = sy-datum.
*        gw_goodsmvt_item-prod_date = wa_data-date_of_manufacture.
*      ENDIF.
      gw_goodsmvt_item-prod_date = wa_data-date_of_manufacture.
      APPEND  gw_goodsmvt_item TO gt_goodsmvt_item.

*<<<<<<<Post goods movements with MB_CREATE_GOODS_MOVEMENT>>>>>>>>>>
      IF p_test IS INITIAL.
        LOOP AT gt_automigo_log ASSIGNING FIELD-SYMBOL(<fs_log>)
                                WHERE   ebeln = wa_data-ebeln AND
                                        ebelp = wa_data-ebelp AND
                                        matnr = wa_data-matnr AND
                                        charg = wa_data-charg.
          <fs_log>-lgort  = wa_data-lgort.
          <fs_log>-charg_v  =  <fs_log>-charg.
          <fs_log>-charg = gw_goodsmvt_item-batch.
        ENDLOOP.

        IF wa_data-migo_qty <> wa_data-asn_qty.
          lw_delv_note_qty-ebeln = wa_data-ebeln.
          lw_delv_note_qty-ebelp = wa_data-ebelp.
          lw_delv_note_qty-batch = gw_goodsmvt_item-batch.
          lw_delv_note_qty-lsmng = wa_data-asn_qty.
          APPEND lw_delv_note_qty TO lt_delv_note_qty.
        ENDIF.

* Update Batch Classification of charcateristics type SPECIFIC_GRAVITY and TEMPERATURE_SPGR

        lv_objkey = wa_data-matnr.
        CALL FUNCTION 'CLAF_CLASSIFICATION_OF_OBJECTS'
          EXPORTING
            classtype    = '023'
            object       = lv_objkey
            objecttable  = 'MARA'
          TABLES
            t_class      = lt_class
            t_objectdata = lt_objectdata.
        IF sy-subrc <> 0.
*       Implement suitable error handling here
        ELSE.
          DATA(l_class) = VALUE #( lt_class[ 1 ]-class OPTIONAL ).
          IF l_class = 'ACTIVE_SOLVENT'.

            lw_objectkeytable-key_field = 'MATNR'.
            lw_objectkeytable-value_int = wa_data-matnr.
            APPEND lw_objectkeytable TO lt_objectkeytable.

            lw_objectkeytable-key_field = 'CHARG'.
            lw_objectkeytable-value_int = gw_goodsmvt_item-batch.
            APPEND lw_objectkeytable TO lt_objectkeytable.

            lw_objectkeytable-key_field = 'WERKS'.
            lw_objectkeytable-value_int = wa_data-werks.

            APPEND lw_objectkeytable TO lt_objectkeytable.

            CALL FUNCTION 'BAPI_OBJCL_CONCATENATEKEY'
              EXPORTING
                objecttable    = 'MCH1'
              IMPORTING
                objectkey_conc = lv_objectkeys
              TABLES
                objectkeytable = lt_objectkeytable
                return         = lt_return.

            lw_val_char-charact = 'SPECIFIC_GRAVITY'.
*          lw_val_char-value_char = 'SPECIFIC_GRAVITY'.
            lw_val_char-value_neutral = wa_data-gravity.
            APPEND lw_val_char TO lt_val_char.

            lw_val_char-charact = 'TEMPERATURE_SPGR'.
*          lw_val_char-value_char = 'TEMPERATURE_SPGR'.
            lw_val_char-value_neutral = wa_data-temprature.
            APPEND lw_val_char TO lt_val_char.

            CALL FUNCTION 'BAPI_OBJCL_CREATE'
              EXPORTING
                objectkeynew    = lv_objectkeys
                objecttablenew  = 'MCH1'
                classnumnew     = 'ACTIVE_SOLVENT'
                classtypenew    = '023'
              IMPORTING
                classif_status  = lv_status
              TABLES
                allocvalueschar = lt_val_char
                return          = lt_return.
            IF VALUE #( lt_return[ type = 'E' ]-type OPTIONAL ) = 'E'.
              DATA: lt_allocnum  TYPE STANDARD TABLE OF  bapi1003_alloc_values_num,
                    lt_alloccurr TYPE STANDARD TABLE OF  bapi1003_alloc_values_curr.
              CALL FUNCTION 'BAPI_OBJCL_CHANGE'
                EXPORTING
                  objectkey          = lv_objectkeys
                  objecttable        = 'MCH1'
                  classnum           = 'ACTIVE_SOLVENT'
                  classtype          = '023'
                TABLES
                  return             = lt_return
                  allocvaluesnumnew  = lt_allocnum[]
                  allocvaluescharnew = lt_val_char[]
                  allocvaluescurrnew = lt_alloccurr[].
            ENDIF.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait   = 'X'
              IMPORTING
                return = l_return.
          ENDIF.
        ENDIF.
        CLEAR: l_class,lv_objkey,
               lw_objectkeytable,
               lt_objectkeytable,
               lt_return,
               lv_objectkeys,
               lt_class,
               lt_objectdata,
               lt_val_char,
               lw_val_char.
      ENDIF.
    ENDLOOP.

    goodsmvt_code_tmp               = '01'. "01 - MB01 - Goods Receipts for Purchase Order
    gw_goodsmvt_header-pstng_date   = sy-datum.
    gw_goodsmvt_header-doc_date     = zmm_ge_asn-ddate.
    gw_goodsmvt_header-pr_uname     = sy-uname.
    gw_goodsmvt_header-header_txt   =  p_htext. "|{ wa_data-ebeln }/1|.
    gw_goodsmvt_header-bill_of_lading = p_frbnr.
    gw_goodsmvt_header-ref_doc_no     = p_d_note.
    IF p_test IS INITIAL.
* Update vai Enchancement for add Quantity in Delivery Note
      IF lt_delv_note_qty IS NOT INITIAL.
        EXPORT my_tab FROM lt_delv_note_qty TO MEMORY ID 'C_ASN_DELV_QTY'. " Ench Spot FM: MAP2I_B2017_GM_ITEM_TO_IMSEG
      ENDIF.
    ENDIF.
    CALL FUNCTION 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]"Added by SPLABAP during code remediation
      EXPORTING
        goodsmvt_header  = gw_goodsmvt_header
        goodsmvt_code    = goodsmvt_code_tmp
        testrun          = p_test
      IMPORTING
        goodsmvt_headret = gw_goodsmvt_headret
      TABLES
        goodsmvt_item    = gt_goodsmvt_item
*       GOODSMVT_SERIALNUMBER =
        return           = gt_return.

    IF gt_return[] IS INITIAL.
      IF p_test = abap_true.
        DATA(l_text) = 'Simulation: No Error found'.
        MESSAGE l_text TYPE 'I' DISPLAY LIKE 'S'.
      ELSE.
        FREE MEMORY ID 'C_ASN_DELV_QTY'.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait   = 'X'
          IMPORTING
            return = l_return.
        IF l_return IS INITIAL.
          LOOP AT gt_inv_log ASSIGNING FIELD-SYMBOL(<fs_inv>).
            <fs_inv>-mblnr = gw_goodsmvt_headret-mat_doc.
            <fs_inv>-orbyr = zmm_ge_asn-orbyr.
            <fs_inv>-vbeln = zmm_ge_asn-vbeln.
            <fs_inv>-ddate = zmm_ge_asn-ddate.
            <fs_inv>-vehno = zmm_ge_asn-vehno.
            <fs_inv>-intim = p_intime.
            <fs_inv>-outim = p_outtime.
          ENDLOOP.
          LOOP AT gt_automigo_log ASSIGNING <fs_log>.
            <fs_log>-mblnr = gw_goodsmvt_headret-mat_doc.
          ENDLOOP.

*          CLEAR : nriv.
*          SELECT SINGLE * FROM nriv INTO nriv
*            WHERE object    = 'ZASN_SNRO'
*              AND subobject = p_werks
*              AND nrrangenr = '01'
*              AND toyear  = gv_gjahr.
*          IF nriv-nrlevel IS INITIAL.
*            nriv-nrlevel = nriv-fromnumber.
*          ELSE.
*            nriv-nrlevel = nriv-nrlevel + 1.
*          ENDIF.
*
*          MODIFY nriv FROM nriv.
          MODIFY zmm_ge_asn_migo FROM TABLE gt_automigo_log.
          MODIFY zmm_ge_asn FROM TABLE gt_inv_log.

          me->update_asn_migo_table( ).


          IF gv_fname IS NOT INITIAL.
            DATA(flag) = space.
            DATA l_fname TYPE string.
            l_fname = gv_fname.
            DATA lo_file_att TYPE REF TO zcl_attach_document.
            CREATE OBJECT lo_file_att.
            CALL METHOD lo_file_att->upload_file
              EXPORTING
*               bukrs                     =
                gjahr                     = gw_goodsmvt_headret-doc_year
*               belnr                     =
                mblnr                     = gw_goodsmvt_headret-mat_doc
                objecttype                = 'BUS2017'
                documenttype              = 'ZGR'
                fname                     = l_fname
              IMPORTING
                upload_status             = flag
              EXCEPTIONS
                error_archive             = 1
                error_communicationtable  = 2
                error_connectiontable     = 3
                error_kernel              = 4
                error_parameter           = 5
                document_number_not_found = 6
                OTHERS                    = 7.
          ENDIF.
          l_text = |GRN: { gw_goodsmvt_headret-mat_doc } Sucessfully Uploadad|. "+0(10)
          MESSAGE l_text TYPE 'I' DISPLAY LIKE 'S'.
        ENDIF.
      ENDIF.
    ELSE.
      LOOP AT gt_return INTO DATA(lw_ret).
        ls_tab-lineno = lw_ret-row.
        ls_tab-msgid  = lw_ret-id.
        ls_tab-msgno  = lw_ret-number.
        ls_tab-msgty  = lw_ret-type.
        ls_tab-msgv1  = lw_ret-message_v1.
        ls_tab-msgv2  = lw_ret-message.
        APPEND ls_tab TO lt_tab.
      ENDLOOP.

      CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
        TABLES
          i_message_tab = lt_tab.
    ENDIF.

    CLEAR:wa_data, gt_goodsmvt_item.
    REFRESH:gt_goodsmvt_item.
  ENDMETHOD.
  METHOD update_asn_migo_table.

    TYPES: BEGIN OF ty_msg,
             vendor  TYPE string,
             asnno   TYPE string,
             migo_no TYPE string,
             po_no   TYPE string,
             status  TYPE string,
           END OF ty_msg.

    TYPES: BEGIN OF ty_status,
             vendor TYPE string,
             asnno  TYPE string,
             status TYPE string,
           END OF ty_status.

    DATA: lo_http_client TYPE REF TO if_http_client.

    DATA: lv_response   TYPE string, "API Response
          lv_codes      TYPE i,      "STATUS Code
          lv_http_error TYPE string. "STATUS Description
    DATA: lv_response1 TYPE string.
    DATA: lv_date(10) TYPE c.

    DATA: gt_response TYPE TABLE OF ty_msg.
    DATA: gw_response TYPE ty_msg.
    DATA: gt_status TYPE TABLE OF ty_status.
    DATA: lt_ge_asn TYPE STANDARD TABLE OF zmm_ge_asn.
    DATA : v_jsonload TYPE string.

    DATA : lv_bearer_token TYPE string.
    DATA : lv_msg TYPE string.
    DATA : l_uname TYPE  syst_uname.
    DATA : l_password TYPE  char50.

    SELECT SINGLE low FROM tvarvc
      INTO l_password
      WHERE name = 'ZANS_UPD_PWORD'
      AND   type = 'P'.
    SELECT SINGLE low FROM tvarvc
      INTO l_uname
      WHERE name = 'ZANS_UPD_UNAME'
      AND   type = 'P'.

    CALL FUNCTION 'ZGET_BEARER_TOKEN_MIS'
      EXPORTING
        username          = l_uname
        password          = l_password
      IMPORTING
        bearer_token      = lv_bearer_token
        msg               = lv_msg
      EXCEPTIONS
        maintain_url_link = 1
        input_error       = 2
        OTHERS            = 3.

    IF sy-subrc <> 0.
      CASE sy-subrc.
        WHEN 1.
          lv_msg = 'ULR Link Error'.
        WHEN 2.
          lv_msg = 'Invalid Input'.
        WHEN OTHERS.
          lv_msg = 'Unknown Error'.
      ENDCASE.
* Implement suitable error handling here
    ENDIF.
    IF lv_msg IS NOT INITIAL.
      DATA(l_txt) = |Reacord's not updated in MIS Usename/Password { lv_msg }|.
      MESSAGE l_txt TYPE 'I' DISPLAY LIKE 'S'.
    ELSE.
      SELECT *
         FROM zmm_ge_asn
        INTO TABLE lt_ge_asn
        WHERE mis_update = ''.

      CHECK lt_ge_asn IS NOT INITIAL.
      SORT lt_ge_asn BY  lifnr asnno.
      DATA(lt_ge_asn_t) = lt_ge_asn.
      DELETE ADJACENT DUPLICATES FROM lt_ge_asn_t COMPARING lifnr asnno.
      SELECT mblnr,
             asnno,
             lifnr,
             ebeln
         FROM zmm_ge_asn_migo
        INTO TABLE @DATA(lt_ge_asn_mogo)
        FOR ALL ENTRIES IN @lt_ge_asn_t
        WHERE lifnr = @lt_ge_asn_t-lifnr AND
              asnno = @lt_ge_asn_t-asnno.

      LOOP AT lt_ge_asn_t ASSIGNING FIELD-SYMBOL(<fs_output1>).
        LOOP AT lt_ge_asn_mogo ASSIGNING FIELD-SYMBOL(<fs_output>)
                              WHERE lifnr = <fs_output1>-lifnr AND
                                    asnno = <fs_output1>-asnno.
          gw_response-vendor = <fs_output>-lifnr.
          gw_response-asnno = <fs_output>-asnno.
          gw_response-migo_no = <fs_output>-mblnr.
          IF gw_response-po_no IS INITIAL.
            gw_response-po_no = |-{ <fs_output>-ebeln }|.
          ELSE.
            gw_response-po_no = |{ gw_response-po_no }-{ <fs_output>-ebeln }|.
          ENDIF.
        ENDLOOP.
        APPEND gw_response TO gt_response.
        CLEAR gw_response.
      ENDLOOP.

      DATA(lv_lines) = lines( gt_response ).
      LOOP AT gt_response INTO gw_response.
        CASE sy-tabix.
          WHEN 1.
            CASE sy-tabix.
              WHEN lv_lines.
                CONCATENATE '[{'
                      '"vendor": "' gw_response-vendor '",'
                      '"asnno": "' gw_response-asnno '",'
                      '"migO_NO": "' gw_response-migo_no '",'
                      '"pO_NO": "' gw_response-po_no '",'
                      '"status": "" '
                      '}]' INTO v_jsonload.
              WHEN OTHERS.
                CONCATENATE '[{'
                      '"vendor": "' gw_response-vendor '",'
                      '"asnno": "' gw_response-asnno '",'
                      '"migO_NO": "' gw_response-migo_no '",'
                      '"pO_NO": "' gw_response-po_no '",'
                      '"status": "" '
                      '}' INTO v_jsonload.
            ENDCASE.
          WHEN OTHERS.
            CASE sy-tabix.
              WHEN lv_lines.
                CONCATENATE v_jsonload
                     ',{'
                    '"vendor": "' gw_response-vendor '",'
                    '"asnno": "' gw_response-asnno '",'
                    '"migO_NO": "' gw_response-migo_no '",'
                    '"pO_NO": "' gw_response-po_no '",'
                    '"status": "" '
                      '}]' INTO v_jsonload.
              WHEN OTHERS.
                CONCATENATE v_jsonload
                     ',{'
                    '"vendor": "' gw_response-vendor '",'
                    '"asnno": "' gw_response-asnno '",'
                    '"migO_NO": "' gw_response-migo_no '",'
                    '"pO_NO": "' gw_response-po_no '",'
                    '"status": "" '
                      '}' INTO v_jsonload.
            ENDCASE.
        ENDCASE.
      ENDLOOP.
      CLEAR: gt_response[],gw_response.
      SELECT SINGLE low FROM tvarvc
        INTO @DATA(l_url)
        WHERE name = 'ZANS_UPD_URL'
        AND   type = 'P'.

*    l_url  = https://misapi.sheenlac.com/api/DistributorStock/ASNSTATUSUpdate.
*    MIS New Dev URL = https://misdevapi.sheenlac.com/api/DistributorStock/ASNSTATUSUpdate

*  lv_url = 'https://webdevqas.sheenlac.com:44300/sap/zapi_service/ZMM_ASN_TEST?sap-client=500'. " SAP System

      DATA(create_url) = CONV string( l_url ).
      cl_http_client=>create_by_url(
        EXPORTING
        url = create_url
        IMPORTING
        client = lo_http_client
        EXCEPTIONS
        argument_not_found = 1
        plugin_not_active = 2
        internal_error = 3
        OTHERS = 4 ).

      CHECK lo_http_client IS BOUND.

      lo_http_client->propertytype_logon_popup = if_http_client=>co_disabled.

*    lo_http_client->authenticate(
*        EXPORTING
*         username = 'KPABAP'
*         password = 'Sheenlac@12' ).

      lo_http_client->request->set_method(  EXPORTING method = if_http_entity=>co_request_method_post ).

      lo_http_client->request->set_content_type( EXPORTING content_type = if_rest_media_type=>gc_appl_json ).

      "Header Data Fields for API
*      lo_http_client->request->set_header_field( name = 'token_type' value = 'Bearer' ).
      lv_bearer_token = |Bearer { lv_bearer_token }|.
      lo_http_client->request->set_header_field(  EXPORTING  name  = 'Authorization'  value = CONV string( lv_bearer_token ) ).

      lo_http_client->request->set_cdata(
        EXPORTING
        data = '{ JSON_Payload }' ).

      CONDENSE v_jsonload NO-GAPS.

      lo_http_client->request->set_cdata(
      EXPORTING
       data = v_jsonload ).

      lo_http_client->send(
       EXCEPTIONS
       http_communication_failure = 1
       http_invalid_state = 2 ).


      CHECK sy-subrc = 0.
      lo_http_client->receive(
       EXCEPTIONS
       http_communication_failure = 1
       http_invalid_state = 2
       http_processing_failed = 3 ).


      lo_http_client->response->get_status(
      IMPORTING
        code = lv_codes ).

      lo_http_client->response->get_status(
      IMPORTING
        reason = lv_http_error ).

      CLEAR lv_response.
      IF lv_codes = 200.
        lv_response = lo_http_client->response->get_cdata( ).
        REPLACE ALL OCCURRENCES OF 'u0022'   IN lv_response WITH '"'.
        REPLACE ALL OCCURRENCES OF '\'  IN lv_response WITH ''.
        REPLACE ALL OCCURRENCES OF 'rn'  IN lv_response WITH ''.
*REPLACE ALL OCCURRENCES OF '""'  IN lv_response WITH ''.
        REPLACE ALL OCCURRENCES OF '/'  IN lv_response WITH ''.
        REPLACE ALL OCCURRENCES OF '[]'  IN lv_response WITH ''.


*    CLEAR lv_response1.
*    CALL METHOD /ui2/cl_json=>serialize
*      EXPORTING
*        data        = lv_response
*        pretty_name = /ui2/cl_json=>pretty_mode-user
*      RECEIVING
*        r_json      = lv_response1.
*    .
*
*    REPLACE ALL OCCURRENCES OF '\'  IN lv_response1 WITH ''.
*    REPLACE ALL OCCURRENCES OF '/'  IN lv_response1 WITH ''.
*    SHIFT lv_response1 LEFT DELETING LEADING '"'.

        /ui2/cl_json=>deserialize(
        EXPORTING
         json         = lv_response
         pretty_name  = /ui2/cl_json=>pretty_mode-user
        CHANGING
         data         = gt_status ).

*  ENDIF.
        DATA: lr_asn  TYPE RANGE OF text30,
              lwa_asn LIKE LINE OF lr_asn.
        DATA: lr_lifnr  TYPE RANGE OF lifnr,
              lwa_lifnr LIKE LINE OF lr_lifnr.
        LOOP AT gt_status ASSIGNING FIELD-SYMBOL(<fs_out>) WHERE status = 'OK'.
          lwa_asn-sign = 'I'.
          lwa_asn-option = 'EQ'.
          lwa_asn-low = <fs_out>-asnno.
          APPEND lwa_asn TO lr_asn.
          lwa_lifnr-sign = 'I'.
          lwa_lifnr-option = 'EQ'.
          lwa_lifnr-low = <fs_out>-vendor.
          APPEND lwa_lifnr TO lr_lifnr.
        ENDLOOP.
*        IF lr_lifnr IS NOT INITIAL.
*          DELETE lt_ge_asn WHERE lifnr IN lr_lifnr.
*        ENDIF.
*        IF lr_asn IS NOT INITIAL.
*          DELETE lt_ge_asn WHERE asnno IN lr_asn.
*        ENDIF.
        LOOP AT lt_ge_asn ASSIGNING FIELD-SYMBOL(<fs>) WHERE lifnr IN lr_lifnr
                                                         AND asnno IN lr_asn.
          <fs>-mis_update = 'X'.
        ENDLOOP.
        IF sy-subrc = 0.
          MODIFY zmm_ge_asn FROM TABLE lt_ge_asn.
        ENDIF.
      ELSE.
        l_txt = |MIS Update Error Code: { lv_codes }: { lv_http_error }|.
        MESSAGE l_txt TYPE 'I' DISPLAY LIKE 'S'.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD on_user_command_g1.
    CASE e_salv_function.
      WHEN 'OPEN'.
        PERFORM f_get_open_op.
      WHEN 'SPLIT'.
        CLEAR: g_nasnqty,gt_asn_split.
        LOOP AT gt_out_1 INTO DATA(w_data) WHERE mark = 'X'.

          SELECT SINGLE mtart FROM mara INTO @DATA(l_mtype) WHERE matnr = @w_data-cmaterialno.
          IF l_mtype NE 'FERT' AND l_mtype NE 'HAWA'.
            DATA(l_err_flag) = 'X'.
            CLEAR gt_asn_split.
            EXIT.
          ELSE.
            g_nasnqty =  g_nasnqty + w_data-nasnqty .
            w_data-mark = ''.
            APPEND w_data TO gt_asn_split.
          ENDIF.
        ENDLOOP.
        IF l_err_flag IS INITIAL.

          CALL SCREEN 102
                         STARTING AT 10 5
                         ENDING AT 150 20.

          PERFORM f_get_open_op.
        ELSE.
          CLEAR l_err_flag.
          DATA(l_text) = |Batch Split is allowed only for FG and Trading Goods|.
          MESSAGE l_text TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.

      WHEN 'EDIT'.
*        IF gv_editable = abap_false.
*          gv_editable = abap_true.
*        ELSE.
*          gv_editable = abap_false.
*        ENDIF.
*        lcl_salv_buddy=>set_editable( i_fieldname = 'RADIO1' i_salv_table = gr_salv_1 i_editable = gv_editable ).
    ENDCASE.
  ENDMETHOD.
  METHOD on_user_command.

    CASE e_salv_function.
      WHEN 'TEST'.
        PERFORM validate_vbeln.
        CHECK gv_inv_err IS INITIAL.
        p_test = abap_true.
        SELECT SINGLE mblnr
        FROM zmm_ge_asn INTO @DATA(l_mblnr)
        WHERE asnno = @zmm_ge_asn-asnno AND
              lifnr = @zmm_ge_asn-lifnr..
        IF sy-subrc = 0.
          DATA(l_asn) = zmm_ge_asn-asnno.
          CONDENSE l_asn.
          DATA(l_text) = |ASNo: { l_asn } already processed with GRN No-{ l_mblnr }|.
          MESSAGE l_text TYPE 'S' DISPLAY LIKE 'E'.
        ELSE.
          me->get_field_values( fname = 'ZMM_GE_ASN-VBELN').
          me->get_field_values( fname = 'ZMM_GE_ASN-DDATE').
          me->get_field_values( fname = 'ZMM_GE_ASN-VEHNO').

          me->get_field_values( fname = 'P_INTIME').
          me->get_field_values( fname = 'P_OUTTIME').
          me->get_field_values( fname = 'P_FRBNR').
          me->get_field_values( fname = 'P_D_NOTE').
          me->get_field_values( fname = 'P_HTEXT').
          me->fill_required_field( ).
          LOOP AT gt_out_1 INTO DATA(w_out_1).
            IF w_out_1-cbatchno CS ';'.
              DATA(l_spl_flag) = abap_true.
              EXIT.
            ENDIF.
            IF w_out_1-cbatchno CS ','.
              l_spl_flag  = abap_true.
              EXIT.
            ENDIF.
          ENDLOOP.
          IF l_spl_flag = abap_true.
            DATA l_answer           TYPE c.
            CALL FUNCTION 'POPUP_TO_CONFIRM'
              EXPORTING
                titlebar              = 'Inward Gate Entry Confirmation'
                text_question         = 'ASN Batch have special symbols.Please Split the Batch or Ignore if its not required'
                text_button_1         = 'Confirm'(101)
                text_button_2         = 'Goback'(102)
                default_button        = '1'
                display_cancel_button = ''
                start_column          = 25
                start_row             = 6
              IMPORTING
                answer                = l_answer
              EXCEPTIONS
                text_not_found        = 1
                OTHERS                = 2.
            CHECK  l_answer = '1' .
            CLEAR: l_answer,l_spl_flag.
          ENDIF.
*        IF gv_field_flag IS INITIAL.
*          me->process_migo( ).
*        ENDIF.
        ENDIF.
      WHEN 'MIGO'.
        PERFORM validate_vbeln.
        CHECK gv_inv_err IS INITIAL.
        CLEAR p_test.
        SELECT SINGLE mblnr
        FROM zmm_ge_asn INTO l_mblnr
        WHERE asnno = zmm_ge_asn-asnno AND
              lifnr = zmm_ge_asn-lifnr.
        IF sy-subrc = 0.
          l_asn = zmm_ge_asn-asnno.
          CONDENSE l_asn.
          l_text = |ASNo: { l_asn } already processed with GRN No-{ l_mblnr }|.
          MESSAGE l_text TYPE 'S' DISPLAY LIKE 'E'.
        ELSE.
          me->get_field_values( fname = 'ZMM_GE_ASN-VBELN').
          me->get_field_values( fname = 'ZMM_GE_ASN-DDATE').
          me->get_field_values( fname = 'ZMM_GE_ASN-VEHNO').
          me->get_field_values( fname = 'P_INTIME').
          me->get_field_values( fname = 'P_OUTTIME').
          me->get_field_values( fname = 'P_FRBNR').
*          me->get_field_values( fname = 'P_D_NOTE').
          me->get_field_values( fname = 'P_HTEXT').
          me->fill_required_field( ).
          IF gv_field_flag IS INITIAL.
            me->process_migo( ).
          ENDIF.
        ENDIF.
      WHEN 'FILE'.
        CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
          EXPORTING
            program_name  = syst-repid
            dynpro_number = syst-dynnr
            field_name    = ' '
            static        = ' '
            mask          = ' '
          CHANGING
            file_name     = gv_fname.
      WHEN 'EDIT'.
        IF gv_editable = abap_false.
          gv_editable = abap_true.
        ELSE.
          gv_editable = abap_false.
          LOOP AT gt_migo ASSIGNING FIELD-SYMBOL(<fs_migo>).
            SELECT SINGLE
                  umrez,
                  umren FROM marm
                  INTO @DATA(lw_marm)
                  WHERE matnr = @<fs_migo>-matnr AND
                        meinh = @<fs_migo>-meins.
            IF sy-subrc = 0.
              <fs_migo>-sku_qty = ( lw_marm-umrez / lw_marm-umren ) * <fs_migo>-migo_qty.
            ENDIF.
            IF <fs_migo>-gravity GT 0.
              <fs_migo>-sku_qty = <fs_migo>-migo_qty / <fs_migo>-gravity.
            ENDIF.
          ENDLOOP.
          gr_salv_2->refresh( ).
        ENDIF.

        lcl_salv_buddy=>set_editable( i_fieldname = 'LGORT' i_salv_table = gr_salv_2 i_editable = gv_editable ).
        lcl_salv_buddy=>set_editable( i_fieldname = 'MIGO_QTY' i_salv_table = gr_salv_2 i_editable = gv_editable ).
        lcl_salv_buddy=>set_editable( i_fieldname = 'ASN_QTY' i_salv_table = gr_salv_2 i_editable = gv_editable ).
        lcl_salv_buddy=>set_editable( i_fieldname = 'REMARK' i_salv_table = gr_salv_2 i_editable = gv_editable ).
        lcl_salv_buddy=>set_editable( i_fieldname = 'GRAVITY' i_salv_table = gr_salv_2 i_editable = gv_editable ).
        lcl_salv_buddy=>set_editable( i_fieldname = 'MEINS' i_salv_table = gr_salv_2 i_editable = gv_editable ).
        lcl_salv_buddy=>set_editable( i_fieldname = 'DATE_OF_MANUFACTURE' i_salv_table = gr_salv_2 i_editable = gv_editable ).
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.                    "on_user_command

*  METHOD on_user_command_pop_up.
*
*    CASE e_salv_function.
*      WHEN 'NEWASN'.
*        IF gt_output IS INITIAL OR
*          gv_newasn = abap_true .
*          gv_newasn = abap_true.
*          CLEAR gs_output.
**          SELECT MAX( asnno )
**            FROM zmm_ge_asn
**            INTO @DATA(l_asnno) WHERE asnno LIKE 'SAP%'.
**          IF l_asnno IS NOT INITIAL.
***            SORT lt_asn BY asnno. " ASN No "F2022-23-ASN-74" from MIS
***            DATA(l_line) = lines( lt_asn ).
***            l_asnno = VALUE #( lt_asn[ asnno = l_asnno ]-asnno OPTIONAL ).
**            DATA(lv_asnno) = l_asnno+3(10) + 1.
**            gs_output-asnno = |SAP{ lv_asnno }|.
**            DO 10 TIMES.
**              APPEND gs_output TO gt_output.
**            ENDDO.
**          ELSE.
**            l_asnno = 'SAP1000000001'.
***      lv_asnno = CONV text30( |{ lv_asnno ALPHA = IN }| ).
**            DO 10 TIMES.
**              gs_output-asnno = l_asnno.
**              APPEND gs_output TO gt_output.
**            ENDDO.
**          ENDIF.
***          IF gv_editable = abap_false.
***            gv_editable = abap_true.
***          ELSE.
***            gv_editable = abap_false.
***          ENDIF.
*          DO 10 TIMES.
*            gs_output-asnno = 'NEW'.
*            APPEND gs_output TO gt_output.
*          ENDDO.
*          lcl_salv_buddy=>set_editable( i_fieldname = 'CMATERIALNO' i_salv_table = lo_popup_alv i_editable = abap_true ).
*          lcl_salv_buddy=>set_editable( i_fieldname = 'NASNQTY' i_salv_table = lo_popup_alv i_editable = abap_true ).
*          lcl_salv_buddy=>set_editable( i_fieldname = 'CBATCHNO' i_salv_table = lo_popup_alv i_editable = abap_true ).
*          lo_popup_alv->refresh( ).
**          CLEAR gv_confirm.
*        ELSE.
*          MESSAGE 'Invoice details with existing ASN No cant be modified' TYPE 'S' DISPLAY LIKE 'E'.
*        ENDIF.
*
*      WHEN 'CONFIRM'.
*        IF gv_newasn = abap_true.
**          gv_confirm = abap_true .
*          SORT gt_output BY cmaterialno.
*          DELETE gt_output WHERE cmaterialno = space.
*          LOOP AT gt_output ASSIGNING FIELD-SYMBOL(<fs>).
*            <fs>-asnno = 'NEW'.
*          ENDLOOP.
*          lo_popup_alv->refresh( ).
*          lcl_salv_buddy=>set_editable( i_fieldname = 'CMATERIALNO' i_salv_table = lo_popup_alv i_editable = abap_false ).
*          lcl_salv_buddy=>set_editable( i_fieldname = 'NASNQTY' i_salv_table = lo_popup_alv i_editable = abap_false ).
*          lcl_salv_buddy=>set_editable( i_fieldname = 'CBATCHNO' i_salv_table = lo_popup_alv i_editable = abap_false ).
*
*          LEAVE TO SCREEN 0.
*        ENDIF.
*      WHEN OTHERS.
*    ENDCASE.
*    CLEAR gs_output.
*  ENDMETHOD.                    "on_user_command
  METHOD handle_double_click.
    CHECK column = 'LGORT'.
    IF gv_editable = abap_false.
      gv_editable = abap_true.
    ELSE.
      gv_editable = abap_false.
    ENDIF.
    lcl_salv_buddy=>set_editable( i_fieldname = 'LGORT' i_salv_table = gr_salv_2 i_editable = gv_editable ).
  ENDMETHOD.                    "handle_double_click
  METHOD on_link_click.

    DATA: refr_stable TYPE lvc_s_stbl.

    IF column = 'RADIO1'.

      LOOP AT gt_out_1 ASSIGNING FIELD-SYMBOL(<fs_itab>).
        IF sy-tabix <> row.
          <fs_itab>-radio1 = icon_wd_radio_button_empty. "Empty Radio Buttons
          <fs_itab>-mark = ''.
        ENDIF.
      ENDLOOP.

      READ TABLE gt_out_1 INDEX row ASSIGNING <fs_itab>.
      IF <fs_itab>-radio1 = icon_radiobutton.
        <fs_itab>-mark = 'X'.
      ELSE.
        <fs_itab>-radio1 = icon_radiobutton.
        <fs_itab>-mark = 'X'.
      ENDIF.

    ENDIF.

    refr_stable-row = abap_true.
    gr_salv_1->refresh( s_stable = refr_stable refresh_mode = if_salv_c_refresh=>full ).


  ENDMETHOD.


  METHOD handle_double_click_g1.
*    CHECK column = 'LGORT'.
*    IF gv_editable = abap_false.
*      gv_editable = abap_true.
*    ELSE.
*      gv_editable = abap_false.
*    ENDIF.
*    lcl_salv_buddy=>set_editable( i_fieldname = 'LGORT' i_salv_table = gr_salv_2 i_editable = gv_editable ).
  ENDMETHOD.                    "handle_double_click
*  METHOD handle_double_click_popup.
*    CHECK column = 'ASNNO' .
*    READ TABLE gt_output INTO gs_output INDEX row.
*    IF sy-subrc = 0.
*      zmm_ge_asn-asnno = gs_output-asnno.
*      LEAVE TO SCREEN 0.
*    ENDIF.
*  ENDMETHOD.                    "handle_double_click
ENDCLASS.

* declare object for dragdrop object.
DATA : go_main TYPE REF TO lcl_auto_migo.
