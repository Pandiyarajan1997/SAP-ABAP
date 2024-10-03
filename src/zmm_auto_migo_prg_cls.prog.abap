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
    TYPES: ty_r_lifnr TYPE RANGE OF ekko-lifnr.
    CLASS-DATA: gv_editable TYPE abap_bool,
                gr_salv_1   TYPE REF TO cl_salv_table,
                gr_salv_2   TYPE REF TO cl_salv_table.
    METHODS:
      constructor IMPORTING vbeln  TYPE vbeln_vf
                            splant TYPE werks_d
                            rplant TYPE werks_d
                            lifnr  TYPE ty_r_lifnr
                            frbnr  TYPE frbnr
                            lfsnr  TYPE lfsnr1
                            test   TYPE flag
                            header TYPE bktxt,
      fetch_invoice_data,
      fetch_purchase_data,
      process_migo,
      build_alv.
    METHODS:
      on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function.

    CLASS-METHODS: handle_double_click
      FOR EVENT double_click OF cl_salv_events_table
      IMPORTING row column.

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
    TYPES:
      BEGIN OF ty_invoice,
        vbeln  TYPE vbrk-vbeln,
        posnr  TYPE vbrp-posnr,
        fkdat  TYPE vbrk-fkdat,
        matnr  TYPE vbrp-matnr,
        arktx  TYPE lips-arktx,
        werks  TYPE lips-werks,
        lgort  TYPE lips-lgort,
        charg  TYPE lips-charg,
        lfimg  TYPE lips-lfimg,
        vrkme  TYPE lips-vrkme,
        netwr  TYPE vbrp-netwr,
        fkimg  TYPE vbrp-fkimg,
        uprice TYPE vbrp-netwr,
        cmpre  TYPE vbrp-cmpre,
        flag   TYPE flag,
        vgbel  TYPE vbrp-vgbel,
        vgpos  TYPE vbrp-vgpos,
        aubel  TYPE vbrp-aubel,
        aupos  TYPE vbrp-aupos,
        tprice TYPE vbrp-netwr,
      END OF ty_invoice,
      BEGIN OF ty_migo,
        ebeln    TYPE ekpo-ebeln,
        ebelp    TYPE ekpo-ebelp,
        bedat    TYPE ekko-bedat,
        matnr    TYPE ekpo-matnr,
        txz01    TYPE ekpo-txz01,
        werks    TYPE ekpo-werks,
        lgort    TYPE lips-lgort,
        charg    TYPE lips-charg,
        po_qty   TYPE eket-menge,
        del_qty  TYPE eket-wemng,
        open_qty TYPE eket-menge,
        migo_qty TYPE eket-wemng,
        meins    TYPE ekpo-meins,
        netpr    TYPE ekpo-netpr,
        flag     TYPE flag,
      END OF ty_migo.

    DATA: gw_goodsmvt_header TYPE bapi2017_gm_head_01,
          gt_goodsmvt_item   TYPE STANDARD TABLE OF bapi2017_gm_item_create,
          gw_goodsmvt_item   TYPE bapi2017_gm_item_create,
          gt_return          TYPE STANDARD TABLE OF bapiret2.
    DATA: gw_goodsmvt_headret TYPE bapi2017_gm_head_ret,
          goodsmvt_code_tmp   TYPE bapi2017_gm_code,
          l_return            TYPE bapiret2.
    DATA : gv_inv_no       TYPE vbeln_vf,
           gv_inv_plant    TYPE werks_d,
           gv_rec_plant    TYPE werks_d,
           gv_frbnr        TYPE frbnr,
           gv_del_note     TYPE lfsnr1,
           gv_bktxt        TYPE bktxt,
           gv_test         TYPE flag,
           gr_lifnr        TYPE RANGE OF ekko-lifnr,
           gt_out_1        TYPE STANDARD TABLE OF ty_invoice,
           wa_data         TYPE ty_migo,
           gt_out_2        TYPE STANDARD TABLE OF ty_migo,
           gt_migo         TYPE STANDARD TABLE OF ty_migo,
           gw_automigo_log TYPE zmm_automigo_log,
           gt_automigo_log TYPE STANDARD TABLE OF zmm_automigo_log.
    DATA: gt_matnr_po  TYPE STANDARD TABLE OF ty_qty,
          gt_matnr_inv TYPE STANDARD TABLE OF ty_qty,
          gw_matnr     TYPE ty_qty.
    DATA: gt_po_item_qty TYPE STANDARD TABLE OF ty_po_qty,
          gw_po_item_qty TYPE ty_po_qty.

    DATA:
      gr_splitter    TYPE REF TO cl_gui_splitter_container,
      gr_container_1 TYPE REF TO cl_gui_container,
      gr_container_2 TYPE REF TO cl_gui_container,
      gr_salv_func_1 TYPE REF TO cl_salv_functions,
      gr_salv_func_2 TYPE REF TO cl_salv_functions.

ENDCLASS.


CLASS lcl_auto_migo IMPLEMENTATION.
  METHOD constructor.
    gv_inv_no = vbeln.
    gv_inv_plant = splant.
    gv_rec_plant = rplant.
    gr_lifnr = lifnr.
    gv_frbnr = frbnr.
    gv_del_note = lfsnr.
    gv_test = test.
    gv_bktxt = header.
  ENDMETHOD.
  METHOD build_alv.

*<-------------------------------------------

    DATA: lr_column_1 TYPE REF TO cl_salv_column_list,
          lr_column_2 TYPE REF TO cl_salv_column_list.
    DATA: lo_events TYPE REF TO cl_salv_events_table.

* ------------------
* Create ALV screens

    CHECK gt_out_1 IS NOT INITIAL.

    TRY.

* Splitter - it is neccessary to specify the default_screen as parent

        gr_splitter = NEW #( parent = cl_gui_container=>default_screen
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
                                          IMPORTING r_salv_table  = gr_salv_1
                                          CHANGING t_table         = gt_out_1 ).

* Set ALV functions - should you wish to include any

        gr_salv_func_1 = gr_salv_1->get_functions( ).
        gr_salv_func_1->set_all( abap_true ).

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
                                          IMPORTING r_salv_table  = gr_salv_2
                                          CHANGING t_table         = gt_migo ).
* ALV functions

        gr_salv_func_2 = gr_salv_2->get_functions( ).
        gr_salv_func_2->set_all( abap_true ).

        INCLUDE <icon>.
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
              text     = `Edit Storage Location`
              tooltip  = `Storage Location`
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
        lr_column_1 ?= lr_columns_1->get_column('FLAG').
        lr_column_1->set_visible( abap_false ).
        lr_column_1 ?= lr_columns_1->get_column('VGBEL').
        lr_column_1->set_visible( abap_false ).
        lr_column_1 ?= lr_columns_1->get_column('VGPOS').
        lr_column_1->set_visible( abap_false ).
        lr_column_1 ?= lr_columns_1->get_column('AUBEL').
        lr_column_1->set_visible( abap_false ).
        lr_column_1 ?= lr_columns_1->get_column('AUPOS').
        lr_column_1->set_visible( abap_false ).

        lr_column_1 ?= lr_columns_1->get_column('NETWR').
        lr_column_1->set_visible( abap_false ).
        lr_column_1 ?= lr_columns_1->get_column('FKIMG').
        lr_column_1->set_visible( abap_false ).

        lr_column_1 ?= lr_columns_1->get_column('CMPRE').
        lr_column_1->set_long_text( 'Unit Price with Tax' ).
        lr_column_1->set_medium_text( 'Unit Price(Tax)' ).
        lr_column_1->set_short_text( 'Price(Tax)' ).

        lr_column_1 ?= lr_columns_1->get_column('UPRICE').
        lr_column_1->set_long_text( 'Unit Price' ).
        lr_column_1->set_medium_text( 'Unit Price' ).
        lr_column_1->set_short_text( 'Unit Price' ).

        lr_column_1 ?= lr_columns_1->get_column('TPRICE').
        lr_column_1->set_long_text( 'Total Price' ).
        lr_column_1->set_medium_text( 'Total Price' ).
        lr_column_1->set_short_text( 'Total' ).

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

        lr_column_2 ?= lr_columns_2->get_column('MIGO_QTY').
        lr_column_2->set_long_text( 'MIGO Quantity' ).
        lr_column_2->set_medium_text( 'MIGO Qty' ).
        lr_column_2->set_short_text( 'MIGO Qty' ).

        lr_column_2 ?= lr_columns_2->get_column('NETPR').
        lr_column_2->set_long_text( 'Unit Price' ).
        lr_column_2->set_medium_text( 'Unit Price' ).
        lr_column_2->set_short_text( 'Unit Price' ).

        lr_column_2 ?= lr_columns_2->get_column('FLAG').
        lr_column_2->set_visible( abap_false ).

        lr_column_2 ?= lr_columns_2->get_column('LGORT').
        DATA  p_ddic TYPE salv_s_ddic_reference.
        p_ddic-table = 'T001L'.
        p_ddic-field = 'LGORT'.
        lr_column_2->set_ddic_reference( p_ddic ).
        lr_column_2->set_f4( if_salv_c_bool_sap=>true ).

*        lr_column_1->set_cell_type( if_salv_c_cell_type=>hotspot ).
** ALV events - the implementation of the on link click event follows further in the post

* Register events
        lo_events = gr_salv_2->get_event( ).
        SET HANDLER on_user_command FOR lo_events.
        SET HANDLER handle_double_click FOR lo_events.
*
**        gr_salv_1->set_list_header( 'Invoice Details' ).
*
*        DATA : lref_structype TYPE  REF TO cl_abap_structdescr,
*               lt_dis_comps   TYPE abap_component_tab,
*               ls_comp        TYPE abap_componentdescr,
*               ls_scol        TYPE lvc_s_colo,
*               lt_colors      TYPE lvc_t_scol,
*               l_fname        TYPE lvc_fname.
*        DATA: lo_header  TYPE REF TO cl_salv_form_layout_grid,
*              lo_h_label TYPE REF TO cl_salv_form_label,
*              lo_h_flow  TYPE REF TO cl_salv_form_layout_flow.
*
*        lref_structype ?= cl_abap_typedescr=>describe_by_data( wa_data ).
*        lt_dis_comps = lref_structype->get_components( ).
*
*        LOOP AT lt_dis_comps INTO ls_comp.
*          l_fname = ls_comp-name.
*          lr_column_2 ?= lr_columns_2->get_column( l_fname ).
*          ls_scol-col = 7.
*          ls_scol-int = 1.
*          ls_scol-inv = 0.
*          lr_column_2->set_color( value = ls_scol ).
*        ENDLOOP.
*
**   header object
*        CREATE OBJECT lo_header.
**
**   To create a Lable or Flow we have to specify the target
**     row and column number where we need to set up the output
**     text.
**
*        DATA: lo_functions TYPE REF TO cl_salv_functions_list.
**   Default Functions
*        lo_functions = gr_salv_1->get_functions( ).
*        lo_functions->set_default( abap_true ).
*
**   information in Bold
*        lo_h_label = lo_header->create_label( row = 1 column = 1 ).
*        lo_h_label->set_text( 'Invoice Details' ).
*
**   set the top of list using the header for Online.
*        gr_salv_1->set_top_of_list( lo_header ).
**
**   set the top of list using the header for Print.
*        gr_salv_1->set_top_of_list_print( lo_header ).

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

  ENDMETHOD.
  METHOD fetch_invoice_data.
    SELECT SINGLE
           mblnr
      FROM zmm_automigo_log INTO @DATA(l_mblnr)
      WHERE vbeln = @gv_inv_no.
    IF sy-subrc = 0.
      DATA(l_text) = |Invoice { gv_inv_no } is already processed with GRN No: { l_mblnr }|.
      MESSAGE l_text TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    SELECT  a~vbeln
            b~posnr
            a~fkdat
            c~matnr
            b~arktx
            b~cmpre
            b~netwr
            b~fkimg
            c~werks
            c~lgort
            c~charg
            c~lfimg
            c~vrkme
            INTO CORRESPONDING FIELDS OF TABLE me->gt_out_1
            FROM vbrk AS a
            INNER JOIN vbrp AS b ON a~vbeln = b~vbeln
            INNER JOIN lips AS c ON b~vgbel = c~vbeln AND
*                                    b~vgpos = c~posnr AND
                                    b~aubel = c~vgbel AND
                                    b~aupos = c~vgpos "AND
*                                    b~matnr = c~matnr
            WHERE a~vbeln = gv_inv_no AND
                  b~werks = gv_inv_plant AND
                  c~bwart = '601' AND "Movement type
                  c~lgort NE space AND
                  c~charg NE space AND
                  c~lfimg NE 0.
*    SORT me->gt_out_1 by vbeln posnr charg.

*    SELECT  a~vbeln
*            b~posnr
*            a~fkdat
*            b~matnr
*            b~arktx
*            b~cmpre
*            b~netwr
*            b~fkimg
*            b~vgbel
*            b~vgpos
*            INTO CORRESPONDING FIELDS OF TABLE me->gt_out_1
*            FROM vbrk AS a
*            INNER JOIN vbrp AS b ON a~vbeln = b~vbeln
*            WHERE a~vbeln = gv_inv_no AND
*                  b~werks = gv_inv_plant .
*    IF sy-subrc = 0.
*      SORT me->gt_out_1 BY vbeln posnr.
*      SELECT
*       vbeln,
*       posnr,
*       werks,
*       lgort,
*       charg,
*       lfimg,
*       vrkme
*      INTO TABLE @DATA(lt_lips)
*      FROM lips
*      FOR ALL ENTRIES IN @me->gt_out_1
*      WHERE vbeln = @me->gt_out_1-vgbel AND
**            posnr = @me->gt_out_1-vgpos AND
**            matnr = @me->gt_out_1-matnr AND
*            bwart = '601' AND "Movement type
**            lgort NE @space AND
*            charg NE @space.
*      SORT lt_lips BY vbeln posnr.
*    ENDIF.
*    IF sy-subrc = 0.
*      LOOP AT gt_out_1 ASSIGNING FIELD-SYMBOL(<fs_out1>).
*        <fs_out1>-werks = VALUE #( lt_lips[ vbeln = <fs_out1>-vgbel
*                                            posnr = <fs_out1>-posnr ]-werks OPTIONAL ).
*        <fs_out1>-lgort = VALUE #( lt_lips[ vbeln = <fs_out1>-vgbel
*                                            posnr = <fs_out1>-posnr ]-lgort OPTIONAL ).
*        <fs_out1>-charg = VALUE #( lt_lips[ vbeln = <fs_out1>-vgbel
*                                            posnr = <fs_out1>-posnr ]-charg OPTIONAL ).
*        <fs_out1>-lfimg = VALUE #( lt_lips[ vbeln = <fs_out1>-vgbel
*                                            posnr = <fs_out1>-posnr ]-lfimg OPTIONAL ).
*        <fs_out1>-vrkme = VALUE #( lt_lips[ vbeln = <fs_out1>-vgbel
*                                            posnr = <fs_out1>-posnr ]-vrkme OPTIONAL ).
*
*      ENDLOOP.
*    ENDIF.

  ENDMETHOD.
  METHOD fetch_purchase_data.
    CHECK gt_out_1 IS NOT INITIAL.
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
                  a~lifnr IN gr_lifnr[] AND
                  b~werks = gv_rec_plant AND
                  b~matnr =  me->gt_out_1-matnr AND
                  b~loekz EQ '' AND
                  b~elikz = space .

    SORT me->gt_out_1 BY matnr vbeln posnr.
    SORT me->gt_out_2 BY matnr ebeln ebelp.

    LOOP AT gt_out_1 ASSIGNING FIELD-SYMBOL(<fs_out1>).
      <fs_out1>-uprice = <fs_out1>-netwr  / <fs_out1>-fkimg .
      <fs_out1>-tprice = <fs_out1>-lfimg * <fs_out1>-cmpre.
      gw_matnr-matnr = <fs_out1>-matnr.
      gw_matnr-qty   = <fs_out1>-lfimg.
      COLLECT gw_matnr INTO gt_matnr_inv.
    ENDLOOP.

    LOOP AT gt_out_2 ASSIGNING FIELD-SYMBOL(<fs_out2>).
      <fs_out2>-open_qty = <fs_out2>-po_qty - <fs_out2>-del_qty.
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
                                    WHERE matnr = <fs_inv>-matnr AND
                                          flag = space.
        IF <fs_po>-open_qty <= 0.
          <fs_po>-flag = 'X'.
          CONTINUE.
        ENDIF.
        IF <fs_po>-open_qty >= <fs_inv>-lfimg.
          <fs_po>-migo_qty = <fs_inv>-lfimg.
          <fs_po>-lgort   = <fs_inv>-lgort.
          <fs_po>-charg   = <fs_inv>-charg.
          MOVE-CORRESPONDING <fs_inv> TO gw_automigo_log.
          MOVE-CORRESPONDING <fs_po> TO gw_automigo_log.
          gw_automigo_log-werks_from = <fs_inv>-werks.
          gw_automigo_log-werks_to = <fs_po>-werks.
          gw_automigo_log-ctime = sy-uzeit.
          gw_automigo_log-cdate = sy-datum.
          gw_automigo_log-usnam = sy-uname.
          APPEND gw_automigo_log TO gt_automigo_log. CLEAR gw_automigo_log.
          APPEND <fs_po> TO gt_migo.
          <fs_po>-open_qty = <fs_po>-open_qty - <fs_inv>-lfimg.
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
          <fs_po>-lgort   = <fs_inv>-lgort.
          <fs_po>-charg   = <fs_inv>-charg.
          MOVE-CORRESPONDING <fs_inv> TO gw_automigo_log.
          MOVE-CORRESPONDING <fs_po> TO gw_automigo_log.
          gw_automigo_log-werks_from = <fs_inv>-werks.
          gw_automigo_log-werks_to = <fs_po>-werks.
          gw_automigo_log-ctime = sy-uzeit.
          gw_automigo_log-cdate = sy-datum.
          gw_automigo_log-usnam = sy-uname.
          APPEND gw_automigo_log TO gt_automigo_log. CLEAR gw_automigo_log.
          APPEND <fs_po> TO gt_migo.
          <fs_inv>-lfimg =  <fs_inv>-lfimg - <fs_po>-open_qty.
          <fs_po>-open_qty = <fs_po>-open_qty - <fs_inv>-lfimg.
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
    LOOP AT gt_migo INTO wa_data.
*<<<<<BAPI Communication Structure: Material Document Item Data>>>>>>>>>
      gw_goodsmvt_item-move_type            = '101'.
      gw_goodsmvt_item-mvt_ind              = 'B'.
      gw_goodsmvt_item-plant                = wa_data-werks.
      gw_goodsmvt_item-material             = wa_data-matnr.
      gw_goodsmvt_item-entry_qnt            = wa_data-migo_qty.
      gw_goodsmvt_item-move_stloc           = wa_data-lgort.
      gw_goodsmvt_item-stge_loc             = wa_data-lgort.
*      gw_goodsmvt_item-entry_uom            = wa_data-meins.
      gw_goodsmvt_item-batch                = wa_data-charg.
      gw_goodsmvt_item-po_number            = wa_data-ebeln.
      gw_goodsmvt_item-po_item              = wa_data-ebelp.
*      CONCATENATE pcitab-del_no pcitab-del_item INTO itab-item_text.
*      itab-move_reas  = pcitab-scrap_reason.
      gw_goodsmvt_item-item_text            = wa_data-txz01.
*      gw_goodsmvt_item-no_more_gr           = 'X'.
      APPEND  gw_goodsmvt_item TO gt_goodsmvt_item.

*<<<<<<<Post goods movements with MB_CREATE_GOODS_MOVEMENT>>>>>>>>>>
    ENDLOOP.
    goodsmvt_code_tmp               = '01'. "01 - MB01 - Goods Receipts for Purchase Order
    gw_goodsmvt_header-pstng_date   = sy-datum.
    gw_goodsmvt_header-doc_date     = sy-datum.
    gw_goodsmvt_header-pr_uname     = sy-uname.
    gw_goodsmvt_header-header_txt   =  gv_bktxt. "|{ wa_data-ebeln }/1|.
    gw_goodsmvt_header-bill_of_lading = gv_frbnr.
    gw_goodsmvt_header-ref_doc_no     = gv_del_note.
    CALL FUNCTION 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]"Added by SPLABAP during code remediation
      EXPORTING
        goodsmvt_header  = gw_goodsmvt_header
        goodsmvt_code    = goodsmvt_code_tmp
        testrun          = gv_test
      IMPORTING
        goodsmvt_headret = gw_goodsmvt_headret
      TABLES
        goodsmvt_item    = gt_goodsmvt_item
*       GOODSMVT_SERIALNUMBER =
        return           = gt_return.
    IF gt_return[] IS INITIAL.
      IF gv_test = abap_true.
        DATA(l_text) = 'Simulation: No Error found'.
        MESSAGE l_text TYPE 'I' DISPLAY LIKE 'S'.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait   = 'X'
          IMPORTING
            return = l_return.
        IF l_return IS INITIAL.
          LOOP AT gt_automigo_log ASSIGNING FIELD-SYMBOL(<fs_log>).
            <fs_log>-mblnr = gw_goodsmvt_headret-mat_doc.
          ENDLOOP.
          MODIFY zmm_automigo_log FROM TABLE gt_automigo_log.
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
  METHOD on_user_command.

    CASE e_salv_function.
      WHEN 'MIGO'.
          DATA popup_return TYPE c.
          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              titlebar              = 'Confirmation'
              text_question         = 'Do you want to confirm the details are verified ( i.e Storage Location, Price ect )'
              text_button_1         = 'Yes'
              text_button_2         = 'No'
              default_button        = '2'
              display_cancel_button = 'X'
            IMPORTING
              answer                = popup_return " to hold the FM's return value
            EXCEPTIONS
              text_not_found        = 1
              OTHERS                = 2.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.
          IF popup_return EQ '1'.
            me->process_migo( ).
          ENDIF.

      WHEN 'EDIT'.
        IF gv_editable = abap_false.
          gv_editable = abap_true.
        ELSE.
          gv_editable = abap_false.
        ENDIF.
        lcl_salv_buddy=>set_editable( i_fieldname = 'LGORT' i_salv_table = gr_salv_2 i_editable = gv_editable ).
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.                    "on_user_command
  METHOD handle_double_click.
    CHECK column = 'LGORT'.
    IF gv_editable = abap_false.
      gv_editable = abap_true.
    ELSE.
      gv_editable = abap_false.
    ENDIF.
    lcl_salv_buddy=>set_editable( i_fieldname = 'LGORT' i_salv_table = gr_salv_2 i_editable = gv_editable ).
*    lcl_salv_buddy=>set_editable( i_fieldname = 'JUPER' i_salv_table = gr_salv_2 i_editable = gv_editable ).
  ENDMETHOD.                    "handle_double_click
ENDCLASS.
