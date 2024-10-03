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
      constructor IMPORTING ebeln  TYPE ebeln
                            vbeln  TYPE vbeln_vf
*                            splant TYPE werks_d
*                            rplant TYPE werks_d
*                            lifnr  TYPE ty_r_lifnr
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
*      BEGIN OF ty_invoice,
*        ebeln   TYPE vbrk-vbeln,
*        ebelp   TYPE vbrp-posnr,
**        fkdat  TYPE vbrk-fkdat,
*        matnr   TYPE vbrp-matnr,
*        txz01   TYPE lips-arktx,
*        r_plant TYPE lips-werks,
*        werks   TYPE lips-werks,
*        lgort   TYPE lips-lgort,
*        charg   TYPE lips-charg,
*        lfimg   TYPE lips-lfimg,
*        meins   TYPE lips-meins,
**        vrkme  TYPE lips-vrkme,
**        netwr  TYPE vbrp-netwr,
**        fkimg  TYPE vbrp-fkimg,
**        uprice TYPE vbrp-netwr,
**        cmpre  TYPE vbrp-cmpre,
*        flag    TYPE flag,
*      END OF ty_invoice,
      BEGIN OF ty_migo,
        ebeln   TYPE ekpo-ebeln,
        ebelp   TYPE ekpo-ebelp,
        vbeln   TYPE lips-vbeln,
        posnr   TYPE lips-posnr,
        erdat   TYPE ekko-bedat,
        matnr   TYPE ekpo-matnr,
        txz01   TYPE ekpo-txz01,
        r_plant TYPE lips-werks,
        werks   TYPE ekpo-werks,
        lgort   TYPE lips-lgort,
        charg   TYPE lips-charg,
        po_qty  TYPE eket-menge,
        lfimg   TYPE lips-lfimg,
        menge   TYPE mseg-menge,
        lfimg_1 TYPE lips-lfimg,
*        open_qty TYPE eket-menge,
*        migo_qty TYPE eket-wemng,
        meins   TYPE lips-meins,
*        netpr    TYPE ekpo-netpr,
*        flag    TYPE flag,
      END OF ty_migo.

    DATA: gw_goodsmvt_header TYPE bapi2017_gm_head_01,
          gt_goodsmvt_item   TYPE STANDARD TABLE OF bapi2017_gm_item_create,
          gw_goodsmvt_item   TYPE bapi2017_gm_item_create,
          gt_return          TYPE STANDARD TABLE OF bapiret2.
    DATA: gw_goodsmvt_headret TYPE bapi2017_gm_head_ret,
          goodsmvt_code_tmp   TYPE bapi2017_gm_code,
          l_return            TYPE bapiret2.
    DATA : gv_po_no        TYPE ebeln,
           gv_inv_no       TYPE vbeln_vf,
           gv_inv_plant    TYPE werks_d,
           gv_rec_plant    TYPE werks_d,
           gv_frbnr        TYPE frbnr,
           gv_del_note     TYPE lfsnr1,
           gv_bktxt        TYPE bktxt,
           gv_test         TYPE flag,
*           gr_lifnr        TYPE RANGE OF ekko-lifnr,
           gt_out_1        TYPE STANDARD TABLE OF ty_migo,
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

    gv_po_no = ebeln.
    gv_inv_no = vbeln.
*    gv_inv_plant = splant.
*    gv_rec_plant = rplant.
*    gr_lifnr = lifnr.
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
            IF p_ub IS NOT INITIAL.
              DATA(l_txt) = `Edit Storage Location / MIGO Qty`.
            ELSE.
              l_txt = `Edit Storage Location`.
            ENDIF.
            gr_salv_func_2->add_function(
              name     = 'EDIT'
              icon     = CONV string( icon_wd_text_edit )
              text     = l_txt
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
*        lr_column_1 ?= lr_columns_1->get_column('FLAG').
*        lr_column_1->set_visible( abap_false ).
        lr_column_1 ?= lr_columns_1->get_column('WERKS').
        lr_column_1->set_long_text( 'Source Plant' ).
        lr_column_1->set_medium_text( 'Source Plant' ).
        lr_column_1->set_short_text( 'Src Plant' ).
*
        lr_column_1 ?= lr_columns_1->get_column('R_PLANT').
        lr_column_1->set_long_text( 'Receiving Plant' ).
        lr_column_1->set_medium_text( 'Receiving Plant' ).
        lr_column_1->set_short_text( 'Recv Pt' ).

        lr_column_1 ?= lr_columns_1->get_column('PO_QTY').
        lr_column_1->set_long_text( 'PO Quantity' ).
        lr_column_1->set_medium_text( 'PO Qty' ).
        lr_column_1->set_short_text( 'PO Qty' ).

        lr_column_1 ?= lr_columns_1->get_column('LFIMG').
        lr_column_1->set_long_text( 'Issued Quantity' ).
        lr_column_1->set_medium_text( 'Issued Qty' ).
        lr_column_1->set_short_text( 'Issued Qty' ).

        lr_column_1 ?= lr_columns_1->get_column('MENGE').
        lr_column_1->set_long_text( 'Delivered Quantity' ).
        lr_column_1->set_medium_text( 'Delivered Qty' ).
        lr_column_1->set_short_text( 'Deliv.Qty' ).

        lr_column_1 ?= lr_columns_1->get_column('LFIMG_1').
        lr_column_1->set_long_text( 'MIGO Quantity' ).
        lr_column_1->set_medium_text( 'MIGO Qty' ).
        lr_column_1->set_short_text( 'MIGO Qty' ).

        DATA(lr_columns_2) = gr_salv_2->get_columns( ).
        lr_columns_2->set_optimize( abap_true ).
*
        lr_column_2 ?= lr_columns_2->get_column('WERKS').
        lr_column_2->set_long_text( 'Source Plant' ).
        lr_column_2->set_medium_text( 'Source Plant' ).
        lr_column_2->set_short_text( 'Src Plant' ).
*
        lr_column_2 ?= lr_columns_2->get_column('R_PLANT').
        lr_column_2->set_long_text( 'Receiving Plant' ).
        lr_column_2->set_medium_text( 'Receiving Plant' ).
        lr_column_2->set_short_text( 'Recv Pt' ).

        lr_column_2 ?= lr_columns_2->get_column('LFIMG').
        lr_column_2->set_long_text( 'Issued Quantity' ).
        lr_column_2->set_medium_text( 'Issued Qty' ).
        lr_column_2->set_short_text( 'Issued Qty' ).

        lr_column_2 ?= lr_columns_2->get_column('MENGE').
        lr_column_2->set_long_text( 'Delivered Quantity' ).
        lr_column_2->set_medium_text( 'Delivered Qty' ).
        lr_column_2->set_short_text( 'Deliv.Qty' ).

        lr_column_2 ?= lr_columns_2->get_column('LFIMG_1').
        lr_column_2->set_long_text( 'MIGO Quantity' ).
        lr_column_2->set_medium_text( 'MIGO Qty' ).
        lr_column_2->set_short_text( 'MIGO Qty' ).

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
    TYPES: BEGIN OF ty_qty,
             ebeln TYPE ebeln,
             ebelp TYPE ebelp,
             bwart TYPE bwart,
             matnr TYPE matnr,
             werks TYPE ewerk,
             menge TYPE bstmg,
             lfimg TYPE lfimg,
             lgort TYPE lgort_d,
             charg TYPE charg_d,
           END OF ty_qty.
    DATA: lt_po TYPE STANDARD TABLE OF ty_qty,
          lw_po TYPE ty_qty.
*          lt_mseg TYPE STANDARD TABLE OF ty_qty,
*          lw_mseg TYPE ty_qty.
    IF p_zub = abap_true.
      SELECT  a~vbeln,
              b~posnr,
              b~erdat,
              b~matnr,
              b~arktx AS  txz01,
              b~lfimg,
              b~vrkme AS meins,
              a~werks AS r_plant,
              b~werks,
              b~lgort,
              b~charg,
              d~menge,
              e~bwart,
              d~ebeln,
              d~ebelp
*              INTO CORRESPONDING FIELDS OF TABLE me->gt_out_1
              INTO TABLE @DATA(lt_ub_tab)
              FROM likp AS a
              INNER JOIN lips AS b ON b~vbeln = a~vbeln
              INNER JOIN ekko AS c ON b~vgbel = c~ebeln
              INNER JOIN ekpo AS d ON b~vgbel = d~ebeln AND
                                      b~matnr = d~matnr
              INNER JOIN mseg AS e ON d~ebeln = e~ebeln AND
                                      d~ebelp = e~ebelp AND
                                      e~lgort NE @space AND
                                      e~charg NE @space
              WHERE a~vbeln = @gv_inv_no AND
*                  b~werks = gv_inv_plant AND
                    b~bwart = '641' AND "Movement type
                    b~lgort NE @space AND
                    b~charg NE @space AND
                    c~bsart = 'ZUB' .
      SORT lt_ub_tab BY ebeln ebelp matnr charg.
      DELETE ADJACENT DUPLICATES FROM lt_ub_tab COMPARING ebeln ebelp matnr charg lfimg.
    ENDIF.

    IF p_ub = abap_true.

      SELECT  a~ebeln,
              b~ebelp,
              c~bwart,
              c~budat_mkpf AS erdat,
              b~matnr,
              b~txz01,
              c~menge AS lfimg,
              c~meins,
              a~reswk AS werks,
              b~werks AS r_plant,
              c~lgort,
              c~charg,
              b~menge
              INTO CORRESPONDING FIELDS OF TABLE @lt_ub_tab
              FROM  ekko AS a
              INNER JOIN ekpo AS b ON a~ebeln = b~ebeln
*              INNER JOIN ekbe AS c ON b~ebeln = c~ebeln AND
*                                      b~ebelp = c~ebelp AND
*                                      b~matnr = c~matnr
              INNER JOIN mseg AS c ON c~ebeln = b~ebeln AND
                                      c~ebelp = b~ebelp
              WHERE
                    a~bsart = 'UB' AND
                    a~ebeln = @gv_po_no AND
*                    b~werks = @gv_inv_plant AND
                    c~bwart = '351' AND "Movement type
                    c~xauto EQ @space AND
                    c~lgort NE @space AND
                    c~charg NE @space .
    ENDIF.
    IF lt_ub_tab IS NOT INITIAL.
      SELECT mblnr,
             ebeln,
             ebelp,
             bwart,
             cpudt_mkpf AS erdat,
             menge AS lfimg,
             meins,
             lgort,
             charg
           INTO TABLE @DATA(lt_mseg)
           FROM  mseg
           FOR ALL ENTRIES IN @lt_ub_tab
            WHERE ebeln = @lt_ub_tab-ebeln AND
*                    werks = @lt_ub_tab-werks AND
                  bwart IN ( '101','102' ) AND "Movement type
                  xauto EQ @space AND
                  lgort NE @space AND
                  charg = @lt_ub_tab-charg.
    ENDIF.
    SORT lt_ub_tab BY ebeln ebelp.
    SORT lt_mseg BY ebeln ebelp.
    DATA lt_ub_tab2 LIKE lt_ub_tab.
    LOOP AT lt_ub_tab INTO DATA(lw_ub_tab).
*        MOVE-CORRESPONDING lw_ub_tab TO lw_mseg.
**        COLLECT lw_mseg INTO lt_mseg.
      READ TABLE lt_ub_tab2 ASSIGNING FIELD-SYMBOL(<fs>) WITH KEY ebeln = lw_ub_tab-ebeln
                                                                  ebelp = lw_ub_tab-ebelp
                                                                  charg = lw_ub_tab-charg.
      IF sy-subrc = 0.
        <fs>-lfimg = <fs>-lfimg + lw_ub_tab-lfimg.
      ELSE.
        APPEND lw_ub_tab TO lt_ub_tab2.
      ENDIF.
    ENDLOOP.
    DATA lt_mseg2 LIKE lt_mseg.
    LOOP AT lt_mseg INTO DATA(lw_mseg).
*        MOVE-CORRESPONDING lw_ub_tab TO lw_mseg.
**        COLLECT lw_mseg INTO lt_mseg.
      READ TABLE lt_mseg2 ASSIGNING FIELD-SYMBOL(<fs2>) WITH KEY ebeln = lw_mseg-ebeln
                                                                 ebelp = lw_mseg-ebelp
                                                                 charg = lw_mseg-charg
                                                                 bwart = lw_mseg-bwart.
      IF sy-subrc = 0.
        <fs2>-lfimg = <fs2>-lfimg + lw_mseg-lfimg.
      ELSE.
        APPEND lw_mseg TO lt_mseg2.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_ub_tab2 INTO lw_ub_tab.
      READ TABLE gt_out_1 ASSIGNING FIELD-SYMBOL(<fs3>) WITH KEY ebeln = lw_ub_tab-ebeln
                                                                 ebelp = lw_ub_tab-ebelp
                                                                 charg = lw_ub_tab-charg.
      IF sy-subrc = 0.
        <fs3>-lfimg_1 = <fs3>-lfimg - <fs3>-menge.
      ELSE.
        MOVE-CORRESPONDING lw_ub_tab TO wa_data.
*          wa_data-erdat = lw_ub_tab-erdat.
*          wa_data-lgort = VALUE #( lt_mseg2[ ebeln = wa_data-ebeln
*                                             ebelp = wa_data-ebelp
*                                             charg = wa_data-charg ]-lgort OPTIONAL ).
*          wa_data-charg = VALUE #( lt_mseg2[ ebeln = wa_data-ebeln
*                                            ebelp = wa_data-ebelp ]-charg OPTIONAL ).
        wa_data-po_qty = lw_ub_tab-menge.
        wa_data-lfimg  = lw_ub_tab-lfimg.
        wa_data-menge  = VALUE #( lt_mseg2[ ebeln = wa_data-ebeln
                                          ebelp = wa_data-ebelp
*                                          lgort = wa_data-lgort
                                          charg = wa_data-charg
                                          bwart = '101' ]-lfimg OPTIONAL ) -
                         VALUE #( lt_mseg2[ ebeln = wa_data-ebeln
                                          ebelp = wa_data-ebelp
*                                          lgort = wa_data-lgort
                                          charg = wa_data-charg
                                          bwart = '102' ]-lfimg OPTIONAL ).
        wa_data-lfimg_1 = wa_data-lfimg - wa_data-menge. " lw_ub_tab-menge - wa_data-menge." - lw_ub_tab-LFIMG.
        APPEND wa_data TO gt_out_1.
      ENDIF.
    ENDLOOP.

    SORT gt_out_1 BY ebeln ebelp lgort charg.
    DELETE ADJACENT DUPLICATES FROM gt_out_1 COMPARING ebeln ebelp lgort charg.
  ENDMETHOD.
  METHOD fetch_purchase_data.
    CHECK gt_out_1 IS NOT INITIAL.
    gv_rec_plant = gt_out_1[ 1 ]-r_plant.

    SORT me->gt_out_1 BY matnr.
    APPEND LINES OF gt_out_1 TO gt_migo.
  ENDMETHOD.
  METHOD process_migo.

    DATA: lt_tab TYPE esp1_message_tab_type.
    DATA: ls_tab     TYPE esp1_message_wa_type,
          l_err_flag TYPE flag.
    DATA: lw_sto_migo_log TYPE zmm_sto_migo_log,
          lt_sto_migo_log TYPE STANDARD TABLE OF zmm_sto_migo_log.
    DELETE gt_migo WHERE lfimg_1 = 0.

    LOOP AT gt_migo INTO wa_data.
      IF ( wa_data-lfimg_1 + wa_data-menge ) > wa_data-lfimg.
        DATA(l_text) = |Material-{ wa_data-txz01 } entered migo quantity is more than issued quantity { wa_data-lfimg }|.
        MESSAGE l_text TYPE 'S' DISPLAY LIKE 'E'.
        l_err_flag = 'X'.
        EXIT.
      ENDIF.
      IF gv_test = abap_false.
        MOVE-CORRESPONDING wa_data TO lw_sto_migo_log.
        lw_sto_migo_log-ctime = sy-uzeit.
        lw_sto_migo_log-cdate = sy-datum.
        lw_sto_migo_log-usnam = sy-uname.
        APPEND lw_sto_migo_log TO lt_sto_migo_log.
      ENDIF.
    ENDLOOP.
    IF l_err_flag IS INITIAL.
      LOOP AT gt_migo INTO wa_data.
*<<<<<BAPI Communication Structure: Material Document Item Data>>>>>>>>>
        gw_goodsmvt_item-material             = wa_data-matnr.
        gw_goodsmvt_item-plant = gv_rec_plant.
        gw_goodsmvt_item-move_stloc           = wa_data-lgort.
        gw_goodsmvt_item-stge_loc             = wa_data-lgort.
        gw_goodsmvt_item-batch                = wa_data-charg.
        gw_goodsmvt_item-move_type            = '101'.
        gw_goodsmvt_item-entry_qnt            = wa_data-lfimg_1.
        gw_goodsmvt_item-entry_uom            = wa_data-meins.
        gw_goodsmvt_item-mvt_ind = 'B'.
        IF p_zub = abap_true.
          gw_goodsmvt_item-deliv_numb = wa_data-vbeln.
          gw_goodsmvt_item-deliv_item = wa_data-posnr.
        ENDIF.
        gw_goodsmvt_item-po_number  = wa_data-ebeln.
        gw_goodsmvt_item-po_item    = wa_data-ebelp.

*      gw_goodsmvt_item-item_text            = wa_data-txz01.
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
*         GOODSMVT_SERIALNUMBER =
          return           = gt_return.
      IF gt_return[] IS INITIAL.
        IF gv_test = abap_true.
          l_text = 'Simulation: No Error found'.
          MESSAGE l_text TYPE 'I' DISPLAY LIKE 'S'.
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait   = 'X'
            IMPORTING
              return = l_return.
          IF l_return IS INITIAL.
            LOOP AT lt_sto_migo_log ASSIGNING FIELD-SYMBOL(<fs_log>).
              <fs_log>-mblnr = gw_goodsmvt_headret-mat_doc.
            ENDLOOP.
            MODIFY zmm_sto_migo_log FROM TABLE lt_sto_migo_log.
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
    ENDIF.
  ENDMETHOD.
  METHOD on_user_command.

    CASE e_salv_function.
      WHEN 'MIGO'.
        me->process_migo( ).
      WHEN 'EDIT'.
        IF gv_editable = abap_false.
          gv_editable = abap_true.
        ELSE.
          gv_editable = abap_false.
        ENDIF.
        lcl_salv_buddy=>set_editable( i_fieldname = 'LGORT' i_salv_table = gr_salv_2 i_editable = gv_editable ).
*        IF p_ub IS NOT INITIAL.
        lcl_salv_buddy=>set_editable( i_fieldname = 'LFIMG_1' i_salv_table = gr_salv_2 i_editable = gv_editable ).
*        ENDIF.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.                    "on_user_command
  METHOD handle_double_click.
    CASE column.
      WHEN 'LGORT' OR
           'LFIMG_1'.
        IF gv_editable = abap_false.
          gv_editable = abap_true.
        ELSE.
          gv_editable = abap_false.
        ENDIF.
        lcl_salv_buddy=>set_editable( i_fieldname = 'LGORT' i_salv_table = gr_salv_2 i_editable = gv_editable ).
*        IF p_ub IS NOT INITIAL.
        lcl_salv_buddy=>set_editable( i_fieldname = 'LFIMG_1' i_salv_table = gr_salv_2 i_editable = gv_editable ).
*        ENDIF.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.                    "handle_double_click
ENDCLASS.
