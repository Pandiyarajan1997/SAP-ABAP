
*&---------------------------------------------------------------------*
*& Include          ZHR_HOADMIN_EMP_CLS
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
*&---------------------------------------------------------------------*
*& Include          ZMM_SERVICE_PR_CREATION_CLS
*&---------------------------------------------------------------------*
*---------------------------------------------------------------------*
*       CLASS lcl_handle_events DEFINITION
*---------------------------------------------------------------------*
* §5.1 define a local class for handling events of cl_salv_table
*---------------------------------------------------------------------*
CLASS lcl_main DEFINITION.

  PUBLIC SECTION.
    CLASS-DATA : lo_gr_alv       TYPE REF TO cl_salv_table. " Variables for ALV properties
    METHODS:
      process_data,
      build_alv,
      update_9011,
      update_9013,
      on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function.

    TYPES:BEGIN OF ty_log_report,
            type(1) TYPE c,
            message TYPE text255,
          END OF ty_log_report.
    DATA: gt_log_report TYPE TABLE OF ty_log_report,
          gs_log_report TYPE ty_log_report.
  PRIVATE SECTION.

    DATA: go_alv_custom  TYPE REF TO cl_gui_custom_container,
          gr_splitter    TYPE REF TO cl_gui_splitter_container,
          gr_container_1 TYPE REF TO cl_gui_container,
          gr_container_2 TYPE REF TO cl_gui_container,
          gr_salv_func_2 TYPE REF TO cl_salv_functions.
    TYPES: BEGIN OF ty_data,
             pernr TYPE pernr_d,
             ename TYPE emnam, " XLS Field 2
             plans TYPE plans, " XLS Field 3
             stext TYPE stext, " XLS Field 4
             f1    TYPE string, " XLS Field 5
             f2    TYPE string, " XLS Field 5
             f3    TYPE string, " XLS Field 5
             f4    TYPE string, " XLS Field 5
           END OF ty_data.
    DATA: gt_data TYPE TABLE OF ty_data,
          gs_data TYPE ty_data.

ENDCLASS.                    "lcl_handle_events DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_handle_events IMPLEMENTATION
*---------------------------------------------------------------------*
* §5.2 implement the events for handling the events of cl_salv_table
*---------------------------------------------------------------------*
CLASS lcl_main IMPLEMENTATION.
  METHOD on_user_command.
    PERFORM handle_user_command USING e_salv_function.
  ENDMETHOD.                    "on_user_command
  METHOD process_data.

    IF s_pernr[] IS NOT INITIAL.

      SELECT a~pernr,
             a~ename,
             a~plans,
             b~stext  INTO TABLE @DATA(lt_tab)
            FROM pa0001 AS a INNER JOIN hrp1000 AS b ON b~plvar = '01' AND
                                                        b~otype = 'S'  AND
                                                        b~objid = a~plans AND
                                                        b~istat = '1' AND
                                                        b~endda  >= @sy-datum
           WHERE a~pernr IN @s_pernr AND
                 a~endda = '99991231'.
      IF sy-subrc = 0.
        IF r1 = abap_true.

          SELECT  objid,
                  latitude,
                  longitude FROM hrp9011
                  INTO TABLE @DATA(lt_9011)
                  FOR ALL ENTRIES IN @lt_tab
            WHERE  plvar = '01' AND
                   otype = 'S'  AND
                   objid = @lt_tab-plans AND
                   istat = '1' AND
*                 begda  LE @sy-datum AND
                   endda  EQ '99991231'.
        ELSE.
          SELECT  objid,
                  travel,
                  category FROM hrp9013
                  INTO TABLE @DATA(lt_9013)
                  FOR ALL ENTRIES IN @lt_tab
            WHERE  plvar = '01' AND
                   otype = 'S'  AND
                   objid = @lt_tab-plans AND
                   istat = '1' AND
*                 begda  LE @sy-datum AND
                   endda  EQ '99991231'.
        ENDIF.
      ENDIF.
    ENDIF.
    LOOP AT lt_tab INTO DATA(lw_tab).
      MOVE-CORRESPONDING lw_tab TO gs_data.
      IF r1 = abap_true.
        gs_data-f1 = VALUE #( lt_9011[ objid = gs_data-plans ]-latitude OPTIONAL ).
        gs_data-f2 = gs_data-f1.
        gs_data-f3 = VALUE #( lt_9011[ objid = gs_data-plans ]-longitude OPTIONAL ).
        gs_data-f4 = gs_data-f3.
      ELSE.
        gs_data-f1 = VALUE #( lt_9013[ objid = gs_data-plans ]-travel OPTIONAL ).
        gs_data-f2 = gs_data-f1.
        gs_data-f3 = VALUE #( lt_9013[ objid = gs_data-plans ]-category OPTIONAL ).
        gs_data-f4 = gs_data-f3.
      ENDIF.
      APPEND gs_data TO gt_data.
    ENDLOOP.

  ENDMETHOD.
  METHOD build_alv.
*<-------------------------------------------

    DATA: lo_column TYPE REF TO cl_salv_column_list.

    DATA: lo_events TYPE REF TO cl_salv_events_table.
*    DATA: "lo_gr_alv       TYPE REF TO cl_salv_table, " Variables for ALV properties
*          lo_gr_functions TYPE REF TO cl_salv_functions_list.
*
*    DATA: lo_event_handler TYPE REF TO lcl_main, " Variables for events
*          lo_events        TYPE REF TO cl_salv_events_table.
*
*    DATA: lo_grid        TYPE REF TO cl_salv_form_layout_grid, " Variables for header
*          lo_layout_logo TYPE REF TO cl_salv_form_layout_logo,
*          lo_content     TYPE REF TO cl_salv_form_element,
*
*          lv_title       TYPE string,
*          lv_rows        TYPE string.
*
*    DATA: lo_layout TYPE REF TO cl_salv_layout, " Variables for enabling Save button
*          lv_key    TYPE salv_s_layout_key.
*
*    DATA: lo_display TYPE REF TO cl_salv_display_settings. " Variable for layout settings
*
*    DATA: lo_selections TYPE REF TO cl_salv_selections, " Variables for selection mode and column properties
*          lo_columns    TYPE REF TO cl_salv_columns,
*          lo_column     TYPE REF TO cl_salv_column_table.
*    DATA: lr_aggregations TYPE REF TO cl_salv_aggregations.
*    DATA: lr_groups TYPE REF TO cl_salv_sorts .
*    DATA: toolbar TYPE REF TO cl_salv_functions_list .
    IF go_alv_custom IS INITIAL .
* Create Custo Container
      CREATE OBJECT go_alv_custom
        EXPORTING
          container_name = 'MCONTAINER'. "Name of Screen CustCtrl Name to Link
** Splitter - it is neccessary to specify the default_screen as parent
*      gr_splitter = NEW #( parent = go_alv_custom
*                          no_autodef_progid_dynnr   = abap_true
*                          rows                      = 2
*                          columns                   = 1 ).
** Container 1 for header
*      gr_container_1 = gr_splitter->get_container(  row  = 1
*                                                    column = 1 ).
** Container 2 for detail
*      gr_container_2 = gr_splitter->get_container( row  = 2
*                                                   column = 1 ).
*
** Create the ALV object
*      cl_salv_table=>factory( EXPORTING r_container             = gr_container_1
*                                        container_name = 'MCONTAINER'
*                                        IMPORTING r_salv_table  = lo_gr_alv
*                                        CHANGING t_table         = gt_data ).
**
      TRY.
          CALL METHOD cl_salv_table=>factory
            EXPORTING
              r_container  = go_alv_custom "'MCONTAINER'
            IMPORTING
              r_salv_table = lo_gr_alv
            CHANGING
              t_table      = gt_data.
        CATCH cx_salv_msg.
      ENDTRY.

*    lo_gr_alv->set_screen_status(
*      pfstatus      =  'HO_STATUS'
*      report        =  sy-repid
*      set_functions = lo_gr_alv->c_functions_all ).
* ALV functions

      gr_salv_func_2 = lo_gr_alv->get_functions( ).
      gr_salv_func_2->set_all( abap_true ).
      INCLUDE <icon>.
      TRY.
          gr_salv_func_2->add_function(
            name     = '&EDIT'
            icon     = CONV string( icon_wd_text_edit )
            text     = 'EDIT'
            tooltip  = 'Edit'
            position = if_salv_c_function_position=>right_of_salv_functions ).
        CATCH cx_salv_existing cx_salv_wrong_call.
      ENDTRY.

      TRY.
          gr_salv_func_2->add_function(
            name     = '&UPD'
            icon     = CONV string( icon_system_save )
            text     = 'SAVE'
            tooltip  = 'Save'
            position = if_salv_c_function_position=>right_of_salv_functions ).
        CATCH cx_salv_existing cx_salv_wrong_call.
      ENDTRY.


* Fit the columns
      DATA(lo_columns) = lo_gr_alv->get_columns( ).
*  lo_columns->set_optimize( 'X' ).

** Create header
*    DESCRIBE TABLE gt_data LINES lv_rows.
*    CONCATENATE 'Number of records: ' lv_rows INTO lv_title SEPARATED BY space.

      TRY.
          lo_column ?= lo_columns->get_column( 'ENAME' ).
          lo_column->set_output_length( 25 ).
        CATCH cx_salv_not_found.
        CATCH cx_salv_existing.
        CATCH cx_salv_data_error.
      ENDTRY.
      TRY.
          lo_column ?= lo_columns->get_column( 'PLANS' ).
          lo_column->set_output_length( 10 ).
        CATCH cx_salv_not_found.
        CATCH cx_salv_existing.
        CATCH cx_salv_data_error.
      ENDTRY.
      TRY.
          lo_column ?= lo_columns->get_column( 'STEXT' ).
          lo_column->set_visible( if_salv_c_bool_sap=>true ).
          lo_column->set_output_length( 20 ).
        CATCH cx_salv_not_found.
        CATCH cx_salv_existing.
        CATCH cx_salv_data_error.
      ENDTRY.
      IF r2 = abap_true.
*      Search Help
        lo_column ?= lo_columns->get_column('F2').
        DATA  p_ddic TYPE salv_s_ddic_reference.
        p_ddic-table = 'HRP9013'.
        p_ddic-field = 'TRAVEL'.
        lo_column->set_ddic_reference( p_ddic ).
        lo_column->set_f4( if_salv_c_bool_sap=>true ).
        lo_column ?= lo_columns->get_column('F4').
        p_ddic-table = 'HRP9013'.
        p_ddic-field = 'CATEGORY'.
        lo_column->set_ddic_reference( p_ddic ).
        lo_column->set_f4( if_salv_c_bool_sap=>true ).

        TRY.
            lo_column ?= lo_columns->get_column( 'F1' ).
            lo_column->set_visible( if_salv_c_bool_sap=>true ).
            lo_column->set_long_text( 'Travel Allowed' ).
            lo_column->set_medium_text( 'TravelAllowed' ).
            lo_column->set_short_text( 'Tr.Allowed' ).
            lo_column->set_output_length( 25 ).
          CATCH cx_salv_not_found.
          CATCH cx_salv_existing.
          CATCH cx_salv_data_error.
        ENDTRY.
        TRY.
            lo_column ?= lo_columns->get_column( 'F2' ).
            lo_column->set_visible( if_salv_c_bool_sap=>true ).
            lo_column->set_long_text( 'New Travel Allowed' ).
            lo_column->set_medium_text( 'TravelAllowed' ).
            lo_column->set_short_text( 'Trvl.Allw' ).
            lo_column->set_output_length( 25 ).
          CATCH cx_salv_not_found.
          CATCH cx_salv_existing.
          CATCH cx_salv_data_error.
        ENDTRY.

        TRY.
            lo_column ?= lo_columns->get_column( 'F3' ).
            lo_column->set_visible( if_salv_c_bool_sap=>true ).
            lo_column->set_long_text( 'Travel Category' ).
            lo_column->set_medium_text( 'Travel Cat' ).
            lo_column->set_short_text( 'Trav.Cat' ).
            lo_column->set_output_length( 25 ).
          CATCH cx_salv_not_found.
          CATCH cx_salv_existing.
          CATCH cx_salv_data_error.
        ENDTRY.
        TRY.
            lo_column ?= lo_columns->get_column( 'F4' ).
            lo_column->set_visible( if_salv_c_bool_sap=>true ).
            lo_column->set_long_text( 'New Travel Category' ).
            lo_column->set_medium_text( 'Travel Cat' ).
            lo_column->set_short_text( 'Travel Cat' ).
            lo_column->set_output_length( 25 ).
          CATCH cx_salv_not_found.
          CATCH cx_salv_existing.
          CATCH cx_salv_data_error.
        ENDTRY.
      ENDIF.
      IF r1 = abap_true.
        TRY.
            lo_column ?= lo_columns->get_column( 'F1' ).
            lo_column->set_visible( if_salv_c_bool_sap=>true ).
            lo_column->set_long_text( 'Latitude' ).
            lo_column->set_medium_text( 'Latitude' ).
            lo_column->set_short_text( 'Latitude' ).
            lo_column->set_output_length( 25 ).
          CATCH cx_salv_not_found.
          CATCH cx_salv_existing.
          CATCH cx_salv_data_error.
        ENDTRY.
        TRY.
            lo_column ?= lo_columns->get_column( 'F2' ).
            lo_column->set_visible( if_salv_c_bool_sap=>true ).
            lo_column->set_long_text( 'New Latitude' ).
            lo_column->set_medium_text( 'Latitude' ).
            lo_column->set_short_text( 'Latitude' ).
            lo_column->set_output_length( 25 ).
          CATCH cx_salv_not_found.
          CATCH cx_salv_existing.
          CATCH cx_salv_data_error.
        ENDTRY.
        TRY.
            lo_column ?= lo_columns->get_column( 'F3' ).
            lo_column->set_visible( if_salv_c_bool_sap=>true ).
            lo_column->set_long_text( 'Longitude' ).
            lo_column->set_medium_text( 'Longitude' ).
            lo_column->set_short_text( 'Longitude' ).
            lo_column->set_output_length( 25 ).
          CATCH cx_salv_not_found.
          CATCH cx_salv_existing.
          CATCH cx_salv_data_error.
        ENDTRY.
        TRY.
            lo_column ?= lo_columns->get_column( 'F4' ).
            lo_column->set_visible( if_salv_c_bool_sap=>true ).
            lo_column->set_long_text( 'New Longitude' ).
            lo_column->set_medium_text( 'Longitude' ).
            lo_column->set_short_text( 'Longitude' ).
            lo_column->set_output_length( 25 ).
          CATCH cx_salv_not_found.
          CATCH cx_salv_existing.
          CATCH cx_salv_data_error.
        ENDTRY.
      ENDIF.
* Register events
      lo_events = lo_gr_alv->get_event( ).
      SET HANDLER on_user_command FOR lo_events.

      lo_gr_alv->display( ).
    ELSE.
      lo_gr_alv->refresh( ).
    ENDIF.
  ENDMETHOD.
*&---------------------------------------------------------------------*
*& Form update_9011
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
  METHOD update_9011 .

    DATA ls_p9011 TYPE p9011.

    DATA lt_p9011 TYPE TABLE OF p9011.

    DATA: lt_enque_tab TYPE TABLE OF ppenq,
          ls_enque_tab TYPE ppenq.
*  CONSTANTS: act_vtask TYPE hrrhap-vtask VALUE 'B'.

    LOOP AT gt_data INTO gs_data.

      CLEAR: lt_p9011, ls_p9011.
      ls_p9011-objid = gs_data-plans.
      ls_p9011-otype = 'S'.
      ls_p9011-istat = '1'.
      ls_p9011-plvar = '01'.
      ls_p9011-infty = '9011'.
      ls_p9011-begda = p_date.
      ls_p9011-endda = '99991231'.
      ls_p9011-latitude = gs_data-f2.
      ls_p9011-longitude = gs_data-f4.

      APPEND ls_p9011 TO lt_p9011.

      CLEAR ls_enque_tab.
      REFRESH: lt_enque_tab.

      ls_enque_tab-plvar = ls_p9011-plvar.
      ls_enque_tab-otype = 'S'.
      ls_enque_tab-objid = ls_p9011-objid.
      APPEND ls_enque_tab TO lt_enque_tab.

      CALL FUNCTION 'RH_ENQUEUE_LIST'
        TABLES
          enq_tab = lt_enque_tab
        EXCEPTIONS
          OTHERS  = 1.
      IF sy-subrc NE 0.
        gs_log_report-type = 'E'.
        gs_log_report-message = |{ ls_p9011-objid }- could not be lock|.
        APPEND gs_log_report TO gt_log_report.
        CONTINUE.
      ENDIF.
      CALL FUNCTION 'RH_INSERT_INFTY'
        EXPORTING
          fcode               = 'INSE'
          vtask               = 'B' "act_vtask
        TABLES
          innnn               = lt_p9011
        EXCEPTIONS
          corr_exit           = 01
          no_authorization    = 08
          error_during_insert = 04
          OTHERS              = 04.

      CASE sy-subrc.
        WHEN 0.
          CALL FUNCTION 'RH_UPDATE_DATABASE'
            EXPORTING
              vtask     = 'D'
            EXCEPTIONS
              corr_exit = 1
              OTHERS    = 2.
          IF sy-subrc = 0.
            gs_log_report-type = 'S'.
            gs_log_report-message = |{ ls_p9011-objid }- Base Location Changed Successfully|.
          ELSE.
            gs_log_report-type = 'E'.
            gs_log_report-message = |{ ls_p9011-objid }- Error during updating the database|.
          ENDIF.
        WHEN 1 OR 4.
          gs_log_report-type = 'E'.
          gs_log_report-message = |{ ls_p9011-objid }- Error during creating the record|.
        WHEN 8.
          gs_log_report-type = 'E'.
          gs_log_report-message = |{ ls_p9011-objid }- No Authorization to Create Relationship record|.
        WHEN OTHERS.
          gs_log_report-type = 'E'.
          gs_log_report-message = |{ ls_p9011-objid }- Error during creating the record|.
      ENDCASE.

      APPEND gs_log_report TO gt_log_report.
      CLEAR gs_log_report.

      CALL FUNCTION 'RH_DEQUEUE_LIST'
        TABLES
          deq_tab = lt_enque_tab.
      CALL FUNCTION 'RH_CLEAR_BUFFER'.
      CALL FUNCTION 'RH_CLEAR_PLOG_TAB'.
      CALL FUNCTION 'DEQUEUE_ALL'.
    ENDLOOP.
  ENDMETHOD.
*&---------------------------------------------------------------------*
*& Form update_9013
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
  METHOD update_9013 .

    DATA ls_p9013 TYPE p9013.

    DATA lt_p9013 TYPE TABLE OF p9013.

    DATA: lt_enque_tab TYPE TABLE OF ppenq,
          ls_enque_tab TYPE ppenq.
*  CONSTANTS: act_vtask LIKE hrrhap-vtask VALUE 'B'.

    LOOP AT gt_data INTO gs_data.

      CLEAR ls_p9013.
      ls_p9013-objid = gs_data-plans.
      ls_p9013-otype = 'S'.
      ls_p9013-istat = '1'.
      ls_p9013-plvar = '01'.
      ls_p9013-infty = '9013'.
      ls_p9013-begda = p_date.
      ls_p9013-endda = '99991231'.
      ls_p9013-travel = gs_data-f2.
      ls_p9013-category = gs_data-f4.

      APPEND ls_p9013 TO lt_p9013.

      CLEAR ls_enque_tab.
      REFRESH: lt_enque_tab.

      ls_enque_tab-plvar = ls_p9013-plvar.
      ls_enque_tab-otype = 'S'.
      ls_enque_tab-objid = ls_p9013-objid.
      APPEND ls_enque_tab TO lt_enque_tab.

      CALL FUNCTION 'RH_ENQUEUE_LIST'
        TABLES
          enq_tab = lt_enque_tab
        EXCEPTIONS
          OTHERS  = 1.
      IF sy-subrc NE 0.
        gs_log_report-type = 'E'.
        gs_log_report-message = |{ ls_p9013-objid }- could not be lock|.
        APPEND gs_log_report TO gt_log_report.
        CONTINUE.
      ENDIF.

      CALL FUNCTION 'RH_INSERT_INFTY'
        EXPORTING
          fcode               = 'INSE'
          vtask               = 'B' "act_vtask
        TABLES
          innnn               = lt_p9013
        EXCEPTIONS
          corr_exit           = 01
          no_authorization    = 08
          error_during_insert = 04
          OTHERS              = 04.

      CASE sy-subrc.
        WHEN 0.
          CALL FUNCTION 'RH_UPDATE_DATABASE'
            EXPORTING
              vtask     = 'D'
            EXCEPTIONS
              corr_exit = 1
              OTHERS    = 2.
          IF sy-subrc = 0.
            gs_log_report-type = 'S'.
            gs_log_report-message = |{ ls_p9013-objid }- Travel Allowed Changed Successfully|.
          ELSE.
            gs_log_report-type = 'E'.
            gs_log_report-message = |{ ls_p9013-objid }- Error during updating the database|.
          ENDIF.
        WHEN 1 OR 4.
          gs_log_report-type = 'E'.
          gs_log_report-message = |{ ls_p9013-objid }- Error during creating the record|.
        WHEN 8.
          gs_log_report-type = 'E'.
          gs_log_report-message = |{ ls_p9013-objid }- No Authorization to Create Relationship record|.
        WHEN OTHERS.
          gs_log_report-type = 'E'.
          gs_log_report-message = |{ ls_p9013-objid }- Error during creating the record|.
      ENDCASE.

      APPEND gs_log_report TO gt_log_report.
      CLEAR gs_log_report.


      CALL FUNCTION 'RH_DEQUEUE_LIST'
        TABLES
          deq_tab = lt_enque_tab.
      CALL FUNCTION 'RH_CLEAR_BUFFER'.
      CALL FUNCTION 'RH_CLEAR_PLOG_TAB'.
      CALL FUNCTION 'DEQUEUE_ALL'.
    ENDLOOP.
  ENDMETHOD..

ENDCLASS.                    "lcl_handle_events IMPLEMENTATION

* declare object for dragdrop object.
DATA : go_main TYPE REF TO lcl_main.
*&---------------------------------------------------------------------*
*& Form handle_user_command
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_SALV_FUNCTION
*&---------------------------------------------------------------------*
FORM handle_user_command  USING i_ucomm TYPE  salv_de_function.
  DATA answer           TYPE c.
  CASE i_ucomm.
    WHEN '&UPD'.
*      lo_gr_alv->refresh( ).
      IF go_main->gt_log_report IS NOT INITIAL.
        CALL FUNCTION 'Z_POPUP_ALV'
          EXPORTING
            i_start_column      = 25
            i_start_line        = 6
            i_end_column        = 100
            i_end_line          = 20
            i_title             = 'HO Admin Changes Status'
            i_status_field_name = 'TYPE'
            i_popup             = 'X'
          TABLES
            it_alv              = go_main->gt_log_report.
      ELSE.
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = 'HO Admin Changes for Eompoyee''s Information'
            text_question         = 'Do you want to Confirm for Changes'
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

        IF r1 = abap_true." IT9011 Base Location
          go_main->update_9011( ).
        ENDIF.
        IF r2 = abap_true." IT9013 Travel Allowed
          go_main->update_9013( ).
        ENDIF.
        IF go_main->gt_log_report IS NOT INITIAL.
          CALL FUNCTION 'Z_POPUP_ALV'
            EXPORTING
              i_start_column      = 25
              i_start_line        = 6
              i_end_column        = 100
              i_end_line          = 20
              i_title             = 'HO Admin Changes Status'
              i_status_field_name = 'TYPE'
              i_popup             = 'X'
            TABLES
              it_alv              = go_main->gt_log_report.
          LEAVE TO SCREEN 0.
        ENDIF.
      ENDIF.
    WHEN '&EDIT'.
      lcl_salv_buddy=>set_editable( i_fieldname = 'F2' i_salv_table = go_main->lo_gr_alv i_editable = abap_true ).
      lcl_salv_buddy=>set_editable( i_fieldname = 'F4' i_salv_table = go_main->lo_gr_alv i_editable = abap_true ).
    WHEN OTHERS.
  ENDCASE.
ENDFORM.

*&---------------------------------------------------------------------*
*& Module STATUS_9001 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_9001 OUTPUT.
  SET PF-STATUS 'HO_STATUS'.
  SET TITLEBAR 'HO_TITLE'.

  go_main->process_data( ).

  go_main->build_alv( ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9001 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK' OR '%EX' OR 'RW' OR 'EXIT'. LEAVE TO SCREEN 0 .
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
