*
**&---------------------------------------------------------------------*
**& Include          ZHR_HOADMIN_EMP_CLS
**&---------------------------------------------------------------------*
**----------------------------------------------------------------------*
**       CLASS lcl_salv_buddy DEFINITION
**----------------------------------------------------------------------*
**
** This class is meant to be used with SALV objects to gain access to the GUI control that exist behind them.
** All SALV objects are descendants of the CL_SALV_MODEL_BASE class. Currently, there are the following SALV object classes:
**   CL_SALV_TABLE
**   CL_SALV_HIERSEQ_TABLE
**   CL_SALV_TREE
**
** We inherit from the abstract class CL_SAL_CONTROLLER because SALV objects have it as a FRIEND, thus
**   making us a FRIEND of SALV objects too. That allows us to have access to ALL attributes and methods of
**   SALV objects, regardless of them being PUBLIC, PROTECTED or PRIVATE. Example: R_CONTROLLER.
** Since this class only has STATIC methods, there's no need to instantiate objects of its type, thus the CRIATE PRIVATE.
** Finally, I see no need to inherit from this class, so this class is declared FINAL.
**----------------------------------------------------------------------*
*CLASS lcl_salv_buddy DEFINITION INHERITING FROM cl_salv_controller CREATE PRIVATE FINAL.
*
*  PUBLIC SECTION.
*
**----------------------------------------------------------------------*
** GET_CONTROL_RTTI - Returns runtime type information for the control that is behind a SALV object.
**   Parameter E_ADAPTER_TYPE returns the adapter type of a SALV object.
**     Based on this information, method GET_CONTROL will return a different control in its returrning parameter R_CONTROL.
**     You can use this runtime type information to choose the right control object to supply to the returning parameter R_CONTROL of method GET_CONTROL.
**   Parameter E_CONTROL_RTTI returns a TYPE HANDLE that you can use to create an object compatible with the returning parameter R_CONTROL of method GET_CONTROL.
** Below there is a correspondence between the adapter type returned in parameter E_ADAPTER_TYPE and
** the type of the control expected in parameter R_CONTROL of method GET_CONTROL:
**   IF_SALV_ADAPTER~C_ADAPTER_TYPE_FULLSCREEN       CL_GUI_ALV_GRID
**   IF_SALV_ADAPTER~C_ADAPTER_TYPE_GRID             CL_GUI_ALV_GRID
**   IF_SALV_ADAPTER~C_ADAPTER_TYPE_HIERSEQ          nothing (null)
**   IF_SALV_ADAPTER~C_ADAPTER_TYPE_LIST             CL_SALV_TABLE
**   IF_SALV_ADAPTER~C_ADAPTER_TYPE_TREE             CL_SALV_GUI_TREE
**   IF_SALV_ADAPTER~C_ADAPTER_TYPE_APPEND           nothing (null)
**----------------------------------------------------------------------*
*    CLASS-METHODS: get_control_rtti IMPORTING i_salv         TYPE REF TO cl_salv_model_base
*                                    EXPORTING e_adapter_type TYPE salv_de_adapter_type
*                                              e_control_rtti TYPE REF TO cl_abap_typedescr,
*
**----------------------------------------------------------------------*
** GET_CONTROL - Returns the control that is behind the SALV object.
**   MUST be called after the DISPLAY method of the SALV object, so that its control gets created.
**   See method GET_CONTROL_RTTI above for a correspondence between what you supply in paramter I_SALV and what you get back in parameter R_CONTROL.
**----------------------------------------------------------------------*
*      get_control IMPORTING i_salv           TYPE REF TO cl_salv_model_base
*                  RETURNING VALUE(r_control) TYPE REF TO object,
*
**----------------------------------------------------------------------*
** SET_EDITABLE - Enables OR disables editing on a CL_SALV_TABLE object.
**   If you supply parameter I_FIELDNAME and supply it NOT INITIAL, you get that
**     field enabled or disabled for editing, depending on parameter I_EDITABLE.
**   If you do not supply parameter I_FIELDNAME or supply it INITIAL, you get
**     all fields of the table enabled or disabled for editing, depending on parameter I_EDITABLE.
**   Parameter I_SALV_TABLE is the CL_SALV_TABLE object you want to enable or disable editing for.
**   Parameter I_EDITABLE should be ABAP_TRUE or ABAP_FALSE to enable or disable editing.
**   Parameter I_REFRESH indicates whether you want the control to be refreshed or not. You'll only see the changes
**     you've made using this method AFTER you do a refresh on the CL_SALV_TABLE object.
**   NOTE: If you want field per field editing capabilities, you MUST make sure editing for the whole table is disabled.
**         You can disable editing for the whole table simply by issuing a call to this method, omitting parameter I_FIELDNAME and
**         passing parameter I_EDITABLE as ABAP_FALSE. After that you can enable or disable editing on a field per field basis.
**         The CL_SALV_TABLE is disabled for editing by default.
**----------------------------------------------------------------------*
*      set_editable IMPORTING VALUE(i_fieldname) TYPE csequence OPTIONAL
*                             i_salv_table       TYPE REF TO cl_salv_table
*                             VALUE(i_editable)  TYPE abap_bool DEFAULT abap_true
*                             VALUE(i_refresh)   TYPE abap_bool DEFAULT abap_true.
*
*  PRIVATE SECTION.
*
**----------------------------------------------------------------------*
** GET_CONTROL_INTERNAL - It is the guts of method GET_CONTROL. It does all the heavy work and method GET_CONTROL gets all the credits
**----------------------------------------------------------------------*
*    CLASS-METHODS: get_control_internal IMPORTING i_salv         TYPE REF TO cl_salv_model_base
*                                        EXPORTING e_adapter_type TYPE salv_de_adapter_type
*                                                  e_control      TYPE REF TO object.
*
*ENDCLASS.                    "lcl_salv_buddy DEFINITION
*
**----------------------------------------------------------------------*
**       CLASS lcl_salv_buddy IMPLEMENTATION
**----------------------------------------------------------------------*
*CLASS lcl_salv_buddy IMPLEMENTATION.
*
*
*  METHOD get_control_internal.
*
*    DATA: lo_controller            TYPE REF TO cl_salv_controller_model,
*          lo_adapter               TYPE REF TO cl_salv_adapter,
*          lo_fullscreen_adapter    TYPE REF TO cl_salv_fullscreen_adapter,
*          lo_grid_adapter          TYPE REF TO cl_salv_grid_adapter,
*          lo_table_display_adapter TYPE REF TO if_salv_table_display_adapter,
*          lo_tree_adapter_base     TYPE REF TO cl_salv_tree_adapter_base.
*
*    CHECK e_adapter_type IS REQUESTED OR
*          e_control      IS REQUESTED.
*
*    IF  e_adapter_type IS REQUESTED.
*      CLEAR e_adapter_type.
*    ENDIF.
*
*    IF  e_control IS REQUESTED.
*      CLEAR e_control.
*    ENDIF.
*
*    lo_controller = i_salv->r_controller.
*    CHECK lo_controller IS BOUND.
*
*    lo_adapter = lo_controller->r_adapter.
*    CHECK lo_adapter IS BOUND.
*
*    IF e_adapter_type IS REQUESTED.
*      e_adapter_type = lo_adapter->type.
*    ENDIF.
*
*    CHECK e_control IS REQUESTED.
*
*    CASE lo_adapter->type.
*      WHEN lo_adapter->if_salv_adapter~c_adapter_type_fullscreen.
*        lo_fullscreen_adapter ?= lo_adapter.
*        e_control = lo_fullscreen_adapter->get_grid( ).
*
*      WHEN lo_adapter->if_salv_adapter~c_adapter_type_grid.
*        lo_grid_adapter ?= lo_adapter.
*        e_control = lo_grid_adapter->get_grid( ).
*
*      WHEN lo_adapter->if_salv_adapter~c_adapter_type_hierseq.
*
*      WHEN lo_adapter->if_salv_adapter~c_adapter_type_list.
*        lo_table_display_adapter ?= lo_adapter.
*        e_control = lo_table_display_adapter->r_table.
*
*      WHEN lo_adapter->if_salv_adapter~c_adapter_type_tree.
*        lo_tree_adapter_base ?= lo_adapter.
*        e_control = lo_tree_adapter_base->r_tree.
*
*      WHEN lo_adapter->if_salv_adapter~c_adapter_type_append.
*
*    ENDCASE.
*
*  ENDMETHOD.                    "get_control_internal
*
*  METHOD get_control_rtti.
*
*    DATA: lv_adapter_type TYPE salv_de_adapter_type,
*          lo_control      TYPE REF TO object.
*
*    CHECK e_adapter_type IS REQUESTED OR
*          e_control_rtti IS REQUESTED.
*
*    IF  e_adapter_type IS REQUESTED.
*      CLEAR e_adapter_type.
*    ENDIF.
*
*    IF  e_control_rtti IS REQUESTED.
*      CLEAR e_control_rtti.
*    ENDIF.
*
*    get_control_internal( EXPORTING i_salv = i_salv IMPORTING e_adapter_type = lv_adapter_type e_control = lo_control ).
*
*    IF e_adapter_type IS REQUESTED.
*      e_adapter_type = lv_adapter_type.
*    ENDIF.
*
*    IF e_control_rtti IS REQUESTED.
*      e_control_rtti = cl_abap_typedescr=>describe_by_object_ref( lo_control ).
*    ENDIF.
*
*  ENDMETHOD.                    "get_control_rtti
*
*  METHOD get_control.
*
*    CHECK r_control IS REQUESTED.
*
*    get_control_internal( EXPORTING i_salv = i_salv IMPORTING e_control = r_control ).
*
*  ENDMETHOD.                    "get_control
*
*  METHOD set_editable.
*    CONSTANTS: lc_stable TYPE lvc_s_stbl VALUE 'XX'.
*
*    DATA: lo_grid     TYPE REF TO cl_gui_alv_grid,
*          lt_fieldcat TYPE lvc_t_fcat,
*          ls_layout   TYPE lvc_s_layo.
*
*    FIELD-SYMBOLS: <fs_fieldcat> LIKE LINE OF lt_fieldcat.
*
*    lo_grid ?= get_control( i_salv_table ).
*    CHECK lo_grid IS BOUND.
*
*    IF i_fieldname IS SUPPLIED AND
*       i_fieldname IS NOT INITIAL.
*      lo_grid->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = lt_fieldcat ).
*      READ TABLE lt_fieldcat ASSIGNING <fs_fieldcat> WITH KEY fieldname = i_fieldname.
*      CHECK sy-subrc = 0.
*      <fs_fieldcat>-edit = i_editable.
*      lo_grid->set_frontend_fieldcatalog( lt_fieldcat ).
*    ELSE.
*      lo_grid->get_frontend_layout( IMPORTING es_layout = ls_layout ).
*      ls_layout-edit = i_editable.
*      lo_grid->set_frontend_layout( EXPORTING is_layout = ls_layout ).
*    ENDIF.
*
*    CHECK i_refresh = abap_true.
*    i_salv_table->refresh( lc_stable ).
*
*  ENDMETHOD.                    "set_editable
*
*ENDCLASS.                    "lcl_salv_buddy IMPLEMENTATION
**&---------------------------------------------------------------------*
**& Include          ZMM_SERVICE_PR_CREATION_CLS
**&---------------------------------------------------------------------*
**---------------------------------------------------------------------*
**       CLASS lcl_handle_events DEFINITION
**---------------------------------------------------------------------*
** §5.1 define a local class for handling events of cl_salv_table
**---------------------------------------------------------------------*
*CLASS lcl_main DEFINITION.
*
*  PUBLIC SECTION.
*    CLASS-DATA : lo_gr_alv       TYPE REF TO cl_salv_table. " Variables for ALV properties
*    METHODS:
*      process_data,
*      build_alv,
*      print_appt_letter,
**      update_9011,
**      update_9013,
*      on_user_command FOR EVENT added_function OF cl_salv_events
*        IMPORTING e_salv_function.
*
*    TYPES:BEGIN OF ty_log_report,
*            type(1) TYPE c,
*            message TYPE text255,
*          END OF ty_log_report.
*    DATA: gt_log_report TYPE TABLE OF ty_log_report,
*          gs_log_report TYPE ty_log_report.
*  PRIVATE SECTION.
*    TYPES: BEGIN OF ty_output,
*             mark(1)     TYPE c,
*             empno       TYPE p0001-pernr,
*             title       TYPE char2,
*             empname     TYPE p0001-ename,
*             designation TYPE p9020-design,
*             department  TYPE p9020-department,
*             location    TYPE p9020-location,
*             repmanager  TYPE p0001-ename,
*             address1    TYPE p0006-stras,
*             address2    TYPE p0006-locat,
*             addresscity TYPE p0006-ort01,
*             addressdist TYPE p0006-ort02,
*             addresspin  TYPE p0006-pstlz,
*             joindate    TYPE p0001-begda,
*             grosspay    TYPE char10,
*             variable    TYPE char10,
*           END OF ty_output.
*    DATA: go_alv_custom  TYPE REF TO cl_gui_custom_container,
*          gr_splitter    TYPE REF TO cl_gui_splitter_container,
*          gr_container_1 TYPE REF TO cl_gui_container,
*          gr_container_2 TYPE REF TO cl_gui_container,
*          gr_salv_func_2 TYPE REF TO cl_salv_functions.
*
*    DATA: gt_data TYPE TABLE OF ty_output,
*          gs_data TYPE ty_output.
*
*ENDCLASS.                    "lcl_handle_events DEFINITION
*
**---------------------------------------------------------------------*
**       CLASS lcl_handle_events IMPLEMENTATION
**---------------------------------------------------------------------*
** §5.2 implement the events for handling the events of cl_salv_table
**---------------------------------------------------------------------*
*CLASS lcl_main IMPLEMENTATION.
*  METHOD on_user_command.
*    PERFORM handle_user_command USING e_salv_function.
*  ENDMETHOD.                    "on_user_command
*  METHOD process_data.
*
*    IF s_pernr[] IS NOT INITIAL.
*      SELECT  a~sprps,
*              a~pernr    AS empno,
*       CASE WHEN f~gesch = '1' THEN 'Mr'
*            WHEN f~gesch = '2' THEN 'Ms'
*       END AS title,
*             a~ename    AS empname,
**             a~plans,
*             b~design   AS designation,
**             a~orgeh,
*             b~department,
*             b~location,
**             c~greytmngid,
*             d~ename    AS repmanager,
*             e~stras    AS address1,
*             e~locat    AS address2,
*             e~ort01    AS addresscity,
*             e~ort02    AS addressdist,
*             e~pstlz    AS addresspin,
*             CASE WHEN g~dar01 = 'S1' THEN g~dat01
*                  WHEN g~dar02 = 'S1' THEN g~dat02
*                  WHEN g~dar03 = 'S1' THEN g~dat03
*                  WHEN g~dar04 = 'S1' THEN g~dat04
*                  WHEN g~dar05 = 'S1' THEN g~dat05
*                  WHEN g~dar06 = 'S1' THEN g~dat06
*                  END
*                        AS joindate
**              a~kostl   AS grosspay,
**              a~kostl   AS variable
*                  INTO TABLE @DATA(lt_tab)
*                  FROM pa0001 AS a INNER JOIN pa9020 AS b ON b~pernr  =   a~pernr     AND
*                                                             b~endda  >=  @sy-datum
*                                   INNER JOIN pa9021 AS c ON c~pernr  =   a~pernr     AND
*                                                             c~endda  >=  @sy-datum
*                                   INNER JOIN pa0001 AS d ON d~pernr  =   c~greytmngid AND
*                                                             d~endda  >=  @sy-datum
*                                   INNER JOIN pa0006 AS e ON e~pernr  =   a~pernr AND
*                                                             e~subty  =   '1'     AND
*                                                             e~endda  >=  @sy-datum
*                                   INNER JOIN pa0002 AS f ON f~pernr  =   a~pernr AND
*                                                             f~endda  >=  @sy-datum
*                                   INNER JOIN pa0041 AS g ON g~pernr  =   a~pernr AND
*                                                             g~endda  >=  @sy-datum
*                  WHERE a~pernr IN @s_pernr AND
*                        a~endda = '99991231'.
*    ENDIF.
*    gt_data = lt_tab.
*
*  ENDMETHOD.
*  METHOD build_alv.
**<-------------------------------------------
*
*    DATA: lo_column TYPE REF TO cl_salv_column_list.
*
*    DATA: lo_events TYPE REF TO cl_salv_events_table.
*    IF go_alv_custom IS INITIAL .
** Create Custo Container
*      CREATE OBJECT go_alv_custom
*        EXPORTING
*          container_name = 'MCONTAINER'. "Name of Screen CustCtrl Name to Link
*
** Create the ALV object
*      TRY.
*          cl_salv_table=>factory( EXPORTING r_container             = gr_container_1
*                                            container_name = 'MCONTAINER'
*                                            IMPORTING r_salv_table  = lo_gr_alv
*                                            CHANGING t_table         = gt_data ).
*        CATCH cx_salv_msg.
*      ENDTRY.
**
***
*      CALL METHOD cl_salv_table=>factory
*        EXPORTING
*          r_container  = go_alv_custom "'MCONTAINER'
*        IMPORTING
*          r_salv_table = lo_gr_alv
*        CHANGING
*          t_table      = gt_data.
*
** ALV functions
*
*      gr_salv_func_2 = lo_gr_alv->get_functions( ).
*      gr_salv_func_2->set_all( abap_true ).
*      INCLUDE <icon>.
*      TRY.
*          gr_salv_func_2->add_function(
*            name     = '&EDIT'
*            icon     = CONV string( icon_wd_text_edit )
*            text     = 'EDIT'
*            tooltip  = 'Edit'
*            position = if_salv_c_function_position=>right_of_salv_functions ).
*        CATCH cx_salv_existing cx_salv_wrong_call.
*      ENDTRY.
*
*      TRY.
*          gr_salv_func_2->add_function(
*            name     = '&UPD'
*            icon     = CONV string( icon_system_save )
*            text     = 'Print Letter'
*            tooltip  = 'Print Letter'
*            position = if_salv_c_function_position=>right_of_salv_functions ).
*        CATCH cx_salv_existing cx_salv_wrong_call.
*      ENDTRY.
*
*
** Fit the columns
*      DATA(lo_columns) = lo_gr_alv->get_columns( ).
*      lo_columns->set_optimize( 'X' ).
*
*      TRY.
*          lo_column ?= lo_columns->get_column( 'MARK' ).
*          lo_column->set_visible( if_salv_c_bool_sap=>true ).
*          lo_column->set_long_text( 'Title' ).
*          lo_column->set_medium_text( 'Title' ).
*          lo_column->set_short_text( 'Title' ).
*        CATCH cx_salv_not_found.
*        CATCH cx_salv_existing.
*        CATCH cx_salv_data_error.
*      ENDTRY.
*      TRY.
*          lo_column ?= lo_columns->get_column( 'TITLE' ).
**          lo_column->set_visible( if_salv_c_bool_sap=>true ).
*          lo_column->set_long_text( 'Title' ).
*          lo_column->set_medium_text( 'Title' ).
*          lo_column->set_short_text( 'Title' ).
*        CATCH cx_salv_not_found.
*        CATCH cx_salv_existing.
*        CATCH cx_salv_data_error.
*      ENDTRY.
*      TRY.
*          lo_column ?= lo_columns->get_column( 'REPMANAGER' ).
*          lo_column->set_long_text( 'Rep.Manager' ).
*          lo_column->set_medium_text( 'Rep.Manager' ).
*          lo_column->set_short_text( 'Rep.Manger' ).
*        CATCH cx_salv_not_found.
*        CATCH cx_salv_existing.
*        CATCH cx_salv_data_error.
*      ENDTRY.
*
*      TRY.
*          lo_column ?= lo_columns->get_column( 'JOINDATE' ).
*          lo_column->set_long_text( 'Joining Date' ).
*          lo_column->set_medium_text( 'Joining Date' ).
*          lo_column->set_short_text( 'JoinDate' ).
*        CATCH cx_salv_not_found.
*        CATCH cx_salv_existing.
*        CATCH cx_salv_data_error.
*      ENDTRY.
*      TRY.
*          lo_column ?= lo_columns->get_column( 'GROSSPAY' ).
*          lo_column->set_long_text( 'Gross Pay' ).
*          lo_column->set_medium_text( 'Gross Pay' ).
*          lo_column->set_short_text( 'GrossPay' ).
*          lo_column->set_output_length( 15 ).
*        CATCH cx_salv_not_found.
*        CATCH cx_salv_existing.
*        CATCH cx_salv_data_error.
*      ENDTRY.
*      TRY.
*          lo_column ?= lo_columns->get_column( 'VARIABLE' ).
*          lo_column->set_long_text( 'Variable Pay' ).
*          lo_column->set_medium_text( 'Variable Pay' ).
*          lo_column->set_short_text( 'Variable' ).
*          lo_column->set_output_length( 15 ).
*        CATCH cx_salv_not_found.
*        CATCH cx_salv_existing.
*        CATCH cx_salv_data_error.
*      ENDTRY.
** Register events
*      lo_events = lo_gr_alv->get_event( ).
*      SET HANDLER on_user_command FOR lo_events.
*
*      lo_gr_alv->display( ).
*    ELSE.
*      lo_gr_alv->refresh( ).
*    ENDIF.
*  ENDMETHOD.
*  METHOD print_appt_letter.
*    DATA lw_appointment_letter TYPE zhr_appointment_letter.
*    DATA ls_docpara TYPE sfpdocparams.
*    DATA ls_outpara TYPE sfpoutputparams.
*    DATA ls_output  TYPE fpformoutput.
*    DATA  ls_frmname TYPE fpname.
*    DATA lv_fm      TYPE rs38l_fnam.
*    ls_frmname = 'ZHR_APPMT_LETTER'.
*
*
*    lw_appointment_letter-empno = '701975'.
*    lw_appointment_letter-title = 'Mr'.
*    lw_appointment_letter-empname = 'Ramakrishnan Janardhanam'.
*    lw_appointment_letter-letterref = 'REF1/2023/01/062023'.
*    lw_appointment_letter-letterdate = '1 Jun 2023'.
*    lw_appointment_letter-address1 = 'TVS Colony avenue'.
*    lw_appointment_letter-address2 = 'Anna Nagar West Ext'.
*    lw_appointment_letter-addresscity = 'Chennai'.
*    lw_appointment_letter-addresspin = '600101'.
*    lw_appointment_letter-joindate = '01.01.2021'.
*    lw_appointment_letter-designation = 'IT SAP Manager SAP and Infra'.
*    lw_appointment_letter-department = 'IT SAP'.
*    lw_appointment_letter-repmanager = 'Sanjhana Sudhir'.
*    lw_appointment_letter-location = 'Head Office Chennai'.
*
*    lw_appointment_letter-w_sal_month-basic = '5,800'.
*    lw_appointment_letter-w_sal_month-hra = '2,900'.
*    lw_appointment_letter-w_sal_month-conveyance = '1,600'.
*    lw_appointment_letter-w_sal_month-medical = '-'.
*    lw_appointment_letter-w_sal_month-food = '2,500'.
*    lw_appointment_letter-w_sal_month-special = '1,701'.
*    lw_appointment_letter-w_sal_month-grosspay = '14,501'.
*    lw_appointment_letter-w_sal_month-pfemployee = '1,392'.
*    lw_appointment_letter-w_sal_month-esiemployee = '109'.
*    lw_appointment_letter-w_sal_month-netpay = '13,000'.
*    lw_appointment_letter-w_sal_month-pfemployer = '1,392'.
*    lw_appointment_letter-w_sal_month-esiemployer = '471'.
*    lw_appointment_letter-w_sal_month-fixedctc = '16,364'.
*    lw_appointment_letter-w_sal_month-variablepay = '-'.
*    lw_appointment_letter-w_sal_month-totalctc = '16,364'.
*
*    lw_appointment_letter-w_sal_annual-basic = '69,600'.
*    lw_appointment_letter-w_sal_annual-hra = '34,800'.
*    lw_appointment_letter-w_sal_annual-conveyance = '19,200'.
*    lw_appointment_letter-w_sal_annual-medical = '-'.
*    lw_appointment_letter-w_sal_annual-food = '30,000'.
*    lw_appointment_letter-w_sal_annual-special = '20,412'.
*    lw_appointment_letter-w_sal_annual-grosspay = '1,74,012'.
*    lw_appointment_letter-w_sal_annual-pfemployee = '16,704'.
*    lw_appointment_letter-w_sal_annual-esiemployee = '1,308'.
*    lw_appointment_letter-w_sal_annual-netpay = '1,56,000'.
*    lw_appointment_letter-w_sal_annual-pfemployer = '16,704'.
*    lw_appointment_letter-w_sal_annual-esiemployer = '5,652'.
*    lw_appointment_letter-w_sal_annual-fixedctc = '1,96,368'.
*    lw_appointment_letter-w_sal_annual-variablepay = '-'.
*    lw_appointment_letter-w_sal_annual-totalctc = '1,96,368'.
**ls_outpara-getpdf = abap_true.
*
**ls_outpara-nodialog  = 'X'.
*    ls_outpara-preview = abap_true.
*    CALL FUNCTION 'FP_JOB_OPEN'
*      CHANGING
*        ie_outputparams = ls_outpara
*      EXCEPTIONS
*        cancel          = 1
*        usage_error     = 2
*        system_error    = 3
*        internal_error  = 4
*        OTHERS          = 5.
*    IF sy-subrc <> 0.
*      RETURN.
*    ENDIF.
*
*    TRY.
*        CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
*          EXPORTING
*            i_name     = ls_frmname
*          IMPORTING
*            e_funcname = lv_fm.
*      CATCH cx_root.
*        RETURN.
*    ENDTRY.
*
** 3. Generate the PDF data.
*    CALL FUNCTION lv_fm
*      EXPORTING
*        /1bcdwb/docparams  = ls_docpara
*        appt_letter        = lw_appointment_letter
*      IMPORTING
*        /1bcdwb/formoutput = ls_output
*      EXCEPTIONS
*        usage_error        = 1
*        system_error       = 2
*        internal_error     = 3.
*    IF sy-subrc <> 0.
*      RETURN.
*    ENDIF.
*
*    CALL FUNCTION 'FP_JOB_CLOSE'
**   IMPORTING
**     E_RESULT      =
*      EXCEPTIONS
*        usage_error    = 1
*        system_error   = 2
*        internal_error = 3
*        OTHERS         = 4.
*  ENDMETHOD.
*
*ENDCLASS.                    "lcl_handle_events IMPLEMENTATION
*
** declare object for dragdrop object.
*DATA : go_main TYPE REF TO lcl_main.
**&---------------------------------------------------------------------*
**& Form handle_user_command
**&---------------------------------------------------------------------*
**& text
**&---------------------------------------------------------------------*
**&      --> E_SALV_FUNCTION
**&---------------------------------------------------------------------*
*FORM handle_user_command  USING i_ucomm TYPE  salv_de_function.
*  DATA answer           TYPE c.
*  CASE i_ucomm.
*    WHEN '&UPD'.
**      lo_gr_alv->refresh( ).
*      IF go_main->gt_log_report IS NOT INITIAL.
*        CALL FUNCTION 'Z_POPUP_ALV'
*          EXPORTING
*            i_start_column      = 25
*            i_start_line        = 6
*            i_end_column        = 100
*            i_end_line          = 20
*            i_title             = 'HO Admin Changes Status'
*            i_status_field_name = 'TYPE'
*            i_popup             = 'X'
*          TABLES
*            it_alv              = go_main->gt_log_report.
*      ELSE.
*        CALL FUNCTION 'POPUP_TO_CONFIRM'
*          EXPORTING
*            titlebar              = 'HO Admin Letters for Eompoyee''s Information'
*            text_question         = 'Do you want to Confirm for print'
*            text_button_1         = 'Confirm'(101)
*            text_button_2         = 'Goback'(102)
*            default_button        = '1'
*            display_cancel_button = ''
*            start_column          = 25
*            start_row             = 6
*          IMPORTING
*            answer                = answer
*          EXCEPTIONS
*            text_not_found        = 1
*            OTHERS                = 2.
*        CHECK  answer = '1' .
*        go_main->print_appt_letter( ).
**        IF r1 = abap_true." IT9011 Base Location
**          go_main->update_9011( ).
**        ENDIF.
**        IF r2 = abap_true." IT9013 Travel Allowed
**          go_main->update_9013( ).
**        ENDIF.
*        IF go_main->gt_log_report IS NOT INITIAL.
*          CALL FUNCTION 'Z_POPUP_ALV'
*            EXPORTING
*              i_start_column      = 25
*              i_start_line        = 6
*              i_end_column        = 100
*              i_end_line          = 20
*              i_title             = 'HO Admin Changes Status'
*              i_status_field_name = 'TYPE'
*              i_popup             = 'X'
*            TABLES
*              it_alv              = go_main->gt_log_report.
*          LEAVE TO SCREEN 0.
*        ENDIF.
*      ENDIF.
*    WHEN '&EDIT'.
*      lcl_salv_buddy=>set_editable( i_fieldname = 'GROSSPAY' i_salv_table = go_main->lo_gr_alv i_editable = abap_true ).
*      lcl_salv_buddy=>set_editable( i_fieldname = 'VARIABLE' i_salv_table = go_main->lo_gr_alv i_editable = abap_true ).
*    WHEN OTHERS.
*  ENDCASE.
*ENDFORM.
*
**&---------------------------------------------------------------------*
**& Module STATUS_9001 OUTPUT
**&---------------------------------------------------------------------*
**&
**&---------------------------------------------------------------------*
*MODULE status_9001 OUTPUT.
*  SET PF-STATUS 'HO_STATUS'.
*  SET TITLEBAR 'HO_TITLE'.
*  IF gt_list IS INITIAL.
*    gv_name = 'G_LETTER_TYP'.
*    gw_list-key = '1'.
*    gw_list-text = 'Appointment Letter'.
*    APPEND gw_list TO gt_list.
*
*    gw_list-key = '2'.
*    gw_list-text = 'Confirmation Letter'.
*    APPEND gw_list TO gt_list.
*  ENDIF.
*  CALL FUNCTION 'VRM_SET_VALUES'
*    EXPORTING
*      id     = gv_name
*      values = gt_list.
*
*  go_main->process_data( ).
*
*  go_main->build_alv( ).
*ENDMODULE.
**&---------------------------------------------------------------------*
**&      Module  USER_COMMAND_9001  INPUT
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
*MODULE user_command_9001 INPUT.
*  CASE sy-ucomm.
*    WHEN 'BACK' OR '%EX' OR 'RW' OR 'EXIT'.
*      LEAVE TO SCREEN 0 .
*    WHEN OTHERS.
*  ENDCASE.
*ENDMODULE.
