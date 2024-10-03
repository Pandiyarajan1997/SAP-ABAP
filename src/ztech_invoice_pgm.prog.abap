*&---------------------------------------------------------------------*
*&  Include           ZD_EINVOICE_MAIN
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&  Include           ZD_EINVOICE_MAIN
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_SELECT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_select_data .

  DATA: v_refnum  TYPE j_1ig_docno.


  IF p_rad4 IS NOT INITIAL.
    CLEAR: v_refnum.
    IF s_vbeln[]  IS INITIAL.
      MESSAGE 'Select Billing doc no' TYPE 'I' DISPLAY LIKE 'E'.
      RETURN.
    ELSE.
      SELECT docno UP TO 1 ROWS FROM j_1ig_invrefnum INTO v_refnum WHERE docno IN s_vbeln ORDER BY PRIMARY KEY.
      ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation
      IF v_refnum IS INITIAL.
        MESSAGE 'No records found for the input' TYPE 'I' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.
    ENDIF.
  ENDIF.

  "Select Invoice Header Data
  IF p_rad2 IS NOT INITIAL.   "For Cancel IRN/ Eway Bill Invoice Details

    SELECT a~vbeln "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
           a~fkart
           a~fkdat
           a~bukrs
           a~erdat
           a~kunag
           b~irn
           b~irn_status
           b~ack_no
           b~ack_date
           c~ebillno
           c~vdfmdate          "Valid From date
           c~vdfmtime          "Valid From Time
           c~status            "Eway bill Status
                FROM vbrk AS a
                INNER JOIN j_1ig_invrefnum AS b
                ON b~docno = a~vbeln AND b~irn_status = 'ACT'
                LEFT OUTER JOIN j_1ig_ewaybill AS c
                ON c~docno = a~vbeln            "
                INTO TABLE l_tab_vbrk
                WHERE a~vbeln IN s_vbeln
                 AND a~fkart IN ('YRF2','YBFS','YBDP','YBBR','IV','YSTO','YBTE','YBRE',
                                     'YIRE','YFRE','ZSIN','ZMSP','ZML2','ZL2','L2','YBEO')       "Cancelled Invoice
                AND   a~vkorg = '1400'
                AND   a~fksto = ' '
                AND   a~erdat IN s_erdat.

    IF sy-subrc = 0.

      IF s_werks IS NOT INITIAL.
        PERFORM plant_filter.
      ENDIF.


      LOOP AT l_tab_vbrk INTO l_wrk_vbrk.
        MOVE-CORRESPONDING l_wrk_vbrk TO lw_einv_final.
        IF l_wrk_vbrk-irn_status = 'ACT'.
          lw_einv_final-status = icon_led_green.
        ENDIF.

        IF l_wrk_vbrk-status = 'A'.              "E-Way Bill Status
          lw_einv_final-can_status = 'Active'.
        ELSEIF l_wrk_vbrk-status = 'C'.         "E-Way Bill Cancel Status
          lw_einv_final-can_status = 'Cancelled'.
        ENDIF.

        APPEND lw_einv_final TO lt_einv_final.
        CLEAR :lw_einv_final.
      ENDLOOP.
    ENDIF.

  ELSEIF p_rad3 IS NOT INITIAL.            "For Display Invoice/IRN Details

    SELECT a~vbeln "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
           a~fkart
           a~fkdat
           a~bukrs
           a~erdat
           a~kunag
           b~irn
           b~irn_status
           b~ack_no
           b~ack_date
           c~ebillno           "Eway bill no
           c~vdfmdate          "Valid From date
           c~vdfmtime          "Valid From Time
           c~status            "Eway bill Status
                FROM vbrk AS a
                INNER JOIN j_1ig_invrefnum AS b
                ON b~docno = a~vbeln       "AND   irn_status = 'ACT' AND b~irn_status = 'CNL'
                LEFT OUTER JOIN j_1ig_ewaybill AS c
                ON c~docno = a~vbeln
                INTO TABLE l_tab_vbrk
                WHERE a~vbeln IN s_vbeln
                AND   a~fkart IN ('YRF2','YBFS','YBDP','YBBR','IV','YSTO','YBTE','YBRE',
                                  'YIRE','YFRE','ZSIN','ZMSP','ZML2','ZL2','L2','YBEO')
               AND   a~vkorg = '1400'
                AND   a~fksto = ' '
                AND   a~erdat IN s_erdat.


    IF s_werks IS NOT INITIAL.
      PERFORM plant_filter.
    ENDIF.


    LOOP AT l_tab_vbrk INTO l_wrk_vbrk.
      MOVE-CORRESPONDING l_wrk_vbrk TO lw_einv_final.
      IF l_wrk_vbrk-irn_status = 'CNL'.
        lw_einv_final-status = icon_delete.
*          lw_einv_final-can_status = 'IRN Canceld'.

      ELSEIF l_wrk_vbrk-irn_status = 'ACT'.
        lw_einv_final-status = icon_led_green.
      ENDIF.

      IF l_wrk_vbrk-status = 'A'.         "E-Way Bill Status
        lw_einv_final-can_status = 'Active'.
        lw_einv_final-status = icon_led_green.
      ELSEIF l_wrk_vbrk-status = 'C'.
        lw_einv_final-can_status = 'Cancelled'.
        lw_einv_final-status = icon_delete.
      ENDIF.

      APPEND lw_einv_final TO lt_einv_final.
      CLEAR :lw_einv_final.
    ENDLOOP.

  ELSEIF p_rad1 IS NOT INITIAL.       "CREATE INVOICE

    SELECT a~vbeln "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
           a~fkart
           a~fkdat
           a~bukrs
           a~erdat
           a~kunag
           b~irn
           b~irn_status
           b~ack_no
           b~ack_date
           c~ebillno
                FROM vbrk AS a
*                LEFT JOIN vbrp as d
*                on d~vbeln = a~vbeln and d~werks = p_werks
                LEFT JOIN j_1ig_invrefnum AS b
                ON b~docno = a~vbeln
                LEFT OUTER JOIN j_1ig_ewaybill AS c
                ON c~docno = a~vbeln
                INTO TABLE l_tab_vbrk
                WHERE a~vbeln IN s_vbeln
                  AND a~fkart  IN ('YRF2','YBFS','YBDP','YBBR','IV','YSTO','YBTE','YBRE',
                                      'YIRE','YFRE','ZSIN','ZMSP','ZML2','ZL2','L2','YBEO')
                  AND a~vkorg = '1400'
                  AND a~fksto = ' '
                  AND a~erdat IN s_erdat.


    IF sy-subrc = 0.
*      DELETE l_tab_vbrk WHERE irn <> space.

      IF s_werks IS NOT INITIAL.
        PERFORM plant_filter.
      ENDIF.

      LOOP AT l_tab_vbrk INTO l_wrk_vbrk.
        MOVE-CORRESPONDING l_wrk_vbrk TO lw_einv_final.
        lw_einv_final-status = icon_create.
        APPEND lw_einv_final TO lt_einv_final.
        CLEAR :lw_einv_final.
      ENDLOOP.

    ENDIF.

  ELSEIF p_rad4 IS NOT INITIAL.            "For Eway Bill Create for IRN Created Invoice

    SELECT a~vbeln "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
           a~fkart
           a~fkdat
           a~bukrs
           a~erdat
           a~kunag
           b~irn
           b~irn_status
           b~ack_no
           b~ack_date
           c~ebillno
           c~vdfmdate          "Valid From date
           c~vdfmtime          "Valid From Time
           c~status
                FROM vbrk AS a
                INNER JOIN j_1ig_invrefnum AS b
                ON b~docno = a~vbeln AND b~irn_status = 'ACT'
                LEFT OUTER JOIN j_1ig_ewaybill AS c
                ON c~docno = a~vbeln          "AND c~status NE 'A'
                INTO TABLE l_tab_vbrk
                WHERE a~vbeln IN s_vbeln
                AND   a~erdat IN s_erdat
                AND   a~fksto = ' '
                AND a~vkorg = '1400'
                AND   a~fkart IN ('YRF2','YBFS','YBDP','YBBR','IV','YSTO','YBTE','YBRE',
                                   'YIRE','YFRE','ZSIN','ZMSP','ZML2','ZL2','L2','YBEO').      "Restricted Service



*****Removing the Active Eway Bill ********
    DATA: wa_ewb TYPE j_1ig_ewaybill.

    LOOP AT l_tab_vbrk INTO l_wrk_vbrk.
      SELECT * UP TO 1 ROWS FROM j_1ig_ewaybill INTO wa_ewb
                           WHERE docno = l_wrk_vbrk-vbeln AND status = 'A' ORDER BY PRIMARY KEY.
      ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation
      IF wa_ewb IS NOT INITIAL.
        DELETE l_tab_vbrk INDEX sy-tabix.
      ENDIF.
      CLEAR:wa_ewb,l_wrk_vbrk.
    ENDLOOP.

*******Cancelled E-way bill in Creation of E-waybill to Create*********
    SORT l_tab_vbrk BY vdfmdate vdfmtime DESCENDING.
    DELETE ADJACENT DUPLICATES FROM l_tab_vbrk COMPARING vbeln bukrs irn_status status.

    IF s_werks IS NOT INITIAL.
      PERFORM plant_filter.
    ENDIF.

    LOOP AT l_tab_vbrk INTO l_wrk_vbrk.
      MOVE-CORRESPONDING l_wrk_vbrk TO lw_einv_final.
      IF l_wrk_vbrk-irn_status = 'ACT'.
        lw_einv_final-status = icon_create.
      ENDIF.

      IF l_wrk_vbrk-status = 'C'.
        CLEAR: lw_einv_final-ebillno.
      ENDIF.

      APPEND lw_einv_final TO lt_einv_final.
      CLEAR :lw_einv_final.
    ENDLOOP.


  ENDIF.



ENDFORM.                    "get_select_data
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  DATA : BEGIN OF lt_fcode OCCURS 0,
           fcode LIKE rsmpe-func,
         END OF lt_fcode.

  CLEAR : lt_fcode,lt_fcode[].
  SET TITLEBAR '100' WITH 'eInvoicing Process'.
  SET PF-STATUS '100' EXCLUDING lt_fcode.

ENDMODULE.                    "status_0100 OUTPUT
*&---------------------------------------------------------------------*
*&      Module  INIT_ALV_CLASS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE init_alv_class OUTPUT.
  IF gv_custom_container IS INITIAL.
    PERFORM create_container.
    PERFORM set_grid_exclude.
    PERFORM set_grid_layout.
    PERFORM set_grid_fieldcat.
    PERFORM create_event_receiver.
    PERFORM set_build_cell.
    PERFORM display_grid .

  ELSE.
    CALL METHOD gv_grid->refresh_table_display.
  ENDIF.
ENDMODULE.                    "init_alv_class OUTPUT
*&---------------------------------------------------------------------*
*&      Form  CREATE_CONTAINER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_container .

  CREATE OBJECT gv_custom_container
    EXPORTING
      container_name              = gv_container
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5.

  CREATE OBJECT gv_grid
    EXPORTING
      i_parent      = gv_custom_container
      i_appl_events = 'X'.

ENDFORM.                    "create_container
*&---------------------------------------------------------------------*
*&      Form  SET_GRID_EXCLUDE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_grid_exclude .
  PERFORM exclude_tb_functions USING 'GT_EXCLUDE'.
ENDFORM.                    "set_grid_exclude
*&---------------------------------------------------------------------*
*&      Form  EXCLUDE_TB_FUNCTIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0128   text
*----------------------------------------------------------------------*
FORM exclude_tb_functions  USING p_tabname.


*  ### ### #### ## ### ####.
  FIELD-SYMBOLS : <table> TYPE ui_functions.

  DATA : ls_exclude   TYPE ui_func.
  DATA : l_table_name LIKE feld-name.

  CONCATENATE p_tabname '[]' INTO  l_table_name.
  ASSIGN     (l_table_name)    TO <table>.

  PERFORM append_exclude_functions
        TABLES <table>
        USING : cl_gui_alv_grid=>mc_fc_loc_undo, " ####&LOCAL&UNDO
                cl_gui_alv_grid=>mc_fc_send,
                cl_gui_alv_grid=>mc_fc_to_office,
*                cl_gui_alv_grid=>mc_fc_detail,
                cl_gui_alv_grid=>mc_fc_call_abc, " &ABC
*                cl_gui_alv_grid=>mc_fc_view_crystal,
                cl_gui_alv_grid=>mc_fc_help,
                cl_gui_alv_grid=>mc_fc_info,
                cl_gui_alv_grid=>mc_fc_loc_delete_row,    " ###.
                cl_gui_alv_grid=>mc_fc_loc_copy,          " # ##.
*                cl_gui_alv_grid=>mc_fc_html,
                cl_gui_alv_grid=>mc_fc_loc_copy_row,      " # ##.
                cl_gui_alv_grid=>mc_fc_loc_cut,           " ##.
                cl_gui_alv_grid=>mc_fc_graph,
                cl_gui_alv_grid=>mc_fc_refresh,
                cl_gui_alv_grid=>mc_fc_loc_insert_row,    " ###.
                cl_gui_alv_grid=>mc_fc_loc_move_row,
                cl_gui_alv_grid=>mc_fc_loc_append_row,    " ####.
                cl_gui_alv_grid=>mc_fc_loc_paste,         " ####.
                cl_gui_alv_grid=>mc_fc_loc_paste_new_row. " ####.



ENDFORM.                    "exclude_tb_functions
*&---------------------------------------------------------------------*
*&      Form  APPEND_EXCLUDE_FUNCTIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<TABLE>  text
*      -->P_CL_GUI_ALV_GRID=>MC_FC_LOC_UND  text
*----------------------------------------------------------------------*
FORM append_exclude_functions  TABLES p_table
                               USING p_value.

  DATA : ls_exclude TYPE ui_func.
  ls_exclude = p_value.
  APPEND ls_exclude TO p_table.
ENDFORM.                    "append_exclude_functions
*&---------------------------------------------------------------------*
*&      Form  SET_GRID_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_grid_layout .

  DATA lv_txt TYPE char100.

*  gs_layout-edit = 'X'.
  gs_layout-smalltitle  = 'X'.
  gs_layout-cwidth_opt  = 'X'.
  gs_layout-zebra    = 'X'.
*  gs_layout-no_rowins   = 'X'.
  gs_layout-edit        = 'X'.
  gs_layout-box_fname   = 'STATUS'.
  gs_layout-stylefname = 'CELLTAB'.
  gs_layout-ctab_fname = 'COLTAB'.


  IF p_rad1 IS NOT INITIAL.
    gs_layout-grid_title = 'EInvoicing Create Process'.
  ELSEIF p_rad2 IS NOT INITIAL.
    gs_layout-grid_title = 'EInvoicing Cancel Process'.
  ELSEIF p_rad3 IS NOT INITIAL.
    gs_layout-grid_title = 'EInvoicing Display'.
  ENDIF.

  gs_layout-detailtitl = 'EInvoicing Process'.
*  gs_layout- = 'EInvoicing Process'.
  gs_variant-report = sy-repid.
  gs_variant-handle = '9999'.
  gs_variant-report = '9999'.

ENDFORM.                    "set_grid_layout
*&---------------------------------------------------------------------*
*&      Form  SET_GRID_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_grid_fieldcat .
  REFRESH gt_fieldcat.  CLEAR gt_fieldcat.

*  GET REFERENCE OF gt_fieldcat INTO gv_pointer.
*  ASSIGN gv_pointer->* TO <fcat>.

  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'STATUS'.
  gs_fieldcat-tabname = 'ICON'.
  gs_fieldcat-col_pos = 0.
  gs_fieldcat-coltext = 'Check'.
*  gs_fieldcat-no_out = 'X'. " Do not Display Column
  INSERT gs_fieldcat INTO TABLE gt_fieldcat.

  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'VBELN'.
  gs_fieldcat-tabname = 'VBRK'.
  gs_fieldcat-col_pos = 1.
  gs_fieldcat-coltext = 'Invoice Number'.
*  gs_fieldcat-no_out = 'X'. " Do not Display Column
  INSERT gs_fieldcat INTO TABLE gt_fieldcat.

*
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'FKART'.
  gs_fieldcat-tabname = 'VBRK'.
  gs_fieldcat-col_pos = 2.
  gs_fieldcat-coltext = 'Doc Type'.
*  gs_fieldcat-no_out = 'X'. " Do not Display Column
  INSERT gs_fieldcat INTO TABLE gt_fieldcat.

*
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'ERDAT'.
  gs_fieldcat-tabname = 'VBRK'.
  gs_fieldcat-col_pos = 3.
  gs_fieldcat-coltext = 'Doc Date'.
*  gs_fieldcat-no_out = 'X'. " Do not Display Column
  INSERT gs_fieldcat INTO TABLE gt_fieldcat.

*
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'KUNAG'.
  gs_fieldcat-tabname = 'VBRK'.
  gs_fieldcat-col_pos = 4.
  gs_fieldcat-coltext = 'Customer Number'.
*  gs_fieldcat-no_out = 'X'. " Do not Display Column
  INSERT gs_fieldcat INTO TABLE gt_fieldcat.

  IF p_rad2 = abap_false .
    CLEAR gs_fieldcat.
    gs_fieldcat-fieldname   = 'IRN'.
    gs_fieldcat-tabname     = 'J_1IG_INVREFNUM'.
    gs_fieldcat-col_pos     = 5.
    gs_fieldcat-outputlen   = 10.
    gs_fieldcat-coltext     = 'Invoice Reference Number'.
*  gs_fieldcat-no_out = 'X'. " Do not Display Column
    INSERT gs_fieldcat INTO TABLE gt_fieldcat.
  ENDIF.

  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'ACK_NO'.
  gs_fieldcat-tabname = 'J_1IG_INVREFNUM'.
  gs_fieldcat-col_pos = 6.
  gs_fieldcat-coltext = 'Acknowledgement Number'.
*  gs_fieldcat-no_out = 'X'. " Do not Display Column
  INSERT gs_fieldcat INTO TABLE gt_fieldcat.
*
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'ACK_DATE'.
  gs_fieldcat-tabname = 'J_1IG_INVREFNUM'.
  gs_fieldcat-col_pos = 7.
  gs_fieldcat-coltext = 'Acknowledgement Date'.
*  gs_fieldcat-no_out = 'X'. " Do not Display Column
  INSERT gs_fieldcat INTO TABLE gt_fieldcat.

  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'IRN_STATUS'.
  gs_fieldcat-tabname = 'J_1IG_INVREFNUM'.
  gs_fieldcat-col_pos = 8.
  gs_fieldcat-coltext = 'IRN Status'.
*  gs_fieldcat-no_out = 'X'. " Do not Display Column
  INSERT gs_fieldcat INTO TABLE gt_fieldcat.

  IF p_rad2 IS NOT INITIAL OR p_rad3 IS NOT INITIAL OR p_rad4 IS NOT INITIAL. "Create Ewaybill / Cancel Ewaybill / Display
    CLEAR gs_fieldcat.
    gs_fieldcat-fieldname = 'EBILLNO'.
    gs_fieldcat-tabname = 'J_1IG_EWAYBILL'.
    gs_fieldcat-col_pos = 9.
    gs_fieldcat-coltext = 'E-Way Bill Number'.
*  gs_fieldcat-no_out = 'X'. " Do not Display Column
    INSERT gs_fieldcat INTO TABLE gt_fieldcat.
  ENDIF.

  IF p_rad3 = abap_true OR p_rad2 = abap_true.          "For Display / Cancel Ewaybill
    CLEAR gs_fieldcat.
    gs_fieldcat-fieldname = 'CAN_STATUS'.
*  gs_fieldcat-tabname = 'J_1IG_EWAYBILL'.
    gs_fieldcat-col_pos = 10.
    gs_fieldcat-coltext = 'Eway Bill Status'.
*  gs_fieldcat-no_out = 'X'. " Do not Display Column
    INSERT gs_fieldcat INTO TABLE gt_fieldcat.
  ENDIF.
*ENDIF.

ENDFORM.                    "set_grid_fieldcat
*&---------------------------------------------------------------------*
*&      Form  MAKE_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0341   text
*      -->P_0342   text
*      -->P_0343   text
*----------------------------------------------------------------------*
FORM make_fieldcat USING p_gub p_fname p_con.


  IF p_gub = 'S'.
    CLEAR gs_fieldcat.
  ENDIF.

  DATA lv_col(40).
  FIELD-SYMBOLS <fs>.
  CONCATENATE 'GS_FIELDCAT-' p_fname  INTO lv_col.
  ASSIGN      (lv_col)       TO       <fs>.
  MOVE         p_con         TO       <fs>.


  IF p_gub = 'E'.
    APPEND gs_fieldcat TO <fcat>.
  ENDIF.
ENDFORM.                    "make_fieldcat
*&---------------------------------------------------------------------*
*&      Form  CREATE_EVENT_RECEIVER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_event_receiver .

  CALL METHOD gv_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CREATE OBJECT gv_event_receiver
    EXPORTING
      e_object_text = 'GV_GRID'.

  SET HANDLER gv_event_receiver->handle_toolbar       FOR gv_grid.
  SET HANDLER gv_event_receiver->handle_user_command  FOR gv_grid.
  SET HANDLER gv_event_receiver->handle_data_changed  FOR gv_grid.
  SET HANDLER gv_event_receiver->handle_hotspot_click FOR gv_grid.
  SET HANDLER gv_event_receiver->handle_double_click  FOR gv_grid.
  SET HANDLER gv_event_receiver->handle_onf4          FOR gv_grid.
  SET HANDLER gv_event_receiver->handle_button_click  FOR gv_grid.


ENDFORM.                    "create_event_receiver
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_GRID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_grid .
*  BREAK-POINT.
  CALL METHOD gv_grid->set_table_for_first_display
    EXPORTING
      i_save                        = abap_true
      i_structure_name              = ''
*     is_variant                    = gs_variant
      is_layout                     = gs_layout
      it_toolbar_excluding          = gt_exclude
    CHANGING
      it_fieldcatalog               = gt_fieldcat
*     it_sort                       = gt_sort[]
      it_outtab                     = lt_einv_final[]
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.


  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    "display_grid
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.


  DATA : l_answer.

  IF sy-subrc = 0.
    CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
      EXPORTING
        defaultoption  = 'N'
        textline1      = 'Unsaved data will be lost,'
        textline2      = 'Do you want to exit?'
        titel          = 'Exit'
        cancel_display = ' '
      IMPORTING
        answer         = l_answer.
    IF l_answer = 'J'.
      LEAVE TO SCREEN 0.
    ELSE.
* Stay
    ENDIF.
  ELSE.
    LEAVE TO SCREEN 0.
  ENDIF.

ENDMODULE.                    "exit INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  save_ok = ok_code.
  CLEAR ok_code.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT' OR 'CANCEL'.
      LEAVE PROGRAM.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.                    "user_command_0100 INPUT
*&---------------------------------------------------------------------*
*& Form ADD_FUNCTION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_E_OBJECT
*&      --> P_
*&---------------------------------------------------------------------*
FORM add_function  USING p_e_object TYPE REF TO cl_alv_event_toolbar_set
                         p_ucomm.

  DATA: ls_toolbar TYPE stb_button,
        l_icon     LIKE ls_toolbar-icon,
        l_text     LIKE stb_button-text.

  CASE p_ucomm.
    WHEN 'ZNEW'.
      l_icon = icon_create.
      l_text = 'Create IRN'.
    WHEN 'CANEWY'.
      l_icon = icon_system_cancel.
      l_text = 'Cancel eWay'.
    WHEN 'CANIRN'.
      l_icon = icon_delete.
      l_text = 'Cancel IRN'.

    WHEN 'CREWAY'.
      l_icon = icon_create.
      l_text = 'Create E-Way Bill'.
  ENDCASE.



  MOVE : p_ucomm            TO ls_toolbar-function,
         l_icon             TO ls_toolbar-icon,
         l_text             TO ls_toolbar-quickinfo,
         l_text             TO ls_toolbar-text,
         ' '                TO ls_toolbar-disabled.
  APPEND ls_toolbar TO p_e_object->mt_toolbar.
  CLEAR  ls_toolbar.


ENDFORM.                    "add_function
*&---------------------------------------------------------------------*
*& Form SET_BUILD_CELL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM set_build_cell .

  DATA : lt_lvc_styl TYPE lvc_t_styl,
         lt_lvc_scol TYPE lvc_t_scol,
         ls_lvc_scol TYPE lvc_s_scol,
         l_index     TYPE i,
         l_mode      TYPE c.

  LOOP AT lt_einv_final INTO lw_einv_final.
    l_index = sy-tabix.
*    IF gt_tab-znew = 'N'.
*    l_mode = gt_tab-znew .
*    ELSE.
*      l_mode = 'D'.
*    ENDIF.

    REFRESH: lt_lvc_styl,
             lt_lvc_scol.
    PERFORM fill_celltab USING    l_mode
                         CHANGING lt_lvc_styl.


    CLEAR: lw_einv_final-celltab,
           lw_einv_final-coltab.
    INSERT LINES OF lt_lvc_styl INTO TABLE lw_einv_final-celltab.
    INSERT LINES OF lt_lvc_scol INTO TABLE lw_einv_final-coltab.

    MODIFY lt_einv_final FROM lw_einv_final.
  ENDLOOP.

ENDFORM.                    "set_build_cell
*&---------------------------------------------------------------------*
*& Form FILL_CELLTAB
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> L_MODE
*&      <-- LT_LVC_STYL
*&---------------------------------------------------------------------*
FORM fill_celltab USING    p_mode
                  CHANGING p_celltab TYPE lvc_t_styl.

  DATA : ls_celltab TYPE lvc_s_styl,
         l_mode     TYPE raw4.

  DATA : l_fieldcat LIKE lvc_s_fcat.

  IF p_mode = ' '.
    LOOP AT gt_fieldcat INTO l_fieldcat.
      ls_celltab-fieldname = l_fieldcat-fieldname.

      ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
      ls_celltab-style2 = cl_gui_alv_grid=>mc_style_disabled.
      ls_celltab-style3 = cl_gui_alv_grid=>mc_style_disabled.
      ls_celltab-style4 = cl_gui_alv_grid=>mc_style_disabled.
      INSERT ls_celltab INTO TABLE p_celltab.
    ENDLOOP.
  ENDIF.


ENDFORM.                    "fill_celltab
*&---------------------------------------------------------------------*
*&      Form  PLANT_FILTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM plant_filter .
  TYPES: BEGIN OF ty_werks,
          vbeln  TYPE vbeln,
          werks  TYPE werks_d,
         END OF ty_werks.

  DATA: gt_werks  TYPE TABLE OF ty_werks,
        gw_werks  TYPE ty_werks,
        in        TYPE sy-tabix.


  CLEAR:gt_werks,gw_werks.

********Plant Filteration***************
  IF l_tab_vbrk IS NOT INITIAL.
    REFRESH: gt_werks[].
    SELECT vbeln werks FROM vbrp INTO TABLE gt_werks
                                      FOR ALL ENTRIES IN l_tab_vbrk
                                      WHERE vbeln = l_tab_vbrk-vbeln
                                         AND werks IN s_werks.

  ENDIF.

  LOOP AT l_tab_vbrk INTO l_wrk_vbrk.
    in = sy-tabix.
    READ TABLE gt_werks INTO gw_werks WITH KEY vbeln = l_wrk_vbrk-vbeln.
    IF sy-subrc IS NOT INITIAL.
      DELETE l_tab_vbrk INDEX in.
    ENDIF.
    CLEAR:gw_werks,l_wrk_vbrk.
  ENDLOOP.
*******Plant Filteration ******************

ENDFORM.                    " PLANT_FILTER
