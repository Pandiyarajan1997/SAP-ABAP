*&---------------------------------------------------------------------*
*& Include          ZHR_POSITION_MANAGER_CHG_FORMS
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form alv_display
*&---------------------------------------------------------------------*
CLASS      lcl_handle_events DEFINITION.
  PUBLIC SECTION.
    METHODS:
      data_changed
        FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed.

    METHODS:
      handle_on_f4
        FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING e_fieldname es_row_no
                  er_event_data.
ENDCLASS.
CLASS lcl_handle_events IMPLEMENTATION.
  METHOD data_changed.
    DATA: ls_row   TYPE lvc_s_modi,
          lv_index TYPE i.
*New Reporting Manager Position
    CLEAR: ls_row.
    READ TABLE er_data_changed->mt_good_cells INTO ls_row WITH KEY fieldname = 'NEWMGRPOS'.
    IF sy-subrc = 0.
      READ TABLE gt_posdtls ASSIGNING <fs_posdtls> INDEX ls_row-row_id.
      IF sy-subrc = 0.
        <fs_posdtls>-newmgrpos = ls_row-value.
        IF <fs_posdtls>-newmgrpos IS NOT INITIAL.
          SELECT SINGLE stext FROM hrp1000
            INTO @<fs_posdtls>-newmgrpdes
            WHERE objid = @<fs_posdtls>-newmgrpos.
*New Manager Position Pernr Number
          SELECT SINGLE sobid FROM hrp1001
            INTO @DATA(l_newmgr)
            WHERE otype = 'S'
            AND objid = @<fs_posdtls>-newmgrpos
            AND plvar = '01'
            AND rsign = 'A'
            AND relat = '008'
            AND begda LE @sy-datum
            AND endda GE @sy-datum
            AND sclas = 'P'.
          IF sy-subrc EQ 0.
            <fs_posdtls>-newmgr = l_newmgr.
*Position Employee Name
            SELECT SINGLE sname FROM pa0001
              INTO @<fs_posdtls>-newmgrname
              WHERE pernr = @l_newmgr
              AND begda LE @sy-datum
              AND endda GE @sy-datum.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    lo_grid->refresh_table_display( ).
  ENDMETHOD.

  METHOD handle_on_f4.
    PERFORM handle_on_f4
     USING e_fieldname
           es_row_no
           er_event_data.
  ENDMETHOD.

ENDCLASS.
FORM alv_display .
  REFRESH: gt_fcat.
*** Change Reporting manger ****
  PERFORM f_fieldcat USING 'SEL'          1  'X'   .
  PERFORM f_fieldcat USING 'POSITION'     2  space .
  PERFORM f_fieldcat USING 'POSDESC'      3  space .
  PERFORM f_fieldcat USING 'POSEMPNO'     4  space .
  PERFORM f_fieldcat USING 'POSEMPNAME'   5  space .
  PERFORM f_fieldcat USING 'OLDREPMGR'    6  space .
  PERFORM f_fieldcat USING 'OLDMGRNAME'   7  space .
  PERFORM f_fieldcat USING 'OLDMGRPOS'    8  space .
  PERFORM f_fieldcat USING 'OLDMGRPDES'   9  space .
  PERFORM f_fieldcat USING 'BEGDA'       10  'X'   .
  PERFORM f_fieldcat USING 'NEWMGR'      13  space .
  PERFORM f_fieldcat USING 'NEWMGRNAME'  14  space .
  PERFORM f_fieldcat USING 'NEWMGRPOS'   11  'X'   .
  PERFORM f_fieldcat USING 'NEWMGRPDES'  12  space .
  PERFORM f_fieldcat USING 'TYPE'        15  space .
  PERFORM f_fieldcat USING 'MSG'         16  space .
  CALL SCREEN 100.
ENDFORM.
FORM f_fieldcat  USING f_var1 f_var2 f_var3.
  gs_fcat-fieldname = f_var1.
  gs_fcat-col_pos = f_var2.
  gs_fcat-edit = f_var3.

  CASE gs_fcat-fieldname.
    WHEN 'SEL'.
      gs_fcat-coltext = 'Select'.
      gs_fcat-checkbox = abap_true.
    WHEN 'POSITION'.
      gs_fcat-coltext = 'Position'.
    WHEN 'POSDESC'.
      gs_fcat-coltext = 'Posdesc'.
    WHEN 'POSEMPNO'.
      gs_fcat-coltext = 'Posempno'.
    WHEN 'POSEMPNAME'.
      gs_fcat-coltext = 'Posempname'.
    WHEN 'OLDREPMGR'.
      gs_fcat-coltext = 'Oldrepmgr'.
    WHEN 'OLDMGRNAME'.
      gs_fcat-coltext = 'Oldmgrname'.
    WHEN 'OLDMGRPOS'.
      gs_fcat-coltext = 'Oldmgrpos'.
    WHEN 'OLDMGRPDES'.
      gs_fcat-coltext = 'Oldmgrpdes'.
    WHEN 'BEGDA'.
      gs_fcat-coltext = 'Start Date'.
      gs_fcat-f4availabl = abap_true.
      gs_fcat-ref_table = 'SYST'.
      gs_fcat-ref_field = 'DATUM'.
    WHEN 'NEWMGR'.
      gs_fcat-coltext = 'Newmgr'.
    WHEN 'NEWMGRNAME'.
      gs_fcat-coltext = 'Newmgrname'.
    WHEN 'NEWMGRPOS'.
      gs_fcat-coltext = 'Newmgrpos'.
      gs_fcat-f4availabl = abap_true.
    WHEN 'NEWMGRPDES'.
      gs_fcat-coltext = 'Newmgrpdes'.
    WHEN 'TYPE'.
      gs_fcat-coltext = 'Type'.
    WHEN 'MSG'.
      gs_fcat-coltext = 'Message'.
  ENDCASE.


  APPEND gs_fcat TO gt_fcat.
  CLEAR gs_fcat.
ENDFORM.
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'CHANGE_REPMAN'.
  SET TITLEBAR 'ZCHANGE_REP'.
  DATA ls_exclude TYPE ui_func.
  DATA: lt_exclude TYPE ui_functions.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_delete_row.
  APPEND ls_exclude TO lt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.
  APPEND ls_exclude TO lt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  APPEND ls_exclude TO lt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy.
  APPEND ls_exclude TO lt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  APPEND ls_exclude TO lt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_subtot.
  APPEND ls_exclude TO lt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_cut.
  APPEND ls_exclude TO lt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste.
  APPEND ls_exclude TO lt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
  APPEND ls_exclude TO lt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_undo.
  APPEND ls_exclude TO lt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_check.
  APPEND ls_exclude TO lt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_sum.
  APPEND ls_exclude TO lt_exclude.

  IF lo_grid IS INITIAL.
    CREATE OBJECT lo_grid
      EXPORTING
        i_parent = cl_gui_container=>default_screen.

    CREATE OBJECT lo_events.

    SET HANDLER: lo_events->data_changed FOR lo_grid.
    SET HANDLER: lo_events->handle_on_f4 FOR lo_grid.

    gs_layout-zebra = 'X'.
    gs_layout-col_opt = 'X'.
*    gs_layout-no_toolbar = 'X'.
    CALL METHOD lo_grid->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout
        it_toolbar_excluding = lt_exclude
      CHANGING
        it_outtab            = gt_posdtls
        it_fieldcatalog      = gt_fcat.

** Register for EDIT Fields and Events
    CALL METHOD lo_grid->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.

    CALL METHOD lo_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified
      EXCEPTIONS
        error      = 1
        OTHERS     = 2.

    DATA: lt_f4 TYPE lvc_t_f4 WITH HEADER LINE.
    REFRESH lt_f4.
    CLEAR lt_f4.
    lt_f4-fieldname = 'NEWMGRPOS'.
    lt_f4-register  = 'X'.
    INSERT TABLE lt_f4.

    CALL METHOD lo_grid->register_f4_for_fields
      EXPORTING
        it_f4 = lt_f4[].

  ELSE.
    CALL METHOD lo_grid->refresh_table_display( ).
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.

    WHEN 'BACK' OR 'CANC'.
      LEAVE TO SCREEN 0.

    WHEN 'SEL_ALL'.
      LOOP AT gt_posdtls ASSIGNING <fs_posdtls>.
        <fs_posdtls>-sel = 'X'.
        MODIFY gt_posdtls FROM <fs_posdtls>.
      ENDLOOP.
      CALL METHOD lo_grid->refresh_table_display( ).

    WHEN 'DESEL_ALL'.
      LOOP AT gt_posdtls ASSIGNING <fs_posdtls>.
        <fs_posdtls>-sel = ''.
        MODIFY gt_posdtls FROM <fs_posdtls>.
      ENDLOOP.
      CALL METHOD lo_grid->refresh_table_display( ).

    WHEN 'CHANGE'.
      LOOP AT gt_posdtls ASSIGNING <fs_posdtls> WHERE sel = 'X'.
        IF <fs_posdtls>-begda IS INITIAL.
          <fs_posdtls>-type = 'E'.
          <fs_posdtls>-msg  = TEXT-011.
        ELSEIF <fs_posdtls>-newmgrpos IS INITIAL.
          <fs_posdtls>-type = 'E'.
          <fs_posdtls>-msg  = TEXT-012.
        ELSE.
          IF <fs_posdtls>-begda GE sy-datum.
            <fs_posdtls>-type =  'E'.
            <fs_posdtls>-msg  = |Future Start Date is Not allowed|.
            CONTINUE.
          ENDIF.
          PERFORM f_change_reporting_mager USING <fs_posdtls>.
          <fs_posdtls>-type = <fs_posdtls>-type.
          <fs_posdtls>-msg  = <fs_posdtls>-msg.
        ENDIF.
      ENDLOOP.
      CALL METHOD lo_grid->refresh_table_display( ).
  ENDCASE.
ENDMODULE.
FORM f_change_reporting_mager USING ps_chg_rep TYPE ty_posmgrchg.
  DATA ls_p1000 TYPE p1000.
  DATA ls_p1001 TYPE p1001.

  DATA lt_p1000 TYPE TABLE OF p1000.
  DATA lt_p1001 TYPE TABLE OF p1001.

  DATA: lt_enque_tab TYPE TABLE OF ppenq,
        ls_enque_tab TYPE ppenq.
  CONSTANTS: act_vtask LIKE hrrhap-vtask VALUE 'B'.

  CLEAR ls_p1001.
  ls_p1001-objid = ps_chg_rep-position.
  ls_p1001-otype = gc_otype_s.
  ls_p1001-istat = '1'.
  ls_p1001-plvar = gc_plan_ver.
  ls_p1001-infty = '1001'.
  ls_p1001-rsign = gc_rsign_a.
  ls_p1001-relat = gc_relat_002.
  ls_p1001-prozt = 0.
  ls_p1001-begda = ps_chg_rep-begda.
  ls_p1001-endda = gc_high_date.
  ls_p1001-sclas = gc_otype_s.
  ls_p1001-sobid = ps_chg_rep-newmgrpos.
  APPEND ls_p1001 TO lt_p1001.

  CLEAR ls_enque_tab.
  REFRESH: lt_enque_tab.

  ls_enque_tab-plvar = ls_p1001-plvar.
  ls_enque_tab-otype = ls_p1001-sclas.
  ls_enque_tab-objid = ps_chg_rep-newmgrpos+0(8).
  APPEND ls_enque_tab TO lt_enque_tab.

  CALL FUNCTION 'RH_ENQUEUE_LIST'
    TABLES
      enq_tab = lt_enque_tab
    EXCEPTIONS
      OTHERS  = 1.
  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty
            NUMBER sy-msgno
            INTO ps_chg_rep-msg
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.
  CALL FUNCTION 'RH_INSERT_INFTY'
    EXPORTING
      fcode               = 'INSE'
      vtask               = act_vtask
    TABLES
      innnn               = lt_p1001
    EXCEPTIONS
      corr_exit           = 01
      no_authorization    = 08
      error_during_insert = 04
      OTHERS              = 04.

  IF sy-subrc EQ 4.
    CALL FUNCTION 'RH_CLEAR_BUFFER'.
    CALL FUNCTION 'RH_CLEAR_PLOG_TAB'.
    CALL FUNCTION 'RH_DEQUEUE_LIST'
      TABLES
        deq_tab = lt_enque_tab.
    CALL FUNCTION 'RH_CLEAR_BUFFER'.
    CALL FUNCTION 'RH_CLEAR_PLOG_TAB'.
    CALL FUNCTION 'DEQUEUE_ALL'.
    ps_chg_rep-type = 'E'.
    ps_chg_rep-msg = 'Error during creating the relationship record'.
    EXIT.
  ELSEIF sy-subrc EQ 8.
    CALL FUNCTION 'RH_CLEAR_BUFFER'.
    CALL FUNCTION 'RH_CLEAR_PLOG_TAB'.
    CALL FUNCTION 'RH_DEQUEUE_LIST'
      TABLES
        deq_tab = lt_enque_tab.
    CALL FUNCTION 'RH_CLEAR_BUFFER'.
    CALL FUNCTION 'RH_CLEAR_PLOG_TAB'.
    CALL FUNCTION 'DEQUEUE_ALL'.
    ps_chg_rep-type = 'E'.
    ps_chg_rep-msg = 'No Authorization to Create Relationship record'.
    EXIT.
  ELSEIF sy-subrc EQ 1.
    CALL FUNCTION 'RH_CLEAR_BUFFER'.
    CALL FUNCTION 'RH_CLEAR_PLOG_TAB'.
    CALL FUNCTION 'RH_DEQUEUE_LIST'
      TABLES
        deq_tab = lt_enque_tab.
    CALL FUNCTION 'RH_CLEAR_BUFFER'.
    CALL FUNCTION 'RH_CLEAR_PLOG_TAB'.
    CALL FUNCTION 'DEQUEUE_ALL'.
    ps_chg_rep-type = 'E'.
    ps_chg_rep-msg = 'Error during Saving the Relationship Record'.
    EXIT.
  ENDIF.

  CALL FUNCTION 'RH_UPDATE_DATABASE'
    EXPORTING
      vtask     = 'D'
    EXCEPTIONS
      corr_exit = 1
      OTHERS    = 2.

  IF sy-subrc GT 0.
    CALL FUNCTION 'RH_CLEAR_BUFFER'.
    CALL FUNCTION 'RH_CLEAR_PLOG_TAB'.
    ps_chg_rep-type = 'E'.
    ps_chg_rep-msg = 'Error during updating the database'.
  ELSE.
    ps_chg_rep-type = 'S'.
    ps_chg_rep-msg = 'Reporting Manager Changed Successfully'.
  ENDIF.

  CALL FUNCTION 'RH_DEQUEUE_LIST'
    TABLES
      deq_tab = lt_enque_tab.
  CALL FUNCTION 'RH_CLEAR_BUFFER'.
  CALL FUNCTION 'RH_CLEAR_PLOG_TAB'.
  CALL FUNCTION 'DEQUEUE_ALL'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form handle_on_f4
*&---------------------------------------------------------------------*
FORM handle_on_f4  USING    p_e_fieldname TYPE lvc_fname
                            p_es_row_no  TYPE lvc_s_roid
                            p_er_event_data TYPE REF TO cl_alv_event_data.
  DATA: lt_map    TYPE TABLE OF dselc,
        ls_map    TYPE dselc,
        lt_return TYPE TABLE OF ddshretval,
        ls_return TYPE ddshretval,
        ls_stable TYPE lvc_s_stbl.
  DATA: lt_domain  TYPE TABLE OF dd07v.
  DATA: ls_selec TYPE objec,
        lt_selec TYPE TABLE OF objec,
        ls_plvar TYPE plogi-plvar,
        ls_otype TYPE plogi-otype.
  REFRESH : lt_return.
  CASE p_e_fieldname.
    WHEN 'NEWMGRPOS'.
      READ TABLE gt_posdtls ASSIGNING <fs_posdtls> INDEX p_es_row_no-row_id.
      IF sy-subrc EQ 0.
        CALL FUNCTION 'RH_OBJID_REQUEST'
          EXPORTING
            plvar           = '01'
            otype           = 'S'
            dynpro_repid    = sy-repid
            dynpro_dynnr    = sy-dynnr
          IMPORTING
            sel_plvar       = ls_plvar
            sel_otype       = ls_otype
            sel_object      = ls_selec
          TABLES
            sel_objects     = lt_selec
          EXCEPTIONS
            cancelled       = 1
            wrong_condition = 2
            nothing_found   = 3
            internal_error  = 4
            illegal_mode    = 5
            OTHERS          = 6.
        <fs_posdtls>-newmgrpos = ls_selec-objid.  "Reporting Manager Position
        SELECT SINGLE mc_stext FROM hrp1000
          INTO @<fs_posdtls>-newmgrpdes
          WHERE objid = @<fs_posdtls>-newmgrpos. "Reporting Manager Position Desc
*New Manager Position Pernr Number
        SELECT SINGLE sobid FROM hrp1001
          INTO @DATA(l_newmgr)
          WHERE otype = 'S'
          AND objid = @<fs_posdtls>-newmgrpos
          AND plvar = '01'
          AND rsign = 'A'
          AND relat = '008'
          AND begda LE @sy-datum
          AND endda GE @sy-datum
          AND sclas = 'P'.
        IF sy-subrc EQ 0.
          <fs_posdtls>-newmgr = l_newmgr.
*Position Employee Name
          SELECT SINGLE sname FROM pa0001
            INTO @<fs_posdtls>-newmgrname
            WHERE pernr = @l_newmgr
            AND begda LE @sy-datum
            AND endda GE @sy-datum.
        ENDIF.
      ENDIF.
  ENDCASE.
  ls_stable = 'XX'. " set stable refresh for row and column
  " alv refresh
  CALL METHOD lo_grid->refresh_table_display
    EXPORTING
      is_stable      = ls_stable
      i_soft_refresh = 'X'
    EXCEPTIONS
      finished       = 1
      OTHERS         = 2.
  p_er_event_data->m_event_handled = 'X'.
ENDFORM.
