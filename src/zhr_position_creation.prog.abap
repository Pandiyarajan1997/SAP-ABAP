*&---------------------------------------------------------------------*
*& Report ZHR_POSITION_CREATION
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zhr_position_creation.

**   Data Declarations  **
INCLUDE zhr_position_creation_top.
**   Selection Screen Design   **
INCLUDE zhr_position_creation_sel.
** Include for all subroutine **
INCLUDE zhr_pos_creation_subroutine.

INITIALIZATION.

  IF sy-tcode EQ 'ZHR_POS_PROP'.
    LOOP AT SCREEN.
      IF screen-name ='P_RAD1' OR screen-name ='P_RAD7'.
        AUTHORITY-CHECK OBJECT 'ZPOSCHECK'
          ID 'ACTVT' FIELD '01'
          ID 'ZOPTION' FIELD 'C'.
        IF sy-subrc NE 0.
          screen-input = 0.
          screen-output = 1.
          screen-active = 1.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

** Selection Screen Adjustments **
AT SELECTION-SCREEN OUTPUT.
  PERFORM f_screen_adjust.

** Value request for selection screen **
AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_orgid-low.
  PERFORM f4_valuehelp USING 'S_ORGID-LOW'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_orgid-high.
  PERFORM f4_valuehelp USING 'S_ORGID-HIGH'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_jobid-low.
  PERFORM f4_valuehelp USING 'S_JOBID-LOW'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_jobid-high.
  PERFORM f4_valuehelp USING 'S_JOBID-HIGH'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_repmg-low.
  PERFORM f4_valuehelp USING 'S_REPMG-LOW'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_repmg-high.
  PERFORM f4_valuehelp USING 'S_REPMG-HIGH'.

** Editable Fields for Propose Screen  **
CLASS lcl_handle_events IMPLEMENTATION.
  METHOD data_changed.
    DATA: ls_row   TYPE lvc_s_modi,
          lv_index TYPE i.
** Editable field for Orgunit_ID  **
    CLEAR:gs_display, ls_row.
    READ TABLE er_data_changed->mt_good_cells INTO ls_row WITH KEY fieldname = 'ORGUNIT_ID'.
    IF sy-subrc = 0.
      CLEAR gs_display.
      READ  TABLE gt_display INTO gs_display INDEX ls_row-row_id.
      IF sy-subrc = 0.
        gs_display-orgunit_id = ls_row-value.
        IF gs_display-orgunit_id IS NOT INITIAL.
          SELECT SINGLE stext INTO gs_display-orgunit_des FROM hrp1000
                               WHERE otype = 'O'
                               AND objid = gs_display-orgunit_id
                               AND begda LE sy-datum
                               AND endda GE sy-datum.
        ENDIF.
        MODIFY gt_display FROM gs_display INDEX ls_row-row_id.
      ENDIF.
      lo_grid->refresh_table_display( ).
    ENDIF.
** Editable field for POSITION_SHORT DEsc **
    CLEAR:gs_display, ls_row.
    READ TABLE er_data_changed->mt_good_cells INTO ls_row WITH KEY fieldname = 'POS_SDES'.
    IF sy-subrc = 0.
      CLEAR gs_display.
      READ  TABLE gt_display INTO gs_display INDEX ls_row-row_id.
      IF sy-subrc = 0.
        gs_display-pos_sdes = ls_row-value.
        MODIFY gt_display FROM gs_display INDEX ls_row-row_id.
      ENDIF.
      lo_grid->refresh_table_display( ).
    ENDIF.
** Editable field for Position Long Desc **
    CLEAR:gs_display, ls_row.
    READ TABLE er_data_changed->mt_good_cells INTO ls_row WITH KEY fieldname = 'POS_LDES'.
    IF sy-subrc = 0.
      CLEAR gs_display.
      READ  TABLE gt_display INTO gs_display INDEX ls_row-row_id.
      IF sy-subrc = 0.
        gs_display-pos_ldes = ls_row-value.
        MODIFY gt_display FROM gs_display INDEX ls_row-row_id.
      ENDIF.
      lo_grid->refresh_table_display( ).
    ENDIF.

** Editable field for REPMANAGER_POS  **
    CLEAR:gs_display, ls_row.
    READ TABLE er_data_changed->mt_good_cells INTO ls_row WITH KEY fieldname = 'REPMANAGER_POS'.
    IF sy-subrc = 0.
      CLEAR gs_display.
      READ  TABLE gt_display INTO gs_display INDEX ls_row-row_id.
      IF sy-subrc = 0.
        gs_display-repmanager_pos = ls_row-value.
        IF gs_display-repmanager_pos IS NOT INITIAL.
          SELECT SINGLE stext INTO gs_display-repmng_pdes FROM hrp1000
                                WHERE plvar = '01'
                                AND otype = 'S'
                                AND objid = gs_display-repmanager_pos
                                AND begda LE sy-datum
                                AND endda GE sy-datum.
          READ TABLE gt_position INTO gs_position WITH KEY objid = gs_display-repmanager_pos.
          IF sy-subrc EQ 0.
            CLEAR: gs_display-rep_manager,gs_display-repmng_name.
            READ TABLE gt_name INTO gs_name WITH KEY plans = gs_display-repmanager_pos.
            IF sy-subrc EQ 0.
              gs_display-rep_manager = gs_name-pernr.
              gs_display-repmng_name = gs_name-ename.
            ENDIF.
          ENDIF.
        ENDIF.
        MODIFY gt_display FROM gs_display INDEX ls_row-row_id.
      ENDIF.
      lo_grid->refresh_table_display( ).
    ENDIF.
** Editable field for Job_ID  **
    CLEAR:gs_display, ls_row.
    READ TABLE er_data_changed->mt_good_cells INTO ls_row WITH KEY fieldname = 'STELL'.
    IF sy-subrc = 0.
      CLEAR gs_display.
      READ  TABLE gt_display INTO gs_display INDEX ls_row-row_id.
      IF sy-subrc = 0.
        gs_display-stell = ls_row-value.
        IF gs_display-stell IS NOT INITIAL.
          SELECT SINGLE stext INTO gs_display-job_des FROM hrp1000
                                WHERE plvar = '01'
                                AND otype = 'C'
                                AND objid = gs_display-stell
                                AND begda LE sy-datum
                                AND endda GE sy-datum.
        ENDIF.
        MODIFY gt_display FROM gs_display INDEX ls_row-row_id.
      ENDIF.
      lo_grid->refresh_table_display( ).
    ENDIF.
** Editable start Date field **
    CLEAR:gs_display, ls_row.
    READ TABLE er_data_changed->mt_good_cells INTO ls_row WITH KEY fieldname = 'START_DATE'.
    IF sy-subrc = 0.
      CLEAR gs_display.
      READ  TABLE gt_display INTO gs_display INDEX ls_row-row_id.
      IF sy-subrc = 0.
        gs_display-start_date = ls_row-value.
        MODIFY gt_display FROM gs_display INDEX ls_row-row_id.
      ENDIF.
      lo_grid->refresh_table_display( ).
    ENDIF.
**** latitude and longitude and travel allowed and travel category ***
    CLEAR:gs_display, ls_row.
    READ TABLE er_data_changed->mt_good_cells INTO ls_row WITH KEY fieldname = 'LATITUDE'.
    IF sy-subrc = 0.
      CLEAR gs_display.
      READ  TABLE gt_display INTO gs_display INDEX ls_row-row_id.
      IF sy-subrc = 0.
        gs_display-latitude = ls_row-value.
        MODIFY gt_display FROM gs_display INDEX ls_row-row_id.
      ENDIF.
      lo_grid->refresh_table_display( ).
    ENDIF.
************************************************************************************************
    CLEAR:gs_display, ls_row.
    READ TABLE er_data_changed->mt_good_cells INTO ls_row WITH KEY fieldname = 'LONGITUDE'.
    IF sy-subrc = 0.
      CLEAR gs_display.
      READ  TABLE gt_display INTO gs_display INDEX ls_row-row_id.
      IF sy-subrc = 0.
        gs_display-longitude = ls_row-value.
        MODIFY gt_display FROM gs_display INDEX ls_row-row_id.
      ENDIF.
      lo_grid->refresh_table_display( ).
    ENDIF.
************************************************************************************************
    CLEAR:gs_display, ls_row.
    READ TABLE er_data_changed->mt_good_cells INTO ls_row WITH KEY fieldname = 'TRAVEL'.
    IF sy-subrc = 0.
      CLEAR gs_display.
      READ  TABLE gt_display INTO gs_display INDEX ls_row-row_id.
      IF sy-subrc = 0.
        gs_display-travel = ls_row-value.
        MODIFY gt_display FROM gs_display INDEX ls_row-row_id.
      ENDIF.
      lo_grid->refresh_table_display( ).
    ENDIF.
**************************************************************************************************
    CLEAR:gs_display, ls_row.
    READ TABLE er_data_changed->mt_good_cells INTO ls_row WITH KEY fieldname = 'CATEGORY'.
    IF sy-subrc = 0.
      CLEAR gs_display.
      READ  TABLE gt_display INTO gs_display INDEX ls_row-row_id.
      IF sy-subrc = 0.
        gs_display-category = ls_row-value.
        MODIFY gt_display FROM gs_display INDEX ls_row-row_id.
      ENDIF.
      lo_grid->refresh_table_display( ).
    ENDIF.
**************************************************************************************************
    CLEAR:gs_display, ls_row.
    READ TABLE er_data_changed->mt_good_cells INTO ls_row WITH KEY fieldname = 'COSTCENTER'.
    IF sy-subrc = 0.
      CLEAR gs_display.
      READ  TABLE gt_display INTO gs_display INDEX ls_row-row_id.
      IF sy-subrc = 0.
        gs_display-costcenter = ls_row-value.
        MODIFY gt_display FROM gs_display INDEX ls_row-row_id.
      ENDIF.
      lo_grid->refresh_table_display( ).
    ENDIF.
** Editable Date field **
    CLEAR: ls_row, gs_chg_rep.
    READ TABLE er_data_changed->mt_good_cells INTO ls_row WITH KEY fieldname = 'DATE'.
    IF sy-subrc = 0.
      CLEAR gs_chg_rep.
      READ  TABLE gt_chg_rep INTO gs_chg_rep INDEX ls_row-row_id.
      IF sy-subrc = 0.
        gs_chg_rep-date = ls_row-value.
        MODIFY gt_chg_rep FROM gs_chg_rep INDEX ls_row-row_id.
      ENDIF.
      lo_grid->refresh_table_display( ).
    ENDIF.
** Editable New Reporting manageer field **
    CLEAR: ls_row,gs_chg_rep.
    READ TABLE er_data_changed->mt_good_cells INTO ls_row WITH KEY fieldname = 'NEWREP'.
    IF sy-subrc = 0.
      CLEAR gs_chg_rep.
      READ  TABLE gt_chg_rep INTO gs_chg_rep INDEX ls_row-row_id.
      IF sy-subrc = 0.
        gs_chg_rep-newrep = ls_row-value.
        READ TABLE gt_name ASSIGNING FIELD-SYMBOL(<fls_name>) WITH KEY pernr = gs_chg_rep-newrep.
        IF sy-subrc = 0.
          gs_chg_rep-newrepname = <fls_name>-ename.
          gs_chg_rep-newrepos = <fls_name>-plans.
        ELSE.
          MESSAGE 'Incorrect manager employee Number' TYPE 'E'.
        ENDIF.
        MODIFY gt_chg_rep FROM gs_chg_rep INDEX ls_row-row_id.
      ENDIF.
      lo_grid->refresh_table_display( ).
    ENDIF.
  ENDMETHOD.
  METHOD handle_on_f4.
    PERFORM handle_on_f4
     USING e_fieldname
           es_row_no
           er_event_data.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
** Initial Selection of Data **
  PERFORM f_initial_selections.
** Status Description **
  PERFORM f_get_status_des.

** Create Position and Propose Screen
  IF p_rad1 IS NOT INITIAL AND p_pos IS NOT INITIAL.
    DO p_pos TIMES.
      APPEND gs_display TO gt_display.
    ENDDO.
** ALV Display For Create and Propose  Screen **
    PERFORM display_alv.
  ENDIF.

** Display Table Data **
  IF p_rad5 IS NOT INITIAL OR p_rad4 IS NOT INITIAL.
    REFRESH gt_display.
    SELECT * FROM zhr_position_tab
             INTO CORRESPONDING FIELDS OF TABLE gt_display
             WHERE orgunit_id IN s_orgid
             AND stell IN s_jobid
             AND start_date IN s_begda
             AND repmanager_pos IN s_repmg.

    LOOP AT gt_display INTO gs_display.
      CLEAR ls_domain.
      READ TABLE lt_domain INTO ls_domain WITH KEY domvalue_l = gs_display-status.
      IF sy-subrc EQ 0.
        gs_display-status_desc = ls_domain-ddtext.
      ENDIF.
** Orgunit Description ***
      SELECT SINGLE stext INTO gs_display-orgunit_des FROM hrp1000
                        WHERE plvar = '01'
                        AND otype = 'O'
                        AND objid = gs_display-orgunit_id
                        AND begda LE sy-datum
                        AND endda GE sy-datum.

**Rreporting Manager Position Description **
      SELECT SINGLE stext INTO gs_display-repmng_pdes FROM hrp1000
                        WHERE plvar = '01'
                        AND otype = 'S'
                        AND objid = gs_display-repmanager_pos
                        AND begda LE sy-datum
                        AND endda GE sy-datum.
** Job ID Description **
      SELECT SINGLE stext INTO gs_display-job_des FROM hrp1000
                        WHERE plvar = '01'
                        AND otype = 'C'
                        AND objid = gs_display-stell
                        AND begda LE sy-datum
                        AND endda GE sy-datum.
** Rep Manager Name **
      CLEAR gs_name.
      READ TABLE gt_name INTO gs_name WITH KEY pernr = gs_display-rep_manager.
      IF sy-subrc EQ 0.
        gs_display-repmng_name = gs_name-ename.
      ENDIF.

      MODIFY gt_display FROM gs_display.
      CLEAR: gs_display.

    ENDLOOP.

    PERFORM display_alv.
  ENDIF.

** Approve Screen  **
  IF p_rad3 IS NOT INITIAL.
    REFRESH: gt_display.
    SELECT * FROM zhr_position_tab
             INTO CORRESPONDING FIELDS OF TABLE gt_display
             WHERE status = '02'.
    LOOP AT gt_display INTO gs_display.
      CLEAR ls_domain.
      READ TABLE lt_domain INTO ls_domain WITH KEY domvalue_l = gs_display-status.
      IF sy-subrc EQ 0.
        gs_display-status_desc = ls_domain-ddtext.
      ENDIF.
** Orgunit Description ***
      SELECT SINGLE stext INTO gs_display-orgunit_des FROM hrp1000
                        WHERE plvar = '01'
                        AND otype = 'O'
                        AND objid = gs_display-orgunit_id
                        AND begda LE sy-datum
                        AND endda GE sy-datum.

**Rreporting Manager Position Description **
      SELECT SINGLE stext INTO gs_display-repmng_pdes FROM hrp1000
                        WHERE plvar = '01'
                        AND otype = 'S'
                        AND objid = gs_display-repmanager_pos
                        AND begda LE sy-datum
                        AND endda GE sy-datum.
** Job ID Description **
      SELECT SINGLE stext INTO gs_display-job_des FROM hrp1000
                        WHERE plvar = '01'
                        AND otype = 'C'
                        AND objid = gs_display-stell
                        AND begda LE sy-datum
                        AND endda GE sy-datum.
** Rep Manager Name **
      CLEAR gs_name.
      READ TABLE gt_name INTO gs_name WITH KEY pernr = gs_display-rep_manager.
      IF sy-subrc EQ 0.
        gs_display-repmng_name = gs_name-ename.
      ENDIF.

      MODIFY gt_display FROM gs_display.
      CLEAR: gs_display.

    ENDLOOP.
** ALV Display For Approve Screen **
    PERFORM display_alv.
  ENDIF.

*** Change Reporting Manager **
  IF p_rad2 = abap_true.
    REFRESH: gt_chg_rep,gt_pernr.
*** getting the pernr given in input**
    SELECT pernr FROM pa0001 INTO TABLE gt_pernr
                 WHERE pernr IN s_repman
                   AND begda LE sy-datum
                   AND endda GE sy-datum.
    IF sy-subrc = 0.
      SORT gt_pernr[] BY pernr.
    ENDIF.
*** perform to get employee details ***
*    LOOP AT lt_pernr ASSIGNING FIELD-SYMBOL(<fls_pernr>).
    PERFORM fm_to_get_empdetails.
*    ENDLOOP.
**** Final alv display ****
    PERFORM display_alv.
  ENDIF.
*added by: Samsudeen M on 26.05.2023
  IF p_rad6 IS NOT INITIAL.
    "Calling another Program for Position Based Reporting Manager Change
    CALL TRANSACTION 'ZHR_POSREPNMGR_CHG'.
  ENDIF.

*Added by Samsudeen on 21.06.2023
  IF p_rad7 IS NOT INITIAL.
    "Copy Position details to New Position
    PERFORM copy_pos_to_pos.
    "ALV Display
    PERFORM display_alv.
  ENDIF.
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  SET PF-STATUS 'ZPOS_PROP'.
  SET TITLEBAR 'ZPROP'.

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
*    gs_layout-no_toolbar = 'X'.
    CALL METHOD lo_grid->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout
        it_toolbar_excluding = lt_exclude
      CHANGING
        it_outtab            = gt_display
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
    lt_f4-fieldname = 'ORGUNIT_ID'.
    lt_f4-register  = 'X'.
    INSERT TABLE lt_f4.

    CLEAR lt_f4.
    lt_f4-fieldname = 'POSITION_ID'.
    lt_f4-register = 'X'.
    INSERT TABLE lt_f4.

    CLEAR lt_f4.
    lt_f4-fieldname = 'REPMANAGER_POS'.
    lt_f4-register = 'X'.
    INSERT TABLE lt_f4.

    CLEAR lt_f4.
    lt_f4-fieldname = 'STELL'.
    lt_f4-register = 'X'.
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

  CONSTANTS: c_check TYPE char01 VALUE 'X'.
  DATA: lt_resulttab TYPE TABLE OF swhactor.
  DATA: ls_resulttab TYPE swhactor.

  DATA: lt_orgcomp TYPE TABLE OF zhr_pos_orgcomp,
        ls_orgcomp TYPE zhr_pos_orgcomp.

  SELECT * FROM zhr_pos_orgcomp INTO TABLE lt_orgcomp.

*get the Sales department type for orgunit
  SELECT * from hrp9010 INTO TABLE @DATA(lt_hrp9010).

  CASE sy-ucomm.

    WHEN 'BACK' OR 'CANC'.
      LEAVE TO SCREEN 0.

    WHEN 'SEL_ALL'.
*** To Select all the check box **
      LOOP AT gt_display ASSIGNING FIELD-SYMBOL(<lgs_display>).
        <lgs_display>-sel = 'X'.
        MODIFY gt_display FROM <lgs_display>.
      ENDLOOP.
      CALL METHOD lo_grid->refresh_table_display( ).

    WHEN 'DESEL_ALL'.
*** To Deselect all the check box **
      LOOP AT gt_display ASSIGNING FIELD-SYMBOL(<lgs_display1>).
        <lgs_display1>-sel = ''.
        MODIFY gt_display FROM <lgs_display1>.
      ENDLOOP.
      CALL METHOD lo_grid->refresh_table_display( ).

    WHEN 'PROP'.
      LOOP AT gt_display INTO gs_display WHERE sel = 'X'.

        IF gs_display-orgunit_id IS INITIAL.
          MESSAGE TEXT-101 TYPE 'E'.

        ELSEIF gs_display-bukrs IS INITIAL.
          MESSAGE TEXT-114 TYPE 'E'.

        ELSEIF gs_display-start_date IS INITIAL.
          MESSAGE TEXT-102 TYPE 'E'.

        ELSEIF gs_display-pos_sdes IS INITIAL AND gs_display-pos_ldes IS INITIAL.
          MESSAGE TEXT-103 TYPE 'E'.

        ELSEIF gs_display-stell IS INITIAL.
          MESSAGE TEXT-105 TYPE 'E'.

        ELSEIF gs_display-repmanager_pos IS INITIAL.
          MESSAGE TEXT-104 TYPE 'E'.

        ELSEIF gs_display-orgunit_id IS NOT INITIAL.
          READ TABLE gt_orgunit INTO gs_orgunit WITH KEY objid = gs_display-orgunit_id.
          IF sy-subrc NE 0.
            MESSAGE TEXT-106 TYPE 'E'.
          ENDIF.

          IF lt_orgcomp[] IS INITIAL.
            MESSAGE TEXT-116 TYPE 'E'.
          ENDIF.

          REFRESH: lt_resulttab.
          CALL FUNCTION 'RH_STRUC_GET'
            EXPORTING
              act_otype      = 'O'
              act_objid      = gs_display-orgunit_id
              act_wegid      = 'O-O'
*             ACT_INT_FLAG   =
*             ACT_PLVAR      = ' '
*             ACT_BEGDA      = SY-DATUM
*             ACT_ENDDA      = SY-DATUM
*             ACT_TDEPTH     = 0
*             ACT_TFLAG      = 'X'
*             ACT_VFLAG      = 'X'
*             AUTHORITY_CHECK        = 'X'
*             TEXT_BUFFER_FILL       =
*             BUFFER_MODE    =
*           IMPORTING
*             ACT_PLVAR      =
            TABLES
              result_tab     = lt_resulttab
*             RESULT_OBJEC   =
*             RESULT_STRUC   =
            EXCEPTIONS
              no_plvar_found = 1
              no_entry_found = 2
              OTHERS         = 3.
          IF sy-subrc <> 0.
            MESSAGE TEXT-115 TYPE 'E'.

          ELSE.
            LOOP AT lt_resulttab INTO ls_resulttab.
              READ TABLE lt_orgcomp INTO ls_orgcomp WITH KEY otype = 'O' objid = ls_resulttab-objid.
              IF sy-subrc = 0.
                IF gs_display-bukrs <> ls_orgcomp-bukrs.
                  MESSAGE TEXT-117 TYPE 'E'.
                ENDIF.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.

        IF gs_display-repmanager_pos IS NOT INITIAL.
          READ TABLE gt_position INTO gs_position WITH KEY objid = gs_display-repmanager_pos.
          IF sy-subrc NE 0.
            MESSAGE TEXT-108 TYPE 'E'.
          ENDIF.
        ENDIF.

        IF gs_display-stell IS NOT INITIAL.
          READ TABLE gt_jobid INTO gs_jobid WITH KEY objid = gs_display-stell.
          IF sy-subrc NE 0.
            MESSAGE TEXT-107 TYPE 'E'.
          ENDIF.
        ENDIF.

        IF gs_display-costcenter IS NOT INITIAL.
          SELECT SINGLE kostl FROM csks INTO @DATA(lv_cc) WHERE kostl = @gs_display-costcenter.
          IF sy-subrc NE 0.
            MESSAGE TEXT-113 TYPE 'E'.
          ENDIF.

          READ TABLE lt_hrp9010 INTO DATA(ls_hrp9010)
           with key otype = 'O' objid = gs_display-orgunit_id.
          IF sy-subrc = 0.
          else.
            MESSAGE TEXT-118 TYPE 'E'.
          ENDIF.

        ENDIF.
      ENDLOOP.
*** Updating the table After Checking ****
      LOOP AT gt_display ASSIGNING FIELD-SYMBOL(<gs_display>) WHERE sel = 'X'.

        CLEAR lv_refno.
        CALL FUNCTION 'NUMBER_GET_NEXT'
          EXPORTING
            nr_range_nr             = '01'
            object                  = 'ZREF_POSID'
          IMPORTING
            number                  = lv_refno
          EXCEPTIONS
            interval_not_found      = 1
            number_range_not_intern = 2
            object_not_found        = 3
            quantity_is_0           = 4
            quantity_is_not_1       = 5
            interval_overflow       = 6
            buffer_overflow         = 7
            OTHERS                  = 8.
        IF sy-subrc <> 0.
* Implement suitable error handling here
        ENDIF.
        <gs_display>-reference_no = lv_refno.
        MODIFY gt_display FROM <gs_display>.


        gs_pos_tab-reference_no = lv_refno.
        gs_pos_tab-bukrs = gs_display-bukrs.
        gs_pos_tab-orgunit_id = <gs_display>-orgunit_id.
        gs_pos_tab-pos_sdes = <gs_display>-pos_sdes.
        gs_pos_tab-pos_ldes = <gs_display>-pos_ldes.
        gs_pos_tab-start_date = <gs_display>-start_date.
        gs_pos_tab-stell = <gs_display>-stell.
        gs_pos_tab-rep_manager = <gs_display>-rep_manager.
        gs_pos_tab-repmanager_pos = <gs_display>-repmanager_pos.
        gs_pos_tab-status = '02'.
        gs_pos_tab-created_on = sy-datum.
        gs_pos_tab-created_by = sy-uname.
        gs_pos_tab-latitude = <gs_display>-latitude.
        gs_pos_tab-longitude = <gs_display>-longitude.
        gs_pos_tab-travel = <gs_display>-travel.
        gs_pos_tab-category = <gs_display>-category.
        gs_pos_tab-costcenter = <gs_display>-costcenter.
        MODIFY zhr_position_tab FROM gs_pos_tab.
        COMMIT WORK AND WAIT.
      ENDLOOP.
** Propose Email
      PERFORM propose_email.
      IF sy-subrc EQ 0.
        MESSAGE TEXT-109 TYPE 'S'.
        LEAVE TO SCREEN 0.
      ENDIF.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Form handle_on_f4
*&---------------------------------------------------------------------*
FORM handle_on_f4  USING      e_fieldname   TYPE lvc_fname
                             es_row_no     TYPE lvc_s_roid
                             er_event_data TYPE REF TO cl_alv_event_data.
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
  FIELD-SYMBOLS: <l_out> TYPE ty_alv. " alv table line

  REFRESH : lt_return.
  CASE e_fieldname.

    WHEN 'ORGUNIT_ID'.
      READ TABLE gt_display ASSIGNING <l_out> INDEX es_row_no-row_id.
      IF sy-subrc EQ 0.
        CALL FUNCTION 'RH_OBJID_REQUEST'
          EXPORTING
            plvar           = '01'
            otype           = 'O'
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
        <l_out>-orgunit_id = ls_selec-objid.  "Organizational Unit
        <l_out>-orgunit_des = ls_selec-stext. "Organization Unit Desc
      ENDIF.

    WHEN 'POSITION_ID'.
      READ TABLE gt_display ASSIGNING <l_out> INDEX es_row_no-row_id.
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
        <l_out>-position_id = ls_selec-objid.
        <l_out>-pos_sdes = ls_selec-short.
        <l_out>-pos_ldes = ls_selec-stext.
      ENDIF.

    WHEN 'REPMANAGER_POS'.
      READ TABLE gt_display ASSIGNING <l_out> INDEX es_row_no-row_id.
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
        <l_out>-repmanager_pos = ls_selec-objid.  "Reporting Manager Position
        <l_out>-repmng_pdes = ls_selec-stext. "Reporting Manager Position Desc
        CLEAR gs_name.
        READ TABLE gt_name INTO gs_name WITH KEY plans = <l_out>-repmanager_pos.
        IF sy-subrc EQ 0.
          <l_out>-rep_manager = gs_name-pernr. "Reporting Manager ID
          <l_out>-repmng_name = gs_name-ename. "Reporting Manager Name
        ENDIF.
      ENDIF.

    WHEN 'STELL'.
      READ TABLE gt_display ASSIGNING <l_out> INDEX es_row_no-row_id.
      IF sy-subrc EQ 0.
        CALL FUNCTION 'RH_OBJID_REQUEST'
          EXPORTING
            plvar           = '01'
            otype           = 'C'
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
        <l_out>-stell = ls_selec-objid.  "Job ID
        <l_out>-job_des = ls_selec-stext. "Job ID Desc
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
  er_event_data->m_event_handled = 'X'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Module STATUS_0200 OUTPUT
MODULE status_0200 OUTPUT.
  SET PF-STATUS 'ZPOS_APPR'.
  SET TITLEBAR 'ZAPPR'.

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
*    gs_layout-cwidth_opt = 'X'.
*    gs_layout-no_toolbar = 'X'.
    CREATE OBJECT lo_events.

    SET HANDLER: lo_events->data_changed FOR lo_grid.
    SET HANDLER: lo_events->handle_on_f4 FOR lo_grid.

    CALL METHOD lo_grid->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout
        it_toolbar_excluding = lt_exclude
      CHANGING
        it_outtab            = gt_display
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
  ELSE.
    CALL METHOD lo_grid->refresh_table_display( ).
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
MODULE user_command_0200 INPUT.
  CASE sy-ucomm.

    WHEN 'BACK' OR 'CANC'.
      LEAVE TO SCREEN 0.

    WHEN 'SEL_ALL'.
*** To Select all the check box **
      LOOP AT gt_display ASSIGNING FIELD-SYMBOL(<lgs_disp>).
        <lgs_disp>-sel = 'X'.
        MODIFY gt_display FROM <lgs_disp>.
      ENDLOOP.
      CALL METHOD lo_grid->refresh_table_display( ).

    WHEN 'DESEL_ALL'.
*** To Deselect all the check box **
      LOOP AT gt_display ASSIGNING FIELD-SYMBOL(<lgs_disp1>).
        <lgs_disp1>-sel = ''.
        MODIFY gt_display FROM <lgs_disp1>.
      ENDLOOP.
      CALL METHOD lo_grid->refresh_table_display( ).

    WHEN 'APPR'.

      LOOP AT gt_display INTO gs_display WHERE sel = 'X'.

        IF gs_display-position_id IS NOT INITIAL.
          MESSAGE TEXT-112 TYPE 'E'.
        ENDIF.
** Position Creation With All Relationship **
        PERFORM f_create_posall USING ls_object_op  CHANGING gs_output_op.
        CLEAR gs_display.
      ENDLOOP.
** Approve Email
      PERFORM approve_mail.
      IF sy-subrc EQ 0.
        MESSAGE TEXT-111 TYPE 'S'.
      ENDIF.

    WHEN 'REJE'.
      LOOP AT gt_display INTO gs_display WHERE sel = 'X'.
        " Lock Table for update
        CALL FUNCTION 'ENQUEUE_EZZHPOSITION'
          EXPORTING
            mode_zhr_position_tab = 'E'
            mandt                 = sy-mandt
            reference_no          = gs_display-reference_no
          EXCEPTIONS
            foreign_lock          = 1
            system_failure        = 2
            OTHERS                = 3.
        IF sy-subrc EQ 0.
          UPDATE zhr_position_tab SET status = '04'
                                  WHERE reference_no = gs_display-reference_no.
          MESSAGE TEXT-110 TYPE 'S'.
        ENDIF.
        " Release Lock for  Table
        CALL FUNCTION 'DEQUEUE_EZZHPOSITION'
          EXPORTING
            mode_zhr_position_tab = 'E'
            mandt                 = sy-mandt
            reference_no          = gs_display-reference_no.
      ENDLOOP.
      DELETE gt_display WHERE sel = 'X'.
  ENDCASE.
ENDMODULE.
** Approve Screen Position Create with all Relationship **
FORM f_create_posall  USING    ps_object_op TYPE ty_create_object
                      CHANGING ps_output_op TYPE ty_object_out.
  DATA ls_p1000 TYPE p1000.
  DATA ls_p1001 TYPE p1001.
  DATA ls_p9013 TYPE p9013.
  DATA ls_p9011 TYPE p9011.
  DATA:ls_P1008 TYPE p1008.


  DATA lt_p9013 TYPE TABLE OF p9013.
  DATA:lt_P1008  TYPE TABLE OF p1008.
  DATA lt_p9011 TYPE TABLE OF p9011.

  DATA lt_p1000 TYPE TABLE OF p1000.
  DATA lt_p1001 TYPE TABLE OF p1001.

  DATA: lt_enque_tab TYPE TABLE OF ppenq,
        ls_enque_tab TYPE ppenq.

  CONSTANTS: act_vtask LIKE hrrhap-vtask VALUE 'B'.

  CLEAR:ls_p1000, ls_p1001.

  CLEAR dum.
  CALL FUNCTION 'RH_GET_NEXT_NUMBER'
    EXPORTING
      action     = 'DIRECT'
      ext_number = dum-objid
      otype      = gc_otype_s
      plvar      = gc_plan_ver
    IMPORTING
      number     = ls_p1000-objid.

  ps_output_op-objid = ls_p1000-objid.

  ls_p1000-otype = gc_otype_s.
  ls_p1000-istat = '1'.
  ls_p1000-plvar = gc_plan_ver.
  ls_p1000-infty = '1000'.
  ls_p1000-langu = sy-langu.
  ls_p1000-short = gs_display-pos_sdes.
  ls_p1000-stext = gs_display-pos_ldes.
  ls_p1000-begda = gs_display-start_date.
  ls_p1000-endda = gc_high_date.
  APPEND ls_p1000 TO lt_p1000.

  CLEAR ls_p1001.
  ls_p1001-objid = ls_p1000-objid.
  ls_p1001-otype = gc_otype_s.
  ls_p1001-istat = '1'.
  ls_p1001-plvar = gc_plan_ver.
  ls_p1001-infty = '1001'.
  ls_p1001-rsign = gc_rsign_a.
  ls_p1001-relat = gc_relat_003.
  ls_p1001-prozt = 0.
  ls_p1001-begda = gs_display-start_date.
  ls_p1001-endda = gc_high_date.
  ls_p1001-sclas = gc_otype_o.
  ls_p1001-sobid = gs_display-orgunit_id.
  APPEND ls_p1001 TO lt_p1001.

  CLEAR ls_p1001.
  ls_p1001-objid = ls_p1000-objid.
  ls_p1001-otype = gc_otype_s.
  ls_p1001-istat = '1'.
  ls_p1001-plvar = gc_plan_ver.
  ls_p1001-infty = '1001'.
  ls_p1001-rsign = gc_rsign_b.
  ls_p1001-relat = gc_relat_007.
  ls_p1001-prozt = 0.
  ls_p1001-begda = gs_display-start_date.
  ls_p1001-endda = gc_high_date.
  ls_p1001-sclas = gc_otype_c.
  ls_p1001-sobid = gs_display-stell.
  APPEND ls_p1001 TO lt_p1001.

  CLEAR ls_p1001.
  ls_p1001-objid = ls_p1000-objid.
  ls_p1001-otype = gc_otype_s.
  ls_p1001-istat = '1'.
  ls_p1001-plvar = gc_plan_ver.
  ls_p1001-infty = '1001'.
  ls_p1001-rsign = gc_rsign_a.
  ls_p1001-relat = gc_relat_002.
  ls_p1001-prozt = 0.
  ls_p1001-begda = gs_display-start_date.
  ls_p1001-endda = gc_high_date.
  ls_p1001-sclas = gc_otype_s.
  ls_p1001-sobid = gs_display-repmanager_pos.
  APPEND ls_p1001 TO lt_p1001.

*For infotype 1008 to add the company code
  REFRESH lt_P1008.
  CLEAR ls_p1008.
  ls_p1008-objid = ls_p1000-objid.
  ls_p1008-otype = gc_otype_s.
  ls_p1008-istat = '1'.
  ls_p1008-plvar = gc_plan_ver.
  ls_p1008-infty = '9013'.
  ls_p1008-begda = gs_display-start_date.
  ls_p1008-endda = gc_high_date.
  ls_p1008-bukrs = gs_display-bukrs.
  APPEND ls_p1008 TO lt_p1008.



  IF gs_display-category IS NOT INITIAL.
    REFRESH lt_p9013.
    CLEAR ls_p9013.
    ls_p9013-objid = ls_p1000-objid.
    ls_p9013-otype = gc_otype_s.
    ls_p9013-istat = '1'.
    ls_p9013-plvar = gc_plan_ver.
    ls_p9013-infty = '9013'.
    ls_p9013-begda = gs_display-start_date.
    ls_p9013-endda = gc_high_date.
    ls_p9013-travel = gs_display-travel.
    ls_p9013-category = gs_display-category.
    APPEND ls_p9013 TO lt_p9013.
  ENDIF.

  IF gs_display-latitude IS NOT INITIAL AND gs_display-longitude IS NOT INITIAL.
    REFRESH lt_p9011.
    CLEAR ls_p9011.
    ls_p9011-objid = ls_p1000-objid.
    ls_p9011-otype = gc_otype_s.
    ls_p9011-istat = '1'.
    ls_p9011-plvar = gc_plan_ver.
    ls_p9011-infty = '9011'.
    ls_p9011-begda = gs_display-start_date.
    ls_p9011-endda = gc_high_date.
    ls_p9011-latitude = gs_display-latitude.
    ls_p9011-longitude = gs_display-longitude.
    APPEND ls_p9011 TO lt_p9011.
  ENDIF.

  CLEAR ls_enque_tab.
  REFRESH: lt_enque_tab.

  ls_enque_tab-plvar = ls_p1001-plvar.
  ls_enque_tab-otype = ls_p1001-otype.
  ls_enque_tab-objid = ls_p1001-objid.
  APPEND ls_enque_tab TO lt_enque_tab.

  ls_enque_tab-plvar = ls_p1001-plvar.
  ls_enque_tab-otype = ls_p1001-sclas.
  ls_enque_tab-objid = gs_display-orgunit_id+0(8).
  APPEND ls_enque_tab TO lt_enque_tab.

  ls_enque_tab-plvar = ls_p1001-plvar.
  ls_enque_tab-otype = ls_p1001-sclas.
  ls_enque_tab-objid = gs_display-stell+0(8).
  APPEND ls_enque_tab TO lt_enque_tab.

  ls_enque_tab-plvar = ls_p1001-plvar.
  ls_enque_tab-otype = ls_p1001-sclas.
  ls_enque_tab-objid = gs_display-repmanager_pos+0(8).
  APPEND ls_enque_tab TO lt_enque_tab.


  CALL FUNCTION 'RH_ENQUEUE_LIST'
    TABLES
      enq_tab = lt_enque_tab
    EXCEPTIONS
      OTHERS  = 1.
  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty
            NUMBER sy-msgno
            INTO ps_output_op-p1000
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ps_output_op-indic = 'N'.
    EXIT.
  ENDIF.

*  prepare db-update
  CALL FUNCTION 'RH_INSERT_INFTY'
    EXPORTING
      fcode               = 'INSE'
      vtask               = act_vtask
    TABLES
      innnn               = lt_p1000
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
    ps_output_op-p1000 = 'Error during createing the object record'.
    APPEND ps_output_op TO gt_output_op.
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
    ps_output_op-indic = 'N'.
    ps_output_op-p1000 = 'No Authorization to Create object record'.
    APPEND ps_output_op TO gt_output_op.
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
    ps_output_op-indic = 'N'.
    ps_output_op-p1000 = 'Error during Saving the object Record'. "MESSAGE e377.
    APPEND ps_output_op TO gt_output_op.
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
    ps_output_op-p1001 = 'Error during creating the relationship record'.
    APPEND ps_output_op TO gt_output_op.
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
    ps_output_op-indic = 'N'.
    ps_output_op-p1001 = 'No Authorization to Create Relationship record'.
    APPEND ps_output_op TO gt_output_op.
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
    ps_output_op-indic = 'N'.
    ps_output_op-p1001 = 'Error during Saving the Relationship Record'.
    APPEND ps_output_op TO gt_output_op.
    EXIT.
  ENDIF.

  IF gs_display-category IS NOT INITIAL.
    CALL FUNCTION 'RH_INSERT_INFTY'
      EXPORTING
        fcode               = 'INSE'
        vtask               = act_vtask
      TABLES
        innnn               = lt_p9013
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
      ps_output_op-p1001 = 'Error during creating the relationship record'.
      APPEND ps_output_op TO gt_output_op.
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
      ps_output_op-indic = 'N'.
      ps_output_op-p1001 = 'No Authorization to Create Relationship record'.
      APPEND ps_output_op TO gt_output_op.
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
      ps_output_op-indic = 'N'.
      ps_output_op-p1001 = 'Error during Saving the Relationship Record'.
      APPEND ps_output_op TO gt_output_op.
      EXIT.
    ENDIF.
  ENDIF.
  IF gs_display-latitude IS NOT INITIAL AND gs_display-longitude IS NOT INITIAL.
    CALL FUNCTION 'RH_INSERT_INFTY'
      EXPORTING
        fcode               = 'INSE'
        vtask               = act_vtask
      TABLES
        innnn               = lt_p9011
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
      ps_output_op-p1001 = 'Error during creating the relationship record'.
      APPEND ps_output_op TO gt_output_op.
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
      ps_output_op-indic = 'N'.
      ps_output_op-p1001 = 'No Authorization to Create Relationship record'.
      APPEND ps_output_op TO gt_output_op.
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
      ps_output_op-indic = 'N'.
      ps_output_op-p1001 = 'Error during Saving the Relationship Record'.
      APPEND ps_output_op TO gt_output_op.
      EXIT.
    ENDIF.
  ENDIF.

  IF gs_display-bukrs IS NOT INITIAL.
    CALL FUNCTION 'RH_INSERT_INFTY'
      EXPORTING
        fcode               = 'INSE'
        vtask               = act_vtask
      TABLES
        innnn               = lt_p1008
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
      ps_output_op-p1001 = 'Error during creating the 1008 record'.
      APPEND ps_output_op TO gt_output_op.
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
      ps_output_op-indic = 'N'.
      ps_output_op-p1001 = 'No Authorization to Create 1008 record'.
      APPEND ps_output_op TO gt_output_op.
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
      ps_output_op-indic = 'N'.
      ps_output_op-p1001 = 'Error during Saving the 1008 Record'.
      APPEND ps_output_op TO gt_output_op.
      EXIT.
    ENDIF.
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
    ps_output_op-indic = 'N'.
    ps_output_op-p1000 = 'Error during updating the database'.
    ps_output_op-p1001 = 'Error during updating the database'.
    APPEND ps_output_op TO gt_output_op.
  ELSE.
    ps_output_op-indic = 'S'.
*    ps_output_op-p1000 = 'Object Created successfully'.
    ps_output_op-p1001 = ' objects and all Reltionship created successfully'.
    APPEND ps_output_op TO gt_output_op.
  ENDIF.

  CALL FUNCTION 'RH_DEQUEUE_LIST'
    TABLES
      deq_tab = lt_enque_tab.
  CALL FUNCTION 'RH_CLEAR_BUFFER'.
  CALL FUNCTION 'RH_CLEAR_PLOG_TAB'.
  CALL FUNCTION 'DEQUEUE_ALL'.

  gs_display-position_id = ls_p1000-objid.
  gs_display-status = '03'.
  CLEAR ls_domain.
  READ TABLE lt_domain INTO ls_domain WITH KEY domvalue_l = gs_display-status.
  IF sy-subrc EQ 0.
    gs_display-status_desc = ls_domain-ddtext.
  ENDIF.
  gs_display-approved_on = sy-datum.
  gs_display-approved_by = sy-uname.
*** Error Message Handling for display **
  IF ps_output_op-p1000 IS NOT INITIAL.
    gs_display-message = ps_output_op-p1000.
  ELSEIF ps_output_op-p1001 IS NOT INITIAL.
    gs_display-message = ps_output_op-p1001.
  ENDIF.
***********Costcenter relation through bdc *********************************
  DATA: lv_bpartner TYPE bu_partner,
        lv_error    TYPE flag.
  CLEAR: lv_bpartner,lv_error.
  lv_bpartner = ls_p1000-objid.
  IF gs_display-costcenter IS NOT INITIAL.
    "Costcenter Mapping to Position ID
    PERFORM f_costcenter_relation USING gs_display-costcenter CHANGING lv_error.
    IF lv_error IS NOT INITIAL.
      gs_display-ccrel = 'X'.
    ENDIF.
  ENDIF.
***** Business Partner Creation *************
  IF gs_display-bp = abap_true.
    CLEAR lv_error.
    PERFORM f_bp_create USING lv_bpartner CHANGING lv_error.
    IF lv_error IS INITIAL.
      gs_display-bpcreate = 'X'.
      gs_display-bpextend = 'X'.
*** Relationship create for position to BP ***
      CLEAR lv_error.
      PERFORM f_pos_to_bp_rshp USING lv_bpartner CHANGING lv_error.
      IF lv_error IS NOT INITIAL.
        gs_display-bprel = 'X'.
      ENDIF.
    ENDIF.
  ENDIF.
  MODIFY gt_display FROM gs_display.
  " Lock Table for update
  CALL FUNCTION 'ENQUEUE_EZZHPOSITION'
    EXPORTING
      mode_zhr_position_tab = 'E'
      mandt                 = sy-mandt
      reference_no          = gs_display-reference_no
    EXCEPTIONS
      foreign_lock          = 1
      system_failure        = 2
      OTHERS                = 3.
  IF sy-subrc EQ 0.
** Updating Table for Position Creation **
    UPDATE zhr_position_tab SET position_id = ls_p1001-objid
                                status = '03'
                                approved_by = sy-uname
                                approved_on = sy-datum
                                WHERE reference_no = gs_display-reference_no.
  ENDIF.
  " Release Lock for  Table
  CALL FUNCTION 'DEQUEUE_EZZHPOSITION'
    EXPORTING
      mode_zhr_position_tab = 'E'
      mandt                 = sy-mandt
      reference_no          = gs_display-reference_no.
ENDFORM.
*&
*&---------------------------------------------------------------------*
MODULE status_0300 OUTPUT.

  SET PF-STATUS 'CHANGE_REPMAN'.
  SET TITLEBAR 'ZCHANGE_REP'.

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
*    gs_layout-no_toolbar = 'X'.
    CALL METHOD lo_grid->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout
        it_toolbar_excluding = lt_exclude
      CHANGING
        it_outtab            = gt_chg_rep
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
  ELSE.
    CALL METHOD lo_grid->refresh_table_display( ).
  ENDIF.
ENDMODULE.
*----------------------------------------------------------------------*
MODULE user_command_0300 INPUT.
  CASE sy-ucomm.

    WHEN 'BACK' OR 'CANC'.
      LEAVE TO SCREEN 0.

    WHEN 'SEL_ALL'.
      LOOP AT gt_chg_rep ASSIGNING FIELD-SYMBOL(<fls_chg_rep>).
        <fls_chg_rep>-sel = 'X'.
        MODIFY gt_chg_rep FROM <fls_chg_rep>.
      ENDLOOP.
      CALL METHOD lo_grid->refresh_table_display( ).

    WHEN 'DESEL_ALL'.
      LOOP AT gt_chg_rep ASSIGNING FIELD-SYMBOL(<fls_chg_rep1>).
        <fls_chg_rep1>-sel = ''.
        MODIFY gt_chg_rep FROM <fls_chg_rep1>.
      ENDLOOP.
      CALL METHOD lo_grid->refresh_table_display( ).

    WHEN 'CHANGE'.
      LOOP AT gt_chg_rep ASSIGNING FIELD-SYMBOL(<fgs_chg_rep>) WHERE sel = 'X'.
        IF <fgs_chg_rep>-date IS INITIAL.
          MESSAGE 'Date is Mandatory' TYPE 'E'.
        ELSEIF <fgs_chg_rep>-newrep IS INITIAL.
          MESSAGE 'New Reporting Manager Number is Mandatory' TYPE 'E'.
        ENDIF.
      ENDLOOP.

      LOOP AT gt_chg_rep ASSIGNING FIELD-SYMBOL(<fgs_chg_rep1>) WHERE sel = 'X'.
        PERFORM f_change_reporting_mager USING <fgs_chg_rep1>.
      ENDLOOP.

  ENDCASE.

ENDMODULE.
FORM f_change_reporting_mager USING ps_chg_rep TYPE ty_chg_rep.
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
  ls_p1001-begda = ps_chg_rep-date.
  ls_p1001-endda = gc_high_date.
  ls_p1001-sclas = gc_otype_s.
  ls_p1001-sobid = ps_chg_rep-newrepos.
  APPEND ls_p1001 TO lt_p1001.

  CLEAR ls_enque_tab.
  REFRESH: lt_enque_tab.

  ls_enque_tab-plvar = ls_p1001-plvar.
  ls_enque_tab-otype = ls_p1001-sclas.
  ls_enque_tab-objid = ps_chg_rep-newrepos+0(8).
  APPEND ls_enque_tab TO lt_enque_tab.

  CALL FUNCTION 'RH_ENQUEUE_LIST'
    TABLES
      enq_tab = lt_enque_tab
    EXCEPTIONS
      OTHERS  = 1.
  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty
            NUMBER sy-msgno
            INTO ps_chg_rep-message
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
    ps_chg_rep-message = 'Error during creating the relationship record'.
    MODIFY gt_chg_rep FROM ps_chg_rep INDEX ps_chg_rep-pernr.
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
    ps_chg_rep-message = 'No Authorization to Create Relationship record'.
    MODIFY gt_chg_rep FROM ps_chg_rep INDEX ps_chg_rep-pernr.
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
    ps_chg_rep-message = 'Error during Saving the Relationship Record'.
    MODIFY gt_chg_rep FROM ps_chg_rep INDEX ps_chg_rep-pernr.
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
    ps_chg_rep-message = 'Error during updating the database'.
    MODIFY gt_chg_rep FROM ps_chg_rep INDEX ps_chg_rep-pernr.
  ELSE.
    ps_chg_rep-message = 'Reporting Manager Changed Successfully'.
    MODIFY gt_chg_rep FROM ps_chg_rep INDEX ps_chg_rep-pernr.
  ENDIF.

  CALL FUNCTION 'RH_DEQUEUE_LIST'
    TABLES
      deq_tab = lt_enque_tab.
  CALL FUNCTION 'RH_CLEAR_BUFFER'.
  CALL FUNCTION 'RH_CLEAR_PLOG_TAB'.
  CALL FUNCTION 'DEQUEUE_ALL'.
ENDFORM.
FORM f_bp_create USING p_partner TYPE bu_partner CHANGING p_flag TYPE xfeld.
  DATA: businesspartner  TYPE bapibus1006_head-bpartner. " Business Partner Number
  DATA: partnercategory         TYPE bapibus1006_head-partn_cat, " Business Partner Category
        partnergroup            TYPE bapibus1006_head-partn_grp, " Business Partner Grouping
        centraldataorganization TYPE bapibus1006_central_organ. " SAP BP: BAPI Structure for Organization Data
  DATA: it_telephondata TYPE STANDARD TABLE OF bapiadtel, " BAPI Structure for Telephone Numbers (Bus. Address Services)
        it_maildata     TYPE STANDARD TABLE OF bapiadsmtp, " BAPI Structure for E-Mail Addresses (Bus. Address Services)
        return          TYPE STANDARD TABLE OF bapiret2. " Return Parameter
  DATA(lv_partner) = p_partner.
  CLEAR: businesspartner,partnercategory,partnergroup,centraldataorganization.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = lv_partner
    IMPORTING
      output = lv_partner.

*  gv_bpcat = <fs_excel>-bpcat.
  partnercategory = '2'.
  partnergroup = 'YEVP'.
  DATA(ls_central) = VALUE bapibus1006_central( searchterm1 = p_partner
                                                title_key = '0003' ).
  centraldataorganization-name1 = gs_display-pos_ldes.
*  CONCATENATE gs_display-pos_sdes lv_partner
*      INTO centraldataorganization-name1 SEPARATED BY space.

**** adddress DATA filling in business partner creation **
  DATA(ls_address) = VALUE bapibus1006_address( country = 'IN'
                                                 langu = 'E' ).
  REFRESH: it_telephondata.
  it_telephondata[] = VALUE #(  ( country = 'IN'
                                  telephone = '1234567890'
                                  std_no = 'X'
                                  r_3_user = '3'
                                  home_flag = 'X' )  ).
  REFRESH: it_maildata.
  it_maildata[] = VALUE #( ( e_mail = 'noreply@sheenlac.in'
                             std_no = 'X'
                             std_recip = 'X'
                             home_flag = 'X'
                             consnumber = '001' ) ).
*** Bapi to create Business Partner ***
  REFRESH: return.
  CALL FUNCTION 'BAPI_BUPA_CREATE_FROM_DATA'
    EXPORTING
      businesspartnerextern   = lv_partner
      partnercategory         = partnercategory
      partnergroup            = partnergroup
      centraldata             = ls_central
*     centraldataperson       = centraldataperson
      centraldataorganization = centraldataorganization
      addressdata             = ls_address
      duplicate_message_type  = 'W'
    IMPORTING
      businesspartner         = businesspartner
    TABLES
      telefondata             = it_telephondata
      e_maildata              = it_maildata
      return                  = return.
  READ TABLE return INTO DATA(ls_ret) WITH KEY type = 'E'.
  IF sy-subrc EQ 0.
    p_flag = abap_true.
  ELSE.
    IF p_flag IS INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
      PERFORM f_role_extend USING businesspartner.
    ENDIF.
  ENDIF.
ENDFORM.
FORM f_role_extend USING p_partner TYPE bu_partner.
  DATA businesspartnerrolecategory TYPE bapibus1006_bproles-partnerrolecategory. " BP Role Category
  DATA all_businesspartnerroles    TYPE bapibus1006_x-mark. " Data element for domain BOOLE: TRUE (='X') and FALSE (=' ')
  DATA businesspartnerrole         TYPE bapibus1006_bproles-partnerrole. " BP Role
  DATA differentiationtypevalue    TYPE bapibus1006_bproles-difftypevalue. " BP: Differentiation type value
  DATA validfromdate               TYPE bapibus1006_bprole_validity-bprolevalidfrom.
  DATA validuntildate              TYPE bapibus1006_bprole_validity-bprolevalidto.
  DATA return                      TYPE STANDARD TABLE OF bapiret2. " Return Parameter
*** Tvarvc variable for Adding roles to BP ****
  SELECT * FROM tvarvc INTO TABLE @DATA(lt_tvarv)
                       WHERE name = 'BP_POS_ROLE_EXTEND'
                       AND type = 'S'.
  IF sy-subrc = 0.
    LOOP AT lt_tvarv INTO DATA(ls_tvarv).

      businesspartnerrole = ls_tvarv-low.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = p_partner
        IMPORTING
          output = p_partner.

      CALL FUNCTION 'BAPI_BUPA_ROLE_ADD_2'
        EXPORTING
          businesspartner             = p_partner
          businesspartnerrolecategory = businesspartnerrolecategory
          all_businesspartnerroles    = ' '
          businesspartnerrole         = businesspartnerrole
          differentiationtypevalue    = differentiationtypevalue
*         VALIDFROMDATE               = P_DATE
          validuntildate              = '99991231'
        TABLES
          return                      = return.
      READ TABLE return ASSIGNING FIELD-SYMBOL(<fs_return>) WITH KEY type = 'E'.
      IF sy-subrc NE 0 .
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.
FORM f_pos_to_bp_rshp USING p_partner TYPE bu_partner CHANGING p_flag TYPE flag.
  DATA ls_p1000 TYPE p1000.
  DATA ls_p1001 TYPE p1001.

  DATA lt_p1000 TYPE TABLE OF p1000.
  DATA lt_p1001 TYPE TABLE OF p1001.

  DATA: lt_enque_tab TYPE TABLE OF ppenq,
        ls_enque_tab TYPE ppenq.
  CONSTANTS: act_vtask LIKE hrrhap-vtask VALUE 'B'.
  DATA(lv_partner) = p_partner.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = lv_partner
    IMPORTING
      output = lv_partner.
  CLEAR lt_p1001.
  CLEAR ls_p1001.
  ls_p1001-objid = gs_display-position_id.
  ls_p1001-otype = gc_otype_s.
  ls_p1001-istat = '1'.
  ls_p1001-plvar = gc_plan_ver.
  ls_p1001-infty = '1001'.
  ls_p1001-rsign = gc_rsign_a.
  ls_p1001-relat = '008'.
  ls_p1001-prozt = 0.
  ls_p1001-begda = gs_display-start_date.
  ls_p1001-endda = gc_high_date.
  ls_p1001-sclas = 'BP'.
  ls_p1001-sobid = lv_partner.
  APPEND ls_p1001 TO lt_p1001.

  CLEAR ls_enque_tab.
  REFRESH: lt_enque_tab.

  ls_enque_tab-plvar = ls_p1001-plvar.
  ls_enque_tab-otype = ls_p1001-sclas.
  ls_enque_tab-objid = gs_display-position_id+0(8).
  APPEND ls_enque_tab TO lt_enque_tab.

  CALL FUNCTION 'RH_ENQUEUE_LIST'
    TABLES
      enq_tab = lt_enque_tab
    EXCEPTIONS
      OTHERS  = 1.
  IF sy-subrc NE 0.
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
  ELSE.
    p_flag = 'X'.
  ENDIF.

  CALL FUNCTION 'RH_DEQUEUE_LIST'
    TABLES
      deq_tab = lt_enque_tab.
  CALL FUNCTION 'RH_CLEAR_BUFFER'.
  CALL FUNCTION 'RH_CLEAR_PLOG_TAB'.
  CALL FUNCTION 'DEQUEUE_ALL'.
ENDFORM.
FORM f_costcenter_relation USING p_costcenter TYPE kostl CHANGING p_flag TYPE flag.
** Internal Table for bdc **
  DATA: lv_coarea TYPE kokrs VALUE '1000'.
  DATA: lv_sobid TYPE sobid.
  DATA: lv_sobid1 TYPE sobid.
  DATA: lv_length    TYPE i,
        lv_begda(10) TYPE c,
        lv_kostl     TYPE kostl.

  CLEAR: lv_sobid,lv_sobid1,lv_kostl.
  lv_kostl = p_costcenter.
  MOVE lv_kostl TO lv_sobid.
  lv_sobid+10 = lv_coarea.
  SHIFT lv_sobid LEFT DELETING LEADING ''.

  CLEAR lv_begda.
  WRITE gs_display-start_date TO lv_begda DD/MM/YYYY.

  REFRESH: it_bdcdata,gt_bdcmsg.
  PERFORM bdc_dynpro      USING 'SAPMH5A0' '1000'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'PPHDR-BEGDA'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'PPHDR-PLVAR'
                                '01'.
  PERFORM bdc_field       USING 'PPHDR-OTYPE'
                                'S'.
  PERFORM bdc_field       USING 'PM0D1-SEARK'
                                gs_display-position_id.
  PERFORM bdc_field       USING 'PPHDR-INFTY'
                                '1001'.
  PERFORM bdc_field       USING 'PPHDR-SUBTY'
                                'A011'.
  PERFORM bdc_field       USING 'PPHDR-ISTAT'
                                '1'.
  PERFORM bdc_field       USING 'PPHDR-BEGDA'
                                lv_begda.
  PERFORM bdc_field       USING 'PPHDR-ENDDA'
                                '31.12.9999'.
  PERFORM bdc_dynpro      USING 'SAPMH5A0' '1000'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'PPHDR-PLVAR'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=INSE'.
  PERFORM bdc_field       USING 'PPHDR-PLVAR'
                                '01'.
  PERFORM bdc_field       USING 'PPHDR-OTYPE'
                                'S'.
  PERFORM bdc_field       USING 'PM0D1-SEARK'
                                gs_display-position_id.
  PERFORM bdc_field       USING 'PPHDR-INFTY'
                                '1001'.
  PERFORM bdc_field       USING 'PPHDR-SUBTY'
                                'A011'.
  PERFORM bdc_field       USING 'PPHDR-ISTAT'
                                '1'.
  PERFORM bdc_field       USING 'PPHDR-BEGDA'
                                lv_begda.
  PERFORM bdc_field       USING 'PPHDR-ENDDA'
                                '31.12.9999'.
  PERFORM bdc_dynpro      USING 'MP100100' '2000'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'P1001-SOBID'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'P1001-BEGDA'
                                lv_begda.
  PERFORM bdc_field       USING 'P1001-ENDDA'
                                '31.12.9999'.
  PERFORM bdc_field       USING 'P1001-RSIGN'
                                'A'.
  PERFORM bdc_field       USING 'P1001-RELAT'
                                '011'.
  PERFORM bdc_field       USING 'P1001-SCLAS'
                                'K'.
  PERFORM bdc_field       USING 'P1001-SOBID'
                                lv_sobid.
  PERFORM bdc_dynpro      USING 'MP100100' '2000'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'P1001-BEGDA'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=UPD'.
  PERFORM bdc_field       USING 'P1001-BEGDA'
                                lv_begda.
  PERFORM bdc_field       USING 'P1001-ENDDA'
                                '31.12.9999'.
  PERFORM bdc_field       USING 'P1001-RSIGN'
                                'A'.
  PERFORM bdc_field       USING 'P1001-RELAT'
                                '011'.
  PERFORM bdc_field       USING 'P1001-SCLAS'
                                'K'.
  PERFORM bdc_field       USING 'P1001-SOBID'
                                lv_sobid.
  PERFORM bdc_dynpro      USING 'MP100100' '5010'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'PKEYK-KOSTL'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=UPD'.
*  PERFORM bdc_field       USING 'PKEYK-KOSTL'
*                                'HPITSUPPRT'.
*  PERFORM bdc_field       USING 'PKEYK-KOKRS'
*                                '1000'.
  PERFORM bdc_dynpro      USING 'SAPMH5A0' '1000'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'PPHDR-PLVAR'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=BACK'.
  PERFORM bdc_field       USING 'PPHDR-PLVAR'
                                '01'.
  PERFORM bdc_field       USING 'PPHDR-OTYPE'
                                'S'.
  PERFORM bdc_field       USING 'PM0D1-SEARK'
                                gs_display-position_id.
  PERFORM bdc_field       USING 'PPHDR-INFTY'
                                '1001'.
  PERFORM bdc_field       USING 'PPHDR-SUBTY'
                                'A011'.
  PERFORM bdc_field       USING 'PPHDR-ISTAT'
                                '1'.
  PERFORM bdc_field       USING 'PPHDR-BEGDA'
                                lv_begda.
  PERFORM bdc_field       USING 'PPHDR-ENDDA'
                                '31.12.9999'.
  DATA(ls_options) = VALUE ctu_params(  dismode = 'N'
                                        updmode = 'S'
                                        defsize = ''
                                        nobinpt = 'X'
                                        racommit = 'X' ).
  TRY.
      CALL TRANSACTION 'PP02' WITH AUTHORITY-CHECK
                            USING it_bdcdata
                            OPTIONS FROM ls_options
                            MESSAGES INTO gt_bdcmsg.
    CATCH cx_sy_authorization_error.
  ENDTRY.
  READ TABLE gt_bdcmsg INTO DATA(ls_msg) WITH KEY msgtyp = 'E'.
  IF sy-subrc NE 0.
    p_flag = abap_true.
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
