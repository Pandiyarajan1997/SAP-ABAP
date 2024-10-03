*&---------------------------------------------------------------------*
*& Report ZHR_POSITION_MASTER
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zhr_position_master.

TABLES: hrp1000.

DATA: it_position_tab TYPE TABLE OF zhr_postion_tab,
      wa_position_tab TYPE zhr_postion_tab.

DATA: it_pos_tab TYPE lso_range_objid_tab,
      wa_pos_tab TYPE lso_range_objid.

TYPES: BEGIN OF ty_hrp1000,
         otype TYPE otype,
         objid TYPE hrobjid,
         begda TYPE begdatum,
       END OF ty_hrp1000.

DATA: it_hrp1000_B TYPE TABLE OF ty_hrp1000,
      wa_hrp1000_B TYPE ty_hrp1000.

*Data Decleration
TYPES: BEGIN OF ty_output.
         INCLUDE STRUCTURE zhr_postion_tab.
TYPES:   emp_name TYPE pa0001-ename,
         mgr_name TYPE pa0001-ename,
         main     TYPE char1,
         pbegda   TYPE begdatum,
*         posemp   TYPE char20,
*         posmgpos TYPE char20,
       END OF ty_output.

DATA: lt_output TYPE TABLE OF ty_output,
      wa_output TYPE ty_output.

TYPES: BEGIN OF ty_pa0001,
         pernr TYPE pa0001-pernr,
         ename TYPE pa0001-ename,
         plans TYPE pa0001-plans,
       END OF ty_pa0001.

DATA: it_pa0001 TYPE TABLE OF ty_pa0001,
      wa_pa0001 TYPE ty_pa0001.

DATA: gs_layout TYPE  slis_layout_alv,
* Internal Table Declaration for FieldCatlog
      gt_fcat   TYPE  slis_t_fieldcat_alv,
* Work Area Declaration for FieldCatlog
      gs_fcat   TYPE slis_fieldcat_alv.

DATA: gt_variant TYPE disvariant.

DATA: lv_per(8) TYPE c.

*Selection Screen
SELECTION-SCREEN: BEGIN OF BLOCK blk1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: so_pos FOR hrp1000-objid.
SELECTION-SCREEN: END OF BLOCK blk1.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
  PARAMETERS p_layo LIKE disvariant-variant.
SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN FUNCTION KEY 1. "Added by zakir for refresh button function

INITIALIZATION.
  PERFORM get_default_layout CHANGING p_layo.

AT SELECTION-SCREEN.
  DATA: gx_variant TYPE disvariant.

  IF NOT p_layo IS INITIAL.
    MOVE gt_variant TO gx_variant.
    MOVE: p_layo         TO gx_variant-variant,
          sy-repid       TO gx_variant-report.
    CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE'
      EXPORTING
        i_save     = 'A'
      CHANGING
        cs_variant = gx_variant.
    gt_variant = gx_variant.
  ELSE.
    CLEAR gt_variant.
    gt_variant-report = sy-repid.
    gt_variant-username = sy-uname.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_layo.
  PERFORM select_alv_variant CHANGING p_layo.



START-OF-SELECTION.

* get all the active pernr, name and positions
  SELECT pa0001~pernr pa0001~ename pa0001~plans
      INTO TABLE it_pa0001 FROM pa0000
      INNER JOIN pa0001 ON pa0000~pernr = pa0001~pernr
      WHERE pa0000~stat2 EQ '3'
      AND pa0001~endda GE sy-datum AND pa0000~endda GE sy-datum.
  IF sy-subrc = 0.
    SORT it_pa0001 BY pernr.
  ENDIF.

*  SELECT pernr ename plans FROM pa0001 INTO TABLE it_pa0001 WHERE begda <= sy-datum and endda >= sy-datum

  IF so_pos[] IS INITIAL.
    CALL FUNCTION 'ZHR_POSITION_DETAILS'
* EXPORTING
*   POSITION          =
      TABLES
        it_position = it_position_tab.
  ELSE.

    REFRESH it_pos_tab.

    LOOP AT so_pos.
      CLEAR wa_pos_tab.
      wa_pos_tab-sign = so_pos-sign.
      wa_pos_tab-option = so_pos-option.
      wa_pos_tab-low = so_pos-low.
      wa_pos_tab-high = so_pos-high.
      APPEND wa_pos_tab TO it_pos_tab.
    ENDLOOP.

    CALL FUNCTION 'ZHR_POSITION_DETAILS'
      EXPORTING
        position    = it_pos_tab
      TABLES
        it_position = it_position_tab.
  ENDIF.

end-of-SELECTION.

  REFRESH lt_output.

  REFRESH: it_hrp1000_B.

  SELECT otype objid begda
    FROM hrp1000
    INTO TABLE it_hrp1000_B
    WHERE plvar = '01'
      AND otype = 'S'
      AND objid BETWEEN '20000000' AND '29999999'.
  IF sy-subrc = 0.
    SORT it_hrp1000_B BY objid begda ASCENDING.

    DELETE ADJACENT DUPLICATES FROM it_hrp1000_B COMPARING objid begda.
  ENDIF.

  LOOP AT it_position_tab INTO wa_position_tab.
    CLEAR wa_output.
    MOVE-CORRESPONDING wa_position_tab TO wa_output.
* get the employee name
    IF wa_position_tab-empid IS NOT INITIAL.
      CLEAR wa_pa0001.
      READ TABLE it_pa0001 INTO wa_pa0001 WITH KEY pernr = wa_position_tab-empid BINARY SEARCH.
      IF sy-subrc = 0.
        wa_output-emp_name = wa_pa0001-ename.
      ENDIF.
*check if this the main poition for the employee
      READ TABLE it_pa0001 WITH KEY pernr = wa_position_tab-empid plans = wa_position_tab-objid TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        wa_output-main = abap_true.
      ENDIF.

    ENDIF.

    IF wa_position_tab-mgr_emp IS NOT INITIAL.
      CLEAR wa_pa0001.
      READ TABLE it_pa0001 INTO wa_pa0001 WITH KEY pernr = wa_position_tab-mgr_emp BINARY SEARCH.
      IF sy-subrc = 0.
        wa_output-mgr_name = wa_pa0001-ename.
      ENDIF.
    ENDIF.

*    lv_per = wa_output-empid.
*    IF lv_per IS INITIAL.
*      CLEAR lv_per.
*    ELSE.
*      SHIFT lv_per LEFT DELETING LEADING '0'.
*    ENDIF.
*    CONCATENATE wa_output-objid '-' lv_per '-' wa_output-main INTO wa_output-posemp.
*
*    CONCATENATE wa_output-objid '-' wa_output-mgr_pos INTO wa_output-posmgpos.

    CLEAR wa_hrp1000_B.
    READ TABLE it_hrp1000_B INTO wa_hrp1000_B WITH KEY objid = wa_position_tab-objid.
    IF sy-subrc = 0.
      wa_output-pbegda = wa_hrp1000_B-begda.
    ENDIF.

    APPEND wa_output TO lt_output.

  ENDLOOP.



  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name     = sy-repid
*     i_internal_tabname = 'IT_POSITION_TAB'
      i_internal_tabname = 'LT_OUTPUT'
      i_structure_name   = 'ZHR_POSTION_TAB'
      i_inclname         = sy-repid
    CHANGING
      ct_fieldcat        = gt_fcat.

  gs_layout-colwidth_optimize = 'X'.


  LOOP AT gt_fcat INTO gs_fcat.

    CASE gs_fcat-fieldname.
      WHEN 'OBJID'.
        gs_fcat-seltext_m  = 'Position ID'.
        gs_fcat-seltext_l  = 'Position ID'.
      WHEN 'STEXT'.
        gs_fcat-seltext_m  = 'Position Desc'.
        gs_fcat-seltext_l  = 'Position Desc'.
      WHEN 'EMPID'.
        gs_fcat-seltext_m  = 'Employee ID'.
        gs_fcat-seltext_l  = 'Employee ID'.
      WHEN 'MGR_POS'.
        gs_fcat-seltext_m  = 'Mgr Pos ID'.
        gs_fcat-seltext_l  = 'Mgr Pos ID'.
      WHEN 'MGR_POS_TXT'.
        gs_fcat-seltext_m  = 'Mgr Pos Desc'.
        gs_fcat-seltext_l  = 'Mgr Pos Desc'.
      WHEN 'MGR_EMP'.
        gs_fcat-seltext_m  = 'Mgr Emp ID'.
        gs_fcat-seltext_l  = 'Mgr Emp ID'.
      WHEN 'KOSTL'.
        gs_fcat-seltext_m  = 'Cost Center'.
        gs_fcat-seltext_l  = 'Cost Center'.
      WHEN 'EMP_TYPE'.
        gs_fcat-seltext_m  = 'Emp Type'.
        gs_fcat-seltext_l  = 'Emp Type'.
      WHEN 'EMP_CATEGORY'.
        gs_fcat-seltext_m  = 'Emp Travel Cat'.
        gs_fcat-seltext_l  = 'Emp Travel Cat'.
      WHEN 'EMP_TRAVEL_ALL'.
        gs_fcat-seltext_m  = 'Travel Allowed'.
        gs_fcat-seltext_l  = 'Travel Allowed'.
      WHEN 'LATITUDE'.
        gs_fcat-seltext_m  = 'Latitude'.
        gs_fcat-seltext_l  = 'Latitude'.
      WHEN 'LONGITUDE'.
        gs_fcat-seltext_m  = 'Longitude'.
        gs_fcat-seltext_l  = 'Longitude'.
      WHEN 'ORGUNIT'.
        gs_fcat-seltext_m  = 'Orgunit'.
        gs_fcat-seltext_l  = 'Orgunit'.
      WHEN 'ORGUNIT_TXT'.
        gs_fcat-seltext_m  = 'Orgunit Desc'.
        gs_fcat-seltext_l  = 'Orgunit Desc'.
      WHEN 'JOBCODE'.
        gs_fcat-seltext_m  = 'Job ID'.
        gs_fcat-seltext_l  = 'Job ID'.
      WHEN 'JOBCODE_TXT'.
        gs_fcat-seltext_m  = 'Job Desc'.
        gs_fcat-seltext_l  = 'Job Desc'.
      WHEN 'POSBPID'.
        gs_fcat-seltext_m  = 'Pos-BP ID'.
        gs_fcat-seltext_l  = 'Pos-BP ID'.
      WHEN OTHERS.
    ENDCASE.

    gs_fcat-seltext_l = gs_fcat-reptext_ddic = gs_fcat-seltext_m.
    CLEAR: gs_fcat-seltext_s, gs_fcat-ref_fieldname, gs_fcat-ref_tabname.
    MODIFY gt_fcat FROM gs_fcat INDEX sy-tabix.
  ENDLOOP.

  CLEAR gs_fcat.
  gs_fcat-col_pos = '18'.
  gs_fcat-fieldname = 'EMP_NAME'.
  gs_fcat-tabname  = 'LT_OUTPUT'.
  gs_fcat-seltext_m  = 'Employee Name'.
  APPEND gs_fcat TO gt_fcat.

  CLEAR gs_fcat.
  gs_fcat-col_pos = '19'.
  gs_fcat-fieldname = 'MGR_NAME'.
  gs_fcat-tabname  = 'LT_OUTPUT'.
  gs_fcat-seltext_m  = 'Manager Name'.
  APPEND gs_fcat TO gt_fcat.

  CLEAR gs_fcat.
  gs_fcat-col_pos = '20'.
  gs_fcat-fieldname = 'MAIN'.
  gs_fcat-tabname  = 'LT_OUTPUT'.
  gs_fcat-seltext_m  = 'Primary'.
  APPEND gs_fcat TO gt_fcat.

*  CLEAR gs_fcat.
*  gs_fcat-col_pos = '21'.
*  gs_fcat-fieldname = 'POSEMP'.
*  gs_fcat-tabname  = 'LT_OUTPUT'.
*  gs_fcat-seltext_m  = 'POS-EMP CON'.
*  APPEND gs_fcat TO gt_fcat.
*
*  CLEAR gs_fcat.
*  gs_fcat-col_pos = '22'.
*  gs_fcat-fieldname = 'POSMGPOS'.
*  gs_fcat-tabname  = 'LT_OUTPUT'.
*  gs_fcat-seltext_m  = 'Pos-MGPOS CON'.
*  APPEND gs_fcat TO gt_fcat.

  CLEAR gs_fcat.
  gs_fcat-col_pos = '21'.
  gs_fcat-fieldname = 'PBEGDA'.
  gs_fcat-tabname  = 'LT_OUTPUT'.
  gs_fcat-seltext_m  = 'POS ST Date'.
  APPEND gs_fcat TO gt_fcat.

***********ALV DISPLAY  *******************
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = sy-repid
      is_layout                = gs_layout
      it_fieldcat              = gt_fcat
      i_save                   = 'X'
      is_variant               = gt_variant
      i_callback_pf_status_set = 'SET_PF_STATUS'
      i_callback_user_command  = 'USER_COMMAND'
    TABLES
*     t_outtab                 = it_position_tab.
      t_outtab                 = lt_output.

FORM set_pf_status USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'POS_STATUS'.
ENDFORM.

FORM user_command USING R_ucomm TYPE sy-ucomm
                        r_selfield TYPE slis_selfield.
CASE r_ucomm.
WHEN '&REFRESH'.
r_selfield-refresh = 'X'.
r_selfield-col_stable = 'X'.
r_selfield-row_stable = 'X'.
ENDCASE.

ENDFORM.

FORM get_default_layout CHANGING cv_layout TYPE disvariant-variant.
  DATA:
    ls_variant TYPE disvariant.

  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
    EXPORTING
      i_save        = 'A'
    CHANGING
      cs_variant    = ls_variant
    EXCEPTIONS
      wrong_input   = 1
      not_found     = 2
      program_error = 3
      OTHERS        = 4.
  IF sy-subrc = 0.
    cv_layout = ls_variant-variant.
  ENDIF.
ENDFORM.

FORM select_alv_variant CHANGING cv_layout TYPE disvariant-variant.
  DATA:
    ls_variant TYPE disvariant.

  ls_variant-report = sy-repid.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant    = ls_variant
      i_save        = 'A'
    IMPORTING
      es_variant    = ls_variant
    EXCEPTIONS
      not_found     = 1
      program_error = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    cv_layout = ls_variant-variant.
  ENDIF.
ENDFORM.
