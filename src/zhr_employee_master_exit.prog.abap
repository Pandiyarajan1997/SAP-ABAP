*&---------------------------------------------------------------------*
*& Report ZHR_EMPLOYEE_MASTER
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zhr_employee_master_exit.

TABLES: pa0001,zhr_emp_details.

TYPES: BEGIN OF ty_output.
         INCLUDE STRUCTURE ZHR_EMP_DETAILS_EXIT.
TYPES:   mgrposname     TYPE hrp1000-stext,
         mgrname        TYPE p0001-ename,
         Dummyid        TYPE xfeld,
         PER_email      TYPE p0105-usrid_long,
         per_mobile     TYPE p0105-usrid,
         costcenter     TYPE p0001-kostl,
         emp_type       TYPE char1,
         emp_category   TYPE char1,
         emp_travel_all TYPE char1,
         emp_greyid     TYPE char10,
         job_desc       TYPE hrp1000-stext,
         cp_no          TYPE hrp1001-objid,
         bp_no          TYPE hrp1001-sobid,
       END OF ty_output.

TYPES: BEGIN OF ty_hrp1001_cp,
         objid TYPE hrp1001-objid,
         sobid TYPE hrp1001-sobid,
       END OF ty_hrp1001_cp.

TYPES: BEGIN OF ty_hrp1001_obj,
         objid TYPE hrp1001-objid,
       END OF ty_hrp1001_obj.

DATA: lt_output1 TYPE TABLE OF ty_output,
      ls_output1 TYPE ty_output.

DATA: lt_hrp1000 TYPE TABLE OF hrp1000,
      ls_hrp1000 TYPE hrp1000.

DATA : lt_hrp1001_cp TYPE TABLE OF ty_hrp1001_cp,
       lt_hrp1001_bp TYPE TABLE OF ty_hrp1001_cp.

DATA: ls_hrp1001_cp TYPE ty_hrp1001_cp.

DATA: lt_hrp1001_obj TYPE TABLE OF ty_hrp1001_obj,
      ls_hrp1001_obj TYPE ty_hrp1001_obj.



TYPES: BEGIN OF ty_name,
         pernr TYPE p0001-pernr,
         ename TYPE p0001-ename,
         kostl TYPE p0001-kostl,
       END OF ty_name.

DATA: lt_name TYPE TABLE OF ty_name,
      ls_name TYPE ty_name.

TYPES: BEGIN OF ty_Mphn,
         pernr      TYPE p0105-pernr,
         subty      TYPE p0105-subty,
         usrid      TYPE p0105-usrid,
         usrid_long TYPE p0105-usrid_long,
       END OF ty_mphn.

DATA: lt_mphn TYPE TABLE OF ty_mphn,
      ls_mphn TYPE ty_mphn.

DATA : lt_output     TYPE STANDARD TABLE OF ZHR_EMP_DETAILS_EXIT,
       lt_output_tmp TYPE STANDARD TABLE OF ZHR_EMP_DETAILS_EXIT.

DATA: lv_pernr TYPE pernr_d.

DATA: gs_layout TYPE  slis_layout_alv,
* Internal Table Declaration for FieldCatlog
      gt_fcat   TYPE  slis_t_fieldcat_alv,
* Work Area Declaration for FieldCatlog
      gs_fcat   TYPE slis_fieldcat_alv.

DATA: gt_variant TYPE disvariant.

DATA: ls_restrict TYPE sscr_restrict,
      ls_opt_list TYPE sscr_opt_list,
      ls_ass      TYPE sscr_ass.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS:     p_bukrs TYPE pa0001-bukrs." OBLIGATORY.
  SELECT-OPTIONS: SO_pernr FOR pa0001-pernr MATCHCODE OBJECT prem NO INTERVALS.
  PARAMETERS    : P_Prevp TYPE char1.
SELECTION-SCREEN END OF BLOCK b1.

*SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-004.
*  SELECT-OPTIONS: so_mgper FOR pa0001-pernr MATCHCODE OBJECT prem NO INTERVALS.
*SELECTION-SCREEN END OF BLOCK b4.

*SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
*  PARAMETERS:      p_dumm AS CHECKBOX.
*SELECTION-SCREEN END OF BLOCK b2.


SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
  PARAMETERS p_layo LIKE disvariant-variant.
SELECTION-SCREEN END OF BLOCK b3.


INITIALIZATION.
  PERFORM get_default_layout CHANGING p_layo.

  CLEAR: ls_restrict,
         ls_opt_list,
         ls_ass.

  REFRESH: ls_restrict-opt_list_tab,ls_restrict-ass_tab.

  ls_opt_list-name       = 'EQUAL'.
  ls_opt_list-options-eq = 'X'.   " Restrict to equal
  APPEND ls_opt_list TO ls_restrict-opt_list_tab.

  ls_ass-kind            = 'S'.
  ls_ass-name            = 'SO_PERNR'. " select option which you want
  "to restrict
  ls_ass-sg_main         = 'I'.
  ls_ass-sg_addy         = ''.
  ls_ass-op_main         = 'EQUAL'.
  APPEND ls_ass TO ls_restrict-ass_tab.

*** To apply restrictions to select option
  CALL FUNCTION 'SELECT_OPTIONS_RESTRICT'
    EXPORTING
      restriction            = ls_restrict
    EXCEPTIONS
      too_late               = 1
      repeated               = 2
      selopt_without_options = 3
      selopt_without_signs   = 4
      invalid_sign           = 5
      empty_option_list      = 6
      invalid_kind           = 7
      repeated_kind_a        = 8
      OTHERS                 = 9.

  IF sy-subrc NE 0.
    WRITE:'error'.
  ENDIF.

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

  REFRESH lt_output.

****Function module to see employee details  ******
  REFRESH: lt_output_tmp.

  CLEAR lv_pernr.

  DATA: lt_pernr_r TYPE p3pr_rt_pernr.

  REFRESH lt_pernr_r.
  IF so_pernr[] IS NOT INITIAL.
    lt_pernr_r[] = so_pernr[].
  ENDIF.

  CALL FUNCTION 'ZHR_EMP_DETAILS_EXIT'
    EXPORTING
      in_bukrs          = p_bukrs
      in_pernr          = lt_pernr_r
      in_prevp          = P_Prevp
    TABLES
      it_hr_emp_details = lt_output.

* Call the second function module also for display in output

  DATA: lt_pernr_cat TYPE TABLE OF zhr_pernr_type,
        wa_pernr_cat TYPE zhr_pernr_type.

  CALL FUNCTION 'ZHR_EMP_TYPE_CAT'
* EXPORTING
*   IN_BUKRS        =
*   IN_PERNR        =
    TABLES
      it_output = lt_pernr_cat.

  SORT lt_pernr_cat BY pernr.

  IF lt_output[] IS NOT INITIAL.

    SELECT * FROM hrp1000 INTO TABLE lt_hrp1000
      WHERE plvar = '01' AND  ( otype = 'S' OR otype = 'C' ) AND begda LE sy-datum AND endda GE sy-datum.
    IF sy-subrc = 0.
      SORT lt_hrp1000 BY otype Objid.
    ENDIF.

    SELECT pernr ename kostl
        INTO TABLE lt_name from pa0001
        WHERE begda LE sy-datum AND endda GE sy-datum.
*    SELECT pa0001~pernr pa0001~ename pa0001~kostl
*        INTO TABLE lt_name FROM pa0000
*        INNER JOIN pa0001 ON pa0000~pernr = pa0001~pernr
*        WHERE pa0000~stat2 EQ '3'
*        AND pa0001~begda LE sy-datum AND pa0000~endda GE sy-datum.
    IF sy-subrc = 0.
      SORT lt_name BY pernr.
    ENDIF.


    SELECT pernr subty usrid usrid_long FROM pa0105 INTO TABLE lt_mphn
      FOR ALL ENTRIES IN lt_output
      WHERE pernr = lt_output-pernr AND begda LE sy-datum AND endda GE sy-datum.
    IF sy-subrc = 0.
      SORT lt_mphn BY pernr subty.
    ENDIF.

*Get CP based on pernr
    SELECT objid sobid
      FROM hrp1001
      INTO TABLE lt_hrp1001_cp
      FOR ALL ENTRIES IN lt_output
      WHERE otype = 'P' AND Objid = lt_output-pernr
      AND plvar = '01' AND rsign = 'A'
      AND relat = '209' AND istat = '1'
      AND begda LE sy-datum
      AND endda GE sy-datum
      AND sclas = 'CP'.
    IF sy-subrc = 0.

      SORT lt_hrp1001_cp BY objid.

      REFRESH lt_hrp1001_bp.
      LOOP AT lt_hrp1001_cp INTO ls_hrp1001_cp.
        CLEAR ls_hrp1001_obj.
        ls_hrp1001_obj-objid = ls_hrp1001_cp-sobid+0(8).
        APPEND ls_hrp1001_obj TO lt_hrp1001_obj.
      ENDLOOP.

      IF lt_hrp1001_obj[] IS NOT INITIAL.
        SORT lt_hrp1001_obj BY Objid.
        DELETE ADJACENT DUPLICATES FROM lt_hrp1001_obj COMPARING Objid.
*Get CP based on pernr
        SELECT objid sobid
          FROM hrp1001
          INTO TABLE lt_hrp1001_bp
          FOR ALL ENTRIES IN lt_hrp1001_obj
          WHERE otype = 'CP' AND Objid = lt_hrp1001_obj-objid
          AND plvar = '01' AND rsign = 'B'
          AND relat = '207' AND istat = '1'
          AND begda LE sy-datum
          AND endda GE sy-datum
          AND sclas = 'BP'.
      ENDIF.

    ENDIF.

    REFRESH lt_output1.
    lt_output1[] = lt_output[].

    LOOP AT lt_output1 INTO ls_output1.

* Manager position description
      CLEAR ls_hrp1000.
      READ TABLE lt_hrp1000 INTO ls_hrp1000 WITH KEY otype = 'S' objid = ls_output1-sobid BINARY SEARCH.
      IF sy-subrc = 0.
        ls_output1-mgrposname = ls_hrp1000-stext.
      ENDIF.

* Manager position description
      CLEAR ls_hrp1000.
      READ TABLE lt_hrp1000 INTO ls_hrp1000 WITH KEY otype = 'C' objid = ls_output1-stell BINARY SEARCH.
      IF sy-subrc = 0.
        ls_output1-job_desc = ls_hrp1000-stext.
      ENDIF.

* manager name
      READ TABLE lt_name INTO ls_name WITH KEY pernr = ls_output1-pernr_rep BINARY SEARCH.
      IF sy-subrc = 0.
        ls_output1-mgrname = ls_name-ename.
      ENDIF.

* Assigned costcenter name
      READ TABLE lt_name INTO ls_name WITH KEY pernr = ls_output1-pernr BINARY SEARCH.
      IF sy-subrc = 0.
        ls_output1-costcenter = ls_name-kostl.
      ENDIF.

*dummy pernr indicator
      READ TABLE lt_mphn WITH KEY pernr = ls_output1-pernr subty = 'MPHN' TRANSPORTING NO FIELDS BINARY SEARCH.
      IF sy-subrc = 0.
        ls_output1-dummyid = 'Y'.
      ENDIF.

*Personal email
      CLEAR ls_mphn.
      READ TABLE lt_mphn INTO ls_mphn WITH KEY pernr = ls_output1-pernr subty = '0030' BINARY SEARCH .
      IF sy-subrc = 0.
        ls_output1-PER_email = ls_mphn-usrid_long.
      ENDIF.

*personal mobile phone
      CLEAR ls_mphn.
      READ TABLE lt_mphn INTO ls_mphn WITH KEY pernr = ls_output1-pernr subty = 'CELL' BINARY SEARCH .
      IF sy-subrc = 0.
        ls_output1-PER_mobile = ls_mphn-usrid.
      ENDIF.

      CLEAR wa_pernr_cat.
      READ TABLE lt_pernr_cat INTO wa_pernr_cat WITH KEY pernr = ls_output1-pernr BINARY SEARCH.
      IF sy-subrc = 0.
        ls_output1-emp_type = wa_pernr_cat-emp_type.
        ls_output1-emp_category  = wa_pernr_cat-emp_category.
        ls_output1-emp_travel_all = wa_pernr_cat-emp_travel_all.
        ls_output1-emp_greyid = wa_pernr_cat-emp_greyid.
      ENDIF.

      CLEAR ls_hrp1001_cp.
      READ TABLE lt_hrp1001_cp INTO ls_hrp1001_cp WITH KEY objid = ls_output1-pernr BINARY SEARCH.
      IF sy-subrc = 0.
        ls_output1-cp_no = ls_hrp1001_cp-sobid+0(8).

        CLEAR ls_hrp1001_cp.
        READ TABLE lt_hrp1001_bp INTO ls_hrp1001_cp WITH KEY objid = ls_output1-cp_no.
        IF sy-subrc = 0.
          ls_output1-bp_no = ls_hrp1001_cp-sobid.
        ENDIF.
      ENDIF.

      MODIFY lt_output1 FROM ls_output1.

    ENDLOOP.

  ENDIF.

END-OF-SELECTION.

*  IF p_dumm = abap_true.
*    IF lt_output1[] IS NOT INITIAL.
*      DELETE lt_output1 WHERE dummyid = 'Y'.
*    ENDIF.
*  ENDIF.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name     = sy-repid
      i_internal_tabname = 'LT_OUTPUT1'
      i_structure_name   = 'ZHR_EMP_DETAILS_EXIT'
      i_inclname         = sy-repid
    CHANGING
      ct_fieldcat        = gt_fcat.

*Adjust Field Names
  LOOP AT gt_fcat INTO gs_fcat.

    CASE gs_fcat-fieldname.
      WHEN 'PERNR'.
        gs_fcat-seltext_m  = 'Emp No.'.
        gs_fcat-seltext_l  = 'Emp No.'.
      WHEN 'SNAME'.
        gs_fcat-seltext_m  = 'Name'.
        gs_fcat-seltext_l  = 'Name'.
      WHEN 'DAT01'.
        gs_fcat-seltext_m  = 'DOJ'.
        gs_fcat-seltext_l  = 'DOJ'.
      WHEN 'EXIT_DATE'.
        gs_fcat-seltext_m  = 'DOE'.
        gs_fcat-seltext_l  = 'DOE'.
      WHEN 'ORGEH'.
        gs_fcat-seltext_m  = 'Org Unit'.
        gs_fcat-seltext_l  = 'Org Unit'.
      WHEN 'ORGTX'.
        gs_fcat-seltext_m  = 'Org Desc'.
        gs_fcat-seltext_l  = 'Org Desc'.
      WHEN 'PLANS'.
        gs_fcat-seltext_m  = 'Position'.
        gs_fcat-seltext_l  = 'Position'.
      WHEN 'PLSTX'.
        gs_fcat-seltext_m  = 'Position  Short Desc'.
        gs_fcat-seltext_l  = 'Position  Short Desc'.
        gs_fcat-no_out = 'X'.
      WHEN 'SHORT_PL'.
        gs_fcat-seltext_m  = 'Pos Short Id'.
        gs_fcat-seltext_l  = 'Pos Short Id'.
        gs_fcat-no_out = 'X'.
      WHEN 'STEXT_PL'.
        gs_fcat-seltext_m  = 'Position Long Desc'.
        gs_fcat-seltext_l  = 'Position Long Desc'.
      WHEN 'PERSK'.
        gs_fcat-seltext_m  = 'Emp Grp'.
        gs_fcat-seltext_l  = 'Emp Grp'.
      WHEN 'PTEXT'.
        gs_fcat-seltext_m  = 'Emp Grp Desc'.
        gs_fcat-seltext_l  = 'Emp Grp Desc'.
      WHEN 'STELL'.
        gs_fcat-seltext_m  = 'Job Code'.
        gs_fcat-seltext_l  = 'Job Code'.
        gs_fcat-no_out = 'X'.
      WHEN 'BTRTL'.
        gs_fcat-seltext_m  = 'Per SA'.
        gs_fcat-seltext_l  = 'Per SA'.
      WHEN 'BTEXT'.
        gs_fcat-seltext_m  = 'Per SA Desc'.
        gs_fcat-seltext_l  = 'Per SA Desc'.
      WHEN 'GBDAT'.
        gs_fcat-seltext_m  = 'DOB'.
        gs_fcat-seltext_l  = 'DOB'.
      WHEN 'SOBID'.
        gs_fcat-seltext_m  = 'Mgr. Pos'.
        gs_fcat-seltext_l  = 'Mgr. Pos'.
      WHEN 'PERNR_REP'.
        gs_fcat-seltext_m  = 'Mgr Emp No'.
        gs_fcat-seltext_l  = 'Mgr Emp No'.
      WHEN 'NATIO'.
        gs_fcat-seltext_m  = 'Nationality'.
        gs_fcat-seltext_l  = 'Nationality'.
      WHEN 'GESCH'.
        gs_fcat-seltext_m  = 'Gender'.
        gs_fcat-seltext_l  = 'Gender'.
        gs_fcat-no_out = 'X'.
      WHEN 'STRAS'.
        gs_fcat-seltext_m  = 'Address1'.
        gs_fcat-seltext_l  = 'Address1'.
        gs_fcat-no_out = 'X'.
      WHEN 'LOCAT'.
        gs_fcat-seltext_m  = 'Address2'.
        gs_fcat-seltext_l  = 'Address2'.
        gs_fcat-no_out = 'X'.
      WHEN 'ORT01'.
        gs_fcat-seltext_m  = 'City'.
        gs_fcat-seltext_l  = 'Postal Code'.
        gs_fcat-no_out = 'X'.
      WHEN 'PSTLZ'.
        gs_fcat-seltext_m  = 'Postal Code'.
        gs_fcat-seltext_l  = 'Postal Code'.
        gs_fcat-no_out = 'X'.
      WHEN 'BLAND'.
        gs_fcat-seltext_m  = 'Region Code'.
        gs_fcat-seltext_l  = 'Region Code'.
        gs_fcat-no_out = 'X'.
      WHEN 'BEZEI'.
        gs_fcat-seltext_m  = 'Region Desc'.
        gs_fcat-seltext_l  = 'Region Desc'.
        gs_fcat-no_out = 'X'.
      WHEN 'LAND1'.
        gs_fcat-seltext_m  = 'Country Code'.
        gs_fcat-seltext_l  = 'Country Code'.
        gs_fcat-no_out = 'X'.
      WHEN 'USRID_LONG'.
        gs_fcat-seltext_m  = 'Official Email'.
        gs_fcat-seltext_l  = 'Official Email'.
        gs_fcat-outputlen = '30'.
      WHEN 'USRID'.
        gs_fcat-seltext_m  = 'Official Mobile'.
        gs_fcat-seltext_l  = 'Official Mobile'.
      WHEN 'TEXT_TLINE'.
        gs_fcat-seltext_m  = 'Job Desc'.
        gs_fcat-seltext_l  = 'Job Desc'.
        gs_fcat-tech = 'X'.
        gs_fcat-no_out = 'X'.
      WHEN 'ORIGMGREMP'.
        gs_fcat-seltext_m  = 'ORG MGR EMP'.
        gs_fcat-seltext_l  = 'ORG MGR EMP'.
      WHEN 'ORIGMGRPOS'.
        gs_fcat-seltext_m  = 'ORG MGR POS'.
        gs_fcat-seltext_l  = 'ORG MGR POS'.
      WHEN 'ORIGMGPOSDES'.
        gs_fcat-seltext_m  = 'ORG MGR POS Desc'.
        gs_fcat-seltext_l  = 'ORG MGR POS Desc'.
      WHEN OTHERS.
    ENDCASE.
    gs_fcat-seltext_l = gs_fcat-reptext_ddic = gs_fcat-seltext_m.
    CLEAR: gs_fcat-seltext_s, gs_fcat-ref_fieldname, gs_fcat-ref_tabname.
    MODIFY gt_fcat FROM gs_fcat INDEX sy-tabix.
  ENDLOOP.


  CLEAR gs_fcat.
  gs_fcat-col_pos = '35'.
  gs_fcat-fieldname = 'MGRPOSNAME'.
  gs_fcat-tabname  = 'LT_OUTPUT1'.
  gs_fcat-seltext_m  = 'Manager position'.
  APPEND gs_fcat TO gt_fcat.

  CLEAR gs_fcat.
  gs_fcat-col_pos = '36'.
  gs_fcat-fieldname = 'MGRNAME'.
  gs_fcat-tabname  = 'LT_OUTPUT1'.
  gs_fcat-seltext_m  = 'Manager Name'.
  APPEND gs_fcat TO gt_fcat.

  CLEAR gs_fcat.
  gs_fcat-col_pos = '37'.
  gs_fcat-fieldname = 'DUMMYID'.
  gs_fcat-tabname  = 'LT_OUTPUT1'.
  gs_fcat-seltext_m  = 'Dummy Emp Code'.
  gs_fcat-no_out = 'X'.
  APPEND gs_fcat TO gt_fcat.

  CLEAR gs_fcat.
  gs_fcat-col_pos = '38'.
  gs_fcat-fieldname = 'PER_EMAIL'.
  gs_fcat-outputlen = '30'.
  gs_fcat-tabname  = 'LT_OUTPUT1'.
  gs_fcat-seltext_m  = 'Pesonal Email'.
  APPEND gs_fcat TO gt_fcat.

  CLEAR gs_fcat.
  gs_fcat-col_pos = '39'.
  gs_fcat-fieldname = 'PER_MOBILE'.
  gs_fcat-tabname  = 'LT_OUTPUT1'.
  gs_fcat-seltext_m  = 'Personal Mobile'.
  APPEND gs_fcat TO gt_fcat.

  CLEAR gs_fcat.
  gs_fcat-col_pos = '40'.
  gs_fcat-fieldname = 'COSTCENTER'.
  gs_fcat-tabname  = 'LT_OUTPUT1'.
  gs_fcat-seltext_m  = 'Costcenter'.
  gs_fcat-no_out = 'X'.
  APPEND gs_fcat TO gt_fcat.

  CLEAR gs_fcat.
  gs_fcat-col_pos = '41'.
  gs_fcat-fieldname = 'EMP_TYPE'.
  gs_fcat-tabname  = 'LT_OUTPUT1'.
  gs_fcat-seltext_m  = 'Emp Type'.
  APPEND gs_fcat TO gt_fcat.

  CLEAR gs_fcat.
  gs_fcat-col_pos = '42'.
  gs_fcat-fieldname = 'EMP_CATEGORY'.
  gs_fcat-tabname  = 'LT_OUTPUT1'.
  gs_fcat-seltext_m  = 'Emp Travel Cat'.
  APPEND gs_fcat TO gt_fcat.

  CLEAR gs_fcat.
  gs_fcat-col_pos = '43'.
  gs_fcat-fieldname = 'EMP_TRAVEL_ALL'.
  gs_fcat-tabname  = 'LT_OUTPUT1'.
  gs_fcat-seltext_m  = 'Emp Travel Allowed'.
  APPEND gs_fcat TO gt_fcat.

  CLEAR gs_fcat.
  gs_fcat-col_pos = '44'.
  gs_fcat-fieldname = 'EMP_GREYID'.
  gs_fcat-tabname  = 'LT_OUTPUT1'.
  gs_fcat-seltext_m  = 'Emp Greyt ID'.
  gs_fcat-no_out = 'X'.
  APPEND gs_fcat TO gt_fcat.

  CLEAR gs_fcat.
  gs_fcat-col_pos = '45'.
  gs_fcat-fieldname = 'JOB_DESC'.
  gs_fcat-tabname  = 'LT_OUTPUT1'.
  gs_fcat-seltext_m  = 'Job Description'.
  APPEND gs_fcat TO gt_fcat.

  CLEAR gs_fcat.
  gs_fcat-col_pos = '46'.
  gs_fcat-fieldname = 'CP_NO'.
  gs_fcat-tabname  = 'LT_OUTPUT1'.
  gs_fcat-seltext_m  = 'CP Number'.
  APPEND gs_fcat TO gt_fcat.

  CLEAR gs_fcat.
  gs_fcat-col_pos = '47'.
  gs_fcat-fieldname = 'BP_NO'.
  gs_fcat-tabname  = 'LT_OUTPUT1'.
  gs_fcat-seltext_m  = 'BP Number'.
  APPEND gs_fcat TO gt_fcat.

  gs_layout-colwidth_optimize = 'X'.

***********ALV DISPLAY  *******************
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      is_layout          = gs_layout
      it_fieldcat        = gt_fcat
      i_save             = 'X'
      is_variant         = gt_variant
    TABLES
      t_outtab           = lt_output1.


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
