*&---------------------------------------------------------------------*
*& Report ZHR_EMPLOYEE_MASTER
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zhr_employee_master.

TABLES: pa0001,zhr_emp_details.

TABLES: sscrfields.

TYPES: BEGIN OF ty_output.
         INCLUDE STRUCTURE zhr_emp_details.
TYPES:   mgrposname     TYPE hrp1000-stext,
         mgrname        TYPE p0001-ename,
         per_email      TYPE p0105-usrid_long,
         per_mobile     TYPE p0105-usrid,
         emp_type       TYPE char1,
         emp_category   TYPE char1,
         emp_travel_all TYPE char1,
         job_desc       TYPE hrp1000-stext,
         latitude       TYPE zhrlatit,
         longitude      TYPE zhrlong,
         v_bankl        TYPE lfbk-bankl,
         v_bankn        TYPE lfbk-bankn,
         location       TYPE p9020-location,
         department     TYPE p9020-department,
         design         TYPE p9020-design,
         worklocation   TYPE p9020-worklocation,
         baselocation   TYPE p9020-baselocation,
         pillers        TYPE p9020-pillers,
         gr_kostl       TYPE p9001-kostl,
         greytmngid     TYPE p9021-greytmngid,
         greytmngnm     TYPE p0001-ename,
         codeextn       TYPE char1,
*         hasaddtnlpos   TYPE char1,
       END OF ty_output.

TYPES: BEGIN OF ty_hrp1001_cp,
         objid TYPE hrp1001-objid,
         sobid TYPE hrp1001-sobid,
       END OF ty_hrp1001_cp.

TYPES: BEGIN OF ty_hrp1001_obj,
         objid TYPE hrp1001-objid,
       END OF ty_hrp1001_obj.

DATA: lt_output1 TYPE TABLE OF ty_output,
      lt_output2 TYPE TABLE OF ty_output,
      ls_output1 TYPE ty_output.

DATA: lt_hrp1000 TYPE TABLE OF hrp1000,
      ls_hrp1000 TYPE hrp1000.

DATA : lt_hrp1001_cp TYPE TABLE OF ty_hrp1001_cp,
       lt_hrp1001_bp TYPE TABLE OF ty_hrp1001_cp.

DATA: ls_hrp1001_cp TYPE ty_hrp1001_cp.

DATA: lt_hrp1001_obj TYPE TABLE OF ty_hrp1001_obj,
      ls_hrp1001_obj TYPE ty_hrp1001_obj.

DATA: lt_lfbk TYPE TABLE OF lfbk,
      wa_lfbk TYPE lfbk.

DATA: lt_lfb1 TYPE TABLE OF lfb1,
      wa_lfb1 TYPE lfb1.

DATA: lt_pa9020 TYPE TABLE OF pa9020,
      wa_pa9020 TYPE pa9020.

DATA: lt_pa9021 TYPE TABLE OF pa9021,
      wa_pa9021 TYPE pa9021.

DATA: lt_pa9001 TYPE TABLE OF pa9001,
      wa_pa9001 TYPE pa9001.

TYPES: BEGIN OF ty_name,
         pernr TYPE p0001-pernr,
         ename TYPE p0001-ename,
         kostl TYPE p0001-kostl,
       END OF ty_name.

DATA: lt_name TYPE TABLE OF ty_name,
      ls_name TYPE ty_name.

TYPES: BEGIN OF ty_mphn,
         pernr      TYPE p0105-pernr,
         subty      TYPE p0105-subty,
         usrid      TYPE p0105-usrid,
         usrid_long TYPE p0105-usrid_long,
       END OF ty_mphn.

DATA: lt_mphn TYPE TABLE OF ty_mphn,
      ls_mphn TYPE ty_mphn.


DATA : lt_output     TYPE STANDARD TABLE OF zhr_emp_details,
       lt_output_tmp TYPE STANDARD TABLE OF zhr_emp_details.

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

DATA: lt_pernr TYPE p3pr_rt_pernr,
      ls_pernr TYPE p3pr_rs_pernr.

CONSTANTS: c_on  TYPE char1 VALUE '1',
           c_off TYPE char1 VALUE '0'.

CONSTANTS:
  BEGIN OF c_stat,
    open        TYPE char1 VALUE 'O',
    close       TYPE char1 VALUE 'C',
    close_w_val TYPE char1 VALUE 'V',
  END   OF c_stat.

DATA: v_stat_so  TYPE char1.   " O - Open, C - Closed, V - Closed with Value.

DATA: v_w_value  TYPE flag.


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS:     p_bukrs TYPE pa0001-bukrs." OBLIGATORY.
  SELECT-OPTIONS: so_pernr FOR pa0001-pernr MATCHCODE OBJECT prem NO INTERVALS.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-004.
  SELECT-OPTIONS: so_mgper FOR pa0001-pernr MATCHCODE OBJECT prem NO INTERVALS.
SELECTION-SCREEN END OF BLOCK b4.

SELECTION-SCREEN BEGIN OF BLOCK b6 WITH FRAME TITLE TEXT-006.
  SELECT-OPTIONS: so_join FOR pa0001-begda NO-EXTENSION.
SELECTION-SCREEN END OF BLOCK b6.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS:      p_dumm AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE TEXT-005.
  PARAMETERS:      p_addnl AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b5.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
  PARAMETERS p_layo LIKE disvariant-variant.
SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN PUSHBUTTON (4) pb_so USER-COMMAND u_so.
  SELECTION-SCREEN COMMENT 6(50) v_so.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN: BEGIN OF BLOCK blk2 WITH FRAME TITLE TEXT-t02.
  PARAMETERS: P_all  RADIOBUTTON GROUP rb1 MODIF ID so DEFAULT 'X',
              p_Cmis RADIOBUTTON GROUP rb1 MODIF ID so,
              P_cpc  RADIOBUTTON GROUP rb1 MODIF ID so,
              p_Cgc  RADIOBUTTON GROUP rb1 MODIF ID so,
              P_gcc  RADIOBUTTON GROUP rb1 MODIF ID so,
              p_bnk  RADIOBUTTON GROUP rb1 MODIF ID so,
              P_bvk  RADIOBUTTON GROUP rb1 MODIF ID so,
              P_vmis RADIOBUTTON GROUP rb1 MODIF ID so,
              p_mgrc RADIOBUTTON GROUP rb1 MODIF ID so,
              P_depm RADIOBUTTON GROUP rb1 MODIF ID so,
              P_addt RADIOBUTTON GROUP rb1 MODIF ID so,
              P_cext RADIOBUTTON GROUP rb1 MODIF ID so.
*              P_adps RADIOBUTTON GROUP rb1 MODIF ID so.
SELECTION-SCREEN: END   OF BLOCK blk2.


INITIALIZATION.

  v_so  = 'Additional Reporting Filter'.
  PERFORM f_set_initial_icons.

  PERFORM get_default_layout CHANGING p_layo.

  CLEAR: ls_restrict,
         ls_opt_list,
         ls_ass.

  REFRESH: ls_restrict-opt_list_tab,ls_restrict-ass_tab.


  CLEAR: ls_restrict,
         ls_opt_list,
         ls_ass.

  REFRESH: ls_restrict-opt_list_tab,ls_restrict-ass_tab.

  ls_opt_list-name       = 'EQUAL'.
  ls_opt_list-options-eq = 'X'.   " Restrict to equal
  APPEND ls_opt_list TO ls_restrict-opt_list_tab.

  ls_ass-kind            = 'S'.
  ls_ass-name            = 'SO_MGPER'. " select option which you want
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

  CASE sscrfields-ucomm.
    WHEN 'U_SO'.
      CLEAR v_w_value.
      v_w_value = 'X'.
      PERFORM f_set_stat USING v_w_value CHANGING v_stat_so.
    WHEN OTHERS.
  ENDCASE.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_layo.
  PERFORM select_alv_variant CHANGING p_layo.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    CASE screen-group1.
      WHEN 'SO'.
        IF v_stat_so = c_stat-open.   "'O'.
          screen-active = c_on.    "'1'.
        ELSE.
          screen-active = c_off.   "'0'.
        ENDIF.
    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.
  PERFORM f_set_icon USING v_stat_so CHANGING pb_so.

START-OF-SELECTION.

  REFRESH lt_output.

  REFRESH lt_pernr.
  IF so_pernr[] IS NOT INITIAL.
    LOOP AT so_pernr.
      CLEAR ls_pernr.

      ls_pernr-sign   = so_pernr-sign.
      ls_pernr-option = so_pernr-option.
      ls_pernr-low    = so_pernr-low.
      ls_pernr-high   = so_pernr-high.

      APPEND ls_pernr TO lt_pernr.
    ENDLOOP.

  ENDIF.

  CALL FUNCTION 'ZHR_EMP_DETAILS'
    EXPORTING
      in_bukrs          = p_bukrs
      in_pernr          = lt_pernr
    TABLES
      it_hr_emp_details = lt_output_tmp.

  IF lt_output_tmp[] IS NOT INITIAL.
    IF p_dumm = abap_true.
      DELETE lt_output_tmp WHERE dummy_emp = 'X'.
    ENDIF.

    IF so_join[] IS NOT INITIAL.
      DELETE lt_output_tmp WHERE dat01 NOT IN so_join.
    ENDIF.

  ENDIF.

  APPEND LINES OF lt_output_tmp TO lt_output.


  DATA: lt_position_tab TYPE TABLE OF zhr_postion_tab,
        ls_position_tab TYPE zhr_postion_tab.

  CALL FUNCTION 'ZHR_POSITION_DETAILS'
*   EXPORTING
*     POSITION          =
    TABLES
      it_position = lt_position_tab.



  IF lt_output[] IS NOT INITIAL.

    IF so_mgper[] IS NOT INITIAL.
      DELETE lt_output WHERE pernr_rep NOT IN so_mgper.
    ENDIF.
*to get object type descriptions
    SELECT * FROM hrp1000 INTO TABLE lt_hrp1000
      WHERE plvar = '01' AND  ( otype = 'S' OR otype = 'C' ) AND begda LE sy-datum AND endda GE sy-datum.
    IF sy-subrc = 0.
      SORT lt_hrp1000 BY otype objid.
    ENDIF.

*to get names
    SELECT pa0001~pernr pa0001~ename pa0001~kostl
        INTO TABLE lt_name FROM pa0000
        INNER JOIN pa0001 ON pa0000~pernr = pa0001~pernr
        WHERE pa0000~stat2 EQ '3'
        AND pa0001~endda GE sy-datum AND pa0000~endda GE sy-datum.
    IF sy-subrc = 0.
      SORT lt_name BY pernr.
    ENDIF.

*to check the bank details
    SELECT * FROM lfbk INTO TABLE lt_lfbk FOR ALL ENTRIES IN lt_output WHERE lifnr = lt_output-lifnr .
    IF sy-subrc = 0.
      SORT lt_lfbk BY lifnr.
    ENDIF.


*to check if the vendor is exednded for the respective company code
    SELECT * FROM lfb1 INTO TABLE lt_lfb1 FOR ALL ENTRIES IN lt_output WHERE lifnr = lt_output-lifnr AND bukrs = lt_output-bukrs.
    IF sy-subrc = 0.
      SORT lt_lfb1 BY lifnr bukrs.
    ENDIF.

    SELECT pernr subty usrid usrid_long FROM pa0105 INTO TABLE lt_mphn
      FOR ALL ENTRIES IN lt_output
      WHERE pernr = lt_output-pernr AND begda LE sy-datum AND endda GE sy-datum.
    IF sy-subrc = 0.
      SORT lt_mphn BY pernr subty.
    ENDIF.

    IF p_addnl IS NOT INITIAL.
      SELECT * FROM pa9020 INTO TABLE lt_pa9020 FOR ALL ENTRIES IN lt_output WHERE pernr = lt_output-pernr AND begda <= sy-datum AND endda >= sy-datum.
      IF sy-subrc = 0.
        SORT lt_pa9020 BY pernr.
      ENDIF.

      SELECT * FROM pa9021 INTO TABLE lt_pa9021 FOR ALL ENTRIES IN lt_output WHERE pernr = lt_output-pernr AND begda <= sy-datum AND endda >= sy-datum.
      IF sy-subrc = 0.
        SORT lt_pa9021 BY pernr.
      ENDIF.

      SELECT * FROM pa9001 INTO TABLE lt_pa9001 FOR ALL ENTRIES IN lt_output WHERE pernr = lt_output-pernr AND begda <= sy-datum AND endda >= sy-datum.
      IF sy-subrc = 0.
        SORT lt_pa9001 BY pernr.
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

*Personal email
      CLEAR ls_mphn.
      READ TABLE lt_mphn INTO ls_mphn WITH KEY pernr = ls_output1-pernr subty = '0030' BINARY SEARCH .
      IF sy-subrc = 0.
        ls_output1-per_email = ls_mphn-usrid_long.
      ENDIF.

*personal mobile phone
      CLEAR ls_mphn.
      READ TABLE lt_mphn INTO ls_mphn WITH KEY pernr = ls_output1-pernr subty = 'CELL' BINARY SEARCH .
      IF sy-subrc = 0.
        ls_output1-per_mobile = ls_mphn-usrid.
      ENDIF.


      CLEAR ls_position_tab.
      READ TABLE lt_position_tab INTO ls_position_tab WITH KEY objid = ls_output1-plans BINARY SEARCH.
      IF sy-subrc = 0.
        ls_output1-emp_type = ls_position_tab-emp_type.
        ls_output1-emp_category  = ls_position_tab-emp_category.
        ls_output1-emp_travel_all = ls_position_tab-emp_travel_all.
*        ls_output1-pos_costcent = ls_position_tab-kostl.
        ls_output1-latitude     = ls_position_tab-latitude.
        ls_output1-longitude    = ls_position_tab-longitude.
      ENDIF.

      READ TABLE lt_lfbk INTO wa_lfbk WITH KEY lifnr = ls_output1-lifnr BINARY SEARCH.
      IF sy-subrc = 0.
        ls_output1-v_bankl = wa_lfbk-bankl.
        ls_output1-v_bankn = wa_lfbk-bankn.
      ENDIF.

      CLEAR wa_lfb1.
      READ TABLE lt_lfb1 INTO wa_lfb1 WITH KEY lifnr = ls_output1-lifnr bukrs = ls_output1-bukrs BINARY SEARCH.
      IF sy-subrc = 0.
        ls_output1-codeextn = 'Y'.
      ELSE.
        ls_output1-codeextn = 'N'.
      ENDIF.
*         codeextn       TYPE char1,
*         hasaddtnlpos   TYPE char1,
      IF p_addnl IS NOT INITIAL.
*Get the Work detaisl from greythr downloaded data
        READ TABLE lt_pa9020 INTO wa_pa9020 WITH KEY pernr = ls_output1-pernr BINARY SEARCH.
        IF sy-subrc = 0.
          ls_output1-location     = wa_pa9020-location.
          ls_output1-department   = wa_pa9020-department.
          ls_output1-design       = wa_pa9020-design.
          ls_output1-worklocation = wa_pa9020-worklocation.
          ls_output1-baselocation = wa_pa9020-baselocation.
          ls_output1-pillers      = wa_pa9020-pillers.
*          ls_output1-gr_kostl     = wa_pa9020-kostl.
        ENDIF.

*Get the Manager ID downloaded from Greythr
        READ TABLE lt_pa9021 INTO wa_pa9021 WITH KEY pernr = ls_output1-pernr BINARY SEARCH.
        IF sy-subrc = 0.
          ls_output1-greytmngid = wa_pa9021-greytmngid.
          READ TABLE lt_name INTO ls_name WITH KEY pernr = ls_output1-greytmngid.
          IF sy-subrc = 0.
            ls_output1-greytmngnm = ls_name-ename.
          ENDIF.
        ENDIF.

*get the cost center downlaoded from greythr
        READ TABLE lt_pa9001 INTO wa_pa9001 WITH KEY pernr = ls_output1-pernr BINARY SEARCH.
        IF sy-subrc = 0.
          ls_output1-gr_kostl = wa_pa9001-kostl.
        ENDIF.
      ENDIF.

      MODIFY lt_output1 FROM ls_output1.

    ENDLOOP.

  ENDIF.

END-OF-SELECTION.

  lt_output2[] = lt_output1[].
  REFRESH lt_output1.

  IF p_all = abap_true.
    lt_output1[] = lt_output2[].
  ELSEIF p_cmis = abap_true.
    DELETE lt_output2 WHERE pos_kostl IS NOT INITIAL.
    lt_output1[] = lt_output2[].
  ELSEIF p_gcc = abap_true.
    DELETE lt_output2 WHERE gr_kostl IS NOT INITIAL.
    lt_output1[] = lt_output2[].
  ELSEIF p_vmis = abap_true.
    DELETE lt_output2 WHERE lifnr IS NOT INITIAL.
    lt_output1[] = lt_output2[].
  ELSEIF p_depm = abap_true.
    DELETE lt_output2 WHERE department IS NOT INITIAL.
    lt_output1[] = lt_output2[].
  ELSEIF p_cpc = abap_true OR p_cgc = abap_true OR p_bnk = abap_true OR p_bvk = abap_true OR p_mgrc = abap_true OR P_addt = abap_true.
    LOOP AT lt_output2 INTO ls_output1.

* comparing Postion cost center to employee cost center
      IF p_cpc = abap_true.
        IF ls_output1-pos_kostl NE ls_output1-emp_kostl.
          APPEND ls_output1 TO lt_output1.
        ENDIF.
      ENDIF.

* Comparing Position cost center to GreytHR costcenter
      IF p_cgc = abap_true.
        IF ls_output1-pos_kostl NE ls_output1-gr_kostl.
          APPEND ls_output1 TO lt_output1.
        ENDIF.
      ENDIF.

      IF P_bnk = abap_true.
        IF ls_output1-bankl IS INITIAL OR
           ls_output1-bankl = '913020026408306' OR
           ls_output1-v_bankl IS INITIAL OR
           ls_output1-v_bankl = '913020026408306'.
          APPEND ls_output1 TO lt_output1.
        ENDIF.
      ENDIF.

      IF p_bvk = abap_true.
        IF ls_output1-bankl <> ls_output1-v_bankl OR
           ls_output1-bankn <> ls_output1-v_bankn.
          APPEND ls_output1 TO lt_output1.
        ENDIF.
      ENDIF.

      IF p_mgrc = abap_true.
        IF ls_output1-pernr_rep <> ls_output1-greytmngid.
          APPEND ls_output1 TO lt_output1.
        ENDIF.
      ENDIF.

      IF P_addt = abap_true.
        IF ls_output1-location IS INITIAL OR
           ls_output1-department IS INITIAL OR
           ls_output1-design IS INITIAL OR
           ls_output1-worklocation IS INITIAL OR
           ls_output1-baselocation IS INITIAL OR
           ls_output1-pillers IS INITIAL OR
           ls_output1-gr_kostl IS INITIAL OR
           ls_output1-greytmngid IS INITIAL OR
           ls_output1-greytmngnm IS INITIAL.
          APPEND ls_output1 TO lt_output1.
        ENDIF.
      ENDIF.

    ENDLOOP.

  ELSEIF P_cext = abap_true.
    DELETE lt_output2 WHERE CODEEXTN = 'Y'.
    lt_output1[] = lt_output2[].
  ENDIF.


  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name     = sy-repid
      i_internal_tabname = 'LT_OUTPUT1'
      i_structure_name   = 'ZHR_EMP_DETAILS'
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
      WHEN 'DUMMY_EMP'.
        gs_fcat-seltext_m  = 'Dummy Emp'.
        gs_fcat-seltext_l  = 'Dummy Emp'.
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
      WHEN 'BANKL'.
        gs_fcat-seltext_m  = 'IFSC Code'.
        gs_fcat-seltext_l  = 'IFSC Code'.
      WHEN 'BANKN'.
        gs_fcat-seltext_m  = 'Bank Acc Number'.
        gs_fcat-seltext_l  = 'Bank Acc Number'.
      WHEN 'GREYTID'.
        gs_fcat-seltext_m  = 'GreyT ID'.
        gs_fcat-seltext_l  = 'GreyT ID'.
      WHEN 'POS_KOSTL'.
        gs_fcat-seltext_m  = 'Postion Cost Center'.
        gs_fcat-seltext_l  = 'Postion Cost Center'.
      WHEN 'EMP_KOSTL'.
        gs_fcat-seltext_m  = 'Employee Cost Center'.
        gs_fcat-seltext_l  = 'Employee Cost Center'.
      WHEN 'LIFNR'.
        gs_fcat-seltext_m  = 'Vendor Code'.
        gs_fcat-seltext_l  = 'Vendor Code'.
      WHEN OTHERS.
    ENDCASE.
    gs_fcat-seltext_l = gs_fcat-reptext_ddic = gs_fcat-seltext_m.
    CLEAR: gs_fcat-seltext_s, gs_fcat-ref_fieldname, gs_fcat-ref_tabname.
    MODIFY gt_fcat FROM gs_fcat INDEX sy-tabix.
  ENDLOOP.


  CLEAR gs_fcat.
  gs_fcat-col_pos = '40'.
  gs_fcat-fieldname = 'MGRPOSNAME'.
  gs_fcat-tabname  = 'LT_OUTPUT1'.
  gs_fcat-seltext_m  = 'Manager position'.
  APPEND gs_fcat TO gt_fcat.

  CLEAR gs_fcat.
  gs_fcat-col_pos = '41'.
  gs_fcat-fieldname = 'MGRNAME'.
  gs_fcat-tabname  = 'LT_OUTPUT1'.
  gs_fcat-seltext_m  = 'Manager Name'.
  APPEND gs_fcat TO gt_fcat.


  CLEAR gs_fcat.
  gs_fcat-col_pos = '42'.
  gs_fcat-fieldname = 'PER_EMAIL'.
  gs_fcat-outputlen = '30'.
  gs_fcat-tabname  = 'LT_OUTPUT1'.
  gs_fcat-seltext_m  = 'Pesonal Email'.
  APPEND gs_fcat TO gt_fcat.

  CLEAR gs_fcat.
  gs_fcat-col_pos = '43'.
  gs_fcat-fieldname = 'PER_MOBILE'.
  gs_fcat-tabname  = 'LT_OUTPUT1'.
  gs_fcat-seltext_m  = 'Personal Mobile'.
  APPEND gs_fcat TO gt_fcat.


  CLEAR gs_fcat.
  gs_fcat-col_pos = '44'.
  gs_fcat-fieldname = 'EMP_TYPE'.
  gs_fcat-tabname  = 'LT_OUTPUT1'.
  gs_fcat-seltext_m  = 'Emp Type'.
  APPEND gs_fcat TO gt_fcat.

  CLEAR gs_fcat.
  gs_fcat-col_pos = '45'.
  gs_fcat-fieldname = 'EMP_CATEGORY'.
  gs_fcat-tabname  = 'LT_OUTPUT1'.
  gs_fcat-seltext_m  = 'Emp Travel Cat'.
  APPEND gs_fcat TO gt_fcat.

  CLEAR gs_fcat.
  gs_fcat-col_pos = '46'.
  gs_fcat-fieldname = 'EMP_TRAVEL_ALL'.
  gs_fcat-tabname  = 'LT_OUTPUT1'.
  gs_fcat-seltext_m  = 'Emp Travel Allowed'.
  APPEND gs_fcat TO gt_fcat.

  CLEAR gs_fcat.
  gs_fcat-col_pos = '47'.
  gs_fcat-fieldname = 'JOB_DESC'.
  gs_fcat-tabname  = 'LT_OUTPUT1'.
  gs_fcat-seltext_m  = 'Job Description'.
  APPEND gs_fcat TO gt_fcat.

  CLEAR gs_fcat.
  gs_fcat-col_pos = '48'.
  gs_fcat-fieldname = 'LATITUDE'.
  gs_fcat-tabname  = 'LT_OUTPUT1'.
  gs_fcat-seltext_m  = 'Latitude'.
  APPEND gs_fcat TO gt_fcat.

  CLEAR gs_fcat.
  gs_fcat-col_pos = '49'.
  gs_fcat-fieldname = 'LONGITUDE'.
  gs_fcat-tabname  = 'LT_OUTPUT1'.
  gs_fcat-seltext_m  = 'Longitude'.
  APPEND gs_fcat TO gt_fcat.

  CLEAR gs_fcat.
  gs_fcat-col_pos = '50'.
  gs_fcat-fieldname = 'V_BANKL'.
  gs_fcat-tabname  = 'LT_OUTPUT1'.
  gs_fcat-seltext_m  = 'Ven_BKey'.
  APPEND gs_fcat TO gt_fcat.

  CLEAR gs_fcat.
  gs_fcat-col_pos = '51'.
  gs_fcat-fieldname = 'V_BANKN'.
  gs_fcat-tabname  = 'LT_OUTPUT1'.
  gs_fcat-seltext_m  = 'Ven_BNum'.
  APPEND gs_fcat TO gt_fcat.

  IF p_addnl IS NOT INITIAL.

    CLEAR gs_fcat.
    gs_fcat-col_pos = '52'.
    gs_fcat-fieldname = 'LOCATION'.
    gs_fcat-tabname  = 'LT_OUTPUT1'.
    gs_fcat-seltext_m  = 'G LOC'.
    APPEND gs_fcat TO gt_fcat.


    CLEAR gs_fcat.
    gs_fcat-col_pos = '53'.
    gs_fcat-fieldname = 'DEPARTMENT'.
    gs_fcat-tabname  = 'LT_OUTPUT1'.
    gs_fcat-seltext_m  = 'G Dept'.
    APPEND gs_fcat TO gt_fcat.

    CLEAR gs_fcat.
    gs_fcat-col_pos = '54'.
    gs_fcat-fieldname = 'DESIGN'.
    gs_fcat-tabname  = 'LT_OUTPUT1'.
    gs_fcat-seltext_m  = 'G Designation'.
    APPEND gs_fcat TO gt_fcat.

    CLEAR gs_fcat.
    gs_fcat-col_pos = '55'.
    gs_fcat-fieldname = 'WORKLOCATION'.
    gs_fcat-tabname  = 'LT_OUTPUT1'.
    gs_fcat-seltext_m  = 'G WK Loc'.
    APPEND gs_fcat TO gt_fcat.

    CLEAR gs_fcat.
    gs_fcat-col_pos = '56'.
    gs_fcat-fieldname = 'BASELOCATION'.
    gs_fcat-tabname  = 'LT_OUTPUT1'.
    gs_fcat-seltext_m  = 'G BS LOC'.
    APPEND gs_fcat TO gt_fcat.

    CLEAR gs_fcat.
    gs_fcat-col_pos = '57'.
    gs_fcat-fieldname = 'PILLERS'.
    gs_fcat-tabname  = 'LT_OUTPUT1'.
    gs_fcat-seltext_m  = 'G Piller'.
    APPEND gs_fcat TO gt_fcat.

    CLEAR gs_fcat.
    gs_fcat-col_pos = '58'.
    gs_fcat-fieldname = 'GR_KOSTL'.
    gs_fcat-tabname  = 'LT_OUTPUT1'.
    gs_fcat-seltext_m  = 'G GR CCTR'.
    APPEND gs_fcat TO gt_fcat.

    CLEAR gs_fcat.
    gs_fcat-col_pos = '59'.
    gs_fcat-fieldname = 'GREYTMNGID'.
    gs_fcat-tabname  = 'LT_OUTPUT1'.
    gs_fcat-seltext_m  = 'G MGR ID'.
    APPEND gs_fcat TO gt_fcat.

    CLEAR gs_fcat.
    gs_fcat-col_pos = '60'.
    gs_fcat-fieldname = 'GREYTMNGNM'.
    gs_fcat-tabname  = 'LT_OUTPUT1'.
    gs_fcat-seltext_m  = 'G MGR Name'.
    APPEND gs_fcat TO gt_fcat.

    CLEAR gs_fcat.
    gs_fcat-col_pos = '61'.
    gs_fcat-fieldname = 'CODEEXTN'.
    gs_fcat-tabname  = 'LT_OUTPUT1'.
    gs_fcat-seltext_m  = 'BP COMP EXTN'.
    APPEND gs_fcat TO gt_fcat.

*    CLEAR gs_fcat.
*    gs_fcat-col_pos = '62'.
*    gs_fcat-fieldname = 'HASADDTNLPOS'.
*    gs_fcat-tabname  = 'LT_OUTPUT1'.
*    gs_fcat-seltext_m  = 'Addtnl POS'.
*    APPEND gs_fcat TO gt_fcat.
  ENDIF.


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
*&---------------------------------------------------------------------*
*& Form f_set_initial_icons
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_set_initial_icons .

  MOVE c_stat-close TO: v_stat_so.

  PERFORM f_set_icon USING v_stat_so CHANGING pb_so.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_set_icon
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> V_STAT_SO
*&      <-- PB_SO
*&---------------------------------------------------------------------*
FORM f_set_icon USING       iv_stat TYPE char1
                CHANGING    cv_icon TYPE any.

  CASE iv_stat.
    WHEN c_stat-close.        cv_icon =  icon_data_area_expand.
    WHEN c_stat-open.         cv_icon =  icon_data_area_collapse.
    WHEN c_stat-close_w_val.  cv_icon =  icon_view_create. " ICON_status_best.
  ENDCASE.

ENDFORM.

FORM f_set_stat USING    iv_w_value TYPE flag
                CHANGING cv_stat    TYPE char1.

  IF cv_stat = c_stat-open.
    IF iv_w_value IS INITIAL.
      cv_stat = c_stat-close.
    ELSE.
      cv_stat = c_stat-close_w_val.
    ENDIF.
  ELSEIF ( cv_stat = c_stat-close
       OR  cv_stat = c_stat-close_w_val ).
    cv_stat = c_stat-open.
  ENDIF.

ENDFORM.                    "f_set_Stat
