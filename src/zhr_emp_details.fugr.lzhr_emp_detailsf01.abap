*----------------------------------------------------------------------*
***INCLUDE LZHR_EMP_DETAILSF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form relationship_determine
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_SOBID
*&      <-- LS_HIERARCHY
*&---------------------------------------------------------------------*
FORM relationship_determine  USING  p_lv_parvw TYPE parvw
                                    p_lv_par_unc TYPE char02
                                    p_lv_sobid TYPE sobid
                             CHANGING p_ls_hierarchy TYPE zhr_so_to_top
                                      p_lv_msg.
  DATA: lt_objects   TYPE TABLE OF hrobject,
        lv_parvw_new TYPE parvw.

  IF p_lv_sobid IS NOT INITIAL.
    REFRESH: lt_objects.
    CALL FUNCTION 'RH_GET_LEADING_POSITION'
      EXPORTING
        plvar             = '01'
        otype             = 'S'
        sobid             = p_lv_sobid
        date              = sy-datum
      TABLES
        leading_pos       = lt_objects
      EXCEPTIONS
        no_lead_pos_found = 1
        OTHERS            = 2.
    IF sy-subrc = 0.
      DATA(lv_objects) = VALUE #( lt_objects[ 1 ]-objid OPTIONAL ).
      SELECT SINGLE sobid FROM hrp1001
           INTO @DATA(l_jobcode)
           WHERE otype = 'S'
           AND objid = @lv_objects
           AND plvar = '01'
           AND rsign = 'B'
           AND relat = '007'
           AND endda GE @sy-datum
           AND begda LE @sy-datum
           AND sclas = 'C'.
      IF sy-subrc EQ 0.
** Sales Officer Job Code is in Correct Category or not Checks **
        SELECT SINGLE * FROM zhr_jobcode
        INTO @DATA(ls_jobcode)
        WHERE parvw = @p_lv_parvw
        AND stell = @l_jobcode.
        IF sy-subrc EQ 0.
** Position Text
          SELECT SINGLE stext FROM hrp1000 INTO @DATA(l_stext)
            WHERE otype = 'S'
            AND plvar = '01'
            AND objid = @lv_objects
            AND begda LE @sy-datum
            AND endda GE @sy-datum.
          IF sy-subrc EQ 0.

          ENDIF.
** Assigned Employee Number **
          SELECT SINGLE sobid FROM hrp1001
            INTO @DATA(lv_pernr)
            WHERE otype = 'S'
            AND objid = @lv_objects
            AND plvar = '01'
            AND rsign = 'A'
            AND relat = '008'
            AND endda GE @sy-datum
            AND begda LE @sy-datum
            AND sclas = 'P'.
          IF sy-subrc EQ 0.
            SELECT SINGLE ename FROM pa0001 INTO @DATA(l_name)
              WHERE pernr = @lv_pernr
              AND begda LE @sy-datum
              AND endda GE @sy-datum.
          ELSE.
            CASE p_lv_parvw.
              WHEN 'L3'.
                lv_pernr = '9003002'.
                l_name = 'VACANT CM'.
              WHEN 'L2'.
                lv_pernr = '9003003'.
                l_name = 'VACANT CGM'.
              WHEN 'ZS'.
                lv_pernr = '9003004'.
                l_name = 'VACANT CVH'.
              WHEN 'L1'.
                lv_pernr = '9005599'.
                l_name = 'VACANT NSM'.
              WHEN 'PW'.
                lv_pernr = '9003005'.
                l_name = 'VACANT ZCGM'.
            ENDCASE.
          ENDIF.
** Sales Officer Business Partner Number **
          SELECT SINGLE sobid FROM hrp1001
            INTO @DATA(lv_bpartner)
            WHERE otype = 'S'
            AND objid = @lv_objects
            AND plvar = '01'
            AND rsign = 'A'
            AND relat = '008'
            AND endda GE @sy-datum
            AND begda LE @sy-datum
            AND sclas = 'BP'.
          IF sy-subrc EQ 0.

            CLEAR lv_parvw_new.
            CASE p_lv_parvw.
              WHEN 'L3'.
                lv_parvw_new = 'XL'.
              WHEN 'L2'.
                lv_parvw_new = 'XK'.
              WHEN 'ZS'.
                lv_parvw_new = 'YD'.
              WHEN 'L1'.
                lv_parvw_new = 'XJ'.
              WHEN 'PW'.
                lv_parvw_new = 'XS'.
            ENDCASE.
            p_ls_hierarchy-parvw_new  = lv_parvw_new.
            p_ls_hierarchy-plans      = lv_objects.
            p_ls_hierarchy-stext      = l_stext.
            p_ls_hierarchy-parvw_old  = p_lv_parvw.
            p_ls_hierarchy-parvw_unc  = p_lv_par_unc.
            p_ls_hierarchy-pernr      = lv_pernr.
            p_ls_hierarchy-ename      = l_name.
            p_ls_hierarchy-buspartner = lv_bpartner.
            CLEAR lv_objects.
          ELSE.
            CLEAR lv_parvw_new.
            CASE p_lv_parvw.
              WHEN 'L3'.
                lv_parvw_new = 'XL'.
              WHEN 'L2'.
                lv_parvw_new = 'XK'.
              WHEN 'ZS'.
                lv_parvw_new = 'YD'.
              WHEN 'L1'.
                lv_parvw_new = 'XJ'.
              WHEN 'PW'.
                lv_parvw_new = 'XS'.
            ENDCASE.
            p_ls_hierarchy-parvw_new  = lv_parvw_new.
            p_ls_hierarchy-plans      = lv_objects.
            p_ls_hierarchy-stext      = l_stext.
            p_ls_hierarchy-parvw_old  = p_lv_parvw.
            p_ls_hierarchy-parvw_unc  = p_lv_par_unc.
            p_ls_hierarchy-pernr      = lv_pernr.
            p_ls_hierarchy-ename      = l_name.
*            p_ls_hierarchy-buspartner = lv_bpartner.
            CLEAR lv_objects.
          ENDIF.
        ELSE.
          CASE p_lv_parvw.
            WHEN 'L3'.
              p_lv_msg = 'Cluster Manager Position is not correct category'.
            WHEN 'L2'.
              p_lv_msg = 'Cluster General Manager Position is not correct category'.
            WHEN 'ZS'.
              p_lv_msg = 'Cluster Vertical Head Position is not correct category'.
            WHEN 'L1'.
              p_lv_msg = 'National Sales Manager Position is not correct category'.
            WHEN 'PW'.
              p_lv_msg = 'Zonal Cluster General Manager Position is not correct category'.
          ENDCASE.
        ENDIF.
      ELSE.
        CASE p_lv_parvw.
          WHEN 'L3'.
            p_lv_msg = 'Cluster Manager Position is not correct category'.
          WHEN 'L2'.
            p_lv_msg = 'Cluster General Manager Position is not correct category'.
          WHEN 'ZS'.
            p_lv_msg = 'Cluster Vertical Head Position is not correct category'.
          WHEN 'L1'.
            p_lv_msg = 'National Sales Manager Position is not correct category'.
          WHEN 'PW'.
            p_lv_msg = 'Zonal Cluster General Manager Position is not correct category'.
        ENDCASE.
      ENDIF.
    ELSE.
      CASE sy-subrc.
        WHEN '1'.
          CASE p_lv_parvw.
            WHEN 'L3'.
              p_lv_msg = 'Sales Manager Position is Empty'.
            WHEN 'L2'.
              p_lv_msg = 'Area Sales Manager Position is Empty'.
            WHEN 'ZS'.
              p_lv_msg = 'Zonal Sales Manager Position is Empty'.
            WHEN 'L1'.
              p_lv_msg = 'National Sales Manager Position is Empty'.
          ENDCASE.
        WHEN '2'.
          p_lv_msg = 'Error in Function Module'.
      ENDCASE.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_initial_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ZHR_SO_TO_TOP_NEW
*&---------------------------------------------------------------------*
FORM f_get_initial_data USING lt_plans TYPE hrpad_t_plans.

  DATA: ltm_plans TYPE hrpad_t_plans,
        ls_plans  TYPE hrpad_s_plans.

  DATA: ls_sobid TYPE ty_sobid.

  DATA: lt_jobcode TYPE TABLE OF zhr_jobcode.

*Select all job codes from zhr_jobcode table
  SELECT * FROM zhr_jobcode INTO TABLE gt_jobcode.
  IF sy-subrc = 0.
    SORT gt_jobcode BY parvw stell.
    lt_jobcode[] = gt_jobcode[].

    DELETE lt_jobcode WHERE parvw NE 'L5'.

  ENDIF.


  IF lt_plans[] IS INITIAL.
* get all the positions attached to the sales job code
    SELECT sobid
      FROM hrp1001
      INTO TABLE gt_sobid
      FOR ALL ENTRIES IN lt_jobcode
     WHERE otype = 'C' AND objid = lt_jobcode-stell
       AND rsign = 'A' AND relat = '007'
       AND begda <= sy-datum AND endda >= sy-datum
       AND sclas = 'S'.
    IF sy-subrc = 0.
      SORT gt_sobid.
      LOOP AT gt_sobid INTO ls_sobid.
        CLEAR ls_plans.
        ls_plans-plans = ls_sobid-sobid+0(8).
        APPEND ls_plans TO ltm_plans.
      ENDLOOP.
    ENDIF.
  ELSE.
    ltm_plans[] = lt_plans[].
  ENDIF.

  IF ltm_plans[] IS NOT INITIAL.

    gt_plans[] = ltm_plans[].

*get the position to orgunit relationshipo
    SELECT otype objid rsign relat sclas sobid
      FROM hrp1001
      INTO TABLE gt_stoo
      WHERE otype = 'S'
        AND objid BETWEEN '20000000' AND '29999999'
        AND rsign = 'A'
        AND relat = '003'
        AND begda <= sy-datum
        AND endda >= sy-datum
        AND sclas = 'O'.
    IF sy-subrc = 0.
      SORT gt_stoo BY objid.
    ENDIF.

*get the position to Jobcode relationshipo
    SELECT otype objid rsign relat sclas sobid
      FROM hrp1001
      INTO TABLE gt_stoc
      WHERE otype = 'S'
        AND objid BETWEEN '20000000' AND '29999999'
        AND rsign = 'B'
        AND relat = '007'
        AND begda <= sy-datum
        AND endda >= sy-datum
        AND sclas = 'C'.
    IF sy-subrc = 0.
      SORT gt_stoc BY objid.
    ENDIF.

*get the position to Manager Position relationshipo
    SELECT otype objid rsign relat sclas sobid
      FROM hrp1001
      INTO TABLE gt_stos
      WHERE otype = 'S'
        AND objid BETWEEN '20000000' AND '29999999'
        AND rsign = 'A'
        AND relat = '002'
        AND begda <= sy-datum
        AND endda >= sy-datum
        AND sclas = 'S'.
    IF sy-subrc = 0.
      SORT gt_stos BY objid.
    ENDIF.

*get the position to position vendor relationshipo
    SELECT otype objid rsign relat sclas sobid
      FROM hrp1001
      INTO TABLE gt_stobp
      WHERE otype = 'S'
        AND objid BETWEEN '20000000' AND '29999999'
        AND rsign = 'A'
        AND relat = '008'
        AND begda <= sy-datum
        AND endda >= sy-datum
        AND sclas = 'BP'.
    IF sy-subrc = 0.
      SORT gt_stobp BY objid.
    ENDIF.

*get the position to pernr relationshipo
    SELECT otype objid rsign relat sclas sobid
      FROM hrp1001
      INTO TABLE gt_stop
      WHERE otype = 'S'
        AND objid BETWEEN '20000000' AND '29999999'
        AND rsign = 'A'
        AND relat = '008'
        AND begda <= sy-datum
        AND endda >= sy-datum
        AND sclas = 'P'.
    IF sy-subrc = 0.
      SORT gt_stop BY objid.
    ENDIF.

  ENDIF.



*select hrp1000 desc
  SELECT otype objid stext
    FROM hrp1000
    INTO TABLE gt_hrp1000
   WHERE otype IN ('S', 'O', 'C')
     AND begda <= sy-datum
     AND endda >= sy-datum.
  IF sy-subrc = 0.
    SORT gt_hrp1000 BY otype objid.
  ENDIF.

* select all active employees and names
  SELECT pernr ename
    FROM pa0001
    INTO TABLE gt_empdet
   WHERE begda <= sy-datum
     AND endda >= sy-datum
     AND plans NE '99999999'.


ENDFORM.

*&---------------------------------------------------------------------*
*& Form relationship_determine
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_SOBID
*&      <-- LS_HIERARCHY
*&---------------------------------------------------------------------*
FORM relationship_determine_new USING  p_lv_parvw   TYPE parvw
                                       p_lv_par_unc TYPE char02
                                       lv_orplans   TYPE hrobjid
                                       lv_plans     TYPE hrobjid
                                       p_orgid      TYPE hrobjid
                             CHANGING p_ls_hierarchy TYPE zhr_so_to_top_new
                                      p_lv_msg.
  DATA: lt_objects   TYPE TABLE OF hrobject,
        lv_parvw_new TYPE parvw.

  DATA: lv_so_pernr TYPE pernr_d,
        l_so_name   TYPE emnam,
        lv_BP       TYPE bu_partner,
        lv_so_text  TYPE stext,
        lv_orgid    TYPE hrobjid.

  READ TABLE gt_stos INTO DATA(ls_stos) WITH KEY objid = lv_plans.
  IF sy-subrc = 0.

*Get the orgunit for the given position.*Special handling required for PLatinum as there is NO SO/CO positions
    READ TABLE gt_stoo INTO DATA(ls_stoo) WITH KEY objid = ls_stos-sobid+0(8) BINARY SEARCH.
    IF sy-subrc = 0.
      IF p_lv_parvw  = 'L1'.
        lv_orgid = p_orgid.
      ELSE.
        lv_orgid = ls_stoo-sobid+0(8).
      ENDIF.

    ENDIF.

* get the Position Vendor i.e. the position BP value value
    READ TABLE gt_stobp INTO DATA(ls_stobp) WITH KEY objid = ls_stos-sobid+0(8) BINARY SEARCH.
    IF sy-subrc = 0.
      lv_BP = ls_stobp-sobid.
    ENDIF.

* get the position description
    READ TABLE gt_hrp1000 INTO DATA(ls_hrp1000) WITH KEY otype = 'S' objid = ls_stos-sobid+0(8).
    IF sy-subrc = 0.
      lv_so_text = ls_hrp1000-stext.
    ENDIF.

** respective reporting manager Job Code
    READ TABLE gt_stoc INTO DATA(ls_stoc) WITH KEY objid = ls_stos-sobid+0(8) BINARY SEARCH.
    IF sy-subrc = 0.
** Sales Officer Job Code is in Correct Category or not Checks **
      READ TABLE gt_jobcode INTO DATA(ls_jobcode) WITH KEY parvw = p_lv_parvw stell = ls_stoc-sobid. " BINARY SEARCH.
      IF sy-subrc = 0.
        READ TABLE gt_stop INTO DATA(ls_stop) WITH KEY objid = ls_stos-sobid+0(8) BINARY SEARCH.
        IF sy-subrc = 0.
          READ TABLE gt_empdet INTO DATA(ls_empdet) WITH KEY pernr = ls_stop-sobid+0(8).
          IF sy-subrc = 0.
            lv_so_pernr = ls_empdet-pernr.
            l_so_name = ls_empdet-ename.
          ELSE.
*              ERROR HANDLING TO THINK
          ENDIF.
        ELSE.
          CASE p_lv_parvw.
            WHEN 'L3'.
              lv_so_pernr = '9003002'.
              l_so_name = 'VACANT CM'.
            WHEN 'L2'.
              lv_so_pernr = '9003003'.
              l_so_name = 'VACANT CGM'.
            WHEN 'ZS'.
              lv_so_pernr = '9003004'.
              l_so_name = 'VACANT CVH'.
            WHEN 'L1'.
              lv_so_pernr = '9005599'.
              l_so_name = 'VACANT NSM'.
            WHEN 'PW'.
              lv_so_pernr = '9003005'.
              l_so_name = 'VACANT ZCGM'.
          ENDCASE.
        ENDIF.

        CLEAR lv_parvw_new.
        CASE p_lv_parvw.
          WHEN 'L3'.
            lv_parvw_new = 'XL'.
          WHEN 'L2'.
            lv_parvw_new = 'XK'.
          WHEN 'ZS'.
            lv_parvw_new = 'YD'.
          WHEN 'L1'.
            lv_parvw_new = 'XJ'.
          WHEN 'PW'.
            lv_parvw_new = 'XS'.
        ENDCASE.


        p_ls_hierarchy-plans_m = lv_orplans.
        p_ls_hierarchy-orgid = lv_orgid.
        p_ls_hierarchy-parvw_new  = lv_parvw_new.
        p_ls_hierarchy-plans      = ls_stos-sobid+0(8).
        p_ls_hierarchy-stext      = lv_so_text.
        p_ls_hierarchy-parvw_old  = p_lv_parvw.
        p_ls_hierarchy-parvw_unc  = p_lv_par_unc.
        p_ls_hierarchy-pernr      = lv_so_pernr.
        p_ls_hierarchy-ename      = l_so_name.
        p_ls_hierarchy-buspartner = lv_bp.

      ELSE.
*        CONTINUE.
      ENDIF.
    ELSE.
    ENDIF.

  ELSE.
  ENDIF.



*  IF p_lv_sobid IS NOT INITIAL.
*    REFRESH: lt_objects.
*    CALL FUNCTION 'RH_GET_LEADING_POSITION'
*      EXPORTING
*        plvar             = '01'
*        otype             = 'S'
*        sobid             = p_lv_sobid
*        date              = sy-datum
*      TABLES
*        leading_pos       = lt_objects
*      EXCEPTIONS
*        no_lead_pos_found = 1
*        OTHERS            = 2.
*    IF sy-subrc = 0.
*      DATA(lv_objects) = VALUE #( lt_objects[ 1 ]-objid OPTIONAL ).
*      SELECT SINGLE sobid FROM hrp1001
*           INTO @DATA(l_jobcode)
*           WHERE otype = 'S'
*           AND objid = @lv_objects
*           AND plvar = '01'
*           AND rsign = 'B'
*           AND relat = '007'
*           AND endda GE @sy-datum
*           AND begda LE @sy-datum
*           AND sclas = 'C'.
*      IF sy-subrc EQ 0.
*** Sales Officer Job Code is in Correct Category or not Checks **
*        SELECT SINGLE * FROM zhr_jobcode
*        INTO @DATA(ls_jobcode)
*        WHERE parvw = @p_lv_parvw
*        AND stell = @l_jobcode.
*        IF sy-subrc EQ 0.
*** Position Text
*          SELECT SINGLE stext FROM hrp1000 INTO @DATA(l_stext)
*            WHERE otype = 'S'
*            AND plvar = '01'
*            AND objid = @lv_objects
*            AND begda LE @sy-datum
*            AND endda GE @sy-datum.
*          IF sy-subrc EQ 0.
*
*          ENDIF.
*** Assigned Employee Number **
*          SELECT SINGLE sobid FROM hrp1001
*            INTO @DATA(lv_pernr)
*            WHERE otype = 'S'
*            AND objid = @lv_objects
*            AND plvar = '01'
*            AND rsign = 'A'
*            AND relat = '008'
*            AND endda GE @sy-datum
*            AND begda LE @sy-datum
*            AND sclas = 'P'.
*          IF sy-subrc EQ 0.
*            SELECT SINGLE ename FROM pa0001 INTO @DATA(l_name)
*              WHERE pernr = @lv_pernr
*              AND begda LE @sy-datum
*              AND endda GE @sy-datum.
*          ELSE.
*            CASE p_lv_parvw.
*              WHEN 'L3'.
*                lv_pernr = '9003002'.
*                l_name = 'VACANT SM'.
*              WHEN 'L2'.
*                lv_pernr = '9003003'.
*                l_name = 'VACANT RSM'.
*              WHEN 'ZS'.
*                lv_pernr = '9003004'.
*                l_name = 'VACANT ZSM'.
*              WHEN 'L1'.
*                lv_pernr = '9005599'.
*                l_name = 'VACANT NSM'.
*            ENDCASE.
*          ENDIF.
*** Sales Officer Business Partner Number **
*          SELECT SINGLE sobid FROM hrp1001
*            INTO @DATA(lv_bpartner)
*            WHERE otype = 'S'
*            AND objid = @lv_objects
*            AND plvar = '01'
*            AND rsign = 'A'
*            AND relat = '008'
*            AND endda GE @sy-datum
*            AND begda LE @sy-datum
*            AND sclas = 'BP'.
*          IF sy-subrc EQ 0.
*
*            CLEAR lv_parvw_new.
*            CASE p_lv_parvw.
*              WHEN 'L3'.
*                lv_parvw_new = 'XL'.
*              WHEN 'L2'.
*                lv_parvw_new = 'XK'.
*              WHEN 'ZS'.
*                lv_parvw_new = 'YD'.
*              WHEN 'L1'.
*                lv_parvw_new = 'XJ'.
*            ENDCASE.
*            p_ls_hierarchy-parvw_new  = lv_parvw_new.
*            p_ls_hierarchy-plans      = lv_objects.
*            p_ls_hierarchy-stext      = l_stext.
*            p_ls_hierarchy-parvw_old  = p_lv_parvw.
*            p_ls_hierarchy-parvw_unc  = p_lv_par_unc.
*            p_ls_hierarchy-pernr      = lv_pernr.
*            p_ls_hierarchy-ename      = l_name.
*            p_ls_hierarchy-buspartner = lv_bpartner.
*            CLEAR lv_objects.
*          ELSE.
**            CASE p_lv_parvw.
**              WHEN 'L3'.
**                p_lv_msg = 'Sales Manager Position to BP relationship missing'.
**              WHEN 'L2'.
**                p_lv_msg = 'Regional Sales Manager Position to BP relationship missing'.
**              WHEN 'ZS'.
**                p_lv_msg = 'Zonal Sales Manager Position to BP relationship missing'.
**              WHEN 'L1'.
**                p_lv_msg = 'National Sales Manager Position to BP relationship missing'.
**            ENDCASE.
*            CLEAR lv_parvw_new.
*            CASE p_lv_parvw.
*              WHEN 'L3'.
*                lv_parvw_new = 'XL'.
*              WHEN 'L2'.
*                lv_parvw_new = 'XK'.
*              WHEN 'ZS'.
*                lv_parvw_new = 'YD'.
*              WHEN 'L1'.
*                lv_parvw_new = 'XJ'.
*            ENDCASE.
*            p_ls_hierarchy-parvw_new  = lv_parvw_new.
*            p_ls_hierarchy-plans      = lv_objects.
*            p_ls_hierarchy-stext      = l_stext.
*            p_ls_hierarchy-parvw_old  = p_lv_parvw.
*            p_ls_hierarchy-parvw_unc  = p_lv_par_unc.
*            p_ls_hierarchy-pernr      = lv_pernr.
*            p_ls_hierarchy-ename      = l_name.
**            p_ls_hierarchy-buspartner = lv_bpartner.
*            CLEAR lv_objects.
*          ENDIF.
*        ELSE.
*          CASE p_lv_parvw.
*            WHEN 'L3'.
*              p_lv_msg = 'Sales Manager Position is not correct category'.
*            WHEN 'L2'.
*              p_lv_msg = 'Regional Sales Manager Position is not correct category'.
*            WHEN 'ZS'.
*              p_lv_msg = 'Zonal Sales Manager Position is not correct category'.
*            WHEN 'L1'.
*              p_lv_msg = 'National Sales Manager Position is not correct category'.
*          ENDCASE.
*        ENDIF.
*      ELSE.
*        CASE p_lv_parvw.
*          WHEN 'L3'.
*            p_lv_msg = 'Sales Manager Position Job Code relationship Missing'.
*          WHEN 'L2'.
*            p_lv_msg = 'Regional Sales Manager Position Job Code relationship Missing'.
*          WHEN 'ZS'.
*            p_lv_msg = 'Zonal Sales Manager Position Job Code relationship Missing'.
*          WHEN 'L1'.
*            p_lv_msg = 'National Sales Manager Position Job Code relationship Missing'.
*        ENDCASE.
*      ENDIF.
*    ELSE.
*      CASE sy-subrc.
*        WHEN '1'.
*          CASE p_lv_parvw.
*            WHEN 'L3'.
*              p_lv_msg = 'Sales Manager Position is Empty'.
*            WHEN 'L2'.
*              p_lv_msg = 'Area Sales Manager Position is Empty'.
*            WHEN 'ZS'.
*              p_lv_msg = 'Zonal Sales Manager Position is Empty'.
*            WHEN 'L1'.
*              p_lv_msg = 'National Sales Manager Position is Empty'.
*          ENDCASE.
*        WHEN '2'.
*          p_lv_msg = 'Error in Function Module'.
*      ENDCASE.
*    ENDIF.
*  ENDIF.
ENDFORM.
