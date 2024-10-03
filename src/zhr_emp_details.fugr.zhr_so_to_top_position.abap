FUNCTION zhr_so_to_top_position.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(SO_POSITION) TYPE  PLANS
*"  EXPORTING
*"     REFERENCE(MESSAGE) TYPE  STRING
*"  TABLES
*"      IT_POSITIONS STRUCTURE  ZHR_SO_TO_TOP
*"----------------------------------------------------------------------
*" Created By: Samsudeen M
*" Created On: 20.08.2023
*" Purpose: To Get Top Reporting Manager Hierarchy from Sales Officer Positions
*" Reference: Ramakrishnan J
*----------------------------------------------------------------------------------

  DATA: ls_hierarchy TYPE zhr_so_to_top.
  DATA: lv_sobid TYPE hrp1001-sobid.
  DATA: lv_parvw TYPE parvw.
  DATA: lv_parvw_unc TYPE CHar02.
  DATA: lv_times TYPE i,
        lv_step  TYPE i.

  DATA: lv_plat    TYPE char01,
        lv_Dfhier  TYPE char01,
        lv_plat_so TYPE pernr_d,
        lv_plat_nm TYPE emnam.

  lv_plat = abap_false.
  lv_Dfhier = abap_false.

*store exceptional orgunit ID with no SO in a TVARVC Variable
  SELECT * FROM tvarvc INTO TABLE @DATA(lt_exporg) WHERE name = 'ZSOTOPPOSEXPORG' AND type = 'S'.
  IF sy-subrc = 0.
  ENDIF.

*store Additional orgunit ID with no additional ZCGM in a TVARVC Variable
  SELECT * FROM tvarvc INTO TABLE @DATA(lt_difforg) WHERE name = 'ZSOTOPPOSDIFFORG' AND type = 'S'.
  IF sy-subrc = 0.
  ENDIF.

  IF so_position IS NOT INITIAL.

*Get the orgunit for the given position.
*Special handling required for PLatinum and wholesale as there is NO SO/CO positions
    SELECT SINGLE sobid FROM hrp1001
      INTO @DATA(l_orgid)
      WHERE otype = 'S' AND objid = @so_position AND plvar = '01'
      AND rsign = 'A' AND relat = '003' AND begda LE @sy-datum AND endda GE @sy-datum AND sclas = 'O'.
    IF sy-subrc = 0. " AND ( l_orgid = '40000176' or l_orgid = '40000270' ).
      READ TABLE lt_exporg WITH KEY low = l_orgid TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        lv_plat = abap_true.
      ENDIF.

      READ TABLE lt_difforg WITH KEY low = l_orgid TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        lv_Dfhier = abap_true.
      ENDIF.

    ENDIF.

** Sales Officer's Job Code
    SELECT SINGLE sobid FROM hrp1001
      INTO @DATA(l_so_jobcode)
      WHERE otype = 'S' AND objid = @so_position AND plvar = '01'
      AND rsign = 'B' AND relat = '007' AND endda GE @sy-datum AND begda LE @sy-datum AND sclas = 'C'.
    IF sy-subrc EQ 0.
** Sales Officer Job Code is in Correct Category or not Checks **
      SELECT SINGLE * FROM zhr_jobcode INTO @DATA(ls_jobcode) WHERE parvw = 'L5' AND stell = @l_so_jobcode.
      IF sy-subrc EQ 0.
** Position Text
        SELECT SINGLE stext FROM hrp1000 INTO @DATA(l_so_stext)
          WHERE otype = 'S' AND plvar = '01' AND objid = @so_position AND begda LE @sy-datum AND endda GE @sy-datum.
        IF sy-subrc EQ 0.

        ENDIF.
** Sales Officer Employee Number **
        SELECT SINGLE sobid FROM hrp1001
          INTO @DATA(lv_so_pernr)
          WHERE otype = 'S' AND objid = @so_position AND plvar = '01'
          AND rsign = 'A' AND relat = '008' AND endda GE @sy-datum AND begda LE @sy-datum AND sclas = 'P'.
        IF sy-subrc EQ 0.
          SELECT SINGLE ename FROM pa0001 INTO @DATA(l_so_name)
            WHERE pernr = @lv_so_pernr AND begda LE @sy-datum AND endda GE @sy-datum.
        ELSE.
          lv_so_pernr = '9003001'.
          l_so_name = 'VACANT SO'.
        ENDIF.
** Sales Officer Business Partner Number **
        SELECT SINGLE sobid FROM hrp1001
          INTO @DATA(lv_so_bpartner)
          WHERE otype = 'S' AND objid = @so_position AND plvar = '01'
          AND rsign = 'A' AND relat = '008' AND endda GE @sy-datum AND begda LE @sy-datum AND sclas = 'BP'.
        IF sy-subrc EQ 0.

        ENDIF.

*different handling for Platinum as there is not SO/CO concept
        IF lv_plat EQ abap_true.
* the XN Will have the Platimum SM position and the dummy SO Emp NO
          CLEAR: lv_plat_nm, lv_plat_so.
          lv_plat_so = '9003001'.
          lv_plat_nm = 'VACANT SO'.

          APPEND VALUE #( parvw_new  = 'XN'
                plans      = so_position
                stext      = l_so_stext
                parvw_old  = 'L5'
                parvw_unc  = 'L5'
*                pernr      = lv_plat_so
*                ename      = lv_plat_nm
                pernr      = lv_so_pernr
                ename      = l_so_name
                buspartner = lv_so_bpartner ) TO it_positions.

*Next entry wil be for the XL/AS - SM position entry
*if SM possition is empty use the dummy position of SM instead of SO
          IF lv_so_pernr = '9003001'.
            lv_so_pernr = '9003002'.
            l_so_name = 'VACANT SM'.
          ENDIF.

          APPEND VALUE #( parvw_new  = 'XL'
                plans      = so_position
                stext      = l_so_stext
                parvw_old  = 'L3'
                parvw_unc  = 'L3'
                pernr      = lv_so_pernr
                ename      = l_so_name
                buspartner = lv_so_bpartner ) TO it_positions.


        ELSE.
* Normal
          APPEND VALUE #( parvw_new  = 'XN'
                         plans      = so_position
                         stext      = l_so_stext
                         parvw_old  = 'L5'
                         parvw_unc   = 'L5'
                         pernr      = lv_so_pernr
                         ename      = l_so_name
                         buspartner = lv_so_bpartner ) TO it_positions.
        ENDIF.

      ELSE.
        message = 'Sales Officer Position is not correct category'.
      ENDIF.
    ELSE.
      message = 'Sales Officer Position Job Code Relationship Missing'.
    ENDIF.

    IF message IS INITIAL.

*if the Position deos not belongs to platimum then do it 4 times
* L5-->L3-->L2-->ZS-->L1  or L5-->L3-->L2-->PW-->ZS-->L1
      IF lv_plat = abap_false.

        IF lv_Dfhier = abap_true.
          lv_times = 5.
        ELSE.
          lv_times = 4.
        ENDIF.

        lv_step = 1.
        lv_sobid = so_position.

        DO lv_times TIMES.
          CLEAR lv_parvw.
          IF lv_Dfhier = abap_false.
            CASE lv_step.
              WHEN '1'.
                lv_parvw = 'L3'. "Cluster Manager Position & Pernr
              WHEN '2'.
                lv_parvw = 'L2'. "Cluster General Manager Position & Pernr
              WHEN '3'.
                lv_parvw = 'ZS'. "Cluster Vertical Head Posiiton & Pernr
              WHEN '4'.
                lv_parvw = 'L1'. "NSM Posiiton & Pernr
            ENDCASE.
          ELSE.
            CASE lv_step.
              WHEN '1'.
                lv_parvw = 'L3'. "Cluster Manager Position & Pernr
              WHEN '2'.
                lv_parvw = 'L2'. "Cluster General Manager Position & Pernr
              WHEN '3'.
                lv_parvw = 'PW'. "Zonal Cluster General Manager Position & Pernr
              WHEN '4'.
                lv_parvw = 'ZS'. "Cluster Vertical Head Posiiton & Pernr
              WHEN '5'.
                lv_parvw = 'L1'. "NSM Posiiton & Pernr
            ENDCASE.
          ENDIF.

          CLEAR ls_hierarchy.
          "Main Perform to get exact hirerachy
          CLEAR lv_parvw_unc.
          lv_parvw_unc = lv_parvw.
          PERFORM relationship_determine USING    lv_parvw
                                                  lv_parvw_unc
                                                  lv_sobid
                                         CHANGING ls_hierarchy
                                                  message.
          IF message IS INITIAL.
            APPEND ls_hierarchy TO it_positions.
            CLEAR: lv_sobid.
            lv_sobid = ls_hierarchy-plans.
            lv_step = lv_step + 1.
          ENDIF.
        ENDDO.
      ELSE.

        IF lv_Dfhier = abap_true.
          lv_times = 4.
        ELSE.
          lv_times = 3.
        ENDIF.
*        lv_times = 3.
        lv_step = 1.
        lv_sobid = so_position.

        DO lv_times TIMES.
          CLEAR lv_parvw.
          IF lv_Dfhier = abap_true.
            CASE lv_step.
              WHEN '1'.
                lv_parvw = 'L2'.
              WHEN '2'.
                lv_parvw = 'ZS'.
              WHEN '3'.
                lv_parvw = 'PW'.
              WHEN '4'.
                lv_parvw = 'L1'.
            ENDCASE.
          ELSE.
            CASE lv_step.
              WHEN '1'.
                lv_parvw = 'L2'.
              WHEN '2'.
                lv_parvw = 'ZS'.
              WHEN '3'.
                lv_parvw = 'L1'.
            ENDCASE.
          ENDIF.

          CLEAR ls_hierarchy.
          "Main Perform to get exact hirerachy
          CLEAR lv_parvw_unc.
          lv_parvw_unc = lv_parvw.
          PERFORM relationship_determine USING    lv_parvw
                                                  lv_parvw_unc
                                                  lv_sobid
                                         CHANGING ls_hierarchy
                                                  message.
          IF message IS INITIAL.
            APPEND ls_hierarchy TO it_positions.
            CLEAR: lv_sobid.
            lv_sobid = ls_hierarchy-plans.
            lv_step = lv_step + 1.
          ENDIF.
        ENDDO.
      ENDIF.
    ENDIF.
  ELSE.
    message = 'Input is Missing'.
  ENDIF.



ENDFUNCTION.
