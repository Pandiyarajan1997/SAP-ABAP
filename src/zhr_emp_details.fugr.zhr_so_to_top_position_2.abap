FUNCTION zhr_so_to_top_position_2.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(SO_POSITION) TYPE  HRPAD_T_PLANS OPTIONAL
*"  EXPORTING
*"     REFERENCE(MESSAGE) TYPE  STRING
*"  TABLES
*"      IT_POSITIONS STRUCTURE  ZHR_SO_TO_TOP_NEW
*"      IT_POSTAB STRUCTURE  ZHR_SO_TO_TOP_TS
*"----------------------------------------------------------------------
*----------------------------------------------------------------------------------

  DATA: ls_hierarchy TYPE zhr_so_to_top_new.
  DATA: lv_sobid TYPE hrp1001-objid.
  DATA: lv_parvw TYPE parvw.
  DATA: lv_parvw_unc TYPE CHar02.
  DATA: lv_times TYPE i,
        lv_step  TYPE i.

  DATA: lv_plat    TYPE char01,
        lv_Dfhier  TYPE char01,
        lv_plat_so TYPE pernr_d,
        lv_plat_nm TYPE emnam.

  DATA: lv_so_pernr TYPE pernr_d,
        l_so_name   TYPE emnam,
        lv_BP       TYPE bu_partner,
        lv_so_text  TYPE stext,
        lv_orgid    TYPE hrobjid.

  DATA: ls_ZHR_SO_TO_TOP_TB TYPE zhr_so_to_top_tb.
  DATA: lt_ZHR_SO_TO_TOP_TB TYPE TABLE OF zhr_so_to_top_tb.

  DATA: ls_ZHR_SO_TO_TOP_TS TYPE zhr_so_to_top_ts.

  DATA: lt_pos_temp TYPE table of ZHR_SO_TO_TOP_NEW.

*store exceptional orgunit ID with no SO in a TVARVC Variable
  SELECT * FROM tvarvc INTO TABLE @DATA(lt_exporg) WHERE name = 'ZSOTOPPOSEXPORG' AND type = 'S'.
  IF sy-subrc = 0.
  ENDIF.

*store Additional orgunit ID with no additional ZCGM in a TVARVC Variable
  SELECT * FROM tvarvc INTO TABLE @DATA(lt_difforg) WHERE name = 'ZSOTOPPOSDIFFORG' AND type = 'S'.
  IF sy-subrc = 0.
  ENDIF.

  REFRESH gt_plans.
  PERFORM f_get_initial_data USING so_position.

  IF gt_plans[] IS NOT INITIAL.

    LOOP AT gt_plans INTO DATA(ls_plans) .
      CLEAR: lv_plat, lv_Dfhier, lv_bp, lv_so_pernr, l_so_name, lv_plat_so, lv_plat_nm, lv_so_text.
      refresh : lt_pos_temp.
*Get the orgunit for the given position.*Special handling required for PLatinum as there is NO SO/CO positions
      READ TABLE gt_stoo INTO DATA(ls_stoo) WITH KEY objid = ls_plans-plans BINARY SEARCH.
      IF sy-subrc = 0.

        lv_orgid = ls_stoo-sobid+0(8).

        READ TABLE lt_exporg WITH KEY low = lv_orgid TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          lv_plat = abap_true.
        ENDIF.

        READ TABLE lt_difforg WITH KEY low = lv_orgid TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          lv_Dfhier = abap_true.
        ENDIF.

      ENDIF.

* get the Position Vendor i.e. the position BP value value
      READ TABLE gt_stobp INTO DATA(ls_stobp) WITH KEY objid = ls_plans-plans BINARY SEARCH.
      IF sy-subrc = 0.
        lv_BP = ls_stobp-sobid.
      ENDIF.

* get the position description
      READ TABLE gt_hrp1000 INTO DATA(ls_hrp1000) WITH KEY otype = 'S' objid = ls_plans-plans.
      IF sy-subrc = 0.
        lv_so_text = ls_hrp1000-stext.
      ENDIF.

** Sales Officer's Job Code
      READ TABLE gt_stoc INTO DATA(ls_stoc) WITH KEY objid = ls_plans-plans BINARY SEARCH.
      IF sy-subrc = 0.
** Sales Officer Job Code is in Correct Category or not Checks **
        READ TABLE gt_jobcode INTO DATA(ls_jobcode) WITH KEY parvw = 'L5' stell = ls_stoc-sobid+0(8) BINARY SEARCH.
        IF sy-subrc = 0.
          READ TABLE gt_stop INTO DATA(ls_stop) WITH KEY objid = ls_plans-plans BINARY SEARCH.
          IF sy-subrc = 0.
            READ TABLE gt_empdet INTO DATA(ls_empdet) WITH KEY pernr = ls_stop-sobid+0(8).
            IF sy-subrc = 0.
              lv_so_pernr = ls_empdet-pernr.
              l_so_name = ls_empdet-ename.
            ELSE.
*              ERROR HANDLING TO THINK
            ENDIF.
          ELSE.
            lv_so_pernr = '9003001'.
            l_so_name = 'VACANT SO'.
          ENDIF.
        ELSE.
          CONTINUE.
        ENDIF.

        IF lv_plat = abap_true.
* the XN Will have the Platimum SM position and the L5/SO will have the same SM employee number
          APPEND VALUE #( plans_m = ls_plans-plans orgid = lv_orgid parvw_new = 'XN' plans = ls_plans-plans stext = lv_so_text parvw_old = 'L5'
                          parvw_unc = 'L5' pernr = lv_so_pernr ename = l_so_name buspartner = lv_bp ) TO lt_pos_temp.

*Next entry wil be for the XL/AS - SM position entry - if SM possition is empty use the dummy position of SM instead of SO
          IF lv_so_pernr = '9003001'.
            lv_so_pernr = '9003002'.
            l_so_name   = 'VACANT SM'.
          ENDIF.

          APPEND VALUE #( plans_m = ls_plans-plans orgid = lv_orgid parvw_new  = 'XL' plans = ls_plans-plans stext = lv_so_text parvw_old  = 'L3'
                          parvw_unc  = 'L3' pernr = lv_so_pernr ename = l_so_name buspartner = lv_bp ) TO lt_pos_temp.

        ELSE.
          APPEND VALUE #( plans_m = ls_plans-plans orgid = lv_orgid parvw_new  = 'XN' plans = ls_plans-plans stext = lv_so_text parvw_old  = 'L5'
                          parvw_unc  = 'L5' pernr = lv_so_pernr ename = l_so_name buspartner = lv_bp ) TO lt_pos_temp.

        ENDIF.

        IF lv_plat = abap_true.
          lv_times = 3.
        ELSE.
          lv_times = 4.
        ENDIF.

        IF lv_Dfhier = abap_true.
          lv_times = lv_times + 1.
        ENDIF.

        lv_step = 1.
        lv_sobid = ls_plans-plans.

        DO lv_times TIMES.
          CLEAR lv_parvw.
          IF lv_Dfhier = abap_true.
            CASE lv_step.
              WHEN '1'.
                IF lv_plat = abap_true.
                  lv_parvw = 'L2'.
                ELSE.
                  lv_parvw = 'L3'. "SM Position & Pernr
                ENDIF.
              WHEN '2'.
                IF lv_plat = abap_true.
                  lv_parvw = 'PW'.
                ELSE.
                  lv_parvw = 'L2'. "RSM Position & Pernr
                ENDIF.
              WHEN '3'.
                IF lv_plat = abap_true.
                  lv_parvw = 'ZS'.
                ELSE.
                  lv_parvw = 'PW'. "ZSM Posiiton & Pernr
                ENDIF.
              WHEN '4'.
                IF lv_plat = abap_true.
                  lv_parvw = 'L1'.
                ELSE.
                  lv_parvw = 'ZS'. "ZSM Posiiton & Pernr
                ENDIF.
              WHEN '5'.
                lv_parvw = 'L1'. "NSM Posiiton & Pernr
            ENDCASE.
          ELSE.
            CASE lv_step.
              WHEN '1'.
                IF lv_plat = abap_true.
                  lv_parvw = 'L2'.
                ELSE.
                  lv_parvw = 'L3'. "SM Position & Pernr
                ENDIF.
              WHEN '2'.
                IF lv_plat = abap_true.
                  lv_parvw = 'ZS'.
                ELSE.
                  lv_parvw = 'L2'. "RSM Position & Pernr
                ENDIF.
              WHEN '3'.
                IF lv_plat = abap_true.
                  lv_parvw = 'L1'.
                ELSE.
                  lv_parvw = 'ZS'. "ZSM Posiiton & Pernr
                ENDIF.
              WHEN '4'.
                lv_parvw = 'L1'. "NSM Posiiton & Pernr
            ENDCASE.
          ENDIF.

          CLEAR ls_hierarchy.
          "Main Perform to get exact hirerachy
          CLEAR lv_parvw_unc.
          lv_parvw_unc = lv_parvw.
          PERFORM relationship_determine_new USING    lv_parvw
                                                      lv_parvw_unc
                                                      ls_plans-plans
                                                      lv_sobid
                                                      lv_orgid
                                             CHANGING ls_hierarchy
                                                      message.
          IF ls_hierarchy IS NOT INITIAL.
*            APPEND ls_hierarchy TO it_positions.
             APPEND ls_hierarchy to lt_pos_temp.
            CLEAR: lv_sobid.
            lv_sobid = ls_hierarchy-plans.
            lv_step = lv_step + 1.
          else.
            refresh lt_pos_temp.
            exit.
          ENDIF.
        ENDDO.

        IF lt_pos_temp[] is NOT INITIAL.
          APPEND LINES OF lt_pos_temp TO it_positions.
        ENDIF.

      ELSE.
        CONTINUE.
      ENDIF.
    ENDLOOP.
  ENDIF.

  SORT it_positions BY orgid plans_m.

  LOOP AT it_positions INTO DATA(ls_positions).

    AT NEW plans_m.
      CLEAR ls_ZHR_SO_TO_TOP_TB.
      ls_zhr_so_to_top_ts-plans_m = ls_positions-plans_m.
    ENDAT.

    ls_zhr_so_to_top_ts-orgid   = ls_positions-orgid.

    CASE ls_positions-parvw_new .
      WHEN 'XN'.
        ls_zhr_so_to_top_ts-pos_xn    = ls_positions-plans.
        ls_zhr_so_to_top_ts-pernr_l5  = ls_positions-pernr.
        ls_zhr_so_to_top_ts-bp_xn     = ls_positions-buspartner.
      WHEN 'XL'.
        ls_zhr_so_to_top_ts-pos_xl    = ls_positions-plans.
        ls_zhr_so_to_top_ts-pernr_l3  = ls_positions-pernr.
        ls_zhr_so_to_top_ts-bp_xl     = ls_positions-buspartner.
      WHEN 'XK'.
        ls_zhr_so_to_top_ts-pos_xk    = ls_positions-plans.
        ls_zhr_so_to_top_ts-pernr_l2  = ls_positions-pernr.
        ls_zhr_so_to_top_ts-bp_xk     = ls_positions-buspartner.
      WHEN 'YD'.
        ls_zhr_so_to_top_ts-pos_yd    = ls_positions-plans.
        ls_zhr_so_to_top_ts-pernr_zs  = ls_positions-pernr.
        ls_zhr_so_to_top_ts-bp_yd     = ls_positions-buspartner.
      WHEN 'XJ'.
        ls_zhr_so_to_top_ts-pos_xj    = ls_positions-plans.
        ls_zhr_so_to_top_ts-pernr_l1  = ls_positions-pernr.
        ls_zhr_so_to_top_ts-bp_xj     = ls_positions-buspartner.
      WHEN 'XS'.
        ls_zhr_so_to_top_ts-pos_xs    = ls_positions-plans.
        ls_zhr_so_to_top_ts-pernr_pw  = ls_positions-pernr.
        ls_zhr_so_to_top_ts-bp_xs     = ls_positions-buspartner.
    ENDCASE.

    AT END OF plans_m.
      APPEND ls_zhr_so_to_top_ts TO it_postab.

      CLEAR ls_ZHR_SO_TO_TOP_TB.
      MOVE-CORRESPONDING ls_ZHR_SO_TO_TOP_ts TO ls_ZHR_SO_TO_TOP_TB.
      ls_ZHR_SO_TO_TOP_TB-mandt = sy-mandt.
      APPEND ls_ZHR_SO_TO_TOP_TB TO lt_ZHR_SO_TO_TOP_TB.

      CLEAR ls_zhr_so_to_top_ts.
    ENDAT.

  ENDLOOP.

  IF so_position[] IS INITIAL.
    IF lt_ZHR_SO_TO_TOP_TB[] IS NOT INITIAL.
      DELETE FROM zhr_so_to_top_tb.
      COMMIT WORK AND WAIT.

      MODIFY zhr_so_to_top_tb FROM TABLE lt_ZHR_SO_TO_TOP_TB.
    ENDIF.
  ENDIF.

ENDFUNCTION.
