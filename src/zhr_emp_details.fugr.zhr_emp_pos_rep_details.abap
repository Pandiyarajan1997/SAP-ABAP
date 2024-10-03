FUNCTION zhr_emp_pos_rep_details.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      LT_POSITION STRUCTURE  ZSTR_POS_REP
*"----------------------------------------------------------------------

  DATA: lt_hrp1001 TYPE STANDARD TABLE OF hrp1001,
        ls_hrp1001 TYPE hrp1001.

  DATA: lt_hrp1000 TYPE STANDARD TABLE OF hrp1000,
        ls_hrp1000 TYPE hrp1000.

  DATA: ls_position TYPE zstr_pos_rep.

  REFRESH: lt_hrp1001.
**Fetching Position and Reporting Manager Position**
  SELECT * FROM hrp1001
           INTO TABLE lt_hrp1001
           WHERE otype = 'S'
           AND ( objid BETWEEN '20000000' AND '29999999' )
           AND plvar = '01'
           AND rsign = 'A'
           AND relat = '002'
           AND begda <= sy-datum
           AND endda >= sy-datum.
  IF sy-subrc EQ 0.
    SORT lt_hrp1001[] ASCENDING.
  ENDIF.

  IF lt_hrp1001[] IS NOT INITIAL.

    REFRESH: lt_hrp1000.
**Fetching employee position Desc and Rep Manager Desc**
    SELECT * FROM hrp1000
             INTO TABLE lt_hrp1000
             WHERE plvar = '01'
             AND otype = 'S'
             AND ( objid BETWEEN '20000000' AND '29999999' )
             AND begda <= sy-datum
             AND endda >= sy-datum.
    IF sy-subrc EQ 0.
      SORT lt_hrp1000[] ASCENDING.
    ENDIF.
  ENDIF.

  CLEAR ls_hrp1001.
  LOOP AT lt_hrp1001 INTO ls_hrp1001.

    CLEAR ls_position.
    ls_position-position = ls_hrp1001-objid.

    CLEAR ls_hrp1000.
    READ TABLE lt_hrp1000 INTO ls_hrp1000 WITH KEY objid = ls_hrp1001-objid.
    IF sy-subrc EQ 0.
      ls_position-position_des = ls_hrp1000-stext.
    ENDIF.

    ls_position-report_manager = ls_hrp1001-sobid.

    CLEAR ls_hrp1000.
    READ TABLE lt_hrp1000 INTO ls_hrp1000 WITH KEY objid = ls_hrp1001-sobid.
    IF sy-subrc EQ 0.
      ls_position-manager_des = ls_hrp1000-stext.
    ENDIF.

    APPEND ls_position TO lt_position.
  ENDLOOP.



ENDFUNCTION.
