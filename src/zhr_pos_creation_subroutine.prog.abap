*&---------------------------------------------------------------------*
*& Include          ZHR_POS_CREATION_SUBROUTINE
*&---------------------------------------------------------------------*
*& Form F_screen_adjust
*&---------------------------------------------------------------------*
FORM f_screen_adjust .
  IF sy-tcode EQ 'ZHR_POS_PROP'. "Propose Screen

    LOOP AT SCREEN.
      IF screen-group1 = 'BL4'.
        screen-input = 0.
        screen-active = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'CH1'.
        screen-input = 0.
        screen-active = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.

      IF p_rad2 IS NOT INITIAL.
        IF screen-group1 = 'BL2'.
          screen-input = 0.
          screen-active = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
      ELSE.
        IF screen-group1 = 'BL3'.
          screen-input = 0.
          screen-active = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.

      IF p_rad7 IS INITIAL.
        IF screen-group1 = 'BL6'.
          screen-input = 0.
          screen-active = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
      ELSE.
        IF screen-group1 = 'BL6'.
          screen-input = 1.
          screen-active = 1.
          screen-invisible = 0.
          MODIFY SCREEN.
        ENDIF.
        IF screen-group1 = 'BL2'.
          screen-input = 0.
          screen-active = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.

      IF p_rad5 IS INITIAL.
        IF screen-group1 = 'BL5'.
          screen-input = 0.
          screen-active = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
      ELSE.
        IF screen-group1 = 'BL5'.
          screen-input = 1.
          screen-active = 1.
          screen-invisible = 0.
          MODIFY SCREEN.
        ENDIF.
        IF screen-group1 = 'BL2'.
          screen-input = 0.
          screen-active = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.

    ENDLOOP.

  ELSEIF sy-tcode EQ 'ZHR_POS_APPR'. "Approve Screeen

    LOOP AT SCREEN.
      IF screen-group1 = 'BL1'.
        screen-input = 0.
        screen-active = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'BL2'.
        screen-input = 0.
        screen-active = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'BL3'.
        screen-input = 0.
        screen-active = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'BL6'.
        screen-input = 0.
        screen-active = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
      IF p_rad4 IS INITIAL.
        IF screen-group1 = 'BL5'.
          screen-input = 0.
          screen-active = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
      ELSE.
        IF screen-group1 = 'BL5'.
          screen-input = 1.
          screen-active = 1.
          screen-invisible = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.


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
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_status_des
*&---------------------------------------------------------------------*
FORM f_get_status_des .
  REFRESH: lt_domain.
  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      domname    = 'ZSTATUS2'
    TABLES
      values_tab = lt_domain[].
  IF sy-subrc EQ 0.

  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_initial_selections
*&---------------------------------------------------------------------*
FORM f_initial_selections .
  REFRESH: gt_name.
  SELECT pernr
         plans
         kostl
         ename INTO TABLE gt_name FROM pa0001
               WHERE begda LE sy-datum AND endda GE sy-datum.
  IF sy-subrc EQ 0.
    SORT gt_name[] BY plans.
  ENDIF.
** Orgunit ID Chacks ***
  REFRESH: gt_orgunit.
  SELECT objid FROM hrp1000
               INTO TABLE gt_orgunit
               WHERE otype = 'O'
               AND plvar = '01'
               AND begda LE sy-datum
               AND endda GE sy-datum.
  IF sy-subrc EQ 0.
    SORT gt_orgunit[] BY objid.
  ENDIF.
** Job ID checks **
  REFRESH: gt_jobid.
  SELECT objid FROM hrp1000
              INTO TABLE gt_jobid
              WHERE otype = 'C'
              AND plvar = '01'
              AND istat = '1'
              AND begda LE sy-datum
              AND endda GE sy-datum.
  IF sy-subrc EQ 0.
    SORT gt_jobid[] BY objid.
  ENDIF.
** Position ID checks **
  SELECT * FROM hrp1000
         INTO TABLE gt_position
         WHERE otype EQ 'S'
        AND begda <= sy-datum AND endda GE sy-datum.
  IF sy-subrc EQ 0.
    SORT gt_position[] BY objid.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form propose_alv
*&---------------------------------------------------------------------*
FORM display_alv .

  REFRESH gt_fcat.

  IF sy-tcode EQ 'ZHR_POS_PROP'. "Propose Screen
    IF p_rad2 = abap_true.
*** Change Reporting manger ****
      PERFORM f_fieldcat USING 'SEL'         1  'X'.
      PERFORM f_fieldcat USING 'PERNR'       2  space.
      PERFORM f_fieldcat USING 'NAME'        3  space.
      PERFORM f_fieldcat USING 'POSITION'    4  space.
      PERFORM f_fieldcat USING 'POSDESS'     5  space.
      PERFORM f_fieldcat USING 'POSDESL'     6  space.
      PERFORM f_fieldcat USING 'REPMNG'      7  space.
      PERFORM f_fieldcat USING 'RMNAME'      8  space.
      PERFORM f_fieldcat USING 'RMPOS'       9  space.
      PERFORM f_fieldcat USING 'RMPOSDES'   10  space.
      PERFORM f_fieldcat USING 'DATE'       11  'X'.
      PERFORM f_fieldcat USING 'NEWREP'     12  'X'.
      PERFORM f_fieldcat USING 'NEWREPNAME' 13  space.
      PERFORM f_fieldcat USING 'NEWREPOS'   14  space.
      PERFORM f_fieldcat USING 'MESSAGE'    15  space.
      CALL SCREEN 300..
    ELSE.
      IF p_rad1 = abap_true OR p_rad7 = abap_true.
** Create Position Propose Screen **
        PERFORM f_fieldcat USING 'SEL'              1  'X'.
        PERFORM f_fieldcat USING 'BUKRS'            2  'X'.
        PERFORM f_fieldcat USING 'FROMPOS'          3  space.
        PERFORM f_fieldcat USING 'ORGUNIT_ID'       4  'X'.
        PERFORM f_fieldcat USING 'ORGUNIT_DES'      5  space.
        PERFORM f_fieldcat USING 'POSITION_ID'      6  space.
        PERFORM f_fieldcat USING 'START_DATE'       7  'X'.
        PERFORM f_fieldcat USING 'POS_SDES'         8  'X'.
        PERFORM f_fieldcat USING 'POS_LDES'         9  'X'.
        PERFORM f_fieldcat USING 'REPMANAGER_POS'   10  'X'.
        PERFORM f_fieldcat USING 'REPMNG_PDES'     11   space.
        PERFORM f_fieldcat USING 'REP_MANAGER'     12  space.
        PERFORM f_fieldcat USING 'REPMNG_NAME'     13  space.
        PERFORM f_fieldcat USING 'STELL'           14  'X'.
        PERFORM f_fieldcat USING 'JOB_DES'         15  space.
        PERFORM f_fieldcat USING 'LATITUDE'        16  'X'.
        PERFORM f_fieldcat USING 'LONGITUDE'       17  'X'.
        PERFORM f_fieldcat USING 'TRAVEL'          18  'X'.
        PERFORM f_fieldcat USING 'CATEGORY'        19  'X'.
        PERFORM f_fieldcat USING 'COSTCENTER'      20  'X'.

      ELSEIF p_rad5 = abap_true.
** Table Display for Reference
        PERFORM f_fieldcat USING 'BUKRS'             1  space.
        PERFORM f_fieldcat USING 'ORGUNIT_ID'       2  space.
        PERFORM f_fieldcat USING 'ORGUNIT_DES'      3  space.
        PERFORM f_fieldcat USING 'POSITION_ID'      4  space.
        PERFORM f_fieldcat USING 'START_DATE'       5  space.
        PERFORM f_fieldcat USING 'POS_SDES'         6  space.
        PERFORM f_fieldcat USING 'POS_LDES'         7  space.
        PERFORM f_fieldcat USING 'REPMANAGER_POS'   8  space.
        PERFORM f_fieldcat USING 'REPMNG_PDES'      9  space.
        PERFORM f_fieldcat USING 'REP_MANAGER'      10  space.
        PERFORM f_fieldcat USING 'REPMNG_NAME'     11  space.
        PERFORM f_fieldcat USING 'STELL'           12  space.
        PERFORM f_fieldcat USING 'JOB_DES'         13  space.
        PERFORM f_fieldcat USING 'STATUS_DESC'     14  space.
        PERFORM f_fieldcat USING 'CREATED_BY'      15  space.
        PERFORM f_fieldcat USING 'CREATED_ON'      16  space.
        PERFORM f_fieldcat USING 'APPROVED_BY'     17  space.
        PERFORM f_fieldcat USING 'APPROVED_ON'     18  space.
      ENDIF.
      CALL SCREEN 100.
    ENDIF.
  ELSEIF sy-tcode EQ 'ZHR_POS_APPR'. "Approve Screen
    IF p_rad4 = abap_true.
** Table Display for Reference
      PERFORM f_fieldcat USING 'BUKRS'             1  space.
      PERFORM f_fieldcat USING 'ORGUNIT_ID'       2  space.
      PERFORM f_fieldcat USING 'ORGUNIT_DES'      3  space.
      PERFORM f_fieldcat USING 'POSITION_ID'      4  space.
      PERFORM f_fieldcat USING 'START_DATE'       5  space.
      PERFORM f_fieldcat USING 'POS_SDES'         6  space.
      PERFORM f_fieldcat USING 'POS_LDES'         7  space.
      PERFORM f_fieldcat USING 'REPMANAGER_POS'   8  space.
      PERFORM f_fieldcat USING 'REPMNG_PDES'      9  space.
      PERFORM f_fieldcat USING 'REP_MANAGER'      10  space.
      PERFORM f_fieldcat USING 'REPMNG_NAME'     11  space.
      PERFORM f_fieldcat USING 'STELL'           12  space.
      PERFORM f_fieldcat USING 'JOB_DES'         13  space.
      PERFORM f_fieldcat USING 'STATUS_DESC'     14  space.
      PERFORM f_fieldcat USING 'CREATED_BY'      15  space.
      PERFORM f_fieldcat USING 'CREATED_ON'      16  space.
      PERFORM f_fieldcat USING 'APPROVED_BY'     17  space.
      PERFORM f_fieldcat USING 'APPROVED_ON'     18  space.
      CALL SCREEN 100.
    ELSEIF p_rad3 = abap_true.
** Table Display for Reference
      PERFORM f_fieldcat USING 'SEL'             1  'X'.
      PERFORM f_fieldcat USING 'BP'              2  'X'.
      PERFORM f_fieldcat USING 'BUKRS'            3  space.
      PERFORM f_fieldcat USING 'ORGUNIT_ID'      4  space.
      PERFORM f_fieldcat USING 'ORGUNIT_DES'     5  space.
      PERFORM f_fieldcat USING 'POSITION_ID'     6  space.
      PERFORM f_fieldcat USING 'START_DATE'      7  space.
      PERFORM f_fieldcat USING 'POS_SDES'        8  space.
      PERFORM f_fieldcat USING 'POS_LDES'        9  space.
      PERFORM f_fieldcat USING 'REPMANAGER_POS' 10  space.
      PERFORM f_fieldcat USING 'REPMNG_PDES'    11  space.
      PERFORM f_fieldcat USING 'REP_MANAGER'    12  space.
      PERFORM f_fieldcat USING 'REPMNG_NAME'    13  space.
      PERFORM f_fieldcat USING 'STELL'          14  space.
      PERFORM f_fieldcat USING 'JOB_DES'        15  space.
      PERFORM f_fieldcat USING 'STATUS_DESC'    16  space.
      PERFORM f_fieldcat USING 'CREATED_BY'     17  space.
      PERFORM f_fieldcat USING 'CREATED_ON'     18  space.
      PERFORM f_fieldcat USING 'APPROVED_BY'    19  space.
      PERFORM f_fieldcat USING 'APPROVED_ON'    20  space.
      PERFORM f_fieldcat USING 'BPCREATE'       21 space.
      PERFORM f_fieldcat USING 'BPEXTEND'       22  space.
      PERFORM f_fieldcat USING 'BPREL'          23  space.
      PERFORM f_fieldcat USING 'CCREL'          24  space.
      PERFORM f_fieldcat USING 'MESSAGE'        25  space.
      CALL SCREEN 200.
    ENDIF.
  ENDIF.

ENDFORM.

** ALV Display **
FORM f_fieldcat  USING f_var1 f_var2 f_var3.
  gs_fcat-fieldname = f_var1.
  gs_fcat-col_pos = f_var2.
  gs_fcat-edit = f_var3.

** Position Propose screen **
  IF p_rad1 = abap_true OR p_rad7 = abap_true.

    IF gs_fcat-fieldname = 'SEL'.
      gs_fcat-seltext = 'Select'.
      gs_fcat-checkbox = 'X'.

    ELSEIF gs_fcat-fieldname = 'BUKRS'.
      gs_fcat-coltext = 'Company'.
      gs_fcat-f4availabl = 'X'.
      gs_fcat-ref_table = 'T001'.
      gs_fcat-ref_field = 'BUKRS'.

    ELSEIF gs_fcat-fieldname = 'FROMPOS'.  "Org ID
      gs_fcat-coltext = 'FromPos'.
      IF p_rad1 = abap_true.
        gs_fcat-no_out = abap_true.
      ENDIF.

    ELSEIF gs_fcat-fieldname = 'ORGUNIT_ID'.  "Org ID
      gs_fcat-coltext = 'Org ID'.
      gs_fcat-f4availabl = 'X'.

    ELSEIF gs_fcat-fieldname = 'ORGUNIT_DES'. "Org ID Des
      gs_fcat-coltext = 'Org ID Desc'.

    ELSEIF gs_fcat-fieldname = 'POSITION_ID'. "Position ID
      gs_fcat-coltext = 'Position ID'.
      gs_fcat-no_out = abap_true.

    ELSEIF gs_fcat-fieldname = 'START_DATE'. "Start Date
      gs_fcat-f4availabl = 'X'.
      gs_fcat-coltext = 'Start Date'.
      gs_fcat-ref_table = 'SYST'.
      gs_fcat-ref_field = 'DATUM'.

    ELSEIF gs_fcat-fieldname = 'POS_SDES'.  "Pos Sdes
      gs_fcat-coltext = 'Position Short Desc'.
      gs_fcat-ref_table = 'HRP1000'.
      gs_fcat-ref_field = 'SHORT'.

    ELSEIF gs_fcat-fieldname = 'POS_LDES'.  "Pos Sdes
      gs_fcat-coltext = 'Position Long Desc'.
      gs_fcat-ref_table = 'HRP1000'.
      gs_fcat-ref_field = 'STEXT'.

    ELSEIF gs_fcat-fieldname = 'REPMANAGER_POS'. "Rep Mgr Pos
      gs_fcat-coltext = 'Rep Manager Pos'.
      gs_fcat-f4availabl = 'X'.

    ELSEIF gs_fcat-fieldname = 'REPMNG_PDES'. "Repp Mgr Pdes
      gs_fcat-coltext = 'Rep Manager Posdes'.

    ELSEIF gs_fcat-fieldname = 'REP_MANAGER'. "Rep Mgr Emp id
      gs_fcat-coltext = 'Rep manager'.

    ELSEIF gs_fcat-fieldname = 'REPMNG_NAME'. "Rep Manager Name
      gs_fcat-coltext = 'Rep Manager Name'.

    ELSEIF gs_fcat-fieldname = 'STELL'.  " Job ID
      gs_fcat-coltext = 'Job ID'.
      gs_fcat-f4availabl = 'X'.

    ELSEIF gs_fcat-fieldname = 'JOB_DES'. "Job Id Des
      gs_fcat-coltext = 'Job ID Desc'.

    ELSEIF gs_fcat-fieldname = 'LATITUDE'. "Latitude
      gs_fcat-coltext = 'Latitude'.
      gs_fcat-ref_table = 'HRP9011'.
      gs_fcat-ref_field = 'LATITUDE'.
      gs_fcat-no_out  = abap_true.

    ELSEIF gs_fcat-fieldname = 'LONGITUDE'. "Longitude
      gs_fcat-coltext = 'Longitude'.
      gs_fcat-ref_table = 'HRP9011'.
      gs_fcat-ref_field = 'LONGITUDE'.
      gs_fcat-no_out  = abap_true.

    ELSEIF gs_fcat-fieldname = 'TRAVEL'. "Travel
      gs_fcat-coltext = 'Travel'.
      gs_fcat-f4availabl = 'X'.
      gs_fcat-ref_table = 'HRP9013'.
      gs_fcat-ref_field = 'TRAVEL'.

    ELSEIF gs_fcat-fieldname = 'CATEGORY'. "Travel Category
      gs_fcat-coltext = 'Travel Category'.
      gs_fcat-f4availabl = 'X'.
      gs_fcat-ref_table = 'HRP9013'.
      gs_fcat-ref_field = 'CATEGORY'.

    ELSEIF gs_fcat-fieldname = 'COSTCENTER'. "Travel Category
      gs_fcat-coltext = 'Costcenter'.
      gs_fcat-f4availabl = 'X'.
      gs_fcat-ref_table = 'CSKS'.
      gs_fcat-ref_field = 'KOSTL'.

    ENDIF.
    APPEND gs_fcat TO gt_fcat.
    CLEAR gs_fcat.
** Propose Display ALV **
  ELSEIF p_rad5 = abap_true.

    IF gs_fcat-fieldname = 'ORGUNIT_ID'.
      gs_fcat-coltext = 'Org ID'.

    ELSEIF gs_fcat-fieldname = 'BUKRS'.
      gs_fcat-coltext = 'Company'.

    ELSEIF gs_fcat-fieldname = 'ORGUNIT_ID'.
      gs_fcat-coltext = 'Org ID'.

    ELSEIF gs_fcat-fieldname = 'ORGUNIT_DES'.
      gs_fcat-coltext = 'Org ID Desc'.

    ELSEIF gs_fcat-fieldname = 'POSITION_ID'.
      gs_fcat-coltext = 'Position ID'.

    ELSEIF gs_fcat-fieldname = 'START_DATE'.
      gs_fcat-coltext = 'Start Date'.

    ELSEIF gs_fcat-fieldname = 'POS_SDES'.
      gs_fcat-coltext = 'Position Short Desc'.

    ELSEIF gs_fcat-fieldname = 'POS_LDES'.
      gs_fcat-coltext = 'Position Short Desc'.

    ELSEIF gs_fcat-fieldname = 'REPMANAGER_POS'.
      gs_fcat-coltext = 'Rep Manager Pos'.

    ELSEIF gs_fcat-fieldname = 'REPMNG_PDES'.
      gs_fcat-coltext = 'Rep Manager Posdes'.

    ELSEIF gs_fcat-fieldname = 'REP_MANAGER'.
      gs_fcat-coltext = 'Rep manager'.

    ELSEIF gs_fcat-fieldname = 'REPMNG_NAME'.
      gs_fcat-coltext = 'Rep Manager Name'.

    ELSEIF gs_fcat-fieldname = 'STELL'.
      gs_fcat-coltext = 'Job ID'.

    ELSEIF gs_fcat-fieldname = 'JOB_DES'.
      gs_fcat-coltext = 'Job ID Desc'.

    ELSEIF gs_fcat-fieldname = 'STATUS_DESC'.
      gs_fcat-coltext = 'Status Desc'.

    ELSEIF gs_fcat-fieldname = 'CREATED_BY'.
      gs_fcat-coltext = 'Created by'.

    ELSEIF gs_fcat-fieldname = 'CREATED_ON'.
      gs_fcat-coltext = 'Created On'.

    ELSEIF gs_fcat-fieldname = 'APPROVED_BY'.
      gs_fcat-coltext = 'Approved by'.

    ELSEIF gs_fcat-fieldname = 'APPROVED_ON'.
      gs_fcat-coltext = 'Approved on'.

    ENDIF.
    APPEND gs_fcat TO gt_fcat.
    CLEAR gs_fcat.
** Approve Screen Display ALV **
  ELSEIF p_rad4 = abap_true.

    IF gs_fcat-fieldname = 'ORGUNIT_ID'.
      gs_fcat-coltext = 'Org ID'.

    ELSEIF gs_fcat-fieldname = 'BUKRS'.
      gs_fcat-coltext = 'Company'.

    ELSEIF gs_fcat-fieldname = 'ORGUNIT_DES'.
      gs_fcat-coltext = 'Org ID Desc'.

    ELSEIF gs_fcat-fieldname = 'POSITION_ID'.
      gs_fcat-coltext = 'Position ID'.

    ELSEIF gs_fcat-fieldname = 'START_DATE'.
      gs_fcat-coltext = 'Start Date'.

    ELSEIF gs_fcat-fieldname = 'POS_SDES'.
      gs_fcat-coltext = 'Position Short Desc'.

    ELSEIF gs_fcat-fieldname = 'POS_LDES'.
      gs_fcat-coltext = 'Position Short Desc'.

    ELSEIF gs_fcat-fieldname = 'REPMANAGER_POS'.
      gs_fcat-coltext = 'Rep Manager Pos'.

    ELSEIF gs_fcat-fieldname = 'REPMNG_PDES'.
      gs_fcat-coltext = 'Rep Manager Posdes'.

    ELSEIF gs_fcat-fieldname = 'REP_MANAGER'.
      gs_fcat-coltext = 'Rep manager'.

    ELSEIF gs_fcat-fieldname = 'REPMNG_NAME'.
      gs_fcat-coltext = 'Rep Manager Name'.

    ELSEIF gs_fcat-fieldname = 'STELL'.
      gs_fcat-coltext = 'Job ID'.

    ELSEIF gs_fcat-fieldname = 'JOB_DES'.
      gs_fcat-coltext = 'Job ID Desc'.

    ELSEIF gs_fcat-fieldname = 'STATUS_DESC'.
      gs_fcat-coltext = 'Status Desc'.

    ELSEIF gs_fcat-fieldname = 'CREATED_BY'.
      gs_fcat-coltext = 'Created by'.

    ELSEIF gs_fcat-fieldname = 'CREATED_ON'.
      gs_fcat-coltext = 'Created On'.

    ELSEIF gs_fcat-fieldname = 'APPROVED_BY'.
      gs_fcat-coltext = 'Approved by'.

    ELSEIF gs_fcat-fieldname = 'APPROVED_ON'.
      gs_fcat-coltext = 'Approved on'.

    ENDIF.
    APPEND gs_fcat TO gt_fcat.
    CLEAR gs_fcat.
** Approve Screen **
  ELSEIF p_rad3 = abap_true.

    IF gs_fcat-fieldname = 'SEL'.
      gs_fcat-coltext = 'Select'.
      gs_fcat-checkbox = abap_true.

    ELSEIF gs_fcat-fieldname = 'BP'.
      gs_fcat-coltext = 'BP'.
      gs_fcat-checkbox = abap_true.

    ELSEIF gs_fcat-fieldname = 'BUKRS'.
      gs_fcat-coltext = 'Company'.

    ELSEIF gs_fcat-fieldname = 'ORGUNIT_ID'.
      gs_fcat-coltext = 'Org ID'.

    ELSEIF gs_fcat-fieldname = 'ORGUNIT_DES'.
      gs_fcat-coltext = 'Org ID Desc'.

    ELSEIF gs_fcat-fieldname = 'POSITION_ID'.
      gs_fcat-coltext = 'Position ID'.

    ELSEIF gs_fcat-fieldname = 'START_DATE'.
      gs_fcat-coltext = 'Start Date'.

    ELSEIF gs_fcat-fieldname = 'POS_SDES'.
      gs_fcat-coltext = 'Position Short Desc'.

    ELSEIF gs_fcat-fieldname = 'POS_LDES'.
      gs_fcat-coltext = 'Position Long Desc'.

    ELSEIF gs_fcat-fieldname = 'REPMANAGER_POS'.
      gs_fcat-coltext = 'Rep Manager Pos'.

    ELSEIF gs_fcat-fieldname = 'REPMNG_PDES'.
      gs_fcat-coltext = 'Rep Manager Posdes'.

    ELSEIF gs_fcat-fieldname = 'REP_MANAGER'.
      gs_fcat-coltext = 'Rep manager'.

    ELSEIF gs_fcat-fieldname = 'REPMNG_NAME'.
      gs_fcat-coltext = 'Rep Manager Name'.

    ELSEIF gs_fcat-fieldname = 'STELL'.
      gs_fcat-coltext = 'Job ID'.

    ELSEIF gs_fcat-fieldname = 'JOB_DES'.
      gs_fcat-coltext = 'Job ID Desc'.

    ELSEIF gs_fcat-fieldname = 'STATUS_DESC'.
      gs_fcat-coltext = 'Status Desc'.

    ELSEIF gs_fcat-fieldname = 'CREATED_BY'.
      gs_fcat-coltext = 'Created by'.

    ELSEIF gs_fcat-fieldname = 'CREATED_ON'.
      gs_fcat-coltext = 'Created On'.

    ELSEIF gs_fcat-fieldname = 'APPROVED_BY'.
      gs_fcat-coltext = 'Approved by'.
      gs_fcat-no_out = 'X'.

    ELSEIF gs_fcat-fieldname = 'APPROVED_ON'.
      gs_fcat-coltext = 'Approved on'.
      gs_fcat-no_out = 'X'.
    ELSEIF gs_fcat-fieldname = 'BPCREATE'.
      gs_fcat-coltext = 'BPcreate'.

    ELSEIF gs_fcat-fieldname = 'BPEXTEND'.
      gs_fcat-coltext = 'BPext'.

    ELSEIF gs_fcat-fieldname = 'BPREL'.
      gs_fcat-coltext = 'BPRelshp'.

    ELSEIF gs_fcat-fieldname = 'CCREL'.
      gs_fcat-coltext = 'CCrelshp'.

    ELSEIF gs_fcat-fieldname = 'MESSAGE'.
      gs_fcat-coltext = 'Mesage'.
    ENDIF.
    APPEND gs_fcat TO gt_fcat.
    CLEAR gs_fcat.
** Approve Screen **
  ELSEIF p_rad2 = abap_true.

    IF gs_fcat-fieldname = 'SEL'.
      gs_fcat-seltext = 'Select'.
      gs_fcat-checkbox = abap_true.

    ELSEIF gs_fcat-fieldname = 'BP'.
      gs_fcat-coltext = 'Select'.
      gs_fcat-checkbox = abap_true.

    ELSEIF gs_fcat-fieldname = 'PERNR'.
      gs_fcat-coltext = 'Employee Number'.

    ELSEIF gs_fcat-fieldname = 'NAME'.
      gs_fcat-coltext = 'Employee Name'.

    ELSEIF gs_fcat-fieldname = 'POSITION'.
      gs_fcat-coltext = 'Position ID'.

    ELSEIF gs_fcat-fieldname = 'POSDESS'.
      gs_fcat-coltext = 'Pos short Des'.

    ELSEIF gs_fcat-fieldname = 'POSDESL'.
      gs_fcat-coltext = 'Pos long Des'.

    ELSEIF gs_fcat-fieldname = 'REPMNG'.
      gs_fcat-coltext = 'Old RepManager'.

    ELSEIF gs_fcat-fieldname = 'RMNAME'.
      gs_fcat-coltext = 'Old Repmgr name'.

    ELSEIF gs_fcat-fieldname = 'RMPOS'.
      gs_fcat-coltext = 'Old Repmgr Position'.

    ELSEIF gs_fcat-fieldname = 'RMPOSDES'.
      gs_fcat-coltext = 'Old Repmgr Posdes'.

    ELSEIF gs_fcat-fieldname = 'DATE'.
      gs_fcat-f4availabl = 'X'.
      gs_fcat-coltext = 'Date'.
      gs_fcat-ref_table = 'SYST'.
      gs_fcat-ref_field = 'DATUM'.

    ELSEIF gs_fcat-fieldname = 'NEWREP'.
      gs_fcat-coltext = 'New RepManager'.
      gs_fcat-f4availabl = 'X'.

    ELSEIF gs_fcat-fieldname = 'NEWREPNAME'.
      gs_fcat-coltext = 'New Repmgr name'.

    ELSEIF gs_fcat-fieldname = 'NEWREPOS'.
      gs_fcat-coltext = 'New Repmgr Position'.

    ELSEIF gs_fcat-fieldname = 'BP_CREATE'.
      gs_fcat-coltext = 'BPcreate'.

    ELSEIF gs_fcat-fieldname = 'BP_EXTEND'.
      gs_fcat-seltext = 'BPext'.

    ELSEIF gs_fcat-fieldname = 'BP_REL'.
      gs_fcat-seltext = 'BPRelshp'.

    ELSEIF gs_fcat-fieldname = 'CC_REL'.
      gs_fcat-seltext = 'CCrelshp'.

    ELSEIF gs_fcat-fieldname = 'MESSAGE'.
      gs_fcat-coltext = 'Message'.

    ENDIF.
    APPEND gs_fcat TO gt_fcat.
    CLEAR gs_fcat.
  ENDIF.


ENDFORM.

*&---------------------------------------------------------------------*
*& Form propose_email
FORM propose_email .

*  DATA: lt_propose TYPE STANDARD TABLE OF zhr_position_tab,
*        ls_propose TYPE zhr_position_tab.

  REFRESH: lt_attachment.

  SELECT * FROM zhr_position_tab
           INTO TABLE @DATA(lt_propose)
           FOR ALL ENTRIES IN @gt_display
           WHERE reference_no = @gt_display-reference_no
           AND status = '02'.

  CLASS cl_abap_char_utilities DEFINITION LOAD.
  CONSTANTS:
    con_tab  TYPE c VALUE cl_abap_char_utilities=>horizontal_tab,
    con_cret TYPE c VALUE cl_abap_char_utilities=>cr_lf.

  CONCATENATE 'Orgunit_Id'
              'Orgunit_des'
              'Start_date'
              'Job ID'
              'Job_des'
              'Reporting Manager'
              'Repmanager Name'
              'Repmanager_pos'
              'Repmanager Pos Name'
              'Created_by'
              'Created_on'
            INTO ls_attachment SEPARATED BY con_tab.

  APPEND ls_attachment TO lt_attachment.
  CLEAR  ls_attachment.

  CONCATENATE  ls_attachment lv_data
     INTO lv_data SEPARATED BY cl_abap_char_utilities=>newline.

  LOOP AT lt_propose ASSIGNING FIELD-SYMBOL(<ls_propose>).

    CLEAR lv_date.
    WRITE <ls_propose>-start_date TO lv_date DD/MM/YYYY.

    CLEAR lv_date1.
    WRITE <ls_propose>-created_on TO lv_date1 DD/MM/YYYY.

** Orgunit Description ***
    CLEAR lv_orgname.
    SELECT SINGLE stext INTO lv_orgname FROM hrp1000
                      WHERE plvar = '01'
                      AND otype = 'O'
                      AND objid = <ls_propose>-orgunit_id
                      AND begda LE sy-datum
                      AND endda GE sy-datum.

**Rreporting Manager Position Description **
    CLEAR lv_posname.
    SELECT SINGLE stext INTO lv_posname FROM hrp1000
                      WHERE plvar = '01'
                      AND otype = 'S'
                      AND objid = <ls_propose>-repmanager_pos
                      AND begda LE sy-datum
                      AND endda GE sy-datum.
** Job ID Description **
    CLEAR lv_jobname.
    SELECT SINGLE stext INTO lv_jobname FROM hrp1000
                      WHERE plvar = '01'
                      AND otype = 'C'
                      AND objid = <ls_propose>-stell
                      AND begda LE sy-datum
                      AND endda GE sy-datum.
** RepManager Name ***
    CLEAR: gs_name,lv_mgrname.
    READ TABLE gt_name INTO gs_name WITH KEY pernr = <ls_propose>-rep_manager.
    IF sy-subrc EQ 0.
      lv_mgrname = gs_name-ename.
    ENDIF.

** Build excel data for Mail **
    CONCATENATE <ls_propose>-orgunit_id
                lv_orgname
                lv_date
                <ls_propose>-stell
                lv_jobname
                <ls_propose>-rep_manager
                lv_mgrname
                <ls_propose>-repmanager_pos
                lv_posname
                <ls_propose>-created_by
                lv_date1
                INTO ls_attachment SEPARATED BY con_tab.

    CONCATENATE con_cret ls_attachment
    INTO ls_attachment.
    APPEND ls_attachment TO lt_attachment.
    CLEAR ls_attachment.

  ENDLOOP.

  CALL FUNCTION 'SCMS_TEXT_TO_BINARY'
    TABLES
      text_tab   = lt_attachment
      binary_tab = lt_bin.


  CLEAR lv_date.
  WRITE sy-datum TO lv_date DD/MM/YYYY.

  "create send request
  TRY.
      lo_send_request = cl_bcs=>create_persistent( ).
    CATCH  cx_send_req_bcs INTO DATA(lx_send_req_bcs1).
  ENDTRY.

  salutation ='Dear Sir/Madam ,'.
  APPEND salutation TO lt_message_body.
  APPEND INITIAL LINE TO lt_message_body.

  CLEAR: lv_body.
  CONCATENATE 'New Position are Proposed for Creation on' lv_date INTO lv_body SEPARATED BY space.
  CONDENSE lv_body.
  body = lv_body.
  CONDENSE body.
  APPEND body TO lt_message_body.
  APPEND INITIAL LINE TO lt_message_body.

  footer = 'Thanks & Best Regards,'.
  CONDENSE footer.
  APPEND footer TO lt_message_body.

  footer = 'Sheenlac Paints Ltd'.
  CONDENSE footer.
  APPEND footer TO lt_message_body.

  lv_tot = 'Position Creation Proposal'.
  "put your text into the document
  TRY.
      lo_document = cl_document_bcs=>create_document(
      i_type = 'RAW'
      i_text = lt_message_body
      i_subject = lv_tot ).
    CATCH cx_document_bcs INTO DATA(lc_document_bcs1).
  ENDTRY.

  APPEND LINES OF lt_attachment TO it_body_msg.

  TRY.
      lo_document->add_attachment(
      EXPORTING
      i_attachment_type = 'XLS'
      i_attachment_subject = 'Position Creation Proposal'
      i_att_content_text = lt_attachment ).
    CATCH cx_document_bcs INTO lx_document_bcs.
  ENDTRY.

* Pass the document to send request
  TRY.
      lo_send_request->set_document( lo_document ).
    CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs2).
  ENDTRY.

  CLEAR ls_tvarvc.
  SELECT SINGLE * FROM tvarvc INTO ls_tvarvc WHERE name = 'ZHR_POS_PROP'
                                             AND type = 'P'.
  sender_mail = ls_tvarvc-low .
  "Sender Mail ID
  TRY.
      lo_sender = cl_cam_address_bcs=>create_internet_address( sender_mail ).
    CATCH cx_address_bcs INTO DATA(lx_address_bcs2).
  ENDTRY.

  TRY.
      lo_send_request->set_sender( lo_sender ).
    CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs3).
  ENDTRY.

  SELECT * FROM zhr_hiring_email
           INTO TABLE @DATA(lt_mail)
           WHERE id_val = '03'.
** TO Mail Recipients **
  LOOP AT lt_mail ASSIGNING FIELD-SYMBOL(<ls_mail_to>) WHERE e_type = 'TO'.
    in_mailid = <ls_mail_to>-email.
    APPEND in_mailid TO it_mailid.
    CLEAR in_mailid.
  ENDLOOP.
** CC Mail Recipients
  LOOP AT lt_mail ASSIGNING FIELD-SYMBOL(<ls_mail_cc>) WHERE e_type = 'CC'.
    in_mailid1 = <ls_mail_cc>-email.
    APPEND in_mailid1 TO it_mailid1.
    CLEAR in_mailid1.
  ENDLOOP.
** TO Mail Recipients **
  LOOP AT it_mailid INTO in_mailid.
    TRY.
        lo_recipient = cl_cam_address_bcs=>create_internet_address( in_mailid ).
      CATCH cx_address_bcs INTO DATA(lx_address_bcs1).
    ENDTRY.
*set recipient
    TRY.
        lo_send_request->add_recipient(
           EXPORTING
           i_recipient = lo_recipient
            i_express = abap_true
           ).
      CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs4).
    ENDTRY.
  ENDLOOP.
** CC Mail Recipients
  LOOP AT it_mailid1 INTO in_mailid1.

    TRY.
        lo_recipient1 = cl_cam_address_bcs=>create_internet_address( in_mailid1 ).
      CATCH cx_address_bcs INTO DATA(lx_address_bcs).
    ENDTRY.
    "Set recipient
    TRY.
        lo_send_request->add_recipient(
         EXPORTING
         i_recipient = lo_recipient1
         i_copy = 'X'
         i_express = abap_true
         ).
        in_mailid2 = in_mailid1.
      CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs5).
    ENDTRY.
  ENDLOOP.
* Send email
  TRY.
      lo_send_request->send(
      EXPORTING
      i_with_error_screen = abap_true
      RECEIVING
      result = lv_sent_to_all ).
    CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs).
  ENDTRY.

  COMMIT WORK.

  CLEAR : in_mailid,in_mailid1.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form approve_mail
*&---------------------------------------------------------------------*
FORM approve_mail .

  DATA: lv_date1(10) TYPE c.
  DATA: lv_date2(10) TYPE c.

  REFRESH: lt_attachment.

  SELECT * FROM zhr_position_tab
           INTO TABLE @DATA(lt_approve)
           FOR ALL ENTRIES IN @gt_display
           WHERE reference_no = @gt_display-reference_no
           AND status = '03'.

  CLASS cl_abap_char_utilities DEFINITION LOAD.
  CONSTANTS:
    con_tab  TYPE c VALUE cl_abap_char_utilities=>horizontal_tab,
    con_cret TYPE c VALUE cl_abap_char_utilities=>cr_lf.

  CONCATENATE 'Orgunit_Id'
              'Orgunit_des'
              'Position_id'
              'Pos_sdes'
              'pos_ldes'
              'Start_date'
              'Job ID'
              'Job_des'
              'Reporting Manager'
              'Repmanager Name'
              'Repmanager_pos'
              'Repmanager Pos Name'
              'Created_by'
              'Created_on'
              'Approved_by'
              'Approved_on'
            INTO ls_attachment SEPARATED BY con_tab.

  APPEND ls_attachment TO lt_attachment.
  CLEAR  ls_attachment.

  CONCATENATE  ls_attachment lv_data
     INTO lv_data SEPARATED BY cl_abap_char_utilities=>newline.

  LOOP AT lt_approve ASSIGNING FIELD-SYMBOL(<ls_approve>).

    CLEAR lv_date.
    WRITE <ls_approve>-start_date TO lv_date DD/MM/YYYY.

    CLEAR lv_date1.
    WRITE <ls_approve>-created_on TO lv_date1 DD/MM/YYYY.

    CLEAR lv_date2.
    WRITE <ls_approve>-approved_on TO lv_date2 DD/MM/YYYY.

** Orgunit Description ***
    CLEAR lv_orgname.
    SELECT SINGLE stext INTO lv_orgname FROM hrp1000
                      WHERE plvar = '01'
                      AND otype = 'O'
                      AND objid = <ls_approve>-orgunit_id
                      AND begda LE sy-datum
                      AND endda GE sy-datum.

**Rreporting Manager Position Description **
    CLEAR lv_posname.
    SELECT SINGLE stext INTO lv_posname FROM hrp1000
                      WHERE plvar = '01'
                      AND otype = 'S'
                      AND objid = <ls_approve>-repmanager_pos
                      AND begda LE sy-datum
                      AND endda GE sy-datum.
** Job ID Description **
    CLEAR lv_jobname.
    SELECT SINGLE stext INTO lv_jobname FROM hrp1000
                      WHERE plvar = '01'
                      AND otype = 'C'
                      AND objid = <ls_approve>-stell
                      AND begda LE sy-datum
                      AND endda GE sy-datum.
** RepManager Name ***
    CLEAR: gs_name,lv_mgrname.
    READ TABLE gt_name INTO gs_name WITH KEY pernr = <ls_approve>-rep_manager.
    IF sy-subrc EQ 0.
      lv_mgrname = gs_name-ename.
    ENDIF.

** Build excel data for Mail **
    CONCATENATE <ls_approve>-orgunit_id
                lv_orgname
                <ls_approve>-position_id
                <ls_approve>-pos_sdes
                <ls_approve>-pos_ldes
                lv_date
                <ls_approve>-stell
                lv_jobname
                <ls_approve>-rep_manager
                lv_mgrname
                <ls_approve>-repmanager_pos
                lv_posname
                <ls_approve>-created_by
                lv_date1
                <ls_approve>-approved_by
                lv_date2
                INTO ls_attachment SEPARATED BY con_tab.

    CONCATENATE con_cret ls_attachment
    INTO ls_attachment.
    APPEND ls_attachment TO lt_attachment.
    CLEAR ls_attachment.

  ENDLOOP.

  CALL FUNCTION 'SCMS_TEXT_TO_BINARY'
    TABLES
      text_tab   = lt_attachment
      binary_tab = lt_bin.


  CLEAR lv_date.
  WRITE sy-datum TO lv_date DD/MM/YYYY.

  "create send request
  TRY.
      lo_send_request = cl_bcs=>create_persistent( ).
    CATCH  cx_send_req_bcs INTO DATA(lx_send_req_bcs1).
  ENDTRY.

  salutation ='Dear Sir/Madam ,'.
  APPEND salutation TO lt_message_body.
  APPEND INITIAL LINE TO lt_message_body.

  CLEAR: lv_body.
  CONCATENATE 'New Position are Created on' lv_date INTO lv_body SEPARATED BY space.
  CONDENSE lv_body.
  body = lv_body.
  CONDENSE body.
  APPEND body TO lt_message_body.
  APPEND INITIAL LINE TO lt_message_body.

  footer = 'Thanks & Best Regards,'.
  CONDENSE footer.
  APPEND footer TO lt_message_body.

  footer = 'Sheenlac Paints Ltd'.
  CONDENSE footer.
  APPEND footer TO lt_message_body.

  lv_tot = 'Position Creation Approval'.

  "put your text into the document
  TRY.
      lo_document = cl_document_bcs=>create_document(
      i_type = 'RAW'
      i_text = lt_message_body
      i_subject = lv_tot ).
    CATCH cx_document_bcs INTO DATA(lx_document_bcs1).
  ENDTRY.

  APPEND LINES OF lt_attachment TO it_body_msg.

  TRY.
      lo_document->add_attachment(
      EXPORTING
      i_attachment_type = 'XLS'
      i_attachment_subject = 'Position Creation Approval'
      i_att_content_text = lt_attachment ).
    CATCH cx_document_bcs INTO lx_document_bcs.
  ENDTRY.

* pass the document to send request
  TRY.
      lo_send_request->set_document( lo_document ).
    CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs2).
  ENDTRY.

  CLEAR ls_tvarvc.
  SELECT SINGLE * FROM tvarvc INTO ls_tvarvc WHERE name = 'ZHR_POS_PROP'
                                             AND type = 'P'.
  sender_mail = ls_tvarvc-low .

  "Sender Mail ID
  TRY.
      lo_sender = cl_cam_address_bcs=>create_internet_address( sender_mail ).
    CATCH cx_address_bcs INTO DATA(lx_address_bcs1).
  ENDTRY.

  TRY.
      lo_send_request->set_sender( lo_sender ).
    CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs3).
  ENDTRY.

*** Getting the Mail ID Details to who the mail has to sent***
  SELECT * FROM zhr_hiring_email
           INTO TABLE @DATA(lt_mail)
           WHERE id_val = '04'.
*** TO Mail Recipients ***
  LOOP AT lt_mail ASSIGNING FIELD-SYMBOL(<lfs_mail_to>) WHERE e_type = 'TO'.
    in_mailid = <lfs_mail_to>-email.
    APPEND in_mailid TO it_mailid.
    CLEAR in_mailid.
  ENDLOOP.
*** CC Mail Recipients ***
  LOOP AT lt_mail ASSIGNING FIELD-SYMBOL(<lfs_mail_cc>)  WHERE e_type = 'CC'.
    in_mailid1 = <lfs_mail_cc>-email.
    APPEND in_mailid1 TO it_mailid1.
    CLEAR in_mailid1.
  ENDLOOP.
** TO Mail Recipients **
  LOOP AT it_mailid INTO in_mailid.
    TRY.
        lo_recipient = cl_cam_address_bcs=>create_internet_address( in_mailid ).
      CATCH cx_address_bcs INTO DATA(lx_address_bcs2).
    ENDTRY.
*set recipient
    TRY.
        lo_send_request->add_recipient(
           EXPORTING
           i_recipient = lo_recipient
            i_express = abap_true
           ).
      CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs4).
    ENDTRY.
  ENDLOOP.
** CC Mail Recipients
  LOOP AT it_mailid1 INTO in_mailid1.
    TRY.
        lo_recipient1 = cl_cam_address_bcs=>create_internet_address( in_mailid1 ).
      CATCH cx_address_bcs INTO DATA(lx_address_bcs3).
    ENDTRY.
    "Set recipient
    TRY.
        lo_send_request->add_recipient(
         EXPORTING
         i_recipient = lo_recipient1
         i_copy = 'X'
         i_express = abap_true
         ).
      CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs5).
    ENDTRY.
  ENDLOOP.
* Send email
  TRY.
      lo_send_request->send(
      EXPORTING
      i_with_error_screen = abap_true
      RECEIVING
      result = lv_sent_to_all ).
    CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs6).
  ENDTRY.

  COMMIT WORK.

  CLEAR : in_mailid,in_mailid1.

ENDFORM.
** F4 help for orgunit ID **
FORM f4_valuehelp USING p_name TYPE rsrestrict-objectname.

  DATA: lt_map    TYPE TABLE OF dselc,
        ls_map    TYPE dselc,
        lt_return TYPE TABLE OF ddshretval,
        ls_return TYPE ddshretval,
        ls_stable TYPE lvc_s_stbl.

  DATA: ls_selec TYPE objec,
        lt_selec TYPE TABLE OF objec,
        ls_plvar TYPE plogi-plvar,
        ls_otype TYPE plogi-otype.

  IF p_name = 'S_ORGID-LOW'. "Orgunit ID Help
    CLEAR ls_selec.
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

    IF ls_selec-objid IS NOT INITIAL.
      s_orgid-low = ls_selec-objid.  "Organizational Unit
    ENDIF.

  ELSEIF p_name = 'S_ORGID-HIGH'.
    CLEAR ls_selec.
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

    IF ls_selec-objid IS NOT INITIAL.
      s_orgid-high = ls_selec-objid. "Organizational Unit
    ENDIF.

  ELSEIF p_name = 'S_JOBID-LOW'. "Job ID help
    CLEAR ls_selec.
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
    IF ls_selec-objid IS NOT INITIAL.
      s_jobid-low = ls_selec-objid.  "Job ID
    ENDIF.

  ELSEIF p_name = 'S_JOBID-HIGH'.
    CLEAR ls_selec.
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
    IF ls_selec-objid IS NOT INITIAL.
      s_jobid-high = ls_selec-objid."Job ID
    ENDIF.

  ELSEIF p_name = 'S_REPMG-LOW'.
    CLEAR ls_selec.
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
    IF ls_selec-objid IS NOT INITIAL.
      s_repmg-low = ls_selec-objid.  "Reporting Manager Position
    ENDIF.

  ELSEIF p_name = 'S_REPMG-HIGH'.
    CLEAR ls_selec.
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
    IF ls_selec-objid IS NOT INITIAL.
      s_repmg-high = ls_selec-objid. "Reporting Manager Position
    ENDIF.
  ENDIF.


ENDFORM.
*** Function Module to get all the employee Details ***
FORM fm_to_get_empdetails.
*** Function Module to get pernr details ***
  DATA: lt_emp_details TYPE STANDARD TABLE OF zhr_emp_details.
  DATA: lt_pernr TYPE p3pr_rt_pernr.

  REFRESH: lt_pernr,lt_emp_details.

  LOOP AT gt_pernr ASSIGNING FIELD-SYMBOL(<gs_pernr>).
    APPEND VALUE #( sign = 'I'
                    option = 'EQ'
                    low = <gs_pernr>-pernr ) TO lt_pernr.
  ENDLOOP.

  CALL FUNCTION 'ZHR_EMP_DETAILS'
    EXPORTING
      in_pernr          = lt_pernr[]
    TABLES
      it_hr_emp_details = lt_emp_details.

  LOOP AT lt_emp_details ASSIGNING FIELD-SYMBOL(<fls_emp_det>).
    APPEND VALUE #(  pernr = <fls_emp_det>-pernr
                     name = <fls_emp_det>-sname
                     position = <fls_emp_det>-plans
                     posdess = <fls_emp_det>-short_pl
                     posdesl = <fls_emp_det>-stext_pl
                     repmng = <fls_emp_det>-origmgremp
                     rmname = VALUE #( gt_name[ pernr = <fls_emp_det>-origmgremp ]-ename OPTIONAL )
                     rmpos = <fls_emp_det>-origmgrpos
                     rmposdes = <fls_emp_det>-origmgposdes ) TO gt_chg_rep .
  ENDLOOP.

ENDFORM.
"Copy Position Details to New Position
FORM copy_pos_to_pos.

  TYPES: BEGIN OF ty_costcenter,
           kostl TYPE kostl,
           kokrs TYPE kokrs,
         END OF ty_costcenter.
  DATA: ls_costcenter TYPE ty_costcenter.

  DATA: lt_objid_dtls  TYPE TABLE OF p1000,
        lt_objtxt_dtls TYPE TABLE OF p1000,
        lt_rshpdtls    TYPE STANDARD TABLE OF p1001.

  SELECT objid FROM hrp1000
         INTO TABLE @DATA(lt_pos)
         WHERE plvar EQ '01'
         AND otype EQ 'S'
         AND objid IN @s_pos
         AND begda LE @sy-datum
         AND endda GE @sy-datum.
  IF sy-subrc = 0.
    SORT lt_pos[] BY objid.
    LOOP AT lt_pos ASSIGNING FIELD-SYMBOL(<fs_pos>).
      CLEAR gs_display.
      gs_display-frompos = <fs_pos>-objid.

* read company code from HRP1008
      SELECT SINGLE bukrs FROM hrp1008 INTO @DATA(lv_bukrs)
        WHERE plvar = '01'
          AND otype = 'S'
          AND objid = @gs_display-frompos
          AND begda <= @sy-datum
          AND endda >= @sy-datum.
      IF sy-subrc = 0.
        gs_display-bukrs = lv_bukrs.
      ENDIF.
      CLEAR lv_bukrs.

**** Read Position Details *****
      REFRESH: lt_objid_dtls.
      CALL FUNCTION 'RH_READ_INFTY_1000'
        EXPORTING
          authority        = 'DISP'
          with_stru_auth   = 'X'
          plvar            = '01'
          otype            = 'S'
          objid            = <fs_pos>-objid
          istat            = '1'
          begda            = sy-datum
          endda            = sy-datum
        TABLES
          i1000            = lt_objid_dtls
        EXCEPTIONS
          nothing_found    = 1
          wrong_condition  = 2
          wrong_parameters = 3
          OTHERS           = 4.
      IF sy-subrc = 0.
        DELETE lt_objid_dtls WHERE langu NE sy-langu.
        gs_display-pos_sdes = VALUE #( lt_objid_dtls[ 1 ]-short OPTIONAL ).
        gs_display-pos_ldes = VALUE #( lt_objid_dtls[ 1 ]-stext OPTIONAL ).
      ENDIF.

*---- Relationship Details ----*
      "Reporting Manager
      SELECT SINGLE sobid FROM hrp1001 INTO @DATA(l_repmng_pos)
        WHERE otype = 'S'
        AND plvar = '01'
        AND objid = @<fs_pos>-objid
        AND rsign = 'A'
        AND relat = '002'
        AND begda LE @sy-datum
        AND endda GE @sy-datum
        AND sclas = 'S'.
      IF sy-subrc EQ 0.
        gs_display-repmanager_pos = l_repmng_pos.
        REFRESH: lt_objtxt_dtls.
        CALL FUNCTION 'RH_READ_HRP_OBJECT_TEXT'
          EXPORTING
            act_langu       = sy-langu
            act_otype       = 'S'
            act_objid       = gs_display-repmanager_pos
            act_plvar       = '01'
            act_begda       = sy-datum
            act_endda       = sy-datum
            act_state       = '1'
          TABLES
            act_p1000       = lt_objtxt_dtls
          EXCEPTIONS
            no_data_found   = 1
            no_valid_object = 2
            OTHERS          = 3.
        IF sy-subrc = 0.
          gs_display-repmng_pdes = VALUE #( lt_objtxt_dtls[ 1 ]-stext OPTIONAL ).
        ENDIF.
*Reporting Manager Personel Number
        SELECT SINGLE sobid FROM hrp1001
          INTO @DATA(l_pernr)
          WHERE otype = 'S'
          AND objid = @gs_display-repmanager_pos
          AND plvar = '01'
          AND rsign = 'A'
          AND relat = '008'
          AND istat = '1'
          AND begda LE @sy-datum
          AND endda GE @sy-datum
          AND sclas = 'P'.
        IF sy-subrc = 0.
          gs_display-rep_manager = l_pernr.
        ENDIF.
*Reporting Manager Name
        SELECT SINGLE ename FROM pa0001
          INTO gs_display-repmng_name
          WHERE pernr = gs_display-rep_manager
          AND begda LE sy-datum
          AND endda GE sy-datum.
      ENDIF.
      "Orguinit ID relationship
      SELECT SINGLE sobid FROM hrp1001 INTO @DATA(l_orgunit)
        WHERE otype = 'S'
        AND plvar = '01'
        AND objid = @<fs_pos>-objid
        AND rsign = 'A'
        AND relat = '003'
        AND begda LE @sy-datum
        AND endda GE @sy-datum
        AND sclas = 'O'.
      IF sy-subrc EQ 0.
        gs_display-orgunit_id = l_orgunit.
        REFRESH: lt_objtxt_dtls.
        CALL FUNCTION 'RH_READ_HRP_OBJECT_TEXT'
          EXPORTING
            act_langu       = sy-langu
            act_otype       = 'O'
            act_objid       = gs_display-orgunit_id
            act_plvar       = '01'
            act_begda       = sy-datum
            act_endda       = sy-datum
            act_state       = '1'
          TABLES
            act_p1000       = lt_objtxt_dtls
          EXCEPTIONS
            no_data_found   = 1
            no_valid_object = 2
            OTHERS          = 3.
        IF sy-subrc = 0.
          gs_display-orgunit_des = VALUE #( lt_objtxt_dtls[ 1 ]-stext OPTIONAL ).
        ENDIF.
      ENDIF.

      "Job Code Relationship
      SELECT SINGLE sobid FROM hrp1001 INTO @DATA(l_jobcode)
            WHERE otype = 'S'
            AND plvar = '01'
            AND objid = @<fs_pos>-objid
            AND rsign = 'B'
            AND relat = '007'
            AND begda LE @sy-datum
            AND endda GE @sy-datum
            AND sclas = 'C'.
      IF sy-subrc EQ 0.
        gs_display-stell = l_jobcode.
        REFRESH: lt_objtxt_dtls.
        CALL FUNCTION 'RH_READ_HRP_OBJECT_TEXT'
          EXPORTING
            act_langu       = sy-langu
            act_otype       = 'C'
            act_objid       = gs_display-stell
            act_plvar       = '01'
            act_begda       = sy-datum
            act_endda       = sy-datum
            act_state       = '1'
          TABLES
            act_p1000       = lt_objtxt_dtls
          EXCEPTIONS
            no_data_found   = 1
            no_valid_object = 2
            OTHERS          = 3.
        IF sy-subrc = 0.
          gs_display-job_des = VALUE #( lt_objtxt_dtls[ 1 ]-stext OPTIONAL ).
        ENDIF.
      ENDIF.
      "Costcenter
      SELECT SINGLE sobid FROM hrp1001 INTO gs_display-costcenter
            WHERE otype = 'S'
            AND plvar = '01'
            AND objid = <fs_pos>-objid
            AND rsign = 'A'
            AND relat = '011'
            AND begda LE sy-datum
            AND endda GE sy-datum
            AND sclas = 'K'.
      IF sy-subrc EQ 0.

      ENDIF.
*Travel Category Details
      SELECT SINGLE * FROM hrp9013
        INTO @DATA(ls_travel)
        WHERE plvar = '01'
        AND otype = 'S'
        AND objid = @<fs_pos>-objid
        AND istat = '1'
        AND begda LE @sy-datum
        AND endda GE @sy-datum.
      IF sy-subrc = 0.
        gs_display-travel = ls_travel-travel.
        gs_display-category = ls_travel-category.
      ENDIF.

      APPEND gs_display TO gt_display.
    ENDLOOP.
  ENDIF.
ENDFORM.
