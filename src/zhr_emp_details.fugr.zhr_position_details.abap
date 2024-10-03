FUNCTION zhr_position_details.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(POSITION) TYPE  LSO_RANGE_OBJID_TAB OPTIONAL
*"  TABLES
*"      IT_POSITION STRUCTURE  ZHR_POSTION_TAB
*"----------------------------------------------------------------------

  TYPES: BEGIN OF ty_hrp1000,
           otype TYPE hrp1000-otype,
           objid TYPE hrp1000-objid,
           short TYPE hrp1000-short,
           stext TYPE hrp1000-stext,
         END OF ty_hrp1000.

  TYPES: BEGIN OF ty_hrp1001,
           otype TYPE hrp1001-otype,
           objid TYPE hrp1001-objid,
           rsign TYPE hrp1001-rsign,
           relat TYPE hrp1001-relat,
           sclas TYPE hrp1001-sclas,
           sobid TYPE hrp1001-sobid,
         END OF ty_hrp1001.

  DATA: It_hrp1000_S  TYPE TABLE OF ty_hrp1000,
        It_hrp1000_O  TYPE TABLE OF ty_hrp1000,
        It_hrp1000_C  TYPE TABLE OF ty_hrp1000,
        wa_hrp1000_s  TYPE ty_hrp1000,
        wa_hrp1000_o  TYPE ty_hrp1000,
        wa_hrp1000_c  TYPE ty_hrp1000,
        wa_hrp1000_s2 TYPE ty_hrp1000.


  DATA: it_hrp1001_s TYPE TABLE OF ty_hrp1001,
        wa_hrp1001_s TYPE ty_hrp1001.

  DATA: it_hrp1001_p TYPE TABLE OF ty_hrp1001,
        wa_hrp1001_p TYPE ty_hrp1001.

  DATA: it_hrp1001_m TYPE TABLE OF ty_hrp1001,
        wa_hrp1001_m TYPE ty_hrp1001.

  DATA: it_hrp1001_k TYPE TABLE OF ty_hrp1001,
        wa_hrp1001_k TYPE ty_hrp1001.

  DATA: it_hrp1001_O TYPE TABLE OF ty_hrp1001,
        wa_hrp1001_O TYPE ty_hrp1001.

  DATA: it_hrp1001_C TYPE TABLE OF ty_hrp1001,
        wa_hrp1001_C TYPE ty_hrp1001.

  DATA: it_hrp1001_bp TYPE TABLE OF ty_hrp1001,
        wa_hrp1001_bp TYPE ty_hrp1001.

  DATA: it_hrp9010 TYPE TABLE OF hrp9010,
        wa_hrp9010 TYPE hrp9010.

  DATA: it_hrp9011 TYPE TABLE OF hrp9011,
        wa_hrp9011 TYPE hrp9011.

  DATA: it_hrp9013 TYPE TABLE OF hrp9013,
        wa_hrp9013 TYPE hrp9013.

  DATA: it_hrp1008 TYPE TABLE OF hrp1008,
        wa_hrp1008 TYPE hrp1008.

  DATA: wa_position TYPE lso_range_objid.
  DATA: wa_orgunit TYPE lso_range_objid,
        lt_orgunit TYPE lso_range_objid_tab,
        wa_jobcode TYPE lso_range_objid,
        lt_jobcode TYPE lso_range_objid_tab.

  DATA: wa_position_tab TYPE zhr_postion_tab.

  DATA: lt_tvarvc_pos TYPE TABLE OF tvarvc,
        ls_tvarvc_pos TYPE tvarvc.

  CONSTANTS: lv_tvarvc_n TYPE tvarvc-name VALUE 'ZHR_TS_SALESPOS',
             lv_tvarvc_S TYPE tvarvc-type VALUE 'S'.

  SELECT * FROM tvarvc INTO TABLE lt_tvarvc_pos WHERE name = lv_tvarvc_n AND type = lv_tvarvc_S.
  IF sy-subrc = 0.
*      do nothing
  ENDIF.

  IF sy-sysid NE 'DEV'.
    IF position[] IS INITIAL.
      CLEAR wa_position.
      wa_position-sign = 'I'.
      wa_position-option = 'BT'.
      wa_position-low = '20000000'.
      wa_position-high = '29999999'.
      APPEND wa_position TO position.
    ENDIF.

    CLEAR wa_orgunit.
    wa_orgunit-sign = 'I'.
    wa_orgunit-option = 'BT'.
    wa_orgunit-low = '40000000'.
    wa_orgunit-high = '49999999'.
    APPEND wa_orgunit TO lt_orgunit.

    CLEAR wa_jobcode.
    wa_jobcode-sign = 'I'.
    wa_jobcode-option = 'BT'.
    wa_jobcode-low = '60000000'.
    wa_jobcode-high = '69999999'.
    APPEND wa_jobcode TO lt_jobcode.
  ELSE.
    IF sy-mandt NE '500'.
      IF position[] IS INITIAL.
        CLEAR wa_position.
        wa_position-sign = 'I'.
        wa_position-option = 'BT'.
        wa_position-low = '20000000'.
        wa_position-high = '29999999'.
        APPEND wa_position TO position.
      ENDIF.

      CLEAR wa_orgunit.
      wa_orgunit-sign = 'I'.
      wa_orgunit-option = 'BT'.
      wa_orgunit-low = '40000000'.
      wa_orgunit-high = '49999999'.
      APPEND wa_orgunit TO lt_orgunit.

      CLEAR wa_jobcode.
      wa_jobcode-sign = 'I'.
      wa_jobcode-option = 'BT'.
      wa_jobcode-low = '60000000'.
      wa_jobcode-high = '69999999'.
      APPEND wa_jobcode TO lt_jobcode.
    ENDIF.
  ENDIF.


* get all active position details.
  SELECT otype objid short stext
    FROM hrp1000
    INTO TABLE It_hrp1000_S
    WHERE plvar = '01'
      AND otype = 'S'
      AND objid IN position
      AND istat = '1'
      AND begda <= sy-datum
      AND endda >= sy-datum
      AND langu = sy-langu.
  IF sy-subrc = 0.
    SORT It_hrp1000_S BY objid.
  ENDIF.

* get all active orgunit details.
  SELECT otype objid short stext
    FROM hrp1000
    INTO TABLE It_hrp1000_o
    WHERE plvar = '01'
      AND otype = 'O'
      AND objid IN lt_orgunit
      AND istat = '1'
      AND begda <= sy-datum
      AND endda >= sy-datum
      AND langu = sy-langu.
  IF sy-subrc = 0.
    SORT It_hrp1000_O BY objid.
  ENDIF.

* get all active Job Codes details.
  SELECT otype objid short stext
    FROM hrp1000
    INTO TABLE It_hrp1000_C
    WHERE plvar = '01'
      AND otype = 'C'
      AND objid IN lt_jobcode
      AND istat = '1'
      AND begda <= sy-datum
      AND endda >= sy-datum
      AND langu = sy-langu.
  IF sy-subrc = 0.
    SORT It_hrp1000_C BY objid.
  ENDIF.

  IF it_hrp1000_s[] IS NOT INITIAL.
    SORT It_hrp1000_S BY objid.
*Position to Orgunit
    SELECT otype objid rsign relat sclas sobid
      FROM hrp1001
      INTO TABLE it_hrp1001_s
      FOR ALL ENTRIES IN it_hrp1000_s
     WHERE otype = 'S'
       AND objid = It_hrp1000_S-objid
       AND plvar = '01'
       AND rsign = 'A'
       AND relat = '003'
       AND istat = '1'
       AND begda <= sy-datum
       AND endda >= sy-datum
       AND sclas = 'O'.
    IF sy-subrc = 0.
      SORT it_hrp1001_s BY objid.
    ENDIF.

* Position to Employee
    SELECT otype objid rsign relat sclas sobid
      FROM hrp1001
      INTO TABLE it_hrp1001_p
      FOR ALL ENTRIES IN it_hrp1000_s
     WHERE otype = 'S'
       AND objid = It_hrp1000_S-objid
       AND plvar = '01'
       AND rsign = 'A'
       AND relat = '008'
       AND istat = '1'
       AND begda <= sy-datum
       AND endda >= sy-datum
       AND sclas = 'P'.
    IF sy-subrc = 0.
      SORT it_hrp1001_P BY objid.
    ENDIF.

* Position to Manager
    SELECT otype objid rsign relat sclas sobid
      FROM hrp1001
      INTO TABLE it_hrp1001_m
      FOR ALL ENTRIES IN it_hrp1000_s
     WHERE otype = 'S'
       AND objid = It_hrp1000_S-objid
       AND plvar = '01'
       AND rsign = 'A'
       AND relat = '002'
       AND istat = '1'
       AND begda <= sy-datum
       AND endda >= sy-datum
       AND sclas = 'S'.
    IF sy-subrc = 0.
      SORT it_hrp1001_M BY objid.
    ENDIF.

* Position to Costcenter
    SELECT otype objid rsign relat sclas sobid
      FROM hrp1001
      INTO TABLE it_hrp1001_k
      FOR ALL ENTRIES IN it_hrp1000_s
     WHERE otype = 'S'
       AND objid = It_hrp1000_S-objid
       AND plvar = '01'
       AND rsign = 'A'
       AND relat = '011'
       AND istat = '1'
       AND begda <= sy-datum
       AND endda >= sy-datum
       AND sclas = 'K'.
    IF sy-subrc = 0.
      SORT it_hrp1001_K BY objid.
    ENDIF.

* Position to orgunit
    SELECT otype objid rsign relat sclas sobid
      FROM hrp1001
      INTO TABLE it_hrp1001_O
      FOR ALL ENTRIES IN it_hrp1000_s
     WHERE otype = 'S'
       AND objid = It_hrp1000_S-objid
       AND plvar = '01'
       AND rsign = 'A'
       AND relat = '003'
       AND istat = '1'
       AND begda <= sy-datum
       AND endda >= sy-datum
       AND sclas = 'O'.
    IF sy-subrc = 0.
      SORT it_hrp1001_O BY objid.
    ENDIF.

* Position to Job Code
    SELECT otype objid rsign relat sclas sobid
      FROM hrp1001
      INTO TABLE it_hrp1001_C
      FOR ALL ENTRIES IN it_hrp1000_s
     WHERE otype = 'S'
       AND objid = It_hrp1000_S-objid
       AND plvar = '01'
       AND rsign = 'B'
       AND relat = '007'
       AND istat = '1'
       AND begda <= sy-datum
       AND endda >= sy-datum
       AND sclas = 'C'.
    IF sy-subrc = 0.
      SORT it_hrp1001_C BY objid.
    ENDIF.

* Position to BP Code
    SELECT otype objid rsign relat sclas sobid
      FROM hrp1001
      INTO TABLE it_hrp1001_bp
      FOR ALL ENTRIES IN it_hrp1000_s
     WHERE otype = 'S'
       AND objid = It_hrp1000_S-objid
       AND plvar = '01'
       AND rsign = 'A'
       AND relat = '008'
       AND istat = '1'
       AND begda <= sy-datum
       AND endda >= sy-datum
       AND sclas = 'BP'.
    IF sy-subrc = 0.
      SORT it_hrp1001_BP BY objid.
    ENDIF.

* Position to Company Assignment
    SELECT *
      FROM hrp1008
      INTO TABLE it_hrp1008
      FOR ALL ENTRIES IN it_hrp1000_s
     WHERE otype = 'S'
       AND objid = It_hrp1000_S-objid
       AND plvar = '01'
       AND istat = '1'
       AND begda <= sy-datum
       AND endda >= sy-datum.
    IF sy-subrc = 0.
      SORT it_hrp1008 BY objid.
    ENDIF.

*Infotype to find if the position is a Sales position
    SELECT *
      FROM hrp9010
      INTO TABLE it_hrp9010
      WHERE begda <= sy-datum
        AND endda >= sy-datum.
    IF sy-subrc = 0.
      SORT it_hrp9010 BY objid.
    ENDIF.

*infotype for travel category and travel allowed based on position
    SELECT *
      FROM hrp9011
      INTO TABLE it_hrp9011
      WHERE begda <= sy-datum
        AND endda >= sy-datum.
    IF sy-subrc = 0.
      SORT it_hrp9011 BY objid.
    ENDIF.

*infotype for Latitude and longitude based on position
    SELECT *
      FROM hrp9013
      INTO TABLE it_hrp9013
      WHERE begda <= sy-datum
        AND endda >= sy-datum.
    IF sy-subrc = 0.
      SORT it_hrp9013 BY objid.
    ENDIF.

    SELECT bukrs, butxt from t001 INTO TABLE @DATA(lt_t001).

    LOOP AT it_hrp1000_s INTO wa_hrp1000_s.

      CLEAR wa_position_tab.

      wa_position_tab-objid = wa_hrp1000_s-objid.
      wa_position_tab-stext = wa_hrp1000_s-stext.

      CLEAR wa_hrp1008.
      READ TABLE it_hrp1008 INTO wa_hrp1008 with key objid = wa_hrp1000_s-objid BINARY SEARCH.
      IF sy-subrc = 0.
        wa_position_tab-bukrs = wa_hrp1008-bukrs.

        READ TABLE lt_t001 INTO DATA(ls_t001) WITH key bukrs = wa_hrp1008-bukrs.
        IF sy-subrc = 0.
          wa_position_tab-butxt = ls_t001-butxt.
        ENDIF.
      ENDIF.

      CLEAR wa_hrp1001_s.
      READ TABLE It_hrp1001_s INTO wa_hrp1001_s WITH KEY objid = wa_hrp1000_s-objid BINARY SEARCH.
      IF sy-subrc = 0.

* get the employee id assigned with the position id
        CLEAR wa_hrp1001_p.
        READ TABLE It_hrp1001_p INTO wa_hrp1001_p WITH KEY objid = wa_hrp1000_s-objid BINARY SEARCH.
        IF sy-subrc = 0.
          wa_position_tab-empid = wa_hrp1001_p-sobid+0(8).
        ENDIF.

* get the employee type Sales/Non Sales
* if the orgunit has a HRP9010 entry then they belong to sales category
        CLEAR wa_hrp9010.
        READ TABLE it_hrp9010 INTO wa_hrp9010 WITH KEY objid = wa_hrp1001_s-sobid+0(8) BINARY SEARCH.
        IF sy-subrc = 0.
          wa_position_tab-emp_type = 'S'.
        ELSE.
          wa_position_tab-emp_type = 'N'.
        ENDIF.

        CLEAR: wa_hrp1001_m, wa_hrp1001_p, wa_hrp1001_k.

*get the manager position  and manager text
        READ TABLE It_hrp1001_m INTO wa_hrp1001_m WITH KEY objid = wa_hrp1000_s-objid BINARY SEARCH.
        IF sy-subrc = 0.
          wa_position_tab-mgr_pos = wa_hrp1001_m-sobid+0(8).

*Manager Position Description
          READ TABLE It_hrp1000_S INTO wa_hrp1000_s2 WITH KEY objid = wa_position_tab-mgr_pos BINARY SEARCH.
          IF sy-subrc = 0.
            wa_position_tab-mgr_pos_txt = wa_hrp1000_s2-stext.
          ELSE.
            IF wa_position_tab-mgr_pos IS NOT INITIAL.
              SELECT SINGLE stext
                INTO wa_position_tab-mgr_pos_txt
                FROM hrp1000
               WHERE plvar = '01'
                 AND otype = 'S'
                 AND objid = wa_position_tab-mgr_pos.
            ENDIF.
          ENDIF.

*Employee no Assigned to the manager ID
          CLEAR wa_hrp1001_p.
          READ TABLE It_hrp1001_p INTO wa_hrp1001_p WITH KEY objid = wa_position_tab-mgr_pos.
          IF sy-subrc = 0.
            wa_position_tab-mgr_emp = wa_hrp1001_p-sobid+0(8).
          ENDIF.

        ENDIF.

*get the orgunit and descriptin
        READ TABLE It_hrp1001_o INTO wa_hrp1001_o WITH KEY objid = wa_hrp1000_s-objid BINARY SEARCH.
        IF sy-subrc = 0.
          wa_position_tab-orgunit = wa_hrp1001_o-sobid+0(8).

*orgunit Description
          READ TABLE It_hrp1000_o INTO wa_hrp1000_o WITH KEY objid = wa_position_tab-orgunit BINARY SEARCH.
          IF sy-subrc = 0.
            wa_position_tab-ORGUNIT_txt = wa_hrp1000_o-stext.
          ENDIF.

        ENDIF.

*get the jobcode and descriptin
        READ TABLE It_hrp1001_c INTO wa_hrp1001_c WITH KEY objid = wa_hrp1000_s-objid BINARY SEARCH.
        IF sy-subrc = 0.
          wa_position_tab-jobcode = wa_hrp1001_c-sobid+0(8).

*orgunit Description
          READ TABLE It_hrp1000_c INTO wa_hrp1000_c WITH KEY objid = wa_position_tab-jobcode BINARY SEARCH.
          IF sy-subrc = 0.
            wa_position_tab-jobcode_txt = wa_hrp1000_c-stext.
          ENDIF.

        ENDIF.

*get the positon to BP relation ID
        READ TABLE It_hrp1001_bp INTO wa_hrp1001_bp WITH KEY objid = wa_hrp1000_s-objid BINARY SEARCH.
        IF sy-subrc = 0.
          wa_position_tab-posbpid = wa_hrp1001_bp-sobid.
        ENDIF.

*get the cost Center
        READ TABLE It_hrp1001_k INTO wa_hrp1001_k WITH KEY objid = wa_hrp1000_s-objid BINARY SEARCH.
        IF sy-subrc = 0.
          wa_position_tab-kostl = wa_hrp1001_k-sobid+0(10).
          CONDENSE wa_position_tab-kostl.
        ENDIF.

      ENDIF.

*Additional Positions to be set as Sales position will be provided in the tvarvc variable
      CLEAR ls_tvarvc_pos.
      READ TABLE lt_tvarvc_pos INTO ls_tvarvc_pos WITH KEY low = wa_hrp1000_s-objid.
      IF sy-subrc = 0.
        wa_position_tab-emp_type = 'S'.
      ENDIF.

*For Travel Category and Travel Allowed indicator
      READ TABLE it_hrp9013 INTO wa_hrp9013 WITH KEY objid = wa_hrp1000_s-objid.
      IF sy-subrc = 0.

        IF wa_hrp9013-travel = 'X'.
          wa_position_tab-emp_travel_all = 'Y'.
        ELSE.
          wa_position_tab-emp_travel_all = 'N'.
        ENDIF.

        wa_position_tab-emp_category = wa_hrp9013-category.

      ENDIF.

* For Position based latitude and Longitude
      READ TABLE it_hrp9011 INTO wa_hrp9011 WITH KEY objid = wa_hrp1000_s-objid.
      IF sy-subrc = 0.

        wa_position_tab-latitude = wa_hrp9011-latitude.
        wa_position_tab-longitude = wa_hrp9011-longitude.

      ENDIF.


      APPEND wa_position_tab TO it_position.

    ENDLOOP.


  ENDIF.

*ORGUNIT  1 Types HROBJID
*ORGUNIT_TXT  1 Types STEXT
*JOBCODE  1 Types HROBJID
*JOBCODE_TXT  1 Types STEXT
*POSBPID  1 Types SOBID
ENDFUNCTION.
