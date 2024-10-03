FUNCTION zhr_emp_details.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IN_BUKRS) TYPE  BUKRS OPTIONAL
*"     VALUE(IN_PERNR) TYPE  P3PR_RT_PERNR OPTIONAL
*"  TABLES
*"      IT_HR_EMP_DETAILS STRUCTURE  ZHR_EMP_DETAILS
*"----------------------------------------------------------------------

  TYPES:BEGIN OF ty_pa0001,
          pernr TYPE pa0001-pernr,
          sname TYPE pa0001-sname,
          begda TYPE pa0001-begda,
          bukrs TYPE pa0001-bukrs,
          orgeh TYPE pa0001-orgeh,
          plans TYPE pa0001-plans,
          stell TYPE pa0001-stell,
          btrtl TYPE pa0001-btrtl,
          persk TYPE pa0001-persk,
          werks TYPE pa0001-werks,
          kostl TYPE pa0001-kostl,
        END OF ty_pa0001.

  DATA:it_pa0001 TYPE TABLE OF ty_pa0001,
       wa_pa0001 TYPE ty_pa0001.

  TYPES :BEGIN OF ty_pa0002,
           pernr TYPE pa0006-pernr,
           gbdat TYPE pa0002-gbdat,
           natio TYPE pa0002-natio,
           gesch TYPE pa0002-gesch,
         END OF ty_pa0002.

  DATA:it_pa0002 TYPE TABLE OF ty_pa0002,
       wa_pa0002 TYPE ty_pa0002.

  TYPES: BEGIN OF ty_pa0041,
           pernr TYPE pa0041-pernr,
           dar01 TYPE pa0041-dar01,
           dat01 TYPE pa0041-dat01,
         END OF ty_pa0041.
  DATA: it_pa0041 TYPE TABLE OF ty_pa0041,
        wa_pa0041 TYPE  ty_pa0041.

  TYPES : BEGIN OF ty_pa0006,
            pernr TYPE pa0006-pernr,
            stras TYPE pa0006-stras,
            locat TYPE pa0006-locat,
            ort01 TYPE pa0006-ort01,
            pstlz TYPE pa0006-pstlz,
            state TYPE pa0006-state,
            land1 TYPE pa0006-land1,
          END OF ty_pa0006.

  DATA:it_pa0006 TYPE TABLE OF ty_pa0006,
       wa_pa0006 TYPE ty_pa0006.

  TYPES : BEGIN OF ty_pa0009,
            pernr TYPE pa0009-pernr,
            bankl TYPE pa0009-bankl,
            bankn TYPE pa0009-bankn,
          END OF ty_pa0009.

  DATA: it_pa0009 TYPE TABLE OF ty_pa0009,
        wa_pa0009 TYPE ty_pa0009.

  TYPES: BEGIN OF ty_pa0105,
           pernr      TYPE pa0105-pernr,
           subty      TYPE pa0105-subty,
           usrid_long TYPE pa0105-usrid_long,
           usrid      TYPE pa0105-usrid,
         END OF ty_pa0105.

  DATA:it_pa0105     TYPE TABLE OF ty_pa0105,
       it_pa0105_mgr TYPE TABLE OF ty_pa0105,
       wa_pa0105     TYPE ty_pa0105.

  TYPES:BEGIN OF ty_t528t,
          plans TYPE t528t-plans,
          plstx TYPE t528t-plstx,
        END OF ty_t528t.

  DATA: it_t528t TYPE TABLE OF ty_t528t,
        wa_t528t TYPE ty_t528t.
  TYPES:BEGIN OF ty_t503t,
          persk TYPE t503t-persk,
          ptext TYPE t503t-ptext,
        END OF ty_t503t.

  DATA: it_t503t TYPE TABLE OF ty_t503t,
        wa_t503t TYPE ty_t503t.

  TYPES: BEGIN OF ty_t001p,
           werks TYPE t001p-werks,
           btrtl TYPE t001p-btrtl,
           btext TYPE t001p-btext,
         END OF ty_t001p.

  DATA:it_t001p TYPE TABLE OF ty_t001p,
       wa_t001p TYPE ty_t001p.

  TYPES:BEGIN OF ty_t527x,
          orgeh TYPE t527x-orgeh,
          orgtx TYPE t527x-orgtx,
        END OF ty_t527x.

  DATA :it_t527x TYPE TABLE OF ty_t527x,
        wa_t527x TYPE ty_t527x.

  TYPES:BEGIN OF ty_t005s,
          land1 TYPE t005s-land1,
          bland TYPE t005s-bland,
          bezei TYPE t005u-bezei,
        END OF ty_t005s.

  DATA:it_t005s TYPE TABLE OF ty_t005s,
       wa_t005s TYPE ty_t005s.

  TYPES:BEGIN OF ty_hrp1001,
          objid     TYPE hrp1001-objid,
          sobid     TYPE hrp1001-sobid,
          sobid_new TYPE numc08,
        END OF ty_hrp1001.

  DATA: it_hrp1001    TYPE TABLE OF ty_hrp1001,
        it_hrp1001_tp TYPE TABLE OF ty_hrp1001,
        it_hrp1001_CS TYPE TABLE OF ty_hrp1001,
        it_hrp1001_CP TYPE TABLE OF ty_hrp1001,
        it_hrp1001_BP TYPE TABLE OF ty_hrp1001,
        wa_hrp1001    TYPE ty_hrp1001,
        wa_hrp1001_2  TYPE ty_hrp1001.

  TYPES:BEGIN OF ty_hrp1002,
          objid TYPE hrp1002-objid,
          tabnr TYPE hrp1002-tabnr,
        END OF ty_hrp1002.

  DATA: it_hrp1002 TYPE TABLE OF ty_hrp1002,
        wa_hrp1002 TYPE ty_hrp1002.

  TYPES:BEGIN OF ty_hrt1002,
          tabnr    TYPE hrt1002-tabnr,
          tabseqnr TYPE hrt1002-tabseqnr,
          tline    TYPE hrt1002-tline,
        END OF ty_hrt1002.

  DATA: it_hrt1002 TYPE TABLE OF ty_hrt1002,
        wa_hrt1002 TYPE ty_hrt1002.

  TYPES : BEGIN OF ty_hrp1000,
            short TYPE HRp1000-short,
            stext TYPE hrp1000-stext,
            objid TYPE hrp1000-objid,
          END OF ty_hrp1000.

  DATA :it_hrp10000_pl TYPE TABLE OF ty_hrp1000,
        wa_HRP10000_PL TYPE ty_hrp1000,
        plans_sob      TYPE plans.

  DATA :it_hrp10000_Mgs TYPE TABLE OF ty_hrp1000,
        wa_HRP10000_mgs TYPE ty_hrp1000.

  TYPES: BEGIN OF ty_pa0001_sob,
           pernr TYPE pa0001-pernr,
           plans TYPE pa0001-plans,
         END OF ty_pa0001_sob.

  DATA: it_pa0001_sob TYPE TABLE OF ty_pa0001_sob,
        wa_pa0001_sob TYPE ty_pa0001_sob.


*  DATA: It_hrp9010 TYPE TABLE OF hrp9010,
*        wa_hrp9010 TYPE hrp9010.

* To get all Active employees
  SELECT
    pa0001~pernr
    pa0001~sname
    pa0001~begda
    pa0001~bukrs
    pa0001~orgeh
    pa0001~plans
    pa0001~stell
    pa0001~btrtl
    pa0001~persk
    pa0001~werks
    Pa0001~kostl
    INTO TABLE it_pa0001 FROM pa0000 INNER JOIN pa0001 ON pa0000~pernr = pa0001~pernr
    WHERE pa0000~pernr IN in_pernr AND  pa0000~stat2 EQ '3'  "pa0001~bukrs EQ in_bukrs AND
    AND pa0001~endda GE sy-datum AND pa0000~endda GE sy-datum.

* if company code is supplied then delete all employees not belonging to the company code
  IF in_bukrs IS NOT INITIAL.
    DELETE it_pa0001 WHERE bukrs NE in_bukrs.
  ENDIF.

  IF it_pa0001 IS NOT INITIAL.

* get the Manger psotion based on the employee position from it0001 based on A002 relationship
    SELECT objid sobid INTO TABLE it_hrp1001 FROM hrp1001 FOR ALL ENTRIES IN it_pa0001
      WHERE plvar = '01' AND objid = it_pa0001-plans AND otype = 'S' AND subty = 'A002' AND endda GE sy-datum AND sclas = 'S'.
    LOOP AT it_hrp1001 INTO wa_hrp1001.
      CONDENSE wa_hrp1001-sobid NO-GAPS.
      wa_hrp1001-sobid_new = wa_hrp1001-sobid+0(8) .
      MODIFY it_hrp1001 FROM wa_hrp1001 TRANSPORTING sobid_new.
    ENDLOOP.

*get the COstcenter assigned to the employee position
    SELECT objid sobid INTO TABLE it_hrp1001_cs FROM hrp1001 FOR ALL ENTRIES IN it_pa0001
      WHERE plvar = '01' AND objid = it_pa0001-plans AND otype = 'S' AND subty = 'A011' AND begda LE sy-datum AND endda GE sy-datum AND sclas = 'K'.

*for getting the employee vendor code need to get P - CP and CP - BP
    SELECT objid sobid INTO TABLE it_hrp1001_cp FROM hrp1001 FOR ALL ENTRIES IN it_pa0001
      WHERE plvar = '01' AND objid = it_pa0001-pernr AND otype = 'P' AND subty = 'A209' AND begda LE sy-datum AND endda GE sy-datum AND sclas = 'CP'.
    LOOP AT it_hrp1001_cp INTO wa_hrp1001.
      CONDENSE wa_hrp1001-sobid NO-GAPS.
      wa_hrp1001-sobid_new = wa_hrp1001-sobid+0(8) .
      MODIFY it_hrp1001_cp FROM wa_hrp1001 TRANSPORTING sobid_new.
    ENDLOOP.

    SELECT objid sobid INTO TABLE it_hrp1001_bp FROM hrp1001 FOR ALL ENTRIES IN it_hrp1001_cp
      WHERE plvar = '01' AND objid = it_hrp1001_cp-sobid_new AND otype = 'CP' AND subty = 'B207' AND begda LE sy-datum AND endda GE sy-datum AND sclas = 'BP'.

*To get the Employee joining date based on the Date type 'S1' from Infotype 0041
    SELECT pernr dar01 dat01 INTO TABLE it_pa0041 FROM pa0041 FOR ALL ENTRIES IN it_pa0001 WHERE pernr = it_pa0001-pernr AND dar01 EQ 'S1'.

* To get the Date of birth, nationality and Sex from infotype 0002
    SELECT pernr
           gbdat
           natio
           gesch INTO TABLE it_pa0002 FROM pa0002 FOR ALL ENTRIES IN it_pa0001 WHERE pernr EQ it_pa0001-pernr.

* To get the Address
    SELECT pernr
           stras
           locat
           ort01
           pstlz
           state
           land1
           INTO TABLE it_pa0006 FROM pa0006 FOR ALL ENTRIES IN it_pa0001 WHERE pernr EQ it_pa0001-pernr.

* To get details like official and personal contact no, Official e-mail ID, Greytid,
    SELECT pernr
           subty
           usrid_long
           usrid
           INTO TABLE it_pa0105 FROM pa0105 FOR ALL ENTRIES IN it_pa0001
           WHERE pernr EQ it_pa0001-pernr AND pa0105~endda GE sy-datum AND subty IN ('0010','MOBI','CELL','MPHN','ECLN','0002').

*get the bank details
    SELECT pernr
           bankl
           bankn
           INTO TABLE it_pa0009 FROM pa0009 FOR ALL ENTRIES IN it_pa0001
           WHERE pernr EQ it_pa0001-pernr AND SUBty = '0' AND begda <= sy-datum AND endda >= sy-datum.

    SELECT plans
           plstx
           INTO TABLE it_t528t FROM t528t FOR ALL ENTRIES IN it_pa0001
           WHERE sprsl EQ sy-langu AND plans EQ it_pa0001-plans.
    SELECT persk
           ptext INTO TABLE it_t503t FROM t503t FOR ALL ENTRIES IN it_pa0001
           WHERE sprsl EQ sy-langu AND persk EQ it_pa0001-persk.

    SELECT
         werks
         btrtl
         btext
         INTO TABLE it_t001p FROM t001p FOR ALL ENTRIES IN it_pa0001
         WHERE werks EQ it_pa0001-werks AND btrtl EQ it_pa0001-btrtl.

    SELECT
      orgeh
      orgtx
      INTO TABLE it_t527x FROM t527x FOR ALL ENTRIES IN it_pa0001
      WHERE orgeh EQ it_pa0001-orgeh AND sprsl EQ sy-langu.

    IF it_pa0006 IS NOT INITIAL.
      SELECT
        land1
        bland
        bezei
        INTO TABLE it_t005s FROM t005u FOR ALL ENTRIES IN it_pa0006
        WHERE land1 EQ it_pa0006-land1 AND bland EQ it_pa0006-state AND spras EQ sy-langu.
    ENDIF.

*Get Position description details based on todays date
    SELECT short stext objid INTO TABLE it_hrp10000_pl FROM hrp1000 FOR ALL ENTRIES IN it_pa0001
                                                                       WHERE plvar = '01'
                                                                       AND otype = 'S' AND objid = it_pa0001-plans
                                                                       AND begda <= sy-datum
                                                                       AND endda >= sy-datum.

*get all the description of Positions in SAP from HRP1000
    SELECT short stext objid INTO TABLE it_hrp10000_mgs FROM hrp1000 WHERE plvar = '01'
                                                                       AND otype = 'S'
                                                                       AND begda <= sy-datum
                                                                       AND endda >= sy-datum.

*    SELECT * FROM hrp9010 INTO TABLE It_hrp9010 WHERE plvar = '01' AND begda <= sy-datum AND endda >= sy-datum.

    IF it_hrp1001[] IS NOT INITIAL.
*for managers holding multiple positions. getting the employee details based on the position
      SELECT objid sobid FROM hrp1001 INTO TABLE it_hrp1001_tp FOR ALL ENTRIES IN it_hrp1001 WHERE otype = 'S'  AND objid = it_hrp1001-sobid_new
              AND plvar = '01' AND rsign = 'A' AND relat = '008' AND begda LE sy-datum AND endda GE sy-datum AND sclas = 'P'.

      SELECT pernr plans INTO TABLE it_pa0001_sob FROM pa0001 FOR ALL ENTRIES IN it_hrp1001 WHERE plans = it_hrp1001-sobid_new
                                                                                              AND begda LE sy-datum AND endda GE sy-datum.

*for managers having multiple positions
      LOOP AT it_hrp1001_tp INTO wa_hrp1001.
        CLEAR wa_pa0001_sob.
        wa_pa0001_sob-pernr = wa_hrp1001-sobid+0(8).
        wa_pa0001_sob-plans = wa_hrp1001-objid.
        APPEND wa_pa0001_sob TO it_pa0001_sob.
      ENDLOOP.
      SORT it_pa0001_sob BY pernr plans.
      DELETE ADJACENT DUPLICATES FROM it_pa0001_sob COMPARING ALL FIELDS.

      IF it_pa0001_sob[] IS NOT INITIAL.
* To get vacant indicator for the manager pernr
        SELECT pernr subty usrid_long usrid
             INTO TABLE it_pa0105_mgr FROM pa0105 FOR ALL ENTRIES IN it_pa0001_sob
             WHERE pernr EQ it_pa0001_sob-pernr AND pa0105~endda GE sy-datum AND subty = 'MPHN'.
      ENDIF.
    ENDIF.



  ENDIF.
  SORT it_pa0001 BY pernr ASCENDING.
  SORT it_pa0002 BY pernr ASCENDING.
  SORT it_pa0006 BY pernr ASCENDING.
  SORT it_pa0105 BY pernr ASCENDING.
  SORT it_hrp1002 BY objid tabnr.
  SORT it_hrp10000_pl BY objid.
  SORT it_hrt1002 BY tabnr tabseqnr.
  SORT it_hrp10000_mgs BY objid.
  TYPES:BEGIN OF temp_tline1,
          emp_tline(5000) TYPE c,
        END OF temp_tline1.
  DATA:temp_tline(5000) TYPE c,
       it_temp_tline    TYPE TABLE OF temp_tline1.

  LOOP AT it_pa0001 INTO wa_pa0001.
    CLEAR it_hr_emp_details.
    it_hr_emp_details-pernr = wa_pa0001-pernr. "EMPLOYEE ID
    it_hr_emp_details-sname = wa_pa0001-sname. "FIRST NAME AND LAST NAME
    it_hr_emp_details-bukrs = wa_pa0001-bukrs. "Company Code
    it_hr_emp_details-emp_kostl = wa_pa0001-kostl. "Company Code
    it_hr_emp_details-stell = wa_pa0001-stell. "Job Code

    READ TABLE it_hrp1001_cs INTO wa_hrp1001 WITH KEY objid = wa_pa0001-plans.
    IF sy-subrc = 0.
      it_hr_emp_details-pos_kostl = wa_hrp1001-sobid+0(10).
    ENDIF.

    READ TABLE it_pa0041 INTO wa_pa0041 WITH KEY pernr = wa_pa0001-pernr.
    it_hr_emp_details-dat01 = wa_pa0041-dat01.  "Date of Joining

    READ TABLE it_t527x INTO wa_t527x WITH KEY orgeh = wa_pa0001-orgeh.
    it_hr_emp_details-orgeh = wa_pa0001-orgeh. "WORK LOCATION
    it_hr_emp_details-orgtx = wa_t527x-orgtx.

    READ TABLE it_t528t INTO wa_t528t WITH KEY plans = wa_pa0001-plans.
    it_hr_emp_details-plans = wa_pa0001-plans. "DESIGNATION / ROLE
    it_hr_emp_details-plstx = wa_t528t-plstx.

    READ TABLE it_pa0002 INTO wa_pa0002 WITH KEY pernr = wa_pa0001-pernr.
    it_hr_emp_details-gbdat = wa_pa0002-gbdat. "DOB
    it_hr_emp_details-natio = wa_pa0002-natio.
    it_hr_emp_details-gesch = wa_pa0002-gesch.

    READ TABLE it_t001p INTO wa_t001p WITH KEY werks = wa_pa0001-werks btrtl = wa_pa0001-btrtl.
    it_hr_emp_details-btrtl = wa_t001p-btrtl.  "Personal Sub Area
    it_hr_emp_details-btext = wa_t001p-btext.


    READ TABLE it_t503t INTO wa_t503t WITH KEY persk = wa_pa0001-persk.
    it_hr_emp_details-persk = wa_t503t-persk.
    it_hr_emp_details-ptext = wa_t503t-ptext.

*Official e-mail id
    READ TABLE it_pa0105 INTO wa_pa0105 WITH KEY pernr = wa_pa0001-pernr subty = '0010'.
    it_hr_emp_details-usrid_long = wa_pa0105-usrid_long.

*Official Mobile no
    CLEAR wa_pa0105.
    READ TABLE it_pa0105 INTO wa_pa0105 WITH KEY pernr = wa_pa0001-pernr subty = 'MOBI'.
    IF sy-subrc = 0.
      it_hr_emp_details-usrid = wa_pa0105-usrid.
    ELSE.
* If official mobile number is not there then put the personal number as official mobile no
      READ TABLE it_pa0105 INTO wa_pa0105 WITH KEY pernr = wa_pa0001-pernr subty = 'CELL'.
      IF sy-subrc = 0.
        it_hr_emp_details-usrid = wa_pa0105-usrid.
      ENDIF.
    ENDIF.

*Check for Dummy Emp Code
    READ TABLE it_pa0105 INTO wa_pa0105 WITH KEY pernr = wa_pa0001-pernr subty = 'MPHN'.
    IF sy-subrc = 0.
      it_hr_emp_details-dummy_emp = abap_true.
    ENDIF.

*Check and update for Emp Greytip Code
    READ TABLE it_pa0105 INTO wa_pa0105 WITH KEY pernr = wa_pa0001-pernr subty = '0002'.
    IF sy-subrc = 0.
      it_hr_emp_details-greytid = wa_pa0105-usrid.
    ENDIF.

*get bank account details
    READ TABLE it_pa0009 INTO wa_pa0009 WITH KEY pernr = wa_pa0001-pernr.
    IF sy-subrc = 0.
      it_hr_emp_details-bankl = wa_pa0009-bankl.
      it_hr_emp_details-bankn = wa_pa0009-bankn.
    ENDIF.

    READ TABLE it_hrp1001_cp INTO wa_hrp1001 with key objid = wa_pa0001-pernr.
    IF sy-subrc = 0.
      READ TABLE it_hrp1001_bp INTO wa_hrp1001_2 with KEY objid = wa_hrp1001-sobid_new.
      IF sy-subrc = 0.
        it_hr_emp_details-lifnr = wa_hrp1001_2-sobid.
        condense it_hr_emp_details-lifnr.
      ENDIF.
    ENDIF.

    READ TABLE it_pa0006 INTO wa_pa0006  WITH KEY pernr = wa_pa0001-pernr.
    it_hr_emp_details-stras = wa_pa0006-stras.
    it_hr_emp_details-locat = wa_pa0006-locat.
    it_hr_emp_details-ort01 = wa_pa0006-ort01.
    it_hr_emp_details-pstlz = wa_pa0006-pstlz.

    READ TABLE it_t005s INTO wa_t005s WITH KEY bland = wa_pa0006-state land1 = wa_pa0006-land1.
    it_hr_emp_details-bland = wa_t005s-bland.
    it_hr_emp_details-bezei = wa_t005s-bezei.
    it_hr_emp_details-land1 = wa_pa0006-land1.

    READ TABLE it_hrp10000_pl INTO wa_hrp10000_pl WITH KEY objid = wa_pa0001-plans.
    it_hr_emp_details-short_pl = wa_hrp10000_pl-short.
    it_hr_emp_details-stext_pl = wa_hrp10000_pl-stext.


    DATA: ls_hrp1001 TYPE hrp1001.
    DATA: lv_objid_mgs TYPE hrp1001-objid,
          lv_objid_mgp TYPE hrp1001-objid,
          lv_pa0001    TYPE pa0001.

* for getting the original manager and manager pernr based on the A002 relationship
    IF wa_pa0001-plans IS NOT INITIAL AND wa_pa0001-plans <> '99999999'.

      CLEAR wa_hrp1001.
      READ TABLE it_hrp1001 INTO wa_hrp1001 WITH KEY objid = wa_pa0001-plans.
      IF sy-subrc = 0.
        it_hr_emp_details-origmgrpos = wa_hrp1001-sobid.

        CLEAR wa_pa0001_sob.
        READ TABLE it_pa0001_sob INTO wa_pa0001_sob WITH KEY plans = it_hr_emp_details-origmgrpos.
        IF sy-subrc = 0." if there is some employee in that position
          it_hr_emp_details-origmgremp = wa_pa0001_sob-pernr.
        ENDIF.

        READ TABLE it_hrp10000_mgs INTO wa_hrp10000_mgs WITH KEY objid = it_hr_emp_details-origmgrpos.
        IF sy-subrc = 0.
          it_hr_emp_details-origmgposdes = wa_hrp10000_mgs-stext.
        ENDIF.


      ENDIF.
    ENDIF.

*for getting the actual manaager relationship by ignoring the vacant and dummy codes
    IF wa_pa0001-plans IS NOT INITIAL AND wa_pa0001-plans <> '99999999'.
      CLEAR wa_hrp1001.
      READ TABLE it_hrp1001 INTO wa_hrp1001 WITH KEY objid = wa_pa0001-plans.
      IF sy-subrc = 0.
        it_hr_emp_details-sobid = wa_hrp1001-sobid.

        CLEAR wa_pa0001_sob.
        READ TABLE it_pa0001_sob INTO wa_pa0001_sob WITH KEY plans = it_hr_emp_details-sobid.
        IF sy-subrc = 0." if there is some employee in that position
          it_hr_emp_details-pernr_rep = wa_pa0001_sob-pernr.
*check if the manager position is a dummy or vacant position
          READ TABLE it_pa0105_mgr INTO wa_pa0105 WITH KEY pernr = it_hr_emp_details-pernr_rep.
          IF sy-subrc = 0.  "If position with dummy ID

            lv_objid_mgs = it_hr_emp_details-sobid.
            DO .
* Get next level Manager
              CLEAR ls_hrp1001.
              SELECT SINGLE * FROM hrp1001 INTO ls_hrp1001 WHERE otype = 'S' AND objid = lv_objid_mgs
              AND plvar = '01' AND rsign = 'A' AND relat = '002' AND begda LE sy-datum AND endda GE sy-datum AND sclas = 'S'.
              IF sy-subrc = 0.
*Get the pernr in the next level manager
                CLEAR lv_objid_mgp.
                lv_objid_mgp = ls_hrp1001-sobid.
                CLEAR ls_hrp1001.
                SELECT SINGLE * FROM hrp1001 INTO ls_hrp1001 WHERE otype = 'S' AND objid = lv_objid_mgp
                AND plvar = '01' AND rsign = 'A' AND relat = '008' AND begda LE sy-datum AND endda GE sy-datum AND sclas = 'P'.
                IF sy-subrc = 0.
*check if the pernr is also a vacant position pernr
                  CLEAR wa_pa0105.
                  SELECT SINGLE pernr subty usrid_long usrid FROM pa0105
                    INTO wa_pa0105 WHERE pernr = ls_hrp1001-sobid+0(8)
                      AND subty = 'MPHN' AND begda LE sy-datum AND endda GE sy-datum.
                  IF sy-subrc = 0.
                    lv_objid_mgs = lv_objid_mgp.
                    CONTINUE.
                  ELSE.
*                   correct manager position and employee number
                    it_hr_emp_details-sobid = lv_objid_mgp.
                    it_hr_emp_details-pernr_rep = ls_hrp1001-sobid+0(8).
                    EXIT.
                  ENDIF.
                ELSE. "No employee in position then go to next level
                  lv_objid_mgs = lv_objid_mgp.
                  CONTINUE.
                ENDIF.
              ELSE. "no next level manager
                EXIT.
              ENDIF.
            ENDDO.
          ENDIF.
        ELSE.   "If no Pernr in the initial Manager position


          lv_objid_mgs = it_hr_emp_details-sobid.

          DO .
* Get next level Manager

            CLEAR ls_hrp1001.
            SELECT SINGLE * FROM hrp1001 INTO ls_hrp1001 WHERE otype = 'S' AND objid = lv_objid_mgs
            AND plvar = '01' AND rsign = 'A' AND relat = '002' AND begda LE sy-datum AND endda GE sy-datum AND sclas = 'S'.
            IF sy-subrc = 0.
*Get the pernr in the next level manager
              CLEAR lv_objid_mgp.
              lv_objid_mgp = ls_hrp1001-sobid.
              CLEAR ls_hrp1001.
              SELECT SINGLE * FROM hrp1001 INTO ls_hrp1001 WHERE otype = 'S' AND objid = lv_objid_mgp
              AND plvar = '01' AND rsign = 'A' AND relat = '008' AND begda LE sy-datum AND endda GE sy-datum AND sclas = 'P'.
              IF sy-subrc = 0.
*check if the pernr is also a vacant position pernr
                CLEAR wa_pa0105.
                SELECT SINGLE pernr subty usrid_long usrid FROM pa0105
                  INTO wa_pa0105 WHERE pernr = ls_hrp1001-sobid+0(8)
                    AND subty = 'MPHN' AND begda LE sy-datum AND endda GE sy-datum.
                IF sy-subrc = 0.
                  lv_objid_mgs = lv_objid_mgp.
                  CONTINUE.
                ELSE.
*                   correct manager position and employee number
                  it_hr_emp_details-sobid = lv_objid_mgp.
                  it_hr_emp_details-pernr_rep = ls_hrp1001-sobid+0(8).
                  EXIT.
                ENDIF.
              ELSE. "No employee in position then go to next level
                lv_objid_mgs = lv_objid_mgp.
                CONTINUE.
              ENDIF.
            ELSE. "no next level manager
              EXIT.
            ENDIF.
          ENDDO.
        ENDIF.
      ENDIF.
    ENDIF.


    CLEAR wa_pa0001_sob.

    APPEND it_hr_emp_details.

    CLEAR :temp_tline,wa_hrp10000_pl,wa_hrp1001,wa_hrp1002,wa_hrt1002,   ",temp_tline3
    wa_pa0001,wa_pa0002,wa_pa0006,wa_pa0105,wa_t001p,wa_t005s,wa_t503t,wa_t527x,wa_t528t,wa_pa0001_sob, wa_pa0041,wa_pa0009.
  ENDLOOP.

ENDFUNCTION.
