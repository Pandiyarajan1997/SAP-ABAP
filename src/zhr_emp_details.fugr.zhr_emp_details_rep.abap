FUNCTION zhr_emp_details_rep.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IN_BUKRS) TYPE  BUKRS OPTIONAL
*"     REFERENCE(IN_PERNR) TYPE  /SHCM/T_PERNR_BP OPTIONAL
*"  TABLES
*"      IT_HR_EMP_DETAILS STRUCTURE  ZHR_EMP_DETAILS_REP
*"----------------------------------------------------------------------

  TYPES:BEGIN OF ty_pa0001,
          pernr TYPE pa0001-pernr,
          ename TYPE pa0001-ename,
          begda TYPE pa0001-begda,
          bukrs TYPE pa0001-bukrs,
          orgeh TYPE pa0001-orgeh,
          plans TYPE pa0001-plans,
          stell TYPE pa0001-stell,
          btrtl TYPE pa0001-btrtl,
          persk TYPE pa0001-persk,
          werks TYPE pa0001-werks,
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

  DATA: it_hrp1001 TYPE TABLE OF ty_hrp1001,
        wa_hrp1001 TYPE ty_hrp1001.

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

  TYPES: BEGIN OF ty_pa0001_sob,
           pernr TYPE pa0001-pernr,
           plans TYPE pa0001-plans,
           ename TYPE pa0001-ename,
         END OF ty_pa0001_sob.

  DATA: it_pa0001_sob TYPE TABLE OF ty_pa0001_sob,
        wa_pa0001_sob TYPE ty_pa0001_sob.

  DATA: ls_hr_emp_details TYPE zhr_emp_details_rep.

  IF in_bukrs IS NOT INITIAL.
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
        INTO TABLE it_pa0001 FROM pa0000 INNER JOIN pa0001 ON pa0000~pernr = pa0001~pernr
        WHERE pa0001~bukrs EQ in_bukrs AND pa0000~stat2 EQ '3'
        AND pa0001~endda GE sy-datum AND pa0000~endda GE sy-datum.

  ELSEIF in_pernr[] IS NOT INITIAL.
    SELECT
       pa0001~pernr
       pa0001~ename
       pa0001~begda
       pa0001~bukrs
       pa0001~orgeh
       pa0001~plans
       pa0001~stell
       pa0001~btrtl
       pa0001~persk
       pa0001~werks
       INTO TABLE it_pa0001
      FROM pa0000
      INNER JOIN pa0001 ON pa0000~pernr = pa0001~pernr
       FOR ALL ENTRIES IN in_pernr
       WHERE pa0001~pernr = in_pernr-pernr
         AND pa0001~bukrs EQ in_bukrs
         AND pa0000~stat2 EQ '3'
         AND pa0001~endda GE sy-datum AND pa0000~begda LE sy-datum.
  ENDIF.


  IF it_pa0001 IS NOT INITIAL.

*Getting Manager relationship based an A002
    SELECT objid sobid
      INTO TABLE it_hrp1001 FROM hrp1001
      FOR ALL ENTRIES IN it_pa0001
      WHERE objid = it_pa0001-plans
      AND otype = 'S' AND subty = 'A002' AND Begda <= sy-datum AND endda GE sy-datum.
    LOOP AT it_hrp1001 INTO wa_hrp1001.
      CONDENSE wa_hrp1001-sobid NO-GAPS.
      wa_hrp1001-sobid_new = wa_hrp1001-sobid+0(8) .
      MODIFY it_hrp1001 FROM wa_hrp1001 TRANSPORTING sobid_new.
    ENDLOOP.
    IF it_hrp1001 IS NOT INITIAL.
*      SELECT objid tabnr INTO TABLE it_hrp1002 FROM hrp1002 FOR ALL ENTRIES IN it_pa0001 WHERE objid = it_pa0001-stell AND otype = 'C'.
      SELECT pernr plans INTO TABLE it_pa0001_sob FROM pa0001 FOR ALL ENTRIES IN it_hrp1001 WHERE plans = it_hrp1001-sobid_new AND endda GE sy-datum.
    ENDIF.

    IF it_hrp1002 IS NOT INITIAL.
      SELECT tabnr tabseqnr tline INTO TABLE it_hrt1002 FROM hrt1002 FOR ALL ENTRIES IN it_hrp1002 WHERE tabnr = it_hrp1002-tabnr.
    ENDIF.

    SELECT pernr
           dar01
           dat01
      INTO TABLE it_pa0041
      FROM pa0041
      FOR ALL ENTRIES IN it_pa0001
     WHERE pernr = it_pa0001-pernr AND dar01 EQ 'S1'.

    SELECT pernr
           gbdat
           natio
           gesch INTO TABLE it_pa0002 FROM pa0002 FOR ALL ENTRIES IN it_pa0001 WHERE pernr EQ it_pa0001-pernr.

    SELECT pernr
           stras
           locat
           ort01
           pstlz
           state
           land1
           INTO TABLE it_pa0006 FROM pa0006 FOR ALL ENTRIES IN it_pa0001 WHERE pernr EQ it_pa0001-pernr.

    SELECT pernr
           subty
           usrid_long
           usrid
           INTO TABLE it_pa0105 FROM pa0105 FOR ALL ENTRIES IN it_pa0001
           WHERE pernr EQ it_pa0001-pernr AND pa0105~endda GE sy-datum AND subty IN ('0010','MOBI','MPHN').

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

    SELECT short stext objid INTO TABLE it_hrp10000_pl FROM hrp1000 FOR ALL ENTRIES IN it_pa0001 WHERE objid = it_pa0001-plans.

* Manager pernr and Position
    SELECT pernr
           plans
           ename
      INTO TABLE it_pa0001_sob
      FROM pa0001
      FOR ALL ENTRIES IN it_hrp1001 WHERE plans = it_hrp1001-sobid_new.
    IF sy-subrc = 0.
      SELECT pernr subty usrid_long usrid
           INTO TABLE it_pa0105_mgr FROM pa0105 FOR ALL ENTRIES IN it_pa0001_sob
           WHERE pernr EQ it_pa0001_sob-pernr AND pa0105~endda GE sy-datum AND subty = 'MPHN'.
    ENDIF.

  ENDIF.


  SORT it_pa0001 BY pernr ASCENDING.
  SORT it_pa0002 BY pernr ASCENDING.
  SORT it_pa0006 BY pernr ASCENDING.
  SORT it_pa0105 BY pernr ASCENDING.
  SORT it_hrp1002 BY objid tabnr.
  SORT it_hrp10000_pl BY objid.
  SORT it_hrt1002 BY tabnr tabseqnr.

  TYPES:BEGIN OF temp_tline1,
          emp_tline(5000) TYPE c,
        END OF temp_tline1.
  DATA:temp_tline(5000) TYPE c,
       it_temp_tline    TYPE TABLE OF temp_tline1.

  LOOP AT it_pa0001 INTO wa_pa0001.
    ls_hr_emp_details-pernr = wa_pa0001-pernr. "EMPLOYEE ID
    ls_hr_emp_details-sname = wa_pa0001-ename. "FIRST NAME AND LAST NAME
    ls_hr_emp_details-bukrs = wa_pa0001-bukrs.

    READ TABLE it_pa0041 INTO wa_pa0041 WITH KEY pernr = wa_pa0001-pernr.
    ls_hr_emp_details-dat01 = wa_pa0041-dat01.

    READ TABLE it_t527x INTO wa_t527x WITH KEY orgeh = wa_pa0001-orgeh.
    ls_hr_emp_details-orgeh = wa_pa0001-orgeh. "WORK LOCATION
    ls_hr_emp_details-orgtx = wa_t527x-orgtx.

    READ TABLE it_t528t INTO wa_t528t WITH KEY plans = wa_pa0001-plans.
    ls_hr_emp_details-plans = wa_pa0001-plans. "DESIGNATION / ROLE
    ls_hr_emp_details-plstx = wa_t528t-plstx.

    READ TABLE it_pa0002 INTO wa_pa0002 WITH KEY pernr = wa_pa0001-pernr.
    ls_hr_emp_details-gbdat = wa_pa0002-gbdat. "DOB
    ls_hr_emp_details-stell = wa_pa0001-stell.


    READ TABLE it_t001p INTO wa_t001p WITH KEY werks = wa_pa0001-werks btrtl = wa_pa0001-btrtl.
    ls_hr_emp_details-btrtl = wa_t001p-btrtl.
    ls_hr_emp_details-btext = wa_t001p-btext.
    ls_hr_emp_details-natio = wa_pa0002-natio.
    ls_hr_emp_details-gesch = wa_pa0002-gesch.

    READ TABLE it_t503t INTO wa_t503t WITH KEY persk = wa_pa0001-persk.
    ls_hr_emp_details-persk = wa_t503t-persk.
    ls_hr_emp_details-ptext = wa_t503t-ptext.

*Email id
    CLEAR wa_pa0105.
    READ TABLE it_pa0105 INTO wa_pa0105 WITH KEY pernr = wa_pa0001-pernr subty = '0010'.
    ls_hr_emp_details-usrid_long = wa_pa0105-usrid_long.

*Mobile Number
    CLEAR wa_pa0105.
    READ TABLE it_pa0105 INTO wa_pa0105 WITH KEY pernr = wa_pa0001-pernr subty = 'MOBI'.
    ls_hr_emp_details-vacant_ind = wa_pa0105-usrid.

*Vacation Indicatior
    CLEAR wa_pa0105.
    READ TABLE it_pa0105 INTO wa_pa0105 WITH KEY pernr = wa_pa0001-pernr subty = 'MPHN'.
    ls_hr_emp_details-usrid = wa_pa0105-usrid.

*Address
    READ TABLE it_pa0006 INTO wa_pa0006  WITH KEY pernr = wa_pa0001-pernr.
    ls_hr_emp_details-stras = wa_pa0006-stras.
    ls_hr_emp_details-locat = wa_pa0006-locat.
    ls_hr_emp_details-ort01 = wa_pa0006-ort01.
    ls_hr_emp_details-pstlz = wa_pa0006-pstlz.

    READ TABLE it_t005s INTO wa_t005s WITH KEY bland = wa_pa0006-state land1 = wa_pa0006-land1.
    ls_hr_emp_details-bland = wa_t005s-bland.
    ls_hr_emp_details-bezei = wa_t005s-bezei.
    ls_hr_emp_details-land1 = wa_pa0006-land1.

    READ TABLE it_hrp10000_pl INTO wa_hrp10000_pl WITH KEY objid = wa_pa0001-plans.
    ls_hr_emp_details-short_pl = wa_hrp10000_pl-short.
    ls_hr_emp_details-stext_pl = wa_hrp10000_pl-stext.

    READ TABLE it_hrp1001 INTO wa_hrp1001 WITH KEY objid = wa_pa0001-plans.
    ls_hr_emp_details-sobid = wa_hrp1001-sobid.

    READ TABLE it_pa0001_sob INTO wa_pa0001_sob WITH KEY plans = ls_hr_emp_details-sobid.
    IF sy-subrc = 0.
      ls_hr_emp_details-pernr_rep = wa_pa0001_sob-pernr.

      DATA: lv_objid_mgs TYPE hrp1001-objid,
            lv_objid_mgp TYPE hrp1001-objid,
            lv_pa0001    TYPE pa0001.

      lv_objid_mgs = ls_hr_emp_details-sobid.

*      DO.
*        CLEAR wa_pa0105.
*        READ TABLE it_pa0105_mgr WITH KEY pernr = wa_pa0001_sob-pernr TRANSPORTING NO FIELDS.
*        IF sy-subrc = 0.
*          CLEAR wa_hrp1001.
*          SELECT SINGLE *
*            FROM hrp1001
*            INTO wa_hrp1001
*            WHERE otype = 'S'
*              AND objid = lv_objid_mgs.
*              AND plvar = '01'
*              AND rsign = 'A'
*              AND relat = '002'
*              AND sclas = 'S'.
*          IF sy-subrc = 0.
*          lv_objid_mgp = wa_hrp1001-sobid.
*
*          CLEAR wa_hrp1001.
*          SELECT SINGLE *
*            FROM hrp1001
*            INTO wa_hrp1001
*            WHERE otype = 'S'
*              AND objid = lv_objid_mgp.
*              AND plvar = '01'
*              AND rsign = 'A'
*              AND relat = '008'
*              AND sclas = 'P'.
*           IF sy-subrc = 0.
*
*             SELECT SINGLE *
*               from pa0001
*               into lv_pa0001
*              WHERE pernr = wa_hrp1001-sobid+0(8)
*                and begda <= sy-datum
*                and endda >= sy-datum.
*             IF sy-subrc = 0.
*               SELECT
*             else.
*               exit.
*             ENDIF.
*
*           else.
*             exit.
*           ENDIF.
*          else.
*            exit.
*          ENDIF.
*        ELSE.
*          exit.
*        ENDIF.
*      ENDDO.


    ENDIF.






    APPEND ls_hr_emp_details TO it_hr_emp_details.
    CLEAR ls_hr_emp_details.

    CLEAR :temp_tline,wa_hrp10000_pl,wa_hrp1001,wa_hrp1002,wa_hrt1002,
    wa_pa0001,wa_pa0002,wa_pa0006,wa_pa0105,wa_t001p,wa_t005s,wa_t503t,wa_t527x,wa_t528t,wa_pa0001_sob.

    CLEAR wa_pa0001_sob.
  ENDLOOP.


*temp_tline3


ENDFUNCTION.
