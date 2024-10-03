FUNCTION zhr_emp_type_cat.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IN_BUKRS) TYPE  BUKRS OPTIONAL
*"     VALUE(IN_PERNR) TYPE  P3PR_RT_PERNR OPTIONAL
*"  TABLES
*"      IT_OUTPUT STRUCTURE  ZHR_PERNR_TYPE
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
        END OF ty_pa0001.

  DATA:it_pa0001 TYPE TABLE OF ty_pa0001,
       wa_pa0001 TYPE ty_pa0001.

  DATA: It_hrp9010 TYPE TABLE OF hrp9010,
        wa_hrp9010 TYPE hrp9010.


  TYPES: BEGIN OF ty_pa0105,
           pernr TYPE pa0105-pernr,
           subty TYPE pa0105-subty,
           usrid TYPE pa0105-usrid,
         END OF ty_pa0105.

  DATA:it_pa0105     TYPE TABLE OF ty_pa0105,
       wa_pa0105     TYPE ty_pa0105.


  DATA: lt_hrp1001_CT TYPE TABLE of hrp1001,
        wa_hrp1001_ct TYPE hrp1001.

  DATA: lt_hrp1000_T TYPE TABLE of hrp1000,
        wa_hrp1000_t TYPE hrp1000.

  DATA: lt_tvarvc_job TYPE TABLE OF tvarvc,
        ls_tvarvc_job TYPE tvarvc.

  DATA: wa_output TYPE ZHR_PERNR_TYPE.

  CONSTANTS: lv_tvarvc_n TYPE tvarvc-name VALUE 'ZHR_TS_JOBID',
             lv_tvarvc_S TYPE tvarvc-type VALUE 'S'.

  SELECT * FROM tvarvc INTO TABLE lt_tvarvc_job WHERE name = lv_tvarvc_n AND type = lv_tvarvc_S.
  IF sy-subrc = 0.
*      do nothing
  ENDIF.

*Select all active employees based on employee status = 3
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
      WHERE pa0001~pernr IN in_pernr
        AND pa0000~stat2 EQ '3' "pa0001~bukrs EQ in_bukrs AND
      AND pa0001~endda GE sy-datum AND pa0000~endda GE sy-datum.

  IF sy-subrc = 0.
* Selection entires for employees with travel allowed
    SELECT pernr subty usrid
       INTO TABLE it_pa0105 FROM pa0105 FOR ALL ENTRIES IN it_pa0001
       WHERE pernr EQ it_pa0001-pernr AND subty in ('ECLN','0002') AND begda LE sy-datum AND endda GE sy-datum.

  ENDIF.

  IF in_bukrs IS NOT INITIAL.
    DELETE it_pa0001 WHERE bukrs NE in_bukrs.
  ENDIF.

* get the task type from hrp1000 for Travel categorization
  SELECT * from hrp1000 INTO TABLE lt_hrp1000_T WHERE plvar = '01'
                                                  and otype = 'T'
                                                  and begda <= sy-datum
                                                  and endda >= sy-datum
                                                  and langu = sy-langu
                                                  and uname <> 'SAP'.

  SELECT * from hrp1001 INTO TABLE lt_hrp1001_CT WHERE otype = 'C'
*                                                   and objiD BETWEEN '60000000' AND '69999999'
                                                   and plvar = '01'
                                                   and rsign = 'B'
                                                   and relat = '007'
                                                   and sclas = 'T'.

* Sales type table for orgunit
  SELECT * FROM hrp9010 INTO TABLE It_hrp9010 WHERE plvar = '01' AND begda <= sy-datum AND endda >= sy-datum.

  LOOP AT it_pa0001 INTO wa_pa0001.

    CLEAR wa_output.

    wa_output-pernr = wa_pa0001-pernr.
    wa_output-emp_type = 'N'.  "default to Non sales for everybody
    wa_output-emp_travel_all = 'N'.  "Default to No travel for all
    wa_OUTPUT-emp_category = 'A'.

*Code to determine Sales/Non Sales employee type.
    CLEAR wa_hrp9010.
    READ TABLE It_hrp9010 INTO wa_hrp9010 WITH KEY otype = 'O' objid = wa_pa0001-orgeh.
    IF sy-subrc = 0.
      wa_output-emp_type = 'S'.  "Sales employee
      wa_output-emp_travel_all = 'Y'.
    ENDIF.

    CLEAR ls_tvarvc_job.
    READ TABLE lt_tvarvc_job INTO ls_tvarvc_job WITH KEY low = wa_pa0001-stell.
    IF sy-subrc = 0.
      wa_output-emp_type = 'S'.  "Sales employee
      wa_output-emp_travel_all = 'Y'.
    ENDIF.

    READ TABLE it_pa0105 INTO wa_pa0105 WITH key pernr = wa_pa0001-pernr subty = 'ECLN'.
    IF sy-subrc = 0.
      wa_output-emp_travel_all = 'Y'.
    ENDIF.

    READ TABLE it_pa0105 INTO wa_pa0105 WITH key pernr = wa_pa0001-pernr subty = '0002'.
    IF sy-subrc = 0.
      wa_output-EMP_GREYID = wa_pa0105-usrid.
    ENDIF.

* Code to determine the travel classification type based on Job type and task type classification
    READ TABLE lt_hrp1001_CT INTO wa_hrp1001_CT with key objid = wa_pa0001-stell.
    IF sy-subrc = 0.
      READ TABLE lt_hrp1000_T into wa_hrp1000_t with key objid = wa_hrp1001_CT-sobid.
      IF sy-subrc = 0.
        wa_OUTPUT-emp_category = wa_hrp1000_t-stext+0(1).
      ENDIF.
    ENDIF.

    append wa_output to it_output.
  ENDLOOP.

ENDFUNCTION.
