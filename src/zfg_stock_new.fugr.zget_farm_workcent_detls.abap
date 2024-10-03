FUNCTION zget_farm_workcent_detls.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(COMP_CODE) TYPE  BUKRS
*"  EXPORTING
*"     VALUE(FARM_DETLS) TYPE  ZZFARM_WORK_CENTRE
*"----------------------------------------------------------------------


************** T001K and CRHD TABLES TYPE structure *******************
  TYPES : BEGIN OF lv_crhd,
            bukrs TYPE bukrs,
            bwkey TYPE bwkey,
            objid TYPE cr_objid,
            arbpl TYPE arbpl,
          END OF lv_crhd.

  DATA : lt_crhd TYPE TABLE OF lv_crhd.
  DATA : ls_crhd TYPE lv_crhd.

***************** CRTX table type structure ********************
  TYPES : BEGIN OF lv_crtx,
            objid    TYPE objid,
            spras    TYPE spras,
            ktext_up TYPE cr_ktextup,
            kostl    TYPE kostl,
          END OF lv_crtx.

  DATA : lt_crtx TYPE TABLE OF lv_crtx.
  DATA : ls_crtx TYPE lv_crtx.

****************** work area of export *****************
  DATA : ls_final TYPE zfarm_work_centre.





  SELECT a~bukrs a~bwkey b~objid b~arbpl  " this select query is for fetch the FARM and WORK CENTRE
    FROM t001k AS a INNER JOIN crhd AS b
    ON a~bwkey = b~werks
    INTO TABLE lt_crhd
    WHERE bukrs = comp_code.

  IF lt_crhd IS NOT INITIAL.         " this select query is for fetch the description of FARM

    SELECT objid spras ktext_up
      FROM crtx
      INTO TABLE lt_crtx
      FOR ALL ENTRIES IN lt_crhd
      WHERE objid = lt_crhd-objid
      AND spras = sy-langu.
    IF sy-subrc = 0.
      SORT : lt_crtx BY objid.
    ENDIF.

    SELECT objid, kostl              " to fetch the cost centre
      FROM crco
      INTO TABLE @DATA(lt_crco)
      FOR ALL ENTRIES IN @lt_crhd
      WHERE objid = @lt_crhd-objid.
    IF sy-subrc = 0.
      SORT : lt_crco BY objid.
    ENDIF.

  ENDIF.

  LOOP AT lt_crhd INTO ls_crhd.
    ls_final-company_code = ls_crhd-bukrs.
    ls_final-plant = ls_crhd-bwkey.
    ls_final-work_centre = ls_crhd-arbpl.

    CLEAR : ls_crtx.
    READ TABLE lt_crtx INTO ls_crtx WITH KEY objid = ls_crhd-objid BINARY SEARCH.
    IF sy-subrc = 0.
      ls_final-work_desc = ls_crtx-ktext_up.
    ENDIF.

    READ TABLE lt_crco INTO DATA(ls_crco) WITH KEY objid = ls_crhd-objid BINARY SEARCH.
    IF sy-subrc = 0.
      ls_final-cost_centre = ls_crco-kostl.
    ENDIF.

    APPEND ls_final TO farm_detls.
    CLEAR : ls_final.


  ENDLOOP.

ENDFUNCTION.
