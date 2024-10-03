FUNCTION zbapi_emp_saletype.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(EMPLOY_CODE) TYPE  PERNR_D OPTIONAL
*"  TABLES
*"      LT_SALTYP STRUCTURE  ZSTR_SALTYP
*  Date Created - 12.05.2022
*& Created By   - KPABAP (samsudeen)
*& Description  - BAPI for Employee mapping With saletype
*  Reference - Ramakrishnan
*"----------------------------------------------------------------------

  DATA: ls_saltyp TYPE zstr_saltyp.
  DATA: lt_sales TYPE STANDARD TABLE OF zsd_saltyp,
        ls_sales TYPE zsd_saltyp.

  DATA: lv_saldes TYPE char50.

  DATA: lt_domain TYPE TABLE OF dd07v,
        ls_domain TYPE dd07v.

  REFRESH: lt_sales,lt_saltyp.

  IF employ_code IS NOT INITIAL.
    SELECT * FROM zsd_saltyp
           INTO TABLE lt_sales
           WHERE employ_code = employ_code.

  ELSEIF employ_code IS INITIAL.
    SELECT * FROM zsd_saltyp
             INTO TABLE lt_sales.
  ENDIF.


** Getting Domain Values **
  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      domname    = 'ZSALTYPE'
    TABLES
      values_tab = lt_domain[].

  LOOP AT lt_sales INTO ls_sales.
    CLEAR ls_saltyp.
    ls_saltyp-pernr = ls_sales-employ_code.
    ls_saltyp-saletype = ls_sales-saletype.
    CLEAR ls_domain.
    READ TABLE lt_domain INTO ls_domain WITH KEY domvalue_l = ls_sales-saletype.
    IF sy-subrc EQ 0.
      ls_saltyp-saltype_des = ls_domain-ddtext.
    ENDIF.
  APPEND ls_saltyp TO lt_saltyp.
  CLEAR: ls_sales,ls_saltyp.
ENDLOOP.

ENDFUNCTION.
