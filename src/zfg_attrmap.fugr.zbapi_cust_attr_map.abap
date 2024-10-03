FUNCTION zbapi_cust_attr_map.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(CUSTOMER) TYPE  KUNNR OPTIONAL
*"  TABLES
*"      IT_CUSTATTR STRUCTURE  ZSTR_CUSTATTR
*"----------------------------------------------------------------------

  DATA: gt_cust TYPE STANDARD TABLE OF zcust_attribute,
        gs_cust TYPE zcust_attribute.
  DATA: ls_cust TYPE zstr_custattr.
  DATA: lt_domain  TYPE TABLE OF dd07v,
        lt_domain1 TYPE TABLE OF dd07v,
        ls_domain  TYPE dd07v.

** getting domain Values **
  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      domname    = 'ZZATTR2'
    TABLES
      values_tab = lt_domain[].
** Getting Domain Values **
  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      domname    = 'ZZATTR3'
    TABLES
      values_tab = lt_domain1[].

  REFRESH: gt_cust.
  IF customer IS NOT INITIAL.
** Selecting Specific customer record **
    SELECT * FROM zcust_attribute
             INTO TABLE gt_cust
             WHERE customer EQ customer.
    IF sy-subrc EQ 0.
      SORT gt_cust[] BY customer.
    ENDIF.
  ELSE.
** Selecting all record from table**
    SELECT * FROM zcust_attribute
             INTO TABLE gt_cust.
    IF sy-subrc EQ 0.
      SORT gt_cust[] BY customer.
    ENDIF.
  ENDIF.

  LOOP AT gt_cust INTO gs_cust.

    MOVE-CORRESPONDING gs_cust TO ls_cust.
    CLEAR ls_domain.
    READ TABLE lt_domain INTO ls_domain WITH KEY domvalue_l = ls_cust-attribute.
    IF sy-subrc EQ 0.
      ls_cust-attr_des = ls_domain-ddtext.
    ENDIF.
    CLEAR ls_domain.
    READ TABLE lt_domain1 INTO ls_domain WITH KEY domvalue_l = ls_cust-attribute_des.
    IF sy-subrc EQ 0.
      ls_cust-attr1_des = ls_domain-ddtext.
    ENDIF.
    ls_cust-date = gs_cust-reference_date.
    APPEND ls_cust TO it_custattr.
    CLEAR: ls_cust,gs_cust.

  ENDLOOP.

  IF it_custattr[] IS NOT INITIAL.
    SORT it_custattr[] BY customer.
  ENDIF.



ENDFUNCTION.
