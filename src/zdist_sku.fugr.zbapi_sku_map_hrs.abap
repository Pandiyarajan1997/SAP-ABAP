FUNCTION zbapi_sku_map_hrs.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(KUNNR) TYPE  KUNNR OPTIONAL
*"     VALUE(BEGDA) TYPE  ERDAT OPTIONAL
*"     VALUE(ENDDA) TYPE  AEDAT OPTIONAL
*"  TABLES
*"      IT_SKU_MAP STRUCTURE  ZSKU_STRUC
*"----------------------------------------------------------------------
  DATA: lt_sku_map TYPE STANDARD TABLE OF zsku_table,
        ls_sku_map TYPE zsku_table,
        ls_sku     TYPE zsku_struc.

  REFRESH: lt_sku_map,it_sku_map.

  IF kunnr IS INITIAL AND begda IS INITIAL AND endda IS INITIAL.

    SELECT *   FROM zsku_table INTO TABLE lt_sku_map.

  ELSEIF kunnr IS NOT INITIAL AND begda IS INITIAL AND endda IS INITIAL.

    SELECT *  FROM zsku_table INTO TABLE lt_sku_map WHERE customer_no = kunnr.

  ELSEIF begda IS NOT INITIAL AND endda IS NOT INITIAL.

    SELECT *  FROM zsku_table INTO TABLE lt_sku_map WHERE ( changed_on BETWEEN begda AND endda ).

  ELSEIF begda IS NOT INITIAL AND endda IS INITIAL.

    endda = sy-datum.

    SELECT * FROM zsku_table INTO TABLE lt_sku_map WHERE ( changed_on BETWEEN begda AND endda ).

  ENDIF.

  LOOP AT  lt_sku_map INTO ls_sku_map.


    CLEAR ls_sku-distributor_1.
    ls_sku-customer_no =  ls_sku_map-customer_no.
    ls_sku-distributor_1 = ls_sku_map-distributor_2.
    ls_sku-changed_on = ls_sku_map-changed_on.
    ls_sku-status = ls_sku_map-status.
    APPEND ls_sku TO it_sku_map.

    CLEAR ls_sku-distributor_1.
    ls_sku-customer_no =  ls_sku_map-customer_no.
    ls_sku-distributor_1 = ls_sku_map-distributor_3.
    ls_sku-changed_on = ls_sku_map-changed_on.
    ls_sku-status = ls_sku_map-status.
    APPEND ls_sku TO it_sku_map.

  ENDLOOP.




ENDFUNCTION.
