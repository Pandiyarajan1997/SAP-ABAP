FUNCTION zmis_cust_status.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(KUNNR) TYPE  KUNNR OPTIONAL
*"  TABLES
*"      IT_MIS STRUCTURE  ZSTRUC_MIS
*"----------------------------------------------------------------------

  DATA: lt_status TYPE STANDARD TABLE OF zmis_cust_st,
        ls_status TYPE zmis_cust_st,
        ls_mis    TYPE zstruc_mis.

  REFRESH: lt_status.

  IF KUNNR IS NOT INITIAL.

    SELECT * FROM zmis_cust_st INTO TABLE lt_status WHERE customer_no EQ KUNNR.

  ELSEIF KUNNR IS INITIAL.

    SELECT * FROM zmis_cust_st INTO TABLE lt_status.

  ENDIF.

  REFRESH: it_mis.

  LOOP AT lt_status INTO ls_status.

    CLEAR ls_mis.
    ls_mis-customer_no = ls_status-customer_no.
    ls_mis-status = ls_status-status.
    APPEND ls_mis TO it_mis.
  ENDLOOP.






ENDFUNCTION.
