FUNCTION zbapi_cust_potential.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(YEAR) TYPE  GJAHR OPTIONAL
*"     VALUE(MONTH) TYPE  ZMONTHS OPTIONAL
*"     VALUE(REGIO) TYPE  REGIO OPTIONAL
*"     VALUE(KATR6) TYPE  KATR6 OPTIONAL
*"  TABLES
*"      IT_POTENTIAL STRUCTURE  ZSTRUC_POTENTIAL
*"----------------------------------------------------------------------
*&---------------------------------------------------------------------*
*& Date Created - 29.03.2022
*& Created By   - KPABAP (Shamsudeen)
*& Description  - Program to set the sales potential for a customer
*  TR No        - DEVK931600
*&---------------------------------------------------------------------*
* Change Date   - 21.04.2022
* TR NO         - DEVK931619
* Changed By    - KPABAP (Shamsudeen)
* Reference     - User Discussion of required changes
* Description   - to send all details from sales table along with Material group
*                 Description
*&----------------------------------------------------------------------

  DATA: lt_zsales     TYPE STANDARD TABLE OF  zsales_table,
        lt_zsales1    TYPE STANDARD TABLE OF  zsales_table,
        ls_sales      TYPE  zsales_table,
        ls_potential  TYPE zstruc_potential,
        ls_potential1 TYPE zstruc_potential,
        lt_potential  TYPE STANDARD TABLE OF zstruc_potential,
        lt_potential1 TYPE STANDARD TABLE OF zstruc_potential,
        lt_t023t      TYPE STANDARD TABLE OF t023t,
        ls_t023t      TYPE t023t,
        lv_potent     TYPE volum,
        lv_commit     TYPE zcommitment.

  DATA lv_where TYPE string.
  DATA : ls_condtab TYPE hrcond.

  REFRESH:lt_zsales,it_potential.

  DATA : lt_condtab      TYPE STANDARD TABLE OF hrcond,
         lt_where_clause TYPE STANDARD TABLE OF ty_where_clause.
* Get Data From Sales custom table based on the input

  IF Year IS NOT INITIAL.
    ls_condtab-field = 'GJAHR'.
    ls_condtab-opera = 'EQ'.
    ls_condtab-low   = year.
    APPEND ls_condtab TO lt_condtab.
  ENDIF.

  IF month IS NOT INITIAL.
    ls_condtab-field = 'ZMONTHS'.
    ls_condtab-opera = 'EQ'.
    ls_condtab-low   = month.
    APPEND ls_condtab TO lt_condtab.
  ENDIF.

  IF regio IS NOT INITIAL.
    ls_condtab-field = 'REGIO'.
    ls_condtab-opera = 'EQ'.
    ls_condtab-low   = regio.
    APPEND ls_condtab TO lt_condtab.
  ENDIF.

  IF katr6 IS NOT INITIAL.
    ls_condtab-field = 'KATR6'.
    ls_condtab-opera = 'EQ'.
    ls_condtab-low   = katr6.
    APPEND ls_condtab TO lt_condtab.
  ENDIF.

  CALL FUNCTION 'RH_DYNAMIC_WHERE_BUILD'
    EXPORTING
      dbtable         = 'ZSALES_TABLE'
    TABLES
      condtab         = lt_condtab
      where_clause    = lt_where_clause
    EXCEPTIONS
      empty_condtab   = 1
      no_db_field     = 2
      unknown_db      = 3
      wrong_condition = 4
      OTHERS          = 5.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.


  IF lt_condtab[] IS INITIAL.
    SELECT  *
      FROM zsales_table
      INTO TABLE lt_zsales .
  ELSE.
    SELECT  *
      FROM zsales_table
      INTO TABLE lt_zsales
      WHERE (lt_where_clause).
  ENDIF.

*  IF regio IS NOT INITIAL AND katr6 IS NOT INITIAL.
*    SELECT  *
*      FROM zsales_table
*      INTO TABLE lt_zsales
*     WHERE regio EQ regio
*       AND katr6 EQ katr6.
*  ELSEIF regio IS INITIAL AND katr6 IS INITIAL.
*    SELECT  *
*      FROM zsales_table
*      INTO TABLE lt_zsales .
*  ELSEIF regio IS INITIAL AND katr6 IS NOT INITIAL.
*    SELECT  *
*      FROM zsales_table
*      INTO TABLE lt_zsales
*     WHERE katr6 EQ katr6.
*  ELSEIF regio IS NOT INITIAL AND katr6 IS INITIAL.
*    SELECT  *
*      FROM zsales_table
*      INTO TABLE lt_zsales
*     WHERE regio EQ regio.
*  ENDIF.

*GETTING MATERIAL GRP DESCRIPTION FROM TABLE T023T********
  SELECT * FROM t023t
           INTO TABLE lt_t023t
           WHERE spras EQ sy-langu.


  REFRESH:lt_potential,Lt_potential1,it_potential.

  LOOP AT lt_zsales INTO ls_sales.
    CLEAR ls_potential.
    ls_potential-kunnr = ls_sales-kunnr.
    ls_potential-matkl = ls_sales-matkl.
    READ TABLE lt_t023t INTO ls_t023t WITH KEY matkl = ls_sales-matkl.
    IF sy-subrc = 0.
      ls_potential-wgbez60 = ls_t023t-wgbez60.
    ENDIF.
    ls_potential-gjahr = ls_sales-gjahr.
    ls_potential-zmonths = ls_sales-zmonths.
    ls_potential-regio =  ls_sales-regio.
    ls_potential-katr6 = ls_sales-katr6.
*    ls_potential-zeinr = ls_sales-zeinr.
    ls_potential-potential = ls_sales-potential.
    ls_potential-commitment = ls_sales-commitment.
    APPEND  ls_potential TO it_potential.
  ENDLOOP.

*  Lt_potential1[] = Lt_potential[].
*  SORT Lt_potential1 BY kunnr zeinr.
*  DELETE ADJACENT DUPLICATES FROM Lt_potential1 COMPARING kunnr zeinr.
*
*  CLEAR ls_potential1.
*  LOOP AT Lt_potential1 INTO Ls_potential1.
*    CLEAR:lv_potent,Ls_potential.
*    LOOP AT lt_potential INTO Ls_potential WHERE kunnr = Ls_potential1-kunnr AND
*                                                  zeinr = Ls_potential1-zeinr.
*
*      lv_potent  = lv_potent + Ls_potential-potential.
*    ENDLOOP.
*    CLEAR:lv_commit,Ls_potential.
*    LOOP AT lt_potential INTO Ls_potential WHERE kunnr = Ls_potential1-kunnr AND
*                                                  zeinr = Ls_potential1-zeinr.
*
*      lv_commit  = lv_commit + ls_potential-commitment.
*    ENDLOOP.
*    CLEAR Ls_potential1-potential.
*    CLEAR ls_potential1-commitment.
*    Ls_potential1-potential = lv_potent.
*    ls_potential1-commitment = lv_commit.
*    APPEND Ls_potential1 TO it_potential.
*    CLEAR Ls_potential1.



*  Endloop.


ENDFUNCTION.
