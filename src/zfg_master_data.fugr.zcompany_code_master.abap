FUNCTION zcompany_code_master.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IN_BUKRS) TYPE  /ACCGO/CMN_TT_BUKRS OPTIONAL
*"  TABLES
*"      IT_COMP_CODE STRUCTURE  ZSTR_COMP_CODE
*"  EXCEPTIONS
*"      MAINTAIN_COUNTRY_VARIABLE
*"----------------------------------------------------------------------
*& Created by: Samsudeen M
*& Created on: 02.03.2023
*& Purpose: Company Code Master Data
*& Reference by: Ramakrishnan J
*-----------------------------------------------------------------------

  DATA: gt_comp_code TYPE TABLE OF zstr_comp_code.
  DATA: lr_bukrs   TYPE RANGE OF t001-bukrs,
        lr_country TYPE RANGE OF t001-land1.

  IF in_bukrs IS NOT INITIAL.
    REFRESH: lr_bukrs.
    LOOP AT in_bukrs INTO DATA(lw_bukrs).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = lw_bukrs-bukrs ) TO lr_bukrs.
    ENDLOOP.
  ENDIF.
*---- Country Variable for specific Country -----*
  SELECT * FROM tvarvc INTO TABLE @DATA(lt_tvarvc) WHERE name = 'COMP_CODE_MASTER_COUNTRY'
                                                   AND type = 'S'.
  IF sy-subrc = 0.
    REFRESH: lr_country.
    LOOP AT lt_tvarvc INTO DATA(lw_tvarvc).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = lw_tvarvc-low ) TO lr_country.
    ENDLOOP.
  ELSE.
    RAISE maintain_country_variable.
  ENDIF.
  REFRESH: gt_comp_code,it_comp_code.
  "Data Fetching for Company code
  SELECT bukrs
         butxt
         ort01
         land1
         ktopl INTO CORRESPONDING FIELDS OF TABLE gt_comp_code
               FROM t001 WHERE bukrs IN lr_bukrs
               AND land1 IN lr_country.
  IF sy-subrc = 0.
    SORT gt_comp_code[] BY land1 bukrs.
    APPEND LINES OF gt_comp_code TO it_comp_code.
  ENDIF.


ENDFUNCTION.
