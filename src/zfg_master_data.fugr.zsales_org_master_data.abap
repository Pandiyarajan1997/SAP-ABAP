FUNCTION zsales_org_master_data.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IN_BUKRS) TYPE  /ACCGO/CMN_TT_BUKRS OPTIONAL
*"  TABLES
*"      IT_SALES_ORG STRUCTURE  ZSTR_SALES_ORG
*"----------------------------------------------------------------------
*& Created by: Samsudeen M
*& Created on: 02.03.2023
*& Purpose: Sales Organization Master Data
*& Reference by: Ramakrishnan J
*-----------------------------------------------------------------------

  DATA: gt_sales_org TYPE STANDARD TABLE OF zstr_sales_org.
  DATA: lr_bukrs TYPE RANGE OF t001-bukrs.

  IF in_bukrs IS NOT INITIAL.
    REFRESH: lr_bukrs.
    LOOP AT in_bukrs INTO DATA(lw_bukrs).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = lw_bukrs-bukrs ) TO lr_bukrs.
    ENDLOOP.
  ENDIF.

  REFRESH: gt_sales_org,it_sales_org.
  SELECT a~vkorg
         b~vtext
         a~bukrs
         c~butxt INTO CORRESPONDING FIELDS OF TABLE gt_sales_org
                 FROM tvko AS a INNER JOIN tvkot AS b
                 ON a~vkorg = b~vkorg
                 INNER JOIN t001 AS c
                 ON a~bukrs = c~bukrs
                 WHERE b~spras = sy-langu.
  IF sy-subrc = 0.
    SORT gt_sales_org[] BY bukrs vkorg.
    APPEND LINES OF gt_sales_org TO it_sales_org.
  ENDIF.

ENDFUNCTION.
