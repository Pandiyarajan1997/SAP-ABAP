FUNCTION zbapi_plant_regionwise.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(REGION) TYPE  /ACCGO/CAS_TT_REGIO_RANGE OPTIONAL
*"  TABLES
*"      LT_PLANT_REG STRUCTURE  ZSTR_REG_PLANT
*"----------------------------------------------------------------------
*& changed by: Samsudeen M
*& Changed on: 02.03.2023
*& Purpose: Plant Master Data
*& Reference by: Ramakrishnan J
*-----------------------------------------------------------------------
  DATA: gt_plant_data TYPE STANDARD TABLE OF zstr_reg_plant.
  DATA: lr_region TYPE RANGE OF t001w-regio.

  IF region IS NOT INITIAL.
    REFRESH lr_region.
    LOOP AT region INTO DATA(ls_regio).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = ls_regio-low ) TO lr_region.
    ENDLOOP.
  ENDIF.

  REFRESH: gt_plant_data,lt_plant_reg.
  " Data fetching
  SELECT a~werks
         a~name1
         a~stras
         a~pstlz
         a~ort01
         a~land1
         a~regio
         b~bukrs INTO CORRESPONDING FIELDS OF TABLE gt_plant_data
                 FROM t001w AS a INNER JOIN t001k AS b
                 ON a~werks = b~bwkey
                 WHERE regio IN lr_region.
  IF sy-subrc = 0.
    SORT gt_plant_data[] BY regio werks.
    APPEND LINES OF gt_plant_data TO lt_plant_reg.
  ENDIF.

ENDFUNCTION.
