FUNCTION zbapi_supply_quan_plantwise.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(PLANT) TYPE  RANGE_S_WERKS OPTIONAL
*"  TABLES
*"      LT_SUPPLY_QUAN STRUCTURE  ZSTR_SUPPLY_QUAN
*"  EXCEPTIONS
*"      INCORRECT_PLANT
*"----------------------------------------------------------------------
  " Created by: Samsudeen M
  "Created on : 28.07.2022
  "Description : Sending New fields which added in MARC for Quantity
  "Reference by: Nagarathinam
*--------------------------------------------------------------------
  TYPES: BEGIN OF ty_marc,
           werks           TYPE werks_d,
           matnr           TYPE matnr,
           supply_quantity TYPE menge_d,
           suplly_uom      TYPE meins,
         END OF ty_marc.

  DATA: lt_marc TYPE TABLE OF ty_marc,
        ls_marc TYPE ty_marc.

  DATA: ls_supply_quan TYPE zstr_supply_quan.

  DATA: gs_marc TYPE marc.

  IF plant-low IS   NOT INITIAL.
    CLEAR gs_marc.
    SELECT SINGLE * FROM marc
                    INTO gs_marc WHERE werks EQ plant-low.
    IF sy-subrc NE 0.
      RAISE incorrect_plant.
    ENDIF.
  ENDIF.



  REFRESH: lt_marc.
  CLEAR ls_marc.

  IF plant IS INITIAL.
********** Fetching all record from marc *******************
    SELECT werks
           matnr
           supply_quantity
           suplly_uom  FROM marc
                       INTO TABLE  lt_marc
                       WHERE supply_quantity NE '0'.
    IF sy-subrc EQ 0.
      SORT lt_marc[] BY werks matnr.
    ENDIF.

  ELSEIF plant-low IS NOT INITIAL.
********** Fetching all record from marc *******************
    SELECT werks
           matnr
           supply_quantity
           suplly_uom  FROM marc
                       INTO TABLE  lt_marc
                       WHERE werks EQ plant-low
                       AND supply_quantity NE '0'.
    IF sy-subrc EQ 0.
      SORT lt_marc[] BY werks matnr.
    ENDIF.

  ELSEIF plant-low IS NOT INITIAL AND plant-high IS NOT INITIAL.
********** Fetching all record from marc *******************
    SELECT werks
           matnr
           supply_quantity
           suplly_uom  FROM marc
                       INTO TABLE  lt_marc
                       WHERE ( werks BETWEEN plant-low AND plant-high )
                       AND supply_quantity NE '0'.
    IF sy-subrc EQ 0.
      SORT lt_marc[] BY werks matnr.
    ENDIF.

  ENDIF.

  IF lt_marc[] IS NOT INITIAL.
    REFRESH lt_supply_quan.
    CLEAR ls_marc.
    LOOP AT lt_marc INTO ls_marc.
      CLEAR ls_supply_quan.
      ls_supply_quan-plant = ls_marc-werks.
      ls_supply_quan-material = ls_marc-matnr.
      ls_supply_quan-supply_quantity = ls_marc-supply_quantity.
      ls_supply_quan-supply_uom = ls_marc-suplly_uom.
      APPEND ls_supply_quan TO lt_supply_quan.
    ENDLOOP.
  ENDIF.

ENDFUNCTION.
