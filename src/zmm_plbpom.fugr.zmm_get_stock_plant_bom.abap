FUNCTION zmm_get_stock_plant_bom.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(MATNR) TYPE  MATNR
*"     VALUE(PLANT) TYPE  MARC-WERKS
*"     VALUE(FKIMG) TYPE  FKIMG
*"     VALUE(VRKME) TYPE  VRKME
*"  EXPORTING
*"     VALUE(AVAILABLE) TYPE  CHAR1
*"  TABLES
*"      IT_COMP STRUCTURE  ZSSTPOX
*"  EXCEPTIONS
*"      MATERIAL_INCORRECT
*"      MATERIAL_NOT_IN_PLANT
*"      INCORRECT_UOM
*"      SALE_QUANTITY_MISSING
*"      NO_BOM_FOUND
*"      OTHER_ERROR
*"----------------------------------------------------------------------
**************************************************************************
*Developed By : Ramakrishnan ( 705322 ).
*Reference By : Praveen
*Requirement By: Purchase Team
*Description : Check stock for all RM/PM used for Material and compare
*             available qty vs required quantity
**************************************************************************


  DATA: ls_mara TYPE mara,
        ls_marc TYPE marc.

  DATA: lt_stb    TYPE TABLE OF stpox,
        ls_stb    TYPE stpox,
        lt_matcat TYPE TABLE OF cscmat,
        ls_matcat TYPE cscmat.

  DATA: lv_req_qty TYPE ekpo-menge.

  DATA: lt_stock TYPE TABLE OF zstr_safety_stock,
        ls_stock TYPE zstr_safety_stock,
        lt_matnr TYPE TABLE OF mat_range.

  DATA: lv_stock TYPE labst.

  DATA:     ls_mat_range TYPE mat_range.

  DATA: ls_comp TYPE zsstpox.

*Set Variable as 'N' initially
  available = 'N'.

*Material No Validation
  SELECT SINGLE * FROM mara INTO ls_mara WHERE matnr = matnr.
  IF sy-subrc NE 0.
    RAISE material_incorrect.
  ENDIF.

*Material - Plant Validation
  SELECT SINGLE * FROM marc INTO ls_marc WHERE matnr = matnr AND werks = plant.
  IF sy-subrc NE 0.
    RAISE material_not_in_plant.
  ENDIF.

*Convert Material from Sale UOM to Material Base UOM
  CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
    EXPORTING
      i_matnr              = ls_mara-matnr
      i_in_me              = vrkme
      i_out_me             = ls_mara-meins
      i_menge              = fkimg
    IMPORTING
      e_menge              = lv_req_qty
    EXCEPTIONS
      error_in_application = 1
      error                = 2
      OTHERS               = 3.
  IF sy-subrc <> 0.
    RAISE incorrect_uom.
  ENDIF.

*Expand the BOM completely and get all the relevant componets
  CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
    EXPORTING
      capid                 = 'PP01'
      datuv                 = sy-datum
      emeng                 = lv_req_qty
      mehrs                 = 'X'
      mtnrv                 = ls_marc-matnr
      werks                 = ls_marc-werks
    TABLES
      stb                   = lt_stb
      matcat                = lt_matcat
    EXCEPTIONS
      alt_not_found         = 1
      call_invalid          = 2
      material_not_found    = 3
      missing_authorization = 4
      no_bom_found          = 5
      no_plant_data         = 6
      no_suitable_bom_found = 7
      conversion_error      = 8
      OTHERS                = 9.
  IF sy-subrc <> 0.
    IF sy-subrc = '1' OR sy-subrc = 5 OR sy-subrc = 7.
      RAISE no_bom_found.
    ELSEIF sy-subrc = '3'.
      RAISE material_incorrect.
    ELSEIF sy-subrc = '6'.
      RAISE material_not_in_plant.
    ELSEIF sy-subrc = '8'.
      RAISE incorrect_uom.
    ELSE.
      RAISE other_error.
    ENDIF.

  ELSE.

    IF lt_stb[] IS NOT INITIAL.
* Remove all the manufactured items and keep only the RM and PM
      LOOP AT lt_matcat INTO ls_matcat.
        DELETE lt_stb WHERE idnrk = ls_matcat-matnr.
      ENDLOOP.

      IF lt_stb[] IS NOT INITIAL.
*Collect all the required materials to get the unrestricted quantity
        LOOP AT lt_stb INTO ls_stb.
          ls_mat_range-sign   = 'I'.
          ls_mat_range-option = 'EQ'.
          ls_mat_range-matnr_low    = ls_stb-idnrk.
          APPEND ls_mat_range TO lt_matnr.
        ENDLOOP.
*Get the unrestricted quantity
        CALL FUNCTION 'ZBAPI_SAFETY_STOCK_MIS'
          EXPORTING
            plant    = ls_marc-werks
          TABLES
            it_stock = lt_stock
            it_matnr = lt_matnr.

        IF lt_stock[] IS NOT INITIAL.


*Consider materials available
          available = 'Y'.
          LOOP AT lt_stb INTO ls_stb.
            CLEAR ls_comp.
            MOVE-CORRESPONDING ls_stb TO ls_comp.

*Check stock based on the material
            CLEAR lv_stock.
            LOOP AT lt_stock INTO ls_stock WHERE material = ls_stb-idnrk.
              lv_stock = lv_stock + ls_stock-unrestricted_stk.
            ENDLOOP.
*            READ TABLE lt_stock1 INTO ls_stock1 WITH KEY material = ls_stb-idnrk.
            IF sy-subrc = 0.
*Compare required quantity vs available quantity based on Stock of Material Base UOM
              IF ls_stb-mnglg GT lv_stock.
                available = 'N'.
              ENDIF.
              ls_comp-avbqty = lv_stock.
            ELSE.
              available = 'N'.
            ENDIF.
            APPEND ls_comp TO it_comp.
          ENDLOOP.
        ELSE.
*if the Stock is not available for any UOM then set as "N"
          available = 'N'.
        ENDIF.

      ENDIF.
    ENDIF.

  ENDIF.



ENDFUNCTION.
