FUNCTION zmm_get_matnr_bom.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(MATNR) TYPE  WRBA_MATNR_TABLE OPTIONAL
*"     VALUE(PLANT) TYPE  MARC-WERKS
*"  TABLES
*"      IT_COMP STRUCTURE  ZPP_ST_BOM
*"----------------------------------------------------------------------
**************************************************************************
*Developed By : Ramakrishnan ( 705322 ).
*Reference By : Praveen
*Requirement By: Purchase Team
*Description : Check stock for all RM/PM used for Material and compare
*             available qty vs required quantity
**************************************************************************


*  DATA: lt_mara TYPE STANDARD TABLE OF mara,
*        lt_marc TYPE STANDARD TABLE OF marc.

  DATA: lt_stb    TYPE TABLE OF stpox,
        lt_stb_2  TYPE TABLE OF stpox,
        ls_stb    TYPE stpox,
        lt_matcat TYPE TABLE OF cscmat,
        ls_matcat TYPE cscmat.

  DATA: ls_comp TYPE zpp_st_bom.

*Material No Validation
  SELECT a~matnr,
         a~meins,
         b~maktx FROM mara AS a
    INNER JOIN makt AS b ON a~matnr = b~matnr
    INTO TABLE @DATA(lt_mara)
    FOR ALL ENTRIES IN @matnr
    WHERE a~matnr = @matnr-matnr AND
          a~mtart IN ('FERT', 'HALB') AND
          status EQ @space.

  CHECK lt_mara IS NOT INITIAL.
*Material - Plant Validation
  SELECT  matnr,
          werks FROM marc INTO TABLE @DATA(lt_marc)
    FOR ALL ENTRIES IN @lt_mara WHERE matnr = @lt_mara-matnr
                                 AND werks = @plant.

  CHECK lt_marc IS NOT INITIAL.
  DATA: lv_req_qty TYPE ekpo-menge.
  lv_req_qty = 1.
  LOOP AT lt_marc INTO DATA(ls_marc).
    CLEAR: lt_stb[],lt_matcat[].
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
    IF sy-subrc = 0.
      IF lt_stb[] IS NOT INITIAL.
* Remove all the manufactured items and keep only the RM and PM
        LOOP AT lt_matcat INTO ls_matcat.
          DELETE lt_stb WHERE idnrk = ls_matcat-matnr.
        ENDLOOP.
        LOOP AT lt_stb INTO ls_stb.
          CLEAR ls_comp.
          MOVE-CORRESPONDING ls_stb TO ls_comp.
          ls_comp-matnr = ls_marc-matnr.
          ls_comp-maktx = VALUE #( lt_mara[ matnr = ls_marc-matnr ]-maktx OPTIONAL ).

          COLLECT ls_comp INTO it_comp.
        ENDLOOP.
      ENDIF.
    ENDIF.
    APPEND LINES OF lt_stb TO lt_stb_2.
  ENDLOOP.
  SORT it_comp[] BY matnr idnrk.
  SORT lt_stb_2 BY idnrk.
  DELETE ADJACENT DUPLICATES FROM lt_stb_2 COMPARING idnrk.
  IF it_comp[] IS NOT INITIAL.
    SELECT  matnr,
            vprsv,
            verpr,
            stprs
          FROM mbew
      INTO TABLE @DATA(lt_mbew)
      FOR ALL ENTRIES IN @it_comp
      WHERE matnr = @it_comp-idnrk AND
            bwkey = @plant.
    IF sy-subrc = 0.
      DELETE ADJACENT DUPLICATES FROM lt_mbew COMPARING matnr.
      LOOP AT it_comp ASSIGNING FIELD-SYMBOL(<fs>).
        DATA(l_pcont) = VALUE #( lt_mbew[ matnr = <fs>-idnrk ]-vprsv OPTIONAL ).
        CASE l_pcont.
          WHEN 'V'.
            <fs>-base_unit_pr = VALUE #( lt_mbew[ matnr = <fs>-idnrk ]-verpr OPTIONAL ).
          WHEN 'S'.
            <fs>-base_unit_pr = VALUE #( lt_mbew[ matnr = <fs>-idnrk ]-stprs OPTIONAL ).
          WHEN OTHERS.
        ENDCASE.
        IF <fs>-mmein = <fs>-meins.
          <fs>-bom_unit_pr = <fs>-base_unit_pr.
        ELSE.
          <fs>-bom_unit_pr = ( VALUE #( lt_stb_2[ idnrk = <fs>-idnrk ]-umrez OPTIONAL ) / VALUE #( lt_stb_2[ idnrk = <fs>-idnrk ]-umren OPTIONAL ) ) * <fs>-base_unit_pr.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFUNCTION.
