FUNCTION ZPP_GET_BOM_HIER.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(MATNR) TYPE  WRBA_MATNR_TABLE OPTIONAL
*"     VALUE(PLANT) TYPE  MARC-WERKS
*"  TABLES
*"      IT_HIER STRUCTURE  ZPP_BOM_HIER
*"----------------------------------------------------------------------

  DATA: lt_stb    TYPE TABLE OF stpox,
        ls_stb    TYPE stpox,
        lt_matcat TYPE TABLE OF cscmat,
        ls_matcat TYPE cscmat.

  DATA: ls_hier TYPE zpp_bom_hier.

*Material No Validation
  SELECT matnr,
         mtart
    FROM mara
    INTO TABLE @DATA(lt_mara)
    FOR ALL ENTRIES IN @matnr
    WHERE matnr = @matnr-matnr AND
          mtart = 'FERT' AND
          status EQ @space.

  DELETE lt_mara WHERE mtart ne 'FERT'.

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
    CLEAR: ls_hier.
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
      IF lt_matcat[] is NOT INITIAL.

        SORT lt_matcat by INDEX ASCENDING.

        CLEAR ls_hier.
        LOOP AT lt_matcat INTO ls_matcat.
          CASE sy-tabix.
            WHEN 1.
              ls_hier-MATNR01 = ls_matcat-matnr.
            WHEN 2.
              ls_hier-MATNR02 = ls_matcat-matnr.
            WHEN 3.
              ls_hier-MATNR03 = ls_matcat-matnr.
            WHEN 4.
              ls_hier-MATNR04 = ls_matcat-matnr.
            WHEN 5.
              ls_hier-MATNR05 = ls_matcat-matnr.
            WHEN 6.
              ls_hier-MATNR06 = ls_matcat-matnr.
            WHEN OTHERS.
          ENDCASE.
        ENDLOOP.
        IF sy-subrc = 0.
          append ls_hier to it_hier.
        ENDIF.

      ENDIF.
    ENDIF.

  ENDLOOP.

ENDFUNCTION.
