FUNCTION zbapi_material_source_list.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(PLANT) TYPE  WERKS_D OPTIONAL
*"     VALUE(MATERIAL) TYPE  RANGE_S_MATNR OPTIONAL
*"  TABLES
*"      LT_LIST STRUCTURE  ZSTR_SOURCE
*"----------------------------------------------------------------------

  TYPES: BEGIN OF lty_eord,
           matnr TYPE matnr,
           werks TYPE ewerk,
           vdatu TYPE ordab,
           bdatu TYPE ordbi,
           lifnr TYPE elifn,
           reswk TYPE bewrk,
           ekorg TYPE ekorg,
         END OF lty_eord.

  DATA: lt_eord TYPE TABLE OF lty_eord,
        ls_eord TYPE lty_eord.

  DATA: ls_source TYPE zstr_source.

  REFRESH:lt_eord.

  IF plant IS INITIAL AND material IS INITIAL.

    SELECT matnr
           werks
           vdatu
           bdatu
           lifnr
           reswk
           ekorg FROM eord
                 INTO TABLE lt_eord
                 WHERE bdatu GE sy-datum.
    IF sy-subrc EQ 0.
      SORT lt_eord[] BY matnr werks.
    ENDIF.

  ELSEIF plant IS NOT INITIAL AND material IS INITIAL.

    SELECT matnr
           werks
           vdatu
           bdatu
           lifnr
           reswk
           ekorg FROM eord
                 INTO TABLE lt_eord
                 WHERE werks EQ plant
                 AND bdatu GE sy-datum.
    IF sy-subrc EQ 0.
      SORT lt_eord[] BY matnr werks.
    ENDIF.

  ELSEIF plant IS NOT INITIAL AND material-low IS NOT INITIAL.

    SELECT matnr
           werks
           vdatu
           bdatu
           lifnr
           reswk
           ekorg FROM eord
                 INTO TABLE lt_eord
                 WHERE matnr EQ material-low
                 AND werks EQ plant
                 AND bdatu GE sy-datum.
    IF sy-subrc EQ 0.
      SORT lt_eord[] BY matnr werks.
    ENDIF.

  ELSEIF plant IS NOT INITIAL AND material-low IS NOT INITIAL
                              AND material-high IS NOT INITIAL.

    SELECT matnr
           werks
           vdatu
           bdatu
           lifnr
           reswk
           ekorg FROM eord
                 INTO TABLE lt_eord
                 WHERE ( matnr BETWEEN material-low AND material-high )
                 AND werks EQ plant
                 AND bdatu GE sy-datum.
    IF sy-subrc EQ 0.
      SORT lt_eord[] BY matnr werks.
    ENDIF.

  ENDIF.

  IF lt_eord[] IS NOT INITIAL.

    "Select material Desc and Vendor name"
    SELECT matnr,maktx FROM makt
                       INTO TABLE @DATA(lt_makt)
                       FOR ALL ENTRIES IN @lt_eord
                       WHERE matnr = @lt_eord-matnr
                       AND spras = @sy-langu.
    IF sy-subrc EQ 0.
      SORT lt_makt[] BY matnr.
    ENDIF.

    SELECT lifnr,name1 FROM lfa1
                       INTO TABLE @DATA(lt_lfa1)
                       FOR ALL ENTRIES IN @lt_eord
                       WHERE lifnr = @lt_eord-lifnr.
    IF sy-subrc EQ 0.
      SORT lt_lfa1[] BY lifnr.
    ENDIF.

  ENDIF.


  LOOP AT lt_eord INTO ls_eord.
    CLEAR ls_source.
    ls_source-material = ls_eord-matnr.

    READ TABLE lt_makt INTO DATA(ls_makt) WITH KEY matnr = ls_eord-matnr.
    IF sy-subrc EQ 0.
      ls_source-material_des = ls_makt-maktx.
    ENDIF.

    ls_source-plant = ls_eord-werks.
    ls_source-valid_from = ls_eord-vdatu.
    ls_source-valid_to = ls_eord-bdatu.
    ls_source-supplier = ls_eord-lifnr.

    READ TABLE lt_lfa1 INTO DATA(ls_lfa1) WITH KEY lifnr = ls_eord-lifnr.
    IF sy-subrc EQ 0.
      ls_source-supplier_name = ls_lfa1-name1.
    ENDIF.

    ls_source-reswk = ls_eord-reswk.
    ls_source-purchase_org = ls_eord-ekorg.

    APPEND ls_source TO lt_list.

    CLEAR ls_eord.
  ENDLOOP.

ENDFUNCTION.
