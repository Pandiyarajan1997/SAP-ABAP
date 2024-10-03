FUNCTION zbapi_totstock.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(COMP_CODE) TYPE  BUKRS
*"  TABLES
*"      PLANT STRUCTURE  WERKS_RAN
*"      GT_OUTPUT STRUCTURE  ZST_DIST_STOCK_DMS
*"----------------------------------------------------------------------

  SELECT bwkey,bukrs FROM t001k INTO TABLE @DATA(lt_t001k) WHERE  bwkey IN @plant AND
                                                                  bukrs = @comp_code.

  IF sy-subrc = 0.
***********FETCH the datas from mard & mara**********
    SELECT c~kunnr,
           a~bwkey,
           a~matnr,
           a~lbkum,
           b~meins,a~salk3 INTO TABLE @gt_output
                   FROM mbew AS a
                   INNER JOIN mara AS b
                   ON b~matnr = a~matnr
                   INNER JOIN kna1 AS c
                   ON c~werks = a~bwkey
                   FOR ALL ENTRIES IN @lt_t001k
                   WHERE a~bwkey = @lt_t001k-bwkey
                   AND lbkum NE ' '.
    IF sy-subrc = 0.

      "fetch the data from the MARM table

      SELECT matnr,meinh FROM marm
        INTO TABLE @DATA(it_marm)
        FOR ALL ENTRIES IN @gt_output
        WHERE matnr = @gt_output-material.

      SORT: it_marm BY matnr meinh.
      SORT : gt_output BY plant material.
    ENDIF.
  ENDIF.

*  DATA : ls_output LIKE LINE OF gt_output.

  LOOP AT gt_output INTO DATA(ls_output).
    READ TABLE it_marm INTO DATA(ls_marm) WITH KEY matnr = ls_output-material
                                                   meinh = 'L' BINARY SEARCH.
    IF sy-subrc = 0.

      CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
        EXPORTING
          i_matnr  = ls_output-material
          i_in_me  = ls_output-uom
          i_out_me = 'L'
          i_menge  = ls_output-qty
        IMPORTING
          e_menge  = ls_output-stock_lit.


    ELSE.

      READ TABLE it_marm INTO ls_marm WITH KEY matnr = ls_output-material
                                                  meinh = 'KG' BINARY SEARCH.
      IF sy-subrc = 0.

        CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
          EXPORTING
            i_matnr  = ls_output-material
            i_in_me  = ls_output-uom
            i_out_me = 'KG'
            i_menge  = ls_output-qty
          IMPORTING
            e_menge  = ls_output-stock_lit.

      ENDIF.

    ENDIF.



    MODIFY gt_output FROM ls_output.
    CLEAR : ls_output.

  ENDLOOP.


ENDFUNCTION.
