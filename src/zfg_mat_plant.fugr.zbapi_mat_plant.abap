FUNCTION zbapi_mat_plant.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(PLANT) TYPE  WERKS_D
*"  TABLES
*"      GT_OUTPUT TYPE  ZTY_MAT_PLANT
*"----------------------------------------------------------------------

  TYPES : BEGIN OF lv_marc,
            werks TYPE werks_d,
            matnr TYPE matnr,
          END OF lv_marc.

  TYPES : BEGIN OF lv_mara,
            matnr TYPE matnr,
            maktx TYPE maktx,
            mtart TYPE mtart,
            matkl TYPE matkl,
            spart TYPE spart,
          END OF lv_mara.

  DATA : it_marc TYPE TABLE OF lv_marc.
  DATA : ls_marc TYPE lv_marc.

  DATA : it_mara TYPE TABLE OF lv_mara.
  DATA : ls_mara TYPE lv_mara.

  DATA : ls_output TYPE Zstr_MAT_PLANT.

*------------fetching data from MARC---------*
  SELECT werks matnr
    FROM marc
    INTO TABLE it_marc
    WHERE werks EQ plant.

*----------fetching data from MARA and MAKT----------*
  IF it_marc IS NOT INITIAL.

    SELECT a~matnr b~maktx a~mtart a~matkl a~spart
      INTO TABLE it_mara
      FROM mara AS a LEFT OUTER JOIN makt AS b
      ON a~matnr = b~matnr
      FOR ALL ENTRIES IN it_marc
      WHERE a~matnr = it_marc-matnr.

    IF sy-subrc = 0.
      SORT : it_mara BY matnr.
    ENDIF.

  ENDIF.
*-------------------------------------------*
  LOOP AT it_marc INTO ls_marc.
    ls_output-plant = ls_marc-werks.
    ls_output-mat_no = ls_marc-matnr.

    CLEAR : ls_marA.
    READ TABLE it_mara INTO ls_mara WITH KEY matnr = ls_marc-matnr BINARY SEARCH.
    IF sy-subrc = 0.
      ls_output-mAT_DES = ls_mara-maktx.
      ls_output-mAT_TYPE = ls_mara-mtart.
      ls_output-mat_GRP = ls_mara-matkl.
      ls_output-division = ls_mara-spart.
    ENDIF.

    APPEND ls_output TO gt_output.
    CLEAR : ls_output.
  ENDLOOP.




ENDFUNCTION.
