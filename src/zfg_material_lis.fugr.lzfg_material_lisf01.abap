*----------------------------------------------------------------------*
***INCLUDE LZFG_MATERIAL_LISF01.
*----------------------------------------------------------------------*
FORM new_entry.

  IF zmm_mat_list-werks IS INITIAL.
  MESSAGE 'plant is Mandatory' TYPE 'E'.
  ENDIF.

  IF zmm_mat_list-matnr IS NOT INITIAL.
    SELECT SINGLE matnr FROM mara INTO @DATA(lv_matnr) WHERE matnr = @zmm_mat_list-matnr
                                                       AND mtart IN ( 'ROH' , 'VERP' ).
    IF sy-subrc = 0.
      SELECT SINGLE maktx FROM makt INTO @DATA(lv_desc) WHERE matnr = @zmm_mat_list-matnr.
      IF sy-subrc = 0.
        zmm_mat_list-maktx = lv_desc.
      ENDIF.
    ELSE.
      MESSAGE 'Please verify material and Material type' TYPE 'E'.
    ENDIF.
  ENDIF.
ENDFORM.
