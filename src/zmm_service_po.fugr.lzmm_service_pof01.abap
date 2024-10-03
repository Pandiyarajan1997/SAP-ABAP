*----------------------------------------------------------------------*
***INCLUDE LZMM_SERVICE_POF01.
*----------------------------------------------------------------------*
FORM create_newentry.
  IF zmm_po_plant_cc-zwerks IS NOT INITIAL.
    SELECT SINGLE werks FROM t001w INTO @DATA(lv_werks) WHERE werks = @zmm_po_plant_cc-zwerks.
    IF sy-subrc NE 0.
      MESSAGE TEXT-001 TYPE 'E'.
    ENDIF.
  ENDIF.
  IF zmm_po_plant_cc-zkostl IS NOT INITIAL.
    SELECT SINGLE kostl FROM csks INTO @DATA(lv_cc) WHERE kostl = @zmm_po_plant_cc-zkostl.
    IF sy-subrc NE 0.
      MESSAGE TEXT-002 TYPE 'E'.
    ENDIF.
  ENDIF.
ENDFORM.
