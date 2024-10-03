*----------------------------------------------------------------------*
***INCLUDE LZMM_SERVICE_POF03.
*----------------------------------------------------------------------*
FORM create_newentry1.
  IF zmm_po_ven_tax-vendor IS NOT INITIAL.
    SELECT SINGLE lifnr FROM lfa1 INTO @DATA(lv_lifnr) WHERE lifnr = @zmm_po_ven_tax-vendor.
    IF sy-subrc NE 0.
      MESSAGE TEXT-003 TYPE 'E'.
    ENDIF.
  ENDIF.
ENDFORM.
