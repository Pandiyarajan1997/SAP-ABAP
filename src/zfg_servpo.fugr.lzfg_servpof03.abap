*----------------------------------------------------------------------*
***INCLUDE LZFG_SERVPOF03.
*----------------------------------------------------------------------*
FORM new_entry.
  IF zmm_regven_txbp-vendor IS NOT INITIAL.
    SELECT SINGLE lifnr FROM lfa1 INTO @DATA(l_vendor)
      WHERE lifnr = @zmm_regven_txbp-vendor.
    IF sy-subrc NE 0.
      MESSAGE 'Incorrect Vendor Number' TYPE'E'.
    ENDIF.
    zmm_regven_txbp-ernam = sy-uname.
    zmm_regven_txbp-erdat = sy-datum.
  ENDIF.
ENDFORM.
