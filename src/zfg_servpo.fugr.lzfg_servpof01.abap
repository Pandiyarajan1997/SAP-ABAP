*----------------------------------------------------------------------*
***INCLUDE LZFG_SERVPOF01.
*----------------------------------------------------------------------*
FORM create_entry.
  IF zmm_serpo_tax_bp-from_region IS NOT INITIAL
        AND zmm_serpo_tax_bp-taxcode IS NOT INITIAL AND zmm_serpo_tax_bp-bplace IS NOT INITIAL.
    zmm_serpo_tax_bp-erdat = sy-datum.
    zmm_serpo_tax_bp-ernam = sy-uname.
  ENDIF.
ENDFORM.
