*----------------------------------------------------------------------*
***INCLUDE LZSD_GST_REGF01.
*----------------------------------------------------------------------*
FORM create_entry.
  IF zsd_gst_reg-region IS NOT INITIAL.
    SELECT SINGLE bezei FROM t005u INTO zsd_gst_reg-descr WHERE spras = sy-langu
                                   AND land1 = 'IN'
                                   AND bland = zsd_gst_reg-region.
    IF sy-subrc EQ 0.

    ENDIF.
  ENDIF.
ENDFORM.
