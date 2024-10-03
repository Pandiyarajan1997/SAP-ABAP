*----------------------------------------------------------------------*
***INCLUDE LZFG_DMS_REGIONNF01.
*----------------------------------------------------------------------*
FORM region_descr.
  IF zdms_regio_map-from_reg IS NOT INITIAL.
    SELECT SINGLE * FROM t005u INTO @DATA(ls_from) WHERE land1 = 'IN'
                                                   AND   bland = @zdms_regio_map-from_reg.
    IF sy-subrc = 0.
      zdms_regio_map-from_desc = ls_from-bezei.
      CLEAR : ls_from.
      ELSE.
        MESSAGE : 'Incorrect from region' TYPE 'E'.
    ENDIF.
    SELECT SINGLE * FROM t005u INTO ls_from WHERE land1 = 'IN'
                                            AND   bland = zdms_regio_map-to_reg.
    IF sy-subrc = 0.
      zdms_regio_map-to_desc = ls_from-bezei.
      ELSE.
        MESSAGE : 'Incorrect To region' TYPE 'E'.
    ENDIF.
  ENDIF.
ENDFORM.
