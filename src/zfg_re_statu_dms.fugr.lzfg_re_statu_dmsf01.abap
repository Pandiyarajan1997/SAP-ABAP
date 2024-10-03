*----------------------------------------------------------------------*
***INCLUDE LZFG_RE_STATU_DMSF01.
*----------------------------------------------------------------------*

FORM new_entry.
  SELECT SINGLE * FROM kna1 INTO @DATA(ls_kna1) WHERE kunnr = @zsd_re_statu_dms-distributor
                                                AND   werks NE ' '.
  IF sy-subrc NE 0.
    MESSAGE : 'Distributor is not found' TYPE 'E'.
  ENDIF.
ENDFORM.
