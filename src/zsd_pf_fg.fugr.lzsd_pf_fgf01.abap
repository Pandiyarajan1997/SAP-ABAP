*----------------------------------------------------------------------*
***INCLUDE LZSD_PF_FGF01.
*----------------------------------------------------------------------*
FORM check_and_description.
  IF zsd_pf_old_new-old_pf IS NOT INITIAL.
    SELECT SINGLE * FROM tpart INTO @DATA(ls_old_pf) WHERE spras = @sy-langu AND parvw = @zsd_pf_old_new-old_pf.
    IF sy-subrc NE 0.
      MESSAGE TEXT-001 TYPE 'E'.
    ELSE.
      zsd_pf_old_new-old_des = ls_old_pf-vtext.
    ENDIF.
  ENDIF.
  IF zsd_pf_old_new-new_pf IS NOT INITIAL.
    SELECT SINGLE * FROM tpart INTO @DATA(ls_new_pf) WHERE spras = @sy-langu AND parvw = @zsd_pf_old_new-new_pf.
    IF sy-subrc NE 0.
      MESSAGE TEXT-002 TYPE 'E'.
    ELSE.
      zsd_pf_old_new-new_des = ls_new_pf-vtext.
    ENDIF.
  ENDIF.
ENDFORM.
