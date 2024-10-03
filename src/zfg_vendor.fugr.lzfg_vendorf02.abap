*----------------------------------------------------------------------*
***INCLUDE LZFG_VENDORF02.
*----------------------------------------------------------------------*
FORM create_entry.
  IF zta_emp_pymtblk-pernr IS NOT INITIAL.
    SELECT SINGLE * FROM pa0001 INTO @DATA(l_pa0001)
      WHERE pernr = @zta_emp_pymtblk-pernr
      AND begda LE @sy-datum
      AND endda GE @sy-datum.
    IF sy-subrc = 0.
      zta_emp_pymtblk-sname = l_pa0001-ename.
    ELSE.
      MESSAGE 'Incorrect employee Number' TYPE 'E'.
    ENDIF.
  ENDIF.
  zta_emp_pymtblk-flag = abap_true.
  zta_emp_pymtblk-erdat = sy-datum.
  zta_emp_pymtblk-ernam = sy-uname.

ENDFORM.
