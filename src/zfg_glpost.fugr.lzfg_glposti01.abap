*----------------------------------------------------------------------*
***INCLUDE LZFG_GLPOSTI01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  VALUE_CHECK  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE value_check INPUT.
  IF zfi_glacc_master-fgl IS NOT INITIAL.
    DATA(lv_gl) = |{ zfi_glacc_master-fgl ALPHA = IN }|.
    SELECT SINGLE saknr FROM skat INTO @DATA(gl_acc)
      WHERE saknr = @lv_gl AND ktopl = 'YAIN'.
    IF sy-subrc NE 0.
      MESSAGE 'Incorrect from Glacc Number' TYPE 'E'.
    ENDIF.
  ENDIF.
  IF zfi_glacc_master-tgl IS NOT INITIAL.
    lv_gl = |{ zfi_glacc_master-tgl ALPHA = IN }|.
    SELECT SINGLE saknr FROM skat INTO gl_acc
      WHERE saknr = lv_gl AND ktopl = 'YAIN'.
    IF sy-subrc NE 0.
      MESSAGE 'Incorrect To Glacc Number' TYPE 'E'.
    ENDIF.
  ENDIF.
ENDMODULE.
