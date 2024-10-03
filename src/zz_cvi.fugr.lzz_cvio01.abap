*----------------------------------------------------------------------*
***INCLUDE LZZ_CVIO01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module PBO OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE pbo OUTPUT.
  PERFORM pbo.
ENDMODULE.
FORM pbo.
  CALL FUNCTION 'BUS_PBO'.
ENDFORM.
MODULE pai INPUT.
  PERFORM pai.
ENDMODULE.
FORM pai.
  CALL FUNCTION 'BUS_PAI'.
ENDFORM.
MODULE validation INPUT.
  IF gs_kna1-zcapacity IS NOT INITIAL.
    SELECT SINGLE * FROM zsd_cus_ton INTO @DATA(ls_kna1) WHERE zcapacity = @gs_kna1-zcapacity.
    IF sy-subrc NE 0.
      MESSAGE 'Enter from search help provided' TYPE 'E'.
    ELSE.
      zsd_cus_ton-description = ls_kna1-description.
    ENDIF.
  ENDIF.
ENDMODULE.
MODULE validation1 INPUT.
  IF gs_kna1-zkostl IS NOT INITIAL.
    SELECT SINGLE kostl FROM csks INTO @DATA(lv_costcenter) WHERE kostl = @gs_kna1-zkostl.
    IF sy-subrc NE 0.
      MESSAGE 'Costcenter Not available in SAP' TYPE 'E'.
    ENDIF.
  ENDIF.
ENDMODULE.
MODULE fill_data OUTPUT.
  IF gs_kna1-zcapacity IS NOT INITIAL.
    SELECT SINGLE * FROM zsd_cus_ton INTO @DATA(lw_kna1) WHERE zcapacity = @gs_kna1-zcapacity.
    IF sy-subrc EQ 0.
      zsd_cus_ton-description = lw_kna1-description.
    ENDIF.

  ELSE.

    CLEAR zsd_cus_ton-description.

  endif.

ENDMODULE.
