*----------------------------------------------------------------------*
***INCLUDE LZZ_CVI1O01.
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
*&---------------------------------------------------------------------*
*&      Module  PAI  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai INPUT.

  PERFORM pai.

ENDMODULE.
FORM pai.

  CALL FUNCTION 'BUS_PAI'.

ENDFORM.
MODULE validation1 INPUT.
  IF gs_kna1-zcustype IS INITIAL.
    CLEAR zfi_custtype-stext.
    IF gs_kna1-zfincode IS NOT INITIAL.
      MESSAGE 'Customer Type is Mandatory' TYPE 'E'.
    ENDIF.
  ELSE.
    SELECT SINGLE * FROM zfi_custtype INTO @DATA(ls_custype) WHERE zcustype = @gs_kna1-zcustype.
    IF sy-subrc NE 0.
      MESSAGE 'Enter from search help provided' TYPE 'E'.
    ELSE.
      zfi_custtype-stext = ls_custype-stext.
      IF gs_kna1-zfincode IS INITIAL.
        MESSAGE 'Borrower Code is Mandatory' TYPE 'E'.
      ENDIF.
    ENDIF.
  ENDIF.
ENDMODULE.
MODULE validation2 INPUT.
  IF gs_kna1-zfincode IS NOT INITIAL..
    SELECT SINGLE * FROM kna1 INTO @DATA(ls_kna1)
     WHERE zfincode = @gs_kna1-zfincode
     AND kunnr NE @gs_kna1-kunnr.
    IF sy-subrc EQ 0.
      MESSAGE 'Already this borrower Code is Assigned' TYPE 'E'.
    ENDIF.
  ENDIF.
ENDMODULE.
MODULE fill_data OUTPUT.
  IF gs_kna1-zcustype IS NOT INITIAL.
    SELECT SINGLE * FROM zfi_custtype INTO @DATA(lw_custype) WHERE zcustype = @gs_kna1-zcustype.
    IF sy-subrc EQ 0.
      zfi_custtype-stext = lw_custype-stext.
    ENDIF.
  ENDIF.
ENDMODULE.
