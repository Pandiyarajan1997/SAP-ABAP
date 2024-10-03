*----------------------------------------------------------------------*
***INCLUDE MP901700_F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  P_TEXT_CHECK  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
DATA p_text TYPE text50.
DATA p_subtype TYPE sutext.
MODULE p_text_check INPUT.
  PERFORM f_text.
ENDMODULE.
FORM f_text.
  CHECK p9017-idno IS NOT INITIAL AND p9017-srno IS NOT INITIAL.
  p9017-subty = p9017-idno.
  SELECT SINGLE sutxt INTO p_subtype
    FROM t777u WHERE
      langu = sy-langu  AND
      infty = '9017'    AND
      subty = p9017-idno.

  SELECT SINGLE text
    FROM zhrp_pos_main INTO p_text
    WHERE idno = p9017-idno
      AND srno = p9017-srno.
  IF sy-subrc <> 0.
    MESSAGE 'IDNo entry is not found in ZHRP_POS_MAIN' TYPE 'S' DISPLAY LIKE 'E'.
    CLEAR ok-code.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Module P_TEXT_CHECK OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE p_text_check OUTPUT.
  PERFORM f_text.
* SET PF-STATUS 'xxxxxxxx'.
* SET TITLEBAR 'xxx'.
ENDMODULE.
