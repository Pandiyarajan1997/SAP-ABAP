*----------------------------------------------------------------------*
***INCLUDE LZFG_CUSTOMER_DATO01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module CUSTOMER_NAME_DISP OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE customer_name_disp OUTPUT.
  IF zcus_excl_stmnt-kunnr IS NOT INITIAL.
    SELECT SINGLE name1 FROM kna1 INTO cusname
                        WHERE kunnr = zcus_excl_stmnt-kunnr.
    IF sy-subrc = 0.

    ENDIF.
  ENDIF.
ENDMODULE.
