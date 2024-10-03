*----------------------------------------------------------------------*
***INCLUDE LZFG_POORD_CHKF01.
*----------------------------------------------------------------------*
"Changed By: Samsudeen M
"Changed On: 05.05.2023
"Purpose: New Entry at that date and person auto population
"Reference By: Ramakrishan J
*----------------------------------------------------------------------*
FORM new_entry.
  IF zpoord_chk_skip-aufnr IS NOT INITIAL.
* Process Order Number Checks
    SELECT SINGLE aufnr FROM aufk
      INTO @DATA(l_pro_order)
      WHERE aufnr = @zpoord_chk_skip-aufnr.
    IF sy-subrc NE 0.
      DATA(l_msg) = |Process Order Number is Incorrect|.
      MESSAGE l_msg TYPE 'E'.
    ELSE.
      zpoord_chk_skip-ernam = sy-uname.
      zpoord_chk_skip-ersda = sy-datum.
    ENDIF.
  ENDIF.
ENDFORM.
