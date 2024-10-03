*----------------------------------------------------------------------*
***INCLUDE LZFG_ATTRMAPF01.
*----------------------------------------------------------------------*
FORM newentries.
  zcust_attribute-valid_to = '99991231'.
  zcust_attribute-reference_date = sy-datum.
ENDFORM.
