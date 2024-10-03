*----------------------------------------------------------------------*
***INCLUDE LZSD_INV_CUSTOMERF03.
*----------------------------------------------------------------------*
FORM  f_new_enrty.
  zsd_inv_customer-ctreated_by = sy-uname.
  zsd_inv_customer-ctreated = sy-datum.
ENDFORM.
