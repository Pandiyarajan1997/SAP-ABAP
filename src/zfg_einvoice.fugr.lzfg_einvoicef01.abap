*----------------------------------------------------------------------*
***INCLUDE LZFG_EINVOICEF01.
*----------------------------------------------------------------------*

FORM create .
  IF zdist_einv_dtls-act_date IS INITIAL.

    MESSAGE : 'Please Enter Activate Date' TYPE 'E'.

  ENDIF.
ENDFORM.
