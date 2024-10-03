*----------------------------------------------------------------------*
***INCLUDE LZPAINTER_SCHEMEF01.
*----------------------------------------------------------------------*
FORM check_for_date.
  IF zpainter_master-from_per IS NOT INITIAL AND zpainter_master-to_per IS NOT INITIAL.
    IF zpainter_master-from_per GT zpainter_master-to_per.
      MESSAGE 'Please enter gretaer validity to period' TYPE 'E'.
    ENDIF.
  ENDIF.
ENDFORM.
