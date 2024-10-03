*----------------------------------------------------------------------*
***INCLUDE LZFG_USERIDF01.
*----------------------------------------------------------------------*
FORM username.
  DATA: ls_struc TYPE qisrsuser_data.
  IF zmm_userid-ernam IS NOT INITIAL.
    CLEAR ls_struc.
    DATA(lv_userid) = CONV syuname( zmm_userid-ernam ).
    CALL FUNCTION 'ISR_GET_USER_DETAILS'
      EXPORTING
        id_user_id     = lv_userid
      CHANGING
        is_user_data   = ls_struc
      EXCEPTIONS
        user_not_found = 1
        OTHERS         = 2.
    IF sy-subrc = 0.
      zmm_userid-username = ls_struc-fullname.
    ENDIF.
  ENDIF.
ENDFORM.
