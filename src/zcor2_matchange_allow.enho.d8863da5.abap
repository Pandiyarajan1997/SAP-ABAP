"Name: \PR:SAPLCOMD\FO:_O_XCURSOR_FIELD_INIT\SE:END\EI
ENHANCEMENT 0 ZCOR2_MATCHANGE_ALLOW.
*
  IF sy-tcode = 'COR1' or SY-tcode = 'COR2'.
     SELECT SINGLE * from ZCOR2_MATCHNG INTO @DATA(ls_zcor2) WHERE werks = @resbd-werks
                                                               AND ZUSER = @sy-uname .
      IF sy-subrc ne 0 .
        LOOP AT SCREEN.
          screen-input     = '0'.
*          screen-active    = '0'.
*          screen-invisible = '1'.
*          screen-required  = '0'.
*          screen-output    = '0'.
          MODIFY SCREEN.
        ENDLOOP.
      ENDIF.
  ENDIF.

ENDENHANCEMENT.
