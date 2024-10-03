"Name: \PR:SAPLCOMD\FO:_O_SET_TXT_KZ\SE:END\EI
ENHANCEMENT 0 ZMM_MAT_DESC_HIDE_CORIX.
*
 DATA: lv_userid TYPE ernam.
 CLEAR lv_userid.
 SELECT SINGLE ernam FROM zmm_userid INTO lv_userid WHERE ernam = sy-uname.
  IF sy-subrc <> 0.
  CLEAR resbd-ltext.
  CLEAR resbd-matxt.
  ENDIF.
ENDENHANCEMENT.
