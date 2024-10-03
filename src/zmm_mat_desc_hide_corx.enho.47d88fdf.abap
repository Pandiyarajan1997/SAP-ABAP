"Name: \PR:SAPLCOKO\FO:SFE_PBO_ORDER_HEADER\SE:END\EI
ENHANCEMENT 0 ZMM_MAT_DESC_HIDE_CORX.
*Added by: Samsudeen M
*Added on: 10.02.2023
*Purpose : Hiding material Description for Process Order Screen
IF sy-tcode EQ 'COR1' OR
   sy-tcode EQ 'COR2' OR
   sy-tcode EQ 'COR3'.
  DATA: lv_userid TYPE sy-uname.
  CLEAR lv_userid.
  SELECT SINGLE ernam FROM zmm_userid INTO lv_userid WHERE ernam = sy-uname.
    IF sy-subrc <> 0.
    CLEAR resbd-matxt.
    CLEAR resbd-ltext.
    ENDIF.

ENDIF.
ENDENHANCEMENT.
