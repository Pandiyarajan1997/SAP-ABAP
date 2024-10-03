"Name: \PR:SAPLCOWB\FO:DO_INIT\SE:END\EI
ENHANCEMENT 0 ZMM_MAT_DESC_HIDE_COR6N.
*Added by: Samsudeen M
*Added on: 11.02.2023
*Purpose: Enhancemenmt for hiding Material Description
         "Based on Userid
DATA lv_userid TYPE sy-uname.
SELECT SINGLE ernam FROM zmm_userid INTO lv_userid WHERE ernam = sy-uname.
  IF sy-subrc <> 0.
  LOOP AT p_comp_tab ASSIGNING FIELD-SYMBOL(<fs_comptab>).
  CLEAR <fs_comptab>-maktx.
  ENDLOOP.
  LOOP AT gt_comp ASSIGNING FIELD-SYMBOL(<fgs_comp>).
  CLEAR <fgs_comp>-maktx.
  ENDLOOP.
  ENDIF.
ENDENHANCEMENT.
