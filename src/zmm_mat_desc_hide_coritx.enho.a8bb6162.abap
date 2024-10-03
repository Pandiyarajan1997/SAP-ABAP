"Name: \PR:SAPLCOMK\FO:_O_MODIFY_FIELDS\SE:END\EI
ENHANCEMENT 0 ZMM_MAT_DESC_HIDE_CORITX.
*Created by: Samsudeen M
*Added on: 10.02.2023
*Purpose : Hiding Material Description in the components material list
DATA: lv_userid TYPE ernam.
SELECT SINGLE ernam FROM zmm_userid INTO lv_userid WHERE ernam = sy-uname.
IF sy-subrc <> 0.
LOOP AT komp_int.
CLEAR komp_int-matxt.
MODIFY komp_int.
ENDLOOP.
CLEAR resbd-matxt.
ENDIF.
ENDENHANCEMENT.
