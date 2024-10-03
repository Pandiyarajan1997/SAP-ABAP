"Name: \PR:SAPLMGMM\FO:MAKT_GET_SUB\SE:END\EI
ENHANCEMENT 0 ZMM_MATNR_DES_HIDE.
*
AUTHORITY-CHECK OBJECT 'ZMAT_DESC' ID 'DISPLAY' FIELD '01'.
IF sy-subrc <> 0.
CLEAR: makt-maktx,makt-maktg.
CLEAR:lmakt-maktx,lmakt-maktg.

LOOP AT ktext ASSIGNING FIELD-SYMBOL(<fs_ktext>).
CLEAR <fs_ktext>-maktx.
ENDLOOP.
LOOP AT dktext ASSIGNING FIELD-SYMBOL(<fs_dktext>).
CLEAR <fs_dktext>-maktx.
ENDLOOP.
LOOP AT lktext ASSIGNING FIELD-SYMBOL(<fs_lktext>).
CLEAR <fs_lktext>-maktx.
ENDLOOP.
ENDIF.
ENDENHANCEMENT.
