"Name: \PR:SAPLCOKO\FO:CHECK_MATERIAL_PLANT\SE:END\EI
ENHANCEMENT 0 ZCOR1_MATERIAL_LOCK.

IF sy-TCODE = 'COR1' .

  IF NOT mtcor-lvorm IS INITIAL.
    MESSAGE E732 WITH caufvd-matnr caufvd-werks.
  ENDIF.

ENDIF.


ENDENHANCEMENT.
