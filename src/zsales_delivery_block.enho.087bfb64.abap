"Name: \PR:SAPMV50A\FO:USEREXIT_MOVE_FIELD_TO_LIKP\SE:END\EI
ENHANCEMENT 0 ZSALES_DELIVERY_BLOCK.
*
  if sy-TCODE eq 'VL01N' .
    DATA : LV_KUNNR TYPE VBAK-KUNNR .
    DATA : LV_FLAG TYPE ZSD_CREDIT-BLOCK .
    SELECT SINGLE KUNNR INTO LV_KUNNR FROM VBAK WHERE VBELN EQ CVBUP-VBELN .
*    SELECT SINGLE BLOCK INTO LV_FLAG FROM ZSD_CREDIT WHERE KUNNR = LV_KUNNR .  "Commented by SPLABAP during code remediation
    SELECT BLOCK INTO LV_FLAG FROM ZSD_CREDIT UP TO 1 ROWS WHERE KUNNR = LV_KUNNR ORDER BY PRIMARY KEY.   "Added by SPLABAP during code remediation
      ENDSELECT.
      IF LV_fLAG IS NOT INITIAL.
       IF LV_FLAG EQ 'X'.
         MESSAGE 'Margin Lock Exists' TYPE 'E' DISPLAY LIKE 'E' .
        ENDIF.
ENDIF.
    ENDIF.

ENDENHANCEMENT.
