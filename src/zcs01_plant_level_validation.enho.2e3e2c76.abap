"Name: \FU:CSBD_START_SCREEN_CHECK\SE:BEGIN\EI
ENHANCEMENT 0 ZCS01_PLANT_LEVEL_VALIDATION.
if SY-TCODE = 'CS01'.
      if I_RC29N-werks = '1401' OR I_RC29N-werks = '1014'.
        MESSAGE 'Only Group BOM allowed in Plant 1401 and 1014 ' TYPE 'E'.
    ENDIF.
ENDIF.

ENDENHANCEMENT.
