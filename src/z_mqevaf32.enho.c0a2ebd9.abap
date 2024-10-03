"Name: \PR:SAPMQEVA\FO:PRUEFEN_GESAMTMENGE\SE:END\EI
ENHANCEMENT 0 Z_MQEVAF32.
*Recuirment given by Umapathy , created by Govind - Inspection
  IF SY-TCODE EQ 'QA11' OR SY-TCODE EQ 'QA12'.
IF    L_ZUBUCHOFF LT QALS-GESSTICHPR
        AND L_KZ_PAKO IS INITIAL.                          .
        CLEAR L_MENGE_CHAR.
        MOVE L_ZUBUCHOFF   TO L_MENGE_CHAR.
        Message 'Inspection still active enter all inspection area' type 'I' DISPLAY LIKE 'E'.

        LEAVE TO CURRENT TRANSACTION.
        ENDIF.
*        MESSAGE W214 WITH  L_MENGE_CHAR QALS-EINHPROBE.
      ENDIF.
ENDENHANCEMENT.
