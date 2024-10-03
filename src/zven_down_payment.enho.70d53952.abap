"Name: \PR:SAPMF05A\FO:OPEN_FI_PERFORM_1140_E_CHECK\SE:END\EI
ENHANCEMENT 0 ZVEN_DOWN_PAYMENT.

         TYPES : BEGIN OF IS_BSIK,
           BUKRS TYPE BSIK-BUKRS,
           LIFNR TYPE BSIK-LIFNR,
           ZFBDT TYPE BSIK-ZFBDT,
         END OF IS_BSIK.

 DATA : IT_BSIK TYPE TABLE OF IS_BSIK,
        WA_BSIK TYPE IS_BSIK.

 DATA: IT_VEND_F47 TYPE TABLE OF ZVEND_F47,
      WA_VEND_F47 TYPE ZVEND_F47.

 DATA : FLAG TYPE ZVEND_F47-RE_FLAG.

DATA : LV_DATE TYPE BSIK-ZFBDT.

 LV_DATE  = SY-DATUM - 15 .
 SELECT SINGLE RE_FLAG INTO FLAG FROM ZVEND_F47 WHERE LIFNR = RF05A-NEWKO .
      WA_VEND_F47-LIFNR = RF05A-NEWKO.
      WA_VEND_F47-RE_FLAG = 'F'.
      WA_VEND_F47-RE_DATE = SY-DATUM .
    IF FLAG IS INITIAL.
        INSERT INTO ZVEND_F47 VALUES WA_VEND_F47 .
        COMMIT WORK.
    ENDIF.

"IF FLAG IS NOT INITIAL.
IF FLAG <> 'T' .

" IF FLAG IS NOT INITIAL AND FLAG = 'F' .

   IF SY-TCODE EQ 'FBA6' .
     IF RF05A-NEWKO IS NOT INITIAL.
     SELECT BUKRS LIFNR ZFBDT INTO TABLE IT_BSIK FROM BSIK WHERE BUKRS = XBKPF-BUKRS
                                                                 AND AUGBL =  ' '
                                                                 AND LIFNR = RF05A-NEWKO
                                                                 AND UMSKZ = 'A'
                                                                 AND SHKZG <> 'H' ORDER BY PRIMARY KEY.   "Added by SPLABAP during code remediation
    ENDIF.
    IF IT_BSIK IS NOT INITIAL.
    LOOP AT IT_BSIK INTO WA_BSIK .
     IF WA_BSIK-ZFBDT < LV_DATE .
        " CONCATENATE 'Vendor By' MS_LIMIT 'Rs And' MS_DAYS 'Days' INTO MSG SEPARATED BY SPACE.
       "  MESSAGE MSG TYPE 'E' DISPLAY LIKE 'E'.
         MESSAGE 'Unable to Process the payment due to Old Advance pending' TYPE 'E' DISPLAY LIKE 'E'.
         LEAVE TO CURRENT TRANSACTION.
     ENDIF.
    ENDLOOP.
    ENDIF.
 ENDIF.
"ENDIF.
"ENDIF.
ENDIF.

DATA : LV_LIFNR TYPE LFA1-LIFNR.

LV_LIFNR = RF05A-NEWKO.

IF SY-TCODE EQ 'FBA6' AND SY-UCOMM = 'BU'..


*     SELECT BUKRS LIFNR ZFBDT INTO TABLE IT_BSIK FROM BSIK WHERE BUKRS = XBKPF-BUKRS
*                                                                 AND AUGBL =  ' '
*                                                                 AND LIFNR = RF05A-NEWKO
*                                                                 AND ZUMSK = 'A' .
*    ENDIF.

     "SHIFT LV_LIFNR LEFT DELETING LEADING '0' .
     UPDATE ZVEND_F47 SET RE_FLAG = 'F' WHERE LIFNR = LV_LIFNR     .
     commit work.

*    LOOP AT IT_BSIK INTO WA_BSIK .
*     IF WA_BSIK-ZFBDT < LV_DATE .
*        " CONCATENATE 'Vendor By' MS_LIMIT 'Rs And' MS_DAYS 'Days' INTO MSG SEPARATED BY SPACE.
*       "  MESSAGE MSG TYPE 'E' DISPLAY LIKE 'E'.
*         MESSAGE 'Unable to Process the payment due to Old Advance pending' TYPE 'E' DISPLAY LIKE 'E'.
*         LEAVE TO CURRENT TRANSACTION.
*     ENDIF.
*    ENDLOOP.

 "ENDIF.
ENDIF.
ENDENHANCEMENT.
