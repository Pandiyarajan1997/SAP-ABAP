"Name: \PR:SAPMV45A\FO:USEREXIT_PRICING_PREPARE_TKOMP\SE:BEGIN\EI
ENHANCEMENT 0 ZSIVA_CONDITION.
"ADDED ON 21/11
  DATA : LV_ZEIAR TYPE MARA-ZEIAR .
  DATA : LV_ZTERM TYPE MARA-ZTERM.
IF SY-TCODE EQ 'VA01' OR SY-TCODE EQ 'VA02' .
      IF vbak-auart eq 'YBBR'.
    SELECT SINGLE ZEIAR INTO LV_ZEIAR FROM MARA WHERE MATNR = VBAP-MATNR .
          IF LV_ZEIAR EQ 'JNP' .
                 IF ( VBKD-ZTERM NE 'FP' AND VBKD-ZTERM NE 'APC' ) .
                       MESSAGE 'Selected Payment Term Applicable for JNPL Products only' TYPE 'E'.
                 ENDIF.
     "     ENDIF.
*Commented by Samsudeen M on 08.06.2023 for disabling this Check and error
              ELSE.
            "IF LV_ZEIAR NE 'JNP' .
                   SELECT SINGLE ZTERM INTO LV_ZTERM FROM MARA WHERE MATNR = VBAP-MATNR.
                     IF VBKD-ZTERM NE LV_ZTERM .
                       MESSAGE 'Selected Payment Term is not Applicable for the Products entered' TYPE 'E'.
             "        ENDIF.
            ENDIF.
            ENDIF.
      ENDIF.
ENDIF.
"ENDED ON 21/11

*DATA : LV_PLANT TYPE MARA-ZEIAR .
*CLEAR: LV_PLANT.
*
*IF SY-TCODE EQ 'VA01' OR SY-TCODE EQ 'VA02' .
*  IF vbak-auart <> 'YBRE'.
*
*IF VBKD-ZTERM EQ 'FP' OR VBKD-ZTERM EQ 'APC' .
*  SELECT SINGLE ZEIAR INTO LV_PLANT FROM MARA WHERE MATNR = VBAP-MATNR .
*   IF LV_PLANT NE 'JNP' .
*    MESSAGE 'Selected Payment Term Applicable for JNPL Products only' TYPE 'E'.
*      ENDIF.
*ELSE.
*  SELECT SINGLE ZEIAR INTO LV_PLANT FROM MARA WHERE MATNR = VBAP-MATNR .
*     IF LV_PLANT EQ 'JNP' .
*     MESSAGE 'Selected Payment Term Not Applicable for JNPL Products' TYPE 'E'.
*      ENDIF.
*    ENDIF.
*  ENDIF.
*
*ENDIF.
*
*DATA : LV_KNUMH TYPE A952-KNUMH .
*DATA : LV_KO TYPE KONP-LOEVM_KO.
*IF SY-TCODE EQ 'VA01' OR SY-TCODE EQ 'VA02' .
*IF VBKD-ZTERM EQ 'APC' and vbak-vkorg = '1000' AND vbak-auart <> 'YBRE'.
*  SELECT SINGLE ZEIAR INTO LV_PLANT FROM MARA WHERE MATNR = VBAP-MATNR .
*    IF LV_PLANT EQ 'JNP' .
*      SELECT SINGLE KNUMH INTO LV_KNUMH FROM A952 WHERE KUNNR = VBAK-KUNNR AND VKORG = VBAK-VKORG .
*      SELECT SINGLE LOEVM_KO INTO LV_KO FROM KONP WHERE KNUMH = LV_KNUMH.
* IF LV_KNUMH IS INITIAL OR LV_KO EQ 'X' .
*    MESSAGE 'Selected Payment Term is not applicable this Customer' TYPE 'E'.
*ENDIF.
*ENDIF.
*      ENDIF.
*
*ENDIF.

"ended on 31/05/19
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  if sy-tcode = 'VA01' or sy-tcode = 'VA02' .

        if vbak-vkorg = '1000' AND vbak-auart = 'YBBR' AND vbkd-zterm <> 'FP'. "and vbak-auart = 'YBBR'.
   LOOP AT XKOMV WHERE KSCHL = 'SKTO'.

   delete xkomv.
        ENDLOOP.
        ENDIF.
                if vbak-vkorg = '1000' AND vbak-auart = 'YBBR' AND vbkd-zterm = 'FP'. "and vbak-auart = 'YBBR'.
   LOOP AT XKOMV WHERE KSCHL = 'ZCAD'.

   delete xkomv.
        ENDLOOP.
        ENDIF.
"ADDED ON 24/10
endif.

IF SY-TCODE EQ 'VA01' OR SY-TCODE EQ 'VA02' .
  IF vbak-auart EQ  'YBBP'.
*
 LOOP AT XKOMV WHERE KSCHL = 'PRQT'.
*
  LOOP AT SCREEN.
 " IF SCREEN-NAME = 'TAXI_TABSTRIP_CAPTIONS-TAB32' .
 if SCREEN-GROUP2 = 'LOO' . "AND SCREEN-GROUP3 = '014' .
        SCREEN-INPUT = '0' .
      MODIFY SCREEN.
      ENDIF.
   ENDLOOP.

ENDLOOP.
ENDIF.
ENDIF.

DATA : MSG(400).
IF SY-TCODE EQ 'VA01' OR SY-TCODE EQ 'VA02' .
  IF VBAK-AUART NE 'YBRE' .
   AUTHORITY-CHECK OBJECT 'ZCUGR'
   ID 'KDGRP' FIELD TKOMK-KDGRP.
     IF SY-SUBRC NE 0.
      CONCATENATE 'No Create Authorization for Customer Group - ' TKOMK-KDGRP into MSG SEPARATED BY SPACE.
      MESSAGE MSG TYPE 'E'.
       "MESSAGE E077(S#) WITH 'Customer Group'.
      ENDIF.
      ENDIF.
ENDIF.

ENDENHANCEMENT.
