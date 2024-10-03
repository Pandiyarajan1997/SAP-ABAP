"Name: \PR:SAPMV45A\FO:USEREXIT_SAVE_DOCUMENT_PREPARE\SE:BEGIN\EI
ENHANCEMENT 0 ZSD_CREDIT_CHECK.

IF SY-TCODE EQ 'VA01' OR SY-TCODE EQ 'VA02' .

  IF vbak-auart EQ 'YBBR' .

IF VBAK-CMGST eq 'B'.

  exit.

  else.

types: BEGIN OF TY_BSID,
        MANDT TYPE BSID-MANDT,
        BUKRS TYPE BSID-BUKRS,
        KUNNR TYPE BSID-KUNNR,
        BELNR TYPE BSID-BELNR,
        SHKZG TYPE BSID-SHKZG,
        ZFBDT TYPE BSID-ZFBDT,
      END OF TY_BSID.


DATA: GT_VBUK TYPE TABLE OF VBAK,
      GS_VBUK TYPE VBAK,
      GT_BSID TYPE TABLE OF TY_BSID,
      GS_BSID TYPE TY_BSID,
      LV_RDATE TYPE RFPOSX-VERZN,
      LV_FLAG.





IF vbak-vkorg EQ '1000' .

      SELECT MANDT
             BUKRS
             KUNNR
             BELNR
             SHKZG
             ZFBDT
             FROM BSID CLIENT SPECIFIED
             INTO TABLE GT_BSID
             WHERE MANDT EQ SY-MANDT AND
           ( BUKRS EQ '2000' OR BUKRS EQ '4000' ) AND
            KUNNR EQ VBAK-KUNNR and
            SHKZG eq 'S'.


  ELSEIF vbak-vkorg EQ '2000' .
      SELECT MANDT
             BUKRS
             KUNNR
             BELNR
             SHKZG
             ZFBDT
             FROM BSID CLIENT SPECIFIED
             INTO TABLE GT_BSID
             WHERE MANDT EQ SY-MANDT AND
           ( BUKRS EQ '1000' OR BUKRS EQ '4000' ) AND
            KUNNR EQ VBAK-KUNNR and
            SHKZG eq 'S'.

*
  ELSEIF vbak-vkorg EQ '4000' .
      SELECT MANDT
             BUKRS
             KUNNR
             BELNR
             SHKZG
             ZFBDT
             FROM BSID CLIENT SPECIFIED
             INTO TABLE GT_BSID
             WHERE MANDT EQ SY-MANDT AND
           ( BUKRS EQ '1000' OR BUKRS EQ '2000' ) AND
            KUNNR EQ VBAK-KUNNR and
            SHKZG eq 'S'.

   ENDIF.



      IF GT_BSID IS NOT INITIAL.

        LOOP AT GT_BSID INTO GS_BSID.



        CALL FUNCTION 'ITEM_OVERDUE_DAYS'
          EXPORTING
* *          KEY_DATE          = SY-DATLO
*           PAY_DATE          =
            DUE_DATE          = GS_BSID-ZFBDT
*           CLEAR_DATE        =
          IMPORTING
*           OVER_SKONTO1_DAYS =
            OVERDUE_DAYS      = LV_RDATE.

        IF LV_RDATE GT 0 OR VBAK-CMGST IS INITIAL or VBAK-CMGST eq 'A'.

          LV_FLAG = 'X'.

          SET PARAMETER ID 'MEM' FIELD LV_FLAG.


        ENDIF.


      ENDLOOP.


      ENDIF.


ENDIF.
ENDIF.



ENDIF.



ENDENHANCEMENT.
