*&---------------------------------------------------------------------*
*& Report ZVEND_AUTO_PAY
*&*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&Functional                   : Mr.Govindarajan M         *
*& Developer                   : Mr.Ramachandaran                      *
*& Company                     : Sheenlac Paints Pvt Ltd               *
*& Verified By                 :                                       *
*& Title                       : ZVendor Auto Payment        *
*& Report Name                 : ZVEND_AUTO_PAY                          *
*& Development Id              : kpabap                                *
*& Related Information         : Vendor Auto Payment Register       *
*&---------------------------------------------------------------------*
REPORT ZVENDOR_AUTO_PAYMENT1.

TYPES: BEGIN OF GS_REGUH,                                                                           " Table 1

         LAUFD TYPE REGUH-LAUFD,
         LAUFI TYPE REGUH-LAUFI,

         XVORL TYPE REGUH-XVORL,

         ZBUKR TYPE REGUH-ZBUKR,
         LIFNR TYPE REGUH-LIFNR,
         KUNNR TYPE REGUH-KUNNR,
         VBLNR TYPE REGUH-VBLNR ,
         WAERS TYPE REGUH-WAERS,
         NAME1 TYPE REGUH-NAME1,
         NAME2 TYPE REGUH-NAME2,
         NAME3 TYPE REGUH-NAME3 ,
         NAME4 TYPE REGUH-NAME4 ,
         PSTLZ TYPE REGUH-PSTLZ,
         ORT01 TYPE REGUH-ORT01,
         STRAS TYPE REGUH-STRAS,
         ZREGI TYPE REGUH-ZREGI ,
         ZBNKS TYPE REGUH-ZBNKS ,
         ZBNKN TYPE REGUH-ZBNKN ,
         ZBNKL TYPE REGUH-ZBNKL,                        "BANK NO
         ZBKON TYPE REGUH-ZBKON,                        "CONTROL KEY
         RZAWE TYPE REGUH-RZAWE,                        "PAYMENT METHOD
         HBKID TYPE REGUH-HBKID,                         " HOUSE BANK
         UBKNT TYPE REGUH-UBKNT,                        " OUR ACC NO
         RWBTR TYPE REGUH-RWBTR ,
         AUGDT TYPE REGUH-AUGDT ,

    END OF GS_REGUH.

DATA: GT_REGUH TYPE TABLE OF GS_REGUH,
      WA_REGUH TYPE GS_REGUH.

TYPES: BEGIN OF GS_BSAK,                                                                           " Table 1
         BUKRS TYPE BSAK-BUKRS,
         LIFNR TYPE BSAK-LIFNR,
         AUGBL TYPE BSAK-AUGBL ,
         BELNR TYPE BSAK-BELNR ,
         XBLNR TYPE BSAK-XBLNR  ,

         BLART TYPE BSAK-BLART,

      END OF GS_BSAK .

DATA: GT_BSAK TYPE TABLE OF GS_BSAK,
      WA_BSAK TYPE GS_BSAK.

DATA: GT_BSAK1 TYPE TABLE OF GS_BSAK,
      WA_BSAK1 TYPE GS_BSAK.

TYPES : BEGIN OF GS_LFA1 ,
          LIFNR TYPE LFA1-LIFNR ,
          ADRNR TYPE LFA1-ADRNR ,
        END OF GS_LFA1 .

DATA: GT_LFA1 TYPE TABLE OF GS_LFA1,
      WA_LFA1 TYPE GS_LFA1.

TYPES : BEGIN OF GS_LFB1 ,              "ADDED BY RAM ON 29/8/15
          LIFNR TYPE LFB1-LIFNR ,
          ZWELS TYPE LFB1-ZWELS ,
        END OF GS_LFB1 .

DATA: GT_LFB1 TYPE TABLE OF GS_LFB1,
      WA_LFB1 TYPE GS_LFB1.

TYPES : BEGIN OF GS_ADR6 ,
          ADDRNUMBER TYPE ADR6-ADDRNUMBER ,
          SMTP_ADDR TYPE ADR6-SMTP_ADDR ,
        END OF GS_ADR6 .

DATA: GT_ADR6 TYPE TABLE OF GS_ADR6,
      WA_ADR6 TYPE GS_ADR6.

TYPES : BEGIN OF GS_ADR2 ,
          ADDRNUMBER TYPE ADR2-ADDRNUMBER,
          TEL_NUMBER TYPE ADR2-TEL_NUMBER,
        END OF GS_ADR2 .

DATA: GT_ADR2 TYPE TABLE OF GS_ADR2,
      WA_ADR2 TYPE GS_ADR2.

TYPES : BEGIN OF GS_LFBK ,
            LIFNR TYPE LFBK-LIFNR ,
            BANKL TYPE LFBK-BANKL ,
            BANKN TYPE LFBK-BANKN ,
            BKONT TYPE LFBK-BKONT ,
        END OF GS_LFBK .

DATA : GT_LFBK TYPE TABLE OF GS_LFBK ,
       WA_LFBK TYPE GS_LFBK .


TYPES : BEGIN OF GS_FINAL ,
         LAUFD TYPE REGUH-LAUFD,
         LAUFI TYPE REGUH-LAUFI,
         ZBUKR TYPE REGUH-ZBUKR,
         LIFNR TYPE REGUH-LIFNR,
         KUNNR TYPE REGUH-KUNNR,
         VBLNR TYPE REGUH-VBLNR ,
         WAERS TYPE REGUH-WAERS,
         NAME1 TYPE REGUH-NAME1,
         NAME2 TYPE REGUH-NAME2,
         NAME3 TYPE REGUH-NAME3 ,
         NAME4 TYPE REGUH-NAME4 ,
         PSTLZ TYPE REGUH-PSTLZ,
         ORT01 TYPE REGUH-ORT01,
         STRAS TYPE REGUH-STRAS,
         ZREGI TYPE REGUH-ZREGI ,
         ZBNKS TYPE REGUH-ZBNKS ,
         ZBNKN TYPE REGUH-ZBNKN ,
         ZBNKL TYPE REGUH-ZBNKL,                        "BANK NO
         ZBKON TYPE REGUH-ZBKON,                        "CONTROL KEY
         RZAWE TYPE REGUH-RZAWE,                        "PAYMENT METHOD
         HBKID TYPE REGUH-HBKID,                         " HOUSE BANK
         UBKNT TYPE REGUH-UBKNT,                        " OUR ACC NO
         RWBTR TYPE REGUH-RWBTR ,
         AUGDT TYPE REGUH-AUGDT ,
         BANKL TYPE LFBK-BANKL ,
         BANKN TYPE LFBK-BANKN ,
         BKONT TYPE LFBK-BKONT ,
         ADRNR TYPE LFA1-ADRNR ,
         ZWELS TYPE LFB1-ZWELS ,
         ADDRNUMBER TYPE ADR6-ADDRNUMBER ,
         SMTP_ADDR TYPE ADR6-SMTP_ADDR ,

         TEL_NUMBER TYPE ADR2-TEL_NUMBER,

         AUGBL TYPE BSAK-AUGBL ,
         BELNR TYPE BSAK-BELNR ,
         XBLNR TYPE BSAK-XBLNR  ,
         XBLNR1 TYPE BSAK-XBLNR ,
         XBLNR2 TYPE BSAK-XBLNR ,
         XBLNR3 TYPE BSAK-XBLNR ,
         LNAME TYPE STRING ,
         DI_DATE TYPE SY-DATUM ,
         TOT_PR  TYPE P DECIMALS 3,
         V_WERKCOUNT TYPE I,

        EMAILBODY(100) TYPE C,

        CRN(100) TYPE C,
        PBRANCH(10) TYPE C,
        PLOCATION(10) TYPE C,
        INSR(10) TYPE C,
        PAY2(10) TYPE C,
        PAY3(10) TYPE C,
        PAY4(10) TYPE C,
        PAY5(10) TYPE C,
        PAY6(10) TYPE C,
        PAY7(10) TYPE C,
        PAY8(10) TYPE C,
        PAY9(10) TYPE C,
        PAY10(10) TYPE C,
        PAY11(10) TYPE C,
        PAY12(10) TYPE C,
        PAY13(10) TYPE C,
        PAY14(10) TYPE C,
        PAY15(10) TYPE C,
        PAY16(100) TYPE C,
        PAY17(100) TYPE C,

        END OF GS_FINAL .

DATA : GT_FINAL TYPE TABLE OF GS_FINAL ,
       WA_FINAL TYPE GS_FINAL .

DATA : GT_FINAL1 TYPE TABLE OF GS_FINAL ,
       WA_FINAL1 TYPE GS_FINAL .

DATA : GT_FINAL2 TYPE TABLE OF GS_FINAL ,
       WA_FINAL2 TYPE GS_FINAL .

DATA : GT_FCAT TYPE SLIS_T_FIELDCAT_ALV,
      IT_SORT TYPE SLIS_T_SORTINFO_ALV,
      WA_FCAT TYPE SLIS_FIELDCAT_ALV,
      WA_SORT TYPE SLIS_SORTINFO_ALV.

DATA : LV_BUKRS TYPE REGUH-ZBUKR ,
       LV_BLDAT TYPE REGUH-LAUFD ,
       LV_LIFNR TYPE REGUH-LIFNR .

DATA : LAYOUT TYPE SLIS_LAYOUT_ALV.

DATA: LV_WERKCOUNT TYPE I VALUE 0.

DATA : LV_SUM TYPE BSAK-DMBTR .

DATA : LV_EMAIL(100) TYPE C.

DATA : LV_PAY16(100) TYPE C.

DATA : LV_PAY17(100) TYPE C.

*DATA : LV_CRN(100) TYPE C.


SELECTION-SCREEN : BEGIN OF BLOCK C1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS  :  SO_BUKRS FOR LV_BUKRS OBLIGATORY,
                   SO_BLDAT FOR LV_BLDAT ,
                   SO_LIFNR FOR LV_LIFNR NO-DISPLAY .

SELECTION-SCREEN : END OF BLOCK C1.

START-OF-SELECTION.
  PERFORM GET_DATA.
  PERFORM READ_DATA.

END-OF-SELECTION.
  PERFORM FIELD_CATLOG.
  PERFORM ALV_DISPLAY.

*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA .


  SELECT
   LAUFD
   LAUFI
   XVORL
   ZBUKR
   LIFNR
   KUNNR
   VBLNR
   WAERS
   NAME1
   NAME2
   NAME3
   NAME4
   PSTLZ
   ORT01
   STRAS
   ZREGI
   ZBNKS
   ZBNKN
   ZBNKL
   ZBKON
   RZAWE
   HBKID
   UBKNT
   RWBTR
   AUGDT
       FROM REGUH INTO TABLE GT_REGUH WHERE ZBUKR IN SO_BUKRS AND LAUFD IN SO_BLDAT AND LIFNR IN SO_LIFNR AND XVORL NE 'X'. "AND LIFNR IN SO_LIFNR

  SELECT
    LIFNR
    ADRNR
      FROM LFA1 INTO TABLE GT_LFA1 FOR ALL ENTRIES IN GT_REGUH WHERE LIFNR = GT_REGUH-LIFNR .

    SELECT
      LIFNR
      ZWELS FROM LFB1 INTO  TABLE GT_LFB1 FOR ALL ENTRIES IN GT_REGUH WHERE LIFNR = GT_REGUH-LIFNR .


  SELECT
      LIFNR
      BANKL
      BANKN
      BKONT
        FROM LFBK INTO TABLE GT_LFBK FOR ALL ENTRIES IN GT_REGUH WHERE LIFNR = GT_REGUH-LIFNR .

 IF  GT_LFA1[] IS NOT INITIAL.

  SELECT
    ADDRNUMBER
    SMTP_ADDR FROM ADR6 INTO TABLE GT_ADR6 FOR ALL ENTRIES IN GT_LFA1 WHERE  ADDRNUMBER = GT_LFA1-ADRNR .

  SELECT
      ADDRNUMBER
      TEL_NUMBER
   FROM  ADR2 INTO TABLE GT_ADR2 FOR ALL ENTRIES IN GT_LFA1 WHERE ADDRNUMBER = GT_LFA1-ADRNR .

  ENDIF.

  SELECT
     BUKRS
     LIFNR
     AUGBL
     BELNR
     XBLNR
     BLART
        FROM BSAK INTO TABLE GT_BSAK FOR ALL ENTRIES IN GT_REGUH WHERE AUGBL = GT_REGUH-VBLNR AND BLART EQ 'ZP'. "Removed by savariar s

  SELECT
    BUKRS
    LIFNR
    AUGBL
    BELNR
    XBLNR
    BLART
   FROM BSAK INTO TABLE GT_BSAK1 FOR ALL ENTRIES IN GT_REGUH WHERE AUGBL = GT_REGUH-VBLNR AND BLART EQ 'ZP'."Removed by savariar s


ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_DATA .

* BREAK-POINT .

  LOOP AT GT_REGUH INTO WA_REGUH .

    WA_FINAL-LAUFD = WA_REGUH-LAUFD .
    WA_FINAL-LAUFI = WA_REGUH-LAUFI .
    WA_FINAL-ZBUKR = WA_REGUH-ZBUKR .
    WA_FINAL-LIFNR = WA_REGUH-LIFNR .
    WA_FINAL-KUNNR = WA_REGUH-KUNNR .
    WA_FINAL-VBLNR = WA_REGUH-VBLNR .
    WA_FINAL-WAERS = WA_REGUH-WAERS .

    WA_FINAL-NAME1 = WA_REGUH-NAME1 .
    WA_FINAL-NAME2 = WA_REGUH-NAME2 .
    WA_FINAL-NAME3 = WA_REGUH-NAME3 .
    WA_FINAL-NAME4 = WA_REGUH-NAME4 .
    WA_FINAL-PSTLZ = WA_REGUH-PSTLZ .

    WA_FINAL-ORT01 = WA_REGUH-ORT01 .
    WA_FINAL-STRAS = WA_REGUH-STRAS .
    WA_FINAL-ZREGI = WA_REGUH-ZREGI .
    WA_FINAL-ZBNKS = WA_REGUH-ZBNKS .

    WA_FINAL-ZBNKN = WA_REGUH-ZBNKN .
    WA_FINAL-ZBNKL = WA_REGUH-ZBNKL .
    WA_FINAL-ZBKON = WA_REGUH-ZBKON .
    WA_FINAL-RZAWE = WA_REGUH-RZAWE .
    WA_FINAL-HBKID = WA_REGUH-HBKID .
    WA_FINAL-UBKNT = WA_REGUH-UBKNT .
    WA_FINAL-RWBTR = WA_REGUH-RWBTR .
    WA_FINAL-AUGDT = WA_REGUH-AUGDT .

    CONCATENATE WA_FINAL-LIFNR WA_FINAL-NAME1 INTO WA_FINAL-LNAME SEPARATED BY SPACE .

    READ TABLE GT_LFA1 INTO WA_LFA1 WITH KEY LIFNR = WA_REGUH-LIFNR .
    WA_FINAL-ADRNR = WA_LFA1-ADRNR .

    READ TABLE GT_LFB1 INTO WA_LFB1 WITH KEY LIFNR = WA_REGUH-LIFNR .  "ADDED BY RAM ON 29/8/15
    WA_FINAL-ZWELS = WA_LFB1-ZWELS .

    READ TABLE GT_ADR6 INTO WA_ADR6 WITH KEY ADDRNUMBER = WA_LFA1-ADRNR .
    IF SY-SUBRC = 0.
    WA_FINAL-ADDRNUMBER = WA_ADR6-ADDRNUMBER .
    WA_FINAL-SMTP_ADDR = WA_ADR6-SMTP_ADDR .
    ENDIF.

    READ TABLE GT_ADR2 INTO WA_ADR2 WITH KEY ADDRNUMBER = WA_LFA1-ADRNR .
    IF SY-SUBRC = 0.
     WA_FINAL-TEL_NUMBER = WA_ADR2-TEL_NUMBER.
    ENDIF.


    LOOP AT GT_LFBK INTO WA_LFBK WHERE LIFNR = WA_REGUH-LIFNR .
      WA_FINAL-BANKL = WA_LFBK-BANKL .
      WA_FINAL-BANKN = WA_LFBK-BANKN .
      WA_FINAL-BKONT = WA_LFBK-BKONT .
    ENDLOOP.

    LOOP AT GT_BSAK INTO WA_BSAK WHERE AUGBL = WA_REGUH-VBLNR .

      WA_FINAL-AUGBL = WA_BSAK-AUGBL .
      WA_FINAL-BELNR = WA_BSAK-BELNR .
      WA_FINAL-XBLNR = WA_BSAK-XBLNR .

    ENDLOOP.

    WA_FINAL-DI_DATE = SY-DATUM .
    SHIFT WA_FINAL-LIFNR LEFT DELETING LEADING '0'.
    APPEND WA_FINAL TO GT_FINAL.
    CLEAR WA_FINAL.

  ENDLOOP .
  SORT GT_FINAL DESCENDING BY LAUFI LIFNR RWBTR .

  LOOP AT GT_FINAL INTO WA_FINAL .
    ON CHANGE OF WA_FINAL-LAUFI OR WA_FINAL-LIFNR.
      CLEAR LV_SUM.
    ENDON.
    IF WA_FINAL-RWBTR NE 0 . "  AND WA_FINAL-RWBTR > 0.
      LV_SUM = LV_SUM + WA_FINAL-RWBTR   .
      WA_FINAL-TOT_PR = LV_SUM. "#EC CI_FLDEXT_OK[2610650]    "Added by SPLABAP during code remediation
      MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING TOT_PR.
      CLEAR : WA_FINAL.
    ENDIF.

  ENDLOOP.

  " BREAK-POINT .
  SORT GT_FINAL ASCENDING BY LAUFI LIFNR TOT_PR .

  LOOP AT GT_FINAL INTO WA_FINAL .
    IF WA_FINAL-LAUFI IS NOT INITIAL . " OR WA_FINAL-TOT_PR > 0 .
      LV_WERKCOUNT = LV_WERKCOUNT  + 1.
      ON CHANGE OF WA_FINAL-LAUFI OR WA_FINAL-LIFNR.
        WA_FINAL-V_WERKCOUNT = LV_WERKCOUNT.
        MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING V_WERKCOUNT.
        CLEAR WA_FINAL.
      ENDON.
    ENDIF.
  ENDLOOP.

  LOOP AT GT_FINAL INTO WA_FINAL.
    IF WA_FINAL-V_WERKCOUNT = 0 .
      DELETE GT_FINAL .
    ENDIF.
  ENDLOOP .

*BREAK-POINT.


  LOOP AT GT_FINAL INTO WA_FINAL.

    LV_EMAIL = ''.
    LV_PAY16 = ''.
    LV_PAY17 = ''.

    MOVE LV_EMAIL TO WA_FINAL-EMAILBODY.

    MOVE LV_PAY16 TO WA_FINAL-PAY16.

    MOVE LV_PAY17 TO WA_FINAL-PAY17.

    MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING EMAILBODY PAY16 PAY17 .

    CLEAR WA_FINAL.


  ENDLOOP.


LOOP AT GT_FINAL INTO WA_FINAL .  "ADDED BY RAM ON 10/8/15

  IF WA_FINAL-RZAWE EQ 'I' .
      WA_FINAL-BANKL = '' .
      WA_FINAL-BKONT = '' .
      MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING BANKL BKONT .
    CLEAR WA_FINAL .
    ENDIF .

ENDLOOP .

ENDFORM.                    " READ_DATA
*&---------------------------------------------------------------------*
*&      Form  FIELD_CATLOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FIELD_CATLOG .

  PERFORM ALV_LAYOUT USING 1 'Identification' 'LAUFI' 'GT_FINAL' '' ''.
  " PERFORM ALV_LAYOUT USING 3 'Trans.Method' 'RZAWE' 'GT_FINAL' '' ''.

  PERFORM ALV_LAYOUT USING 3 'Trans.Method' 'ZWELS' 'GT_FINAL' '' ''.
  PERFORM ALV_LAYOUT USING 2 'Payment' 'VBLNR' 'GT_FINAL' '' 'X'.
  PERFORM ALV_LAYOUT USING 5 'Amount' 'TOT_PR' 'GT_FINAL' '' ''.  "TOT_PR
  PERFORM ALV_LAYOUT USING 7 'Payment Date' 'LAUFD' 'GT_FINAL' '' ''.
  " PERFORM ALV_LAYOUT USING 7 'Payment Date' 'DI_DATE' 'GT_FINAL' '' ''.
 " PERFORM ALV_LAYOUT USING 9 'Vendor No' 'LIFNR' 'GT_FINAL' '' ''.
  PERFORM ALV_LAYOUT USING 11 'Vendor Name' 'NAME1' 'GT_FINAL' '' ''.
  PERFORM ALV_LAYOUT USING 13 'Address1' 'NAME2' 'GT_FINAL' '' ''.
  PERFORM ALV_LAYOUT USING 15 'Address2' 'NAME3' 'GT_FINAL' '' ''.
  PERFORM ALV_LAYOUT USING 17 'Address3' 'NAME4' 'GT_FINAL' '' ''.
  PERFORM ALV_LAYOUT USING 19 'City' 'ORT01' 'GT_FINAL' '' ''.
  PERFORM ALV_LAYOUT USING 21 'State' 'ZREGI' 'GT_FINAL' '' ''.
  PERFORM ALV_LAYOUT USING 23 'Pincode' 'PSTLZ' 'GT_FINAL' '' ''.

  PERFORM ALV_LAYOUT USING 24 'Bene Acc No' 'BANKN' 'GT_FINAL' '' ''.

  PERFORM ALV_LAYOUT USING 25 'Email' 'SMTP_ADDR' 'GT_FINAL' '' ''.
  PERFORM ALV_LAYOUT USING 26 'Email Body' 'EMAILBODY' 'GT_FINAL' '' ''.

  PERFORM ALV_LAYOUT USING 29 'SPL Bank Acc No' 'UBKNT' 'GT_FINAL' '' ''.

  PERFORM ALV_LAYOUT USING 30 'CRN (Narration/Remarks)' 'CRN' 'GT_FINAL' '' ''.

  PERFORM ALV_LAYOUT USING 30 'IFSC Code' 'BANKL' 'GT_FINAL' '' ''.
  PERFORM ALV_LAYOUT USING 32 'Receiver A/c Type' 'BKONT' 'GT_FINAL' '' ''.

  PERFORM ALV_LAYOUT USING 33 'Print Branch' 'PBRANCH' 'GT_FINAL' '' ''.
  PERFORM ALV_LAYOUT USING 34 'Payable Location' 'PLOCATION' 'GT_FINAL' '' ''.

  PERFORM ALV_LAYOUT USING 35 'Instrument Date' 'INSR' 'GT_FINAL' '' ''.


  PERFORM ALV_LAYOUT USING 36 'Payment Details1' 'LNAME' 'GT_FINAL' '' ''.

  PERFORM ALV_LAYOUT USING 37 'Payment Details2' 'PAY2' 'GT_FINAL' '' ''.

  PERFORM ALV_LAYOUT USING 39 'Payment Details3' 'PAY3' 'GT_FINAL' '' ''.
  PERFORM ALV_LAYOUT USING 40 'Payment Details4' 'PAY4' 'GT_FINAL' '' ''.
  PERFORM ALV_LAYOUT USING 41 'Payment Details5' 'PAY5' 'GT_FINAL' '' ''.
  PERFORM ALV_LAYOUT USING 43 'Payment Details6' 'PAY6' 'GT_FINAL' '' ''.
  PERFORM ALV_LAYOUT USING 45 'Payment Details7' 'PAY7' 'GT_FINAL' '' ''.
  PERFORM ALV_LAYOUT USING 46 'Payment Details8' 'PAY8' 'GT_FINAL' '' ''.
  PERFORM ALV_LAYOUT USING 47 'Payment Details9' 'PAY9' 'GT_FINAL' '' ''.
  PERFORM ALV_LAYOUT USING 48 'Payment Details10' 'PAY10' 'GT_FINAL' '' ''.
  PERFORM ALV_LAYOUT USING 49 'Payment Details11' 'PAY11' 'GT_FINAL' '' ''.
  PERFORM ALV_LAYOUT USING 50 'Payment Details12' 'PAY12' 'GT_FINAL' '' ''.
  PERFORM ALV_LAYOUT USING 52 'Payment Details13' 'PAY13' 'GT_FINAL' '' ''.
  PERFORM ALV_LAYOUT USING 52 'Payment Details14' 'PAY14' 'GT_FINAL' '' ''.
  PERFORM ALV_LAYOUT USING 54 'Payment Details15' 'PAY15' 'GT_FINAL' '' ''.
  PERFORM ALV_LAYOUT USING 55 'Payment Details16' 'PAY16' 'GT_FINAL' '' ''.
  PERFORM ALV_LAYOUT USING 56 'Payment Details17(Vendor Contact no.)' 'TEL_NUMBER' 'GT_FINAL' '' ''.
*  PERFORM ALV_LAYOUT USING 57 'Payment Details18' 'PAY18' 'GT_FINAL' '' ''.






*  PERFORM ALV_LAYOUT USING 36 'Payment Details2' 'XBLNR' 'GT_FINAL' ''.


*  PERFORM ALV_LAYOUT USING 37 'Payment Details3' 'XBLNR1' 'GT_FINAL' ''.
*
*  PERFORM ALV_LAYOUT USING 38 'Payment Details4' 'XBLNR2' 'GT_FINAL' ''.

*  PERFORM ALV_LAYOUT USING 39 'Payment Details5' 'XBLNR3' 'GT_FINAL' ''.
*
*   PERFORM ALV_LAYOUT USING 42 'Payment Details6' 'XBLNR' 'GT_FINAL' ''.
*
*  PERFORM ALV_LAYOUT USING 43 'Payment Details7' 'DOC7' 'GT_FINAL' ''.
*
*  PERFORM ALV_LAYOUT USING 44 'Payment Details8' 'DOC8' 'GT_FINAL' ''.
*
*  PERFORM ALV_LAYOUT USING 45 'Payment Details9' 'DOC9' 'GT_FINAL' ''.
*
*  PERFORM ALV_LAYOUT USING 46 'Payment Details10' 'DOC10' 'GT_FINAL' ''.


*  PERFORM ALV_LAYOUT USING 47 'work' 'V_WERKCOUNT' 'GT_FINAL' ''.
*
*  PERFORM ALV_LAYOUT USING 48 'AMT' 'RWBTR' 'GT_FINAL' ''.

ENDFORM.                    " FIELD_CATLOG
*&---------------------------------------------------------------------*
*&      Form  ALV_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ALV_DISPLAY .

  LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  LAYOUT-ZEBRA = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
            EXPORTING
*                      I_INTERFACE_CHECK                 = ' '
*                      I_BYPASSING_BUFFER                = ' '
*                      I_BUFFER_ACTIVE                   = ' '
                       I_CALLBACK_PROGRAM                = SY-REPID
*                      I_CALLBACK_PF_STATUS_SET          = ' '
                       I_CALLBACK_USER_COMMAND           = 'MY_USER_COMMAND'
                       I_CALLBACK_TOP_OF_PAGE            = 'ALV_CATALOG_HEADER'
*                      I_CALLBACK_HTML_TOP_OF_PAGE       =
*                      I_CALLBACK_HTML_END_OF_LIST       = ' '
*                      I_STRUCTURE_NAME                  =
*                      I_BACKGROUND_ID                   = ' '
*                      I_GRID_TITLE                      =
*                      I_GRID_SETTINGS                   =
              IS_LAYOUT                         = LAYOUT
               IT_FIELDCAT                       = GT_FCAT[]
*                      IT_EXCLUDING                      =
*                      IT_SPECIAL_GROUPS                 =
               IT_SORT                           = IT_SORT
*                      IT_FILTER                         =
*                      IS_SEL_HIDE                       =
*                      I_DEFAULT                         = 'X'
*                      I_SAVE                            = ' '
*                      IS_VARIANT                        =
*                      IT_EVENTS                         =
*                      IT_EVENT_EXIT                     =
*                      IS_PRINT                          =
*                      IS_REPREP_ID                      =
*                      I_SCREEN_START_COLUMN             = 0
*                      I_SCREEN_START_LINE               = 0
*                      I_SCREEN_END_COLUMN               = 0
*                      I_SCREEN_END_LINE                 = 0
*                      I_HTML_HEIGHT_TOP                 = 0
*                      I_HTML_HEIGHT_END                 = 0
*                      IT_ALV_GRAPHICS                   =
*                      IT_HYPERLINK                      =
*                      IT_ADD_FIELDCAT                   =
*                      IT_EXCEPT_QINFO                   =
*                      IR_SALV_FULLSCREEN_ADAPTER        =
*                    IMPORTING
*                      E_EXIT_CAUSED_BY_CALLER           =
*                      ES_EXIT_CAUSED_BY_USER            =
             TABLES
               T_OUTTAB                          = GT_FINAL[]
*                    EXCEPTIONS
*                      PROGRAM_ERROR                     = 1
*                      OTHERS                            = 2
                     .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.
ENDFORM.                    " ALV_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  ALV_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0412   text
*      -->P_0413   text
*      -->P_0414   text
*      -->P_0415   text
*----------------------------------------------------------------------*
FORM ALV_LAYOUT USING P1 P2 P3 P4 P5 P6.
  CLEAR WA_FCAT.
  WA_FCAT-COL_POS = P1.
  WA_FCAT-SELTEXT_L = P2.
  WA_FCAT-FIELDNAME = P3.
  WA_FCAT-TABNAME = P4.
  WA_FCAT-DO_SUM = P5.
  WA_FCAT-HOTSPOT = P6.
  APPEND WA_FCAT TO GT_FCAT.

ENDFORM.                    " ALV_LAYOUT

*---------------------------------------------------
*&      Form  ALV_CATALOG_HEADER
*&-------------------------------------------------------------------
*       text
*--------------------------------------------------------------------
*  -->  p1        text
*  <--  p2        text
*--------------------------------------------------------------------
FORM ALV_CATALOG_HEADER.
  DATA : LIT_HEADER TYPE  SLIS_T_LISTHEADER,
       LS_LINE TYPE SLIS_LISTHEADER.
  CLEAR LS_LINE.
  LS_LINE-TYP  = 'H'.
  LS_LINE-KEY = ' '.
  LS_LINE-INFO = 'Vendor Auto Payment Register' .
  APPEND LS_LINE TO LIT_HEADER.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = LIT_HEADER
      I_LOGO             = 'ZLOGO'.

ENDFORM.                    "ALV_CATALOG_HEADER

FORM MY_USER_COMMAND USING R_UCOMM LIKE SY-UCOMM RS_SELFIELD TYPE SLIS_SELFIELD.


  IF RS_SELFIELD-FIELDNAME = 'VBLNR'.

    SET PARAMETER ID 'BLN' FIELD RS_SELFIELD-VALUE.
    CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
  ENDIF.




ENDFORM.
