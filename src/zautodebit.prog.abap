REPORT ZAUTODEBIT
       NO STANDARD PAGE HEADING LINE-SIZE 255.


DATA:   IT_BDCDATA LIKE BDCDATA    OCCURS 0 WITH HEADER LINE.
*      wa_bdcdata like line of it_bdcdata.

DATA: I_BDCMSG  LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE,
       BDCMSGCOLL LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE ,
        EXP_CMNMSG_05 TYPE C ,
         C_DISMODE   TYPE CHAR1 VALUE 'N'.

DATA : RAW TYPE TABLE OF ALSMEX_TABLINE ,
       WRAW TYPE  ALSMEX_TABLINE ,
       WRA TYPE  ALSMEX_TABLINE .

DATA:   MESSTAB LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE.
DATA:   WA_MESSAGE LIKE BDCMSGCOLL.

TYPES : BEGIN OF TY_FINAL,
        STATUS(10) TYPE C,
        MESSAGE(50) TYPE C,
        FIELD(132)  TYPE C,
          FIELD1(132)  TYPE C,
          FIELD2(132)  TYPE C,
          FIELD3(132)  TYPE C,
        END OF TY_FINAL.


DATA : IT_FINAL TYPE TABLE OF TY_FINAL,
       WA_FINAL2 TYPE TY_FINAL.


DATA : FCAT TYPE SLIS_T_FIELDCAT_ALV,
       W_FCAT TYPE SLIS_FIELDCAT_ALV.

DATA: S_LAYOUT TYPE SLIS_LAYOUT_ALV.
TYPE-POOLS: SLIS.
TABLES:BSID,KNA1.
DATA: "OR_LIFNR TYPE LFA1-LIFNR,
      OR_BUKRS TYPE T001-BUKRS,
      OR_VKBUR TYPE KNVV-VKBUR,
      OR_ZTERM TYPE T052-ZTERM ,
       OR_GJAHR TYPE BSIK-GJAHR,
       A TYPE DATUM.

DATA:        IT_FIELDCAT  TYPE TABLE OF SLIS_FIELDCAT_ALV,
      WA_FIELDCAT  LIKE LINE OF IT_FIELDCAT.


DATA: GD_LAYOUT    TYPE SLIS_LAYOUT_ALV.
TYPES: BEGIN OF GS_T001W,
       WERKS TYPE T001W-WERKS,
       BNAME TYPE T001W-NAME1,
      END OF GS_T001W.
TYPES: BEGIN OF GS_KNVV,
 KUNNR TYPE KNVV-KUNNR,                    " Customer Code
 VKBUR TYPE KNVV-VKBUR,                    " Customer Name
 END OF GS_KNVV.

DATA: GT_KNVV TYPE TABLE OF GS_KNVV,
WA_KNVV TYPE GS_KNVV.

TYPES: BEGIN OF GS_VBRK,
 VBELN TYPE VBRK-VBELN,                    " Customer Code
 ZTERM TYPE VBRK-ZTERM,
 KNUMV TYPE VBRK-KNUMV,                    " Customer Name
 END OF GS_VBRK.

DATA: BEGIN OF IT_DATE OCCURS 0,
      DATE  TYPE SY-DATUM ,
      END OF IT_DATE .

DATA: DATE TYPE SY-DATUM,
      DAY TYPE I,
      DAY1 TYPE I,
        WA_HOLIDAY   TYPE     ZDN_CAL,
        IT_HOLIDAY TYPE TABLE OF ZDN_CAL.



DATA: GT_VBRK TYPE TABLE OF GS_VBRK,
WA_VBRK TYPE GS_VBRK.

TYPES: BEGIN OF GS_KONV,
KNUMV TYPE KONV-KNUMV,
KPOSN TYPE KONV-KPOSN,                    " Customer Code
KSCHL TYPE KONV-KSCHL,
KAWRT TYPE KONV-KAWRT,
KBETR TYPE KONV-KBETR,
KWERT TYPE KONV-KWERT,                    " Customer Name
END OF GS_KONV.
DATA: GT_KONV TYPE TABLE OF GS_KONV,
WA_KONV TYPE GS_KONV.

TYPES: BEGIN OF GS_BSID,
       BUKRS TYPE BSID-BUKRS,                    " Company Code
       KUNNR TYPE BSID-KUNNR,                    " Customer Code
       BELNR TYPE BSIK-BELNR,                    " Accounting Document Number
       DMBTR TYPE BSID-DMBTR,                    " Bill Amount ( In Doc Currency )
       BLDAT TYPE BSID-BLDAT,
       ZTERM TYPE BSID-ZTERM,

       END OF GS_BSID.

TYPES: BEGIN OF GS_FINAL,
BUKRS TYPE BSID-BUKRS,                    " Company Code
KUNNR TYPE BSID-KUNNR,                    " Customer Code
XBLNR TYPE BSIK-XBLNR,                    " Accounting Document Number
DMBTR TYPE BSID-DMBTR,                    " Bill Amount ( In Doc Currency )
BLDAT TYPE BSID-BLDAT,
ZTERM TYPE BSID-ZTERM,
KNUMV TYPE VBRK-KNUMV,
BLART TYPE BSID-BLART,
ZDYAYS TYPE I,
POSNR TYPE KONV-KPOSN,
KSCHL TYPE KONV-KSCHL,
  sample(2) type c,
 KAWRT TYPE KONV-KAWRT,
 KBETR TYPE KONV-KBETR,
  KWERT TYPE KONV-KWERT,
  WKREG TYPE VBRP-WKREG,
  VKBUR TYPE VBRP-VKBUR,
  NAME1 TYPE KNA1-NAME1,
  aufnr type aufnr,
  VAT(20) TYPE c,
  CASHD(20) TYPE c,
  TOTAL(20) TYPE c,
*  kunnr type bsid-kunnr,
END OF GS_FINAL.

DATA: GT_FINAL TYPE TABLE OF GS_FINAL,
WA_FINAL TYPE GS_FINAL.
TYPES:BEGIN OF CUSTNAME,
  KUNNR TYPE KNA1-KUNNR,
  NAME1 TYPE KNA1-NAME1,
  END OF CUSTNAME.

DATA: IT_CUST TYPE TABLE OF CUSTNAME,
      WA_CUST TYPE CUSTNAME.
DATA:
      B(20) TYPE C,
      C(20) TYPE C,
      D(20) TYPE C,
      E(20) TYPE C,
      F(20) TYPE C,
      G(20) TYPE C,
      H(20) TYPE C,
      I(20) TYPE C.
DATA:G_INDEX TYPE I,
      G_STR_INDEX TYPE STRING,
      G_STR_FIELDNAME TYPE STRING.

DATA: L1(20) TYPE C,
      L2(20) TYPE C.


DATA: KAWRT1 TYPE KONV-KAWRT,
      KWERT1 TYPE KONV-KWERT,
      KAWRT2 TYPE KONV-KAWRT,
       KAWRT3 TYPE KONV-KAWRT,
        KAWRT4 TYPE KONV-KAWRT,
         KAWRT5 TYPE KONV-KAWRT,
          KAWRT6 TYPE KONV-KAWRT,
          KAWRT7 TYPE KONV-KAWRT,
          KAWRT8 TYPE KONV-KAWRT,
          KAWRT9 TYPE KONV-KAWRT,
          KAWRT10 TYPE KONV-KAWRT,
      KWERT2 TYPE KONV-KWERT,
      KBETR1 TYPE KONV-KBETR,
       KBETR2 TYPE KONV-KBETR,
        KBETR3 TYPE KONV-KBETR,
         KBETR4 TYPE KONV-KBETR.
data: t1(20) type c value 0,
      t2(20) type c value 0,
      t3(20) type c value 0,
      XBLNR1 type bsid-xblnr,
      XBLNR2 type bsid-xblnr.

TYPES: BEGIN OF GS_FINAL1,
                 " Company Code
KUNNR TYPE BSID-KUNNR,                    " Customer Code
XBLNR TYPE BSIk-XBLNR,                    " Accounting Document Number

ZTERM TYPE BSID-ZTERM,

ZDYAYS TYPE I,
totvat(20) type c,
  totcashd(20) type c,
  totTotal(20) type c,
SAMPLE(2) TYPE C,
kschl type konv-kschl,
AUFNR TYPE AUFNR,

END OF GS_FINAL1.
DATA: GT_FINAL1 TYPE TABLE OF GS_FINAL1,
WA_FINAL1 TYPE GS_FINAL1.
DATA: GT_BSID TYPE TABLE OF GS_BSID,
WA_BSID TYPE GS_BSID.

DATA: DATE1(10) TYPE C.
SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:      SO_BUKRS FOR OR_BUKRS DEFAULT 4000 ."MODIF ID MOD .
*SELECT-OPTIONS:      SO_Zterm FOR or_Zterm MODIF ID MOD .
SELECT-OPTIONS:      SO_VKBUR FOR OR_VKBUR MODIF ID MOD . " NO-DISPLAY.
SELECT-OPTIONS:      SO_KUNNR FOR BSID-KUNNR . "MODIF ID MOD .
PARAMETERS: P_DATE TYPE SY-DATUM DEFAULT SY-DATUM OBLIGATORY.

SELECTION-SCREEN: END OF BLOCK B1.

*include bdcrecx1.

START-OF-SELECTION.

  SELECT
       KUNNR
       VKBUR
         FROM KNVV INTO TABLE GT_KNVV WHERE VKBUR IN SO_VKBUR .

*select kunnr name1 from kna1 into table it_cust where kunnr in so_kunnr.
*  SELECT BUKRS KUNNR BELNR DMBTR BLDAT ZTERM FROM BSID INTO CORRESPONDING FIELDS OF TABLE GT_FINAL FOR ALL ENTRIES IN GT_KNVV WHERE
*   KUNNR =  GT_KNVV-KUNNR AND BUKRS = '4000' AND
*         UMSKZ NE 'F' AND  ZFBDT <= P_DATE.
  SELECT A~BUKRS A~KUNNR A~XBLNR A~DMBTR A~BLDAT A~BLART A~ZTERM  "#EC CI_DB_OPERATION_OK[2768887]
    C~POSNR  B~KNUMV C~WKREG  C~VKBUR
    "Added by SPLABAP during code remediation
    FROM BSID AS A  INNER JOIN VBRK AS B ON A~XBLNR = B~VBELN AND B~FKSTO = ' '
     INNER JOIN VBRP AS C ON B~VBELN  = C~VBELN INNER JOIN KNA1 AS D ON A~KUNNR = D~KUNNR

     INTO CORRESPONDING FIELDS OF TABLE GT_FINAL  WHERE
           A~KUNNR IN SO_KUNNR AND C~VKBUR IN SO_VKBUR AND A~BUKRS IN SO_BUKRS AND ZFBDT <= P_DATE and a~blart = 'RV' . "and a~zterm  = 'RPPD' or a~zterm  = 'CD'."a~zterm in so_zterm ."and a~blart = 'RV' and a~blart = 'VG'.
*
*SELECT BUKRS KUNNR BELNR DMBTR BLDAT blart ZTERM FROM BSID INTO CORRESPONDING FIELDS OF TABLE GT_FINAL  WHERE
*   BUKRS in so_bukrs and ZFBDT <= P_DATE and zterm in so_zterm.


*  if sy-subrc = 0.
*
**select vbeln zterm knumv from vbrk into CORRESPONDING FIELDS OF TABLE gt_vbrk   where
**    zterm in so_zterm.
*  select a~vbeln a~zterm a~knumv from vbrk as a INNER JOIN bsid as b on a~vbeln = b~belnr into CORRESPONDING FIELDS OF TABLE gt_vbrk where
*    a~zterm in so_zterm and vkorg = '4000'.

  IF SY-SUBRC = 0.
    SELECT KNUMV KPOSN KSCHL KAWRT KBETR KWERT FROM PRCD_ELEMENTS INTO CORRESPONDING FIELDS OF TABLE GT_KONV FOR ALL ENTRIES IN
      GT_FINAL WHERE KNUMV = GT_FINAL-KNUMV ."and "kschl = 'JIVP'. " and kschl = 'JIVA' and kschl = 'JVSR' and kschl = 'JIVC'.

*     endif.

  ELSE.
    MESSAGE 'Nodata for selected company code' TYPE 'E'.
  ENDIF.
*        UMSKZ NE 'F' AND  ZFBDT <= P_DATE.

  SELECT * FROM ZDN_CAL  INTO  TABLE IT_HOLIDAY .
  LOOP AT GT_FINAL INTO WA_FINAL.


    DATE = WA_FINAL-BLDAT + 1.
    IT_DATE-DATE =  DATE.
*APPEND it_date.
    DO.

      IF IT_DATE-DATE  = P_DATE.
        EXIT.
      ENDIF.


      READ TABLE IT_HOLIDAY   INTO WA_HOLIDAY WITH KEY  REGION = WA_FINAL-WKREG  HOLIDAY = IT_DATE-DATE .

      IF SY-SUBRC = 0.
        DAY = DAY + 1.
      ENDIF.
      IT_DATE-DATE =  IT_DATE-DATE + 1.
*  APPEND it_date.
      CLEAR :WA_HOLIDAY.
    ENDDO.

    DAY1 = SY-DATUM - DATE.
    WA_FINAL-ZDYAYS = DAY1 - DAY.
    SELECT SINGLE  KUNNR NAME1 FROM KNA1 INTO WA_CUST WHERE KUNNR = WA_FINAL-KUNNR.
    IF SY-SUBRC = 0.

      WA_FINAL-NAME1 = WA_CUST-NAME1.
      CLEAR WA_CUST.
    ENDIF.
 select   aufnr from aufk into wa_final-aufnr up to 1 rows  where  werks = wa_final-vkbur
   ORDER BY PRIMARY KEY."Added by SPLABAP during code remediation
      ENDSELECT.
      if sy-subrc = 0.
        ENDIF.
*    CALL FUNCTION 'HR_99S_INTERVAL_BETWEEN_DATES'
*          EXPORTING
*            BEGDA = WA_FINAL-BLDAT
*            ENDDA = P_DATE
*          IMPORTING
*            DAYS  = WA_FINAL-ZDYAYS.

*    read table gt_vbrk into wa_vbrk with key vbeln = WA_FINAL-belnr .
*
*   if sy-subrc = 0.
**      select knumv kposn kschl kawrt kbetr kwert from konv into CORRESPONDING FIELDS OF TABLE gt_konv  where knumv = wa_vbrk-knumv and kschl = 'JIVP'.
*      read table gt_konv into wa_konv with key kschl = 'JIVP' knumv = wa_vbrk-knumv  kposn = WA_FINAL-posnr.
*
*
*    wa_final-kschl = wa_konv-kschl.
*    wa_final-kawrt = wa_konv-kawrt.
*    wa_final-kbetr = wa_konv-kbetr.
*    wa_final-kwert = wa_konv-kwert.
*
*else.
*  read table gt_konv into wa_konv with key kschl = 'JIVA' knumv = wa_vbrk-knumv  kposn = WA_FINAL-posnr.
*
*if sy-subrc = 0.
*    wa_final-kschl = wa_konv-kschl.
*    wa_final-kawrt = wa_konv-kawrt.
*    wa_final-kbetr = wa_konv-kbetr.
*    wa_final-kwert = wa_konv-kwert.
*
*    else.
*
*      read table gt_konv into wa_konv with key kschl = 'JVSR' knumv = wa_vbrk-knumv  kposn = WA_FINAL-posnr.
*
*if sy-subrc = 0.
*    wa_final-kschl = wa_konv-kschl.
*    wa_final-kawrt = wa_konv-kawrt.
*    wa_final-kbetr = wa_konv-kbetr.
*    wa_final-kwert = wa_konv-kwert.
*
*    else.
*      read table gt_konv into wa_konv with key kschl = 'JVSR' knumv = wa_vbrk-knumv  kposn = WA_FINAL-posnr.
*
*if sy-subrc = 0.
*    wa_final-kschl = wa_konv-kschl.
*    wa_final-kawrt = wa_konv-kawrt.
*    wa_final-kbetr = wa_konv-kbetr.
*    wa_final-kwert = wa_konv-kwert.
*
*    else.
*
*      read table gt_konv into wa_konv with key kschl = 'JIVC' knumv = wa_vbrk-knumv  kposn = WA_FINAL-posnr.
*
*if sy-subrc = 0.
*    wa_final-kschl = wa_konv-kschl.
*    wa_final-kawrt = wa_konv-kawrt.
*    wa_final-kbetr = wa_konv-kbetr.
*    wa_final-kwert = wa_konv-kwert.
*else.
    READ TABLE GT_KONV INTO WA_KONV WITH KEY KSCHL = 'ZCAD' KNUMV = WA_FINAL-KNUMV  KPOSN = WA_FINAL-POSNR.
*select  single kschl kawrt kbetr kwert from konv into wa_konv  where kschl = 'ZCAD'  and knumv = wa_final-knumv and kposn = WA_FINAL-posnr .


    IF SY-SUBRC = 0.
      WA_FINAL-KSCHL = WA_KONV-KSCHL.
      WA_FINAL-KAWRT = WA_KONV-KAWRT.
      WA_FINAL-KBETR = WA_KONV-KBETR.
      WA_FINAL-KWERT = WA_KONV-KWERT.

      IF   WA_FINAL-ZDYAYS > 21 AND WA_FINAL-ZTERM = 'CD'.
        KWERT1  =  ( WA_FINAL-KAWRT * 3 ) / 100 .
*
        KAWRT1 =  WA_FINAL-KAWRT - KWERT1.

        READ TABLE GT_KONV INTO WA_KONV WITH KEY KSCHL = 'JIVP' KNUMV = WA_FINAL-KNUMV  KPOSN = WA_FINAL-POSNR.
        IF SY-SUBRC = 0.

           KBETR1 = WA_KONV-KBETR / 1 .
          KAWRT2 = ( ( KAWRT1 * KBETR1 ) / 100 ) .
          B =  KAWRT1 - WA_KONV-KAWRT  .
          KAWRT6 = WA_KONV-KAWRT + WA_KONV-KWERT.
          F = KAWRT2 - WA_KONV-KWERT.
        ENDIF.

        READ TABLE GT_KONV INTO WA_KONV WITH KEY KSCHL = 'JIVA' KNUMV = WA_FINAL-KNUMV  KPOSN = WA_FINAL-POSNR.
        IF SY-SUBRC = 0.

           KBETR2 = WA_KONV-KBETR / 1 .
          KAWRT3 =   ( ( KAWRT1 * KBETR2 ) / 100 ) .
          C =  KAWRT1 - WA_KONV-KAWRT  .
          KAWRT7 = WA_KONV-KAWRT + WA_KONV-KWERT.
          G = KAWRT3 - WA_KONV-KWERT.

        ENDIF.

        READ TABLE GT_KONV INTO WA_KONV WITH KEY KSCHL = 'JVSR' KNUMV = WA_FINAL-KNUMV  KPOSN = WA_FINAL-POSNR.
        IF SY-SUBRC = 0.
           KBETR3 = WA_KONV-KBETR / 1 .
          KAWRT4 =  ( ( KAWRT1 * KBETR3 ) / 100 ) .
          D =  KAWRT1 - WA_KONV-KAWRT  .
          KAWRT8 = WA_KONV-KAWRT + WA_KONV-KWERT.
          H = KAWRT4 - WA_KONV-KWERT.
        ENDIF.

        READ TABLE GT_KONV INTO WA_KONV WITH KEY KSCHL = 'JIVC' KNUMV = WA_FINAL-KNUMV  KPOSN = WA_FINAL-POSNR.
        IF SY-SUBRC = 0.
           KBETR4 = WA_KONV-KBETR / 1 .
          KAWRT5 =  ( ( KAWRT1 * KBETR4 ) / 100 ) .
          E =  KAWRT1 - WA_KONV-KAWRT  .
          KAWRT9 = WA_KONV-KAWRT + WA_KONV-KWERT.
          I = KAWRT5 - WA_KONV-KWERT.
          wa_final-sample = 'AB'.
        ENDIF.
*    endif.
*      endif.
*
*      endif.
*
*      endif.
*
*     endif.
        IF ( B IS NOT INITIAL  ) OR ( C IS NOT INITIAL ) .
          IF  ( B = E ) OR  ( B = C )  .
            L1 = B.


          ELSEIF  ( C IS NOT INITIAL ).
            IF ( ( C = E ) OR ( C = D ) ) AND  ( ( B NE E ) OR  ( B NE C ) OR ( B NE D ) ).

              L1 = C.
            ENDIF.
          ELSE.
            L1 = B + C + D + E.
          ENDIF.
        ELSE.
          L1 = B + C + D + E.

        ENDIF.
        WA_FINAL-CASHD = L1.
        L2 = F + G + H + I.
        WA_FINAL-VAT = L2.
        WA_FINAL-TOTAL = L1 + L2.

      ENDIF.

 IF   WA_FINAL-ZDYAYS > 45 AND WA_FINAL-ZTERM = 'CD'.
        KWERT1  =  ( WA_FINAL-KAWRT * 2 ) / 100 .

        KAWRT1 =  WA_FINAL-KAWRT - KWERT1.

        READ TABLE GT_KONV INTO WA_KONV WITH KEY KSCHL = 'JIVP' KNUMV = WA_FINAL-KNUMV  KPOSN = WA_FINAL-POSNR.
        IF SY-SUBRC = 0.

           KBETR1 = WA_KONV-KBETR / 1 .
          KAWRT2 = ( ( KAWRT1 * KBETR1 ) / 100 ) .
          B =  KAWRT1 - WA_KONV-KAWRT  .
          KAWRT6 = WA_KONV-KAWRT + WA_KONV-KWERT.
          F = KAWRT2 - WA_KONV-KWERT.
        ENDIF.

        READ TABLE GT_KONV INTO WA_KONV WITH KEY KSCHL = 'JIVA' KNUMV = WA_FINAL-KNUMV  KPOSN = WA_FINAL-POSNR.
        IF SY-SUBRC = 0.

           KBETR2 = WA_KONV-KBETR / 1 .
          KAWRT3 =   ( ( KAWRT1 * KBETR2 ) / 100 ) .
          C =  KAWRT1 - WA_KONV-KAWRT  .
          KAWRT7 = WA_KONV-KAWRT + WA_KONV-KWERT.
          G = KAWRT3 - WA_KONV-KWERT.

        ENDIF.

        READ TABLE GT_KONV INTO WA_KONV WITH KEY KSCHL = 'JVSR' KNUMV = WA_FINAL-KNUMV  KPOSN = WA_FINAL-POSNR.
        IF SY-SUBRC = 0.
           KBETR3 = WA_KONV-KBETR / 1 .
          KAWRT4 =  ( ( KAWRT1 * KBETR3 ) / 100 ) .
          D =  KAWRT1 - WA_KONV-KAWRT  .
          KAWRT8 = WA_KONV-KAWRT + WA_KONV-KWERT.
          H = KAWRT4 - WA_KONV-KWERT.
        ENDIF.

        READ TABLE GT_KONV INTO WA_KONV WITH KEY KSCHL = 'JIVC' KNUMV = WA_FINAL-KNUMV  KPOSN = WA_FINAL-POSNR.
        IF SY-SUBRC = 0.
           KBETR4 = WA_KONV-KBETR / 1 .
          KAWRT5 =  ( ( KAWRT1 * KBETR4 ) / 100 ) .
          E =  KAWRT1 - WA_KONV-KAWRT  .
          KAWRT9 = WA_KONV-KAWRT + WA_KONV-KWERT.
          I = KAWRT5 - WA_KONV-KWERT.
          wa_final-sample = 'AB'.
        ENDIF.
*    endif.
*      endif.
*
*      endif.
*
*      endif.
*
*     endif.

        IF ( B IS NOT INITIAL  ) OR ( C IS NOT INITIAL ) .
          IF  ( B = E ) OR  ( B = C )  .
            L1 = B.


          ELSEIF  ( C IS NOT INITIAL ).
            IF ( ( C = E ) OR ( C = D ) ) AND  ( ( B NE E ) OR  ( B NE C ) OR ( B NE D ) ).

              L1 = C.
            ENDIF.

          ELSE.
            L1 = B + C + D + E.
          ENDIF.
        ELSE.
          L1 = B + C + D + E.

        ENDIF.
        WA_FINAL-CASHD = L1.
        L2 = F + G + H + I.
        WA_FINAL-VAT = L2.
        WA_FINAL-TOTAL = L1 + L2.

      ENDIF.

      IF   WA_FINAL-ZDYAYS > 45 AND WA_FINAL-ZTERM = 'RPPD'.
        KWERT1  =  ( WA_FINAL-KAWRT * 0 ) / 100 .

        KAWRT1 =  WA_FINAL-KAWRT - KWERT1.

        READ TABLE GT_KONV INTO WA_KONV WITH KEY KSCHL = 'JIVP' KNUMV = WA_FINAL-KNUMV  KPOSN = WA_FINAL-POSNR.
        IF SY-SUBRC = 0.

           KBETR1 = WA_KONV-KBETR / 1 .
          KAWRT2 = ( ( KAWRT1 * KBETR1 ) / 100 ) .
          B =  KAWRT1 - WA_KONV-KAWRT  .
          KAWRT6 = WA_KONV-KAWRT + WA_KONV-KWERT.
          F = KAWRT2 - WA_KONV-KWERT.
        ENDIF.

        READ TABLE GT_KONV INTO WA_KONV WITH KEY KSCHL = 'JIVA' KNUMV = WA_FINAL-KNUMV  KPOSN = WA_FINAL-POSNR.
        IF SY-SUBRC = 0.

           KBETR2 = WA_KONV-KBETR / 1 .
          KAWRT3 =   ( ( KAWRT1 * KBETR2 ) / 100 ) .
          C =  KAWRT1 - WA_KONV-KAWRT  .
          KAWRT7 = WA_KONV-KAWRT + WA_KONV-KWERT.
          G = KAWRT3 - WA_KONV-KWERT.

        ENDIF.

        READ TABLE GT_KONV INTO WA_KONV WITH KEY KSCHL = 'JVSR' KNUMV = WA_FINAL-KNUMV  KPOSN = WA_FINAL-POSNR.
        IF SY-SUBRC = 0.
           KBETR3 = WA_KONV-KBETR / 1 .
          KAWRT4 =  ( ( KAWRT1 * KBETR3 ) / 100 ) .
          D =  KAWRT1 - WA_KONV-KAWRT  .
          KAWRT8 = WA_KONV-KAWRT + WA_KONV-KWERT.
          H = KAWRT4 - WA_KONV-KWERT.
        ENDIF.

        READ TABLE GT_KONV INTO WA_KONV WITH KEY KSCHL = 'JIVC' KNUMV = WA_FINAL-KNUMV  KPOSN = WA_FINAL-POSNR.
        IF SY-SUBRC = 0.
           KBETR4 = WA_KONV-KBETR / 1 .
          KAWRT5 =  ( ( KAWRT1 * KBETR4 ) / 100 ) .
          E =  KAWRT1 - WA_KONV-KAWRT  .
          KAWRT9 = WA_KONV-KAWRT + WA_KONV-KWERT.
          I = KAWRT5 - WA_KONV-KWERT.
          wa_final-sample = 'AB'.
        ENDIF.
*    endif.
*      endif.
*
*      endif.
*
*      endif.
*
*     endif.

        IF ( B IS NOT INITIAL  ) OR ( C IS NOT INITIAL ) .
          IF  ( B = E ) OR  ( B = C ) OR ( B = D ) .
            L1 = B.


          ELSEIF  ( C IS NOT INITIAL ).
            IF ( ( C = E ) OR ( C = D ) ) AND  ( ( B NE E ) OR  ( B NE C ) OR ( B NE D ) ).

              L1 = C.
            ENDIF.
          ELSE.
            L1 = B + C + D + E.
          ENDIF.
        ELSE.
          L1 = B + C + D + E.

        ENDIF.
        WA_FINAL-CASHD = L1.
        L2 = F + G + H + I.
        WA_FINAL-VAT = L2.
        WA_FINAL-TOTAL = L1 + L2.

      ENDIF.

    ENDIF.




    MODIFY GT_FINAL FROM WA_FINAL.
    CLEAR: WA_FINAL,IT_DATE,WA_KONV.
    CLEAR: DAY,DAY1.
    CLEAR:A,B,C,D,E,F,G,H,I,KAWRT1,KAWRT2,KAWRT3,KAWRT4,KAWRT5,KAWRT6,KAWRT7,KAWRT9,KBETR1,KBETR2,KBETR3,KBETR4.
    CLEAR :L1,L2.
  ENDLOOP.
* update konv  set kwert = '9.54-' where knumv = '0000170226' and kposn = '000001' and kschl = 'ZCAD'.
  DELETE GT_FINAL WHERE KSCHL IS INITIAL.
  DELETE GT_FINAL WHERE ZTERM NE 'RPPD' AND  ZTERM NE 'CD' .
 delete gt_final where total is INITIAL.


 loop at gt_final into wa_final.
   t1 = wa_final-cashd + t1.
t2 = wa_final-vat + t2.
t3 = wa_final-total + t3.
wa_final1-xblnr = wa_final-xblnr.
  wa_final1-kschl = wa_final-kschl.
  wa_final1-zterm = wa_final-zterm.
   wa_final1-kunnr = wa_final-kunnr.
    wa_final1-zdyays = wa_final-zdyays.
     wa_final1-sample = wa_final-sample.
      wa_final1-aufnr = wa_final-aufnr.

at END OF xblnr."#EC CI_SORTED
  "Added by SPLABAP during code remediation
  wa_final1-totcashd = t1.
  wa_final1-totvat = t2.
  wa_final1-tottotal = t3.

clear: wa_final, t1,t2,t3.

  ENDAT.
append wa_final1 to gt_final1.
  clear: wa_final1.
   ENDLOOP.

   DELETE GT_FINAL1 WHERE KSCHL IS INITIAL.
  DELETE GT_FINAL1 WHERE ZTERM NE 'RPPD' AND  ZTERM NE 'CD' .
 delete gt_final1 where tottotal is INITIAL.
*perform open_group.
  CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT'
    EXPORTING
      INPUT  = SY-DATUM
    IMPORTING
      OUTPUT = DATE1.
  .
  G_INDEX = 1.
*

  LOOP AT GT_FINAL1 INTO WA_FINAL1.
* perform bdc_dynpro      using 'SAPMF05A' '1200'.
*
*perform bdc_field       using 'BDC_OKCODE'
*                              '=DUMMY'.
*perform bdc_dynpro      using 'SAPLACHD' '1000'.
*perform bdc_field       using 'BDC_CURSOR'
*                              'BKPF-BUKRS'.
*perform bdc_field       using 'BDC_OKCODE'
*                              '=ENTR'.
*perform bdc_field       using 'BKPF-BUKRS'
*                             '4000'.
if wa_final1-zterm = 'CD' . "and  ( wa_final-zdyays = 22 or wa_final-zdyays = 46 ).
  IF  WA_FINAL1-sample = 'AB'.

perform bdc_dynpro      using 'SAPMF05A' '1200'.
perform bdc_field       using 'BDC_OKCODE'
                              '=DUMMY'.
perform bdc_field       using 'RF05A-BUSCS'
                              'R'.
perform bdc_field       using 'BDC_CURSOR'
                              'INVFO-ACCNT'.
perform bdc_field       using 'INVFO-BUDAT'
                             DATE1 ."  '05.04.2016'.
perform bdc_field       using 'INVFO-WAERS'
                              'INR'.
perform bdc_field       using 'INVFO-ACCNT'
                              WA_FINAL1-KUNNR. "   'IC4159'.
perform bdc_field       using 'INVFO-BLDAT'
                            DATE1 ."   '05.04.2016'.
perform bdc_field       using 'INVFO-XBLNR'
                              WA_FINAL1-XBLNR. "'ADASD'.
perform bdc_dynpro      using 'SAPMF05A' '1200'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
perform bdc_field       using 'RF05A-BUSCS'
                              'R'.
perform bdc_field       using 'INVFO-ACCNT'
                            WA_FINAL1-KUNNR. "  'IC4159'.
perform bdc_field       using 'INVFO-XBLNR'
                             WA_FINAL1-XBLNR. " 'ADASD'.
condense : wa_final1-totcashd, wa_final1-totvat,wa_final1-tottotal.
perform bdc_field       using 'INVFO-WRBTR'
                              wa_final1-tottotal. " '18.28'.
perform bdc_field       using 'INVFO-WAERS'
                              'INR'.
perform bdc_field       using 'INVFO-SGTXT'
                              'Auto Debit note'.
perform bdc_field       using 'BDC_CURSOR'
                              'ACGL_ITEM-WRBTR(02)'.
perform bdc_field       using 'ACGL_ITEM-HKONT(01)'
                              '45030012'.
perform bdc_field       using 'ACGL_ITEM-HKONT(02)'
                               '16622121'.
perform bdc_field       using 'ACGL_ITEM-WRBTR(01)'
                               wa_final1-totcashd . " '15.96'.
perform bdc_field       using 'ACGL_ITEM-WRBTR(02)'
                              wa_final1-totvat.  "  '2.32'.
*perform bdc_field       using 'ACGL_ITEM-KOSTL(01)'
*                              '4000AMBHO'.
perform bdc_field       using 'ACGL_ITEM-AUFNR(01)'
                             wa_final1-aufnr ." ''.
*PERFORM BDC_DYNPRO      USING 'SAPMF05A' '1200'.
*PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                              '=DUMMY'.
*PERFORM BDC_FIELD       USING 'RF05A-BUSCS'
*                              'R'.
*PERFORM BDC_FIELD       USING 'BDC_CURSOR'
*                              'INVFO-MWSKZ'.
*PERFORM BDC_FIELD       USING 'INVFO-ACCNT'
*                              WA_FINAL-KUNNR.            "  'IC4159'.
*PERFORM BDC_FIELD       USING 'INVFO-XBLNR'
*                              WA_FINAL-XBLNR. "  '4101000009'.
*PERFORM BDC_FIELD       USING 'INVFO-WRBTR'
*                             WA_FINAL-TOTAL. "  '500.00'.
*perform bdc_field       using 'INVFO-XMWST'
*                              'X'.
*perform bdc_field       using 'INVFO-MWSKZ'
*                              'O0'.
*PERFORM BDC_FIELD       USING 'INVFO-SGTXT'
*                             'Auto Debit note'. " 'fdcgfdfgfcgc'.
perform bdc_dynpro      using 'SAPMF05A' '1200'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
perform bdc_field       using 'BDC_CURSOR'
                              'INVFO-ZTERM'.
perform bdc_field       using 'INVFO-ZFBDT'
                            date1 . " '05.04.2016'.
perform bdc_field       using 'INVFO-ZTERM'
                             WA_FINAL1-ZTERM. " 'CD'.
perform bdc_dynpro      using 'SAPMF05A' '1200'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
perform bdc_field       using 'INVFO-ZFBDT'
                            date1 . "   '05.04.2016'.
perform bdc_field       using 'INVFO-ZTERM'
                              WA_FINAL1-ZTERM. " 'CD'.
*perform bdc_field       using 'INVFO-ZBD1T'
*                              '21'.
*perform bdc_field       using 'INVFO-ZBD1P'
*                              '5.000'.
*perform bdc_field       using 'INVFO-ZBD2T'
*                              '45'.
*perform bdc_field       using 'INVFO-ZBD2P'
*                              '3.000'.
*perform bdc_field       using 'INVFO-ZBD3T'
*                              '46'.
perform bdc_field       using 'BDC_CURSOR'
                              'ACGL_ITEM-HKONT(03)'.
perform bdc_dynpro      using 'SAPMF05A' '1200'.
perform bdc_field       using 'BDC_OKCODE'
                              'BU'.
perform bdc_field       using 'INVFO-ZFBDT'
                               date1 . "'05.04.2016'.
perform bdc_field       using 'INVFO-ZTERM'
                              WA_FINAL1-ZTERM. "'CD'.
*perform bdc_field       using 'INVFO-ZBD1T'
*                              '21'.
*perform bdc_field       using 'INVFO-ZBD1P'
*                              '5.000'.
*perform bdc_field       using 'INVFO-ZBD2T'
*                              '45'.
*perform bdc_field       using 'INVFO-ZBD2P'
*                              '3.000'.
*perform bdc_field       using 'INVFO-ZBD3T'
*                              '46'.
perform bdc_field       using 'BDC_CURSOR'
                              'ACGL_ITEM-HKONT(03)'.
    CALL TRANSACTION 'FB70' USING IT_BDCDATA
                                MODE 'S'
                                UPDATE 'S' MESSAGES INTO I_BDCMSG.
    IF NOT I_BDCMSG[] IS INITIAL.
*    PERFORM FORMAT_MESSAGE.
      READ TABLE I_BDCMSG INTO WA_MESSAGE .
      PERFORM ERROR.
    ENDIF.


    REFRESH IT_BDCDATA.

    APPEND WA_FINAL2 TO IT_FINAL.

    CLEAR : WA_FINAL1 , MESSTAB.
    REFRESH : MESSTAB[].
    else.
*    perform bdc_dynpro      using 'SAPMF05A' '1200'.
*perform bdc_field       using 'BDC_OKCODE'
*                              '=DUMMY'.

perform bdc_dynpro      using 'SAPMF05A' '1200'.
perform bdc_field       using 'BDC_OKCODE'
                              '=DUMMY'.
perform bdc_field       using 'RF05A-BUSCS'
                              'R'.
perform bdc_field       using 'BDC_CURSOR'
                              'INVFO-ACCNT'.
perform bdc_field       using 'INVFO-BUDAT'
                             DATE1 ."  '05.04.2016'.
perform bdc_field       using 'INVFO-WAERS'
                              'INR'.
perform bdc_field       using 'INVFO-ACCNT'
                              WA_FINAL1-KUNNR. "   'IC4159'.
perform bdc_field       using 'INVFO-BLDAT'
                            DATE1 ."   '05.04.2016'.
perform bdc_field       using 'INVFO-XBLNR'
                              WA_FINAL1-XBLNR. "'ADASD'.
perform bdc_dynpro      using 'SAPMF05A' '1200'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
perform bdc_field       using 'RF05A-BUSCS'
                              'R'.
perform bdc_field       using 'INVFO-ACCNT'
                            WA_FINAL1-KUNNR. "  'IC4159'.
perform bdc_field       using 'INVFO-XBLNR'
                             WA_FINAL1-XBLNR. " 'ADASD'.
condense : wa_final1-totcashd, wa_final1-totvat,wa_final1-tottotal.
perform bdc_field       using 'INVFO-WRBTR'
                              wa_final1-tottotal. " '18.28'.
perform bdc_field       using 'INVFO-WAERS'
                              'INR'.
perform bdc_field       using 'INVFO-SGTXT'
                              'Auto Debit note'.
perform bdc_field       using 'BDC_CURSOR'
                              'ACGL_ITEM-WRBTR(02)'.
perform bdc_field       using 'ACGL_ITEM-HKONT(01)'
                              '45030012'.
perform bdc_field       using 'ACGL_ITEM-HKONT(02)'
                                '16622111'.
perform bdc_field       using 'ACGL_ITEM-WRBTR(01)'
                               wa_final1-totcashd . " '15.96'.
perform bdc_field       using 'ACGL_ITEM-WRBTR(02)'
                              wa_final1-totvat.  "  '2.32'.
*perform bdc_field       using 'ACGL_ITEM-KOSTL(01)'
*                              '4000AMBHO'.
perform bdc_field       using 'ACGL_ITEM-AUFNR(01)'
                             wa_final1-aufnr ." ''.
*PERFORM BDC_DYNPRO      USING 'SAPMF05A' '1200'.
*PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                              '=DUMMY'.
*PERFORM BDC_FIELD       USING 'RF05A-BUSCS'
*                              'R'.
*PERFORM BDC_FIELD       USING 'BDC_CURSOR'
*                              'INVFO-MWSKZ'.
*PERFORM BDC_FIELD       USING 'INVFO-ACCNT'
*                              WA_FINAL-KUNNR.            "  'IC4159'.
*PERFORM BDC_FIELD       USING 'INVFO-XBLNR'
*                              WA_FINAL-XBLNR. "  '4101000009'.
*PERFORM BDC_FIELD       USING 'INVFO-WRBTR'
*                             WA_FINAL-TOTAL. "  '500.00'.
*perform bdc_field       using 'INVFO-XMWST'
*                              'X'.
*perform bdc_field       using 'INVFO-MWSKZ'
*                              'O0'.
*PERFORM BDC_FIELD       USING 'INVFO-SGTXT'
*                             'Auto Debit note'. " 'fdcgfdfgfcgc'.
perform bdc_dynpro      using 'SAPMF05A' '1200'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
perform bdc_field       using 'BDC_CURSOR'
                              'INVFO-ZTERM'.
perform bdc_field       using 'INVFO-ZFBDT'
                            date1 . " '05.04.2016'.
perform bdc_field       using 'INVFO-ZTERM'
                             WA_FINAL1-ZTERM. " 'CD'.
perform bdc_dynpro      using 'SAPMF05A' '1200'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
perform bdc_field       using 'INVFO-ZFBDT'
                            date1 . "   '05.04.2016'.
perform bdc_field       using 'INVFO-ZTERM'
                              WA_FINAL1-ZTERM. " 'CD'.
*perform bdc_field       using 'INVFO-ZBD1T'
*                              '21'.
*perform bdc_field       using 'INVFO-ZBD1P'
*                              '5.000'.
*perform bdc_field       using 'INVFO-ZBD2T'
*                              '45'.
*perform bdc_field       using 'INVFO-ZBD2P'
*                              '3.000'.
*perform bdc_field       using 'INVFO-ZBD3T'
*                              '46'.
perform bdc_field       using 'BDC_CURSOR'
                              'ACGL_ITEM-HKONT(03)'.
perform bdc_dynpro      using 'SAPMF05A' '1200'.
perform bdc_field       using 'BDC_OKCODE'
                              'BU'.
perform bdc_field       using 'INVFO-ZFBDT'
                               date1 . "'05.04.2016'.
perform bdc_field       using 'INVFO-ZTERM'
                              WA_FINAL1-ZTERM. "'CD'.
*perform bdc_field       using 'INVFO-ZBD1T'
*                              '21'.
*perform bdc_field       using 'INVFO-ZBD1P'
*                              '5.000'.
*perform bdc_field       using 'INVFO-ZBD2T'
*                              '45'.
*perform bdc_field       using 'INVFO-ZBD2P'
*                              '3.000'.
*perform bdc_field       using 'INVFO-ZBD3T'
*                              '46'.
perform bdc_field       using 'BDC_CURSOR'
                              'ACGL_ITEM-HKONT(03)'.
    CALL TRANSACTION 'FB70' USING IT_BDCDATA
                                MODE 'S'
                                UPDATE 'S' MESSAGES INTO I_BDCMSG.
    IF NOT I_BDCMSG[] IS INITIAL.
*    PERFORM FORMAT_MESSAGE.
      READ TABLE I_BDCMSG INTO WA_MESSAGE .
      PERFORM ERROR.
    ENDIF.


    REFRESH IT_BDCDATA.

    APPEND WA_FINAL2 TO IT_FINAL.

    CLEAR : WA_FINAL , MESSTAB.
    REFRESH : MESSTAB[].

  ENDIF.

endif.

  ENDLOOP.

loop at GT_final1 into wa_final1.
  if wa_final1-zterm = 'RPPD' . "and   wa_final-zdyays = 46.
  IF  WA_FINAL1-sample = 'AB'.
*   perform bdc_dynpro      using 'SAPMF05A' '1200'.
*perform bdc_field       using 'BDC_OKCODE'
*                              '=DUMMY'.

perform bdc_dynpro      using 'SAPMF05A' '1200'.
perform bdc_field       using 'BDC_OKCODE'
                              '=DUMMY'.
perform bdc_field       using 'RF05A-BUSCS'
                              'R'.
perform bdc_field       using 'BDC_CURSOR'
                              'INVFO-ACCNT'.
perform bdc_field       using 'INVFO-BUDAT'
                             DATE1 ."  '05.04.2016'.
perform bdc_field       using 'INVFO-WAERS'
                              'INR'.
perform bdc_field       using 'INVFO-ACCNT'
                              WA_FINAL1-KUNNR. "   'IC4159'.
perform bdc_field       using 'INVFO-BLDAT'
                            DATE1 ."   '05.04.2016'.
perform bdc_field       using 'INVFO-XBLNR'
                              WA_FINAL1-XBLNR. "'ADASD'.
perform bdc_dynpro      using 'SAPMF05A' '1200'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
perform bdc_field       using 'RF05A-BUSCS'
                              'R'.
perform bdc_field       using 'INVFO-ACCNT'
                            WA_FINAL1-KUNNR. "  'IC4159'.
perform bdc_field       using 'INVFO-XBLNR'
                             WA_FINAL1-XBLNR. " 'ADASD'.
condense : wa_final1-totcashd, wa_final1-totvat,wa_final1-tottotal.
perform bdc_field       using 'INVFO-WRBTR'
                              wa_final1-tottotal. " '18.28'.
perform bdc_field       using 'INVFO-WAERS'
                              'INR'.
perform bdc_field       using 'INVFO-SGTXT'
                              'Auto Debit note'.
perform bdc_field       using 'BDC_CURSOR'
                              'ACGL_ITEM-WRBTR(02)'.
perform bdc_field       using 'ACGL_ITEM-HKONT(01)'
                              '45030012'.
perform bdc_field       using 'ACGL_ITEM-HKONT(02)'
                               '16622121'.
perform bdc_field       using 'ACGL_ITEM-WRBTR(01)'
                               wa_final1-totcashd . " '15.96'.
perform bdc_field       using 'ACGL_ITEM-WRBTR(02)'
                              wa_final1-totvat.  "  '2.32'.
*perform bdc_field       using 'ACGL_ITEM-KOSTL(01)'
*                              '4000AMBHO'.
perform bdc_field       using 'ACGL_ITEM-AUFNR(01)'
                             wa_final1-aufnr ." ''.
*PERFORM BDC_DYNPRO      USING 'SAPMF05A' '1200'.
*PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                              '=DUMMY'.
*PERFORM BDC_FIELD       USING 'RF05A-BUSCS'
*                              'R'.
*PERFORM BDC_FIELD       USING 'BDC_CURSOR'
*                              'INVFO-MWSKZ'.
*PERFORM BDC_FIELD       USING 'INVFO-ACCNT'
*                              WA_FINAL-KUNNR.            "  'IC4159'.
*PERFORM BDC_FIELD       USING 'INVFO-XBLNR'
*                              WA_FINAL-XBLNR. "  '4101000009'.
*PERFORM BDC_FIELD       USING 'INVFO-WRBTR'
*                             WA_FINAL-TOTAL. "  '500.00'.
*perform bdc_field       using 'INVFO-XMWST'
*                              'X'.
*perform bdc_field       using 'INVFO-MWSKZ'
*                              'O0'.
*PERFORM BDC_FIELD       USING 'INVFO-SGTXT'
*                             'Auto Debit note'. " 'fdcgfdfgfcgc'.
perform bdc_dynpro      using 'SAPMF05A' '1200'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
perform bdc_field       using 'BDC_CURSOR'
                              'INVFO-ZTERM'.
perform bdc_field       using 'INVFO-ZFBDT'
                            date1 . " '05.04.2016'.
perform bdc_field       using 'INVFO-ZTERM'
                             WA_FINAL1-ZTERM. " 'CD'.
perform bdc_dynpro      using 'SAPMF05A' '1200'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
perform bdc_field       using 'INVFO-ZFBDT'
                            date1 . "   '05.04.2016'.
perform bdc_field       using 'INVFO-ZTERM'
                              WA_FINAL1-ZTERM. " 'CD'.
*perform bdc_field       using 'INVFO-ZBD1T'
*                              '21'.
*perform bdc_field       using 'INVFO-ZBD1P'
*                              '5.000'.
*perform bdc_field       using 'INVFO-ZBD2T'
*                              '45'.
*perform bdc_field       using 'INVFO-ZBD2P'
*                              '3.000'.
*perform bdc_field       using 'INVFO-ZBD3T'
*                              '46'.
perform bdc_field       using 'BDC_CURSOR'
                              'ACGL_ITEM-HKONT(03)'.
perform bdc_dynpro      using 'SAPMF05A' '1200'.
perform bdc_field       using 'BDC_OKCODE'
                              'BU'.
perform bdc_field       using 'INVFO-ZFBDT'
                               date1 . "'05.04.2016'.
perform bdc_field       using 'INVFO-ZTERM'
                              WA_FINAL1-ZTERM. "'CD'.
*perform bdc_field       using 'INVFO-ZBD1T'
*                              '21'.
*perform bdc_field       using 'INVFO-ZBD1P'
*                              '5.000'.
*perform bdc_field       using 'INVFO-ZBD2T'
*                              '45'.
*perform bdc_field       using 'INVFO-ZBD2P'
*                              '3.000'.
*perform bdc_field       using 'INVFO-ZBD3T'
*                              '46'.
perform bdc_field       using 'BDC_CURSOR'
                              'ACGL_ITEM-HKONT(03)'.
    CALL TRANSACTION 'FB70' USING IT_BDCDATA
                                MODE 'S'
                                UPDATE 'S' MESSAGES INTO I_BDCMSG.
    IF NOT I_BDCMSG[] IS INITIAL.
*    PERFORM FORMAT_MESSAGE.
      READ TABLE I_BDCMSG INTO WA_MESSAGE .
      PERFORM ERROR.
    ENDIF.


    REFRESH IT_BDCDATA.

    APPEND WA_FINAL2 TO IT_FINAL.

    CLEAR : WA_FINAL1 , MESSTAB.
    REFRESH : MESSTAB[].
 ELSE.

perform bdc_dynpro      using 'SAPMF05A' '1200'.
perform bdc_field       using 'BDC_OKCODE'
                              '=DUMMY'.
perform bdc_field       using 'RF05A-BUSCS'
                              'R'.
perform bdc_field       using 'BDC_CURSOR'
                              'INVFO-ACCNT'.
perform bdc_field       using 'INVFO-BUDAT'
                             DATE1 ."  '05.04.2016'.
perform bdc_field       using 'INVFO-WAERS'
                              'INR'.
perform bdc_field       using 'INVFO-ACCNT'
                              WA_FINAL1-KUNNR. "   'IC4159'.
perform bdc_field       using 'INVFO-BLDAT'
                            DATE1 ."   '05.04.2016'.
perform bdc_field       using 'INVFO-XBLNR'
                              WA_FINAL1-XBLNR. "'ADASD'.
perform bdc_dynpro      using 'SAPMF05A' '1200'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
perform bdc_field       using 'RF05A-BUSCS'
                              'R'.
perform bdc_field       using 'INVFO-ACCNT'
                            WA_FINAL1-KUNNR. "  'IC4159'.
perform bdc_field       using 'INVFO-XBLNR'
                             WA_FINAL1-XBLNR. " 'ADASD'.
condense : wa_final1-totcashd, wa_final1-totvat,wa_final1-tottotal.
perform bdc_field       using 'INVFO-WRBTR'
                              wa_final1-tottotal. " '18.28'.
perform bdc_field       using 'INVFO-WAERS'
                              'INR'.
perform bdc_field       using 'INVFO-SGTXT'
                              'Auto Debit note'.
perform bdc_field       using 'BDC_CURSOR'
                              'ACGL_ITEM-WRBTR(02)'.
perform bdc_field       using 'ACGL_ITEM-HKONT(01)'
                              '45030012'.
perform bdc_field       using 'ACGL_ITEM-HKONT(02)'
                                '16622111'.
perform bdc_field       using 'ACGL_ITEM-WRBTR(01)'
                               wa_final1-totcashd . " '15.96'.
perform bdc_field       using 'ACGL_ITEM-WRBTR(02)'
                              wa_final1-totvat.  "  '2.32'.
*perform bdc_field       using 'ACGL_ITEM-KOSTL(01)'
*                              '4000AMBHO'.
perform bdc_field       using 'ACGL_ITEM-AUFNR(01)'
                             wa_final1-aufnr ." ''.
*PERFORM BDC_DYNPRO      USING 'SAPMF05A' '1200'.
*PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                              '=DUMMY'.
*PERFORM BDC_FIELD       USING 'RF05A-BUSCS'
*                              'R'.
*PERFORM BDC_FIELD       USING 'BDC_CURSOR'
*                              'INVFO-MWSKZ'.
*PERFORM BDC_FIELD       USING 'INVFO-ACCNT'
*                              WA_FINAL-KUNNR.            "  'IC4159'.
*PERFORM BDC_FIELD       USING 'INVFO-XBLNR'
*                              WA_FINAL-XBLNR. "  '4101000009'.
*PERFORM BDC_FIELD       USING 'INVFO-WRBTR'
*                             WA_FINAL-TOTAL. "  '500.00'.
*perform bdc_field       using 'INVFO-XMWST'
*                              'X'.
*perform bdc_field       using 'INVFO-MWSKZ'
*                              'O0'.
*PERFORM BDC_FIELD       USING 'INVFO-SGTXT'
*                             'Auto Debit note'. " 'fdcgfdfgfcgc'.
perform bdc_dynpro      using 'SAPMF05A' '1200'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
perform bdc_field       using 'BDC_CURSOR'
                              'INVFO-ZTERM'.
perform bdc_field       using 'INVFO-ZFBDT'
                            date1 . " '05.04.2016'.
perform bdc_field       using 'INVFO-ZTERM'
                             WA_FINAL1-ZTERM. " 'CD'.
perform bdc_dynpro      using 'SAPMF05A' '1200'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
perform bdc_field       using 'INVFO-ZFBDT'
                            date1 . "   '05.04.2016'.
perform bdc_field       using 'INVFO-ZTERM'
                              WA_FINAL1-ZTERM. " 'CD'.
*perform bdc_field       using 'INVFO-ZBD1T'
*                              '21'.
*perform bdc_field       using 'INVFO-ZBD1P'
*                              '5.000'.
*perform bdc_field       using 'INVFO-ZBD2T'
*                              '45'.
*perform bdc_field       using 'INVFO-ZBD2P'
*                              '3.000'.
*perform bdc_field       using 'INVFO-ZBD3T'
*                              '46'.
perform bdc_field       using 'BDC_CURSOR'
                              'ACGL_ITEM-HKONT(03)'.
perform bdc_dynpro      using 'SAPMF05A' '1200'.
perform bdc_field       using 'BDC_OKCODE'
                              'BU'.
perform bdc_field       using 'INVFO-ZFBDT'
                               date1 . "'05.04.2016'.
perform bdc_field       using 'INVFO-ZTERM'
                              WA_FINAL1-ZTERM. "'CD'.
*perform bdc_field       using 'INVFO-ZBD1T'
*                              '21'.
*perform bdc_field       using 'INVFO-ZBD1P'
*                              '5.000'.
*perform bdc_field       using 'INVFO-ZBD2T'
*                              '45'.
*perform bdc_field       using 'INVFO-ZBD2P'
*                              '3.000'.
*perform bdc_field       using 'INVFO-ZBD3T'
*                              '46'.
perform bdc_field       using 'BDC_CURSOR'
                              'ACGL_ITEM-HKONT(03)'.
    CALL TRANSACTION 'FB70' USING IT_BDCDATA
                                MODE 'S'
                                UPDATE 'S' MESSAGES INTO I_BDCMSG.
    IF NOT I_BDCMSG[] IS INITIAL.
*    PERFORM FORMAT_MESSAGE.
      READ TABLE I_BDCMSG INTO WA_MESSAGE .
      PERFORM ERROR.
    ENDIF.


    REFRESH IT_BDCDATA.

    APPEND WA_FINAL2 TO IT_FINAL.

    CLEAR : WA_FINAL , MESSTAB.
    REFRESH : MESSTAB[].


  ENDIF.
ENDIF.
  ENDLOOP.


  CLEAR : W_FCAT .
  W_FCAT-FIELDNAME = 'STATUS' .
  W_FCAT-COL_POS = 1 .
  W_FCAT-SELTEXT_L = 'Status' .
  W_FCAT-OUTPUTLEN = 10 .
  W_FCAT-TABNAME = 'IT_FINAL'.
  APPEND W_FCAT TO FCAT.

  CLEAR : W_FCAT .
  W_FCAT-FIELDNAME = 'MESSAGE' .
  W_FCAT-COL_POS = 2 .
  W_FCAT-SELTEXT_L = 'Message' .
  W_FCAT-TABNAME = 'IT_FINAL'.
  W_FCAT-OUTPUTLEN = 50 .
  APPEND W_FCAT TO FCAT.

  CLEAR : W_FCAT .
  W_FCAT-FIELDNAME = 'FIELD' .
  W_FCAT-COL_POS = 3 .
  W_FCAT-SELTEXT_L = 'Field Name' .
  W_FCAT-TABNAME = 'IT_FINAL'.
  W_FCAT-OUTPUTLEN = 50 .
  APPEND W_FCAT TO FCAT.

  CLEAR : W_FCAT .
  W_FCAT-FIELDNAME = 'FIELD1' .
  W_FCAT-COL_POS = 3 .
  W_FCAT-SELTEXT_L = 'Field Name' .
  W_FCAT-TABNAME = 'IT_FINAL'.
  W_FCAT-OUTPUTLEN = 50 .
  APPEND W_FCAT TO FCAT.

*  CLEAR : W_FCAT .
*  W_FCAT-FIELDNAME = 'FIELD2' .
*  W_FCAT-COL_POS = 3 .
*  W_FCAT-SELTEXT_L = 'Field Name' .
*  W_FCAT-TABNAME = 'IT_FINAL'.
*W_FCAT-OUTPUTLEN = 50 .
*  APPEND W_FCAT TO FCAT.
*
*  CLEAR : W_FCAT .
*  W_FCAT-FIELDNAME = 'FIELD3' .
*  W_FCAT-COL_POS = 3 .
*  W_FCAT-SELTEXT_L = 'Field Name' .
*  W_FCAT-TABNAME = 'IT_FINAL'.
*W_FCAT-OUTPUTLEN = 50 .
*  APPEND W_FCAT TO FCAT.

  S_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*   I_INTERFACE_CHECK                 = ' '
*   I_BYPASSING_BUFFER                = ' '
*   I_BUFFER_ACTIVE                   = ' '
*   I_CALLBACK_PROGRAM                = ' '
*   I_CALLBACK_PF_STATUS_SET          = ' '
*   I_CALLBACK_USER_COMMAND           = ' '
*   I_CALLBACK_TOP_OF_PAGE            = ' '
*   I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*   I_CALLBACK_HTML_END_OF_LIST       = ' '
*   I_STRUCTURE_NAME                  =
*   I_BACKGROUND_ID                   = ' '
*   I_GRID_TITLE                      =
*   I_GRID_SETTINGS                   =
     IS_LAYOUT                         = S_LAYOUT
     IT_FIELDCAT                       = FCAT
*   IT_EXCLUDING                      =
*   IT_SPECIAL_GROUPS                 =
*   IT_SORT                           =
*   IT_FILTER                         =
*   IS_SEL_HIDE                       =
*   I_DEFAULT                         = 'X'
*   I_SAVE                            = ' '
*   IS_VARIANT                        =
*   IT_EVENTS                         =
*   IT_EVENT_EXIT                     =
*   IS_PRINT                          =
*   IS_REPREP_ID                      =
*   I_SCREEN_START_COLUMN             = 0
*   I_SCREEN_START_LINE               = 0
*   I_SCREEN_END_COLUMN               = 0
*   I_SCREEN_END_LINE                 = 0
*   I_HTML_HEIGHT_TOP                 = 0
*   I_HTML_HEIGHT_END                 = 0
*   IT_ALV_GRAPHICS                   =
*   IT_HYPERLINK                      =
*   IT_ADD_FIELDCAT                   =
*   IT_EXCEPT_QINFO                   =
*   IR_SALV_FULLSCREEN_ADAPTER        =
* IMPORTING
*   E_EXIT_CAUSED_BY_CALLER           =
*   ES_EXIT_CAUSED_BY_USER            =
     TABLES
       T_OUTTAB                          = IT_FINAL
* EXCEPTIONS
*   PROGRAM_ERROR                     = 1
*   OTHERS                            = 2
             .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.







*&---------------------------------------------------------------------*
*&      Form  BDC_DYNPRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PROGRAM    text
*      -->DYNPRO     text
*----------------------------------------------------------------------*
FORM BDC_DYNPRO USING PROGRAM DYNPRO.
  CLEAR IT_BDCDATA.
*  wa_BDCDATA-PROGRAM  = PROGRAM.
*  wa_BDCDATA-DYNPRO   = DYNPRO.
*  wa_BDCDATA-DYNBEGIN = 'X'.
  IT_BDCDATA-PROGRAM  = PROGRAM.
  IT_BDCDATA-DYNPRO   = DYNPRO.
  IT_BDCDATA-DYNBEGIN = 'X'.
  APPEND IT_BDCDATA.
*  APPEND wa_BDCDATA to IT_BDCDATA.
ENDFORM.                    "BDC_DYNPRO

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM BDC_FIELD USING FNAM FVAL.

  CLEAR IT_BDCDATA.
*  wa_BDCDATA-FNAM = FNAM.
*  wa_BDCDATA-FVAL = FVAL.
*  APPEND wa_BDCDATA to IT_BDCDATA.
  IT_BDCDATA-FNAM = FNAM.
  IT_BDCDATA-FVAL = FVAL.
  APPEND  IT_BDCDATA.

ENDFORM.                    "BDC_FIELD

*FORM FORMAT_MESSAGE.
*DATA: L_MSG(100).
*  LOOP AT I_BDCMSG.
**  READ TABLE I_BDCMSG index 1.
*
*
*    CALL FUNCTION 'FORMAT_MESSAGE'
*         EXPORTING
*              ID        = I_BDCMSG-MSGID
*              LANG      = SY-LANGU
*              NO        = I_BDCMSG-MSGNR
*              V1        = I_BDCMSG-MSGV1
*              V2        = I_BDCMSG-MSGV2
*              V3        = I_BDCMSG-MSGV3
*              V4        = I_BDCMSG-MSGV4
*         IMPORTING
*              MSG       = L_MSG
*         EXCEPTIONS
*              NOT_FOUND = 1
*              OTHERS    = 2.
*    IF SY-SUBRC <> 0.
*MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*        WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ENDIF.
*    write:/ l_msg.
*  ENDLOOP.
*ENDFORM.                    " FORMAT_MESSAGE
FORM ERROR.
  DATA: L_MSG(100).
  LOOP AT I_BDCMSG.
    CALL FUNCTION 'FORMAT_MESSAGE'
      EXPORTING
        ID        = I_BDCMSG-MSGID
        LANG      = SY-LANGU
        NO        = I_BDCMSG-MSGNR
        V1        = I_BDCMSG-MSGV1
        V2        = I_BDCMSG-MSGV2
        V3        = I_BDCMSG-MSGV3
        V4        = I_BDCMSG-MSGV4
      IMPORTING
        MSG       = L_MSG
      EXCEPTIONS
        NOT_FOUND = 1
        OTHERS    = 2.


    IF I_BDCMSG-MSGTYP  = 'E'.
*    LOOP AT I_BDCMSG to wa_message.
*
*
*WA_FINAL-STATUS = 'Success'.

      WA_FINAL2-STATUS = I_BDCMSG-MSGTYP.
      WA_FINAL2-FIELD  = MESSTAB-FLDNAME.
      CONCATENATE WA_FINAL-KUNNR 'Ended with Error' INTO WA_FINAL2-MESSAGE SEPARATED BY SPACE.
      WA_FINAL2-FIELD = I_BDCMSG-MSGV1.
      WA_FINAL2-FIELD1 = I_BDCMSG-MSGV2.
*    wa_final-field2 = I_BDCMSG-MSGV3.
*    wa_final-field3 = I_BDCMSG-MSGV4.

*ENDLOOP.
    ELSE.


      WA_FINAL2-STATUS =  I_BDCMSG-MSGTYP.
      CONCATENATE WA_FINAL-KUNNR 'Saved Successfully' INTO WA_FINAL2-MESSAGE SEPARATED BY SPACE.
      WA_FINAL2-FIELD = I_BDCMSG-MSGV1.
      WA_FINAL2-FIELD1 = I_BDCMSG-MSGV2.
    ENDIF.

  ENDLOOP.
ENDFORM.                    "error
