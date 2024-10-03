*&---------------------------------------------------------------------*
*& Report  ZFI_OPENITEMS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ZFI_OPENITEMS.

TYPE-POOLS: SLIS.
tables:BSID,kna1.
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
WA_KNVV TYPE GS_knvv.

TYPES: BEGIN OF GS_vbrk,
 vbeln TYPE vbrk-vbeln,                    " Customer Code
 zterm TYPE vbrk-zterm,
 knumv TYPE vbrk-knumv,                    " Customer Name
 END OF GS_vbrk.

DATA: BEGIN OF it_date OCCURS 0,
      date  TYPE sy-datum ,
      END OF it_date .

      data: date type sy-datum,
            day type i,
            day1 type i.
      types: begin of zdn_cal1,
        region type  zdn_cal-region,
        sales_office type zdn_cal-sales_office,
        holiday type zdn_cal-holiday,

        end of zdn_cal1.
            data:  wa_holiday   type     ZDN_CAL1,
              it_holiday type table of zdn_cal1.



 DATA: GT_vbrk TYPE TABLE OF GS_vbrk,
WA_vbrk TYPE GS_vbrk.

 TYPES: BEGIN OF GS_Konv,
 knumv TYPE konv-knumv,
 kposn TYPE konv-kposn,                    " Customer Code
 kschl TYPE konv-kschl,
 kawrt TYPE konv-kawrt,
 kbetr TYPE konv-kbetr,
 kwert TYPE konv-kwert,                    " Customer Name
 END OF GS_Konv.
DATA: GT_konv TYPE TABLE OF GS_konv,
WA_konv TYPE GS_konv.

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
knumv type vbrk-knumv,
blart type bsid-blart,
ZDYAYS TYPE I,
posnr type konv-kposn,
kschl type konv-kschl,
 kawrt type konv-kawrt,
 kbetr type konv-kbetr,
  kwert type konv-kwert,
  wkreg type vbrp-wkreg,
  vkbur type vbrp-vkbur,
  name1 type kna1-name1,
*  vat(20) type c,
*  cashd(20) type c,
*  Total(20) type c,
*  totvat(20) type c,
*  totcashd(20) type c,
*  totTotal(20) type c,

  vat type p,
  cashd type p,
  total type p,
  totvat type p,
  totcashd type p,
  tottotal type p,
 SAMPLE(2) TYPE C,
AUFNR TYPE AUFNR,
*  kunnr type bsid-kunnr,
END OF GS_FINAL.

DATA: GT_FINAL TYPE TABLE OF GS_FINAL,
WA_FINAL TYPE GS_FINAL.
types:begin of custname,
  kunnr type kna1-kunnr,
  name1 type kna1-name1,
  END OF custname.

data: it_cust type table of custname,
      wa_cust type custname.
data:
      b(20) type c,
      c(20) type c,
      d(20) type c,
      e(20) type c,
      f(20) type c,
      g(20) type c,
      h(20) type c,
      i(20) type c.


data: l1(20) type c,
      l2(20) type c.

data: t1(20) type c value 0,
      t2(20) type c value 0,
      t3(20) type c value 0,
      XBLNR1 type bsid-xblnr,
      XBLNR2 type bsid-xblnr.


 data: kawrt1 type konv-kawrt,
       kwert1 type konv-kwert,
       kawrt2 type konv-kawrt,
        kawrt3 type konv-kawrt,
         kawrt4 type konv-kawrt,
          kawrt5 type konv-kawrt,
           kawrt6 type konv-kawrt,
           kawrt7 type konv-kawrt,
           kawrt8 type konv-kawrt,
           kawrt9 type konv-kawrt,
           kawrt10 type konv-kawrt,
       kwert2 type konv-kwert,
       kbetr1 type konv-kbetr,
        kbetr2 type konv-kbetr,
         kbetr3 type konv-kbetr,
          kbetr4 type konv-kbetr.
data: date1 type datum.

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

SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:      SO_BUKRS FOR OR_BUKRS default 4000 OBLIGATORY."MODIF ID MOD .
*SELECT-OPTIONS:      SO_Zterm FOR or_Zterm MODIF ID MOD .
SELECT-OPTIONS:      SO_VKBUR FOR OR_VKBUR MODIF ID MOD OBLIGATORY . " NO-DISPLAY.
SELECT-OPTIONS:      SO_kunnr FOR bsid-kunnr . "MODIF ID MOD .
PARAMETERS: P_DATE TYPE SY-DATUM DEFAULT SY-DATUM OBLIGATORY.

SELECTION-SCREEN: END OF BLOCK B1.

START-OF-SELECTION.
date1 =  P_DATE - 200.
  SELECT
     KUNNR
     VKBUR
       FROM KNVV INTO TABLE GT_KNVV WHERE VKBUR IN SO_VKBUR .

*select kunnr name1 from kna1 into table it_cust where kunnr in so_kunnr.
*  SELECT BUKRS KUNNR BELNR DMBTR BLDAT ZTERM FROM BSID INTO CORRESPONDING FIELDS OF TABLE GT_FINAL FOR ALL ENTRIES IN GT_KNVV WHERE
*   KUNNR =  GT_KNVV-KUNNR AND BUKRS = '4000' AND
*         UMSKZ NE 'F' AND  ZFBDT <= P_DATE.
SELECT a~BUKRS a~KUNNR a~XBLNR a~DMBTR a~BLDAT a~blart a~ZTERM   c~posnr  b~knumv c~wkreg  c~vkbur FROM BSID as a  INNER JOIN vbrk as b on a~Xblnr = b~vbeln and b~fksto = ' '"#EC CI_DB_OPERATION_OK[2768887] "Added by SPLABAP during code remediation
   INNER JOIN vbrp as c on b~vbeln  = c~vbeln inner join kna1 as d on a~KUNNR = d~kunnr"#EC CI_DB_OPERATION_OK[2768887] "Added by SPLABAP during code remediation

   INTO CORRESPONDING FIELDS OF TABLE GT_FINAL  WHERE
         a~KUNNR in so_kunnr and c~vkbur in so_vkbur and a~BUKRS eq '4000' and ZFBDT <= P_DATE and  ZFBDT > date1 and a~blart = 'RV' . "and a~zterm  = 'RPPD' or a~zterm  = 'CD'."a~zterm in so_zterm ."and a~blart = 'RV' and a~blart = 'VG'.
*
*SELECT BUKRS KUNNR BELNR DMBTR BLDAT blart ZTERM FROM BSID INTO CORRESPONDING FIELDS OF TABLE GT_FINAL  WHERE
*   BUKRS in so_bukrs and ZFBDT <= P_DATE and zterm in so_zterm.


*  if sy-subrc = 0.
*
**select vbeln zterm knumv from vbrk into CORRESPONDING FIELDS OF TABLE gt_vbrk   where
**    zterm in so_zterm.
*  select a~vbeln a~zterm a~knumv from vbrk as a INNER JOIN bsid as b on a~vbeln = b~belnr into CORRESPONDING FIELDS OF TABLE gt_vbrk where
*    a~zterm in so_zterm and vkorg = '4000'.

   if sy-subrc = 0.
     sort gt_final ascending.
     select knumv kposn kschl kawrt kbetr kwert from PRCD_ELEMENTS into CORRESPONDING FIELDS OF TABLE gt_konv for all entries in
       gt_final where knumv = gt_final-knumv ."and "kschl = 'JIVP'. " and kschl = 'JIVA' and kschl = 'JVSR' and kschl = 'JIVC'.

*     endif.

    else.
      MESSAGE 'Nodata for selected company code' type 'E'.
      endif.
*        UMSKZ NE 'F' AND  ZFBDT <= P_DATE.

select region sales_office  holiday from ZDN_CAL  into  table it_holiday  .
  delete gt_final where zterm ne 'CD' and zterm ne 'RPPD'.
* delete gt_konv where kschl ne 'JIVA' and kschl ne 'JIVC' and kschl ne 'JVSR' and kschl ne 'JIVP' and kschl ne 'JIVC'.

  sort gt_final by xblnr posnr.
 sort  gt_konv ASCENDING by kschl knumv kposn.
 sort it_holiday ascending by region sales_office holiday.

  LOOP AT GT_FINAL INTO WA_FINAL.


    DATE = WA_FINAL-BLDAT + 1.
    IT_DATE-DATE =  DATE.
*APPEND it_date.

    if wa_final-zterm = 'CD' or  wa_final-zterm = 'RPPD' .
    DO.

      IF IT_DATE-DATE  = P_DATE.
        EXIT.
      ENDIF.


      READ TABLE IT_HOLIDAY   INTO WA_HOLIDAY WITH KEY  REGION = WA_FINAL-WKREG  sales_office = wa_final-vkbur HOLIDAY = IT_DATE-DATE BINARY SEARCH .

      IF SY-SUBRC = 0.
        DAY = DAY + 1.
      ENDIF.
      IT_DATE-DATE =  IT_DATE-DATE + 1.
*  APPEND it_date.
      CLEAR :WA_HOLIDAY.
    ENDDO.
endif.
    DAY1 = SY-DATUM - DATE.
    WA_FINAL-ZDYAYS = DAY1 - DAY.
    SELECT SINGLE  KUNNR NAME1 FROM KNA1 INTO WA_CUST WHERE KUNNR = WA_FINAL-KUNNR .
    IF SY-SUBRC = 0.

      WA_FINAL-NAME1 = WA_CUST-NAME1.
      CLEAR WA_CUST.
    ENDIF.
 SELECT   AUFNR FROM AUFK INTO WA_FINAL-AUFNR UP TO 1 ROWS  WHERE  WERKS = WA_FINAL-VKBUR
   ORDER BY PRIMARY KEY.  "Added by SPLABAP during code remediation
      ENDSELECT.
      IF SY-SUBRC = 0.
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
    READ TABLE GT_KONV INTO WA_KONV WITH KEY KSCHL = 'ZCAD' KNUMV = WA_FINAL-KNUMV  KPOSN = WA_FINAL-POSNR BINARY SEARCH.
*select  single kschl kawrt kbetr kwert from konv into wa_konv  where kschl = 'ZCAD'  and knumv = wa_final-knumv and kposn = WA_FINAL-posnr .


    IF SY-SUBRC = 0.
      WA_FINAL-KSCHL = WA_KONV-KSCHL.
      WA_FINAL-KAWRT = WA_KONV-KAWRT.
      WA_FINAL-KBETR = WA_KONV-KBETR.
      WA_FINAL-KWERT = WA_KONV-KWERT.

      IF   WA_FINAL-ZDYAYS > 19 AND WA_FINAL-ZTERM = 'CD'.
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

        READ TABLE GT_KONV INTO WA_KONV WITH KEY KSCHL = 'JIVA' KNUMV = WA_FINAL-KNUMV  KPOSN = WA_FINAL-POSNR binary search.
        IF SY-SUBRC = 0.

           KBETR2 = WA_KONV-KBETR / 1 .
          KAWRT3 =   ( ( KAWRT1 * KBETR2 ) / 100 ) .
          C =  KAWRT1 - WA_KONV-KAWRT  .
          KAWRT7 = WA_KONV-KAWRT + WA_KONV-KWERT.
          G = KAWRT3 - WA_KONV-KWERT.

        ENDIF.

        READ TABLE GT_KONV INTO WA_KONV WITH KEY KSCHL = 'JVSR' KNUMV = WA_FINAL-KNUMV  KPOSN = WA_FINAL-POSNR binary search.
        IF SY-SUBRC = 0.
           KBETR3 = WA_KONV-KBETR / 1 .
          KAWRT4 =  ( ( KAWRT1 * KBETR3 ) / 100 ) .
          D =  KAWRT1 - WA_KONV-KAWRT  .
          KAWRT8 = WA_KONV-KAWRT + WA_KONV-KWERT.
          H = KAWRT4 - WA_KONV-KWERT.
        ENDIF.

        READ TABLE GT_KONV INTO WA_KONV WITH KEY KSCHL = 'JIVC' KNUMV = WA_FINAL-KNUMV  KPOSN = WA_FINAL-POSNR binary search.
        IF SY-SUBRC = 0.
           KBETR4 = WA_KONV-KBETR / 1 .
          KAWRT5 =  ( ( KAWRT1 * KBETR4 ) / 100 ) .
          E =  KAWRT1 - WA_KONV-KAWRT  .
          KAWRT9 = WA_KONV-KAWRT + WA_KONV-KWERT.
          I = KAWRT5 - WA_KONV-KWERT.
          WA_FINAL-SAMPLE = 'AB'.
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

 IF   WA_FINAL-ZDYAYS > 42 AND WA_FINAL-ZTERM = 'CD'.
        KWERT1  =  ( WA_FINAL-KAWRT * 2 ) / 100 .

        KAWRT1 =  WA_FINAL-KAWRT - KWERT1.

        READ TABLE GT_KONV INTO WA_KONV WITH KEY KSCHL = 'JIVP' KNUMV = WA_FINAL-KNUMV  KPOSN = WA_FINAL-POSNR binary search.
        IF SY-SUBRC = 0.

           KBETR1 = WA_KONV-KBETR / 1 .
          KAWRT2 = ( ( KAWRT1 * KBETR1 ) / 100 ) .
          B =  KAWRT1 - WA_KONV-KAWRT  .
          KAWRT6 = WA_KONV-KAWRT + WA_KONV-KWERT.
          F = KAWRT2 - WA_KONV-KWERT.
        ENDIF.

        READ TABLE GT_KONV INTO WA_KONV WITH KEY KSCHL = 'JIVA' KNUMV = WA_FINAL-KNUMV  KPOSN = WA_FINAL-POSNR binary search.
        IF SY-SUBRC = 0.

           KBETR2 = WA_KONV-KBETR / 1 .
          KAWRT3 =   ( ( KAWRT1 * KBETR2 ) / 100 ) .
          C =  KAWRT1 - WA_KONV-KAWRT  .
          KAWRT7 = WA_KONV-KAWRT + WA_KONV-KWERT.
          G = KAWRT3 - WA_KONV-KWERT.

        ENDIF.

        READ TABLE GT_KONV INTO WA_KONV WITH KEY KSCHL = 'JVSR' KNUMV = WA_FINAL-KNUMV  KPOSN = WA_FINAL-POSNR binary search.
        IF SY-SUBRC = 0.
           KBETR3 = WA_KONV-KBETR / 1 .
          KAWRT4 =  ( ( KAWRT1 * KBETR3 ) / 100 ) .
          D =  KAWRT1 - WA_KONV-KAWRT  .
          KAWRT8 = WA_KONV-KAWRT + WA_KONV-KWERT.
          H = KAWRT4 - WA_KONV-KWERT.
        ENDIF.

        READ TABLE GT_KONV INTO WA_KONV WITH KEY KSCHL = 'JIVC' KNUMV = WA_FINAL-KNUMV  KPOSN = WA_FINAL-POSNR binary search.
        IF SY-SUBRC = 0.
           KBETR4 = WA_KONV-KBETR / 1 .
          KAWRT5 =  ( ( KAWRT1 * KBETR4 ) / 100 ) .
          E =  KAWRT1 - WA_KONV-KAWRT  .
          KAWRT9 = WA_KONV-KAWRT + WA_KONV-KWERT.
          I = KAWRT5 - WA_KONV-KWERT.
          WA_FINAL-SAMPLE = 'AB'.
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
       if ( B = D ).
         L1 =  C + E.
         endif.

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

      IF   WA_FINAL-ZDYAYS > 42 AND WA_FINAL-ZTERM = 'RPPD'.
        KWERT1  =  ( WA_FINAL-KAWRT * 0 ) / 100 .

        KAWRT1 =  WA_FINAL-KAWRT - KWERT1.

        READ TABLE GT_KONV INTO WA_KONV WITH KEY KSCHL = 'JIVP' KNUMV = WA_FINAL-KNUMV  KPOSN = WA_FINAL-POSNR binary search.
        IF SY-SUBRC = 0.

           KBETR1 = WA_KONV-KBETR / 1 .
          KAWRT2 = ( ( KAWRT1 * KBETR1 ) / 100 ) .
          B =  KAWRT1 - WA_KONV-KAWRT  .
          KAWRT6 = WA_KONV-KAWRT + WA_KONV-KWERT.
          F = KAWRT2 - WA_KONV-KWERT.
        ENDIF.

        READ TABLE GT_KONV INTO WA_KONV WITH KEY KSCHL = 'JIVA' KNUMV = WA_FINAL-KNUMV  KPOSN = WA_FINAL-POSNR binary search.
        IF SY-SUBRC = 0.

           KBETR2 = WA_KONV-KBETR / 1 .
          KAWRT3 =   ( ( KAWRT1 * KBETR2 ) / 100 ) .
          C =  KAWRT1 - WA_KONV-KAWRT  .
          KAWRT7 = WA_KONV-KAWRT + WA_KONV-KWERT.
          G = KAWRT3 - WA_KONV-KWERT.

        ENDIF.

        READ TABLE GT_KONV INTO WA_KONV WITH KEY KSCHL = 'JVSR' KNUMV = WA_FINAL-KNUMV  KPOSN = WA_FINAL-POSNR binary search.
        IF SY-SUBRC = 0.
           KBETR3 = WA_KONV-KBETR / 1 .
          KAWRT4 =  ( ( KAWRT1 * KBETR3 ) / 100 ) .
          D =  KAWRT1 - WA_KONV-KAWRT  .
          KAWRT8 = WA_KONV-KAWRT + WA_KONV-KWERT.
          H = KAWRT4 - WA_KONV-KWERT.
        ENDIF.

        READ TABLE GT_KONV INTO WA_KONV WITH KEY KSCHL = 'JIVC' KNUMV = WA_FINAL-KNUMV  KPOSN = WA_FINAL-POSNR binary search.
        IF SY-SUBRC = 0.
           KBETR4 = WA_KONV-KBETR / 1 .
          KAWRT5 =  ( ( KAWRT1 * KBETR4 ) / 100 ) .
          E =  KAWRT1 - WA_KONV-KAWRT  .
          KAWRT9 = WA_KONV-KAWRT + WA_KONV-KWERT.
          I = KAWRT5 - WA_KONV-KWERT.
          WA_FINAL-SAMPLE = 'AB'.
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

          IF  ( B = E ) OR  ( B = C ) .
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
 DELETE GT_FINAL WHERE TOTAL IS INITIAL.


* DELETE GT_FINAL1 WHERE KSCHL IS INITIAL.
*  DELETE GT_FINAL1 WHERE ZTERM NE 'RPPD' AND  ZTERM NE 'CD' .


  WA_FIELDCAT-FIELDNAME   = 'BUKRS'.
  WA_FIELDCAT-SELTEXT_M   = 'Company code'.
  WA_FIELDCAT-COL_POS     = 1.
  WA_FIELDCAT-TABNAME =  'GT_FINAL' .
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR  WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME   = 'VKBUR'.
  WA_FIELDCAT-SELTEXT_M   = 'Sale Office'.
  WA_FIELDCAT-COL_POS     = 2.
  WA_FIELDCAT-TABNAME =  'GT_FINAL' .
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR  WA_FIELDCAT.



  WA_FIELDCAT-FIELDNAME   = 'WKREG'.
  WA_FIELDCAT-SELTEXT_M   = 'Region Name'.
  WA_FIELDCAT-COL_POS     = 3.
  WA_FIELDCAT-TABNAME =  'GT_FINAL' .
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR  WA_FIELDCAT.


  WA_FIELDCAT-FIELDNAME   = 'KUNNR'.
  WA_FIELDCAT-SELTEXT_M   = 'Customer Code'.
  WA_FIELDCAT-COL_POS     = 4.
  WA_FIELDCAT-TABNAME =  'GT_FINAL' .
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR  WA_FIELDCAT.
*
  WA_FIELDCAT-FIELDNAME   = 'NAME1'.
  WA_FIELDCAT-SELTEXT_M   = 'Customer Name'.
  WA_FIELDCAT-COL_POS     = 5.
  WA_FIELDCAT-TABNAME =  'GT_FINAL' .
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR  WA_FIELDCAT.

*  WA_FIELDCAT-FIELDNAME   = 'DMBTR1'.
*  WA_FIELDCAT-SELTEXT_M   = 'Receivable Amt'.
*  WA_FIELDCAT-COL_POS     = 4.
*  WA_FIELDCAT-TABNAME =  'GT_FINAL1' .
*  APPEND WA_FIELDCAT TO IT_FIELDCAT.
*  CLEAR  WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME   = 'XBLNR'.
  WA_FIELDCAT-SELTEXT_M   = 'Billing Doc. Number'.
  WA_FIELDCAT-COL_POS     = 6.
  WA_FIELDCAT-TABNAME =  'GT_FINAL' .
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR  WA_FIELDCAT.

*  WA_FIELDCAT-FIELDNAME   = 'DMBTR'.
*  WA_FIELDCAT-SELTEXT_M   = 'Amount in Local Currency'.
*  WA_FIELDCAT-COL_POS     = 4.
*  WA_FIELDCAT-TABNAME =  'GT_FINAL' .
*  APPEND WA_FIELDCAT TO IT_FIELDCAT.
*  CLEAR  WA_FIELDCAT.
 WA_FIELDCAT-FIELDNAME   = 'POSNR'.
  WA_FIELDCAT-SELTEXT_M   = 'Line items'.
  WA_FIELDCAT-COL_POS     = 7.
  WA_FIELDCAT-TABNAME =  'GT_FINAL' .
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR  WA_FIELDCAT.



  WA_FIELDCAT-FIELDNAME   = 'KAWRT'.
  WA_FIELDCAT-SELTEXT_M   = 'Condition Base Value'.
  WA_FIELDCAT-COL_POS     = 8.
  WA_FIELDCAT-TABNAME =  'GT_FINAL' .
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR  WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME   = 'KBETR'.
  WA_FIELDCAT-SELTEXT_M   = 'Payment Term %'.
  WA_FIELDCAT-COL_POS     = 9.
  WA_FIELDCAT-TABNAME =  'GT_FINAL' .
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR  WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME   = 'KWERT'.
  WA_FIELDCAT-SELTEXT_M   = ' Discount Value'.
  WA_FIELDCAT-COL_POS     = 10.
  WA_FIELDCAT-TABNAME =  'GT_FINAL' .
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR  WA_FIELDCAT.



  WA_FIELDCAT-FIELDNAME   = 'BLDAT'.
  WA_FIELDCAT-SELTEXT_M   = 'Billing Date'.
  WA_FIELDCAT-COL_POS     = 11.
  WA_FIELDCAT-TABNAME =  'GT_FINAL' .
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR  WA_FIELDCAT.



 WA_FIELDCAT-FIELDNAME   = 'ZTERM'.
  WA_FIELDCAT-SELTEXT_M   = 'Payment Terms'.
  WA_FIELDCAT-COL_POS     = 12.
  WA_FIELDCAT-TABNAME =  'GT_FINAL' .
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR  WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME   = 'ZDYAYS'.
  WA_FIELDCAT-SELTEXT_M   = 'No of Days'.
  WA_FIELDCAT-COL_POS     = 13.
  WA_FIELDCAT-TABNAME =  'GT_FINAL' .
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR  WA_FIELDCAT.

   WA_FIELDCAT-FIELDNAME   = 'TOTAL'.
  WA_FIELDCAT-SELTEXT_M   = 'Debit Note amount'.
  WA_FIELDCAT-COL_POS     = 14.
  WA_FIELDCAT-TABNAME =  'GT_FINAL' .
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR  WA_FIELDCAT.

WA_FIELDCAT-FIELDNAME   = 'VAT'.
  WA_FIELDCAT-SELTEXT_M   = 'Tax Amount'.
  WA_FIELDCAT-COL_POS     = 15.
  WA_FIELDCAT-TABNAME =  'GT_FINAL' .
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR  WA_FIELDCAT.


  WA_FIELDCAT-FIELDNAME   = 'CASHD'.
  WA_FIELDCAT-SELTEXT_M   = 'cash discount'.
  WA_FIELDCAT-COL_POS     = 16.
  WA_FIELDCAT-TABNAME =  'GT_FINAL' .
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR  WA_FIELDCAT.
* WA_FIELDCAT-FIELDNAME   = 'WKREG'.
*  WA_FIELDCAT-SELTEXT_M   = 'Region'.
*  WA_FIELDCAT-COL_POS     = 13.
*  WA_FIELDCAT-TABNAME =  'GT_FINAL' .
*  APPEND WA_FIELDCAT TO IT_FIELDCAT.
*  CLEAR  WA_FIELDCAT.


  GD_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.

  GD_LAYOUT-COLTAB_FIELDNAME = 'CELLCOLOR'.  "CTAB_FNAME

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
   EXPORTING
*   I_INTERFACE_CHECK                 = ' '
*   I_BYPASSING_BUFFER                = ' '
*   I_BUFFER_ACTIVE                   = ' '
     I_CALLBACK_PROGRAM                 = SY-REPID
*   I_CALLBACK_PF_STATUS_SET          = ' '
*   I_CALLBACK_USER_COMMAND           = ' '
     I_CALLBACK_TOP_OF_PAGE            = 'ALV_CATALOG_HEADER'
*   I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*   I_CALLBACK_HTML_END_OF_LIST       = ' '
*   I_STRUCTURE_NAME                  =
*   I_BACKGROUND_ID                   = ' '
*   I_GRID_TITLE                      =
*   I_GRID_SETTINGS                   =
*   IS_LAYOUT                         =  GD_LAYOUT
     IT_FIELDCAT                        = IT_FIELDCAT
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
      T_OUTTAB                          = GT_FINAL[]
* EXCEPTIONS
*   PROGRAM_ERROR                     = 1
*   OTHERS                            = 2
            .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.


*&---------------------------------------------------------------------*
*&      Form  ALV_CATALOG_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ALV_CATALOG_HEADER.
  DATA : LIT_HEADER TYPE  SLIS_T_LISTHEADER,
       LS_LINE TYPE SLIS_LISTHEADER.
  CLEAR LS_LINE.
  LS_LINE-TYP  = 'H'.
  LS_LINE-KEY = ' '.
  LS_LINE-INFO = 'Automatic Debit Process' .
  APPEND LS_LINE TO LIT_HEADER.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = LIT_HEADER
      I_LOGO             = 'ZLOGO'.

ENDFORM.                    "ALV_CATALOG_HEADER
