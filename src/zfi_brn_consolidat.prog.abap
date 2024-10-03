*&---------------------------------------------------------------------*
*& Report  ZVEN_CUST_AGEREP
*&
*&---------------------------------------------------------------------*
*&Functional                   : Govindarajan.M                        *
*& Developer                   : RAMACHANDRAN.M                        *
*& Created On                  : 03 June  2014                         *
*& Company                     : Sheenlac Paints Pvt. Ltd              *
*& Title                       : BRANCHWISE CONSOLIDATED OUTSTANDING & CLOSING STOCK STATEMENT            *
*& Report Name                 : ZFI_BRN_CONSOLIDAT                      *
*& Development Id              : kpabap                                *
*& Solman call No              :                                       *
*& Transport Request           :                                       *
*& Related Information         : CONSOLIDATED OUTSTANDING & CLOSING STOCK           *
*&---------------------------------------------------------------------*

REPORT ZFI_BRN_CONSOLIDAT.

TYPE-POOLS: SLIS.

*&---------------------------------------------------------------------*
*& Structure & Internal  Table For Vendor Decleration
*&---------------------------------------------------------------------*



DATA: LV_ZFAEDT TYPE SY-DATUM,
      LV_NDUEAMT TYPE P DECIMALS 2,
      T_DAYS TYPE I,
      TOT_AMNT TYPE P DECIMALS 2,
      A1_AMNT TYPE P DECIMALS 2,
      A2_AMNT TYPE P DECIMALS 2,
      A3_AMNT TYPE P DECIMALS 2,
      A4_AMNT TYPE P DECIMALS 2,
      A5_AMNT TYPE P DECIMALS 2,
      A6_AMNT TYPE P DECIMALS 2.

DATA: OR_LIFNR TYPE LFA1-LIFNR,
      OR_PRCTR TYPE CEPC-PRCTR,
      OR_KTOKK TYPE T077K-KTOKK.

DATA : L_PRCTR TYPE CEPC-PRCTR ,
       L_LIFNR TYPE LFA1-LIFNR ,
       L_KTOKK TYPE T077K-KTOKK .

TYPES: BEGIN OF GS_FAGLFLEXA,
       DOCNR TYPE FAGLFLEXA-DOCNR,
       PRCTR TYPE FAGLFLEXA-PRCTR,
       BSCHL TYPE FAGLFLEXA-BSCHL,
       END OF GS_FAGLFLEXA.

DATA: GT_FAGLFLEXA TYPE TABLE OF GS_FAGLFLEXA,
      WA_FAGLFLEXA TYPE GS_FAGLFLEXA,
      GT_FAGLFLEXA1 TYPE TABLE OF GS_FAGLFLEXA,
      WA_FAGLFLEXA1 TYPE GS_FAGLFLEXA.


DATA: LV_DATE TYPE SY-DATUM,
      LV_DOCNR TYPE FAGLFLEXA-DOCNR.

*&---------------------------------------------------------------------*
*& Structure & Internal  Table For Customer Decleration
*&---------------------------------------------------------------------*
TYPES: BEGIN OF GS_KNA1,
       KUNNR TYPE KNA1-KUNNR,                    " Customer Code
       NAME1 TYPE KNA1-NAME1,                    " Customer Name
       END OF GS_KNA1.

DATA: GT_KNA1 TYPE TABLE OF GS_KNA1,
      WA_KNA1 TYPE GS_KNA1.

TYPES: BEGIN OF GS_KNVV,
       KUNNR TYPE KNVV-KUNNR,                    " Customer Code
       VKBUR TYPE KNVV-VKBUR,                    " Customer Name
       END OF GS_KNVV.

DATA: GT_KNVV TYPE TABLE OF GS_KNVV,
      WA_KNVV TYPE GS_KNVV.

TYPES: BEGIN OF GS_BSID,
       BUKRS TYPE BSID-BUKRS,                    " Company Code
       AUGDT TYPE BSIK-AUGDT,	                   " Clearing Date
       AUGBL TYPE BSIK-AUGBL,	                   " Document Number of the Clearing Document
       KUNNR TYPE BSID-KUNNR,                    " Customer Code
       BELNR TYPE BSIK-BELNR,                    " Accounting Document Number
       DMBTR TYPE BSID-DMBTR,                    " Bill Amount ( In Doc Currency )
       BUDAT TYPE BSID-BUDAT,
       BLDAT TYPE BSID-BLDAT,
       ZFBDT TYPE BSID-ZFBDT,                    " Baseline Due Date For Calculation
       ZBD1T TYPE BSID-ZBD1T,                    " Credit Days
       PRCTR TYPE BSID-PRCTR,                    " Profit Center
       SHKZG TYPE BSID-SHKZG,                    " Debit/Credit Indicator
       END OF GS_BSID.

TYPES: BEGIN OF GS_T001W,
       WERKS TYPE T001W-WERKS,
       BNAME TYPE T001W-NAME1,
      END OF GS_T001W.

DATA: GT_T001W TYPE TABLE OF GS_T001W,
      WA_T001W TYPE GS_T001W .

TYPES: BEGIN OF GS_BSAD,
       BUKRS TYPE BSAD-BUKRS,                    " Company Code
       AUGDT TYPE BSAD-AUGDT,	                   " Clearing Date
       AUGBL TYPE BSAD-AUGBL,	                   " Document Number of the Clearing Document
       KUNNR TYPE BSAD-KUNNR,                    " Customer Code
       BELNR TYPE BSAD-BELNR,                    " Accounting Document Number
       DMBTR TYPE BSAD-DMBTR,                    " Bill Amount ( In Doc Currency )
       BUDAT TYPE BSAD-BUDAT,
       BLDAT TYPE BSAD-BLDAT,
       ZFBDT TYPE BSAD-ZFBDT,                    " Baseline Due Date For Calculation
       ZBD1T TYPE BSAD-ZBD1T,                    " Credit Days
       PRCTR TYPE BSAD-PRCTR,                    " Profit Center
       SHKZG TYPE BSAD-SHKZG,                    " Debit/Credit Indicator
       END OF GS_BSAD.


DATA: GT_BSID TYPE TABLE OF GS_BSID,
      WA_BSID TYPE GS_BSID,
      GT_BSAD TYPE TABLE OF GS_BSAD,
      WA_BSAD TYPE GS_BSAD.

TYPES: BEGIN OF GS_FINAL,
       VKBUR TYPE KNVV-VKBUR,
       KUNNR TYPE KNA1-KUNNR,                    " Customer Code
       NAME1 TYPE KNA1-NAME1,                    " Customer Name
       DMBTR TYPE BSID-DMBTR,                    " Total Bill Amount ( In Doc Currency )

       BNAME TYPE T001W-NAME1,

       DMBTR1 TYPE BSID-DMBTR,                    " Total Bill Amount ( In Doc Currency )

       ZA1_AMT1 TYPE BSID-DMBTR,                  " A5-Amount Due – Above 180  Days
       ZA2_AMT2 TYPE BSID-DMBTR,                  " A5-Amount Due – Above 180  Days
       ZA3_AMT3 TYPE BSID-DMBTR,                  " A5-Amount Due – Above 180  Days
       ZA4_AMT4 TYPE BSID-DMBTR,                  " A5-Amount Due – Above 180  Days
       ZA5_AMT5 TYPE BSID-DMBTR,                  " A5-Amount Due – Above 180  Days
       ZA6_AMT6 TYPE BSID-DMBTR,                  " A5-Amount Due – Above 180  Days

      ZNDUE_AMT1 TYPE BSID-DMBTR,                " Bill Amount – Not Due

       ZNDUE_AMT TYPE BSID-DMBTR,                " Bill Amount – Not Due
       ZA1_AMT TYPE BSID-DMBTR,                  " A1-Amount Due Less Than 30 Days
       ZA2_AMT TYPE BSID-DMBTR,                  " A2-Amount Due Between 31 – 60 Days
       ZA3_AMT TYPE BSID-DMBTR,                  " A3-Amount Due Between 61 – 90 Days
       ZA4_AMT TYPE BSID-DMBTR,                  " A4-Amount Due Between 91 - 180  Days
       ZA5_AMT TYPE BSID-DMBTR,                  " A5-Amount Due – Above 180  Days
       ZA6_AMT TYPE BSID-DMBTR,                  " A5-Amount Due – Above 180  Days
        V_COUNT TYPE I,

        V_WERKCOUNT TYPE I,

        ZA1_PER TYPE P DECIMALS 2 ,
        ZA2_PER TYPE P DECIMALS 2 ,
        ZA3_PER TYPE P DECIMALS 2 ,
        ZA4_PER TYPE P DECIMALS 2 ,
        ZA5_PER TYPE P DECIMALS 2 ,
        ZA6_PER TYPE P DECIMALS 2 ,
        ZNDUE_PER TYPE P DECIMALS 2 ,

       END OF GS_FINAL.

TYPES: BEGIN OF GS_FINAL1,
       VKBUR TYPE KNVV-VKBUR,
*       KUNNR TYPE KNA1-KUNNR,                    " Customer Code
*       NAME1 TYPE KNA1-NAME1,                    " Customer Name
       DMBTR TYPE BSID-DMBTR,                    " Total Bill Amount ( In Doc Currency )
       ZNDUE_AMT TYPE BSID-DMBTR,                " Bill Amount – Not Due
       ZA1_AMT TYPE BSID-DMBTR,                  " A1-Amount Due Less Than 30 Days
       ZA2_AMT TYPE BSID-DMBTR,                  " A2-Amount Due Between 31 – 60 Days
       ZA3_AMT TYPE BSID-DMBTR,                  " A3-Amount Due Between 61 – 90 Days
       ZA4_AMT TYPE BSID-DMBTR,                  " A4-Amount Due Between 91 - 180  Days
       ZA5_AMT TYPE BSID-DMBTR,                  " A5-Amount Due – Above 180  Days
       ZA6_AMT TYPE BSID-DMBTR,                  " A5-Amount Due – Above 180  Days

       END OF GS_FINAL1.

DATA: GT_FINAL TYPE TABLE OF GS_FINAL,
      WA_FINAL TYPE GS_FINAL.

DATA: GT_FINAL1 TYPE TABLE OF GS_FINAL1,
      WA_FINAL1 TYPE GS_FINAL1.

DATA: OR_KUNNR TYPE KNA1-KUNNR.
*      OR_PRCTR TYPE CEPC-PRCTR.
DATA: L_KUNNR TYPE KNA1-KUNNR.
*     L_PRCTR TYPE CEPC-PRCTR.

DATA : LAYOUT TYPE SLIS_LAYOUT_ALV.

DATA : LV_SUM TYPE BSID-DMBTR.
DATA : LV_SUM1 TYPE BSID-DMBTR.
DATA : LV_SUM2 TYPE BSID-DMBTR.
DATA : LV_SUM3 TYPE BSID-DMBTR.
DATA : LV_SUM4 TYPE BSID-DMBTR.
DATA : LV_SUM5 TYPE BSID-DMBTR.
DATA : LV_SUM6 TYPE BSID-DMBTR.
DATA : LV_SUM8 TYPE BSID-DMBTR.  " Added by ram on 23/01/2015

DATA : LV_SUM7 TYPE BSID-DMBTR.

DATA : V_COUNT TYPE I VALUE 0.

DATA: LV_WERKCOUNT TYPE I VALUE 0.

DATA: LV_COUNT TYPE SY-TABIX.

DATA: GD_LAYOUT    TYPE SLIS_LAYOUT_ALV.

" DATA : LAYOUT TYPE SLIS_LAYOUT_ALV.

*Data declarations for ALV
DATA:        IT_FIELDCAT  TYPE TABLE OF SLIS_FIELDCAT_ALV,
      WA_FIELDCAT  LIKE LINE OF IT_FIELDCAT.
DATA: "OR_LIFNR TYPE LFA1-LIFNR,
      OR_BUKRS TYPE T001-BUKRS,
      OR_VKBUR TYPE KNVV-VKBUR,
      OR_ZTERM TYPE T052-ZTERM ,
       OR_GJAHR TYPE BSIK-GJAHR.
"OR_KTOKK TYPE T077K-KTOKK.



SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:      SO_BUKRS FOR OR_BUKRS MODIF ID MOD NO-DISPLAY.
SELECT-OPTIONS:      SO_VKBUR FOR OR_VKBUR MODIF ID MOD.
PARAMETERS: P_DATE TYPE SY-DATUM DEFAULT SY-DATUM OBLIGATORY.
SELECTION-SCREEN: END OF BLOCK B1.


********** Customer Age Wise Report****************
*
*AT SELECTION-SCREEN .
*  IF SO_PRCTR IS NOT INITIAL.
*    SELECT SINGLE PRCTR FROM CEPC INTO L_PRCTR WHERE PRCTR IN SO_PRCTR .
*    IF SY-SUBRC NE 0.
*      MESSAGE 'Enter Valid Profit Center' TYPE 'E'.
*    ENDIF.
*  ENDIF.

*
START-OF-SELECTION.
  PERFORM FATCH_DATA.
  PERFORM BUILD_FIELDCATALOG_OVERVIEW .
  PERFORM BUILD_ALV_OVERVIEW.

  PERFORM BUILD_LAYOUT .
*&---------------------------------------------------------------------*
*& Main Logic of Program
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  FATCH_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FATCH_DATA .
  " BREAK-POINT .
  SELECT
    KUNNR
    VKBUR
      FROM KNVV INTO TABLE GT_KNVV WHERE VKBUR IN SO_VKBUR .

  SELECT
     WERKS
     NAME1
      FROM T001W INTO TABLE GT_T001W FOR ALL ENTRIES IN GT_KNVV WHERE WERKS = GT_KNVV-VKBUR .

  SELECT
    BUKRS
    AUGDT
    AUGBL
    KUNNR
    BELNR
    DMBTR
    BUDAT
    BLDAT
    ZFBDT
    ZBD1T
    PRCTR
    SHKZG FROM BSID INTO CORRESPONDING FIELDS OF TABLE GT_BSID FOR ALL ENTRIES IN GT_KNVV WHERE
    KUNNR =  GT_KNVV-KUNNR AND BUKRS IN SO_BUKRS AND
          UMSKZ NE 'F' AND  ZFBDT <= P_DATE.



*  SELECT
*BUKRS
*AUGDT
*AUGBL
*AUGDT
*AUGBL
*KUNNR
*BELNR
*DMBTR
*BUDAT
*BLDAT
*ZFBDT
*ZBD1T
*PRCTR
*SHKZG FROM BSAD INTO CORRESPONDING FIELDS OF TABLE GT_BSAD
*WHERE  "BELNR = GT_FAGLFLEXA-DOCNR AND
*    BUKRS IN SO_BUKRS AND
*      UMSKZ NE 'F' AND    ZFBDT <= P_DATE.



*  IF GT_BSAD[] IS NOT INITIAL.
*    APPEND LINES OF GT_BSAD TO GT_BSID.
*  ENDIF.

  IF GT_BSID[] IS NOT INITIAL.
    SELECT
      KUNNR
      NAME1 FROM KNA1 INTO TABLE GT_KNA1 FOR ALL ENTRIES IN GT_BSID
      WHERE KUNNR = GT_BSID-KUNNR.
  ENDIF.



  LOOP AT GT_KNVV INTO WA_KNVV.


    LOOP AT GT_BSID INTO WA_BSID WHERE KUNNR = WA_KNVV-KUNNR.


*      IF RD_BASDT = 'X'.
      LV_ZFAEDT = WA_BSID-ZFBDT + WA_BSID-ZBD1T.   " Net Due Date For Payment Using Base Line Date
*      ELSEIF RD_INVDT = 'X'.
      LV_ZFAEDT = WA_BSID-BLDAT + WA_BSID-ZBD1T.   " Net Due Date For Payment Using Invoice Date
*      ELSEIF RD_IVPDT = 'X'.,
      LV_ZFAEDT = WA_BSID-BUDAT + WA_BSID-ZBD1T.   " Net Due Date For Payment Invoice Posting Date
*      ENDIF.

      IF LV_ZFAEDT >= P_DATE.
        IF WA_BSID-SHKZG = 'S'.
          LV_NDUEAMT = LV_NDUEAMT + WA_BSID-DMBTR.
        ELSEIF WA_BSID-SHKZG = 'H'.
          LV_NDUEAMT = LV_NDUEAMT - WA_BSID-DMBTR.
        ENDIF.
      ELSEIF LV_ZFAEDT < P_DATE.

        CALL FUNCTION 'HR_99S_INTERVAL_BETWEEN_DATES'
          EXPORTING
            BEGDA = LV_ZFAEDT
            ENDDA = P_DATE
          IMPORTING
            DAYS  = T_DAYS.

        IF T_DAYS BETWEEN 0 AND 7.
          IF WA_BSID-SHKZG = 'S'.
            A1_AMNT = A1_AMNT + WA_BSID-DMBTR.
          ELSEIF WA_BSID-SHKZG = 'H'.
            A1_AMNT = A1_AMNT - WA_BSID-DMBTR.
          ENDIF.
        ELSEIF T_DAYS BETWEEN 8 AND 15 .
          IF WA_BSID-SHKZG = 'S'.
            A2_AMNT = A2_AMNT + WA_BSID-DMBTR.
          ELSEIF WA_BSID-SHKZG = 'H'.
            A2_AMNT = A2_AMNT - WA_BSID-DMBTR.
          ENDIF.
        ELSEIF T_DAYS BETWEEN 16 AND 30 .
          IF WA_BSID-SHKZG = 'S'.
            A3_AMNT = A3_AMNT + WA_BSID-DMBTR.
          ELSEIF WA_BSID-SHKZG = 'H'.
            A3_AMNT = A3_AMNT - WA_BSID-DMBTR.
          ENDIF.
        ELSEIF T_DAYS BETWEEN 31 AND 45.
          IF WA_BSID-SHKZG = 'S'.
            A4_AMNT = A4_AMNT + WA_BSID-DMBTR.
          ELSEIF WA_BSID-SHKZG = 'H'.
            A4_AMNT = A4_AMNT - WA_BSID-DMBTR.
          ENDIF.
        ELSEIF T_DAYS BETWEEN 46 AND 60.
          IF WA_BSID-SHKZG = 'S'.
            A5_AMNT = A5_AMNT + WA_BSID-DMBTR.
          ELSEIF WA_BSID-SHKZG = 'H'.
            A5_AMNT = A5_AMNT - WA_BSID-DMBTR.
          ENDIF.
        ELSEIF T_DAYS > 60.
          IF WA_BSID-SHKZG = 'S'.
            A6_AMNT = A6_AMNT + WA_BSID-DMBTR.
          ELSEIF WA_BSID-SHKZG = 'H'.
            A6_AMNT = A6_AMNT - WA_BSID-DMBTR.
          ENDIF.
        ENDIF.

      ENDIF.

      IF WA_BSID-SHKZG = 'S'.
        TOT_AMNT = TOT_AMNT + WA_BSID-DMBTR.
      ELSEIF WA_BSID-SHKZG = 'H'.
        TOT_AMNT = TOT_AMNT - WA_BSID-DMBTR.
      ENDIF.
      CLEAR WA_BSID.


    ENDLOOP.


    WA_FINAL-KUNNR = WA_KNVV-KUNNR.
**    WA_FINAL-NAME1 = WA_KNA1-NAME1.
    WA_FINAL-VKBUR = WA_KNVV-VKBUR.
    WA_FINAL-DMBTR = WA_BSID-DMBTR + TOT_AMNT.
    WA_FINAL-ZNDUE_AMT =   WA_FINAL-ZNDUE_AMT + LV_NDUEAMT.
    WA_FINAL-ZA1_AMT = WA_FINAL-ZA1_AMT + A1_AMNT.
    WA_FINAL-ZA2_AMT = WA_FINAL-ZA2_AMT + A2_AMNT.
    WA_FINAL-ZA3_AMT = WA_FINAL-ZA3_AMT + A3_AMNT.
    WA_FINAL-ZA4_AMT = WA_FINAL-ZA4_AMT + A4_AMNT.
    WA_FINAL-ZA5_AMT = WA_FINAL-ZA5_AMT + A5_AMNT.
    WA_FINAL-ZA6_AMT =  WA_FINAL-ZA6_AMT + A6_AMNT.



    APPEND WA_FINAL TO GT_FINAL.
    CLEAR: WA_FINAL, WA_KNA1,WA_KNVV,TOT_AMNT,LV_NDUEAMT,
           A1_AMNT,A2_AMNT,A3_AMNT,A4_AMNT,A5_AMNT,A6_AMNT.



  ENDLOOP.



*LOOP AT GT_FINAL INTO WA_FINAL WHERE VKBUR = WA_FINAL-VKBUR.
*ON CHANGE OF WA_FINAL-VKBUR.
*ENDON.
*     WA_FINAL1-DMBTR = WA_FINAL-DMBTR + WA_FINAL1-DMBTR .
*    WA_FINAL1-ZA1_AMT = WA_FINAL-ZA1_AMT + WA_FINAL1-ZA1_AMT.
*    WA_FINAL1-ZA2_AMT = WA_FINAL-ZA2_AMT + WA_FINAL1-ZA2_AMT.
*    WA_FINAL1-ZA3_AMT = WA_FINAL-ZA3_AMT + WA_FINAL1-ZA3_AMT.
*    WA_FINAL1-ZA4_AMT = WA_FINAL-ZA4_AMT + WA_FINAL1-ZA4_AMT.
*    WA_FINAL1-ZA5_AMT = WA_FINAL-ZA5_AMT + WA_FINAL1-ZA5_AMT.
*    WA_FINAL1-ZA6_AMT =  WA_FINAL-ZA6_AMT + WA_FINAL1-ZA6_AMT .
*
*    APPEND WA_FINAL1 TO GT_FINAL1 .
*
*    CLEAR : WA_FINAL1 .
*
*
*  ENDLOOP.
    SORT GT_FINAL BY KUNNR VKBUR DMBTR.

    DELETE ADJACENT DUPLICATES FROM GT_FINAL COMPARING KUNNR VKBUR DMBTR.

*  BREAK-POINT.

  SORT GT_FINAL BY VKBUR  .


  LOOP AT GT_FINAL INTO WA_FINAL.

    ON CHANGE OF WA_FINAL-VKBUR.
      CLEAR LV_SUM.
      CLEAR LV_SUM1.
      CLEAR LV_SUM2.
      CLEAR LV_SUM3.
      CLEAR LV_SUM4.
      CLEAR LV_SUM5.
      CLEAR LV_SUM6.
      CLEAR LV_SUM7.
    ENDON.
    IF WA_FINAL-DMBTR IS NOT INITIAL.
      LV_SUM = LV_SUM + WA_FINAL-DMBTR.
      LV_SUM1 = LV_SUM1 + WA_FINAL-ZA1_AMT.
      LV_SUM2 = LV_SUM2 + WA_FINAL-ZA2_AMT.
      LV_SUM3 = LV_SUM3 + WA_FINAL-ZA3_AMT.
      LV_SUM4 = LV_SUM4 + WA_FINAL-ZA4_AMT.
      LV_SUM5 = LV_SUM5 + WA_FINAL-ZA5_AMT.
      LV_SUM6 = LV_SUM6 + WA_FINAL-ZA6_AMT.
      LV_SUM7 = LV_SUM7 + WA_FINAL-ZNDUE_AMT.
      WA_FINAL-DMBTR1 = LV_SUM.
      WA_FINAL-ZA1_AMT1 = LV_SUM1.
      WA_FINAL-ZA2_AMT2 = LV_SUM2.
      WA_FINAL-ZA3_AMT3 = LV_SUM3.
      WA_FINAL-ZA4_AMT4 = LV_SUM4.
      WA_FINAL-ZA5_AMT5 = LV_SUM5.
      WA_FINAL-ZA6_AMT6 = LV_SUM6.
      WA_FINAL-ZNDUE_AMT1 = LV_SUM7.



      MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING DMBTR1 ZA1_AMT1 ZA2_AMT2 ZA3_AMT3 ZA4_AMT4 ZA5_AMT5 ZA6_AMT6 ZNDUE_AMT1.
      CLEAR WA_FINAL.
    ENDIF.
  ENDLOOP.

  SORT GT_FINAL DESCENDING BY VKBUR DMBTR1.
*
*
*BREAK-POINT.

  LOOP AT GT_FINAL INTO WA_FINAL .
    IF WA_FINAL-VKBUR IS NOT INITIAL.
      LV_WERKCOUNT = LV_WERKCOUNT  + 1.
      ON CHANGE OF WA_FINAL-VKBUR.
        WA_FINAL-V_WERKCOUNT = LV_WERKCOUNT.
        MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING V_WERKCOUNT.
        CLEAR WA_FINAL.
      ENDON.
    ENDIF.
  ENDLOOP.



  LOOP AT GT_FINAL INTO WA_FINAL.
    IF WA_FINAL-V_WERKCOUNT = 0 AND WA_FINAL-DMBTR1 >= 0.

      WA_FINAL-VKBUR = ' '.
*      WA_FINAL-DMBTR = ' '.
      WA_FINAL-ZA1_AMT1 = ' '.
      WA_FINAL-ZA2_AMT2 = ' '.
      WA_FINAL-ZA3_AMT3 = ' '.
      WA_FINAL-ZA4_AMT4 = ' '.
      WA_FINAL-ZA5_AMT5 = ' '.
      WA_FINAL-ZA6_AMT6 = ' '.
      WA_FINAL-DMBTR1 = ''.
*      WA_FINAL-ZA5_AMT1 = ''.


      MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING VKBUR ZA1_AMT1 ZA2_AMT2 ZA3_AMT3 ZA4_AMT4 ZA5_AMT5 ZA6_AMT6 DMBTR1.
    ENDIF.
    CLEAR WA_FINAL.


    DELETE GT_FINAL WHERE ZA1_AMT1 = 0 AND ZA2_AMT2 = 0 AND ZA3_AMT3 = 0 AND ZA4_AMT4 = 0 AND ZA5_AMT5 = 0 AND ZA6_AMT6 = 0 AND DMBTR1 = 0.

*IF WA_FINAL-DMBTR1 >= 0.
    LOOP AT GT_FINAL INTO WA_FINAL.
      READ TABLE GT_T001W INTO WA_T001W WITH KEY WERKS = WA_FINAL-VKBUR  .
      IF SY-SUBRC  = 0.
        WA_FINAL-BNAME = WA_T001W-BNAME.

        MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING BNAME.
        CLEAR WA_FINAL.
      ENDIF.
    ENDLOOP.
    DELETE GT_FINAL WHERE V_WERKCOUNT = 0.
*
*    ENDIF.
  ENDLOOP.

  LOOP AT GT_FINAL INTO WA_FINAL.

    WA_FINAL-ZA1_PER = WA_FINAL-ZA1_AMT1 / WA_FINAL-DMBTR1 * 100."#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation

    WA_FINAL-ZA2_PER = WA_FINAL-ZA2_AMT2 / WA_FINAL-DMBTR1 * 100."#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation

    WA_FINAL-ZA3_PER = WA_FINAL-ZA3_AMT3 / WA_FINAL-DMBTR1 * 100."#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
    WA_FINAL-ZA4_PER = WA_FINAL-ZA4_AMT4 / WA_FINAL-DMBTR1 * 100."#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
    WA_FINAL-ZA5_PER = WA_FINAL-ZA5_AMT5 / WA_FINAL-DMBTR1 * 100."#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
    WA_FINAL-ZA6_PER = WA_FINAL-ZA6_AMT6 / WA_FINAL-DMBTR1 * 100."#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation

    WA_FINAL-ZNDUE_PER = WA_FINAL-ZNDUE_AMT1 / WA_FINAL-DMBTR1 * 100 ."#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation

    MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING ZA1_PER ZA2_PER ZA3_PER ZA4_PER ZA5_PER ZA6_PER ZNDUE_PER.

    CLEAR WA_FINAL.



  ENDLOOP.


ENDFORM.                    " FATCH_DATA


*&---------------------------------------------------------------------*
*&      Form  BUILD_ALV_OVERVIEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_ALV_OVERVIEW .

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
   IS_LAYOUT                         =  GD_LAYOUT
     IT_FIELDCAT                        = IT_FIELDCAT[]
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
ENDFORM.                    " BUILD_ALV_OVERVIEW
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCATALOG_OVERVIEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_FIELDCATALOG_OVERVIEW .

  WA_FIELDCAT-FIELDNAME   = 'VKBUR'.
  WA_FIELDCAT-SELTEXT_M   = 'Sales Office'.
  WA_FIELDCAT-COL_POS     = 1.
  WA_FIELDCAT-TABNAME =  'GT_FINAL1' .
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR  WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME   = 'BNAME'.
  WA_FIELDCAT-SELTEXT_M   = 'Branch Name'.
  WA_FIELDCAT-COL_POS     = 2.
  WA_FIELDCAT-TABNAME =  'GT_FINAL1' .
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR  WA_FIELDCAT.
*
*  WA_FIELDCAT-FIELDNAME   = 'NAME1'.
*  WA_FIELDCAT-SELTEXT_M   = 'Customer Name'.
*  WA_FIELDCAT-COL_POS     = 3.
*  WA_FIELDCAT-TABNAME =  'GT_FINAL1' .
*  APPEND WA_FIELDCAT TO IT_FIELDCAT.
*  CLEAR  WA_FIELDCAT.

*  WA_FIELDCAT-FIELDNAME   = 'DMBTR1'.
*  WA_FIELDCAT-SELTEXT_M   = 'Receivable Amt'.
*  WA_FIELDCAT-COL_POS     = 4.
*  WA_FIELDCAT-TABNAME =  'GT_FINAL1' .
*  APPEND WA_FIELDCAT TO IT_FIELDCAT.
*  CLEAR  WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME   = 'ZA1_AMT1'.
  WA_FIELDCAT-SELTEXT_M   = '0-7 days'.
  WA_FIELDCAT-COL_POS     = 3.
  WA_FIELDCAT-TABNAME =  'GT_FINAL1' .
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR  WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME   = 'ZA1_PER'.
  WA_FIELDCAT-SELTEXT_M   = '0-7 Per'.
  WA_FIELDCAT-COL_POS     = 4.
  WA_FIELDCAT-TABNAME =  'GT_FINAL1' .
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR  WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME   = 'ZA2_AMT2'.
  WA_FIELDCAT-SELTEXT_M   = '8-15 days'.
  WA_FIELDCAT-COL_POS     = 5.
  WA_FIELDCAT-TABNAME =  'GT_FINAL1' .
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR  WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME   = 'ZA2_PER'.
  WA_FIELDCAT-SELTEXT_M   = '8-15 Per'.
  WA_FIELDCAT-COL_POS     = 6.
  WA_FIELDCAT-TABNAME =  'GT_FINAL1' .
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR  WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME   = 'ZA3_AMT3'.
  WA_FIELDCAT-SELTEXT_M   = '16-30 days'.
  WA_FIELDCAT-COL_POS     = 7.
  WA_FIELDCAT-TABNAME =  'GT_FINAL1' .
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR  WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME   = 'ZA3_PER'.
  WA_FIELDCAT-SELTEXT_M   = '16-30 Per'.
  WA_FIELDCAT-COL_POS     = 8.
  WA_FIELDCAT-TABNAME =  'GT_FINAL' .
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR  WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME   = 'ZA4_AMT4'.
  WA_FIELDCAT-SELTEXT_M   = '31-45 days'.
  WA_FIELDCAT-COL_POS     = 9.
  WA_FIELDCAT-TABNAME =  'GT_FINAL1' .
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR  WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME   = 'ZA4_PER'.
  WA_FIELDCAT-SELTEXT_M   = '31-45 Per'.
  WA_FIELDCAT-COL_POS     = 10.
  WA_FIELDCAT-TABNAME =  'GT_FINAL' .
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR  WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME   = 'ZA5_AMT5'.
  WA_FIELDCAT-SELTEXT_M   = '46-60 days'.
  WA_FIELDCAT-COL_POS     = 11.
  WA_FIELDCAT-TABNAME =  'GT_FINAL1' .
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR  WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME   = 'ZA5_PER'.
  WA_FIELDCAT-SELTEXT_M   = '46-60 Per'.
  WA_FIELDCAT-COL_POS     = 12.
  WA_FIELDCAT-TABNAME =  'GT_FINAL' .
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR  WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME   = 'ZA6_AMT6'.
  WA_FIELDCAT-SELTEXT_M   = '60 above'.
  WA_FIELDCAT-COL_POS     = 13.
  WA_FIELDCAT-TABNAME =  'GT_FINAL1' .
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR  WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME   = 'ZA6_PER'.
  WA_FIELDCAT-SELTEXT_M   = '60 Per'.
  WA_FIELDCAT-COL_POS     = 14.
  WA_FIELDCAT-TABNAME =  'GT_FINAL' .
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR  WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME   = 'ZNDUE_AMT1'.
  WA_FIELDCAT-SELTEXT_M   = 'Not In Due Amount'.
  WA_FIELDCAT-COL_POS     = 15.
  WA_FIELDCAT-TABNAME =  'GT_FINAL' .
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR  WA_FIELDCAT.




  WA_FIELDCAT-FIELDNAME   = 'ZNDUE_PER'.
  WA_FIELDCAT-SELTEXT_M   = 'In Due Per'.
  WA_FIELDCAT-COL_POS     = 16.
  WA_FIELDCAT-TABNAME =  'GT_FINAL' .
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR  WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME   = 'DMBTR1'.
  WA_FIELDCAT-SELTEXT_M   = 'Receivable Amt'.
  WA_FIELDCAT-COL_POS     = 17.
  WA_FIELDCAT-TABNAME =  'GT_FINAL1' .
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR  WA_FIELDCAT.



*  WA_FIELDCAT-FIELDNAME   = 'V_WERKCOUNT'.
*  WA_FIELDCAT-SELTEXT_M   = 'V_WERKCOUNT'.
*  WA_FIELDCAT-COL_POS     = 22.
*  WA_FIELDCAT-TABNAME =  'GT_FINAL' .
*  APPEND WA_FIELDCAT TO IT_FIELDCAT.
*  CLEAR  WA_FIELDCAT.



ENDFORM.                    " BUILD_FIELDCATALOG_OVERVIEW


*&---------------------------------------------------------------------*
*&      Form  BUILD_LAYOUT
*&---------------------------------------------------------------------*
*       Build layout for ALV grid report
*----------------------------------------------------------------------*
FORM BUILD_LAYOUT.
* " GD_LAYOUT-NO_INPUT          = 'X'.
*  GD_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
*  "  GD_LAYOUT-TOTALS_TEXT       = 'Totals'(201).
*  GD_LAYOUT-COLTAB_FIELDNAME = 'CELLCOLOR'.  "CTAB_FNAME
ENDFORM.                    " BUILD_LAYOUT
*&---------------------------------------------------------------------*

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
  LS_LINE-INFO = 'Branch Wise Consolidated Outstanding Statement' .
  APPEND LS_LINE TO LIT_HEADER.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = LIT_HEADER
      I_LOGO             = 'ZLOGO'.

ENDFORM.                    "ALV_CATALOG_HEADER
