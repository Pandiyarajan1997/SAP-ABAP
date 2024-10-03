*&---------------------------------------------------------------------*
*& Report  ZVEN_CUST_AGEREP
*&
*&---------------------------------------------------------------------*
*&Functional                   : Govindarajan.M                        *
*& Developer                   : Govindarajan.M                        *
*& Created On                  : 03 June  2014                         *
*& Company                     : Sheenlac Paints Pvt. Ltd              *
*& Title                       : Vendor And Customer Balance           *
*& Report Name                 : ZVEN_CUST_AGEREP                      *
*& Development Id              : kpabap                                *
*& Solman call No              :                                       *
*& Transport Request           :                                       *
*& Related Information         : Vendor And Customer Balance           *
*&---------------------------------------------------------------------*

REPORT ZVEN_CUST_AGEREP.

TYPE-POOLS : SLIS.
*************  Accounting: Secondary Index for Vendor **********************

TYPES: BEGIN OF GS_BSIK,
       BUKRS TYPE BSIK-BUKRS,                    " Company Code
       AUGDT TYPE BSIK-AUGDT,	                   " Clearing Date
       AUGBL TYPE BSIK-AUGBL,	                   " Document Number of the Clearing Document
       LIFNR TYPE BSIK-LIFNR,                    " Vendor Code
       BELNR TYPE BSIK-BELNR,                    " Accounting Document Number
       XBLNR TYPE BSIK-XBLNR,                    " Bill Number
       DMBTR TYPE BSIK-DMBTR,                    " Bill Amount ( In Doc Currency )
       ZFBDT TYPE BSIK-ZFBDT,                    " Baseline Due Date For Calculation
       BUDAT TYPE BSIK-BUDAT,                    " Posting Date in the Document
       BLDAT TYPE BSIK-BLDAT,                    " Document Date in Document
       ZBD1T TYPE BSIK-ZBD1T,                    " Credit Days
       PRCTR TYPE BSIK-PRCTR,                    " Profit Center
       SHKZG TYPE BSIK-SHKZG,                    " Debit/Credit Indicator
       ZTERM TYPE BSIK-ZTERM,                    " Terms of Payment Key
       UMSKZ TYPE BSIK-UMSKZ,                    " Spl GL /Ind
       END OF GS_BSIK.


TYPES: BEGIN OF GS_LFA1,
       LIFNR TYPE LFA1-LIFNR,                    " Vendor Code
       NAME1 TYPE LFA1-NAME1,                    " Vendor Name
       END OF GS_LFA1.
DATA: GT_LFA1 TYPE TABLE OF GS_LFA1,
     WA_LFA1 TYPE GS_LFA1.

*************  Accounting: Secondary Index for Customer **********************


TYPES: BEGIN OF GS_KNA1,
       KUNNR TYPE KNA1-KUNNR,                    " Customer Code
       NAME1 TYPE KNA1-NAME1,                    " Customer Name
       END OF GS_KNA1.

DATA: GT_KNA1 TYPE TABLE OF GS_KNA1,
     WA_KNA1 TYPE GS_KNA1.
TYPES : BEGIN OF GS_KNVV,
        KUNNR TYPE KNVV-KUNNR,
        VKORG TYPE KNVV-VKORG,
        VKBUR TYPE KNVV-VKBUR,
       END OF GS_KNVV.

DATA: GT_KNVV TYPE TABLE OF GS_KNVV,
WA_KNVV TYPE GS_KNVV.

TYPES : BEGIN OF GS_T001W,
        WERKS TYPE T001W-WERKS,
        NAME1 TYPE T001W-NAME1,
        END OF GS_T001W.


DATA: GT_T001W TYPE TABLE OF GS_T001W,
      WA_T001W TYPE GS_T001W.


TYPES: BEGIN OF GS_BSID,
       BUKRS TYPE BSID-BUKRS,                    " Company Code
       UMSKZ TYPE BSId-UMSKZ,
       AUGDT TYPE BSID-AUGDT,	                   " Clearing Date
       AUGBL TYPE BSID-AUGBL,	                   " Document Number of the Clearing Document
       KUNNR TYPE BSID-KUNNR,                    " Customer Code
       BELNR TYPE BSID-BELNR,                    " Accounting Document Number
       DMBTR TYPE BSID-DMBTR,                    " Bill Amount ( In Doc Currency )
       BUDAT TYPE BSID-BUDAT,
       BLDAT TYPE BSID-BLDAT,
       BLART TYPE BSID-BLART,
       ZFBDT TYPE BSID-ZFBDT,                    " Baseline Due Date For Calculation
       ZBD1T TYPE BSID-ZBD1T,                    " Credit Days
       PRCTR TYPE BSID-PRCTR,                    " Profit Center
       SHKZG TYPE BSID-SHKZG,                    " Debit/Credit Indicator
       ZTERM TYPE BSID-ZTERM,                     " Terms of Payment Key
       END OF GS_BSID.

TYPES: BEGIN OF GS_BSAD,
       BUKRS TYPE BSAD-BUKRS,                    " Company Code
       AUGDT TYPE BSAD-AUGDT,	                   " Clearing Date
       AUGBL TYPE BSAD-AUGBL,	                   " Document Number of the Clearing Document
       KUNNR TYPE BSAD-KUNNR,                    " Customer Code
       BELNR TYPE BSAD-BELNR,                    " Accounting Document Number
       DMBTR TYPE BSAD-DMBTR,                    " Bill Amount ( In Doc Currency )
       BUDAT TYPE BSAD-BUDAT,
       BLDAT TYPE BSAD-BLDAT,
       BLART TYPE BSAD-BLART,
       ZFBDT TYPE BSAD-ZFBDT,                    " Baseline Due Date For Calculation
       ZBD1T TYPE BSAD-ZBD1T,                    " Credit Days
       PRCTR TYPE BSAD-PRCTR,                    " Profit Center
       SHKZG TYPE BSAD-SHKZG,                    " Debit/Credit Indicator
       END OF GS_BSAD.

TYPES: BEGIN OF GS_HEADER,
       LIFNR TYPE LFA1-LIFNR,                    " Vendor Code
       NAME1 TYPE LFA1-NAME1,                    " Vendor Name
       DMBTR TYPE BSIK-DMBTR,                    " Total Bill Amount ( In Doc Currency )
       ZNDUE_AMT TYPE BSIK-DMBTR,                " Bill Amount – Not Due
       ZTERM TYPE BSIK-ZTERM,                    " Terms of Payment Key
       ZA1_AMT TYPE BSIK-DMBTR,                  " A1-Amount Due Less Than 30 Days
       ZA2_AMT TYPE BSIK-DMBTR,                  " A2-Amount Due Between 31 – 60 Days
       ZA3_AMT TYPE BSIK-DMBTR,                  " A3-Amount Due Between 61 – 90 Days
       ZA4_AMT TYPE BSIK-DMBTR,                  " A4-Amount Due Between 91 - 180  Days
       ZA5_AMT TYPE BSIK-DMBTR,                  " A5-Amount Due – Above 180  Days
       ZA6_AMT TYPE BSIK-DMBTR,                  " A5-Amount Due – Above 180  Days
       ZA7_AMT TYPE BSIK-DMBTR,                  " A5-Amount Due – Above 180  Days
       ZA8_AMT TYPE BSIK-DMBTR,                  " A5-Amount Due – Above 180  Days
       ZA9_AMT TYPE BSIK-DMBTR,                  " A5-Amount Due – Above 180  Days
       EXPAND,
       END OF GS_HEADER.


TYPES: BEGIN OF GS_FAGLFLEXA,
       DOCNR TYPE FAGLFLEXA-DOCNR,
       PRCTR TYPE FAGLFLEXA-PRCTR,
       BSCHL TYPE FAGLFLEXA-BSCHL,
       END OF GS_FAGLFLEXA.


DATA: GT_HEADER TYPE TABLE OF GS_HEADER,
      WA_HEADER TYPE GS_HEADER.


TYPES: BEGIN OF GS_FINAL,
       KUNNR TYPE KNA1-KUNNR,                    " Customer Code
       NAME1 TYPE KNA1-NAME1,                    " Customer Name
       ZTERM TYPE BSID-ZTERM,                     " Terms of Payment Key
       DMBTR TYPE BSID-DMBTR,                    " Total Bill Amount ( In Doc Currency )
       ZNDUE_AMT TYPE BSID-DMBTR,                " Bill Amount – Not Due
       ZA1_AMT TYPE BSID-DMBTR,                  " A1-Amount Due Less Than 8 Days
       ZA2_AMT TYPE BSID-DMBTR,                  " A2-Amount Due Between 8 –  Days
       ZA3_AMT TYPE BSID-DMBTR,
       ZA4_AMT TYPE BSID-DMBTR,
       ZA5_AMT TYPE BSID-DMBTR,
       ZA6_AMT TYPE BSID-DMBTR,
       ZA7_AMT TYPE BSID-DMBTR,
       ZA8_AMT TYPE BSID-DMBTR,
       ZA9_AMT TYPE BSID-DMBTR,
       ZA10_AMT TYPE BSID-DMBTR,
       ZA11_AMT TYPE BSID-DMBTR,
       ZA12_AMT TYPE BSID-DMBTR,
       ZA13_AMT TYPE BSID-DMBTR,
       PRCTR  TYPE CEPC-PRCTR,
       VKBUR   TYPE KNVV-VKBUR,
       PNAME TYPE T001W-NAME1,
       END OF GS_FINAL.

DATA: GT_FINAL TYPE TABLE OF GS_FINAL,
      WA_FINAL TYPE GS_FINAL.



DATA : LV_BUKRS TYPE BSIK-BUKRS.
DATA : GT_BSIK TYPE STANDARD TABLE OF GS_BSIK ,
             WA_BSIK TYPE GS_BSIK,
        GT_BSID TYPE STANDARD TABLE OF GS_BSID ,
             WA_BSID TYPE GS_BSID,
             GT_BSAD TYPE TABLE OF GS_BSAD,
            WA_BSAD TYPE GS_BSAD.



DATA: GT_FAGLFLEXA TYPE TABLE OF GS_FAGLFLEXA,
      WA_FAGLFLEXA TYPE GS_FAGLFLEXA.
*      GT_FAGLFLEXA1 TYPE TABLE OF GS_FAGLFLEXA,
*      WA_FAGLFLEXA1 TYPE GS_FAGLFLEXA.

DATA: LV_DATE TYPE SY-DATUM,
      LV_DOCNR TYPE FAGLFLEXA-DOCNR.

DATA: OR_LIFNR TYPE LFA1-LIFNR,
      OR_BUKRS TYPE T001-BUKRS,
      OR_ZTERM TYPE T052-ZTERM ,
      OR_PRCTR TYPE FAGLFLEXA-PRCTR,"CEPC-PRCTR,
      OR_BUDAT TYPE BSID-BUDAT,
      OR_GJAHR TYPE BSIK-GJAHR,
      OR_KTOKK TYPE T077K-KTOKK,
      OR_VKBUR TYPE KNVV-VKBUR ,
      OR_KUNNR TYPE KNA1-KUNNR.

DATA : L_PRCTR TYPE CEPC-PRCTR ,
       L_LIFNR TYPE LFA1-LIFNR ,
       L_KTOKK TYPE LFA1-KTOKK .

DATA: LV_ZFAEDT TYPE SY-DATUM,
      LV_NDUEAMT TYPE P DECIMALS 2,
      T_NDUEAMT TYPE P DECIMALS 2,
      T_DAYS TYPE I,
      H_DAYS TYPE I,
      TOT_AMNT TYPE P DECIMALS 2,
      A1_AMNT TYPE P DECIMALS 2,
      A2_AMNT TYPE P DECIMALS 2,
      A3_AMNT TYPE P DECIMALS 2,
      A4_AMNT TYPE P DECIMALS 2,
      A5_AMNT TYPE P DECIMALS 2,
      A6_AMNT TYPE P DECIMALS 2,
      A7_AMNT TYPE P DECIMALS 2,
      A8_AMNT TYPE P DECIMALS 2,
      A9_AMNT TYPE P DECIMALS 2,
      A10_AMNT TYPE P DECIMALS 2,
      A11_AMNT TYPE P DECIMALS 2,
      A12_AMNT TYPE P DECIMALS 2,
      B1_AMNT TYPE P DECIMALS 2,
      B2_AMNT TYPE P DECIMALS 2,
      B3_AMNT TYPE P DECIMALS 2,
      B4_AMNT TYPE P DECIMALS 2,
      B5_AMNT TYPE P DECIMALS 2,
      B6_AMNT TYPE P DECIMALS 2,
      B7_AMNT TYPE P DECIMALS 2,
      B8_AMNT TYPE P DECIMALS 2,
      B9_AMNT TYPE P DECIMALS 2,
      B10_AMNT TYPE P DECIMALS 2,
      B11_AMNT TYPE P DECIMALS 2,
      B12_AMNT TYPE P DECIMALS 2,
      ZA9_AMT TYPE BSID-DMBTR,
      ZA10_AMT TYPE BSID-DMBTR,
      ZA11_AMT TYPE BSID-DMBTR,
      ZA12_AMT TYPE BSID-DMBTR,
      ZA13_AMT TYPE BSID-DMBTR,
      LV_PRCTR TYPE CEPC-PRCTR,
      LV_VKBUR TYPE KNVV-VKBUR,
      LV_NAME TYPE T001W-NAME1.

*Data declarations for ALV
DATA: C_CCONT TYPE REF TO CL_GUI_CUSTOM_CONTAINER,   "Custom container object
      C_ALVGD         TYPE REF TO CL_GUI_ALV_GRID,   "ALV grid object
      IT_FCAT            TYPE LVC_T_FCAT,            "Field catalogue
            IT_LAYOUT          TYPE LVC_S_LAYO,            "Layout
      C_CCONT1 TYPE REF TO CL_GUI_CUSTOM_CONTAINER,   "Custom container object
      C_ALVGD1         TYPE REF TO CL_GUI_ALV_GRID,   "ALV grid object
      IT_FCAT1            TYPE LVC_T_FCAT,            "Field catalogue
      IT_LAYOUT1          TYPE LVC_S_LAYO.

SELECTION-SCREEN: BEGIN OF BLOCK B5 WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS : SO_BUKRS FOR OR_BUKRS MODIF ID MOD,
*                 SO_GJAHR FOR OR_GJAHR MODIF ID MOD,
                  SO_BUDAT FOR OR_BUDAT,
                 SO_KUNNR FOR OR_KUNNR MODIF ID MOD,
                 SO_LIFNR FOR OR_LIFNR MODIF ID MOD,
                  SO_ZTERM FOR OR_ZTERM MODIF ID MOD MATCHCODE OBJECT  ZTERMS ,
*                 SO_VKBUR FOR OR_VKBUR MODIF ID MOD,
                 SO_PRCTR FOR OR_PRCTR MODIF ID MOD.
PARAMETERS: P_DATE TYPE SY-DATUM DEFAULT SY-DATUM OBLIGATORY.

SELECTION-SCREEN: END OF BLOCK B5.


AT SELECTION-SCREEN .

  IF SO_PRCTR IS NOT INITIAL .
    SELECT SINGLE PRCTR FROM CEPC INTO L_PRCTR WHERE PRCTR IN SO_PRCTR .
    IF SY-SUBRC <> 0.
*    MESSAGE E001(YMSG) .
      MESSAGE ' Enter valid profit center' TYPE 'E'.
    ENDIF.
  ENDIF .

START-OF-SELECTION.
*  BREAK-POINT.

*SELECT
*      DOCNR
*      PRCTR
*      BSCHL  FROM FAGLFLEXA INTO CORRESPONDING FIELDS OF TABLE GT_FAGLFLEXA WHERE PRCTR IN SO_PRCTR AND BUDAT IN SO_BUDAT ."GJAHR IN SO_GJAHR .
*  SORT GT_FAGLFLEXA BY DOCNR.
*  DELETE ADJACENT DUPLICATES FROM GT_FAGLFLEXA COMPARING DOCNR.

  SELECT
       BUKRS
       AUGDT
       AUGBL
       LIFNR
       BELNR
       XBLNR
       DMBTR
       ZFBDT
       BUDAT
       BLDAT
       ZBD1T
       PRCTR
       SHKZG
       ZTERM
       UMSKZ   FROM BSIK INTO TABLE GT_BSIK
       WHERE BUKRS IN SO_BUKRS AND   LIFNR IN SO_LIFNR AND  ZTERM IN SO_ZTERM AND
              BUDAT IN SO_BUDAT AND
                 UMSKZ NOT IN  ('F' , 'H') AND ZFBDT <= P_DATE.

  IF GT_BSIK[] IS NOT INITIAL.
    SELECT
      LIFNR
      NAME1 FROM LFA1 INTO TABLE GT_LFA1 FOR ALL ENTRIES IN GT_BSIK
      WHERE LIFNR = GT_BSIK-LIFNR.

  ENDIF.
  LOOP AT GT_LFA1 INTO WA_LFA1.

    LOOP AT GT_BSIK INTO WA_BSIK WHERE LIFNR = WA_LFA1-LIFNR.

      LV_ZFAEDT = WA_BSIK-BUDAT + WA_BSIK-ZBD1T.   " Net Due Date For Payment Using Invoice Date

      IF LV_ZFAEDT >= P_DATE.
        IF WA_BSIK-SHKZG = 'S'.
          LV_NDUEAMT = LV_NDUEAMT + WA_BSIK-DMBTR.
          T_NDUEAMT = T_NDUEAMT + WA_BSIK-DMBTR.
        ELSEIF WA_BSIK-SHKZG = 'H'.
          LV_NDUEAMT = LV_NDUEAMT - WA_BSIK-DMBTR.
          T_NDUEAMT = T_NDUEAMT - WA_BSIK-DMBTR.
        ENDIF.

      ELSEIF LV_ZFAEDT < P_DATE.


        CALL FUNCTION 'HR_99S_INTERVAL_BETWEEN_DATES'
          EXPORTING
            BEGDA = LV_ZFAEDT
            ENDDA = P_DATE
          IMPORTING
            DAYS  = T_DAYS.

        IF T_DAYS =< 7  AND WA_BSIK-ZTERM = 'NT07'.
          IF WA_BSIK-SHKZG = 'S'  .
            A1_AMNT = A1_AMNT + WA_BSIK-DMBTR.

          ELSEIF WA_BSIK-SHKZG = 'H' .
            A1_AMNT = A1_AMNT - WA_BSIK-DMBTR.
            B1_AMNT = B1_AMNT - WA_BSIK-DMBTR.
          ENDIF.
        ENDIF.
        IF T_DAYS BETWEEN 8 AND 21 AND WA_BSIK-ZTERM = 'NT07'.
          IF WA_BSIK-SHKZG = 'S' .
            A2_AMNT = A2_AMNT + WA_BSIK-DMBTR.
            ELSEIF WA_BSIK-SHKZG = 'H' .
            A2_AMNT = A2_AMNT - WA_BSIK-DMBTR.
          ENDIF.
        ENDIF.
        IF T_DAYS BETWEEN 22 AND 30 AND WA_BSIK-ZTERM = 'NT07'.
          IF WA_BSIK-SHKZG = 'S' .
            A3_AMNT = A3_AMNT + WA_BSIK-DMBTR.
          ELSEIF WA_BSIK-SHKZG = 'H' .
            A3_AMNT = A3_AMNT - WA_BSIK-DMBTR.
          ENDIF.
        ENDIF.
        IF T_DAYS > 30 AND WA_BSIK-ZTERM = 'NT07'.
          IF WA_BSIK-SHKZG = 'S' .
            A4_AMNT = A4_AMNT + WA_BSIK-DMBTR.
          ELSEIF WA_BSIK-SHKZG = 'H' .
            A4_AMNT = A4_AMNT - WA_BSIK-DMBTR.
          ENDIF.
        ENDIF.

        IF T_DAYS =< 30 AND WA_BSIK-ZTERM = 'NT30'.
          IF WA_BSIK-SHKZG = 'S'  .
            A5_AMNT = A5_AMNT + WA_BSIK-DMBTR.
            ELSEIF WA_BSIK-SHKZG = 'H'.
            A5_AMNT = A5_AMNT - WA_BSIK-DMBTR.
               ENDIF.
        ENDIF.

        IF T_DAYS BETWEEN  30 AND 60 AND WA_BSIK-ZTERM = 'NT30'.
          IF WA_BSIK-SHKZG = 'S'.
            A6_AMNT = A6_AMNT + WA_BSIK-DMBTR.
                ELSEIF WA_BSIK-SHKZG = 'H'.
            A7_AMNT = A7_AMNT - WA_BSIK-DMBTR.
                    ENDIF.
        ENDIF.
        IF T_DAYS BETWEEN 61 AND 90 AND WA_BSIK-ZTERM = 'NT30'.
          IF WA_BSIK-SHKZG = 'S'.
            A8_AMNT = A8_AMNT + WA_BSIK-DMBTR.
                      ELSEIF WA_BSIK-SHKZG = 'H'.
            A8_AMNT = A8_AMNT - WA_BSIK-DMBTR.
                     ENDIF.
        ENDIF.

          IF T_DAYS =<  90 AND WA_BSIK-ZTERM = 'NT60'.
          IF WA_BSIK-SHKZG = 'S'.
            A9_AMNT = A9_AMNT + WA_BSIK-DMBTR.
                     ELSEIF WA_BSIK-SHKZG = 'H'.
            A9_AMNT = A9_AMNT - WA_BSIK-DMBTR.
                   ENDIF.
        ENDIF.



      ENDIF.

       IF WA_BSIK-SHKZG = 'S'.
          TOT_AMNT = TOT_AMNT + WA_BSIK-DMBTR.
        ELSEIF WA_BSIK-SHKZG = 'H'.
          TOT_AMNT = TOT_AMNT - WA_BSIK-DMBTR.
        ENDIF.

    ENDLOOP.

    WA_HEADER-LIFNR = WA_LFA1-LIFNR.
    WA_HEADER-NAME1 = WA_LFA1-NAME1.
    WA_HEADER-ZTERM  = WA_BSIK-ZTERM.
    WA_HEADER-DMBTR = TOT_AMNT.
    WA_HEADER-ZNDUE_AMT = T_NDUEAMT.
    WA_HEADER-ZA1_AMT = A1_AMNT.
    WA_HEADER-ZA2_AMT = A2_AMNT.
    WA_HEADER-ZA3_AMT = A3_AMNT.
    WA_HEADER-ZA4_AMT = A4_AMNT.
    WA_HEADER-ZA5_AMT = A5_AMNT.
    WA_HEADER-ZA6_AMT = A6_AMNT.
    WA_HEADER-ZA7_AMT = A7_AMNT.
    WA_HEADER-ZA8_AMT = A8_AMNT.
    WA_HEADER-ZA9_AMT = A9_AMNT.
    APPEND WA_HEADER TO GT_HEADER.
    CLEAR: WA_HEADER, WA_LFA1,WA_BSIK,TOT_AMNT,T_NDUEAMT,
           A1_AMNT,A2_AMNT,A3_AMNT,A4_AMNT,A5_AMNT,A6_AMNT,A7_AMNT,A8_AMNT,A9_AMNT,A10_AMNT.
*    ENDIF.
  ENDLOOP.

*  BREAK-POINT.

  SELECT
      DOCNR
      PRCTR
      BSCHL  FROM FAGLFLEXA INTO CORRESPONDING FIELDS OF TABLE GT_FAGLFLEXA WHERE PRCTR IN SO_PRCTR AND BUDAT IN SO_BUDAT ."GJAHR IN SO_GJAHR .
  SORT GT_FAGLFLEXA BY DOCNR.
  DELETE ADJACENT DUPLICATES FROM GT_FAGLFLEXA COMPARING DOCNR.

*     LOOP AT GT_FAGLFLEXA INTO WA_FAGLFLEXA .
*      IF WA_FAGLFLEXA-DOCNR NE LV_DOCNR.
*        LV_DOCNR = WA_FAGLFLEXA-DOCNR.
*        IF WA_FAGLFLEXA-PRCTR IN SO_PRCTR.
*          APPEND WA_FAGLFLEXA TO GT_FAGLFLEXA.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.


*   SELECT KUNNR VKORG VKBUR FROM KNVV INTO TABLE GT_KNVV  WHERE  VKBUR IN SO_VKBUR AND VKORG IN SO_BUKRS .
*
*BREAK-POINT.
*     IF GT_KNVV[] IS NOT  INITIAL.

  IF GT_FAGLFLEXA[] IS NOT INITIAL .
    SELECT
          BUKRS
          UMSKZ
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
          SHKZG
          ZTERM FROM BSID INTO CORRESPONDING FIELDS OF TABLE GT_BSID FOR ALL ENTRIES IN GT_FAGLFLEXA
          WHERE                  KUNNR IN SO_KUNNR AND UMSKZ <> 'H' AND
       BUKRS IN SO_BUKRS  AND BELNR = GT_FAGLFLEXA-DOCNR
            AND ZTERM IN SO_ZTERM AND BUDAT IN SO_BUDAT AND "AND GJAHR IN SO_GJAHR AND
                              ZFBDT <= P_DATE  .   " %_HINTS ORACLE 'INDEX("BSID" "BSID~ZSU" )'. Added by <IT-CAR Tool> during Code Remediation
*
*    SELECT
*        BUKRS
*        AUGDT
*        AUGBL
*        KUNNR
*        BELNR
*        DMBTR
*        BUDAT
*        BLDAT
*        BLART
*        ZFBDT
*        ZBD1T
*        PRCTR
*        SHKZG FROM BSAD INTO CORRESPONDING FIELDS OF TABLE GT_BSAD FOR ALL ENTRIES IN GT_BSID
*        WHERE  BUKRS IN SO_BUKRS  AND BELNR = GT_BSID-BELNR AND KUNNR IN SO_KUNNR
*    "AND BUKRS = GT_KNVV-VKORG
*    AND ZTERM IN SO_ZTERM AND  AUGDT BETWEEN LV_DATE AND SY-DATUM AND  GJAHR IN SO_GJAHR AND
*                            ZFBDT <= P_DATE %_HINTS ORACLE 'INDEX("BSAD" "BSAD~ZSU" )'.
***
*
*****
*  IF GT_BSAD[] IS NOT INITIAL.
*  APPEND LINES OF GT_BSAD TO GT_BSID.
*  ENDIF.

    ENDIF.

  IF GT_BSID[] IS NOT INITIAL.
    SELECT
      KUNNR
      NAME1 FROM KNA1 INTO TABLE GT_KNA1 FOR ALL ENTRIES IN GT_BSID
      WHERE KUNNR = GT_BSID-KUNNR.
  ENDIF.


*SELECT WERKS
*       NAME1 FROM T001W INTO TABLE GT_T001W  FOR ALL ENTRIES IN GT_KNVV WHERE WERKS = GT_KNVV-VKBUR.

  LOOP AT GT_KNA1 INTO WA_KNA1 .

    LOOP AT GT_BSID INTO WA_BSID WHERE KUNNR = WA_KNA1-KUNNR .


      LV_ZFAEDT = WA_BSID-BUDAT + WA_BSID-ZBD1T.   " Net Due Date For Payment Using Invoice Date

      IF LV_ZFAEDT >= P_DATE.
        IF WA_BSID-SHKZG = 'S'.
          LV_NDUEAMT = LV_NDUEAMT + WA_BSID-DMBTR.
          T_NDUEAMT = T_NDUEAMT + WA_BSID-DMBTR.
        ELSEIF WA_BSID-SHKZG = 'H'.
          LV_NDUEAMT = LV_NDUEAMT - WA_BSID-DMBTR.
          T_NDUEAMT = T_NDUEAMT - WA_BSID-DMBTR.
        ENDIF.

      ELSEIF LV_ZFAEDT < P_DATE.

        CALL FUNCTION 'HR_99S_INTERVAL_BETWEEN_DATES'
          EXPORTING
            BEGDA = LV_ZFAEDT
            ENDDA = P_DATE
          IMPORTING
            DAYS  = T_DAYS.

        IF T_DAYS =< 7 AND  WA_BSID-ZTERM = 'NT07'.
          IF WA_BSID-SHKZG = 'S'  .
            A1_AMNT = A1_AMNT + WA_BSID-DMBTR.
               ELSEIF WA_BSID-SHKZG = 'H'  .
             A1_AMNT = A1_AMNT - WA_BSID-DMBTR.
             ENDIF.
            ENDIF.


        IF T_DAYS BETWEEN 8 AND 21 AND  WA_BSID-ZTERM = 'NT07' .
          IF WA_BSID-SHKZG = 'S'  .
            A2_AMNT = A2_AMNT + WA_BSID-DMBTR.

             ELSEIF WA_BSID-SHKZG = 'H'  .
             A2_AMNT = A2_AMNT - WA_BSID-DMBTR.
               ENDIF.
             ENDIF.


        IF T_DAYS BETWEEN 22 AND 30  AND  WA_BSID-ZTERM = 'NT07'  .
          IF WA_BSID-SHKZG = 'S'.
            A3_AMNT = A3_AMNT + WA_BSID-DMBTR.
              ELSEIF WA_BSID-SHKZG = 'H'  .
             A3_AMNT = A3_AMNT - WA_BSID-DMBTR.
             ENDIF.
                         ENDIF.

        IF T_DAYS > 30  AND  WA_BSID-ZTERM = 'NT07'.
          IF WA_BSID-SHKZG = 'S' .
            A4_AMNT = A4_AMNT + WA_BSID-DMBTR.
             ELSEIF WA_BSID-SHKZG = 'H'  .
             A4_AMNT = A4_AMNT - WA_BSID-DMBTR.
             ENDIF.
                          ENDIF.
******************** NT30************************
                          IF T_DAYS =< 15  AND WA_BSID-ZTERM = 'NT30'.
          IF WA_BSID-SHKZG = 'S' .
            A5_AMNT = A5_AMNT + WA_BSID-DMBTR.
            ELSEIF WA_BSID-SHKZG = 'H'  .
             A5_AMNT = A5_AMNT - WA_BSID-DMBTR.
             ENDIF.
                         ENDIF.


        IF T_DAYS BETWEEN 16 AND 30  AND WA_BSID-ZTERM = 'NT30' .
          IF WA_BSID-SHKZG = 'S' .
            A6_AMNT = A6_AMNT + WA_BSID-DMBTR.
             ELSEIF WA_BSID-SHKZG = 'H' .
             A6_AMNT = A6_AMNT - WA_BSID-DMBTR.
             ENDIF.
                          ENDIF.

        IF T_DAYS BETWEEN 31 AND 45 AND WA_BSID-ZTERM = 'NT30'.
          IF WA_BSID-SHKZG = 'S'  .
            A7_AMNT = A7_AMNT + WA_BSID-DMBTR.
            ELSEIF WA_BSID-SHKZG = 'H'  .
             A7_AMNT = A7_AMNT - WA_BSID-DMBTR.
             ENDIF.
                            ENDIF.

        IF  T_DAYS > 45 AND WA_BSID-ZTERM = 'NT30'.
          IF WA_BSID-SHKZG = 'S' .
            A8_AMNT = A8_AMNT + WA_BSID-DMBTR.
            ELSEIF WA_BSID-SHKZG = 'H'  .
             A8_AMNT = A8_AMNT - WA_BSID-DMBTR.
             ENDIF.

             ENDIF.


****************** NT15 **************************

        IF T_DAYS =< 30  AND WA_BSID-ZTERM = 'NT15'.
          IF WA_BSID-SHKZG = 'S' .
            A9_AMNT = A9_AMNT + WA_BSID-DMBTR.
            ELSEIF WA_BSID-SHKZG = 'H'  .
             A9_AMNT = A9_AMNT - WA_BSID-DMBTR.
             ENDIF.
                         ENDIF.


        IF T_DAYS BETWEEN 31 AND 60  AND WA_BSID-ZTERM = 'NT15' .
          IF WA_BSID-SHKZG = 'S' .
            A10_AMNT = A10_AMNT + WA_BSID-DMBTR.
             ELSEIF WA_BSID-SHKZG = 'H' .
             A10_AMNT = A10_AMNT - WA_BSID-DMBTR.
             ENDIF.
                          ENDIF.

        IF T_DAYS BETWEEN 61 AND 90 AND WA_BSID-ZTERM = 'NT15'.
          IF WA_BSID-SHKZG = 'S'  .
            A11_AMNT = A11_AMNT + WA_BSID-DMBTR.
            ELSEIF WA_BSID-SHKZG = 'H'  .
             A11_AMNT = A11_AMNT - WA_BSID-DMBTR.
             ENDIF.
                            ENDIF.

        IF  T_DAYS > 90 AND WA_BSID-ZTERM = 'NT15'.
          IF WA_BSID-SHKZG = 'S' .
            A12_AMNT = A12_AMNT + WA_BSID-DMBTR.
            ELSEIF WA_BSID-SHKZG = 'H'  .
             A12_AMNT = A12_AMNT - WA_BSID-DMBTR.
             ENDIF.

             ENDIF.


              IF WA_BSID-SHKZG = 'S'.
          TOT_AMNT = TOT_AMNT + WA_BSID-DMBTR.
*          READ TABLE GT_BSAD INTO WA_BSAD WITH KEY BELNR = WA_BSID-BELNR BLART = 'DZ'.
          ELSEIF WA_BSID-SHKZG = 'H'.
            TOT_AMNT = TOT_AMNT - WA_BSID-DMBTR.
          ENDIF.
ENDIF.




      READ TABLE GT_FAGLFLEXA INTO WA_FAGLFLEXA WITH KEY DOCNR = WA_BSID-BELNR .
      LV_PRCTR = WA_FAGLFLEXA-PRCTR.


    ENDLOOP.

    WA_FINAL-KUNNR = WA_KNA1-KUNNR.
    WA_FINAL-NAME1 = WA_KNA1-NAME1.
    WA_FINAL-ZTERM  = WA_BSID-ZTERM.
    WA_FINAL-DMBTR = TOT_AMNT.
    WA_FINAL-ZNDUE_AMT =  T_NDUEAMT.
    WA_FINAL-ZA1_AMT = A1_AMNT.
    WA_FINAL-ZA2_AMT = A2_AMNT.
    WA_FINAL-ZA3_AMT = A3_AMNT.
    WA_FINAL-ZA4_AMT = A4_AMNT.
    WA_FINAL-ZA5_AMT = A5_AMNT.
    WA_FINAL-ZA6_AMT = A6_AMNT.
    WA_FINAL-ZA7_AMT = A7_AMNT.
    WA_FINAL-ZA8_AMT = A8_AMNT.
    WA_FINAL-ZA9_AMT = A9_AMNT.
    WA_FINAL-ZA10_AMT = A10_AMNT.
    WA_FINAL-ZA11_AMT = A11_AMNT.
    WA_FINAL-ZA12_AMT = A12_AMNT.
    WA_FINAL-PRCTR = LV_PRCTR.
*    WA_FINAL-VKBUR = LV_VKBUR.
*    WA_FINAL-PNAME = LV_NAME.
    APPEND WA_FINAL TO GT_FINAL.
    CLEAR: WA_FINAL, WA_KNA1,WA_BSID,TOT_AMNT,LV_NDUEAMT,LV_NAME,LV_VKBUR,
           A1_AMNT,A2_AMNT,A3_AMNT,A4_AMNT,A5_AMNT,A6_AMNT,A7_AMNT,A8_AMNT,A9_AMNT,A10_AMNT,A11_AMNT,A12_AMNT.
  ENDLOOP.


  CALL SCREEN 09010.



* calling the PBO module ALV_GRID.
*&---------------------------------------------------------------------*
*&      Module  ALV_GRID  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DISPLAY_BSIK OUTPUT.

*  BREAK-POINT.
  CREATE OBJECT C_CCONT
    EXPORTING
      CONTAINER_NAME = 'VENDOR'.
  CREATE OBJECT C_ALVGD
    EXPORTING
      I_PARENT = C_CCONT.
* Set field for ALV
  PERFORM ALV_BUILD_FIELDCAT.
* Set ALV attributes FOR LAYOUT
  PERFORM ALV_REPORT_LAYOUT.
  CHECK NOT C_ALVGD IS INITIAL.
* Call ALV GRID
  CALL METHOD C_ALVGD->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_LAYOUT                     = IT_LAYOUT
    CHANGING
      IT_OUTTAB                     = GT_HEADER
      IT_FIELDCATALOG               = IT_FCAT
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDMODULE.                 " ALV_GRID  OUTPUT



*&---------------------------------------------------------------------*
*&      Form  alv_build_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_IT_FCAT  text
*----------------------------------------------------------------------*

FORM ALV_BUILD_FIELDCAT.
*  BREAK-POINT.
  DATA LV_FLDCAT TYPE LVC_S_FCAT.
  CLEAR LV_FLDCAT.
  LV_FLDCAT-ROW_POS   = '1'.
  LV_FLDCAT-COL_POS   = '1'.
  LV_FLDCAT-FIELDNAME = 'LIFNR'.
  LV_FLDCAT-TABNAME   = 'GT_HEADER'.
  LV_FLDCAT-OUTPUTLEN = 8.
  LV_FLDCAT-SCRTEXT_M = 'Vendor Name'.
  LV_FLDCAT-ICON = ''.
  APPEND LV_FLDCAT TO IT_FCAT.
  CLEAR LV_FLDCAT.

  LV_FLDCAT-ROW_POS   = '1'.
  LV_FLDCAT-COL_POS   = '2'.
  LV_FLDCAT-FIELDNAME = 'NAME1'.
  LV_FLDCAT-TABNAME   = 'GT_HEADER'.
  LV_FLDCAT-OUTPUTLEN = 15.
  LV_FLDCAT-SCRTEXT_M = 'Vendor Name'.
  LV_FLDCAT-ICON = ''.
  APPEND LV_FLDCAT TO IT_FCAT.
  CLEAR LV_FLDCAT.

  LV_FLDCAT-ROW_POS   = '1'.
  LV_FLDCAT-COL_POS   = '3'.
  LV_FLDCAT-FIELDNAME = 'ZTERM'.
  LV_FLDCAT-TABNAME   = 'GT_HEADER'.
  LV_FLDCAT-OUTPUTLEN = 15.
  LV_FLDCAT-SCRTEXT_M = 'Payment Terms'.
  LV_FLDCAT-ICON = ''.
  LV_FLDCAT-NO_OUT  = 'X'.
  APPEND LV_FLDCAT TO IT_FCAT.
  CLEAR LV_FLDCAT.



  LV_FLDCAT-ROW_POS   = '1'.
  LV_FLDCAT-COL_POS   = '4'.
  LV_FLDCAT-FIELDNAME = 'DMBTR'.
  LV_FLDCAT-TABNAME   = 'GT_HEADER'.
  LV_FLDCAT-OUTPUTLEN = 60.
  LV_FLDCAT-SCRTEXT_M = 'Payable Amount'.
  LV_FLDCAT-ICON = ''.
  APPEND LV_FLDCAT TO IT_FCAT.
  CLEAR LV_FLDCAT.

  LV_FLDCAT-ROW_POS   = '1'.
  LV_FLDCAT-COL_POS   = '5'.
  LV_FLDCAT-FIELDNAME = 'ZA1_AMT'.
  LV_FLDCAT-TABNAME   = 'GT_HEADER'.
  LV_FLDCAT-OUTPUTLEN = 15.
  LV_FLDCAT-SCRTEXT_M = 'NT07'.
  LV_FLDCAT-ICON = ''.
  APPEND LV_FLDCAT TO IT_FCAT.
  CLEAR LV_FLDCAT.

  LV_FLDCAT-ROW_POS   = '1'.
  LV_FLDCAT-COL_POS   = '6'.
  LV_FLDCAT-FIELDNAME = 'ZA2_AMT'.
  LV_FLDCAT-TABNAME   = 'GT_HEADER'.
  LV_FLDCAT-OUTPUTLEN = 15.
  LV_FLDCAT-SCRTEXT_M = '8-21 Days'.
  LV_FLDCAT-ICON = ''.
  APPEND LV_FLDCAT TO IT_FCAT.
  CLEAR LV_FLDCAT.

  LV_FLDCAT-ROW_POS   = '1'.
  LV_FLDCAT-COL_POS   = '7'.
  LV_FLDCAT-FIELDNAME = 'ZA3_AMT'.
  LV_FLDCAT-TABNAME   = 'GT_HEADER'.
  LV_FLDCAT-OUTPUTLEN = 15.
  LV_FLDCAT-SCRTEXT_M = '21-30 Days'.
  LV_FLDCAT-ICON = ''.
  APPEND LV_FLDCAT TO IT_FCAT.
  CLEAR LV_FLDCAT.

  LV_FLDCAT-ROW_POS   = '1'.
  LV_FLDCAT-COL_POS   = '8'.
  LV_FLDCAT-FIELDNAME = 'ZA4_AMT'.
  LV_FLDCAT-TABNAME   = 'GT_HEADER'.
  LV_FLDCAT-OUTPUTLEN = 15.
  LV_FLDCAT-SCRTEXT_M = 'Above 30 Days'.
  LV_FLDCAT-ICON = ''.
  APPEND LV_FLDCAT TO IT_FCAT.
  CLEAR LV_FLDCAT.


  LV_FLDCAT-ROW_POS   = '1'.
  LV_FLDCAT-COL_POS   = '9'.
  LV_FLDCAT-FIELDNAME = 'ZA5_AMT'.
  LV_FLDCAT-TABNAME   = 'GT_HEADER'.
*  LV_FLDCAT-OUTPUTLEN = 60.
  LV_FLDCAT-SCRTEXT_M = 'NT30'.
  LV_FLDCAT-ICON = ''.
  APPEND LV_FLDCAT TO IT_FCAT.
  CLEAR LV_FLDCAT.
  LV_FLDCAT-ROW_POS   = '1'.
  LV_FLDCAT-COL_POS   = '10'.
  LV_FLDCAT-FIELDNAME = 'ZA6_AMT'.
  LV_FLDCAT-TABNAME   = 'GT_HEADER'.
  LV_FLDCAT-OUTPUTLEN = 60.
  LV_FLDCAT-SCRTEXT_M = '31-60 Days'.
  LV_FLDCAT-ICON = ''.
  APPEND LV_FLDCAT TO IT_FCAT.
  CLEAR LV_FLDCAT.

  LV_FLDCAT-ROW_POS   = '1'.
  LV_FLDCAT-COL_POS   = '11'.
  LV_FLDCAT-FIELDNAME = 'ZA7_AMT'.
  LV_FLDCAT-TABNAME   = 'GT_HEADER'.
  LV_FLDCAT-OUTPUTLEN = 60.
  LV_FLDCAT-SCRTEXT_M = '61-90 Days'.
  LV_FLDCAT-ICON = ''.
  APPEND LV_FLDCAT TO IT_FCAT.
  CLEAR LV_FLDCAT.
  LV_FLDCAT-ROW_POS   = '1'.
  LV_FLDCAT-COL_POS   = '12'.
  LV_FLDCAT-FIELDNAME = 'ZA8_AMT'.
  LV_FLDCAT-TABNAME   = 'GT_HEADER'.
  LV_FLDCAT-OUTPUTLEN = 60.
  LV_FLDCAT-SCRTEXT_M = 'Above 90 Days'.
  LV_FLDCAT-ICON = ''.
  APPEND LV_FLDCAT TO IT_FCAT.
  CLEAR LV_FLDCAT.
   LV_FLDCAT-ROW_POS   = '1'.
  LV_FLDCAT-COL_POS   = '13'.
  LV_FLDCAT-FIELDNAME = 'ZA9_AMT'.
  LV_FLDCAT-TABNAME   = 'GT_HEADER'.
  LV_FLDCAT-OUTPUTLEN = 60.
  LV_FLDCAT-SCRTEXT_M = 'NT60'.
  LV_FLDCAT-ICON = ''.
  APPEND LV_FLDCAT TO IT_FCAT.
  CLEAR LV_FLDCAT.



ENDFORM.                    " alv_build_fieldcat


*&---------------------------------------------------------------------*
*&      Form  alv_report_layout
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_IT_LAYOUT  text
*----------------------------------------------------------------------*
FORM ALV_REPORT_LAYOUT.
  IT_LAYOUT-CWIDTH_OPT = 'X'.
  IT_LAYOUT-ZEBRA = 'X'.
ENDFORM.                    " alv_report_layout


*&---------------------------------------------------------------------*
*&      Module  DISPLAY_DSID  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DISPLAY_DSID OUTPUT.
*  BREAK-POINT.
  CREATE OBJECT C_CCONT1
    EXPORTING
      CONTAINER_NAME = 'CUSTOMER'.
  CREATE OBJECT C_ALVGD1
    EXPORTING
      I_PARENT = C_CCONT1.
* Set field for ALV
  PERFORM ALV_BUILD_FIELDCAT1.
* Set ALV attributes FOR LAYOUT
  PERFORM ALV_REPORT_LAYOUT1.
  CHECK NOT C_ALVGD1 IS INITIAL.
* Call ALV GRID
  CALL METHOD C_ALVGD1->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_LAYOUT                     = IT_LAYOUT1
    CHANGING
      IT_OUTTAB                     = GT_FINAL
      IT_FIELDCATALOG               = IT_FCAT1
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDMODULE.                 " DISPLAY_DSID  OUTPUT


*&---------------------------------------------------------------------*
*&      Module  STATUS_9010  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  ALV_BUILD_FIELDCAT1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ALV_BUILD_FIELDCAT1 .
  DATA LV_FLDCAT TYPE LVC_S_FCAT.
  CLEAR LV_FLDCAT.
  LV_FLDCAT-ROW_POS   = '1'.
  LV_FLDCAT-COL_POS   = '1'.
  LV_FLDCAT-FIELDNAME = 'KUNNR'.
  LV_FLDCAT-TABNAME   = 'GT_FINAL'.
  LV_FLDCAT-OUTPUTLEN = 8.
  LV_FLDCAT-SCRTEXT_M = 'Customer No'.
  LV_FLDCAT-ICON = ''.
  APPEND LV_FLDCAT TO IT_FCAT1.
  CLEAR LV_FLDCAT.
  LV_FLDCAT-ROW_POS   = '1'.
  LV_FLDCAT-COL_POS   = '2'.
  LV_FLDCAT-FIELDNAME = 'NAME1'.
  LV_FLDCAT-TABNAME   = 'GT_FINAL'.
  LV_FLDCAT-OUTPUTLEN = 15.
  LV_FLDCAT-SCRTEXT_M = 'Customer Name'.
  LV_FLDCAT-ICON = ''.
  APPEND LV_FLDCAT TO IT_FCAT1.
  CLEAR LV_FLDCAT.

*  LV_FLDCAT-ROW_POS   = '1'.
*  LV_FLDCAT-COL_POS   = '3'.
*  LV_FLDCAT-FIELDNAME = 'ZTERM'.
*  LV_FLDCAT-TABNAME   = 'GT_FINAL'.
*  LV_FLDCAT-OUTPUTLEN = 15.
*  LV_FLDCAT-SCRTEXT_M = 'Payment Terms'.
*  LV_FLDCAT-ICON = ''.
*  LV_FLDCAT-NO_OUT  = 'X'.
*  APPEND LV_FLDCAT TO IT_FCAT1.
*  CLEAR LV_FLDCAT.

  LV_FLDCAT-ROW_POS   = '1'.
  LV_FLDCAT-COL_POS   = '3'.
  LV_FLDCAT-FIELDNAME = 'DMBTR'.
  LV_FLDCAT-TABNAME   = 'GT_FINAL'.
  LV_FLDCAT-OUTPUTLEN = 15.
  LV_FLDCAT-SCRTEXT_M = 'Receivable Amt'.
  LV_FLDCAT-ICON = ''.
*   LV_FLDCAT-DO_SUM = 'X'.
  APPEND LV_FLDCAT TO IT_FCAT1.
  CLEAR LV_FLDCAT.

  LV_FLDCAT-ROW_POS   = '1'.
  LV_FLDCAT-COL_POS   = '4'.
  LV_FLDCAT-FIELDNAME = 'ZA1_AMT'.
  LV_FLDCAT-TABNAME   = 'GT_FINAL'.
  LV_FLDCAT-OUTPUTLEN = 15.
  LV_FLDCAT-SCRTEXT_M = 'NT07'.
*  LV_FLDCAT-ICON = ''.
*   LV_FLDCAT-DO_SUM = 'X'.
  APPEND LV_FLDCAT TO IT_FCAT1.
  CLEAR LV_FLDCAT.

  LV_FLDCAT-ROW_POS   = '1'.
  LV_FLDCAT-COL_POS   = '5'.
  LV_FLDCAT-FIELDNAME = 'ZA2_AMT'.
  LV_FLDCAT-TABNAME   = 'GT_FINAL'.
  LV_FLDCAT-OUTPUTLEN = 15.
  LV_FLDCAT-SCRTEXT_M = '8-21 Days'.
*  LV_FLDCAT-ICON = ''.
*  LV_FLDCAT-DO_SUM = 'X'.
  APPEND LV_FLDCAT TO IT_FCAT1.
  CLEAR LV_FLDCAT.

  LV_FLDCAT-ROW_POS   = '1'.
  LV_FLDCAT-COL_POS   = '6'.
  LV_FLDCAT-FIELDNAME = 'ZA3_AMT'.
  LV_FLDCAT-TABNAME   = 'GT_FINAL'.
  LV_FLDCAT-OUTPUTLEN = 15.
  LV_FLDCAT-SCRTEXT_M = '21-30 Days'.
*  LV_FLDCAT-ICON = ''.
*  LV_FLDCAT-DO_SUM = 'X'.
  APPEND LV_FLDCAT TO IT_FCAT1.
  CLEAR LV_FLDCAT.

  LV_FLDCAT-ROW_POS   = '1'.
  LV_FLDCAT-COL_POS   = '7'.
  LV_FLDCAT-FIELDNAME = 'ZA4_AMT'.
  LV_FLDCAT-TABNAME   = 'GT_FINAL'.
  LV_FLDCAT-OUTPUTLEN = 15.
  LV_FLDCAT-SCRTEXT_M = '30 Day Above'.
*  LV_FLDCAT-ICON = ''.
*  LV_FLDCAT-DO_SUM = 'X'.
  APPEND LV_FLDCAT TO IT_FCAT1.
  CLEAR LV_FLDCAT.

  LV_FLDCAT-ROW_POS   = '1'.
  LV_FLDCAT-COL_POS   = '8'.
  LV_FLDCAT-FIELDNAME = 'ZA5_AMT'.
  LV_FLDCAT-TABNAME   = 'GT_FINAL'.
  LV_FLDCAT-OUTPUTLEN = 15.
  LV_FLDCAT-SCRTEXT_M = 'NT30'.
*  LV_FLDCAT-ICON = ''.
**   LV_FLDCAT-DO_SUM = 'X'.
  APPEND LV_FLDCAT TO IT_FCAT1.
  CLEAR LV_FLDCAT.
  LV_FLDCAT-ROW_POS   = '1'.
  LV_FLDCAT-COL_POS   = '9'.
  LV_FLDCAT-FIELDNAME = 'ZA6_AMT'.
  LV_FLDCAT-TABNAME   = 'GT_FINAL'.
  LV_FLDCAT-OUTPUTLEN = 15.
  LV_FLDCAT-SCRTEXT_M = '31-60 Days'.
*  LV_FLDCAT-ICON = ''.
*   LV_FLDCAT-DO_SUM = 'X'.
  APPEND LV_FLDCAT TO IT_FCAT1.
  CLEAR LV_FLDCAT.

  LV_FLDCAT-ROW_POS   = '1'.
  LV_FLDCAT-COL_POS   = '10'.
  LV_FLDCAT-FIELDNAME = 'ZA7_AMT'.
  LV_FLDCAT-TABNAME   = 'GT_FINAL'.
  LV_FLDCAT-OUTPUTLEN = 15.
  LV_FLDCAT-SCRTEXT_M = '61-90 Days'.
*  LV_FLDCAT-ICON = ''.
*   LV_FLDCAT-DO_SUM = 'X'.
  APPEND LV_FLDCAT TO IT_FCAT1.
  CLEAR LV_FLDCAT.

  LV_FLDCAT-ROW_POS   = '1'.
  LV_FLDCAT-COL_POS   = '11'.
  LV_FLDCAT-FIELDNAME = 'ZA8_AMT'.
  LV_FLDCAT-TABNAME   = 'GT_FINAL'.
  LV_FLDCAT-OUTPUTLEN = 15.
  LV_FLDCAT-SCRTEXT_M = 'Above 90 Days'.
*  LV_FLDCAT-ICON = ''.
*   LV_FLDCAT-DO_SUM = 'X'.
  APPEND LV_FLDCAT TO IT_FCAT1.
  CLEAR LV_FLDCAT.

  LV_FLDCAT-ROW_POS   = '1'.
  LV_FLDCAT-COL_POS   = '12'.
  LV_FLDCAT-FIELDNAME = 'PRCTR'.
  LV_FLDCAT-TABNAME   = 'GT_FINAL'.
  LV_FLDCAT-OUTPUTLEN = 15.
  LV_FLDCAT-SCRTEXT_M = 'Profit Center'.
*  LV_FLDCAT-ICON = ''.
*   LV_FLDCAT-DO_SUM = 'X'.
  APPEND LV_FLDCAT TO IT_FCAT1.
  CLEAR LV_FLDCAT.

 LV_FLDCAT-ROW_POS   = '1'.
  LV_FLDCAT-COL_POS   = '13'.
  LV_FLDCAT-FIELDNAME = 'ZA9_AMT'.
  LV_FLDCAT-TABNAME   = 'GT_FINAL'.
  LV_FLDCAT-OUTPUTLEN = 15.
  LV_FLDCAT-SCRTEXT_M = 'ZT15'.
*  LV_FLDCAT-ICON = ''.
*   LV_FLDCAT-DO_SUM = 'X'.
  APPEND LV_FLDCAT TO IT_FCAT1.
  CLEAR LV_FLDCAT.

 LV_FLDCAT-ROW_POS   = '1'.
  LV_FLDCAT-COL_POS   = '14'.
  LV_FLDCAT-FIELDNAME = 'ZA10_AMT'.
  LV_FLDCAT-TABNAME   = 'GT_FINAL'.
  LV_FLDCAT-OUTPUTLEN = 15.
  LV_FLDCAT-SCRTEXT_M = '16 - 30 Days'.
*  LV_FLDCAT-ICON = ''.
*   LV_FLDCAT-DO_SUM = 'X'.
  APPEND LV_FLDCAT TO IT_FCAT1.
  CLEAR LV_FLDCAT.

  LV_FLDCAT-ROW_POS   = '1'.
  LV_FLDCAT-COL_POS   = '15'.
  LV_FLDCAT-FIELDNAME = 'ZA11_AMT'.
  LV_FLDCAT-TABNAME   = 'GT_FINAL'.
  LV_FLDCAT-OUTPUTLEN = 15.
  LV_FLDCAT-SCRTEXT_M = '31 - 45 Days'.
*  LV_FLDCAT-ICON = ''.
*   LV_FLDCAT-DO_SUM = 'X'.
  APPEND LV_FLDCAT TO IT_FCAT1.
  CLEAR LV_FLDCAT.

  LV_FLDCAT-ROW_POS   = '1'.
  LV_FLDCAT-COL_POS   = '16'.
  LV_FLDCAT-FIELDNAME = 'ZA11_AMT'.
  LV_FLDCAT-TABNAME   = 'GT_FINAL'.
  LV_FLDCAT-OUTPUTLEN = 15.
  LV_FLDCAT-SCRTEXT_M = 'Above 45 Days'.
*  LV_FLDCAT-ICON = ''.
*   LV_FLDCAT-DO_SUM = 'X'.
  APPEND LV_FLDCAT TO IT_FCAT1.
  CLEAR LV_FLDCAT.

ENDFORM.                    " ALV_BUILD_FIELDCAT1
*&---------------------------------------------------------------------*
*&      Form  ALV_REPORT_LAYOUT1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ALV_REPORT_LAYOUT1 .
  IT_LAYOUT1-CWIDTH_OPT = 'X'.
  IT_LAYOUT1-ZEBRA = 'X'.
ENDFORM.                    " ALV_REPORT_LAYOUT1


*----------------------------------------------------------------------*
*  MODULE STATUS_9010 OUTPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE STATUS_9010 OUTPUT.
  SET PF-STATUS 'STANDARD'.
**  SET TITLEBAR 'XXX'.

ENDMODULE.                 " STATUS_9010  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9010  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9010 INPUT.
*  OK_CODE = SY-UCOMM.
  CASE SY-UCOMM.
    WHEN 'BACK'.
      LEAVE PROGRAM.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'CANCEL'.
      LEAVE PROGRAM.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_9010  INPUT
