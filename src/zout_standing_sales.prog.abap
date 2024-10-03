*&---------------------------------------------------------------------*
*& Report  ZOUT_STANDING_SALES
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ZOUT_STANDING_SALES.


*TYPES Decleration

TYPE-POOLS:SLIS .

*Table deceleration

TABLES:T001,T001W,VBRK,VBRP,MARA,BSID.

*Structure Decleration


TYPES: BEGIN OF STR_VBRK,
        VBELN TYPE VBRK-VBELN,
        FKART TYPE VBRK-FKART,
        VKORG TYPE VBRK-VKORG,
        VTWEG TYPE VBRK-VTWEG,
        KNUMV TYPE VBRK-KNUMV,
        FKDAT TYPE VBRK-FKDAT,
        BELNR TYPE VBRK-BELNR,
        ZTERM TYPE VBRK-ZTERM,
        REGIO TYPE VBRK-REGIO,
        BUKRS TYPE VBRK-BUKRS,
        NETWR TYPE VBRK-NETWR,
        KUNRG TYPE VBRK-KUNRG,
        SPART TYPE VBRK-SPART,
      END OF STR_VBRK.


TYPES: BEGIN OF STR_VBRP,

        VBELN TYPE VBRP-VBELN,
        POSNR TYPE VBRP-POSNR,
        FKIMG TYPE VBRP-FKIMG,
        VRKME TYPE VBRP-VRKME,
        NETWR TYPE VBRP-NETWR,
        AUBEL TYPE VBRP-AUBEL,
        MATNR TYPE VBRP-MATNR,
        ARKTX TYPE VBRP-ARKTX,
        CHARG TYPE VBRP-CHARG,
        MATKL TYPE VBRP-MATKL,
       WERKS TYPE VBRP-WERKS,
       VKBUR TYPE VBRP-VKBUR,
      END OF STR_VBRP.


TYPES: BEGIN OF STR_T001W,
        WERKS TYPE T001W-WERKS,
        NAME1 TYPE T001W-NAME1,
      END OF STR_T001W.


TYPES: BEGIN OF STR_MARA,
        MATNR TYPE MARA-MATNR,
        MTART TYPE MARA-MTART,
        MATKL TYPE MARA-MATKL,
        VOLUM TYPE MARA-VOLUM,
      END OF STR_MARA.

TYPES:BEGIN OF STR_MCHB,
       MATNR TYPE MCHB-MATNR,              " Material Code
       WERKS TYPE MCHB-WERKS,              " Valuation Area / Plant
       LGORT TYPE MCHB-LGORT,              "Stor. Location    Added by savariar s as on 21/10/2014.
       CHARG TYPE MCHB-CHARG,              " Batch
       ERSDA TYPE MCHB-ERSDA,              " Created On
       CLABS TYPE MCHB-CLABS,               " Stock Qty
       CINSM TYPE MCHB-CINSM,               "Quality Insp
       CSPEM TYPE MCHB-CSPEM,               " Blocked
    END OF STR_MCHB.

TYPES:BEGIN OF STR_MBEW,
        MATNR TYPE MBEW-MATNR,            "Material
        BWKEY TYPE MBEW-BWKEY,            "Valuation Area / Plant
        VPRSV TYPE MBEW-VPRSV,            " Price Control
        VERPR TYPE MBEW-VERPR,            "Moving Price
        STPRS TYPE MBEW-STPRS,            "Standard price
    END OF STR_MBEW .



TYPES: BEGIN OF GS_KNVV,
       KUNNR TYPE KNVV-KUNNR,                    " Customer Code
       VKBUR TYPE KNVV-VKBUR,                    " Customer Name
       END OF GS_KNVV.


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
       SHKZG TYPE BSID-SHKZG,                    " Debit/Credit ndicator
       END OF GS_BSID.


TYPES: BEGIN OF GS_BSAD,
BUKRS TYPE BSAD-BUKRS,                    " Company Code
AUGDT TYPE BSAD-AUGDT,                     " Clearing Date
AUGBL TYPE BSAD-AUGBL,                     " Document Number of the Clearing Document
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


TYPES:BEGIN OF ES_MARD,
       MATNR TYPE MARD-MATNR,
       WERKS TYPE MARD-WERKS,
       LABST TYPE MARD-LABST,
       SPEME TYPE MARD-SPEME,
     END OF ES_MARD.

DATA: GT_MARD TYPE TABLE OF ES_MARD,
      WA_MARD TYPE ES_MARD.

TYPES:BEGIN OF ES_MARC,
      MATNR TYPE MARC-MATNR,
      WERKS TYPE MARD-WERKS,
      TRAME TYPE MARC-TRAME,
     END OF ES_MARC.

DATA: GT_MARC TYPE TABLE OF ES_MARC,
      WA_MARC TYPE ES_MARC.




TYPES:BEGIN OF STR_FINAL,
        WERKS TYPE VBRP-WERKS,
        VBELN TYPE VBRK-VBELN,
        FKART TYPE VBRK-FKART,
        VKORG TYPE VBRK-VKORG,
        VTWEG TYPE VBRK-VTWEG,
        KNUMV TYPE VBRK-KNUMV,
        FKDAT TYPE VBRK-FKDAT,
*        BELNR TYPE VBRK-BELNR,
        ZTERM TYPE VBRK-ZTERM,
        REGIO TYPE VBRK-REGIO,
        BUKRS TYPE VBRK-BUKRS,
        NETWR TYPE VBRK-NETWR,
        KUNRG TYPE VBRK-KUNRG,
        SPART TYPE VBRK-SPART,
*   VBELN TYPE VBRP-VBELN,
        POSNR TYPE VBRP-POSNR,
        FKIMG TYPE VBRP-FKIMG,
        VRKME TYPE VBRP-VRKME,
*        NETWR TYPE VBRP-NETWR,
        AUBEL TYPE VBRP-AUBEL,
        MATNR TYPE VBRP-MATNR,
        ARKTX TYPE VBRP-ARKTX,
        CHARG TYPE VBRP-CHARG,
        MATKL TYPE VBRP-MATKL,
        VKBUR TYPE VBRP-VKBUR,
*        WERKS TYPE VBRP-WERKS,
*   WERKS TYPE T001W-WERKS,
        NAME1 TYPE T001W-NAME1,
*  MATNR TYPE MARA-MATNR,
        MTART TYPE MARA-MTART,
*        MATKL TYPE MARA-MATKL,
        VOLUM TYPE MARA-VOLUM,
   LGORT TYPE MCHB-LGORT,              "Stor. Location    Added by savariar s as on 21/10/2014.
*       CHARG TYPE MCHB-CHARG,              " Batch
       ERSDA TYPE MCHB-ERSDA,              " Created On
       CLABS TYPE MCHB-CLABS,               " Stock Qty
       CINSM TYPE MCHB-CINSM,               "Quality Insp
       CSPEM TYPE MCHB-CSPEM,               " Blocked

          BWKEY TYPE MBEW-BWKEY,            "Valuation Area / Plant
        VPRSV TYPE MBEW-VPRSV,            " Price Control
        VERPR TYPE MBEW-VERPR,            "Moving Price
        STPRS TYPE MBEW-STPRS,            "Standard price

        NETWR1 TYPE VBRK-NETWR,
        FKIMG1 TYPE VBRP-FKIMG,
        ASP TYPE P DECIMALS 2,
        NETWR1_PRE TYPE VBRK-NETWR,
        FKIMG1_PRE TYPE VBRP-FKIMG,
        ASP_PRE    TYPE P DECIMALS 2,
  PRICE TYPE P DECIMALS 2,       " STOCKVALUE
  STOCK_VOL TYPE P DECIMALS 2,       " STOCKVALUE
  STOCK_VAL TYPE P DECIMALS 2,       " STOCKVALUE


*
         AUGDT TYPE BSIK-AUGDT,                     " Clearing Date
       AUGBL TYPE BSIK-AUGBL,                     " Document Number of the Clearing Document
       KUNNR TYPE BSID-KUNNR,                    " Customer Code
       BELNR TYPE BSIK-BELNR,                    " Accounting Document Number
       DMBTR TYPE BSID-DMBTR,                    " Bill Amount ( In Doc Currency )
       BUDAT TYPE BSID-BUDAT,
       BLDAT TYPE BSID-BLDAT,
       ZFBDT TYPE BSID-ZFBDT,                    " Baseline Due Date For Calculation
       ZBD1T TYPE BSID-ZBD1T,                    " Credit Days
       PRCTR TYPE BSID-PRCTR,                    " Profit Center
       SHKZG TYPE BSID-SHKZG,                    " Debit/Credit Indicator

       LABST TYPE MARD-LABST,
       SPEME TYPE MARD-SPEME,

      TRAME TYPE MARC-TRAME,

      AMT TYPE P DECIMALS 2,
      T_DA TYPE I,

  END OF STR_FINAL.


DATA:NET_SUM TYPE VBRP-NETWR.
DATA:NET_SUM1 TYPE VBRP-NETWR.
DATA:VOL_SUM TYPE VBRP-FKIMG.
DATA:VOL_SUM1 TYPE VBRP-FKIMG.
DATA:STK_VOL TYPE P DECIMALS 2.
DATA:STK_VAL TYPE P DECIMALS 2.
DATA:DATE2 TYPE VBRK-FKDAT.
DATA:LV_ZFAEDT TYPE SY-DATUM.
DATA:LV_NDUEAMT TYPE P DECIMALS 2.
DATA:TOT_AMNT1 TYPE P DECIMALS 2.
DATA:TOT_AMNT TYPE P DECIMALS 2,
A1_AMNT TYPE P DECIMALS 2,
A2_AMNT TYPE P DECIMALS 2,
A3_AMNT TYPE P DECIMALS 2,
A4_AMNT TYPE P DECIMALS 2,
A5_AMNT TYPE P DECIMALS 2,
A6_AMNT TYPE P DECIMALS 2.

DATA:IND TYPE CHAR10.

DATA: GT_KNVV TYPE TABLE OF GS_KNVV,
      WA_KNVV TYPE GS_KNVV.

DATA: GT_BSID TYPE TABLE OF GS_BSID,
      WA_BSID TYPE GS_BSID,
      GT_BSAD TYPE TABLE OF GS_BSAD,
      WA_BSAD TYPE GS_BSAD.


*work area & internal table decleration

DATA:WA_VBRK TYPE STR_VBRK,
      IT_VBRK TYPE TABLE OF STR_VBRK,
      WA_VBRP TYPE STR_VBRP,
      IT_VBRP TYPE TABLE OF STR_VBRP,
      WA_T001W TYPE STR_T001W,
      IT_T001W TYPE TABLE OF STR_T001W,
      WA_MARA TYPE STR_MARA,
      IT_MARA TYPE TABLE OF STR_MARA,
      WA_FINAL TYPE STR_FINAL,
      WA_FINAL1 TYPE STR_FINAL,
      IT_FINAL TYPE TABLE OF STR_FINAL ,
      IT_FINAL1 TYPE TABLE OF STR_FINAL .

DATA:WA_MCHB TYPE STR_MCHB,
      IT_MCHB TYPE TABLE OF STR_MCHB,
      WA_MBEW  TYPE STR_MBEW ,
      IT_MBEW TYPE TABLE OF STR_MBEW .


DATA:MAT TYPE MATNR .

*ALV Decleration


DATA:WA_FCAT TYPE SLIS_FIELDCAT_ALV,
      IT_FCAT TYPE SLIS_T_FIELDCAT_ALV,
      WA_LAYOUT TYPE SLIS_LAYOUT_ALV,
      WA_SORT TYPE SLIS_SORTINFO_ALV,
      IT_SORT TYPE SLIS_T_SORTINFO_ALV.


*Input screen decleration

SELECTION-SCREEN BEGIN OF BLOCK A1 WITH FRAME TITLE TEXT-000.

SELECT-OPTIONS :S_BUKRS FOR VBRK-BUKRS,
                S_WERKS FOR VBRP-WERKS,
                S_FKDAT FOR VBRK-FKDAT NO-EXTENSION NO INTERVALS OBLIGATORY.

SELECTION-SCREEN END OF BLOCK A1.


DATA:DATDES TYPE SY-DATUM.
DATA:S_FKDAT1 TYPE SY-DATUM.
DATA:MON TYPE FCMNR.
DATA:DD TYPE CHAR2 VALUE '01'.
DATA:S_FKDAT3 TYPE SY-DATUM.
DATA:V1 TYPE SY-DATUM.
DATA:V2 TYPE SY-DATUM.
DATA:V3 TYPE SY-DATUM.
DATA:P_DATE TYPE SY-DATUM.
DATA:T_DAYS TYPE I.

DATA: CALC_DATE TYPE SY-DATUM,
      P_YEAR TYPE T5A4A-DLYYR,
      P_MONTH TYPE T5A4A-DLYMO.


S_FKDAT = S_FKDAT .
S_FKDAT1 = S_FKDAT-LOW .
S_FKDAT3 = S_FKDAT-LOW .

CONCATENATE    S_FKDAT1+4(2)
               S_FKDAT1+0(4)
               S_FKDAT1+6(2)
                INTO DATDES .

MON = DATDES .

CONCATENATE    S_FKDAT1+0(4)
               S_FKDAT1+4(2)
               S_FKDAT1+6(2)
                INTO DATDES .

CONCATENATE    S_FKDAT1+0(4)
               MON
               DD INTO DATDES.


V1 = DATDES.
V2 = S_FKDAT3.
P_DATE = V2.

CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
  EXPORTING
    DATE      = V2
    DAYS      = 1
    MONTHS    = P_MONTH
    SIGNUM    = '-'
    YEARS     = P_YEAR
  IMPORTING
    CALC_DATE = CALC_DATE.


V3 = CALC_DATE .

PERFORM READ_DATA.
PERFORM ALV_DIS.


*&---------------------------------------------------------------------*
*&      Form  Read_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM READ_DATA .


  SELECT VBELN "#EC CI_DB_OPERATION_OK[2768887] " Added by <IT-CAR Tool> during Code Remediation
         FKART
         VKORG
         VTWEG
         KNUMV
         FKDAT
         BELNR
         ZTERM
         REGIO
         BUKRS
         NETWR
         KUNRG
         SPART FROM VBRK
         INTO TABLE IT_VBRK
         WHERE BUKRS IN S_BUKRS AND  FKDAT BETWEEN V1 AND V2 .


  IF IT_VBRK IS NOT INITIAL.
    SELECT  VBELN
            POSNR
            FKIMG
            VRKME
            NETWR
            AUBEL
            MATNR
            ARKTX
            CHARG
            MATKL
            WERKS
            VKBUR FROM VBRP
            INTO TABLE IT_VBRP
            FOR ALL ENTRIES IN IT_VBRK
            WHERE VBELN = IT_VBRK-VBELN AND WERKS IN S_WERKS.


    SELECT
  KUNNR
  VKBUR
    FROM KNVV INTO TABLE GT_KNVV
      FOR ALL ENTRIES IN IT_VBRP
      WHERE VKBUR = IT_VBRP-WERKS .


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
KUNNR =  GT_KNVV-KUNNR ."AND BUKRS IN SO_BUKRS AND
*          UMSKZ NE 'F' AND  ZFBDT <= P_DATE.

*    SELECT
*    MATNR
*    WERKS
*    LABST
*    SPEME
*    FROM MARD INTO TABLE GT_MARD FOR ALL ENTRIES IN GT_KNVV WHERE WERKS = GT_KNVV-VKBUR." AND WERKS IN SO_VKBUR .
*
*    SELECT
*       MATNR
*       WERKS
*       TRAME
*    FROM MARC INTO TABLE GT_MARC FOR ALL ENTRIES IN GT_KNVV WHERE WERKS = GT_KNVV-VKBUR ."AND WERKS IN SO_VKBUR .


    SELECT MATNR
           MTART
           MATKL
           VOLUM FROM MARA
           INTO TABLE IT_MARA
           FOR ALL ENTRIES IN IT_VBRP
           WHERE MATNR = IT_VBRP-MATNR .

    SELECT WERKS
           NAME1 FROM T001W
           INTO TABLE IT_T001W
           FOR ALL ENTRIES IN IT_VBRP
           WHERE WERKS = IT_VBRP-WERKS.

    SELECT MATNR
           WERKS
           LGORT
           CHARG
           ERSDA
           CLABS
           CINSM
           CSPEM FROM MCHB INTO TABLE IT_MCHB
           FOR ALL ENTRIES IN IT_VBRP
           WHERE WERKS = IT_VBRP-WERKS." AND WERKS = IT_VBRP-WERKS.

    SELECT MATNR
           BWKEY
           VPRSV
           VERPR
           STPRS FROM MBEW
           INTO TABLE IT_MBEW
           FOR ALL ENTRIES IN IT_VBRP
           WHERE BWKEY = IT_VBRP-WERKS." AND BWKEY = IT_VBRP-WERKS.

  ENDIF.

  SORT IT_VBRP BY MATNR.

  LOOP AT IT_VBRP INTO WA_VBRP.

    WA_FINAL-VBELN = WA_VBRP-VBELN .
    WA_FINAL-POSNR = WA_VBRP-POSNR .
    WA_FINAL-FKIMG = WA_VBRP-FKIMG .
    WA_FINAL-VRKME = WA_VBRP-VRKME .
    WA_FINAL-NETWR = WA_VBRP-NETWR .
    WA_FINAL-AUBEL = WA_VBRP-AUBEL .
    WA_FINAL-MATNR = WA_VBRP-MATNR .
    WA_FINAL-ARKTX = WA_VBRP-ARKTX .
    WA_FINAL-CHARG = WA_VBRP-CHARG .
    WA_FINAL-MATKL = WA_VBRP-MATKL .
    WA_FINAL-WERKS = WA_VBRP-WERKS .

    READ TABLE IT_VBRK INTO WA_VBRK
    WITH KEY VBELN = WA_VBRP-VBELN.
    IF SY-SUBRC = 0.
      WA_FINAL-VBELN = WA_VBRK-VBELN .
      WA_FINAL-FKART = WA_VBRK-FKART .
      WA_FINAL-VKORG = WA_VBRK-VKORG .
      WA_FINAL-VTWEG = WA_VBRK-VTWEG .
      WA_FINAL-KNUMV = WA_VBRK-KNUMV .
      WA_FINAL-FKDAT = WA_VBRK-FKDAT .
      WA_FINAL-REGIO = WA_VBRK-REGIO .
      WA_FINAL-BUKRS = WA_VBRK-BUKRS .
*      WA_FINAL-NETWR = WA_VBRK-NETWR .
      WA_FINAL-KUNRG = WA_VBRK-KUNRG .
      WA_FINAL-SPART = WA_VBRK-SPART .
    ENDIF.

    READ TABLE IT_MARA INTO WA_MARA
    WITH KEY MATNR = WA_VBRP-MATNR.
    IF SY-SUBRC = 0.

      WA_FINAL-MATNR = WA_MARA-MATNR .
      WA_FINAL-VOLUM = WA_MARA-VOLUM .

    ENDIF.

    READ TABLE IT_T001W INTO WA_T001W
    WITH KEY WERKS = WA_VBRP-WERKS .

    IF SY-SUBRC = 0.
      WA_FINAL-NAME1 = WA_T001W-NAME1 .
    ENDIF.

    READ TABLE IT_MCHB INTO WA_MCHB
    WITH KEY WERKS = WA_VBRP-WERKS.

    IF SY-SUBRC = 0.

      IF MAT NE WA_FINAL-MATNR .

        MAT = WA_FINAL-MATNR .

        WA_FINAL-CLABS  = WA_MCHB-CLABS .

      ENDIF.

    ENDIF.

    READ TABLE IT_MBEW INTO WA_MBEW
    WITH KEY BWKEY = WA_VBRP-WERKS.

    IF SY-SUBRC = 0.
      WA_FINAL-VPRSV = WA_MBEW-VPRSV.
      IF WA_MBEW-VPRSV = 'S'.
        WA_FINAL-PRICE = WA_MBEW-STPRS.
      ELSEIF WA_MBEW-VPRSV = 'V'.
        WA_FINAL-PRICE = WA_MBEW-VERPR.
      ENDIF.
    ENDIF.

    APPEND WA_FINAL TO IT_FINAL.
    CLEAR WA_FINAL .


  ENDLOOP.


  SORT IT_FINAL BY WERKS FKDAT.


  DELETE IT_FINAL WHERE FKDAT EQ V2.


  LOOP AT IT_FINAL INTO WA_FINAL." WHERE FKDAT NE V2.


    NET_SUM = NET_SUM + WA_FINAL-NETWR.
    VOL_SUM = VOL_SUM + ( WA_FINAL-FKIMG * WA_FINAL-VOLUM ).
    STK_VOL = STK_VOL + ( WA_FINAL-CLABS * WA_FINAL-VOLUM ).
    STK_VAL = STK_VAL + ( WA_FINAL-CLABS * WA_FINAL-PRICE ) .

    IF WA_FINAL-FKDAT = V3.
      NET_SUM1 = NET_SUM1 + WA_FINAL-NETWR.
      VOL_SUM1 = VOL_SUM1 + ( WA_FINAL-FKIMG * WA_FINAL-VOLUM ).

      DATE2 = V3.
    ENDIF.

    AT END OF WERKS .

      WA_FINAL-NETWR1  = NET_SUM .
      WA_FINAL-FKIMG1  = VOL_SUM .

*   IF WA_FINAL-FKDAT = V3.
      WA_FINAL-NETWR1_PRE  = NET_SUM1 .
      WA_FINAL-FKIMG1_PRE  = VOL_SUM1 .

      WA_FINAL-STOCK_VOL = STK_VOL.
      WA_FINAL-STOCK_VAL = STK_VAL.

      IF WA_FINAL-FKIMG1_PRE NE 0.
        WA_FINAL-ASP_PRE = ( WA_FINAL-NETWR1_PRE / WA_FINAL-FKIMG1_PRE ).
      ENDIF.

      MODIFY IT_FINAL FROM WA_FINAL TRANSPORTING NETWR1_PRE FKIMG1_PRE ASP_PRE STOCK_VOL STOCK_VAL.

      CLEAR: NET_SUM1,VOL_SUM1 .
* ENDIF.


      IF WA_FINAL-FKIMG1 NE 0.
        WA_FINAL-ASP = ( WA_FINAL-NETWR1 / WA_FINAL-FKIMG1 ).
      ENDIF.
      MODIFY IT_FINAL FROM WA_FINAL TRANSPORTING NETWR1 FKIMG1 ASP.

      CLEAR WA_FINAL.
      CLEAR: NET_SUM,VOL_SUM .
    ENDAT.
  ENDLOOP.

  LOOP AT IT_FINAL INTO WA_FINAL.
AT END OF WERKS.
    LOOP AT GT_KNVV INTO WA_KNVV WHERE VKBUR = WA_FINAL-WERKS.

      LOOP AT GT_BSID INTO WA_BSID WHERE KUNNR = WA_KNVV-KUNNR.

        IF WA_BSID-SHKZG = 'S'.
          TOT_AMNT = TOT_AMNT + WA_BSID-DMBTR.
        ELSEIF WA_BSID-SHKZG = 'H'.
          TOT_AMNT = TOT_AMNT - WA_BSID-DMBTR.
        ENDIF.
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
        ENDIF.

        CLEAR WA_BSID.
      ENDLOOP.

      TOT_AMNT1 = TOT_AMNT1 + TOT_AMNT .
      TOT_AMNT = IND.

    WA_FINAL-T_DA = T_DAYS.
      WA_FINAL-AMT = TOT_AMNT1 .
      MODIFY IT_FINAL FROM WA_FINAL TRANSPORTING AMT T_DA.
      CLEAR WA_FINAL.


    ENDLOOP.
      clear TOT_AMNT1.
      clear TOT_AMNT.
      clear T_DAYS.

ENDAT.
  ENDLOOP.




  DELETE IT_FINAL WHERE NETWR1 = '0' .



ENDFORM .                    "Read_data


*&---------------------------------------------------------------------*
*&      Form  Alv_dis
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ALV_DIS .

  WA_FCAT-FIELDNAME = 'WERKS'.
  WA_FCAT-SELTEXT_M = 'Plant'.
  WA_FCAT-TABNAME = 'IT_FINAL'.
  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.


  WA_FCAT-FIELDNAME = 'NAME1'.
  WA_FCAT-SELTEXT_M = 'Branch Name'.
  WA_FCAT-TABNAME = 'IT_FINAL'.
  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.

  WA_FCAT-FIELDNAME = 'FKIMG1'.
  WA_FCAT-SELTEXT_M = 'Volume'.
  WA_FCAT-TABNAME = 'IT_FINAL'.
  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.


  WA_FCAT-FIELDNAME = 'NETWR1'.
  WA_FCAT-SELTEXT_M = 'Value'.
  WA_FCAT-TABNAME = 'IT_FINAL'.
  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.


  WA_FCAT-FIELDNAME = 'ASP'.
  WA_FCAT-SELTEXT_M = 'ASP'.
  WA_FCAT-TABNAME = 'IT_FINAL'.
  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.

  WA_FCAT-FIELDNAME = 'AMT'.
  WA_FCAT-SELTEXT_M = 'Outstanding'.
  WA_FCAT-TABNAME = 'IT_FINAL'.
  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.


  WA_FCAT-FIELDNAME = 'T_DA'.
  WA_FCAT-SELTEXT_M = 'Days'.
  WA_FCAT-TABNAME = 'IT_FINAL'.
  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.


  WA_FCAT-FIELDNAME = 'STOCK_VOL'.
  WA_FCAT-SELTEXT_M = 'Inventory Volume'.
  WA_FCAT-TABNAME = 'IT_FINAL'.
  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.


  WA_FCAT-FIELDNAME = 'STOCK_VAL'.
  WA_FCAT-SELTEXT_M = 'Inventory Value'.
  WA_FCAT-TABNAME = 'IT_FINAL'.
  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.


  WA_FCAT-FIELDNAME = 'FKIMG1_PRE'.
  WA_FCAT-SELTEXT_M = 'Pre.Day Volume'.
  WA_FCAT-TABNAME = 'IT_FINAL'.
  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.


  WA_FCAT-FIELDNAME = 'NETWR1_PRE'.
  WA_FCAT-SELTEXT_M = 'Pre.Day Value'.
  WA_FCAT-TABNAME = 'IT_FINAL'.
  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.


  WA_FCAT-FIELDNAME = 'ASP_PRE'.
  WA_FCAT-SELTEXT_M = 'Pre.Day ASP'.
  WA_FCAT-TABNAME = 'IT_FINAL'.
  APPEND WA_FCAT TO IT_FCAT.
  CLEAR WA_FCAT.






  WA_LAYOUT-ZEBRA = 'X'.
  WA_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.



  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
   EXPORTING
*   I_INTERFACE_CHECK                 = ' '
*   I_BYPASSING_BUFFER                = ' '
*   I_BUFFER_ACTIVE                   = ' '
   I_CALLBACK_PROGRAM                = SY-REPID
*   I_CALLBACK_PF_STATUS_SET          = ' '
*   I_CALLBACK_USER_COMMAND           = ' '
   I_CALLBACK_TOP_OF_PAGE            = 'TOP_OF_PAGE'
*   I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*   I_CALLBACK_HTML_END_OF_LIST       = ' '
*   I_STRUCTURE_NAME                  =
*   I_BACKGROUND_ID                   = ' '
*   I_GRID_TITLE                      =
*   I_GRID_SETTINGS                   =
     IS_LAYOUT                         = WA_LAYOUT
     IT_FIELDCAT                       = IT_FCAT
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
* Implement suitable error handling here
  ENDIF.


ENDFORM.                    "TOP_OF_PAGE









*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM TOP_OF_PAGE.

  DATA:WA_HEADER TYPE SLIS_LISTHEADER,
        IT_HEADER TYPE SLIS_T_LISTHEADER.

  WA_HEADER-TYP = 'H'.
  WA_HEADER-INFO  = 'Branch Outstanding Details'.
  APPEND WA_HEADER TO IT_HEADER.
  CLEAR WA_HEADER.



  WA_HEADER-TYP = 'S'.
  CONCATENATE 'Report Run Date:'
                 SY-DATUM+6(2) '-'
                 SY-DATUM+4(2) '-'
                 SY-DATUM+0(4) INTO WA_HEADER-INFO.
  APPEND WA_HEADER TO IT_HEADER.
  CLEAR WA_HEADER.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = IT_HEADER
      I_LOGO             = 'ZLOGO'
*     I_END_OF_LIST_GRID =
*     I_ALV_FORM         =
    .


ENDFORM.                    "Alv_dis
