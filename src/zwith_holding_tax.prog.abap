*&---------------------------------------------------------------------*
*& Report ZMM_ME2L_DOC
*&*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&Functional                   : Mr.Doss                    *
*& Developer                   : Mr.Ramachandaran M                      *
*& Company                     : Sheenlac Paints Pvt Ltd               *
*& Verified By                 :                                       *
*& Title                       : Withholding tax report     *
*& Report Name                 : ZWITH_HOLDING_TAX                  *
*& Development Id              : kpabap                                *
*& Related Information         : Withholding tax report      *
*&---------------------------------------------------------------------*

REPORT ZWITH_HOLDING_TAX.

TYPES : BEGIN OF GS_BSEG,
            BUKRS TYPE BSEG-BUKRS,
            BELNR TYPE BSEG-BELNR,
            GJAHR TYPE BSEG-GJAHR,
            BUZEI TYPE BSEG-BUZEI,
            BUZID TYPE BSEG-BUZID,
            AUGDT TYPE BSEG-AUGDT,
            AUGCP TYPE BSEG-AUGCP,
            AUGBL TYPE BSEG-AUGBL,
            BSCHL TYPE BSEG-BSCHL,
            KOART TYPE BSEG-KOART,
            UMSKZ TYPE BSEG-UMSKZ,
            UMSKS TYPE BSEG-UMSKS,
            SHKZG TYPE BSEG-SHKZG,
            MWSKZ TYPE BSEG-MWSKZ,
            QSSKZ TYPE BSEG-QSSKZ,
            DMBTR TYPE BSEG-DMBTR,
            TXBHW TYPE BSEG-TXBHW,
            SKFBT TYPE BSEG-SKFBT,
            MWART TYPE BSEG-MWART,
            SAKNR TYPE BSEG-SAKNR,
            HKONT TYPE BSEG-HKONT,
            LIFNR TYPE BSEG-LIFNR,
         END OF GS_BSEG.

TYPES : BEGIN OF GS_BKPF,
          BUKRS TYPE BKPF-BUKRS,
          BELNR TYPE BKPF-BELNR,
          GJAHR TYPE BKPF-GJAHR,
          BLART TYPE BKPF-BLART,
          BLDAT TYPE BKPF-BLDAT,
          BUDAT TYPE BKPF-BUDAT,
       END OF GS_BKPF.

TYPES : BEGIN OF GS_LFA1,
          LIFNR TYPE LFA1-LIFNR,
          NAME1 TYPE LFA1-NAME1,
          "NAME2 TYPE LFA1-NAME2,
          "NAME3 TYPE LFA1-NAME3,
          "NAME4 TYPE LFA1-NAME4,
          "ORT01 TYPE LFA1-ORT01,
       END OF GS_LFA1.

TYPES : BEGIN OF GS_J_1IMOVEND,
          LIFNR TYPE J_1IMOVEND-LIFNR, "#EC CI_USAGE_OK[2877717] " Added by <IT-CAR Tool> during Code Remediation
          J_1IPANNO TYPE J_1IMOVEND-J_1IPANNO, "#EC CI_USAGE_OK[2877717] " Added by <IT-CAR Tool> during Code Remediation
        END OF GS_J_1IMOVEND.

TYPES : BEGIN OF GS_T059Z,
        LAND1 TYPE T059Z-LAND1,
        WITHT TYPE T059Z-WITHT,
        WT_WITHCD TYPE T059Z-WT_WITHCD,
        QSCOD TYPE T059Z-QSCOD,
        QPROZ TYPE T059Z-QPROZ,
        QSATZ TYPE T059Z-QSATZ,
        QSATR TYPE T059Z-QSATR,
        "TEXT40 TYPE T059Z-TEXT40,
      END OF GS_T059Z.

TYPES : BEGIN OF GS_T059ZT,
         SPRAS TYPE T059ZT-SPRAS,
         LAND1 TYPE T059ZT-LAND1,
         WITHT TYPE T059ZT-WITHT,
         WT_WITHCD TYPE T059ZT-WT_WITHCD,
         TEXT40 TYPE T059ZT-TEXT40,
        END OF GS_T059ZT.


TYPES : BEGIN OF GS_BSIK,
             BUKRS TYPE BSIK-BUKRS,
             LIFNR TYPE BSIK-LIFNR,
             UMSKS TYPE BSIK-UMSKS,
             UMSKZ TYPE BSIK-UMSKZ,
             AUGDT TYPE BSIK-AUGDT,
             AUGBL TYPE BSIK-AUGBL,
             ZUONR TYPE BSIK-ZUONR,
             GJAHR TYPE BSIK-GJAHR,
             BELNR TYPE BSIK-BELNR,
             BUZEI TYPE BSIK-BUZEI,
             BUDAT TYPE BSIK-BUDAT,
             BLDAT TYPE BSIK-BLDAT,
             XBLNR TYPE BSIK-XBLNR,
             BLART TYPE BSIK-BLART,
             DMBTR TYPE BSIK-DMBTR,
             WRBTR TYPE BSIK-WRBTR,
             SAKNR TYPE BSIK-SAKNR,
             MWSTS TYPE BSIK-MWSTS,
             QSSHB TYPE BSIK-QSSHB,
             QBSHB TYPE BSIK-QBSHB,
           END OF GS_BSIK.

TYPES : BEGIN OF GS_BSAK,
            BUKRS TYPE BSAK-BUKRS,
            LIFNR TYPE BSAK-LIFNR,
            UMSKS TYPE BSAK-UMSKS,
            UMSKZ TYPE BSAK-UMSKZ,
            AUGDT TYPE BSAK-AUGDT,
            AUGBL TYPE BSAK-AUGBL,
            ZUONR TYPE BSAK-ZUONR,
            GJAHR TYPE BSAK-GJAHR,
            BELNR TYPE BSAK-BELNR,
            BUZEI TYPE BSAK-BUZEI,
            BUDAT TYPE BSAK-BUDAT,
            BLDAT TYPE BSAK-BLDAT,
            XBLNR TYPE BSAK-XBLNR,
            BLART TYPE BSAK-BLART,
            DMBTR TYPE BSAK-DMBTR,
            WRBTR TYPE BSAK-WRBTR,
            SAKNR TYPE BSAK-SAKNR,
            MWSTS TYPE BSAK-MWSTS,
            QSSHB TYPE BSAK-QSSHB,
            QBSHB TYPE BSAK-QBSHB,
          END OF GS_BSAK.

TYPES : BEGIN OF GS_FINAL,
*          BUKRS TYPE BSEG-BUKRS,
*          BELNR TYPE BSEG-BELNR,
*          GJAHR TYPE BSEG-GJAHR,
*          BUZEI TYPE BSEG-BUZEI,
*          BUZID TYPE BSEG-BUZID,
*          AUGDT TYPE BSEG-AUGDT,
*          AUGCP TYPE BSEG-AUGCP,
*          AUGBL TYPE BSEG-AUGBL,
*          BSCHL TYPE BSEG-BSCHL,
*          KOART TYPE BSEG-KOART,
*          UMSKZ TYPE BSEG-UMSKZ,
*          UMSKS TYPE BSEG-UMSKS,
*          MWSKZ TYPE BSEG-MWSKZ,
*          QSSKZ TYPE BSEG-QSSKZ,
*          DMBTR TYPE BSEG-DMBTR,
*          MWART TYPE BSEG-MWART,
*          SAKNR TYPE BSEG-SAKNR,
*          LIFNR TYPE BSEG-LIFNR,
*          SKFBT TYPE BSEG-SKFBT,
*          "BELNR TYPE BKPF-BELNR,
*          "GJAHR TYPE BKPF-GJAHR,
*          BLART TYPE BKPF-BLART,
*          BLDAT TYPE BKPF-BLDAT,
*          BUDAT TYPE BKPF-BUDAT,
          "LIFNR TYPE LFA1-LIFNR,
                BUKRS TYPE BSIK-BUKRS,
                LIFNR TYPE BSIK-LIFNR,
                UMSKS TYPE BSIK-UMSKS,
                UMSKZ TYPE BSIK-UMSKZ,
                AUGDT TYPE BSIK-AUGDT,
                AUGBL TYPE BSIK-AUGBL,
                ZUONR TYPE BSIK-ZUONR,
                GJAHR TYPE BSIK-GJAHR,
                BELNR TYPE BSIK-BELNR,
                BUZEI TYPE BSIK-BUZEI,
                BUDAT TYPE BSIK-BUDAT,
                BLDAT TYPE BSIK-BLDAT,
                XBLNR TYPE BSIK-XBLNR,
                BLART TYPE BSIK-BLART,
                "DMBTR TYPE BSIK-DMBTR,
                WRBTR TYPE BSIK-WRBTR,
                SAKNR TYPE BSIK-SAKNR,
                HKONT TYPE BSIK-HKONT,
                MWSTS TYPE BSIK-MWSTS,
                QSSHB TYPE BSIK-QSSHB,
                QBSHB TYPE BSIK-QBSHB,
                QSSKZ TYPE BSEG-QSSKZ,
                SHKZG TYPE BSEG-SHKZG,
                DMBTR TYPE BSEG-DMBTR,
                TXBHW TYPE BSEG-TXBHW,
                SKFBT TYPE BSEG-SKFBT,
          NAME1 TYPE LFA1-NAME1,
          NAME2 TYPE LFA1-NAME2,
          NAME3 TYPE LFA1-NAME3,
          NAME4 TYPE LFA1-NAME4,
          ORT01 TYPE LFA1-ORT01,
          "LIFNR TYPE J_1IMOVEND-LIFNR,
          J_1IPANNO TYPE J_1IMOVEND-J_1IPANNO, "#EC CI_USAGE_OK[2877717] " Added by <IT-CAR Tool> during Code Remediation
          LAND1 TYPE T059Z-LAND1,
          WITHT TYPE T059Z-WITHT,
          WT_WITHCD TYPE T059Z-WT_WITHCD,
          QSCOD TYPE T059Z-QSCOD,
          QPROZ TYPE T059Z-QPROZ,
          QSATZ TYPE T059Z-QSATZ,
          QSATR TYPE T059Z-QSATR,
          TEXT40 TYPE T059ZT-TEXT40,
          BASE_AMO TYPE BSEG-DMBTR,
      END OF GS_FINAL.

DATA: GT_BSEG TYPE TABLE OF GS_BSEG,
      WA_BSEG TYPE GS_BSEG,
      GT_BSEG1 TYPE TABLE OF GS_BSEG,
      WA_BSEG1 TYPE GS_BSEG,
      GT_BSEG2 TYPE TABLE OF GS_BSEG,
      WA_BSEG2 TYPE GS_BSEG,
      GT_BSEG3 TYPE TABLE OF GS_BSEG,
      WA_BSEG3 TYPE GS_BSEG,
      GT_BKPF TYPE TABLE OF GS_BKPF,
      WA_BKPF TYPE GS_BKPF,
      GT_LFA1 TYPE TABLE OF GS_LFA1,
      WA_LFA1 TYPE GS_LFA1,
      GT_J_1IMOVEND TYPE TABLE OF GS_J_1IMOVEND,
      WA_J_1IMOVEND TYPE GS_J_1IMOVEND,
      GT_T059Z TYPE TABLE OF GS_T059Z,
      WA_T059Z TYPE GS_T059Z ,
      GT_T059ZT TYPE TABLE OF GS_T059ZT,
      WA_T059ZT TYPE GS_T059ZT,
      GT_BSIK TYPE TABLE OF GS_BSIK,
      WA_BSIK TYPE GS_BSIK,
      GT_BSAK TYPE TABLE OF GS_BSAK,
      WA_BSAK TYPE GS_BSAK.

DATA : GT_FINAL TYPE TABLE OF GS_FINAL,
       WA_FINAL TYPE GS_FINAL.

DATA : GT_FIELDCAT TYPE TABLE OF SLIS_FIELDCAT_ALV,
       WA_FIELDCAT TYPE SLIS_FIELDCAT_ALV,
       LAYOUT TYPE SLIS_LAYOUT_ALV.

DATA : LV_BUKRS TYPE BSEG-BUKRS,
       LV_LIFNR TYPE BSEG-LIFNR,
       LV_BUDAT TYPE BKPF-BUDAT,
       LV_BLART TYPE BKPF-BLART .

SELECTION-SCREEN : BEGIN OF BLOCK B1 WITH FRAME .
SELECT-OPTIONS : SO_BUKRS FOR LV_BUKRS OBLIGATORY ,
                 SO_LIFNR FOR LV_LIFNR ,
                 SO_BUDAT FOR LV_BUDAT  ,
                 SO_BLART FOR LV_BLART  NO INTERVALS NO-EXTENSION  .

SELECTION-SCREEN : END OF BLOCK B1.

*----------------------------------------------------------------------*
*       CLASS PRICE DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS PRICE DEFINITION.
  PUBLIC SECTION.
    METHODS : GET_DATA,
              ALV_FIELDCAT,
              DISPLAY_DATA.
ENDCLASS.                    "PRICE DEFINITION

*----------------------------------------------------------------------*
*       CLASS PRICE IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS PRICE IMPLEMENTATION.
  METHOD : GET_DATA.
    PERFORM GET_DATA.
  ENDMETHOD.                    ":
  METHOD: ALV_FIELDCAT.
    PERFORM ALV_FIELDCAT.
  ENDMETHOD.                    "ALV_FIELDCAT
  METHOD DISPLAY_DATA.
    PERFORM DISPLAY_DATA.
  ENDMETHOD.                    "DISPLAY_DATA
ENDCLASS.                    "PRICE IMPLEMENTATION

START-OF-SELECTION.
  DATA : PRICE TYPE REF TO PRICE  .

  CREATE OBJECT PRICE.

  CALL METHOD PRICE->GET_DATA.
  CALL METHOD PRICE->ALV_FIELDCAT.
  CALL METHOD PRICE->DISPLAY_DATA.

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
                BUKRS
                LIFNR
                UMSKS
                UMSKZ
                AUGDT
                AUGBL
                ZUONR
                GJAHR
                BELNR
                BUZEI
                BUDAT
                BLDAT
                XBLNR
                BLART
                DMBTR
                WRBTR
                SAKNR
                MWSTS
                QSSHB
                QBSHB
      FROM BSIK INTO TABLE GT_BSIK WHERE BUKRS IN SO_BUKRS AND LIFNR IN SO_LIFNR AND BUDAT IN SO_BUDAT AND BLART IN SO_BLART .

  SELECT
            BUKRS
            LIFNR
            UMSKS
            UMSKZ
            AUGDT
            AUGBL
            ZUONR
            GJAHR
            BELNR
            BUZEI
            BUDAT
            BLDAT
            XBLNR
            BLART
            DMBTR
            WRBTR
            SAKNR
            MWSTS
            QSSHB
            QBSHB
  FROM BSAK INTO TABLE GT_BSAK WHERE BUKRS IN SO_BUKRS AND LIFNR IN SO_LIFNR AND BUDAT IN SO_BUDAT AND BLART IN SO_BLART .

  IF GT_BSIK IS NOT INITIAL.


    SELECT "#EC CI_DB_OPERATION_OK[2431747] " Added by <IT-CAR Tool> during Code Remediation
            BUKRS
            BELNR
            GJAHR
            BUZEI
            BUZID
            AUGDT
            AUGCP
            AUGBL
            BSCHL
            KOART
            UMSKZ
            UMSKS
            SHKZG
            MWSKZ
            QSSKZ
            DMBTR
            TXBHW
            SKFBT
            MWART
            SAKNR
            HKONT
            LIFNR
          FROM   BSEG INTO TABLE GT_BSEG FOR ALL ENTRIES IN GT_BSIK WHERE BELNR = GT_BSIK-BELNR AND GJAHR = GT_BSIK-GJAHR AND BUKRS = GT_BSIK-BUKRS
    AND SHKZG EQ 'H' AND QSSKZ <> ' ' AND QSSKZ <> 'XX'  ORDER BY PRIMARY KEY.  " Added by <IT-CAR Tool> during Code Remediation




    SELECT "#EC CI_DB_OPERATION_OK[2431747] " Added by <IT-CAR Tool> during Code Remediation
      BUKRS
      BELNR
      GJAHR
      BUZEI
      BUZID
      AUGDT
      AUGCP
      AUGBL
      BSCHL
      KOART
      UMSKZ
      UMSKS
      SHKZG
      MWSKZ
      QSSKZ
      DMBTR
      TXBHW
      SKFBT
      MWART
      SAKNR
      HKONT
      LIFNR
    FROM   BSEG INTO TABLE GT_BSEG1 FOR ALL ENTRIES IN GT_BSIK WHERE BELNR = GT_BSIK-BELNR AND GJAHR = GT_BSIK-GJAHR AND BUKRS = GT_BSIK-BUKRS
  AND MWSKZ EQ 'I8'   ORDER BY PRIMARY KEY.  " Added by <IT-CAR Tool> during Code Remediation



  ENDIF.

  IF GT_BSAK IS NOT INITIAL.


  SELECT "#EC CI_DB_OPERATION_OK[2431747] " Added by <IT-CAR Tool> during Code Remediation
    BUKRS
    BELNR
    GJAHR
    BUZEI
    BUZID
    AUGDT
    AUGCP
    AUGBL
    BSCHL
    KOART
    UMSKZ
    UMSKS
    SHKZG
    MWSKZ
    QSSKZ
    DMBTR
    TXBHW
    SKFBT
    MWART
    SAKNR
    HKONT
    LIFNR
  FROM   BSEG INTO TABLE GT_BSEG2 FOR ALL ENTRIES IN GT_BSAK WHERE BELNR = GT_BSAK-BELNR AND GJAHR = GT_BSAK-GJAHR AND BUKRS = GT_BSAK-BUKRS
AND SHKZG EQ 'H' AND QSSKZ <> ' ' AND QSSKZ <> 'XX'  ORDER BY PRIMARY KEY.  " Added by <IT-CAR Tool> during Code Remediation

*AND BUKRS IN SO_BUKRS AND LIFNR IN SO_LIFNR,

  SELECT "#EC CI_DB_OPERATION_OK[2431747] " Added by <IT-CAR Tool> during Code Remediation
    BUKRS
    BELNR
    GJAHR
    BUZEI
    BUZID
    AUGDT
    AUGCP
    AUGBL
    BSCHL
    KOART
    UMSKZ
    UMSKS
    SHKZG
    MWSKZ
    QSSKZ
    DMBTR
    TXBHW
    SKFBT
    MWART
    SAKNR
    HKONT
    LIFNR
  FROM   BSEG INTO TABLE GT_BSEG3 FOR ALL ENTRIES IN GT_BSAK WHERE BELNR = GT_BSAK-BELNR AND GJAHR = GT_BSAK-GJAHR AND BUKRS = GT_BSAK-BUKRS
AND MWSKZ EQ 'I8'   ORDER BY PRIMARY KEY.  " Added by <IT-CAR Tool> during Code Remediation
*BUKRS IN SO_BUKRS AND LIFNR IN SO_LIFNR
  ENDIF.

*  SELECT
*            BUKRS
*            BELNR
*            GJAHR
*            BUZEI
*            BUZID
*            AUGDT
*            AUGCP
*            AUGBL
*            BSCHL
*            KOART
*            UMSKZ
*            UMSKS
*            MWSKZ
*            QSSKZ
*            DMBTR
*            MWART
*            SAKNR
*            LIFNR
*            SKFBT
*     FROM   BSEG INTO TABLE GT_BSEG WHERE BUKRS IN SO_BUKRS AND LIFNR IN SO_LIFNR .
*
*  SELECT
*          BUKRS
*          BELNR
*          GJAHR
*          BLART
*          BLDAT
*          BUDAT
*      FROM BKPF INTO TABLE GT_BKPF FOR ALL ENTRIES IN GT_BSEG WHERE BELNR = GT_BSEG-BELNR AND GJAHR = GT_BSEG-GJAHR AND
*                   BUKRS = GT_BSEG-BUKRS AND BUDAT IN SO_BUDAT AND BLART IN SO_BLART AND BUKRS IN SO_BUKRS .

*  SELECT
*          BUKRS
*          BELNR
*          GJAHR
*          BLART
*          BLDAT
*          BUDAT
*      FROM BKPF INTO TABLE GT_BKPF WHERE BUDAT IN SO_BUDAT AND BLART IN SO_BLART AND BUKRS IN SO_BUKRS .


*  SELECT
*            BUKRS
*            BELNR
*            GJAHR
*            BUZEI
*            BUZID
*            AUGDT
*            AUGCP
*            AUGBL
*            BSCHL
*            KOART
*            UMSKZ
*            UMSKS
*            MWSKZ
*            QSSKZ
*            DMBTR
*            MWART
*            SAKNR
*            LIFNR
*            SKFBT
*     FROM   BSEG INTO TABLE GT_BSEG FOR ALL ENTRIES IN GT_BKPF WHERE BUKRS IN SO_BUKRS AND BELNR = GT_BKPF-BELNR AND GJAHR = GT_BKPF-GJAHR AND BUKRS = GT_BKPF-BUKRS
*                                                                     AND LIFNR IN SO_LIFNR  .

 APPEND LINES OF GT_BSAK TO GT_BSIK .

  SELECT
     LIFNR
     NAME1
   FROM LFA1 INTO TABLE GT_LFA1 FOR ALL ENTRIES IN GT_BSIK WHERE LIFNR = GT_BSIK-LIFNR .

  SELECT
 LIFNR
 J_1IPANNO
FROM LFA1 INTO TABLE GT_J_1IMOVEND FOR ALL ENTRIES IN GT_BSIK WHERE LIFNR = GT_BSIK-LIFNR .

  SELECT
          LAND1
          WITHT
          WT_WITHCD
          QSCOD
          QPROZ
          QSATZ
          QSATR
    FROM T059Z INTO TABLE GT_T059Z FOR ALL ENTRIES IN GT_BSEG WHERE  WT_WITHCD = GT_BSEG-QSSKZ AND LAND1 = 'IN'  .

  SELECT
         SPRAS
         LAND1
         WITHT
         WT_WITHCD
         TEXT40
  FROM T059ZT INTO TABLE GT_T059ZT FOR ALL ENTRIES IN GT_BSEG WHERE  WT_WITHCD = GT_BSEG-QSSKZ AND SPRAS = 'EN' AND LAND1 = 'IN' .

*  LOOP AT GT_BSEG INTO WA_BSEG .
*    MOVE-CORRESPONDING WA_BSEG TO WA_FINAL.
*
*    READ TABLE GT_LFA1 INTO WA_LFA1 WITH KEY LIFNR = WA_BSEG-LIFNR.
*    IF SY-SUBRC = 0 .
*      WA_FINAL-NAME1 = WA_LFA1-NAME1.
*    ENDIF.
*
*    READ TABLE GT_T059Z INTO WA_T059Z WITH KEY WT_WITHCD = WA_BSEG-QSSKZ .
*    IF SY-SUBRC = 0 .
*      WA_FINAL-QSATZ = WA_T059Z-QSATZ.
*     ENDIF.
*
*     READ TABLE GT_T059ZT INTO WA_T059ZT WITH KEY WT_WITHCD = WA_BSEG-QSSKZ .
*    IF SY-SUBRC = 0 .
*      WA_FINAL-TEXT40 = WA_T059ZT-TEXT40.
*     ENDIF.
*
*    READ TABLE GT_J_1IMOVEND INTO WA_J_1IMOVEND WITH KEY LIFNR = WA_BSEG-LIFNR.
*    IF SY-SUBRC = 0 .
*      WA_FINAL-J_1IPANNO = WA_J_1IMOVEND-J_1IPANNO.
*    ENDIF.
*
*    LOOP AT GT_BKPF INTO WA_BKPF WHERE BUKRS = WA_BSEG-BUKRS AND BELNR = WA_BSEG-BELNR AND GJAHR = WA_BSEG-GJAHR .
*      WA_FINAL-BUDAT = WA_BKPF-BUDAT .
*      WA_FINAL-BLART = WA_BKPF-BLART .
*    ENDLOOP.
*
*    SHIFT WA_FINAL-LIFNR LEFT DELETING LEADING '0' .
*    SHIFT WA_FINAL-SAKNR LEFT DELETING LEADING '0' .
*    APPEND WA_FINAL TO GT_FINAL .
*    CLEAR WA_FINAL .
*  ENDLOOP.

*  LOOP AT GT_BKPF INTO WA_BKPF .
*    MOVE-CORRESPONDING WA_BKPF TO WA_FINAL.
*
*    READ TABLE GT_LFA1 INTO WA_LFA1 WITH KEY LIFNR = WA_BSEG-LIFNR.
*    IF SY-SUBRC = 0 .
*      WA_FINAL-NAME1 = WA_LFA1-NAME1.
*    ENDIF.
*
*    READ TABLE GT_T059Z INTO WA_T059Z WITH KEY WT_WITHCD = WA_BSEG-QSSKZ .
*    IF SY-SUBRC = 0 .
*      WA_FINAL-QSATZ = WA_T059Z-QSATZ.
*     ENDIF.
*
*     READ TABLE GT_T059ZT INTO WA_T059ZT WITH KEY WT_WITHCD = WA_BSEG-QSSKZ .
*    IF SY-SUBRC = 0 .
*      WA_FINAL-TEXT40 = WA_T059ZT-TEXT40.
*     ENDIF.
*
*
*    READ TABLE GT_J_1IMOVEND INTO WA_J_1IMOVEND WITH KEY LIFNR = WA_BSEG-LIFNR.
*    IF SY-SUBRC = 0 .
*      WA_FINAL-J_1IPANNO = WA_J_1IMOVEND-J_1IPANNO.
*    ENDIF.
*
*    LOOP AT GT_BKPF INTO WA_BKPF WHERE BUKRS = WA_BSEG-BUKRS AND BELNR = WA_BSEG-BELNR AND GJAHR = WA_BSEG-GJAHR .
*      WA_FINAL-BUDAT = WA_BKPF-BUDAT .
*      WA_FINAL-BLART = WA_BKPF-BLART .
*    ENDLOOP.
*
*    SHIFT WA_FINAL-LIFNR LEFT DELETING LEADING '0' .
*    SHIFT WA_FINAL-SAKNR LEFT DELETING LEADING '0' .
*    APPEND WA_FINAL TO GT_FINAL .
*    CLEAR WA_FINAL .
*  ENDLOOP.



*LOOP AT GT_BSIK INTO WA_BSIK .
*
*   MOVE-CORRESPONDING WA_BSIK TO WA_FINAL.
*
*    LOOP AT GT_BSEG INTO WA_BSEG WHERE BUKRS = WA_BSIK-BUKRS AND BELNR = WA_BSIK-BELNR AND GJAHR = WA_BSIK-GJAHR .
*       WA_FINAL-QSSKZ = WA_BSEG-QSSKZ .
*
*    READ TABLE GT_T059Z INTO WA_T059Z WITH KEY WT_WITHCD = WA_FINAL-QSSKZ .
*    IF SY-SUBRC = 0 .
*      WA_FINAL-QSATZ = WA_T059Z-QSATZ.
*     ENDIF.
*    READ TABLE GT_T059ZT INTO WA_T059ZT WITH KEY WT_WITHCD = WA_FINAL-QSSKZ .
*    IF SY-SUBRC = 0 .
*      WA_FINAL-TEXT40 = WA_T059ZT-TEXT40.
*     ENDIF.
*    ENDLOOP.
*
*  READ TABLE GT_LFA1 INTO WA_LFA1 WITH KEY LIFNR = WA_BSIK-LIFNR.
*    IF SY-SUBRC = 0 .
*     WA_FINAL-NAME1 = WA_LFA1-NAME1.
*   ENDIF.
*  READ TABLE GT_J_1IMOVEND INTO WA_J_1IMOVEND WITH KEY LIFNR = WA_BSIK-LIFNR.
*    IF SY-SUBRC = 0 .
*      WA_FINAL-J_1IPANNO = WA_J_1IMOVEND-J_1IPANNO.
*    ENDIF.
*
*   APPEND WA_FINAL TO GT_FINAL .
*   CLEAR WA_FINAL .
*ENDLOOP.



  APPEND LINES OF GT_BSEG2 TO GT_BSEG.

  APPEND LINES OF GT_BSEG3 TO GT_BSEG1.

  LOOP AT GT_BSEG INTO WA_BSEG .

    WA_FINAL-DMBTR = WA_BSEG-DMBTR .
    "WA_FINAL-SKFBT = WA_BSEG-SKFBT .
    WA_FINAL-HKONT = WA_BSEG-HKONT.
    LOOP AT GT_BSIK INTO WA_BSIK WHERE BUKRS = WA_BSEG-BUKRS AND BELNR = WA_BSEG-BELNR AND GJAHR = WA_BSEG-GJAHR .
      WA_FINAL-BELNR = WA_BSIK-BELNR .
      WA_FINAL-LIFNR = WA_BSIK-LIFNR .
      WA_FINAL-BUDAT = WA_BSIK-BUDAT .
      WA_FINAL-BLART = WA_BSIK-BLART .
      WA_FINAL-QSSHB = WA_FINAL-QSSHB + WA_BSIK-QSSHB.
      WA_FINAL-SAKNR = WA_BSIK-SAKNR.
    ENDLOOP.


    LOOP AT GT_BSEG1 INTO WA_BSEG1 WHERE BUKRS = WA_BSEG-BUKRS AND BELNR = WA_BSEG-BELNR AND GJAHR = WA_BSEG-GJAHR .
      IF SY-SUBRC = 0 .
        WA_FINAL-BASE_AMO = WA_FINAL-BASE_AMO + WA_BSEG1-TXBHW.
      ENDIF.
    ENDLOOP.

    READ TABLE GT_T059Z INTO WA_T059Z WITH KEY WT_WITHCD = WA_BSEG-QSSKZ .
    IF SY-SUBRC = 0 .
      WA_FINAL-QSCOD = WA_T059Z-QSCOD.
      WA_FINAL-QSATZ = WA_T059Z-QSATZ.
    ENDIF.
    READ TABLE GT_T059ZT INTO WA_T059ZT WITH KEY WT_WITHCD = WA_BSEG-QSSKZ .
    IF SY-SUBRC = 0 .
      WA_FINAL-TEXT40 = WA_T059ZT-TEXT40.
    ENDIF.


    READ TABLE GT_LFA1 INTO WA_LFA1 WITH KEY LIFNR = WA_BSIK-LIFNR.
    IF SY-SUBRC = 0 .
      WA_FINAL-NAME1 = WA_LFA1-NAME1.
    ENDIF.

    READ TABLE GT_J_1IMOVEND INTO WA_J_1IMOVEND WITH KEY LIFNR = WA_BSIK-LIFNR.
    IF SY-SUBRC = 0 .
      WA_FINAL-J_1IPANNO = WA_J_1IMOVEND-J_1IPANNO.
    ENDIF.

    APPEND WA_FINAL TO GT_FINAL .
    CLEAR WA_FINAL .
  ENDLOOP.


   DELETE GT_FINAL WHERE QSATZ = '0.0000' .
   DELETE GT_FINAL WHERE BLART = 'KA' .

  "DELETE GT_FINAL WHERE QSSKZ = ' ' .

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  ALV_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ALV_FIELDCAT .

*  WA_FIELDCAT-FIELDNAME   = 'SAKNR'.
*  WA_FIELDCAT-SELTEXT_M   = 'G/L Account'.
*  WA_FIELDCAT-COL_POS     = 1.
*  APPEND WA_FIELDCAT TO GT_FIELDCAT.
*  CLEAR  WA_FIELDCAT.
*
*
**   WA_FIELDCAT-FIELDNAME   = 'BUKRS'.
**  WA_FIELDCAT-SELTEXT_M   = 'G/L Account'.
**  WA_FIELDCAT-COL_POS     = 1.
**  APPEND WA_FIELDCAT TO GT_FIELDCAT.
**  CLEAR  WA_FIELDCAT.
*
*  WA_FIELDCAT-FIELDNAME   = 'LIFNR'.
*  WA_FIELDCAT-SELTEXT_M   = 'Vendor Code'.
*  WA_FIELDCAT-COL_POS     = 2.
*  APPEND WA_FIELDCAT TO GT_FIELDCAT.
*  CLEAR  WA_FIELDCAT.
*
*  WA_FIELDCAT-FIELDNAME   = 'NAME1'.
*  WA_FIELDCAT-SELTEXT_M   = 'Vendor Name'.
*  WA_FIELDCAT-COL_POS     = 3.
*  APPEND WA_FIELDCAT TO GT_FIELDCAT.
*  CLEAR  WA_FIELDCAT.
*
*  WA_FIELDCAT-FIELDNAME   = 'BUDAT'.
*  WA_FIELDCAT-SELTEXT_M   = 'Posting Date'.
*  WA_FIELDCAT-COL_POS     = 4.
*  APPEND WA_FIELDCAT TO GT_FIELDCAT.
*  CLEAR  WA_FIELDCAT.
*
*  WA_FIELDCAT-FIELDNAME   = 'BELNR'.
*  WA_FIELDCAT-SELTEXT_M   = 'Document No'.
*  WA_FIELDCAT-COL_POS     = 5.
*  APPEND WA_FIELDCAT TO GT_FIELDCAT.
*  CLEAR  WA_FIELDCAT.
*
*  WA_FIELDCAT-FIELDNAME   = 'BLART'.
*  WA_FIELDCAT-SELTEXT_M   = 'Document Type'.
*  WA_FIELDCAT-COL_POS     = 6.
*  APPEND WA_FIELDCAT TO GT_FIELDCAT.
*  CLEAR  WA_FIELDCAT.
*
*  WA_FIELDCAT-FIELDNAME   = 'QSSHB'.
*  WA_FIELDCAT-SELTEXT_M   = 'Base Amount'.
*  WA_FIELDCAT-COL_POS     = 7.
*  APPEND WA_FIELDCAT TO GT_FIELDCAT.
*  CLEAR  WA_FIELDCAT.
*
*  WA_FIELDCAT-FIELDNAME   = 'QBSHB'.
*  WA_FIELDCAT-SELTEXT_M   = 'Tax Amount'.
*  WA_FIELDCAT-COL_POS     = 8.
*  APPEND WA_FIELDCAT TO GT_FIELDCAT.
*  CLEAR  WA_FIELDCAT.
*
*  WA_FIELDCAT-FIELDNAME   = 'TEXT40'.
*  WA_FIELDCAT-SELTEXT_M   = 'Section'.
*  WA_FIELDCAT-COL_POS     = 10.
*  APPEND WA_FIELDCAT TO GT_FIELDCAT.
*  CLEAR  WA_FIELDCAT.
*
*  WA_FIELDCAT-FIELDNAME   = 'J_1IPANNO'.
*  WA_FIELDCAT-SELTEXT_M   = 'PAN No'.
*  WA_FIELDCAT-COL_POS     = 12.
*  APPEND WA_FIELDCAT TO GT_FIELDCAT.
*  CLEAR  WA_FIELDCAT.
*
*  WA_FIELDCAT-FIELDNAME   = 'QSATZ'.
*  WA_FIELDCAT-SELTEXT_M   = 'Percentage'.
*  WA_FIELDCAT-COL_POS     = 13.
*  APPEND WA_FIELDCAT TO GT_FIELDCAT.
*  CLEAR  WA_FIELDCAT.




*  WA_FIELDCAT-FIELDNAME   = 'SAKNR'.
*  WA_FIELDCAT-SELTEXT_M   = 'G/L Account'.
*  WA_FIELDCAT-COL_POS     = 1.
*  APPEND WA_FIELDCAT TO GT_FIELDCAT.
*  CLEAR  WA_FIELDCAT.


*   WA_FIELDCAT-FIELDNAME   = 'BUKRS'.
*  WA_FIELDCAT-SELTEXT_M   = 'G/L Account'.
*  WA_FIELDCAT-COL_POS     = 1.
*  APPEND WA_FIELDCAT TO GT_FIELDCAT.
*  CLEAR  WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME   = 'HKONT'.
  WA_FIELDCAT-SELTEXT_M   = 'G/L Account'.
  WA_FIELDCAT-COL_POS     = 1.
  APPEND WA_FIELDCAT TO GT_FIELDCAT.
  CLEAR  WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME   = 'LIFNR'.
  WA_FIELDCAT-SELTEXT_M   = 'Vendor Code'.
  WA_FIELDCAT-COL_POS     = 2.
  APPEND WA_FIELDCAT TO GT_FIELDCAT.
  CLEAR  WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME   = 'NAME1'.
  WA_FIELDCAT-SELTEXT_M   = 'Vendor Name'.
  WA_FIELDCAT-COL_POS     = 3.
  APPEND WA_FIELDCAT TO GT_FIELDCAT.
  CLEAR  WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME   = 'BUDAT'.
  WA_FIELDCAT-SELTEXT_M   = 'Posting Date'.
  WA_FIELDCAT-COL_POS     = 4.
  APPEND WA_FIELDCAT TO GT_FIELDCAT.
  CLEAR  WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME   = 'BELNR'.
  WA_FIELDCAT-SELTEXT_M   = 'Document No'.
  WA_FIELDCAT-COL_POS     = 5.
  APPEND WA_FIELDCAT TO GT_FIELDCAT.
  CLEAR  WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME   = 'BLART'.
  WA_FIELDCAT-SELTEXT_M   = 'Document Type'.
  WA_FIELDCAT-COL_POS     = 6.
  APPEND WA_FIELDCAT TO GT_FIELDCAT.
  CLEAR  WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME   = 'QSSHB'.
  WA_FIELDCAT-SELTEXT_M   = 'Base Amount'.
  WA_FIELDCAT-COL_POS     = 7.
  APPEND WA_FIELDCAT TO GT_FIELDCAT.
  CLEAR  WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME   = 'DMBTR'.
  WA_FIELDCAT-SELTEXT_M   = 'Tax Amount'.
  WA_FIELDCAT-COL_POS     = 8.
  APPEND WA_FIELDCAT TO GT_FIELDCAT.
  CLEAR  WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME   = 'QSCOD'.
  WA_FIELDCAT-SELTEXT_M   = 'Off. W/Tax Key'.
  WA_FIELDCAT-COL_POS     = 9.
  APPEND WA_FIELDCAT TO GT_FIELDCAT.
  CLEAR  WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME   = 'TEXT40'.
  WA_FIELDCAT-SELTEXT_M   = 'Section'.
  WA_FIELDCAT-COL_POS     = 10.
  APPEND WA_FIELDCAT TO GT_FIELDCAT.
  CLEAR  WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME   = 'J_1IPANNO'.
  WA_FIELDCAT-SELTEXT_M   = 'PAN No'.
  WA_FIELDCAT-COL_POS     = 12.
  APPEND WA_FIELDCAT TO GT_FIELDCAT.
  CLEAR  WA_FIELDCAT.

  WA_FIELDCAT-FIELDNAME   = 'QSATZ'.
  WA_FIELDCAT-SELTEXT_M   = 'Percentage'.
  WA_FIELDCAT-COL_POS     = 13.
  APPEND WA_FIELDCAT TO GT_FIELDCAT.
  CLEAR  WA_FIELDCAT.


ENDFORM.                    " ALV_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_DATA .
  LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  LAYOUT-ZEBRA = 'X'.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
   EXPORTING
*   I_INTERFACE_CHECK                 = ' '
*   I_BYPASSING_BUFFER                = ' '
*   I_BUFFER_ACTIVE                   = ' '
     I_CALLBACK_PROGRAM                =  SY-REPID
*   I_CALLBACK_PF_STATUS_SET          = ' '
*   I_CALLBACK_USER_COMMAND           = ' '
  I_CALLBACK_TOP_OF_PAGE            = 'ALV_CATALOG_HEADER'
"   I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*   I_CALLBACK_HTML_END_OF_LIST       = ' '
*   I_STRUCTURE_NAME                  =
*   I_BACKGROUND_ID                   = ' '
*   I_GRID_TITLE                      =
*   I_GRID_SETTINGS                   =
   IS_LAYOUT                         = LAYOUT
     IT_FIELDCAT                       = GT_FIELDCAT
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
      T_OUTTAB                          = GT_FINAL
   EXCEPTIONS
     PROGRAM_ERROR                     = 1
     OTHERS                            = 2
            .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.


ENDFORM.                    " DISPLAY_DATA

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
  LS_LINE-INFO = 'Withholding Tax Report' .
  APPEND LS_LINE TO LIT_HEADER.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = LIT_HEADER
      I_LOGO             = 'ZLOGO'.
*  CALL FUNCTION 'CONVERSION_EXIT_SDATE_OUTPUT'
*    EXPORTING
*      INPUT  = LV_BUDAT_MKPF
*    IMPORTING
*      OUTPUT = LV_BUDAT_MKPF.

**  CALL FUNCTION 'CONVERSION_EXIT_SDATE_OUTPUT'
**
**  EXPORTING
**    INPUT         = LV_BEDAT.

*      I_LOGO             = ' '.

ENDFORM.                    "ALV_CATALOG_HEADER
