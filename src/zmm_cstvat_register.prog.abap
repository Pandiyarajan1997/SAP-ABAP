*&---------------------------------------------------------------------*
*& Report  ZMM_CSTVAT_REGISTER
*&
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&Functional                   : Govindarajan.M                        *
*& Developer                   : Govindarajan.M                        *
*& Created On                  : 27 Aug 2014                           *
*& Title                       : Purchace Order VAT/CST Register       *
*& Report Name                 : ZMM_CSTVAT_REGISTER                   *
*& Development Id              : Kpabap                                *
*& Related Information         : Purchace Order VAT/CST Register       *
***********************************************************************


REPORT ZMM_CSTVAT_REGISTER.

TYPE-POOLS : SLIS.

*&---------------------------------------------------------------------*
*&  Structure And Internal Table Decleration
*&---------------------------------------------------------------------*

TYPES: BEGIN OF GS_RBKP,
       BELNR TYPE RBKP-BELNR,                             " Document Number of an Invoice Document
       BUKRS TYPE RBKP-BUKRS,                             " Company Code
       LIFNR TYPE RBKP-LIFNR,                             " Different Invoicing Party
       BLDAT TYPE RBKP-BLDAT,                             " Invoice Date
       SGTXT TYPE RBKP-SGTXT,                             " Item Text
       XBLNR TYPE RSEG-XBLNR,                             " Reference Document Number
       RMWWR TYPE RBKP-RMWWR,                             " Gross invoice amount in document currency
       WMWST1 TYPE RBKP-WMWST1,                           " Tax Amount in Document Currency with +/- Sign
       MWSKZ1 TYPE RBKP-MWSKZ1,                           " Tax Code
       END OF GS_RBKP.

DATA: GT_RBKP TYPE TABLE OF GS_RBKP,
      WA_RBKP TYPE GS_RBKP.

TYPES: BEGIN OF GS_LFA1,
       LIFNR TYPE LFA1-LIFNR,                             " Account Number of Vendor or Creditor
       NAME1 TYPE LFA1-NAME1,                               " Name 1
       WERKS TYPE LFA1-WERKS,                             " Plant
       REGIO TYPE LFA1-REGIO,                             " Region
       END OF GS_LFA1.

DATA: GT_LFA1 TYPE TABLE OF GS_LFA1,
      WA_LFA1 TYPE GS_LFA1.

TYPES: BEGIN OF GS_J_1IMOVEND,
       LIFNR TYPE J_1IMOVEND-LIFNR,                        "#EC CI_USAGE_OK[2877717] " Account Number of Vendor or CreditorAdded by <IT-CAR Tool> during Code Remediation
       J_1ILSTNO TYPE J_1IMOVEND-J_1ILSTNO,                "#EC CI_USAGE_OK[2877717] " Local Sales Tax NumberAdded by <IT-CAR Tool> during Code Remediation
       J_1ICSTNO TYPE J_1IMOVEND-J_1ICSTNO,                "#EC CI_USAGE_OK[2877717] " CST NOAdded by <IT-CAR Tool> during Code Remediation
       END OF GS_J_1IMOVEND.

DATA: GT_J_1IMOVEND TYPE TABLE OF GS_J_1IMOVEND,
      WA_J_1IMOVEND TYPE GS_J_1IMOVEND.

TYPES: BEGIN OF GS_RSEG,
       BELNR TYPE RSEG-BELNR,                             " Document Number of an Invoice Document
       BUZEI TYPE RSEG-BUZEI,                             " Document Item in Invoice Document
       MATNR TYPE RSEG-MATNR,                             " Material Number
       XBLNR TYPE RSEG-XBLNR,                             " Reference Document Number
       WRBTR TYPE RSEG-WRBTR,                             " Amount in Document Currency
       MENGE TYPE RSEG-MENGE,                             " Quantity
       WERKS TYPE RSEG-WERKS,                             " Plant
       SHKZG TYPE RSEG-SHKZG,                             " Debit/Credit Indicator
       PSTYP TYPE RSEG-PSTYP,                             " Item Category in Purchasing Document
       SGTXT TYPE RSEG-SGTXT,                             " Item Text
       KSCHL TYPE RSEG-KSCHL,                             " Condition Type
       END OF GS_RSEG.

DATA: GT_RSEG TYPE TABLE OF GS_RSEG,
      WA_RSEG TYPE GS_RSEG.

TYPES: BEGIN OF GS_BSEG,
       BELNR TYPE BSEG-BELNR,                             " Document Number of an Invoice Document
       BUZEI TYPE BSEG-BUZEI,                             " Number of Line Item Within Accounting Document
       BUZID TYPE BSEG-BUZID,                             " Identification of the Line Item
       SHKZG TYPE BSEG-SHKZG,                             " Debit/Credit Indicator
       MWSKZ TYPE BSEG-MWSKZ,                             " Sales Tax Code
       DMBTR TYPE BSEG-DMBTR,                             " Amount in Local Currency
       HWBAS TYPE BSEG-HWBAS,                             " Tax Base Amount in Local Currency
       TXGRP TYPE BSEG-TXGRP,                             " Group Indicator for Tax Line Items
       END OF GS_BSEG.

DATA: GT_BSEG TYPE TABLE OF GS_BSEG,
      WA_BSEG TYPE GS_BSEG.

TYPES: BEGIN OF GS_MARA,
       MATNR TYPE MARA-MATNR,
       NORMT TYPE MARA-NORMT,                             " Commodity Code
       END OF GS_MARA.

DATA: GT_MARA TYPE TABLE OF GS_MARA,
      WA_MARA TYPE GS_MARA.

TYPES: BEGIN OF GS_MAKT,
       MATNR TYPE MAKT-MATNR,
       MAKTX TYPE MAKT-MAKTX,
       END OF GS_MAKT.

DATA: GT_MAKT TYPE TABLE OF GS_MAKT,
      WA_MAKT TYPE GS_MAKT.

TYPES : BEGIN OF GS_T001W,
        WERKS TYPE T001W-WERKS,
        NAME1 TYPE T001W-NAME1,
        END OF GS_T001W.

DATA : GT_T001W TYPE TABLE OF GS_T001W,
      WA_T001W TYPE GS_T001W.


TYPES : BEGIN OF GS_BSET,
       BUKRS TYPE BSET-BUKRS,
       BELNR TYPE BSET-BELNR,
       GJAHR TYPE BSET-GJAHR,
       TXGRP(4) TYPE N, " TYPE bset-txgrp, "GR Invoice item
       SHKZG TYPE BSET-SHKZG, "Debit/Credit Indicator
       MWSKZ TYPE BSET-MWSKZ, "Tax Code
       HWBAS TYPE BSET-HWBAS, "Tax Base amount in local currency
       HWSTE TYPE BSET-HWSTE, "Tax Amount in local currency
       KTOSL TYPE BSET-KTOSL, "Transaction key
       KSCHL TYPE BSET-KSCHL, "Condition Type
       KBETR TYPE BSET-KBETR, "Tax Rate
       EBELN TYPE EBELN,
       EBELP TYPE EBELP,
       AWKEY TYPE BKPF-AWKEY,
       BUZEI TYPE BSET-BUZEI,
       HKONT TYPE BSET-HKONT,
       KNUMH TYPE BSET-KNUMH,
       LFBNR TYPE MSEG-LFBNR,
       STUNR TYPE RSEG-STUNR,
       SER_TAX TYPE BSET-HWSTE,
       E_SER_TAX TYPE BSET-HWSTE,
       S_SER_TAX TYPE BSET-HWSTE,
       END OF GS_BSET.

DATA : GT_BSET TYPE TABLE OF GS_BSET,
       WA_BSET TYPE GS_BSET.

TYPES: BEGIN OF GS_FINAL,
       BUKRS TYPE RBKP-BUKRS,                             " Company Code
       BELNR TYPE RBKP-BELNR,                             " Bill Number
       BLDAT TYPE RBKP-BLDAT,                             " Invoice Date in Document
       LIFNR TYPE LFA1-LIFNR,                             " Vendor Code
       NAME1 TYPE LFA1-NAME1,                             " Name 1
       J_1ILSTNO TYPE J_1IMOVEND-J_1ILSTNO,                "#EC CI_USAGE_OK[2877717] " Local Sales Tax NumberAdded by <IT-CAR Tool> during Code Remediation
       J_1ICSTNO TYPE J_1IMOVEND-J_1ICSTNO, "#EC CI_USAGE_OK[2877717] " Added by <IT-CAR Tool> during Code Remediation
       MAKTX TYPE MAKT-MAKTX,                             " Item Text
       WERKS TYPE RSEG-WERKS,                             " Plant
       PNAME TYPE T001W-NAME1,                            " Plant Name
       XBLNR TYPE RSEG-XBLNR,                             " Reference Document Number
       MENGE TYPE RSEG-MENGE,                             " Quantity
       WRBTR TYPE RSEG-WRBTR,                             " Amount in Document Currency
       TAXRATE TYPE P DECIMALS 2,                         " Tax Rate
       TAXAMT TYPE RBKP-WMWST1,                           " VAT Amount in Document
       CSTAMT TYPE RBKP-WMWST1,                            " CST Amount
       VATBAS     TYPE HWBAS,              "Tax Base Amount in Document Currency for VAT
       SERBAS     TYPE HWBAS,              "Tax Base Amount in Document Currency for Service Tax
       BED        TYPE KWERT,              "BED
       BEDRATE    TYPE KWERT,              "BED Rate
       ADDBED     TYPE KWERT,                          "Addl Bed                                     "added 22.02.2009
       ECS        TYPE KWERT,              "Ecess Setoff
       EBED       TYPE KWERT,              "Ecess on BED
       RMDED      TYPE KWERT,              "RM Deductable
       RMDEDRATE  TYPE KWERT,              "VAT rate (RM Deductible)
       RMNON      TYPE KWERT,              "RM Non-Deductable
       CGDED      TYPE KWERT,              "CG Deductable
       CGDEDRATE  TYPE KWERT,              "ADD.VAT rate (CG Deductible)                          "line add 03.03.2009
       CGNON      TYPE KWERT,              "CG Non-Deductable
       CST        TYPE KWERT,              "CST
       CSTRATE    TYPE KWERT,              "CST Rate
       SERTAX     TYPE KWERT,              "Service Tax
       ESERTAX    TYPE KWERT,              "ECess on Service Tax
       SHCESS     TYPE KWERT,              "SHCess
       SHRATE     TYPE KWERT,              "SHCess Rate
       ACD        TYPE WRBTR,              "Additional Customs Duty
       BASCUS     TYPE WRBTR,              "Basic Customs
       CVD        TYPE WRBTR,              "CVD
       ECVD       TYPE WRBTR,              "ECess on CVD
       HECVD      TYPE WRBTR,              "Hecess on CVD
       ECUSTOM    TYPE WRBTR,              "Customs Ecess
       HECUSTOM   TYPE WRBTR,              "Customs HEcess
       WCT        TYPE DMBTR,              "W.C.T
       TDS        TYPE DMBTR,              "T.D.S
       ESIC       TYPE DMBTR,              "E.S.I.C
       GROSS      TYPE KWERT,              "Gross Total
       BED_N      TYPE KWERT,               " Bed Non Deductable
       CES_N      TYPE KWERT,               " Cesss Non Deductable
       HCESS_N    TYPE KWERT,               " H cesss Non Deductable
       AED_N      TYPE KWERT,               " AED Non Deductable
       HCESS      TYPE KWERT,              "SHCess on BED
       INVAMT TYPE RSEG-WRBTR,                            " Invoice Amount

       END OF GS_FINAL.

DATA: GT_FINAL TYPE TABLE OF GS_FINAL,
      WA_FINAL TYPE GS_FINAL.

DATA: OR_BUKRS TYPE RBKP-BUKRS,
      OR_BELNR TYPE RBKP-BELNR, "OR_BELNR  OR_LIFNR OR_BUDAT
      OR_LIFNR TYPE RBKP-LIFNR,
      OR_BUDAT TYPE RBKP-BUDAT,
      OR_WERKS TYPE T001W-WERKS,
      OR_MWSKZ TYPE T007A-MWSKZ.


DATA: L_WERKS TYPE T001W-WERKS,               "Plant
      L_LIFNR TYPE RBKP-LIFNR,               "Vendor Code
      L_BUDAT TYPE RBKP-BUDAT.               "Posting Date



DATA: LV_TRATE TYPE I.  " Tax Rate

*&---------------------------------------------------------------------*
*&  ALV Structure & Internal Table
*&---------------------------------------------------------------------*

DATA: GT_FCAT TYPE SLIS_T_FIELDCAT_ALV,
      WA_FCAT TYPE SLIS_FIELDCAT_ALV,
      V_LAYOUT TYPE SLIS_LAYOUT_ALV,
      GT_EVENTS TYPE SLIS_T_EVENT,
      WA_EVENTS TYPE SLIS_ALV_EVENT.

DATA: LS_VARIANT TYPE DISVARIANT.
LS_VARIANT-REPORT = SY-REPID.

DATA : GT_SORT TYPE SLIS_T_SORTINFO_ALV,
         WA_SORT TYPE SLIS_SORTINFO_ALV.

*&---------------------------------------------------------------------*
*&  Selection Screen Fields
*&---------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS :  SO_BUKRS FOR OR_BUKRS,
                  SO_WERKS FOR OR_WERKS,
                  SO_LIFNR FOR OR_LIFNR,
                  SO_BELNR FOR OR_BELNR,
                  SO_BUDAT FOR OR_BUDAT.
SELECTION-SCREEN: END OF BLOCK B1.

SELECTION-SCREEN: BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-101.
PARAMETERS: RD_VAT RADIOBUTTON GROUP G1 DEFAULT 'X',
            RD_CST RADIOBUTTON GROUP G1.
SELECTION-SCREEN: END OF BLOCK B2.


*&---------------------------------------------------------------------*
*&  Main logic Of Program
*&---------------------------------------------------------------------*
SELECT
    BELNR
    BUZEI
    MATNR
    XBLNR
    WRBTR
    MENGE
    WERKS
    SHKZG
    PSTYP
    SGTXT
    KSCHL

  FROM RSEG INTO TABLE GT_RSEG "FOR ALL ENTRIES IN GT_RBKP
    WHERE           WERKS IN SO_WERKS.

SELECT
  BELNR
  BUKRS
  LIFNR
  BLDAT
  SGTXT
  XBLNR
  RMWWR
  WMWST1
  MWSKZ1 FROM RBKP INTO TABLE GT_RBKP FOR ALL ENTRIES IN  GT_RSEG
  WHERE BUDAT IN SO_BUDAT AND BUKRS IN SO_BUKRS AND   BELNR = GT_RSEG-BELNR AND BELNR IN SO_BELNR AND
        LIFNR IN SO_LIFNR AND   RBSTAT NE 'E' AND
        BLART = 'RE'  .

IF GT_RBKP[] IS NOT INITIAL.



  IF GT_RBKP[] IS NOT INITIAL.
    SELECT BUKRS
         BELNR
         GJAHR
         TXGRP
         SHKZG
         MWSKZ
         HWBAS
         HWSTE
         KTOSL
         KSCHL
         KBETR

             FROM BSET INTO TABLE GT_BSET FOR ALL ENTRIES IN GT_RBKP WHERE BELNR = GT_RBKP-BELNR AND SHKZG = 'S'. "#EC CI_NOORDER  " Added by <IT-CAR Tool> during Code Remediation

  ENDIF.
  SELECT LIFNR
         NAME1
         WERKS
         REGIO FROM LFA1 INTO TABLE GT_LFA1 FOR ALL ENTRIES IN GT_RBKP
         WHERE LIFNR = GT_RBKP-LIFNR.

  SELECT WERKS
    NAME1 FROM T001W INTO TABLE GT_T001W FOR ALL ENTRIES IN GT_RSEG
     WHERE WERKS = GT_RSEG-WERKS.

  IF GT_LFA1 IS NOT INITIAL.
    SELECT LIFNR
           J_1ILSTNO
           J_1ICSTNO
       FROM LFA1 INTO TABLE GT_J_1IMOVEND FOR ALL ENTRIES IN GT_LFA1
           WHERE LIFNR = GT_LFA1-LIFNR.
  ENDIF.


ENDIF.



LOOP AT GT_RBKP INTO WA_RBKP.


  WA_FINAL-BELNR = WA_RBKP-BELNR.
  WA_FINAL-XBLNR = WA_RBKP-XBLNR.
  WA_FINAL-BLDAT = WA_RBKP-BLDAT.
  WA_FINAL-MENGE = WA_RSEG-MENGE.
  WA_FINAL-BUKRS = WA_RBKP-BUKRS.

  READ TABLE GT_LFA1 INTO WA_LFA1 WITH KEY LIFNR = WA_RBKP-LIFNR.
  IF SY-SUBRC = 0.
    WA_FINAL-LIFNR = WA_LFA1-LIFNR.
    WA_FINAL-NAME1 = WA_LFA1-NAME1.
  ENDIF.

  READ TABLE GT_J_1IMOVEND INTO WA_J_1IMOVEND WITH KEY LIFNR = WA_RBKP-LIFNR.
  IF SY-SUBRC = 0.
    WA_FINAL-J_1ILSTNO = WA_J_1IMOVEND-J_1ILSTNO.
    WA_FINAL-J_1ICSTNO = WA_J_1IMOVEND-J_1ICSTNO.
  ENDIF.

*  LV_TRATE = ( WA_RBKP-WMWST1 * 100 ) / ( WA_RBKP-RMWWR - WA_RBKP-WMWST1 ). " Tax Rate Calculation

  LOOP AT GT_RSEG INTO WA_RSEG WHERE BELNR = WA_RBKP-BELNR.
    WA_FINAL-WERKS = WA_RSEG-WERKS.
    WA_FINAL-WRBTR = WA_FINAL-WRBTR  + WA_RSEG-WRBTR.
    READ TABLE GT_T001W INTO WA_T001W WITH KEY WERKS = WA_RSEG-WERKS.
    WA_FINAL-PNAME = WA_T001W-NAME1.
  ENDLOOP.


  LOOP AT  GT_BSET INTO WA_BSET WHERE BELNR = WA_RBKP-BELNR.


    IF WA_BSET-KSCHL = 'JMOP'.
      IF SY-SUBRC = 0.
*          wa_final-exbas     = wa_final-exbas + wa_bset-hwbas.
        WA_FINAL-BED     = WA_FINAL-BED + WA_BSET-HWSTE.
*        WA_FINAL-BEDRATE = WA_BSET-KBETR / 10."Commented by SPLABAP during code remediation
        WA_FINAL-BEDRATE = CONV KWERT( WA_BSET-KBETR / 10 )."Added by SPLABAP during code remediation
      ENDIF.
    ENDIF.

    IF WA_BSET-KSCHL = 'JAOP'.
      IF SY-SUBRC = 0.                                           "line chg 22.02.2009 added cond JAOP
        WA_FINAL-ADDBED = WA_FINAL-ADDBED + WA_BSET-HWSTE.
      ENDIF.
    ENDIF.

    IF WA_BSET-KSCHL = 'JEC1'.
      IF SY-SUBRC = 0.
        WA_FINAL-ECS = WA_FINAL-ECS + WA_BSET-HWSTE.
      ENDIF.
    ENDIF.

    IF WA_BSET-KSCHL = 'JEC2'.
      IF SY-SUBRC = 0.
        WA_FINAL-EBED = WA_FINAL-EBED + WA_BSET-HWSTE.
      ENDIF.
    ENDIF.

    IF RD_VAT = 'X'.
      IF WA_BSET-KSCHL = 'JVRD'.
*        IF sy-subrc = 0.
        WA_FINAL-RMDED     = WA_FINAL-RMDED + WA_BSET-HWSTE.
*        WA_FINAL-RMDEDRATE = WA_BSET-KBETR / 10."Commented by SPLABAP during code remediation
        WA_FINAL-RMDEDRATE = CONV KWERT( WA_BSET-KBETR / 10 )."Added by SPLABAP during code remediation
*        ENDIF.

        IF NOT WA_FINAL-RMDEDRATE = 0.
          WA_FINAL-VATBAS    = WA_BSET-HWSTE + WA_FINAL-WRBTR.
*   wa_final-vatbas    = wa_bset-hwste + wa_final-wrbtr.

        ENDIF.
      ENDIF.

      IF WA_BSET-KSCHL = 'JVRN'.
        IF SY-SUBRC = 0.
          WA_FINAL-RMNON = WA_FINAL-RMNON + WA_BSET-HWSTE.
        ENDIF.
      ENDIF.

      IF WA_BSET-KSCHL = 'JVCD'.
        IF SY-SUBRC = 0.                                        "line chg 05.02.2009 added cond JVCD
          WA_FINAL-CGDED = WA_FINAL-CGDED + WA_BSET-HWSTE.
*          WA_FINAL-CGDEDRATE = WA_BSET-KBETR / 10."Commented by SPLABAP during code remediation
          WA_FINAL-CGDEDRATE = CONV KWERT( WA_BSET-KBETR / 10 )."Added by SPLABAP during code remediation
        ENDIF.
      ENDIF.

      IF WA_BSET-KSCHL = 'ZADD'.
        IF SY-SUBRC = 0.                                        "line chg 05.02.2009 added cond JVCD
          WA_FINAL-CGDED = WA_FINAL-CGDED + WA_BSET-HWSTE.
*          WA_FINAL-CGDEDRATE = WA_BSET-KBETR / 10."Commented by SPLABAP during code remediation
          WA_FINAL-CGDEDRATE = CONV KWERT( WA_BSET-KBETR / 10 )."Added by SPLABAP during code remediation
        ENDIF.
      ENDIF.

      IF WA_BSET-KSCHL = 'ZADN'.
        IF SY-SUBRC = 0.                                      "line chg 05.02.2009 added cond JVCD
          WA_FINAL-CGNON = WA_FINAL-CGNON + WA_BSET-HWSTE.
        ENDIF.
      ENDIF.

      IF WA_BSET-KSCHL = 'JVCN'.
        IF SY-SUBRC = 0.                                        "line chg 05.02.2009 added cond JVCD
          WA_FINAL-CGNON = WA_FINAL-CGNON + WA_BSET-HWSTE.
        ENDIF.
      ENDIF.
    ELSEIF RD_CST = 'X'.
      IF WA_BSET-KSCHL = 'JVCS'. "  Modified By Govind On 16/07/2014
*      IF wa_bset-kschl = 'JIPC'."                             'JVCS' .
        IF SY-SUBRC = 0.
          WA_FINAL-CST     = WA_FINAL-CST + WA_BSET-HWSTE.
*          WA_FINAL-CSTRATE = WA_BSET-KBETR / 10."Commented by SPLABAP during code remediation
          WA_FINAL-CSTRATE = CONV KWERT( WA_BSET-KBETR / 10 )."Added by SPLABAP during code remediation
        ENDIF.

        IF NOT WA_FINAL-CSTRATE = 0.
          WA_FINAL-VATBAS    = WA_BSET-HWSTE + WA_FINAL-WRBTR.
        ENDIF.
      ENDIF.
    ENDIF.
*Added by Rameshwar
*      IF wa_bset-kschl = 'JSTX'.
*        IF sy-subrc = 0.
*          wa_final-sertax = wa_final-sertax + wa_bset-hwste.
*        ENDIF.

    IF WA_BSET-KSCHL = 'JSVD'.
      IF SY-SUBRC = 0.
        WA_FINAL-SERTAX = WA_FINAL-SERTAX + WA_BSET-HWSTE.
      ENDIF.
      IF NOT WA_FINAL-SERTAX = 0.
        WA_FINAL-SERBAS    = WA_BSET-HWBAS.
      ENDIF.
    ENDIF.

*      IF wa_bset-kschl = 'JEC5'.
*        IF sy-subrc = 0.
*          wa_final-esertax = wa_final-esertax + wa_bset-hwste.
*        ENDIF.
*      ENDIF.
    IF WA_BSET-KSCHL = 'JEC3'.
      IF SY-SUBRC = 0.
        WA_FINAL-ESERTAX = WA_FINAL-ESERTAX + WA_BSET-HWSTE.
      ENDIF.
    ENDIF.

    IF WA_BSET-KSCHL = 'JSEP'.
      IF SY-SUBRC = 0.
        WA_FINAL-HCESS = WA_FINAL-HCESS + WA_BSET-HWSTE.
      ENDIF.
    ENDIF.

*      IF wa_bset-kschl = 'JEC6'.
*        IF sy-subrc = 0.
*          wa_final-shcess = wa_final-shcess + wa_bset-hwste.
*          wa_final-shrate = wa_bset-kbetr / 10.
*        ENDIF.
*      ENDIF.
    IF WA_BSET-KSCHL = 'JSE1'.
      IF SY-SUBRC = 0.
        WA_FINAL-SHCESS = WA_FINAL-SHCESS + WA_BSET-HWSTE.
*        WA_FINAL-SHRATE = WA_BSET-KBETR / 10."Commented by SPLABAP during code remediation
        WA_FINAL-SHRATE = CONV KWERT( WA_BSET-KBETR / 10 )."Added by SPLABAP during code remediation
      ENDIF.
    ENDIF.
*End of changes
    IF WA_BSET-KSCHL = 'JMIP'.
      IF SY-SUBRC = 0.
        WA_FINAL-BED_N = WA_FINAL-BED_N + WA_BSET-HWSTE. " Bed Non Deductable JMIP
      ENDIF.
    ENDIF.

    IF WA_BSET-KSCHL = 'JEC2'.
      IF SY-SUBRC = 0.
        WA_FINAL-CES_N = WA_FINAL-CES_N + WA_BSET-HWSTE. " Cesss Non Deductable
      ENDIF.
    ENDIF.

    IF WA_BSET-KSCHL = 'JSEI'.
      IF SY-SUBRC = 0.
        WA_FINAL-HCESS_N = WA_FINAL-HCESS_N + WA_BSET-HWSTE. " H cesss Non Deductable
      ENDIF.
    ENDIF.

    IF WA_BSET-KSCHL = 'JAIP'.
      IF SY-SUBRC = 0.
        WA_FINAL-AED_N = WA_FINAL-AED_N + WA_BSET-HWSTE. " H cesss Non Deductable
      ENDIF.
    ENDIF.

  ENDLOOP.

  WA_FINAL-TAXAMT = WA_FINAL-TAXAMT + WA_FINAL-RMDED.
  WA_FINAL-CSTAMT = WA_FINAL-CSTAMT + WA_FINAL-CST.


  APPEND WA_FINAL TO GT_FINAL.
  CLEAR: WA_FINAL, WA_BSEG, WA_MARA.

  CLEAR: WA_J_1IMOVEND,WA_LFA1,WA_RBKP,LV_TRATE.

ENDLOOP.

LOOP AT GT_FINAL INTO WA_FINAL.

  IF RD_VAT = 'X'.
    WA_FINAL-INVAMT = WA_FINAL-WRBTR + WA_FINAL-TAXAMT .
  ELSEIF RD_CST = 'X'    .
    WA_FINAL-INVAMT = WA_FINAL-WRBTR + WA_FINAL-CSTAMT.
  ENDIF.

  SHIFT WA_FINAL-LIFNR LEFT DELETING LEADING '0'.

  MODIFY GT_FINAL FROM WA_FINAL TRANSPORTING INVAMT LIFNR
  .

  CLEAR WA_FINAL.

ENDLOOP.



*&---------------------------------------------------------------------*
*&  ALV Layout
*&---------------------------------------------------------------------*
PERFORM ALV_LAYOUT USING 1 'Company Code' 'BUKRS' 'GT_FINAL' '' ''.
PERFORM ALV_LAYOUT USING 3 'Plant Code' 'WERKS' 'GT_FINAL' '' ''.
PERFORM ALV_LAYOUT USING 5 'Plant Name' 'PNAME' 'GT_FINAL' '' ''.

PERFORM ALV_LAYOUT USING 7 'Invoice No' 'BELNR' 'GT_FINAL' '' ''.
PERFORM ALV_LAYOUT USING 9 'Invoice Date' 'BLDAT' 'GT_FINAL' '' ''.

PERFORM ALV_LAYOUT USING 11 'Vendor Code' 'LIFNR' 'GT_FINAL' '' ''.
PERFORM ALV_LAYOUT USING 12 'Vendor Name' 'NAME1' 'GT_FINAL' '' '' .
PERFORM ALV_LAYOUT USING 16 'Purchase Value' 'WRBTR' 'GT_FINAL' 'X' 'X'.
IF RD_VAT = 'X'.
  PERFORM ALV_LAYOUT USING 14 'Vendor  TIN No' 'J_1ILSTNO' 'GT_FINAL' '' ''.
  PERFORM ALV_LAYOUT USING 19 'Tax Code' 'RMDEDRATE' 'GT_FINAL' '' 'X'.
  PERFORM ALV_LAYOUT USING 21 'VAT Amount' 'TAXAMT' 'GT_FINAL' 'X' 'X'.
  PERFORM ALV_LAYOUT USING 23 'Total Amt' 'INVAMT' 'GT_FINAL' 'X' ''.
ELSEIF RD_CST = 'X'.
  PERFORM ALV_LAYOUT USING 15 'Vendor  CST No' 'J_1ICSTNO' 'GT_FINAL' '' ''.
  PERFORM ALV_LAYOUT USING 27 'Tax Code' 'CSTRATE' 'GT_FINAL' 'X' 'X'.
  PERFORM ALV_LAYOUT USING 29 'CST Amount' 'CSTAMT' 'GT_FINAL' 'X' 'X'.
  PERFORM ALV_LAYOUT USING 31 'Total Amt' 'INVAMT' 'GT_FINAL' 'X' ''.

ENDIF.

  WA_SORT-FIELDNAME = 'NAME1'.
  WA_SORT-TABNAME = 'GT_FINAL'.
  WA_SORT-UP = 'X'.
  WA_SORT-SUBTOT = 'X'.
  APPEND WA_SORT TO GT_SORT.
  CLEAR WA_SORT.




*&---------------------------------------------------------------------*
*&   ALV Grid Display
*&---------------------------------------------------------------------*

PERFORM ALV_GRID_DISPLAY.


*&---------------------------------------------------------------------*
*&      Form  ALV_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P1  text
*      -->P2  text
*      -->P3  text
*      -->P4  text
*      -->P5  text
*----------------------------------------------------------------------*
FORM ALV_LAYOUT  USING    P1 P2 P3 P4 P5 P6.
  CLEAR WA_FCAT.
  WA_FCAT-COL_POS = P1.
  WA_FCAT-SELTEXT_L = P2.
  WA_FCAT-FIELDNAME = P3.
  WA_FCAT-TABNAME = P4.
  WA_FCAT-DO_SUM = P5.
  WA_FCAT-NO_ZERO = P6.
  APPEND WA_FCAT TO GT_FCAT.

ENDFORM.                    " ALV_LAYOUT


*&---------------------------------------------------------------------*
*&      Form  ALV_GRID_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ALV_GRID_DISPLAY .
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
     EXPORTING
*   I_INTERFACE_CHECK                 = ' '
*   I_BYPASSING_BUFFER                = ' '
*   I_BUFFER_ACTIVE                   = ' '
   I_CALLBACK_PROGRAM                 = SY-REPID
*   I_CALLBACK_PF_STATUS_SET          = ' '
*   I_CALLBACK_USER_COMMAND           = ' '
*   I_CALLBACK_TOP_OF_PAGE            = ' '
    I_CALLBACK_TOP_OF_PAGE            = 'ALV_CATALOG_HEADER'
*   I_CALLBACK_HTML_END_OF_LIST       = ' '
*   I_STRUCTURE_NAME                  =
*   I_BACKGROUND_ID                   = ' '
*   I_GRID_TITLE                      =
*   I_GRID_SETTINGS                   =
*   IS_LAYOUT                         =
       IT_FIELDCAT                      = GT_FCAT[]
*   IT_EXCLUDING                      =
*   IT_SPECIAL_GROUPS                 =
    IT_SORT                           = GT_SORT[]
*   IT_FILTER                         =
*   IS_SEL_HIDE                       =
   I_DEFAULT                         = 'X'
   I_SAVE                            = 'A'
   IS_VARIANT                        = LS_VARIANT
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
        T_OUTTAB                         = GT_FINAL[]
* EXCEPTIONS
*   PROGRAM_ERROR                     = 1
*   OTHERS                            = 2
              .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " ALV_GRID_DISPLAY




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

  DATA : L_WERKS(100) TYPE C,
         L_LIFNR(100) TYPE C,
         L_BUDAT(100) TYPE C.

  CLEAR : L_WERKS,
          L_LIFNR,
          L_BUDAT.

  IF SO_WERKS-HIGH IS NOT INITIAL.
    CONCATENATE 'Plant :' SO_WERKS-LOW 'To' SO_WERKS-HIGH INTO L_WERKS SEPARATED BY SPACE.
  ELSE.
    CONCATENATE 'Plant :' SO_WERKS-LOW INTO L_WERKS SEPARATED BY SPACE.
  ENDIF.


  IF SO_LIFNR-HIGH IS NOT INITIAL.
    CONCATENATE 'Vendor Code :' SO_LIFNR-LOW 'To' SO_LIFNR-HIGH INTO L_LIFNR SEPARATED BY SPACE.
  ELSE.
    CONCATENATE 'Vendor Code :' SO_LIFNR-LOW INTO L_LIFNR SEPARATED BY SPACE.
  ENDIF.


  IF SO_BUDAT-HIGH IS NOT INITIAL.
    CONCATENATE 'Document Date :' SO_BUDAT-LOW 'To' SO_BUDAT-HIGH INTO L_BUDAT SEPARATED BY SPACE.
  ELSE.
    CONCATENATE 'Document Date :' SO_BUDAT-LOW INTO L_BUDAT SEPARATED BY SPACE.
  ENDIF.


  CLEAR LS_LINE.
  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY = ' '.
  LS_LINE-INFO = L_WERKS.
  APPEND LS_LINE TO LIT_HEADER.

  CLEAR LS_LINE.
  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY = ' '.
  LS_LINE-INFO = L_LIFNR.
  APPEND LS_LINE TO LIT_HEADER.


  CLEAR LS_LINE.
  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY = ' '.
  LS_LINE-INFO = L_BUDAT.
  APPEND LS_LINE TO LIT_HEADER.

  CLEAR LS_LINE.
  LS_LINE-TYP  = 'H'.
  LS_LINE-KEY = ' '.
  LS_LINE-INFO = 'Vendor VAT/CST Purchase Details Reports' .
  APPEND LS_LINE TO LIT_HEADER.

*  CLEAR LS_LINE.
*  LS_LINE-TYP  = 'S'.
*  LS_LINE-KEY = ' '.
*  LS_LINE-INFO = LV_BEDAT.
*  APPEND LS_LINE TO LIT_HEADER.

*  CLEAR LS_LINE.

*CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
*  EXPORTING
**    IT_LIST_COMMENTARY       = LIT_HEADER
**   I_LOGO                   = 'ZLOGO' .
*   I_END_OF_LIST_GRID       =
*   I_ALV_FORM               =
*          .
*
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = LIT_HEADER
      I_LOGO             = 'ZLOGO'.

*  CALL FUNCTION 'CONVERSION_EXIT_SDATE_OUTPUT'
*    EXPORTING
*      INPUT  = LV_BEDAT
*    IMPORTING
*      OUTPUT = LV_BEDAT.

**  CALL FUNCTION 'CONVERSION_EXIT_SDATE_OUTPUT'
**
**  EXPORTING
**    INPUT         = LV_BEDAT.

*      I_LOGO             = ' '.


ENDFORM.                    "ALV_CATALOG_HEADER
