*&---------------------------------------------------------------------*
*& Report  ZMM_VATCST_REGISTER
*&
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&Functional                   : Govindarajan.M                        *
*& Developer                   : Govindarajan.M                        *
*& Created On                  : 27 Aug 2014                           *
*& Title                       : Purchace Order VAT/CST Register       *
*& Report Name                 : ZMM_VATCST_REGISTER                   *
*& Development Id              : Kpabap                               *
*& Solman call No              :                                       *
*& Transport Request           :                                       *
*& Related Information         : Purchace Order VAT/CST Register       *


REPORT ZMM_VATCST_REGISTER.


** TABLES DECLARATION
TABLES : BKPF,
         RBKP,
         BSEG,
         BSET,
         MAKT,
         MSEG,
         RSEG,
         LFA1,
         MKPF,
         J_1IEXCDTL,

         T025T,
         T005U.      " Taxes: Region Key: Texts

TYPE-POOLS : SLIS.

DATA : BEGIN OF IT_BKPF OCCURS 0,
       BUKRS   LIKE BKPF-BUKRS,
       BELNR   LIKE BKPF-BELNR,
       GJAHR   LIKE BKPF-GJAHR,
       BLART   LIKE BKPF-BLART,
       BLDAT   LIKE BKPF-BLDAT,
       BUDAT   LIKE BKPF-BUDAT,
       XBLNR   LIKE BKPF-XBLNR,
       WAERS   LIKE BKPF-WAERS,
       KURSF   LIKE BKPF-KURSF,
       AWKEY   LIKE BKPF-AWKEY,
       G_BELNR LIKE RSEG-BELNR, "GR Inv No
       G_GJAHR LIKE RSEG-GJAHR, "GR Inv Year
       TCODE   LIKE BKPF-TCODE,
       END OF IT_BKPF.

DATA  IT_BKPF1 LIKE IT_BKPF OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF IT_RSEG OCCURS 0,
       BELNR1  LIKE BKPF-BELNR,
       BELNR  LIKE RSEG-BELNR,
       GJAHR  LIKE RSEG-GJAHR,
       BUZEI  LIKE RSEG-BUZEI,
       EBELN  LIKE RSEG-EBELN,
       EBELP(6) TYPE N,  "LIKE rseg-ebelp,
       MATNR  LIKE RSEG-MATNR,
       BUKRS  LIKE RSEG-BUKRS,
       SHKZG  LIKE RSEG-SHKZG,
       WRBTR  LIKE RSEG-WRBTR,
       MWSKZ  LIKE RSEG-MWSKZ,
       BKLAS  LIKE RSEG-BKLAS,
       BNKAN  LIKE RSEG-BNKAN,
       KSCHL  LIKE RSEG-KSCHL, "Condition type in RSEG
       LFBNR  LIKE RSEG-LFBNR, "Reference Document
       LFPOS  LIKE RSEG-LFPOS, "Reference Documenet item
       WERKS  LIKE RSEG-WERKS,
       LIFNR  LIKE RBKP-LIFNR,
       BLART  LIKE RBKP-BLART, " Add By Vivek
       NAME1  LIKE LFA1-NAME1,
       REGIO  LIKE LFA1-REGIO,
       ORT01  LIKE LFA1-ORT01,
       LAND1  LIKE LFA1-LAND1,"lifnr name1 regio ort01 land1
       MENGE  LIKE RSEG-MENGE,
       MEINS  LIKE RSEG-MEINS,
*       land1  LIKE lfa1-land1,          " Vendor Country code
       BUKRS1 LIKE BKPF-BUKRS,
       BUDAT  LIKE BKPF-BUDAT,
       GJAHR1 LIKE BKPF-GJAHR,
       GSBER LIKE BSEG-GSBER,
       COUNT1(4) TYPE N,
       MTART LIKE MARA-MTART,
       MATKL LIKE MARA-MATKL,
*       blart like bkpf-blart,
*** Add by Vivek
       TXGRP LIKE BSET-TXGRP,
       AWKEY LIKE BKPF-AWKEY,
       ADD   LIKE RSEG-BELNR,
       STUNR LIKE RSEG-STUNR,
       EXKBE LIKE RSEG-EXKBE,
       BSART LIKE EKKO-BSART,
       END OF IT_RSEG.

DATA : BEGIN OF BASE_UNIT OCCURS 0,
       BELNR  LIKE RSEG-BELNR,
       EBELN  LIKE RSEG-EBELN,
       EBELP  LIKE RSEG-EBELP,
       MEINS  LIKE RSEG-MEINS,
       BSTME  LIKE RSEG-BSTME,
       BELNR2 LIKE RSEG-BELNR,
       END OF BASE_UNIT.

DATA : BEGIN OF IT_RBKP OCCURS 0,
         BELNR  LIKE RBKP-BELNR,
         GJAHR  LIKE RBKP-GJAHR,
         LIFNR  LIKE RBKP-LIFNR,
         BLART  LIKE RBKP-BLART, " Add By Vivek
       END OF IT_RBKP.

DATA : BEGIN OF IT_LFA1 OCCURS 0,
          LIFNR  LIKE LFA1-LIFNR,
          NAME1  LIKE LFA1-NAME1,
          REGIO  LIKE LFA1-REGIO,
          ORT01  LIKE LFA1-ORT01,
          LAND1  LIKE LFA1-LAND1,
       END OF IT_LFA1.

DATA : BEGIN OF IT_J_1IEXCDTL OCCURS 0,
        DOCNO  TYPE J_1IDOCNO,
        EXBAS  TYPE J_1IEXCBAS,
        RDOC1  TYPE J_1IRDOC1,
*        ritem1 TYPE j_1irem1,
        RITEM1 TYPE J_1IRITEM1,
        RDOC2  TYPE J_1IRDOC2,
       END OF IT_J_1IEXCDTL.

DATA : BEGIN OF IT_MKPF OCCURS 0,
       MBLNR TYPE MBLNR,
       BUDAT TYPE BUDAT,
       MJAHR TYPE MJAHR,
       AWKEY LIKE BKPF-AWKEY,
       END OF IT_MKPF.

DATA : L_LFPOS(4) TYPE C.

DATA : BEGIN OF IT_BSET OCCURS 0,
       COUNT TYPE SY-TABIX,
       BUKRS LIKE BSET-BUKRS,
       BELNR LIKE BSET-BELNR,
       GJAHR LIKE BSET-GJAHR,
       TXGRP(4) TYPE N, " like bset-txgrp, "GR Invoice item
       SHKZG LIKE BSET-SHKZG, "Debit/Credit Indicator
       MWSKZ LIKE BSET-MWSKZ, "Tax Code
       HWBAS LIKE BSET-HWBAS, "Tax Base amount in local currency
       HWSTE LIKE BSET-HWSTE, "Tax Amount in local currency
       KTOSL LIKE BSET-KTOSL, "Transaction key
       KSCHL LIKE BSET-KSCHL, "Condition Type
       KBETR LIKE BSET-KBETR, "Tax Rate
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
       END OF IT_BSET.

DATA : BEGIN OF IT_MAKT OCCURS 0,
       MATNR LIKE MAKT-MATNR,
       MAKTX LIKE MAKT-MAKTX,
       END OF IT_MAKT.

DATA : BEGIN OF IT_J_1IMOVEND OCCURS 0,
       LIFNR     TYPE LIFNR,                "Vendor
       J_1ILSTNO TYPE J_1ILSTNO,            "LST No (TIN No)
       J_1IPANNO TYPE J_1IPANNO,            "PAN No
       J_1ISERN  TYPE J_1ISERN,             "Service Tax Registration Number
       J_1ICSTNO TYPE J_1ICSTNO,            "CST No
       J_1IEXCD  TYPE J_1IEXCD,
       END OF IT_J_1IMOVEND.

DATA : BEGIN OF IT_BSEG OCCURS 0,

       BUKRS LIKE BSEG-BUKRS,
       BELNR LIKE BSEG-BELNR,
       GJAHR LIKE BSEG-GJAHR,
       BUZID TYPE BUZID,
       BUZEI TYPE BUZEI,
       SHKZG TYPE SHKZG,
       DMBTR TYPE DMBTR,
       WRBTR TYPE WRBTR,
       HKONT LIKE BSEG-HKONT,
       HWBAS LIKE BSEG-HWBAS,
       EBELN LIKE BSEG-EBELN,
       WERKS TYPE BSEG-WERKS,
       EBELP LIKE BSEG-EBELP,
       GSBER LIKE BSEG-GSBER,
       MATNR TYPE BSEG-MATNR,
       COUNT TYPE SY-TABIX,
       BELNR1 LIKE BSEG-BELNR,
       GJAHR1 LIKE BSEG-GJAHR,
       MWSKZ TYPE BSEG-MWSKZ,
       TXGRP TYPE BSET-TXGRP,
       AWKEY TYPE BKPF-AWKEY,
       KOART LIKE BSEG-KOART,
       KTOSL LIKE BSEG-KTOSL,
       END OF IT_BSEG.

DATA : IT_BSEG1 LIKE IT_BSEG OCCURS 0 WITH HEADER LINE.
DATA : IT_BSEG2 LIKE IT_BSEG OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF IT_EKBE OCCURS 0,
       BUZEI LIKE EKBE-BUZEI,
       LFBNR LIKE EKBE-LFBNR,
       LFPOS LIKE RSEG-LFPOS,
       END OF IT_EKBE.

DATA : BEGIN OF IT_T025T OCCURS 0,
       BKLAS TYPE BKLAS,
       BKBEZ TYPE BKBEZ,
       END OF IT_T025T.

DATA: BEGIN OF IT_EKPO OCCURS 0,
        EBELN TYPE EKPO-EBELN,
        EBELP TYPE EKPO-EBELP,
        MATNR TYPE EKPO-MATNR,
        WERKS TYPE EKPO-WERKS,
        MENGE TYPE EKPO-MENGE,
        NETPR TYPE EKPO-NETPR,
        NETWR TYPE EKPO-NETWR,
        MWSKZ TYPE EKPO-MWSKZ,
      END OF IT_EKPO.


* Region Discription
DATA  : BEGIN OF IT_T005U OCCURS 0.
        INCLUDE STRUCTURE T005U.
DATA  : END OF IT_T005U.

DATA : BEGIN OF IT_FINAL OCCURS 0,
       BLDAT      LIKE BKPF-BLDAT,         "Doc Date
       POSTDAT    LIKE BKPF-BUDAT,         "Posting Date
       BUKRS      LIKE BKPF-BUKRS,         "Company Code
       BELNR      LIKE BKPF-BELNR,         "Document No
       BLART      LIKE BKPF-BLART,         "Document Type
       DOC_TYPE(20)   TYPE C,
       WAERS      LIKE BKPF-WAERS,         "Currency
       KURSF      LIKE BKPF-KURSF,         "Exchange Rate
       GJAHR      LIKE BKPF-GJAHR,         "Fiscal Year
       LIFNR      LIKE LFA1-LIFNR,         "Vendor
       NAME1      LIKE LFA1-NAME1,         "Vendor Name
       XBLNR      LIKE BKPF-XBLNR,         "Vendor Invoice No
       G_BELNR    LIKE RSEG-BELNR,         "MIRO Invoice No
       BUZEI      LIKE RSEG-BUZEI,         "MIRO Invoice item
       BUZEI1     LIKE RSEG-BUZEI,
       LFBNR      LIKE RSEG-LFBNR,         "Grr No
       BUDAT      TYPE BUDAT,              "GRR Date
       BKLAS      LIKE RSEG-BKLAS,         "Valuation Class
       BKBEZ      TYPE BKBEZ,              "Valuation Class Description
       BNKAN      LIKE RSEG-BNKAN,         "Unplanned cost in Doc Curr
       BNKANLOC   LIKE RSEG-BNKAN,         "Unplanned cost in Local Curr
       EXBAS      TYPE J_1IEXCBAS,         "Assessible Value
*       exbas1      TYPE j_1iexcbas,         "Assessible Value
       HKONT      LIKE BSEG-HKONT,         "GL A/C
       MWSKZ      LIKE BSET-MWSKZ,         "Tax Code
       MATNR      LIKE RSEG-MATNR,         "Material
       MAKTX      LIKE MAKT-MAKTX,         "Material Description
       EBELN      LIKE RSEG-EBELN,         "PO
       EBELP      LIKE RSEG-EBELP,         "PO item

*       wrbtr      LIKE rseg-wrbtr,         "Amount

       DMBTR      LIKE BSEG-DMBTR,         "Amount
       WRBTR      LIKE BSEG-WRBTR,         "Document Currency Amount
       FRGT       LIKE BSEG-WRBTR,         "Freight                                                       "added 18.02.2009
       PCKN       LIKE BSEG-WRBTR,         "Packing                                                       "added 18.02.2009
       MISC       LIKE BSEG-WRBTR,         "Misc.Chgs.                                                    "added 18.02.2009
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
       J_1ILSTNO  TYPE J_1ILSTNO,          "LST No (TIN No)
       J_1IPANNO  TYPE J_1IPANNO,          "PAN No
       J_1ISERN   TYPE J_1ISERN,           "Service Tax Registration Number
       DISC       TYPE KWERT,              "Discount
       HCESS      TYPE KWERT,              "SHCess on BED
       TAX_DESC   TYPE TEXT1_007S,         "Tax Code Desc
       GSBER      LIKE BSEG-GSBER,         " Business Area
       CSTNO      TYPE J_1ICSTNO,          " CST No
       REGIO      LIKE LFA1-REGIO,         " Region
       ORT01      LIKE LFA1-ORT01,         " City
       CHAPID LIKE J_1IMTCHID-J_1ICHID,    " Chapter ID
       CHAPID_DESC LIKE J_1ICHIDTX-J_1ICHT1, " Chapter ID Desc
       GL_DESC    LIKE SKAT-TXT50,           " G/L Account Description
       ECCNO     LIKE J_1IMOVEND-J_1IEXCD,    "#EC CI_USAGE_OK[2877717] " ECC NoAdded by <IT-CAR Tool> during Code Remediation
       REGIO_DESC LIKE T005U-BEZEI,
       WERKS LIKE  RSEG-WERKS,
       IMP_FREIGHT_VAL LIKE RSEG-WRBTR, " Import Freight Value
*       prcdiff    TYPE kwert,              "Price Differnce
*       prcdiffind TYPE shkzg,              "Debit/Credit Indicator
*       othr       TYPE kwert,              "Others
*       othrind    TYPE shkzg,              "Debit/Credit Indicator
       QTY        TYPE MSEG-MENGE,          " Quantity
       BED_N      TYPE KWERT,               " Bed Non Deductable
       CES_N      TYPE KWERT,               " Cesss Non Deductable
       HCESS_N    TYPE KWERT,               " H cesss Non Deductable
       AED_N      TYPE KWERT,               " AED Non Deductable
       LND_CST    TYPE KBETR,               " Landing cost
       LAND1      TYPE LFA1-LAND1,          " Vendor Country code
       MATKL      TYPE MARA-MATKL,          " material group
       MTART      TYPE MARA-MTART,          " Material Type
       MEINS      TYPE RSEG-MEINS,          " Base Unit
       EKGRP      TYPE EKKO-EKGRP,          " Prchase Group
       NAME2      TYPE T001W-NAME1,         " plant name
       RATE       TYPE KWERT,               " rate
       WGBEZ      TYPE T023T-WGBEZ,         " Material Group text
       MENGE      TYPE RSEG-MENGE,
       SETTLED    TYPE RSEG-MENGE,          " Settled Qty
       SHKZG      TYPE BSET-SHKZG,
       AWKEY1     TYPE BKPF-AWKEY,          " Concatenate IN it_final (it_final-g_belnr + it_final-buzei)
       AWKEY2     TYPE BKPF-AWKEY,          " Concatenate IN it_final (it_final-g_ebeln + it_final-ebelp)
       AWKEY3     TYPE BKPF-AWKEY,          " Concatenate IN it_final (it_final-belnr + it_final-ktosl)
       HKONT1     TYPE BSET-HKONT,
       KNUMH      TYPE BSET-KNUMH,
       AWKEY4     TYPE BKPF-AWKEY,
       TXGRP      TYPE BSET-TXGRP,
       BSTME      LIKE RSEG-BSTME,
       TCODE      LIKE BKPF-TCODE,
       KSCHL      LIKE RSEG-KSCHL,
       PRCTR      TYPE MARC-PRCTR,          " Profit Center
       BSART      TYPE EKKO-BSART,          " Purchasing Document Type
       DOC_TYP_DESC TYPE TEXT30,
       REVNO      TYPE REVNO ,
       EXKBE      LIKE RSEG-EXKBE,
       KOART      LIKE BSEG-KOART,
       GL_ACNT    LIKE BSEG-HKONT,
       GL_AMT     LIKE BSEG-DMBTR,
       DF_AMT     LIKE BSEG-DMBTR,
       ANLN1      LIKE MSEG-ANLN1,
       TXT50      LIKE ANLA-TXT50,
       GL_TEXT    LIKE SKAT-TXT50,
       CON_GL_ACNT    LIKE BSEG-HKONT,
       CON_GL_TEXT    LIKE SKAT-TXT50,
       VAL_TEXT    TYPE CHAR50,
       PRCT_TEXT    TYPE CHAR50,
END OF IT_FINAL.

DATA : IT_FINALN LIKE TABLE OF IT_FINAL ,
       WA_FINALN LIKE IT_FINAL.
DATA:BEGIN OF IT_EKKO OCCURS 0,
       EBELN      TYPE EKKO-EBELN,
       EKGRP      TYPE EKKO-EKGRP,
       BSART      TYPE EKKO-BSART,          " Purchasing Document Type
       DOC_TYP_DESC TYPE TEXT30,
       REVNO      TYPE REVNO ,
     END OF IT_EKKO.
DATA : T_MARA         TYPE STANDARD TABLE OF MARA WITH HEADER LINE,
       T_T023T        TYPE STANDARD TABLE OF T023T WITH HEADER LINE,
       T_LISTHEADER TYPE SLIS_T_LISTHEADER WITH HEADER LINE,
       T_FIELDCATALOG TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE,
       T_EVENT        TYPE SLIS_T_EVENT        WITH HEADER LINE,
       FS_LAYOUT      TYPE SLIS_LAYOUT_ALV,
       T_FIELDCAT     TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE,
       GT_SORT     TYPE SLIS_T_SORTINFO_ALV WITH HEADER LINE.

DATA : BEGIN OF IT_BSET1 OCCURS 0,
       BUKRS LIKE BSET-BUKRS,
       BELNR LIKE BSET-BELNR,
       GJAHR LIKE BSET-GJAHR,
       END OF IT_BSET1.

DATA : BEGIN OF IT_RSEG1 OCCURS 0,
       EBELN  LIKE RSEG-EBELN,
       EBELP  LIKE RSEG-EBELP,
       BELNR  LIKE RSEG-BELNR,
       GJAHR  LIKE RSEG-GJAHR,
       BUZEI  LIKE RSEG-BUZEI,
       MATNR  LIKE RSEG-MATNR,
       BUKRS  LIKE RSEG-BUKRS,
       WRBTR  LIKE RSEG-WRBTR,
       SHKZG  LIKE RSEG-SHKZG,
       MWSKZ  LIKE RSEG-MWSKZ,
       KSCHL  LIKE RSEG-KSCHL, "Condition type in RSEG
       LFPOS  LIKE RSEG-LFPOS, "Reference Documenet item
       LIFNR  LIKE RBKP-LIFNR,
       NAME1  LIKE LFA1-NAME1,
       BUKRS1 LIKE BKPF-BUKRS,
       BELNR1 LIKE BKPF-BELNR,
       GJAHR1 LIKE BKPF-GJAHR,
       END OF IT_RSEG1.

DATA: I TYPE SY-TABIX.

DATA : BEGIN OF R_RSEG OCCURS 0,
       BELNR LIKE RSEG-BELNR,
       GJAHR LIKE RSEG-GJAHR,
       END OF R_RSEG.

DATA :BEGIN OF IT_CHPT OCCURS 0,
       MATNR LIKE J_1IMTCHID-MATNR,
       CHAPID LIKE J_1IMTCHID-J_1ICHID,
     END OF IT_CHPT.

DATA :BEGIN OF IT_CHPT_DESC OCCURS 0,
       CHAPID LIKE J_1ICHIDTX-J_1ICHID,
       CHAPID_DESC LIKE J_1ICHIDTX-J_1ICHT1,
     END OF IT_CHPT_DESC.

DATA : BEGIN OF IT_EKKN OCCURS 0,
        EBELN LIKE EKKN-EBELN,
        EBELP LIKE EKKN-EBELP,
        SAKTO LIKE EKKN-SAKTO,
      END OF IT_EKKN.

DATA: BEGIN OF IT_MSEG OCCURS 0,
        MBLNR TYPE MSEG-MBLNR,
        MJAHR TYPE MSEG-MJAHR,
        BWART TYPE MSEG-BWART,
        LFBNR TYPE MSEG-LFBNR,
        AWKEY TYPE BKPF-AWKEY,
        MATNR TYPE MSEG-MATNR,
        LIFNR TYPE MSEG-LIFNR,
        BUKRS TYPE MSEG-BUKRS,
        GJAHR TYPE MSEG-GJAHR,
        EBELN TYPE MSEG-EBELN,
        PPRCTR TYPE MSEG-PPRCTR,
        ANLN1  TYPE ANLN1,
      END OF IT_MSEG.

* GL Account Description
DATA : BEGIN OF IT_GLDESC OCCURS 0.
        INCLUDE STRUCTURE SKAT.
DATA :END OF IT_GLDESC.
DATA : BEGIN OF IT_GLDESC1 OCCURS 0.
        INCLUDE STRUCTURE SKAT.
DATA :END OF IT_GLDESC1.
*DATA : it_ekko TYPE STANDARD TABLE OF ekko WITH HEADER LINE,
DATA:    IT_T001W TYPE STANDARD TABLE OF T001W WITH HEADER LINE.
**--------------------------------------------------** Start of Modif.       "18.02.2009 John

DATA : WA_BSET  LIKE LINE OF IT_BSET.

DATA : WA_FINAL LIKE LINE OF IT_FINAL.

DATA : IT_BKPF2 LIKE TABLE OF IT_BKPF,
       WA_BKPF2 LIKE LINE OF IT_BKPF.

DATA : IT_RSEG_CN LIKE TABLE OF IT_RSEG,
       WA_RSEG_CN LIKE LINE OF IT_RSEG.

DATA : IT_FINAL1 LIKE TABLE OF IT_FINAL,
       WA_FINAL1 LIKE LINE OF IT_FINAL.

DATA : IT_FINAL_TMP LIKE TABLE OF IT_FINAL,
       WA_FINAL_TMP LIKE LINE OF IT_FINAL.

DATA : WA_FINAL1TMP LIKE LINE OF IT_FINAL1.                     "line add 22.02.2009

DATA : MARC_MATNR TYPE MARC-MATNR,
       MARC_WERKS TYPE MARC-WERKS,
       MARC_PRCTR TYPE MARC-PRCTR,
       IT_MARC    TYPE STANDARD TABLE OF MARC WITH HEADER LINE.

**--------------------------------------------------** End of Modif.       "18.02.2009 John
***                                                                    "added 25.02.2009  Start
DATA : WA_RSEG1 LIKE LINE OF IT_RSEG,
       WA_RSEG2 LIKE LINE OF IT_RSEG,
       WA_RSEG3 LIKE LINE OF IT_RSEG.

DATA : IT_RSEG4 LIKE IT_RSEG OCCURS 0 WITH HEADER LINE.
TYPE-POOLS : VRM.
DATA: PARAM TYPE VRM_ID,
       VALUES     TYPE VRM_VALUES,
       VALUE LIKE LINE OF VALUES,
       LV_BELNR LIKE EKBE-BELNR,
       LV_GJAHR LIKE EKBE-GJAHR.
CONSTANTS:C_LIFNR1 LIKE LFA1-LIFNR VALUE '0000100520',
          C_LIFNR2 LIKE LFA1-LIFNR VALUE '0000100521'.
****DATA : wa_ekko TYPE ekko,
****      wa_rseg TYPE rseg.
*****      wa_final LIKE it_final.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS : S_BUKRS FOR BKPF-BUKRS OBLIGATORY,
                 S_WERKS FOR RSEG-WERKS ,"OBLIGATORY,
                 P_LIFNR FOR RBKP-LIFNR,
                 S_BELNR FOR BKPF-BELNR,
                 S_GJAHR FOR BKPF-GJAHR OBLIGATORY,
                 S_BUDAT FOR BKPF-BUDAT OBLIGATORY,
                 S_MWSKZ FOR BSET-MWSKZ NO-DISPLAY,

                 S_MATNR FOR RSEG-MATNR NO-DISPLAY,
                 S_MTART FOR T_MARA-MTART NO-DISPLAY,
                 S_MATKL FOR T_MARA-MATKL NO-DISPLAY.
*                 s_blart FOR bkpf-blart  OBLIGATORY.
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN: BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-101.
PARAMETERS: RD_VAT RADIOBUTTON GROUP G1 ,
            RD_CST RADIOBUTTON GROUP G1.
SELECTION-SCREEN: END OF BLOCK B2.

**--------------------------------------------------**         Start of Modif.     "03.03.2009 John
*SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
*PARAMETERS : chk1 AS CHECKBOX.
*SELECTION-SCREEN END OF BLOCK b2.
**--------------------------------------------------**         End  of Modif.      "03.03.2009 John
START-OF-SELECTION.
*&---------------------------------------------------------------------*
*& Subroutine to process report data.
*&---------------------------------------------------------------------*
  PERFORM PROCESS_DATA.
*&---------------------------------------------------------------------*
*&      Form  process_data
*&---------------------------------------------------------------------*

FORM PROCESS_DATA .

  DATA : IT_RSEGT LIKE IT_RSEG OCCURS 0,
         IT_BSEGT LIKE IT_BSEG OCCURS 0 WITH HEADER LINE,
         IT_BSETT LIKE IT_BSET OCCURS 0 WITH HEADER LINE.
  DATA : IT_BSETTT LIKE IT_BSET OCCURS 0 WITH HEADER LINE.
*Get Data from BKPF Table

  SELECT BUKRS
         BELNR
         GJAHR
         BLART
         BLDAT
         BUDAT
         XBLNR
         WAERS
         KURSF
         AWKEY
         TCODE FROM BKPF
         INTO CORRESPONDING FIELDS OF TABLE IT_BKPF
         WHERE BUKRS IN S_BUKRS AND
               BELNR IN S_BELNR AND
               GJAHR IN S_GJAHR AND
               BUDAT IN S_BUDAT. "AND
*               blart IN s_blart.
  IT_BKPF2[] = IT_BKPF[].

  LOOP AT IT_BKPF.
    IT_BKPF-G_BELNR = IT_BKPF-AWKEY+0(10).
    IT_BKPF-G_GJAHR = IT_BKPF-AWKEY+10(4).
    IF SY-SUBRC EQ 0.
      MODIFY IT_BKPF TRANSPORTING G_BELNR G_GJAHR.
    ENDIF.

  ENDLOOP.

  IF NOT IT_BKPF[] IS INITIAL.

    SELECT BELNR GJAHR LIFNR BLART
      INTO TABLE IT_RBKP
      FROM RBKP
      FOR ALL ENTRIES IN IT_BKPF
      WHERE BELNR = IT_BKPF-G_BELNR
        AND GJAHR = IT_BKPF-G_GJAHR
        AND BLART = IT_BKPF-BLART
        AND LIFNR IN P_LIFNR.

    IF NOT IT_RBKP[] IS INITIAL.
      SELECT BELNR GJAHR BUZEI EBELN EBELP MATNR BUKRS WRBTR MENGE MEINS
          SHKZG MWSKZ BKLAS BNKAN KSCHL LFBNR LFPOS WERKS STUNR EXKBE
     INTO CORRESPONDING FIELDS OF TABLE  IT_RSEG
     FROM RSEG
     FOR ALL ENTRIES IN IT_RBKP
     WHERE BELNR = IT_RBKP-BELNR
       AND GJAHR = IT_RBKP-GJAHR
       AND WERKS IN S_WERKS
       AND MATNR IN S_MATNR.

      SELECT BELNR MEINS BSTME EBELN EBELP
        FROM RSEG
        INTO CORRESPONDING FIELDS OF TABLE BASE_UNIT
        FOR ALL ENTRIES IN IT_RBKP
        WHERE BELNR = IT_RBKP-BELNR
          AND GJAHR = IT_RBKP-GJAHR.

    ENDIF.
    IF IT_RSEG[] IS NOT INITIAL.                               "Added by Rameshwar
      SELECT EBELN EKGRP BSART REVNO
      FROM EKKO
        INTO TABLE IT_EKKO
        FOR ALL ENTRIES IN IT_RSEG
      WHERE EBELN = IT_RSEG-EBELN AND BSART NE 'ZSC'.
    ENDIF.
    IT_RSEG4[] = IT_RSEG[].

    LOOP AT IT_BKPF.

      LOOP AT IT_RSEG WHERE BELNR = IT_BKPF-G_BELNR.

        MOVE : IT_BKPF-BUKRS TO IT_RSEG-BUKRS,
               IT_BKPF-BELNR TO IT_RSEG-BELNR1,
               IT_BKPF-GJAHR TO IT_RSEG-GJAHR,
               IT_BKPF-BUDAT TO IT_RSEG-BUDAT,
               IT_BKPF-BLART TO IT_RSEG-BLART.
        MODIFY IT_RSEG.
      ENDLOOP.

    ENDLOOP.

    LOOP AT IT_BKPF.

      LOOP AT BASE_UNIT WHERE BELNR = IT_BKPF-G_BELNR.
        MOVE : IT_BKPF-BELNR TO BASE_UNIT-BELNR2.
        MODIFY BASE_UNIT.
      ENDLOOP.

    ENDLOOP.

    LOOP AT IT_RBKP.
      LOOP AT IT_RSEG WHERE BELNR = IT_RBKP-BELNR.
        MOVE : IT_RBKP-LIFNR TO IT_RSEG-LIFNR.
        MODIFY IT_RSEG.
      ENDLOOP.
    ENDLOOP.

    SELECT LIFNR NAME1 REGIO ORT01 LAND1 FROM LFA1
      INTO TABLE IT_LFA1
      FOR ALL ENTRIES IN IT_RBKP
      WHERE LIFNR = IT_RBKP-LIFNR.


    SELECT * FROM T005U
            INTO TABLE IT_T005U
    FOR ALL ENTRIES IN IT_LFA1
    WHERE SPRAS = SY-LANGU
    AND   LAND1 = IT_LFA1-LAND1
    AND   BLAND = IT_LFA1-REGIO.

    SELECT * FROM MARA INTO TABLE T_MARA
                       FOR ALL ENTRIES IN IT_RSEG
                       WHERE MATNR = IT_RSEG-MATNR AND
                             MATKL IN S_MATKL AND
                             MTART IN S_MTART.

    LOOP AT T_MARA.
      LOOP AT IT_RSEG WHERE MATNR = T_MARA-MATNR.
        MOVE : T_MARA-MTART TO IT_RSEG-MTART.
        MOVE : T_MARA-MATKL TO IT_RSEG-MATKL.
        MODIFY IT_RSEG.
      ENDLOOP.
    ENDLOOP.

    SELECT * FROM T023T INTO TABLE T_T023T
                   FOR ALL ENTRIES IN T_MARA
                   WHERE MATKL = T_MARA-MATKL
                   AND SPRAS = 'E'.

    SELECT * FROM T001W INTO TABLE IT_T001W
                          FOR ALL ENTRIES IN IT_RSEG
                          WHERE WERKS = IT_RSEG-WERKS.
  ENDIF.

  IF NOT IT_RSEG[] IS INITIAL.

* TO CHECK FOR 103 MOVEMENT TYPE
    SELECT MBLNR MJAHR BWART LFBNR MATNR LIFNR BUKRS GJAHR EBELN PPRCTR  ANLN1"menge
          INTO CORRESPONDING FIELDS OF TABLE IT_MSEG
          FROM MSEG FOR ALL ENTRIES IN IT_RSEG
          WHERE MBLNR = IT_RSEG-LFBNR AND
                GJAHR = IT_RSEG-GJAHR AND
                BUKRS = IT_RSEG-BUKRS AND
                MATNR = IT_RSEG-MATNR AND
                EBELN = IT_RSEG-EBELN AND
                BWART = '101'.
* Find GRR Date
    SELECT MBLNR BUDAT MJAHR
            FROM MKPF
            INTO TABLE IT_MKPF
            FOR ALL ENTRIES IN IT_RSEG
            WHERE MBLNR EQ IT_RSEG-LFBNR.

    LOOP AT IT_MSEG  .
      IF SY-SUBRC = 0.
        CONCATENATE IT_MSEG-MBLNR IT_MSEG-MJAHR INTO IT_MSEG-AWKEY.
        MODIFY IT_MSEG TRANSPORTING AWKEY.
      ENDIF.
    ENDLOOP.
  ENDIF.

  SELECT DOCNO EXBAS RDOC1 RITEM1 RDOC2
          FROM J_1IEXCDTL
          INTO TABLE IT_J_1IEXCDTL
          FOR ALL ENTRIES IN IT_RSEG
          WHERE RDOC1  EQ IT_RSEG-EBELN
          AND   RITEM1 EQ IT_RSEG-EBELP
          AND   RDOC2  EQ IT_RSEG-LFBNR.

  SELECT MATNR
         J_1ICHID
         FROM J_1IMTCHID
         INTO TABLE IT_CHPT
         FOR ALL ENTRIES IN IT_RSEG
         WHERE MATNR = IT_RSEG-MATNR.

  SELECT J_1ICHID J_1ICHT1
  FROM J_1ICHIDTX
  INTO TABLE IT_CHPT_DESC
  FOR ALL ENTRIES IN IT_CHPT
  WHERE J_1ICHID = IT_CHPT-CHAPID
  AND   LANGU = 'EN'.

  SORT IT_RSEG BY BKLAS.
  IT_RSEGT[] = IT_RSEG[].
  DELETE ADJACENT DUPLICATES FROM IT_RSEGT COMPARING BKLAS.

  SELECT BKLAS BKBEZ
          FROM T025T
          INTO TABLE IT_T025T
          FOR ALL ENTRIES IN IT_RSEGT
          WHERE SPRAS EQ SY-LANGU
          AND   BKLAS EQ IT_RSEGT-BKLAS.
  CLEAR IT_RSEGT[].

  SORT IT_RBKP BY LIFNR.

  SELECT LIFNR J_1ILSTNO J_1IPANNO J_1ISERN J_1ICSTNO J_1IEXCD
          FROM LFA1 " J_1IMOVEND Replaced With LFA1 Added by <IT-CAR Tool> during Code Remediation - tool
          INTO TABLE IT_J_1IMOVEND
          FOR ALL ENTRIES IN IT_LFA1
          WHERE LIFNR EQ IT_LFA1-LIFNR.

*Get Material Description

  IF NOT IT_RSEG[] IS INITIAL.
    SORT IT_RSEG BY MATNR.
    IT_RSEGT[] = IT_RSEG[].
    DELETE ADJACENT DUPLICATES FROM IT_RSEGT COMPARING MATNR.

    SELECT MATNR
           MAKTX FROM MAKT
           INTO TABLE IT_MAKT
           FOR ALL ENTRIES IN IT_RSEGT
           WHERE MATNR = IT_RSEGT-MATNR AND
                 SPRAS = 'EN'.
    CLEAR IT_RSEGT[].

    IF NOT IT_BKPF[] IS INITIAL.

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
             BUZEI
             HKONT
             KNUMH FROM BSET
             INTO CORRESPONDING FIELDS OF TABLE IT_BSET
             FOR ALL ENTRIES IN IT_BKPF
             WHERE BUKRS = IT_BKPF-BUKRS AND
                   BELNR = IT_BKPF-BELNR AND
                   GJAHR = IT_BKPF-GJAHR AND
                   MWSKZ IN S_MWSKZ ORDER BY PRIMARY KEY.  " Added by <IT-CAR Tool> during Code Remediation

    ENDIF.
    SORT IT_BSET BY BELNR.
    LOOP AT IT_BSET .

      IF IT_BSET-SHKZG = 'H'.
*        it_bset-hwste = it_bset-hwste * ( -1 ).
*        it_bset-hwbas = it_bset-hwbas * ( -1 ).
        IT_BSET-HWSTE = IT_BSET-HWSTE * ( -1 ).
        IT_BSET-HWBAS = IT_BSET-HWBAS * ( -1 ).
        MODIFY IT_BSET TRANSPORTING HWSTE HWBAS.
      ENDIF.
    ENDLOOP..
    IT_BSETTT[] = IT_BSET[].

    DELETE ADJACENT DUPLICATES FROM IT_BSETTT COMPARING TXGRP BELNR.

    LOOP AT IT_BKPF2 INTO WA_BKPF2.

      WA_BKPF2-G_BELNR = WA_BKPF2-AWKEY+0(10).
      WA_BKPF2-G_GJAHR = WA_BKPF2-AWKEY+10(4).
      IF SY-SUBRC EQ 0.
        MODIFY IT_BKPF2 FROM WA_BKPF2 TRANSPORTING G_BELNR G_GJAHR.
      ENDIF.
      CLEAR WA_BKPF2.
    ENDLOOP.

    SORT IT_BKPF2 BY G_BELNR.
    LOOP AT IT_BKPF2 INTO WA_BKPF2.
      LOOP AT IT_RSEG WHERE BELNR = WA_BKPF2-G_BELNR .
        IF SY-SUBRC = 0.
          IT_RSEG-ADD = WA_BKPF2-BELNR.
        ENDIF.
        MODIFY IT_RSEG TRANSPORTING ADD .
      ENDLOOP.
    ENDLOOP.

*    LOOP AT it_bseg." INTO wa_bseg.
*      LOOP AT it_bset into wa_bset WHERE belnr = it_bseg-belnr .
*        IF sy-subrc = 0.
*          move : it_bseg-ebeln to wa_bset-ebeln.
*        ENDIF.
*        MODIFY it_bset.
*      ENDLOOP.
*    ENDLOOP.

    LOOP AT IT_RSEG.
      LOOP AT IT_BSETTT WHERE BELNR = IT_RSEG-ADD AND
                            TXGRP = IT_RSEG-BUZEI..
        CONCATENATE IT_BSETTT-BELNR IT_BSETTT-TXGRP INTO IT_BSETTT-AWKEY.
        IT_RSEG-AWKEY = IT_BSETTT-AWKEY.
        MODIFY IT_RSEG TRANSPORTING AWKEY.
      ENDLOOP..
    ENDLOOP.

    LOOP AT IT_BSET.
      CONCATENATE IT_BSET-BELNR IT_BSET-TXGRP INTO IT_BSET-AWKEY.
      MODIFY IT_BSET TRANSPORTING AWKEY.
    ENDLOOP.

    SORT IT_RSEG BY BELNR GJAHR.

    LOOP AT IT_RSEG.

      READ TABLE IT_MSEG WITH KEY LFBNR = IT_RSEG-LFBNR.
      IF  ( SY-SUBRC = 0 AND  IT_MSEG-BWART = '105'  ).
        IT_RSEG-LFBNR = IT_MSEG-MBLNR.
        MODIFY IT_RSEG TRANSPORTING LFBNR..

      ENDIF.

      READ TABLE R_RSEG WITH KEY BELNR = IT_RSEG-BELNR
                                 GJAHR = IT_BSEG-GJAHR.
      IF SY-SUBRC NE 0.
        R_RSEG-BELNR = IT_RSEG-BELNR.
        R_RSEG-GJAHR = IT_RSEG-GJAHR.
        APPEND R_RSEG.
        CLEAR  R_RSEG.
      ENDIF.
    ENDLOOP.

    SORT R_RSEG BY BELNR GJAHR.
    DELETE ADJACENT DUPLICATES FROM R_RSEG COMPARING BELNR GJAHR.
    SORT R_RSEG BY BELNR GJAHR.

    SORT IT_RSEG BY BELNR BUZEI.

    LOOP AT IT_RSEG.
      IF IT_RSEG-SHKZG = 'H'.
        IT_RSEG-WRBTR = IT_RSEG-WRBTR * ( -1 ).
        MODIFY IT_RSEG TRANSPORTING WRBTR.
      ENDIF.
      READ TABLE IT_BKPF WITH KEY G_BELNR = IT_RSEG-BELNR
                                  G_GJAHR = IT_RSEG-GJAHR.
      IF SY-SUBRC EQ 0.
        IT_RSEG-BUKRS1 = IT_BKPF-BUKRS.
        IT_RSEG-BELNR1 = IT_BKPF-BELNR.
        IT_RSEG-GJAHR1 = IT_BKPF-GJAHR.
        MODIFY IT_RSEG TRANSPORTING BUKRS1 BELNR1 GJAHR1.
      ENDIF.

      IF IT_RSEG-LFPOS = 0.
        READ TABLE IT_BSET WITH KEY BELNR = IT_RSEG-BELNR1
                                    BUKRS = IT_RSEG-BUKRS1
                                    GJAHR = IT_RSEG-GJAHR1.
        IF SY-SUBRC EQ 0.
          L_LFPOS = IT_RSEG-BUZEI+2(4).
          IT_RSEG-LFPOS = L_LFPOS.
          MODIFY IT_RSEG TRANSPORTING LFPOS.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.
*Get G/L Account from bseg

  IF NOT IT_BKPF[] IS INITIAL.

    SELECT BUKRS "#EC CI_DB_OPERATION_OK[2431747] " Added by <IT-CAR Tool> during Code Remediation
           BELNR
           GJAHR
           BUZID
           BUZEI
           SHKZG
           DMBTR
           WRBTR
           HKONT
           FWBAS
           EBELN
           EBELP
           GSBER
           KOART
           KTOSL
           MWSKZ FROM BSEG
                 INTO CORRESPONDING FIELDS OF TABLE IT_BSEG
                 FOR ALL ENTRIES IN IT_BKPF
                 WHERE BUKRS = IT_BKPF-BUKRS AND
                       BELNR = IT_BKPF-BELNR AND
                       GJAHR = IT_BKPF-GJAHR  ORDER BY PRIMARY KEY.  " Added by <IT-CAR Tool> during Code Remediation
*                             gsber IN s_gsber and
*                       buzei NE '1'.

    LOOP AT IT_BSEG.
      LOOP AT IT_RSEG WHERE BELNR1 = IT_BSEG-BELNR.
        MOVE : IT_BSEG-GSBER TO IT_RSEG-GSBER.
        MODIFY IT_RSEG.
      ENDLOOP.
    ENDLOOP.

    IF IT_BSEG IS NOT INITIAL..

      SELECT EBELN EBELP MATNR WERKS MENGE NETPR NETWR MWSKZ
             FROM EKPO INTO TABLE IT_EKPO
             FOR ALL ENTRIES IN IT_BSEG
             WHERE EBELN = IT_BSEG-EBELN
               AND EBELP = IT_BSEG-EBELP.

    ENDIF.


***Comment By Vivek
    IF NOT IT_BKPF[] IS INITIAL.
** Find GL Account for GR No
      SELECT BUKRS "#EC CI_DB_OPERATION_OK[2431747] " Added by <IT-CAR Tool> during Code Remediation
                BELNR
                GJAHR
                BUZID
                SHKZG
                DMBTR
                WRBTR
                HKONT
                FWBAS
                EBELN
                EBELP
                GSBER
                MWSKZ FROM BSEG
                      INTO CORRESPONDING FIELDS OF TABLE IT_BSEG1
                      FOR ALL ENTRIES IN IT_BKPF
                      WHERE BUKRS = IT_BKPF-BUKRS AND
                            BELNR = IT_BKPF-BELNR AND
                            GJAHR = IT_BKPF-GJAHR AND
*                            gsber IN s_gsber AND
                            BUZID = 'M'. "#EC CI_NOORDER  " Added by <IT-CAR Tool> during Code Remediation

    ENDIF.
    IF NOT IT_BKPF[] IS INITIAL.
** Find GL Account for GR No
      SELECT BUKRS "#EC CI_DB_OPERATION_OK[2431747] " Added by <IT-CAR Tool> during Code Remediation
                BELNR
                GJAHR
                BUZID
                SHKZG
                DMBTR
                WRBTR
                HKONT
                FWBAS
                EBELN
                EBELP
                GSBER
                MWSKZ FROM BSEG
                      INTO CORRESPONDING FIELDS OF TABLE IT_BSEG2
                      FOR ALL ENTRIES IN IT_BKPF
                      WHERE BUKRS = IT_BKPF-BUKRS AND
                            BELNR = IT_BKPF-BELNR AND
                            GJAHR = IT_BKPF-GJAHR AND
                            BUZEI = '1'. "#EC CI_NOORDER " Added by <IT-CAR Tool> during Code Remediation
    ENDIF.
    SELECT *  FROM SKAT
            INTO TABLE IT_GLDESC
            FOR ALL ENTRIES IN IT_BSEG1
            WHERE SAKNR = IT_BSEG1-HKONT
            AND   KTOPL = 'YAIN'
            AND   SPRAS = SY-LANGU.

    SELECT * FROM SKAT
            INTO TABLE IT_GLDESC1
            FOR ALL ENTRIES IN IT_BSEG2
            WHERE SAKNR = IT_BSEG2-HKONT
            AND   KTOPL = 'YAIN'
            AND   SPRAS = SY-LANGU.

  ENDIF.

  LOOP AT IT_RSEG.
    READ TABLE IT_BSET WITH KEY BUKRS = IT_RSEG-BUKRS1
                                BELNR = IT_RSEG-BELNR1
                                GJAHR = IT_RSEG-GJAHR1
                                TXGRP = IT_RSEG-LFPOS.
*                                txgrp = it_rseg-count1.
    IF SY-SUBRC = 0.
      IT_BSET-EBELN = IT_RSEG-EBELN.
      IT_BSET-EBELP = IT_RSEG-EBELP.
      MODIFY IT_BSET TRANSPORTING EBELN EBELP WHERE BUKRS = IT_RSEG-BUKRS1
                                              AND   BELNR = IT_RSEG-BELNR1
                                              AND   GJAHR = IT_RSEG-GJAHR1
                                              AND   TXGRP = IT_RSEG-LFPOS.
*                                              AND   txgrp = it_rseg-count1.
    ENDIF.
  ENDLOOP.
  SORT IT_BSET BY EBELN EBELP BELNR.

  LOOP AT IT_RSEG.
    MOVE : IT_BSET-TXGRP TO IT_RSEG-TXGRP.
    MODIFY IT_RSEG.
  ENDLOOP.
  SORT IT_BSEG2 BY BELNR .



  SORT IT_BSET BY BELNR TXGRP.
  SORT IT_BSEG BY BELNR.
  SORT IT_RSEG BY BELNR.
  LOOP AT IT_BSEG.

    MOVE IT_BSEG-GSBER  TO IT_FINAL-GSBER.
    MOVE IT_BSEG-BELNR  TO IT_FINAL-BELNR.
    MOVE IT_BSEG-GJAHR  TO IT_FINAL-GJAHR.


*    MOVE it_bseg-bukrs  TO it_final-burrs.
*          MOVE it_rseg-ebeln  TO it_final-ebeln.
*      MOVE it_rseg-ebelp  TO it_final-ebelp.

    READ TABLE IT_EKPO WITH KEY EBELN = IT_BSEG-EBELN
                                EBELP = IT_BSEG-EBELP.

    IT_FINAL-RATE = IT_EKPO-NETPR.

    READ TABLE IT_BKPF WITH KEY BUKRS = IT_BSEG-BUKRS
                                BELNR = IT_BSEG-BELNR
                                GJAHR = IT_BSEG-GJAHR.   " Added By Vivek 28022011
    MOVE IT_BKPF-WAERS TO IT_FINAL-WAERS.
    MOVE IT_BKPF-KURSF TO IT_FINAL-KURSF.
********
    READ TABLE IT_RSEG WITH KEY BELNR1 = IT_BSEG-BELNR
                                EBELN  = IT_BSEG-EBELN
                                EBELP  = IT_BSEG-EBELP.
    IF SY-SUBRC = 0.
      MOVE IT_RSEG-EXKBE TO IT_FINAL-EXKBE .
      MOVE IT_RSEG-BUDAT  TO IT_FINAL-BUDAT.
      MOVE IT_RSEG-MWSKZ  TO IT_FINAL-MWSKZ.
      MOVE IT_RSEG-WERKS  TO IT_FINAL-WERKS.
      MOVE IT_RSEG-LIFNR  TO IT_FINAL-LIFNR.
      MOVE IT_RSEG-MATNR  TO IT_FINAL-MATNR.
      MOVE IT_RSEG-MTART  TO IT_FINAL-MTART.
      MOVE IT_RSEG-MATKL  TO IT_FINAL-MATKL.
      MOVE IT_RSEG-BLART  TO IT_FINAL-BLART.
      MOVE IT_RSEG-BUZEI  TO IT_FINAL-BUZEI1.
      MOVE IT_RSEG-KSCHL  TO IT_FINAL-KSCHL.
*    MOVE it_rseg-meins  TO it_final-meins.
      MOVE IT_RSEG-BUKRS TO IT_FINAL-BUKRS.
*      MOVE it_rseg-bukrs1 TO it_final-bukrs.
      MOVE IT_RSEG-GJAHR1 TO IT_FINAL-GJAHR.
      MOVE IT_RSEG-EBELN  TO IT_FINAL-EBELN.
      MOVE IT_RSEG-EBELP  TO IT_FINAL-EBELP.
      MOVE IT_RSEG-AWKEY  TO IT_FINAL-AWKEY4.
      MOVE IT_RSEG-LFBNR  TO IT_FINAL-LFBNR.
      MOVE IT_RSEG-BNKAN  TO IT_FINAL-BNKAN.
      MOVE IT_RSEG-BKLAS  TO IT_FINAL-BKLAS.
      MOVE IT_RSEG-WERKS  TO IT_FINAL-WERKS.

      READ TABLE IT_EKKO WITH KEY EBELN = IT_FINAL-EBELN.      "Added by Rameshwar
      IF SY-SUBRC = 0.
        MOVE IT_EKKO-EKGRP TO IT_FINAL-EKGRP.
        MOVE IT_EKKO-BSART TO IT_FINAL-BSART.
        MOVE IT_EKKO-REVNO TO IT_FINAL-REVNO.

        SELECT BATXT UP TO 1 ROWS FROM T161T
               INTO IT_FINAL-DOC_TYP_DESC
               WHERE SPRAS = 'E'
               AND   BSART = IT_FINAL-BSART ORDER BY PRIMARY KEY.
        ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation

      ENDIF.
      READ TABLE IT_T001W WITH KEY WERKS = IT_RSEG-WERKS.

      IF SY-SUBRC = 0.
        MOVE IT_T001W-NAME1  TO IT_FINAL-NAME2.
      ENDIF.

      MOVE IT_RSEG-MEINS  TO IT_FINAL-MEINS.
      MOVE IT_RSEG-BELNR  TO IT_FINAL-G_BELNR.

      READ TABLE BASE_UNIT WITH KEY BELNR2 = IT_BSEG-BELNR
                                    EBELN  = IT_BSEG-EBELN
                                    EBELP  = IT_BSEG-EBELP.
      MOVE BASE_UNIT-BSTME  TO IT_FINAL-BSTME.
      IF IT_FINAL-BSTME = 'LE'.
        IT_FINAL-BSTME = 'AU'.
      ENDIF.
      IF IT_FINAL-BSTME = 'PAK'.
        IT_FINAL-BSTME = 'PAC'.
      ENDIF.
      SELECT BUDAT
          UP TO 1 ROWS FROM MKPF
          INTO IT_FINAL-BUDAT
          WHERE MBLNR = IT_RSEG-LFBNR ORDER BY PRIMARY KEY.
      ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation
      READ TABLE IT_MSEG WITH KEY MBLNR = IT_RSEG-LFBNR
                           GJAHR = IT_RSEG-GJAHR
                           BUKRS = IT_RSEG-BUKRS .
      IF SY-SUBRC = 0.
        MOVE IT_MSEG-PPRCTR TO IT_FINAL-PRCTR.
        MOVE IT_MSEG-ANLN1 TO IT_FINAL-ANLN1.
        SELECT TXT50 UP TO 1 ROWS FROM ANLA INTO IT_FINAL-TXT50 WHERE ANLN1 = IT_FINAL-ANLN1 ORDER BY PRIMARY KEY.
        ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation
      ELSE.
        SELECT SINGLE PRCTR
      FROM MARC
      INTO IT_FINAL-PRCTR
      WHERE MATNR = IT_FINAL-MATNR
        AND WERKS = IT_FINAL-WERKS.
        IF SY-SUBRC NE 0.
          CLEAR:LV_BELNR,LV_GJAHR.
          SELECT BELNR GJAHR
              UP TO 1 ROWS FROM EKBE
              INTO (LV_BELNR,LV_GJAHR)
             WHERE LFBNR = IT_RSEG-LFBNR
               AND GJAHR = IT_FINAL-BUDAT+0(4) ORDER BY PRIMARY KEY.
          ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation
          SELECT PPRCTR UP TO 1 ROWS FROM MSEG
             INTO IT_FINAL-PRCTR
             WHERE MBLNR = LV_BELNR
              AND  MJAHR = LV_GJAHR ORDER BY PRIMARY KEY.
          ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation

        ENDIF.
      ENDIF.


      READ TABLE IT_RBKP WITH KEY BELNR = IT_FINAL-G_BELNR.
      MOVE IT_RBKP-LIFNR  TO IT_FINAL-LIFNR.

      READ TABLE IT_LFA1 WITH KEY LIFNR = IT_FINAL-LIFNR.

      MOVE IT_LFA1-NAME1  TO IT_FINAL-NAME1.
      MOVE IT_RSEG-MATNR  TO IT_FINAL-MATNR.
      READ TABLE T_MARA WITH KEY MATNR = IT_RSEG-MATNR.
      IF SY-SUBRC = 0.
        MOVE T_MARA-MATKL  TO IT_FINAL-MATKL.
        READ TABLE T_T023T WITH KEY MATKL = T_MARA-MATKL.
        IF SY-SUBRC = 0.
          MOVE T_T023T-WGBEZ  TO IT_FINAL-WGBEZ.
        ENDIF.
        MOVE T_MARA-MTART  TO IT_FINAL-MTART.
      ENDIF.
      MOVE IT_RSEG-MWSKZ  TO IT_FINAL-MWSKZ.
      MOVE IT_LFA1-REGIO  TO IT_FINAL-REGIO.
      MOVE IT_LFA1-ORT01  TO IT_FINAL-ORT01.

      SELECT SINGLE TEXT1
      FROM T007S
      INTO IT_FINAL-TAX_DESC
      WHERE SPRAS = SY-LANGU
      AND   MWSKZ = IT_RSEG-MWSKZ
      AND   KALSM = 'TAXINN'.

      MOVE IT_RSEG-BUZEI  TO IT_FINAL-BUZEI.

      IF IT_FINAL-WAERS = 'INR'.
        MOVE IT_RSEG-WRBTR  TO IT_FINAL-DMBTR.
        IF  ( IT_FINAL-LIFNR = C_LIFNR1 OR IT_FINAL-LIFNR = C_LIFNR2 ).
*        it_final-dmbtr = it_final-dmbtr + it_final-dmbtr.
          IT_FINAL-DMBTR = '0.00'.
        ENDIF.
      ENDIF.
      IF IT_FINAL-WAERS <> 'INR'.
        MOVE IT_RSEG-WRBTR  TO IT_FINAL-DMBTR .
*        it_final-dmbtr = it_final-dmbtr * it_final-kursf.
        IF  ( IT_FINAL-LIFNR = C_LIFNR1 OR IT_FINAL-LIFNR = C_LIFNR2 ).
*        it_final-dmbtr = it_final-dmbtr + it_final-dmbtr.
          IT_FINAL-DMBTR = '0.00'.
        ENDIF.
      ENDIF.


      READ TABLE IT_T025T WITH KEY BKLAS = IT_RSEG-BKLAS.
      IF SY-SUBRC = 0.
        MOVE IT_T025T-BKBEZ TO IT_FINAL-BKBEZ.
      ENDIF.

* changes done by Marmik Shah - 12.03.2009



*      READ TABLE it_j_1iexcdtl WITH KEY rdoc1  = it_rseg-ebeln
*                                        ritem1 = it_rseg-ebelp
*                                        rdoc2  = it_rseg-lfbnr.
*      IF sy-subrc = 0.
*        MOVE it_j_1iexcdtl-exbas TO it_final-exbas.
*      ENDIF.

      READ TABLE IT_MAKT WITH KEY MATNR = IT_RSEG-MATNR.
      IF SY-SUBRC EQ 0.
        MOVE IT_MAKT-MAKTX TO IT_FINAL-MAKTX.
      ENDIF.

      READ TABLE IT_J_1IMOVEND WITH KEY LIFNR = IT_RBKP-LIFNR.
      IF SY-SUBRC = 0.
        MOVE IT_J_1IMOVEND-J_1ILSTNO TO IT_FINAL-J_1ILSTNO.
        MOVE IT_J_1IMOVEND-J_1IPANNO TO IT_FINAL-J_1IPANNO.
        MOVE IT_J_1IMOVEND-J_1ISERN  TO IT_FINAL-J_1ISERN.
        MOVE IT_J_1IMOVEND-J_1ICSTNO  TO IT_FINAL-CSTNO.
        MOVE IT_J_1IMOVEND-J_1IEXCD  TO IT_FINAL-ECCNO.

      ENDIF.

      READ TABLE IT_CHPT WITH KEY MATNR = IT_RSEG-MATNR.
      IF SY-SUBRC = 0.
        MOVE IT_CHPT-CHAPID TO IT_FINAL-CHAPID.
      ENDIF.

      READ TABLE IT_CHPT_DESC WITH KEY CHAPID = IT_FINAL-CHAPID.
      IF SY-SUBRC = 0.
        MOVE IT_CHPT_DESC-CHAPID_DESC TO IT_FINAL-CHAPID_DESC.
      ENDIF.

      READ TABLE IT_BKPF WITH KEY BUKRS = IT_BSEG-BUKRS
                                  BELNR = IT_BSEG-BELNR
                                  GJAHR = IT_BSEG-GJAHR.
      .
      IF SY-SUBRC EQ 0.
        MOVE IT_BKPF-BLDAT TO IT_FINAL-BLDAT.
        MOVE IT_BKPF-BUDAT TO IT_FINAL-POSTDAT.
        MOVE IT_BKPF-XBLNR TO IT_FINAL-XBLNR.
        MOVE IT_BKPF-BLART TO IT_FINAL-BLART.
        MOVE IT_BKPF-WAERS TO IT_FINAL-WAERS.
        MOVE IT_BKPF-KURSF TO IT_FINAL-KURSF.
        MOVE IT_BKPF-TCODE TO IT_FINAL-TCODE.
      ENDIF.

      IF IT_FINAL-BLART = 'RE'.
        IT_FINAL-DOC_TYPE = 'Invoice'.
      ENDIF.
      IF IT_FINAL-BLART = 'RL'.
        IT_FINAL-DOC_TYPE = 'Credit Memo'.
      ENDIF.
      IF IT_FINAL-BLART = 'RC'.
        IT_FINAL-DOC_TYPE = 'Subsequent Debit'.
      ENDIF.
      IF IT_FINAL-BLART = 'RA'.
        IT_FINAL-DOC_TYPE = 'Subsequent Credit'.
      ENDIF.

**Add by Vivek
*LOOP at it_bkpf WHERE bukrs = it_bseg-bukrs AND " Added 01032011
*                      belnr = it_bseg-belnr AND
*                      gjahr = it_bseg-gjahr.
      IF ( IT_FINAL-BLART = 'RE' OR IT_FINAL-BLART = 'RL' ) .
        IF SY-SUBRC EQ 0.
          MOVE : IT_RSEG-MENGE TO IT_FINAL-QTY.
        ENDIF.
      ENDIF.

      IF ( IT_FINAL-BLART = 'RC' OR IT_FINAL-BLART = 'RA' ) .
        MOVE : IT_RSEG-MENGE TO IT_FINAL-QTY.
        IT_FINAL-QTY = IT_FINAL-QTY * -1.
      ENDIF.

      IF IT_FINAL-TCODE = 'MR8M'.
        IT_FINAL-QTY = IT_FINAL-QTY * -1.
      ENDIF.
*ENDLOOP.
      MOVE IT_RSEG-MENGE  TO IT_FINAL-MENGE.
      IF IT_FINAL-MENGE <> IT_FINAL-QTY.

        IT_FINAL-SETTLED = IT_FINAL-MENGE + IT_FINAL-QTY.

      ENDIF.
***End by Vivek
      IF IT_FINAL-WAERS = 'JPY'.
        IT_FINAL-DMBTR = IT_FINAL-DMBTR / 10.
        IT_FINAL-WRBTR = IT_FINAL-DMBTR.
        IT_FINAL-WRBTR = ( IT_FINAL-WRBTR ) * IT_FINAL-KURSF / 100.
        IT_FINAL-BNKAN = IT_FINAL-BNKAN / 10.
        IT_FINAL-BNKANLOC = ( IT_FINAL-BNKAN ) * IT_FINAL-KURSF / 100.
*       IF it_final-waers <> 'INR'.
*        it_final-dmbtr = it_final-dmbtr / 10.
*        it_final-wrbtr = it_final-dmbtr.
*        it_final-wrbtr = ( it_final-wrbtr ) * it_final-kursf / 100.
*        it_final-bnkan = it_final-bnkan / 10.
*        it_final-bnkanloc = ( it_final-bnkan ) * it_final-kursf / 100.

      ELSE.
        IT_FINAL-WRBTR = IT_FINAL-DMBTR * IT_FINAL-KURSF."#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
*****Comment 03032011
*        IF it_final-qty IS NOT INITIAL.
*          If it_final-waers = 'INR'.
*          it_final-rate = it_final-wrbtr / it_final-qty.
*          Endif.
*          IF it_final-waers <> 'INR'.
*          it_final-rate = it_final-wrbtr / it_final-qty.
**          it_final-rate = it_final-rate * it_final-kursf.
*          ENDIF.
*        ENDIF.
        IT_FINAL-BNKANLOC = ( IT_FINAL-BNKAN ) * IT_FINAL-KURSF.
      ENDIF.

*    READ TABLE it_bseg WITH KEY bukrs = it_rseg-bukrs1
*                                belnr = it_rseg-belnr1
*                                gjahr = it_rseg-gjahr1
*                                ebeln = it_rseg-ebeln
*                                ebelp = it_rseg-ebelp.
*    IF sy-subrc EQ 0.
*      it_final-gsber = it_bseg-gsber.
*    ENDIF.

      READ TABLE IT_BSEG2 WITH KEY BUKRS = IT_RSEG-BUKRS
                                   BELNR = IT_RSEG-BELNR1.

*      IF it_bseg2-mwskz NE ' '.
      IF SY-SUBRC EQ 0.
        IT_FINAL-HKONT = IT_BSEG2-HKONT.
      ENDIF.
*      ENDIF.

      READ TABLE IT_GLDESC WITH KEY SAKNR = IT_FINAL-HKONT.

      IF SY-SUBRC EQ 0.
        IT_FINAL-GL_DESC = IT_GLDESC-TXT50.
      ENDIF.

      READ TABLE IT_GLDESC1 WITH KEY SAKNR = IT_FINAL-HKONT.

      IF SY-SUBRC EQ 0.
        IT_FINAL-GL_DESC = IT_GLDESC1-TXT50.
      ENDIF.

      READ TABLE IT_T005U WITH KEY BLAND = IT_LFA1-REGIO
                                   LAND1 = IT_LFA1-LAND1.
      IF SY-SUBRC EQ 0.
        IT_FINAL-REGIO_DESC = IT_T005U-BEZEI.
      ENDIF.

      IT_FINAL-LAND1 = IT_LFA1-LAND1.
*********************************************************************************

*********************************************************************************


      CONCATENATE IT_FINAL-G_BELNR IT_FINAL-BUZEI INTO IT_FINAL-AWKEY1.
      CONCATENATE IT_FINAL-EBELN IT_FINAL-EBELP INTO IT_FINAL-AWKEY2.

      CONCATENATE IT_FINAL-BELNR IT_FINAL-BUZEI+3(3) INTO IT_FINAL-AWKEY3.
      READ TABLE IT_BSET WITH KEY BELNR = IT_BSET-BELNR.
*                                buzei = it_bset-buzei.
      IF SY-SUBRC EQ 0.
        IT_FINAL-BUZEI = IT_BSET-BUZEI.
      ENDIF.

      IT_FINAL-EXBAS = IT_FINAL-QTY * IT_FINAL-RATE.

      AT END OF TXGRP.
        IT_FINAL-GROSS = IT_FINAL-WRBTR                       + IT_FINAL-BNKAN"#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
                       + IT_FINAL-BED                         + IT_FINAL-ADDBED                             "line add 22.02.2009
                       + IT_FINAL-ECS                         + IT_FINAL-HCESS                              "line add 22.02.2009
                       + IT_FINAL-EBED                        + IT_FINAL-RMDED
                       + IT_FINAL-RMNON                       + IT_FINAL-CGDED
                       + IT_FINAL-CGNON                       + IT_FINAL-CST
                       + IT_FINAL-SERTAX                      + IT_FINAL-ESERTAX
                       + IT_FINAL-SHCESS                      + IT_FINAL-ACD
                       + IT_FINAL-BASCUS                      + IT_FINAL-CVD
                       + IT_FINAL-ECVD                        + IT_FINAL-HECVD
                       + IT_FINAL-ECUSTOM                     + IT_FINAL-HECUSTOM
                       + IT_FINAL-IMP_FREIGHT_VAL             - IT_FINAL-DISC
                       + IT_FINAL-BED_N                       + IT_FINAL-CES_N
                       + IT_FINAL-HCESS_N                     + IT_FINAL-AED_N
                       + IT_FINAL-FRGT.

        IF IT_FINAL-GROSS GT 0.
* Commented by Bhavesh
*IF it_final-qty <> 0.
          IT_FINAL-LND_CST = IT_FINAL-GROSS / IT_FINAL-QTY .
*          ENDIF.
        ENDIF.

*SHIFT it_final-buzei1 LEFT DELETING LEADING '0'.    "Added By Vivek 01032011
*        MODIFY it_final.
        APPEND IT_FINAL.

        CLEAR : IT_FINAL, IT_RSEG, IT_BSEG.
        CLEAR : IT_FINAL-AWKEY3,IT_FINAL-CGDED,IT_FINAL-RMDED,IT_FINAL-HKONT.",it_bset.

      ENDAT.
    ENDIF.
  ENDLOOP.
***-------------------------------------------------------** Lines added 28.09.2010  Start
  DELETE ADJACENT DUPLICATES FROM IT_FINAL COMPARING ALL FIELDS. "belnr. " Added by vivek 28022011

*  LOOP AT it_final into wa_final.
*
*
*
*      COLLECT wa_final INTO it_finaln.
*      MOVE-CORRESPONDING wa_final TO wa_finaln.
*      CLEAR wa_finaln.
*  ENDLOOP.
*CLEAR it_final.
*it_finaln[] = it_final[].
*  BREAK-POINT.
  SORT IT_FINAL BY BELNR.
  SORT IT_BSET  BY BELNR.
  LOOP AT IT_FINAL INTO WA_FINAL.

    LOOP AT IT_RSEG WHERE BELNR = WA_FINAL-G_BELNR
                      AND EBELN = WA_FINAL-EBELN
                      AND EBELP = WA_FINAL-EBELP.

*      IF wa_final-waers <> 'INR'.
*      IF it_rseg-mwskz = 'UI'.      " Commented by rameshwar

      IF IT_RSEG-KSCHL = 'J1CV'.
        WA_FINAL-HECVD = IT_RSEG-WRBTR * WA_FINAL-KURSF."#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
      ENDIF.

      IF IT_RSEG-KSCHL = 'JADC'.
        WA_FINAL-ACD = IT_RSEG-WRBTR * WA_FINAL-KURSF."#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
      ENDIF.

      IF IT_RSEG-KSCHL = 'JCDB'.
        WA_FINAL-BASCUS = IT_RSEG-WRBTR * WA_FINAL-KURSF."#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
      ENDIF.

      IF IT_RSEG-KSCHL = 'JCV1'.
        WA_FINAL-CVD = IT_RSEG-WRBTR * WA_FINAL-KURSF."#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
      ENDIF.

      IF IT_RSEG-KSCHL = 'JECV'.
        WA_FINAL-ECVD = IT_RSEG-WRBTR * WA_FINAL-KURSF."#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
      ENDIF.

      IF IT_RSEG-KSCHL = 'JEDB'.
        WA_FINAL-ECUSTOM = IT_RSEG-WRBTR * WA_FINAL-KURSF."#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
      ENDIF.

      IF IT_RSEG-KSCHL = 'JSED'."it_rseg-kschl = 'JSDB'.
        WA_FINAL-HECUSTOM = IT_RSEG-WRBTR * WA_FINAL-KURSF."#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
      ENDIF.
      IF IT_RSEG-KSCHL = 'FRB1'.
        WA_FINAL-FRGT = WA_FINAL-FRGT + IT_RSEG-WRBTR ."* wa_final-kursf.
      ENDIF.

*      ENDIF.
      MODIFY IT_FINAL FROM WA_FINAL.
    ENDLOOP.

  ENDLOOP.

*CLEAR wa_final-exbas.
  LOOP AT IT_FINAL INTO WA_FINAL.

    LOOP AT IT_BSET INTO WA_BSET WHERE BELNR = WA_FINAL-BELNR
                                   AND GJAHR = WA_FINAL-GJAHR
                                   AND EBELN = WA_FINAL-EBELN
                                   AND EBELP = WA_FINAL-EBELP.


*      SHIFT it_final-buzei1 LEFT DELETING LEADING '0'.    "Added By Vivek 01032011
*      SHIFT wa_bset-txgrp LEFT DELETING LEADING '0'.                   "Added By Vivek 01032011

      IF WA_BSET-KSCHL = 'JMOP'.
        IF SY-SUBRC = 0.
*          wa_final-exbas     = wa_final-exbas + wa_bset-hwbas.
          WA_FINAL-BED     = WA_FINAL-BED + WA_BSET-HWSTE.
*          WA_FINAL-BEDRATE = WA_BSET-KBETR / 10."Commented by SPLABAP during code remediation
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
*          WA_FINAL-SHRATE = WA_BSET-KBETR / 10."Commented by SPLABAP during code remediation
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

********************************************************************************
*      AT END OF txgrp.
*        it_final-gross = it_final-wrbtr                       + it_final-bnkan
*                       + it_final-bed                         + it_final-addbed                             "line add 22.02.2009
*                       + it_final-ecs                         + it_final-hcess                              "line add 22.02.2009
*                       + it_final-ebed                        + it_final-rmded
*                       + it_final-rmnon                       + it_final-cgded
*                       + it_final-cgnon                       + it_final-cst
*                       + it_final-sertax                      + it_final-esertax
*                       + it_final-shcess                      + it_final-acd
*                       + it_final-bascus                      + it_final-cvd
*                       + it_final-ecvd                        + it_final-hecvd
*                       + it_final-ecustom                     + it_final-hecustom
*                       + it_final-imp_freight_val             - it_final-disc
*                       + it_final-bed_n                       + it_final-ces_n
*                       + it_final-hcess_n                     + it_final-aed_n
*                       + it_final-frgt.
*
*        IF it_final-gross GT 0.
*          IF it_final-qty <> 0.
*            it_final-lnd_cst = it_final-gross / it_final-qty .
*          ENDIF.
*        ENDIF.
***************************************************************
*    ENDLOOP.
      MODIFY IT_FINAL FROM WA_FINAL.
    ENDLOOP.
    CLEAR : WA_FINAL,WA_BSET,IT_BSET-HWBAS.
    CLEAR : WA_FINAL-BED,WA_FINAL-BEDRATE,WA_FINAL-ADDBED,WA_FINAL-ECS,WA_FINAL-EBED,
            WA_FINAL-RMDED ,WA_FINAL-RMDEDRATE,WA_FINAL-VATBAS,WA_FINAL-RMNON,
            WA_FINAL-CGDED,WA_FINAL-CGDEDRATE,WA_FINAL-CGNON,WA_FINAL-CST,
            WA_FINAL-CSTRATE,WA_FINAL-SERTAX,WA_FINAL-SERBAS,WA_FINAL-ESERTAX,
            WA_FINAL-HCESS,WA_FINAL-SHCESS,WA_FINAL-SHRATE,WA_FINAL-BED_N,
            WA_FINAL-CES_N,WA_FINAL-HCESS_N,WA_FINAL-AED_N,WA_BSET..
  ENDLOOP.

*  DELETE ADJACENT DUPLICATES FROM it_final COMPARING ALL FIELDS. "belnr. " Added by vivek 28022011
  IT_FINAL_TMP[] = IT_FINAL[].
  SORT IT_FINAL BY BELNR EBELN EBELP.                     "line add 22.02.2009
  CLEAR : WA_FINAL, WA_FINAL1, WA_FINAL1TMP.
  LOOP AT IT_FINAL INTO WA_FINAL.      "where dmbtr ne 0.      "line chg 07.03.2009
    CLEAR : WA_FINAL_TMP, WA_FINAL1.
***Add by Vivek 17/09/2010
    IF SY-SUBRC = 0.

      READ TABLE IT_FINAL1 WITH KEY WA_FINAL-AWKEY1 INTO WA_FINAL1TMP.

      IF NOT SY-SUBRC = 0.                                                                                                           "line add 22.02.2009
        MOVE-CORRESPONDING WA_FINAL TO WA_FINAL1.
        CLEAR : WA_FINAL1-DMBTR,                      "added 03.03.2009
                WA_FINAL1-WRBTR,                        "added 03.03.2009
                WA_FINAL1-FRGT,WA_FINAL1-PCKN,WA_FINAL1-MISC,WA_FINAL1-BNKAN,
                WA_FINAL1-EXBAS,WA_FINAL1-VATBAS,WA_FINAL1-SERBAS,WA_FINAL1-BED,
                WA_FINAL1-ADDBED,                                "line add 22.02.2009
                WA_FINAL1-ECS,WA_FINAL1-HCESS,WA_FINAL1-EBED,WA_FINAL1-RMDED,
                WA_FINAL1-RMNON,WA_FINAL1-CGDED,WA_FINAL1-CGNON,WA_FINAL1-CST,
                WA_FINAL1-SERTAX,WA_FINAL1-ESERTAX,WA_FINAL1-SHCESS,WA_FINAL1-ACD,
                WA_FINAL1-BASCUS,WA_FINAL1-CVD,WA_FINAL1-ECVD,WA_FINAL1-HECVD,
                WA_FINAL1-ECUSTOM,WA_FINAL1-HECUSTOM,WA_FINAL1-IMP_FREIGHT_VAL,WA_FINAL1-DISC,
                WA_FINAL1-WCT,WA_FINAL1-TDS,WA_FINAL1-ESIC,WA_FINAL1-GROSS.

        LOOP AT IT_FINAL_TMP INTO WA_FINAL_TMP WHERE AWKEY1 = WA_FINAL-AWKEY1.

          CLEAR : WA_RSEG1, WA_RSEG2, WA_RSEG3.

          READ TABLE IT_RSEG WITH KEY BELNR = WA_FINAL_TMP-G_BELNR
                                      BUZEI = WA_FINAL_TMP-BUZEI
                                      EBELN = WA_FINAL_TMP-EBELN
                                      EBELP = WA_FINAL_TMP-EBELP
                                      KSCHL = 'FRA1' INTO WA_RSEG1.

          READ TABLE IT_RSEG WITH KEY BELNR = WA_FINAL_TMP-G_BELNR
                                      BUZEI = WA_FINAL_TMP-BUZEI
                                      EBELN = WA_FINAL_TMP-EBELN
                                      EBELP = WA_FINAL_TMP-EBELP
                                      KSCHL = 'FRB1' INTO WA_RSEG2.

          READ TABLE IT_RSEG WITH KEY BELNR = WA_FINAL_TMP-G_BELNR
                                      BUZEI = WA_FINAL_TMP-BUZEI
                                      EBELN = WA_FINAL_TMP-EBELN
                                      EBELP = WA_FINAL_TMP-EBELP
                                      KSCHL = 'FRC1' INTO WA_RSEG3.

          WA_FINAL1-DMBTR =  WA_RSEG1-WRBTR + WA_RSEG2-WRBTR + WA_RSEG3-WRBTR .
          WA_FINAL1-WRBTR =  WA_RSEG1-WRBTR + WA_RSEG2-WRBTR + WA_RSEG3-WRBTR .

          CLEAR : WA_RSEG1, WA_RSEG2, WA_RSEG3.
***                                                                    "added 25.02.2009  End
*          wa_final1-dmbtr    = wa_final1-dmbtr  + wa_final_tmp-dmbtr.    "line added 03.03.2009
*          wa_final1-wrbtr    = wa_final1-wrbtr  + wa_final_tmp-wrbtr.                         "line added 03.03.2009
          WA_FINAL1-DMBTR    =  WA_FINAL_TMP-DMBTR.    "line added 03.03.2009
          WA_FINAL1-WRBTR    =  WA_FINAL_TMP-WRBTR.    "line added 03.03.2009
          WA_FINAL1-PCKN     = WA_FINAL1-PCKN   + WA_FINAL_TMP-PCKN.
          WA_FINAL1-MISC     = WA_FINAL1-MISC   + WA_FINAL_TMP-MISC.
          WA_FINAL1-BNKAN    = WA_FINAL1-BNKAN  + WA_FINAL_TMP-BNKAN.
          WA_FINAL1-EXBAS    = WA_FINAL1-EXBAS  + WA_FINAL_TMP-EXBAS.
          WA_FINAL1-VATBAS   = WA_FINAL1-VATBAS + WA_FINAL_TMP-VATBAS.
          WA_FINAL1-SERBAS   = WA_FINAL1-SERBAS + WA_FINAL_TMP-SERBAS.
          WA_FINAL1-BED      = WA_FINAL1-BED    + WA_FINAL_TMP-BED.
          WA_FINAL1-ADDBED   = WA_FINAL1-ADDBED + WA_FINAL_TMP-ADDBED.  "line add 22.02.2009
          WA_FINAL1-ECS      = WA_FINAL1-ECS    + WA_FINAL_TMP-ECS.
          WA_FINAL1-HCESS    = WA_FINAL1-HCESS  + WA_FINAL_TMP-HCESS.
          WA_FINAL1-EBED     = WA_FINAL1-EBED   + WA_FINAL_TMP-EBED.
          WA_FINAL1-RMDED    = WA_FINAL1-RMDED  + WA_FINAL_TMP-RMDED.
          WA_FINAL1-RMNON    = WA_FINAL1-RMNON  + WA_FINAL_TMP-RMNON.
          WA_FINAL1-CGDED    = WA_FINAL1-CGDED  + WA_FINAL_TMP-CGDED.
          WA_FINAL1-CGNON    = WA_FINAL1-CGNON  + WA_FINAL_TMP-CGNON.
          WA_FINAL1-CST      = WA_FINAL1-CST    + WA_FINAL_TMP-CST .
          WA_FINAL1-SERTAX   = WA_FINAL1-SERTAX   + WA_FINAL_TMP-SERTAX.
          WA_FINAL1-ESERTAX  = WA_FINAL1-ESERTAX  + WA_FINAL_TMP-ESERTAX.
          WA_FINAL1-SHCESS   = WA_FINAL1-SHCESS   + WA_FINAL_TMP-SHCESS.
          WA_FINAL1-ACD      = WA_FINAL1-ACD      + WA_FINAL_TMP-ACD.
          WA_FINAL1-BASCUS   = WA_FINAL1-BASCUS   + WA_FINAL_TMP-BASCUS.
          WA_FINAL1-CVD      = WA_FINAL1-CVD      + WA_FINAL_TMP-CVD.
          WA_FINAL1-ECVD     = WA_FINAL1-ECVD     + WA_FINAL_TMP-ECVD.
          WA_FINAL1-HECVD    = WA_FINAL1-HECVD    + WA_FINAL_TMP-HECVD.
          WA_FINAL1-ECUSTOM  = WA_FINAL1-ECUSTOM  + WA_FINAL_TMP-ECUSTOM.
          WA_FINAL1-HECUSTOM = WA_FINAL1-HECUSTOM + WA_FINAL_TMP-HECUSTOM.
          WA_FINAL1-DISC     = WA_FINAL1-DISC     + WA_FINAL_TMP-DISC.
          WA_FINAL1-IMP_FREIGHT_VAL = WA_FINAL1-IMP_FREIGHT_VAL + WA_FINAL_TMP-IMP_FREIGHT_VAL.
          WA_FINAL1-WCT      = WA_FINAL1-WCT    + WA_FINAL_TMP-WCT.
          WA_FINAL1-TDS      = WA_FINAL1-TDS    + WA_FINAL_TMP-TDS.
          WA_FINAL1-ESIC     = WA_FINAL1-ESIC   + WA_FINAL_TMP-ESIC.
          WA_FINAL1-FRGT     = WA_FINAL-FRGT  ."  + wa_final_tmp-esic.

***Add by Vivek
* IF wa_final-waers <> 'INR'.
*          wa_final1-gross = "wa_final1-wrbtr                      +
*                           wa_final1-bnkan
*                         + wa_final1-bed                         + wa_final1-addbed                             "line add 22.02.2009
*                         + wa_final1-ecs                         + wa_final1-hcess                              "line add 22.02.2009
*                         + wa_final1-ebed                        + wa_final1-rmded
*                         + wa_final1-rmnon                       + wa_final1-cgded
*                         + wa_final1-cgnon                       + wa_final1-cst
*                         + wa_final1-sertax                      + wa_final1-esertax
*                         + wa_final1-shcess                      + wa_final1-acd
*                         + wa_final1-bascus                      + wa_final1-cvd
*                         + wa_final1-ecvd                        + wa_final1-hecvd
*                         + wa_final1-ecustom                     + wa_final1-hecustom
*                         + wa_final1-imp_freight_val             - wa_final1-disc
*                         + wa_final1-bed_n                       + wa_final1-ces_n
*                         + wa_final-hcess_n                      + wa_final1-aed_n
*                         + wa_final-frgt.
*endif.
* IF wa_final-waers = 'INR'.

          WA_FINAL1-GROSS = WA_FINAL1-WRBTR                      + WA_FINAL1-BNKAN"#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
                         + WA_FINAL1-BED                         + WA_FINAL1-ADDBED                             "line add 22.02.2009
                         + WA_FINAL1-ECS                         + WA_FINAL1-HCESS                              "line add 22.02.2009
                         + WA_FINAL1-EBED                        + WA_FINAL1-RMDED
                         + WA_FINAL1-RMNON                       + WA_FINAL1-CGDED
                         + WA_FINAL1-CGNON                       + WA_FINAL1-CST
                         + WA_FINAL1-SERTAX                      + WA_FINAL1-ESERTAX
                         + WA_FINAL1-SHCESS                      + WA_FINAL1-ACD
                         + WA_FINAL1-BASCUS                      + WA_FINAL1-CVD
                         + WA_FINAL1-ECVD                        + WA_FINAL1-HECVD
                         + WA_FINAL1-ECUSTOM                     + WA_FINAL1-HECUSTOM
                         + WA_FINAL1-IMP_FREIGHT_VAL             - WA_FINAL1-DISC
                         + WA_FINAL1-BED_N                       + WA_FINAL1-CES_N
                         + WA_FINAL-HCESS_N                      + WA_FINAL1-AED_N
                         + WA_FINAL-FRGT.
*endif.
*          IF wa_final1-lfbnr =  ' ' AND wa_final-qty <> ' '.
*            wa_final1-frgt = wa_final1-gross / wa_final-qty.
*          ENDIF.

          IF WA_FINAL1-GROSS GT 0.
            IF WA_FINAL1-QTY <> 0.
              WA_FINAL1-LND_CST = WA_FINAL1-GROSS / WA_FINAL1-QTY .
            ENDIF.
          ENDIF.

        ENDLOOP.

        APPEND WA_FINAL1 TO IT_FINAL1.
        CLEAR : WA_FINAL1.
      ENDIF.                                                           "line add 22.02.2009
    ENDIF.

  ENDLOOP.

*** Re calc of Vat Base Value.
  CLEAR : WA_FINAL1.
  CLEAR : WA_FINAL1.
*******************************************Comment By Vivek 31012011
* add by bhavesh

* End by bhavesh panchal
*******************************************Comment By Vivek 31012011
  SORT IT_FINAL1 BY BUKRS BELNR GJAHR G_BELNR BUZEI.
***-------------------------------------------------------** Lines added 19.02.2009  End

**--------------------------------------------------**         Start of Modif.     "03.03.2009 John
*  IF NOT chk1 = 'X'.
*    DELETE it_final1 WHERE belnr+0(1) = '9'.
*  ENDIF.
  SORT IT_FINAL1 BY BUKRS BELNR GJAHR G_BELNR BUZEI.
**--------------------------------------------------**         End  of Modif.      "03.03.2009 John

  SORT IT_FINAL BY BUKRS BELNR GJAHR G_BELNR BUZEI.
  DELETE IT_FINAL WHERE BELNR EQ SPACE.
*  DELETE it_final WHERE gsber NOT IN s_gsber.

  DELETE ADJACENT DUPLICATES FROM IT_FINAL COMPARING BELNR G_BELNR QTY XBLNR EBELN EBELP.
  DELETE ADJACENT DUPLICATES FROM IT_FINAL1 COMPARING BELNR G_BELNR QTY XBLNR EBELN EBELP.

  SORT IT_BSET1 BY BUKRS BELNR GJAHR.
  DELETE ADJACENT DUPLICATES FROM IT_BSET1 COMPARING BUKRS BELNR GJAHR.

  SORT IT_FINAL BY BUKRS BELNR GJAHR G_BELNR BUZEI.

  LOOP AT IT_FINAL1 INTO WA_FINAL1.

    IF WA_FINAL1-EXKBE = ' '.

      READ TABLE IT_BSEG WITH KEY BELNR = WA_FINAL1-BELNR
                                  EBELN = WA_FINAL1-EBELN
                                  EBELP = WA_FINAL1-EBELP
                                  KOART = 'K'.
      IF SY-SUBRC = 0.
        MOVE IT_BSEG-WRBTR TO WA_FINAL1-WRBTR.
        MOVE IT_BSEG-DMBTR TO WA_FINAL1-DMBTR.
        WA_FINAL1-GROSS = WA_FINAL1-DMBTR."#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
      ENDIF.
    ENDIF.
    READ TABLE IT_BSEG WITH KEY BELNR = WA_FINAL1-BELNR
                                KTOSL = 'KBS'
                                KOART = 'S'.
    IF SY-SUBRC = 0.
      MOVE IT_BSEG-WRBTR TO  WA_FINAL1-GL_AMT.
      MOVE IT_BSEG-HKONT TO WA_FINAL1-GL_ACNT.
    ENDIF.
    READ TABLE IT_BSEG WITH KEY BELNR = WA_FINAL1-BELNR
                                KTOSL = 'FRL'
                                KOART = 'S'.
    IF SY-SUBRC = 0.
      MOVE IT_BSEG-WRBTR TO  WA_FINAL1-GL_AMT.
      MOVE IT_BSEG-HKONT TO WA_FINAL1-GL_ACNT.
    ENDIF.
    READ TABLE IT_BSEG WITH KEY BELNR = WA_FINAL1-BELNR
                               EBELN = ' '
                               KTOSL = 'DIF'
                               KOART = 'S'.
    IF SY-SUBRC = 0.
      MOVE IT_BSEG-WRBTR TO  WA_FINAL1-DF_AMT.
    ENDIF.
    WA_FINAL1-GROSS = WA_FINAL1-GROSS - WA_FINAL1-GL_AMT.
    SELECT TXT50
      UP TO 1 ROWS FROM SKAT
      INTO WA_FINAL1-GL_TEXT
      WHERE SPRAS = 'E'
      AND SAKNR = WA_FINAL1-GL_ACNT ORDER BY PRIMARY KEY.
    ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation

    READ TABLE IT_MSEG WITH KEY MBLNR = WA_FINAL1-LFBNR.
    IF SY-SUBRC = 0.
      READ TABLE IT_BKPF WITH KEY AWKEY = IT_MSEG-AWKEY.
      IF SY-SUBRC = 0.
        READ TABLE IT_BSEG WITH KEY BELNR = IT_BKPF-BELNR
                             KTOSL = 'KBS'.
        IF SY-SUBRC = 0.
          WA_FINAL1-CON_GL_ACNT = IT_BSEG-HKONT.
        ELSE.
          READ TABLE IT_BSEG WITH KEY BELNR = IT_BKPF-BELNR
                              KTOSL = 'BSX'.
          IF SY-SUBRC = 0.
            WA_FINAL1-CON_GL_ACNT = IT_BSEG-HKONT.
          ELSE.
            READ TABLE IT_BSEG WITH KEY BELNR = IT_BKPF-BELNR
                            KTOSL = 'FRL'.
          ENDIF.

        ENDIF.
      ENDIF.
    ELSE.
      DATA:LV_MBLNR TYPE MBLNR,
           LV_MJAHR TYPE MJAHR,
           LV_AWKEY TYPE AWKEY.
*      READ TABLE it_mseg with key lfbnr = wa_final1-lfbnr.
      CLEAR:LV_MBLNR,LV_MJAHR,LV_AWKEY.
      SELECT MBLNR MJAHR
         UP TO 1 ROWS FROM MSEG INTO (LV_MBLNR,LV_MJAHR)
        WHERE LFBNR = WA_FINAL1-LFBNR ORDER BY PRIMARY KEY.
      ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation
      CONCATENATE LV_MBLNR LV_MJAHR INTO LV_AWKEY.
      IF SY-SUBRC = 0.
        READ TABLE IT_BKPF WITH KEY AWKEY = LV_AWKEY.
        IF SY-SUBRC = 0.
          READ TABLE IT_BSEG WITH KEY BELNR = IT_BKPF-BELNR
                               KTOSL = 'KBS'.
          IF SY-SUBRC = 0.
            WA_FINAL1-CON_GL_ACNT = IT_BSEG-HKONT.
          ELSE.
            READ TABLE IT_BSEG WITH KEY BELNR = IT_BKPF-BELNR
                                KTOSL = 'BSX'.
            IF SY-SUBRC = 0.
              WA_FINAL1-CON_GL_ACNT = IT_BSEG-HKONT.
            ELSE.
              READ TABLE IT_BSEG WITH KEY BELNR = IT_BKPF-BELNR
                             KTOSL = 'FRL'.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
      IF WA_FINAL1-CON_GL_ACNT IS INITIAL.
        READ TABLE IT_BSEG WITH KEY BELNR = IT_BKPF-BELNR
                                 KTOSL = 'BSX'.
        IF SY-SUBRC = 0.
          WA_FINAL1-CON_GL_ACNT = IT_BSEG-HKONT.
        ENDIF.
      ENDIF.
    ENDIF.
    IF NOT WA_FINAL1-CON_GL_ACNT IS INITIAL.
      SELECT TXT50
        UP TO 1 ROWS FROM SKAT
        INTO WA_FINAL1-CON_GL_TEXT
        WHERE SPRAS = 'E'
        AND SAKNR = WA_FINAL1-CON_GL_ACNT ORDER BY PRIMARY KEY.
      ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation
    ENDIF.
    SELECT KTEXT UP TO 1 ROWS FROM CEPCT
        INTO WA_FINAL1-PRCT_TEXT
       WHERE SPRAS = 'E'
        AND  PRCTR = WA_FINAL1-PRCTR ORDER BY PRIMARY KEY.
    ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation

    MODIFY IT_FINAL1 FROM WA_FINAL1 TRANSPORTING DMBTR WRBTR GROSS GL_AMT GL_ACNT DF_AMT GL_TEXT CON_GL_ACNT CON_GL_TEXT PRCT_TEXT.
    CLEAR :WA_FINAL1.
  ENDLOOP.
*
*    SELECT SINGLE ekgrp bsart REVNO
*      FROM ekko
*      INTO (wa_final1-ekgrp,wa_final1-bsart,wa_final1-REVNO)
*      WHERE ebeln = wa_final1-ebeln.
*
*    if wa_final1-bsart is not initial.
*      select single BATXT from T161T
*             into wa_final1-doc_typ_desc
*             where spras = 'E'
*             and   BSART = wa_final1-bsart.
*    endif.
**Commented by Rameshwar
***    SELECT SINGLE prctr
***      FROM mseg
***      INTO wa_final1-prctr
***      WHERE mblnr = wa_final1-lfbnr AND
***            gjahr = wa_final1-gjahr AND
***            bukrs = wa_final1-bukrs.
*
*
*    MODIFY it_final1 FROM wa_final1 TRANSPORTING ekgrp bsart prctr doc_typ_desc REVNO.
*    CLEAR wa_final1.
***********************************************************************
*

  PERFORM F_LISTHEADER.
  PERFORM F_FIELDCATALOG.
  PERFORM F_LAYOUT.
  PERFORM F_DISPLAYGRID.

ENDFORM. " process_data

*&---------------------------------------------------------------------*
*&      Form  f_fieldcatalog
*&---------------------------------------------------------------------*

FORM F_FIELDCATALOG .


  T_FIELDCATALOG-FIELDNAME = 'BELNR'.
  T_FIELDCATALOG-SELTEXT_L = 'Document Number'.
  T_FIELDCATALOG-OUTPUTLEN = '15'.
  T_FIELDCATALOG-KEY = 'X'.
  APPEND T_FIELDCATALOG.
  CLEAR T_FIELDCATALOG.

  T_FIELDCATALOG-FIELDNAME = 'BLDAT'.
  T_FIELDCATALOG-SELTEXT_L = 'Doc Date'.
  T_FIELDCATALOG-OUTPUTLEN = '10'.
  T_FIELDCATALOG-KEY = 'X'.
  APPEND T_FIELDCATALOG.
  CLEAR T_FIELDCATALOG.

  T_FIELDCATALOG-FIELDNAME = 'XBLNR'.
  T_FIELDCATALOG-SELTEXT_L = 'Vendor Inv No'.
  T_FIELDCATALOG-OUTPUTLEN = '10'.
  T_FIELDCATALOG-KEY = 'X'.
  APPEND T_FIELDCATALOG.
  CLEAR T_FIELDCATALOG.

  T_FIELDCATALOG-FIELDNAME = 'G_BELNR'.
  T_FIELDCATALOG-SELTEXT_L = 'Inv No'.
  T_FIELDCATALOG-KEY = 'X'.
  T_FIELDCATALOG-OUTPUTLEN = '10'.
  APPEND T_FIELDCATALOG.
  CLEAR T_FIELDCATALOG.


  T_FIELDCATALOG-FIELDNAME = 'POSTDAT'.
  T_FIELDCATALOG-SELTEXT_L = 'Posting Date'.
  T_FIELDCATALOG-OUTPUTLEN = '10'.
  APPEND T_FIELDCATALOG.
  CLEAR T_FIELDCATALOG.


*  T_FIELDCATALOG-FIELDNAME = 'BUZEI'.
*  T_FIELDCATALOG-SELTEXT_L = 'Invoice Line Item'.
*  T_FIELDCATALOG-OUTPUTLEN = '7'.
*  APPEND T_FIELDCATALOG.
*  CLEAR T_FIELDCATALOG.



  T_FIELDCATALOG-FIELDNAME = 'WERKS'.
  T_FIELDCATALOG-SELTEXT_L = 'Plant'.
  APPEND T_FIELDCATALOG.
  CLEAR T_FIELDCATALOG.





  T_FIELDCATALOG-FIELDNAME = 'NAME2'.
  T_FIELDCATALOG-SELTEXT_L = 'Plant Name'.
  APPEND  T_FIELDCATALOG.
  CLEAR  T_FIELDCATALOG.


  T_FIELDCATALOG-FIELDNAME = 'LIFNR'.
  T_FIELDCATALOG-SELTEXT_L = 'Vendor Code'.
  T_FIELDCATALOG-OUTPUTLEN = '10'.
  T_FIELDCATALOG-NO_ZERO = 'X'.
  APPEND T_FIELDCATALOG.
  CLEAR T_FIELDCATALOG.



  T_FIELDCATALOG-FIELDNAME = 'NAME1'.
  T_FIELDCATALOG-SELTEXT_L = 'Vendor Name'.
  T_FIELDCATALOG-OUTPUTLEN = '35'.
  APPEND T_FIELDCATALOG.
  CLEAR T_FIELDCATALOG.


*
*
*  T_FIELDCATALOG-FIELDNAME = 'MATNR'.
*  T_FIELDCATALOG-SELTEXT_L = 'Material'.
*  T_FIELDCATALOG-OUTPUTLEN = '18'.
*  T_FIELDCATALOG-NO_ZERO = 'X'.
*  APPEND T_FIELDCATALOG.
*  CLEAR T_FIELDCATALOG.
*
*  T_FIELDCATALOG-FIELDNAME = 'MAKTX'.
*  T_FIELDCATALOG-SELTEXT_L = 'Material Description'.
*  T_FIELDCATALOG-OUTPUTLEN = '40'.
*  APPEND T_FIELDCATALOG.
*  CLEAR T_FIELDCATALOG.


*
*
*  T_FIELDCATALOG-FIELDNAME = 'DMBTR'.
*  T_FIELDCATALOG-SELTEXT_L = 'Doc Currency Amount'.
*  T_FIELDCATALOG-OUTPUTLEN = '15'.
*  T_FIELDCATALOG-DO_SUM = 'X'.
*  APPEND T_FIELDCATALOG.
*  CLEAR T_FIELDCATALOG.
****-------------------------------------------------------** Lines added 18.02.2009  End
*
*  T_FIELDCATALOG-FIELDNAME = 'EXBAS'.
*  T_FIELDCATALOG-SELTEXT_L = 'Assessable Value'.
*  T_FIELDCATALOG-OUTPUTLEN = '10'.
*  T_FIELDCATALOG-DO_SUM = 'X'.
*  APPEND T_FIELDCATALOG.
*  CLEAR T_FIELDCATALOG.



    T_FIELDCATALOG-FIELDNAME = 'VATBAS'.
    T_FIELDCATALOG-SELTEXT_L = 'Purchase Value'.
    T_FIELDCATALOG-DO_SUM = 'X'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.


  IF RD_VAT = 'X'.


    T_FIELDCATALOG-FIELDNAME = 'RMDED'.
    T_FIELDCATALOG-SELTEXT_L = 'VAT Amount'.
    T_FIELDCATALOG-OUTPUTLEN = '15'.
    T_FIELDCATALOG-DO_SUM = 'X'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'RMDEDRATE'.
    T_FIELDCATALOG-SELTEXT_L = 'VAT Rate'.
    T_FIELDCATALOG-OUTPUTLEN = '15'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.



    T_FIELDCATALOG-FIELDNAME = 'CGDED'.
    T_FIELDCATALOG-SELTEXT_L = 'Add. Tax '.
    T_FIELDCATALOG-OUTPUTLEN = '15'.
    T_FIELDCATALOG-DO_SUM = 'X'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'J_1ILSTNO'.
    T_FIELDCATALOG-SELTEXT_L = 'TIN No.'.
    T_FIELDCATALOG-OUTPUTLEN = '10'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

  ELSEIF RD_CST = 'X'.

***-------------------------------------------------------** Lines added 03.03.2009  Start
    T_FIELDCATALOG-FIELDNAME = 'CGDEDRATE'.
    T_FIELDCATALOG-SELTEXT_L = 'Add.Tax Rate'.
    T_FIELDCATALOG-OUTPUTLEN = '15'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.


    T_FIELDCATALOG-FIELDNAME = 'CST'.
    T_FIELDCATALOG-SELTEXT_L = 'CST'.
    T_FIELDCATALOG-OUTPUTLEN = '15'.
    T_FIELDCATALOG-DO_SUM = 'X'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'CSTRATE'.
    T_FIELDCATALOG-SELTEXT_L = 'CST Rate'.
    T_FIELDCATALOG-OUTPUTLEN = '8'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'CSTNO'.
    T_FIELDCATALOG-SELTEXT_L = 'CST No.'.
    T_FIELDCATALOG-OUTPUTLEN = '10'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.
  ENDIF.



  T_FIELDCATALOG-FIELDNAME = 'GROSS'.
  T_FIELDCATALOG-SELTEXT_L = 'Gross Total'.
  T_FIELDCATALOG-OUTPUTLEN = '15'.
  T_FIELDCATALOG-DO_SUM = 'X'.
  APPEND T_FIELDCATALOG.
  CLEAR T_FIELDCATALOG.





**  t_fieldcatalog-fieldname = 'GSBER'.
**  t_fieldcatalog-seltext_l = 'Business Area'.
*** t_fieldcatalog-outputlen = '7'.
**  APPEND t_fieldcatalog.
**  CLEAR t_fieldcatalog.





ENDFORM. " f_fieldcatalog

*&---------------------------------------------------------------------*
*&      Form  f_displaygrid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

FORM F_DISPLAYGRID .

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM     = SY-REPID
      I_CALLBACK_TOP_OF_PAGE = 'TOP_OF_PAGE'
      IS_LAYOUT              = FS_LAYOUT
      IT_FIELDCAT            = T_FIELDCATALOG[]
      IT_SORT                = GT_SORT[]
      IT_EVENTS              = T_EVENT[]
      I_SAVE                 = 'A'
    TABLES
      T_OUTTAB               = IT_FINAL1
    EXCEPTIONS
      PROGRAM_ERROR          = 1
      OTHERS                 = 2.
  IF SY-SUBRC <> 0.

* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.

  ENDIF.


ENDFORM. " f_displaygrid

*&---------------------------------------------------------------------*
*&      Form  f_listheader
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

FORM F_LISTHEADER .

  DATA : FRMDT(10) TYPE C,
          TODT(10) TYPE C,
          V_STR    TYPE STRING.


  T_LISTHEADER-TYP = 'H'.
  T_LISTHEADER-KEY = ' '.
  T_LISTHEADER-INFO = 'Purchase Register'.
  APPEND T_LISTHEADER.

  IF S_BUDAT-LOW NE '00000000' AND  S_BUDAT-HIGH NE '00000000'      .
    WRITE S_BUDAT-LOW  TO FRMDT USING EDIT MASK '__.__.____'.
    WRITE S_BUDAT-HIGH TO TODT  USING EDIT MASK '__.__.____'.
    CONCATENATE 'FROM'
                 FRMDT
                 'TO'
                 TODT
           INTO V_STR SEPARATED BY SPACE.
  ELSEIF S_BUDAT-LOW NE '00000000' AND  S_BUDAT-HIGH EQ '00000000'      .
    WRITE S_BUDAT-LOW  TO FRMDT USING EDIT MASK '__.__.____'.
    CONCATENATE 'ON'
                 FRMDT
           INTO V_STR SEPARATED BY SPACE.
  ELSEIF S_BUDAT-LOW EQ '00000000' AND  S_BUDAT-HIGH NE '00000000'      .
    WRITE S_BUDAT-HIGH  TO TODT USING EDIT MASK '__.__.____'.
    CONCATENATE 'ON'
                 TODT
           INTO V_STR SEPARATED BY SPACE.
  ENDIF.
  T_LISTHEADER-TYP = 'S'.
  T_LISTHEADER-KEY = ' '.
  T_LISTHEADER-INFO = V_STR.
  APPEND T_LISTHEADER.


ENDFORM. " f_listheader


*&---------------------------------------------------------------------*
*&      Form  f_layout
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

FORM F_LAYOUT .

  T_EVENT-NAME = SLIS_EV_TOP_OF_PAGE.
  T_EVENT-FORM = 'TOP_OF_PAGE'.
  APPEND T_EVENT.
  FS_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
ENDFORM. " f_layout


*&---------------------------------------------------------------------*
*&      Form  top_of_page
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM TOP_OF_PAGE.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = T_LISTHEADER[].

ENDFORM. "top_of_page

*&---------------------------------------------------------------------*
*&      Form  f_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

FORM F_FIELDCAT .

  T_FIELDCAT-FIELDNAME = 'BUKRS'.
  T_FIELDCAT-SELTEXT_L = 'C Code'.
  T_FIELDCAT-OUTPUTLEN = '10'.
  APPEND T_FIELDCAT.
  CLEAR T_FIELDCAT.

  T_FIELDCAT-FIELDNAME = 'BELNR'.
  T_FIELDCAT-SELTEXT_L = 'Document No'.
  T_FIELDCAT-OUTPUTLEN = '10'.
  APPEND T_FIELDCAT.
  CLEAR T_FIELDCAT.

  T_FIELDCAT-FIELDNAME = 'GJAHR'.
  T_FIELDCAT-SELTEXT_L = 'Year'.
  T_FIELDCAT-OUTPUTLEN = '4'.
  APPEND T_FIELDCAT.
  CLEAR T_FIELDCAT.


ENDFORM. " f_fieldcat

*&---------------------------------------------------------------------*
*&      Form  f_dispgrid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

FORM F_DISPGRID.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM = SY-REPID
      IT_FIELDCAT        = T_FIELDCAT[]
      I_SAVE             = 'A'
    TABLES
      T_OUTTAB           = IT_BSET1
    EXCEPTIONS
      PROGRAM_ERROR      = 1
      OTHERS             = 2.
  IF SY-SUBRC <> 0.

* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.

  ENDIF.

ENDFORM. " f_dispgrid
