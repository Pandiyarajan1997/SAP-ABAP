
REPORT ZKP_MM_PURCHASE_REGISTER_NEW .

*&----------------------------------------------------------------------------*
*& Report : ZKP_MM_PURCHASE_REGISTER_NEW
*&----------------------------------------------------------------------------*
*& Title:  PURCHASE REGISTER
*&----------------------------------------------------------------------------*
*& Report Author         : JESTOP JESWIN (ABAP Consultant)
*& Functional Consultant :
*& Company               : Sphinax Info Systems
*& Report Creation Date  : 07/10/2020
*& Transaction Code      : ZPUR_REG
*& Request Number        : DEVK921369
*&----------------------------------------------------------------------------*



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
         T005U,
         MARA,
         T023T.

TYPES : BEGIN OF TY_BKPF,
          BUKRS   TYPE BKPF-BUKRS,
          BELNR   TYPE BKPF-BELNR,
          GJAHR   TYPE BKPF-GJAHR,
          BLART   TYPE BKPF-BLART,
          BLDAT   TYPE BKPF-BLDAT,
          BUDAT   TYPE BKPF-BUDAT,
          XBLNR   TYPE BKPF-XBLNR,
          WAERS   TYPE BKPF-WAERS,
          KURSF   TYPE BKPF-KURSF,
          AWKEY   TYPE BKPF-AWKEY,
          G_BELNR TYPE RSEG-BELNR, "GR Inv No
          G_GJAHR TYPE RSEG-GJAHR, "GR Inv Year
          TCODE   TYPE BKPF-TCODE,
        END OF TY_BKPF.

DATA: WA_BKPF TYPE TY_BKPF,
      IT_BKPF TYPE TABLE OF TY_BKPF.



TYPES : BEGIN OF TY_RSEG,
            BELNR1    TYPE BKPF-BELNR,
            BELNR     TYPE RSEG-BELNR,
            GJAHR     TYPE RSEG-GJAHR,
            BUZEI     TYPE RSEG-BUZEI,
            EBELN     TYPE RSEG-EBELN,
            EBELP(6)  TYPE N,                                           "TYPE rseg-ebelp,
            MATNR     TYPE RSEG-MATNR,
            BUKRS     TYPE RSEG-BUKRS,
            SHKZG     TYPE RSEG-SHKZG,
            WRBTR     TYPE RSEG-WRBTR,
            MWSKZ     TYPE RSEG-MWSKZ,
            BKLAS     TYPE RSEG-BKLAS,
            BNKAN     TYPE RSEG-BNKAN,
            KSCHL     TYPE RSEG-KSCHL,                                   "Condition type in RSEG
            LFBNR     TYPE RSEG-LFBNR,                                   "Reference Document
            LFPOS     TYPE RSEG-LFPOS,                                   "Reference Documenet item
            LFGJA     TYPE RSEG-LFGJA,
            WERKS     TYPE RSEG-WERKS,
            FRBNR     TYPE RSEG-FRBNR,                                    "Number of Bill of Lading at Time of Goods Receipt
            LIFNR     TYPE RBKP-LIFNR,
            BLART     TYPE RBKP-BLART,
            NAME1     TYPE LFA1-NAME1,
            NAME3     TYPE LFA1-NAME3,
            REGIO     TYPE LFA1-REGIO,
            ORT01     TYPE LFA1-ORT01,
            LAND1     TYPE LFA1-LAND1,                                   "lifnr name1 regio ort01 land1
            MENGE     TYPE RSEG-MENGE,
            MEINS     TYPE RSEG-MEINS,
            BUKRS1    TYPE BKPF-BUKRS,
            BUDAT     TYPE BKPF-BUDAT,
            GJAHR1    TYPE BKPF-GJAHR,
            GSBER     TYPE BSEG-GSBER,
            COUNT1(4) TYPE N,
            MTART     TYPE MARA-MTART,
            MATKL     TYPE MARA-MATKL,
            TXGRP     TYPE BSET-TXGRP,
            AWKEY     TYPE BKPF-AWKEY,
            ADD       TYPE RSEG-BELNR,
            STUNR     TYPE RSEG-STUNR,
            EXKBE     TYPE RSEG-EXKBE,
            BSART     TYPE EKKO-BSART,
       END OF TY_RSEG.

DATA: WA_RSEG TYPE TY_RSEG,
     IT_RSEG TYPE TABLE OF TY_RSEG,

     WA_RSEG01 TYPE TY_RSEG,
     IT_RSEG01 TYPE TABLE OF TY_RSEG.

TYPES : BEGIN OF TY_BASE_UNIT,
       BELNR  TYPE RSEG-BELNR,
       EBELN  TYPE RSEG-EBELN,
       EBELP  TYPE RSEG-EBELP,
       MEINS  TYPE RSEG-MEINS,
       BSTME  TYPE RSEG-BSTME,
       BELNR2 TYPE RSEG-BELNR,
       END OF TY_BASE_UNIT.

DATA: WA_BASE_UNIT TYPE TY_BASE_UNIT,
      IT_BASE_UNIT TYPE TABLE OF TY_BASE_UNIT.

TYPES: BEGIN OF TY_RBKP,
         BELNR  TYPE RBKP-BELNR,
         GJAHR  TYPE RBKP-GJAHR,
         LIFNR  TYPE RBKP-LIFNR,
         BLART  TYPE RBKP-BLART,
       END OF TY_RBKP.

DATA: WA_RBKP TYPE TY_RBKP,
      IT_RBKP TYPE TABLE OF TY_RBKP.

TYPES : BEGIN OF TY_LFA1,
          LIFNR  TYPE LFA1-LIFNR,
          NAME1  TYPE LFA1-NAME1,
          NAME3  TYPE LFA1-NAME3,
          REGIO  TYPE LFA1-REGIO,
          ORT01  TYPE LFA1-ORT01,
          LAND1  TYPE LFA1-LAND1,
       END OF TY_LFA1.

DATA: WA_LFA1 TYPE TY_LFA1,
     IT_LFA1 TYPE TABLE OF TY_LFA1.

TYPES : BEGIN OF TY_J_1IEXCDTL,
          DOCNO  TYPE J_1IDOCNO,
          EXBAS  TYPE J_1IEXCBAS,
          RDOC1  TYPE J_1IRDOC1,
          RITEM1 TYPE J_1IRITEM1,
          RDOC2  TYPE J_1IRDOC2,
        END OF TY_J_1IEXCDTL.

DATA: WA_J_1IEXCDTL TYPE TY_J_1IEXCDTL,
     IT_J_1IEXCDTL TYPE TABLE OF TY_J_1IEXCDTL.

TYPES : BEGIN OF TY_MKPF,
          MBLNR TYPE MBLNR,
          BUDAT TYPE BUDAT,
          MJAHR TYPE MJAHR,
          AWKEY TYPE BKPF-AWKEY,
        END OF TY_MKPF.

DATA: WA_MKPF TYPE TY_MKPF,
      IT_MKPF TYPE TABLE OF TY_MKPF.

DATA : L_LFPOS(4) TYPE C.

TYPES: BEGIN OF TY_BSET,
          COUNT     TYPE SY-TABIX,
          BUKRS     TYPE BSET-BUKRS,
          BELNR     TYPE BSET-BELNR,
          GJAHR     TYPE BSET-GJAHR,
          TXGRP(4)  TYPE N, " like bset-txgrp, "GR Invoice item
          SHKZG     TYPE BSET-SHKZG, "Debit/Credit Indicator
          MWSKZ     TYPE BSET-MWSKZ, "Tax Code
          HWBAS     TYPE BSET-HWBAS, "Tax Base amount in local currency
          HWSTE     TYPE BSET-HWSTE, "Tax Amount in local currency
          KTOSL     TYPE BSET-KTOSL, "Transaction key
          KSCHL     TYPE BSET-KSCHL, "Condition Type
          KBETR     TYPE BSET-KBETR, "Tax Rate
          EBELN     TYPE EBELN,
          EBELP     TYPE EBELP,
          AWKEY     TYPE BKPF-AWKEY,
          BUZEI     TYPE BSET-BUZEI,
          HKONT     TYPE BSET-HKONT,
          KNUMH     TYPE BSET-KNUMH,
          LFBNR     TYPE MSEG-LFBNR,
          STUNR     TYPE RSEG-STUNR,
          SER_TAX   TYPE BSET-HWSTE,
          E_SER_TAX TYPE BSET-HWSTE,
          S_SER_TAX TYPE BSET-HWSTE,
       END OF TY_BSET.

DATA: WA_BSET TYPE TY_BSET,
      IT_BSET TYPE TABLE OF TY_BSET.

TYPES : BEGIN OF TY_MAKT,
       MATNR TYPE MAKT-MATNR,
       MAKTX TYPE MAKT-MAKTX,
       END OF TY_MAKT.

DATA: WA_MAKT TYPE TY_MAKT,
      IT_MAKT TYPE TABLE OF TY_MAKT.


TYPES : BEGIN OF TY_J_1IMOVEND,
          LIFNR     TYPE LIFNR,                "Vendor
          J_1ILSTNO TYPE J_1ILSTNO,            "LST No (TIN No)
          J_1IPANNO TYPE J_1IPANNO,            "PAN No
          J_1ISERN  TYPE J_1ISERN,             "Service Tax Registration Number
          J_1ICSTNO TYPE J_1ICSTNO,            "CST No
          J_1IEXCD  TYPE J_1IEXCD,
       END OF TY_J_1IMOVEND.

DATA: WA_J_1IMOVEND TYPE TY_J_1IMOVEND,
      IT_J_1IMOVEND TYPE TABLE OF TY_J_1IMOVEND.



TYPES : BEGIN OF TY_BSEG,

            BUKRS TYPE BSEG-BUKRS,
           BELNR TYPE BSEG-BELNR,
           GJAHR TYPE BSEG-GJAHR,
           BUZID TYPE BSEG-BUZID,
           BUZEI TYPE BSEG-BUZEI,
           SHKZG TYPE BSEG-SHKZG,
           DMBTR TYPE BSEG-DMBTR,
           WRBTR TYPE BSEG-WRBTR,
           HKONT TYPE BSEG-HKONT,
           FWBAS TYPE BSEG-FWBAS,
           EBELN TYPE BSEG-EBELN,
           EBELP TYPE BSEG-EBELP,
           GSBER TYPE BSEG-GSBER,
           KOART TYPE BSEG-KOART,
           KTOSL TYPE BSEG-KTOSL,
           MWSKZ TYPE BSEG-MWSKZ,
           TXGRP TYPE BSEG-TXGRP,
           XREF3 TYPE BSEG-XREF3,
       END OF TY_BSEG.

DATA: WA_BSEG TYPE TY_BSEG,
      IT_BSEG TYPE TABLE OF TY_BSEG,

      WA_BSEG01 TYPE TY_BSEG,
      IT_BSEG01 TYPE TABLE OF TY_BSEG.





DATA : IT_BSEG1 TYPE TABLE OF TY_BSEG,
       WA_BSEG2 TYPE TY_BSEG,
       IT_BSEG2 TYPE TABLE OF TY_BSEG.

TYPES : BEGIN OF TY_T025T,
         BKLAS TYPE BKLAS,
         BKBEZ TYPE BKBEZ,
        END OF TY_T025T.

DATA: WA_T025T TYPE TY_T025T,
      IT_T025T TYPE TABLE OF TY_T025T.

TYPES : BEGIN OF TY_EKPO,
            EBELN TYPE EKPO-EBELN,
            EBELP TYPE EKPO-EBELP,
            MATNR TYPE EKPO-MATNR,
            WERKS TYPE EKPO-WERKS,
            MENGE TYPE EKPO-MENGE,
            NETPR TYPE EKPO-NETPR,
            NETWR TYPE EKPO-NETWR,
            MWSKZ TYPE EKPO-MWSKZ,
         END OF TY_EKPO.

DATA: WA_EKPO TYPE TY_EKPO,
      IT_EKPO TYPE TABLE OF TY_EKPO.

TYPES: BEGIN OF TY_T005U,
        LAND1 TYPE T005U-LAND1,
        BLAND TYPE T005U-BLAND,
        BEZEI TYPE T005U-BEZEI,
      END OF TY_T005U.


DATA: WA_T005U TYPE TY_T005U,
      IT_T005U TYPE TABLE OF TY_T005U.

TYPES : BEGIN OF TY_FINAL,
          BLDAT           TYPE BKPF-BLDAT,         "Doc Date
          POSTDAT         TYPE BKPF-BUDAT,         "Posting Date
          BUKRS           TYPE BKPF-BUKRS,         "Company Code
          BELNR           TYPE BKPF-BELNR,         "Document No
          BLART           TYPE BKPF-BLART,         "Document Type
          DOC_TYPE(20)    TYPE C,
          WAERS           TYPE BKPF-WAERS,         "Currency
          KURSF           TYPE BKPF-KURSF,         "Exchange Rate
          GJAHR           TYPE BKPF-GJAHR,         "Fiscal Year
          LIFNR           TYPE LFA1-LIFNR,         "Vendor
          NAME1           TYPE LFA1-NAME1,         "Vendor Name
          NAME3           TYPE LFA1-NAME3,         "Vendor Name3
          XBLNR           TYPE BKPF-XBLNR,         "Vendor Invoice No
          G_BELNR         TYPE RSEG-BELNR,         "MIRO Invoice No
          BUZEI           TYPE RSEG-BUZEI,         "MIRO Invoice item
          BUZEI1          TYPE RSEG-BUZEI,
          LFBNR           TYPE RSEG-LFBNR,         "Grr No
          FRBNR           TYPE RSEG-FRBNR,              "   Number of Bill of Lading at Time of Goods Receipt
          BUDAT           TYPE BUDAT,              "GRR Date
          BKLAS           TYPE RSEG-BKLAS,         "Valuation Class
          BKBEZ           TYPE BKBEZ,              "Valuation Class Description
          BNKAN           TYPE RSEG-BNKAN,         "Unplanned cost in Doc Curr
          BNKANLOC        TYPE RSEG-BNKAN,         "Unplanned cost in Local Curr
          EXBAS           TYPE J_1IEXCBAS,         "Assessible Value
          HKONT           TYPE BSEG-HKONT,         "GL A/C
          MWSKZ           TYPE BSET-MWSKZ,         "Tax Code
          MATNR           TYPE RSEG-MATNR,         "Material
          MAKTX           TYPE MAKT-MAKTX,         "Material Description
          EBELN           TYPE RSEG-EBELN,         "PO
          EBELP           TYPE RSEG-EBELP,         "PO item
          DMBTR           TYPE BSEG-DMBTR,         "Amount
          WRBTR           TYPE BSEG-WRBTR,         "Document Currency Amount
          FRGT            TYPE BSEG-WRBTR,         "Freight
          PCKN            TYPE BSEG-WRBTR,         "Packing
          MISC            TYPE BSEG-WRBTR,         "Misc.Chgs.
          VATBAS          TYPE HWBAS,              "Tax Base Amount in Document Currency for VAT
          SERBAS          TYPE HWBAS,              "Tax Base Amount in Document Currency for Service Tax
          BED             TYPE KWERT,              "BED
          BEDRATE         TYPE KWERT,              "BED Rate
          ADDBED          TYPE KWERT,                          "Addl Bed
          ECS             TYPE KWERT,              "Ecess Setoff
          EBED            TYPE KWERT,              "Ecess on BED
          RMDED           TYPE KWERT,              "RM Deductable
          RMDEDRATE       TYPE KWERT,              "VAT rate (RM Deductible)
          RMNON           TYPE KWERT,              "RM Non-Deductable
          CGDED           TYPE KWERT,              "CG Deductable
          CGDEDRATE       TYPE KWERT,              "ADD.VAT rate (CG Deductible)
          CGNON           TYPE KWERT,              "CG Non-Deductable
          CST             TYPE KWERT,              "CST
          CSTRATE         TYPE KWERT,              "CST Rate
          SERTAX          TYPE KWERT,              "Service Tax
          ESERTAX         TYPE KWERT,              "ECess on Service Tax
          SHCESS          TYPE KWERT,              "SHCess
          SHRATE          TYPE KWERT,              "SHCess Rate
          ACD             TYPE WRBTR,              "Additional Customs Duty
          BASCUS          TYPE WRBTR,              "Basic Customs
          CVD             TYPE WRBTR,              "CVD
          ECVD            TYPE WRBTR,              "ECess on CVD
          HECVD           TYPE WRBTR,              "Hecess on CVD
          ECUSTOM         TYPE WRBTR,              "Customs Ecess
          HECUSTOM        TYPE WRBTR,              "Customs HEcess
          WCT             TYPE DMBTR,              "W.C.T
          TDS             TYPE DMBTR,              "T.D.S
          ESIC            TYPE DMBTR,              "E.S.I.C
          GROSS           TYPE KWERT,              "Gross Total
          J_1ILSTNO       TYPE J_1ILSTNO,          "LST No (TIN No)
          J_1IPANNO       TYPE J_1IPANNO,          "PAN No
          J_1ISERN        TYPE J_1ISERN,           "Service Tax Registration Number
          DISC            TYPE KWERT,              "Discount
          HCESS           TYPE KWERT,              "SHCess on BED
          TAX_DESC        TYPE TEXT1_007S,         "Tax Code Desc
          GSBER           TYPE BSEG-GSBER,         " Business Area
          CSTNO           TYPE J_1ICSTNO,          " CST No
          REGIO           TYPE LFA1-REGIO,         " Region
          ORT01           TYPE LFA1-ORT01,         " City
          CHAPID          TYPE J_1IMTCHID-J_1ICHID,    " Chapter ID
          CHAPID_DESC     TYPE J_1ICHIDTX-J_1ICHT1, " Chapter ID Desc
          GL_DESC         TYPE SKAT-TXT50,           " G/L Account Description
          GL_DESC_TXT20   TYPE SKAT-TXT20,           " G/L TEXT20 Description
          ECCNO           TYPE J_1IMOVEND-J_1IEXCD,    "#EC CI_USAGE_OK[2877717] " ECC NoAdded by <IT-CAR Tool> during Code Remediation
          REGIO_DESC      TYPE T005U-BEZEI,
          WERKS           TYPE RSEG-WERKS,
          IMP_FREIGHT_VAL TYPE RSEG-WRBTR, " Import Freight Value
          QTY             TYPE MSEG-MENGE,          " Quantity
          BED_N           TYPE KWERT,               " Bed Non Deductable
          CES_N           TYPE KWERT,               " Cesss Non Deductable
          HCESS_N         TYPE KWERT,               " H cesss Non Deductable
          AED_N           TYPE KWERT,               " AED Non Deductable
          LND_CST         TYPE KBETR,               " Landing cost
          LAND1           TYPE LFA1-LAND1,          " Vendor Country code
          MATKL           TYPE MARA-MATKL,          " material group
          MTART           TYPE MARA-MTART,          " Material Type
          MEINS           TYPE RSEG-MEINS,          " Base Unit
          EKGRP           TYPE EKKO-EKGRP,          " Prchase Group
          NAME2           TYPE T001W-NAME1,         " plant name
          RATE            TYPE KWERT,               " rate
          WGBEZ           TYPE T023T-WGBEZ,         " Material Group text
          MENGE           TYPE RSEG-MENGE,
          SETTLED         TYPE RSEG-MENGE,          " Settled Qty
          SHKZG           TYPE BSET-SHKZG,
          AWKEY1          TYPE BKPF-AWKEY,          " Concatenate IN it_final (it_final-g_belnr + it_final-buzei)
          AWKEY2          TYPE BKPF-AWKEY,          " Concatenate IN it_final (it_final-g_ebeln + it_final-ebelp)
          AWKEY3          TYPE BKPF-AWKEY,          " Concatenate IN it_final (it_final-belnr + it_final-ktosl)
          HKONT1          TYPE BSET-HKONT,
          KNUMH           TYPE BSET-KNUMH,
          AWKEY4          TYPE BKPF-AWKEY,
          TXGRP           TYPE BSET-TXGRP,
          BSTME           TYPE RSEG-BSTME,
          TCODE           TYPE BKPF-TCODE,
          KSCHL           TYPE RSEG-KSCHL,
          PRCTR           TYPE MARC-PRCTR,          " Profit Center
          BSART           TYPE EKKO-BSART,          " Purchasing Document Type
          BEDAT           TYPE EKKO-BEDAT,          " PO Date Added by savariar s as on 20/01/2015.
          DOC_TYP_DESC    TYPE TEXT30,
          REVNO           TYPE REVNO ,
          EXKBE           TYPE RSEG-EXKBE,
          KOART           TYPE BSEG-KOART,
          GL_ACNT         TYPE BSEG-HKONT,
          GL_AMT          TYPE BSEG-DMBTR,
          DF_AMT          TYPE BSEG-DMBTR,
          ANLN1           TYPE MSEG-ANLN1,
          TXT50           TYPE ANLA-TXT50,
          GL_TEXT         TYPE SKAT-TXT50,
          GL_TEXT20       TYPE SKAT-TXT20,
          CON_GL_ACNT     TYPE BSEG-HKONT,
          CON_GL_TEXT     TYPE SKAT-TXT50,
          CON_GL_TEXT20   TYPE SKAT-TXT20,
          VAL_TEXT        TYPE CHAR50,
          PRCT_TEXT       TYPE CHAR50,
          CC1             TYPE  CHAR50,
          CATE            TYPE CHAR50,
          PUR_VAL         TYPE P DECIMALS 2,
          VAT_VAL         TYPE P DECIMALS 2,
          CST_VAL         TYPE P DECIMALS 2,
          PUR_VAL1        TYPE P DECIMALS 2,
          SGST            TYPE WRBTR,
          IGST            TYPE WRBTR,
          UGST            TYPE WRBTR,
          CGST            TYPE WRBTR,
          JCOS            TYPE WRBTR,
          SGSTR           TYPE KBETR,
          IGSTR           TYPE KBETR,
          UGSTR           TYPE KBETR,
          CGSTR           TYPE KBETR,
          JCOSR           TYPE KBETR,


END OF TY_FINAL.

DATA: WA_FINAL TYPE TY_FINAL,
      IT_FINAL TYPE TABLE OF TY_FINAL.

TYPES :BEGIN OF TY_EKKO,
         EBELN      TYPE EKKO-EBELN,
         EKGRP      TYPE EKKO-EKGRP,
         BSART      TYPE EKKO-BSART,          " Purchasing Document Type
         BEDAT      TYPE EKKO-BEDAT,          " PO Date.
         DOC_TYP_DESC TYPE TEXT30,
         REVNO      TYPE REVNO ,
       END OF TY_EKKO.

DATA: WA_EKKO TYPE TY_EKKO,
      IT_EKKO TYPE TABLE OF TY_EKKO.


TYPES : BEGIN OF TY_MARA,
        MATNR TYPE MARA-MATNR,
        MTART TYPE MARA-MTART,
        MATKL TYPE MARA-MATKL,
      END OF TY_MARA.


DATA : WA_MARA         TYPE  TY_MARA,
       IT_MARA         TYPE TABLE OF TY_MARA.

TYPES: BEGIN OF TY_T023T,
        MATKL TYPE T023T-MATKL,
        WGBEZ TYPE T023T-WGBEZ,
      END OF TY_T023T.


DATA:  WA_T023T        TYPE  TY_T023T,
       IT_T023T        TYPE TABLE OF TY_T023T.

DATA:  T_LISTHEADER    TYPE SLIS_T_LISTHEADER WITH HEADER LINE,
       T_FIELDCATALOG  TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE,
       T_EVENT         TYPE SLIS_T_EVENT WITH HEADER LINE,
       FS_LAYOUT       TYPE SLIS_LAYOUT_ALV,
       GT_SORT         TYPE SLIS_T_SORTINFO_ALV WITH HEADER LINE.

TYPES : BEGIN OF TY_BSET1,
           BUKRS TYPE BSET-BUKRS,
           BELNR TYPE BSET-BELNR,
           GJAHR TYPE BSET-GJAHR,
        END OF TY_BSET1.

DATA: WA_BSET1 TYPE TY_BSET1,
      IT_BSET1 TYPE TABLE OF TY_BSET1.

TYPES : BEGIN OF TY_RSEG1,
            EBELN  TYPE RSEG-EBELN,
            EBELP  TYPE RSEG-EBELP,
            BELNR  TYPE RSEG-BELNR,
            GJAHR  TYPE RSEG-GJAHR,
            BUZEI  TYPE RSEG-BUZEI,
            MATNR  TYPE RSEG-MATNR,
            BUKRS  TYPE RSEG-BUKRS,
            WRBTR  TYPE RSEG-WRBTR,
            SHKZG  TYPE RSEG-SHKZG,
            MWSKZ  TYPE RSEG-MWSKZ,
            KSCHL  TYPE RSEG-KSCHL,                                  "Condition type in RSEG
            LFPOS  TYPE RSEG-LFPOS,                                  "Reference Documenet item
            LIFNR  TYPE RBKP-LIFNR,
            NAME1  TYPE LFA1-NAME1,
            NAME3  TYPE LFA1-NAME3,
            BUKRS1 TYPE BKPF-BUKRS,
            BELNR1 TYPE BKPF-BELNR,
            GJAHR1 TYPE BKPF-GJAHR,
        END OF TY_RSEG1.

DATA: WA_RSEG1 TYPE TY_RSEG,
      IT_RSEG1 TYPE TABLE OF TY_RSEG1.

DATA: I TYPE I.

TYPES : BEGIN OF TY_R_RSEG,
         BELNR TYPE RSEG-BELNR,
         GJAHR TYPE RSEG-GJAHR,
       END OF TY_R_RSEG.

DATA: WA_R_RSEG TYPE TY_R_RSEG,
      IT_R_RSEG TYPE TABLE OF TY_R_RSEG.

TYPES :BEGIN OF TY_CHPT,
         MATNR TYPE J_1IMTCHID-MATNR,
         CHAPID TYPE J_1IMTCHID-J_1ICHID,
       END OF TY_CHPT.

DATA: WA_CHPT TYPE TY_CHPT,
      IT_CHPT TYPE TABLE OF TY_CHPT.

TYPES :BEGIN OF TY_CHPT_DESC,
        CHAPID      TYPE J_1ICHIDTX-J_1ICHID,
        CHAPID_DESC TYPE J_1ICHIDTX-J_1ICHT1,
       END OF TY_CHPT_DESC.

DATA: WA_CHPT_DESC TYPE TY_CHPT_DESC,
      IT_CHPT_DESC TYPE TABLE OF TY_CHPT_DESC.

TYPES : BEGIN OF TY_EKKN,
          EBELN TYPE EKKN-EBELN,
          EBELP TYPE EKKN-EBELP,
          SAKTO TYPE EKKN-SAKTO,
        END OF TY_EKKN.

DATA: WA_EKKN TYPE TY_EKKN,
      IT_EKKN TYPE TABLE OF TY_EKKN.

TYPES: BEGIN OF TY_MSEG,
          MBLNR  TYPE MSEG-MBLNR,
          MJAHR  TYPE MSEG-MJAHR,
          BWART  TYPE MSEG-BWART,
          LFBNR  TYPE MSEG-LFBNR,
          AWKEY  TYPE BKPF-AWKEY,
          MATNR  TYPE MSEG-MATNR,
          LIFNR  TYPE MSEG-LIFNR,
          BUKRS  TYPE MSEG-BUKRS,
          GJAHR  TYPE MSEG-GJAHR,
          EBELN  TYPE MSEG-EBELN,
          PPRCTR TYPE MSEG-PPRCTR,
          ANLN1  TYPE ANLN1,
         END OF TY_MSEG.

DATA: WA_MSEG TYPE TY_MSEG,
      IT_MSEG TYPE TABLE OF TY_MSEG.






TYPES: BEGIN OF TY_SKAT,
        SAKNR TYPE SKAT-SAKNR,
        TXT20 TYPE SKAT-TXT20,
        TXT50 TYPE SKAT-TXT50,
      END OF TY_SKAT.

DATA: WA_GLDESC  TYPE TY_SKAT,
      IT_GLDESC  TYPE TABLE OF TY_SKAT,
      WA_GLDESC1  TYPE TY_SKAT,
      IT_GLDESC1  TYPE TABLE OF TY_SKAT.

TYPES: BEGIN OF TY_T001W,
        WERKS TYPE T001W-WERKS,
        NAME1 TYPE T001W-NAME1,
      END OF TY_T001W.


DATA: WA_T001W   TYPE TY_T001W,
      IT_T001W   TYPE TABLE OF TY_T001W.

DATA : IT_BKPF2     TYPE TABLE OF TY_BKPF,
       WA_BKPF2     TYPE TY_BKPF,
       IT_RSEG_CN   TYPE TABLE OF TY_RSEG,
       WA_RSEG_CN   TYPE TY_RSEG,
       IT_FINAL1    TYPE TABLE OF TY_FINAL,
       WA_FINAL1    TYPE TY_FINAL,
       IT_FINAL_TMP TYPE TABLE OF TY_FINAL,
       WA_FINAL_TMP TYPE TY_FINAL,
       WA_FINAL1TMP TYPE TY_FINAL,
       MARC_MATNR   TYPE MARC-MATNR,
       MARC_WERKS   TYPE MARC-WERKS,
       MARC_PRCTR   TYPE MARC-PRCTR,
       IT_MARC      TYPE TABLE OF MARC.

DATA : WA_RSEG2 TYPE TY_RSEG,
       WA_RSEG3 TYPE TY_RSEG,
       IT_RSEG4 TYPE TABLE OF TY_RSEG .

DATA : FIN_TOTQTY  TYPE P DECIMALS 2,
       VAT_QTY     TYPE P DECIMALS 2,
       CST_QTY     TYPE P DECIMALS 2,
       FIN_TOTQTY1 TYPE P DECIMALS 2.

TYPE-POOLS : VRM.
DATA:  PARAM    TYPE VRM_ID,
       VALUES   TYPE VRM_VALUES,
       VALUE    LIKE LINE OF VALUES,
       LV_BELNR TYPE EKBE-BELNR,
       LV_GJAHR TYPE EKBE-GJAHR.
CONSTANTS:C_LIFNR1 TYPE LFA1-LIFNR VALUE '0000100520',
          C_LIFNR2 TYPE LFA1-LIFNR VALUE '0000100521'.


DATA: TEMP_BELNR TYPE BSEG-BELNR,
      TEMP_EBELN TYPE BSEG-EBELN,
      TEMP_EBELP TYPE BSEG-EBELP,
      TEMP_LFBNR TYPE RSEG-LFBNR,
      TEMP_LFPOS TYPE RSEG-LFPOS,
      TEMP_LFGJA TYPE RSEG-LFGJA,
      TEMP_XREF3 TYPE BSEG-XREF3.


TYPES :  BEGIN OF TY_MSEG01,
           MBLNR  TYPE MSEG-MBLNR,
           MJAHR TYPE MSEG-MJAHR,
           LFBNR TYPE MSEG-LFBNR,
           AWKEY TYPE BKPF-AWKEY,
         END OF TY_MSEG01.

DATA: WA_MSEG01 TYPE TY_MSEG01,
      IT_MSEG01 TYPE TABLE OF TY_MSEG01.

TYPES :  BEGIN OF TY_BKPF01,
           BELNR  TYPE MSEG-BELNR,
           GJAHR TYPE MSEG-GJAHR,
           AWKEY TYPE BKPF-AWKEY,
           TCODE TYPE BKPF-TCODE,
         END OF TY_BKPF01.

DATA: WA_BKPF01 TYPE TY_BKPF01,
      IT_BKPF01 TYPE TABLE OF TY_BKPF01.


TYPES :  BEGIN OF TY_BSEG01,
           BELNR  TYPE bseg-BELNR,
           GJAHR TYPE  bseg-GJAHR,
           HKONT TYPE  bseg-HKONT,
           KTOSL TYPE  bseg-KTOSL,
         END OF TY_Bseg01.

DATA: WA_Bseg02 TYPE TY_Bseg01,
      IT_Bseg02 TYPE TABLE OF TY_Bseg01.


TYPES : BEGIN OF ty_data,
            LFBNR TYPE MSEG-LFBNR,
           AWKEY TYPE BKPF-AWKEY,
           BELNR  TYPE MSEG-BELNR,
           GJAHR TYPE MSEG-GJAHR,
           HKONT TYPE  bseg-HKONT,
           KTOSL TYPE  bseg-KTOSL,
        END OF ty_data.

data: wa_data TYPE ty_data,
      it_data TYPE TABLE OF ty_data.





******************************************SELECTION SCREEN***************************************************

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS : S_BUKRS FOR BKPF-BUKRS OBLIGATORY,
                 S_BELNR FOR BKPF-BELNR,
                 S_GJAHR FOR BKPF-GJAHR OBLIGATORY,
                 S_BUDAT FOR BKPF-BUDAT OBLIGATORY,
                 S_MWSKZ FOR BSET-MWSKZ,
                 S_WERKS FOR RSEG-WERKS ,
                 S_LIFNR FOR RBKP-LIFNR,
                 S_MATNR FOR RSEG-MATNR,
                 S_MTART FOR MARA-MTART,
                 S_MATKL FOR MARA-MATKL.
SELECTION-SCREEN END OF BLOCK B1.


SELECTION-SCREEN: BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
PARAMETERS: P_CB AS CHECKBOX USER-COMMAND CBC MODIF ID TB2.
SELECTION-SCREEN:END OF BLOCK B2.
******************************************SELECTION SCREEN***************************************************

START-OF-SELECTION.

  PERFORM PROCESS_DATA.
*&---------------------------------------------------------------------*
*&      Form  PROCESS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PROCESS_DATA .

  DATA:  IT_RSEGT  TYPE TABLE OF TY_RSEG,
         IT_BSEGT  TYPE TABLE OF TY_BSEG ,
         IT_BSETT  TYPE TABLE OF TY_BSET ,
         WA_BSETTT TYPE TY_BSET,
         IT_BSETTT TYPE TABLE OF TY_BSET .

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
               WHERE  BUKRS IN S_BUKRS AND
                      BELNR IN S_BELNR AND
                      GJAHR IN S_GJAHR AND
                      BUDAT IN S_BUDAT AND
                       TCODE = 'MIRO'  .

 if it_bkpf is NOT INITIAL.
*
  IT_BKPF2[] = IT_BKPF[].

  LOOP AT IT_BKPF INTO WA_BKPF.

    WA_BKPF-G_BELNR = WA_BKPF-AWKEY+0(10).
    WA_BKPF-G_GJAHR = WA_BKPF-AWKEY+10(4).

    IF SY-SUBRC EQ 0.
      MODIFY IT_BKPF FROM WA_BKPF TRANSPORTING G_BELNR G_GJAHR.
      CLEAR WA_BKPF.
    ENDIF.

  ENDLOOP.

  IF IT_BKPF[] IS NOT  INITIAL.


    SELECT BELNR
           GJAHR
           LIFNR
           BLART INTO TABLE IT_RBKP
                 FROM RBKP
                 FOR ALL ENTRIES IN IT_BKPF
                 WHERE BELNR = IT_BKPF-G_BELNR
                   AND GJAHR = IT_BKPF-G_GJAHR
                   AND  BLART = IT_BKPF-BLART
                   AND BUDAT IN S_BUDAT
                   AND LIFNR IN S_LIFNR AND
                       STBLG = ' '.

    IF IT_RBKP[] IS NOT INITIAL.

      SELECT BELNR
             GJAHR
             BUZEI
             EBELN
             EBELP
             MATNR
             BUKRS
             WRBTR
             MENGE
             MEINS
             SHKZG
             MWSKZ
             BKLAS
             BNKAN
             KSCHL
             LFBNR
             LFPOS
             LFGJA
             WERKS
             FRBNR
             STUNR
             EXKBE
                  INTO CORRESPONDING FIELDS OF TABLE  IT_RSEG
                  FROM RSEG
                  FOR ALL ENTRIES IN IT_RBKP
                  WHERE BELNR = IT_RBKP-BELNR
                    AND GJAHR = IT_RBKP-GJAHR
                    AND WERKS IN S_WERKS
                    AND MATNR IN S_MATNR.

      SELECT BELNR
             MEINS
             BSTME
             EBELN
             EBELP
        FROM RSEG
        INTO CORRESPONDING FIELDS OF TABLE IT_BASE_UNIT
        FOR ALL ENTRIES IN IT_RBKP
        WHERE BELNR = IT_RBKP-BELNR
          AND GJAHR = IT_RBKP-GJAHR.

    ENDIF.

    IF IT_RSEG[] IS NOT INITIAL.

      SELECT EBELN
             EKGRP
             BSART
             BEDAT
             REVNO                     " PO Date
        FROM EKKO
        INTO TABLE IT_EKKO
        FOR ALL ENTRIES IN IT_RSEG
      WHERE EBELN = IT_RSEG-EBELN AND BSART NE 'ZSC'.
    ENDIF.
    IT_RSEG4[] = IT_RSEG[].

    LOOP AT IT_BKPF INTO WA_BKPF.

      WA_RSEG-BUKRS  =  WA_BKPF-BUKRS .
      WA_RSEG-BELNR1 =  WA_BKPF-BELNR .
      WA_RSEG-GJAHR  =  WA_BKPF-GJAHR .
      WA_RSEG-BUDAT  =  WA_BKPF-BUDAT .
      WA_RSEG-BLART  =  WA_BKPF-BLART .

      MODIFY IT_RSEG FROM WA_RSEG TRANSPORTING BUKRS BELNR1 GJAHR BUDAT BLART WHERE BELNR = WA_BKPF-G_BELNR .

      WA_BASE_UNIT-BELNR2 =  WA_BKPF-BELNR.

      MODIFY IT_BASE_UNIT FROM WA_BASE_UNIT TRANSPORTING BELNR2 WHERE BELNR = WA_BKPF-G_BELNR.

      CLEAR : WA_RSEG, WA_BKPF, WA_BASE_UNIT.

    ENDLOOP.



    LOOP AT IT_RBKP INTO WA_RBKP.

      WA_RSEG-LIFNR = WA_RBKP-LIFNR.
      MODIFY IT_RSEG FROM WA_RSEG TRANSPORTING LIFNR WHERE BELNR = WA_RBKP-BELNR .
      CLEAR: WA_RSEG , WA_RBKP.

    ENDLOOP.

    SELECT LIFNR
           NAME1
           NAME3
           REGIO
           ORT01
           LAND1 FROM LFA1
                 INTO TABLE IT_LFA1
                 FOR ALL ENTRIES IN IT_RBKP
                 WHERE LIFNR = IT_RBKP-LIFNR.


    SELECT LAND1
           BLAND
           BEZEI FROM T005U
            INTO TABLE IT_T005U
    FOR ALL ENTRIES IN IT_LFA1
    WHERE SPRAS = SY-LANGU
    AND   LAND1 = IT_LFA1-LAND1
    AND   BLAND = IT_LFA1-REGIO.

    SELECT MATNR
           MTART
           MATKL FROM MARA INTO TABLE IT_MARA
                       FOR ALL ENTRIES IN IT_RSEG
                       WHERE MATNR = IT_RSEG-MATNR AND
                             MATKL IN S_MATKL AND
                             MTART IN S_MTART.

    LOOP AT IT_MARA INTO WA_MARA.

      CLEAR WA_RSEG.
      WA_RSEG-MTART = WA_MARA-MTART .
      WA_RSEG-MATKL = WA_MARA-MATKL .
      MODIFY IT_RSEG FROM WA_RSEG TRANSPORTING MTART MATKL WHERE MATNR = WA_MARA-MATNR .

    ENDLOOP.

    SELECT MATKL
           WGBEZ FROM T023T INTO TABLE IT_T023T
                   FOR ALL ENTRIES IN IT_MARA
                   WHERE MATKL = IT_MARA-MATKL
                   AND SPRAS = 'E'.

    SELECT WERKS
           NAME1 FROM T001W INTO TABLE IT_T001W
                          FOR ALL ENTRIES IN IT_RSEG
                          WHERE WERKS = IT_RSEG-WERKS.
  ENDIF.

  IF NOT IT_RSEG[] IS INITIAL.

* TO CHECK FOR 103 MOVEMENT TYPE
    SELECT MBLNR
           MJAHR
           BWART
           LFBNR
           MATNR
           LIFNR
           BUKRS
           GJAHR
           EBELN
           PPRCTR
           ANLN1"menge
          INTO CORRESPONDING FIELDS OF TABLE IT_MSEG
          FROM MSEG FOR ALL ENTRIES IN IT_RSEG
          WHERE MBLNR = IT_RSEG-LFBNR AND
                GJAHR = IT_RSEG-GJAHR AND
                BUKRS = IT_RSEG-BUKRS AND
                MATNR = IT_RSEG-MATNR AND
                EBELN = IT_RSEG-EBELN AND
                BWART = '101'.
* Find GRR Date
    SELECT MBLNR
           BUDAT
           MJAHR
            FROM MKPF
            INTO TABLE IT_MKPF
            FOR ALL ENTRIES IN IT_RSEG
            WHERE MBLNR EQ IT_RSEG-LFBNR.

    LOOP AT IT_MSEG INTO WA_MSEG.
      IF SY-SUBRC = 0.
        CONCATENATE WA_MSEG-MBLNR WA_MSEG-MJAHR INTO WA_MSEG-AWKEY.
        MODIFY IT_MSEG FROM WA_MSEG TRANSPORTING AWKEY WHERE MBLNR EQ WA_MSEG-MBLNR AND MJAHR = WA_MSEG-MJAHR .
        CLEAR WA_MSEG.
      ENDIF.
    ENDLOOP.
  ENDIF.

  SELECT DOCNO
         EXBAS
         RDOC1
         RITEM1
         RDOC2
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

  SELECT J_1ICHID
         J_1ICHT1
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
    LOOP AT IT_BSET INTO WA_BSET .

      IF WA_BSET-SHKZG = 'H'.
        WA_BSET-HWSTE = WA_BSET-HWSTE * ( -1 ).
        WA_BSET-HWBAS = WA_BSET-HWBAS * ( -1 ).
        MODIFY IT_BSET FROM WA_BSET TRANSPORTING HWSTE HWBAS .
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

      CLEAR WA_RSEG.
      WA_RSEG-ADD = WA_BKPF2-BELNR.

      MODIFY IT_RSEG FROM WA_RSEG TRANSPORTING ADD WHERE BELNR EQ WA_BKPF2-G_BELNR.
    ENDLOOP.

    LOOP AT IT_RSEG INTO WA_RSEG.

      READ TABLE IT_BSETTT INTO WA_BSETTT WITH KEY BELNR = WA_RSEG-ADD
                            TXGRP = WA_RSEG-BUZEI.

      CONCATENATE WA_BSETTT-BELNR WA_BSETTT-TXGRP INTO WA_BSETTT-AWKEY.
      WA_RSEG-AWKEY = WA_BSETTT-AWKEY.
      MODIFY IT_RSEG FROM WA_RSEG TRANSPORTING AWKEY .

    ENDLOOP.

    LOOP AT IT_BSET INTO WA_BSET.
      CONCATENATE WA_BSET-BELNR WA_BSET-TXGRP INTO WA_BSET-AWKEY.
      MODIFY IT_BSET FROM WA_BSET TRANSPORTING AWKEY .
      CLEAR WA_BSET.
    ENDLOOP.

    SORT IT_RSEG BY BELNR GJAHR.

    LOOP AT IT_RSEG INTO WA_RSEG.
      CLEAR WA_MSEG.
      READ TABLE IT_MSEG INTO WA_MSEG WITH KEY LFBNR = WA_RSEG-LFBNR.
      IF  ( SY-SUBRC = 0 AND  WA_MSEG-BWART = '105'  ).
        WA_RSEG-LFBNR = WA_MSEG-MBLNR.
        MODIFY IT_RSEG  FROM WA_RSEG TRANSPORTING LFBNR .

      ENDIF.

      READ TABLE IT_R_RSEG INTO WA_R_RSEG WITH KEY BELNR = WA_RSEG-BELNR
                                 GJAHR = WA_BSEG-GJAHR.
      IF SY-SUBRC NE 0.
        WA_R_RSEG-BELNR = WA_RSEG-BELNR.
        WA_R_RSEG-GJAHR = WA_RSEG-GJAHR.
        APPEND WA_R_RSEG TO IT_R_RSEG.
        CLEAR  WA_R_RSEG.
      ENDIF.
    ENDLOOP.

    SORT IT_R_RSEG BY BELNR GJAHR.
    DELETE ADJACENT DUPLICATES FROM IT_R_RSEG COMPARING BELNR GJAHR.
    SORT IT_R_RSEG BY BELNR GJAHR.

    SORT IT_RSEG BY BELNR BUZEI.

    LOOP AT IT_RSEG INTO WA_RSEG.
      IF WA_RSEG-SHKZG = 'H'.
        WA_RSEG-WRBTR = WA_RSEG-WRBTR * ( -1 ).
        MODIFY IT_RSEG FROM WA_RSEG TRANSPORTING WRBTR  .
      ENDIF.
      READ TABLE IT_BKPF INTO WA_BKPF WITH KEY G_BELNR = WA_RSEG-BELNR
                                  G_GJAHR = WA_RSEG-GJAHR.
      IF SY-SUBRC EQ 0.
        WA_RSEG-BUKRS1 = WA_BKPF-BUKRS.
        WA_RSEG-BELNR1 = WA_BKPF-BELNR.
        WA_RSEG-GJAHR1 = WA_BKPF-GJAHR.
        MODIFY IT_RSEG  FROM WA_RSEG TRANSPORTING BUKRS1 BELNR1 GJAHR1 .
      ENDIF.

      IF WA_RSEG-LFPOS = 0.
        READ TABLE IT_BSET INTO WA_BSET WITH KEY BELNR = WA_RSEG-BELNR1
                                    BUKRS = WA_RSEG-BUKRS1
                                    GJAHR = WA_RSEG-GJAHR1.
        IF SY-SUBRC EQ 0.
          L_LFPOS = WA_RSEG-BUZEI+2(4).
          WA_RSEG-LFPOS = L_LFPOS.
          MODIFY IT_RSEG FROM WA_RSEG TRANSPORTING LFPOS.
        ENDIF.
      ENDIF.
      CLEAR WA_RSEG.
    ENDLOOP.
  ENDIF.

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
           MWSKZ
           XREF3
            FROM BSEG
                 INTO CORRESPONDING FIELDS OF TABLE IT_BSEG
                 FOR ALL ENTRIES IN IT_BKPF
                 WHERE BUKRS = IT_BKPF-BUKRS AND
                       BELNR = IT_BKPF-BELNR AND
                       GJAHR = IT_BKPF-GJAHR ORDER BY PRIMARY KEY.  " Added by <IT-CAR Tool> during Code Remediation



    LOOP AT IT_BSEG INTO WA_BSEG.
      CLEAR WA_RSEG.
      WA_RSEG-GSBER = WA_BSEG-GSBER.
      MODIFY IT_RSEG FROM WA_RSEG TRANSPORTING GSBER WHERE BELNR1 = WA_BSEG-BELNR.

    ENDLOOP.

    IF IT_BSEG IS NOT INITIAL..

      SELECT EBELN EBELP MATNR WERKS MENGE NETPR NETWR MWSKZ
             FROM EKPO INTO TABLE IT_EKPO
             FOR ALL ENTRIES IN IT_BSEG
             WHERE EBELN = IT_BSEG-EBELN
               AND EBELP = IT_BSEG-EBELP.

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
                      INTO CORRESPONDING FIELDS OF TABLE IT_BSEG1
                      FOR ALL ENTRIES IN IT_BKPF
                      WHERE BUKRS = IT_BKPF-BUKRS AND
                            BELNR = IT_BKPF-BELNR AND
                            GJAHR = IT_BKPF-GJAHR AND
                            BUZID = 'M'. "#EC CI_NOORDER  " Added by <IT-CAR Tool> during Code Remediation

    ENDIF.
    IF NOT IT_BKPF[] IS INITIAL.
** Find GL Account for GR No
      SELECT    BUKRS "#EC CI_DB_OPERATION_OK[2431747] " Added by <IT-CAR Tool> during Code Remediation
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
                MWSKZ FROM BSEG
                      INTO CORRESPONDING FIELDS OF TABLE IT_BSEG2
                      FOR ALL ENTRIES IN IT_BKPF
                      WHERE BUKRS = IT_BKPF-BUKRS AND
                            BELNR = IT_BKPF-BELNR AND
                            GJAHR = IT_BKPF-GJAHR and buzei = 1 ORDER BY PRIMARY KEY.  " Added by <IT-CAR Tool> during Code Remediation
    ENDIF.
    SELECT SAKNR
           TXT50
           TXT20  FROM SKAT
            INTO TABLE IT_GLDESC
            FOR ALL ENTRIES IN IT_BSEG1
            WHERE SAKNR = IT_BSEG1-HKONT
            AND   KTOPL = 'YAIN'
            AND   SPRAS = SY-LANGU.

    SELECT SAKNR
           TXT50
           TXT20  FROM SKAT
            INTO TABLE IT_GLDESC1
            FOR ALL ENTRIES IN IT_BSEG2
            WHERE SAKNR = IT_BSEG2-HKONT
            AND   KTOPL = 'YAIN'
            AND   SPRAS = SY-LANGU.


  ENDIF.


  LOOP AT IT_RSEG INTO WA_RSEG.
    CLEAR WA_BSET.
    READ TABLE IT_BSET INTO WA_BSET WITH KEY BUKRS = WA_RSEG-BUKRS1
                                BELNR = WA_RSEG-BELNR1
                                GJAHR = WA_RSEG-GJAHR1.

    IF SY-SUBRC = 0.
      WA_BSET-EBELN = WA_RSEG-EBELN.
      WA_BSET-EBELP = WA_RSEG-EBELP.
      MODIFY IT_BSET FROM WA_BSET TRANSPORTING EBELN EBELP WHERE BUKRS = WA_RSEG-BUKRS1
                                              AND   BELNR = WA_RSEG-BELNR1
                                              AND   GJAHR = WA_RSEG-GJAHR1
                                              AND   TXGRP = WA_RSEG-LFPOS.
*
    ENDIF.
  ENDLOOP.

  SORT IT_BSET BY EBELN EBELP BELNR.

  LOOP AT IT_RSEG INTO WA_RSEG.
    READ TABLE IT_BSET INTO WA_BSET WITH KEY BELNR = WA_RSEG-BELNR.
    WA_RSEG-TXGRP = WA_BSET-TXGRP.
    MODIFY IT_RSEG FROM WA_RSEG TRANSPORTING TXGRP WHERE BELNR = WA_RSEG-BELNR.
  ENDLOOP.
  SORT IT_BSEG2 BY BELNR .



  SORT IT_BSET BY BELNR TXGRP.
  SORT IT_BSEG BY BELNR EBELN EBELP.
  SORT IT_RSEG BY BELNR1 EBELN EBELP BUZEI.

  IT_RSEG01[] = IT_RSEG[].
  IT_BSEG01[] = IT_BSEG[].

  DELETE ADJACENT DUPLICATES FROM IT_BSEG01 COMPARING BELNR EBELN EBELP.

  LOOP AT IT_BSEG01 INTO WA_BSEG01.

    LOOP AT IT_RSEG01 INTO WA_RSEG01  WHERE BELNR1 = WA_BSEG01-BELNR AND
                                        EBELN  = WA_BSEG01-EBELN AND
                                        EBELP  = WA_BSEG01-EBELP.




      WA_FINAL-GSBER = WA_BSEG01-GSBER .
      WA_FINAL-BELNR = WA_BSEG01-BELNR .
      WA_FINAL-GJAHR = WA_BSEG01-GJAHR .


      READ TABLE IT_EKPO INTO WA_EKPO WITH KEY EBELN = WA_BSEG01-EBELN
                                               EBELP = WA_BSEG01-EBELP.

      WA_FINAL-RATE = WA_EKPO-NETPR.

      READ TABLE IT_BKPF  INTO WA_BKPF WITH KEY BUKRS = WA_BSEG01-BUKRS
                                                BELNR = WA_BSEG01-BELNR
                                                GJAHR = WA_BSEG01-GJAHR.

      WA_FINAL-WAERS = WA_BKPF-WAERS.
      WA_FINAL-KURSF = WA_BKPF-KURSF.






      WA_FINAL-FRBNR  = WA_RSEG01-FRBNR   .
      WA_FINAL-EXKBE  = WA_RSEG01-EXKBE   .
      WA_FINAL-BUDAT  = WA_RSEG01-BUDAT   .
      WA_FINAL-MWSKZ  = WA_RSEG01-MWSKZ   .
      WA_FINAL-WERKS  = WA_RSEG01-WERKS   .
      WA_FINAL-LIFNR  = WA_RSEG01-LIFNR   .
      WA_FINAL-MATNR  = WA_RSEG01-MATNR   .
      WA_FINAL-MTART  = WA_RSEG01-MTART   .
      WA_FINAL-MATKL  = WA_RSEG01-MATKL   .
      WA_FINAL-BLART  = WA_RSEG01-BLART   .
      WA_FINAL-BUZEI1 = WA_RSEG01-BUZEI    .
      WA_FINAL-KSCHL  = WA_RSEG01-KSCHL   .
      WA_FINAL-BUKRS  = WA_RSEG01-BUKRS   .
      WA_FINAL-GJAHR  = WA_RSEG01-GJAHR1  .
      WA_FINAL-EBELN  = WA_RSEG01-EBELN   .
      WA_FINAL-EBELP  = WA_RSEG01-EBELP   .
      WA_FINAL-AWKEY4 = WA_RSEG01-AWKEY   .
      WA_FINAL-LFBNR  = WA_RSEG01-LFBNR   .
      WA_FINAL-BNKAN  = WA_RSEG01-BNKAN   .
      WA_FINAL-BKLAS  = WA_RSEG01-BKLAS   .
      WA_FINAL-WERKS  = WA_RSEG01-WERKS   .






      READ TABLE IT_EKKO INTO WA_EKKO WITH KEY EBELN = WA_FINAL-EBELN.

      IF SY-SUBRC = 0.

        WA_FINAL-EKGRP = WA_EKKO-EKGRP  .
        WA_FINAL-BSART = WA_EKKO-BSART  .
        WA_FINAL-BEDAT = WA_EKKO-BEDAT  .
        WA_FINAL-REVNO = WA_EKKO-REVNO  .

        SELECT BATXT UP TO 1 ROWS FROM T161T
               INTO WA_FINAL-DOC_TYP_DESC
               WHERE SPRAS = 'E'
               AND   BSART = WA_FINAL-BSART ORDER BY PRIMARY KEY.
        ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation

      ENDIF.

      READ TABLE IT_T001W INTO WA_T001W WITH KEY WERKS = WA_RSEG01-WERKS.

      IF SY-SUBRC = 0.
        WA_FINAL-NAME2 = WA_T001W-NAME1.
      ENDIF.

      WA_FINAL-MEINS   = WA_RSEG01-MEINS.
      WA_FINAL-G_BELNR = WA_RSEG01-BELNR.

      READ TABLE IT_BASE_UNIT INTO WA_BASE_UNIT WITH KEY BELNR2 = WA_BSEG01-BELNR
                                                         EBELN  = WA_BSEG01-EBELN
                                                         EBELP  = WA_BSEG01-EBELP.




      WA_FINAL-BSTME = WA_BASE_UNIT-BSTME.

      IF WA_FINAL-BSTME = 'LE'.
        WA_FINAL-BSTME = 'AU'.

      ELSEIF WA_FINAL-BSTME = 'PAK'.
        WA_FINAL-BSTME = 'PAC'.
      ENDIF.


      SELECT BUDAT
            UP TO 1 ROWS FROM MKPF
            INTO WA_FINAL-BUDAT
            WHERE MBLNR = WA_RSEG01-LFBNR ORDER BY PRIMARY KEY.
      ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation

      READ TABLE IT_MSEG INTO WA_MSEG WITH KEY MBLNR = WA_RSEG01-LFBNR
                           GJAHR = WA_RSEG01-GJAHR
                           BUKRS = WA_RSEG01-BUKRS .

      IF SY-SUBRC = 0.
        WA_FINAL-PRCTR  = WA_MSEG-PPRCTR .
        WA_FINAL-ANLN1  = WA_MSEG-ANLN1  .


        SELECT TXT50 UP TO 1 ROWS FROM ANLA INTO WA_FINAL-TXT50 WHERE ANLN1 = WA_FINAL-ANLN1 ORDER BY PRIMARY KEY.
        ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation
      ELSE.
        SELECT SINGLE PRCTR
      FROM MARC
      INTO WA_FINAL-PRCTR
      WHERE MATNR = WA_FINAL-MATNR
        AND WERKS = WA_FINAL-WERKS.
        IF SY-SUBRC NE 0.
          CLEAR:LV_BELNR,LV_GJAHR.
          SELECT BELNR GJAHR
              UP TO 1 ROWS FROM EKBE
              INTO (LV_BELNR,LV_GJAHR)
             WHERE LFBNR = WA_RSEG01-LFBNR
               AND GJAHR = WA_FINAL-BUDAT+0(4) ORDER BY PRIMARY KEY.
          ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation
          SELECT PPRCTR UP TO 1 ROWS FROM MSEG
             INTO WA_FINAL-PRCTR
             WHERE MBLNR = LV_BELNR
              AND  MJAHR = LV_GJAHR ORDER BY PRIMARY KEY.
          ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation

        ENDIF.
      ENDIF.

      READ TABLE IT_RBKP INTO WA_RBKP WITH KEY BELNR = WA_FINAL-G_BELNR.
      WA_FINAL-LIFNR = WA_RBKP-LIFNR .

      READ TABLE IT_LFA1 INTO WA_LFA1 WITH KEY LIFNR = WA_FINAL-LIFNR.

      WA_FINAL-NAME1  = WA_LFA1-NAME1.
      WA_FINAL-NAME3  = WA_LFA1-NAME3.
      WA_FINAL-MATNR  = WA_RSEG01-MATNR.

      READ TABLE IT_MARA INTO WA_MARA WITH KEY MATNR = WA_RSEG01-MATNR.

      IF SY-SUBRC = 0.
        WA_FINAL-MATKL = WA_MARA-MATKL.
        READ TABLE IT_T023T  INTO WA_T023T WITH KEY MATKL = WA_MARA-MATKL.
        IF SY-SUBRC = 0.
          WA_FINAL-WGBEZ = WA_T023T-WGBEZ.
        ENDIF.
        WA_FINAL-MTART = WA_MARA-MTART.
      ENDIF.

      WA_FINAL-MWSKZ = WA_RSEG01-MWSKZ.
      WA_FINAL-REGIO = WA_LFA1-REGIO.
      WA_FINAL-ORT01 = WA_LFA1-ORT01.

      SELECT SINGLE TEXT1
        FROM T007S
        INTO WA_FINAL-TAX_DESC
        WHERE SPRAS = SY-LANGU
        AND   MWSKZ = WA_RSEG01-MWSKZ
        AND   KALSM = 'TAXINN'.

      WA_FINAL-BUZEI = WA_RSEG01-BUZEI.

      IF WA_FINAL-WAERS = 'INR'.
        WA_FINAL-DMBTR = WA_RSEG01-WRBTR .
        IF  ( WA_FINAL-LIFNR = C_LIFNR1 OR WA_FINAL-LIFNR = C_LIFNR2 ).
*        it_final-dmbtr = it_final-dmbtr + it_final-dmbtr.
          WA_FINAL-DMBTR = '0.00'.
        ENDIF.
      ENDIF.
      IF WA_FINAL-WAERS <> 'INR'.
        WA_FINAL-DMBTR  = WA_RSEG01-WRBTR.
*        it_final-dmbtr = it_final-dmbtr * it_final-kursf.
        IF  ( WA_FINAL-LIFNR = C_LIFNR1 OR WA_FINAL-LIFNR = C_LIFNR2 ).
*        it_final-dmbtr = it_final-dmbtr + it_final-dmbtr.
          WA_FINAL-DMBTR = '0.00'.
        ENDIF.
      ENDIF.

      READ TABLE IT_T025T INTO WA_T025T WITH KEY BKLAS = WA_RSEG01-BKLAS.
      IF SY-SUBRC = 0.
        WA_FINAL-BKBEZ = WA_T025T-BKBEZ.
      ENDIF.

      READ TABLE IT_MAKT INTO WA_MAKT WITH KEY MATNR = WA_RSEG01-MATNR.
      IF SY-SUBRC EQ 0.
        WA_FINAL-MAKTX = WA_MAKT-MAKTX.
      ENDIF.

      READ TABLE IT_J_1IMOVEND INTO WA_J_1IMOVEND WITH KEY LIFNR = WA_RBKP-LIFNR.
      IF SY-SUBRC = 0.
        WA_FINAL-J_1ILSTNO = WA_J_1IMOVEND-J_1ILSTNO .
        WA_FINAL-J_1IPANNO = WA_J_1IMOVEND-J_1IPANNO .
        WA_FINAL-J_1ISERN  = WA_J_1IMOVEND-J_1ISERN  .
        WA_FINAL-CSTNO     = WA_J_1IMOVEND-J_1ICSTNO .
        WA_FINAL-ECCNO     = WA_J_1IMOVEND-J_1IEXCD  .

      ENDIF.

      READ TABLE IT_CHPT INTO WA_CHPT WITH KEY MATNR = WA_RSEG01-MATNR.
      IF SY-SUBRC = 0.
        WA_FINAL-CHAPID =  WA_CHPT-CHAPID.
      ENDIF.

      READ TABLE IT_CHPT_DESC INTO WA_CHPT_DESC WITH KEY CHAPID = WA_FINAL-CHAPID.
      IF SY-SUBRC = 0.
        WA_FINAL-CHAPID_DESC = WA_CHPT_DESC-CHAPID_DESC.
      ENDIF.

      READ TABLE IT_BKPF INTO WA_BKPF WITH KEY BUKRS = WA_BSEG01-BUKRS
                                  BELNR = WA_BSEG01-BELNR
                                  GJAHR = WA_BSEG01-GJAHR.
      IF SY-SUBRC EQ 0.
        WA_FINAL-BLDAT   = WA_BKPF-BLDAT  .
        WA_FINAL-POSTDAT = WA_BKPF-BUDAT  .
        WA_FINAL-XBLNR   = WA_BKPF-XBLNR  .
        WA_FINAL-BLART   = WA_BKPF-BLART  .
        WA_FINAL-WAERS   = WA_BKPF-WAERS  .
        WA_FINAL-KURSF   = WA_BKPF-KURSF  .
        WA_FINAL-TCODE   = WA_BKPF-TCODE  .
      ENDIF.

      CASE WA_FINAL-BLART.
        WHEN 'RE'.

          WA_FINAL-DOC_TYPE = 'Invoice'.

        WHEN 'RL'.

          WA_FINAL-DOC_TYPE = 'Credit Memo'.

        WHEN 'RC'.

          WA_FINAL-DOC_TYPE = 'Subsequent Debit'.

        WHEN 'RA'.
          WA_FINAL-DOC_TYPE = 'Subsequent Credit'.
      ENDCASE.

      IF ( WA_FINAL-BLART = 'RE' OR WA_FINAL-BLART = 'RL' ) .
        IF SY-SUBRC EQ 0.
          WA_FINAL-QTY = WA_RSEG01-MENGE.
        ENDIF.
      ENDIF.

      IF ( WA_FINAL-BLART = 'RC' OR WA_FINAL-BLART = 'RA' ) .
        WA_FINAL-QTY = WA_RSEG01-MENGE.
        WA_FINAL-QTY = WA_FINAL-QTY * -1.
      ENDIF.

      IF WA_FINAL-TCODE = 'MR8M'.
        WA_FINAL-QTY = WA_FINAL-QTY * -1.
      ENDIF.

      WA_FINAL-MENGE = WA_RSEG01-MENGE.

      IF WA_FINAL-MENGE <> WA_FINAL-QTY.

        WA_FINAL-SETTLED = WA_FINAL-MENGE + WA_FINAL-QTY.

      ENDIF.

      IF WA_FINAL-WAERS = 'JPY'.
        WA_FINAL-DMBTR = WA_FINAL-DMBTR / 10.
        WA_FINAL-WRBTR = WA_FINAL-DMBTR.
        WA_FINAL-WRBTR = ( WA_FINAL-WRBTR ) * WA_FINAL-KURSF / 100.
        WA_FINAL-BNKAN = WA_FINAL-BNKAN / 10.
        WA_FINAL-BNKANLOC = ( WA_FINAL-BNKAN ) * WA_FINAL-KURSF / 100.

      ELSE.
        WA_FINAL-WRBTR = WA_FINAL-DMBTR * WA_FINAL-KURSF."#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation

        WA_FINAL-BNKANLOC = ( WA_FINAL-BNKAN ) * WA_FINAL-KURSF.
      ENDIF.


      READ TABLE IT_BSEG2 INTO WA_BSEG2 WITH KEY BUKRS = WA_RSEG01-BUKRS
                                                 BELNR = WA_RSEG01-BELNR1.


      IF SY-SUBRC EQ 0.
        WA_FINAL-HKONT = WA_BSEG2-HKONT.
      ENDIF.

      READ TABLE IT_GLDESC INTO WA_GLDESC WITH KEY SAKNR = WA_FINAL-HKONT.

      IF SY-SUBRC EQ 0.
        WA_FINAL-GL_DESC = WA_GLDESC-TXT50.
        WA_FINAL-GL_DESC_TXT20 = WA_GLDESC-TXT20.
      ENDIF.

      READ TABLE IT_GLDESC1 INTO WA_GLDESC1 WITH KEY SAKNR = WA_FINAL-HKONT.

      IF SY-SUBRC EQ 0.
        WA_FINAL-GL_DESC = WA_GLDESC1-TXT50.
        WA_FINAL-GL_DESC_TXT20 = WA_GLDESC1-TXT20.
      ENDIF.


      READ TABLE IT_T005U INTO WA_T005U WITH KEY BLAND = WA_LFA1-REGIO
                                        LAND1 = WA_LFA1-LAND1.

      IF SY-SUBRC EQ 0.
        WA_FINAL-REGIO_DESC = WA_T005U-BEZEI.
      ENDIF.

      WA_FINAL-LAND1 = WA_LFA1-LAND1.


      CONCATENATE WA_FINAL-G_BELNR WA_FINAL-BUZEI INTO WA_FINAL-AWKEY1.
      CONCATENATE WA_FINAL-EBELN   WA_FINAL-EBELP INTO WA_FINAL-AWKEY2.

      CONCATENATE WA_FINAL-BELNR WA_FINAL-BUZEI+3(3) INTO WA_FINAL-AWKEY3.
      READ TABLE IT_BSET  INTO WA_BSET WITH KEY BELNR = WA_BSET-BELNR.

      IF SY-SUBRC EQ 0.
        WA_FINAL-BUZEI = WA_BSET-BUZEI.
      ENDIF.

      WA_FINAL-EXBAS = WA_FINAL-QTY * WA_FINAL-RATE.


      WA_FINAL-GROSS = WA_FINAL-WRBTR                       + WA_FINAL-BNKAN"#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
                     + WA_FINAL-BED                         + WA_FINAL-ADDBED
                     + WA_FINAL-ECS                         + WA_FINAL-HCESS
                     + WA_FINAL-EBED                        + WA_FINAL-RMDED
                     + WA_FINAL-RMNON                       + WA_FINAL-CGDED
                     + WA_FINAL-CGNON                       + WA_FINAL-CST
                     + WA_FINAL-SERTAX                      + WA_FINAL-ESERTAX
                     + WA_FINAL-SHCESS                      + WA_FINAL-ACD
                     + WA_FINAL-BASCUS                      + WA_FINAL-CVD
                     + WA_FINAL-ECVD                        + WA_FINAL-HECVD
                     + WA_FINAL-ECUSTOM                     + WA_FINAL-HECUSTOM
                     + WA_FINAL-IMP_FREIGHT_VAL             - WA_FINAL-DISC
                     + WA_FINAL-BED_N                       + WA_FINAL-CES_N
                     + WA_FINAL-HCESS_N                     + WA_FINAL-AED_N
                     + WA_FINAL-FRGT + WA_FINAL-SGST + WA_FINAL-UGST + WA_FINAL-IGST + WA_FINAL-CGST .

      IF WA_FINAL-GROSS GT 0.

        WA_FINAL-LND_CST = WA_FINAL-GROSS / WA_FINAL-QTY .
*          ENDIF.
      ENDIF.


      APPEND WA_FINAL TO IT_FINAL.



      CLEAR : WA_FINAL, WA_RSEG01.
      CLEAR : WA_FINAL-AWKEY3,WA_FINAL-CGDED,WA_FINAL-RMDED,WA_FINAL-HKONT.

    ENDLOOP.

    CLEAR WA_BSEG01.


  ENDLOOP.





  DELETE ADJACENT DUPLICATES FROM IT_FINAL COMPARING ALL FIELDS.

  SORT IT_FINAL BY BELNR.
  SORT IT_BSET  BY BELNR.


  LOOP AT IT_FINAL INTO WA_FINAL.
    CLEAR WA_RSEG.
    READ TABLE IT_RSEG INTO WA_RSEG WITH  KEY BELNR = WA_FINAL-G_BELNR
                                              EBELN = WA_FINAL-EBELN
                                              EBELP = WA_FINAL-EBELP
                                              BUZEI  = WA_FINAL-BUZEI1.                          "**********************************************

    CASE WA_RSEG-KSCHL.
      WHEN 'J1CV'.
        WA_FINAL-HECVD = WA_RSEG-WRBTR * WA_FINAL-KURSF."#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
      WHEN 'JADC'.
        WA_FINAL-ACD = WA_RSEG-WRBTR * WA_FINAL-KURSF."#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
      WHEN 'JCDB'.
        WA_FINAL-BASCUS = WA_RSEG-WRBTR * WA_FINAL-KURSF."#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
      WHEN  'JCV1'.
        WA_FINAL-CVD = WA_RSEG-WRBTR * WA_FINAL-KURSF."#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
      WHEN 'JECV'.
        WA_FINAL-ECVD = WA_RSEG-WRBTR * WA_FINAL-KURSF."#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
      WHEN 'JEDB'.
        WA_FINAL-ECUSTOM = WA_RSEG-WRBTR * WA_FINAL-KURSF."#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
      WHEN 'JSED'.
        WA_FINAL-HECUSTOM = WA_RSEG-WRBTR * WA_FINAL-KURSF."#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
      WHEN 'FRB1'.
        WA_FINAL-FRGT = WA_FINAL-FRGT + WA_RSEG-WRBTR .
    ENDCASE.

    MODIFY IT_FINAL FROM WA_FINAL.
    CLEAR WA_FINAL.
  ENDLOOP.

  LOOP AT IT_FINAL INTO WA_FINAL.

    LOOP AT IT_BSET INTO WA_BSET  WHERE  BELNR = WA_FINAL-BELNR AND
                                              GJAHR = WA_FINAL-GJAHR AND
                                              EBELN = WA_FINAL-EBELN AND
                                              EBELP = WA_FINAL-EBELP.

      CASE WA_BSET-KSCHL.
        WHEN 'JMOP'.
          WA_FINAL-BED     = WA_FINAL-BED + WA_BSET-HWSTE.
          WA_FINAL-BEDRATE = WA_BSET-KBETR / 10."#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
        WHEN 'JAOP'.
          WA_FINAL-ADDBED = WA_FINAL-ADDBED + WA_BSET-HWSTE.
        WHEN 'JEC1'.
          WA_FINAL-ECS = WA_FINAL-ECS + WA_BSET-HWSTE.
        WHEN 'JEC2'.
          WA_FINAL-EBED = WA_FINAL-EBED + WA_BSET-HWSTE.
        WHEN 'JVRD'.
          WA_FINAL-RMDED     = WA_FINAL-RMDED + WA_BSET-HWSTE.
          WA_FINAL-RMDEDRATE = WA_BSET-KBETR / 10."#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
          IF NOT WA_FINAL-RMDEDRATE = 0.
            WA_FINAL-VATBAS    = WA_BSET-HWSTE + WA_FINAL-WRBTR.
          ENDIF.
        WHEN 'JVRN'.
          WA_FINAL-RMNON = WA_FINAL-RMNON + WA_BSET-HWSTE.
        WHEN 'JVCD' OR 'ZADD'.
          WA_FINAL-CGDED = WA_FINAL-CGDED + WA_BSET-HWSTE.
          WA_FINAL-CGDEDRATE = WA_BSET-KBETR / 10."#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
        WHEN 'ZADN' OR 'JVCN'.
          WA_FINAL-CGNON = WA_FINAL-CGNON + WA_BSET-HWSTE.
        WHEN 'JISG'.
          WA_FINAL-SGST = WA_FINAL-SGST + WA_BSET-HWSTE.
          WA_FINAL-SGSTR = WA_BSET-KBETR / 10."#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
        WHEN 'JICG'.
          WA_FINAL-CGST = WA_FINAL-CGST + WA_BSET-HWSTE.
          WA_FINAL-CGSTR = WA_BSET-KBETR / 10."#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
        WHEN 'JIUG'.
          WA_FINAL-UGST = WA_FINAL-UGST + WA_BSET-HWSTE.
          WA_FINAL-UGSTR = WA_BSET-KBETR / 10."#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
        WHEN 'JIIG'.
          WA_FINAL-IGST = WA_FINAL-IGST + WA_BSET-HWSTE.
          WA_FINAL-IGSTR = WA_BSET-KBETR / 10."#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
        WHEN 'JCOS'.
          WA_FINAL-JCOS = WA_FINAL-JCOS + WA_BSET-HWSTE.
          WA_FINAL-JCOSR = WA_BSET-KBETR / 10."#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
        WHEN 'JVCS'.
          WA_FINAL-CST     = WA_FINAL-CST + WA_BSET-HWSTE.
          WA_FINAL-CSTRATE = WA_BSET-KBETR / 10."#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
          IF NOT WA_FINAL-CSTRATE = 0.
            WA_FINAL-VATBAS    = WA_BSET-HWSTE + WA_FINAL-WRBTR.
          ENDIF.
        WHEN 'JSVD'.
          WA_FINAL-SERTAX = WA_FINAL-SERTAX + WA_BSET-HWSTE.
          IF NOT WA_FINAL-SERTAX = 0.
            WA_FINAL-SERBAS    = WA_BSET-HWBAS.
          ENDIF.
        WHEN 'JEC3'.
          WA_FINAL-ESERTAX = WA_FINAL-ESERTAX + WA_BSET-HWSTE.
        WHEN 'JSEP'.
          WA_FINAL-HCESS = WA_FINAL-HCESS + WA_BSET-HWSTE.
        WHEN 'JSE1'.
          WA_FINAL-SHCESS = WA_FINAL-SHCESS + WA_BSET-HWSTE.
          WA_FINAL-SHRATE = WA_BSET-KBETR / 10."#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
        WHEN 'JMIP'.
          WA_FINAL-BED_N = WA_FINAL-BED_N + WA_BSET-HWSTE.
        WHEN 'JEC2'.
          WA_FINAL-CES_N = WA_FINAL-CES_N + WA_BSET-HWSTE.
        WHEN 'JSEI'.
          WA_FINAL-HCESS_N = WA_FINAL-HCESS_N + WA_BSET-HWSTE.
        WHEN 'JAIP'.
          WA_FINAL-AED_N = WA_FINAL-AED_N + WA_BSET-HWSTE.
      ENDCASE.

      MODIFY IT_FINAL FROM WA_FINAL.
    ENDLOOP.
    CLEAR : WA_FINAL,WA_BSET, WA_BSET-HWBAS.
    CLEAR : WA_FINAL-BED,WA_FINAL-BEDRATE,WA_FINAL-ADDBED,WA_FINAL-ECS,WA_FINAL-EBED,
           WA_FINAL-RMDED ,WA_FINAL-RMDEDRATE,WA_FINAL-VATBAS,WA_FINAL-RMNON,
           WA_FINAL-CGDED,WA_FINAL-CGDEDRATE,WA_FINAL-CGNON,WA_FINAL-CST,
           WA_FINAL-CSTRATE,WA_FINAL-SERTAX,WA_FINAL-SERBAS,WA_FINAL-ESERTAX,
           WA_FINAL-HCESS,WA_FINAL-SHCESS,WA_FINAL-SHRATE,WA_FINAL-BED_N,
           WA_FINAL-CES_N,WA_FINAL-HCESS_N,WA_FINAL-AED_N,WA_BSET,

           WA_FINAL-IGST,WA_FINAL-JCOS,WA_FINAL-UGST,WA_FINAL-CGST,WA_FINAL-SGST,
           WA_FINAL-SGSTR,WA_FINAL-CGSTR, WA_FINAL-UGSTR, WA_FINAL-IGSTR, WA_FINAL-JCOSR.
  ENDLOOP.

  IT_FINAL_TMP[] = IT_FINAL[].
  SORT IT_FINAL BY BELNR EBELN EBELP.
  CLEAR : WA_FINAL, WA_FINAL1, WA_FINAL1TMP.

  LOOP AT IT_FINAL INTO WA_FINAL.
    CLEAR : WA_FINAL_TMP, WA_FINAL1.

    IF SY-SUBRC = 0.

      READ TABLE IT_FINAL1 WITH KEY WA_FINAL-AWKEY1 INTO WA_FINAL1TMP.

      IF NOT SY-SUBRC = 0.
        MOVE-CORRESPONDING WA_FINAL TO WA_FINAL1.
        CLEAR : WA_FINAL1-DMBTR,
                WA_FINAL1-WRBTR,
                WA_FINAL1-FRGT,WA_FINAL1-PCKN,WA_FINAL1-MISC,WA_FINAL1-BNKAN,
                WA_FINAL1-EXBAS,WA_FINAL1-VATBAS,WA_FINAL1-SERBAS,WA_FINAL1-BED,
                WA_FINAL1-ADDBED,
                WA_FINAL1-ECS,WA_FINAL1-HCESS,WA_FINAL1-EBED,WA_FINAL1-RMDED,
                WA_FINAL1-RMNON,WA_FINAL1-CGDED,WA_FINAL1-CGNON,WA_FINAL1-CST,
                WA_FINAL1-SERTAX,WA_FINAL1-ESERTAX,WA_FINAL1-SHCESS,WA_FINAL1-ACD,
                WA_FINAL1-BASCUS,WA_FINAL1-CVD,WA_FINAL1-ECVD,WA_FINAL1-HECVD,
                WA_FINAL1-ECUSTOM,WA_FINAL1-HECUSTOM,WA_FINAL1-IMP_FREIGHT_VAL,WA_FINAL1-DISC,
                WA_FINAL1-WCT,WA_FINAL1-TDS,WA_FINAL1-ESIC,WA_FINAL1-GROSS,

                 WA_FINAL1-IGST,
                 WA_FINAL1-JCOS,
                 WA_FINAL1-UGST,
                 WA_FINAL1-CGST,
                 WA_FINAL1-SGST
                .


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

          WA_FINAL1-DMBTR    =  WA_FINAL_TMP-DMBTR.
          WA_FINAL1-WRBTR    =  WA_FINAL_TMP-WRBTR.
          WA_FINAL1-PCKN     = WA_FINAL1-PCKN   + WA_FINAL_TMP-PCKN.
          WA_FINAL1-MISC     = WA_FINAL1-MISC   + WA_FINAL_TMP-MISC.
          WA_FINAL1-BNKAN    = WA_FINAL1-BNKAN  + WA_FINAL_TMP-BNKAN.
          WA_FINAL1-EXBAS    = WA_FINAL1-EXBAS  + WA_FINAL_TMP-EXBAS.
          WA_FINAL1-VATBAS   = WA_FINAL1-VATBAS + WA_FINAL_TMP-VATBAS.
          WA_FINAL1-SERBAS   = WA_FINAL1-SERBAS + WA_FINAL_TMP-SERBAS.
          WA_FINAL1-BED      = WA_FINAL1-BED    + WA_FINAL_TMP-BED.
          WA_FINAL1-ADDBED   = WA_FINAL1-ADDBED + WA_FINAL_TMP-ADDBED.
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
************************************************************************************************** bY SANGEETH
          WA_FINAL1-IGST = WA_FINAL1-IGST + WA_FINAL_TMP-IGST.
          WA_FINAL1-JCOS = WA_FINAL1-JCOS + WA_FINAL_TMP-JCOS.
          WA_FINAL1-UGST = WA_FINAL1-UGST + WA_FINAL_TMP-UGST.
          WA_FINAL1-CGST = WA_FINAL1-CGST + WA_FINAL_TMP-CGST.
          WA_FINAL1-SGST = WA_FINAL1-SGST + WA_FINAL_TMP-SGST.


          WA_FINAL1-FRGT     = WA_FINAL-FRGT  .




          WA_FINAL1-GROSS = WA_FINAL1-WRBTR                      + WA_FINAL1-BNKAN"#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
                         + WA_FINAL1-BED                         + WA_FINAL1-ADDBED
                         + WA_FINAL1-ECS                         + WA_FINAL1-HCESS
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
                         + WA_FINAL-FRGT + WA_FINAL-SGST + WA_FINAL-IGST + WA_FINAL-CGST + WA_FINAL-UGST.



          IF WA_FINAL1-GROSS GT 0.
            IF WA_FINAL1-QTY <> 0.
              WA_FINAL1-LND_CST = WA_FINAL1-GROSS / WA_FINAL1-QTY .
            ENDIF.
          ENDIF.

        ENDLOOP.

        APPEND WA_FINAL1 TO IT_FINAL1.
        CLEAR : WA_FINAL1.

      ENDIF.
    ENDIF.


  ENDLOOP.

*** Re calc of Vat Base Value.
  CLEAR : WA_FINAL1.
  CLEAR : WA_FINAL1.

  SORT IT_FINAL1 BY BUKRS BELNR GJAHR G_BELNR BUZEI1.
  SORT IT_FINAL BY BUKRS BELNR GJAHR G_BELNR BUZEI1.

  DELETE IT_FINAL WHERE BELNR EQ SPACE.
  DELETE ADJACENT DUPLICATES FROM IT_FINAL COMPARING BELNR G_BELNR QTY XBLNR EBELN EBELP BUZEI1.
  DELETE ADJACENT DUPLICATES FROM IT_FINAL1 COMPARING BELNR G_BELNR QTY XBLNR EBELN EBELP BUZEI1.

  SORT IT_BSET1 BY BUKRS BELNR GJAHR.
  DELETE ADJACENT DUPLICATES FROM IT_BSET1 COMPARING BUKRS BELNR GJAHR.

  SORT IT_FINAL BY BUKRS BELNR GJAHR G_BELNR BUZEI1.



*  ********************************************************************************************************************

  SELECT MBLNR
         MJAHR
         LFBNR
              FROM MSEG INTO CORRESPONDING FIELDS OF TABLE IT_MSEG01 FOR ALL ENTRIES IN IT_FINAL1 WHERE lfbnr = IT_FINAL1-LFBNR and
                                                                                                        MJAHR in S_GJAHR and lfbnr ne ' '.



  LOOP AT IT_MSEG01 INTO WA_MSEg01.

    CONCATENATE WA_MSEG01-MBLNR WA_MSEG01-MJAHR INTO WA_MSEG01-AWKEY.


    MODIFY IT_MSEG01 FROM WA_MSEG01 TRANSPORTING AWKEY.

   clear: wa_mseg01.

  ENDLOOP.


  SELECT BELNR
         GJAHR
         AWKEY
         TCODE FROM BKPF INTO  TABLE IT_BKPF01 FOR ALL ENTRIES IN IT_MSEG01 WHERE AWKEY = IT_MSEG01-AWKEY.


  REFRESH IT_BSEG01.

  SELECT BELNR "#EC CI_DB_OPERATION_OK[2431747] " Added by <IT-CAR Tool> during Code Remediation
         GJAHR
         HKONT
         KTOSL FROM BSEG INTO TABLE IT_BSEG02 FOR ALL ENTRIES IN IT_BKPF01 WHERE BELNR = IT_BKPF01-BELNR AND GJAHR = IT_BKPF01-GJAHR."#EC CI_NOORDER  "Added by SPLABAP during code remediation

SORT IT_BSEG02  . " Added by <IT-CAR Tool> during Code Remediation
DELETE ADJACENT DUPLICATES FROM it_bseg02 COMPARING ALL FIELDS.

 LOOP AT it_mseg01 INTO wa_mseg01 WHERE lfbnr ne ' '.

        wa_data-lfbnr = wa_mseg01-lfbnr.
        wa_data-awkey = wa_mseg01-awkey.

   CLEAR WA_BKPF01.

   READ TABLE it_bkpf01 INTO wa_bkpf01 with key awkey = wa_mseg01-awkey.



   if sy-subrc = 0.

               wa_data-belnr = wa_bkpf01-belnr.
               wa_data-gjahr = wa_bkpf01-gjahr.

       LOOP  at it_bseg02 INTO wa_bseg02 where belnr = wa_bkpf01-belnr and gjahr = wa_bkpf01-gjahr.


             IF sy-subrc = 0 AND ( wa_bseg02-ktosl = 'KBS' OR wa_bseg02-ktosl = 'BSX' OR wa_bseg02-ktosl = 'FRL' ) .




                  WA_DATA-KTOSL = WA_BSEG02-KTOSL.
                  WA_DATA-HKONT = WA_BSEG02-HKONT.

               APPEND WA_DATA TO IT_DATA.
               CLEAR WA_DATA.
             ENDIF.

       endloop.



   ENDIF.
    .

CLEAR WA_MSEG01.
 ENDLOOP.

DELETE ADJACENT DUPLICATES FROM it_data COMPARING ALL FIELDS.

delete it_data WHERE lfbnr = ' '.


*  ********************************************************************************************************************
  LOOP AT IT_FINAL1 INTO WA_FINAL1.

    IF WA_FINAL1-EXKBE = ' '.

      READ TABLE IT_BSEG INTO WA_BSEG WITH KEY  BELNR = WA_FINAL1-BELNR
                                                EBELN = WA_FINAL1-EBELN
                                                EBELP = WA_FINAL1-EBELP
                                                KOART = 'K'.
      IF SY-SUBRC = 0.
        WA_FINAL1-WRBTR = WA_BSEG-WRBTR.
        WA_FINAL1-DMBTR = WA_BSEG-DMBTR.
        WA_FINAL1-GROSS = WA_FINAL1-DMBTR."#EC CI_FLDEXT_OK[2610650] "Added by SPLABAP during code remediation
      ENDIF.
    ENDIF.
    CLEAR WA_BSEG.


     READ TABLE IT_BSEG INTO WA_BSEG WITH KEY BELNR = WA_FINAL1-BELNR                      "added
                              KTOSL = 'FR1'
                              KOART = 'S'.
                IF SY-SUBRC = 0.
                   WA_FINAL1-GL_AMT = WA_BSEG-WRBTR.
                   WA_FINAL1-GL_ACNT = WA_BSEG-HKONT.

                   CLEAR WA_BSEG.

               else.

                    READ TABLE IT_BSEG INTO WA_BSEG WITH KEY BELNR = WA_FINAL1-BELNR
                                                             KTOSL = 'KBS'
                                                             KOART = 'S'.

                               IF SY-SUBRC = 0.

                                  WA_FINAL1-GL_AMT = WA_BSEG-WRBTR.
                                  WA_FINAL1-GL_ACNT = WA_BSEG-HKONT.

                                  CLEAR WA_BSEG.
                               else.

                                  READ TABLE IT_BSEG INTO WA_BSEG WITH KEY BELNR = WA_FINAL1-BELNR
                                                                           KTOSL = 'FRL'
                                                                           KOART = 'S'.
                                              IF SY-SUBRC = 0.
                                                WA_FINAL1-GL_AMT = WA_BSEG-WRBTR.
                                                WA_FINAL1-GL_ACNT = WA_BSEG-HKONT.

                                                CLEAR WA_BSEG.
                                              else.

                                                READ TABLE IT_BSEG INTO WA_BSEG WITH KEY BELNR = WA_FINAL1-BELNR"added
                                                                          KTOSL = 'ANL'
                                                                          KOART = 'A'.
                                                          IF SY-SUBRC = 0.
                                                            WA_FINAL1-GL_AMT = WA_BSEG-WRBTR.
                                                            WA_FINAL1-GL_ACNT = WA_BSEG-HKONT.
                                                          endif.
                                                          CLEAR WA_BSEG.
                                                endif.
                                endif.
                endif.




    READ TABLE IT_BSEG INTO WA_BSEG WITH KEY BELNR = WA_FINAL1-BELNR
                            EBELN = ' '
                            KTOSL = 'DIF'
                            KOART = 'S'.

    IF SY-SUBRC = 0.
      WA_FINAL1-DF_AMT = WA_BSEG-WRBTR  .
    ENDIF.

    WA_FINAL1-GROSS = WA_FINAL1-GROSS - WA_FINAL1-GL_AMT.

    SELECT  TXT50
      UP TO 1 ROWS FROM SKAT
      INTO WA_FINAL1-GL_TEXT
      WHERE SPRAS = 'E'
      AND SAKNR = WA_FINAL1-GL_ACNT ORDER BY PRIMARY KEY.
    ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation

    SELECT  TXT20
    UP TO 1 ROWS FROM SKAT
    INTO WA_FINAL1-GL_TEXT20
    WHERE SPRAS = 'E'
    AND SAKNR = WA_FINAL1-GL_ACNT ORDER BY PRIMARY KEY.
    ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation
    CLEAR WA_MSEG.

if wa_final1-lfbnr is NOT INITIAL.

    READ TABLE it_data INTO wa_data with key lfbnr = wa_final1-lfbnr.

    IF  sy-subrc = 0.

      WA_FINAL1-CON_GL_ACNT = wa_data-hkont.

    ENDIF.

endif.


*    READ TABLE IT_MSEG01 INTO WA_MSEG01 WITH KEY MBLNR = WA_FINAL1-LFBNR.
*    IF SY-SUBRC = 0.
*      CLEAR WA_BKPF01.
*      READ TABLE IT_BKPF01 INTO WA_BKPF01 WITH KEY AWKEY = WA_MSEG-AWKEY.
*      IF SY-SUBRC = 0.
*        CLEAR WA_BSEG01.
*        READ TABLE IT_BSEG01 INTO WA_BSEG01 WITH KEY BELNR = WA_BKPF01-BELNR
*                             KTOSL = 'KBS'.
*        IF SY-SUBRC = 0.
*          WA_FINAL1-CON_GL_ACNT = WA_BSEG01-HKONT.
*        ELSE.
*          READ TABLE IT_BSEG01 INTO WA_BSEG01 WITH KEY BELNR = WA_BKPF01-BELNR
*                              KTOSL = 'BSX'.
*          IF SY-SUBRC = 0.
*            WA_FINAL1-CON_GL_ACNT = WA_BSEG01-HKONT.
*          ELSE.
*            READ TABLE IT_BSEG01 INTO WA_BSEG01 WITH KEY BELNR = WA_BKPF01-BELNR
*                            KTOSL = 'FRL'.
*            WA_FINAL1-CON_GL_ACNT = WA_BSEG01-HKONT.
*
*          ENDIF.
*
*        ENDIF.
*      ENDIF.
*      IF WA_FINAL1-CON_GL_ACNT IS INITIAL.
*        CLEAR WA_BSEG.
*        READ TABLE IT_BSEG01  INTO WA_BSEG01 WITH KEY BELNR = WA_BKPF01-BELNR
*                                 KTOSL = 'BSX'.
*        IF SY-SUBRC = 0.
*          WA_FINAL1-CON_GL_ACNT = WA_BSEG01-HKONT.
*        ENDIF.
*      ENDIF.
*    ELSE.
**      DATA:LV_MBLNR TYPE MBLNR,
**           LV_MJAHR TYPE MJAHR,
**           LV_AWKEY TYPE AWKEY.
**
**      CLEAR:LV_MBLNR,LV_MJAHR,LV_AWKEY.
**      SELECT SINGLE MBLNR MJAHR
**         FROM MSEG INTO (LV_MBLNR,LV_MJAHR)
**        WHERE LFBNR = WA_FINAL1-LFBNR.
**      CONCATENATE LV_MBLNR LV_MJAHR INTO LV_AWKEY.
*      READ TABLE IT_MSEG01 INTO WA_MSEG01 WITH KEY MBLNR = WA_FINAL1-LFBNR.
*      IF SY-SUBRC = 0.
*        CLEAR WA_BKPF01.
*        READ TABLE IT_BKPF INTO WA_BKPF WITH KEY AWKEY = WA_MSEG01-AWKEY.
*        IF SY-SUBRC = 0.
*          CLEAR WA_BSEG01.
*          READ TABLE IT_BSEG01 INTO WA_BSEG01 WITH KEY BELNR = WA_BKPF-BELNR
*                               KTOSL = 'KBS'.
*          IF SY-SUBRC = 0.
*            WA_FINAL1-CON_GL_ACNT = WA_BSEG01-HKONT.
*          ELSE.
*            CLEAR WA_BSEG.
*            READ TABLE IT_BSEG01 INTO WA_BSEG01 WITH KEY BELNR = WA_BKPF-BELNR
*                                KTOSL = 'BSX'.
*            IF SY-SUBRC = 0.
*              WA_FINAL1-CON_GL_ACNT = WA_BSEG01-HKONT.
*            ELSE.
*              CLEAR WA_BSEG.
*              READ TABLE IT_BSEG01 INTO WA_BSEG01 WITH KEY BELNR = WA_BKPF-BELNR
*                             KTOSL = 'FRL'.
*              WA_FINAL1-CON_GL_ACNT = WA_BSEG01-HKONT.
*            ENDIF.
*          ENDIF.
*        ENDIF.
*      ENDIF.



*      IF WA_FINAL1-CON_GL_ACNT IS INITIAL.
*        CLEAR WA_BSEG.
*        READ TABLE IT_BSEG  INTO WA_BSEG WITH KEY BELNR = WA_BKPF01-BELNR
*                                 KTOSL = 'BSX'.
*        IF SY-SUBRC = 0.
*          WA_FINAL1-CON_GL_ACNT = WA_BSEG01-HKONT.
*        ENDIF.
*      ENDIF.
*    ENDIF.
    IF NOT WA_FINAL1-CON_GL_ACNT IS INITIAL.
      SELECT TXT50
        UP TO 1 ROWS FROM SKAT
        INTO WA_FINAL1-CON_GL_TEXT
        WHERE SPRAS = 'E'
        AND SAKNR = WA_FINAL1-CON_GL_ACNT ORDER BY PRIMARY KEY.
      ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation
      SELECT TXT20
         UP TO 1 ROWS FROM SKAT
         INTO WA_FINAL1-CON_GL_TEXT20
         WHERE SPRAS = 'E'
         AND SAKNR = WA_FINAL1-CON_GL_ACNT ORDER BY PRIMARY KEY.
      ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation
    ENDIF.
    SELECT KTEXT UP TO 1 ROWS FROM CEPCT
        INTO WA_FINAL1-PRCT_TEXT
       WHERE SPRAS = 'E'
        AND  PRCTR = WA_FINAL1-PRCTR ORDER BY PRIMARY KEY.
    ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation

    MODIFY IT_FINAL1 FROM WA_FINAL1 TRANSPORTING DMBTR WRBTR GROSS GL_AMT GL_ACNT DF_AMT GL_TEXT GL_TEXT20 CON_GL_ACNT CON_GL_TEXT CON_GL_TEXT20 PRCT_TEXT.
    CLEAR :WA_FINAL1.
  ENDLOOP.

  LOOP AT IT_FINAL1 INTO WA_FINAL1."VAT_VAL

    IF WA_FINAL1-RMDEDRATE IS NOT INITIAL.

      VAT_QTY = ( WA_FINAL1-EXBAS + WA_FINAL1-BED + WA_FINAL1-ECS + WA_FINAL1-HCESS + WA_FINAL1-ADDBED ) / 100 * WA_FINAL1-RMDEDRATE.

      WA_FINAL1-VAT_VAL = VAT_QTY .

      MODIFY IT_FINAL1 FROM WA_FINAL1 TRANSPORTING VAT_VAL.
      CLEAR WA_FINAL1.

    ENDIF.

    IF WA_FINAL1-CSTRATE IS NOT INITIAL.


      CST_QTY = ( ( WA_FINAL1-EXBAS + WA_FINAL1-BED + WA_FINAL1-ECS + WA_FINAL1-HCESS + WA_FINAL1-ADDBED ) / 100 ) * ( WA_FINAL1-CSTRATE ).

      WA_FINAL1-CST_VAL =  CST_QTY.

      MODIFY IT_FINAL1 FROM WA_FINAL1 TRANSPORTING CST_VAL.
      CLEAR WA_FINAL1.

    ENDIF.

    FIN_TOTQTY = ( WA_FINAL1-EXBAS + WA_FINAL1-BED + WA_FINAL1-ECS + WA_FINAL1-HCESS + WA_FINAL1-ADDBED ) .

    WA_FINAL1-PUR_VAL = FIN_TOTQTY.

    MODIFY IT_FINAL1 FROM WA_FINAL1 TRANSPORTING PUR_VAL.

    CLEAR WA_FINAL1.

    FIN_TOTQTY1 = ( WA_FINAL1-EXBAS + WA_FINAL1-BED + WA_FINAL1-ECS + WA_FINAL1-HCESS + WA_FINAL1-ADDBED ) + ( WA_FINAL1-VAT_VAL + WA_FINAL1-CST_VAL ).

    WA_FINAL1-PUR_VAL1 = FIN_TOTQTY1.

    MODIFY IT_FINAL1 FROM WA_FINAL1 TRANSPORTING PUR_VAL1.

    CLEAR: WA_FINAL1 , WA_BKPF01, WA_MSEG01 , wa_data.


  ENDLOOP.


  PERFORM F_LISTHEADER.
  PERFORM F_FIELDCATALOG.
  PERFORM F_LAYOUT.
  PERFORM F_DISPLAYGRID.

  else.

    MESSAGE 'NO RECORD EXISTS..................................' TYPE 'E'.

  endif.


ENDFORM.                    " PROCESS_DATA
*&---------------------------------------------------------------------*
*&      Form  F_LISTHEADER
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

  IF P_CB EQ 'X'.

    T_LISTHEADER-TYP = 'H'.
    T_LISTHEADER-KEY = ' '.
    T_LISTHEADER-INFO = 'C-Form Report'.
    APPEND T_LISTHEADER.

  ELSE.

    T_LISTHEADER-TYP = 'H'.
    T_LISTHEADER-KEY = ' '.
    T_LISTHEADER-INFO = 'Purchase Register'.
    APPEND T_LISTHEADER.

  ENDIF.


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

ENDFORM.                    " F_LISTHEADER
*&---------------------------------------------------------------------*
*&      Form  F_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_FIELDCATALOG .


  IF P_CB EQ 'X'.


    T_FIELDCATALOG-FIELDNAME = 'LIFNR'.
    T_FIELDCATALOG-SELTEXT_L = 'Vendor'.
    T_FIELDCATALOG-SELTEXT_S = 'Vendor'.
    T_FIELDCATALOG-OUTPUTLEN = '10'.
    T_FIELDCATALOG-COL_POS   = 1.
    T_FIELDCATALOG-NO_ZERO = 'X'.
    T_FIELDCATALOG-KEY = 'X'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'NAME1'.
    T_FIELDCATALOG-SELTEXT_L = 'Name'.
    T_FIELDCATALOG-SELTEXT_S = 'Name'.
    T_FIELDCATALOG-KEY = 'X'.
    T_FIELDCATALOG-COL_POS   = 2.
    T_FIELDCATALOG-OUTPUTLEN = '35'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'NAME3'.
    T_FIELDCATALOG-SELTEXT_L = 'Alias Name'.
    T_FIELDCATALOG-SELTEXT_S = 'Alias Name'.
    T_FIELDCATALOG-KEY = 'X'.
    T_FIELDCATALOG-COL_POS   = 3.
    T_FIELDCATALOG-OUTPUTLEN = '35'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'GL_DESC_TXT20'.
    T_FIELDCATALOG-SELTEXT_L = 'GL short text'.
    T_FIELDCATALOG-SELTEXT_S = 'GL short text'.
    T_FIELDCATALOG-KEY = 'X'.
    T_FIELDCATALOG-COL_POS   = 4.
    T_FIELDCATALOG-OUTPUTLEN = '35'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.


    T_FIELDCATALOG-FIELDNAME = 'ORT01'.
    T_FIELDCATALOG-SELTEXT_L = 'Vendor Cway'.
    T_FIELDCATALOG-SELTEXT_S = 'Vendor Cway'.
    T_FIELDCATALOG-OUTPUTLEN = '10'.
    T_FIELDCATALOG-COL_POS   = 5.
    T_FIELDCATALOG-KEY = 'X'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'REGIO_DESC'.
    T_FIELDCATALOG-SELTEXT_L = 'Vendor State Desc'.
    T_FIELDCATALOG-SELTEXT_S = 'Vendor State Desc'.
    T_FIELDCATALOG-OUTPUTLEN = '15'.
    T_FIELDCATALOG-COL_POS   = 6.
    T_FIELDCATALOG-KEY = 'X'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'REGIO'.
    T_FIELDCATALOG-SELTEXT_L = 'Vendor State'.
    T_FIELDCATALOG-SELTEXT_S = 'Vendor State'.
    T_FIELDCATALOG-OUTPUTLEN = '10'.
    T_FIELDCATALOG-COL_POS   = 7.
    T_FIELDCATALOG-KEY = 'X'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.


    T_FIELDCATALOG-FIELDNAME = 'J_1ILSTNO'.
    T_FIELDCATALOG-SELTEXT_L = 'TIN No'.
    T_FIELDCATALOG-SELTEXT_S = 'TIN No'.
    T_FIELDCATALOG-OUTPUTLEN = '10'.
    T_FIELDCATALOG-COL_POS   = 8.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'CC1'.
    T_FIELDCATALOG-SELTEXT_L = 'Commodity_Code'.
    T_FIELDCATALOG-SELTEXT_S = 'Commodity_Code'.
    T_FIELDCATALOG-COL_POS   = 9.
    APPEND  T_FIELDCATALOG.
    CLEAR  T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'XBLNR'.
    T_FIELDCATALOG-SELTEXT_L = 'Vendor Inv No'.
    T_FIELDCATALOG-SELTEXT_S = 'Vendor Inv No'.
    T_FIELDCATALOG-COL_POS   = 10.
    T_FIELDCATALOG-OUTPUTLEN = '10'.
*  T_FIELDCATALOG-KEY = 'X'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'BLDAT'.
    T_FIELDCATALOG-SELTEXT_L = 'Doc Date'.
    T_FIELDCATALOG-SELTEXT_S = 'Doc Date'.
    T_FIELDCATALOG-COL_POS   = 11.
    T_FIELDCATALOG-OUTPUTLEN = '10'.
*  T_FIELDCATALOG-KEY = 'X'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.


    T_FIELDCATALOG-FIELDNAME = 'EBELN'.
    T_FIELDCATALOG-SELTEXT_L = 'PO Number'.
    T_FIELDCATALOG-SELTEXT_S = 'PO Number'.
    T_FIELDCATALOG-COL_POS   = 12.
    T_FIELDCATALOG-OUTPUTLEN = '10'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'BEDAT'.
    T_FIELDCATALOG-SELTEXT_L = 'PO Date'.
    T_FIELDCATALOG-SELTEXT_S = 'PO Date'.
    T_FIELDCATALOG-COL_POS   = 13.
    T_FIELDCATALOG-OUTPUTLEN = '10'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'PUR_VAL'.
    T_FIELDCATALOG-SELTEXT_L = 'Purchase BeforeTax Value'.
    T_FIELDCATALOG-SELTEXT_S = 'Purchase BeforeTax Value'.
    T_FIELDCATALOG-COL_POS   = 14.
    T_FIELDCATALOG-DO_SUM = 'X'.
    APPEND  T_FIELDCATALOG.
    CLEAR  T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'RMDEDRATE'.
    T_FIELDCATALOG-SELTEXT_L = 'VAT Rate'.
    T_FIELDCATALOG-SELTEXT_S = 'VAT Rate'.
    T_FIELDCATALOG-COL_POS   = 15.
    T_FIELDCATALOG-OUTPUTLEN = '15'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'CSTRATE'.
    T_FIELDCATALOG-SELTEXT_L = 'CST Rate'.
    T_FIELDCATALOG-SELTEXT_S = 'CST Rate'.
    T_FIELDCATALOG-COL_POS   = 16.
    T_FIELDCATALOG-OUTPUTLEN = '8'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.


    T_FIELDCATALOG-FIELDNAME = 'VAT_VAL'.
    T_FIELDCATALOG-SELTEXT_L = 'VAT Value'.
    T_FIELDCATALOG-SELTEXT_S = 'VAT Value'.
    T_FIELDCATALOG-COL_POS   = 17.
    T_FIELDCATALOG-DO_SUM = 'X'.
    APPEND  T_FIELDCATALOG.
    CLEAR  T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'CST_VAL'.
    T_FIELDCATALOG-SELTEXT_L = 'CST Value'.
    T_FIELDCATALOG-SELTEXT_S = 'CST Value'.
    T_FIELDCATALOG-COL_POS   = 18.
    T_FIELDCATALOG-DO_SUM = 'X'.
    APPEND  T_FIELDCATALOG.
    CLEAR  T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'PUR_VAL1'.
    T_FIELDCATALOG-SELTEXT_L = 'Total_Pur_Value'.
    T_FIELDCATALOG-SELTEXT_S = 'Total_Pur_Value'.
    T_FIELDCATALOG-COL_POS   = 19.
    T_FIELDCATALOG-DO_SUM = 'X'.
    APPEND  T_FIELDCATALOG.
    CLEAR  T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'CATE'.
    T_FIELDCATALOG-SELTEXT_L = 'Category'.
    T_FIELDCATALOG-SELTEXT_S = 'Category'.
    T_FIELDCATALOG-COL_POS   = 20.
    T_FIELDCATALOG-OUTPUTLEN = '20'.
    APPEND  T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'QTY'.
    T_FIELDCATALOG-SELTEXT_L = 'Quantity'.
    T_FIELDCATALOG-SELTEXT_S = 'Quantity'.
    T_FIELDCATALOG-COL_POS   = 21.
    T_FIELDCATALOG-OUTPUTLEN = '7'.
    T_FIELDCATALOG-DO_SUM = 'X'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'MEINS'.
    T_FIELDCATALOG-SELTEXT_L = 'Base Unit1'.
    T_FIELDCATALOG-SELTEXT_S = 'Base Unit1'.
    T_FIELDCATALOG-COL_POS   = 22.
    APPEND  T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'FRBNR'.
    T_FIELDCATALOG-SELTEXT_L = 'Lorry_Receipt'.
    T_FIELDCATALOG-SELTEXT_S = 'Lorry_Receipt'.
    T_FIELDCATALOG-OUTPUTLEN = '10'.
    T_FIELDCATALOG-COL_POS   = 23.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'LFBNR'.
    T_FIELDCATALOG-SELTEXT_L = 'GRR Number'.
    T_FIELDCATALOG-SELTEXT_S = 'GRR Number'.
    T_FIELDCATALOG-COL_POS   = 24.
    T_FIELDCATALOG-OUTPUTLEN = '10'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'BUDAT'.
    T_FIELDCATALOG-SELTEXT_L = 'GRR Date'.
    T_FIELDCATALOG-SELTEXT_S = 'GRR Date'.
    T_FIELDCATALOG-COL_POS   = 25.
    T_FIELDCATALOG-OUTPUTLEN = '10'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'MATNR'.
    T_FIELDCATALOG-SELTEXT_L = 'Material'.
    T_FIELDCATALOG-SELTEXT_S = 'Material'.
    T_FIELDCATALOG-COL_POS   = 26.
    T_FIELDCATALOG-OUTPUTLEN = '18'.
    T_FIELDCATALOG-NO_ZERO = 'X'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'MAKTX'.
    T_FIELDCATALOG-SELTEXT_L = 'Material Description'.
    T_FIELDCATALOG-SELTEXT_S = 'Material Description'.
    T_FIELDCATALOG-COL_POS   = 27.
    T_FIELDCATALOG-OUTPUTLEN = '40'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'BKBEZ'.
    T_FIELDCATALOG-SELTEXT_L = 'Val Class Desp'.
    T_FIELDCATALOG-SELTEXT_S = 'Val Class Desp'.
    T_FIELDCATALOG-COL_POS   = 28.
*  t_fieldcatalog-outputlen = '10'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'RATE'.
    T_FIELDCATALOG-SELTEXT_L = 'Rate'.
    T_FIELDCATALOG-SELTEXT_S = 'Rate'.
    T_FIELDCATALOG-COL_POS   = 29.
    APPEND  T_FIELDCATALOG.
    CLEAR  T_FIELDCATALOG.


  ELSE.



    T_FIELDCATALOG-FIELDNAME = 'BELNR'.
    T_FIELDCATALOG-SELTEXT_L = 'Document Number'.
    T_FIELDCATALOG-SELTEXT_S = 'Document Number'.
    T_FIELDCATALOG-COL_POS   = 1.
    T_FIELDCATALOG-OUTPUTLEN = '15'.
    T_FIELDCATALOG-KEY = 'X'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'BLDAT'.
    T_FIELDCATALOG-SELTEXT_L = 'Doc Date'.
    T_FIELDCATALOG-SELTEXT_S = 'Doc Date'.
    T_FIELDCATALOG-COL_POS   = 2.
    T_FIELDCATALOG-OUTPUTLEN = '10'.
    T_FIELDCATALOG-KEY = 'X'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'XBLNR'.
    T_FIELDCATALOG-SELTEXT_L = 'Vendor Inv No'.
    T_FIELDCATALOG-SELTEXT_S = 'Vendor Inv No'.
    T_FIELDCATALOG-COL_POS   = 3.
    T_FIELDCATALOG-OUTPUTLEN = '10'.
    T_FIELDCATALOG-KEY = 'X'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'G_BELNR'.
    T_FIELDCATALOG-SELTEXT_L = 'Inv No'.
    T_FIELDCATALOG-SELTEXT_S = 'Inv No'.
    T_FIELDCATALOG-COL_POS   = 4.
    T_FIELDCATALOG-KEY = 'X'.
    T_FIELDCATALOG-OUTPUTLEN = '10'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.


    T_FIELDCATALOG-FIELDNAME = 'POSTDAT'.
    T_FIELDCATALOG-SELTEXT_L = 'Posting Date'.
    T_FIELDCATALOG-SELTEXT_S = 'Posting Date'.
    T_FIELDCATALOG-COL_POS   = 5.
    T_FIELDCATALOG-OUTPUTLEN = '10'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'BUZEI1'.
    T_FIELDCATALOG-SELTEXT_L = 'Invoice Line Item'.
    T_FIELDCATALOG-SELTEXT_S = 'Invoice Line Item'.
    T_FIELDCATALOG-COL_POS   = 6.
    T_FIELDCATALOG-OUTPUTLEN = '7'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.


    T_FIELDCATALOG-FIELDNAME = 'BUKRS'.
    T_FIELDCATALOG-SELTEXT_L = 'Company Code'.
    T_FIELDCATALOG-SELTEXT_S = 'Company Code'.
    T_FIELDCATALOG-COL_POS   = 7.
    T_FIELDCATALOG-OUTPUTLEN = '10'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'DOC_TYPE'.
    T_FIELDCATALOG-SELTEXT_L = 'Document Type'.
    T_FIELDCATALOG-SELTEXT_S = 'Company Code'.
    T_FIELDCATALOG-COL_POS   = 8.
    T_FIELDCATALOG-OUTPUTLEN = '15'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'WAERS'.
    T_FIELDCATALOG-SELTEXT_L = 'Currency'.
    T_FIELDCATALOG-SELTEXT_S = 'Currency'.
    T_FIELDCATALOG-COL_POS   = 9.
    T_FIELDCATALOG-OUTPUTLEN = '15'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'KURSF'.
    T_FIELDCATALOG-SELTEXT_L = 'Exchange Rate'.
    T_FIELDCATALOG-SELTEXT_S = 'Exchange Rate'.
    T_FIELDCATALOG-COL_POS   = 10.
    T_FIELDCATALOG-OUTPUTLEN = '15'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'GJAHR'.
    T_FIELDCATALOG-SELTEXT_L = 'Fiscal Year'.
    T_FIELDCATALOG-SELTEXT_S = 'Fiscal Year'.
    T_FIELDCATALOG-COL_POS   = 11.
    T_FIELDCATALOG-OUTPUTLEN = '10'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'LIFNR'.
    T_FIELDCATALOG-SELTEXT_L = 'Vendor'.
    T_FIELDCATALOG-SELTEXT_S = 'Vendor'.
    T_FIELDCATALOG-COL_POS   = 12.
    T_FIELDCATALOG-OUTPUTLEN = '10'.
    T_FIELDCATALOG-NO_ZERO = 'X'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'REGIO'.
    T_FIELDCATALOG-SELTEXT_L = 'Vendor State'.
    T_FIELDCATALOG-SELTEXT_S = 'Vendor State'.
    T_FIELDCATALOG-COL_POS   = 13.
    T_FIELDCATALOG-OUTPUTLEN = '10'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'REGIO_DESC'.
    T_FIELDCATALOG-SELTEXT_L = 'Vendor State Desc'.
    T_FIELDCATALOG-SELTEXT_S = 'Vendor State Desc'.
    T_FIELDCATALOG-COL_POS   = 14.
    T_FIELDCATALOG-OUTPUTLEN = '15'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'ORT01'.
    T_FIELDCATALOG-SELTEXT_L = 'Vendor Cway'.
    T_FIELDCATALOG-SELTEXT_S = 'Vendor Cway'.
    T_FIELDCATALOG-COL_POS   = 15.
    T_FIELDCATALOG-OUTPUTLEN = '10'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'NAME1'.
    T_FIELDCATALOG-SELTEXT_L = 'Name'.
    T_FIELDCATALOG-SELTEXT_S = 'Name'.
    T_FIELDCATALOG-COL_POS   = 16.
    T_FIELDCATALOG-OUTPUTLEN = '35'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.


    T_FIELDCATALOG-FIELDNAME = 'NAME3'.
    T_FIELDCATALOG-SELTEXT_L = 'Alias Name'.
    T_FIELDCATALOG-SELTEXT_S = 'Alias Name'.
    T_FIELDCATALOG-COL_POS   = 17.
    T_FIELDCATALOG-OUTPUTLEN = '35'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.


    T_FIELDCATALOG-FIELDNAME = 'HKONT'.
    T_FIELDCATALOG-SELTEXT_L = 'GL A/C'.
    T_FIELDCATALOG-SELTEXT_S = 'GL A/C'.
    T_FIELDCATALOG-COL_POS   = 18.
    T_FIELDCATALOG-OUTPUTLEN = '10'.
    T_FIELDCATALOG-NO_ZERO = 'X'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.


    T_FIELDCATALOG-FIELDNAME = 'GL_DESC_TXT20'.
    T_FIELDCATALOG-SELTEXT_L = 'GL Short Text'.
    T_FIELDCATALOG-SELTEXT_S = 'GL Short Text'.
    T_FIELDCATALOG-COL_POS   = 19.
    T_FIELDCATALOG-OUTPUTLEN = '35'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.



    T_FIELDCATALOG-FIELDNAME = 'LFBNR'.
    T_FIELDCATALOG-SELTEXT_L = 'GRR Number'.
    T_FIELDCATALOG-SELTEXT_S = 'GRR Number'.
    T_FIELDCATALOG-COL_POS   = 20.
    T_FIELDCATALOG-OUTPUTLEN = '10'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'BUDAT'.
    T_FIELDCATALOG-SELTEXT_L = 'GRR Date'.
    T_FIELDCATALOG-SELTEXT_S = 'GRR Date'.
    T_FIELDCATALOG-COL_POS   = 21.
    T_FIELDCATALOG-OUTPUTLEN = '10'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'MWSKZ'.
    T_FIELDCATALOG-SELTEXT_L = 'Tax Code'.
    T_FIELDCATALOG-SELTEXT_S = 'Tax Code'.
    T_FIELDCATALOG-COL_POS   = 22.
    T_FIELDCATALOG-OUTPUTLEN = '7'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'MATNR'.
    T_FIELDCATALOG-SELTEXT_L = 'Material'.
    T_FIELDCATALOG-SELTEXT_S = 'Material'.
    T_FIELDCATALOG-COL_POS   = 23.
    T_FIELDCATALOG-OUTPUTLEN = '18'.
    T_FIELDCATALOG-NO_ZERO = 'X'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'MAKTX'.
    T_FIELDCATALOG-SELTEXT_L = 'Material Description'.
    T_FIELDCATALOG-SELTEXT_S = 'Material Description'.
    T_FIELDCATALOG-COL_POS   = 24.
    T_FIELDCATALOG-OUTPUTLEN = '40'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'CHAPID'.
    T_FIELDCATALOG-SELTEXT_L = 'Chapter ID'.
    T_FIELDCATALOG-SELTEXT_S = 'Chapter ID'.
    T_FIELDCATALOG-COL_POS   = 25.
    T_FIELDCATALOG-OUTPUTLEN = '18'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'EBELN'.
    T_FIELDCATALOG-SELTEXT_L = 'PO Number'.
    T_FIELDCATALOG-SELTEXT_S = 'PO Number'.
    T_FIELDCATALOG-COL_POS   = 26.
    T_FIELDCATALOG-OUTPUTLEN = '10'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.


    T_FIELDCATALOG-FIELDNAME = 'EBELP'.
    T_FIELDCATALOG-SELTEXT_L = 'PO Line Item'.
    T_FIELDCATALOG-SELTEXT_S = 'PO Line Item'.
    T_FIELDCATALOG-COL_POS   = 27.
    T_FIELDCATALOG-OUTPUTLEN = '7'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'QTY'.
    T_FIELDCATALOG-SELTEXT_L = 'Quantity'.
    T_FIELDCATALOG-SELTEXT_S = 'Quantity'.
    T_FIELDCATALOG-COL_POS   = 28.
    T_FIELDCATALOG-OUTPUTLEN = '7'.
    T_FIELDCATALOG-DO_SUM = 'X'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'DMBTR'.
    T_FIELDCATALOG-SELTEXT_L = 'Doc Currency Amount'.
    T_FIELDCATALOG-SELTEXT_S = 'Doc Currency Amount'.
    T_FIELDCATALOG-COL_POS   = 29.
    T_FIELDCATALOG-OUTPUTLEN = '15'.
    T_FIELDCATALOG-DO_SUM = 'X'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'WRBTR'.
    T_FIELDCATALOG-SELTEXT_L = 'Local. Curr Amount'.
    T_FIELDCATALOG-SELTEXT_S = 'Local. Curr Amount'.
    T_FIELDCATALOG-COL_POS   = 30.
    T_FIELDCATALOG-DO_SUM = 'X'.

    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'BNKAN'.
    T_FIELDCATALOG-SELTEXT_L = 'Unplan Cost Doc Curr'.
    T_FIELDCATALOG-SELTEXT_S = 'Unplan Cost Doc Curr'.
    T_FIELDCATALOG-COL_POS   = 31.
    T_FIELDCATALOG-DO_SUM = 'X'.

    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'BNKANLOC'.
    T_FIELDCATALOG-SELTEXT_L = 'Unplan Cost Loc Curr'.
    T_FIELDCATALOG-SELTEXT_S = 'Unplan Cost Loc Curr'.
    T_FIELDCATALOG-COL_POS   = 32.
    T_FIELDCATALOG-DO_SUM = 'X'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.


    T_FIELDCATALOG-FIELDNAME = 'FRGT'.
    T_FIELDCATALOG-SELTEXT_L = 'Freight Cost'.
    T_FIELDCATALOG-SELTEXT_S = 'Freight Cost'.
    T_FIELDCATALOG-COL_POS   = 33.
    T_FIELDCATALOG-OUTPUTLEN = '12'.
    T_FIELDCATALOG-DO_SUM = 'X'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'MISC'.
    T_FIELDCATALOG-SELTEXT_L = 'Misc.Chg.'.
    T_FIELDCATALOG-SELTEXT_S = 'Misc.Chg.'.
    T_FIELDCATALOG-COL_POS   = 34.
    T_FIELDCATALOG-OUTPUTLEN = '12'.
    T_FIELDCATALOG-DO_SUM = 'X'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'IMP_FREIGHT_VAL'.
    T_FIELDCATALOG-SELTEXT_L = 'Import freight'.
    T_FIELDCATALOG-SELTEXT_S = 'Import freight'.
    T_FIELDCATALOG-COL_POS   = 35.
    T_FIELDCATALOG-OUTPUTLEN = '12'.
    T_FIELDCATALOG-DO_SUM = 'X'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.



    T_FIELDCATALOG-FIELDNAME = 'EXBAS'.
    T_FIELDCATALOG-SELTEXT_L = 'Assessable Value'.
    T_FIELDCATALOG-SELTEXT_S = 'Assessable Value'.
    T_FIELDCATALOG-COL_POS   = 36.
    T_FIELDCATALOG-OUTPUTLEN = '10'.
    T_FIELDCATALOG-DO_SUM = 'X'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'VATBAS'.
    T_FIELDCATALOG-SELTEXT_L = 'VAT/CST Tax Base'.
    T_FIELDCATALOG-SELTEXT_S = 'VAT/CST Tax Base'.
    T_FIELDCATALOG-COL_POS   = 37.
    T_FIELDCATALOG-DO_SUM = 'X'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'SERBAS'.
    T_FIELDCATALOG-SELTEXT_L = 'Service Tax Base'.
    T_FIELDCATALOG-SELTEXT_S = 'Service Tax Base'.
    T_FIELDCATALOG-COL_POS   = 38.
    T_FIELDCATALOG-DO_SUM = 'X'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'BED'.
    T_FIELDCATALOG-SELTEXT_L = 'Basic Excise Duty'.
    T_FIELDCATALOG-SELTEXT_S = 'Basic Excise Duty'.
    T_FIELDCATALOG-COL_POS   = 39.
    T_FIELDCATALOG-OUTPUTLEN = '15'.
    T_FIELDCATALOG-DO_SUM = 'X'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'ADDBED'.
    T_FIELDCATALOG-SELTEXT_L = 'Addl.Excise Duty'.
    T_FIELDCATALOG-SELTEXT_S = 'Addl.Excise Duty'.
    T_FIELDCATALOG-COL_POS   = 40.
    T_FIELDCATALOG-OUTPUTLEN = '15'.
    T_FIELDCATALOG-DO_SUM = 'X'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'BEDRATE'.
    T_FIELDCATALOG-SELTEXT_L = 'BED Rate'.
    T_FIELDCATALOG-SELTEXT_S = 'BED Rate'.
    T_FIELDCATALOG-COL_POS   = 41.
    T_FIELDCATALOG-OUTPUTLEN = '8'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'ECS'.
    T_FIELDCATALOG-SELTEXT_L = 'Ecess on BED'.
    T_FIELDCATALOG-SELTEXT_S = 'Ecess on BED'.
    T_FIELDCATALOG-COL_POS   = 42.
    T_FIELDCATALOG-OUTPUTLEN = '15'.
    T_FIELDCATALOG-DO_SUM = 'X'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'HCESS'.
    T_FIELDCATALOG-SELTEXT_L = 'HECess on BED'.
    T_FIELDCATALOG-SELTEXT_S = 'HECess on BED'.
    T_FIELDCATALOG-COL_POS   = 43.
    T_FIELDCATALOG-OUTPUTLEN = '15'.
    T_FIELDCATALOG-DO_SUM = 'X'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'RMDED'.
    T_FIELDCATALOG-SELTEXT_L = 'VAT Deductable'.
    T_FIELDCATALOG-SELTEXT_S = 'VAT Deductable'.
    T_FIELDCATALOG-COL_POS   = 44.
    T_FIELDCATALOG-OUTPUTLEN = '15'.
    T_FIELDCATALOG-DO_SUM = 'X'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'RMDEDRATE'.
    T_FIELDCATALOG-SELTEXT_L = 'VAT Rate'.
    T_FIELDCATALOG-SELTEXT_S = 'VAT Rate'.
    T_FIELDCATALOG-COL_POS   = 45.
    T_FIELDCATALOG-OUTPUTLEN = '15'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'RMNON'.
    T_FIELDCATALOG-SELTEXT_L = 'VAT Non-Deductable'.
    T_FIELDCATALOG-SELTEXT_S = 'VAT Non-Deductable'.
    T_FIELDCATALOG-COL_POS   = 46.
    T_FIELDCATALOG-OUTPUTLEN = '15'.
    T_FIELDCATALOG-DO_SUM = 'X'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'CGDED'.
    T_FIELDCATALOG-SELTEXT_L = 'Add. Tax Deductable'.
    T_FIELDCATALOG-SELTEXT_S = 'Add. Tax Deductable'.
    T_FIELDCATALOG-COL_POS   = 47.
    T_FIELDCATALOG-OUTPUTLEN = '15'.
    T_FIELDCATALOG-DO_SUM = 'X'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.


    T_FIELDCATALOG-FIELDNAME = 'CGDEDRATE'.
    T_FIELDCATALOG-SELTEXT_L = 'Add.Tax Rate'.
    T_FIELDCATALOG-SELTEXT_S = 'Add.Tax Rate'.
    T_FIELDCATALOG-COL_POS   = 48.
    T_FIELDCATALOG-OUTPUTLEN = '15'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'CGNON'.
    T_FIELDCATALOG-SELTEXT_L = 'Add. Tax Non-Deductable'.
    T_FIELDCATALOG-SELTEXT_S = 'Add. Tax Non-Deductable'.
    T_FIELDCATALOG-COL_POS   = 49.
    T_FIELDCATALOG-OUTPUTLEN = '15'.
    T_FIELDCATALOG-DO_SUM = 'X'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'CST'.
    T_FIELDCATALOG-SELTEXT_L = 'CST'.
    T_FIELDCATALOG-OUTPUTLEN = '15'.
    T_FIELDCATALOG-SELTEXT_S = 'CST'.
    T_FIELDCATALOG-COL_POS   = 50.
    T_FIELDCATALOG-DO_SUM = 'X'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'CSTRATE'.
    T_FIELDCATALOG-SELTEXT_L = 'CST Rate'.
    T_FIELDCATALOG-SELTEXT_S = 'CST Rate'.
    T_FIELDCATALOG-COL_POS   = 51.
    T_FIELDCATALOG-OUTPUTLEN = '8'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'SERTAX'.
    T_FIELDCATALOG-SELTEXT_L = 'Service Tax'.
    T_FIELDCATALOG-SELTEXT_S = 'Service Tax'.
    T_FIELDCATALOG-COL_POS   = 52.
    T_FIELDCATALOG-OUTPUTLEN = '15'.
    T_FIELDCATALOG-DO_SUM = 'X'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'ESERTAX'.
    T_FIELDCATALOG-SELTEXT_L = 'ECess on Ser Tax'.
    T_FIELDCATALOG-SELTEXT_S = 'ECess on Ser Tax'.
    T_FIELDCATALOG-COL_POS   = 53.
    T_FIELDCATALOG-OUTPUTLEN = '15'.
    T_FIELDCATALOG-DO_SUM = 'X'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'SHCESS'.
    T_FIELDCATALOG-SELTEXT_L = 'Sec Higher EduCess'.
    T_FIELDCATALOG-SELTEXT_S = 'Sec Higher EduCess'.
    T_FIELDCATALOG-COL_POS   = 54.
    T_FIELDCATALOG-OUTPUTLEN = '15'.
    T_FIELDCATALOG-DO_SUM = 'X'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'SHRATE'.
    T_FIELDCATALOG-SELTEXT_L = 'SHCess Rate'.
    T_FIELDCATALOG-SELTEXT_S = 'SHCess Rate'.
    T_FIELDCATALOG-COL_POS   = 55.
    T_FIELDCATALOG-OUTPUTLEN = '10'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'ACD'.
    T_FIELDCATALOG-SELTEXT_L = 'Additional Customs Duty'.
    T_FIELDCATALOG-SELTEXT_S = 'Additional Customs Duty'.
    T_FIELDCATALOG-COL_POS   = 56.
    T_FIELDCATALOG-OUTPUTLEN = '15'.
    T_FIELDCATALOG-DO_SUM = 'X'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'BASCUS'.
    T_FIELDCATALOG-SELTEXT_L = 'Basic Customs'.
    T_FIELDCATALOG-SELTEXT_S = 'Basic Customs'.
    T_FIELDCATALOG-COL_POS   = 57.
    T_FIELDCATALOG-OUTPUTLEN = '15'.
    T_FIELDCATALOG-DO_SUM = 'X'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'CVD'.
    T_FIELDCATALOG-SELTEXT_L = 'CVD'.
    T_FIELDCATALOG-SELTEXT_S = 'CVD'.
    T_FIELDCATALOG-COL_POS   = 58.
    T_FIELDCATALOG-OUTPUTLEN = '15'.
    T_FIELDCATALOG-DO_SUM = 'X'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'ECVD'.
    T_FIELDCATALOG-SELTEXT_L = 'ECess on CVD'.
    T_FIELDCATALOG-SELTEXT_S = 'ECess on CVD'.
    T_FIELDCATALOG-COL_POS   = 59.
    T_FIELDCATALOG-OUTPUTLEN = '15'.
    T_FIELDCATALOG-DO_SUM = 'X'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'HECVD'.
    T_FIELDCATALOG-SELTEXT_L = 'HECess on CVD'.
    T_FIELDCATALOG-SELTEXT_S = 'HECess on CVD'.
    T_FIELDCATALOG-COL_POS   = 60.
    T_FIELDCATALOG-OUTPUTLEN = '15'.
    T_FIELDCATALOG-DO_SUM = 'X'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'ECUSTOM'.
    T_FIELDCATALOG-SELTEXT_L = 'Customs ECess'.
    T_FIELDCATALOG-SELTEXT_S = 'Customs ECess'.
    T_FIELDCATALOG-COL_POS   = 61.
    T_FIELDCATALOG-OUTPUTLEN = '15'.
    T_FIELDCATALOG-DO_SUM = 'X'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'HECUSTOM'.
    T_FIELDCATALOG-SELTEXT_L = 'Customs HECess'.
    T_FIELDCATALOG-SELTEXT_S = 'Customs HECess'.
    T_FIELDCATALOG-COL_POS   = 62.
    T_FIELDCATALOG-OUTPUTLEN = '15'.
    T_FIELDCATALOG-DO_SUM = 'X'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'DISC'.
    T_FIELDCATALOG-SELTEXT_L = 'Discount'.
    T_FIELDCATALOG-SELTEXT_S = 'Discount'.
    T_FIELDCATALOG-COL_POS   = 63.
    T_FIELDCATALOG-DO_SUM = 'X'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'BED_N'.
    T_FIELDCATALOG-SELTEXT_L = 'BED Non-Ded'.
    T_FIELDCATALOG-SELTEXT_S = 'BED Non-Ded'.
    T_FIELDCATALOG-COL_POS   = 64.
    T_FIELDCATALOG-DO_SUM = 'X'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'CES_N'.
    T_FIELDCATALOG-SELTEXT_L = 'Cess Non-Ded'.
    T_FIELDCATALOG-SELTEXT_S = 'Cess Non-Ded'.
    T_FIELDCATALOG-COL_POS   = 65.
    T_FIELDCATALOG-DO_SUM = 'X'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'HCESS_N'.
    T_FIELDCATALOG-SELTEXT_L = 'H.Cess Non-Ded'.
    T_FIELDCATALOG-SELTEXT_S = 'H.Cess Non-Ded'.
    T_FIELDCATALOG-COL_POS   = 66.
    T_FIELDCATALOG-DO_SUM = 'X'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'AED_N'.
    T_FIELDCATALOG-SELTEXT_L = 'AED Non-Ded'.
    T_FIELDCATALOG-SELTEXT_S = 'AED Non-Ded'.
    T_FIELDCATALOG-COL_POS   = 67.
    T_FIELDCATALOG-DO_SUM = 'X'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.



    T_FIELDCATALOG-FIELDNAME = 'SGST'.
    T_FIELDCATALOG-SELTEXT_L = 'SGST amt'.
    T_FIELDCATALOG-SELTEXT_S = 'SGST amt'.
    T_FIELDCATALOG-COL_POS   = 68.
    T_FIELDCATALOG-OUTPUTLEN = '15'.
    T_FIELDCATALOG-DO_SUM = 'X'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'SGSTR'.
    T_FIELDCATALOG-SELTEXT_L = 'SGST %'.
    T_FIELDCATALOG-SELTEXT_S = 'SGST %'.
    T_FIELDCATALOG-COL_POS   = 69.
    T_FIELDCATALOG-OUTPUTLEN = '15'.
    T_FIELDCATALOG-DO_SUM = 'X'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'CGST'.
    T_FIELDCATALOG-SELTEXT_L = 'CGST amt'.
    T_FIELDCATALOG-SELTEXT_S = 'CGST amt'.
    T_FIELDCATALOG-COL_POS   = 70.
    T_FIELDCATALOG-OUTPUTLEN = '15'.
    T_FIELDCATALOG-DO_SUM = 'X'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'CGSTR'.
    T_FIELDCATALOG-SELTEXT_L = 'CGST %'.
    T_FIELDCATALOG-SELTEXT_S = 'CGST %'.
    T_FIELDCATALOG-COL_POS   = 71.
    T_FIELDCATALOG-OUTPUTLEN = '15'.
    T_FIELDCATALOG-DO_SUM = 'X'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.


    T_FIELDCATALOG-FIELDNAME = 'IGST'.
    T_FIELDCATALOG-SELTEXT_L = 'IGST amt'.
    T_FIELDCATALOG-SELTEXT_S = 'IGST amt'.
    T_FIELDCATALOG-COL_POS   = 72.
    T_FIELDCATALOG-OUTPUTLEN = '15'.
    T_FIELDCATALOG-DO_SUM = 'X'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'IGSTR'.
    T_FIELDCATALOG-SELTEXT_L = 'IGST %'.
    T_FIELDCATALOG-SELTEXT_S = 'IGST %'.
    T_FIELDCATALOG-COL_POS   = 73.
    T_FIELDCATALOG-OUTPUTLEN = '15'.
    T_FIELDCATALOG-DO_SUM = 'X'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'UGST'.
    T_FIELDCATALOG-SELTEXT_L = 'UGST amt'.
    T_FIELDCATALOG-SELTEXT_S = 'UGST amt'.
    T_FIELDCATALOG-COL_POS   = 74.
    T_FIELDCATALOG-OUTPUTLEN = '15'.
    T_FIELDCATALOG-DO_SUM = 'X'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'UGSTR'.
    T_FIELDCATALOG-SELTEXT_L = 'UGST %'.
    T_FIELDCATALOG-SELTEXT_S = 'UGST %'.
    T_FIELDCATALOG-COL_POS   = 75.
    T_FIELDCATALOG-OUTPUTLEN = '15'.
    T_FIELDCATALOG-DO_SUM = 'X'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'GROSS'.
    T_FIELDCATALOG-SELTEXT_L = 'Gross Total'.
    T_FIELDCATALOG-SELTEXT_S = 'Gross Total'.
    T_FIELDCATALOG-COL_POS   = 76.
    T_FIELDCATALOG-OUTPUTLEN = '15'.
    T_FIELDCATALOG-DO_SUM = 'X'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'BKLAS'.
    T_FIELDCATALOG-SELTEXT_L = 'Valuation Class'.
    T_FIELDCATALOG-SELTEXT_S = 'Valuation Class'.
    T_FIELDCATALOG-COL_POS   = 77.
    T_FIELDCATALOG-OUTPUTLEN = '10'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'BKBEZ'.
    T_FIELDCATALOG-SELTEXT_L = 'Val Class Desp'.
    T_FIELDCATALOG-SELTEXT_S = 'Val Class Desp'.
    T_FIELDCATALOG-COL_POS   = 78.
*  t_fieldcatalog-outputlen = '10'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'J_1ILSTNO'.
    T_FIELDCATALOG-SELTEXT_L = 'TIN No.'.
    T_FIELDCATALOG-SELTEXT_S = 'TIN No'.
    T_FIELDCATALOG-COL_POS   = 79.
    T_FIELDCATALOG-OUTPUTLEN = '10'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'J_1IPANNO'.
    T_FIELDCATALOG-SELTEXT_L = 'PAN No.'.
    T_FIELDCATALOG-SELTEXT_S = 'PAN No.'.
    T_FIELDCATALOG-COL_POS   = 80.
    T_FIELDCATALOG-OUTPUTLEN = '10'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'CSTNO'.
    T_FIELDCATALOG-SELTEXT_L = 'CST No.'.
    T_FIELDCATALOG-SELTEXT_S = 'CST No.'.
    T_FIELDCATALOG-COL_POS   = 81.
    T_FIELDCATALOG-OUTPUTLEN = '10'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'J_1ISERN '.
    T_FIELDCATALOG-SELTEXT_L = 'Service Tax Reg No.'.
    T_FIELDCATALOG-SELTEXT_S = 'Service Tax Reg No.'.
    T_FIELDCATALOG-COL_POS   = 82.
    T_FIELDCATALOG-OUTPUTLEN = '10'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'ECCNO'.
    T_FIELDCATALOG-SELTEXT_L = 'ECC No'.
    T_FIELDCATALOG-SELTEXT_S = 'ECC No'.
    T_FIELDCATALOG-COL_POS   = 83.
    T_FIELDCATALOG-OUTPUTLEN = '15'.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'WERKS'.
    T_FIELDCATALOG-SELTEXT_L = 'Plant'.
    T_FIELDCATALOG-SELTEXT_S = 'Plant'.
    T_FIELDCATALOG-COL_POS   = 84.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'PRCTR'.
    T_FIELDCATALOG-SELTEXT_L = 'Profit Center'.
    T_FIELDCATALOG-SELTEXT_S = 'Profit Center'.
    T_FIELDCATALOG-COL_POS   = 85.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'PRCT_TEXT'.
    T_FIELDCATALOG-SELTEXT_L = 'Profit Center Desc'.
    T_FIELDCATALOG-SELTEXT_S = 'Profit Center Desc'.
    T_FIELDCATALOG-COL_POS   = 86.
    APPEND T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'LAND1'.
    T_FIELDCATALOG-SELTEXT_L = 'Country'.
    T_FIELDCATALOG-SELTEXT_S = 'Country'.
    T_FIELDCATALOG-COL_POS   = 87.
    APPEND  T_FIELDCATALOG.
    CLEAR  T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'MATKL'.
    T_FIELDCATALOG-SELTEXT_L = 'Material Group'.
    T_FIELDCATALOG-SELTEXT_S = 'Material Group'.
    T_FIELDCATALOG-COL_POS   = 88.
    APPEND  T_FIELDCATALOG.
    CLEAR  T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'MTART'.
    T_FIELDCATALOG-SELTEXT_L = 'Material Type'.
    T_FIELDCATALOG-SELTEXT_S = 'Material Type'.
    T_FIELDCATALOG-COL_POS   = 89.
    APPEND  T_FIELDCATALOG.
    CLEAR  T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'WGBEZ'.
    T_FIELDCATALOG-SELTEXT_L = 'Mat Grp Desc'.
    T_FIELDCATALOG-SELTEXT_S = 'Mat Grp Desc'.
    T_FIELDCATALOG-COL_POS   = 90.
    APPEND  T_FIELDCATALOG.
    CLEAR  T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'MEINS'.
    T_FIELDCATALOG-SELTEXT_L = 'Base Unit1'.
    T_FIELDCATALOG-SELTEXT_S = 'Base Unit1'.
    T_FIELDCATALOG-COL_POS   = 91.
    APPEND  T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'BSTME'.
    T_FIELDCATALOG-SELTEXT_L = 'Base Unit2'.
    T_FIELDCATALOG-SELTEXT_S = 'Base Unit2'.
    T_FIELDCATALOG-COL_POS   = 92.
    APPEND  T_FIELDCATALOG.
    CLEAR T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'EKGRP'.
    T_FIELDCATALOG-SELTEXT_L = 'Purchase Group'.
    T_FIELDCATALOG-SELTEXT_S = 'Purchase Group'.
    T_FIELDCATALOG-COL_POS   = 93.
    APPEND  T_FIELDCATALOG.
    CLEAR  T_FIELDCATALOG.


    T_FIELDCATALOG-FIELDNAME = 'BSART'.
    T_FIELDCATALOG-SELTEXT_L = 'Order Type'.
    T_FIELDCATALOG-SELTEXT_S = 'Order Type'.
    T_FIELDCATALOG-COL_POS   = 94.
    APPEND  T_FIELDCATALOG.
    CLEAR  T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'DOC_TYP_DESC'.
    T_FIELDCATALOG-SELTEXT_L = 'Order Type Desc'.
    T_FIELDCATALOG-SELTEXT_S = 'Order Type Desc'.
    T_FIELDCATALOG-COL_POS   = 95.
    APPEND  T_FIELDCATALOG.
    CLEAR  T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'REVNO'.
    T_FIELDCATALOG-SELTEXT_L = 'Po version'.
    T_FIELDCATALOG-SELTEXT_S = 'Po version'.
    T_FIELDCATALOG-COL_POS   = 96.
    APPEND  T_FIELDCATALOG.
    CLEAR  T_FIELDCATALOG.


    T_FIELDCATALOG-FIELDNAME = 'NAME2'.
    T_FIELDCATALOG-SELTEXT_L = 'Plant Name'.
    T_FIELDCATALOG-SELTEXT_S = 'Plant Name'.
    T_FIELDCATALOG-COL_POS   = 97.
    APPEND  T_FIELDCATALOG.
    CLEAR  T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'RATE'.
    T_FIELDCATALOG-SELTEXT_L = 'Rate'.
    T_FIELDCATALOG-SELTEXT_S = 'Rate'.
    T_FIELDCATALOG-COL_POS   = 98.
    APPEND  T_FIELDCATALOG.
    CLEAR  T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'TCODE'.
    T_FIELDCATALOG-SELTEXT_L = 'T-Code'.
    T_FIELDCATALOG-SELTEXT_S = 'T-Code'.
    T_FIELDCATALOG-COL_POS   = 99.
    APPEND  T_FIELDCATALOG.
    CLEAR  T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'GL_ACNT'.
    T_FIELDCATALOG-SELTEXT_L = 'G/L Account'.
    T_FIELDCATALOG-SELTEXT_S = 'G/L Account'.
    T_FIELDCATALOG-COL_POS   = 100.
    APPEND  T_FIELDCATALOG.
    CLEAR  T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'GL_TEXT20'.
    T_FIELDCATALOG-SELTEXT_L = 'G/L Short Text'.
    T_FIELDCATALOG-SELTEXT_S = 'G/L Short Text'.
    T_FIELDCATALOG-COL_POS   = 101.
    APPEND  T_FIELDCATALOG.
    CLEAR  T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'GL_TEXT'.
    T_FIELDCATALOG-SELTEXT_L = 'G/L Account Text'.
    T_FIELDCATALOG-SELTEXT_S = 'G/L Account Text'.
    T_FIELDCATALOG-COL_POS   = 102.
    APPEND  T_FIELDCATALOG.
    CLEAR  T_FIELDCATALOG.


    T_FIELDCATALOG-FIELDNAME = 'GL_AMT'.
    T_FIELDCATALOG-SELTEXT_L = 'G/L Amt'.
    T_FIELDCATALOG-SELTEXT_S = 'G/L Amt'.
    T_FIELDCATALOG-COL_POS   = 103.
    APPEND  T_FIELDCATALOG.
    CLEAR  T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'CON_GL_ACNT'.
    T_FIELDCATALOG-SELTEXT_L = 'Con G/L Account'.
    T_FIELDCATALOG-SELTEXT_S = 'Con G/L Account'.
    T_FIELDCATALOG-COL_POS   = 104.
    APPEND  T_FIELDCATALOG.
    CLEAR  T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'CON_GL_TEXT20'.
    T_FIELDCATALOG-SELTEXT_L = 'Con G/L Account Short Text'.
    T_FIELDCATALOG-SELTEXT_S = 'Con G/L Account Short Text'.
    T_FIELDCATALOG-COL_POS   = 105.
    APPEND  T_FIELDCATALOG.
    CLEAR  T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'CON_GL_TEXT'.
    T_FIELDCATALOG-SELTEXT_L = 'Con G/L Account Text'.
    T_FIELDCATALOG-SELTEXT_S = 'Con G/L Account Text'.
    T_FIELDCATALOG-COL_POS   = 106.
    APPEND  T_FIELDCATALOG.
    CLEAR  T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'DF_AMT'.
    T_FIELDCATALOG-SELTEXT_L = 'Diff Amt'.
    T_FIELDCATALOG-SELTEXT_S = 'Diff Amt'.
    T_FIELDCATALOG-COL_POS   = 107.
    APPEND  T_FIELDCATALOG.
    CLEAR  T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'ANLN1'.
    T_FIELDCATALOG-SELTEXT_L = 'Asset No'.
    T_FIELDCATALOG-SELTEXT_S = 'Asset No'.
    T_FIELDCATALOG-COL_POS   = 108.
    APPEND  T_FIELDCATALOG.
    CLEAR  T_FIELDCATALOG.

    T_FIELDCATALOG-FIELDNAME = 'TXT50'.
    T_FIELDCATALOG-SELTEXT_L = 'Asset Desc'.
    T_FIELDCATALOG-SELTEXT_S = 'Asset Desc'.
    T_FIELDCATALOG-COL_POS   = 109.
    APPEND  T_FIELDCATALOG.
    CLEAR  T_FIELDCATALOG.

  ENDIF.

ENDFORM.                    " F_FIELDCATALOG
*&---------------------------------------------------------------------*
*&      Form  F_LAYOUT
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


ENDFORM.                    " F_LAYOUT
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
*&      Form  F_DISPLAYGRID
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



  ENDIF.

ENDFORM.                    " F_DISPLAYGRID
