*&---------------------------------------------------------------------*
*Include           MZ_FI_FB05_INCOMING_PAYMENTTOP
*&---------------------------------------------------------------------*
DATA: OK_CODE TYPE SY-UCOMM.

TYPES: BEGIN OF TY_LIST,
        FCODE TYPE CHAR6,
       END OF TY_LIST.

TYPES: BEGIN OF TY_TAB,
         MARK(1)     TYPE C,
         KUNNR       TYPE BSAD-KUNNR,        "Customer
         WRBTR       TYPE BSAD-WRBTR,        "Amount
         BELNR       TYPE BELNR_D,         "Bill/DR memo
       END OF TY_TAB.

TYPES: BEGIN OF TY_HEADER,
         BLDAT     TYPE BKPF-BLDAT,       "Document Date
         BUDAT     TYPE BKPF-BUDAT,       "Posting Date
         BLART     TYPE BKPF-BLART,       "Document type
         XBLNR     TYPE BKPF-XBLNR,       "Reference
         XTEXT     TYPE BSEG-SGTXT,       "added on 12/3
         NEWKO     TYPE SAKNR ,"rf05a-newko,      "Bank G/L
         BUKRS     TYPE BKPF-BUKRS,       "Company code
         WAERS     TYPE BKPF-WAERS,       "Currency
         GJAHR     TYPE BKPF-GJAHR,       "Fiscal year
         MONAT     TYPE BKPF-MONAT,       "period
         BKTXT     LIKE BKPF-BKTXT,       "doc text
         AUGTX     TYPE RF05A-AUGTX,      "doc clearing text
         VALUT(10) TYPE C,                "value date
         GLDESC    TYPE TXT50_SKAT,              " GL Desc
         SPLGL(1) TYPE C,
       END OF TY_HEADER.

TYPES: BEGIN OF TY_MESSAGE,
         KUNNR        TYPE BSAD-KUNNR,        "Customer
         WRBTR        TYPE BSAD-WRBTR,        "Amount
         BELNR        TYPE BELNR_D,         "Bill/DR memo
         MESS_TYPE(1) TYPE C,
         MESSAGE      TYPE BAPI_MSG,
         STATUS(4)    TYPE C,
         NAME1        TYPE NAME1_GP,          " Customer description
       END OF TY_MESSAGE.

TYPES: BEGIN OF TY_OPENITEMS.
INCLUDE  STRUCTURE BAPI3007_2.
TYPES:  DUE_DATE        TYPE NETDT,
        DUE_CASHD1      TYPE SK1DT,       "due date for cash discount1
        DUE_CASHD2      TYPE SK2DT,       " due date for cash discount2
        PREVIOUS(1)     TYPE C,
        ROOT_DOC        TYPE BELNR_D,
        ROOT_YEAR       TYPE GJAHR,
        ROOT_ITEM       TYPE BUZEI,
        ROOTDOCYEAR(17) TYPE C,
        DOCYEAR(17)     TYPE C,
        CPUDT TYPE CPUDT,                "Entry Date
        CPUTM TYPE CPUTM,                "Entry time
        CNOTE(1) TYPE C,                 " credit note process required
        END OF TY_OPENITEMS.

TYPES: BEGIN OF TY_AMNT,
         ROOT_DOC  TYPE BELNR_D,
         ROOT_YEAR TYPE GJAHR,
         ROOT_ITEM TYPE BUZEI,
         NET_AMNT  TYPE WRBTR,
         ROOT_AMNT TYPE WRBTR,
         DUE_DATE  TYPE NETDT,
         DISC      TYPE DZPROZ,
         DISC_AMNT TYPE WRBTR,
         COUNT_S   TYPE I,
         COUNT_H   TYPE I,
         COUNT     TYPE I,
         PREV(1)   TYPE C,
         ROOTDOCYEAR(17) TYPE C,
         DOCYEAR(17)     TYPE C,
         CPUDT TYPE CPUDT,                "Entry Date
         CPUTM TYPE CPUTM,                "Entry time
         CNOTE(1) TYPE C,  " credit note process required
         PROC(1) TYPE C,    " process in FB05.
       END OF TY_AMNT.

TYPES: BEGIN OF TY_KNA1,
        KUNNR TYPE KUNNR,
        NAME1 TYPE NAME1_GP,
       END OF TY_KNA1.

TYPES: BEGIN OF TY_BDC,
        FNAM1 TYPE FNAM_____4,
        FVAL1 TYPE BDC_FVAL,
        FNAM2 TYPE FNAM_____4,
        FVAL2 TYPE BDC_FVAL,
       END OF TY_BDC.

TYPES: BEGIN OF TY_CUSVEN,
        KUNNR TYPE KUNNR,
        LIFNR TYPE LIFNR,
       END OF TY_CUSVEN.

DATA: GS_HEADER TYPE TY_HEADER,
      GT_TAB    TYPE STANDARD TABLE OF TY_TAB,
      GS_TAB    TYPE TY_TAB,
      GT1_TAB    TYPE STANDARD TABLE OF TY_TAB,
      GS1_TAB    TYPE TY_TAB,
      GT_KNA1 TYPE STANDARD TABLE OF TY_KNA1,
      GS_KNA1 TYPE TY_KNA1.

DATA: GS_BAPIRETURN TYPE BAPIRETURN,
      GT_LINEITEMS  TYPE STANDARD TABLE OF BAPI3007_2,       " line items for customer
      GS_LINEITEMS  TYPE BAPI3007_2,                         " line items for customer
      GT_LINEITEMSV  TYPE STANDARD TABLE OF BAPI3008_2,     " line items for vendor
      GS_LINEITEMSV  TYPE BAPI3008_2,                        " line items for vendor
      GT_OPENITEMS  TYPE STANDARD TABLE OF TY_OPENITEMS,
      GS_OPENITEMS  TYPE TY_OPENITEMS,
      GS_OPENITEMS1 TYPE TY_OPENITEMS,
      GT_OPENITEMSC  TYPE STANDARD TABLE OF TY_OPENITEMS,  " table for credit notes
      GT_AMNT       TYPE STANDARD TABLE OF TY_AMNT,
      GS_AMNT       TYPE TY_AMNT,
      GT_MESSAGE    TYPE STANDARD TABLE OF TY_MESSAGE,
      GS_MESSAGE    TYPE TY_MESSAGE,
      GT1_MESSAGE    TYPE STANDARD TABLE OF TY_MESSAGE,
      GS1_MESSAGE    TYPE TY_MESSAGE,
      GT_LIST       TYPE STANDARD TABLE OF TY_LIST,
      GS_LIST       TYPE TY_LIST,
      GT_CUSVEN     TYPE STANDARD TABLE OF TY_CUSVEN,               " customer who is also a vendor
      GS_CUSVEN     TYPE TY_CUSVEN,
      GS_FAEDE      TYPE FAEDE,
      GT_BKPF       TYPE STANDARD TABLE OF BKPF,
      GS_BKPF       TYPE BKPF,
      GT_BDC        TYPE STANDARD TABLE OF TY_BDC,
      GT_BDCC        TYPE STANDARD TABLE OF TY_BDC,
      GS_BDC        TYPE TY_BDC,
      GS_BDCC        TYPE TY_BDC.

DATA :  GT_HITEMS  TYPE STANDARD TABLE OF BAPI3007_2,
        WA_HITEMS TYPE BAPI3007_2 .

DATA :  GT_SITEMS  TYPE STANDARD TABLE OF BAPI3007_2,
        WA_SITEMS TYPE BAPI3007_2 .

DATA :  GT1_HITEMS  TYPE STANDARD TABLE OF BAPI3007_2,
        WA1_HITEMS TYPE BAPI3007_2 .

DATA :  GT1_SITEMS  TYPE STANDARD TABLE OF BAPI3007_2,
        WA1_SITEMS TYPE BAPI3007_2 .

DATA: CHECK(1)     TYPE C,
      GV_PARTIAL(1) TYPE C,       " partial case scenario in fb05
      GV_EXCESS(1)  TYPE C,       " excess case scenario in fb05
      GV_EXACT(1)  TYPE C,       " exact case scenario in fb05
      GV_SCLEAR(1) TYPE C,        " Self clear.
      GV_VCLEAR(1) TYPE C,        " vendor as a customer clear.
      GV_NCLEAR(1) TYPE C,        " note/excess/un-addressed partial docs  Clear.
*      gv_resid(1) TYPE c,       " residual and amnt to be posted less than net amnt.
      GV_WRBTR     TYPE BSAD-WRBTR,
      GV_WRBTR1    TYPE BSAD-WRBTR,
      GV_WRBTRC    TYPE BSAD-WRBTR, "credit note amount.
      GV_WRBTRO    TYPE BSAD-WRBTR, "open items net amnt
      GV_PWRBTR    TYPE BSAD-WRBTR, "print amnt for credit cnote payment
      GV_VALUE     LIKE FTPOST-FVAL,
      GV_BUDAT LIKE FTPOST-FVAL,
      GV_BLDAT LIKE FTPOST-FVAL,
      GV_FWRBTR LIKE FTPOST-FVAL,
      GV_MODE      TYPE APQI-PUTACTIVE VALUE 'N',
      GV_CLASS     TYPE XUCLASS,          " user group
      GD_PERCENT    TYPE I,
      MCOBJEKT     LIKE DD23L-MCONAME,      " Matchcodeobjekt
      F4RC         LIKE SY-SUBRC,          " Return-Code
      SHLP_MF05A   TYPE SHLP_DESCR_T,
      SHLPNAME     LIKE DDSHDESCR-SHLPNAME,
      INTERFACE    LIKE DDSHIFACE,
      SELSTR       LIKE RF05A-NEWKO,        " Suchstring fr MC
      F4DYN        LIKE SY-DYNNR.          " Dynpronummer

CONSTANTS: GC_P TYPE CHAR1 VALUE 'P',     " Previous Partial payment done
           GC_R TYPE CHAR1 VALUE 'R',     " previous residual payment done
           GC_N TYPE CHAR1 VALUE 'N',     " No previous payment done
           GC_C TYPE CHAR1 VALUE 'C',     " Credit note
           GC_K TYPE CHAR1 VALUE 'K'.     " KR doc for customer also a vendor(similar to Credit note)

*       Batchinputdata of single transaction
DATA:   BDCDATA LIKE BDCDATA    OCCURS 0 WITH HEADER LINE.
*       messages of call transaction
DATA:   MESSTAB       LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE,
        GS_MESSTAB    TYPE BDCMSGCOLL,
        RETURN_VALUES LIKE DDSHRETVAL OCCURS 0 WITH HEADER LINE.

DATA:    BEGIN OF F4HLP OCCURS 1.
        INCLUDE STRUCTURE DYNPREAD.
DATA:    END OF F4HLP.

*&SPWIZARD: DECLARATION OF TABLECONTROL 'TC_9000' ITSELF
CONTROLS: TC_9000 TYPE TABLEVIEW USING SCREEN 9000.

*&SPWIZARD: LINES OF TABLECONTROL 'TC_9000'
DATA:     G_TC_9000_LINES  LIKE SY-LOOPC,
          WAA LIKE LINE OF TC_9000-COLS.

DATA: GV_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      GV_ALV       TYPE REF TO CL_GUI_ALV_GRID,
      GT_FCAT      TYPE LVC_T_FCAT,
      GS_FCAT      TYPE  LVC_S_FCAT.


TYPES : BEGIN OF GS_T074T,
                      KOART TYPE T074T-KOART,
                      SHBKZ TYPE T074T-SHBKZ,
                      LTEXT TYPE T074T-LTEXT,
                    END OF GS_T074T.
DATA : IT_T074T TYPE TABLE OF GS_T074T,
       WA_T074T TYPE GS_T074T.

DATA : LV1_COUNT TYPE I,
       LV2_COUNT TYPE I.

DATA : H_COUNT TYPE I,
       S_COUNT TYPE I.

DATA : L1_BELNR TYPE BSAD-BELNR,
       L2_BELNR TYPE BSAD-BELNR,
       L1_AMT TYPE BSAD-DMBTR,
       L2_AMT TYPE BSAD-DMBTR,
       BAL_AMT1 TYPE BSAD-DMBTR,
       BAL_AMT TYPE STRING.

DATA : LV_DATE1 TYPE SY-DATUM,
       LV_DATE TYPE CHAR10.
