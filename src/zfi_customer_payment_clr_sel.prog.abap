*&---------------------------------------------------------------------*
*& Include          ZMM_FERT_PRICE_UPDATE_ME12_SEL
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS:

    bldat     TYPE bkpf-bldat DEFAULT sy-datum,       "Document Date
    budat     TYPE bkpf-budat DEFAULT sy-datum,       "Posting Date
    blart     TYPE bkpf-blart DEFAULT 'DZ' NO-DISPLAY,       "Document type
    xblnr     TYPE bkpf-xblnr,       "Reference
    newko     TYPE saknr, "rf05a-newko,      "Bank G/L
    xtext     TYPE bseg-sgtxt,       "added on 12/3
    bukrs     TYPE bkpf-bukrs,       "Company code
    waers     TYPE bkpf-waers DEFAULT 'INR',       "Currency
    gjahr     TYPE bkpf-gjahr OBLIGATORY,       "Fiscal year
    monat     TYPE bkpf-monat OBLIGATORY,       "period
    kunnr     TYPE bsad-kunnr OBLIGATORY,        "Customer
    belnr     TYPE belnr_d NO-DISPLAY,         "Bill/DR memo
    wrbtr     TYPE bsad-wrbtr,        "Amount
    bktxt     LIKE bkpf-bktxt NO-DISPLAY ,       "doc text
    augtx     TYPE rf05a-augtx  NO-DISPLAY ,      "doc clearing text
    valut(10) TYPE c  NO-DISPLAY ,                "value date
    gldesc    TYPE txt50_skat  NO-DISPLAY ,             " GL Desc
    "Added by Samsudeen M on 26.04.2023
    refid     TYPE zref_id.
SELECTION-SCREEN END OF BLOCK b1.
