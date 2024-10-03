*&---------------------------------------------------------------------*
*& Include          ZMM_FERT_PRICE_UPDATE_ME12_SEL
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS:

    bldat     TYPE bkpf-bldat,       "Document Date
    budat     TYPE bkpf-budat,       "Posting Date
    blart     TYPE bkpf-blart DEFAULT 'DZ',       "Document type
    xblnr     TYPE bkpf-xblnr,       "Reference
    newko     TYPE saknr NO-DISPLAY, "rf05a-newko,      "Bank G/L
    xtext     TYPE bseg-sgtxt,       "added on 12/3
    bukrs     TYPE bkpf-bukrs,       "Company code
    waers     TYPE bkpf-waers,       "Currency
    gjahr     TYPE bkpf-gjahr,       "Fiscal year
    monat     TYPE bkpf-monat,       "period
    kunnr     TYPE bsad-kunnr,        "Customer
    belnr     TYPE belnr_d NO-DISPLAY,         "Bill/DR memo
    wrbtr     TYPE bsad-wrbtr NO-DISPLAY,        "Amount
    bktxt     LIKE bkpf-bktxt NO-DISPLAY ,        "doc text
    augtx     TYPE rf05a-augtx  NO-DISPLAY ,      "doc clearing text
    valut(10) TYPE c  NO-DISPLAY ,                "value date
    gldesc    TYPE txt50_skat  NO-DISPLAY ,             " GL Desc
    refid     TYPE zref_id NO-DISPLAY,
    distb     TYPE kunnr,
    gsber     TYPE gsber.
SELECTION-SCREEN END OF BLOCK b1.
