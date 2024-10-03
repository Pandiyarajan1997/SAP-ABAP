*&---------------------------------------------------------------------*
*& Report  ZTECH_EINVOICE
*&*& TITLE         TECH eInvoice
*& Descrition    invoices
*& DATE WRITTEN  26/03/2021
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ZTECH_EINVOICE.


INCLUDE ZTECH_INVOICE_TOP.
*INCLUDE zd_einvoice_top.
INCLUDE ZTECH_INVOICE_FIELD.
*INCLUDE zd_einvoice_sel.
INCLUDE ZTECH_INVOICE_OBJ.
*INCLUDE zd_einvoice_cls.
INCLUDE ZTECH_INVOICE_PGM.
*INCLUDE zd_einvoice_main.


START-OF-SELECTION.
  PERFORM get_select_data.
  IF lt_einv_final IS NOT INITIAL.
    CALL SCREEN 100.
  ENDIF.
