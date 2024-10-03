*&---------------------------------------------------------------------*
*& Report  ZSD_EINVOICE_PROCESS_DMS
*&*& TITLE         DMS eInvoice
*& Descrition    invoices that do not comply to the set requirements will be considered
*&               null and void. Exceptions will only be made for certain exempt businesses.
*&               Companies must prepare quickly and should take advantage of solutions that
*&               facilitate the issuance and receipt of e-invoices.
*& DATE WRITTEN  31/03/2024
*& Developer    : Puratchiveeran
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT zsd_einvoice_process_dms.


INCLUDE zdms_einvoice_top.
INCLUDE zdms_einvoice_sel.
INCLUDE zdms_einvoice_cls.
INCLUDE zdms_einvoice_main.


START-OF-SELECTION.
  PERFORM get_select_data.
  IF lt_einv_final IS NOT INITIAL.
    CALL SCREEN 100.
  ENDIF.
