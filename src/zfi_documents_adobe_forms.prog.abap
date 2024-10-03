*&---------------------------------------------------------------------*
*& Report ZFI_DOCUMENTS_ADOBE_FORMS
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfi_documents_adobe_forms.

INCLUDE zfi_documents_adobe_forms_top.
***** Selection Screen Design ********
DATA: lv_belnr TYPE belnr_d.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_belnr FOR lv_belnr.
  PARAMETERS: p_bukrs TYPE bukrs,
              p_gjahr TYPE bkpf-gjahr.
SELECTION-SCREEN END OF BLOCK b1.

INCLUDE zfi_documents_adobe_pforms.

START-OF-SELECTION.
  "Image Fetching in Xstring format to Display
  PERFORM get_logo_xstring CHANGING gv_logo.
  "Entire Data Fetching
  PERFORM data_selection.
  "Entire data Population
  PERFORM data_population.
