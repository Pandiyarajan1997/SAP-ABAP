*&---------------------------------------------------------------------*
*& Report ZFI_INVOICE_DOCUMENT_POST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfi_invoice_document_post.
**** Include for Data Declarations ****
INCLUDE zfi_invoice_document_top.

*** Selection Screen Parameters ****
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_bukrs TYPE bkpf-bukrs DEFAULT '1000' OBLIGATORY,
              p_blart TYPE bkpf-blart DEFAULT 'KR' OBLIGATORY.
  SELECTION-SCREEN SKIP.
  PARAMETERS: p_fname TYPE localfile.
  SELECTION-SCREEN SKIP.
  PARAMETERS: p_run AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN FUNCTION KEY 1.

INCLUDE zfi_invoice_document_forms.

AT SELECTION-SCREEN OUTPUT.
  SET PF-STATUS 'ZSEL_STAT'.

AT SELECTION-SCREEN.
  CASE sy-ucomm.
    WHEN 'FC01'.
      PERFORM f_excel_download.
  ENDCASE.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.
  PERFORM f4_help_filename USING p_fname.

START-OF-SELECTION.
**** Excel Conversion to internal Format ***
  PERFORM f_excel_conversion.
*** Initial Selection ****
  PERFORM f_initial_selection.
**** Actual Validation and Calculation all process ***
  PERFORM f_validate_calculation.
**** After Process complete its shows messages ***
  PERFORM f_display_alv.
