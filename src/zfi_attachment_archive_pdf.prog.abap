*&---------------------------------------------------------------------*
*& Report ZFI_ATTACHMENT_ARCHIVE_PDF
*&---------------------------------------------------------------------*
*&Created by: Samsudeen M
*&Created on: 15.02.2023
*&Reference by: Ramakrishnan J
*&Purpose : To attach PDF Documents to SAP Business Transaction
*&---------------------------------------------------------------------*
REPORT zfi_attachment_archive_pdf.
"Data Declarations
INCLUDE zfi_attachment_archive_top.

"Selection Screen Design
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_bukrs TYPE bkpf-bukrs OBLIGATORY,
              p_gjahr TYPE bkpf-gjahr OBLIGATORY.
  SELECT-OPTIONS: s_belnr FOR lv_belnr MATCHCODE OBJECT fmkk_f4_belnr.
  SELECTION-SCREEN SKIP.
  PARAMETERS: p_path TYPE rlgrap-filename OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

"Class Definition and implementation
INCLUDE zfi_attachment_archive_forms.

DATA: lo_attachment TYPE REF TO lcl_attachment.

INITIALIZATION.
  CREATE OBJECT lo_attachment.
  "Restricting the Document Number field in selection screen
  lo_attachment->restrict_select_option( ).

AT SELECTION-SCREEN OUTPUT.
  "Disabling field in selection screen
  lo_attachment->screen_adjustment( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
  lo_attachment->get_folder_details( ).

START-OF-SELECTION.
*  IF p_path IS NOT INITIAL.
*    TRANSLATE p_path TO LOWER CASE.
*  ENDIF.
  IF s_belnr[] IS INITIAL.
    MESSAGE 'Enter Document Number' TYPE 'S' DISPLAY LIKE 'E'.
  ELSE.
    "Initial Fetching of Document Number for checks
    lo_attachment->data_selection( ).
    "File existence check and display in alv
    lo_attachment->file_exist_check( ).
    "ALV Display
    lo_attachment->display_alv( ).
  ENDIF.
