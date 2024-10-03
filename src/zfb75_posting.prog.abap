*&---------------------------------------------------------------------*
*& Report ZFB75_POSTING
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfb75_posting.
"Data Declarations
INCLUDE zfb75_posting_top.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_bukrs TYPE t001-bukrs,
              p_path  TYPE rlgrap-filename.
  SELECTION-SCREEN SKIP.
  PARAMETERS: p_run AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b1.

"Subroutines
INCLUDE zfb75_posting_pforms.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
  PERFORM f4_gethelp.

START-OF-SELECTION.
  "Excel Conversion
  PERFORM f_convert_excel.
  "Data Selection
  PERFORM f_data_Selection.
  "Actual Process
  PERFORM f_actual_process.
