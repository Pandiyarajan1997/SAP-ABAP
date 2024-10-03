*&---------------------------------------------------------------------*
*& Report ZCS01_REPORT
*&---------------------------------------------------------------------*
*&Created by: Samsudeen M
*&Created on: 22.02.2023
*&Purpose : Extending the existing BOM
*Reference by: Praveen
*&---------------------------------------------------------------------*
REPORT zcs01_report.


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-200.
  PARAMETERS: p_fname TYPE rlgrap-filename OBLIGATORY.
  SELECTION-SCREEN : SKIP 1.
  PARAMETERS : test_run AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b1.

"Data Declaration
INCLUDE zcs01_top.

"Actual Subroutines
INCLUDE zcs01_include.

" Selection Screen Value Request

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.
  "F4 value Help
  PERFORM f4_value_help USING p_fname.


START-OF-SELECTION.
  "Excel Conversion
  PERFORM excel_conversion USING p_fname.

  "Initial validation and display
  PERFORM validate_excel_data.

  "Display ALV
  PERFORM f_display_alv.
