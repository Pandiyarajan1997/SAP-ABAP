*&---------------------------------------------------------------------*
*& Report ZSD_BP_CORRECTION
*&---------------------------------------------------------------------*
*&Created by: Samsudeen M
*&Created On: 30.05.2023
*&Purpose: Business Partner Data Correction
*&Reference by: Praveen Kumar
*&---------------------------------------------------------------------*
REPORT zsd_bp_correction.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_fname TYPE rlgrap-filename OBLIGATORY.
  SELECTION-SCREEN SKIP.
  PARAMETERS: p_r1 RADIOBUTTON GROUP r1,
              p_r2 RADIOBUTTON GROUP r1,
              p_r3 RADIOBUTTON GROUP r1.
SELECTION-SCREEN END OF BLOCK b1.

INCLUDE zsd_bp_correction_cls.

DATA: go_main TYPE REF TO lcl_main.

INITIALIZATION.
  CREATE OBJECT go_main.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.
  go_main->f4_help( ).

START-OF-SELECTION.
  "Excel Conversion
  go_main->convert_excel_to_sap( ).
  "Whole Process
  go_main->update_process( ).
  "alv Display
  go_main->build_alv( ).
