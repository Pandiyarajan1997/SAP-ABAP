*&---------------------------------------------------------------------*
*& Report ZMM_STOCK_B2B_TRANSFER
*&---------------------------------------------------------------------*
*&Created By: Samsudeen M
*&Created On: 25.05.2023
*&Purpose: Stock transfer from one Batch to Another Batch
*          using Goods Movement
*&Reference: Gopalraja
*&---------------------------------------------------------------------*
REPORT zmm_stock_b2b_transfer.
*Data Declarations
INCLUDE zmm_stock_b2b_transfer_top.
*Subroutines
INCLUDE zmm_stock_b2b_transfer_forms.
*Selection Screen Value Help
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.
  PERFORM f4_help USING p_fname.

START-OF-SELECTION.
*Excel Conversion
  PERFORM excel_conversion USING p_fname.
*Initial Checks for Excel Data
  PERFORM initial_checks.
*ALV Display
  PERFORM alv_display.
