*&---------------------------------------------------------------------*
*& Report ZFI_BUDGET_CALC_DISPLAY
*&---------------------------------------------------------------------*
*&Created By: Samsudeen M
*&Created On: 22.08.2023
*&Purpose: Budget Details of G/L Account and Costcenter Display
*&Reference: Ramakrishnan J
*&---------------------------------------------------------------------*
REPORT zfi_budget_calc_display.


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_bukrs TYPE t001-bukrs OBLIGATORY,
              p_gjahr TYPE bkpf-gjahr OBLIGATORY,
              p_month TYPE zmonths.
SELECTION-SCREEN END OF BLOCK b1.

INCLUDE zfi_budget_calc_disp_cls.

INITIALIZATION.
  DATA: lo_budget TYPE REF TO lcl_budget.
  CREATE OBJECT lo_budget.

AT SELECTION-SCREEN.
  lo_budget->initialization( ).

START-OF-SELECTION.

  lo_budget->budget_calculation( ).

  lo_budget->build_alv( ).
