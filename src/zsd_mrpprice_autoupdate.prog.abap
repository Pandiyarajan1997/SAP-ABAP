*&---------------------------------------------------------------------*
*& Report ZSD_MRPPRICE_AUTOUPDATE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsd_mrpprice_autoupdate.
** Include for Data Declaratons  **
INCLUDE zsd_mrpprice_top.
** Include for Subroutines **
INCLUDE zds_mrpprice_subroutines.

START-OF-SELECTION.
** Filter values through stvarv **
  PERFORM f_get_filtervalues.
** Initial Data selection  **
  PERFORM f_initial_selection.
**Actual Updation and Process **
  PERFORM f_actual_process.

  IF p_run NE abap_true.
*** Log table updation ***
    PERFORM f_table_update.
  ENDIF.
** ALV Display **
  PERFORM alv_display.
