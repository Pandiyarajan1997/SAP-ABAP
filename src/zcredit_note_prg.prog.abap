*&---------------------------------------------------------------------*
*& Report  ZCREDIT_NOTE_PRG
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT zcredit_note_prg.
** Include For Data Declarations **
INCLUDE zcredit_note_prg_top.
** Selection Screen Design **
INCLUDE zcredit_note_prg_sel.
*** Subroutines ***
INCLUDE zcredit_note_prg_forms.
*** Selection Screen Adjustments ***
AT SELECTION-SCREEN OUTPUT.

  PERFORM f_screen_adjust.

START-OF-SELECTION.
*start of Changes on 15.10.2022 **
  PERFORM f_data_selection.
** Actual Process **
  PERFORM actual_process.
