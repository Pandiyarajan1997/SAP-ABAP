*&---------------------------------------------------------------------*
*& Include          ZSD_UPDATE_CUSPOS_S01
*&---------------------------------------------------------------------*

DATA : lv_parvw TYPE knvp-parvw.

SELECTION-SCREEN : BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  PARAMETERS p_fname TYPE rlgrap-filename MODIF ID a.      "for excel file upload

  SELECTION-SCREEN : SKIP 2.

  SELECT-OPTIONS : so_parvw FOR lv_parvw.

SELECTION-SCREEN : END OF BLOCK b1.
