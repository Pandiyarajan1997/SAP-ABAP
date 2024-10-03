*&---------------------------------------------------------------------*
*& Include          ZFB50_GL_LINEITEMS_SEL
*&---------------------------------------------------------------------*
*** Selection Screen Design ***
SELECTION-SCREEN BEGIN OF BLOCK a WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_bukrs TYPE bukrs .
*  PARAMETERS: p_month TYPE isellist-month.
*  PARAMETERS: p_ddate TYPE sy-datum DEFAULT sy-datum.
*  PARAMETERS: p_pdate TYPE sy-datum DEFAULT sy-datum.
*  PARAMETERS: p_reft TYPE bkpf-xblnr .
  PARAMETERS: p_blart TYPE bkpf-blart DEFAULT 'SA'.
  PARAMETERS: fname TYPE rlgrap-filename.
  SELECTION-SCREEN SKIP.
  PARAMETERS: p_run AS CHECKBOX.
  SELECTION-SCREEN SKIP.
*  PARAMETERS: p_run AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK a.
