*&---------------------------------------------------------------------*
*& Include          ZHR_HOADMIN_EMP_TOP
*&---------------------------------------------------------------------*
TABLES:p0001.
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_date TYPE sy-datum  OBLIGATORY.
  SELECT-OPTIONS : s_pernr FOR p0001-pernr NO INTERVALS MATCHCODE OBJECT prem OBLIGATORY.
  PARAMETERS : r1 TYPE c RADIOBUTTON GROUP a DEFAULT 'X',
               r2 TYPE c RADIOBUTTON GROUP a.
SELECTION-SCREEN END OF BLOCK a1.
