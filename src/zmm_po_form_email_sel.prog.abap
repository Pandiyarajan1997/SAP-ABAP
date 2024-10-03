*&---------------------------------------------------------------------*
*& Include          ZMM_PO_FORM_EMAIL_SEL
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001 .
  SELECT-OPTIONS: s_ebeln FOR gv_ebeln,
                  s_aedat FOR gv_aedat.
SELECTION-SCREEN END OF BLOCK b1.
