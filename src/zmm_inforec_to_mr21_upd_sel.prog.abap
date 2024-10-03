*&---------------------------------------------------------------------*
*& Include          ZMM_INFOREC_TO_MR21_UPD_SEL
*&---------------------------------------------------------------------*
*** Selection Screen Design *****
DATA: gv_matnr TYPE mara-matnr,
      gv_mtype TYPE mara-mtart.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_from TYPE werks_d OBLIGATORY,
              p_to   TYPE werks_d OBLIGATORY.
  SELECT-OPTIONS: s_matnr FOR gv_matnr,
                  s_mtype FOR gv_mtype OBLIGATORY.
  SELECTION-SCREEN SKIP.
  PARAMETERS: p_disp AS CHECKBOX.
  PARAMETERS p_mode LIKE ctu_params-dismode DEFAULT 'N'.
SELECTION-SCREEN END OF BLOCK b1.
