*&---------------------------------------------------------------------*
*& Report ZMM_MEQ1_QUOTA_PRG
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmm_meq1_quota_prg.

DATA: gv_matnr TYPE mara-matnr,
      gv_werks TYPE marc-werks.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_matnr FOR gv_matnr,
                  s_werks FOR gv_werks.
  SELECTION-SCREEN SKIP.
  PARAMETERS: p_r1 RADIOBUTTON GROUP r1 USER-COMMAND grp1,
              p_r2 RADIOBUTTON GROUP r1.
SELECTION-SCREEN END OF BLOCK b1.
