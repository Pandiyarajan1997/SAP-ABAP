*&---------------------------------------------------------------------*
*& Include          ZCREDIT_NOTE_PRG_SEL
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

** parameters:S_BELNR type BKPF-BELNR obligatory .
  SELECT-OPTIONS:s_belnr FOR bkpf-belnr  OBLIGATORY.
  PARAMETERS:s_bukrs  TYPE bkpf-bukrs OBLIGATORY.
  PARAMETERS:s_gjahr TYPE bkpf-gjahr OBLIGATORY.
  "Added on 15.10.2022 by samsudeen M
  SELECTION-SCREEN SKIP.
  SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
    PARAMETERS:p_disp1  RADIOBUTTON GROUP rad USER-COMMAND fc,
               p_disp2 RADIOBUTTON GROUP rad.
  SELECTION-SCREEN END OF BLOCK b2.
  SELECTION-SCREEN SKIP.
  SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
    PARAMETERS:p_email  AS CHECKBOX USER-COMMAND cbc MODIF ID bp1,
               p_email1 AS CHECKBOX USER-COMMAND cbc MODIF ID bp2.
  SELECTION-SCREEN END OF BLOCK b3.
SELECTION-SCREEN END OF BLOCK b1 .
