*&---------------------------------------------------------------------*
*& Include          ZMM_MAT_MASTER_S01
*&---------------------------------------------------------------------*

DATA : gv_matnr TYPE marc-matnr,
       gv_werks TYPE marc-werks.

SELECTION-SCREEN : BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  SELECT-OPTIONS : so_matnr FOR gv_matnr,
                   so_werks FOR gv_werks NO INTERVALS.

SELECTION-SCREEN : END OF BLOCK b1.


SELECTION-SCREEN : BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-012.

PARAMETERS p_fname TYPE rlgrap-filename MODIF ID a.      "for excel file upload

SELECTION-SCREEN : END OF BLOCK b2.

PARAMETERS : r1 RADIOBUTTON GROUP g1 USER-COMMAND u1 DEFAULT 'X',       "for dynamic change
             r2 RADIOBUTTON GROUP g1.
