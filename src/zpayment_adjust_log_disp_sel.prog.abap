*&---------------------------------------------------------------------*
*& Include          ZPAYMENT_ADJUST_LOG_DISP_SEL
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_bukrs FOR lv_bukrs,
                  s_belnr FOR lv_docno,
                  s_year FOR lv_year MATCHCODE OBJECT rscalyear,
                  s_actyp FOR lv_actyp,
                  s_docty FOR lv_docty,
                  s_acc FOR lv_account,
                  s_date FOR lv_date.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS p_layo LIKE disvariant-variant.
SELECTION-SCREEN END OF BLOCK b2.
