*&---------------------------------------------------------------------*
*&  Include           ZD_EINVOICE_SEL
*&---------------------------------------------------------------------*
TABLES : vbrk,vbrp.

*//RADIO BUTTON DESIGN
SELECTION-SCREEN BEGIN OF BLOCK blk2 WITH FRAME TITLE text-s01.
*//RADIO BUTTONS DESIGN
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_rad1 RADIOBUTTON GROUP grp1 USER-COMMAND i1.
SELECTION-SCREEN COMMENT 10(10) text-010 FOR FIELD p_rad1.

PARAMETERS: p_rad4 RADIOBUTTON GROUP grp1.
SELECTION-SCREEN COMMENT 25(18) text-013 FOR FIELD p_rad4.  "Create Ewaybill

PARAMETERS: p_rad2 RADIOBUTTON GROUP grp1 .               " Cancel IRN/Eway Bill
SELECTION-SCREEN COMMENT 45(25) text-011 FOR FIELD p_rad2.

PARAMETERS: p_rad3 RADIOBUTTON GROUP grp1 .                 "Display
SELECTION-SCREEN COMMENT 78(15) text-012 FOR FIELD p_rad3.

SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK blk2.


SELECTION-SCREEN : BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS : s_vbeln FOR vbrk-vbeln,
                 s_werks FOR vbrp-werks NO INTERVALS,
                 s_erdat FOR vbrk-erdat.   "NO-EXTENSION NO INTERVALS DEFAULT sy-datum .
SELECTION-SCREEN : END OF BLOCK  b1.

SELECTION-SCREEN : BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
*PARAMETERS:     p_vehno    TYPE char10 MODIF ID i1,
*                p_tid      TYPE char15 MODIF ID i1,
PARAMETERS:     p_vehno    TYPE char15 MODIF ID i1,
                p_tid      TYPE char20 MODIF ID i1,
                p_tnm      TYPE char40 MODIF ID i1,
                p_docno    TYPE char10 MODIF ID i1,
                p_docdt    LIKE sy-datum  MODIF ID i1,
                p_dis      TYPE char5 MODIF ID i1.
SELECTION-SCREEN : END OF BLOCK  b2.
*&---------------------------------------------------------------------*
*&  Initialization
*&---------------------------------------------------------------------*
INITIALIZATION.

  s_erdat-sign  = 'I'.
  s_erdat-option = 'EQ'.
  s_erdat-low  = sy-datum - 1.
  s_erdat-high = sy-datum .
  APPEND s_erdat.

*&---------------------------------------------------------------------*
*&  Selection Screen Output
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

  IF p_rad4 IS NOT INITIAL.         "E-Way Bill Creation

    LOOP AT SCREEN.
      IF screen-group1 = 'I1'.
        screen-input = '1'.           "<----to enable display mode
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSE.
    CLEAR: p_vehno,p_tid,p_tnm,p_docdt,p_docno .
    LOOP AT SCREEN.
      IF screen-group1 = 'I1'.
        screen-invisible = '1'.
        screen-input = '0'.           "<----to Disable display mode
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

  ENDIF.
