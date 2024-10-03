*&---------------------------------------------------------------------*
*& Report ZFI_GST_RETURNS_COMPARE
*&---------------------------------------------------------------------*
*&Created By: Samsudeen M
*&Created On: 10.08.2023
*&Purpose: Vendor GST Returns Comparison With SAP
*&Reference: Samsudeen M
*&---------------------------------------------------------------------*
REPORT zfi_gst_returns_compare.
****************** Selection Screen Design *****************************
DATA: lv_bldat TYPE bldat.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
*SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_bukrs TYPE t001-bukrs DEFAULT '1000'.
*SELECTION-SCREEN END OF BLOCK a1.
*  SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF BLOCK a2." WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_excel RADIOBUTTON GROUP cbx USER-COMMAND FLG MODIF ID bl1.
  PARAMETERS: p_fname TYPE rlgrap-filename MODIF ID b1.
*              p_fname2 TYPE rlgrap-filename MODIF ID b1.
  PARAMETERS: p_itc  RADIOBUTTON GROUP cbx MODIF ID bl3.
  PARAMETERS: p_disp  RADIOBUTTON GROUP cbx MODIF ID bl2.
  SELECT-OPTIONS: s_bldat FOR lv_bldat MODIF ID b2.
SELECTION-SCREEN END OF BLOCK a2.
SELECTION-SCREEN END OF BLOCK b1.
************************************************************************
INCLUDE zfi_gst_returns_compare_cls.

INITIALIZATION.
  DATA: lo_cls_main TYPE REF TO lcl_gst_returns.
  CREATE OBJECT lo_cls_main.

AT SELECTION-SCREEN OUTPUT.
  "List Box Values
*  lo_cls_main->listbox_values( ).
  "Selection Screen Adjsutments
  lo_cls_main->screen_adjustments( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.
  "F4 Value Help for Filename
  lo_cls_main->f4_helpvalues( ).
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname2.
*  "F4 Value Help for Filename
*  lo_cls_main->f4_helpvalues( ).

START-OF-SELECTION.

  IF p_excel EQ abap_true.
    lo_cls_main->excel_upload( ).
  ELSE.
    lo_cls_main->display_oldvalues( ).
  ENDIF.

  lo_cls_main->build_alv( ).
