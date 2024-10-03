*&---------------------------------------------------------------------*
*& Report ZFI_SUNDARAM_REVFILE_READ
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfi_sundaram_revfile_read.

DATA: ftype TYPE char1.

INCLUDE zfi_sundaram_revfile_cls.

SELECTION-SCREEN: BEGIN OF BLOCK blk1 WITH FRAME TITLE t001.
  PARAMETERS: r1 RADIOBUTTON GROUP rb1.
  PARAMETERS: r2 RADIOBUTTON GROUP rb1.
SELECTION-SCREEN: END OF BLOCK blk1.

SELECTION-SCREEN: BEGIN OF BLOCK blk2 WITH FRAME TITLE t002.
  PARAMETERS: p_septr TYPE char01 OBLIGATORY.
SELECTION-SCREEN: END OF BLOCK blk2.

INITIALIZATION.
  DATA: lo_cls_main TYPE REF TO lcl_sf_rev_file.
  CREATE OBJECT lo_cls_main.

START-OF-SELECTION.

  IF r1 = abap_true.
    ftype = 'I'.
    lo_cls_main->get_inv_revfile( ftype ).
    lo_cls_main->read_process_invrev( p_septr ).
    ftype = 'J'.
    lo_cls_main->get_inv_revfile( ftype ).
  ELSE.
    ftype = 'C'.
    lo_cls_main->get_inv_revfile( ftype ).
    lo_cls_main->read_process_canrev( p_septr ).
    ftype = 'D'.
    lo_cls_main->get_inv_revfile( ftype ).
  ENDIF.

  lo_cls_main->alv_display( ftype ).
