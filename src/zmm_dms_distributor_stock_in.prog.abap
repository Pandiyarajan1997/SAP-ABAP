*&---------------------------------------------------------------------*
*& Report ZMM_DMS_DISTRIBUTOR_STOCK_IN
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmm_dms_distributor_stock_in.

INCLUDE zmm_dms_distributor_stk_top.


*********************************************************************
********* Selection Screen Design ***********************************
*********************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_dist  TYPE kna1-kunnr,
              p_lifnr TYPE ekko-lifnr.
SELECTION-SCREEN END OF BLOCK b1.

INCLUDE zmm_dms_distributor_stk_inc.

DATA: lo_main TYPE REF TO lcl_distributor_stk.

START-OF-SELECTION.

  CREATE OBJECT lo_main
    EXPORTING
      distributor = p_dist
      lifnr       = p_lifnr.

  lo_main->f_fetch_dstock_api( ).
  lo_main->f_data_selection( ).
  lo_main->alv_display( ).
