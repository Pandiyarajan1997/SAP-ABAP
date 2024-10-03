*&---------------------------------------------------------------------*
*& Report  ZMM_INITIAL_UPLOAD
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT zmm_auto_migo_prg.
TABLES: ekko.

***********************************************************
*   Selection Screen                                       *
***********************************************************
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_vbeln TYPE vbeln_vf MATCHCODE OBJECT f4_vbrk OBLIGATORY.
  PARAMETERS: p_splant TYPE lips-werks OBLIGATORY." MATCHCODE OBJECT h_t001w .
  PARAMETERS: p_rplant TYPE lips-werks OBLIGATORY." MATCHCODE OBJECT h_t001w OBLIGATORY.
  SELECT-OPTIONS: so_lifnr FOR ekko-lifnr NO INTERVALS OBLIGATORY DEFAULT '0010001546'.
  PARAMETERS: p_frbnr TYPE frbnr OBLIGATORY.
  PARAMETERS: p_d_note TYPE lfsnr1 OBLIGATORY.
  PARAMETERS: p_htext TYPE bktxt OBLIGATORY.
  PARAMETERS: p_test TYPE flag DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK a1.

*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*
*-------------------------DATA Declaration---------------------------*
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*

INCLUDE zmm_auto_migo_prg_cls.

* declare object for dragdrop object.
DATA : lo_main TYPE REF TO lcl_auto_migo.

************************************************************
*   INITIALIZATION                                         *
************************************************************
INITIALIZATION.

************************************************************
*   At selection screen events                             *
************************************************************

************************************************************
*   Start-Of-Selection                                     *
************************************************************
START-OF-SELECTION.

  CREATE OBJECT lo_main
    EXPORTING
      vbeln  = p_vbeln
      splant = p_splant
      rplant = p_rplant
      lifnr  = so_lifnr[]
      frbnr  = p_frbnr
      lfsnr  = p_d_note
      test   = p_test
      header = p_htext.

  lo_main->fetch_invoice_data( ).
  lo_main->fetch_purchase_data( ).
  lo_main->build_alv( ).
