*&---------------------------------------------------------------------*
*& Report  ZMM_INITIAL_UPLOAD
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT zmm_auto_migo_sto_prg.
TABLES: ekko.

***********************************************************
*   Selection Screen                                       *
***********************************************************
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE TEXT-001.
  SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
    SELECTION-SCREEN BEGIN OF LINE.
      PARAMETERS: p_ub TYPE c RADIOBUTTON GROUP rb1 .
      SELECTION-SCREEN COMMENT 2(33) TEXT-002 FOR FIELD p_ub.
      SELECTION-SCREEN POSITION 40.
      PARAMETERS: p_ebeln TYPE ebeln MATCHCODE OBJECT mekk_c.
    SELECTION-SCREEN END OF LINE.

    SELECTION-SCREEN BEGIN OF LINE.
      PARAMETERS: p_zub TYPE c RADIOBUTTON GROUP rb1 .
      SELECTION-SCREEN COMMENT 2(36) TEXT-003 FOR FIELD p_zub.
      SELECTION-SCREEN POSITION 40.
      PARAMETERS: p_vbeln TYPE vbeln_vl MATCHCODE OBJECT f4_likp.
    SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN END OF BLOCK b1.

  PARAMETERS: p_splant TYPE lips-werks NO-DISPLAY." MATCHCODE OBJECT h_t001w .
  PARAMETERS: p_rplant TYPE lips-werks NO-DISPLAY." MATCHCODE OBJECT h_t001w OBLIGATORY.
*  SELECT-OPTIONS: so_lifnr FOR ekko-lifnr NO INTERVALS OBLIGATORY DEFAULT '0010001546'.
  PARAMETERS: p_frbnr TYPE frbnr OBLIGATORY.
  PARAMETERS: p_d_note TYPE lfsnr1 OBLIGATORY.
  PARAMETERS: p_htext TYPE bktxt OBLIGATORY.
  PARAMETERS: p_test TYPE flag DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK a1.

*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*
*-------------------------DATA Declaration---------------------------*
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*

INCLUDE zmm_auto_migo_sto_prg_cls.
*INCLUDE zmm_auto_migo_prg_cls.

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
  IF ( p_ub IS NOT INITIAL AND p_ebeln  IS INITIAL ) AND
       p_zub IS NOT INITIAL AND p_vbeln IS INITIAL.
    DATA(l_text) = 'Please fill either PO NUmber or OBD Number'.
    MESSAGE l_text TYPE 'S' DISPLAY LIKE 'E'.
  ELSE.
    CREATE OBJECT lo_main
      EXPORTING
        ebeln  = p_ebeln
        vbeln  = p_vbeln
*        splant = p_splant
*        rplant = p_rplant
*       lifnr  = so_lifnr[]
        frbnr  = p_frbnr
        lfsnr  = p_d_note
        test   = p_test
        header = p_htext.

    lo_main->fetch_invoice_data( ).
    lo_main->fetch_purchase_data( ).
    lo_main->build_alv( ).
  ENDIF.
