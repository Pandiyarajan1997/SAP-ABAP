*&---------------------------------------------------------------------*
*& Report ZMM_PURCHASE_ORDER_CREATE
*&---------------------------------------------------------------------*
*&"created by: Samsudeen M
"Created on: 05.04.2023
"Purpose : For Creating New standard Purchase Order (ZNB) based on
" Purchase Created automatically for Tech Services Daily (ZSTO)
"Requirement: Purchase Team
"Reference : Ramakrishnan J & Gopalraja
*&---------------------------------------------------------------------*
REPORT zmm_purchase_order_create.

DATA: lv_ebeln TYPE ekko-ebeln,
      lv_lifnr TYPE ekko-lifnr,
      lv_bukrs TYPE ekko-bukrs,
      lv_ekorg TYPE ekko-ekorg,
      lv_werks TYPE ekpo-werks,
      lv_bsart TYPE ekko-bsart,
      lv_aedat TYPE ekko-aedat.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_ebeln FOR lv_ebeln,
                  s_bukrs FOR lv_bukrs,
                  s_ekorg FOR lv_ekorg,
                  s_plant FOR lv_werks,
                  s_aedat FOR lv_aedat.
  PARAMETERS: p_lifnr TYPE ekko-lifnr,
              p_bsart TYPE ekko-bsart.
SELECTION-SCREEN END OF BLOCK b1.

INCLUDE zmm_po_create_forms.

DATA: lo_main TYPE REF TO lcl_po_create.

INITIALIZATION.
  CREATE OBJECT lo_main.

START-OF-SELECTION.
  "Get Purchase Order for Reference
  lo_main->f_get_purchase_order( ).
  "Based on Reference Creating Standard Purchase Order
  lo_main->f_create_po( ).
  "Display Log
  lo_main->f_display_alv( ).
