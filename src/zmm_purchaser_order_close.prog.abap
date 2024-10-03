*&---------------------------------------------------------------------*
*& Report ZMM_PURCHASER_ORDER_CLOSE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
"Created By: Samsudeen M
"Created On: 04.02.2023
"Purpose: To close Purchase Order uding Delivery Completed
"Requirement : Purchase Team
"Reference by: Ramakrishnan J
*&---------------------------------------------------------------------*
REPORT zmm_purchaser_order_close.

DATA: lv_ebeln  TYPE ekko-ebeln,
      lv_ebelp  TYPE ekpo-ebelp,
      lv_bsart  TYPE ekko-bsart,
      lv_splant TYPE ekko-reswk,
      lv_rplant TYPE ekpo-werks,
      lv_datum  TYPE ekko-aedat.
DATA: lv_date  TYPE p0001-begda,
      lv_date1 TYPE p0001-begda.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_ebeln FOR lv_ebeln,
                  s_ebelp FOR lv_ebelp,
                  s_bsart FOR lv_bsart,
                  s_plant FOR lv_splant,
                  s_werks FOR lv_rplant,
                  s_aedat FOR lv_datum.
SELECTION-SCREEN END OF BLOCK b1.
"Class Definition and Implementation
INCLUDE zmm_po_close_cls.

DATA : lo_main TYPE REF TO lcl_po_close.

INITIALIZATION.
  CREATE OBJECT lo_main.
  lo_main->f_date_adjustment( ).
**--- Defaulting the Values in Selection Screen ----------*


START-OF-SELECTION.
  IF s_aedat IS INITIAL.
    s_aedat-sign = 'I'.
    s_aedat-option = 'BT'.
    s_aedat-low = lv_date1.
    s_aedat-high = lv_date.
    APPEND s_aedat.
  ENDIF.
  "Getting All Open Purchase Order
  lo_main->f_get_open_po( ).
  "Close all Fetched Purchase Order
  lo_main->f_close_po( ).
  "Display Log
  lo_main->f_display_alv( ).
