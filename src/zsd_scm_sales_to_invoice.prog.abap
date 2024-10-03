*&---------------------------------------------------------------------*
*& Report ZSD_SCM_SALES_TO_INVOICE
*&---------------------------------------------------------------------*
*&Created by: Samsudeen M
"Created on: 17.04.2023
"Purpose : Program to do Delivery, PGI, Invoices, IRN
"Reference by: Ramakrishnan J
*&---------------------------------------------------------------------*
REPORT zsd_scm_sales_to_invoice.

"DATA DECLARATIONS
INCLUDE zsd_scm_sales_to_invoice_top.

"Selection Screen Design
DATA: lv_vbeln   TYPE vbak-vbeln,
      lv_orderid TYPE zorder_id.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_ordid FOR lv_orderid MATCHCODE OBJECT zsh_scm_log,
                  s_vbeln   FOR lv_vbeln.
SELECTION-SCREEN END OF BLOCK b1.

INCLUDE zsd_scm_sales_to_invoice_cls.

INITIALIZATION.

  DATA: lo_main TYPE REF TO sales_to_invoice.
  CREATE OBJECT lo_main.

START-OF-SELECTION.
  CLEAR msg.
  lo_main->background_job_chk( EXPORTING pname = sy-cprog
                               CHANGING l_msg = msg ).
  IF msg IS INITIAL.
    REFRESH: gt_alv.
    lo_main->delivery_process( ).

*    lo_main->invoice_process( ).

    lo_main->alv_display( ).
  ELSE.
    WRITE : msg.
  ENDIF.
