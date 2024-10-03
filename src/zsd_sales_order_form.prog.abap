*&---------------------------------------------------------------------*
*& Report ZSD_SALES_ORDER_FORM
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsd_sales_order_form.
** Data Declarations **
INCLUDE zsd_sales_order_form_top.
** Selection screen **
INCLUDE zsd_sales_order_form_sel.
** Form Declaration **
INCLUDE zsd_sales_order_form_sub.

START-OF-SELECTION.
  PERFORM get_data.
  PERFORM process_data.
  PERFORM call_form.
