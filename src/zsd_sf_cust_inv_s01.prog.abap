*&---------------------------------------------------------------------*
*& Include          ZSD_SF_CUST_INV_S01
*&---------------------------------------------------------------------*

*TABLES: zsd_sf_cust_inv.
DATA : gv_custno TYPE zsd_sf_cust_inv-custno,
       gv_invno  TYPE zsd_sf_cust_inv-invoiceno,
       gv_date   TYPE zsd_sf_cust_inv-createddate.

SELECTION-SCREEN : BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  SELECT-OPTIONS : so_cno   FOR gv_custno,
                   so_invno FOR gv_invno,
                   so_date  FOR gv_date.

  PARAMETERS:      p_status  TYPE zsd_sf_cust_inv-status.
  PARAMETERS:      p_appr  TYPE c AS CHECKBOX USER-COMMAND uc1.

SELECTION-SCREEN : END OF BLOCK b1.

SELECTION-SCREEN : BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-052.

  PARAMETERS p_layo LIKE disvariant-variant.

SELECTION-SCREEN : END OF BLOCK b2.

DATA l_syucomm  TYPE sy-ucomm.
DATA: fcode TYPE TABLE OF sy-ucomm.
