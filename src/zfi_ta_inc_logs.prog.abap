*&---------------------------------------------------------------------*
*& Report ZFI_TA_INC_LOGS
*&---------------------------------------------------------------------*
*&Created by: Samsudeen M
*&Created On: 08.06.2023
*&Reference : TA logs
*&---------------------------------------------------------------------*
REPORT zfi_ta_inc_logs.

"Output Structure
TYPES: BEGIN OF ty_final,
         employee_no TYPE pernr_d,
         empname     TYPE pa0001-sname,
         emp_vendor  TYPE lifnr,
         venname     TYPE lfa1-name1,
         reference   TYPE xblnr1,
         amount      TYPE wrbtr,
         bldat       TYPE bldat,
         budat       TYPE budat,
         overall_kms TYPE entkm,
         bukrs       TYPE bukrs,
         gjahr       TYPE gjahr,
         belnr       TYPE belnr_d,
         erdat       TYPE erdat,
         er_time     TYPE erzet,
         invtype     TYPE c,
         inv_cat     TYPE char15,
         kostl       TYPE kostl,
         saknr       TYPE saknr,
         type        TYPE c,
         msg         TYPE char200,
       END OF ty_final.

DATA: gt_logs_disp TYPE TABLE OF ty_final.

DATA: lv_pernr     TYPE pernr_d,
      lv_lifnr     TYPE lifnr,
      lv_ref       TYPE xblnr1,
      lv_bldat     TYPE bldat,
      lv_budat     TYPE budat,
      lv_fisyear   TYPE gjahr,
      lv_bukrs     TYPE bukrs,
      lv_belnr     TYPE belnr_d,
      lv_erdat     TYPE erdat,
      lv_intype(1) TYPE c,
      lv_errtyp(1) TYPE c.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_pernr FOR lv_pernr MATCHCODE OBJECT prem,
                  s_lifnr FOR lv_lifnr,
                  s_ref   FOR lv_ref,
                  s_bldat FOR lv_bldat,
                  s_budat FOR lv_budat,
                  s_gjahr FOR lv_fisyear,
                  s_bukrs FOR lv_bukrs,
                  s_belnr FOR lv_belnr,
                  s_erdat FOR lv_erdat,
                  s_type FOR lv_intype,
                  s_etyp FOR lv_errtyp.
SELECTION-SCREEN END OF BLOCK b1.

INCLUDE zta_logs_forms.

DATA: lo_main TYPE REF TO lcl_logs.

START-OF-SELECTION.
  CREATE OBJECT lo_main.
  "Data Fetching From log table
  lo_main->fetch_data( ).

  "ALv Display
  lo_main->build_alv( ).

END-OF-SELECTION.
