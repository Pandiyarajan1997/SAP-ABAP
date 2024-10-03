*&---------------------------------------------------------------------*
*& Include          ZMM_STOCK_B2B_TRANSFER_TOP
*&---------------------------------------------------------------------*
TYPES: BEGIN OF ty_excel,
         from_matnr TYPE matnr,
         to_matnr   TYPE matnr,
         from_plant TYPE werks_d,
         to_plant   TYPE werks_d,
         from_stloc TYPE lgort_d,
         to_stloc   TYPE lgort_d,
         from_batch TYPE charg_d,
         to_batch   TYPE charg_d,
         quantity   TYPE menge_d,
         base_uom   TYPE meins,
         header_txt TYPE bktxt,
       END OF ty_excel.
TYPES: BEGIN OF ty_alv,
         from_matnr  TYPE matnr,
         to_matnr    TYPE matnr,
         from_plant  TYPE werks_d,
         to_plant    TYPE werks_d,
         from_stloc  TYPE lgort_d,
         to_stloc    TYPE lgort_d,
         from_batch  TYPE charg_d,
         to_batch    TYPE charg_d,
         quantity    TYPE menge_d,
         base_uom    TYPE meins,
         header_txt  TYPE bktxt,
         type        TYPE c,
         goodsmvt_no TYPE mblnr,
         msg         TYPE string,
       END OF ty_alv.
DATA: gt_excel TYPE TABLE OF ty_excel.
DATA: gt_alv TYPE TABLE OF ty_alv,
      gs_alv TYPE ty_alv.
FIELD-SYMBOLS <fs_alv> TYPE ty_alv.
DATA lo_gr_alv       TYPE REF TO cl_salv_table. " Variables for ALV properties


**** Selection Screen Design *************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-030.
  PARAMETERS: p_fname TYPE rlgrap-filename.
  SELECTION-SCREEN SKIP.
  PARAMETERS: p_run   AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b1.
