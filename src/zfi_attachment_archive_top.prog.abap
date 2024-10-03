*&---------------------------------------------------------------------*
*& Include          ZFI_ATTACHMENT_ARCHIVE_TOP
*&---------------------------------------------------------------------*
DATA: lv_belnr TYPE bkpf-belnr.

TYPES: BEGIN OF ty_alv,
         bukrs   TYPE bukrs,
         belnr   TYPE belnr_d,
         gjahr   TYPE gjahr,
         fname   TYPE string,
*         accdoc  TYPE belnr_d,
         sobj    TYPE toav0-sap_object,
         dtype   TYPE toav0-ar_object,
         type    TYPE bapi_mtype,
         message TYPE string,
       END OF ty_alv.
TYPES: BEGIN OF ty_belnr,
         belnr TYPE belnr_d,
       END OF ty_belnr.
TYPES: BEGIN OF ty_bkpf,
         bukrs TYPE bukrs,
         belnr TYPE belnr_d,
         gjahr TYPE gjahr,
       END OF ty_bkpf.
TYPES: BEGIN OF ty_rbkp,
         belnr TYPE belnr_d,
         gjahr TYPE gjahr,
         bukrs TYPE bukrs,
       END OF ty_rbkp.
DATA: gt_alv TYPE TABLE OF ty_alv.
DATA: gt_belnr TYPE TABLE OF ty_belnr.
DATA: gt_bkpf TYPE TABLE OF ty_bkpf.
DATA: gt_rbkp TYPE TABLE OF ty_rbkp.
DATA: gv_xstring TYPE xstring,
      gv_length  TYPE sapb-length.
DATA: gt_content    TYPE soli_tab.
DATA: gt_binar TYPE STANDARD TABLE OF tbl1024.
DATA lo_gr_alv       TYPE REF TO cl_salv_table. " Variables for ALV properties
DATA: gt_pdf_list TYPE STANDARD TABLE OF toa03.
DATA: gv_path TYPE string.
