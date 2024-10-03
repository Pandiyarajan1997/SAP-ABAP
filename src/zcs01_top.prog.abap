*&---------------------------------------------------------------------*
*& Include          ZCS01_TOP
*&---------------------------------------------------------------------*

TYPES: BEGIN OF excel,
         matnr    TYPE matnr,
         werks    TYPE werks_d,
         stlal    TYPE stalt,
         base_qty TYPE basmn_bi,
         base_uom TYPE basme,
         idnrk    TYPE idnrk,
         item_qty TYPE menge_d,
         item_uom TYPE kmpme,
       END OF excel.
DATA: gt_excel TYPE TABLE OF excel.

TYPES: BEGIN OF alv,
         matnr    TYPE matnr,
         werks    TYPE werks_d,
         stlal    TYPE stalt,
         base_qty TYPE basmn_bi,
         idnrk    TYPE idnrk,
         item_qty TYPE menge_d,
         item_uom TYPE kmpme,
         type     TYPE bapi_mtype,
         message  TYPE string,
       END OF alv.
DATA: gt_alv TYPE TABLE OF alv.

DATA: gt_raw TYPE truxs_t_text_data.
DATA: lv_refitem(02) TYPE n.
DATA: lv_qty(16) TYPE c.
DATA: lv_matnr    TYPE matnr,
      lv_material TYPE matnr.
DATA lo_gr_alv       TYPE REF TO cl_salv_table. " Variables for ALV properties
