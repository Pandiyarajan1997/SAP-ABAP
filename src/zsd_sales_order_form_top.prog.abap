*&---------------------------------------------------------------------*
*& Include          ZSD_SALES_ORDER_FORM_TOP
*&---------------------------------------------------------------------*
** Sales Document Header  **
TYPES: BEGIN OF st_vbak,
         vbeln TYPE vbeln_va,
         erdat TYPE erdat,
         kunnr TYPE kunag,
       END OF st_vbak.
TYPES: BEGIN OF st_vbap,
         vbeln  TYPE vbeln_va,
         posnr  TYPE posnr_va,
         matnr  TYPE matnr,
         arktx  TYPE arktx,
         meins  TYPE meins,
         netwr  TYPE netwr_ap,
         kwmeng TYPE kwmeng,
       END OF st_vbap.
TYPES: BEGIN OF st_kna1,
         kunnr TYPE kunnr,
         name1 TYPE name1_gp,
         stras TYPE stras_gp,
         ort01 TYPE ort01_gp,
         land1 TYPE land1_gp,
         pstlz TYPE pstlz,
         telf1 TYPE telf1,
       END OF st_kna1.
TYPES: BEGIN OF st_header,
         vbeln TYPE vbeln_va,
         erdat TYPE erdat,
         kunnr TYPE kunnr,
         name1 TYPE name1_gp,
         stras TYPE stras_gp,
         ort01 TYPE ort01_gp,
         land1 TYPE land1_gp,
         pstlz TYPE pstlz,
         telf1 TYPE telf1,
       END OF st_header.
TYPES: BEGIN OF st_item,
         vbeln  TYPE vbeln_va,
         posnr  TYPE posnr_va,
         matnr  TYPE matnr,
         arktx  TYPE arktx,
         meins  TYPE meins,
         netwr  TYPE netwr_ap,
         kwmeng TYPE kwmeng,
       END OF st_item.


** Internal Table and Work area **
DATA: gt_vbak   TYPE TABLE OF st_vbak,
      gs_vbak   TYPE st_vbak,
      gt_vbap   TYPE TABLE OF st_vbap,
      gs_vbap   TYPE st_vbap,
      gt_kna1   TYPE TABLE OF st_kna1,
      gs_kna1   TYPE st_kna1,
      gt_header TYPE TABLE OF st_header,
      gs_header TYPE st_header,
      gt_item   TYPE TABLE OF st_item,
      gs_item   TYPE st_item.
