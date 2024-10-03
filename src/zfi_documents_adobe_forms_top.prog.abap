*&---------------------------------------------------------------------*
*& Include          ZFI_DOCUMENTS_ADOBE_FORMS_TOP
*&---------------------------------------------------------------------*
TYPES: BEGIN OF header,
         belnr TYPE belnr_d,
         bukrs TYPE bukrs,
         gjahr TYPE gjahr,
         blart TYPE blart,
         bldat TYPE bldat,
         budat TYPE budat,
         xblnr TYPE xblnr1,
       END OF header.
TYPES: BEGIN OF account,
         account(10) TYPE c,
         name1       TYPE name1_gp,
         stras       TYPE stras,
         ort01       TYPE ort01_gp,
         land1       TYPE land1_gp,
         telf1       TYPE telf1,
         email       TYPE ad_smtpadr,
       END OF account.

TYPES: BEGIN OF final,
         sno(1)   TYPE c,
         item_txt TYPE sgtxt,
         debit    TYPE dmbtr_cs,
         credit   TYPE dmbtr_cs,
         itax     TYPE dmbtr_cs,
         stax     TYPE dmbtr_cs,
         ctax     TYPE dmbtr_cs,
         tds      TYPE dmbtr_cs,
       END OF final.
DATA: gt_final TYPE TABLE OF final.
DATA: gt_bkpf TYPE STANDARD TABLE OF bkpf.
DATA: gv_logo    TYPE xstring,
      gv_heading TYPE text40.
