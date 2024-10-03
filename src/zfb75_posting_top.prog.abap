*&---------------------------------------------------------------------*
*& Include          ZFB75_POSTING_TOP
*&---------------------------------------------------------------------*
TYPES: BEGIN OF ty_excel,
         account TYPE kunnr,
         bldat   TYPE bldat,
         budat   TYPE budat,
         xblnr   TYPE xblnr1,
         wrbtr   TYPE wrbtr,
         mwskz   TYPE mwskz,
         bupla   TYPE bupla,
         sgtxt   TYPE sgtxt,
         hkont   TYPE hkont,
         kostl   TYPE kostl,
         product TYPE matnr,
         salesno TYPE vbeln_va,
         plant   TYPE werks_d,
         matkl   TYPE matkl,
         vkbur   TYPE vkbur,
         asm     TYPE pernr_d,
         rsm     TYPE pernr_d,
       END OF ty_excel.
DATA: gt_excel TYPE TABLE OF ty_excel.
TYPES: BEGIN OF ty_alv,
         account TYPE kunnr,
         name1   TYPE name1_gp,
         bldat   TYPE bldat,
         budat   TYPE budat,
         xblnr   TYPE xblnr1,
         wrbtr   TYPE wrbtr,
         mwskz   TYPE mwskz,
         bupla   TYPE bupla,
         sgtxt   TYPE sgtxt,
         hkont   TYPE hkont,
         gldes   TYPE text50,
         kostl   TYPE kostl,
         product TYPE matnr,
         salesno TYPE vbeln_va,
         plant   TYPE werks_d,
         matkl   TYPE matkl,
         vkbur   TYPE vkbur,
         asm     TYPE pernr_d,
         rsm     TYPE pernr_d,
         type    TYPE bapi_mtype,
         msg     TYPE string,
       END OF ty_alv.
DATA: gt_alv TYPE TABLE OF ty_alv.
FIELD-SYMBOLS: <fs_alv> TYPE ty_alv.
DATA: gt_type  TYPE truxs_t_text_data.
DATA: gt_skat TYPE STANDARD TABLE OF skat,
      gt_csks TYPE STANDARD TABLE OF csks.
