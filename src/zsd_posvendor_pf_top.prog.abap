*&---------------------------------------------------------------------*
*& Include          ZSD_POSVENDOR_PF_TOP
*&---------------------------------------------------------------------*
TYPES: BEGIN OF excel,
         kunnr TYPE kunnr,
         vkorg TYPE vkorg,
         vtweg TYPE vtweg,
         spart TYPE spart,
         parvw TYPE parvw,
         lifnr TYPE lifnr,
       END OF excel.
TYPES: BEGIN OF ty_alv,
         kunnr    TYPE kunnr,
         vkorg    TYPE vkorg,
         vtweg    TYPE vtweg,
         spart    TYPE spart,
         parvw    TYPE parvw,
         lifnr    TYPE lifnr,
         type(01) TYPE c,
         message  TYPE string,
       END OF ty_alv.
DATA: gt_alv TYPE TABLE OF ty_alv.
DATA: gt_raw   TYPE truxs_t_text_data,
      gt_excel TYPE TABLE OF excel.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_fname TYPE rlgrap-filename OBLIGATORY.
  SELECTION-SCREEN SKIP.
  PARAMETERS: p_run AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b1.
