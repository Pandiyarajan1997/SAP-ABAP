*&---------------------------------------------------------------------*
*& Include          ZMM_MAT_BOM_EXTN_TOP
*&---------------------------------------------------------------------*

TYPES: BEGIN OF excel,
         matnr      TYPE matnr,
         werks      TYPE werks_d,
         stlal      TYPE stalt,
         base_qty   TYPE basmn_bi,
         base_uom   TYPE basme,
         idnrk      TYPE idnrk,
         item_qty   TYPE menge_d,
         item_uom   TYPE kmpme,
       END OF excel.
DATA: gt_excel TYPE TABLE OF excel.

TYPES: BEGIN OF alv,
         matnr   TYPE matnr,
         werks   TYPE werks_d,
         stlal   TYPE stalt,
         idnrk   TYPE idnrk,
         type    TYPE bapi_mtype,
         message TYPE string,
       END OF alv.
DATA: gt_alv TYPE TABLE OF alv.

DATA: gt_raw  TYPE truxs_t_text_data.

    DATA: gt_bdcdata TYPE TABLE OF bdcdata WITH EMPTY KEY,
          gw_bdcdata TYPE bdcdata,
          gt_bdcmsg  TYPE STANDARD TABLE OF bdcmsgcoll,
          gw_bdcmsg  TYPE bdcmsgcoll.
