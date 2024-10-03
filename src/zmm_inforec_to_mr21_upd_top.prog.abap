*&---------------------------------------------------------------------*
*& Include          ZMM_INFOREC_TO_MR21_UPD_TOP
*&---------------------------------------------------------------------*

TYPES: BEGIN OF ty_mara,
         matnr TYPE matnr,
         mtart TYPE mtart,
       END OF ty_mara.

DATA: gt_mara TYPE TABLE OF ty_mara.
DATA: gt_mm_plant TYPE STANDARD TABLE OF zmm_mat_list.
DATA: gt_makt TYPE STANDARD TABLE OF makt.
TYPES: BEGIN OF ty_marc,
         matnr TYPE matnr,
         werks TYPE werks_d,
       END OF ty_marc.
DATA: gt_marc_fr TYPE TABLE OF ty_marc,
      gt_marc_to TYPE TABLE OF ty_marc.
TYPES: BEGIN OF ty_mbew,
         matnr TYPE matnr,
         bwkey TYPE bwkey,
         vprsv TYPE vprsv,
         verpr TYPE verpr,
       END OF ty_mbew.
DATA: gt_mbew TYPE STANDARD TABLE OF ty_mbew.

TYPES: BEGIN OF ty_alv,
         matnr   TYPE matnr,
         matdes  TYPE maktx,
         f_werks TYPE werks_d,
         t_werks TYPE werks_d,
         vprsv   TYPE vprsv,
         unstk   TYPE labst,
         unval   TYPE salk3,
         poqty   TYPE menge_d,
         poval   TYPE netwr,
         fprice  TYPE verpr,
         update TYPE char01,
         message TYPE string,
         remarks TYPE string,
       END OF ty_alv.
DATA: gt_cal_price TYPE TABLE OF ty_alv.

DATA: gt_stock   TYPE STANDARD TABLE OF zstr_safety_stock,
      gt_stock_t TYPE STANDARD TABLE OF zstr_safety_stock.

DATA: gt_open_po TYPE STANDARD TABLE OF zstr_open_po,
      gt_open_pt TYPE STANDARD TABLE OF zstr_open_po.

*TYPES: BEGIN OF ty_final,
*         matnr   TYPE matnr,
*         matdes  TYPE maktx,
*         f_werks TYPE werks_d,
*         t_werks TYPE werks_d,
*         price   TYPE valpr,
*         msg(60) TYPE c,
*       END OF ty_final.
*DATA: gt_final TYPE TABLE OF ty_final,
*      gs_final TYPE ty_final.

DATA: gv_price TYPE eine-netpr.

*** ALV Display Internal tables ***
DATA: gs_layout TYPE  slis_layout_alv,
* Internal Table Declaration for FieldCatlog
      gt_fcat   TYPE  slis_t_fieldcat_alv,
* Work Area Declaration for FieldCatlog
      gs_fcat   TYPE slis_fieldcat_alv.
DATA: gv_org TYPE bukrs.

** Internal Table for BDC **
DATA: wa_bdcdata TYPE bdcdata,
      it_bdcdata TYPE STANDARD TABLE OF bdcdata,
      gt_bdcmsg  TYPE STANDARD TABLE OF bdcmsgcoll,
      gs_bdcmsg  TYPE bdcmsgcoll.
