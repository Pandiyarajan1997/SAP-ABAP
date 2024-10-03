*&---------------------------------------------------------------------*
*& Include          ZMM_INFOREC_COPY_TOP
*&---------------------------------------------------------------------*
TYPES: BEGIN OF ty_mara,
         matnr TYPE matnr,
       END OF ty_mara.
DATA: gt_mara TYPE TABLE OF ty_mara.
TYPES: BEGIN OF ty_marc,
         matnr TYPE matnr,
       END OF ty_marc.
DATA: gt_marc TYPE TABLE OF ty_marc.
TYPES: BEGIN OF ty_sor_a017,
         matnr  TYPE matnr,
         werks  TYPE werks_d,
         lifnr  TYPE elifn,
         knumh  TYPE knumh,
         kschl  TYPE kscha,
         kbetr  TYPE kbetr_kond,
         f_amnt TYPE kbetr_kond,
       END OF ty_sor_a017.
DATA: gt_inre_1005 TYPE TABLE OF ty_sor_a017.
TYPES:BEGIN OF ty_inforec,
        matnr TYPE matnr,
        werks TYPE werks_d,
        lifnr TYPE elifn,
        knumh TYPE knumh,
        kschl TYPE kscha,
        kbetr TYPE kbetr_kond,
      END OF ty_inforec.
DATA: gt_inre_1401 TYPE TABLE OF ty_inforec.
TYPES: BEGIN OF ty_alv,
         matnr   TYPE matnr,
         fplant  TYPE werks_d,
         f_org   TYPE ekorg,
         lifnr   TYPE elifn,
         tplant  TYPE werks_d,
         t_org   TYPE ekorg,
         vendor  TYPE elifn,
         o_price TYPE kbetr_kond,
         n_price TYPE kbetr_kond,
         count   TYPE i,
         type    TYPE bapi_mtype,
         message TYPE string,
       END OF ty_alv.
DATA: gt_alv TYPE TABLE OF ty_alv.
DATA: gt_info_log TYPE STANDARD TABLE OF zmm_inforec_log.
DATA: lt_log TYPE STANDARD TABLE OF zmm_inforec_log.
DATA: gw_bdcdata TYPE bdcdata,
      gt_bdcdata TYPE TABLE OF bdcdata WITH EMPTY KEY,
      gt_bdcmsg  TYPE STANDARD TABLE OF bdcmsgcoll,
      gw_bdcmsg  TYPE bdcmsgcoll.
DATA lv_msg_text TYPE string.
DATA lv_amount(13) TYPE c.
DATA lv_date(10) TYPE c.
DATA lv_unit_price TYPE char15.

TYPE-POOLS: vrm.

DATA: gt_list     TYPE vrm_values.
DATA: gwa_list    TYPE vrm_value.
DATA: gt_values  TYPE TABLE OF dynpread,
      gwa_values TYPE dynpread.

DATA: gv_selected_value(20) TYPE c.
