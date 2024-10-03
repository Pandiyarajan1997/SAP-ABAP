*&---------------------------------------------------------------------*
*& Include          ZMM_DMS_PURCHASING_TOP
*&---------------------------------------------------------------------*
FIELD-SYMBOLS: <fs_log> TYPE zmm_dms_purchase.
DATA lv_unit_price TYPE char15.
DATA: lv_type TYPE bapi_mtype,
      lv_msg  TYPE string.
DATA: gw_bdcdata TYPE bdcdata,
      gt_bdcdata TYPE TABLE OF bdcdata WITH EMPTY KEY,
      gt_bdcmsg  TYPE STANDARD TABLE OF bdcmsgcoll,
      gw_bdcmsg  TYPE bdcmsgcoll.
DATA lv_msg_text TYPE string.
DATA: lv_datum(10) TYPE c,
      lv_date1(10) TYPE c.
DATA lv_amount(13) TYPE c.
DATA: lv_taxcode TYPE mwskz.
DATA: lo_main TYPE REF TO zcl_dms_purchase_process.
