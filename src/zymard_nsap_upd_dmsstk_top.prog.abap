*&---------------------------------------------------------------------*
*& Include          ZYMARD_NSAP_UPD_DMSSTK_TOP
*&---------------------------------------------------------------------*
** Structures for DMS API Stock
TABLES: ymard_nsap.

TYPES: BEGIN OF ty_stock,
         distributor_code         TYPE string,
         item_code                TYPE string,
         uom                      TYPE string,
         stock_accepted           TYPE string,
         stock_yet_to_be_accepted TYPE string,
         pending_order            TYPE string,
         lcreateddate             TYPE string,
       END OF ty_stock.
DATA: gt_stock TYPE TABLE OF ty_stock,
      gs_stock TYPE ty_stock.

DATA: lo_http_client TYPE REF TO if_http_client.
DATA: lv_url TYPE string.
DATA: lv_response   TYPE string, "API Response
      lv_codes      TYPE i,      "STATUS Code
      lv_http_error TYPE string. "STATUS Description
DATA: lv_response1 TYPE string.
DATA: lv_date(10) TYPE c.
*** Internal table for Distributor DMS Stock in SAP **
DATA: gt_ymard_nsap TYPE STANDARD TABLE OF ymard_nsap,
      gt_ymard_nsap1 TYPE STANDARD TABLE OF ymard_nsap,
      gs_ymard_nsap TYPE ymard_nsap.
** Variable for Input JSON for API **
DATA :ls_json    TYPE string,
      v_jsonload TYPE string.

DATA: lv_kunnr TYPE kna1-kunnr. "Selection Screen Input
DATA: lv_lines TYPE i.
DATA: gv_token TYPE string,
      gv_msg TYPE string.
