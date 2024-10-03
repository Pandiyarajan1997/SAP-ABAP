*&---------------------------------------------------------------------*
*& Include          ZINCL_CUST_COMMS_TOP
*&---------------------------------------------------------------------*
TYPES:BEGIN OF ty_salv,
      Invoicekey type ZSD_SF_CUST_INV-invoicekey,
      Company_Code TYPE ZSD_SF_CUST_INV-bukrs,
      Customer TYPE ZSD_SF_CUST_INV-custno,
      Status TYPE ZSD_SF_CUST_INV-status,
      payment_id TYPE ZSD_SF_CUST_INV-payment_refid,
      payment_date TYPE ZSD_SF_CUST_INV-payment_date,
      sent_stat  TYPE string,
      END OF ty_salv.

DATA: lv_invkey TYPE ZSD_SF_CUST_INV-invoicekey,
      lv_cust type kunnr,
      gt_salv_data type table of ty_salv.
DATA: gt_rf_data type table of ZSD_SF_CUST_INV,
      response type string,
      ls_mail_resp type string,
      ls_whats_resp type string.

 DATA: Lv_json type string.
       DATA: lo_log_upd TYPE REF TO zcl_api_log_entries_store.
    CREATE OBJECT lo_log_upd.
