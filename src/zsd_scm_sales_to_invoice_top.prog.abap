*&---------------------------------------------------------------------*
*& Include          ZSD_SCM_SALES_TO_INVOICE_TOP
*&---------------------------------------------------------------------*
DATA: gt_delivery TYPE STANDARD TABLE OF zsd_scm_header.
DATA: gt_invoice TYPE STANDARD TABLE OF zsd_scm_header.
DATA: gt_alv TYPE STANDARD TABLE OF zsd_scm_header.
DATA: msg TYPE char50.
DATA: l_msg TYPE string.
DATA: lv_msg TYPE string.
DATA: gs_update TYPE zsd_scm_header.


CONSTANTS: c_sal_status TYPE num02 VALUE '11',  "Sales Order status
           c_del_status TYPE num02 VALUE '12',  "Delivery Status
           c_pgi_status TYPE num02 VALUE '13',  "Post Goods Issue
           c_inv_status TYPE num02 VALUE '14',  "Invoices
           c_irn_status TYPE num02 VALUE '15',  "E-invoice
           c_ebn_status TYPE num02 VALUE '16',  "E-invoice
           c_com_status TYPE num02 VALUE '17',  "Completed
           c_lgort      TYPE lgort_d VALUE '0006', "Storage location
           c_success    TYPE c VALUE 'S', "Success
           c_error      TYPE c VALUE 'E'. "Error

"Instance Object for the Class
DATA :lo_object_cls TYPE REF TO zcl_scm_sales_to_invoice.
CREATE OBJECT lo_object_cls.
