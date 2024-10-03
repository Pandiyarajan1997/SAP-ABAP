FUNCTION zbapi_sales_order_details.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(SALEDOCUMENT_NO) TYPE  VBELN OPTIONAL
*"     VALUE(KUNNR) TYPE  KUNNR OPTIONAL
*"     VALUE(FROM_DATE) TYPE  DATUM OPTIONAL
*"     VALUE(TO_DATE) TYPE  DATUM OPTIONAL
*"  TABLES
*"      IT_SALES_ORDERS STRUCTURE  ZSTR_SALES_ORDER
*"  EXCEPTIONS
*"      ENTER_DATE
*"----------------------------------------------------------------------
*-----------------------------------------------------------------------
* Created By: Samsudeen M
* Created On: 20.09.2022
* Reference By: Suresh B.V and Praveen
* Sales Order Details for MIS
*------------------------------------------------------------------------
  TYPES: BEGIN OF ty_vbak, "Sales Document Header
           vbeln TYPE vbeln,
           erdat TYPE erdat,
           audat TYPE audat,
           kunnr TYPE kunag,
           cmgst TYPE cmgst,
         END OF ty_vbak.

  DATA: lt_vbak TYPE TABLE OF ty_vbak,
        ls_vbak TYPE ty_vbak.

  DATA: lv_vbeln TYPE bapivbeln-vbeln. "Variable for BAPI

** Header Details For Sales Document  **
  DATA: lt_headers TYPE TABLE OF bapisdhd,
        ls_headers TYPE bapisdhd.

** Table for Sales Document  items Detaisls **
  DATA: lt_lineitems TYPE TABLE OF bapisditbos,
        ls_lineitems TYPE bapisditbos.

** Table for Sales Document  Item status Details **
  DATA: lt_itemstatus TYPE TABLE OF bapisditst,
        ls_itemstatus TYPE bapisditst.

  DATA: lt_sales TYPE TABLE OF zstr_sales_order,
        ls_sales TYPE zstr_sales_order.

** Error Table **
  DATA: lt_return TYPE TABLE OF bapiret2,
        ls_return TYPE bapiret2.

  DATA: lt_domain  TYPE TABLE OF dd07v,
        lt_domain1 TYPE TABLE OF dd07v,
        ls_domain  TYPE dd07v.

** Slection Starts Based on Input **
  IF from_date IS NOT INITIAL AND to_date IS NOT INITIAL
                              AND saledocument_no IS INITIAL AND kunnr IS INITIAL.
    REFRESH lt_vbak.
    SELECT vbeln
           erdat
           audat
           kunnr
           cmgst FROM vbak
                 INTO TABLE lt_vbak
                 WHERE ( erdat BETWEEN from_date AND to_date ).
    IF sy-subrc EQ 0.
      SORT lt_vbak[] BY vbeln.
    ENDIF.
  ELSEIF from_date IS INITIAL AND to_date IS INITIAL
                              AND saledocument_no IS NOT INITIAL AND kunnr IS INITIAL.
    REFRESH lt_vbak.
    SELECT vbeln
           erdat
           audat
           kunnr
           cmgst FROM vbak
                 INTO TABLE lt_vbak
                 WHERE vbeln EQ saledocument_no.

  ELSEIF from_date IS INITIAL AND to_date IS INITIAL
                              AND saledocument_no IS INITIAL AND kunnr IS NOT INITIAL.
    REFRESH lt_vbak.
    SELECT vbeln
           erdat
           audat
           kunnr
           cmgst FROM vbak
                 INTO TABLE lt_vbak
                 WHERE kunnr EQ kunnr.


  ELSEIF from_date IS NOT INITIAL AND to_date IS INITIAL
                                  AND saledocument_no IS INITIAL AND kunnr IS INITIAL.
    REFRESH lt_vbak.
    SELECT vbeln
           erdat
           audat
           kunnr
           cmgst FROM vbak
                 INTO TABLE lt_vbak
                 WHERE erdat EQ from_date.
    IF sy-subrc EQ 0.
      SORT lt_vbak[] BY vbeln.
    ENDIF.
  ENDIF.

** Customer Name Selections **
  SELECT kunnr, name1 FROM kna1 INTO TABLE @DATA(lt_kna1).
  IF sy-subrc EQ 0.
    SORT lt_kna1[] BY kunnr.
  ENDIF.
** Reason Rejection **
  SELECT * FROM tvagt INTO TABLE @DATA(lt_tvagt) WHERE spras EQ @sy-langu..
  IF sy-subrc EQ 0.
    SORT lt_tvagt[] BY abgru.
  ENDIF.

** Getting Domain Values **
  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      domname    = 'STATV'
    TABLES
      values_tab = lt_domain[].
** Getting Domain Values **
  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      domname    = 'CMGST'
    TABLES
      values_tab = lt_domain1[].

  SORT lt_domain[] BY  domvalue_l.
  SORT lt_domain1[] BY domvalue_l.

** Getting one by one sales order Details from Standard BAPI **
  LOOP AT lt_vbak INTO ls_vbak.

    CLEAR: lv_vbeln,ls_headers.
    lv_vbeln = ls_vbak-vbeln.

    REFRESH: lt_return,lt_lineitems,lt_itemstatus.
** Function Module For Getting Sales Document Details **
    CALL FUNCTION 'BAPI_SALESORDER_GETDETAILBOS'
      EXPORTING
        salesdocument    = lv_vbeln
      IMPORTING
        orderheader      = ls_headers
      TABLES
        return           = lt_return
        orderitems       = lt_lineitems
        orderstatusitems = lt_itemstatus.

** Preparing output tables for sales order **
    LOOP AT lt_lineitems INTO ls_lineitems.
      ls_sales-salesdoc_no = ls_lineitems-doc_number. "Sale Doc Number
      ls_sales-item_no = ls_lineitems-itm_number.     "Sale Doc Item No
      ls_sales-doc_type = ls_headers-doc_type.       "Sale Doc Type
      ls_sales-doc_date = ls_headers-doc_date.       "Sale Doc Date
      ls_sales-sale_org = ls_headers-sales_org.      "Sale Org
      ls_sales-dist_chan = ls_headers-distr_chan.    "Dist Channel
      ls_sales-division = ls_headers-division.       "Division
      ls_sales-sale_office = ls_headers-sales_off.   "Sale Office
      ls_sales-plant = ls_lineitems-plant.           "Plant
      ls_sales-material = ls_lineitems-material.     "Material
      ls_sales-material_desc = ls_lineitems-short_text. "material Desc
      ls_sales-order_quan = ls_lineitems-req_qty.    "Order Quantity
      ls_sales-unit_of_measure = ls_lineitems-base_uom. "Order UOM
      ls_sales-order_value = ls_lineitems-net_value. "Order Value
      ls_sales-currency = ls_lineitems-currency. "Order Currency
      ls_sales-customer = ls_headers-sold_to.  "Customer
      READ TABLE lt_kna1 INTO DATA(ls_kna1) WITH KEY kunnr = ls_headers-sold_to BINARY SEARCH.
      IF sy-subrc EQ 0.
        ls_sales-cust_name = ls_kna1-name1. "Customer Name
      ENDIF.
      ls_sales-reason_of_rejec = ls_lineitems-rea_for_re. "Reason_for_rejec
      READ TABLE lt_tvagt INTO DATA(ls_tvagt) WITH KEY abgru = ls_lineitems-rea_for_re BINARY SEARCH.
      IF sy-subrc EQ 0.
        ls_sales-reason_desc = ls_tvagt-bezei. "Reason_for_rejec Desc
      ENDIF.
      CLEAR ls_itemstatus.
      READ TABLE lt_itemstatus INTO ls_itemstatus WITH KEY sd_doc = ls_lineitems-doc_number
                                                          itm_number = ls_lineitems-itm_number.
      IF sy-subrc EQ 0.
        ls_sales-overall_status = ls_itemstatus-ovrprocsta. "Status of item
        CLEAR ls_domain.
        READ TABLE lt_domain INTO ls_domain WITH KEY domvalue_l = ls_sales-overall_status BINARY SEARCH.
        IF sy-subrc EQ 0.
          ls_sales-ovrall_stat_des = ls_domain-ddtext.  "Overall Status Desc
        ENDIF.
        ls_sales-delivery_status = ls_itemstatus-deliv_stat. "Status of Delivery
        CLEAR ls_domain.
        READ TABLE lt_domain INTO ls_domain WITH KEY domvalue_l = ls_sales-delivery_status BINARY SEARCH.
        IF sy-subrc EQ 0.
          ls_sales-del_stat_des = ls_domain-ddtext.   "Delivery Status Desc
        ENDIF.
      ENDIF.
      ls_sales-credit_status = ls_vbak-cmgst. " Credit status
      CLEAR ls_domain.
      READ TABLE lt_domain1 INTO ls_domain WITH KEY domvalue_l = ls_vbak-cmgst BINARY SEARCH.
      IF sy-subrc EQ 0.
        ls_sales-credit_desc = ls_domain-ddtext. "Credit Status Desc
      ENDIF.
      APPEND ls_sales TO it_sales_orders.
      CLEAR: ls_sales,ls_lineitems.
    ENDLOOP.
    CLEAR ls_vbak.
  ENDLOOP.

ENDFUNCTION.
