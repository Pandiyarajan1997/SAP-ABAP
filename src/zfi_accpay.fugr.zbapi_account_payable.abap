FUNCTION zbapi_account_payable.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(COMP_CODE) TYPE  BUKRS
*"     VALUE(VENDOR) TYPE  RANGE_LIFNR OPTIONAL
*"     VALUE(CURRENT_DATE) TYPE  DATUM DEFAULT SY-DATUM
*"  TABLES
*"      LT_VENDOR_OPEN_ITEMS STRUCTURE  ZSTR_BSIK
*"  EXCEPTIONS
*"      INCORRECT_COMPCODE
*"      VENDOR_NOT_AVAILABLE
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(FISCAL_YEAR) TYPE  GJAHR
*"     VALUE(MONTH) TYPE  ZMONTHS
*"  TABLES
*"      LT_BSIK STRUCTURE  ZSTR_BSIK
*********************************************************
*& Date Created - 16.05.2022
*& Created By   - KPABAP (Shamsudeen)
*& Description  - Program to get the vendor line items details from FBL1N
*& TR no: DEVK931698

*& Changed on: 25.07.2022
* Changed By : Samsudeen M
* Description: Changing only getting only Vendor open items for sy-datum

*"------------------------------------------------------------------------------

  TYPES: BEGIN OF ty_bukrs,
           lifnr TYPE lifnr,
           bukrs TYPE bukrs,
           akont TYPE akont,
         END OF ty_bukrs.

  DATA: lt_vendor_open  TYPE TABLE OF bapi3008_2, " Function Module Structure
        ls_vendor_open  TYPE bapi3008_2,
        lt_vendor_open1 TYPE TABLE OF bapi3008_2.

  DATA: lt_bukrs_ven TYPE TABLE OF ty_bukrs,
        ls_bukrs_ven TYPE ty_bukrs.

  DATA: lt_open_items TYPE STANDARD TABLE OF zstr_bsik,
        ls_open_items TYPE zstr_bsik.

  TYPES: tr_lifnr TYPE RANGE OF lfb1-lifnr,
         ty_lifnr TYPE LINE OF tr_lifnr.

  DATA: lr_lifnr TYPE tr_lifnr,
        ls_lifnr TYPE ty_lifnr.


  DATA: gs_t001 TYPE t001.
  DATA: jamon1 TYPE jamon_xpo.
  DATA: verzn1 TYPE verzn_cm.

********Company Code Check Whether Valid Company Code or Not ***********
  CLEAR gs_t001.
  SELECT SINGLE * FROM t001 INTO gs_t001 WHERE bukrs = comp_code
                                         AND spras EQ sy-langu
                                         AND land1 EQ 'IN'.
  IF sy-subrc NE 0.
    RAISE incorrect_compcode.
  ENDIF.

  DATA: gs_lfb1 TYPE lfb1.

  IF comp_code IS NOT INITIAL AND vendor IS INITIAL.
***************** Fetching all Vendor Available under given Comp_code*************
    SELECT lifnr
           bukrs
           akont FROM lfb1
                 INTO TABLE lt_bukrs_ven
                 WHERE bukrs EQ comp_code.
    IF sy-subrc EQ 0.
      SORT lt_bukrs_ven[] BY bukrs lifnr.
    ENDIF.

  ELSEIF comp_code IS NOT INITIAL AND vendor-low IS NOT INITIAL
                                  AND vendor-high IS INITIAL.
*********** Fetching Multipe vendor from Input *********************************

    SELECT lifnr
           bukrs
           akont FROM lfb1
                 INTO TABLE lt_bukrs_ven
                 WHERE lifnr EQ vendor-low
                 AND bukrs EQ comp_code.
    IF sy-subrc EQ 0.
      SORT lt_bukrs_ven[] BY bukrs lifnr.
    ENDIF.

  ELSEIF comp_code IS NOT INITIAL AND vendor-low IS NOT INITIAL
                                  AND vendor-high IS NOT INITIAL.
*********** Fetching Multipe vendor from Input *********************************
    SELECT lifnr
           bukrs
           akont FROM lfb1
                 INTO TABLE lt_bukrs_ven
                 WHERE ( lifnr BETWEEN vendor-low AND vendor-high )
                 AND bukrs EQ comp_code.
    IF sy-subrc EQ 0.
      SORT lt_bukrs_ven[] BY bukrs lifnr.
    ENDIF.
  ENDIF.


  IF lt_bukrs_ven[] IS NOT INITIAL.

    REFRESH: lt_vendor_open,lt_open_items.

    CLEAR ls_bukrs_ven.

    LOOP AT lt_bukrs_ven INTO ls_bukrs_ven.
********* Function Module to get Vendor Open Line Items from FBL1N *****************

      CALL FUNCTION 'BAPI_AP_ACC_GETOPENITEMS'
        EXPORTING
          companycode = ls_bukrs_ven-bukrs
          vendor      = ls_bukrs_ven-lifnr
          keydate     = current_date
        TABLES
          lineitems   = lt_vendor_open.
      IF sy-subrc EQ 0.
        SORT lt_vendor_open[] BY comp_code vendor.
        APPEND LINES OF lt_vendor_open TO lt_vendor_open1.
      ENDIF.
    ENDLOOP.
  ENDIF.
  IF lt_vendor_open1[] IS NOT INITIAL.
    SORT lt_vendor_open1[] BY comp_code vendor.
**********Fetching Vendor Name from LFA1 Table *******************
    SELECT lifnr,
           name1,
           ktokk
            FROM lfa1
            INTO TABLE @DATA(lt_lfa1)
            FOR ALL ENTRIES IN @lt_vendor_open1
            WHERE lifnr EQ @lt_vendor_open1-vendor.
    IF sy-subrc EQ 0.
      SORT lt_lfa1[] BY lifnr.
    ENDIF.

*************** Fetching Document Created user name from BKPF Table *******
    SELECT belnr,usnam FROM bkpf
                       INTO TABLE @DATA(lt_bkpf)
                       FOR ALL ENTRIES IN @lt_vendor_open1
                       WHERE belnr EQ @lt_vendor_open1-doc_no.
    IF sy-subrc EQ 0.
      SORT lt_bkpf[] BY belnr.
    ENDIF.

*************** Fetching Payment date from Payr Table *************************
    SELECT lifnr,
           vblnr,
           zaldt FROM payr
                 INTO TABLE @DATA(lt_payr)
                 FOR ALL ENTRIES IN @lt_vendor_open1
                 WHERE lifnr = @lt_vendor_open1-vendor
                 AND vblnr = @lt_vendor_open1-doc_no.
    IF sy-subrc EQ 0.
      SORT lt_payr[] BY lifnr vblnr.
    ENDIF.
  ENDIF.
  " Start by Puratchiveeran for Get Pernr
**Fetching BP code From IBPSUPPLIER**
  DATA(lt_lfa2) = lt_lfa1.
  DATA l_pernr  TYPE persno.
  SORT lt_lfa2 BY ktokk.
  " YBEV Vendor Employee
  DELETE lt_lfa2 WHERE ktokk NE 'YBEV'.
  SELECT supplier,
         businesspartner,
         supplieralternativepayee AS pernr
    FROM ibpsupplier
    INTO TABLE @DATA(lt_supplier_pernr)
    FOR ALL ENTRIES IN @lt_lfa2
    WHERE supplier = @lt_lfa2-lifnr.
  SORT lt_supplier_pernr by supplier.
  LOOP AT lt_supplier_pernr ASSIGNING FIELD-SYMBOL(<lfs_lfa>).
    CALL FUNCTION 'BAPI_BUPA_GET_EMPLOYEE_FROM_BP'
      EXPORTING
        businesspartner = <lfs_lfa>-businesspartner
      IMPORTING
        employeeid      = l_pernr.
    <lfs_lfa>-pernr = l_pernr.
  ENDLOOP.
  " End by Puratchiveeran for Get Pernr
  CLEAR ls_vendor_open.
  REFRESH : lt_vendor_open_items.
  LOOP AT lt_vendor_open1 INTO ls_vendor_open.
    CLEAR ls_open_items.
    ls_open_items-bukrs = ls_vendor_open-comp_code.
    ls_open_items-lifnr = ls_vendor_open-vendor.
    ls_open_items-pernr = VALUE #( lt_supplier_pernr[ supplier = ls_vendor_open-vendor ]-pernr OPTIONAL ). " Added by Puratchiveeran for Get Pernr
    READ TABLE lt_lfa1 INTO DATA(ls_lfa1) WITH KEY lifnr = ls_vendor_open-vendor.
    IF sy-subrc EQ 0.
      ls_open_items-name1_bte = ls_lfa1-name1.
    ENDIF.

    ls_open_items-gjahr = ls_vendor_open-fisc_year. "Fiscal_year
    ls_open_items-augdt = ls_vendor_open-clear_date.
    ls_open_items-dmbtr = ls_vendor_open-lc_amount.  " Amount Pending
    ls_open_items-belnr = ls_vendor_open-doc_no.
    ls_open_items-xblnr = ls_vendor_open-ref_doc_no.
    ls_open_items-blart = ls_vendor_open-doc_type.
    ls_open_items-bldat = ls_vendor_open-doc_date.
    ls_open_items-budat = ls_vendor_open-pstng_date.
    ls_open_items-wears = ls_vendor_open-loc_currcy.
    ls_open_items-shkzg = ls_vendor_open-db_cr_ind.
    ls_open_items-sgtxt = ls_vendor_open-item_text.
    ls_open_items-zterm = ls_vendor_open-pmnttrms. " Payment terms

    CLEAR jamon1.
    CONCATENATE ls_vendor_open-fisc_year '/' ls_vendor_open-fis_period INTO jamon1.
    CONDENSE jamon1.
    ls_open_items-jamon = jamon1.

    READ TABLE lt_bukrs_ven INTO ls_bukrs_ven WITH KEY bukrs = ls_vendor_open-comp_code
                                                       lifnr = ls_vendor_open-vendor.
    IF sy-subrc EQ 0.
      ls_open_items-hkont = ls_bukrs_ven-akont. " G / L Account
    ENDIF.

    ls_open_items-faedt = ls_vendor_open-bline_date + ls_vendor_open-dsct_days1. " Net Due Date

    CLEAR verzn1.
    CALL FUNCTION 'ITEM_OVERDUE_DAYS'
      EXPORTING
        key_date     = sy-datlo
        due_date     = ls_open_items-faedt
      IMPORTING
        overdue_days = verzn1.

    ls_open_items-verzn = verzn1. " Net Due Days


    READ TABLE lt_bkpf INTO DATA(ls_bkpf) WITH KEY belnr = ls_vendor_open-doc_no.
    IF sy-subrc EQ 0.
      ls_open_items-u_usnam = ls_bkpf-usnam.
    ENDIF.

    READ TABLE lt_payr INTO DATA(ls_payr) WITH KEY vblnr = ls_vendor_open-doc_no
                                                   lifnr = ls_vendor_open-vendor.
    IF sy-subrc EQ 0.
      ls_open_items-zaldt = ls_payr-zaldt. "Payment Date
    ENDIF.
    APPEND ls_open_items TO lt_vendor_open_items.
  ENDLOOP.


  IF lt_vendor_open_items[] IS NOT INITIAL.
    SORT lt_vendor_open_items[] BY bukrs lifnr belnr.
  ENDIF.

ENDFUNCTION.
