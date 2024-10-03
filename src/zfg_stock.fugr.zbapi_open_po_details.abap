FUNCTION zbapi_open_po_details.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(BUKRS) TYPE  BUKRS OPTIONAL
*"     VALUE(SUPPLIER) TYPE  LIFNR OPTIONAL
*"  TABLES
*"      IT_OPEN_PO STRUCTURE  ZSTR_PO_DETAILS
*"----------------------------------------------------------------------
  "Created on: 17.10.2022
  "Created by: Samsudeen M
  "Reference by: Suresh B.V and Nagarathinam
  "Description: As on Day overall Open PO
*-----------------------------------------------------------------------
  TYPES: BEGIN OF ty_ekko,        "Structure For PO header Data
           bukrs TYPE bukrs,
           ebeln TYPE ebeln,
           reswk TYPE reswk,
           bsart TYPE esart,
           ernam TYPE ernam,
           aedat TYPE erdat,
           lifnr TYPE elifn,
         END OF ty_ekko,
         BEGIN OF ty_ekpo,                    "Structure For PO Item Data
           bukrs    TYPE bukrs,
           werks    TYPE werks_d,
           ebeln    TYPE ebeln,
           ebelp    TYPE ebelp,
           matnr    TYPE matnr,
           txz01    TYPE txz01,
           mtart    TYPE mtart,
           menge    TYPE bstmg,
           netpr    TYPE bprei,
           netwr    TYPE netwr,
           elikz    TYPE elikz,
           webre    TYPE webre,
           zremarks TYPE zremarks,
         END OF ty_ekpo,
         BEGIN OF ty_eket,     " Structure For Balance PO Quantity
           ebeln TYPE ebeln,
           ebelp TYPE ebelp,
           menge TYPE etmen,
           wemng TYPE weemg,
         END OF ty_eket,
         BEGIN OF ty_mseg,        "Structure for GR quantity and GR Value
           matnr TYPE matnr,
           bwart TYPE bwart,
           werks TYPE werks_d,
           ebeln TYPE ebeln,
           ebelp TYPE ebelp,
           dmbtr TYPE dmbtr_cs,
           erfmg TYPE erfmg,
           erfme TYPE erfme,
         END OF ty_mseg.
**********Internal Table For PO Details ****************
  DATA: lt_ekpo TYPE STANDARD TABLE OF ty_ekpo,
        lt_ekko TYPE STANDARD TABLE OF ty_ekko,
        lt_eket TYPE STANDARD TABLE OF ty_eket,
        lt_mseg TYPE STANDARD TABLE OF ty_mseg.

**********work Area For po details ****************
  DATA: ls_ekpo TYPE ty_ekpo,
        ls_ekko TYPE ty_ekko,
        ls_eket TYPE ty_eket,
        ls_mseg TYPE ty_mseg.

**********Work Area for Final Internal Table*************
  DATA: lt_purchase TYPE STANDARD TABLE OF zstr_po_details.
  DATA: ls_purchase  TYPE zstr_po_details.

  CONSTANTS: lc_auto(20)   TYPE c VALUE 'AUTO',
             lc_normal(25) TYPE c VALUE 'MANUAL',
             lc_stat1(20)  TYPE c VALUE 'OPEN'.
  DATA: lv_balance TYPE etmen.
  DATA: lv_bal_amount TYPE bwert.

  REFRESH:    lt_ekpo,lt_ekko,lt_eket,it_open_po.
  CLEAR: ls_ekpo,ls_ekko,ls_eket.

  IF bukrs IS INITIAL AND supplier IS INITIAL.
** IF no INPUT just overall run **
    SELECT bukrs
           werks
           ebeln
           ebelp
           matnr
           txz01
           mtart
           menge
           netpr
           netwr
           elikz
           webre
           zremarks FROM ekpo
                    INTO TABLE lt_ekpo
                    WHERE loekz = space " Deletion Indicator
                          AND elikz NE 'X'. " Delivery Indicator
  ELSEIF bukrs IS NOT INITIAL AND supplier IS INITIAL.
** IF comapny code is in INPUT **
    SELECT bukrs
           werks
           ebeln
           ebelp
           matnr
           txz01
           mtart
           menge
           netpr
           netwr
           elikz
           webre
           zremarks FROM ekpo
                    INTO TABLE lt_ekpo
                    WHERE bukrs EQ bukrs
                    AND   loekz = space " Deletion Indicator
                          AND elikz NE 'X'. " Delivery Indicator
  ELSEIF bukrs IS NOT INITIAL AND supplier IS NOT INITIAL.
** If company code and supplier input means specific supplier **
    SELECT bukrs
           ebeln
           reswk
           bsart
           ernam
           aedat
           lifnr FROM ekko
                 INTO TABLE lt_ekko
                 WHERE bukrs EQ bukrs
                 AND loekz = space " Deletion Indicator
                 AND lifnr EQ supplier.
    IF sy-subrc EQ 0.
      SORT lt_ekko[] BY bukrs ebeln.
      SELECT bukrs
             werks
             ebeln
             ebelp
             matnr
             txz01
             mtart
             menge
             netpr
             netwr
             elikz
             webre
             zremarks FROM ekpo
                      INTO TABLE lt_ekpo
                      FOR ALL ENTRIES IN lt_ekko
                      WHERE bukrs EQ lt_ekko-bukrs
                      AND ebeln EQ lt_ekko-ebeln
                      AND loekz = space " Deletion Indicator
                      AND elikz NE 'X'. " Delivery Indicator
    ENDIF.
  ENDIF.

  IF lt_ekpo[] IS NOT INITIAL.
************ Fetching PO Quantity and GR Quantity ************
    SELECT ebeln
           ebelp
           menge
           wemng FROM eket
                 INTO TABLE lt_eket
                 FOR ALL ENTRIES IN lt_ekpo
                 WHERE ebeln EQ lt_ekpo-ebeln
                 AND ebelp EQ lt_ekpo-ebelp.
    IF sy-subrc EQ 0.
      SORT lt_eket[] BY ebeln ebelp.
    ENDIF.
    IF lt_ekko[] IS INITIAL.
** IF company code and supplier not in INPUT means specific supplier **
      SELECT bukrs
             ebeln
             reswk
             bsart
             ernam
             aedat
             lifnr FROM ekko
                   INTO TABLE lt_ekko
                   FOR ALL ENTRIES IN lt_ekpo
                   WHERE bukrs EQ lt_ekpo-bukrs
                   AND ebeln EQ lt_ekpo-ebeln.
      SORT lt_ekko[] BY bukrs ebeln lifnr.
    ENDIF.
*******Fetching GR quantity and GR value from MSEG table ********
    SELECT matnr
           bwart
           werks
           ebeln
           ebelp
           dmbtr
           erfmg
           erfme FROM mseg
                 INTO TABLE lt_mseg
                 FOR ALL ENTRIES IN lt_ekpo
                 WHERE matnr EQ lt_ekpo-matnr
                 AND bwart IN ( '101' , '102' )
                 AND werks EQ lt_ekpo-werks
                 AND ebeln EQ lt_ekpo-ebeln
                 AND ebelp EQ lt_ekpo-ebelp.
    IF sy-subrc EQ 0.
      SORT lt_mseg[] BY matnr werks ebeln ebelp.
    ENDIF.
*************** Fetching Material Group from MARA ***********************
    SELECT matnr,matkl FROM mara INTO TABLE @DATA(lt_mara)
                       FOR ALL ENTRIES IN @lt_ekpo
                       WHERE matnr EQ @lt_ekpo-matnr.
    IF sy-subrc EQ 0.
      SORT lt_mara[] BY matnr.
    ENDIF.
*** Vendor Data from LFA1 **
    SELECT lifnr, name1 FROM lfa1 INTO TABLE @DATA(lt_lfa1).
    IF sy-subrc EQ 0.
      SORT lt_lfa1[] BY lifnr.
    ENDIF.
  ENDIF.


  SELECT * FROM tvarvc INTO TABLE @DATA(lt_tvarvc) WHERE name EQ 'ZPARAMETER_BAPI'
                                                   AND type EQ 'S'.
  SORT lt_tvarvc[] BY low.
*********Looping ekpo table for getting all Line items*************
  LOOP AT lt_ekpo INTO ls_ekpo.

    ls_purchase-bukrs = ls_ekpo-bukrs.  "Comp Code
    ls_purchase-recieving_plant = ls_ekpo-werks. "plant
    ls_purchase-ebeln = ls_ekpo-ebeln. "Po Number
    ls_purchase-ebelp = ls_ekpo-ebelp. "Po Item Number
    ls_purchase-matnr = ls_ekpo-matnr. "Material
    ls_purchase-item_text = ls_ekpo-zremarks.  "Remarks
    READ TABLE lt_mara INTO DATA(ls_mara) WITH KEY matnr = ls_ekpo-matnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      ls_purchase-material_grp = ls_mara-matkl. "Material Grp
    ENDIF.
    ls_purchase-txz01 = ls_ekpo-txz01.
    ls_purchase-mtart = ls_ekpo-mtart. "Materrial type
    ls_purchase-menge = ls_ekpo-menge. "Po quan
    ls_purchase-netwr = ls_ekpo-netwr. "Po value
    ls_purchase-elikz = ls_ekpo-elikz. "Delivery Indic

    IF ls_purchase-elikz NE 'X'.
      ls_purchase-status = lc_stat1. "PO status
    ENDIF.

    CLEAR ls_ekko.
    READ TABLE lt_ekko INTO ls_ekko WITH KEY bukrs = ls_ekpo-bukrs
                                             ebeln = ls_ekpo-ebeln BINARY SEARCH.
    IF sy-subrc EQ 0.
      ls_purchase-supplier = ls_ekko-lifnr. "Supplier
      READ TABLE lt_lfa1 INTO DATA(ls_lfa1) WITH KEY lifnr = ls_purchase-supplier BINARY SEARCH.
      IF sy-subrc EQ 0.
        ls_purchase-supplier_nme = ls_lfa1-name1. "Supplier name
      ENDIF.
      ls_purchase-supplying_plant = ls_ekko-reswk. "Supplying Plant
      ls_purchase-document_type =  ls_ekko-bsart. "PO type
      ls_purchase-created_by = ls_ekko-ernam. "Po Created by
** po_category **
      READ TABLE lt_tvarvc INTO DATA(ls_tvarvc) WITH KEY low = ls_ekko-ernam BINARY SEARCH.
      IF sy-subrc EQ 0.
        ls_purchase-po_category = lc_auto.
      ELSE.
        ls_purchase-po_category = lc_normal.
      ENDIF.
      ls_purchase-creation_date = ls_ekko-aedat. "PO date
    ENDIF.

    CLEAR ls_eket.
    READ TABLE lt_eket INTO ls_eket WITH KEY ebeln = ls_ekpo-ebeln
                                             ebelp = ls_ekpo-ebelp BINARY SEARCH.
    IF sy-subrc EQ 0.

      CLEAR lv_balance.
      lv_balance = ( ls_eket-menge - ls_eket-wemng ). " Balance Quantity
      ls_purchase-po_bal_quan = lv_balance.

      CLEAR lv_bal_amount.
      lv_bal_amount = ls_purchase-po_bal_quan * ls_ekpo-netpr.  " Balance Quantity Amount
      ls_purchase-po_bal_amount = lv_bal_amount.
    ENDIF.

    CLEAR ls_mseg.
    READ TABLE lt_mseg INTO ls_mseg WITH KEY matnr = ls_ekpo-matnr
                                             werks = ls_ekpo-werks
                                             ebeln = ls_ekpo-ebeln
                                             ebelp = ls_ekpo-ebelp BINARY SEARCH.
    IF sy-subrc EQ 0.
      ls_purchase-gr_quantity = ls_mseg-erfmg. " Converted GR Quantity
      ls_purchase-gr_quan_unit = ls_mseg-erfme. "unit of Measure
      ls_purchase-gr_value = ls_mseg-dmbtr. "GRN value
    ENDIF.

    APPEND ls_purchase TO it_open_po.
    CLEAR: ls_ekpo,ls_purchase.
  ENDLOOP.

  IF it_open_po[] IS NOT INITIAL.
    SORT it_open_po[] BY  bukrs recieving_plant ebeln ebelp.
  ENDIF.
ENDFUNCTION.
