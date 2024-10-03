FUNCTION zbapi_open_po_details2.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(BUKRS) TYPE  BUKRS
*"     VALUE(SUPPLIER) TYPE  LIFNR OPTIONAL
*"     VALUE(BSTYP) TYPE  EBSTYP OPTIONAL
*"     VALUE(BSART) TYPE  ESART OPTIONAL
*"     VALUE(EBELN) TYPE  EBELN OPTIONAL
*"     VALUE(FROM_DATE) TYPE  DATUM OPTIONAL
*"     VALUE(TO_DATE) TYPE  DATUM OPTIONAL
*"  TABLES
*"      IT_OPEN_PO STRUCTURE  ZST_PO_DETAILS
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
           bedat TYPE ebdat,
           waers TYPE waers,
           ekorg TYPE ekorg,
           lands TYPE land1_stml,
           zterm TYPE dzterm,
           zbd1t TYPE dzbdet,
           inco1 TYPE inco1,
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
           mwskz    TYPE mwskz,
           peinh    TYPE epein,
           txjcd    TYPE txjcd,
           matkl    TYPE matkl,
           meins    TYPE bstme,
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
  DATA: ls_purchase  TYPE zst_po_details.

  CONSTANTS: lc_auto(20)   TYPE c VALUE 'AUTO',
             lc_normal(25) TYPE c VALUE 'MANUAL',
             lc_stat1(20)  TYPE c VALUE 'OPEN'.
  DATA: lv_balance TYPE etmen.
  DATA: lv_bal_amount TYPE bwert.

  DATA: gv_stax TYPE kbetr_kond,
        gv_itax TYPE kbetr_kond,
        gv_ctax TYPE kbetr_kond.

  DATA: wa_taxcom TYPE taxcom,
        lt_komv   TYPE komv OCCURS 20,
        wa_komv   TYPE komv,
        wa_ekpo1  TYPE ekpo.

**Added by samsudeen on 08.11.2022
  DATA: gs_komk TYPE komk,
        gs_komp TYPE komp.
*************************************

  REFRESH:    lt_ekpo,lt_ekko,lt_eket,it_open_po.
  CLEAR: ls_ekpo,ls_ekko,ls_eket.

  DATA: lr_bstyp TYPE RANGE OF ekko-bstyp,
        lr_bsart TYPE RANGE OF ekko-bsart.
*        ls_bstyp LIKE LINE OF lr_bstyp,
*        ls_bsart LIKE LINE OF lr_bsart.

  DATA: lr_bukrs    TYPE RANGE OF ekko-bukrs,
        lr_ebeln    TYPE RANGE OF ekko-ebeln,
        lr_date     TYPE RANGE OF ekko-aedat,
        lr_supplier TYPE RANGE OF ekko-lifnr.

  DATA: lr_kschl TYPE RANGE OF konp-kschl,
        ls_kschl LIKE LINE OF lr_kschl.

  IF bukrs IS NOT INITIAL.
    lr_bukrs = VALUE #( ( sign = 'I'
                        option = 'EQ'
                        low = bukrs ) ).
  ENDIF.
  IF supplier IS NOT INITIAL.
    lr_supplier = VALUE #( ( sign = 'I'
                        option = 'EQ'
                        low = supplier ) ).
  ENDIF.
  IF bstyp IS NOT INITIAL.
    lr_bstyp = VALUE #( ( sign = 'I'
                        option = 'EQ'
                        low = bstyp ) ).
  ELSE.
    lr_bstyp = VALUE #( ( sign = 'I'
                        option = 'EQ'
                        low = 'F' ) ).

  ENDIF.
  IF bsart IS NOT INITIAL.
    lr_bsart = VALUE #( ( sign = 'I'
                        option = 'EQ'
                        low = bsart ) ).
  ENDIF.
  IF ebeln IS NOT INITIAL.
    lr_ebeln = VALUE #( ( sign = 'I'
                        option = 'EQ'
                        low = ebeln ) ).
  ENDIF.
  IF from_date IS NOT INITIAL AND to_date IS NOT INITIAL.
    lr_date = VALUE #( ( sign = 'I'
                        option = 'BT'
                        low = from_date
                        high = to_date ) ).
  ENDIF.

** Vendor wise report **
  SELECT bukrs
         ebeln
         reswk
         bsart
         ernam
         aedat
         lifnr
         bedat
         waers
         ekorg
         lands
         zterm
         zbd1t
         inco1 FROM ekko
               INTO TABLE lt_ekko
               WHERE  bukrs IN lr_bukrs
               AND    ebeln IN lr_ebeln
               AND    lifnr IN lr_supplier
               AND    bstyp IN lr_bstyp
               AND    bsart IN lr_bsart
               AND    aedat IN lr_date
               AND    loekz = space. " Deletion Indicator.
  IF sy-subrc = 0.
    SORT lt_ekko[] BY ebeln.
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
           mwskz
           peinh
           txjcd
           matkl
           meins
           zremarks FROM ekpo
                    INTO TABLE lt_ekpo
                      FOR ALL ENTRIES IN lt_ekko
                      WHERE bukrs EQ lt_ekko-bukrs
                      AND ebeln EQ lt_ekko-ebeln
                      AND loekz = space " Deletion Indicator
                      AND elikz NE 'X'. " Delivery Indicator
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

*** Vendor Data from LFA1 **
    SELECT lifnr, name1, land1, regio FROM lfa1 INTO TABLE @DATA(lt_lfa1).
    IF sy-subrc EQ 0.
      SORT lt_lfa1[] BY lifnr.
    ENDIF.
*** Plant region data **
    SELECT werks, regio FROM t001w INTO TABLE @DATA(lt_plant)
                        WHERE spras EQ @sy-langu.
    IF sy-subrc EQ 0.
      SORT lt_plant[] BY werks.
    ENDIF.

  ENDIF.


  SELECT * FROM tvarvc INTO TABLE @DATA(lt_tvarvc) WHERE name EQ 'ZPARAMETER_BAPI'
                                                   AND type EQ 'S'.
  SORT lt_tvarvc[] BY low.

*** Tax type inclusion for selection ****
  SELECT * FROM tvarvc INTO TABLE @DATA(gt_tvarvc) WHERE name EQ 'ZPO_GST'
                                                   AND type = 'S'.
  LOOP AT gt_tvarvc INTO DATA(gs_tvarvc).
    ls_kschl-sign = 'I'.
    ls_kschl-option = 'EQ'.
    ls_kschl-low = gs_tvarvc-low.
    APPEND ls_kschl TO lr_kschl.
    CLEAR ls_kschl.
  ENDLOOP.

*** GST Variable for Calculation **
  SELECT kschl, kbetr, mwsk1 FROM konp
                             INTO TABLE @DATA(lt_gst)
                             FOR ALL ENTRIES IN @lt_ekpo
                             WHERE kappl = 'TX'
                             AND kschl IN @lr_kschl
                             AND mwsk1 EQ @lt_ekpo-mwskz.
  IF sy-subrc EQ 0.
    SORT lt_gst[] BY kschl mwsk1.
  ENDIF.

*********Looping ekpo table for getting all Line items*************
  LOOP AT lt_ekpo INTO ls_ekpo.

    ls_purchase-bukrs = ls_ekpo-bukrs.  "Comp Code
    ls_purchase-recieving_plant = ls_ekpo-werks. "plant
    ls_purchase-ebeln = ls_ekpo-ebeln. "Po Number
    ls_purchase-ebelp = ls_ekpo-ebelp. "Po Item Number
    ls_purchase-matnr = ls_ekpo-matnr. "Material
    ls_purchase-item_text = ls_ekpo-zremarks.  "Remarks
    ls_purchase-material_grp = ls_ekpo-matkl. "Material Grp

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
      ls_purchase-zterm = ls_ekko-zterm.
      ls_purchase-zbd1t = ls_ekko-zbd1t.
      ls_purchase-inco1 = ls_ekko-inco1.
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

***************** Tax Amount calculation added by samsudeen *************************************
**********Integrated GST Calculation ************************************************************
    READ TABLE lt_gst INTO DATA(ls_gst) WITH KEY kschl = 'JIIG'
                                                 mwsk1 = ls_ekpo-mwskz BINARY SEARCH.
    IF sy-subrc EQ 0.
      CLEAR gv_itax.
      gv_itax = ( ls_gst-kbetr / 10 ).
      ls_purchase-igst = ( ls_ekpo-netwr * gv_itax ) / 100.
    ENDIF.
************** State GST Calculation ***********************************************************
    READ TABLE lt_gst INTO DATA(ls_gst1) WITH KEY kschl = 'JICG'
                                             mwsk1 = ls_ekpo-mwskz BINARY SEARCH.
    IF sy-subrc EQ 0.
      CLEAR gv_ctax.
      gv_ctax = ( ls_gst1-kbetr / 10 ).
      ls_purchase-cgst = ( ls_ekpo-netwr * gv_ctax ) / 100.
    ENDIF.
***************** Central GST Calculation ********************************************************
    READ TABLE lt_gst INTO DATA(ls_gst2) WITH KEY kschl = 'JISG'
                                         mwsk1 = ls_ekpo-mwskz BINARY SEARCH.
    IF sy-subrc EQ 0.
      CLEAR gv_stax.
      gv_stax = ( ls_gst1-kbetr / 10 ).
      ls_purchase-sgst = ( ls_ekpo-netwr * gv_stax ) / 100.
    ENDIF.
**************************************************************************************************

    APPEND ls_purchase TO it_open_po.
    CLEAR: ls_ekpo,ls_purchase.
  ENDLOOP.

  IF it_open_po[] IS NOT INITIAL.
    SORT it_open_po[] BY  bukrs recieving_plant ebeln ebelp.
  ENDIF.
ENDFUNCTION.
