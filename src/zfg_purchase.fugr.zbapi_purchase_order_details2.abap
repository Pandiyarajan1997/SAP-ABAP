FUNCTION zbapi_purchase_order_details2.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(BUKRS) TYPE  BUKRS
*"     VALUE(BSTYP) TYPE  EBSTYP OPTIONAL
*"     VALUE(BSART) TYPE  ESART OPTIONAL
*"     VALUE(EBELN) TYPE  EBELN OPTIONAL
*"     VALUE(FROM_DATE) TYPE  DATUM
*"     VALUE(TO_DATE) TYPE  DATUM
*"  TABLES
*"      IT_PURCHASE STRUCTURE  ZST_PO_DETAILS
*"----------------------------------------------------------------------
**      Created By:   Samsudeen M
*     Reference by: praveen Kumar
* Description: Getting All Po record line item wise based on date"
* DEVK931786
*
*  " changed_on: 06.07.20222
*  " Adding two fields in output BSART and RESWK
***************************************************************************
***------------------------------------------------------------------------
  "Changed on: 26.07.2022
  "Changed_by: Samsudeen M
  "Description : Changing GR Quantity field for Conversion Quantity
  "Reference by: Nagarathinam
*-----------------------------------------------------------------------------
  TYPES: BEGIN OF ty_ekko,       "Structure For PO header Data
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
         BEGIN OF ty_ekpo,           "Structure For PO Item Data
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
         END OF ty_ekpo,            " Structure For Balance PO Quantity
         BEGIN OF ty_eket,
           ebeln TYPE ebeln,
           ebelp TYPE ebelp,
           menge TYPE etmen,
           wemng TYPE weemg,
         END OF ty_eket,
         BEGIN OF ty_lfa1,            "Structure For Vendor Details Data
           lifnr TYPE lifnr,
           name1 TYPE name1_gp,
         END OF ty_lfa1,
         BEGIN OF ty_mseg,        "Structure for GR quantity and GR Value
           matnr TYPE matnr,
           bwart TYPE bwart,
           werks TYPE werks_d,
           ebeln TYPE ebeln,
           ebelp TYPE ebelp,
           dmbtr TYPE dmbtr_cs,
*           menge TYPE menge_d,
           erfmg TYPE erfmg,
           erfme TYPE erfme,
         END OF ty_mseg.


**********Internal Table For PO Details ****************
  DATA: lt_ekpo TYPE STANDARD TABLE OF ty_ekpo,
        lt_ekko TYPE STANDARD TABLE OF ty_ekko,
        lt_eket TYPE STANDARD TABLE OF ty_eket,
        lt_mseg TYPE STANDARD TABLE OF ty_mseg,
        lt_lfa1 TYPE STANDARD TABLE OF ty_lfa1.

**********Work Area For PO Details ****************
  DATA: ls_ekpo TYPE ty_ekpo,
        ls_ekko TYPE ty_ekko,
        ls_eket TYPE ty_eket,
        ls_mseg TYPE ty_mseg,
        ls_lfa1 TYPE ty_lfa1.

  DATA: lt_tvarvc TYPE STANDARD TABLE OF tvarvc,
        ls_tvarvc TYPE tvarvc.

**********Work Area for Final Internal Table*************
  DATA: lt_purchase TYPE STANDARD TABLE OF zst_po_details.
  DATA: ls_purchase  TYPE zst_po_details.

  DATA: lv_balance TYPE etmen.
  DATA: lv_bal_amount TYPE bwert.
  CONSTANTS: lc_auto(20)   TYPE c VALUE 'AUTO',
             lc_normal(25) TYPE c VALUE 'MANUAL',
             lc_stat1(20)  TYPE c VALUE 'OPEN',
             lc_stat2(20)  TYPE c VALUE 'CLOSED'.
  DATA: lr_bstyp TYPE RANGE OF ekko-bstyp,
        lr_bsart TYPE RANGE OF ekko-bsart,
        ls_bstyp LIKE LINE OF lr_bstyp,
        ls_bsart LIKE LINE OF lr_bsart.
  DATA: lv_name TYPE thead-tdname.
  DATA: lt_text TYPE TABLE OF tline.

  DATA: lr_kschl TYPE RANGE OF konp-kschl,
        ls_kschl LIKE LINE OF lr_kschl.
  DATA: gv_stax TYPE kbetr_kond,
        gv_itax TYPE kbetr_kond,
        gv_ctax TYPE kbetr_kond.

  DATA: wa_taxcom TYPE taxcom,
        lt_komv   TYPE komv OCCURS 20,
        wa_komv   TYPE komv,
*            wa_ekko1  TYPE ekko,
*            it_ekpo1  TYPE TABLE OF ekpo,
        wa_ekpo1  TYPE ekpo.

  REFRESH:    lt_ekpo,lt_ekko,lt_eket,lt_lfa1,it_purchase.
  CLEAR: ls_ekpo,ls_ekko,ls_eket,ls_lfa1.
  IF bsart IS NOT INITIAL.
    ls_bsart-sign = 'I'.
    ls_bsart-option = 'EQ'.
    ls_bsart-low = bsart.
    APPEND ls_bsart TO lr_bsart.
  ENDIF.
  ls_bstyp-sign = 'I'.
  ls_bstyp-option = 'EQ'.
  IF bstyp IS NOT INITIAL.
    ls_bstyp-low = bstyp.
  ELSE.
    ls_bstyp-low = 'F'.
  ENDIF.
  APPEND ls_bstyp TO lr_bstyp.

  IF bukrs IS NOT INITIAL AND ebeln IS INITIAL.
******Fetching Header Data From EKKO Table for each line item wise********
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
                 WHERE bukrs EQ bukrs
                 AND bstyp IN lr_bstyp[]
                 AND bsart IN lr_bsart[]
                 AND ( aedat BETWEEN from_date AND to_date ).

    IF sy-subrc EQ 0.
      SORT lt_ekko[] BY ebeln.
    ENDIF.
  ELSEIF bukrs IS NOT INITIAL AND ebeln IS NOT INITIAL.
******Fetching Header Data From EKKO Table for each line item wise********
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
           inco1
            FROM ekko
                 INTO TABLE lt_ekko
                 WHERE bukrs EQ bukrs
                 AND bstyp IN lr_bstyp[]
                 AND bsart IN lr_bsart[]
                 AND ebeln EQ ebeln
                 AND ( aedat BETWEEN from_date AND to_date ).

    IF sy-subrc EQ 0.
      SORT lt_ekko[] BY ebeln.
    ENDIF.

  ENDIF.

******Fetching Data From EKPO item Table********
  IF lt_ekko[] IS NOT INITIAL.
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
                    AND ebeln EQ lt_ekko-ebeln.
    IF sy-subrc EQ 0.
      SORT lt_ekpo[] BY ebeln.
    ENDIF.

****Fetching Supplier Name from LFA1 Table*******
    SELECT lifnr
           name1 FROM lfa1
                 INTO TABLE lt_lfa1
                 FOR ALL ENTRIES IN lt_ekko
                 WHERE lifnr EQ lt_ekko-lifnr.
    IF sy-subrc EQ 0.
      SORT lt_lfa1[] BY lifnr.
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
      SORT lt_eket[] BY ebeln.
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
      SORT lt_mseg[] BY ebeln.
    ENDIF.

*************** Fetching Material Group from MARA ***********************
    SELECT matnr,matkl FROM mara INTO TABLE @DATA(lt_mara)
                       FOR ALL ENTRIES IN @lt_ekpo
                       WHERE matnr EQ @lt_ekpo-matnr.
    IF sy-subrc EQ 0.
      SORT lt_mara[] BY matnr.
    ENDIF.

  ENDIF.

  CLEAR: ls_tvarvc.
  SELECT * FROM tvarvc INTO TABLE lt_tvarvc WHERE name EQ 'ZPARAMETER_BAPI'
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

    CLEAR: ls_purchase.
    ls_purchase-bukrs = ls_ekpo-bukrs.
    ls_purchase-recieving_plant = ls_ekpo-werks.
    ls_purchase-ebeln = ls_ekpo-ebeln.
    ls_purchase-ebelp = ls_ekpo-ebelp.
    ls_purchase-matnr = ls_ekpo-matnr.
    ls_purchase-item_text = ls_ekpo-zremarks.
    READ TABLE lt_mara INTO DATA(ls_mara) WITH KEY matnr = ls_ekpo-matnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      ls_purchase-material_grp = ls_mara-matkl.
    ENDIF.
    ls_purchase-txz01 = ls_ekpo-txz01.
    ls_purchase-mtart = ls_ekpo-mtart.
    ls_purchase-menge = ls_ekpo-menge.
    ls_purchase-netwr = ls_ekpo-netwr.
    ls_purchase-elikz = ls_ekpo-elikz.

    CLEAR ls_ekko.
    READ TABLE lt_ekko INTO ls_ekko WITH KEY bukrs = ls_ekpo-bukrs
                                             ebeln = ls_ekpo-ebeln  BINARY SEARCH..
    IF sy-subrc EQ 0.
      ls_purchase-supplier = ls_ekko-lifnr.
      ls_purchase-supplying_plant = ls_ekko-reswk.
      ls_purchase-document_type =  ls_ekko-bsart.
      ls_purchase-created_by = ls_ekko-ernam.
      ls_purchase-creation_date = ls_ekko-aedat.
    ENDIF.

    CLEAR ls_lfa1.
    READ TABLE lt_lfa1 INTO ls_lfa1 WITH KEY lifnr = ls_purchase-supplier BINARY SEARCH..
    IF sy-subrc EQ 0.
      ls_purchase-supplier_nme = ls_lfa1-name1.
    ENDIF.

    IF ls_purchase-elikz EQ 'X'.
      ls_purchase-status = lc_stat2.
    ELSE.
      ls_purchase-status = lc_stat1.
    ENDIF.

    READ TABLE lt_eket INTO ls_eket WITH KEY ebeln = ls_ekpo-ebeln
                                             ebelp = ls_ekpo-ebelp  BINARY SEARCH..
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
                                             ebelp = ls_ekpo-ebelp  BINARY SEARCH..
    IF sy-subrc EQ 0.
      ls_purchase-gr_quantity = ls_mseg-erfmg. " Converted GR Quantity
      ls_purchase-gr_quan_unit = ls_mseg-erfme. "unit of Measure
      ls_purchase-gr_value = ls_mseg-dmbtr.
    ENDIF.


    ls_purchase-po_category = lc_normal.
*************************************Tax Code implimentaion by Puratchi******************************************************************
*    wa_taxcom-bukrs = ls_ekpo-bukrs.
*    wa_taxcom-budat = ls_ekko-aedat.
*    wa_taxcom-bldat = ls_ekko-bedat.
*    wa_taxcom-waers = ls_ekko-waers.
*    wa_taxcom-hwaer = ls_ekko-waers.
*    wa_taxcom-kposn = ls_ekpo-ebelp.
*    wa_taxcom-mwskz = ls_ekpo-mwskz.
*    wa_taxcom-wrbtr = CONV wrbtr( ( ls_ekpo-netpr / ls_ekpo-peinh ) * ls_ekpo-menge ).
*    wa_taxcom-xmwst = 'X'.
*    wa_taxcom-txjcd = ls_ekpo-txjcd.
*    wa_taxcom-lifnr = ls_ekko-lifnr.
*    wa_taxcom-ekorg = ls_ekko-ekorg.
*    wa_taxcom-matnr = ls_ekpo-matnr.
*    wa_taxcom-werks = ls_ekpo-werks.
*    wa_taxcom-matkl = ls_ekpo-matkl.
*    wa_taxcom-meins = ls_ekpo-meins.
*    wa_taxcom-mglme = ls_ekpo-menge.
*    wa_taxcom-mtart = ls_ekpo-mtart.
*    wa_taxcom-land1 = ls_ekko-lands.
*
*    CALL FUNCTION 'CALCULATE_TAX_ITEM'
*      EXPORTING
*        i_taxcom            = wa_taxcom
*      TABLES
*        t_xkomv             = lt_komv
*      EXCEPTIONS
*        mwskz_not_defined   = 1
*        mwskz_not_found     = 2
*        mwskz_not_valid     = 3
*        steuerbetrag_falsch = 4
*        country_not_found   = 5
*        txjcd_not_valid     = 6
*        OTHERS              = 7.
*
*
*    LOOP AT lt_komv INTO wa_komv .
*      IF wa_komv-kschl = 'JICG'.
*        ls_purchase-cgst = wa_komv-kwert.
*      ELSEIF wa_komv-kschl = 'JISG'.
*        ls_purchase-sgst = wa_komv-kwert.
*      ELSEIF wa_komv-kschl = 'JIIG'.
*        ls_purchase-igst = wa_komv-kwert.
*      ENDIF.
*    ENDLOOP.
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

    ls_purchase-zterm = ls_ekko-zterm.
    ls_purchase-zbd1t = ls_ekko-zbd1t.
    ls_purchase-inco1 = ls_ekko-inco1.
    APPEND ls_purchase TO lt_purchase.
  ENDLOOP.

  LOOP AT lt_tvarvc INTO ls_tvarvc.
    LOOP AT lt_purchase INTO ls_purchase WHERE created_by EQ ls_tvarvc-low..
      IF ls_purchase-created_by EQ ls_tvarvc-low.
        ls_purchase-po_category = lc_auto.
      ENDIF.
      MODIFY lt_purchase FROM ls_purchase.
    ENDLOOP.
  ENDLOOP.

  APPEND LINES OF lt_purchase TO it_purchase.

  IF it_purchase[] IS NOT INITIAL.
    SORT it_purchase[] BY  bukrs recieving_plant ebeln ebelp.
  ENDIF.

ENDFUNCTION.
