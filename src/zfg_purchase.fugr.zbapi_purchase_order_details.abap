FUNCTION zbapi_purchase_order_details.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(BUKRS) TYPE  BUKRS
*"     VALUE(EBELN) TYPE  EBELN OPTIONAL
*"     VALUE(FROM_DATE) TYPE  DATUM
*"     VALUE(TO_DATE) TYPE  DATUM
*"  TABLES
*"      IT_PURCHASE STRUCTURE  ZSTR_PO_DETAILS
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
  DATA: lt_purchase TYPE STANDARD TABLE OF zstr_po_details.
  DATA: ls_purchase  TYPE zstr_po_details.

  DATA: lv_balance TYPE etmen.
  DATA: lv_bal_amount TYPE bwert.
  CONSTANTS: lc_auto(20)   TYPE c VALUE 'AUTO',
             lc_normal(25) TYPE c VALUE 'MANUAL',
             lc_stat1(20)  TYPE c VALUE 'OPEN',
             lc_stat2(20)  TYPE c VALUE 'CLOSED'.

  DATA: lv_name TYPE thead-tdname.
  DATA: lt_text TYPE TABLE OF tline.
  REFRESH:    lt_ekpo,lt_ekko,lt_eket,lt_lfa1,it_purchase.
  CLEAR: ls_ekpo,ls_ekko,ls_eket,ls_lfa1.

  IF bukrs IS NOT INITIAL AND ebeln IS INITIAL.
******Fetching Header Data From EKKO Table for each line item wise********
    SELECT bukrs
           ebeln
           reswk
           bsart
           ernam
           aedat
           lifnr FROM ekko
                 INTO TABLE lt_ekko
                 WHERE bukrs EQ bukrs
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
           lifnr FROM ekko
                 INTO TABLE lt_ekko
                 WHERE bukrs EQ bukrs
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

*********Looping ekpo table for getting all Line items*************
  LOOP AT lt_ekpo INTO ls_ekpo.

    CLEAR: ls_purchase.
    ls_purchase-bukrs = ls_ekpo-bukrs.
    ls_purchase-recieving_plant = ls_ekpo-werks.
    ls_purchase-ebeln = ls_ekpo-ebeln.
    ls_purchase-ebelp = ls_ekpo-ebelp.
    ls_purchase-matnr = ls_ekpo-matnr.

*    Added by samsudeen on 25.08.2022 *
*  Item Text fields *
*    CLEAR lv_name.
*    CONCATENATE ls_ekpo-ebeln ls_ekpo-ebelp INTO lv_name.
*    CONDENSE lv_name NO-GAPS.

*    CLEAR lt_text.
*    "Function module to get PO Item Text
*    CALL FUNCTION 'READ_TEXT'
*      EXPORTING
*        client                  = sy-mandt
*        id                      = 'F01'
*        language                = sy-langu
*        name                    = lv_name
*        object                  = 'EKPO'
*      TABLES
*        lines                   = lt_text
*      EXCEPTIONS
*        id                      = 1
*        language                = 2
*        name                    = 3
*        not_found               = 4
*        object                  = 5
*        reference_check         = 6
*        wrong_access_to_archive = 7
*        OTHERS                  = 8.
*    IF sy-subrc <> 0.
** Implement suitable error handling here
*    ENDIF.

*    READ TABLE lt_text INTO DATA(ls_text) WITH KEY tdformat = '*'.
*    IF sy-subrc EQ 0.
    ls_purchase-item_text = ls_ekpo-zremarks.
*    ENDIF.
********* End of changes 0n 25.08.2022 ****
    READ TABLE lt_mara INTO DATA(ls_mara) WITH KEY matnr = ls_ekpo-matnr.
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
                                             ebeln = ls_ekpo-ebeln.
    IF sy-subrc EQ 0.
      ls_purchase-supplier = ls_ekko-lifnr.
      ls_purchase-supplying_plant = ls_ekko-reswk.
      ls_purchase-document_type =  ls_ekko-bsart.
      ls_purchase-created_by = ls_ekko-ernam.
      ls_purchase-creation_date = ls_ekko-aedat.
    ENDIF.

    CLEAR ls_lfa1.
    READ TABLE lt_lfa1 INTO ls_lfa1 WITH KEY lifnr = ls_purchase-supplier.
    IF sy-subrc EQ 0.
      ls_purchase-supplier_nme = ls_lfa1-name1.
    ENDIF.

    IF ls_purchase-elikz EQ 'X'.
      ls_purchase-status = lc_stat2.
    ELSE.
      ls_purchase-status = lc_stat1.
    ENDIF.

    READ TABLE lt_eket INTO ls_eket WITH KEY ebeln = ls_ekpo-ebeln
                                             ebelp = ls_ekpo-ebelp.
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
                                             ebelp = ls_ekpo-ebelp.
    IF sy-subrc EQ 0.
      ls_purchase-gr_quantity = ls_mseg-erfmg. " Converted GR Quantity
      ls_purchase-gr_quan_unit = ls_mseg-erfme. "unit of Measure
      ls_purchase-gr_value = ls_mseg-dmbtr.
    ENDIF.


    ls_purchase-po_category = lc_normal.

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
