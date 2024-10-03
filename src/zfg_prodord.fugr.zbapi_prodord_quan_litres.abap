FUNCTION zbapi_prodord_quan_litres.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(PRODUCTION_PLANT) TYPE  WERKS_D
*"     VALUE(FROM_DATE) TYPE  DATUM OPTIONAL
*"     VALUE(TO_DATE) TYPE  DATUM OPTIONAL
*"  TABLES
*"      LT_PRODORD STRUCTURE  ZSTR_PRODORD_QUAN
*"----------------------------------------------------------------------
*=======================================================================
  "Created_on: 29.07.2022
  "Created_by: Samsudeen M
  "Description: Giving plantwise production Order Data
  "             for delivered Quantity in litres
  "Reference: Sanjib & Praveen Kumar
*========================================================================
  TYPES: BEGIN OF ty_aufk,
           aufnr TYPE aufnr,
           werks TYPE werks_d,
           ktext TYPE auftext,
         END OF ty_aufk,
         BEGIN OF ty_afko,
           aufnr  TYPE aufnr,
           gltrp  TYPE co_gltrp,
           gamng  TYPE gamng,
           gmein  TYPE meins,
           plnbez TYPE matnr,
           igmng  TYPE co_igmng,
           iasmg  TYPE co_iasmg,
         END OF ty_afko,
         BEGIN OF ty_afpo,
           aufnr TYPE aufnr,
           wemng TYPE co_wemng,
           amein TYPE co_aufme,
           matnr TYPE co_matnr,
           pwerk TYPE co_pwerk,
           dgltp TYPE co_gltrp,
           charg TYPE charg_d,
         END OF ty_afpo.

  DATA: lt_aufk TYPE TABLE OF ty_aufk,
        ls_aufk TYPE ty_aufk.

  DATA: lt_afko TYPE TABLE OF ty_afko,
        ls_afko TYPE ty_afko.

  DATA: lt_afpo TYPE TABLE OF ty_afpo,
        ls_afpo TYPE ty_afpo.

  DATA: ls_prodord TYPE zstr_prodord_quan.

  IF production_plant IS NOT INITIAL AND from_date IS NOT INITIAL
                              AND to_date IS NOT INITIAL.

    REFRESH:lt_aufk.
***** Fetching order number from input plant *****************************
    SELECT aufnr
           werks
           ktext FROM aufk
                 INTO TABLE lt_aufk
                 WHERE werks EQ production_plant.
    IF sy-subrc EQ 0.
      SORT lt_aufk[] BY aufnr.
    ENDIF.

    REFRESH lt_afko.
******* Based on order number getting header Data ***************
    SELECT aufnr
           gltrp
           gamng
           gmein
           plnbez
           igmng
           iasmg FROM afko
                 INTO TABLE lt_afko
                 FOR ALL ENTRIES IN lt_aufk
                 WHERE aufnr EQ lt_aufk-aufnr
                 AND ( gltrp BETWEEN from_date AND to_date )
                 AND gmein EQ 'L'.
    IF sy-subrc EQ 0.
      SORT lt_afko[] BY aufnr.
    ENDIF.

  ELSEIF production_plant IS NOT INITIAL AND from_date IS INITIAL
                              AND to_date IS INITIAL..

    REFRESH:lt_aufk.
***** Fetching order number from input plant *****************************
    SELECT aufnr
           werks
           ktext FROM aufk
                 INTO TABLE lt_aufk
                  WHERE werks EQ production_plant.
    IF sy-subrc EQ 0.
      SORT lt_aufk[] BY aufnr.
    ENDIF.

    REFRESH lt_afko.
******* Based on order number getting header Data ***************
    SELECT aufnr
           gltrp
           gamng
           gmein
           plnbez
           igmng
           iasmg FROM afko
                 INTO TABLE lt_afko
                 FOR ALL ENTRIES IN lt_aufk
                 WHERE aufnr EQ lt_aufk-aufnr
                 AND gmein EQ 'L'.
    IF sy-subrc EQ 0.
      SORT lt_afko[] BY aufnr.
    ENDIF.

  ELSEIF production_plant IS NOT INITIAL AND from_date IS NOT INITIAL
                                         AND to_date IS INITIAL.

    REFRESH:lt_aufk.
***** Fetching order number from input plant *****************************
    SELECT aufnr
           werks
           ktext FROM aufk
                 INTO TABLE lt_aufk
                 WHERE werks EQ production_plant.
    IF sy-subrc EQ 0.
      SORT lt_aufk[] BY aufnr.
    ENDIF.

    REFRESH lt_afko.
******* Based on order number getting header Data ***************
    SELECT aufnr
           gltrp
           gamng
           gmein
           plnbez
           igmng
           iasmg FROM afko
                 INTO TABLE lt_afko
                 FOR ALL ENTRIES IN lt_aufk
                 WHERE aufnr EQ lt_aufk-aufnr
                 AND ( gltrp BETWEEN from_date AND sy-datum )
                 AND gmein EQ 'L'.
    IF sy-subrc EQ 0.
      SORT lt_afko[] BY aufnr.
    ENDIF.

  ENDIF.


  IF lt_afko[] IS NOT INITIAL .

    REFRESH lt_afpo.
********** Fetching Data from AFPO **********************
    SELECT aufnr
           wemng
           amein
           matnr
           pwerk
           dgltp
           charg FROM afpo
                 INTO TABLE lt_afpo
                 FOR ALL ENTRIES IN lt_afko
                 WHERE aufnr EQ lt_afko-aufnr
                 AND matnr EQ lt_afko-plnbez
                 AND amein EQ lt_afko-gmein.
    IF sy-subrc EQ 0.
      SORT lt_afpo[] BY aufnr.
    ENDIF.

  ENDIF.

  REFRESH: lt_prodord.
  CLEAR ls_afko.

  LOOP AT lt_afko INTO ls_afko.

    CLEAR ls_prodord.
    ls_prodord-aufnr = ls_afko-aufnr. "Order Number
    ls_prodord-material = ls_afko-plnbez. "Material
    ls_prodord-target_qty = ls_afko-gamng.  "Target Qty
    ls_prodord-confirm_qty = ls_afko-igmng. "Confirmation Qty
    ls_prodord-quantity_uom = ls_afko-gmein. "Unit of Measure
    CLEAR ls_aufk.
    READ TABLE lt_aufk INTO ls_aufk WITH KEY aufnr = ls_afko-aufnr.
    IF sy-subrc EQ 0.
      ls_prodord-plant = ls_aufk-werks. "Plant
      ls_prodord-material_des = ls_aufk-ktext. "Material Description
    ENDIF.

    CLEAR ls_afpo.
    READ TABLE lt_afpo INTO ls_afpo WITH KEY aufnr = ls_afko-aufnr
                                             matnr = ls_afko-plnbez.
    IF sy-subrc EQ 0.
      ls_prodord-delivered_qty = ls_afpo-wemng. " Delivered Qty
      ls_prodord-charg = ls_afpo-charg. "Batch Number
    ENDIF.

    APPEND ls_prodord TO lt_prodord.
  ENDLOOP.

  IF lt_prodord[] IS NOT INITIAL.
    SORT lt_prodord[] BY aufnr.
  ENDIF.
ENDFUNCTION.
