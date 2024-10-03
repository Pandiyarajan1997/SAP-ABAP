FUNCTION zbapi_safety_stock_mis.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(PLANT) TYPE  WERKS_D OPTIONAL
*"  TABLES
*"      IT_STOCK STRUCTURE  ZSTR_SAFETY_STOCK
*"      IT_MATNR STRUCTURE  MAT_RANGE OPTIONAL
*"  EXCEPTIONS
*"      INCORRECT_PLANT
*"----------------------------------------------------------------------
**************************************************************************
*Developed By : KPABAP ( Samsudeen ).
*Reference By : Praveen
*Requirement By: Monisha (Purchase Team)
*Description : Sending Safety Stock,Unrestricted Stock, Transit Stock
*              For Material based on plant to MIS
*---------------------------------------------------------------------------
** changed_on : 07.07.2022
**  Reference by: suresh B.V
*    Adding two fields in output
  " TR NO: DEVK931870
**************************************************************************
*********changed by Pandiarajan
*        11.04.2024
*        sending only plant wise stock for 1780 1790
*********************
  TYPES: BEGIN OF ty_marc,   "structure For MARC Table
           matnr TYPE matnr,
           werks TYPE werks_d,
           eisbe TYPE eisbe,
           trame TYPE trame,
         END OF ty_marc,
         BEGIN OF ty_mard,  "Structure For MARD Table
           matnr TYPE matnr,
           werks TYPE werks_d,
           lgort TYPE lgort_d,
           labst TYPE labst,
           lfgja TYPE lfgja,
           lfmon TYPE lfmon,
         END OF ty_mard,
         BEGIN OF ty_mbew,    "structure for MBEW table
           matnr TYPE matnr,
           bwkey TYPE bwkey,
           bwtar TYPE bwtar_d,
           vprsv TYPE vprsv,
           verpr TYPE verpr,
           stprs TYPE stprs,
           salk3 TYPE salk3,
         END OF ty_mbew,
         BEGIN OF ty_eord, "structure for Eord Table
           matnr TYPE matnr,
           werks TYPE werks_d,
           vdatu TYPE ordab,
           bdatu TYPE ordbi,
           lifnr TYPE elifn,
         END OF ty_eord,
         BEGIN OF ty_mara,  "structure for mara table
           matnr TYPE matnr,
           mtart TYPE mtart,
         END OF ty_mara,

         BEGIN OF ty_makt,  "structure For MAKT table
           matnr TYPE matnr,
           maktx TYPE maktx,
         END OF ty_makt,

         BEGIN OF ty_lfa1, "Structure For LFA1 Table
           lifnr TYPE lifnr,
           name1 TYPE name1_gp,
         END OF ty_lfa1,
*** Changes on 26.08.2022  Added by Samsudeen  ***
         BEGIN OF ty_mseg,
           bwart TYPE bwart,
           matnr TYPE matnr,
           werks TYPE werks_d,
           charg TYPE charg_d,
           lifnr TYPE elifn,
           ebeln TYPE ebeln,
         END OF ty_mseg.

  TYPES: tr_matnr TYPE RANGE OF mara-matnr,
         ty_matnr TYPE LINE OF tr_matnr.


  DATA: lr_matnr TYPE tr_matnr,
        ls_matnr TYPE ty_matnr.

  DATA: ls_mat_range TYPE mat_range.

  DATA: lt_stock  TYPE STANDARD TABLE OF zstr_safety_stock,
        lt_stock1 TYPE STANDARD TABLE OF zstr_safety_stock,
        lt_stock2 TYPE STANDARD TABLE OF zstr_safety_stock,
        ls_stock1 TYPE zstr_safety_stock.

  DATA: lv_unrestrict TYPE labst.

*  DATA: lt_marc TYPE TABLE OF ty_marc,
*        ls_marc TYPE ty_marc.

********** Internal table Declaration ***************
  DATA: lt_mard TYPE TABLE OF ty_mard,
        ls_mard TYPE ty_mard.

  DATA: lt_eord  TYPE TABLE OF ty_eord,
        lt_eord1 TYPE TABLE OF ty_eord,
        ls_eord  TYPE ty_eord.

  DATA: lt_mbew TYPE TABLE OF ty_mbew,
        ls_mbew TYPE ty_mbew.

  DATA: lt_mara TYPE TABLE OF ty_mara,
        ls_mara TYPE ty_mara.

  DATA: lt_makt TYPE TABLE OF ty_makt,
        ls_makt TYPE ty_makt.

  DATA: lt_lfa1 TYPE TABLE OF ty_lfa1,
        ls_lfa1 TYPE ty_lfa1.

*********Added on 26.08.2022 by Samsudeen*******
  DATA: lt_mseg  TYPE TABLE OF ty_mseg,
        lt_mseg1 TYPE TABLE OF ty_mseg,
        ls_mseg  TYPE ty_mseg.

  DATA: lt_batch  TYPE STANDARD TABLE OF zstr_batch_stock,
        lt_batch1 TYPE STANDARD TABLE OF zstr_batch_stock,
        ls_batch  TYPE zstr_batch_stock.
**************************************************

  DATA: lr_matnr1 TYPE RANGE OF mara-matnr.
  DATA: lt_stk   TYPE STANDARD TABLE OF zstr_safety_stock,
        lt_stk1  TYPE STANDARD TABLE OF zstr_safety_stock,
        ls_stock TYPE zstr_safety_stock.
  DATA: lv_value TYPE salk3.

  REFRESH: lt_mara,lt_makt,lt_lfa1,lt_mard,lt_mbew.

  IF it_matnr[] IS NOT INITIAL.
    LOOP AT it_matnr INTO ls_mat_range.
      CLEAR ls_matnr.
      ls_matnr-sign   = ls_mat_range-sign.
      ls_matnr-option = ls_mat_range-option.
      ls_matnr-low    = ls_mat_range-matnr_low.
      ls_matnr-high   = ls_mat_range-matnr_high.
      APPEND ls_matnr TO lr_matnr.
    ENDLOOP.
  ENDIF.

  DATA: gs_marc TYPE marc.
****** Plant checks based on input **************************
  CLEAR gs_marc.
  SELECT SINGLE * FROM marc INTO gs_marc WHERE werks = plant.
  IF sy-subrc NE 0.
    RAISE incorrect_plant.
  ENDIF.


  IF plant IS NOT INITIAL.
***Fetching Unrestricted stock from mard based on plant ************
    SELECT a~matnr
           a~werks
           a~lgort
           a~labst
           a~lfgja
           a~lfmon FROM mard AS a INNER JOIN mara AS b ON a~matnr = b~matnr
                   INTO TABLE lt_mard
                   WHERE a~matnr IN lr_matnr
                   AND a~werks EQ plant
                   AND b~lvorm NE 'X'
                   AND b~status NE 'X'.
  ELSEIF plant IS INITIAL.
***Fetching Unrestricted stock from mard based on plant ************
    SELECT a~matnr
           a~werks
           a~lgort
           a~labst
           a~lfgja
           a~lfmon FROM mard AS a INNER JOIN mara AS b ON a~matnr = b~matnr
                   INTO TABLE lt_mard
                   WHERE a~matnr IN lr_matnr
*                   AND a~werks EQ plant
                   AND b~lvorm NE 'X'
                   AND b~status NE 'X'.
  ENDIF.


  IF sy-subrc EQ 0.
    SORT lt_mard BY matnr werks lgort.
  ENDIF.
**************fetch from only mard table for farm plant stock************
  IF plant NE '1780' AND plant NE '1790'.
    IF lt_mard[] IS NOT INITIAL.
      DELETE ADJACENT DUPLICATES FROM lt_mard[]  COMPARING matnr.
      LOOP AT lt_mard INTO DATA(lw_mard).
        APPEND VALUE #( sign = 'I'
                        option = 'EQ'
                        low = lw_mard-matnr ) TO lr_matnr1.
      ENDLOOP.
*changes on 26.08.2022 by Samsudeen
      CLEAR lt_batch.
      CALL FUNCTION 'ZBAPI_BATCHWISE_STOCK_MIS'
        EXPORTING
          plant        = plant
          matnr        = lr_matnr1
        TABLES
          lt_batchwise = lt_batch.
    ENDIF.

    SORT lt_batch[] BY material plant batch.
    DELETE lt_batch[] WHERE unrestricted_stk EQ 0 AND safety_stock EQ 0.

  ELSE.
*****************safety stock fetch from marc*******************
    SELECT matnr,werks,eisbe FROM marc INTO TABLE @DATA(lt_marc) WHERE werks = @plant.
    IF sy-subrc = 0.
      SORT : lt_marc BY matnr werks.
    ENDIF.
**************Fetch the batch details from mchb**************
    SELECT matnr,werks,lgort,charg,clabs FROM mchb INTO TABLE @DATA(lt_mchb) WHERE werks = @plant
                                                                             AND   clabs NE 0.
    IF sy-subrc = 0.
      SORT : lt_mchb BY matnr werks lgort.
    ENDIF.
****************delete zero stock********
*    SORT lt_mard[] BY matnr werks.
    DELETE lt_mard[] WHERE labst EQ 0.

    LOOP AT lt_mard INTO ls_mard.
***************read marc*************
      READ TABLE lt_marc INTO DATA(ls_marc) WITH KEY matnr = ls_mard-matnr
                                                     werks = ls_mard-werks BINARY SEARCH.
**************read mchb***************
      READ TABLE lt_mchb TRANSPORTING NO FIELDS WITH KEY matnr = ls_mard-matnr
                                                         werks = ls_mard-werks
                                                         lgort = ls_mard-lgort BINARY SEARCH.
      IF sy-subrc = 0.
        LOOP AT lt_mchb INTO DATA(ls_mchb) WHERE matnr = ls_mard-matnr
                                           AND   werks = ls_mard-werks
                                           AND   lgort = ls_mard-lgort.
          APPEND VALUE #( material         = ls_mard-matnr
                          plant            = ls_mard-werks
                          st_loc           = ls_mard-lgort
                          unrestricted_stk = ls_mchb-clabs
                          safety_stock     = ls_marc-eisbe
                          batch            = ls_mchb-charg ) TO lt_batch.
          CLEAR : ls_mchb.
        ENDLOOP.
      ELSE.
        APPEND VALUE #( material         = ls_mard-matnr
                        plant            = ls_mard-werks
                        st_loc           = ls_mard-lgort
                        unrestricted_stk = ls_mard-labst
                        safety_stock     = ls_marc-eisbe ) TO lt_batch.
      ENDIF.

      CLEAR : ls_mard,ls_marc.
    ENDLOOP.
  ENDIF.

  REFRESH: lt_mseg,lt_eord.
  IF lt_batch[] IS NOT INITIAL.
******* Fetching Moving Average price from MBEW for Plant material ***************
    SELECT matnr
           bwkey
           bwtar
           vprsv
           verpr
           stprs
           salk3 FROM mbew
                 INTO TABLE lt_mbew
                 FOR ALL ENTRIES IN lt_batch
                 WHERE matnr = lt_batch-material
                 AND bwkey = lt_batch-plant
                 AND bwtar = ' '.

    IF sy-subrc EQ 0.
      SORT lt_mbew[] BY matnr bwkey.
    ENDIF.

**** Fetching supplier Details based on batch from Mseg*****
    SELECT bwart
           matnr
           werks
           charg
           lifnr
           ebeln FROM mseg
                 INTO TABLE lt_mseg
                 FOR ALL ENTRIES IN lt_batch
                 WHERE bwart EQ '101'
                 AND matnr = lt_batch-material
                 AND werks = lt_batch-plant
                 AND charg = lt_batch-batch.
    IF sy-subrc EQ 0.
      SORT lt_mseg[] BY matnr werks charg.
** Fetching Po details Based on GRN **
      SELECT ebeln,lifnr,reswk FROM ekko INTO TABLE @DATA(lt_ekko)
                               FOR ALL ENTRIES IN @lt_mseg
                               WHERE ebeln = @lt_mseg-ebeln.
      IF sy-subrc = 0.
        SORT lt_ekko[] BY ebeln.
      ENDIF.
****Fetching source list valid from and valid to **************
      SELECT matnr
             werks
             vdatu
             bdatu
             lifnr FROM eord
                   INTO TABLE lt_eord
                   FOR ALL ENTRIES IN lt_mseg
                   WHERE matnr = lt_mseg-matnr
                   AND werks = lt_mseg-werks
                   AND vdatu LE sy-datum
                   AND bdatu GE sy-datum.
      IF sy-subrc EQ 0.
        SORT lt_eord BY matnr werks lifnr.
      ENDIF.
*******Feching supplier Name from LFA1 *****************************
      SELECT lifnr
             name1 FROM lfa1
                   INTO TABLE lt_lfa1.
      IF sy-subrc EQ 0.
        SORT lt_lfa1[] BY lifnr.
      ENDIF.
    ENDIF.

****** Fetching Material type from MARA ******************
    SELECT matnr
           mtart FROM mara
                 INTO TABLE lt_mara
                 FOR ALL ENTRIES IN lt_batch
                 WHERE matnr = lt_batch-material.
    IF sy-subrc EQ 0.
      SORT lt_mara[] BY matnr.
    ENDIF.
****** Fetching Material Description from MAKT ********************
    SELECT matnr
           maktx FROM makt
                 INTO TABLE lt_makt
                 FOR ALL ENTRIES IN lt_batch
                 WHERE matnr = lt_batch-material
                 AND spras EQ sy-langu .
    SORT lt_makt[] BY matnr.


** selecting plant Desc **
    SELECT werks, name1 FROM t001w INTO TABLE @DATA(lt_plant).
    IF sy-subrc EQ 0.
      SORT lt_plant[] BY werks.
    ENDIF.
    SELECT matnr
           werks
           vdatu
           bdatu
           lifnr FROM eord
                 INTO TABLE lt_eord1
                 FOR ALL ENTRIES IN lt_batch
                 WHERE matnr = lt_batch-material
                 AND werks = plant
                 AND vdatu LE sy-datum
                 AND bdatu GE sy-datum.
    IF sy-subrc = 0.
      SORT lt_eord1[] BY matnr.
    ENDIF.
  ENDIF.

  LOOP AT lt_batch INTO ls_batch.
    CLEAR ls_stock.

    ls_stock-material = ls_batch-material.  "Material
    ls_stock-plant = ls_batch-plant.        "Plant
    ls_stock-unrestricted_stk = ls_batch-unrestricted_stk.  "Unrestricted Stock
    ls_stock-safety_stock = ls_batch-safety_stock. "Safety Stock
    ls_stock-batch = ls_batch-batch.   "Batch Number

    CLEAR ls_makt.
    READ TABLE lt_makt INTO ls_makt WITH KEY matnr = ls_batch-material BINARY SEARCH.
    IF sy-subrc EQ 0.
      ls_stock-material_desc = ls_makt-maktx.   "Material Description
    ENDIF.

    CLEAR ls_mara.
    READ TABLE lt_mara INTO ls_mara WITH KEY matnr = ls_batch-material BINARY SEARCH.
    IF sy-subrc EQ 0.
      ls_stock-material_type = ls_mara-mtart.  "Material Type
    ENDIF.

    CLEAR ls_mseg.
    READ TABLE lt_mseg INTO ls_mseg WITH KEY matnr = ls_batch-material
                                             werks = ls_batch-plant
                                             charg = ls_batch-batch BINARY SEARCH.
    IF sy-subrc EQ 0.
      ls_stock-supplier = ls_mseg-lifnr.  "Supplier
      CLEAR ls_lfa1.
      READ TABLE lt_lfa1 INTO ls_lfa1 WITH KEY lifnr = ls_mseg-lifnr BINARY SEARCH.
      IF sy-subrc EQ 0.
        ls_stock-supplier_nme = ls_lfa1-name1. "Supplier name
      ENDIF.
    ENDIF.
    IF ls_stock-supplier IS INITIAL.
      READ TABLE lt_ekko INTO DATA(ls_ekko) WITH KEY ebeln = ls_mseg-ebeln BINARY SEARCH.
      IF sy-subrc EQ 0.
        ls_stock-supplier = ls_ekko-lifnr.  "Supplier
        CLEAR ls_lfa1.
        READ TABLE lt_lfa1 INTO ls_lfa1 WITH KEY lifnr = ls_ekko-lifnr BINARY SEARCH.
        IF sy-subrc EQ 0.
          ls_stock-supplier_nme = ls_lfa1-name1. "Supplier name
        ENDIF.
      ENDIF.
    ENDIF.

    IF ls_stock-supplier IS INITIAL.
      READ TABLE lt_ekko INTO DATA(ls_ekko1) WITH KEY ebeln = ls_mseg-ebeln BINARY SEARCH.
      IF sy-subrc EQ 0.
        ls_stock-supplying_plant = ls_ekko1-reswk.  "Supplier
        CLEAR ls_lfa1.
        READ TABLE lt_plant INTO DATA(ls_plant) WITH KEY werks = ls_ekko1-reswk BINARY SEARCH.
        IF sy-subrc EQ 0.
          ls_stock-plant_name = ls_plant-name1. "Supplier name
        ENDIF.
      ENDIF.
    ENDIF.
    CLEAR ls_eord.
    READ TABLE lt_eord INTO ls_eord WITH KEY matnr = ls_batch-material
                                             werks = ls_batch-plant
                                             lifnr = ls_mseg-lifnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      ls_stock-valid_from = ls_eord-vdatu. "Valid From
      ls_stock-valid_to = ls_eord-bdatu.  "Valid to
    ENDIF.

    ls_stock-unrestricted_stk = ls_batch-unrestricted_stk.

    CLEAR: lv_value,ls_mbew. " ( Calculating unrestricted stock value using Moving Average price from MBEW )
    READ TABLE lt_mbew INTO ls_mbew WITH KEY matnr = ls_stock-material
                                             bwkey = ls_stock-plant BINARY SEARCH.
    IF sy-subrc EQ 0.

      IF ls_mbew-vprsv = 'V'.  " Moving Average Price

        CLEAR lv_value.
        lv_value = ( ls_stock-unrestricted_stk * ls_mbew-verpr ).
        ls_stock-unrestrict_value = lv_value. "Unrestricted Stock Value

      ELSEIF ls_mbew-vprsv = 'S'.  " Standard Price

        CLEAR lv_value.
        lv_value = ( ls_stock-unrestricted_stk * ls_mbew-verpr ).
        ls_stock-unrestrict_value = lv_value. "Unrestricted Stock Value

      ENDIF.
    ENDIF.
    IF ls_stock-supplier IS INITIAL.
      ls_stock-supplier = VALUE #( lt_eord1[ matnr = ls_stock-material ]-lifnr OPTIONAL ).
      ls_stock-supplier_nme = VALUE #( lt_lfa1[ lifnr = ls_stock-supplier ]-name1 OPTIONAL ).
    ENDIF.
    APPEND ls_stock TO it_stock.

    CLEAR ls_batch.

  ENDLOOP.
ENDFUNCTION.
