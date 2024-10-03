FUNCTION zbapi_openstock_price.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(PLANT) TYPE  WERKS_D
*"     VALUE(MATERIAL) TYPE  MATNR OPTIONAL
*"     VALUE(FROM_DATE) TYPE  BUDAT
*"     VALUE(TO_DATE) TYPE  BUDAT
*"  TABLES
*"      IT_OPEN_PRICE STRUCTURE  ZSTR_OPEN_PRICE
*"  EXCEPTIONS
*"      INCORRECT_PLANT
*"----------------------------------------------------------------------
  TYPES: BEGIN OF slis_specialcol_alv,
           fieldname   TYPE slis_fieldname,
           color       TYPE slis_color,
           nokeycol(1) TYPE c,
         END OF slis_specialcol_alv.
  TYPES: slis_t_specialcol_alv TYPE slis_specialcol_alv OCCURS 1.

* new output table for flat list in total mode
  TYPES : BEGIN OF stype_totals_flat,
            matnr        LIKE      mbew-matnr,
            maktx        LIKE      makt-maktx,
            bwkey        LIKE      mbew-bwkey,
            werks        LIKE      mseg-werks,
            charg        LIKE      mseg-charg,
            sobkz        LIKE      mslb-sobkz,
            name1        LIKE      t001w-name1,             "n999530

            start_date   LIKE      sy-datlo,
            end_date     LIKE      sy-datlo,

            anfmenge(09) TYPE p    DECIMALS 3,
            meins        LIKE      mara-meins,
            soll(09)     TYPE p DECIMALS 3,
            haben(09)    TYPE p DECIMALS 3,
            endmenge(09) TYPE p DECIMALS 3.
*
  TYPES: /cwm/anfmenge LIKE mseg-/cwm/menge,
         /cwm/meins    LIKE      mseg-/cwm/meins,
         /cwm/soll     LIKE      mseg-/cwm/menge,
         /cwm/haben    LIKE      mseg-/cwm/menge,
         /cwm/endmenge LIKE      mseg-/cwm/menge.
  TYPES: anfwert(09)   TYPE p DECIMALS 2,
         waers         LIKE t001-waers,             "Währungsschlüssel
         sollwert(09)  TYPE p    DECIMALS 2,
         habenwert(09) TYPE p    DECIMALS 2,
         endwert(09)   TYPE p    DECIMALS 2,
         color         TYPE      slis_t_specialcol_alv,
         END OF stype_totals_flat,

         stab_totals_flat TYPE STANDARD TABLE OF stype_totals_flat
                WITH DEFAULT KEY.

  TYPES: BEGIN OF ty_mbew,
           matnr TYPE matnr,
           bwkey TYPE bwkey,
           bwtar TYPE bwtar_d,
           lfgja TYPE gjahr,
           lfmon TYPE monat,
           lbkum TYPE lbkum,
           salk3 TYPE salk3,
           vprsv TYPE vprsv,
           verpr TYPE verpr,
         END OF ty_mbew.
  DATA: gt_mbew TYPE TABLE OF ty_mbew,
        gs_mbew TYPE ty_mbew.

  DATA : g_s_totals_flat TYPE  stype_totals_flat,
         g_t_totals_flat TYPE  stab_totals_flat.

  DATA: gs_marc TYPE marc.

*DATA: lt_outtab TYPE STANDARD TABLE OF alv_t_t2.
  FIELD-SYMBOLS: <lt_outtab> LIKE  g_t_totals_flat.
  FIELD-SYMBOLS: <lt_outtab1> LIKE  g_t_totals_flat.

  DATA: lt_final  TYPE TABLE OF stype_totals_flat,
        ls_final  LIKE LINE OF lt_final,
        lt_final1 TYPE TABLE OF stype_totals_flat,
        lt_final2 TYPE TABLE OF stype_totals_flat,
        ls_final1 LIKE LINE OF lt_final1.

  DATA lo_data TYPE REF TO data.
  DATA: lo_data1 TYPE REF TO data.

  DATA: lt_seltab TYPE TABLE OF rsparams,
        ls_seltab LIKE LINE OF lt_seltab.

  DATA: ls_open_price TYPE zstr_open_price.
  DATA: lv_lbkum TYPE lbkum.
  DATA: lv_lbkum1 TYPE lbkum.
  DATA: lv_month TYPE lfmon.
  DATA: lv_year TYPE lfgja.
  DATA: lv_date TYPE budat.
  DATA: lv_date1 TYPE budat.
  DATA: lv_verpr TYPE verpr.
  DATA: lt_grn TYPE TABLE OF zstr_grn_quan_price,
        ls_grn TYPE zstr_grn_quan_price.

  DATA: lt_ckm3 TYPE TABLE OF zstr_map_price,
        ls_ckm3 TYPE zstr_map_price.

  TYPES: tr_matnr TYPE RANGE OF mara-matnr,
         ty_matnr TYPE LINE OF tr_matnr.


  DATA: lr_matnr TYPE tr_matnr,
        ls_matnr TYPE ty_matnr.

  DATA: ls_mat_range TYPE mat_range.

  DATA: str3 TYPE lfgja, "Year
        str2 TYPE lfmon, "Month
        str1 TYPE lfmon. "Day

  DATA: lv_poper TYPE poper. "Period (OR) Month

**********checks for plant and material ************************
  CLEAR gs_marc.
  SELECT SINGLE * FROM marc INTO gs_marc WHERE werks = plant.
  IF sy-subrc NE 0.
    RAISE incorrect_plant.
  ENDIF.

  REFRESH: lt_seltab.
  CLEAR ls_seltab.
  ls_seltab-selname = 'WERKS'.          " Name of parameter on submitted program
  ls_seltab-kind    = 'S'.
  ls_seltab-sign    = 'I'.
  ls_seltab-option  = 'EQ'.
  ls_seltab-low     = plant.
  APPEND ls_seltab TO lt_seltab.

  IF material IS NOT INITIAL.
    CLEAR ls_seltab.
    ls_seltab-selname = 'MATNR'.          " Name of parameter on submitted program
    ls_seltab-kind    = 'S'.
    ls_seltab-sign    = 'I'.
    ls_seltab-option  = 'EQ'.
    ls_seltab-low     = material.
    APPEND ls_seltab TO lt_seltab.
  ENDIF.


  CLEAR ls_seltab.
  ls_seltab-selname = 'LGBST'.          " Name of parameter on submitted program
  ls_seltab-kind    = 'P'.
  ls_seltab-sign    = 'I'.
  ls_seltab-option  = 'EQ'.
  ls_seltab-low     = 'X'.
  APPEND ls_seltab TO lt_seltab.

  CLEAR ls_seltab.
  ls_seltab-selname = 'BWBST'.          " Radio button to deselect as it is giving dump
  ls_seltab-kind    = 'P'.
  ls_seltab-sign    = 'I'.
  ls_seltab-option  = 'EQ'.
  ls_seltab-low     = space.
  APPEND ls_seltab TO lt_seltab.

  CLEAR ls_seltab.
  ls_seltab-selname = 'SBBST'.          " Radio button to deselect as it is giving dump
  ls_seltab-kind    = 'P'.
  ls_seltab-sign    = 'I'.
  ls_seltab-option  = 'EQ'.
  ls_seltab-low     = space.
  APPEND ls_seltab TO lt_seltab.

  CLEAR ls_seltab.
  ls_seltab-selname = 'PA_SUMFL'.          " Name of parameter on submitted program
  ls_seltab-kind    = 'P'.
  ls_seltab-sign    = 'I'.
  ls_seltab-option  = 'EQ'.
  ls_seltab-low     = 'X'.
  APPEND ls_seltab TO lt_seltab.

  CLEAR ls_seltab.
  ls_seltab-selname = 'XCHAR'.          " Name of parameter on submitted program
  ls_seltab-kind    = 'P'.
  ls_seltab-sign    = 'I'.
  ls_seltab-option  = 'EQ'.
  ls_seltab-low     = 'X'.
  APPEND ls_seltab TO lt_seltab.

  CLEAR ls_seltab.
  ls_seltab-selname = 'DATUM'.          " Name of parameter on submitted program
  ls_seltab-kind    = 'S'.
  ls_seltab-sign    = 'I'.
  ls_seltab-option  = 'BT'.
  ls_seltab-low     = from_date.
  ls_seltab-high    = to_date.
  APPEND ls_seltab TO lt_seltab.


  IF from_date IS NOT INITIAL AND to_date IS NOT INITIAL.
    REFRESH lt_final.
    "let know the model
    cl_salv_bs_runtime_info=>set(
     EXPORTING
       display  = abap_false
       metadata = abap_false
       data     = abap_true
    ).

    SUBMIT rm07mlbd WITH SELECTION-TABLE lt_seltab
                 AND RETURN.

    TRY.
        " get data from SALV model
        cl_salv_bs_runtime_info=>get_data_ref(
              IMPORTING
                r_data = lo_data
        ).

        ASSIGN lo_data->* TO <lt_outtab>.


        lt_final[] = <lt_outtab>[].
        IF  sy-subrc EQ 0.
          SORT lt_final[] BY matnr.
        ENDIF.
      CATCH cx_salv_bs_sc_runtime_info.

    ENDTRY.
  ENDIF.

  IF lt_final[] IS NOT INITIAL.
    SORT lt_final[] BY matnr werks.
    REFRESH: lt_final1[],lt_final2[].
    lt_final1[] = lt_final[].
    lt_final2[] = lt_final[].
    SORT lt_final1[] BY matnr.
    DELETE lt_final WHERE anfmenge EQ 0 AND endmenge EQ 0.
    DELETE lt_final1 WHERE anfmenge EQ 0 AND endmenge EQ 0.
    DELETE lt_final2 WHERE anfmenge EQ 0 AND endmenge EQ 0.
    DELETE ADJACENT DUPLICATES FROM lt_final2 COMPARING matnr.
****Select Moving Average price from from MBEWH **
    SELECT matnr
           bwkey
           bwtar
           lfgja
           lfmon
           lbkum
           salk3
           vprsv
           verpr FROM mbewh
                 INTO TABLE gt_mbew
                 FOR ALL ENTRIES IN lt_final
                 WHERE matnr = lt_final-matnr
                 AND bwkey = lt_final-werks
                 AND verpr NE '0'.
    IF sy-subrc EQ 0.
      SORT gt_mbew[] BY matnr bwkey lfgja DESCENDING lfmon DESCENDING.
    ENDIF.
***Material Desc*
    SELECT * FROM makt INTO TABLE @DATA(gt_makt) WHERE spras EQ @sy-langu.
    IF sy-subrc EQ 0.
      SORT gt_makt[] BY matnr.
    ENDIF.
  ENDIF.

  LOOP AT lt_final2 INTO ls_final1.
    CLEAR ls_matnr.
    ls_matnr-sign   = 'I'.
    ls_matnr-option = 'EQ'.
    ls_matnr-low    = ls_final1-matnr.
    APPEND ls_matnr TO lr_matnr.
  ENDLOOP.

  CLEAR lv_date.
  CALL FUNCTION 'GET_CURRENT_YEAR'
    EXPORTING
      bukrs = '1000'
      date  = from_date
    IMPORTING
      currm = lv_month
      curry = lv_year.

  DATA: lv_fisyear TYPE ckmlpp-bdatj.
  CLEAR: lv_poper,lv_fisyear.
  IF lv_month EQ '01'.
    lv_year = lv_year - 1.
    lv_month = '12'.
  ELSE.
    lv_month = lv_month - 1.
  ENDIF.
  lv_fisyear = lv_year.
  lv_poper = lv_month.

*** Function Module For Getting Price from CKM3 ***
  CALL FUNCTION 'ZGET_MATERIAL_MOV_AVG_PRICE'
    EXPORTING
      plant        = plant
      period       = lv_poper
      posting_year = lv_fisyear
    TABLES
      it_map_price = lt_ckm3
      it_matnr     = lr_matnr[].

  IF lt_ckm3[] IS NOT INITIAL.
    SORT lt_ckm3[] BY material plant.
  ENDIF.
*** GRN Details for Previous Month ***
  CLEAR lv_date.
  lv_date = from_date - 30.
  CLEAR lv_date1.
  lv_date1 = lv_date + 30.
  CALL FUNCTION 'ZBAPI_GRN_PRICE_QUANTITY'
    EXPORTING
      plant         = plant
*     MATERIAL      =
      from_date     = lv_date
      to_date       = to_date
    TABLES
      lt_quan_price = lt_grn[].
  IF sy-subrc EQ 0.
    SORT lt_grn[] BY material plant movement_type.
  ENDIF.

  LOOP AT lt_final INTO ls_final.
    CLEAR ls_open_price.
    ls_open_price-matnr = ls_final-matnr.
    READ TABLE gt_makt INTO DATA(gs_makt) WITH KEY matnr = ls_final-matnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      ls_open_price-maktx = gs_makt-maktx.
    ENDIF.
    ls_open_price-plant = ls_final-werks.
    ls_open_price-name1 = ls_final-name1.
    ls_open_price-start_date = ls_final-start_date.
    ls_open_price-to_date = ls_final-end_date.

    CLEAR: ls_final1,lv_lbkum, lv_lbkum1.
    LOOP AT lt_final1 INTO ls_final1 WHERE matnr = ls_final-matnr
                                     AND werks = ls_final-werks.
      lv_lbkum = lv_lbkum + ls_final1-anfmenge.
      lv_lbkum1 = lv_lbkum1 + ls_final1-endmenge.
    ENDLOOP.

    ls_open_price-opening_stock = lv_lbkum.
    ls_open_price-closing_stock = lv_lbkum1.

*-----Custom Function Module Checks for MAP price ******
    CLEAR : lv_verpr,ls_ckm3.
    READ TABLE lt_ckm3 INTO ls_ckm3 WITH KEY material = ls_final-matnr
                                             plant = ls_final-werks BINARY SEARCH.
    IF sy-subrc EQ 0.
      lv_verpr = ls_ckm3-map_price.
    ENDIF.

    ls_open_price-open_stock_price = ls_open_price-opening_stock * lv_verpr.
    ls_open_price-unit_price = lv_verpr.
*----------------------------------------------------------------------------------------------
*** For Moving Average Price first we are checking previous Month GRN Line items ****
    CLEAR : lv_verpr,ls_grn.
    IF  ls_open_price-open_stock_price IS INITIAL AND ls_open_price-unit_price IS INITIAL.
      READ TABLE lt_grn INTO ls_grn WITH KEY material = ls_final-matnr
                                             plant = ls_final-werks
                                             movement_type = '101'.
      IF sy-subrc EQ 0.
        lv_verpr = ls_grn-grn_price / ls_grn-grn_quan.
      ENDIF.
      CLEAR: ls_open_price-open_stock_price,ls_open_price-unit_price.
      ls_open_price-open_stock_price = ls_open_price-opening_stock * lv_verpr.
      ls_open_price-unit_price = lv_verpr.
    ENDIF.
*-----------------------------------------------------------------------------------------------*
*** Again Check for History table if unit price is present ***
    IF  ls_open_price-open_stock_price IS INITIAL AND ls_open_price-unit_price IS INITIAL.
      CLEAR gs_mbew.
      READ TABLE gt_mbew INTO gs_mbew WITH KEY matnr = ls_final-matnr
                                               bwkey = ls_final-werks
                                               lfgja = lv_year
                                               lfmon = lv_month BINARY SEARCH.
      IF sy-subrc EQ 0.
        CLEAR: ls_open_price-open_stock_price,ls_open_price-unit_price.
        ls_open_price-open_stock_price = ls_open_price-opening_stock * gs_mbew-verpr.
        ls_open_price-unit_price = gs_mbew-verpr.
      ELSE.
        CLEAR gs_mbew.
        LOOP AT gt_mbew INTO gs_mbew WHERE matnr = ls_final-matnr
                                      AND  bwkey = ls_final-werks.
          IF gs_mbew-verpr IS INITIAL.
            CONTINUE.
          ELSE.
            CLEAR: ls_open_price-open_stock_price,ls_open_price-unit_price.
            ls_open_price-open_stock_price = ls_open_price-opening_stock * gs_mbew-verpr.
            ls_open_price-unit_price = gs_mbew-verpr.
          ENDIF.
          EXIT.
        ENDLOOP.
      ENDIF.
    ENDIF.
*-------------------------------------------------------------------------------------------------------
    APPEND ls_open_price TO it_open_price.
  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM it_open_price[] COMPARING matnr plant.



ENDFUNCTION.
