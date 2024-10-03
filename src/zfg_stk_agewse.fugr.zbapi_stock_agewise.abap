FUNCTION zbapi_stock_agewise.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(COMP_CODE) TYPE  BUKRS DEFAULT 1000
*"     VALUE(PLANT) TYPE  RANGE_S_WERKS
*"     VALUE(MATERIAL) TYPE  RANGE_S_MATNR OPTIONAL
*"     VALUE(DOCUMENT_DATE) TYPE  DATUM DEFAULT SY-DATUM
*"  TABLES
*"      LT_STOCK_AGEWISE STRUCTURE  ZSTR_STOCK_AGEWISE
*"----------------------------------------------------------------------
*" Created_by: Samsudeen M
  " Created_on: 15.07.2022
  " Reference_by: Suresh B V
  " Description: Bapi for Stock based on plant agewise from
  "              t-code: ZSTOCK_AGEWISE.
*--------------------------------------------------------------------------

  TYPES: BEGIN OF gs_final,
           matnr         TYPE mchb-matnr,              " Material Number
           werks         TYPE mchb-werks,              " Plant
           lgort         TYPE mchb-lgort,              "Storage Location
           spart         TYPE mara-spart,              " Division
           meins         TYPE mara-meins,
           mtart         TYPE mara-mtart,              " Material Type                                  "Added by Savariar S
           name1         TYPE t001w-name1,             " Plant Descriprtion
           bukrs         TYPE t001k-bukrs,
           maktx         TYPE makt-maktx,              " Material Description
           charg         TYPE mchb-charg,              " Batch No
           ersda         TYPE mchb-ersda,              " Posting Date / Crated On
           ersda7        TYPE mchb-ersda,              " Posting Date / Crated On
           clabs         TYPE p DECIMALS 3,            " Stock Qty  added by ram on 1/10/2015
           trame         TYPE p DECIMALS 2,
           cinsm         TYPE p DECIMALS 2,            "Quality Insp
           cspem         TYPE p DECIMALS 2,            " Blocked
           labst         TYPE p DECIMALS 2,            " Stock Qty    "CHANGES ON 19/09/2014  BY SAVARIAR
           volum         TYPE mara-volum,              " Volume
           vclabs        TYPE mchb-clabs,              " Stock In Ltrs
           mmatnr        TYPE mard-matnr,              "CHANGES ON 20/09/2014 BY SAVARIAR
           mwerks        TYPE mard-werks,
           mersda        TYPE mard-ersda,
           mlabst        TYPE mard-labst,            " Added by mani 13.02.2016
           mlgort        TYPE mard-lgort,
           vprsv         TYPE mbew-vprsv,              " Price Control
           zage1         TYPE p DECIMALS 2,             " A1 < 30 Days
           vzage1        TYPE p DECIMALS 2,            " Stock In Ltrs
           zage2         TYPE p DECIMALS 2,             " A2 31-45 Days
           vzage2        TYPE p DECIMALS 2,            " Stock In Ltrs
           zage3         TYPE p DECIMALS 2,             " A3 46-60 Days
           vzage3        TYPE p DECIMALS 2,            " Stock In Ltrs
           zage4         TYPE p DECIMALS 2,             " A3 61-75 Days
           vzage4        TYPE p DECIMALS 2,            " Stock In Ltrs
           zage5         TYPE p DECIMALS 2,             " A3 76-90 Days
           vzage5        TYPE p DECIMALS 2,            " Stock In Ltrs
           zage6         TYPE p DECIMALS 2,             " A1 91-180 Days
           vzage6        TYPE p DECIMALS 2,            " Stock In Ltrs
           zage7         TYPE p DECIMALS 2,             " > 180 Days
           vzage7        TYPE p DECIMALS 2,            " Stock In Ltrs
           zage8         TYPE p DECIMALS 2,             " A3 61-90 Days
           vzage8        TYPE p DECIMALS 2,            " Stock In Ltrs
           zage9         TYPE p DECIMALS 2,             " A3 91-180 Days
           vzage9        TYPE p DECIMALS 2,            " Stock In Ltrs
           zage10        TYPE p DECIMALS 2,             " A5 Above 180 Days
           vzage10       TYPE p DECIMALS 2,            " Stock In Ltrs
           zage11        TYPE p DECIMALS 2,             " A5 Above 180 Days
           vzage11       TYPE p DECIMALS 2,            " Stock In Ltrs
           zage12        TYPE p DECIMALS 2,             " A5 Above 180 Days
           vzage12       TYPE p DECIMALS 2,            " Stock In Ltrs
           price         TYPE p DECIMALS 2,       " STOCKVALUE
           stockvalue    TYPE p DECIMALS 2,       " STOCKVALUE
           stockvalue1   TYPE p DECIMALS 2,       " STOCKVALUE
           stockvalue2   TYPE p DECIMALS 2,       " STOCKVALUE
           stockvalue3   TYPE p DECIMALS 2,       " STOCKVALUE
           stockvalue4   TYPE p DECIMALS 2,       " STOCKVALUE
           stockvalue5   TYPE p DECIMALS 2,       " STOCKVALUE
           stockvalue6   TYPE p DECIMALS 2,       " STOCKVALUE
           stockvalue7   TYPE p DECIMALS 2,       " STOCKVALUE
           stockvalue8   TYPE p DECIMALS 2,       " STOCKVALUE
           stockvalue9   TYPE p DECIMALS 2,       " STOCKVALUE
           stockvalue10  TYPE p DECIMALS 2,       " STOCKVALUE
           stockvalue11  TYPE p DECIMALS 2,       " STOCKVALUE
           stockvalue12  TYPE p DECIMALS 2,       " STOCKVALUE
           stockvalue13  TYPE p DECIMALS 2,       " STOCKVALUE
           stockcase1    TYPE p DECIMALS 2,       " STOCKVALUE
           stockcase2    TYPE p DECIMALS 2,       " STOCKVALUE
           stockcase3    TYPE p DECIMALS 2,       " STOCKVALUE
           stockcase4    TYPE p DECIMALS 2,       " STOCKVALUE
           stockcase5    TYPE p DECIMALS 2,       " STOCKVALUE
           stockcase6    TYPE p DECIMALS 2,       " STOCKVALUE
           stockcase7    TYPE p DECIMALS 2,       " STOCKVALUE
           stockcase8    TYPE p DECIMALS 2,       " STOCKVALUE
           stockcase9    TYPE p DECIMALS 2,       " STOCKVALUE
           stockcase10   TYPE p DECIMALS 2,       " STOCKVALUE
           stockcase11   TYPE p DECIMALS 2,       " STOCKVALUE
           stockcase12   TYPE p DECIMALS 2,       " STOCKVALUE
           stock_quality TYPE p DECIMALS 2,       " STOCKVALUE QUALITY
           stock_transit TYPE p DECIMALS 2,       " STOCKVALUE TRANSIT
           stock_blocked TYPE p DECIMALS 2,
           v_werkcount   TYPE i,
           v_matcount    TYPE i,
           v_count       TYPE i,
           old_material  TYPE matnr,
         END OF gs_final.


  DATA: gt_final TYPE TABLE OF gs_final,
        gs_final TYPE gs_final.

  DATA: lt_final TYPE TABLE OF gs_final,
        ls_final TYPE gs_final.


  DATA: lt_seltab TYPE TABLE OF rsparams,
        ls_seltab LIKE LINE OF lt_seltab.

*  TYPES: tr_matnr TYPE RANGE OF mara-matnr,
*         ty_matnr TYPE LINE OF tr_matnr.
*
*
*  DATA: lr_matnr TYPE tr_matnr,
*        ls_matnr TYPE ty_matnr.
*
*  DATA: ls_mat_range TYPE mat_range.

  DATA: ls_stock_age TYPE zstr_stock_agewise.

*DATA: lt_outtab TYPE STANDARD TABLE OF alv_t_t2.
  FIELD-SYMBOLS: <lt_outtab> LIKE gt_final.


  DATA lo_data TYPE REF TO data.


  REFRESH: lt_seltab.
  CLEAR ls_seltab.
  ls_seltab-selname = 'SO_BUKRS'.          " Name of parameter on submitted program
  ls_seltab-kind    = 'S'.
  ls_seltab-sign    = 'I'.
  ls_seltab-option  = 'EQ'.
  ls_seltab-low     = comp_code.
  APPEND ls_seltab TO lt_seltab.


  IF plant-low IS NOT INITIAL AND plant-high IS INITIAL.  "Plant Single Input

    CLEAR ls_seltab.
    ls_seltab-selname = 'SO_WERKS'.
    ls_seltab-kind    = 'S'.
    ls_seltab-sign    = 'I'.
    ls_seltab-option  = 'EQ'.
    ls_seltab-low     = plant-low.
    APPEND ls_seltab TO lt_seltab.

  ELSEIF plant-low IS NOT INITIAL AND plant-high IS NOT INITIAL. "Plant multiple Plant

    CLEAR ls_seltab.
    ls_seltab-selname = 'SO_WERKS'.
    ls_seltab-kind    = 'S'.
    ls_seltab-sign    = 'I'.
    ls_seltab-option  = 'BT'.
    ls_seltab-low     = plant-low.
    ls_seltab-high    = plant-high.
    APPEND ls_seltab TO lt_seltab.

  ENDIF.


  IF material-low IS NOT INITIAL AND material-high IS INITIAL. "Material Single Input

    CLEAR ls_seltab.
    ls_seltab-selname = 'SO_MATNR'.
    ls_seltab-kind    = 'S'.
    ls_seltab-sign    = 'I'.
    ls_seltab-option  = 'EQ'.
    ls_seltab-low     = material-low.
    APPEND ls_seltab TO lt_seltab.

  ELSEIF material-low IS NOT INITIAL AND material-high IS NOT INITIAL. "Material Multiple Input

    CLEAR ls_seltab.
    ls_seltab-selname = 'SO_MATNR'.
    ls_seltab-kind    = 'S'.
    ls_seltab-sign    = 'I'.
    ls_seltab-option  = 'BT'.
    ls_seltab-low     = material-low.
    ls_seltab-high    = material-high.
    APPEND ls_seltab TO lt_seltab.

  ENDIF.

  CLEAR ls_seltab.
  ls_seltab-selname = 'WI_BATCH'.
  ls_seltab-kind    = 'P'.
  ls_seltab-sign    = 'I'.
  ls_seltab-option  = 'EQ'.
  ls_seltab-low     = 'X'.
  APPEND ls_seltab TO lt_seltab.

  CLEAR ls_seltab.
  ls_seltab-selname = 'P_BAPI'.
  ls_seltab-kind    = 'P'.
  ls_seltab-sign    = 'I'.
  ls_seltab-option  = 'EQ'.
  ls_seltab-low     = 'X'.
  APPEND ls_seltab TO lt_seltab.

  CLEAR ls_seltab.
  ls_seltab-selname = 'P_DATE'.
  ls_seltab-kind    = 'P'.
  ls_seltab-sign    = 'I'.
  ls_seltab-option  = 'EQ'.
  ls_seltab-low     = document_date.
  APPEND ls_seltab TO lt_seltab.

  REFRESH gt_final.
  "let know the model
  cl_salv_bs_runtime_info=>set(
   EXPORTING
     display  = abap_false
     metadata = abap_false
     data     = abap_true
  ).

  SUBMIT zstk_agewise_report_new            " Report for Stock agewise
         WITH SELECTION-TABLE lt_seltab
         AND RETURN.

  TRY.
      " get data from SALV model
      cl_salv_bs_runtime_info=>get_data_ref(
            IMPORTING
              r_data = lo_data
      ).
      ASSIGN lo_data->* TO <lt_outtab>.

      gt_final[] = <lt_outtab>[].
      IF  sy-subrc EQ 0.
        SORT gt_final[] BY mmatnr mwerks.
      ENDIF.
    CATCH cx_salv_bs_sc_runtime_info.

  ENDTRY.

  REFRESH : lt_stock_agewise.

  IF gt_final[] IS NOT INITIAL.

    SORT gt_final[] BY matnr werks.

    CLEAR gs_final.
    LOOP AT gt_final INTO gs_final.

      CLEAR ls_stock_age.
      ls_stock_age-bukrs = gs_final-bukrs.
      ls_stock_age-material = gs_final-matnr.
      ls_stock_age-material_des = gs_final-maktx.
      ls_stock_age-plant = gs_final-werks.
      ls_stock_age-plant_name = gs_final-name1.
      ls_stock_age-material_type = gs_final-mtart.
      ls_stock_age-storage_loc = gs_final-lgort.
      ls_stock_age-created_on = gs_final-ersda.
      ls_stock_age-stock_qty = gs_final-clabs.
      ls_stock_age-uom = gs_final-meins.
      ls_stock_age-price_control = gs_final-vprsv.
      ls_stock_age-price = gs_final-price.
      ls_stock_age-stock_value = gs_final-stockvalue.
      ls_stock_age-stk_qty_ltrs = gs_final-vclabs.
      ls_stock_age-30_days_stk = gs_final-zage1.
      ls_stock_age-30days_stk_val = gs_final-stockcase1.
      ls_stock_age-30days_stk_ltrs = gs_final-vzage1.
      ls_stock_age-31_45_days_stk = gs_final-zage2.
      ls_stock_age-31_45_days_val = gs_final-stockcase2.
      ls_stock_age-31_45_days_ltrs = gs_final-vzage2.
      ls_stock_age-46_60_days_stk = gs_final-zage3.
      ls_stock_age-46_60_days_val = gs_final-stockcase3.
      ls_stock_age-46_60_stk_ltrs = gs_final-vzage3.
      ls_stock_age-61_75_days_stk = gs_final-zage4 .
      ls_stock_age-61_75_days_val = gs_final-stockcase4.
      ls_stock_age-61_75_stk_ltrs = gs_final-vzage4.
      ls_stock_age-76_90_days_stk = gs_final-zage5.
      ls_stock_age-76_90_days_val = gs_final-stockcase5.
      ls_stock_age-76_90_stk_ltrs = gs_final-vzage5.
      ls_stock_age-91_120_days_stk = gs_final-zage6 .
      ls_stock_age-91_120_days_val = gs_final-stockcase6.
      ls_stock_age-91_120_stk_ltrs = gs_final-vzage6.
      ls_stock_age-121_150_days_stk = gs_final-zage7 .
      ls_stock_age-121_150_days_val = gs_final-stockcase7.
      ls_stock_age-121_150_days_ltrs = gs_final-vzage7.
      ls_stock_age-151_180_days_stk = gs_final-zage8.
      ls_stock_age-151_180_days_val = gs_final-stockcase8.
      ls_stock_age-151_180_days_ltrs = gs_final-vzage8.
      ls_stock_age-180_days_stk = gs_final-zage9.
      ls_stock_age-180_days_val = gs_final-stockcase9.
      ls_stock_age-180_days_stk_ltrs = gs_final-vzage9.
      ls_stock_age-batch = gs_final-charg.
      APPEND ls_stock_age TO lt_stock_agewise.


    ENDLOOP.
  ENDIF.
ENDFUNCTION.
