FUNCTION zbapi_batch_grn_price_quan.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(PLANT) TYPE  WERKS_D
*"     VALUE(MATERIAL) TYPE  MATNR OPTIONAL
*"     VALUE(FROM_DATE) TYPE  BUDAT
*"     VALUE(TO_DATE) TYPE  BUDAT
*"  TABLES
*"      IT_GRN_DETAILS STRUCTURE  ZSTR_GRN_DETAILS
*"  EXCEPTIONS
*"      INCORRECT_PLANT
*"      INCORRECT_MATERIAL
*"----------------------------------------------------------------------
******************************************************************************
  " Created_on: 20.07.2022
  " Created_by: Samsudeen M
  " Reference by: Suresh B V & Praveen & Gopalraja
  " Description: GRN Quan & Price based on plant and batchwise in Posting date range
  " TR NO: DEVK931919.
**************************************************************************************

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

  TYPES: BEGIN OF ty_mseg,
           mblnr      TYPE mblnr,
           bwart      TYPE bwart,
           matnr      TYPE matnr,
           werks      TYPE werks_d,
           charg      TYPE charg_d,
           lifnr      TYPE elifn,
           dmbtr      TYPE dmbtr_cs,
           menge      TYPE menge_d,
           ebeln      TYPE ebeln,
           budat_mkpf TYPE budat,
         END OF ty_mseg.

  TYPES: BEGIN OF ty_lfa1,
           lifnr TYPE elifn,
           name1 TYPE name1_gp,
         END OF ty_lfa1.

  TYPES: BEGIN OF ty_ekko,
           ebeln TYPE ebeln,
           bsart TYPE bsart,
           reswk TYPE reswk,
         END OF ty_ekko.

  DATA : g_s_totals_flat TYPE  stype_totals_flat,
         g_t_totals_flat TYPE  stab_totals_flat.

  DATA: gs_marc TYPE marc.

  DATA: lt_mseg  TYPE TABLE OF ty_mseg,
        lt_mseg1 TYPE TABLE OF ty_mseg,
        ls_mseg  TYPE ty_mseg,
        ls_mseg1 TYPE ty_mseg.

  DATA: lt_lfa1 TYPE TABLE OF ty_lfa1,
        ls_lfa1 TYPE ty_lfa1.

  DATA: lt_ekko TYPE TABLE OF ty_ekko,
        ls_ekko TYPE ty_ekko.
  DATA: gr_value TYPE menge_d.
*DATA: lt_outtab TYPE STANDARD TABLE OF alv_t_t2.
  FIELD-SYMBOLS: <lt_outtab> LIKE  g_t_totals_flat.
  FIELD-SYMBOLS: <lt_outtab1> LIKE  g_t_totals_flat.

  DATA: lt_final  TYPE TABLE OF stype_totals_flat,
        ls_final  LIKE LINE OF lt_final,
        lt_final1 TYPE TABLE OF stype_totals_flat,
        ls_final1 LIKE LINE OF lt_final1.

  DATA lo_data TYPE REF TO data.
  DATA: lo_data1 TYPE REF TO data.

  DATA: lt_seltab TYPE TABLE OF rsparams,
        ls_seltab LIKE LINE OF lt_seltab.

  DATA: ls_grn_details TYPE zstr_grn_details.

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

*    CLEAR: from_date,to_date.
*    LOOP AT lt_seltab INTO ls_seltab WHERE selname = 'DATUM'.
*      CLEAR: ls_seltab-low,ls_seltab-high.
*      MODIFY lt_seltab FROM ls_seltab.
*    ENDLOOP.
    DELETE lt_seltab WHERE selname = 'DATUM'.
    REFRESH lt_final1.
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
                r_data = lo_data1
        ).

        ASSIGN lo_data1->* TO <lt_outtab1>.


        lt_final1[] = <lt_outtab1>[].
        IF  sy-subrc EQ 0.
          SORT lt_final1[] BY matnr.
        ENDIF.
      CATCH cx_salv_bs_sc_runtime_info.

    ENDTRY.
  ENDIF.


  IF lt_final[] IS NOT INITIAL.
*
*    IF from_date IS NOT INITIAL AND to_date IS NOT INITIAL.
*************** Fetching GR Quantity and GR Value Based on program output *****************
*      REFRESH : lt_mseg.
*      SELECT mblnr
*             bwart
*             matnr
*             werks
*             charg
*             lifnr
*             dmbtr
*             menge
*             ebeln
*             budat_mkpf FROM mseg
*                        INTO TABLE lt_mseg
*                        FOR ALL ENTRIES IN lt_final
*                        WHERE bwart IN ( '101' , '653' )
*                        AND matnr EQ lt_final-matnr
*                        AND werks EQ lt_final-werks
*                        AND charg EQ lt_final-charg
*                        AND ( budat_mkpf BETWEEN from_date AND to_date ).
************** fetching gr quantity and gr Value based ON PROGRAM output *****************
    REFRESH : lt_mseg.
    SELECT mblnr
           bwart
           matnr
           werks
           charg
           lifnr
           dmbtr
           menge
           ebeln
           budat_mkpf FROM mseg
                      INTO TABLE lt_mseg
                      FOR ALL ENTRIES IN lt_final
                      WHERE bwart IN ( '101' , '352' , '343' , '561' , '602' , '642' )
                      AND matnr EQ lt_final-matnr
                      AND werks EQ lt_final-werks
                      AND charg EQ lt_final-charg.

*    ENDIF.

    IF lt_mseg[] IS NOT INITIAL.
      SORT lt_mseg[] BY matnr werks charg.
************ Fetching Supplier Name *************
      REFRESH lt_lfa1.
      SELECT lifnr
             name1 FROM lfa1
                   INTO TABLE lt_lfa1
                   FOR ALL ENTRIES IN lt_mseg
                   WHERE lifnr EQ lt_mseg-lifnr.
      IF sy-subrc EQ 0.
        SORT lt_lfa1[] BY lifnr.
      ENDIF.

**********Fetching Document type from ekko ************
      REFRESH lt_ekko.
      SELECT ebeln
             bsart
             reswk FROM ekko
                   INTO TABLE lt_ekko
                   FOR ALL ENTRIES IN lt_mseg
                   WHERE ebeln EQ lt_mseg-ebeln.
      IF sy-subrc EQ 0.
        SORT lt_ekko[] BY ebeln.
      ENDIF.
    ENDIF.

  ENDIF.

  REFRESH lt_mseg1.
  lt_mseg1[] = lt_mseg[].
  SORT lt_mseg1[] BY matnr werks charg.

  DATA: lv_value TYPE dmbtr_cs. " Variable for GRN value Calculation

  CLEAR: ls_final.
  LOOP AT lt_final INTO ls_final.
    CLEAR ls_grn_details.

    ls_grn_details-matnr = ls_final-matnr.
    ls_grn_details-maktx = ls_final-maktx.
    ls_grn_details-plant = ls_final-werks.
    ls_grn_details-name1 = ls_final-name1.   " Plant Name
    ls_grn_details-batch_no = ls_final-charg. " Batch
    ls_grn_details-start_date = ls_final-start_date.   " Start_date
    ls_grn_details-opening_stock = ls_final-anfmenge.  " Opening_stock
    ls_grn_details-to_date = ls_final-end_date.        " End_date
    ls_grn_details-closing_stock = ls_final-endmenge.  " Closing_stock

    CLEAR: ls_mseg1,lv_value.
    LOOP AT lt_mseg1 INTO ls_mseg1 WHERE matnr = ls_final-matnr
                                  AND werks = ls_final-werks
                                  AND charg = ls_final-charg.
      .
      CLEAR: ls_mseg,lv_value,gr_value.
      LOOP AT lt_mseg INTO ls_mseg WHERE matnr = ls_mseg1-matnr
                                  AND werks = ls_mseg1-werks
                                  AND charg = ls_mseg1-charg.

        lv_value = lv_value + ls_mseg-dmbtr.
      ENDLOOP.

      CLEAR: ls_grn_details-grn_price.
      ls_grn_details-grn_price = lv_value. " GRN Price
      ls_grn_details-supplier = ls_mseg1-lifnr.
      ls_grn_details-posting_date = ls_mseg1-budat_mkpf.

      CLEAR ls_ekko.
      READ TABLE lt_ekko INTO ls_ekko WITH KEY ebeln = ls_mseg1-ebeln.
      IF sy-subrc EQ 0.
        ls_grn_details-document_type = ls_ekko-bsart.  " Document Type
      ENDIF.

      IF ls_grn_details-document_type EQ 'UB'.
        CLEAR ls_grn_details-grn_price.
        READ TABLE lt_ekko INTO ls_ekko WITH KEY ebeln = ls_mseg1-ebeln.
        IF sy-subrc EQ 0.

        ENDIF.
      ENDIF.
    ENDLOOP.

    CLEAR ls_lfa1.
    READ TABLE lt_lfa1 INTO ls_lfa1 WITH KEY lifnr = ls_grn_details-supplier.
    IF sy-subrc EQ 0.
      ls_grn_details-supplier_name = ls_lfa1-name1. "Supplier Name
    ENDIF.

    CLEAR ls_final1.
    READ TABLE lt_final1 INTO ls_final1 WITH KEY matnr = ls_final-matnr
                                                   werks = ls_final-werks
                                                   charg = ls_final-charg.
    IF sy-subrc EQ 0.
      ls_grn_details-grn_quan = ls_final1-soll. " GR Quantity
    ENDIF.

    IF ls_grn_details-grn_quan IS INITIAL.
      CLEAR: ls_grn_details-grn_price,ls_grn_details-supplier,ls_grn_details-supplier_name,ls_grn_details-posting_date.
    ENDIF.
    APPEND ls_grn_details TO it_grn_details.

  ENDLOOP.

ENDFUNCTION.
