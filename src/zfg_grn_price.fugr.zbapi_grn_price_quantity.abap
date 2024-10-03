FUNCTION zbapi_grn_price_quantity.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(PLANT) TYPE  WERKS_D
*"     VALUE(MATERIAL) TYPE  RANGE_S_MATNR OPTIONAL
*"     VALUE(FROM_DATE) TYPE  DATUM
*"     VALUE(TO_DATE) TYPE  DATUM
*"  TABLES
*"      LT_QUAN_PRICE STRUCTURE  ZSTR_GRN_QUAN_PRICE
*"----------------------------------------------------------------------

  TYPES: BEGIN OF ty_mseg,
           mblnr      TYPE mblnr,
           line_id    TYPE mb_line_id,
           parent_id  TYPE mb_parent_id,
           bwart      TYPE bwart,
           matnr      TYPE matnr,
           werks      TYPE werks_d,
           charg      TYPE charg_d,
           lifnr      TYPE elifn,
           shkzg      TYPE shkzg,
           waers      TYPE waers,
           dmbtr      TYPE dmbtr_cs,
           menge      TYPE menge_d,
           ebeln      TYPE bstnr,
           ebelp      TYPE ebelp,
           budat_mkpf TYPE budat,
         END OF ty_mseg,
         BEGIN OF ty_mseg1,
           mblnr      TYPE mblnr,
           line_id    TYPE mb_line_id,
           parent_id  TYPE mb_parent_id,
           bwart      TYPE bwart,
           matnr      TYPE matnr,
           werks      TYPE werks_d,
           charg      TYPE charg_d,
           lifnr      TYPE elifn,
           shkzg      TYPE shkzg,
           waers      TYPE waers,
           dmbtr      TYPE dmbtr_cs,
           menge      TYPE menge_d,
           ebeln      TYPE bstnr,
           ebelp      TYPE ebelp,
           umwrk      TYPE umwrk,
           umcha      TYPE umcha,
           budat_mkpf TYPE budat,
         END OF ty_mseg1,
         BEGIN OF ty_ekko,
           ebeln TYPE ebeln,
           bsart TYPE bsart,
           waers TYPE waers,
           wkurs TYPE wkurs,
         END OF ty_ekko,
         BEGIN OF ty_ekpo,
           ebeln TYPE ebeln,
           ebelp TYPE ebelp,
           matnr TYPE matnr,
           werks TYPE ewerk,
           menge TYPE bstmg,
           netwr TYPE bwert,
         END OF ty_ekpo,
         BEGIN OF ty_vbrp,
           vbeln TYPE vbeln_vf,
           fkimg TYPE fkimg,
           netwr TYPE netwr_fp,
           aubel TYPE vbeln_va,
           matnr TYPE matnr,
           charg TYPE charg_d,
           mwsbp TYPE mwsbp,
         END OF ty_vbrp.

  DATA: lt_mseg  TYPE TABLE OF ty_mseg,
        ls_mseg  TYPE ty_mseg,
        lt_mseg1 TYPE TABLE OF ty_mseg,
        ls_mseg1 TYPE ty_mseg,
        lt_mseg2 TYPE TABLE OF ty_mseg,
        ls_mseg2 TYPE ty_mseg.
  DATA: lt_mt351 TYPE TABLE OF ty_mseg1,
        ls_mt351 TYPE ty_mseg1.
  DATA: lt_mt641 TYPE TABLE OF ty_mseg1,
        ls_mt641 TYPE ty_mseg1.
  DATA: lt_ekko TYPE TABLE OF ty_ekko,
        ls_ekko TYPE ty_ekko.
  DATA: lt_ekpo TYPE TABLE OF ty_ekpo,
        ls_ekpo TYPE ty_ekpo.
  DATA: lt_vbrp TYPE TABLE OF ty_vbrp,
        ls_vbrp TYPE ty_vbrp.

  TYPES: tr_matnr TYPE RANGE OF mara-matnr,
         ty_matnr TYPE LINE OF tr_matnr.


  DATA: lr_matnr TYPE tr_matnr,
        ls_matnr TYPE ty_matnr.

  DATA: ls_mat_range TYPE mat_range.

  DATA: ls_quan_price TYPE zstr_grn_quan_price.

  DATA: lv_quan  TYPE menge_d,
        lv_price TYPE dmbtr_cs.

  DATA: lv_po_quan  TYPE menge_d,
        lv_po_price TYPE dmbtr_cs.

  DATA: lv_foreign TYPE dmbtr_cs.

  DATA: lt_ckm3 TYPE TABLE OF zstr_map_price,
        ls_ckm3 TYPE zstr_map_price.

  DATA: lt_bwart TYPE RANGE OF mseg-bwart,
        ls_bwart LIKE LINE OF lt_bwart.

  DATA: str3 TYPE lfgja, "Year
        str2 TYPE lfmon, "Month
        str1 TYPE lfmon. "Day

  DATA: lv_month TYPE bkpf-monat.
  DATA: lv_year TYPE bkpf-gjahr.

  DATA: lv_poper TYPE poper. "Period (OR) Month

  DATA: lv_verpr TYPE verpr.
********************************
*Movement Type Used:
*  101 => GRN Inward
*  351 => Transfer Posting
*  602 => Reversal
*****************************

  SELECT * FROM tvarvc INTO TABLE @DATA(lt_tvarvc)
           WHERE name = 'INWARD_MVT'
           AND type = 'S'.
  IF lt_tvarvc IS NOT INITIAL.
    LOOP AT lt_tvarvc INTO DATA(ls_tvarvc).
      CLEAR ls_bwart.
      ls_bwart-sign = 'I'.
      ls_bwart-option = 'EQ'.
      ls_bwart-low = ls_tvarvc-low.
      APPEND ls_bwart TO lt_bwart.
    ENDLOOP.
  ENDIF.

  IF plant IS NOT INITIAL AND from_date IS INITIAL AND to_date IS INITIAL.

    REFRESH: lt_mseg.
    SELECT mblnr
           line_id
           parent_id
           bwart
           matnr
           werks
           charg
           lifnr
           shkzg
           waers
           dmbtr
           menge
           ebeln
           ebelp
           budat_mkpf FROM mseg
                      INTO TABLE lt_mseg
                      WHERE bwart IN lt_bwart
                      AND werks EQ plant
                      AND shkzg EQ 'S'.
    IF sy-subrc EQ 0 .
      SORT lt_mseg[] BY bwart matnr werks.
    ENDIF.

  ELSEIF plant IS NOT INITIAL AND from_date IS NOT INITIAL AND to_date IS NOT INITIAL AND material-low IS NOT INITIAL.

    REFRESH: lt_mseg.
    SELECT mblnr
           line_id
           parent_id
           bwart
           matnr
           werks
           charg
           lifnr
           shkzg
           waers
           dmbtr
           menge
           ebeln
           ebelp
           budat_mkpf FROM mseg
                      INTO TABLE lt_mseg
                      WHERE bwart IN lt_bwart
                      AND matnr EQ material-low
                      AND werks EQ plant
                      AND shkzg EQ 'S'
                      AND ( budat_mkpf BETWEEN from_date AND to_date ).
    IF sy-subrc EQ 0 .
      SORT lt_mseg[] BY bwart matnr werks.
    ENDIF.

  ELSEIF plant IS NOT INITIAL AND from_date IS NOT INITIAL AND to_date IS NOT INITIAL
                              AND material-low IS NOT INITIAL AND material-high IS NOT INITIAL.

    REFRESH: lt_mseg.
    SELECT mblnr
           line_id
           parent_id
           bwart
           matnr
           werks
           charg
           lifnr
           shkzg
           waers
           dmbtr
           menge
           ebeln
           ebelp
           budat_mkpf FROM mseg
                      INTO TABLE lt_mseg
                      WHERE bwart IN lt_bwart
                      AND ( matnr BETWEEN material-low AND material-high )
                      AND werks EQ plant
                      AND shkzg EQ 'S'
                      AND ( budat_mkpf BETWEEN from_date AND to_date ).
    IF sy-subrc EQ 0 .
      SORT lt_mseg[] BY bwart matnr werks.
    ENDIF.

  ELSEIF plant IS NOT INITIAL AND from_date IS NOT INITIAL
                              AND to_date IS NOT INITIAL.
    REFRESH: lt_mseg.
    SELECT mblnr
           line_id
           parent_id
           bwart
           matnr
           werks
           charg
           lifnr
           shkzg
           waers
           dmbtr
           menge
           ebeln
           ebelp
           budat_mkpf FROM mseg
                      INTO TABLE lt_mseg
                      WHERE bwart IN lt_bwart
                      AND werks EQ plant
                      AND shkzg EQ 'S'
                      AND ( budat_mkpf BETWEEN from_date AND to_date ).
    IF sy-subrc EQ 0 .
      SORT lt_mseg[] BY bwart matnr werks.
    ENDIF.


  ELSEIF plant IS NOT INITIAL AND from_date IS NOT INITIAL
                              AND to_date IS  INITIAL.
    REFRESH: lt_mseg.
    SELECT mblnr
           line_id
           parent_id
           bwart
           matnr
           werks
           charg
           lifnr
           shkzg
           waers
           dmbtr
           menge
           ebeln
           ebelp
           budat_mkpf FROM mseg
                      INTO TABLE lt_mseg
                      WHERE bwart IN lt_bwart
                      AND werks EQ plant
                      AND shkzg EQ 'S'
                      AND ( budat_mkpf BETWEEN from_date AND sy-datum ).
    IF sy-subrc EQ 0 .
      SORT lt_mseg[] BY bwart matnr werks.
    ENDIF.
  ENDIF.

  IF lt_mseg[] IS NOT INITIAL.

    REFRESH: lt_mseg1[].
    lt_mseg1[] = lt_mseg[].
    lt_mseg2[] = lt_mseg[].
    DELETE ADJACENT DUPLICATES FROM lt_mseg2[] COMPARING matnr.
    SORT lt_mseg1[] BY bwart.
    SORT lt_mseg2[] BY bwart.
    SELECT mblnr
           line_id
           parent_id
           bwart
           matnr
           werks
           charg
           lifnr
           shkzg
           waers
           dmbtr
           menge
           ebeln
           ebelp
           umwrk
           umcha
           budat_mkpf FROM mseg
                      INTO TABLE lt_mt351
                      FOR ALL ENTRIES IN lt_mseg
                      WHERE bwart EQ '351'
                      AND matnr = lt_mseg-matnr
                      AND shkzg EQ 'H'
                      AND umwrk = lt_mseg-werks
                      AND ( budat_mkpf BETWEEN from_date AND to_date ).
    IF sy-subrc EQ 0.
      SORT lt_mt351 BY bwart line_id matnr ebeln ebelp umwrk.
    ENDIF.

    SELECT mblnr
           line_id
           parent_id
           bwart
           matnr
           werks
           charg
           lifnr
           shkzg
           waers
           dmbtr
           menge
           ebeln
           ebelp
           umwrk
           umcha
           budat_mkpf FROM mseg
                      INTO TABLE lt_mt641
                      FOR ALL ENTRIES IN lt_mseg
                      WHERE bwart EQ '641'
                      AND matnr = lt_mseg-matnr
                      AND shkzg EQ 'S'
                      AND xauto = 'X'
                      AND werks = lt_mseg-werks
                      AND ( budat_mkpf BETWEEN from_date AND to_date ).
    IF sy-subrc EQ 0.
      SORT lt_mt641 BY matnr werks parent_id.
    ENDIF.

    REFRESH: lt_ekko.
    SELECT ebeln
           bsart
           waers
           wkurs FROM ekko
                 INTO TABLE lt_ekko
                 FOR ALL ENTRIES IN lt_mseg
                 WHERE ebeln EQ lt_mseg-ebeln.
    IF sy-subrc EQ 0.
      SORT lt_ekko[] BY ebeln.
    ENDIF.

    REFRESH: lt_ekpo.
    SELECT ebeln
           ebelp
           matnr
           werks
           menge
           netwr FROM ekpo
                 INTO TABLE lt_ekpo
                 FOR ALL ENTRIES IN lt_mseg
                 WHERE ebeln EQ lt_mseg-ebeln
                 AND ebelp EQ lt_mseg-ebelp.
    IF sy-subrc EQ 0.
      SORT lt_ekpo[] BY ebeln matnr.
    ENDIF.

    REFRESH: lt_vbrp.
    SELECT vbeln
           fkimg
           netwr
           aubel
           matnr
           charg
           mwsbp FROM vbrp
                 INTO TABLE lt_vbrp
                 FOR ALL ENTRIES IN lt_mseg
                 WHERE fkimg NE '0'  AND
                 aubel EQ lt_mseg-ebeln
                 AND matnr EQ lt_mseg-matnr.
    IF sy-subrc EQ 0.
      SORT lt_vbrp[] BY aubel matnr.
    ENDIF.

    SELECT lifnr,name1 FROM lfa1 INTO TABLE @DATA(lt_lfa1)
                       FOR ALL ENTRIES IN @lt_mseg
                       WHERE lifnr EQ @lt_mseg-lifnr.
    SORT lt_lfa1[] BY lifnr.
  ENDIF.


  LOOP AT lt_mseg2 INTO ls_mseg2.
    CLEAR ls_matnr.
    ls_matnr-sign   = 'I'.
    ls_matnr-option = 'EQ'.
    ls_matnr-low    = ls_mseg2-matnr.
    APPEND ls_matnr TO lr_matnr.
  ENDLOOP.

  CALL FUNCTION 'GET_CURRENT_YEAR'
    EXPORTING
      bukrs = '1000'
      date  = from_date
    IMPORTING
      currm = lv_month
      curry = lv_year.

  IF lv_month = '01'.
    lv_year = lv_year - 1.
    lv_month = '12'.
  ELSE.
    lv_month = lv_month - 1.
  ENDIF.
  CLEAR: lv_poper.
  lv_poper = lv_month.
  DATA: lv_fisyear TYPE ckmlpp-bdatj.
  CLEAR lv_fisyear.
  lv_fisyear = lv_year.
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

  REFRESH: lt_quan_price.
  LOOP AT lt_mseg INTO ls_mseg.
    ls_quan_price-material_doc = ls_mseg-mblnr.
    ls_quan_price-plant = ls_mseg-werks.
    ls_quan_price-movement_type = ls_mseg-bwart.
    ls_quan_price-material = ls_mseg-matnr.
    ls_quan_price-batch_no = ls_mseg-charg.
    ls_quan_price-po_number = ls_mseg-ebeln.


    CLEAR ls_ekko.
    READ TABLE lt_ekko INTO ls_ekko WITH KEY ebeln = ls_quan_price-po_number BINARY SEARCH.
    IF sy-subrc EQ 0.
      ls_quan_price-po_doc_type = ls_ekko-bsart.
      ls_quan_price-currency = ls_ekko-waers.
      ls_quan_price-curr_ex_rate = ls_ekko-wkurs.
    ENDIF.


    ls_quan_price-grn_price = ls_mseg-dmbtr.
    ls_quan_price-grn_quan = ls_mseg-menge.


    ls_quan_price-posting_date = ls_mseg-budat_mkpf.
    ls_quan_price-supplier = ls_mseg-lifnr.

    READ TABLE lt_lfa1 INTO DATA(ls_lfa1) WITH KEY lifnr = ls_mseg-lifnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      ls_quan_price-supplier_nme = ls_lfa1-name1.
    ENDIF.

    IF ls_quan_price-po_doc_type EQ 'ZUB'.
      CLEAR ls_vbrp.
      READ TABLE lt_vbrp INTO ls_vbrp WITH KEY aubel = ls_mseg-ebeln
                                               matnr = ls_mseg-matnr.
      IF sy-subrc EQ 0.
        ls_quan_price-invoice_no = ls_vbrp-vbeln.
        ls_quan_price-invoice_quan = ls_vbrp-fkimg.
        ls_quan_price-invoice_price = ls_vbrp-netwr.
      ENDIF.
    ENDIF.

    IF ls_mseg-ebeln IS NOT INITIAL AND ls_mseg-matnr IS NOT INITIAL.
      CLEAR: ls_ekpo,lv_po_price,lv_po_quan.
      LOOP AT lt_ekpo INTO ls_ekpo WHERE ebeln = ls_mseg-ebeln
                                   AND matnr = ls_mseg-matnr
                                   AND werks = ls_mseg-werks.
        lv_po_quan = lv_po_quan + ls_ekpo-menge.
        lv_po_price = lv_po_price + ls_ekpo-netwr.
      ENDLOOP.
      ls_quan_price-po_quan = lv_po_quan.
      ls_quan_price-po_price = lv_po_price.
    ENDIF.

    IF ls_quan_price-grn_price IS INITIAL.
      CLEAR ls_mt351.
      READ TABLE lt_mt351 INTO ls_mt351 WITH KEY bwart = '351'
                                                 line_id = ls_mseg-line_id
                                                 matnr = ls_mseg-matnr
                                                 ebeln = ls_mseg-ebeln
                                                 ebelp = ls_mseg-ebelp
                                                 umwrk = ls_mseg-werks BINARY SEARCH.
      IF sy-subrc EQ 0.
        ls_quan_price-grn_price = ls_mt351-dmbtr.
      ENDIF.
    ENDIF.

    IF ls_quan_price-grn_price IS INITIAL.
      CLEAR lv_verpr.
*-----Custom Function Module Checks for MAP price ******
      CLEAR ls_ckm3.
      READ TABLE lt_ckm3 INTO ls_ckm3 WITH KEY material = ls_mseg-matnr
                                               plant = ls_mseg-werks BINARY SEARCH.
      IF sy-subrc EQ 0.
        lv_verpr = ls_ckm3-map_price.
        ls_quan_price-grn_price = ls_mseg-menge * lv_verpr.
      ENDIF.
    ENDIF.

    IF ls_quan_price-grn_price IS INITIAL.

      CLEAR: lv_verpr,ls_mt641.
      READ TABLE lt_mt641 INTO ls_mt641 WITH KEY bwart = '641'
                                                 matnr = ls_mseg-matnr
                                                 werks = ls_mseg-werks
                                                 umcha = ls_mseg-charg BINARY SEARCH.
      IF sy-subrc EQ 0.
        lv_verpr = ls_mt641-dmbtr / ls_mt641-menge.
      ENDIF.
      ls_quan_price-grn_price = ls_mseg-menge * lv_verpr.
    ENDIF.

    APPEND ls_quan_price TO lt_quan_price.
    CLEAR: ls_quan_price, ls_mseg.
  ENDLOOP.

ENDFUNCTION.
