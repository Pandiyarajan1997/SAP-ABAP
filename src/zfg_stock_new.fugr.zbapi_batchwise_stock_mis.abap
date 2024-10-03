FUNCTION zbapi_batchwise_stock_mis.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(PLANT) TYPE  WERKS_D
*"     VALUE(MATNR) TYPE  /ACCGO/TT_APP_R_MATNR OPTIONAL
*"  TABLES
*"      LT_BATCHWISE STRUCTURE  ZSTR_BATCH_STOCK
*"----------------------------------------------------------------------
*******Created_on: 09.07.2022
*******Created_by: Samsudeen M
  "TR NO: DEVK93187
*****************************************************************************
  TYPES: BEGIN OF ty_marc,   "structure For MARC Table
           matnr TYPE matnr,
           werks TYPE werks_d,
           eisbe TYPE eisbe,
           trame TYPE trame,
         END OF ty_marc,
         BEGIN OF ty_mard,  "Structure For MARD Table
           matnr TYPE matnr,
           werks TYPE werks_d,
           labst TYPE labst,
           lfgja TYPE lfgja,
           lfmon TYPE lfmon,
         END OF ty_mard,
         BEGIN OF ty_mbew,    "structure for MBEW table
           matnr TYPE matnr,
           bwkey TYPE bwkey,
           salk3 TYPE salk3,
         END OF ty_mbew,
         BEGIN OF ty_eord, "structure for Eord Table
           matnr TYPE matnr,
           werks TYPE werks_d,
           vdatu TYPE ordab,
           bdatu TYPE ordbi,
           lifnr TYPE elifn,
         END OF ty_eord,
         BEGIN OF ty_mchb,     "Structure for MCHB Table
           matnr TYPE matnr,
           werks TYPE werks_d,
           lgort TYPE lgort_d,
           charg TYPE charg_d,
           clabs TYPE labst,
         END OF ty_mchb,
         BEGIN OF ty_ekpo,  " Structure for EKPO table
           matnr TYPE matnr,
           werks TYPE werks_d,
           netpr TYPE bprei,
         END OF ty_ekpo,
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
         END OF ty_lfa1.

  DATA: lt_stock  TYPE STANDARD TABLE OF zstr_batch_stock,
        lt_stock1 TYPE STANDARD TABLE OF zstr_batch_stock,
        ls_stock  TYPE zstr_batch_stock.

  DATA: lv_unrestrict TYPE labst.
  DATA: lt_marc TYPE TABLE OF ty_marc,
        ls_marc TYPE ty_marc.

  DATA: lt_mard TYPE TABLE OF ty_mard,
        ls_mard TYPE ty_mard.

  DATA: lt_eord TYPE TABLE OF ty_eord,
        ls_eord TYPE ty_eord.

  DATA: lt_mchb TYPE TABLE OF ty_mchb,
        ls_mchb TYPE ty_mchb.

  DATA: lt_ekpo TYPE TABLE OF ty_ekpo,
        ls_ekpo TYPE ty_ekpo.

  DATA: lt_mbew TYPE TABLE OF ty_mbew,
        ls_mbew TYPE ty_mbew.

  DATA: lt_mara TYPE TABLE OF ty_mara,
        ls_mara TYPE ty_mara.

  DATA: lt_makt TYPE TABLE OF ty_makt,
        ls_makt TYPE ty_makt.

  DATA: lt_lfa1 TYPE TABLE OF ty_lfa1,
        ls_lfa1 TYPE ty_lfa1.
  TYPES: BEGIN OF ty_matnr,
           matnr TYPE matnr,
         END OF ty_matnr.
  DATA: lt_matnr TYPE TABLE OF ty_matnr.

  DATA: lr_matnr TYPE RANGE OF mara-matnr.

  IF matnr IS NOT INITIAL.
    REFRESH lt_matnr.
    LOOP AT matnr INTO DATA(lw_matnr).
      APPEND VALUE #( matnr = lw_matnr-low ) TO lt_matnr.
    ENDLOOP.
  ENDIF.

  REFRESH: lt_eord,lt_mara,lt_makt,lt_lfa1,lt_marc,lt_mard,lt_mbew,lt_mchb.
  CLEAR: ls_eord,ls_mara,ls_makt,ls_lfa1,ls_marc,ls_mard,ls_mbew,ls_mchb.

  IF plant IS NOT INITIAL AND lt_matnr IS NOT INITIAL.

    SELECT matnr
           werks
           eisbe
           trame FROM marc
                 INTO TABLE lt_marc
                 FOR ALL ENTRIES IN lt_matnr
                 WHERE matnr EQ lt_matnr-matnr
                 AND werks = plant.
  ELSE.
    SELECT matnr
         werks
         eisbe
         trame FROM marc
               INTO TABLE lt_marc
               WHERE werks = plant.
  ENDIF.

  IF sy-subrc EQ 0.
    SORT lt_marc BY matnr werks.
  ENDIF.

  SELECT matnr
       werks
       vdatu
       bdatu
       lifnr FROM eord
             INTO TABLE lt_eord
             FOR ALL ENTRIES IN lt_marc
             WHERE matnr = lt_marc-matnr
             AND werks = lt_marc-werks.

  IF sy-subrc EQ 0.
    SORT lt_eord BY matnr werks.
  ENDIF.

  SELECT matnr
         werks
         lgort
         charg
         clabs FROM mchb
               INTO TABLE lt_mchb
               FOR ALL ENTRIES IN lt_marc
               WHERE matnr = lt_marc-matnr
               AND werks = lt_marc-werks
               AND clabs NE '0.00'.
  IF sy-subrc EQ 0.
    SORT lt_mchb[] BY matnr werks.
  ENDIF.

  IF lt_mchb[] IS NOT INITIAL.
    SELECT matnr
           werks
           netpr FROM ekpo
                 INTO TABLE lt_ekpo
                 FOR ALL ENTRIES IN lt_mchb
                 WHERE matnr = lt_mchb-matnr
                 AND werks = lt_mchb-werks.
    IF sy-subrc EQ 0.
      SORT lt_ekpo[] BY matnr werks.
    ENDIF.
  ENDIF.

  SELECT matnr
         mtart FROM mara
               INTO TABLE lt_mara
               FOR ALL ENTRIES IN lt_marc
               WHERE matnr = lt_marc-matnr.
  IF sy-subrc = 0.
    SORT lt_mara[] BY matnr.
  ENDIF.

  SELECT matnr
         maktx FROM makt
               INTO TABLE lt_makt
               FOR ALL ENTRIES IN lt_marc
               WHERE matnr = lt_marc-matnr.
  IF sy-subrc = 0.
    SORT lt_makt[] BY matnr.
  ENDIF.

  SELECT lifnr
         name1 FROM lfa1
               INTO TABLE lt_lfa1.

  IF sy-subrc EQ 0.
    SORT lt_lfa1 BY lifnr.
  ENDIF.

  DATA: lv_value TYPE salk3.
  LOOP AT lt_marc INTO DATA(lw_marc).
    LOOP AT lt_mchb INTO ls_mchb WHERE matnr = lw_marc-matnr.
      CLEAR: ls_stock.

      ls_stock-material = ls_mchb-matnr.
      ls_stock-plant = ls_mchb-werks.
      ls_stock-unrestricted_stk = ls_mchb-clabs.
      ls_stock-batch = ls_mchb-charg.
      ls_stock-st_loc = ls_mchb-lgort.
      CLEAR ls_marc.
      READ TABLE lt_marc INTO ls_marc WITH KEY matnr = ls_mchb-matnr
                                               werks = ls_mchb-werks BINARY SEARCH.
      IF sy-subrc EQ 0.
        ls_stock-safety_stock = ls_marc-eisbe.
        ls_stock-transit_stk = ls_marc-trame.
      ENDIF.
      CLEAR ls_eord.
      READ TABLE lt_eord INTO ls_eord WITH KEY matnr = ls_mchb-matnr
                                               werks = ls_mchb-werks BINARY SEARCH.
      IF sy-subrc EQ 0.
        ls_stock-supplier = ls_eord-lifnr.
        ls_stock-valid_from = ls_eord-vdatu.
        ls_stock-valid_to = ls_eord-bdatu.
      ENDIF.
      CLEAR ls_makt.
      READ TABLE lt_makt INTO ls_makt WITH KEY matnr = ls_mchb-matnr BINARY SEARCH.
      IF sy-subrc EQ 0.
        ls_stock-material_desc = ls_makt-maktx.
      ENDIF.
      CLEAR ls_lfa1.
      READ TABLE lt_lfa1 INTO ls_lfa1 WITH KEY lifnr = ls_stock-supplier BINARY SEARCH.
      IF sy-subrc EQ 0.
        ls_stock-supplier_nme = ls_lfa1-name1.
      ENDIF.
      CLEAR ls_mara.
      READ TABLE lt_mara INTO ls_mara WITH KEY matnr = ls_mchb-matnr BINARY SEARCH.
      IF sy-subrc EQ 0.
        ls_stock-material_type = ls_mara-mtart.
      ENDIF.

      APPEND ls_stock TO lt_batchwise.
      CLEAR ls_mchb.
    ENDLOOP.
    IF sy-subrc NE 0.
      IF lw_marc-eisbe IS NOT INITIAL.
        CLEAR ls_stock.
        ls_stock-material = lw_marc-matnr.
        ls_stock-plant = lw_marc-werks.
        CLEAR ls_marc.
        READ TABLE lt_marc INTO ls_marc WITH KEY matnr = lw_marc-matnr
                                                 werks = lw_marc-werks BINARY SEARCH.
        IF sy-subrc EQ 0.
          ls_stock-safety_stock = ls_marc-eisbe.
          ls_stock-transit_stk = ls_marc-trame.
        ENDIF.
        CLEAR ls_eord.
        READ TABLE lt_eord INTO ls_eord WITH KEY matnr = lw_marc-matnr
                                                 werks = lw_marc-werks BINARY SEARCH.
        IF sy-subrc EQ 0.
          ls_stock-supplier = ls_eord-lifnr.
          ls_stock-valid_from = ls_eord-vdatu.
          ls_stock-valid_to = ls_eord-bdatu.
        ENDIF.
        CLEAR ls_makt.
        READ TABLE lt_makt INTO ls_makt WITH KEY matnr = lw_marc-matnr BINARY SEARCH.
        IF sy-subrc EQ 0.
          ls_stock-material_desc = ls_makt-maktx.
        ENDIF.
        CLEAR ls_lfa1.
        READ TABLE lt_lfa1 INTO ls_lfa1 WITH KEY lifnr = ls_stock-supplier BINARY SEARCH.
        IF sy-subrc EQ 0.
          ls_stock-supplier_nme = ls_lfa1-name1.
        ENDIF.
        CLEAR ls_mara.
        READ TABLE lt_mara INTO ls_mara WITH KEY matnr = ls_mchb-matnr BINARY SEARCH.
        IF sy-subrc EQ 0.
          ls_stock-material_type = ls_mara-mtart.
        ENDIF.

        APPEND ls_stock TO lt_batchwise.
      ELSE.
        CONTINUE.
      ENDIF.
    ENDIF.
  ENDLOOP.
  DELETE lt_batchwise WHERE unrestricted_stk = 0 AND safety_stock = 0.
  IF plant = '1401'.
*    DELETE lt_batchwise WHERE st_loc = '0004' AND safety_stock EQ 0.
    LOOP AT lt_batchwise ASSIGNING FIELD-SYMBOL(<fs_batchwise>) WHERE st_loc = '0004'.
      CLEAR: <fs_batchwise>-unrestricted_stk,<fs_batchwise>-unrestrict_value.
    ENDLOOP.
  ENDIF.
ENDFUNCTION.
