FUNCTION zkpmg_audit_extract.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(FROM_DATE) TYPE  DATUM
*"     VALUE(TO_DATE) TYPE  DATUM
*"  TABLES
*"      LT_QUAN_PRICE STRUCTURE  ZSTR_KPMG_DATA
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
           cpudt_mkpf TYPE budat,
           erfme      TYPE erfme,
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
           aedat TYPE erdat,
         END OF ty_ekko,
         BEGIN OF ty_ekpo,
           ebeln TYPE ebeln,
           ebelp TYPE ebelp,
           txz01 TYPE txz01,
           matnr TYPE matnr,
           werks TYPE ewerk,
           menge TYPE bstmg,
           meins TYPE bstme,
           netwr TYPE bwert,
           netpr TYPE bprei,
         END OF ty_ekpo,
         BEGIN OF ty_vbrp,
           vbeln TYPE vbeln_vf,
           fkimg TYPE fkimg,
           netwr TYPE netwr_fp,
           aubel TYPE vbeln_va,
           matnr TYPE matnr,
           charg TYPE charg_d,
           mwsbp TYPE mwsbp,
         END OF ty_vbrp,
         BEGIN OF ty_t001W,
           werks TYPE werks_d,
           name1 TYPE name1,
         END OF ty_t001W.

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
  DATA: lt_t001W TYPE TABLE OF ty_t001W,
        ls_t001W TYPE ty_t001W.

  TYPES: tr_matnr TYPE RANGE OF mara-matnr,
         ty_matnr TYPE LINE OF tr_matnr.


  DATA: lr_matnr TYPE tr_matnr,
        ls_matnr TYPE ty_matnr.

  DATA: ls_mat_range TYPE mat_range.

  DATA: ls_quan_price TYPE zstr_kpmg_data.

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

*other table selection for desc
*matgroup desc
  SELECT * FROM t023t INTO TABLE @DATA(lt_matgrp) WHERE spras = @sy-langu.

* Material type
  SELECT * FROM t134t INTO TABLE @DATA(lt_mattype) WHERE spras = @sy-langu.

* Material Desc
  SELECT * FROM makt INTO TABLE @DATA(lt_matdesc) WHERE spras = @sy-langu.

* Plant name
  SELECT werks name1 FROM t001w INTO TABLE lt_t001W.

* Select ZNB only from Plants '1003' and '1005'
  REFRESH: lt_mseg.
  SELECT mblnr line_id parent_id bwart matnr werks charg lifnr shkzg waers
         dmbtr menge ebeln ebelp
         budat_mkpf cpudt_mkpf erfme FROM mseg
                    INTO TABLE lt_mseg
                    WHERE bwart = '101'
                    AND ( werks EQ '1003' OR werks = '1005')
                    AND shkzg EQ 'S'
                    AND ( budat_mkpf BETWEEN from_date AND to_date ).


*Select also from other plants other than 1003 and 1005
  SELECT mblnr line_id parent_id bwart matnr werks charg lifnr shkzg waers
         dmbtr menge ebeln ebelp
         budat_mkpf cpudt_mkpf erfme FROM mseg
                    APPENDING TABLE lt_mseg
                    WHERE bwart = '101'
                    AND  werks NOT IN ( '1003','1005' )
                    AND shkzg EQ 'S'
                    AND ( budat_mkpf BETWEEN from_date AND to_date ).

  IF lt_mseg[] IS NOT INITIAL.

    REFRESH: lt_ekko.
    SELECT ebeln bsart waers wkurs aedat
      FROM ekko
      INTO TABLE lt_ekko
      FOR ALL ENTRIES IN lt_mseg
      WHERE ebeln EQ lt_mseg-ebeln.
    IF sy-subrc EQ 0.
      SORT lt_ekko[] BY ebeln.
    ENDIF.

    REFRESH: lt_ekpo.
    SELECT ebeln
           ebelp
           txz01
           matnr
           werks
           menge
           meins
           netwr
           netpr FROM ekpo
                 INTO TABLE lt_ekpo
                 FOR ALL ENTRIES IN lt_mseg
                 WHERE ebeln EQ lt_mseg-ebeln
                 AND ebelp EQ lt_mseg-ebelp.
    IF sy-subrc EQ 0.
      SORT lt_ekpo[] BY ebeln matnr.
    ENDIF.


    SELECT lifnr,name1,ktokk FROM lfa1 INTO TABLE @DATA(lt_lfa1)
                       FOR ALL ENTRIES IN @lt_mseg
                       WHERE lifnr EQ @lt_mseg-lifnr.
    IF sy-subrc = 0.
      SORT lt_lfa1 BY lifnr.
    ENDIF.


    SELECT matnr,mtart,matkl FROM mara INTO TABLE @DATA(lt_mara)
                       FOR ALL ENTRIES IN @lt_mseg
                       WHERE matnr EQ @lt_mseg-matnr.
    IF sy-subrc = 0.
      SORT lt_mara BY matnr.
    ENDIF.

  ENDIF.


  REFRESH: lt_quan_price.
  LOOP AT lt_mseg INTO ls_mseg.
    ls_quan_price-grn_no = ls_mseg-mblnr.
    ls_quan_price-plant = ls_mseg-werks.
    ls_quan_price-movement_type = ls_mseg-bwart.
    ls_quan_price-material = ls_mseg-matnr.
    ls_quan_price-po_number = ls_mseg-ebeln.
    ls_quan_price-grn_unit = ls_mseg-erfme.

    CLEAR ls_ekko.
    READ TABLE lt_ekko INTO ls_ekko WITH KEY ebeln = ls_quan_price-po_number BINARY SEARCH.
    IF sy-subrc EQ 0.
      ls_quan_price-po_doc_type = ls_ekko-bsart.
      ls_quan_price-currency = ls_ekko-waers.
      ls_quan_price-po_date = ls_ekko-aedat.
    ENDIF.

*for plant
    IF ls_quan_price-plant = '1003' OR ls_quan_price-plant = '1005'.

    ELSE.
*other units show only service PO
      IF ls_quan_price-po_doc_type NE 'ZSR'.
        CONTINUE.
      ENDIF.
    ENDIF.

*remove unit 2 ZNB entries
    IF ls_quan_price-plant = '1002'.
      IF ls_quan_price-po_doc_type EQ 'ZNB'.
        CONTINUE.
      ENDIF.
    ENDIF.

* Take only Material and Service PO details
    IF ls_quan_price-po_doc_type = 'ZNB' OR ls_quan_price-po_doc_type = 'ZSR'.
    ELSE.
      CONTINUE.
    ENDIF.

    READ TABLE lt_lfa1 INTO DATA(ls_lfa1) WITH KEY lifnr = ls_mseg-lifnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      ls_quan_price-supplier_nme = ls_lfa1-name1.
      IF ls_quan_price-po_doc_type = 'ZSR'.
        IF ls_lfa1-ktokk = 'YBTR'.
        ELSE.
          CONTINUE.
        ENDIF.
      ENDIF.

    ENDIF.

    ls_quan_price-grn_price = ls_mseg-dmbtr.
    ls_quan_price-grn_quan = ls_mseg-menge.


    ls_quan_price-posting_date = ls_mseg-budat_mkpf.
    ls_quan_price-grn_date = ls_mseg-cpudt_mkpf.
    ls_quan_price-supplier = ls_mseg-lifnr.


*for NB PO search based on Matnr
    IF ls_mseg-ebeln IS NOT INITIAL AND ls_mseg-matnr IS NOT INITIAL.
      CLEAR: ls_ekpo.
      READ TABLE lt_ekpo INTO ls_ekpo WITH KEY ebeln = ls_mseg-ebeln
                                               ebelp = ls_mseg-ebelp
                                               matnr = ls_mseg-matnr
                                               werks = ls_mseg-werks.

      IF sy-subrc = 0.
        ls_quan_price-po_quan = ls_ekpo-menge.
        ls_quan_price-po_price = ls_ekpo-netwr.
        ls_quan_price-po_unit_price = ls_ekpo-netpr.
        ls_quan_price-po_unit = ls_ekpo-meins.
        ls_quan_price-po_lineitem = ls_ekpo-ebelp.
      ENDIF.
    ELSE.
      CLEAR: ls_ekpo.
      READ TABLE lt_ekpo INTO ls_ekpo WITH KEY ebeln = ls_mseg-ebeln
                                               ebelp = ls_mseg-ebelp
                                               werks = ls_mseg-werks.

      IF sy-subrc = 0.
        ls_quan_price-po_quan = ls_ekpo-menge.
        ls_quan_price-po_price = ls_ekpo-netwr.
        ls_quan_price-po_unit_price = ls_ekpo-netpr.
        ls_quan_price-po_unit = ls_ekpo-meins.
        ls_quan_price-material_name = ls_ekpo-txz01.
        ls_quan_price-po_lineitem = ls_ekpo-ebelp.
      ENDIF.
    ENDIF.

    IF ls_mseg-matnr IS NOT INITIAL.
      READ TABLE lt_mara INTO DATA(ls_mara) WITH KEY matnr = ls_mseg-matnr.
      IF sy-subrc = 0.
        ls_quan_price-material_grp = ls_mara-matkl.
        ls_quan_price-material_type = ls_mara-mtart.
        ls_quan_price-material_name       = VALUE #( lt_matdesc[ matnr = ls_mara-matnr ]-maktx OPTIONAL ).
        ls_quan_price-material_type_desc  = VALUE #( lt_mattype[ mtart = ls_mara-mtart ]-mtbez OPTIONAL ).
        ls_quan_price-material_grp_desc   = VALUE #( lt_matgrp[ matkl = ls_mara-matkl ]-wgbez OPTIONAL ).
      ENDIF.
    ELSE.

    ENDIF.

    ls_quan_price-plant_name = VALUE #( lt_t001W[ werks = ls_mseg-werks ]-name1 OPTIONAL ).


    APPEND ls_quan_price TO lt_quan_price.
    CLEAR: ls_quan_price, ls_mseg.
  ENDLOOP.

  SORT lt_quan_price by plant po_doc_type po_date po_number po_lineitem.


ENDFUNCTION.
