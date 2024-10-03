FUNCTION zbapi_info_record_list.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(MATNR) TYPE  MATNR OPTIONAL
*"     VALUE(PLANT) TYPE  WERKS_D
*"     VALUE(BEGDA) TYPE  BEGDA
*"     VALUE(ENDDA) TYPE  ENDDA
*"  TABLES
*"      IT_SOURCE STRUCTURE  ZSTR_SOURCE_LIST
*"----------------------------------------------------------------------
*      Created By:   Samsudeen M
  "    Reference by: praveen Kumar
  "    Description: Getting latest Info Record based on date"
  "    TR No: DEVK931786
***************************************************************************

  TYPES: BEGIN OF ty_marc,     "Structure Material master Plant wise
           matnr TYPE matnr,
           werks TYPE werks_d,
         END OF ty_marc,

         BEGIN OF ty_mara,      " Structure Material master
           matnr TYPE matnr,
           mtart TYPE mtart,
         END OF ty_mara,

         BEGIN OF ty_eina,     "structure For Info Record General Data
           infnr TYPE infnr,
           matnr TYPE matnr,
           lifnr TYPE elifn,
         END OF ty_eina,

         BEGIN OF ty_eine,      "Structure For Info Record Purchasing Data
           infnr TYPE infnr,
           ekorg TYPE ekorg,
           werks TYPE werks_d,
         END OF ty_eine,

         BEGIN OF ty_a017,      "Structure For Info Record valid Date
           kschl TYPE kschl,
           lifnr TYPE elifn,
           matnr TYPE matnr,
           ekorg TYPE ekorg,
           werks TYPE werks_d,
           datab TYPE kodatab,
           datbi TYPE kodatbi,
           knumh TYPE knumh,
         END OF ty_a017,

         BEGIN OF ty_konp,       "Structure For Pricing Condition
           knumh TYPE knumh,
           kopos TYPE kopos,
           kbetr TYPE kbetr_kond,
           kschl TYPE kschl,
         END OF ty_konp,

         BEGIN OF ty_lfa1,        "Structure Vendor Name
           lifnr TYPE elifn,
           name1 TYPE name1_gp,
         END OF ty_lfa1.

*&&&&&*****Internal Table Declaration *************
  DATA: lt_marc TYPE STANDARD TABLE OF ty_marc,
        lt_mara TYPE STANDARD TABLE OF ty_mara,
        lt_makt TYPE STANDARD TABLE OF makt,
        lt_eina TYPE STANDARD TABLE OF ty_eina,
        lt_eine TYPE STANDARD TABLE OF ty_eine,
        lt_a017 TYPE STANDARD TABLE OF ty_a017,
        lt_konp TYPE STANDARD TABLE OF ty_konp,
        lt_lfa1 TYPE STANDARD TABLE OF ty_lfa1.

*&&&******Work Area Declaration ********************
  DATA: ls_marc TYPE ty_marc,
        ls_mara TYPE ty_mara,
        ls_makt TYPE makt,
        ls_eina TYPE ty_eina,
        ls_eine TYPE ty_eine,
        ls_a017 TYPE ty_a017,
        ls_konp TYPE ty_konp,
        ls_lfa1 TYPE ty_lfa1.

***&&& Final Work Area for IT_Source *************
  DATA: ls_source TYPE zstr_source_list.


  REFRESH: lt_mara,lt_makt,lt_marc,lt_eina,lt_a017,lt_lfa1,lt_konp,it_source.

  IF matnr IS NOT INITIAL AND plant IS NOT INITIAL.
********Fetching material based on input plant from marc*********
    SELECT matnr
           werks FROM marc
                 INTO TABLE lt_marc
                 WHERE matnr EQ matnr
                 AND werks EQ plant.

  ELSEIF plant IS NOT INITIAL.
********Fetching material based on input plant from marc*********
    SELECT matnr
           werks FROM marc
                 INTO TABLE lt_marc
                 WHERE werks EQ plant.

  ENDIF.

******Fetching Data From A017 Table Based on Input***********
  IF lt_marc[] IS NOT INITIAL.
    SELECT kschl
           lifnr
           matnr
           ekorg
           werks
           datab
           datbi
           knumh FROM a017
                 INTO TABLE lt_a017
                 FOR ALL ENTRIES IN lt_marc
                 WHERE matnr EQ lt_marc-matnr
                 AND werks EQ lt_marc-werks
                 AND ( datab LE endda )
                 AND ( datbi GE begda ).

    IF sy-subrc EQ 0.
      SORT lt_a017[] BY kschl matnr datab DESCENDING.
      DELETE ADJACENT DUPLICATES FROM lt_a017 COMPARING kschl matnr lifnr.
    ENDIF.
  ENDIF.
  IF lt_a017[] IS NOT INITIAL.
******Fetching Info Record Number From EINA Table***********
    SELECT infnr
           matnr
           lifnr FROM eina
                 INTO TABLE lt_eina
                 FOR ALL ENTRIES IN lt_a017
                 WHERE matnr EQ lt_a017-matnr
                 AND lifnr EQ lt_a017-lifnr.

    IF sy-subrc EQ 0.
      SORT lt_eina[] BY matnr.
    ENDIF.
******Fetching Price FromKONP table  ****************
    SELECT knumh
           kopos
           kbetr
           kschl FROM konp
                 INTO TABLE lt_konp
                 FOR ALL ENTRIES IN lt_a017
                 WHERE knumh EQ lt_a017-knumh.
    IF sy-subrc EQ 0.
      SORT lt_konp[] BY kschl.
    ENDIF.
**********Fetching material Type From Mara table******
    SELECT matnr
           mtart FROM mara
                 INTO TABLE lt_mara
                 FOR ALL ENTRIES IN lt_a017
                 WHERE matnr EQ lt_a017-matnr.
    IF sy-subrc EQ 0.
      SORT lt_mara[] BY matnr.
    ENDIF.
*********Fetching Supplier name From LFA1 table******************************
    SELECT lifnr
           name1 FROM lfa1
                 INTO TABLE lt_lfa1
                 FOR ALL ENTRIES IN lt_a017
                 WHERE lifnr EQ lt_a017-lifnr.
    IF sy-subrc EQ 0.
      SORT lt_lfa1[] BY lifnr.
    ENDIF.
**********Fetching material Description From Makt table******
    SELECT * FROM makt
             INTO TABLE lt_makt
             FOR ALL ENTRIES IN lt_a017
             WHERE matnr EQ lt_a017-matnr
             AND spras EQ sy-langu.
    IF sy-subrc EQ 0.
      SORT lt_makt[] BY matnr.
    ENDIF.
  ENDIF.

**** Variable For Added Cost **************
  DATA: lv_add_cost TYPE kbetr.

  LOOP AT lt_a017 INTO ls_a017.

    CLEAR: ls_source.
    ls_source-matnr = ls_a017-matnr.
    ls_source-lifnr = ls_a017-lifnr.
    ls_source-ekorg = ls_a017-ekorg.
    ls_source-werks = ls_a017-werks.
    ls_source-datab = ls_a017-datab.
    ls_source-datbi = ls_a017-datbi.
    ls_source-knumh = ls_a017-knumh.

    CLEAR ls_eina.
    READ TABLE lt_eina INTO ls_eina WITH KEY matnr = ls_a017-matnr
                                             lifnr = ls_a017-lifnr.
    IF sy-subrc EQ 0.
      ls_source-infnr = ls_eina-infnr.
    ENDIF.

    CLEAR ls_mara.
    READ TABLE lt_mara INTO ls_mara WITH KEY matnr = ls_a017-matnr.
    IF sy-subrc EQ 0.
      ls_source-mtart = ls_mara-mtart.
    ENDIF.

    CLEAR ls_makt.
    READ TABLE lt_makt INTO ls_makt WITH KEY matnr = ls_a017-matnr.
    IF sy-subrc EQ 0.
      ls_source-maktx = ls_makt-maktx.
    ENDIF.

    CLEAR ls_lfa1.
    READ TABLE lt_lfa1 INTO ls_lfa1 WITH KEY lifnr = ls_a017-lifnr.
    IF sy-subrc EQ 0.
      ls_source-name1 = ls_lfa1-name1.
    ENDIF.

    LOOP AT lt_konp INTO ls_konp WHERE knumh = ls_a017-knumh.
      IF sy-subrc EQ 0.
        ls_source-kschl = ls_konp-kschl.
        IF ls_konp-kschl EQ 'P000'.
          ls_source-kbter = ls_konp-kbetr.
        ELSEIF ls_konp-kschl EQ 'FRB1'.
          ls_source-freight = ls_konp-kbetr.
        ENDIF.
      ENDIF.
    ENDLOOP.

    CLEAR: lv_add_cost.
    lv_add_cost = ls_source-kbter + ls_source-freight.
    ls_source-added_cost = lv_add_cost.

    APPEND ls_source TO it_source.
  ENDLOOP.

ENDFUNCTION.
