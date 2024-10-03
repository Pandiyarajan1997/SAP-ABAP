FUNCTION zbapi_percentage_cogs.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(FROM_DATE) TYPE  DATUM
*"     VALUE(TO_DATE) TYPE  DATUM
*"  TABLES
*"      LT_FINAL STRUCTURE  ZSTR_GM_VALUE
*"----------------------------------------------------------------------
***& ------------------------------------------------------------------
*  & Created_on : 24/06/2022
*  & Created_by : Samsudeen m
*  & Reference by : Sanjib Kumar and Praveen kumar
*  & Description : Material Price based on Region
*  & TR NO: DEVK931818
**-------------------------------------------------------------------------
*** Changed on : 12.07.2022
*** Added by : Samsudeen M
*** Adding Customer type on output table
  " TR NO : DEVK931873
*---------------------------------------------------------------------------
  TYPES: BEGIN OF ty_finaldet,          " Structure for Submit program output
           knumv       TYPE vbrk-knumv,
           fkdat       TYPE vbrk-fkdat,
           fkart       TYPE vbrk-fkart,
           bukrs       TYPE vbrk-bukrs,
           erdat       TYPE vbrp-erdat,
           erzet       TYPE vbrp-erzet,
           vkbur       TYPE vbrp-vkbur,
           bezei       TYPE tvkbt-bezei,
           spart       TYPE vbrk-spart,
           vtext       TYPE tspat-vtext,
           vbeln       TYPE vbrp-vbeln,
           lvbeln      TYPE lips-vbeln,
           order_num   TYPE i,
           charg       TYPE lips-charg,
           sort_key(1) TYPE c,
           sort_crit   TYPE posnr,
           candoc(1)   TYPE c,
           posnr       TYPE vbrp-posnr,
           matnr       TYPE vbrp-matnr,
           arktx       TYPE vbrp-arktx,
*   CHARG TYPE LIPS-CHARG,
           fkimg       TYPE vbrp-fkimg,
           volum       TYPE vbrp-volum,
           voleh       TYPE vbrp-voleh,
           m_volum     TYPE mara-volum,
           t_volum     TYPE mara-volum,
           vrkme       TYPE vbrp-vrkme,
           kursk       TYPE vbrp-kursk,
           vnetwr      TYPE vbrp-netwr,
           werks1      TYPE vbrp-werks,
           netwr(10)   TYPE c,
           kunag       TYPE vbrk-kunag,
           name1       TYPE kna1-name1,
           kunrg       TYPE vbrk-kunrg,
           name2       TYPE kna1-name1,
           sfakn       TYPE vbrk-sfakn,
           lfimg       TYPE lips-lfimg,
           flag2(2)    TYPE c,
           proc_key(1) TYPE c,
           aubel       TYPE vbrp-aubel,
           aubel1      TYPE vbrp-aubel,
           aupos       TYPE vbrp-aupos,
           erdat1      TYPE vbak-erdat,
           erzet1      TYPE vbak-erzet,
           invoiceno   TYPE ztrip_st-invoiceno,
           uniq1       TYPE ztrip_st-uniq1,
           ttdate      TYPE ztrip_st-tdate,
           ttime       TYPE ztrip_st-time,
           cartons     TYPE ztrip_st-cartons,
           city        TYPE ztrip_st-city,
           pernr       TYPE vbpa-pernr,
           sname       TYPE pa0001-sname,
           vsnmr_v     TYPE vbak-vsnmr_v,
           sm_no       TYPE vbpa-pernr,
           sm_name     TYPE pa0001-sname,
           kposn       TYPE konv-kposn,          "13/10
           kschl       TYPE konv-kschl,
           kbetr       TYPE konv-kbetr,
           kwert       TYPE konv-kwert,
           inbill1     TYPE konv-kwert,
           tot_y004    TYPE konv-kwert,
           tot_y008    TYPE konv-kwert,
         END OF ty_finaldet.

  TYPES: BEGIN OF ty_kna1,  " Structure for getting Customer Details
           kunnr TYPE kunnr,
           name1 TYPE name1_gp,
           regio TYPE regio,
           katr2 TYPE katr2,
           katr3 TYPE katr3,
           katr4 TYPE katr4,
           katr5 TYPE katr5,
         END OF ty_kna1.

  TYPES: BEGIN OF ty_mara, " Material Master Details
           matnr TYPE matnr,
           matkl TYPE matkl,
         END OF ty_mara.

  TYPES: BEGIN OF ty_matgrp, " Material Description table
           matkl TYPE matkl,
           wgbez TYPE wgbez,
         END OF ty_matgrp.

  TYPES: BEGIN OF ty_knvv,   " Structure for deleting Intercompany Customer
           kunnr TYPE kunnr,
           spart TYPE spart,
           kdgrp TYPE kdgrp,
         END OF ty_knvv.

  DATA: gv_sales TYPE netwr.  " Gross Sales
  DATA: gv_per TYPE kwert.

  DATA: lt_kna1 TYPE TABLE OF ty_kna1,
        ls_kna1 TYPE ty_kna1.

  DATA: lt_mara TYPE TABLE OF ty_mara,
        ls_mara TYPE ty_mara.

  DATA: lt_matgrp TYPE TABLE OF ty_matgrp,
        ls_matgrp TYPE ty_matgrp.

  DATA: lt_knvv TYPE TABLE OF ty_knvv,
        ls_knvv TYPE ty_knvv.

  DATA: lt_result TYPE STANDARD TABLE OF zsd_cogs_bapi,
        ls_result TYPE zsd_cogs_bapi.

  DATA: lt_finaldet TYPE TABLE OF ty_finaldet,
        ls_finaldet LIKE LINE OF lt_finaldet.

  DATA: gt_final TYPE STANDARD TABLE OF zstr_gm_value,
        ls_final TYPE zstr_gm_value.

  DATA: lt_seltab TYPE TABLE OF rsparams,
        ls_seltab LIKE LINE OF lt_seltab.

  REFRESH: lt_seltab.
  CLEAR ls_seltab.
  ls_seltab-selname = 'SO_FKDAT'.          " Name of parameter on submitted program
  ls_seltab-kind    = 'S'.
  ls_seltab-sign    = 'I'.
  ls_seltab-option  = 'BT'.
  ls_seltab-low     = from_date.
  ls_seltab-high    = to_date.
  APPEND ls_seltab TO lt_seltab.

  CLEAR ls_seltab.
  ls_seltab-selname = 'SO_VKBU2'.
  ls_seltab-kind    = 'S'.
  ls_seltab-sign    = 'I'.
  ls_seltab-option  = 'CP'.
  ls_seltab-low     = '*'.
  APPEND ls_seltab TO lt_seltab.

  CLEAR ls_seltab.
  ls_seltab-selname = 'P_BP'.
  ls_seltab-kind    = 'P'.
  ls_seltab-sign    = 'I'.
  ls_seltab-option  = 'EQ'.
  ls_seltab-low     = 'X'.
  APPEND ls_seltab TO lt_seltab.

  SUBMIT zsd_rep_invoicecount_cogs     " Cogs Report Output from tcode "ZCOGS"
         WITH SELECTION-TABLE lt_seltab
         AND RETURN.

  REFRESH: lt_finaldet.
  IMPORT it_finaldet TO lt_finaldet FROM MEMORY ID 'CTAB'.


  IF lt_finaldet[] IS NOT INITIAL.
    SORT lt_finaldet[] BY vbeln matnr.
    DELETE ADJACENT DUPLICATES FROM lt_finaldet COMPARING vbeln matnr.

    REFRESH : lt_kna1,lt_mara.
    CLEAR: ls_mara,ls_kna1.

***********Fetching Region Based on Payer *************************
    SELECT kunnr
           name1
           regio
           katr2
           katr3
           katr4
           katr5 FROM kna1
                 INTO TABLE lt_kna1
                 FOR ALL ENTRIES IN lt_finaldet
                 WHERE kunnr = lt_finaldet-kunag.
    IF sy-subrc EQ 0.
      SORT lt_kna1[] BY kunnr.
    ENDIF.

************Fetching Material Group based on matnr ***************
    SELECT matnr
           matkl FROM mara
                 INTO TABLE lt_mara
                 FOR ALL ENTRIES IN lt_finaldet
                 WHERE matnr = lt_finaldet-matnr.

    IF sy-subrc EQ 0.
      SORT lt_mara[] BY matnr.
    ENDIF.

    IF lt_mara[] IS NOT INITIAL.
*******Fetching Material Group Description **************
      SELECT matkl
             wgbez FROM t023t
             INTO TABLE lt_matgrp
             FOR ALL ENTRIES IN lt_mara
             WHERE matkl = lt_mara-matkl.
      IF sy-subrc EQ 0.
        SORT lt_matgrp[] BY matkl.
      ENDIF.

    ENDIF.

*******Fetching Customer Group from KNVV table **********
    SELECT kunnr
           spart
           kdgrp FROM knvv
                 INTO TABLE lt_knvv
                 FOR ALL ENTRIES IN lt_finaldet
                 WHERE kunnr = lt_finaldet-kunag
                 AND spart = lt_finaldet-spart.
    IF sy-subrc EQ 0.
      SORT lt_knvv[] BY kunnr.
    ENDIF.

***********Fetching Customer type*****************
    SELECT * FROM zsd_cogs_bapi
             INTO TABLE lt_result
             FOR ALL ENTRIES IN lt_finaldet
             WHERE spart = lt_finaldet-spart.

  ENDIF.

  LOOP AT lt_finaldet INTO ls_finaldet.
    CLEAR: ls_final.
    ls_final-matnr = ls_finaldet-matnr.
    ls_final-maktx = ls_finaldet-arktx.
    ls_final-kunag = ls_finaldet-kunag.
    ls_final-vtext = ls_finaldet-vtext.
    ls_final-vkbur = ls_finaldet-vkbur.
    ls_final-werks = ls_finaldet-werks1.

    READ TABLE lt_kna1 INTO ls_kna1 WITH KEY kunnr = ls_final-kunag.
    IF sy-subrc EQ 0.
      ls_final-name1 = ls_kna1-name1.
      ls_final-region = ls_kna1-regio.
    ENDIF.

    IF ls_finaldet-spart EQ '30' AND ls_kna1-katr3 IS NOT INITIAL.           " Wood products

      READ TABLE lt_result INTO ls_result WITH KEY spart = ls_finaldet-spart
                                                   katr2 = ''
                                                   katr3 = ls_kna1-katr3.
      IF sy-subrc EQ 0.
        ls_final-customer_type = ls_result-result_field.
      ENDIF.

    ELSEIF ls_finaldet-spart EQ '30' AND ls_kna1-katr3 IS INITIAL .   " Wood products

      READ TABLE lt_result INTO ls_result WITH KEY spart = ls_finaldet-spart
                                                   katr2 = ls_kna1-katr2.
      IF sy-subrc EQ 0.
        ls_final-customer_type = ls_result-result_field.
      ENDIF.


    ELSEIF ls_finaldet-spart EQ '12'.   " General Industries

      READ TABLE lt_result INTO ls_result WITH KEY spart = ls_finaldet-spart
                                                   katr2 = ls_kna1-katr2.
      IF sy-subrc EQ 0.
        ls_final-customer_type = ls_result-result_field.
      ENDIF.

    ELSEIF ls_finaldet-spart EQ '11'.   " B&S Deco Projects

      READ TABLE lt_result INTO ls_result WITH KEY spart = ls_finaldet-spart
                                                   katr2 = ls_kna1-katr2.
      IF sy-subrc EQ 0.
        ls_final-customer_type = ls_result-result_field.
      ENDIF.

    ELSEIF ls_finaldet-spart EQ '20'.   " Service

      READ TABLE lt_result INTO ls_result WITH KEY spart = ls_finaldet-spart
                                                   katr2 = ls_kna1-katr2.
      IF sy-subrc EQ 0.
        ls_final-customer_type = ls_result-result_field.
      ENDIF.

    ELSEIF ls_finaldet-spart EQ '10' .     " General Products

      READ TABLE lt_result INTO ls_result WITH KEY spart = ls_finaldet-spart
                                                   katr2 = ls_kna1-katr2.
      IF sy-subrc EQ 0.
        ls_final-customer_type = ls_result-result_field.
      ENDIF.

    ELSEIF ls_finaldet-spart EQ '25'.   " high sales

      READ TABLE lt_result INTO ls_result WITH KEY spart = ls_finaldet-spart
                                                   katr2 = ls_kna1-katr2.
      IF sy-subrc EQ 0.
        ls_final-customer_type = ls_result-result_field.
      ENDIF.


    ELSEIF ls_finaldet-spart EQ '35' AND ls_kna1-katr4 IS NOT INITIAL.   " Decorative Products

      READ TABLE lt_result INTO ls_result WITH KEY spart = ls_finaldet-spart
                                                   katr2 = ''
                                                   katr4 = ls_kna1-katr4.
      IF sy-subrc EQ 0.
        ls_final-customer_type = ls_result-result_field.
      ENDIF.

    ELSEIF ls_finaldet-spart EQ '35' AND ls_kna1-katr4 IS  INITIAL.  " Decorative Products

      READ TABLE lt_result INTO ls_result WITH KEY spart = ls_finaldet-spart
                                                   katr2 = ls_kna1-katr2.
      IF sy-subrc EQ 0.
        ls_final-customer_type = ls_result-result_field.
      ENDIF.


    ELSEIF ls_finaldet-spart EQ '40'.   " Home care Products

      READ TABLE lt_result INTO ls_result WITH KEY spart = ls_finaldet-spart
                                                    katr2 = ls_kna1-katr2.
      IF sy-subrc EQ 0.
        ls_final-customer_type = ls_result-result_field.
      ENDIF.

    ELSEIF ls_finaldet-spart EQ '45'.   " Protective Coating

      READ TABLE lt_result INTO ls_result WITH KEY spart = ls_finaldet-spart
                                                    katr2 = ls_kna1-katr2.
      IF sy-subrc EQ 0.
        ls_final-customer_type = ls_result-result_field.
      ENDIF.


    ELSEIF ls_finaldet-spart EQ '50' AND ls_kna1-katr5 IS NOT INITIAL.    " Auto finishes

      READ TABLE lt_result INTO ls_result WITH KEY spart = ls_finaldet-spart
                                                   katr2 = ''
                                                   katr5 = ls_kna1-katr5.
      IF sy-subrc EQ 0.
        ls_final-customer_type = ls_result-result_field.
      ENDIF.

    ELSEIF ls_finaldet-spart EQ '50' AND ls_kna1-katr5 IS INITIAL. " Auto finishes

      READ TABLE lt_result INTO ls_result WITH KEY spart = ls_finaldet-spart
                                                  katr2 = ls_kna1-katr2.
      IF sy-subrc EQ 0.
        ls_final-customer_type = ls_result-result_field.
      ENDIF.

    ELSEIF ls_finaldet-spart EQ '55'.  " multibrand shop

      READ TABLE lt_result INTO ls_result WITH KEY spart = ls_finaldet-spart
                                                     katr2 = ls_kna1-katr2.
      IF sy-subrc EQ 0.
        ls_final-customer_type = ls_result-result_field.
      ENDIF.

    ELSEIF ls_finaldet-spart EQ '60'.     " Industrial Products

      READ TABLE lt_result INTO ls_result WITH KEY spart = ls_finaldet-spart
                                                     katr2 = ls_kna1-katr2.
      IF sy-subrc EQ 0.
        ls_final-customer_type = ls_result-result_field.
      ENDIF.

    ELSEIF ls_finaldet-spart EQ '65' .     " Global Colour Centre

      READ TABLE lt_result INTO ls_result WITH KEY spart = ls_finaldet-spart
                                                     katr2 = ls_kna1-katr2.
      IF sy-subrc EQ 0.
        ls_final-customer_type = ls_result-result_field.
      ENDIF.

    ELSEIF ls_finaldet-spart EQ '70' .     " SheenWorld

      READ TABLE lt_result INTO ls_result WITH KEY spart = ls_finaldet-spart
                                                     katr2 = ls_kna1-katr2.
      IF sy-subrc EQ 0.
        ls_final-customer_type = ls_result-result_field.
      ENDIF.

    ELSEIF ls_finaldet-spart EQ '75'.     " Abrasive Products

      READ TABLE lt_result INTO ls_result WITH KEY spart = ls_finaldet-spart
                                                     katr2 = ls_kna1-katr2.
      IF sy-subrc EQ 0.
        ls_final-customer_type = ls_result-result_field.
      ENDIF.

    ELSEIF ls_finaldet-spart EQ '80' .     " Hardware fittings

      READ TABLE lt_result INTO ls_result WITH KEY spart = ls_finaldet-spart
                                                     katr2 = ls_kna1-katr2.
      IF sy-subrc EQ 0.
        ls_final-customer_type = ls_result-result_field.
      ENDIF.

    ELSEIF ls_finaldet-spart EQ '85'.     " Sri Lanka

      READ TABLE lt_result INTO ls_result WITH KEY spart = ls_finaldet-spart
                                                     katr2 = ls_kna1-katr2.
      IF sy-subrc EQ 0.
        ls_final-customer_type = ls_result-result_field.
      ENDIF.


    ELSEIF ls_finaldet-spart EQ '90'.    " Sheenlac Norooo

      READ TABLE lt_result INTO ls_result WITH KEY katr2 = ls_kna1-katr2.
      IF sy-subrc EQ 0.
        ls_final-customer_type = ls_result-result_field.
      ENDIF.

    ELSEIF ls_finaldet-spart EQ '95'.    " Precoated Metal

      READ TABLE lt_result INTO ls_result WITH KEY katr2 = ls_kna1-katr2.
      IF sy-subrc EQ 0.
        ls_final-customer_type = ls_result-result_field.
      ENDIF.

    ENDIF.




    READ TABLE lt_mara INTO ls_mara WITH KEY matnr = ls_finaldet-matnr.
    IF sy-subrc EQ 0.
      ls_final-matkl = ls_mara-matkl.
    ENDIF.

    READ TABLE lt_matgrp INTO ls_matgrp WITH KEY matkl = ls_mara-matkl.
    IF sy-subrc EQ 0.
      ls_final-matkl_desc = ls_matgrp-wgbez.
    ENDIF.

    READ TABLE lt_knvv INTO ls_knvv WITH KEY kunnr = ls_finaldet-kunag.
    IF sy-subrc EQ 0.
      ls_final-kdgrp = ls_knvv-kdgrp.
    ENDIF.

    CLEAR gv_sales.
    gv_sales = ls_finaldet-vnetwr + ls_finaldet-inbill1 + ls_finaldet-tot_y004.
    ls_final-gross_sales = ( gv_sales - ls_finaldet-tot_y008 ).
    ls_final-gm_value = ( ls_final-gross_sales - ls_finaldet-kwert ).

    CLEAR gv_per.
    gv_per = ( ls_final-gm_value / ls_final-gross_sales   ).

    ls_final-final_gm = gv_per * 100.


    APPEND ls_final TO gt_final.

  ENDLOOP.

  IF gt_final[] IS NOT INITIAL.
    DELETE ADJACENT DUPLICATES FROM gt_final COMPARING matnr kunag region final_gm.
    DELETE gt_final WHERE kdgrp = '04'.
    APPEND LINES OF gt_final TO lt_final.
  ENDIF.

ENDFUNCTION.
