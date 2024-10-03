FUNCTION zbapi_inv_eway_details.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(BUKRS) TYPE  BUKRS
*"     VALUE(FROM_DATE) TYPE  ERDAT
*"     VALUE(TO_DATE) TYPE  ERDAT
*"     VALUE(VBELN) TYPE  VBELN OPTIONAL
*"  TABLES
*"      LT_FINAL TYPE  ZTT_EWAYBILL_DETAILS
*"----------------------------------------------------------------------

******** vbrk table structure ***********
  TYPES : BEGIN OF st_vbrk,
            vbeln TYPE vbeln_vf,
            kunRG TYPE kunRG,
            bukrs TYPE bukrs,
            vkorg TYPE vkorg,
            fkdat TYPE fkdat,
            fkart TYPE fkart,
          END OF st_vbrk.

*********** kna1 table structure **************
  TYPES : BEGIN OF st_kna1,
            kunnr TYPE kunnr,
            werks TYPE werks_d,
          END OF st_kna1.

*************  j_1ig_invrefnum table structure ****************
  TYPES : BEGIN OF st_irn,
            docno    TYPE j_1ig_docno,
            bukrs    TYPE bukrs,
            irn      TYPE j_1ig_irn,
            erdat    TYPE erdat,
            doc_type TYPE j_1ig_doctyp,
          END OF st_irn.

************  zdms_invoice_irn table structure **************
  TYPES : BEGIN OF st_irn02,
            bukrs    TYPE bukrs,
            distrb   TYPE kunnr,
            dealer   TYPE kunnr,
            docno    TYPE j_1ig_docno,
            irn      TYPE j_1ig_irn,
            erdat    TYPE erdat,
            doc_type TYPE j_1ig_doctyp,
          END OF st_irn02.

****************** j_1ig_ewaybill table structure ***********
  TYPES : BEGIN OF st_eway,
            docno    TYPE j_1ig_docno,
            bukrs    TYPE bukrs,
            ebillno  TYPE j_1ig_ebillno,
            egen_dat TYPE j_1ig_egendat,
            vdfmdate TYPE j_1ig_vdfmdate,
            vdtodate TYPE j_1ig_vdtodate,
          END OF st_eway.

  DATA : lt_vbrk TYPE TABLE OF st_vbrk.
  DATA : ls_vbrk TYPE st_vbrk.

  DATA : lt_kna1 TYPE TABLE OF st_kna1.
  DATA : ls_kna1 TYPE st_kna1.

  DATA : lt_irn TYPE TABLE OF st_irn.
  DATA : ls_irn TYPE st_irn.

  DATA : lt_irn02 TYPE TABLE OF st_irn02.
  DATA : ls_irn02 TYPE st_irn02.

  DATA : lt_eway TYPE TABLE OF st_eway.
  DATA : ls_eway TYPE st_eway.

*  DATA : lt_final TYPE ztt_ewaybill_details.
  DATA : ls_final TYPE zst_inv_ewaybill_vbrk.

  DATA: ls_vbeln TYPE rvbeln,
        lr_vbeln TYPE RANGE OF vbak-vbeln.

  IF lr_vbeln IS NOT INITIAL.
    ls_vbeln-sign   = 'I'.
    ls_vbeln-option = 'EQ'.
    ls_vbeln-low    = vbeln.
    APPEND ls_vbeln TO lr_vbeln.
  ENDIF.


*data : so_vbeln-TYPE RANGE OF




*  DATA: "ls_date TYPE range_s_dats,
*        r_date type  range_t_dats.
  REFRESH : lt_vbrk, lt_kna1, lt_irn, lt_eway,lr_vbeln.
  CLEAR : ls_vbrk, ls_kna1, ls_irn, ls_eway, ls_vbeln.
*
*  data(ls_date) = VALUE range_s_dats(
*      sign   = 'I'
*      option =  'BT'
*      low    = FROM_DATE
*      high   = to_date
*  ).

*APPEND ls_date to r_date.





  SELECT vbeln kunRG bukrs vkorg fkdat fkart
    FROM vbrk
    INTO TABLE lt_vbrk
    WHERE  vbeln IN lr_vbeln
     AND bukrs EQ bukrs
      AND ( fkdat BETWEEN from_date AND to_date ).


  IF sy-subrc EQ 0.
    SORT lt_vbrk BY vbeln.
  ENDIF.

  IF lt_vbrk IS NOT INITIAL.
    SELECT kunnr werks
      FROM kna1
      INTO TABLE lt_kna1
      WHERE werks NE abap_false.


    IF bukrs = 'DMS1'.
      SELECT bukrs distrb dealer docno irn erdat doc_type
        FROM zdms_invoice_irn
        INTO TABLE lt_irn02
        FOR ALL ENTRIES IN lt_vbrk
        WHERE docno  = lt_vbrk-vbeln.

    ELSE.

      SELECT docno bukrs irn erdat doc_type
        FROM j_1ig_invrefnum
        INTO TABLE lt_irn
        FOR ALL ENTRIES IN lt_vbrk
        WHERE docno EQ lt_vbrk-vbeln.
*      and erdat in r_date
*      AND ( erdat BETWEEN from_date AND to_date )
*      AND bukrs = lt_vbrk-bukrs.
    ENDIF.

    SELECT docno bukrs ebillno egen_dat vdfmdate vdtodate
      FROM j_1ig_ewaybill
      INTO TABLE lt_eway
      FOR ALL ENTRIES IN lt_vbrk
      WHERE docno = lt_vbrk-vbeln.
    CLEAR : ls_final.

  ENDIF.

  LOOP AT lt_vbrk INTO ls_vbrk.
    ls_final-inv_no = ls_vbrk-vbeln.
    ls_final-customer = ls_vbrk-kunrg.
    ls_final-comp_code = ls_vbrk-bukrs.
    ls_final-sales_org = ls_vbrk-vkorg.
    ls_final-billing_dat = ls_vbrk-fkdat.
    ls_final-billing_type = ls_vbrk-fkart.



    IF bukrs = 'DMS1'.

********** DMS details ******************
      CLEAR : ls_irn02.
      READ TABLE lt_irn02 INTO ls_irn02 WITH KEY docno = ls_vbrk-vbeln.
      IF sy-subrc EQ 0 AND ls_irn02-irn IS NOT INITIAL.
        ls_final-irn = 'X'.
        ls_final-inv_ref_no = ls_irn02-irn.
        ls_final-doc_type = ls_irn02-doc_type.
        ls_final-irn_created_on = ls_irn02-erdat.
        ls_vbrk-kunrg           = ls_irn02-distrb.
*      ls_final-creation_date = ls_irn-erdat.
      ENDIF.
    ELSE.

*********** invoice reference number details ***************
      CLEAR : ls_irn.
      READ TABLE lt_irn INTO ls_irn WITH KEY docno = ls_vbrk-vbeln.
      IF sy-subrc EQ 0 AND ls_irn-irn IS NOT INITIAL.
        ls_final-irn = 'X'.
        ls_final-inv_ref_no = ls_irn-irn.
        ls_final-doc_type = ls_irn-doc_type.
        ls_final-irn_created_on = ls_irn-erdat.
*      ls_final-creation_date = ls_irn-erdat.

      ENDIF.
    ENDIF.

*********** Eway bill to document details **********
    CLEAR : ls_eway.
    READ TABLE lt_eway INTO ls_eway WITH KEY docno = ls_vbrk-vbeln.
    IF sy-subrc EQ 0.
      ls_FINAL-ewaybill = 'X'.
      ls_final-ebill_no = ls_eway-ebillno.
      ls_final-egen_dat = ls_eway-egen_dat.
      ls_final-vdfm_date = ls_eway-vdfmdate.
      ls_final-vdto_date = ls_eway-vdtodate.
    ENDIF.

*******************plant details**********
    CLEAR : ls_kna1.
    READ TABLE lt_kna1 INTO ls_kna1 WITH KEY kunnr = ls_vbrk-kunrg.
    IF sy-subrc = 0.
      ls_final-dist_plant = ls_kna1-werks.
    ENDIF.

    APPEND ls_final TO lt_final.
    CLEAR : ls_final.

  ENDLOOP.
ENDFUNCTION.
