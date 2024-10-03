FUNCTION zget_customer_bill_for_dms.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(BEGDA) TYPE  BEGDA
*"     VALUE(ENDDA) TYPE  ENDDA
*"     VALUE(CUST_NO) TYPE  KUNNR_TTY OPTIONAL
*"  TABLES
*"      IT_VBRK STRUCTURE  ZSTR_BILL_DMS OPTIONAL
*"      IT_VBRP STRUCTURE  ZSTR_BILL_ITEM_DMS OPTIONAL
*"      IT_BTNO STRUCTURE  ZSTR_LIPS_BNO_DMS OPTIONAL
*"      IT_CUSTOMER STRUCTURE  KUNNR_STY OPTIONAL
*"----------------------------------------------------------------------

  TYPES : BEGIN OF ty_knvv ,
            kunnr TYPE kunnr,
          END OF ty_knvv.

  TYPES : BEGIN OF ty_pa0001 ,
            pernr TYPE pa0001-pernr,
            ename TYPE pa0001-ename,
          END OF ty_pa0001.

  TYPES : BEGIN OF ty_konv ,
            knumv TYPE konv-knumv,
            kposn TYPE konv-kposn,
            stunr TYPE konv-stunr,
            zaehk TYPE konv-zaehk,
            kschl TYPE konv-kschl,
            kwert TYPE konv-kwert,
            kbetr TYPE konv-kbetr,
          END OF ty_konv.

*Start of Changes Ramakrishnan for Batch No
  TYPES: BEGIN OF ty_lips,
           vbeln TYPE vbeln_vl,
           posnr TYPE posnr_vl,
           matnr TYPE matnr,
           werks TYPE werks_d,
           charg TYPE charg_d,
           lfimg TYPE lfimg,
           meins TYPE meins,
           vrkme TYPE vrkme,
           ntgew TYPE	ntgew_15,
           brgew TYPE	brgew_15,
           gewei TYPE	gewei,
           volum TYPE	volum_15,
           voleh TYPE	voleh,
         END OF ty_lips.

  DATA: it_lips TYPE TABLE OF ty_lips,
        ls_lips TYPE ty_lips.

  DATA: it_delno TYPE TABLE OF zstr_bill_item_dms,
        ls_delno TYPE zstr_bill_item_dms.
  DATA: ls_batno TYPE zstr_lips_bno_dms.

  DATA : it_knvv   TYPE STANDARD TABLE OF ty_knvv,
         it_vbpa   TYPE STANDARD TABLE OF vbpa,
         it_pa0001 TYPE STANDARD TABLE OF ty_pa0001,
         it_t151t  TYPE STANDARD  TABLE OF t151t,
         it_konv   TYPE STANDARD  TABLE OF ty_konv,
         wa_pa0001 TYPE ty_pa0001,
         wa_vbpa   TYPE vbpa,
         wa_knvv   TYPE ty_knvv,
         wa_t151t  TYPE t151t,
         wa_konv   TYPE ty_konv,
         wa_vbrp   TYPE zstr_bill_item_dms,
         lv_date   TYPE sy-datum.

  DATA: lv_vkorg TYPE vkorg,
        lv_vtweg TYPE vtweg.

  DATA: ls_customer TYPE kunnr_sty.

*Defaulting vkorg, vtweg as 1000 and 20 to extract only sheenlac
  lv_vkorg = '1000'.
  lv_vtweg = '20'.

  TYPES: t_kunnr TYPE RANGE OF kunnr.

  SELECT bwkey, bukrs FROM t001k INTO TABLE @DATA(lt_t001k) WHERE bukrs = 'DMS1'.
  IF sy-subrc = 0.
    SELECT kunnr, werks, zcustype FROM kna1
      FOR ALL ENTRIES IN @lt_t001k
      WHERE werks = @lt_t001k-bwkey
      INTO TABLE @DATA(lt_kna1).
    IF sy-subrc = 0.
      IF cust_no[] IS NOT INITIAL.
        DATA(lr_kunnr) = VALUE t_kunnr( FOR ls_kna1 IN cust_no
                           LET s = 'I' o = 'EQ' IN sign = s option = o
                           ( low = |{ ls_kna1-kunnr ALPHA = IN }| ) ).
      ELSE.
        lr_kunnr = VALUE t_kunnr( FOR ls_kna11 IN lt_kna1
                     LET s = 'I' o = 'EQ' IN sign = s option = o
                     ( low = |{ ls_kna11-kunnr ALPHA = IN }| ) ).
      ENDIF.

    ENDIF.
  ENDIF.

*select only customres with Plant assigned in customer master



  IF lr_kunnr[] IS NOT INITIAL.
    TYPES: t_vbeln TYPE RANGE OF vbeln.
*First select only the invoices of type YBBR where e-way bill is completed
    SELECT docno AS vbeln,
           gjahr
      FROM  j_1ig_ewaybill
      INTO TABLE @DATA(lt_vbeln) WHERE bukrs = '1000'
                                   AND doctyp = 'YBBR'
                                   AND erdat BETWEEN @begda AND @endda
                                   AND status = 'A'.

*Sales return
** Selecting all the Sales return based on the Billing Date (VBRK-FKDAT)
*    SELECT vbeln, gjahr   FROM  vbrk
*       WHERE  vkorg EQ @lv_vkorg AND vtweg EQ @lv_vtweg AND fkart = 'YBRE' AND fkdat BETWEEN @begda AND @endda
*      APPENDING TABLE @lt_vbeln.
*
** Selecting all the Sales return based on the Entry Date also (VBRK-FKDAT)
** for scenarios where BIlling date is past but entry was made only recently
*    SELECT vbeln, gjahr   FROM  vbrk
*       WHERE  vkorg EQ @lv_vkorg AND vtweg EQ @lv_vtweg AND fkart = 'YBRE' AND erdat BETWEEN @begda AND @endda
*      APPENDING TABLE @lt_vbeln.

** Select the invoice numbers based on the above selection to check for Customer financing
*    SELECT invoiceno AS vbeln
*      FROM zsd_sf_cust_inv  INTO TABLE @DATA(lt_sf_cust)
*      FOR ALL ENTRIES IN @lt_vbeln
*      WHERE bukrs = '1000' AND invoiceno = @lt_vbeln-vbeln AND gjahr = @lt_vbeln-gjahr.
*    IF sy-subrc = 0.
*      DATA(lr_sf_cust) = VALUE t_vbeln( FOR ls_sf_cust IN lt_sf_cust
*                             LET s = 'I'
*                                 o = 'EQ'
*                             IN sign   = s
*                                option = o
*                           ( low = |{ ls_sf_cust-vbeln ALPHA = IN }| )
*                           ).
*      DELETE lt_vbeln WHERE vbeln IN lr_sf_cust.
*    ENDIF.

* Select the Discounted and pre-approved status entrieds from Customer financing table
    SELECT invoiceno AS vbeln
      FROM zsd_sf_cust_inv
      WHERE bukrs = '1000'
        AND custno in @lr_kunnr
        AND status IN ('16','20')
        AND inv_approvedon BETWEEN @begda AND @endda
      APPENDING TABLE @lt_vbeln.
    " End Add lines by Puratchi 06 Oct 2023

* Select the additional invoices added to the exception table for GRN without e-way bill and CF discounting
    SELECT vbeln  FROM  zsd_inv_customer
       WHERE  kunnr in @lr_kunnr
          AND status EQ '10'
          AND ctreated BETWEEN @begda AND @endda
        APPENDING TABLE @lt_vbeln.

    SORT lt_vbeln BY vbeln.
    DELETE ADJACENT DUPLICATES FROM lt_vbeln COMPARING vbeln.

* get the invoices where GRN is completed in SAP .
    SELECT vbeln FROM zdms_grn_header
      INTO TABLE @DATA(ltg_vbeln)
      FOR ALL ENTRIES IN @it_vbrk
      WHERE DISTRIBUTOR in @lr_kunnr and vbeln = @it_vbrk-vbeln.
    IF sy-subrc = 0.
      DATA(lrg_vbeln)  = VALUE t_vbeln( FOR ls_vbeln1 IN ltg_vbeln
                          LET s = 'I' o = 'EQ' IN sign = s option = o
                          ( low = |{ ls_vbeln1-vbeln ALPHA = IN }| ) ).
*delete the invoices where the GRN has been already completed
      DELETE lt_vbeln WHERE vbeln IN lrg_vbeln.
    ENDIF.

*select GRN Completed for legacy invoices where GRN was already completed in the old
*dms system
    SELECT vbeln FROM zdms_legacy_grn
      INTO TABLE @DATA(ltg_vbeln_leg)
      FOR ALL ENTRIES IN @it_vbrk
      WHERE distributor in @lr_kunnr
        and vbeln = @it_vbrk-vbeln.
    IF sy-subrc = 0.
      DATA(lrg_vbeln_leg)  = VALUE t_vbeln( FOR ls_vbeln_leg IN ltg_vbeln_leg
                               LET s = 'I' o = 'EQ' IN sign = s option = o
                               ( low = |{ ls_vbeln_leg-vbeln ALPHA = IN }| ) ).
*delete the invoices where the GRN has been already completed in the old dms system
*      DELETE it_vbrk WHERE vbeln IN lrg_vbeln_leg.
      DELETE lt_vbeln WHERE vbeln IN lrg_vbeln_leg.
    ENDIF.

*enter all the final invoice numbers into the final selection range
    DATA(lr_vbeln)  = VALUE t_vbeln( FOR ls_vbeln IN lt_vbeln
                        LET s = 'I' o = 'EQ' IN sign = s option = o
                        ( low = |{ ls_vbeln-vbeln ALPHA = IN }| ) ).
*

    CHECK lt_vbeln IS NOT INITIAL.
*based on all the vbeln select all the data from VBRK and exclude Cancelled invoices
    SELECT vbeln fkdat fkart vkorg vtweg spart kunag netwr mwsbk bukrs gjahr knumv xblnr AS orig_inv
      FROM vbrk INTO CORRESPONDING FIELDS OF TABLE it_vbrk
     WHERE vbeln IN lr_vbeln
       AND vkorg EQ lv_vkorg
       AND vtweg EQ lv_vtweg
       AND bukrs = '1000'
       AND kunag IN lr_kunnr
       AND fksto NE abap_true.

  ENDIF.

*keep only records of customers marked as distributors
  DELETE it_vbrk WHERE kunag NOT IN lr_kunnr.

  SORT it_vbrk   ASCENDING  BY vbeln.

* get billing details
  IF it_vbrk[] IS NOT INITIAL.
    SELECT vbeln posnr matnr arktx fkimg vrkme netwr mwsbp
           werks meins aubel aupos vgbel
      FROM vbrp INTO CORRESPONDING FIELDS OF TABLE it_vbrp FOR ALL ENTRIES IN it_vbrk
     WHERE vbeln EQ it_vbrk-vbeln.
    IF sy-subrc = 0.
* to get the batch no's for each material as per invoice
      it_delno[] = it_vbrp[].
      SORT it_delno BY vgbel.
      DELETE ADJACENT DUPLICATES FROM it_delno COMPARING vgbel.

      SELECT vbeln posnr matnr werks charg lfimg meins vrkme ntgew brgew gewei volum voleh
        FROM lips INTO TABLE it_lips
        FOR ALL ENTRIES IN it_delno
        WHERE vbeln EQ it_delno-vgbel AND charg NE ''.

      IF sy-subrc EQ 0.
        SORT it_lips BY vbeln.
      ENDIF.
    ENDIF.

* for getting the pricing conditions
    SELECT *
      FROM prcd_elements INTO CORRESPONDING FIELDS OF TABLE it_konv  "KONV
      FOR ALL ENTRIES IN it_vbrk
          WHERE knumv EQ it_vbrk-knumv AND
                kschl  IN ('JOCG','JOSG','JOIG','JTC1') AND
                kinak EQ '' .

* to select the invoice nos and status for the Financing customers
    SELECT invoicekey, status, custno, invoiceno, gjahr
      FROM zsd_sf_cust_inv INTO TABLE @DATA(lt_sf_cust_t)
      FOR ALL ENTRIES IN @it_vbrk
      WHERE bukrs     = @it_vbrk-bukrs
        AND custno    = @it_vbrk-kunag
        AND invoiceno = @it_vbrk-vbeln.

* Select records from exception approval table
    SELECT *
      FROM zsd_inv_customer INTO TABLE @DATA(lt_inv_cust_t)
      FOR ALL ENTRIES IN @it_vbrk
      WHERE bukrs = @it_vbrk-bukrs
        AND vbeln = @it_vbrk-vbeln.


* select the invoices where the e-way bill is completed
    SELECT docno AS vbeln, gjahr,erdat
      FROM  j_1ig_ewaybill
      INTO TABLE @DATA(lt_ewaybill)
      FOR ALL ENTRIES IN @it_vbrk
      WHERE bukrs = '1000'
        AND doctyp = 'YBBR'
        AND docno = @it_vbrk-vbeln
        AND status = 'A'.
  ENDIF.

  SORT it_vbrp   ASCENDING  BY vbeln posnr.

  LOOP AT it_vbrk .

    CLEAR ls_customer.
    ls_customer-kunnr = it_vbrk-kunag.
    APPEND ls_customer TO it_customer.

** sales order no
    READ TABLE it_vbrp INTO wa_vbrp WITH  KEY vbeln = it_vbrk-vbeln BINARY SEARCH.
    IF sy-subrc = 0.
      it_vbrk-aubel  = wa_vbrp-aubel .
    ENDIF.

    it_vbrk-dmbtr = it_vbrk-netwr + it_vbrk-mwsbk.

    READ TABLE lt_kna1 INTO DATA(ls_kna1_t) WITH KEY kunnr = it_vbrk-kunag.
    IF sy-subrc = 0.
      it_vbrk-werks_dist = ls_kna1_t-werks.
      it_vbrk-zcustype = ls_kna1_t-zcustype.
    ENDIF.

*default do not allow GRN
    it_vbrk-z_grn_allow = 'N'.

*if the document type is YBRE - Sales returns then set it by default as 'Y'
    IF it_vbrk-fkart = 'YBRE'.
      it_vbrk-z_grn_allow = 'Y'.
      it_vbrk-remarks = 'Sales Return Invoice'.
    ELSE.
      CLEAR it_vbrk-orig_inv.

*if eway billis created set is as 'Y'
      READ TABLE lt_ewaybill INTO DATA(ls_eway) WITH KEY vbeln = it_vbrk-vbeln .  "gjahr = it_vbrk-gjahr
      IF sy-subrc = 0.
        it_vbrk-z_grn_allow = 'Y'.
        it_vbrk-remarks = 'Eway Bill Generated'.
        it_vbrk-ewaybill_date = ls_eway-erdat.
      ENDIF.
      CLEAR : ls_eway.

* set the GRN Allowed status based on data in the customer financing table
      READ TABLE lt_sf_cust_t INTO DATA(ls_sf_cust_t) WITH KEY custno = it_vbrk-kunag invoiceno = it_vbrk-vbeln gjahr = it_vbrk-gjahr.
      IF sy-subrc = 0.
        IF ls_sf_cust_t-status = '16' OR ls_sf_cust_t-status = '20'.
          it_vbrk-z_grn_allow = 'Y'.
          it_vbrk-remarks = 'Invoice Discounted'.
        ELSE.
          it_vbrk-z_grn_allow = 'N'.
          it_vbrk-remarks = 'Pending Invoice Discounting'.
        ENDIF.
      ENDIF.

* if it is part of the exceptional table also set it as 'Y'
      READ TABLE lt_inv_cust_t WITH KEY vbeln = it_vbrk-vbeln gjahr = it_vbrk-gjahr TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        it_vbrk-z_grn_allow = 'Y'.
        it_vbrk-remarks = 'Exception Approval for GRN'.
      ENDIF.
    ENDIF.

    MODIFY it_vbrk TRANSPORTING dmbtr aubel werks_dist zcustype z_grn_allow remarks ewaybill_date.
  ENDLOOP.


  SORT it_vbrk ASCENDING BY vbeln.
  SORT it_konv ASCENDING BY knumv kposn kschl.

  LOOP AT it_vbrp.

    CLEAR it_vbrk.
    READ TABLE it_vbrk WITH  KEY vbeln = it_vbrp-vbeln BINARY SEARCH.
    IF sy-subrc  = 0.
      READ TABLE it_konv INTO wa_konv WITH  KEY knumv = it_vbrk-knumv
                                                kposn = it_vbrp-posnr
                                                kschl = 'JOCG' BINARY SEARCH .
      IF sy-subrc  = 0.
        it_vbrp-cgst     = wa_konv-kwert .
        it_vbrp-cgst_per = wa_konv-kbetr.  " / 10.
      ENDIF.
      READ TABLE it_konv INTO wa_konv WITH  KEY knumv = it_vbrk-knumv
                                                kposn = it_vbrp-posnr
                                                kschl = 'JOSG' BINARY SEARCH.
      IF sy-subrc  = 0.
        it_vbrp-sgst     = wa_konv-kwert .
        it_vbrp-sgst_per = wa_konv-kbetr.  " / 10.
      ENDIF.
      READ TABLE it_konv INTO wa_konv WITH  KEY knumv = it_vbrk-knumv
                                                kposn = it_vbrp-posnr
                                                kschl = 'JOIG' BINARY SEARCH.
      IF sy-subrc  = 0.
        it_vbrp-igst     = wa_konv-kwert .
        it_vbrp-igst_per = wa_konv-kbetr. " / 10.
      ENDIF.
      READ TABLE it_konv INTO wa_konv WITH  KEY knumv = it_vbrk-knumv
                                                kposn = it_vbrp-posnr
                                                kschl = 'JTC1' BINARY SEARCH.
      IF sy-subrc  = 0.
        it_vbrp-jtc1    = wa_konv-kwert .
      ENDIF.
    ENDIF.
    MODIFY it_vbrp TRANSPORTING igst cgst sgst jtc1 igst_per cgst_per sgst_per.

*Based on the delivery no and material, Get all the batch no and input in the table
    LOOP AT it_lips INTO ls_lips WHERE vbeln = it_vbrp-vgbel AND matnr = it_vbrp-matnr.
      CLEAR ls_batno.
      ls_batno-vbeln   = it_vbrp-vbeln.
      ls_batno-vbeln_d = ls_lips-vbeln.
      ls_batno-posnr   = it_vbrp-posnr.
      ls_batno-matnr   = ls_lips-matnr.
      ls_batno-werks   = ls_lips-werks.
      ls_batno-charg   = ls_lips-charg.
      ls_batno-lfimg   = ls_lips-lfimg.
      ls_batno-meins   = ls_lips-meins.
      ls_batno-vrkme   = ls_lips-vrkme.
      ls_batno-ntgew   = ls_lips-ntgew.
      ls_batno-brgew   = ls_lips-brgew.
      ls_batno-gewei   = ls_lips-gewei.
      ls_batno-volum   = ls_lips-volum.
      ls_batno-voleh   = ls_lips-voleh.

      APPEND ls_batno TO it_btno.
    ENDLOOP.
  ENDLOOP.

  SORT it_customer BY kunnr.
  DELETE ADJACENT DUPLICATES FROM it_customer COMPARING kunnr.

ENDFUNCTION.
