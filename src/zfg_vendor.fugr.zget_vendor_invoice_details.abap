FUNCTION zget_vendor_invoice_details.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(FROM_DATE) TYPE  DATUM OPTIONAL
*"     VALUE(TO_DATE) TYPE  DATUM OPTIONAL
*"     VALUE(VENDOR) TYPE  LIFNR_TTY OPTIONAL
*"     VALUE(COMPANY) TYPE  BUKRS OPTIONAL
*"  TABLES
*"      IT_VEN_INVOICE STRUCTURE  ZSTR_MIRO_INV
*"  EXCEPTIONS
*"      ENTER_GREATER_DATE
*"----------------------------------------------------------------------
**----------------------------------------------------------------------
  "Created on: 28.10.2022
  "Created by: Samsudeen M
  "Reference by: Suresh B.V and Nandhagopal(CFO)
  "Description: Vendor Invoice Details for Payment from MIS
*------------------------------------------------------------------------
  TYPES: BEGIN OF ty_bsik,
           bukrs TYPE bukrs,
           lifnr TYPE lifnr,
           gjahr TYPE gjahr,
           belnr TYPE belnr_d,
           budat TYPE bldat,
           bldat TYPE bldat,
           blart TYPE blart,
           waers TYPE waers,
           xblnr TYPE xblnr1,
           mwskz TYPE mwskz,
           dmbtr TYPE dmbtr,
           zterm TYPE dzterm,
           zbd1t TYPE dzbd1t,
           zfbdt TYPE dzfbdt,
           bstat TYPE bstat_d,
           shkzg TYPE shkzg,
           sgtxt TYPE sgtxt,
           zlspr TYPE dzlspr,
           umskz TYPE umskz,
         END OF ty_bsik.
  TYPES: BEGIN OF ty_bkpf,
           bukrs TYPE bukrs,
           belnr TYPE belnr_d,
           gjahr TYPE gjahr,
           blart TYPE blart,
           xblnr TYPE xblnr1,
           awkey TYPE awkey,
         END OF ty_bkpf.

  TYPES: BEGIN OF ty_awkey,
           belnr TYPE re_belnr,
           bukrs TYPE bukrs,
           gjahr TYPE gjahr,
         END OF ty_awkey.

  DATA: gt_awkey TYPE TABLE OF ty_awkey,
        gs_awkey TYPE ty_awkey.

** Internal Table Declaration **
  DATA: gt_bsik  TYPE TABLE OF ty_bsik,
        gs_bsik  TYPE ty_bsik,
        gt_bsik1 TYPE TABLE OF ty_bsik,
        gt_bsik2 TYPE TABLE OF ty_bsik,
        gs_bsik1 TYPE ty_bsik.
  DATA: gt_bsik_nt TYPE STANDARD TABLE OF bsik.

  DATA: gt_bkpf TYPE TABLE OF ty_bkpf,
        gs_bkpf TYPE ty_bkpf.

  DATA: lv_date TYPE datum.
  DATA: lv_debit_note TYPE char30.
  DATA: lv_debit_price TYPE dmbtr_cs.
  DATA: lt_bukrs TYPE TABLE OF /lsierp/ranges_bukrs.
  DATA: lt_ventyp TYPE TABLE OF /lsierp/ranges_bukrs.
  DATA: ls_rfpos TYPE rfposxext.

  DATA: gs_ven_invoice TYPE zstr_miro_inv.
  DATA: lv_count TYPE i.

  TYPES: t_lifnr TYPE RANGE OF lifnr.
  IF vendor[] IS NOT INITIAL.
    DATA(lr_lifnr) = VALUE t_lifnr( FOR ls_lif1 IN vendor
                       LET s = 'I'
                              o = 'EQ'
                          IN sign   = s
                             option = o
                        ( low = |{ ls_lif1-lifnr ALPHA = IN }| ) ).
  ENDIF.

** Variable for Company Codes which has to go to MIS **
  IF company IS INITIAL.
    SELECT * FROM tvarvc INTO TABLE @DATA(lt_tvarvc)
                         WHERE name = 'BAPI_OPEN_INVOICE_BUKRS'
                         AND type = 'S'.
    LOOP AT lt_tvarvc INTO DATA(ls_tvarvc).
      DATA(ls_bukrs) = VALUE /lsierp/ranges_bukrs( sign = 'I'
                                                   option = 'EQ'
                                                   low = ls_tvarvc-low ).

      APPEND ls_bukrs TO lt_bukrs.
    ENDLOOP.
  ELSE.
    ls_bukrs = VALUE /lsierp/ranges_bukrs( sign = 'I'
                                                 option = 'EQ'
                                                 low = company ).

    APPEND ls_bukrs TO lt_bukrs.
  ENDIF.

*Date Variable from which date the data has to go to MIS*****
  SELECT SINGLE * FROM tvarvc INTO @DATA(gs_tvarvc) WHERE name = 'ZVENDOR_INV_DATE'
                              AND type = 'S'.

  IF gs_tvarvc-low IS INITIAL.
    gs_tvarvc-low = sy-datum.
  ENDIF.

*  DATA(str1) = gs_tvarvc-low+0(4).
*
*  DATA(str2) = from_date+0(4).

*if from date is initial then get the variable from tvarvc and send
  IF from_date IS INITIAL.
    from_date = gs_tvarvc-low.
  ELSE.
    IF from_date < gs_tvarvc-low.
      RAISE enter_greater_date.
    ENDIF.
  ENDIF.

*if end date is initial set it as high end date
  IF to_date IS INITIAL .
    to_date = '99991231'.
  ENDIF.

  REFRESH gt_bsik.
  SELECT bukrs lifnr gjahr belnr budat bldat blart waers xblnr mwskz
         dmbtr zterm zbd1t zfbdt bstat shkzg sgtxt zlspr
         umskz FROM bsik
              INTO TABLE gt_bsik
              WHERE bukrs IN lt_bukrs
                AND lifnr IN lr_lifnr
              AND ( bldat BETWEEN from_date AND to_date )
              AND bstat NOT IN ( 'V' , 'W' , 'Z' ).
  IF sy-subrc EQ 0.
    SORT gt_bsik[] BY bukrs lifnr gjahr belnr.
  ENDIF.

  DELETE gt_bsik[] WHERE ( zlspr EQ 'B' AND zlspr EQ 'A' ). "on 18.04.2023


  gt_bsik1[] = gt_bsik[].
*  DELETE gt_bsik[] WHERE blart = 'KG' AND shkzg EQ 'S'.
  DELETE gt_bsik1 WHERE shkzg EQ 'H'.

  APPEND LINES OF gt_bsik[] TO gt_bsik2[].
  APPEND LINES OF gt_bsik1[] TO gt_bsik2[].

  IF gt_bsik[] IS NOT INITIAL .
** It is for What Kind of vendor Details Has to go to MIS  ***
    SELECT * FROM tvarvc INTO TABLE @DATA(lt_tvarvc1)
             WHERE name = 'BAPI_OPEN_INVOICE_VENTYP'
             AND type = 'S'.
    LOOP AT lt_tvarvc1 INTO DATA(ls_tvarvc1).
      DATA(ls_ventyp) = VALUE /lsierp/ranges_bukrs( sign = 'I'
                                                    option = 'EQ'
                                                    low = ls_tvarvc1-low ).
      APPEND ls_ventyp TO lt_ventyp.
    ENDLOOP.
** Vendors BP number Selection ***
    SELECT supplier, businesspartner FROM ibpsupplier
                                     INTO TABLE @DATA(lt_venbp)
                                     FOR ALL ENTRIES IN @gt_bsik
                                     WHERE supplier = @gt_bsik-lifnr.
    IF sy-subrc EQ 0.
      SORT lt_venbp[] BY supplier.
*** Vendors Grouping Feature ***
      SELECT partner, group_feature FROM bp001
                                    INTO TABLE @DATA(lt_bp001).
      IF sy-subrc EQ 0.
        SORT lt_bp001[] BY partner.
      ENDIF.
    ENDIF.

** Vendor Name **
    SELECT * FROM lfa1 INTO TABLE @DATA(gt_lfa1).
    IF sy-subrc EQ 0.
      SORT gt_lfa1[] BY lifnr.
    ENDIF.
** Select bank Details From LFBK**
    SELECT * FROM lfbk INTO TABLE @DATA(gt_lfbk).
    IF sy-subrc EQ 0.
      SORT gt_lfbk[] BY lifnr.
    ENDIF.
*** payment Block text **
    SELECT * FROM t008t INTO TABLE @DATA(lt_payblk_txt)
             WHERE spras EQ @sy-langu.
    IF sy-subrc EQ 0.
      SORT lt_payblk_txt[] BY zahls.
    ENDIF.

    SELECT * FROM t074t INTO TABLE @DATA(lt_t074t) WHERE spras = @sy-langu AND koart = 'K'.
    IF sy-subrc = 0.
      SORT lt_t074t BY shbkz.
    ENDIF.

*** select invoice payment ****
    SELECT * FROM bsik INTO TABLE gt_bsik_nt
                       FOR ALL ENTRIES IN gt_bsik
                       WHERE bukrs = gt_bsik-bukrs
                       AND lifnr = gt_bsik-lifnr
                       AND belnr = gt_bsik-belnr
                       AND gjahr = gt_bsik-gjahr.
    IF sy-subrc = 0.
      SORT gt_bsik_nt[] BY bukrs lifnr gjahr belnr.
    ENDIF.
  ENDIF.
  "added by Samsudeen M on 27.04.2023 Vendor type exclusion
  SELECT * FROM tvarvc INTO TABLE @DATA(lt_ven_excl) WHERE name = 'BAPI_VENDOR_TYPE_EXCLUDE'
                                                     AND type = 'S'.
  IF sy-subrc = 0.

  ENDIF.

  LOOP AT gt_bsik INTO gs_bsik.

    CLEAR: gs_ven_invoice,ls_rfpos.
    gs_ven_invoice-vendor = gs_bsik-lifnr.

    READ TABLE lt_venbp INTO DATA(ls_venbp) WITH KEY supplier = gs_bsik-lifnr BINARY SEARCH.
    IF sy-subrc EQ 0.

      READ TABLE lt_bp001 INTO DATA(ls_bp001) WITH KEY partner = ls_venbp-businesspartner BINARY SEARCH.
      IF sy-subrc = 0.
        "Vendor Exclusion Checks implemented on 27.04.2023 by Samsudeen M
        READ TABLE lt_ven_excl INTO DATA(ls_ven_excl) WITH KEY low = ls_bp001-group_feature.
        IF sy-subrc NE 0.

          READ TABLE gt_lfa1 INTO DATA(gs_lfa1) WITH KEY lifnr = gs_bsik-lifnr BINARY SEARCH.
          IF sy-subrc EQ 0.

*if the vendor is also a customer. ie sometimed a vendor code will be created for customers
* in those cases those documents as not required
            IF gs_lfa1-kunnr IS NOT INITIAL.
              CONTINUE.
            ENDIF.

*           if amy deletion indicator is set then remove the entry
            IF gs_lfa1-nodel = 'X' OR gs_lfa1-loevm = 'X' OR gs_lfa1-sperz = 'X' OR gs_lfa1-sperr = 'X'.
              CONTINUE.
            ENDIF.

            gs_ven_invoice-vendor_name = gs_lfa1-name1.
            gs_ven_invoice-ven_gst_num = gs_lfa1-stcd3.
            gs_ven_invoice-inv_refno = gs_bsik-belnr.
            gs_ven_invoice-bukrs = gs_bsik-bukrs.
            gs_ven_invoice-inv_date = gs_bsik-bldat.
            gs_ven_invoice-pay_block = gs_bsik-zlspr.
            READ TABLE lt_payblk_txt INTO DATA(ls_payblk_txt) WITH KEY zahls = gs_bsik-zlspr BINARY SEARCH.
            IF sy-subrc EQ 0.
              gs_ven_invoice-pay_blockdes = ls_payblk_txt-textl.
            ENDIF.

            "Added by samsudeen on 18.04.2023
            IF gs_lfa1-ktokk = 'YBEV'.
              gs_ven_invoice-pymnt_type = 'Employee Vendor'.

*if employee vendor any advances paid should not be sent as open items
              IF gs_bsik-shkzg = 'S'.
                CONTINUE.
              ENDIF.
            ELSE.
              READ TABLE lt_bp001 INTO ls_bp001 WITH KEY partner = ls_venbp-businesspartner BINARY SEARCH.
              IF sy-subrc = 0.
                DATA(lv_type) = VALUE #( lt_ventyp[ low = ls_bp001-group_feature ] OPTIONAL ).
                IF lv_type IS NOT INITIAL.
                  gs_ven_invoice-pymnt_type = 'SCM Vendor'.
                ELSE.
                  gs_ven_invoice-pymnt_type = 'NON SCM Vendor'.
                ENDIF.

              ENDIF.
            ENDIF.
          ENDIF.

          gs_ven_invoice-waers = gs_bsik-waers.
          gs_ven_invoice-gross_inv_amt = gs_bsik-dmbtr.
          READ TABLE gt_lfbk INTO DATA(gs_lfbk) WITH KEY lifnr = gs_bsik-lifnr BINARY SEARCH.
          IF sy-subrc EQ 0.
            gs_ven_invoice-ven_bank_acc = gs_lfbk-bankn.
            gs_ven_invoice-ven_bank_ifsc = gs_lfbk-bankl.
          ENDIF.
          gs_ven_invoice-dc_ind = gs_bsik-shkzg.
          gs_ven_invoice-sgtxt = gs_bsik-sgtxt.
          gs_ven_invoice-ref_document = gs_bsik-xblnr.
          CLEAR: lv_debit_price,lv_debit_note.
          IF gs_bsik-xblnr IS NOT INITIAL.
            LOOP AT gt_bsik1 INTO DATA(ls_bsik) WHERE xblnr = gs_bsik-xblnr AND lifnr = gs_bsik-lifnr.
              lv_debit_price = ( lv_debit_price + ls_bsik-dmbtr ).
              CONCATENATE lv_debit_note ',' ls_bsik-belnr   INTO lv_debit_note SEPARATED BY space.
            ENDLOOP.
            SHIFT lv_debit_note LEFT DELETING LEADING ''.
            SHIFT lv_debit_note LEFT DELETING LEADING ','.
            CONDENSE lv_debit_note NO-GAPS.
          ENDIF.
          gs_ven_invoice-debit_note = lv_debit_note.
          gs_ven_invoice-debit_value = lv_debit_price.
          gs_ven_invoice-net_due_days = gs_bsik-zbd1t.
*        gs_ven_invoice-net_due_date = ( gs_bsik-zfbdt + gs_bsik-zbd1t ).
          gs_ven_invoice-tax_code = gs_bsik-mwskz.
          gs_ven_invoice-doc_typ = gs_bsik-blart.

*These are advances entered through F-47
          IF gs_bsik-blart = 'KA'.
            CONTINUE.
          ENDIF.

* geeting details of the Special Gl type mainly used for advance payment
          gs_ven_invoice-umskz = gs_bsik-umskz.
          IF gs_bsik-umskz IS NOT INITIAL.
            READ TABLE lt_t074t INTO DATA(ls_t074t) WITH KEY shbkz = gs_bsik-umskz.
            IF sy-subrc = 0.
              gs_ven_invoice-ltext = ls_t074t-ltext.
            ENDIF.
          ENDIF.

*** select company code ***
** Process for calculating net due date ****
          SELECT SINGLE * FROM t001 INTO @DATA(ls_t001w)
                          WHERE bukrs = @gs_bsik-bukrs.
          DATA(ls_net_cal) = VALUE #( gt_bsik_nt[ bukrs = gs_bsik-bukrs lifnr = gs_bsik-lifnr belnr = gs_bsik-belnr
                                               gjahr = gs_bsik-gjahr ] OPTIONAL ).
          MOVE-CORRESPONDING ls_net_cal TO ls_rfpos.
          CALL FUNCTION 'ITEM_DERIVE_FIELDS'
            EXPORTING
              s_t001    = ls_t001w
*             S_BSEGP   =
              key_date  = sy-datum
*             XOPVW     = 'X'
*             X_ICONS_ONLY       = ' '
*             I_KALSM   =
            CHANGING
              s_item    = ls_rfpos
            EXCEPTIONS
              bad_input = 1
              OTHERS    = 2.
          IF sy-subrc <> 0.
          ENDIF.
*Added by Samsudeen M 0n 18.08.2023 for holidays calculation
          SELECT SINGLE hdate FROM zfi_due_dates INTO @DATA(l_holiday)
            WHERE hdate = @ls_rfpos-faedt.
          IF sy-subrc NE 0.
            gs_ven_invoice-net_due_date = ls_rfpos-faedt.
            gs_ven_invoice-net_due_date = ls_rfpos-faedt.
            APPEND gs_ven_invoice TO it_ven_invoice.
            CONTINUE.
          ELSE.
            DATA(lv_ddate_tmp) = CONV bldat( ls_rfpos-faedt + 1 ).
            DO .
              SELECT SINGLE hdate FROM zfi_due_dates INTO l_holiday
              WHERE hdate = lv_ddate_tmp.
              IF sy-subrc NE 0.
                gs_ven_invoice-net_due_date = lv_ddate_tmp.
                EXIT.
              ELSE.
                lv_ddate_tmp = lv_ddate_tmp + 1.
              ENDIF.
            ENDDO.
          ENDIF.
** END of changes ON 18.08.2023
          gs_ven_invoice-net_due_date = lv_ddate_tmp.

          APPEND gs_ven_invoice TO it_ven_invoice.
        ENDIF.
      ENDIF.
      "End of Changes on 27.04.2023
    ENDIF.
    CLEAR: gs_bsik,ls_rfpos.
  ENDLOOP.

  IF it_ven_invoice[] IS NOT INITIAL.
*    DELETE it_ven_invoice[] WHERE dc_ind EQ 'S'.
    DELETE it_ven_invoice[] WHERE doc_typ EQ 'SA' AND pymnt_type = 'Employee Vendor'.
  ENDIF.

ENDFUNCTION.
