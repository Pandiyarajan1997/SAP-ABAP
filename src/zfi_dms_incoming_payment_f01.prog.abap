FORM f_document_post .
  DATA: lv_index TYPE sy-tabix,
        lv_lines TYPE sy-tabix.

  DATA: lv_fisyear TYPE bapi0002_4-fiscal_year,
        lv_month   TYPE bapi0002_4-fiscal_period,
        l_return   TYPE bapireturn1.

  DATA l_round_off TYPE i.

  DATA : ls_dms_cdtaken_pre TYPE zdms_cdtaken_pre.

  DATA: ls_flexa TYPE faglflexa.
  DATA: ls_ignlog TYPE zdms_inpy_ignlog.

  DELETE gt_tab WHERE kunnr IS INITIAL.
  CLEAR gs_tab.
* loop all the entries in table control of selection screen
  gs_tab-mark = 'X'.
  gs_tab-kunnr = kunnr.
  gs_tab-wrbtr = wrbtr.
  gs_tab-belnr = belnr.
  APPEND gs_tab TO gt_tab.
  LOOP AT gt_tab INTO gs_tab.
    FREE: gv_sclear, gv_vclear, gv_nclear, gs_bapireturn, gt_lineitems, gt_openitems, gt_openitemsc, gt_amnt, gs_amnt,
         gv_partial, gv_excess, gv_exact, gv_wrbtr, gv_wrbtrc, gv_wrbtro, gt_fcat, gt_cusven, gt_lineitemsv.


    IF chbox = abap_true.  "Process the credit note and F-13 logic only for original online payment

* Credit Note start Process by Puratchi 13.Mar.2024
*get open items for customer for
      PERFORM bapi_ar_acc_getopenitems USING gs_header-bukrs gs_tab-kunnr gs_tab-belnr
                                       CHANGING  gs_bapireturn gt_lineitems .
      IF gt_lineitems[] IS NOT INITIAL.

        PERFORM determine_due_date USING gt_lineitems
                                    CHANGING gt_openitems.
      ENDIF.
      DATA l_total_amt TYPE bapidmbtr.
      DATA l_rem_amt TYPE bapidmbtr.
      DATA l_dis_amt TYPE bapidmbtr.

* to select the rounding off credit and debit entries created manually on 31st march
      SELECT * FROM faglflexa INTO TABLE @DATA(lt_faglflexa) WHERE ryear = '2023'
         AND rldnr = '0L' AND rbukrs = 'DMS1' AND racct = '0042002009' AND rbusa = @gsber AND budat = '20240331'.
      IF sy-subrc = 0.
*do nothing
      ENDIF.

      LOOP AT gt_openitems INTO DATA(lw_openitem) WHERE db_cr_ind = 'H'. " H-Credit amount from Sub-Dealers

*        IF lw_openitem-doc_type = 'VG'.
** sales return amount should not be part of the credit note calculation
*          IF lw_openitem-pstng_date <= '20240331'. "if Sales return before GO-live then consider
*            CLEAR ls_IGNLOG.
*            ls_IGNLOG-bukrs         = lw_openitem-comp_code.
*            ls_IGNLOG-distributor   = distb.
*            ls_IGNLOG-kunnr         = gs_tab-kunnr.
*            ls_IGNLOG-xblnr         = xblnr.
*            ls_IGNLOG-reference_id  = refid.
*            ls_IGNLOG-budat         = budat.
*            ls_IGNLOG-remarks       = 'SRet doc Before 31.03.2024 Included For Credit Note Calculation'.
*            ls_IGNLOG-gsber         = gsber.
*            ls_IGNLOG-doc_type      = lw_openitem-doc_type.
*            ls_IGNLOG-doc_no        = lw_openitem-doc_no.
*            ls_IGNLOG-fisc_year     = lw_openitem-fisc_year.
*            ls_IGNLOG-pstng_date    = lw_openitem-pstng_date.
*            ls_IGNLOG-db_cr_ind     = lw_openitem-db_cr_ind.
*            ls_IGNLOG-lc_amount     = lw_openitem-lc_amount.
*            MODIFY zdms_inpy_ignlog FROM ls_IGNLOG.
*          ELSE.
*            CLEAR ls_IGNLOG.
*            ls_IGNLOG-bukrs         = lw_openitem-comp_code.
*            ls_IGNLOG-distributor   = distb.
*            ls_IGNLOG-kunnr         = gs_tab-kunnr.
*            ls_IGNLOG-xblnr         = xblnr.
*            ls_IGNLOG-reference_id  = refid.
*            ls_IGNLOG-budat         = budat.
*            ls_IGNLOG-remarks       = 'Sales Return document Excluded From Credit Note Calculation'.
*            ls_IGNLOG-gsber         = gsber.
*            ls_IGNLOG-doc_type      = lw_openitem-doc_type.
*            ls_IGNLOG-doc_no        = lw_openitem-doc_no.
*            ls_IGNLOG-fisc_year     = lw_openitem-fisc_year.
*            ls_IGNLOG-pstng_date    = lw_openitem-pstng_date.
*            ls_IGNLOG-db_cr_ind     = lw_openitem-db_cr_ind.
*            ls_IGNLOG-lc_amount     = lw_openitem-lc_amount.
**          ls_IGNLOG-BILL_DOC  = lw_openitem-BILL_DOC.
*
** check if there is a sales return of this invoice
*            SELECT SINGLE ref_invoiceno, invoice_no, accounting_doc INTO @DATA(lv_sretlog) FROM zsd_retrn_hd_dms
*                                            WHERE accounting_doc = @lw_openitem-doc_no.
*            IF sy-subrc = 0.
*              ls_IGNLOG-bill_doc  = lv_sretlog-ref_invoiceno.
*              ls_IGNLOG-orig_inv = lv_sretlog-invoice_no.
*            ENDIF.
*
*            MODIFY zdms_inpy_ignlog FROM ls_IGNLOG.
*            CONTINUE.
*
*
*          ENDIF.
*
*        ENDIF.

        IF lw_openitem-doc_type = 'RV'.
* Invoice Cancellation amount should not be part of the credit note calculation
          CLEAR ls_ignlog.
          ls_ignlog-bukrs         = lw_openitem-comp_code.
          ls_ignlog-distributor   = distb.
          ls_ignlog-kunnr         = gs_tab-kunnr.
          ls_ignlog-xblnr         = xblnr.
          ls_ignlog-reference_id  = refid.
          ls_ignlog-budat         = budat.
          ls_ignlog-remarks       = 'INV CNL Credit Amount Excluded From Credit Note Calculation'.
          ls_ignlog-gsber         = gsber.
          ls_ignlog-doc_type      = lw_openitem-doc_type.
          ls_ignlog-doc_no        = lw_openitem-doc_no.
          ls_ignlog-fisc_year     = lw_openitem-fisc_year.
          ls_ignlog-pstng_date    = lw_openitem-pstng_date.
          ls_ignlog-db_cr_ind     = lw_openitem-db_cr_ind.
          ls_ignlog-lc_amount     = lw_openitem-lc_amount.
          ls_ignlog-bill_doc  = lw_openitem-bill_doc.

          MODIFY zdms_inpy_ignlog FROM ls_ignlog.
          CONTINUE.
        ENDIF.

*round up all the credit items. This is due to the Migration roundoff error
        CLEAR l_round_off.
        l_round_off = ceil( lw_openitem-lc_amount ).
        lw_openitem-lc_amount = l_round_off.


        l_total_amt = l_total_amt + lw_openitem-lc_amount.
      ENDLOOP.
      CLEAR ls_ignlog.
      CLEAR l_dis_amt.
      l_dis_amt = l_total_amt + wrbtr.

      ls_ignlog-bukrs         = 'DMS'. "lw_openitem-comp_code.
      ls_ignlog-distributor   = distb.
      ls_ignlog-kunnr         = gs_tab-kunnr.
      ls_ignlog-xblnr         = xblnr.
      ls_ignlog-reference_id  = refid.
      ls_ignlog-budat         = budat.
      ls_ignlog-remarks       = |CN Amt { l_total_amt } and Pay Amt { wrbtr } - Total { l_dis_amt }|.
      ls_ignlog-gsber         = gsber.
      ls_ignlog-doc_no        = '1234567890'.

      IF budat+4(2) LE 3.
        DATA(lv_fs_yr) = budat(4) - 1.
      ELSE.
        lv_fs_yr = budat(4).
      ENDIF.
      ls_ignlog-fisc_year     = lv_fs_yr.
      ls_ignlog-doc_type      = 'SA'.
      ls_ignlog-pstng_date    = budat.
      ls_ignlog-db_cr_ind     = 'H'.
      ls_ignlog-lc_amount     = l_dis_amt.

      MODIFY zdms_inpy_ignlog FROM ls_ignlog.

      l_total_amt = l_total_amt + wrbtr.

* Add 1 rupee to the credit total so round off missing issues will be resolved
      l_total_amt = l_total_amt + 1.

      LOOP AT gt_openitems INTO lw_openitem WHERE db_cr_ind = 'S'. " S-Debit amount from Sub-Dealers
        IF lw_openitem-doc_type = 'DR'.
* if the debit note is relating to manual round off enetered then round down the value for credit note calculation
          READ TABLE lt_faglflexa TRANSPORTING NO FIELDS WITH KEY ryear = '2023' docnr = lw_openitem-doc_no.
          IF sy-subrc = 0.
            CLEAR l_round_off.
            l_round_off = floor( lw_openitem-lc_amount ).
            lw_openitem-lc_amount = l_round_off.
          ELSE.
            CLEAR l_round_off.
            l_round_off = lw_openitem-lc_amount.
            lw_openitem-lc_amount = l_round_off.
          ENDIF.
        ELSE.
          CLEAR l_round_off.
          l_round_off = lw_openitem-lc_amount.
          lw_openitem-lc_amount = l_round_off.
        ENDIF.


        IF lw_openitem-doc_type = 'RV'.
          SELECT SINGLE vbeln, fksto FROM vbrk INTO @DATA(ls_invcan)
                                        WHERE vbeln = @lw_openitem-bill_doc.
          IF sy-subrc = 0. " AND ls_invcan-fksto = abap_true.
            IF ls_invcan-fksto = abap_true.
*Invoice already cancelled so they should not be considered to check Credit notes
              CLEAR ls_ignlog.
              ls_ignlog-bukrs         = lw_openitem-comp_code.
              ls_ignlog-distributor   = distb.
              ls_ignlog-kunnr         = gs_tab-kunnr.
              ls_ignlog-xblnr         = xblnr.
              ls_ignlog-reference_id  = refid.
              ls_ignlog-budat         = budat.
              ls_ignlog-remarks       = 'Invoice Cancelled Debit Amount Excluded From Credit Note Calculation'.
              ls_ignlog-gsber         = gsber.
              ls_ignlog-doc_type      = lw_openitem-doc_type.
              ls_ignlog-doc_no        = lw_openitem-doc_no.
              ls_ignlog-fisc_year     = lw_openitem-fisc_year.
              ls_ignlog-pstng_date    = lw_openitem-pstng_date.
              ls_ignlog-db_cr_ind     = lw_openitem-db_cr_ind.
              ls_ignlog-lc_amount     = lw_openitem-lc_amount.
              ls_ignlog-bill_doc  = lw_openitem-bill_doc.

              MODIFY zdms_inpy_ignlog FROM ls_ignlog.

              CONTINUE.
            ENDIF.

** check if there is a sales return of this invoice
*            SELECT SINGLE ref_invoiceno,invoice_no, accounting_doc INTO @DATA(lv_Sretinv) FROM zsd_retrn_hd_dms
*                                            WHERE ref_invoiceno = @lw_openitem-bill_doc.
*            IF sy-subrc = 0.
**Original Invoice Sales return already done so removed from credit note calculation
*              CLEAR ls_IGNLOG.
*              ls_IGNLOG-bukrs         = lw_openitem-comp_code.
*              ls_IGNLOG-distributor   = distb.
*              ls_IGNLOG-kunnr         = gs_tab-kunnr.
*              ls_IGNLOG-xblnr         = xblnr.
*              ls_IGNLOG-reference_id  = refid.
*              ls_IGNLOG-budat         = budat.
*              ls_IGNLOG-remarks       = 'Invoice-Sales Returned Excluded From Credit Note Calculation'.
*              ls_IGNLOG-gsber         = gsber.
*              ls_IGNLOG-doc_type      = lw_openitem-doc_type.
*              ls_IGNLOG-doc_no        = lw_openitem-doc_no.
*              ls_IGNLOG-fisc_year     = lw_openitem-fisc_year.
*              ls_IGNLOG-pstng_date    = lw_openitem-pstng_date.
*              ls_IGNLOG-db_cr_ind     = lw_openitem-db_cr_ind.
*              ls_IGNLOG-lc_amount     = lw_openitem-lc_amount.
*              ls_IGNLOG-bill_doc      = lw_openitem-bill_doc.
*              ls_IGNLOG-orig_inv      = lv_Sretinv-invoice_no.
*              ls_IGNLOG-accounting_doc = lv_Sretinv-accounting_doc.
*              MODIFY zdms_inpy_ignlog FROM ls_IGNLOG.
*              CONTINUE.
*            ENDIF.
          ENDIF.
        ENDIF.

        CLEAR gw_crnote_log.
        IF l_total_amt GE lw_openitem-lc_amount.
          l_total_amt = l_total_amt - lw_openitem-lc_amount.
          CASE lw_openitem-doc_type.
            WHEN 'RV'.
              SELECT SINGLE *
                FROM zdms_crnote_log INTO gw_crnote_log WHERE bukrs = lw_openitem-comp_code
                  AND acdno = lw_openitem-doc_no AND acdyr = lw_openitem-fisc_year AND msgtyp = 'S'.
              IF sy-subrc = 0." RV Invoice already processed for Credit Note.
*Already Credit note provided for this invoice
                CLEAR ls_ignlog.
                ls_ignlog-bukrs         = lw_openitem-comp_code.
                ls_ignlog-distributor   = distb.
                ls_ignlog-kunnr         = gs_tab-kunnr.
                ls_ignlog-xblnr         = xblnr.
                ls_ignlog-reference_id  = refid.
                ls_ignlog-budat         = budat.
                ls_ignlog-remarks       = 'CN Already provided for this invoice'.
                ls_ignlog-gsber         = gsber.
                ls_ignlog-doc_type      = lw_openitem-doc_type.
                ls_ignlog-doc_no        = lw_openitem-doc_no.
                ls_ignlog-fisc_year     = lw_openitem-fisc_year.
                ls_ignlog-pstng_date    = lw_openitem-pstng_date.
                ls_ignlog-db_cr_ind     = lw_openitem-db_cr_ind.
                ls_ignlog-lc_amount     = lw_openitem-lc_amount.
                ls_ignlog-bill_doc      = lw_openitem-bill_doc.
                MODIFY zdms_inpy_ignlog FROM ls_ignlog.

                CONTINUE.
              ELSE.
                SELECT SINGLE regio FROM kna1 INTO @DATA(l_region) WHERE kunnr = @distb.
                gw_crnote_log-pdays = budat - lw_openitem-pstng_date.
*Need to add 1 day as invoice date also should be considered
                gw_crnote_log-pdays = gw_crnote_log-pdays + 1.
                SELECT SINGLE
                  a~hkont
                  a~perzt
                  FROM zdms_credit_note AS a
                  INNER JOIN zdms_regio_area AS b ON a~areas = b~areas
                  INTO ( gw_crnote_log-hkont,gw_crnote_log-dgper )
                  WHERE b~regio = l_region
                    AND a~sdays LE gw_crnote_log-pdays
                    AND a~edays GE gw_crnote_log-pdays
                    AND a~endda GE sy-datum.
                IF sy-subrc = 0.
                  gw_crnote_log-bukrs  = lw_openitem-comp_code.
                  gw_crnote_log-refid  = refid.
                  gw_crnote_log-xblnr  = xblnr.
                  gw_crnote_log-distr  = distb.
                  gw_crnote_log-kunnr  = kunnr.
                  gw_crnote_log-gsber  = gsber.
                  gw_crnote_log-invflag = 'P'.
                  gw_crnote_log-acdno  = lw_openitem-doc_no.
                  gw_crnote_log-acdyr  = lw_openitem-fisc_year.
                  gw_crnote_log-indat  = lw_openitem-pstng_date.
                  gw_crnote_log-totamt = lw_openitem-amount.
                  gw_crnote_log-payamt = wrbtr.
                  gw_crnote_log-invno  = lw_openitem-bill_doc.

                  CLEAR gw_crnote_log-inamt.
                  IF lw_openitem-bill_doc IS INITIAL. "If the billing document is initial
*try to get the same from Historical table
                    CLEAR ls_zdms_retail_acnt.
                    SELECT SINGLE * FROM zdms_retail_acnt
                             INTO ls_zdms_retail_acnt
                           WHERE distributor = distb
                             AND retailer = kunnr
                             AND ref_doc = lw_openitem-ref_doc_no
                             AND pos_date = lw_openitem-pstng_date
                             AND doc_no = lw_openitem-doc_no.
                    IF sy-subrc = 0.
                      gw_crnote_log-inamt = ls_zdms_retail_acnt-net_amt.
                      gw_crnote_log-invno = ls_zdms_retail_acnt-invoice.
                    ELSE.
*try to get the same from BSEG for
                      CLEAR: lv_fisyear,lv_month,l_return.
                      CALL FUNCTION 'BAPI_COMPANYCODE_GET_PERIOD'
                        EXPORTING
                          companycodeid = 'DMS1'
                          posting_date  = lw_openitem-pstng_date
                        IMPORTING
                          fiscal_year   = lv_fisyear
                          fiscal_period = lv_month
                          return        = l_return.
                      IF lv_fisyear IS NOT INITIAL.
                        CLEAR ls_flexa.
                        SELECT SINGLE * FROM faglflexa INTO ls_flexa
                          WHERE ryear = lv_fisyear AND docnr = lw_openitem-doc_no
                            AND rldnr = '0L' AND rbukrs = 'DMS1' AND racct = '0031100001'
                            AND rbusa = gsber.
                        IF sy-subrc = 0.
                          gw_crnote_log-inamt = ls_flexa-tsl.

                          IF gw_crnote_log-inamt < 0.
                            gw_crnote_log-inamt = gw_crnote_log-inamt * -1.
                          ENDIF.

                          gw_crnote_log-invno = lw_openitem-doc_no.
                        ENDIF.
                      ENDIF.
                    ENDIF.
                  ELSE.
                    SELECT SINGLE vbeln, netwr, kunag FROM vbrk INTO @DATA(ls_vbrk)
                                              WHERE vbeln = @lw_openitem-bill_doc AND kunag = @kunnr.
                    IF sy-subrc = 0.
                      gw_crnote_log-inamt = ls_vbrk-netwr.
                      gw_crnote_log-invno = lw_openitem-bill_doc.
                    ENDIF.
                  ENDIF.
*                  gw_crnote_log-inamt = lw_openitem-amount.

                  gw_crnote_log-paydt = budat.
                  gw_crnote_log-crdat = sy-datum.
                  gw_crnote_log-crnam  = sy-uname.
*                gw_crnote_log-dgper = l_cr_data-perzt.
                  IF gw_crnote_log-inamt GT 0.
                    IF gw_crnote_log-dgper GT 0.
                      CLEAR l_round_off.
                      l_round_off = gw_crnote_log-inamt.
                      gw_crnote_log-inamt = l_round_off.

*Calculate % based on date
                      gw_crnote_log-dgamt = gw_crnote_log-inamt * ( gw_crnote_log-dgper / 100 ).

                      CLEAR l_round_off.
                      l_round_off = gw_crnote_log-dgamt.
                      gw_crnote_log-dgamt = l_round_off.

                      CLEAR ls_dms_cdtaken_pre.
                      SELECT SINGLE * FROM zdms_cdtaken_pre
                         INTO ls_dms_cdtaken_pre
                        WHERE bukrs  = 'DMS1' AND dist = distb AND dealer = kunnr
                          AND acdno  = lw_openitem-doc_no AND acdyr  = lw_openitem-fisc_year
                          AND acddt  = lw_openitem-pstng_date.
                      IF sy-subrc = 0.
                        gw_crnote_log-dgdoc = ls_dms_cdtaken_pre-dgdoc.
                        gw_crnote_log-dgdyr = ls_dms_cdtaken_pre-dgdyr.
                        gw_crnote_log-msgtyp = 'S'.
                        gw_crnote_log-message = 'Already Credit note provided for this invoice'.
                        MODIFY zdms_crnote_log FROM gw_crnote_log.

*Already Credit note provided for this invoice
                        CLEAR ls_ignlog.
                        ls_ignlog-bukrs         = lw_openitem-comp_code.
                        ls_ignlog-distributor   = distb.
                        ls_ignlog-kunnr         = gs_tab-kunnr.
                        ls_ignlog-xblnr         = xblnr.
                        ls_ignlog-reference_id  = refid.
                        ls_ignlog-budat         = budat.
                        ls_ignlog-remarks       = 'CN Already provided for this invoice'.
                        ls_ignlog-gsber         = gsber.
                        ls_ignlog-doc_type      = lw_openitem-doc_type.
                        ls_ignlog-doc_no        = lw_openitem-doc_no.
                        ls_ignlog-fisc_year     = lw_openitem-fisc_year.
                        ls_ignlog-pstng_date    = lw_openitem-pstng_date.
                        ls_ignlog-db_cr_ind     = lw_openitem-db_cr_ind.
                        ls_ignlog-lc_amount     = lw_openitem-lc_amount.
                        ls_ignlog-bill_doc      = lw_openitem-bill_doc.
                        MODIFY zdms_inpy_ignlog FROM ls_ignlog.
                      ELSE.
                        IF gw_crnote_log-dgamt LE 0.
                          gw_crnote_log-msgtyp = 'S'.
                          gw_crnote_log-message = 'CN Amount is 0'.
                          MODIFY zdms_crnote_log FROM gw_crnote_log.
                        ELSE.
                          gw_crnote_log-crflag = 'X'.
                          APPEND gw_crnote_log TO gt_crnote_log.
                        ENDIF.

*Auto CN for inv. satisfies all condition
                        CLEAR ls_ignlog.
                        ls_ignlog-bukrs         = lw_openitem-comp_code.
                        ls_ignlog-distributor   = distb.
                        ls_ignlog-kunnr         = gs_tab-kunnr.
                        ls_ignlog-xblnr         = xblnr.
                        ls_ignlog-reference_id  = refid.
                        ls_ignlog-budat         = budat.
                        IF gw_crnote_log-dgamt LE 0.
                          ls_ignlog-remarks       = |No ACN for inv CN Amount is 0|.
                        else.
                          ls_ignlog-remarks       = |ACN for inv. { gw_crnote_log-dgper } - { gw_crnote_log-dgamt }|.
                        ENDIF.
                        ls_ignlog-gsber         = gsber.
                        ls_ignlog-doc_type      = lw_openitem-doc_type.
                        ls_ignlog-doc_no        = lw_openitem-doc_no.
                        ls_ignlog-fisc_year     = lw_openitem-fisc_year.
                        ls_ignlog-pstng_date    = lw_openitem-pstng_date.
                        ls_ignlog-db_cr_ind     = lw_openitem-db_cr_ind.
                        ls_ignlog-lc_amount     = lw_openitem-lc_amount.
                        ls_ignlog-bill_doc      = lw_openitem-bill_doc.
                        MODIFY zdms_inpy_ignlog FROM ls_ignlog.

                      ENDIF.
                    ELSE.
                      gw_crnote_log-msgtyp = 'S'.
                      gw_crnote_log-message = 'Credit Note is not generated as the days are exceeded'.
                      MODIFY zdms_crnote_log FROM gw_crnote_log.

*Credit Note is not generated as the days are exceeded
                      CLEAR ls_ignlog.
                      ls_ignlog-bukrs         = lw_openitem-comp_code.
                      ls_ignlog-distributor   = distb.
                      ls_ignlog-kunnr         = gs_tab-kunnr.
                      ls_ignlog-xblnr         = xblnr.
                      ls_ignlog-reference_id  = refid.
                      ls_ignlog-budat         = budat.
                      ls_ignlog-remarks       = 'CN not generated days exceeded'.
                      ls_ignlog-gsber         = gsber.
                      ls_ignlog-doc_type      = lw_openitem-doc_type.
                      ls_ignlog-doc_no        = lw_openitem-doc_no.
                      ls_ignlog-fisc_year     = lw_openitem-fisc_year.
                      ls_ignlog-pstng_date    = lw_openitem-pstng_date.
                      ls_ignlog-db_cr_ind     = lw_openitem-db_cr_ind.
                      ls_ignlog-lc_amount     = lw_openitem-lc_amount.
                      ls_ignlog-bill_doc      = lw_openitem-bill_doc.
                      MODIFY zdms_inpy_ignlog FROM ls_ignlog.
                    ENDIF.
                  ELSE.
                    gs_message-belnr = lw_openitem-doc_no.
                    gs_message-mess_type = 'E'.
                    gs_message-message = 'Invoice Details not found'.
                    APPEND gs_message TO gt_message.
                  ENDIF.

                  CLEAR gw_crnote_log.
                ELSE.
                  gs_message-belnr = lw_openitem-doc_no.
                  gs_message-mess_type = 'E'.
                  CONCATENATE 'DocNo:'
                              lw_openitem-bill_doc
                              'Region is maintained for '
                              gw_crnote_log-pdays
                              'in Table ZDMS_CREDIT_NOTE'
                  INTO gs_message-message  SEPARATED BY space.
                  APPEND gs_message TO gt_message.
                  EXIT.
                ENDIF.
              ENDIF.
            WHEN OTHERS.
          ENDCASE.
        ELSE.
*make the total as 0 so it will not move to next invoice
          IF l_total_amt > 0.
            CLEAR l_rem_amt.
            l_rem_amt = l_total_amt.
          ENDIF.
          CLEAR l_total_amt.
          CLEAR ls_ignlog.
          ls_ignlog-bukrs         = lw_openitem-comp_code.
          ls_ignlog-distributor   = distb.
          ls_ignlog-kunnr         = gs_tab-kunnr.
          ls_ignlog-xblnr         = xblnr.
          ls_ignlog-reference_id  = refid.
          ls_ignlog-budat         = budat.
          ls_ignlog-remarks       = |Invoice Value Greater then Remaining Credit Note Amount { l_rem_amt }|.
          ls_ignlog-gsber         = gsber.
          ls_ignlog-doc_type      = lw_openitem-doc_type.
          ls_ignlog-doc_no        = lw_openitem-doc_no.
          ls_ignlog-fisc_year     = lw_openitem-fisc_year.
          ls_ignlog-pstng_date    = lw_openitem-pstng_date.
          ls_ignlog-db_cr_ind     = lw_openitem-db_cr_ind.
          ls_ignlog-lc_amount     = lw_openitem-lc_amount.
          ls_ignlog-bill_doc      = lw_openitem-bill_doc.
          MODIFY zdms_inpy_ignlog FROM ls_ignlog.
        ENDIF.
*    ENDIF.
      ENDLOOP.
      IF VALUE #( gt_message[ mess_type = 'E' ]-mess_type OPTIONAL ) = 'E'.
        SELECT SINGLE name1 FROM kna1 INTO @DATA(l_name) WHERE kunnr = @kunnr.
        LOOP AT gt_message ASSIGNING FIELD-SYMBOL(<lfs_mess>).
          <lfs_mess>-name1 = l_name.
        ENDLOOP.
        SELECT SINGLE * FROM  zfi_dms_cust_pay
          INTO @DATA(ls_table1)
          WHERE bukrs = @bukrs
          AND kunnr = @kunnr
          AND distributor = @distb
          AND xblnr = @xblnr
          AND reference_id = @refid
          AND status = '10'.
        IF sy-subrc = 0.
          ls_table1-remarks = VALUE #( gt_message[ 1 ]-message OPTIONAL ).
          ls_table1-doc_no = g_doc_no.
          IF VALUE #( gt_message[ mess_type = 'E' ]-mess_type OPTIONAL ) = 'E'.
            ls_table1-status = '30'.
            ls_table1-remarks = VALUE #( gt_message[ mess_type = 'E' ]-message OPTIONAL ).
          ENDIF.
          MODIFY zfi_dms_cust_pay FROM ls_table1.
        ENDIF.
        EXIT .
      ENDIF.
      CLEAR : gs_bapireturn, gt_lineitems, gt_openitems .
    ENDIF.
* Credit Note end Process
*
* first clear records using F.13

    PERFORM f13_bdc CHANGING check.

* process invoice where partials exist and the total of partials match with invoice value.
* Process docs in gt_amnt where net amount is 0. this is to process for root doc with 0 net amount.
    IF gs_tab-belnr IS INITIAL .
* get open items for customer
      PERFORM bapi_ar_acc_getopenitems USING gs_header-bukrs gs_tab-kunnr gs_tab-belnr
                                       CHANGING  gs_bapireturn gt_lineitems .
      IF gt_lineitems[] IS NOT INITIAL.

        PERFORM determine_due_date USING gt_lineitems
                                    CHANGING gt_openitems.
* calculate due dates of each open items and sort so that oldest invoice processed first.
        PERFORM process_documents CHANGING gt_openitems.
* each record in gt_amnt is processed
        DELETE gt_amnt WHERE net_amnt NE 0.
        IF gt_amnt[] IS NOT INITIAL.
          gv_sclear = 'X'.
          CLEAR : gv_vclear, gv_nclear.
          FREE: gt_openitemsc[].
* bdc for f-32
          PERFORM f32_bdc_cn CHANGING check.

        ENDIF.

      ENDIF.
      FREE: gs_bapireturn, gt_lineitems, gt_openitems, gt_openitemsc, gt_amnt, gv_wrbtr, gv_wrbtrc, gv_wrbtro, gt_fcat, gt_cusven, gt_lineitemsv.
    ENDIF.

* F-32 for other credit notes/excess payments/un-addresed partial payment docs.
    IF gs_tab-belnr IS INITIAL .
* get open items for customer
      PERFORM bapi_ar_acc_getopenitems USING gs_header-bukrs gs_tab-kunnr gs_tab-belnr
                                       CHANGING  gs_bapireturn gt_lineitems .
      IF gt_lineitems[] IS NOT INITIAL.

        PERFORM determine_due_date USING gt_lineitems
                                    CHANGING gt_openitems.
* calculate due dates of each open items and sort so that oldest invoice processed first.
        PERFORM process_documents CHANGING gt_openitems.
* each record in gt_amnt is processed
        IF gt_amnt[] IS NOT INITIAL.
* if credit notes exist
          IF NOT gt_openitemsc[] IS INITIAL.
            PERFORM check_creditnotes.
* bdc for f-32
            IF NOT gt_openitemsc[] IS INITIAL.
              gv_nclear = 'X'.
              CLEAR : gv_vclear, gv_sclear.

              PERFORM f32_bdc_cn CHANGING check.

            ENDIF.
          ENDIF.
        ENDIF.

      ENDIF.
      FREE: gs_bapireturn, gt_lineitems, gt_openitems, gt_openitemsc, gt_amnt, gv_wrbtr, gv_wrbtrc, gv_wrbtro, gt_fcat, gt_cusven, gt_lineitemsv.
    ENDIF.

*
* pass amount entered for payment
    gv_wrbtr = gs_tab-wrbtr.
* get open items again because f-32 might have run in the case of credit note scenario.

    PERFORM bapi_ar_acc_getopenitems USING gs_header-bukrs gs_tab-kunnr gs_tab-belnr
                                     CHANGING  gs_bapireturn gt_lineitems .

    IF gt_lineitems[] IS  INITIAL.
      IF gv_wrbtr GT 0.
* post as excess payment by customer
*        PERFORM fb05_bdc_excess CHANGING check.
        PERFORM f_excess_payment CHANGING check.
      ENDIF.
      CONTINUE.
    ENDIF.
* get due dates
    PERFORM determine_due_date USING gt_lineitems
                                CHANGING gt_openitems.
* calculate due dates of each open items and sort so that oldest invoice processed first.
    PERFORM process_documents CHANGING gt_openitems.

    IF gt_amnt[] IS  INITIAL.
      IF gv_wrbtr GT 0.
* post as excess payment by customer

*        PERFORM fb05_bdc_excess CHANGING check.
        PERFORM f_excess_payment CHANGING check.

      ENDIF.
      CONTINUE.
    ENDIF.

    CLEAR : gs_amnt, gv_pwrbtr.
    DESCRIBE TABLE gt_amnt LINES lv_lines.
* process documents

* if Bill to Bill scenario.
    IF gs_tab-belnr IS NOT INITIAL.
      LOOP AT gt_amnt INTO gs_amnt.
        lv_index = sy-tabix.
        IF gv_wrbtr LE 0.
          EXIT.
        ENDIF.
        IF gs_amnt-prev IS NOT INITIAL.
          gs_message-mess_type = 'E'.
          gs_message-message = 'Previous Partial payment done on this Bill'.
          MOVE-CORRESPONDING gs_tab TO gs_message.
          APPEND gs_message TO gt_message.
          EXIT.
        ENDIF.
        IF gv_wrbtr LT gs_amnt-net_amnt OR gv_wrbtr GT gs_amnt-net_amnt.
          gs_message-mess_type = 'E'.
          gs_message-message =  'Amount should be equal to Invoice amount less discount'.
          MOVE-CORRESPONDING gs_tab TO gs_message.
          APPEND gs_message TO gt_message.
          EXIT.
        ENDIF.
        gs_amnt-proc = 'X'.
        MODIFY gt_amnt FROM gs_amnt INDEX lv_index TRANSPORTING proc.
        CLEAR gs_amnt.
      ENDLOOP.

      gv_exact = 'X'.  " here in this casen only one root document is processed

* Non Bill to Bill scenario
    ELSE.                       " Non Bill to Bill scenario
      FREE :  gv_exact, gv_partial, gv_excess.
      LOOP AT gt_amnt INTO gs_amnt.
        lv_index = sy-tabix.

        gs_amnt-proc = 'X'.
        MODIFY gt_amnt FROM gs_amnt INDEX lv_index TRANSPORTING proc.

* check for the required root docs to process updating 'PROC' indicator
* Also check if it is an exact/partial/excess case sceanrio.
        IF gv_wrbtr LT gs_amnt-net_amnt.  " amount not sufficient to knock off complete doc
          gv_wrbtr1 = gs_amnt-net_amnt - gv_wrbtr.
          gv_partial = 'X'.
          EXIT.
        ELSEIF gv_wrbtr EQ gs_amnt-net_amnt.
          gv_exact = 'X'.
          gv_wrbtr = gv_wrbtr - gs_amnt-net_amnt.
          gv_wrbtr1 = 0.
          EXIT.
        ELSE.
          IF lv_lines = lv_index.    " for last item
            gv_wrbtr1 =  gv_wrbtr - gs_amnt-net_amnt .
            gv_excess = 'X'.
          ENDIF.
          gv_wrbtr = gv_wrbtr - gs_amnt-net_amnt.
        ENDIF.

        CLEAR :gs_amnt, gv_wrbtr1.
      ENDLOOP.

    ENDIF.
* perform FB05 BDC
    DELETE gt_amnt WHERE proc NE 'X'.
    IF NOT gt_amnt[] IS INITIAL.
*      PERFORM progress_indicator USING 'Posting and clearing' 'for Customer:' gs_tab-kunnr 'is in progress'.
      PERFORM fb05_bdc CHANGING check.
    ENDIF.

    CLEAR: gv_partial, gv_excess, gv_exact, gv_wrbtr, gv_wrbtr1, gs_tab.
*    IF gt_crnote_log[] IS NOT INITIAL AND
*     chbox = abap_true.
*      PERFORM f_create_credit_note. " Puratchi added on 13 Mar 2024
*    ENDIF.

  ENDLOOP.

  FREE: gs_bapireturn, gt_lineitems, gt_openitems, gt_amnt, gv_wrbtr.
  SELECT SINGLE name1 FROM kna1 INTO @l_name WHERE kunnr = @kunnr.
  LOOP AT gt_message ASSIGNING <lfs_mess>.
    <lfs_mess>-name1 = l_name.
  ENDLOOP.
  SELECT SINGLE * FROM  zfi_dms_cust_pay
    INTO @ls_table1
    WHERE bukrs = @bukrs
    AND kunnr = @kunnr
    AND distributor = @distb
    AND xblnr = @xblnr
    AND reference_id = @refid
    AND status = '10'.
  IF sy-subrc = 0.
    ls_table1-remarks = VALUE #( gt_message[ 1 ]-message OPTIONAL ).
    ls_table1-doc_no = g_doc_no.
    IF VALUE #( gt_message[ mess_type = 'E' ]-mess_type OPTIONAL ) = 'E'.
      ls_table1-status = '30'.
      ls_table1-remarks = VALUE #( gt_message[ mess_type = 'E' ]-message OPTIONAL ).
    ELSE.
      IF VALUE #( gt_message[ mess_type = 'S' ]-mess_type OPTIONAL ) = 'S'.
        ls_table1-status = '20'.

        IF gt_crnote_log[] IS NOT INITIAL AND
         chbox = abap_true.
          PERFORM f_create_credit_note. " Puratchi added on 13 Mar 2024
        ENDIF.
      ENDIF.
    ENDIF.
    MODIFY zfi_dms_cust_pay FROM ls_table1.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_header_from_selection
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_header_from_selection .

  DATA: lv_gjahr TYPE gjahr,
        lv_bukrs TYPE bukrs,
        lv_waers TYPE waers.

  SELECT SINGLE bukrs FROM t001 INTO lv_bukrs WHERE bukrs = bukrs.
  IF sy-subrc IS INITIAL.
    SELECT SINGLE waers FROM t001 INTO lv_waers WHERE bukrs = bukrs
                                                  AND waers = waers.
    IF sy-subrc IS NOT INITIAL.
      SET CURSOR FIELD 'WAERS'.
      MESSAGE 'Enter valid Currency' TYPE 'E'.
    ENDIF.
  ELSE.
    SET CURSOR FIELD 'BUKRS'.
    MESSAGE 'Invalid Company Code' TYPE 'E'.
  ENDIF.

  IF xblnr IS INITIAL.
    SET CURSOR FIELD 'XBLNR'.
    MESSAGE 'Enter Reference' TYPE 'E'.
  ENDIF.

  IF xtext IS INITIAL.     "added on 12/3
    SET CURSOR FIELD 'XTEXT'.
    MESSAGE 'Enter Reference Text' TYPE 'E'.
  ENDIF.
  IF newko IS INITIAL.     "added on 12/3
    SET CURSOR FIELD 'NEWKO'.
    MESSAGE 'Enter Bank G/L' TYPE 'E'.
  ENDIF.
  IF kunnr IS INITIAL.     "added on 12/3
    SET CURSOR FIELD 'KUNNR'.
    MESSAGE 'Enter Customer No' TYPE 'E'.
  ENDIF.
  IF wrbtr IS INITIAL.     "added on 12/3
    SET CURSOR FIELD 'WRBTR'.
    MESSAGE 'Enter Amount' TYPE 'E'.
  ENDIF.
*  IF sy-sysid EQ 'PRD'.
*    PERFORM bldat_check USING bldat
*                        CHANGING check.
*    IF NOT check IS INITIAL.
*      SET CURSOR FIELD 'BLDAT'.
*      MESSAGE 'Entered Document date is more than 2 months old' TYPE 'E'.
*    ENDIF.
*  ENDIF.
* get fiscal year and period - (requires date and company code)
  CALL FUNCTION 'BAPI_COMPANYCODE_GET_PERIOD'
    EXPORTING
      companycodeid = bukrs
      posting_date  = bldat
    IMPORTING
      fiscal_year   = lv_gjahr
      fiscal_period = gs_header-monat.

  IF gs_header-gjahr IS  INITIAL.
    gs_header-gjahr = lv_gjahr.
  ENDIF.

  gs_header-bldat = bldat.
  gs_header-budat = budat.
  gs_header-blart = blart.
  gs_header-xblnr = xblnr.
  gs_header-xtext = xtext.
  gs_header-newko = newko.
  gs_header-bukrs = bukrs.
  gs_header-waers = waers.
  monat = gs_header-monat.
  gjahr = gs_header-gjahr.
  gs_header-bktxt = bktxt.
  gs_header-augtx = augtx.
  gs_header-valut = valut.
  gs_header-gldesc = gldesc.
  gs_header-distb = distb.
  gs_header-gsber = gsber.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = gs_header-newko
    IMPORTING
      output = gs_header-newko.
* desc for GL ac
  SELECT txt50 INTO gs_header-gldesc UP TO 1 ROWS FROM skat WHERE spras = sy-langu
                                AND saknr = gs_header-newko ORDER BY PRIMARY KEY.
  ENDSELECT. " Added by <IT-CAR Tool> during Code Remediation

  IF sy-subrc = 0.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = gs_header-newko
      IMPORTING
        output = gs_header-newko.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form bldat_check
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GS_HEADER_BLDAT
*&      <-- CHECK
*&---------------------------------------------------------------------*
FORM bldat_check   USING p_dat TYPE  bkpf-bldat
                  CHANGING p_check LIKE check.

  CLEAR : p_check.

  DATA: lv_monc(2)   TYPE  c, " Current month
        lv_yearc(4)  TYPE c, " current year
        lv_spmonc(6),       " year month
        lv_mond(2)   TYPE c, "document month
        lv_yeard(4)  TYPE c, " document year
        lv_spmond(6),       " doc year month
        lv_mon       TYPE i,  " month
        lv_year(4)   TYPE c, "  year
        lv_spmon(6),       " year month
        lv_mdiff     TYPE i,   " diff in months
        lv_mondi     TYPE i. "document month

  CALL FUNCTION 'CACS_DATE_GET_YEAR_MONTH'
    EXPORTING
      i_date  = sy-datum
    IMPORTING
      e_month = lv_monc
      e_year  = lv_yearc.

  CONCATENATE lv_yearc lv_monc INTO lv_spmonc IN CHARACTER MODE.

  CALL FUNCTION 'CACS_DATE_GET_YEAR_MONTH'
    EXPORTING
      i_date  = p_dat
    IMPORTING
      e_month = lv_mond
      e_year  = lv_yeard.

  CONCATENATE lv_yeard lv_mond INTO lv_spmond IN CHARACTER MODE.
  lv_mondi = lv_mond.

  IF lv_spmond LT lv_spmonc.
    CLEAR : lv_spmon, lv_mdiff.
    DO.
      IF lv_mdiff EQ 0.
        lv_mon = lv_monc.
        lv_year = lv_yearc.
      ENDIF.

      IF lv_year EQ lv_yeard AND lv_mon = lv_mondi.
        EXIT.
      ENDIF.
      IF lv_mon = 1.
        lv_mon = '12'.
        lv_year = lv_year - 1.
      ELSE.
        lv_mon = lv_mon - 1.
      ENDIF.

      lv_mdiff = lv_mdiff + 1.
      IF lv_mdiff GT 2.
        p_check = 'X'.
        EXIT.
      ENDIF.
    ENDDO.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_display_output
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_display_output .
  DATA: lo_gr_alv       TYPE REF TO cl_salv_table, " Variables for ALV properties
        lo_gr_functions TYPE REF TO cl_salv_functions_list.
*
*  DATA: lo_event_handler TYPE REF TO cl_handler, " Variables for events
*        lo_events        TYPE REF TO cl_salv_events_table.

  DATA: lo_grid        TYPE REF TO cl_salv_form_layout_grid, " Variables for header
        lo_layout_logo TYPE REF TO cl_salv_form_layout_logo,
        lo_content     TYPE REF TO cl_salv_form_element,
        lv_title       TYPE string,
        lv_rows        TYPE string.

  DATA: lo_layout TYPE REF TO cl_salv_layout, " Variables for enabling Save button
        lv_key    TYPE salv_s_layout_key.

  DATA: lo_display TYPE REF TO cl_salv_display_settings. " Variable for layout settings

  DATA: lo_selections TYPE REF TO cl_salv_selections, " Variables for selection mode and column properties
        lo_columns    TYPE REF TO cl_salv_columns,
        lo_column     TYPE REF TO cl_salv_column_table.

* Create the ALV object
  TRY.
      CALL METHOD cl_salv_table=>factory
        IMPORTING
          r_salv_table = lo_gr_alv
        CHANGING
          t_table      = gt_message.
    CATCH cx_salv_msg.
  ENDTRY.
* Let's show all default buttons of ALV
  lo_gr_functions = lo_gr_alv->get_functions( ).
  lo_gr_functions->set_all( abap_true ).

* Fit the columns
  lo_columns = lo_gr_alv->get_columns( ).
  lo_columns->set_optimize( 'X' ).

* Create header
  DESCRIBE TABLE gt_message LINES lv_rows.
  CONCATENATE 'Number of records: ' lv_rows INTO lv_title SEPARATED BY space.

  CREATE OBJECT lo_grid.
  CREATE OBJECT lo_layout_logo.
  lo_grid->create_label( row = 1 column = 1 text = lv_title tooltip = lv_title ).
  lo_layout_logo->set_left_content( lo_grid ).
  lo_content = lo_layout_logo.
  lo_gr_alv->set_top_of_list( lo_content ).

* Apply zebra style to lv_rows
  lo_display = lo_gr_alv->get_display_settings( ).
  lo_display->set_striped_pattern( cl_salv_display_settings=>true ).

* Enable the save layout buttons
  lv_key-report = sy-repid.
  lo_layout = lo_gr_alv->get_layout( ).
  lo_layout->set_key( lv_key ).
  lo_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
  lo_layout->set_default( abap_true ).

* Register events
*  lo_events = lo_gr_alv->get_event( ).
*  CREATE OBJECT lo_event_handler.
*  SET HANDLER lo_event_handler->on_double_click FOR lo_events.

* Enable cell selection mode
  lo_selections = lo_gr_alv->get_selections( ).
  lo_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).

*  TRY.
*      lo_column ?= lo_columns->get_column( 'MESSAGE' ).
*      lo_column->set_visible( if_salv_c_bool_sap=>true ).
*      lo_column->set_long_text( 'Remark' ).
*      lo_column->set_medium_text( 'Remark' ).
*      lo_column->set_short_text( 'Remark' ).
*    CATCH cx_salv_not_found.
*    CATCH cx_salv_existing.
*    CATCH cx_salv_data_error.
*  ENDTRY.

  lo_gr_alv->display( ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f13_bdc
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- CHECK
*&---------------------------------------------------------------------*
FORM f13_bdc CHANGING p_lv_check TYPE any.

  FREE: messtab[], gs_messtab.
  CLEAR: p_lv_check.

  DATA: lv_mvar1 TYPE balm-msgv1,
        lv_mvar2 TYPE balm-msgv1,
        lv_mvar3 TYPE balm-msgv1,
        lv_mvar4 TYPE balm-msgv1.

*  PERFORM progress_indicator USING 'Automatic clearing' 'for Customer:' gs_tab-kunnr 'is in progress'.


  CALL FUNCTION 'BPAR_P_FI_CUSTOMER_DEQUEUE'
    EXPORTING
      customer       = gs_tab-kunnr
    EXCEPTIONS
      system_failure = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

***
  PERFORM bdc_dynpro      USING 'SAPF124' '1000'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'X_FEHLER'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=ONLI'.
  PERFORM bdc_field       USING 'BUKRX-LOW'
                                gs_header-bukrs.
  PERFORM bdc_field       USING 'X_KUNNR'
                                'X'.
  PERFORM bdc_field       USING 'KONTD-LOW'
                                gs_tab-kunnr.
  PERFORM bdc_field       USING 'X_TESTL'
                                ''.
  PERFORM bdc_field       USING 'XAUSBEL'
                                'X'.
  PERFORM bdc_field       USING 'XNAUSBEL'
                                ''.
  PERFORM bdc_field       USING 'X_FEHLER'
                                ''.

***
  PERFORM bdc_dynpro      USING 'SAPMSSY0' '0120'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=%EX'.
***

  PERFORM bdc_dynpro      USING 'SAPF124' '1000'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'BUKRX-LOW'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/EENDE'.
***

  REFRESH messtab.
  CALL TRANSACTION 'F.13' WITHOUT AUTHORITY-CHECK
                   USING bdcdata
                   MODE   gv_mode
                   UPDATE 'A'
                   MESSAGES INTO messtab.

  WAIT UP TO 2 SECONDS.
  COMMIT WORK AND WAIT.

  CLEAR: gs_messtab, gs_message.
  READ TABLE messtab INTO gs_messtab WITH KEY msgtyp = 'E'.
  IF sy-subrc = 0 .
    p_lv_check = 'E'.
    lv_mvar1 = gs_messtab-msgv1.
    lv_mvar2 = gs_messtab-msgv2.
    lv_mvar3 = gs_messtab-msgv3.
    lv_mvar4 = gs_messtab-msgv4.
    CALL FUNCTION 'MESSAGE_PREPARE'
      EXPORTING
        language               = sy-langu
        msg_id                 = gs_messtab-msgid
        msg_no                 = gs_messtab-msgnr
        msg_var1               = lv_mvar1
        msg_var2               = lv_mvar2
        msg_var3               = lv_mvar3
        msg_var4               = lv_mvar4
      IMPORTING
        msg_text               = gs_message-message
      EXCEPTIONS
        function_not_completed = 1
        message_not_found      = 2
        OTHERS                 = 3.
    gs_message-mess_type = 'E'.
    APPEND gs_message TO gt_message.
  ENDIF.

  FREE :bdcdata[].
  CLEAR: gs_message.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form bapi_ar_acc_getopenitems
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GS_HEADER_BUKRS
*&      --> GS_TAB_KUNNR
*&      --> GS_TAB_BELNR
*&      <-- GS_BAPIRETURN
*&      <-- GT_LINEITEMS
*&---------------------------------------------------------------------*
FORM bapi_ar_acc_getopenitems  USING    p_bukrs TYPE bukrs
                                         p_kunnr TYPE kunnr
                                         p_belnr TYPE belnr_d
                                 CHANGING  ps_bapireturn LIKE gs_bapireturn
                                           pt_lineitems LIKE gt_lineitems.

*   PERFORM progress_indicator USING 'Fetching open items' 'for Customer:' gs_tab-kunnr 'is in progress'.
  REFRESH pt_lineitems.

  CALL FUNCTION 'BAPI_AR_ACC_GETOPENITEMS' "#EC CI_USAGE_OK[2628704]
    "Added by SPLABAP during code remediation
    EXPORTING
      companycode = p_bukrs
      customer    = p_kunnr
      keydate     = sy-datum
*     NOTEDITEMS  = ' '
*     SECINDEX    = ' '
    IMPORTING
      return      = ps_bapireturn
    TABLES
      lineitems   = pt_lineitems.

  IF pt_lineitems[] IS NOT INITIAL.

    SELECT  rbukrs AS comp_code,
            gjahr AS fisc_year,
            belnr AS doc_no
              FROM acdoca INTO TABLE @DATA(lt_cust_openitem)
                   FOR ALL ENTRIES IN @pt_lineitems
                   WHERE  rbukrs = @pt_lineitems-comp_code
                      AND gjahr = @pt_lineitems-fisc_year
                      AND belnr = @pt_lineitems-doc_no
                      AND kunnr = @kunnr
                      AND rbusa = @gsber.
    IF sy-subrc = 0.
      DATA(lt_lineitems) = pt_lineitems.
      CLEAR lt_lineitems[].
      LOOP AT pt_lineitems INTO DATA(lw_data).
        READ TABLE lt_cust_openitem TRANSPORTING NO FIELDS WITH KEY  comp_code = lw_data-comp_code
                                                                     fisc_year = lw_data-fisc_year
                                                                     doc_no    = lw_data-doc_no.
        IF sy-subrc = 0.
          APPEND lw_data TO lt_lineitems.
        ENDIF.
      ENDLOOP.
    ENDIF.
    CLEAR pt_lineitems[].
    APPEND LINES OF lt_lineitems TO pt_lineitems.

    IF sy-subrc IS NOT INITIAL.                            "#EC FB_NORC
      gs_message-mess_type = gs_bapireturn-type.
      gs_message-message = gs_bapireturn-message.
      MOVE-CORRESPONDING gs_tab TO gs_message.
      APPEND gs_message TO gt_message.
      CLEAR gs_bapireturn.
    ELSE.
* if document number entered in table control, then we need to exclude other open items
* for that customer
      IF p_belnr IS NOT INITIAL.
        DELETE  pt_lineitems WHERE fisc_year NE gs_header-gjahr .
        LOOP AT pt_lineitems INTO gs_lineitems.
          IF gs_lineitems-doc_no EQ p_belnr OR gs_lineitems-inv_ref EQ p_belnr.
          ELSE.
            DELETE  pt_lineitems INDEX sy-tabix.
          ENDIF.
          CLEAR gs_lineitems.
        ENDLOOP.
      ENDIF.
    ENDIF.


    LOOP AT pt_lineitems INTO gs_lineitems WHERE sp_gl_ind NE ''.
      IF gs_lineitems-sp_gl_ind NE 'A'.
        DELETE pt_lineitems INDEX sy-tabix..
      ENDIF.
      CLEAR gs_lineitems.
    ENDLOOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form determine_due_date
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_LINEITEMS
*&      <-- GT_OPENITEMS
*&---------------------------------------------------------------------*
FORM determine_due_date USING    pt_lineitems LIKE gt_lineitems
                          CHANGING pt_openitems LIKE gt_openitems.
  DATA l_round_off TYPE i.
  CLEAR gs_lineitems.

  IF pt_lineitems IS NOT INITIAL.
    SELECT * FROM bkpf INTO TABLE gt_bkpf
             FOR ALL ENTRIES IN pt_lineitems
             WHERE bukrs = pt_lineitems-comp_code
             AND   belnr = pt_lineitems-doc_no
             AND   gjahr = pt_lineitems-fisc_year.
  ENDIF.
  LOOP AT pt_lineitems INTO gs_lineitems.

    IF gs_lineitems-doc_type = 'DR' OR ( gs_lineitems-doc_type = 'AB' )" AND  gs_lineitems-db_cr_ind = 'S' )
      OR gs_lineitems-doc_type = 'RV' OR gs_lineitems-doc_type = 'DZ' OR gs_lineitems-doc_type = 'DG' OR
      gs_lineitems-doc_type = 'VG' OR gs_lineitems-doc_type = 'KR'.
      MOVE-CORRESPONDING gs_lineitems TO gs_openitems.
*   Get Net due date
      CLEAR: gs_faede.
      gs_faede-shkzg = gs_lineitems-db_cr_ind.
      gs_faede-koart = 'D'.
      gs_faede-zfbdt = gs_lineitems-bline_date.
      gs_faede-zbd1t = gs_lineitems-dsct_days1.
      gs_faede-zbd2t = gs_lineitems-dsct_days2.
      gs_faede-zbd3t = gs_lineitems-netterms.
      gs_faede-rebzg = gs_lineitems-inv_ref.
      gs_faede-bldat = gs_lineitems-doc_date.
      CALL FUNCTION 'DETERMINE_DUE_DATE'
        EXPORTING
          i_faede                    = gs_faede
        IMPORTING
          e_faede                    = gs_faede
        EXCEPTIONS
          account_type_not_supported = 1
          OTHERS                     = 2.
      gs_openitems-due_date = gs_faede-netdt.
      gs_openitems-due_cashd1 = gs_faede-sk1dt.
      gs_openitems-due_cashd2 = gs_faede-sk2dt.
      CLEAR gs_bkpf.
      READ TABLE gt_bkpf INTO gs_bkpf WITH KEY bukrs = gs_lineitems-comp_code
                                               belnr = gs_lineitems-doc_no
                                               gjahr = gs_lineitems-fisc_year.
      IF sy-subrc IS INITIAL.
        gs_openitems-cpudt = gs_bkpf-cpudt.
        gs_openitems-cputm = gs_bkpf-cputm.
      ENDIF.

      APPEND gs_openitems TO pt_openitems.
      CLEAR : gs_lineitems, gs_openitems.
    ENDIF.
  ENDLOOP.

  SORT pt_openitems BY pstng_date ASCENDING cpudt ASCENDING cputm ASCENDING.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form process_documents
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- GT_OPENITEMS
*&---------------------------------------------------------------------*
FORM process_documents  CHANGING pt_openitems LIKE gt_openitems.

  DATA: lv_index           TYPE sy-tabix,
        lv_index1          TYPE sy-tabix,
        lv_count_s         TYPE i,
        lv_count_h         TYPE i,
        lv_count           TYPE i,
        lv_root_doc        TYPE belnr_d,
        lv_root_year       TYPE gjahr,
        lv_root_item       TYPE buzei,
        lv_due_date        TYPE netdt,
        lv_disc            TYPE dzproz,
        lv_disc_amnt       TYPE wrbtr,
        lv_prev            TYPE char1,
        lv_rootdocyear(17) TYPE c,
        lv_cpudt           TYPE cpudt,                "Entry Date
        lv_cputm           TYPE cputm.                "Entry time

*   PERFORM progress_indicator USING 'Processing open items' 'for Customer:' gs_tab-kunnr 'is in progress'.

  CLEAR: gs_openitems, gs_openitems1.

  LOOP AT pt_openitems INTO gs_openitems.
    lv_index = sy-tabix.
    IF gs_openitems-doc_type = 'DZ' OR gs_openitems-doc_type = 'AB'.
      CLEAR gs_openitems-inv_ref.
      MODIFY pt_openitems FROM gs_openitems INDEX lv_index.
    ENDIF.
  ENDLOOP.

  CLEAR: gs_openitems, gs_openitems1.
  LOOP AT pt_openitems INTO gs_openitems.
    lv_index = sy-tabix.
    IF gs_openitems-inv_ref IS NOT INITIAL.
*       "means this doc can be linked to previous invoice in different
*       ways , such as partial residual etc

      READ TABLE pt_openitems INTO gs_openitems1 WITH KEY  doc_no = gs_openitems-inv_ref
                                                           fisc_year = gs_openitems-inv_year
                                                           item_num  = gs_openitems-inv_item
                                                           .
      IF sy-subrc IS INITIAL.
        lv_index1 = sy-tabix. "lv_index1 is the index of parent doc
        gs_openitems1-root_doc = gs_openitems1-doc_no.  "original incoice will be called root doc
        gs_openitems1-root_year = gs_openitems1-fisc_year. "original incoice year will be called root year
        gs_openitems1-root_item = gs_openitems1-item_num. "original incoice item will be called root item
        IF gs_openitems1-doc_type = 'DZ'.
* parent/root doc is residual doc
          gs_openitems1-previous = gc_r.
        ELSEIF gs_openitems1-doc_type = 'DG' OR gs_openitems1-doc_type = 'VG'.
          gs_openitems1-previous = gc_c.   "Credit note
        ELSEIF gs_openitems1-doc_type = 'KR'.
          READ TABLE gt_cusven WITH KEY kunnr = gs_tab-kunnr TRANSPORTING NO FIELDS.
          IF sy-subrc IS NOT INITIAL.
            DELETE pt_openitems INDEX lv_index.
            CONTINUE.
          ELSE.
            gs_openitems1-previous = gc_k."customer also vendor, KR docshould consider similar to credit note
          ENDIF.
        ELSE.
* this is a partial doc done for one of the previous invoices
          gs_openitems1-previous = gc_p.
        ENDIF.
        CONCATENATE  gs_openitems1-root_year gs_openitems1-root_doc gs_openitems1-root_item
                                      INTO gs_openitems1-rootdocyear.

* root doc is assigned for this partial type of document
        MODIFY pt_openitems FROM gs_openitems1 INDEX lv_index
                                       TRANSPORTING previous root_doc root_year root_item rootdocyear.
        IF gs_openitems1-root_doc IS INITIAL.
* root doc is assigned same as that of doc number
          MODIFY pt_openitems FROM gs_openitems1 INDEX lv_index1
                                      TRANSPORTING previous root_doc root_year root_item rootdocyear.
        ENDIF.
      ELSE.   " this doc's parent is not available in the available open items
        IF gs_openitems-db_cr_ind  = 'H'.
          IF gs_openitems-doc_type = 'DG' OR gs_openitems-doc_type = 'VG'.
            gs_openitems-previous = gc_c.    " for Credit note case
          ELSEIF gs_openitems-doc_type = 'KR'. "customer also vendor, should consider similar to credit note
            READ TABLE gt_cusven WITH KEY kunnr = gs_tab-kunnr TRANSPORTING NO FIELDS.
            IF sy-subrc IS NOT INITIAL.
              DELETE pt_openitems INDEX lv_index.
              CONTINUE.
            ELSE.
              gs_openitems-previous = gc_k.
            ENDIF.
          ELSE.
* these can be partial doc's, whose parent invoice is not available, so consider them as credit note
            gs_openitems-previous = gc_c.    " for Credit note case
          ENDIF.
        ELSE.                      " for previous residual payment
          gs_openitems-previous = gc_r.
        ENDIF.
        gs_openitems-root_doc = gs_openitems-doc_no.
        gs_openitems-root_year = gs_openitems-fisc_year.
        gs_openitems-root_item = gs_openitems-item_num.
        CONCATENATE  gs_openitems-root_year gs_openitems-root_doc gs_openitems-root_item
                                      INTO gs_openitems-rootdocyear.
        MODIFY pt_openitems FROM gs_openitems INDEX lv_index
                                       TRANSPORTING previous root_doc root_year root_item rootdocyear.
      ENDIF.
    ELSE.  " if inv_ref is not there for this open item, this can be a direct invoice to process
      IF gs_openitems-doc_type = 'DG' OR gs_openitems-doc_type = 'VG'.
        gs_openitems-previous = gc_c.    " for Credit note
      ELSEIF gs_openitems-doc_type = 'DZ' AND gs_openitems-ref_doc_no = 'EXCESS PAYMENT'.
        gs_openitems-previous = gc_c.    " for Credit note
      ELSEIF gs_openitems-doc_type = 'KR'. " customer as  a  vendor scenario, considered as a credit note
        READ TABLE gt_cusven WITH KEY kunnr = gs_tab-kunnr TRANSPORTING NO FIELDS.
        IF sy-subrc IS NOT INITIAL.
          DELETE pt_openitems INDEX lv_index.
          CONTINUE.
        ELSE.
          gs_openitems-previous = gc_k.
        ENDIF.
      ELSE.
        IF gs_openitems-db_cr_ind  = 'H'.
          gs_openitems-previous = gc_c.    " for Credit note
        ELSE.
          gs_openitems-previous = gc_n.  " plain invoice
        ENDIF.
      ENDIF.
* all root values are assigned with same documet no's
      gs_openitems-root_doc = gs_openitems-doc_no.
      gs_openitems-root_year = gs_openitems-fisc_year.
      gs_openitems-root_item = gs_openitems-item_num.
      CONCATENATE  gs_openitems-root_year gs_openitems-root_doc gs_openitems-root_item
                                    INTO gs_openitems-rootdocyear.
      MODIFY pt_openitems FROM gs_openitems INDEX lv_index
                                     TRANSPORTING previous root_doc root_year root_item rootdocyear.
    ENDIF.
    CLEAR : gs_openitems, gs_openitems1, lv_index, lv_index1.
  ENDLOOP.
*
* sort in ascending order of year and date
  CLEAR: gs_openitems, gs_openitems1.
  SORT pt_openitems BY  root_year root_doc  pstng_date ASCENDING item_num ASCENDING.
  LOOP AT pt_openitems INTO gs_openitems .
    lv_index = sy-tabix.
* docyear
    CONCATENATE  gs_openitems-fisc_year gs_openitems-doc_no gs_openitems-item_num
                                  INTO gs_openitems-docyear.
* docyear updation
    MODIFY pt_openitems FROM gs_openitems INDEX lv_index
                                   TRANSPORTING docyear.
* due date is made same for all docs with same root doc
    CLEAR gs_openitems1.
    READ TABLE pt_openitems INTO gs_openitems1 WITH KEY doc_no = gs_openitems-root_doc
                                                        fisc_year = gs_openitems-root_year
                                                        item_num = gs_openitems-root_item.
    IF sy-subrc IS INITIAL.
      gs_openitems-due_date = gs_openitems1-due_date.
      MODIFY pt_openitems FROM gs_openitems INDEX lv_index TRANSPORTING due_date.
    ENDIF.

*  move the credit notes to  internal table 'gt_openitemsc'.
    IF gs_openitems-doc_no EQ gs_openitems-root_doc AND
       gs_openitems-fisc_year EQ gs_openitems-root_year AND
       ( gs_openitems-previous = gc_c OR gs_openitems-previous = gc_k ).
      APPEND gs_openitems TO gt_openitemsc.
      DELETE pt_openitems INDEX lv_index.
      CONTINUE.
    ENDIF.
    CLEAR gs_openitems.
  ENDLOOP.

  CHECK pt_openitems[] IS NOT INITIAL.
* delete normal credit notes in case of KR doc scenario where these KR docs will be cleared using F-32, then fresh open items are pulled and
* FB05 is processed.
  IF gt_cusven[] IS NOT INITIAL.
    READ TABLE gt_openitemsc WITH KEY previous = gc_k TRANSPORTING  NO FIELDS.
    IF sy-subrc IS INITIAL.
      DELETE gt_openitemsc WHERE previous = gc_c.
    ENDIF.
  ENDIF.

  SORT gt_openitemsc BY due_date cpudt ASCENDING cputm ASCENDING.

  SORT pt_openitems BY  root_year root_doc due_date DESCENDING cpudt ASCENDING cputm ASCENDING item_num DESCENDING.

  CLEAR: gs_openitems,  gs_amnt, lv_root_doc, lv_root_year, lv_due_date, lv_rootdocyear.
  LOOP AT pt_openitems INTO gs_openitems.
    lv_index = sy-tabix.
* when ever root doc changes, these are grouped to  internal table 'gt_amnt'
    IF sy-tabix NE 1 AND lv_rootdocyear NE gs_openitems-rootdocyear.
      gs_amnt-root_doc  = lv_root_doc.
      gs_amnt-root_year = lv_root_year.
      gs_amnt-root_item = lv_root_item.
      gs_amnt-due_date =  lv_due_date .
      gs_amnt-count_s = lv_count_s.
      gs_amnt-count_h = lv_count_h.
      gs_amnt-count = lv_count.
      gs_amnt-prev = lv_prev.
      gs_amnt-disc = lv_disc.
      gs_amnt-disc_amnt = lv_disc_amnt.
      gs_amnt-net_amnt = gs_amnt-net_amnt - gs_amnt-disc_amnt.
      gs_amnt-rootdocyear = lv_rootdocyear.
      gs_amnt-cpudt =  lv_cpudt.
      gs_amnt-cputm =  lv_cputm.
      APPEND gs_amnt TO gt_amnt.
      CLEAR: gs_amnt, lv_count_s, lv_count, lv_due_date, lv_prev, lv_disc, lv_disc_amnt.
    ENDIF.
    lv_rootdocyear = gs_openitems-rootdocyear.
    lv_root_doc  = gs_openitems-root_doc.
    lv_root_year = gs_openitems-root_year.
    lv_root_item = gs_openitems-root_item.

    IF gs_openitems-root_doc = gs_openitems-doc_no AND gs_openitems-root_year = gs_openitems-fisc_year
                                                   AND gs_openitems-root_item = gs_openitems-item_num .
      IF gs_openitems-pmnttrms IS NOT INITIAL.
        CLEAR : gs_amnt-disc, gs_amnt-disc_amnt,
                lv_disc, lv_disc_amnt.
* calculate discount amount based on payment terms for the original invoice
        PERFORM discount_calc USING gs_openitems
                               CHANGING lv_disc lv_disc_amnt.
      ENDIF.
      lv_cpudt = gs_openitems-cpudt.
      lv_cputm = gs_openitems-cputm.
    ENDIF.
* if the open item is not a stand alone doc
    IF gs_openitems-root_doc NE gs_openitems-doc_no.
      IF gs_openitems-previous NE gc_r.
        lv_prev = gc_p.
      ELSE.
        lv_prev = gc_r.
      ENDIF.
    ENDIF.

    IF lv_due_date IS INITIAL.
      lv_due_date = gs_openitems-due_date.
    ELSEIF lv_due_date GT gs_openitems-due_date.
      lv_due_date = gs_openitems-due_date.
    ENDIF.
* calculate net amount payable for the all the docs with same root document
* based on debit credit indicator
    IF gs_openitems-db_cr_ind  = 'S'.
      gs_amnt-net_amnt = gs_amnt-net_amnt + gs_openitems-lc_amount. "#EC CI_FLDEXT_OK[2610650]
      "Added by SPLABAP during code remediation
      gs_amnt-root_amnt = gs_amnt-root_amnt + gs_amnt-net_amnt.
      lv_count_s = lv_count_s + 1.
      lv_count = lv_count + 1.
    ELSE.
      gs_amnt-net_amnt = gs_amnt-net_amnt - gs_openitems-lc_amount. "#EC CI_FLDEXT_OK[2610650]
      "Added by SPLABAP during code remediation
      lv_count_h = lv_count_h + 1.
      lv_count = lv_count + 1.
    ENDIF.
  ENDLOOP.
* last doc is appened because of exit from the loop
  gs_amnt-root_doc  = gs_openitems-root_doc.
  gs_amnt-root_year = gs_openitems-root_year.
  gs_amnt-root_item = gs_openitems-root_item.
  gs_amnt-due_date =  lv_due_date .
  gs_amnt-count_s = lv_count_s.
  gs_amnt-count_h = lv_count_h.
  gs_amnt-count = lv_count.
  gs_amnt-prev = lv_prev.
  gs_amnt-disc = lv_disc.
  gs_amnt-disc_amnt = lv_disc_amnt.
  gs_amnt-net_amnt = gs_amnt-net_amnt - gs_amnt-disc_amnt.
  gs_amnt-rootdocyear = lv_rootdocyear.
  gs_amnt-cpudt =  lv_cpudt.
  gs_amnt-cputm =  lv_cputm.
  APPEND gs_amnt TO gt_amnt.
  CLEAR: gs_amnt, lv_count_s, lv_count, lv_due_date, lv_prev, lv_disc, lv_disc_amnt.
* FIFO process, oldest doc needs to be processed first.
  SORT gt_amnt BY due_date  ASCENDING cpudt ASCENDING cputm ASCENDING root_year DESCENDING root_doc DESCENDING root_item.
* sort open items back to ascending
  SORT pt_openitems BY  root_year root_doc root_item due_date ASCENDING cpudt ASCENDING cputm ASCENDING item_num ASCENDING.

  DELETE gt_amnt WHERE net_amnt LT 0.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f32_bdc_cn
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- CHECK
*&---------------------------------------------------------------------*
FORM f32_bdc_cn CHANGING p_lv_check TYPE any.

  FREE: messtab[], gs_messtab,  gt_bdc[], gt_bdcc[].
  CLEAR: p_lv_check, gs_bdc, gs_bdcc.

  DATA: lv_mvar1      TYPE balm-msgv1,
        lv_mvar2      TYPE balm-msgv1,
        lv_mvar3      TYPE balm-msgv1,
        lv_mvar4      TYPE balm-msgv1,
        lv_okcode(5)  TYPE c,
        lv_cursor(15) TYPE c,
        lv_cnt(2)     TYPE n,
        lv_f          TYPE p DECIMALS 2,
        lv_lines      TYPE sy-tabix,
        lv_abpos      TYPE posnr,
        lv_index      TYPE sy-tabix,
        lv_last       TYPE sy-tabix,
        lv_ceil       TYPE sy-tabix,
        lv_exit       TYPE sy-tabix.

*  PERFORM progress_indicator USING 'Clearing using F-32' 'for Customer:' gs_tab-kunnr 'is in progress'.

  CHECK  gv_sclear = 'X' OR  gv_vclear = 'X' OR  gv_nclear = 'X'.

  CALL FUNCTION 'BPAR_P_FI_CUSTOMER_DEQUEUE'
    EXPORTING
      customer       = gs_tab-kunnr
    EXCEPTIONS
      system_failure = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

* 1st screen
  PERFORM bdc_dynpro      USING 'SAPMF05A' '0131'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RF05A-XPOS1(03)'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=SLB'.
  PERFORM bdc_field       USING 'BKPF-BUKRS'
                                gs_header-bukrs.
  PERFORM bdc_field       USING 'RF05A-AGKON'
                                gs_tab-kunnr.
  PERFORM bdc_field       USING 'RF05A-AGUMS'
                                'A'.
  WRITE gs_header-budat TO gv_value.
  PERFORM bdc_field       USING 'BKPF-BUDAT' "Posting Date
                                 gv_value.
  PERFORM bdc_field       USING 'RF05A-XNOPS'
                                'X'.


* 2nd screen

  IF gv_vclear = 'X' OR gv_nclear = 'X'.
    DELETE gt_amnt  WHERE cnote NE 'X'.
  ENDIF.

  CLEAR: gs_bdc, gs_amnt, gs_openitems.
  DESCRIBE TABLE gt_amnt LINES lv_last.
*
  CLEAR gs_amnt.
  READ TABLE gt_amnt INTO gs_amnt INDEX lv_last.
  " This is the line where partial adjustment can be done in bdc via clicking
  DELETE gt_amnt INDEX lv_last.
  INSERT gs_amnt INTO gt_amnt INDEX 1.
*
  LOOP AT gt_amnt INTO gs_amnt.
    lv_index = sy-tabix.

    gs_bdc-fval1 = 'BELNR'.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gs_amnt-root_doc
      IMPORTING
        output = gs_amnt-root_doc.

    CONCATENATE gs_amnt-root_doc gs_amnt-root_year gs_amnt-root_item INTO gs_bdc-fval2.
    lv_lines = lv_lines + 1.
*     IF lv_index = lv_last.
*       INSERT gs_bdc INTO gt_bdc INDEX 1.   " This is the line where partial adjustment can be done in bdc via clicking
*     ELSE.
    APPEND gs_bdc TO gt_bdc.
*     ENDIF.
    CLEAR gs_amnt.
  ENDLOOP.

  LOOP AT gt_amnt INTO gs_amnt.
    CLEAR: gs_bdc, gs_openitems.

    LOOP AT gt_openitems INTO gs_openitems WHERE rootdocyear = gs_amnt-rootdocyear
                                           AND   docyear NE gs_amnt-rootdocyear.

      gs_bdc-fval1 = 'BELNR'.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = gs_openitems-doc_no
        IMPORTING
          output = gs_openitems-doc_no.

      CONCATENATE gs_openitems-doc_no gs_openitems-fisc_year gs_openitems-item_num INTO gs_bdc-fval2.
      lv_lines = lv_lines + 1.
      APPEND gs_bdc TO gt_bdc.
      CLEAR: gs_bdc, gs_openitems.
    ENDLOOP.
    CLEAR gs_amnt.
  ENDLOOP.


  IF gv_nclear = 'X'.
    DELETE gt_openitemsc WHERE previous NE gc_c.
  ENDIF.

  IF gv_vclear = 'X'.
    DELETE gt_openitemsc WHERE previous NE gc_k.
  ENDIF.

  CLEAR gs_openitems.
  LOOP AT gt_openitemsc INTO gs_openitems.
    gs_bdcc-fval1 = 'BELNR'.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gs_openitems-doc_no
      IMPORTING
        output = gs_openitems-doc_no.

    CONCATENATE gs_openitems-doc_no gs_openitems-fisc_year gs_openitems-item_num INTO gs_bdcc-fval2.
    lv_lines = lv_lines + 1.
    APPEND gs_bdcc TO gt_bdcc.
    CLEAR: gs_bdcc, gs_openitems.
  ENDLOOP.


* for regular invoices entries
  CLEAR: lv_index, lv_last, lv_exit, gs_bdc.
  DESCRIBE TABLE gt_bdc LINES lv_last.
  lv_f = lv_last / 18.
  lv_ceil = ceil( lv_f ).

  DO lv_ceil TIMES.

    PERFORM bdc_dynpro      USING 'SAPMF05A' '0733'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'RF05A-SEL01(01)'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=PA'.

    lv_index = lv_index + 1.
    lv_cnt = 0.

    DO 18 TIMES.
      lv_exit = lv_exit + 1.
      lv_cnt = lv_cnt + 1.
      CLEAR gs_bdc.
      READ TABLE gt_bdc INTO gs_bdc INDEX lv_exit.
      IF sy-subrc IS INITIAL.

        CONCATENATE 'RF05A-FELDN(' lv_cnt ')' INTO gs_bdc-fnam1.
        CONCATENATE 'RF05A-SEL01(' lv_cnt ')' INTO gs_bdc-fnam2.

        PERFORM bdc_field       USING gs_bdc-fnam1
                                      gs_bdc-fval1.
        PERFORM bdc_field       USING gs_bdc-fnam2
                                      gs_bdc-fval2.
      ENDIF.
      IF lv_exit = lv_last.
        EXIT.
      ENDIF.

    ENDDO.


* select more items
    IF lv_index NE lv_ceil.
      PERFORM bdc_dynpro      USING 'SAPDF05X' '3100'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                   'DF05B-PSSKT(01)'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=SL'.
      PERFORM bdc_field       USING 'RF05A-ABPOS'
                                    '1'.
* next screen.

      PERFORM bdc_dynpro      USING 'SAPMF05A' '0710'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'RF05A-AGKON'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=SLB'.
      PERFORM bdc_field       USING 'RF05A-AGBUK'
                                    gs_header-bukrs.
      PERFORM bdc_field       USING 'RF05A-AGKON'
                                    gs_tab-kunnr.
      PERFORM bdc_field       USING 'RF05A-AGKOA'
                                    'D'.
      PERFORM bdc_field       USING 'RF05A-XNOPS'
                                    'X'.
      PERFORM bdc_field       USING 'RF05A-XFIFO'
                                    ''.
      PERFORM bdc_field       USING 'RF05A-XPOS1(01)'
                                    'X'.
    ENDIF.
  ENDDO.

**
* for credit notes entries

  CLEAR: lv_index, lv_last, lv_exit, gs_bdcc.
  DESCRIBE TABLE gt_bdcc LINES lv_last.
  lv_f = lv_last / 18.
  lv_ceil = ceil( lv_f ).

  DO lv_ceil TIMES.

    lv_index = lv_index + 1.
    lv_cnt = 0.

* select more items

    PERFORM bdc_dynpro      USING 'SAPDF05X' '3100'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                 'DF05B-PSSKT(01)'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=SL'.
    PERFORM bdc_field       USING 'RF05A-ABPOS'
                                  '1'.
* next screen.
    PERFORM bdc_dynpro      USING 'SAPMF05A' '0710'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'RF05A-AGKON'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=SLB'.
    PERFORM bdc_field       USING 'RF05A-AGBUK'
                                  gs_header-bukrs.

    IF gv_vclear = 'X'.
      CLEAR gs_cusven.
      READ TABLE gt_cusven INTO gs_cusven WITH KEY kunnr = gs_tab-kunnr.
      IF sy-subrc IS INITIAL.
        PERFORM bdc_field       USING 'RF05A-AGKON'
                                      gs_cusven-lifnr.
      ELSE.
        PERFORM bdc_field       USING 'RF05A-AGKON'
                                      ''.
      ENDIF.
      PERFORM bdc_field       USING 'RF05A-AGKOA'
                                    'K'.

    ENDIF.

    IF gv_nclear = 'X'.
      PERFORM bdc_field       USING 'RF05A-AGKON'
                                    gs_tab-kunnr.
      PERFORM bdc_field       USING 'RF05A-AGKOA'
                                    'D'.
    ENDIF.

    PERFORM bdc_field       USING 'RF05A-XNOPS'
                                  'X'.
    PERFORM bdc_field       USING 'RF05A-XFIFO'
                                  ''.
    PERFORM bdc_field       USING 'RF05A-XPOS1(01)'
                                  'X'.


    PERFORM bdc_dynpro      USING 'SAPMF05A' '0733'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'RF05A-SEL01(01)'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=PA'.


    DO 18 TIMES.
      lv_exit = lv_exit + 1.
      lv_cnt = lv_cnt + 1.
      CLEAR gs_bdcc.
      READ TABLE gt_bdcc INTO gs_bdcc INDEX lv_exit.
      IF sy-subrc IS INITIAL.

        CONCATENATE 'RF05A-FELDN(' lv_cnt ')' INTO gs_bdcc-fnam1.
        CONCATENATE 'RF05A-SEL01(' lv_cnt ')' INTO gs_bdcc-fnam2.

        PERFORM bdc_field       USING gs_bdcc-fnam1
                                      gs_bdcc-fval1.
        PERFORM bdc_field       USING gs_bdcc-fnam2
                                      gs_bdcc-fval2.
      ENDIF.
      IF lv_exit = lv_last.
        EXIT.
      ENDIF.

    ENDDO.
  ENDDO.

* Next
  CLEAR: lv_okcode.

  lv_okcode = '=PART'.
  lv_cursor = 'DF05B-PSZAH(01)'.

  PERFORM bdc_dynpro      USING 'SAPDF05X' '3100'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                lv_okcode.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                               'DF05B-PSSKT(01)'.
  PERFORM bdc_field       USING 'RF05A-ABPOS'
                                '1'.


  LOOP AT gt_amnt INTO gs_amnt .

    lv_abpos = sy-tabix.

    PERFORM bdc_dynpro      USING 'SAPDF05X' '3100'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  lv_cursor.
    PERFORM bdc_field       USING 'RF05A-ABPOS'
                                  lv_abpos.
    IF gs_amnt-disc_amnt GT 0 .
      PERFORM bdc_dynpro      USING 'SAPDF05X' '3100'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=Z+S'.
    ELSE.
      PERFORM bdc_dynpro      USING 'SAPDF05X' '3100'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=Z-S'.
    ENDIF.
  ENDLOOP.

  PERFORM bdc_dynpro      USING 'SAPDF05X' '3100'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                lv_cursor.
  PERFORM bdc_field       USING 'RF05A-ABPOS'
                                '1'.

  PERFORM bdc_dynpro      USING 'SAPDF05X' '3100'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=PI'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                lv_cursor.
  PERFORM bdc_field       USING 'RF05A-ABPOS'
                                '1'.

  PERFORM bdc_dynpro      USING 'SAPDF05X' '3100'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=BU'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'DF05B-PSZAH(01)'.
  PERFORM bdc_field       USING 'RF05A-ABPOS'
                                '1'.


  REFRESH messtab.
  CALL TRANSACTION 'F-32' WITHOUT AUTHORITY-CHECK
                   USING bdcdata
                   MODE   gv_mode
                   UPDATE 'A'
                   MESSAGES INTO messtab.

  WAIT UP TO 2 SECONDS.
  COMMIT WORK AND WAIT.

  CLEAR: gs_messtab, gs_message.

  READ TABLE messtab INTO gs_messtab WITH KEY msgtyp = 'S'
                                              msgnr = '312'.
  IF sy-subrc IS INITIAL.
    COMMIT WORK AND WAIT.

    DATA: lv_belnr TYPE belnr_d.
    DO 10 TIMES.
      SELECT SINGLE belnr FROM bkpf INTO lv_belnr
                          WHERE belnr = gs_messtab-msgv1.
      IF sy-subrc IS NOT INITIAL.
        WAIT UP TO 2 SECONDS.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

    gs_message-mess_type = 'S'.
    MOVE-CORRESPONDING gs_tab TO gs_message.

    LOOP AT gt_amnt INTO gs_amnt.
      CLEAR : gs_message-wrbtr, gs_message-belnr.
      gs_message-belnr = gs_amnt-root_doc.
      CONCATENATE 'Clearing Document:' gs_messtab-msgv1 'Posted Successfully' INTO
                  gs_message-message  SEPARATED BY space.

      APPEND gs_message TO gt_message.
    ENDLOOP.
*    PERFORM progress_indicator USING 'Clearing using F-32' 'for Customer:' gs_tab-kunnr 'complete'.
  ELSE.

    READ TABLE messtab INTO gs_messtab WITH KEY msgtyp = 'E'.
    IF sy-subrc = 0 .
      p_lv_check = 'E'.
      lv_mvar1 = gs_messtab-msgv1.
      lv_mvar2 = gs_messtab-msgv2.
      lv_mvar3 = gs_messtab-msgv3.
      lv_mvar4 = gs_messtab-msgv4.
      CALL FUNCTION 'MESSAGE_PREPARE'
        EXPORTING
          language               = sy-langu
          msg_id                 = gs_messtab-msgid
          msg_no                 = gs_messtab-msgnr
          msg_var1               = lv_mvar1
          msg_var2               = lv_mvar2
          msg_var3               = lv_mvar3
          msg_var4               = lv_mvar4
        IMPORTING
          msg_text               = gs_message-message
        EXCEPTIONS
          function_not_completed = 1
          message_not_found      = 2
          OTHERS                 = 3.
      gs_message-mess_type = 'E'.
      APPEND gs_message TO gt_message.

    ELSE.

      LOOP AT messtab INTO gs_messtab WHERE msgtyp = 'S' .
        lv_mvar1 = gs_messtab-msgv1.
        lv_mvar2 = gs_messtab-msgv2.
        lv_mvar3 = gs_messtab-msgv3.
        lv_mvar4 = gs_messtab-msgv4.
        CALL FUNCTION 'MESSAGE_PREPARE'
          EXPORTING
            language               = sy-langu
            msg_id                 = gs_messtab-msgid
            msg_no                 = gs_messtab-msgnr
            msg_var1               = lv_mvar1
            msg_var2               = lv_mvar2
            msg_var3               = lv_mvar3
            msg_var4               = lv_mvar4
          IMPORTING
            msg_text               = gs_message-message
          EXCEPTIONS
            function_not_completed = 1
            message_not_found      = 2
            OTHERS                 = 3.
        gs_message-mess_type = 'W'.
        APPEND gs_message TO gt_message.
        CLEAR gs_messtab.
      ENDLOOP.

    ENDIF.
  ENDIF.


  FREE :bdcdata[].
  CLEAR: gs_message.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form bapi_ap_acc_getopenitems
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GS_HEADER_BUKRS
*&      --> GS_TAB_KUNNR
*&      <-- GS_BAPIRETURN
*&---------------------------------------------------------------------*
FORM bapi_ap_acc_getopenitems  USING    p_bukrs TYPE bukrs
                                          p_kunnr TYPE kunnr
                                  CHANGING  ps_bapireturn LIKE gs_bapireturn.

  DATA: pt_lineitemsv  TYPE STANDARD TABLE OF bapi3008_2.     " line items for vendor


  LOOP AT gt_cusven INTO gs_cusven WHERE kunnr = p_kunnr.
    CLEAR:  ps_bapireturn, gs_lineitemsv.
* get open items for vendor
    CALL FUNCTION 'BAPI_AP_ACC_GETOPENITEMS' "#EC CI_USAGE_OK[2628704]
      "Added by SPLABAP during code remediation
      EXPORTING
        companycode = p_bukrs
        vendor      = gs_cusven-lifnr
        keydate     = sy-datum
*       NOTEDITEMS  = ' '
      IMPORTING
        return      = ps_bapireturn
      TABLES
        lineitems   = pt_lineitemsv.
* KR docs only considered as credit notes for further processing
    DELETE pt_lineitemsv WHERE doc_type NE 'KR'.
    DELETE pt_lineitemsv WHERE db_cr_ind NE 'H'.
* add these vendor items to customer open items gathered before
    LOOP AT pt_lineitemsv INTO gs_lineitemsv.
      APPEND gs_lineitemsv TO gt_lineitemsv.
      CLEAR gs_lineitems.
      MOVE-CORRESPONDING gs_lineitemsv TO gs_lineitems.
      gs_lineitems-customer = p_kunnr.
      APPEND gs_lineitems TO gt_lineitems.
    ENDLOOP.
    FREE pt_lineitemsv.
    CLEAR :gs_cusven, ps_bapireturn .
  ENDLOOP.

ENDFORM.                    " BAPI_AP_ACC_GETOPENITEMS

*&---------------------------------------------------------------------*
*& Form check_creditnotes
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM check_creditnotes .

  DATA: lv_index  TYPE sy-tabix,
        lv_wrbtr  TYPE wrbtr,
        lv_hwrbtr TYPE wrbtr. "high amount

  CLEAR gv_wrbtro.
* calculate open items amount
  LOOP AT gt_amnt INTO gs_amnt.
    gv_wrbtro = gv_wrbtro + gs_amnt-net_amnt.
    CLEAR gs_amnt.
  ENDLOOP.
  CLEAR gv_wrbtrc.
*calculate credit notes amount.
* delete credit note with amounts greater than (total of outstanding invoices )
  DELETE gt_openitemsc WHERE lc_amount GT gv_wrbtro. "#EC CI_FLDEXT_OK[2610650]
  "Added by SPLABAP during code remediation

  IF gt_openitems[] IS INITIAL.
    CLEAR gv_wrbtrc.
    EXIT.
  ENDIF.

  LOOP AT gt_openitemsc INTO gs_openitems.
    lv_index = sy-tabix.
    IF gv_wrbtrc GT gv_wrbtro.
      DELETE gt_openitemsc INDEX lv_index.
    ELSE.
      gv_wrbtrc = gv_wrbtrc + gs_openitems-lc_amount. "#EC CI_FLDEXT_OK[2610650]
      "Added by SPLABAP during code remediation
    ENDIF.
    CLEAR gs_openitems.
  ENDLOOP.

  IF gt_openitems[] IS INITIAL.
    CLEAR gv_wrbtrc.
    EXIT.
  ENDIF.

  CLEAR: lv_wrbtr, lv_index.
* if the total credit note amount is greater than existing invoice's amount
*   then delete appropriate credit notes from the internal table for future invoices as and when they come
  IF gv_wrbtrc GT gv_wrbtro.
    LOOP AT gt_openitemsc INTO gs_openitems.
      lv_wrbtr = lv_wrbtr + gs_openitems-lc_amount. "#EC CI_FLDEXT_OK[2610650]
      "Added by SPLABAP during code remediation
      lv_index = sy-tabix.
      gs_openitems-cnote = 'X'.
* this record needs to processed for  credit note related  logic
      IF gv_wrbtro GE lv_wrbtr.
        MODIFY gt_openitemsc FROM gs_openitems INDEX lv_index TRANSPORTING cnote.
      ELSE.
        EXIT.
      ENDIF.
      CLEAR gs_openitems.
    ENDLOOP.
*then delete appropriate credit notes from the internal table for future invoices as and when they come
    DELETE gt_openitemsc WHERE cnote NE 'X'.
  ENDIF.

  CLEAR: lv_wrbtr, lv_index.
* check no.of records to be processed for the available credit notes
  LOOP AT gt_amnt INTO gs_amnt.
    lv_wrbtr = lv_wrbtr + gs_amnt-net_amnt.
    lv_index = sy-tabix.
* high priced of all the net amount is captured in lv_hwrbtr
    IF lv_hwrbtr LE gs_amnt-net_amnt.
      lv_hwrbtr = gs_amnt-net_amnt.
    ENDIF.
    gs_amnt-cnote = 'X'.
* this record needs to processed for  credit note related  logic
    MODIFY gt_amnt FROM gs_amnt INDEX lv_index TRANSPORTING cnote.
    IF gv_wrbtrc EQ lv_wrbtr.
      CLEAR gv_wrbtr1.
      EXIT.
    ENDIF.
    IF gv_wrbtrc LT lv_wrbtr.          " total credit note amount less than cumulative of open items amount
      IF gt_lineitemsv[] IS INITIAL.           "No vendor payments to be considered for customer as a vendor
        IF gv_wrbtr LT ( lv_wrbtr - gv_wrbtrc ). " if entered amount less than cumulatives of all amounts
*                                                 related to same root doc and available credit note amounts
          gv_wrbtr1 = gv_wrbtr.  " needs to be populated in BDC
          gv_partial = 'X'.  " indicator used in BDC
          gv_pwrbtr = lv_hwrbtr - ( lv_wrbtr - gv_wrbtrc ) + gv_wrbtr.
*           IF gs_tab-residual = 'X'.  " if selected in selection screen
*             gv_resid = 'X'. " indicator used in BDC
*           ENDIF.
        ELSE.
          gv_wrbtr1 = lv_wrbtr - gv_wrbtrc.  "if entered amount is greater than
*                                             ( diff between open and credit notes)
          gv_pwrbtr = lv_hwrbtr.
        ENDIF.
        EXIT.
      ELSE.
        EXIT.
      ENDIF.
    ENDIF.
    CLEAR gs_amnt.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form fb05_bdc_excess
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- CHECK
*&---------------------------------------------------------------------*
FORM fb05_bdc_excess  CHANGING p_lv_check.

*   PERFORM progress_indicator USING 'Excess payment being posted' 'for Customer:' gs_tab-kunnr 'is in progress'.

  FREE: messtab[], gs_messtab.
  CLEAR: p_lv_check.
  DATA: lv_mvar1 TYPE balm-msgv1,
        lv_mvar2 TYPE balm-msgv1,
        lv_mvar3 TYPE balm-msgv1,
        lv_mvar4 TYPE balm-msgv1.

  CALL FUNCTION 'BPAR_P_FI_CUSTOMER_DEQUEUE'
    EXPORTING
      customer       = gs_tab-kunnr
    EXCEPTIONS
      system_failure = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

* 1st screen
  PERFORM bdc_dynpro      USING 'SAPMF05A' '0122'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RF05A-NEWKO'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  CLEAR gv_value.
  WRITE gs_header-bldat TO gv_value.
  PERFORM bdc_field       USING 'BKPF-BLDAT' "document date
                                 gv_value.
  PERFORM bdc_field       USING 'BKPF-BLART'
                                'DZ'.
  PERFORM bdc_field       USING 'BKPF-BUKRS'
                                gs_header-bukrs.
  CLEAR gv_value.
  WRITE gs_header-budat TO gv_value.
  PERFORM bdc_field       USING 'BKPF-BUDAT' "Posting Date
                                 gv_value.
  PERFORM bdc_field       USING 'BKPF-MONAT'  "posting period
                                gs_header-monat.
  PERFORM bdc_field       USING 'BKPF-WAERS'
                                gs_header-waers.
*  gs_header-xblnr = 'EXCESS PAYMENT'.
  PERFORM bdc_field       USING 'BKPF-XBLNR'
                                gs_header-xblnr.
  gs_header-bktxt =  'EXCESS PAYMENT'.
  PERFORM bdc_field       USING 'BKPF-BKTXT'
                                gs_header-bktxt.
  CONCATENATE 'EXCESS PAYMENT FOR CUSTOMER'  gs_tab-kunnr
  INTO  gs_header-augtx SEPARATED BY space.
  PERFORM bdc_field       USING 'RF05A-AUGTX'
                                gs_header-augtx.

  PERFORM bdc_field       USING 'FS006-DOCID'
                                 '*'.
  PERFORM bdc_field       USING 'RF05A-NEWBS'
                                '40'.
  PERFORM bdc_field       USING 'RF05A-NEWKO'
                                gs_header-newko.

* 2nd  Second screen
  PERFORM bdc_dynpro      USING 'SAPMF05A' '0300'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'BSEG-WRBTR'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=PA'.
  WRITE gv_wrbtr TO gv_value.                           "#EC UOM_IN_MES
  CONDENSE gv_value.
  PERFORM bdc_field       USING 'BSEG-WRBTR'
                                gv_value.
  CLEAR gv_value.
  WRITE gs_header-budat TO gv_value.
  PERFORM bdc_field       USING 'BSEG-VALUT'
                                  gv_value.
  CLEAR gv_value.
  gv_value = 'EXCESS PAYMENT'.
  PERFORM bdc_field       USING 'BSEG-SGTXT'
                                gv_value.
  PERFORM bdc_field       USING 'RF05A-NEWBS'
                                '11'.
  PERFORM bdc_field       USING 'RF05A-NEWKO'
                                gs_tab-kunnr.

* subscreen (pop up)
  PERFORM bdc_dynpro      USING 'SAPLKACB' '0002'.

  PERFORM bdc_field       USING 'BDC_CURSOR'
                                 'COBL-PRCTR'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=ENTE'.
  PERFORM bdc_field       USING 'COBL-GSBER'
                                gs_header-gsber.

* 3rd screen
  PERFORM bdc_dynpro      USING 'SAPMF05A' '0301'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                 '/00'.
  WRITE gv_wrbtr TO gv_value.                           "#EC UOM_IN_MES
  CONDENSE gv_value.
  PERFORM bdc_field       USING 'BSEG-WRBTR'
                                gv_value.
  CLEAR gv_value.
  WRITE gs_header-budat TO gv_value.
  PERFORM bdc_field       USING 'BSEG-ZFBDT'
                                  gv_value.
  PERFORM bdc_field       USING 'BSEG-SGTXT'
                                'EXCESS PAYMENT'.


  PERFORM bdc_dynpro      USING 'SAPMF05A' '0301'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=BU'.
  WRITE gv_wrbtr TO gv_value.                           "#EC UOM_IN_MES
  CONDENSE gv_value.
  PERFORM bdc_field       USING 'BSEG-WRBTR'
                                gv_value.
  CLEAR gv_value.
  WRITE gs_header-budat TO gv_value.
  PERFORM bdc_field       USING 'BSEG-ZFBDT'
                                  gv_value.
  PERFORM bdc_field       USING 'BSEG-SGTXT'
                                'EXCESS PAYMENT'.

  REFRESH messtab.
  CALL TRANSACTION 'FB05' WITHOUT AUTHORITY-CHECK
                   USING bdcdata
                   MODE   gv_mode
                   UPDATE 'A'
                   MESSAGES INTO messtab.

  WAIT UP TO 2 SECONDS.
  COMMIT WORK AND WAIT.

  CLEAR: gs_messtab, gs_message.

  READ TABLE messtab INTO gs_messtab WITH KEY msgtyp = 'S'
                                              msgnr = '312'.
  IF sy-subrc IS INITIAL.
    COMMIT WORK AND WAIT.

    DATA: lv_belnr TYPE belnr_d.
    DO 10 TIMES.
      SELECT SINGLE belnr FROM bkpf INTO lv_belnr
                          WHERE belnr = gs_messtab-msgv1.
      IF sy-subrc IS NOT INITIAL.
        WAIT UP TO 2 SECONDS.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

    gs_message-mess_type = 'S'.
    MOVE-CORRESPONDING gs_tab TO gs_message.
    IF gs_tab-belnr IS INITIAL.
      gs_message-belnr = gs_amnt-root_doc.
    ENDIF.
    gs_message-wrbtr = gv_wrbtr.
    CONCATENATE gs_messtab-msgv1 'posted successfully as Excess Payment' INTO
                gs_message-message  SEPARATED BY space.

    APPEND gs_message TO gt_message.

    IF budat+4(2) LE 3.
      DATA(lv_fiscal_yr) = budat(4) - 1.
    ELSE.
      lv_fiscal_yr = budat(4).
    ENDIF.
    SELECT SINGLE * FROM zfi_dms_cust_pay INTO @DATA(l_cust_pay)
      WHERE bukrs = 'DMS1'
      AND kunnr = @kunnr
      AND distributor = @distb.

    DATA(ls_table1) = VALUE zdms_deb_ref_mis( mandt = sy-mandt
                                           bukrs = bukrs
                                           doc_no = gs_messtab-msgv1
                                           gjahr = lv_fiscal_yr
                                           account_type = 'D'
                                           account = kunnr
                                           invoice_refno = gs_header-xblnr
                                           distributor = distb
                                           werks = l_cust_pay-werks
                                           gsber = l_cust_pay-gsber
                                           hkont = l_cust_pay-hkont
                                           amount = wrbtr
                                           ernam = sy-uname
                                           erdat = sy-datum
                                           erzet = sy-uzeit
                                           remarks = gs_header-xtext
                                           pay_reference = gs_header-xblnr
                                           doc_type = 'DZ'
                                           reference_id = refid
                                           ).
    MODIFY zdms_deb_ref_mis FROM ls_table1.
    g_doc_no = gs_messtab-msgv1.
*    PERFORM progress_indicator USING 'Excess payment being posted' 'for Customer:' gs_tab-kunnr 'complete'.
  ELSE.

    READ TABLE messtab INTO gs_messtab WITH KEY msgtyp = 'E'.
    IF sy-subrc = 0 .
      p_lv_check = 'E'.
      lv_mvar1 = gs_messtab-msgv1.
      lv_mvar2 = gs_messtab-msgv2.
      lv_mvar3 = gs_messtab-msgv3.
      lv_mvar4 = gs_messtab-msgv4.
      CALL FUNCTION 'MESSAGE_PREPARE'
        EXPORTING
          language               = sy-langu
          msg_id                 = gs_messtab-msgid
          msg_no                 = gs_messtab-msgnr
          msg_var1               = lv_mvar1
          msg_var2               = lv_mvar2
          msg_var3               = lv_mvar3
          msg_var4               = lv_mvar4
        IMPORTING
          msg_text               = gs_message-message
        EXCEPTIONS
          function_not_completed = 1
          message_not_found      = 2
          OTHERS                 = 3.
      gs_message-mess_type = 'E'.
      APPEND gs_message TO gt_message.

    ELSE.
      LOOP AT messtab INTO gs_messtab WHERE msgtyp = 'S' .
        lv_mvar1 = gs_messtab-msgv1.
        lv_mvar2 = gs_messtab-msgv2.
        lv_mvar3 = gs_messtab-msgv3.
        lv_mvar4 = gs_messtab-msgv4.
        CALL FUNCTION 'MESSAGE_PREPARE'
          EXPORTING
            language               = sy-langu
            msg_id                 = gs_messtab-msgid
            msg_no                 = gs_messtab-msgnr
            msg_var1               = lv_mvar1
            msg_var2               = lv_mvar2
            msg_var3               = lv_mvar3
            msg_var4               = lv_mvar4
          IMPORTING
            msg_text               = gs_message-message
          EXCEPTIONS
            function_not_completed = 1
            message_not_found      = 2
            OTHERS                 = 3.
        gs_message-mess_type = 'W'.
        APPEND gs_message TO gt_message.
        CLEAR gs_messtab.
      ENDLOOP.
    ENDIF.

  ENDIF.


  FREE :bdcdata[].
  CLEAR: gs_message.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form bdc_dynpro
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      --> P_
*&---------------------------------------------------------------------*
FORM bdc_dynpro   USING program dynpro.
  CLEAR bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = dynpro.
  bdcdata-dynbegin = 'X'.
  APPEND bdcdata.
ENDFORM.
*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM bdc_field USING fnam fval.

  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  APPEND bdcdata.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DISCOUNT_CALC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GS_OPENITEMS  text
*      <--P_GS_AMNT_DISC  text
*      <--P_GS_AMNT_DISC_AMNT  text
*----------------------------------------------------------------------*
FORM discount_calc  USING    ps_openitems TYPE ty_openitems
                    CHANGING p_disc TYPE dzproz
                             p_disc_amnt TYPE wrbtr.

  CLEAR : p_disc, p_disc_amnt.
* entered posting date is checked w.r.t due dates
  IF gs_header-bldat LE ps_openitems-due_cashd1.
    p_disc = ps_openitems-dsct_pct1.
  ELSEIF gs_header-bldat LE ps_openitems-due_cashd2.
    p_disc = ps_openitems-dsct_pct2.
  ENDIF.
  p_disc_amnt = ps_openitems-disc_base * p_disc / 100. "#EC CI_FLDEXT_OK[2610650]
  "Added by SPLABAP during code remediation

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FB05_BDC
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM fb05_bdc CHANGING p_lv_check TYPE any.

  FREE: messtab[], gs_messtab, gt_bdc[].
  CLEAR: p_lv_check, gs_bdc.
  DATA: lv_mvar1      TYPE balm-msgv1,
        lv_mvar2      TYPE balm-msgv1,
        lv_mvar3      TYPE balm-msgv1,
        lv_mvar4      TYPE balm-msgv1,
        lv_okcode(5)  TYPE c,
        lv_cursor(15) TYPE c,
        lv_cnt(2)     TYPE n,
        lv_f          TYPE p DECIMALS 2,
        lv_index      TYPE sy-tabix,
        lv_lines      TYPE sy-tabix,
        lv_abpos      TYPE posnr,
        lv_last       TYPE sy-tabix,
        lv_ceil       TYPE sy-tabix,
        lv_floor      TYPE sy-tabix,
        lv_exit       TYPE sy-tabix.

  CALL FUNCTION 'BPAR_P_FI_CUSTOMER_DEQUEUE'
    EXPORTING
      customer       = gs_tab-kunnr
    EXCEPTIONS
      system_failure = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.


* 1st screen
  PERFORM bdc_dynpro      USING 'SAPMF05A' '0122'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RF05A-NEWKO'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  WRITE gs_header-bldat TO gv_value.
  PERFORM bdc_field       USING 'BKPF-BLDAT' "document date
                                 gv_value.
  PERFORM bdc_field       USING 'BKPF-BLART'
                                'DZ'.
  PERFORM bdc_field       USING 'BKPF-BUKRS'
                                gs_header-bukrs.
  WRITE gs_header-budat TO gv_value.
  PERFORM bdc_field       USING 'BKPF-BUDAT' "Posting Date
                                 gv_value.
  PERFORM bdc_field       USING 'BKPF-MONAT'  "posting period
                                gs_header-monat.
  PERFORM bdc_field       USING 'BKPF-WAERS'
                                gs_header-waers.
  PERFORM bdc_field       USING 'BKPF-XBLNR'
                                gs_header-xblnr.
  PERFORM bdc_field       USING 'BKPF-BKTXT'
                                gs_header-bktxt.
*   CONCATENATE 'Payment from Customer:' gs_tab-kunnr
*   INTO  gs_header-augtx SEPARATED BY space.
  PERFORM bdc_field       USING 'RF05A-AUGTX'
                                gs_header-xtext. "modified on 14/3

  PERFORM bdc_field       USING 'FS006-DOCID'
                                 '*'.
  PERFORM bdc_field       USING 'RF05A-NEWBS'
                                '40'.
  PERFORM bdc_field       USING 'RF05A-NEWKO'
                                gs_header-newko.
* 2nd  Second screen
  PERFORM bdc_dynpro      USING 'SAPMF05A' '0300'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'BSEG-WRBTR'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=PA'.

  WRITE gs_tab-wrbtr TO gv_value.                       "#EC UOM_IN_MES
  CONDENSE gv_value.

  PERFORM bdc_field       USING 'BSEG-WRBTR'
                                gv_value.
  CLEAR gv_value.
  " CONCATENATE 'Payment from Customer:' gs_tab-kunnr INTO gv_value SEPARATED BY space.
  "gv_value = gs_header-xtext .
  "CONDENSE gv_value.

  PERFORM bdc_field       USING 'BSEG-SGTXT'
                                gs_header-xtext. "added on 14/3
* subscreen (pop up)SAPLKACB
  PERFORM bdc_dynpro      USING 'SAPLKACB' '0002'.

  PERFORM bdc_field       USING 'BDC_CURSOR'
                                 'COBL-PRCTR'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=ENTE'.
  PERFORM bdc_field       USING 'COBL-GSBER'
                                gs_header-gsber.
* 3rd Third screen

  PERFORM bdc_dynpro      USING 'SAPMF05A' '0710'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RF05A-AGKON'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=SLB'.
  PERFORM bdc_field       USING 'RF05A-AGBUK'
                                gs_header-bukrs.
  PERFORM bdc_field       USING 'RF05A-AGKON'
                                gs_tab-kunnr.
  PERFORM bdc_field       USING 'RF05A-AGKOA'
                                'D'.
  PERFORM bdc_field       USING 'RF05A-XNOPS'
                                'X'.
  PERFORM bdc_field       USING 'RF05A-XFIFO'
                                ''.
  PERFORM bdc_field       USING 'RF05A-XPOS1(01)'
                                'X'.

* 4th screen

  CLEAR: gs_bdc, gs_amnt, gs_openitems, lv_lines.
  DESCRIBE TABLE gt_amnt LINES lv_last.

*
  CLEAR gs_amnt.
  READ TABLE gt_amnt INTO gs_amnt INDEX lv_last.
  " This is the line where partial adjustment can be done in bdc via clicking
  DELETE gt_amnt INDEX lv_last.
  INSERT gs_amnt INTO gt_amnt INDEX 1.
*
  LOOP AT gt_amnt INTO gs_amnt.
    lv_index = sy-tabix.

    gs_bdc-fval1 = 'BELNR'.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gs_amnt-root_doc
      IMPORTING
        output = gs_amnt-root_doc.

    CONCATENATE gs_amnt-root_doc gs_amnt-root_year gs_amnt-root_item INTO gs_bdc-fval2.
    lv_lines = lv_lines + 1.

    APPEND gs_bdc TO gt_bdc.

    CLEAR gs_amnt.
  ENDLOOP.

  LOOP AT gt_amnt INTO gs_amnt.
* because 1st record is the one where we double click in BDC to set the incoming amount.
    IF sy-tabix = 1  AND  gv_partial = 'X'.
      " No Need to add the partials for the root doc in BDC processing
      CONTINUE.
    ENDIF.

    CLEAR: gs_bdc, gs_openitems.
    LOOP AT gt_openitems INTO gs_openitems WHERE rootdocyear = gs_amnt-rootdocyear
                                           AND   docyear NE gs_amnt-rootdocyear.

      gs_bdc-fval1 = 'BELNR'.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = gs_openitems-doc_no
        IMPORTING
          output = gs_openitems-doc_no.

      CONCATENATE gs_openitems-doc_no gs_openitems-fisc_year gs_openitems-item_num INTO gs_bdc-fval2.
      lv_lines = lv_lines + 1.
      APPEND gs_bdc TO gt_bdc.
      CLEAR: gs_bdc, gs_openitems.
    ENDLOOP.

    CLEAR : gs_amnt.
  ENDLOOP.


  CLEAR: lv_index, lv_last, gs_bdc.
  DESCRIBE TABLE gt_bdc LINES lv_last.
  lv_f = lv_last / 18.
  lv_ceil = ceil( lv_f ).
  lv_floor = floor( lv_f ).


  DO lv_ceil TIMES.

    PERFORM bdc_dynpro      USING 'SAPMF05A' '0733'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'RF05A-SEL01(01)'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=PA'.

    lv_index = lv_index + 1.
    lv_cnt = 0.
    DO 18 TIMES.
      lv_exit = lv_exit + 1.
      lv_cnt = lv_cnt + 1.
      CLEAR gs_bdc.
      READ TABLE gt_bdc INTO gs_bdc INDEX lv_exit.
      IF sy-subrc IS INITIAL.

        CONCATENATE 'RF05A-FELDN(' lv_cnt ')' INTO gs_bdc-fnam1.
        CONCATENATE 'RF05A-SEL01(' lv_cnt ')' INTO gs_bdc-fnam2.

        PERFORM bdc_field       USING gs_bdc-fnam1
                                      gs_bdc-fval1.
        PERFORM bdc_field       USING gs_bdc-fnam2
                                      gs_bdc-fval2.
      ENDIF.
      IF lv_exit = lv_last.
        EXIT.
      ENDIF.

    ENDDO.


* select more items
    IF lv_index NE lv_ceil.
      PERFORM bdc_dynpro      USING 'SAPDF05X' '3100'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                   'DF05B-PSSKT(01)'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=SL'.
      PERFORM bdc_field       USING 'RF05A-ABPOS'
                                    '1'.
* next screen.

      PERFORM bdc_dynpro      USING 'SAPMF05A' '0710'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'RF05A-AGKON'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=SLB'.
      PERFORM bdc_field       USING 'RF05A-AGBUK'
                                    gs_header-bukrs.
      PERFORM bdc_field       USING 'RF05A-AGKON'
                                    gs_tab-kunnr.
      PERFORM bdc_field       USING 'RF05A-AGKOA'
                                    'D'.
      PERFORM bdc_field       USING 'RF05A-XNOPS'
                                    'X'.
      PERFORM bdc_field       USING 'RF05A-XFIFO'
                                    ''.
      PERFORM bdc_field       USING 'RF05A-XPOS1(01)'
                                    'X'.
    ENDIF.
  ENDDO.

* 5th screen

  CLEAR: lv_okcode.

  lv_okcode = '=PART'.
  lv_cursor = 'DF05B-PSZAH(01)'.

  PERFORM bdc_dynpro      USING 'SAPDF05X' '3100'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                lv_okcode.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                               'DF05B-PSSKT(01)'.
  PERFORM bdc_field       USING 'RF05A-ABPOS'
                                '1'.

  LOOP AT gt_amnt INTO gs_amnt.

    lv_abpos = sy-tabix.

    PERFORM bdc_dynpro      USING 'SAPDF05X' '3100'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  lv_cursor.
    PERFORM bdc_field       USING 'RF05A-ABPOS'
                                  lv_abpos.
    IF gs_amnt-disc_amnt GT 0 .
      PERFORM bdc_dynpro      USING 'SAPDF05X' '3100'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=Z+S'.
    ELSE.
      PERFORM bdc_dynpro      USING 'SAPDF05X' '3100'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=Z-S'.
    ENDIF.
  ENDLOOP.

  PERFORM bdc_dynpro      USING 'SAPDF05X' '3100'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                lv_cursor.
  PERFORM bdc_field       USING 'RF05A-ABPOS'
                                '1'.

  IF gv_excess NE 'X'.
* for partial or exact payment.
    PERFORM bdc_dynpro      USING 'SAPDF05X' '3100'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=PI'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                 'DF05B-PSZAH(01)'.
    PERFORM bdc_field       USING 'RF05A-ABPOS'
                                  '1'.

    PERFORM bdc_dynpro      USING 'SAPDF05X' '3100'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=BU'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'DF05B-PSZAH(01)'.
    PERFORM bdc_field       USING 'RF05A-ABPOS'
                                  '1'.

  ELSE. " for excess payment
    PERFORM bdc_dynpro      USING 'SAPDF05X' '3100'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=KMD'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                 'DF05B-PSZAH(01)'.
    PERFORM bdc_field       USING 'RF05A-ABPOS'
                                  '1'.

    PERFORM bdc_dynpro      USING 'SAPMF05A' '0700'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'RF05A-NEWKO'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=BU'.
    PERFORM bdc_field       USING 'RF05A-NEWBS'
                                  '11'.
    PERFORM bdc_field       USING 'RF05A-NEWKO'
                                  gs_tab-kunnr.

    PERFORM bdc_dynpro      USING 'SAPMF05A' '0301'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'BSEG-WRBTR'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '/00'.
    WRITE gv_wrbtr TO gv_value.                         "#EC UOM_IN_MES
    CONDENSE gv_value.
    PERFORM bdc_field       USING 'BSEG-WRBTR'
                                  gv_value.
    PERFORM bdc_field       USING 'BSEG-GSBER'
                                  gsber.
    CLEAR gv_value.
    CONCATENATE 'Excess Payment from Customer:' gs_tab-kunnr
    INTO  gv_value SEPARATED BY space.
    PERFORM bdc_field       USING 'BSEG-SGTXT'
                                  gv_value.


    PERFORM bdc_dynpro      USING 'SAPMF05A' '0301'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'BSEG-WRBTR'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=BU'.
  ENDIF.


  REFRESH messtab.
  CALL TRANSACTION 'FB05' WITHOUT AUTHORITY-CHECK
                   USING bdcdata
                   MODE   gv_mode
                   UPDATE 'A'
                   MESSAGES INTO messtab.

  WAIT UP TO 2 SECONDS.
  COMMIT WORK AND WAIT.

  CLEAR: gs_messtab, gs_message.

  READ TABLE messtab INTO gs_messtab WITH KEY msgtyp = 'S'
                                              msgnr = '312'.
  IF sy-subrc IS INITIAL.
    COMMIT WORK AND WAIT.

    DATA: lv_belnr TYPE belnr_d.
    DO 10 TIMES.
      SELECT SINGLE belnr FROM bkpf INTO lv_belnr
                          WHERE belnr = gs_messtab-msgv1.
      IF sy-subrc IS NOT INITIAL.
        WAIT UP TO 2 SECONDS.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

    DATA: lt_rfpos TYPE STANDARD TABLE OF rfpos.
    DATA: l_doc1  TYPE ebeln, l_doc2 TYPE ebeln,
          l_doc3  TYPE ebeln, l_doc4 TYPE ebeln, l_doc5 TYPE ebeln, l_doc6 TYPE ebeln,
          l_doc7  TYPE ebeln, l_doc8 TYPE ebeln, l_doc9 TYPE ebeln, l_doc10 TYPE ebeln.
    CLEAR lt_rfpos[].
    CALL FUNCTION 'CUSTOMER_OPEN_ITEMS'
      EXPORTING
        bukrs    = gs_header-bukrs
        kunnr    = gs_tab-kunnr
      TABLES
        t_postab = lt_rfpos.

    IF sy-subrc <> 0.
      RAISE no_open_items.
    ENDIF.
    SORT lt_rfpos BY ebeln.
    LOOP AT gt_amnt INTO gs_amnt.
      CLEAR : gs_message-wrbtr, gs_message-belnr.
      gs_message-mess_type = 'S'.
      MOVE-CORRESPONDING gs_tab TO gs_message.
      gs_message-belnr = gs_amnt-root_doc.
      CONCATENATE 'Payment Document:' gs_messtab-msgv1 'Posted Successfully' INTO
                  gs_message-message  SEPARATED BY space.
      READ TABLE lt_rfpos INTO DATA(lw_rfpos) WITH KEY belnr = gs_amnt-root_doc.
      IF sy-subrc <> 0.
        IF l_doc1 IS INITIAL.
          l_doc1 = gs_amnt-root_doc.
        ELSEIF l_doc2 IS INITIAL.
          l_doc2 = gs_amnt-root_doc.
        ELSEIF l_doc3 IS INITIAL.
          l_doc3 = gs_amnt-root_doc.
        ELSEIF l_doc4 IS INITIAL.
          l_doc4 = gs_amnt-root_doc.
        ELSEIF l_doc5 IS INITIAL.
          l_doc5 = gs_amnt-root_doc.
        ELSEIF l_doc6 IS INITIAL.
          l_doc6 = gs_amnt-root_doc.
        ELSEIF l_doc7 IS INITIAL.
          l_doc7 = gs_amnt-root_doc.
        ELSEIF l_doc8 IS INITIAL.
          l_doc8 = gs_amnt-root_doc.
        ELSEIF l_doc9 IS INITIAL.
          l_doc9 = gs_amnt-root_doc.
        ELSEIF l_doc10 IS INITIAL.
          l_doc10 = gs_amnt-root_doc.
        ENDIF.
      ENDIF.
      APPEND gs_message TO gt_message.

    ENDLOOP.
*** Based on Document Number Generation updating the Log Table ***

    IF budat+4(2) LE 3.
      DATA(lv_fiscal_yr) = budat(4) - 1.
    ELSE.
      lv_fiscal_yr = budat(4).
    ENDIF.

    SELECT SINGLE * FROM zfi_dms_cust_pay INTO @DATA(l_cust_pay)
      WHERE bukrs = 'DMS1'
      AND kunnr = @kunnr
      AND distributor = @distb.
    DATA(ls_table1) = VALUE zdms_deb_ref_mis( mandt = sy-mandt
                                           bukrs = bukrs
                                           doc_no = gs_messtab-msgv1
                                           gjahr = lv_fiscal_yr
                                           account_type = 'D'
                                           account = kunnr
                                           invoice_refno = gs_header-xblnr
                                           distributor = distb
                                           werks = l_cust_pay-werks
                                           gsber = l_cust_pay-gsber
                                           hkont = l_cust_pay-hkont
                                           amount = wrbtr
                                           ernam = sy-uname
                                           erdat = sy-datum
                                           remarks = gs_header-xtext
                                           pay_reference = gs_header-xblnr
                                           doc_type = 'DZ'
                                           actual_doc1 = l_doc1
                                           actual_doc2 = l_doc2
                                           actual_doc3 = l_doc3
                                           actual_doc4 = l_doc4
                                           actual_doc5 = l_doc5
                                           actual_doc6 = l_doc6
                                           actual_doc7 = l_doc7
                                           actual_doc8 = l_doc8
                                           actual_doc9 = l_doc9
                                           actual_doc10 = l_doc10
                                           reference_id = refid
                                           ).
    MODIFY zdms_deb_ref_mis FROM ls_table1.
    g_doc_no = gs_messtab-msgv1.

*     PERFORM progress_indicator USING 'Posting and clearing' 'for Customer:' gs_tab-kunnr 'complete'.

  ELSE.

    READ TABLE messtab INTO gs_messtab WITH KEY msgtyp = 'E'.
    IF sy-subrc = 0 .
      p_lv_check = 'E'.
      lv_mvar1 = gs_messtab-msgv1.
      lv_mvar2 = gs_messtab-msgv2.
      lv_mvar3 = gs_messtab-msgv3.
      lv_mvar4 = gs_messtab-msgv4.
      CALL FUNCTION 'MESSAGE_PREPARE'
        EXPORTING
          language               = sy-langu
          msg_id                 = gs_messtab-msgid
          msg_no                 = gs_messtab-msgnr
          msg_var1               = lv_mvar1
          msg_var2               = lv_mvar2
          msg_var3               = lv_mvar3
          msg_var4               = lv_mvar4
        IMPORTING
          msg_text               = gs_message-message
        EXCEPTIONS
          function_not_completed = 1
          message_not_found      = 2
          OTHERS                 = 3.
      gs_message-mess_type = 'E'.
      APPEND gs_message TO gt_message.
    ELSE.

      LOOP AT messtab INTO gs_messtab WHERE msgtyp = 'S' .
        lv_mvar1 = gs_messtab-msgv1.
        lv_mvar2 = gs_messtab-msgv2.
        lv_mvar3 = gs_messtab-msgv3.
        lv_mvar4 = gs_messtab-msgv4.
        CALL FUNCTION 'MESSAGE_PREPARE'
          EXPORTING
            language               = sy-langu
            msg_id                 = gs_messtab-msgid
            msg_no                 = gs_messtab-msgnr
            msg_var1               = lv_mvar1
            msg_var2               = lv_mvar2
            msg_var3               = lv_mvar3
            msg_var4               = lv_mvar4
          IMPORTING
            msg_text               = gs_message-message
          EXCEPTIONS
            function_not_completed = 1
            message_not_found      = 2
            OTHERS                 = 3.
        gs_message-mess_type = 'W'.
        APPEND gs_message TO gt_message.
        CLEAR gs_messtab.
      ENDLOOP.
    ENDIF.
  ENDIF.


  FREE :bdcdata[].
  CLEAR: gs_message.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_create_credit_note
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_create_credit_note .

  DATA: lv_objtyp TYPE bapiache09-obj_type.
  DATA: lv_objkey TYPE bapiache09-obj_key.
  DATA: lv_objsys TYPE bapiache09-obj_sys.
  DATA: lv_msg_text TYPE string.
  DATA: lt_glaccount  TYPE TABLE OF bapiacgl09,
        lt_payable    TYPE TABLE OF bapiacap09,
        lt_recievable TYPE TABLE OF bapiacar09,
        lt_curramnt   TYPE TABLE OF bapiaccr09,
        lt_return     TYPE TABLE OF bapiret2,
        lv_bktxt      TYPE bktxt.

  IF budat+4(2) LE 3.
    DATA(lv_fiscal_yr) = budat(4) - 1.
  ELSE.
    lv_fiscal_yr = budat(4).
  ENDIF.
  DATA(l_prev_fyear) = lv_fiscal_yr - 1.

  LOOP AT gt_crnote_log ASSIGNING FIELD-SYMBOL(<gs_crnote_log>).
    CLEAR: lv_objtyp,
           lv_objkey,
           lv_objsys,
           lt_glaccount,
           lt_payable,
           lt_curramnt,
           lt_return.

    IF <gs_crnote_log>-dgamt GE 0.
      DATA(l_ref_no) = <gs_crnote_log>-invno.
      IF l_ref_no IS INITIAL.
        l_ref_no = <gs_crnote_log>-acdno.
      ENDIF.

*********************get the number range*******************
      SELECT SINGLE * FROM zfi_dms_no_serie INTO @DATA(ls_series)
                                            WHERE distributor = @<gs_crnote_log>-distr
                                            AND   plant       = @<gs_crnote_log>-gsber
                                            AND   gjahr       = @lv_fiscal_yr
                                            AND   doc_type    = 'CN'.
      IF sy-subrc <> 0.
        ls_series-distributor = <gs_crnote_log>-distr.
        ls_series-gjahr       = lv_fiscal_yr.
        ls_series-mandt       = sy-mandt.
        ls_series-plant       = <gs_crnote_log>-gsber.
        ls_series-doc_type    = 'CN'.
      ENDIF.
      ls_series-num_range   = ls_series-num_range + 1.
************number series**************
      lv_bktxt = |{ ls_series-plant }{ ls_series-gjahr }{ ls_series-doc_type }{ ls_series-num_range }|.


      DATA(l_headers) = VALUE bapiache09( bus_act = 'RFBU'
                                          username = sy-uname
                                          comp_code = 'DMS1'
                                          doc_date = budat
                                          pstng_date = budat
                                          ref_doc_no = l_ref_no
                                          fisc_year = lv_fiscal_yr
                                          doc_type = 'DG'
                                          header_txt = lv_bktxt ).

      lt_recievable = VALUE #( ( itemno_acc = '1'
                                 customer = kunnr
                                 item_text = |{ l_ref_no }-Credit Note @{ <gs_crnote_log>-dgper }%|
                                 bus_area = gsber  ) ).

      lt_glaccount  = VALUE #( ( itemno_acc = '2'
                                 gl_account = <gs_crnote_log>-hkont
                                 item_text = |{ l_ref_no }-Credit Note @{ <gs_crnote_log>-dgper }%|
                                 bus_area = gsber ) ).

      lt_curramnt = VALUE #( (   itemno_acc = '1'
                                 currency = 'INR'
                                 amt_doccur =  <gs_crnote_log>-dgamt * -1 )
                             (   itemno_acc = '2'
                                 currency = 'INR'
                                 amt_doccur = <gs_crnote_log>-dgamt  ) ).
*      *** Document Check Before posting ***
      CALL FUNCTION 'BAPI_ACC_DOCUMENT_CHECK'
        EXPORTING
          documentheader    = l_headers
        TABLES
          accountgl         = lt_glaccount
          accountreceivable = lt_recievable
*         accountpayable    = lt_payable
          currencyamount    = lt_curramnt
          return            = lt_return.
      READ TABLE lt_return INTO DATA(lw_ret) WITH KEY type = 'E'.
      IF sy-subrc EQ 0.
        CLEAR lv_msg_text.
        LOOP AT lt_return INTO DATA(l_ret) WHERE ( type = 'E' OR type = 'A' ).
          lv_msg_text = |{ lv_msg_text },{ l_ret-message }|.
        ENDLOOP.
        <gs_crnote_log>-msgtyp = 'E'.
        <gs_crnote_log>-message = lv_msg_text.
      ELSE.
        REFRESH: lt_return.
        CLEAR: lv_objkey,lv_objsys,lv_objtyp.
*** Function Module to create Credit note ***
        CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
          EXPORTING
            documentheader    = l_headers
          IMPORTING
            obj_type          = lv_objtyp
            obj_key           = lv_objkey
            obj_sys           = lv_objsys
          TABLES
            accountgl         = lt_glaccount
            accountreceivable = lt_recievable
*           accountpayable    = lt_payable
            currencyamount    = lt_curramnt
            return            = lt_return.
        COMMIT WORK AND WAIT.
        <gs_crnote_log>-dgdoc = lv_objkey+0(10).
        <gs_crnote_log>-dgdyr = lv_fiscal_yr.
        <gs_crnote_log>-msgtyp = 'S'.
        <gs_crnote_log>-crflag = 'X'.
        <gs_crnote_log>-message = |Document { <gs_crnote_log>-dgdoc } Posted successfully|.
        <gs_crnote_log>-new_docno = lv_bktxt.
******************update the next number range***********
        MODIFY zfi_dms_no_serie FROM ls_series.
        COMMIT WORK.
      ENDIF.
      CLEAR : ls_series,lv_bktxt.

    ELSE.
      <gs_crnote_log>-msgtyp = 'S'.
      <gs_crnote_log>-message = |Discount amount is zero|.
    ENDIF.

  ENDLOOP.

  IF gt_crnote_log[] IS NOT INITIAL.
    MODIFY zdms_crnote_log FROM TABLE gt_crnote_log.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_excess_payment
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- CHECK
*&---------------------------------------------------------------------*
FORM f_excess_payment  CHANGING p_lv_check.

  DATA: lv_objtyp TYPE bapiache09-obj_type.
  DATA: lv_objkey TYPE bapiache09-obj_key.
  DATA: lv_objsys TYPE bapiache09-obj_sys.
  DATA: lt_glaccount  TYPE TABLE OF bapiacgl09,
        lt_payable    TYPE TABLE OF bapiacap09,
        lt_recievable TYPE TABLE OF bapiacar09,
        lt_curramnt   TYPE TABLE OF bapiaccr09,
        lt_return     TYPE TABLE OF bapiret2.
  DATA: lv_belnr TYPE belnr_d.
  DATA: lv_mvar1 TYPE balm-msgv1,
        lv_mvar2 TYPE balm-msgv1,
        lv_mvar3 TYPE balm-msgv1,
        lv_mvar4 TYPE balm-msgv1.

  DATA(l_headers) = VALUE bapiache09( bus_act    = 'RFBU'
                                      username   = sy-uname
                                      comp_code  = bukrs
                                      doc_date   = bldat
                                      pstng_date = budat
                                      ref_doc_no = xblnr
                                      fisc_year  = gjahr
                                      doc_type   = 'DZ' ).

  REFRESH: lt_glaccount,lt_curramnt,lt_return.

  lt_recievable = VALUE #( ( itemno_acc = '1'
                             customer   = kunnr
                             item_text  = bktxt
                             bus_area   = gsber  ) ).

  lt_glaccount = VALUE #( ( itemno_acc = '2'
                            gl_account = newko
                            item_text  = xtext
                            bus_area   = gsber ) ).

  lt_curramnt = VALUE #( ( itemno_acc = '1'
                           currency   = 'INR'
                           amt_doccur = gv_wrbtr * -1 )
                         ( itemno_acc = '2'
                           currency   = 'INR'
                           amt_doccur = gv_wrbtr ) ).

*** Document Check Before posting ***
  CALL FUNCTION 'BAPI_ACC_DOCUMENT_CHECK'
    EXPORTING
      documentheader    = l_headers
    TABLES
      accountgl         = lt_glaccount
      accountreceivable = lt_recievable
      accountpayable    = lt_payable
      currencyamount    = lt_curramnt
      return            = lt_return.
  READ TABLE lt_return INTO DATA(lw_ret) WITH KEY type = 'E'.
  IF sy-subrc EQ 0.
    DATA lv_msg_text TYPE string.
    LOOP AT lt_return INTO DATA(l_ret) WHERE ( type = 'E' OR type = 'A' ).
      lv_msg_text = |{ lv_msg_text },{ l_ret-message }|.
    ENDLOOP.
    p_lv_check = 'E'.
    gs_message-mess_type = 'E'.
    gs_message-message = lv_msg_text.
    APPEND gs_message TO gt_message.
  ELSE.
    REFRESH: lt_return.
    CLEAR: lv_objkey,lv_objsys,lv_objtyp.
*** Function Module to create Debit note ***
    CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
      EXPORTING
        documentheader    = l_headers
      IMPORTING
        obj_type          = lv_objtyp
        obj_key           = lv_objkey
        obj_sys           = lv_objsys
      TABLES
        accountgl         = lt_glaccount
        accountreceivable = lt_recievable
        accountpayable    = lt_payable
        currencyamount    = lt_curramnt
        return            = lt_return.
    COMMIT WORK AND WAIT.
    CLEAR lv_belnr.
    lv_belnr         = lv_objkey+0(10).
*    p_lv_check = 'E'.
    gs_message-mess_type = 'S'.
    MOVE-CORRESPONDING gs_tab TO gs_message.
    IF gs_tab-belnr IS INITIAL.
      gs_message-belnr = lv_belnr.
    ENDIF.
    gs_message-wrbtr = gv_wrbtr.
    CONCATENATE lv_belnr 'posted successfully as Excess Payment' INTO
                gs_message-message  SEPARATED BY space.
    gs_message-mess_type = 'S'.
    APPEND gs_message TO gt_message.

    IF budat+4(2) LE 3.
      DATA(lv_fiscal_yr) = budat(4) - 1.
    ELSE.
      lv_fiscal_yr = budat(4).
    ENDIF.
    SELECT SINGLE * FROM zfi_dms_cust_pay INTO @DATA(l_cust_pay)
      WHERE bukrs = 'DMS1'
      AND kunnr = @kunnr
      AND distributor = @distb.
    IF lv_belnr IS NOT INITIAL.

      DATA(ls_table1) = VALUE zdms_deb_ref_mis( mandt = sy-mandt
                                             bukrs = bukrs
                                             doc_no = lv_belnr
                                             gjahr = lv_fiscal_yr
                                             account_type = 'D'
                                             account = kunnr
                                             invoice_refno = gs_header-xblnr
                                             distributor = distb
                                             werks = l_cust_pay-werks
                                             gsber = l_cust_pay-gsber
                                             hkont = l_cust_pay-hkont
                                             amount = wrbtr
                                             ernam = sy-uname
                                             erdat = sy-datum
                                             erzet = sy-uzeit
                                             remarks = gs_header-xtext
                                             pay_reference = gs_header-xblnr
                                             doc_type = 'DZ'
                                             reference_id = refid
                                             ).
      MODIFY zdms_deb_ref_mis FROM ls_table1.
      g_doc_no = lv_belnr.
    ENDIF.
  ENDIF.
ENDFORM.
