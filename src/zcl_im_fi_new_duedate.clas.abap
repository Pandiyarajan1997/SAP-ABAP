class ZCL_IM_FI_NEW_DUEDATE definition
  public
  final
  create public .

public section.

  interfaces IF_EX_FI_ITEMS_CH_DATA .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_FI_NEW_DUEDATE IMPLEMENTATION.


  METHOD if_ex_fi_items_ch_data~change_items.

    DATA: lt_caltax TYPE TABLE OF rtax1u15.
    DATA: lv_netamt  TYPE wrbtr,
          lv_igst    TYPE wrbtr,
          lv_cgst    TYPE wrbtr,
          lv_sgst    TYPE wrbtr,
          lv_tax_amt TYPE fwbas,
          lv_taxamt  TYPE fwbas,
          l_itax     TYPE hwste,
          l_ctax     TYPE hwste,
          l_stax     TYPE hwste,
          l_tds      TYPE wt_wt.

    IF sy-tcode EQ 'FBL1N'.
*** Tax Amount For Documents ***
      SELECT bukrs,
             belnr,
             gjahr,
             buzei,
             mwskz,
             shkzg,
             hwbas,
             hwste,
             kschl FROM bset INTO TABLE @DATA(lt_bset)
                   FOR ALL ENTRIES IN @ct_items
                   WHERE bukrs = @ct_items-bukrs
                   AND belnr = @ct_items-belnr
                   AND gjahr = @ct_items-gjahr
                   AND kschl IN ( 'JIIG', 'JICG', 'JISG', 'JICR', 'JIIR', 'JISR',
                                  'JOIG', 'JOCG', 'JOSG' ).
      IF sy-subrc EQ 0.
        SORT lt_bset[] BY bukrs belnr gjahr.
      ENDIF.

      LOOP AT ct_items ASSIGNING FIELD-SYMBOL(<fs_items>).
        CLEAR: lv_igst,lv_cgst,lv_sgst,lv_taxamt.
        LOOP AT lt_bset ASSIGNING FIELD-SYMBOL(<fs_bset>) WHERE bukrs = <fs_items>-bukrs
                                                          AND belnr = <fs_items>-belnr
                                                          AND gjahr = <fs_items>-gjahr.
          CASE <fs_bset>-kschl.
              "Integrated GST
            WHEN 'JIIG' OR 'JIIR' OR 'JOIG'.
              CLEAR: l_itax.
              IF <fs_bset>-shkzg EQ 'H'.
                l_itax = <fs_bset>-hwste * -1.
              ELSE.
                l_itax = <fs_bset>-hwste.
              ENDIF.
              lv_igst = lv_igst + l_itax.
              lv_taxamt = lv_taxamt + <fs_bset>-hwbas.
              "Central GST
            WHEN 'JICG' OR 'JICR' OR 'JOCG'.
              CLEAR l_ctax.
              IF <fs_bset>-shkzg EQ 'H'.
                l_ctax = <fs_bset>-hwste * -1.
              ELSE.
                l_ctax = <fs_bset>-hwste.
              ENDIF.
              lv_cgst = lv_cgst + l_ctax.
              LOOP AT lt_bset ASSIGNING FIELD-SYMBOL(<fs_bset1>) WHERE bukrs = <fs_items>-bukrs
                                                                               AND belnr = <fs_items>-belnr
                                                                               AND gjahr = <fs_items>-gjahr
                                                                               AND kschl = 'JICG' OR kschl = 'JOCG'.
                lv_taxamt = lv_taxamt + <fs_bset1>-hwbas.
              ENDLOOP.
              "State GST
            WHEN 'JISG' OR 'JISR' OR 'JOSG'.
              CLEAR l_stax.
              IF <fs_bset>-shkzg EQ 'H'.
                l_stax = <fs_bset>-hwste * -1.
              ELSE.
                l_stax = <fs_bset>-hwste.
              ENDIF.
              lv_sgst = lv_sgst + l_stax.
          ENDCASE.
        ENDLOOP.
        IF sy-subrc NE 0.
          <fs_items>-taxable_value = <fs_items>-dmshb.
        ELSE.
          <fs_items>-igst = lv_igst.
          <fs_items>-cgst = lv_cgst.
          <fs_items>-sgst = lv_sgst .
*          ENDIF.
          DATA(lv_tottax) =  lv_igst + lv_cgst  + lv_sgst.

*          IF <fs_items>-blart EQ 'KG'.
*
*          ELSE.
*            lv_tottax_tmp = lv_tottax.
*          ENDIF.

          DATA(lv_tottax_tmp) =  CONV char25( lv_tottax ).
          CONDENSE lv_tottax_tmp NO-GAPS.
          REPLACE ALL OCCURRENCES OF '-' IN lv_tottax_tmp WITH ''.
*Tds Amount
          SELECT wt_qbshh FROM with_item INTO TABLE @DATA(lt_tds)
               WHERE bukrs = @<fs_items>-bukrs
               AND belnr = @<fs_items>-belnr
               AND gjahr = @<fs_items>-gjahr.
          IF sy-subrc EQ 0.
            CLEAR l_tds.
            LOOP AT lt_tds INTO DATA(lw_tds).
              l_tds = l_tds + lw_tds-wt_qbshh.
            ENDLOOP.
            <fs_items>-tds = l_tds.
          ENDIF.
*Tcs Amount
          SELECT SINGLE dmbtr FROM bseg INTO @DATA(lv_tcs)
             WHERE bukrs = @<fs_items>-bukrs
             AND belnr = @<fs_items>-belnr
             AND gjahr = @<fs_items>-gjahr
             AND hkont = '0026644400'.

          IF lv_igst IS INITIAL AND lv_cgst IS INITIAL AND lv_sgst IS INITIAL.
            <fs_items>-taxable_value = <fs_items>-dmshb.
          ELSE.
*            IF <fs_items>-shkzg EQ 'H'.
*              DATA(lv_dmshb) = <fs_items>-dmshb * -1.
*            ELSE.
*              lv_dmshb = <fs_items>-dmshb.
*            ENDIF.
            DATA(lv_dmshb) = CONV char25( <fs_items>-dmshb ).
            CONDENSE lv_dmshb NO-GAPS.
            REPLACE ALL OCCURRENCES OF '-' IN lv_dmshb WITH ''.

            <fs_items>-taxable_value = lv_dmshb - lv_tottax_tmp - lv_tcs + l_tds.
            CLEAR: lv_tcs,l_tds,lv_tottax_tmp,lv_dmshb,lv_igst,lv_cgst,lv_igst,lv_tottax.
          ENDIF.
        ENDIF.

*Based on Holidays Calculation ***
        SELECT SINGLE hdate FROM zfi_due_dates INTO @DATA(l_holiday)
                    WHERE hdate = @<fs_items>-faedt.
        IF sy-subrc NE 0.
          <fs_items>-zdue_date = <fs_items>-faedt.
          CONTINUE.
        ELSE.
          DATA(lv_ddate_tmp) = CONV bldat( <fs_items>-faedt + 1 ).
          DO .
            SELECT SINGLE hdate FROM zfi_due_dates INTO l_holiday
            WHERE hdate = lv_ddate_tmp.
            IF sy-subrc NE 0.
              <fs_items>-zdue_date = lv_ddate_tmp.
              EXIT.
            ELSE.
              lv_ddate_tmp = lv_ddate_tmp + 1.
            ENDIF.
          ENDDO.
          <fs_items>-zdue_date = lv_ddate_tmp.
        ENDIF.
        CLEAR lv_ddate_tmp.
        CLEAR: lv_igst,lv_cgst,lv_sgst,lv_taxamt,lv_tcs.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
