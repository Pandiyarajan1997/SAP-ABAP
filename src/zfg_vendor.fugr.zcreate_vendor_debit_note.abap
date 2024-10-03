FUNCTION zcreate_vendor_debit_note.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(COMP_CODE) TYPE  BUKRS
*"     VALUE(VENDOR) TYPE  LIFNR
*"     VALUE(DATE) TYPE  DATUM
*"     VALUE(ORG_INV) TYPE  BELNR_D
*"     VALUE(INVOICE_REFNO) TYPE  XBLNR1
*"     VALUE(AMOUNT) TYPE  WRBTR
*"     VALUE(DISC_DATE) TYPE  DATUM
*"     VALUE(DIS_PERIOD) TYPE  ZDIS_PER
*"     VALUE(POSTING_DATE) TYPE  BUDAT
*"  EXPORTING
*"     VALUE(DOCUMENT_NO) TYPE  BELNR_D
*"     VALUE(AMOUNTS) TYPE  WRBTR
*"  TABLES
*"      LT_RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"  EXCEPTIONS
*"      ENTER_COMP_CODE
*"      ENTER_VENDOR
*"      ENTER_INREFNO
*"      ENTER_AMOUNT
*"      ENTER_DATE
*"      ENTER_DISDATE
*"      ENTER_DISPERIOD
*"      INCORRECT_INVREFNO
*"      ENTER_ORGINV
*"      ENTER_PDATE
*"      REGION_MISSING
*"----------------------------------------------------------------------
*& Created on: 16.11.2022
*& Created by: Samsudeen M
*& Reference by: Ramakrishnan J
*& Purpose: It helps us to create FB65 Debit Note Entry Based on input
**************************************************************************
  TYPES: BEGIN OF output,
           belnr TYPE belnr_d,
           bukrs TYPE bukrs,
           gjahr TYPE gjahr,
         END OF output.
  DATA: ls_docno TYPE output.
  DATA:lv_amount TYPE wrbtr.
  DATA: lv_date  TYPE char10,
        lv_date1 TYPE char10.   "Posting Date
  DATA: lv_taxcode TYPE mwskz. "Tax code
  DATA: lv_glacc TYPE saknr. "GL Account
  DATA: lv_cost TYPE kostl. "Costcenter
  DATA lv_msg_text TYPE string.
  DATA: lt_seltab TYPE TABLE OF rsparams.
  DATA: arc_params   TYPE arc_params,
        print_params TYPE pri_params,
        valid        TYPE c.

  DATA:e_fwnav TYPE bset-fwste,
       e_fwnvv TYPE bset-fwste,
       e_fwste TYPE bset-fwste,
       e_fwast TYPE bset-fwste.

  DATA: lt_glaccount TYPE TABLE OF bapiacgl09.
  DATA: lt_accountpayable TYPE TABLE OF bapiacap09.
  DATA: lt_curramnt TYPE TABLE OF bapiaccr09.
  DATA: return TYPE STANDARD TABLE OF bapiret2
                                         WITH HEADER LINE.
  DATA: lv_objtyp TYPE bapiache09-obj_type.
  DATA: lv_objkey TYPE bapiache09-obj_key.
  DATA: lv_objsys TYPE bapiache09-obj_sys.
  DATA: lt_tax TYPE TABLE OF bapiactx09.
  DATA: lt_caltax TYPE TABLE OF rtax1u15.
  DATA: lv_tottamt TYPE wrbtr.
  DATA: length TYPE i.
  DATA: lv_refno TYPE xblnr1.

  FIELD-SYMBOLS <wa_return> TYPE bapiret2.

  REFRESH: lt_seltab.
**** Gl Account Number for Bank Charges ***
  SELECT SINGLE * FROM tvarvc INTO @DATA(ls_tvarvc)
                  WHERE name = 'ZGLACC_DEBIT'
                  AND type = 'P'.
  IF sy-subrc EQ 0.
    CLEAR lv_glacc.
    lv_glacc = ls_tvarvc-low.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_glacc
      IMPORTING
        output = lv_glacc.
  ENDIF.
*** Cost Center for Debit Note ***
  SELECT SINGLE * FROM tvarvc INTO @DATA(ls_tvarvc1)
                  WHERE name = 'ZCOSTCENTER_DEBIT'
                  AND type = 'P'.
  IF sy-subrc EQ 0.
    CLEAR lv_cost.
    lv_cost = ls_tvarvc1-low.
  ENDIF.
** Company code **
  IF comp_code IS INITIAL.
    RAISE enter_comp_code.
  ENDIF.
** Vendor Number **
  IF vendor IS NOT INITIAL.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = vendor
      IMPORTING
        output = vendor.
** based on vendor Input get the region of vendor **
    SELECT SINGLE lifnr, name1, regio,adrnr FROM lfa1 INTO @DATA(ls_lfa1)
           WHERE lifnr = @vendor.
    IF sy-subrc EQ 0.
      SELECT SINGLE smtp_addr FROM adr6 INTO @DATA(lv_mail)
                    WHERE addrnumber = @ls_lfa1-adrnr.
      IF ls_lfa1-regio IS NOT INITIAL.
        IF ls_lfa1-regio = 'TN'.
          CLEAR lv_taxcode.
          lv_taxcode = 'GD'.
        ELSE.
          CLEAR lv_taxcode.
          lv_taxcode = 'GE'.
        ENDIF.
      ELSE.
        RAISE region_missing.
      ENDIF.
    ENDIF.
  ELSE.
    RAISE enter_vendor.
  ENDIF.

  DATA: lv_fisyear TYPE bapi0002_4-fiscal_year,
        lv_month   TYPE bapi0002_4-fiscal_period,
        l_return   TYPE bapireturn1.

  CLEAR: lv_fisyear,lv_month,l_return.

*** function module to get fiscal year ***
  CALL FUNCTION 'BAPI_COMPANYCODE_GET_PERIOD'
    EXPORTING
      companycodeid = comp_code
      posting_date  = posting_date
    IMPORTING
      fiscal_year   = lv_fisyear
      fiscal_period = lv_month
      return        = l_return.
*** Original document number check ***
  IF org_inv IS INITIAL.
    RAISE enter_orginv.
  ENDIF.

*** Reference Document Number ***
  IF invoice_refno IS NOT INITIAL.
    CLEAR lv_refno.
    length = strlen( invoice_refno ).
*** getting original invoice Document number ***
    IF length LE 13.
      lv_refno = |{ invoice_refno }{ '-BC' }|.
      TRANSLATE lv_refno TO UPPER CASE.
      SELECT SINGLE * FROM bsik
                      INTO @DATA(ls_bsik)
                      WHERE bukrs = @comp_code
                      AND lifnr = @vendor
                      AND gjahr = @lv_fisyear
                      AND xblnr = @lv_refno
                      AND blart = 'KG'.
      IF sy-subrc = 0.
        RAISE incorrect_invrefno.
      ELSE.
        SELECT SINGLE * FROM zdebit_ref_mis
                        INTO @DATA(l_log)
                        WHERE bukrs = @comp_code
                        AND account = @vendor
                        AND invoice_refno = @lv_refno
                        AND gjahr = @lv_fisyear
                        AND doc_type = 'KG'.
        IF sy-subrc = 0.
          RAISE incorrect_invrefno.
        ELSE.
          SELECT SINGLE * FROM bsak
                          INTO @DATA(ls_bsak)
                          WHERE bukrs = @comp_code
                          AND lifnr = @vendor
                          AND xblnr = @lv_refno
                          AND gjahr = @lv_fisyear
                          AND blart = 'KG'.
          IF sy-subrc EQ 0.
            RAISE incorrect_invrefno.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      CLEAR lv_refno.
      lv_refno = invoice_refno.
      TRANSLATE lv_refno TO UPPER CASE.
      SELECT SINGLE * FROM zdebit_ref_mis
                      INTO @l_log
                      WHERE account = @vendor
                      AND invoice_refno = @lv_refno
                      AND doc_type = 'KG'.
      IF sy-subrc = 0.
        RAISE incorrect_invrefno.
      ELSE.
        SELECT SINGLE * FROM bsik
                        INTO @ls_bsik
                        WHERE bukrs = @comp_code
                        AND lifnr = @vendor
                        AND gjahr = @lv_fisyear
                        AND xblnr = @lv_refno
                        AND blart = 'KG'.
        IF sy-subrc = 0.
          RAISE incorrect_invrefno.
        ELSE.
          SELECT SINGLE * FROM bsak
                          INTO @ls_bsak
                          WHERE bukrs = @comp_code
                          AND lifnr = @vendor
                          AND gjahr = @lv_fisyear
                          AND xblnr = @lv_refno
                          AND blart = 'KG'.
          IF sy-subrc = 0.
            RAISE incorrect_invrefno.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ELSE.
    RAISE enter_inrefno.
  ENDIF.

*** Debit note Amount ***
  IF amount IS NOT INITIAL.
    CLEAR lv_amount.
    lv_amount = amount.
*    SHIFT lv_amount LEFT DELETING LEADING ''.
  ELSE.
    RAISE enter_amount.
  ENDIF.

*** Posting Date ***
  IF date IS INITIAL.
    RAISE enter_date.
  ENDIF.
*** Discount Date **
  IF disc_date IS INITIAL.
    RAISE enter_disdate.
  ENDIF.
** Discount Period ***
  IF dis_period IS INITIAL.
    RAISE enter_disperiod.
  ENDIF.

  IF posting_date IS INITIAL.
    RAISE enter_pdate.
  ENDIF.
  CLEAR: e_fwnav,e_fwnvv,e_fwste,e_fwast.
*** Calculate tax amount ***
  CALL FUNCTION 'CALCULATE_TAX_FROM_NET_AMOUNT'
    EXPORTING
      i_bukrs           = comp_code
      i_mwskz           = lv_taxcode
      i_waers           = 'INR'
      i_wrbtr           = lv_amount
    IMPORTING
      e_fwnav           = e_fwnav
      e_fwnvv           = e_fwnvv
      e_fwste           = e_fwste
      e_fwast           = e_fwast
    TABLES
      t_mwdat           = lt_caltax
    EXCEPTIONS
      bukrs_not_found   = 1
      country_not_found = 2
      mwskz_not_defined = 3
      mwskz_not_valid   = 4
      ktosl_not_found   = 5
      kalsm_not_found   = 6
      parameter_error   = 7
      knumh_not_found   = 8
      kschl_not_found   = 9
      unknown_error     = 10
      account_not_found = 11
      txjcd_not_valid   = 12
      tdt_error         = 13
      txa_error         = 14
      OTHERS            = 15.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
  DATA(lv_sgst) = VALUE #( lt_caltax[ kschl = 'JOSG' ]-wmwst OPTIONAL ).
  DATA(lv_cgst) = VALUE #( lt_caltax[ kschl = 'JOCG' ]-wmwst OPTIONAL ).
  DATA(lv_igst) = VALUE #( lt_caltax[ kschl = 'JOIG' ]-wmwst OPTIONAL ).
  DESCRIBE TABLE lt_caltax LINES DATA(lv_cnt).


  DATA(ls_header) = VALUE bapiache09( bus_act = 'RFBU'
                                      username = sy-uname
                                      comp_code = comp_code
                                      doc_date = date
                                      pstng_date = posting_date
                                      fisc_year = lv_fisyear
                                      doc_type = 'KG'
                                      ref_doc_no = lv_refno ).

  REFRESH: lt_glaccount,lt_accountpayable,lt_curramnt,lt_tax.
** Vendor line entry ***
  lt_accountpayable = VALUE #(
                     ( itemno_acc = '1'
                       vendor_no = vendor
                       bline_date = date
                       item_text = 'Discount Charges'
*                      pymt_cur = 'INR'
                       tax_code = lv_taxcode
                       businessplace = 'TN01' ) ).

  DATA lv_item TYPE i.
  CLEAR lv_item.
  lv_item = '3'.
*** Gl Account Entries ***
  lt_glaccount = VALUE #(
                     ( itemno_acc = '2'
                       gl_account = lv_glacc
                       tax_code = lv_taxcode
                       itemno_tax = '000001') ).

  CLEAR amounts.
  IF lv_igst IS NOT INITIAL.
    amounts = amount + lv_igst.
  ELSEIF lv_cgst IS NOT INITIAL.
    amounts = amount + lv_sgst + lv_cgst.
  ENDIF.
*** Currency Amount details filling table ***
  lt_curramnt = VALUE #(
                     ( itemno_acc = '1'
                       currency = 'INR'
                       amt_doccur = amounts
                       amt_base = amounts  )
                     ( itemno_acc = '2'
                       currency = 'INR'
                       amt_doccur = amount * -1
                       amt_base =  amount * -1
                       ) ).
  lv_item = '3'.
  LOOP AT lt_caltax INTO DATA(ls_caltax1).
    DATA(ls_curramnt) = VALUE bapiaccr09( itemno_acc = lv_item
                                          currency = 'INR'
                                          amt_doccur = ls_caltax1-wmwst * -1
                                          amt_base = amount * -1
                                          tax_amt = ls_caltax1-wmwst * -1
                                           ).

    APPEND ls_curramnt TO lt_curramnt.
    lv_item = lv_item + 1.
  ENDLOOP.
** Select G/L Account from YAIN table ***
  SELECT * FROM j_1it030k  INTO TABLE @DATA(lt_glget)
           WHERE ktopl = 'YAIN'
           AND ktosl IN ( 'JOI', 'JOC', 'JOS' )
           AND bupla = 'TN01'.
  DATA(lv_igl) = VALUE #( lt_glget[ ktosl = 'JOI' ]-konth OPTIONAL ).
  DATA(lv_sgl) = VALUE #( lt_glget[ ktosl = 'JOS' ]-konth OPTIONAL ).
  DATA(lv_cgl) = VALUE #( lt_glget[ ktosl = 'JOC' ]-konth OPTIONAL ).

**** Account tax table filling ****
  DATA: lv_taxitem TYPE i,
        lv_titem   TYPE numc06.
  DATA: ls_tax TYPE bapiactx09.
  CLEAR: lv_taxitem,lv_titem.
  lv_taxitem = '3'.
  lv_titem = '000001'.

  LOOP AT lt_caltax INTO DATA(ls_caltax2).
    CLEAR ls_tax.
    ls_tax-itemno_acc = lv_taxitem.
    ls_tax-cond_key = ls_caltax2-kschl.
    ls_tax-itemno_tax = lv_titem.
    ls_tax-acct_key = ls_caltax2-ktosl.
    ls_tax-tax_rate = ls_caltax2-msatz.
    ls_tax-tax_code = lv_taxcode.
    ls_tax-direct_tax = 'X'.
    CASE ls_caltax2-ktosl.
      WHEN 'JOI'.
        ls_tax-gl_account = lv_igl.
      WHEN 'JOS'.
        ls_tax-gl_account = lv_sgl.
      WHEN OTHERS.
        ls_tax-gl_account = lv_cgl.
    ENDCASE.
    APPEND ls_tax TO lt_tax.
    lv_taxitem = lv_taxitem + 1.
*    lv_titem = lv_titem + 1.
  ENDLOOP.

  REFRESH: return,lt_return.
*** Document Check Before posting ***
  CALL FUNCTION 'BAPI_ACC_DOCUMENT_CHECK'
    EXPORTING
      documentheader = ls_header
    TABLES
      accountgl      = lt_glaccount
      accountpayable = lt_accountpayable
      accounttax     = lt_tax
      currencyamount = lt_curramnt
      return         = return.
** If no error appears in checking ****
  READ TABLE return WITH KEY type = 'S'
                             number = '614'.
  IF sy-subrc EQ 0.
    REFRESH: return.
    CLEAR: lv_objkey,lv_objsys,lv_objtyp.
*** Function Module to create Debit note ***
    CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
      EXPORTING
        documentheader = ls_header
      IMPORTING
        obj_type       = lv_objtyp
        obj_key        = lv_objkey
        obj_sys        = lv_objsys
      TABLES
        accountgl      = lt_glaccount
        accountpayable = lt_accountpayable
        accounttax     = lt_tax
        currencyamount = lt_curramnt
        return         = return.

    READ TABLE return WITH KEY type = 'S'.
    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
      CLEAR ls_docno.
      MOVE lv_objkey TO ls_docno.
      CLEAR: document_no.
      document_no = ls_docno-belnr.
      amounts = amounts.

**** Updating Our log table "ZDEBIT_REF_MIS" *******
      DATA(ls_table) = VALUE zdebit_ref_mis( mandt = sy-mandt
                                             bukrs = comp_code
                                             doc_no = ls_docno-belnr
                                             gjahr = ls_docno-gjahr
                                             account_type = 'K'
                                             account = vendor
                                             invoice_refno = lv_refno
                                             amount = amount
                                             sgst   = lv_sgst
                                             cgst   = lv_cgst
                                             igst   = lv_igst
                                             disc_date = disc_date
                                             disc_period = dis_period
                                             erdat = sy-datum
                                             ernam = sy-uname
                                             erzet = sy-uzeit
                                             doc_type = 'KG'
                                             actual_doc1 = org_inv ).
      INSERT zdebit_ref_mis FROM ls_table.

*** Creating Background job for sending Email to Vendors ***
      DATA(ls_job) = VALUE tbtcjob( jobname = |{ ls_docno-belnr } { ls_docno-gjahr }|
                                    sdlstrttm = sy-uzeit + 000030
                                    jobcount = '' ).


      CALL FUNCTION 'JOB_OPEN'
        EXPORTING
          jobname   = ls_job-jobname
          sdlstrtdt = sy-datum
          sdlstrttm = ls_job-sdlstrttm
        IMPORTING
          jobcount  = ls_job-jobcount.


      CALL FUNCTION 'GET_PRINT_PARAMETERS'
        EXPORTING
          destination            = 'LP01'
          no_dialog              = 'X'
        IMPORTING
          out_archive_parameters = arc_params
          out_parameters         = print_params
          valid                  = valid.

      IF sy-subrc IS INITIAL.
        IF sy-subrc EQ 0.
          lt_seltab = VALUE #(
              ( selname = 'S_BELNR'  kind = 'S' sign = 'I' option = 'EQ' low = ls_docno-belnr )
              ( selname = 'S_BUKRS'  kind = 'S' sign = 'I' option = 'EQ' low = ls_docno-bukrs )
              ( selname = 'S_GJAHR'  kind = 'S' sign = 'I' option = 'EQ' low = ls_docno-gjahr )
              ( selname = 'P_DISP1'  kind = 'P' sign = 'I' option = 'EQ' low = 'X' )
              ( selname = 'P_DISP2'  kind = 'P' sign = 'I' option = 'EQ' low = '' )
              ( selname = 'P_EMAIL'  kind = 'P' sign = 'I' option = 'EQ' low = '' )
              ( selname = 'P_EMAIL1' kind = 'P' sign = 'I' option = 'EQ' low = 'X' ) ).

**** Submitting Vendor Debit Note Email Sending Program ***
          SUBMIT zcredit_note_prg
          WITH SELECTION-TABLE lt_seltab
          AND RETURN
          USER sy-uname
          VIA JOB ls_job-jobname
          NUMBER ls_job-jobcount.

**** Closing the Background Job ***
          DATA(job_rel) = VALUE btch0000( char1 = 'X' ).
          CALL FUNCTION 'JOB_CLOSE'
            EXPORTING
              jobcount         = ls_job-jobcount
              jobname          = ls_job-jobname
              sdlstrtdt        = sy-datum
              sdlstrttm        = ls_job-sdlstrttm
              strtimmed        = ' '
*             dont_release     = 'X'
            IMPORTING
              job_was_released = job_rel-char1.
        ENDIF.

      ENDIF.
    ELSE.
      CLEAR: document_no,amounts.
*** If no error in checking bapi but then also docno not created ***
      LOOP AT return ASSIGNING <wa_return> WHERE type = 'E'.
        DATA(ls_return1) = <wa_return>.
        APPEND ls_return1 TO lt_return.
      ENDLOOP.
    ENDIF.
  ELSE.
    CLEAR amounts.
*** Filling the Error table while do checking Document***
    LOOP AT return ASSIGNING <wa_return> WHERE type = 'E'.
      DATA(ls_return) = <wa_return>.
      APPEND ls_return TO lt_return.
    ENDLOOP.
  ENDIF.

ENDFUNCTION.
