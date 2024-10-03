FUNCTION zauto_clear_for_customer_inv.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(API_INPUT) TYPE  ZCUST_INV_ADJUST_ST
*"  EXPORTING
*"     REFERENCE(CLEARDOCUMENT_NO) TYPE  BELNR_D
*"  TABLES
*"      LT_RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"  EXCEPTIONS
*"      NO_INPUT
*"      DOCUMENT_ALREADY_CLEAR
*"----------------------------------------------------------------------
** Created by: Samsudeen M
** Created On: 21.08.2023
** Reference By: Ramakrishnan
** Purpose: It is for posting Clearing Documents for Customer Invoice Payments
**            based on MIS input **
*"------------------------------------------------------------------------------
  DATA: lv_msgid TYPE syst_msgid,
        lv_msgnr TYPE sy-msgno,
        lv_msgty TYPE sy-msgty,
        lv_msg   TYPE sy-msgv1,
        lv_msg1  TYPE sy-msgv2,
        lv_msg2  TYPE sy-msgv3,
        lv_msg3  TYPE sy-msgv4,
        lv_subrc TYPE sy-subrc.

  DATA: lt_blntab  TYPE TABLE OF blntab,
        lt_ftclear TYPE TABLE OF ftclear,
        lt_ftpost  TYPE TABLE OF ftpost,
        lt_fttax   TYPE TABLE OF fttax.
  DATA: lv_date(10) TYPE c.
  DATA: lv_date1(10) TYPE c.
  DATA:lv_amount(25) TYPE c.
  DATA: lv_fm_msg TYPE string.
  DATA: lv_refno TYPE xblnr1.
  DATA: length TYPE i.
  DATA: lw_disc_charge TYPE bsik,
        lw_old_dt      TYPE bsik.
  DATA: lw_disc_charge1 TYPE zdebit_ref_mis.

  DATA: return TYPE STANDARD TABLE OF bapiret2
                                       WITH HEADER LINE.
  DATA: lv_msgtxt TYPE string.
  DATA: gt_bseg   TYPE STANDARD TABLE OF bseg,
        gt_accchg TYPE TABLE OF accchg.

  DATA: gv_utr_field TYPE    char50 VALUE 'KIDNO'.


  IF api_input IS NOT INITIAL.
    "Billing Document Number
    SELECT SINGLE * FROM vbrk INTO @DATA(l_invheader)
      WHERE vbeln = @api_input-orginv
      AND fksto NE 'X'.
    IF sy-subrc EQ 0.
** Customer Open items from BSID **
      SELECT SINGLE * FROM bsid INTO @DATA(l_openitem)
        WHERE bukrs = @api_input-ccode
        AND kunnr = @api_input-customer
        AND belnr = @l_invheader-belnr
        AND gjahr = @l_invheader-gjahr.
      IF sy-subrc NE 0.
** Customer Cleared items from BSAD **
        SELECT SINGLE * FROM bsad INTO @DATA(l_clearitem)
          WHERE bukrs = @api_input-ccode
          AND kunnr = @api_input-customer
          AND belnr = @l_invheader-belnr
          AND gjahr = @l_invheader-gjahr.
        IF sy-subrc EQ 0.
          RAISE document_already_clear.
        ENDIF.
      ENDIF.
    ENDIF.

    DATA: lv_fisyear TYPE bapi0002_4-fiscal_year,
          lv_month   TYPE bapi0002_4-fiscal_period,
          l_return   TYPE bapireturn1.

    CLEAR: lv_fisyear,lv_month,l_return.

*** function module to get fiscal year ***
    CALL FUNCTION 'BAPI_COMPANYCODE_GET_PERIOD'
      EXPORTING
        companycodeid = api_input-ccode
        posting_date  = api_input-posdate
      IMPORTING
        fiscal_year   = lv_fisyear
        fiscal_period = lv_month
        return        = l_return.

** Actual Clearing Document Post starts here ***
    CALL FUNCTION 'POSTING_INTERFACE_START'
      EXPORTING
        i_function         = 'C'
        i_mode             = 'N'
        i_update           = 'S'
      EXCEPTIONS
        client_incorrect   = 1
        function_invalid   = 2
        group_name_missing = 3
        mode_invalid       = 4
        update_invalid     = 5
        user_invalid       = 6
        OTHERS             = 7.
    IF sy-subrc = 0.
      REFRESH: lt_blntab,lt_ftclear,lt_ftpost,lt_fttax.
      CLEAR: lv_msgid,lv_msgnr,lv_msgty,lv_msg,lv_msg1,lv_msg2,lv_msg3,lv_subrc.
**** Amount adjustments ***
      CLEAR: lv_amount.
      lv_amount = api_input-amount.
      CONDENSE lv_amount NO-GAPS.
*** Actual Document which is getting Cleared ***
      lt_ftclear = VALUE #( ( agkoa = 'D'
                              agkon = api_input-customer
                              agbuk = api_input-ccode
                              xnops = 'X'
                              selfd = 'BELNR'
                              selvon = l_openitem-belnr ) ).
      CLEAR: lv_date,lv_date1.
      WRITE api_input-docdate TO lv_date DD/MM/YYYY.
      WRITE api_input-posdate TO lv_date1 DD/MM/YYYY.

      SELECT SINGLE name1 FROM kna1 INTO @DATA(lv_cusname)
        WHERE kunnr = @api_input-customer.
      SELECT SINGLE akont FROM knb1 INTO @DATA(lv_glacc)
        WHERE kunnr = @api_input-customer.

      DATA(lv_count) = '001'.
      DATA(lv_sgtxt) = CONV sgtxt( |{ l_invheader-vbeln }-{ lv_cusname }| ).

      lt_ftpost = VALUE #(
                 ( stype = 'K' count = lv_count     fnam = 'BKPF-BUKRS' fval = api_input-ccode )
                 ( stype = 'K' count = lv_count     fnam = 'BKPF-BLDAT' fval = lv_date )
                 ( stype = 'K' count = lv_count     fnam = 'BKPF-BUDAT' fval = lv_date1 )
                 ( stype = 'K' count = lv_count     fnam = 'BKPF-WAERS' fval = 'INR' )
                 ( stype = 'K' count = lv_count     fnam = 'BKPF-BLART' fval = 'DZ' )
                 ( stype = 'K' count = lv_count     fnam = 'BKPF-XBLNR' fval = l_invheader-vbeln )
                 ( stype = 'P' count = lv_count     fnam = 'RF05A-NEWBS' fval = '50' )
                 ( stype = 'P' count = lv_count     fnam = 'BSEG-HKONT' fval = lv_glacc )
                 ( stype = 'P' count = lv_count     fnam = 'BSEG-WRBTR' fval = lv_amount )
                 ( stype = 'P' count = lv_count     fnam = 'BSEG-SGTXT' fval = lv_sgtxt ) ).

*** Actual Call for Clearing and Posting ****
      REFRESH lt_return.
      CALL FUNCTION 'POSTING_INTERFACE_CLEARING'
        EXPORTING
          i_auglv                    = 'AUSGZAHL'
          i_tcode                    = 'FB05'
          i_sgfunct                  = 'C'
          i_no_auth                  = ' '
          i_xsimu                    = ' '
        IMPORTING
          e_msgid                    = lv_msgid
          e_msgno                    = lv_msgnr
          e_msgty                    = lv_msgty
          e_msgv1                    = lv_msg
          e_msgv2                    = lv_msg1
          e_msgv3                    = lv_msg2
          e_msgv4                    = lv_msg3
          e_subrc                    = lv_subrc
        TABLES
          t_blntab                   = lt_blntab
          t_ftclear                  = lt_ftclear
          t_ftpost                   = lt_ftpost
          t_fttax                    = lt_fttax
        EXCEPTIONS
          clearing_procedure_invalid = 1
          clearing_procedure_missing = 2
          table_t041a_empty          = 3
          transaction_code_invalid   = 4
          amount_format_error        = 5
          too_many_line_items        = 6
          company_code_invalid       = 7
          screen_not_found           = 8
          no_authorization           = 9
          OTHERS                     = 10.
      IF sy-subrc = 0.
        IF lv_msgid = 'F5' AND lv_msgnr = '312' AND lv_msgty = 'S'.
          CLEAR: cleardocument_no.
          cleardocument_no = lv_msg. "Clearing Document Number
*** Ending the posting INTERFACE **
          CALL FUNCTION 'POSTING_INTERFACE_END'
            EXPORTING
              i_bdcimmed              = 'X'
            EXCEPTIONS
              session_not_processable = 1
              OTHERS                  = 2.
          IF sy-subrc = 0.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ELSE.
    RAISE no_input.
  ENDIF.


*
**** Reference Document Number ***
*  CLEAR lw_disc_charge.
*  IF inv_refno IS NOT INITIAL.
*    CLEAR lv_refno.
*    lv_refno = |{ inv_refno }{ '-BC' }|.
*    TRANSLATE lv_refno TO UPPER CASE.
*    SELECT SINGLE * FROM bsik
*                    INTO lw_disc_charge
*                    WHERE xblnr = lv_refno
**                    AND gjahr = lv_fisyear
*                    AND blart = 'KG'.
*    IF sy-subrc NE 0.
*      CLEAR: lv_refno,lw_disc_charge1.
*      lv_refno = inv_refno.
*      TRANSLATE lv_refno TO UPPER CASE.
*      SELECT SINGLE * FROM zdebit_ref_mis
*                      INTO CORRESPONDING FIELDS OF lw_disc_charge1
*                      WHERE account = vendor
*                     AND invoice_refno = lv_refno
*                     AND doc_type = 'KG'.
*      TRANSLATE lv_refno TO UPPER CASE.
*    ELSE.
*      TRANSLATE lv_refno TO UPPER CASE.
*    ENDIF.
**** Selecting already created non banking charges DN ****
*    CLEAR lw_old_dt.
*    SELECT SINGLE * FROM bsik INTO lw_old_dt
*                    WHERE bukrs = comp_code
*                    AND lifnr = vendor
*                    AND xblnr = inv_refno
*                    AND blart = 'KG'.
*  ELSE.
*    RAISE enter_invrefno.
*  ENDIF.
*
**  IF payment_reference IS INITIAL.
**    RAISE enter_paymentref.
**  ENDIF.
*
*  IF assignment IS INITIAL.
*    RAISE enter_assignment.
*  ENDIF.
*
***** Error Handling Ends ***
*
*** Actual Clearing Document Post starts here ***
*  CALL FUNCTION 'POSTING_INTERFACE_START'
*    EXPORTING
*      i_function         = 'C'
*      i_mode             = 'N'
*      i_update           = 'S'
*    EXCEPTIONS
*      client_incorrect   = 1
*      function_invalid   = 2
*      group_name_missing = 3
*      mode_invalid       = 4
*      update_invalid     = 5
*      user_invalid       = 6
*      OTHERS             = 7.
*  IF sy-subrc = 0.
*


*

*
**** If Discount Charges Available for Input Invoice Referenc ***
*    IF lw_disc_charge IS NOT INITIAL.
*      APPEND VALUE #( agkoa = 'K' agkon = vendor agbuk = comp_code xnops = 'X' selfd = 'BELNR' selvon = lw_disc_charge-belnr ) TO lt_ftclear.
*    ENDIF.
**** If Discount Charges Available for Input Invoice Referenc ***
*    IF lw_disc_charge1 IS NOT INITIAL.
*      APPEND VALUE #( agkoa = 'K' agkon = vendor agbuk = comp_code xnops = 'X' selfd = 'BELNR' selvon = lw_disc_charge1-doc_no ) TO lt_ftclear.
*    ENDIF.
**** old debit note exsists means ***
*    IF lw_old_dt IS NOT INITIAL.
*      APPEND VALUE #( agkoa = 'K' agkon = vendor agbuk = comp_code xnops = 'X' selfd = 'BELNR' selvon = lw_old_dt-belnr ) TO lt_ftclear.
*    ENDIF.
**** Actual Entries to be posted on G/L Accounts ***
*
**    WRITE sy-datum TO lv_date DD/MM/YYYY.

*
**** Getting G/L Account for which the entry have to be post **
**    SELECT SINGLE * FROM tvarvc INTO @DATA(lw_tvarvc) WHERE name = 'ZAUTO_CLEAR_INV_GL'
**                                                      AND type = 'P'.
**    IF sy-subrc EQ 0.
**      DATA(lv_glacc) = lw_tvarvc-low.
**    ENDIF.
*
**** Getting Rounding off G/L Accounts  ****
*    SELECT SINGLE * FROM tvarvc INTO @DATA(lw_tvarvc1) WHERE name = 'ZAUTO_CLEAR_INV_ROGL'
*                                AND type = 'P'.
*    IF sy-subrc EQ 0.
*      DATA(lv_roglacc) = lw_tvarvc1-low.
*    ENDIF.
*
**Changed On: 19.05.2023 *Changed By: Samsudeen M
*    DATA(lv_sgtxt) = |Payment Clearing for { lv_venname }|.
*
*    DATA: l_len TYPE i.
*
*    l_len = strlen( lv_sgtxt ).
*    IF l_len > 50.
*      lv_sgtxt = lv_sgtxt+0(49).
*    ENDIF.
*
*

**               ( stype = 'P' count = lv_count     fnam = 'BSEG-KIDNO' fval = payment_reference )
*               ).
*
*
**** Actual Call for Clearing and Posting ****
*    REFRESH lt_return.
*    CALL FUNCTION 'POSTING_INTERFACE_CLEARING'
*      EXPORTING
*        i_auglv                    = 'EINGZAHL'
*        i_tcode                    = 'FB05'
*        i_sgfunct                  = 'C'
*        i_no_auth                  = ' '
*        i_xsimu                    = ' '
*      IMPORTING
*        e_msgid                    = lv_msgid
*        e_msgno                    = lv_msgnr
*        e_msgty                    = lv_msgty
*        e_msgv1                    = lv_msg
*        e_msgv2                    = lv_msg1
*        e_msgv3                    = lv_msg2
*        e_msgv4                    = lv_msg3
*        e_subrc                    = lv_subrc
*      TABLES
*        t_blntab                   = lt_blntab
*        t_ftclear                  = lt_ftclear
*        t_ftpost                   = lt_ftpost
*        t_fttax                    = lt_fttax
*      EXCEPTIONS
*        clearing_procedure_invalid = 1
*        clearing_procedure_missing = 2
*        table_t041a_empty          = 3
*        transaction_code_invalid   = 4
*        amount_format_error        = 5
*        too_many_line_items        = 6
*        company_code_invalid       = 7
*        screen_not_found           = 8
*        no_authorization           = 9
*        OTHERS                     = 10.
*    IF sy-subrc = 0.
*      IF lv_msgid = 'F5' AND lv_msgnr = '312' AND lv_msgty = 'S'.
*        CLEAR: cleardocument_no,message,msg_typ.
*        cleardocument_no = lv_msg. "Clearing Document Number
*        msg_typ = 'S'.
*        "Added by Samsudeen M on 25.03.2023
*        IF cleardocument_no IS NOT INITIAL.
*          DATA(lv_awref) = CONV awref( cleardocument_no ).
*          DATA(lv_aworg) = CONV aworg( |{ comp_code }{ lv_fisyear }| ).
*          REFRESH: gt_bseg.
*          CALL FUNCTION 'FI_DOCUMENT_READ'
*            EXPORTING
*              i_bukrs     = comp_code
*              i_belnr     = cleardocument_no
*              i_gjahr     = lv_fisyear
*            TABLES
*              t_bseg      = gt_bseg
*            EXCEPTIONS
*              wrong_input = 1
*              not_found   = 2
*              OTHERS      = 3.
*          IF sy-subrc = 0.
*            LOOP AT gt_bseg INTO DATA(lw_bseg).
*              REFRESH gt_accchg.
*              DATA(lw_accchg) = VALUE accchg( fdname = gv_utr_field
*                                              oldval = lw_bseg-kidno
*                                              newval = payment_reference ).
*              APPEND lw_accchg TO gt_accchg.
*              CALL FUNCTION 'FI_DOCUMENT_CHANGE'
*                EXPORTING
*                  i_awtyp              = 'BKPF'
*                  i_awref              = lv_awref
*                  i_aworg              = lv_aworg
*                  i_buzei              = lw_bseg-buzei
*                TABLES
*                  t_accchg             = gt_accchg
*                EXCEPTIONS
*                  no_reference         = 1
*                  no_document          = 2
*                  many_documents       = 3
*                  wrong_input          = 4
*                  overwrite_creditcard = 5
*                  OTHERS               = 6.
*            ENDLOOP.
*          ENDIF.
*        ENDIF.
*        CLEAR lv_msgtxt.
*        CALL FUNCTION 'MESSAGE_TEXT_BUILD'
*          EXPORTING
*            msgid               = lv_msgid
*            msgnr               = lv_msgnr
*            msgv1               = lv_msg
*            msgv2               = lv_msg1
*            msgv3               = lv_msg2
*            msgv4               = lv_msg3
*          IMPORTING
*            message_text_output = lv_msgtxt.
*
***** Message Description Fetching from t100 ***
**        SELECT SINGLE text FROM t100
**                           INTO @DATA(ls_msg)
**                           WHERE sprsl = @sy-langu
**                           AND arbgb = @lv_msgid
**                           AND msgnr = @lv_msgnr.
*        message = lv_msgtxt.
**** Filling the Document Number against which invoices the clearing document is posted ***
*        CLEAR actual_document.
*        DATA(do_count) = '001'.
*        LOOP AT lt_ftclear INTO DATA(fs_clear).
*          APPEND VALUE #( s_no = do_count document_no = fs_clear-selvon ) TO actual_document.
*          do_count = do_count + 1.
*        ENDLOOP.
**** Based on Document Number Generation updating the Log Table ***
*        DATA(ls_table1) = VALUE zdebit_ref_mis( mandt = sy-mandt
*                                               bukrs = comp_code
*                                               doc_no = cleardocument_no
*                                               gjahr = lv_fisyear
*                                               account_type = 'K'
*                                               account = vendor
*                                               invoice_refno = inv_refno
*                                               amount = amount
*                                               ernam = sy-uname
*                                               erdat = sy-datum
*                                               remarks = 'payment Clearing Document Posted'
*                                               pay_reference = payment_reference
*                                               doc_type = 'KZ'
*                                               actual_doc1 = VALUE #( actual_document[ s_no = '001'  ]-document_no OPTIONAL )
*                                               actual_doc2 = VALUE #( actual_document[ s_no = '002'  ]-document_no OPTIONAL )
*                                               actual_doc3 = VALUE #( actual_document[ s_no = '003'  ]-document_no OPTIONAL )
*                                               actual_doc4 = VALUE #( actual_document[ s_no = '004'  ]-document_no OPTIONAL )
*                                               actual_doc5 = VALUE #( actual_document[ s_no = '005'  ]-document_no OPTIONAL )
*                                               actual_doc6 = VALUE #( actual_document[ s_no = '006'  ]-document_no OPTIONAL )
*                                               actual_doc7 = VALUE #( actual_document[ s_no = '007'  ]-document_no OPTIONAL )
*                                               actual_doc8 = VALUE #( actual_document[ s_no = '008'  ]-document_no OPTIONAL )
*                                               actual_doc9 = VALUE #( actual_document[ s_no = '009'  ]-document_no OPTIONAL )
*                                               actual_doc10 = VALUE #( actual_document[ s_no = '010' ]-document_no OPTIONAL )
*                                               ).
*        INSERT zdebit_ref_mis FROM ls_table1.
*        COMMIT WORK AND WAIT.
*
**** Ending the posting Interface **
*        CALL FUNCTION 'POSTING_INTERFACE_END'
*          EXPORTING
*            i_bdcimmed              = 'X'
*          EXCEPTIONS
*            session_not_processable = 1
*            OTHERS                  = 2.
*        IF sy-subrc = 0.
*        ENDIF.
*      ELSE.
*        CLEAR lv_msgtxt.
*        CALL FUNCTION 'MESSAGE_TEXT_BUILD'
*          EXPORTING
*            msgid               = lv_msgid
*            msgnr               = lv_msgnr
*            msgv1               = lv_msg
*            msgv2               = lv_msg1
*            msgv3               = lv_msg2
*            msgv4               = lv_msg3
*          IMPORTING
*            message_text_output = lv_msgtxt.
*        APPEND VALUE #(  type = 'E' message = lv_msgtxt )  TO lt_return.
*      ENDIF.
*    ELSE.
*      CASE sy-subrc.
*        WHEN 1.
*          CLEAR lv_fm_msg.
*          lv_fm_msg = 'clearing_procedure_invalid'.
*        WHEN 2.
*          CLEAR lv_fm_msg.
*          lv_fm_msg = 'clearing_procedure_missing'.
*        WHEN 3.
*          CLEAR lv_fm_msg.
*          lv_fm_msg = 'table_t041a_empty'.
*        WHEN 4.
*          CLEAR lv_fm_msg.
*          lv_fm_msg = 'transaction_code_invalid '.
*        WHEN 5.
*          CLEAR lv_fm_msg.
*          lv_fm_msg = 'amount_format_error'.
*        WHEN 6.
*          CLEAR lv_fm_msg.
*          lv_fm_msg = 'too_many_line_items'.
*        WHEN 7.
*          CLEAR lv_fm_msg.
*          lv_fm_msg = 'company_code_invalid'.
*        WHEN 8.
*          CLEAR lv_fm_msg.
*          lv_fm_msg = 'screen_not_found'.
*        WHEN 9.
*          CLEAR lv_fm_msg.
*          lv_fm_msg = 'no_authorization'.
*        WHEN 10.
*          CLEAR lv_fm_msg.
*          lv_fm_msg = 'Error in calling Psoting FM'.
*      ENDCASE.
*      CLEAR: cleardocument_no,message,msg_typ.
*      DATA(ls_return) = VALUE bapiret2( type = 'E'
*                                       id = 'Function Module Exception occurs'
*                                       message = lv_fm_msg ).
*      APPEND ls_return TO lt_return.
*    ENDIF.
*  ENDIF.
ENDFUNCTION.
