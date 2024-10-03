FUNCTION zven_finance_hdfc__int_post.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(COMP_CODE) TYPE  BUKRS
*"     REFERENCE(INTEREST_AMOUNT) TYPE  WRBTR
*"     REFERENCE(DOC_DATE) TYPE  BLDAT
*"     REFERENCE(POSTING_DATE) TYPE  BUDAT
*"  EXPORTING
*"     REFERENCE(DOCUMENT_NO) TYPE  BELNR_D
*"     REFERENCE(MESSAGE) TYPE  CHAR50
*"  TABLES
*"      LT_RETURN STRUCTURE  BAPIRET2
*"  EXCEPTIONS
*"      ENTER_COMPCODE
*"      ENTER_AMOUNT
*"      ENTER_PDATE
*"      ENTER_DDATE
*"----------------------------------------------------------------------
*"Created By: Samsudeen M
*"Created On: 30.11.2022
*"Purpose: It is developed for Putting Entry for HDFC financing Interest Amount
*"Reference by: Ramakrishnan J
***********************************************************************************
  TYPES: BEGIN OF output,
           belnr TYPE belnr_d,
           bukrs TYPE bukrs,
           gjahr TYPE gjahr,
         END OF output.
  DATA: ls_docno TYPE output.

  DATA: lv_amount TYPE char25.
  DATA: lv_glacc1 TYPE saknr. "GL Account
  DATA: lv_glacc2 TYPE saknr. "GL
  DATA: lv_glacc3 TYPE saknr. "GL Account
  DATA: lt_glaccount TYPE TABLE OF bapiacgl09.
  DATA: lt_curramnt TYPE TABLE OF bapiaccr09.
  DATA: lv_objtyp TYPE bapiache09-obj_type.
  DATA: lv_objkey TYPE bapiache09-obj_key.
  DATA: lv_objsys TYPE bapiache09-obj_sys.
  DATA: lv_fund TYPE bapiacgl09-fund.
  DATA: return TYPE STANDARD TABLE OF bapiret2
                                       WITH HEADER LINE.

  FIELD-SYMBOLS <wa_return> TYPE bapiret2.

  IF comp_code IS INITIAL.
    RAISE enter_compcode.
  ENDIF.

  IF interest_amount IS INITIAL.
    RAISE enter_amount.
  ENDIF.

  IF doc_date IS INITIAL.
    RAISE enter_ddate.
  ENDIF.

  IF posting_date IS INITIAL.
    RAISE enter_pdate.
  ENDIF.

  CLEAR lv_amount.
  lv_amount = interest_amount.
  CONDENSE lv_amount NO-GAPS.

  CLEAR: lv_glacc1,lv_glacc2.
*** getting the G/L Accounts for which the entries has to post ***
  SELECT SINGLE * FROM tvarvc INTO @DATA(lw_tvarvc)
                  WHERE name = 'ZAUTO_POST_INST_HDFC_GL1'
                  AND type = 'P'.
  IF sy-subrc EQ 0.
    lv_glacc1 = lw_tvarvc-low.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_glacc1
      IMPORTING
        output = lv_glacc1.
  ENDIF.

  SELECT SINGLE * FROM tvarvc INTO @DATA(lw_tvarvc1)
                  WHERE name = 'ZAUTO_POST_INST_HDFC_GL2'
                  AND type = 'P'.
  IF sy-subrc EQ 0.
    lv_glacc2 = lw_tvarvc1-low.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_glacc2
      IMPORTING
        output = lv_glacc2.
  ENDIF.

  REFRESH: lt_glaccount,lt_curramnt.
*** Header Content of the Document ***
  DATA(ls_header) = VALUE bapiache09( bus_act = 'RFBU'
                                    username = sy-uname
                                    comp_code = comp_code
                                    doc_date = doc_date
                                    pstng_date = posting_date
                                    fisc_year = sy-datum+0(4)
                                    doc_type = 'KZ' ).
*** gl account Entries ***
  CLEAR lv_glacc3.
  lv_glacc3 = lv_glacc1.
  SHIFT lv_glacc3 LEFT DELETING LEADING '0'.
*** Fund calculation based on system date ***
  DATA(lv_month) = sy-datum+4(2).
  CLEAR lv_fund.
  CASE lv_month.
    WHEN 4.  lv_fund = 'FUNDI'.
    WHEN 5.  lv_fund = 'FUNDII'.
    WHEN 6.  lv_fund = 'FUNDIII'.
    WHEN 7.  lv_fund = 'FUNDIV'.
    WHEN 8.  lv_fund = 'FUNDV'.
    WHEN 9.  lv_fund = 'FUNDVI'.
    WHEN 10. lv_fund = 'FUNDVII'.
    WHEN 11. lv_fund = 'FUNDVIII'.
    WHEN 12. lv_fund = 'FUNDIX'.
    WHEN 1.  lv_fund = 'FUNDX'.
    WHEN 2.  lv_fund = 'FUNDXI'.
    WHEN 3.  lv_fund = 'FUNDXII'.
    WHEN OTHERS.
  ENDCASE.
  lt_glaccount = VALUE #(
                     ( itemno_acc = '1' gl_account = lv_glacc1 costcenter = 'HOFINACCT' fund = lv_fund  cmmt_item = lv_glacc3
                                              item_text = 'Interest on HDFC Vendor Finance'  )
                     ( itemno_acc = '2' gl_account = lv_glacc2 ) ).

*** Currency Amount details filling table ***
  lt_curramnt = VALUE #(
                     ( itemno_acc = '1'
                       currency = 'INR'
                       amt_doccur = lv_amount
                       amt_base = lv_amount  )
                     ( itemno_acc = '2'
                       currency = 'INR'
                       amt_doccur = lv_amount * -1
                       amt_base =  lv_amount * -1
                       ) ).
  REFRESH: return,lt_return.
*** Document Check Before posting ***
  CALL FUNCTION 'BAPI_ACC_DOCUMENT_CHECK'
    EXPORTING
      documentheader = ls_header
    TABLES
      accountgl      = lt_glaccount
      currencyamount = lt_curramnt
      return         = return.
** If no error appears in checking ****
  READ TABLE return WITH KEY type = 'E'.
  IF sy-subrc NE 0.
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
        currencyamount = lt_curramnt
        return         = return.

    READ TABLE return WITH KEY type = 'S'.
    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
      CLEAR ls_docno.
      MOVE lv_objkey TO ls_docno.
      CLEAR: document_no,message.
      document_no = ls_docno-belnr.
      message = 'Interest Posting Done Successfully'.
**** Updating Our log table "ZDEBIT_REF_MIS" *******
      DATA(ls_table) = VALUE zdebit_ref_mis( mandt = sy-mandt
                                             bukrs = comp_code
                                             doc_no = ls_docno-belnr
                                             gjahr = ls_docno-gjahr
                                             account_type = 'S'
                                             account = lv_glacc1
                                             amount = lv_amount
                                             erdat = sy-datum
                                             ernam = sy-uname
                                             remarks = 'Interest Posted Succesfully'
                                             doc_type = 'KZ').
      INSERT zdebit_ref_mis FROM ls_table.
      COMMIT WORK AND WAIT.
    ELSE.
      CLEAR: document_no.
*** If no error in checking bapi but then also docno not created ***
      LOOP AT return ASSIGNING <wa_return> WHERE type = 'E'.
        DATA(ls_return1) = <wa_return>.
        APPEND ls_return1 TO lt_return.
      ENDLOOP.
    ENDIF.
  ELSE.
*** Filling the Error table while do checking Document***
    LOOP AT return ASSIGNING <wa_return> WHERE type = 'E'.
      DATA(ls_return) = <wa_return>.
      APPEND ls_return TO lt_return.
    ENDLOOP.
  ENDIF.
ENDFUNCTION.
