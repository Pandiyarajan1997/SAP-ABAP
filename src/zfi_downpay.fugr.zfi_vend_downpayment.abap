FUNCTION zfi_vend_downpayment.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(COMP_CODE) TYPE  BUKRS
*"     REFERENCE(VENDOR) TYPE  LIFNR
*"     REFERENCE(AMOUNT) TYPE  WRBTR
*"     REFERENCE(DOC_DATE) TYPE  BLDAT
*"     REFERENCE(POSTING_DATE) TYPE  BUDAT
*"     REFERENCE(GLACCOUNT) TYPE  KONTO
*"     REFERENCE(HOUSE_BANK) TYPE  HBKID
*"     REFERENCE(SPCL_IND) TYPE  UMSKZ
*"  EXPORTING
*"     REFERENCE(CLEARDOCUMENT_NO) TYPE  BELNR_D
*"     REFERENCE(MSG_TYP) TYPE  CHAR01
*"     REFERENCE(MESSAGE) TYPE  CHAR50
*"  EXCEPTIONS
*"      ENTER_COMPCODE
*"      ENTER_VENDOR
*"      ENTER_AMOUNT
*"      ENTER_PDATE
*"      ENTER_DDATE
*"      GL_MISSING
*"      ENTER_HOUSE_BANK
*"      ENTER_SPCL_IND
*"----------------------------------------------------------------------
  "Created by   : Pandiarajan
  "Created on   : 17.10.2023
  "Purpose      : Fm for vendor downpayment posting ( f-48 )
  "Reference by : Ramakrishnan J
*-------------------------------------------------------------------------------*


  DATA: lv_date(10)  TYPE c,
        lv_date1(10) TYPE c,
        lv_amount    TYPE char25.

  DATA lt_msg TYPE TABLE OF bdcmsgcoll.


*** Error Handling in Initial Process ***
  IF comp_code IS INITIAL.
    RAISE enter_compcode.
  ENDIF.

  IF vendor IS INITIAL.
    RAISE enter_vendor.
  ELSE.

    DATA(lv_vendor) = vendor.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_vendor
      IMPORTING
        output = lv_vendor.

    SELECT SINGLE name1 FROM lfa1
      INTO @DATA(lv_venname)
      WHERE lifnr = @lv_vendor.
  ENDIF.

*    DATA(lv_invref_up) = inv_refno.
*    TRANSLATE lv_invref_up TO UPPER CASE.
*    SELECT SINGLE * FROM bsik
*                    INTO @DATA(lw_bsik)
*                    WHERE bukrs = @comp_code
*                    AND belnr = @org_inv_no
*                    AND lifnr = @lv_vendor
*                    AND xblnr = @lv_invref_up.
*    IF sy-subrc EQ 0.
*
*    ELSE.
*      SELECT SINGLE * FROM bsak
*                      INTO @DATA(lw_bsak)
*                      WHERE bukrs = @comp_code
*                      AND belnr = @org_inv_no
*                      AND lifnr = @lv_vendor
*                      AND xblnr = @lv_invref_up.
*  IF sy-subrc = 0.
*    RAISE document_already_cleared.
*  ELSE.
*    RAISE document_not_found.
*  ENDIF.
*    ENDIF.
*  ENDIF.

  IF amount IS INITIAL.
    RAISE enter_amount.
  ELSE.
    lv_amount = amount.
    CONDENSE lv_amount NO-GAPS.
  ENDIF.

  IF doc_date IS INITIAL.
    RAISE enter_ddate.
  ELSE.
    CLEAR lv_date.
    WRITE doc_date TO lv_date DD/MM/YYYY.
  ENDIF.

  IF posting_date IS INITIAL.
    RAISE enter_pdate.
  ELSE.
    CLEAR lv_date1.
    WRITE posting_date TO lv_date1 DD/MM/YYYY.
  ENDIF.

  IF glaccount IS INITIAL.
    RAISE gl_missing.
  ENDIF.

  IF house_bank IS INITIAL.
    RAISE enter_house_bank.
  ENDIF.

  IF spcl_ind IS INITIAL.
    RAISE enter_spcl_ind.
  ENDIF.



************* BDC Rec portion*****************************

  PERFORM bdc_dynpro      USING 'SAPMF05A' '0110'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'BSEG-WRBTR'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=AF'.
  PERFORM bdc_field       USING 'BKPF-BLDAT'
                                lv_date1.   "'17.10.2023'.
  PERFORM bdc_field       USING 'BKPF-BLART'
                                'KZ'.
  PERFORM bdc_field       USING 'BKPF-BUKRS'
                                comp_code.  "'1000'.
  PERFORM bdc_field       USING 'BKPF-BUDAT'
                                lv_date.    "'17.10.2023'.
  PERFORM bdc_field       USING 'BKPF-MONAT'
                                '7'.
  PERFORM bdc_field       USING 'BKPF-WAERS'
                                'INR'.
  PERFORM bdc_field       USING 'RF05A-NEWKO'
                                vendor.      "'10000614'.
  PERFORM bdc_field       USING 'RF05A-UMSKZ'
                                spcl_ind.    "'A'.
  PERFORM bdc_field       USING 'BSEG-HBKID'
                                house_bank.  "'7290'.
  PERFORM bdc_field       USING 'RF05A-KONTO'
                                glaccount.   "'19710391'.
  PERFORM bdc_field       USING 'BSEG-WRBTR'
                                lv_amount.                  "'10001'.
  PERFORM bdc_dynpro      USING 'SAPMF05A' '1702'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RFOPS_DK-BELNR(01)'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=AZ'.
  PERFORM bdc_field       USING 'RF05A-ABPOS'
                                '1'.
  PERFORM bdc_field       USING 'RF05A-XPOS1(01)'
                                'X'.
  PERFORM bdc_dynpro      USING 'SAPMF05A' '0700'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'BKPF-XBLNR'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=BU'.

*****************call transaction method with bdc data***************************

  CALL TRANSACTION 'F-48' WITH AUTHORITY-CHECK USING lt_bdcdata
                   MODE   'A'
                   UPDATE 'S'
                   MESSAGES INTO lt_msg.

ENDFUNCTION.
