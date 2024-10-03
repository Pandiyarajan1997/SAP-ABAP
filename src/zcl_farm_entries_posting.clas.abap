class ZCL_FARM_ENTRIES_POSTING definition
  public
  final
  create public .

public section.

  methods CREATE_GLACCOUNT_DOCUMENT
    importing
      !INPUT type ZFARM_API_INPUT_ST
    exporting
      !BELNR type BELNR_D
      !FISYEAR type GJAHR
      !MSG type STRING
    exceptions
      NO_INPUT .
  methods CREATE_VENDOR_INVOICE
    importing
      !INPUT type ZFARM_API_INPUT_ST
    exporting
      !FUNDCTR type FISTL
      !BELNR type BELNR_D
      !FISYEAR type GJAHR
      !MSG type STRING .
protected section.
private section.
ENDCLASS.



CLASS ZCL_FARM_ENTRIES_POSTING IMPLEMENTATION.


  METHOD create_glaccount_document.

    DATA: lv_fisyear TYPE bapi0002_4-fiscal_year,
          lv_month   TYPE bapi0002_4-fiscal_period,
          l_return   TYPE bapireturn1,
          lv_fund    TYPE bapiacgl09-fund.

    DATA: lt_glaccount TYPE TABLE OF bapiacgl09,
          lt_curramnt  TYPE TABLE OF bapiaccr09.
    DATA: lt_return TYPE bapiret2_t.

    DATA: lv_objtyp TYPE bapiache09-obj_type,
          lv_objkey TYPE bapiache09-obj_key,
          lv_objsys TYPE bapiache09-obj_sys.

    DATA: l_msg TYPE string.

    IF input IS NOT INITIAL.
*Fisyear Derive based on Posting Date
      CLEAR: lv_fisyear,lv_month,l_return.
      CALL FUNCTION 'BAPI_COMPANYCODE_GET_PERIOD'
        EXPORTING
          companycodeid = input-compcode
          posting_date  = input-posdate
        IMPORTING
          fiscal_year   = lv_fisyear
          fiscal_period = lv_month
          return        = l_return.
*Header of Document
      DATA(ls_header) = VALUE bapiache09( bus_act    = 'RFBU'
                                          username   = sy-uname
                                          comp_code  = input-compcode
                                          doc_date   = input-docdate
                                          pstng_date = input-posdate
                                          ref_doc_no = input-refdoc
                                          fisc_year  = lv_fisyear
                                          doc_type   = 'SA' ).
*FUND Month Get
      CLEAR lv_fund.
      CALL FUNCTION 'ZFUND_GET'
        EXPORTING
          date       = input-posdate
        IMPORTING
          lv_fund    = lv_fund
        EXCEPTIONS
          enter_date = 1
          OTHERS     = 2.
      IF sy-subrc <> 0.

      ENDIF.
*Deriving the Costcenter Based on Work Center in Input JSON
      SELECT SINGLE * FROM crhd INTO @DATA(l_crhd)
        WHERE objty = 'A' AND arbpl = @input-workctr.
      IF sy-subrc = 0.
        DATA(lv_plant) = l_crhd-werks.
        SELECT SINGLE kostl FROM crco INTO @DATA(lv_costcenter)
          WHERE objty = 'A' AND objid = @l_crhd-objid.
      ENDIF.
*Items Details for Document
      REFRESH: lt_glaccount,lt_curramnt.
      DATA(lv_itmcnt) = CONV posnr_acc( '1' ).

      SELECT SINGLE * FROM zfarm_glacc_mast INTO @DATA(l_debit_gl)
        WHERE enttype = @input-entrytype AND bschl = '40'.
      IF sy-subrc = 0.
        DATA(lv_glacc) = |{ l_debit_gl-saknr ALPHA = IN }|.
        SHIFT lv_glacc LEFT DELETING LEADING '0'.
      ENDIF.

      SELECT SINGLE * FROM zfarm_glacc_mast INTO @DATA(l_credit_gl)
        WHERE enttype = @input-entrytype AND bschl = '50'.
      IF sy-subrc = 0.
        DATA(lv_glacc1) = |{ l_credit_gl-saknr ALPHA = IN }|.
        SHIFT lv_glacc1 LEFT DELETING LEADING '0'.
      ENDIF.
      DATA(lv_date) = |{ input-posdate DATE = ISO }|.
*Debit Item Entries
      lt_glaccount = VALUE #( ( itemno_acc = lv_itmcnt
                                gl_account = l_debit_gl-saknr
                                costcenter = lv_costcenter
                                fund       = lv_fund
                                cmmt_item  = lv_glacc
                                item_text  = |LBR Charges on { lv_date }|
                                plant      = lv_plant ) ).

*Debit Currency Lineitems
      DATA(lv_amount) = CONV char25( input-amount ).
      lt_curramnt = VALUE #( ( itemno_acc = lv_itmcnt
                               currency = 'INR'
                               amt_doccur = lv_amount
                               amt_base = lv_amount  ) ).

      APPEND VALUE #( itemno_acc = lv_itmcnt + 1
                      gl_account = l_credit_gl-saknr
                     " costcenter = lv_costcenter
                      fund       = lv_fund
                      cmmt_item  = lv_glacc1
                      item_text  = |LBR Charges on { lv_date }| ) TO lt_glaccount.
*Credit Currency Lineitems
      APPEND VALUE #( itemno_acc = lv_itmcnt + 1
                      currency   = 'INR'
                      amt_doccur = lv_amount * -1
                      amt_base   = lv_amount * -1 ) TO lt_curramnt.
*Document Check
      CLEAR lt_return.
      CALL FUNCTION 'BAPI_ACC_DOCUMENT_CHECK'
        EXPORTING
          documentheader = ls_header
        TABLES
          accountgl      = lt_glaccount
          currencyamount = lt_curramnt
          return         = lt_return.
      DELETE lt_return[] WHERE id = 'RW'  AND number EQ '609'.
      LOOP AT lt_return ASSIGNING FIELD-SYMBOL(<fs_ret>) WHERE ( type = 'E' OR type = 'A' ).
        CLEAR l_msg.
        l_msg = |{ <fs_ret>-message }{ <fs_ret>-message_v1 }{ <fs_ret>-message_v2 }{ <fs_ret>-message_v3 }{ <fs_ret>-message_v4 }|.
        CONDENSE l_msg NO-GAPS.
        msg = |{ msg }/{ l_msg }|.
      ENDLOOP.
      IF sy-subrc NE 0.
        REFRESH: lt_return.
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
            return         = lt_return.
        COMMIT WORK AND WAIT.
        belnr   = lv_objkey+0(10).
        fisyear = lv_objkey+14(4).
        msg     = |Document{ belnr } Posted Successfully|.
        DATA(log) = VALUE zfi_farm_ent_log( mandt   = sy-mandt
                                            bukrs   = input-compcode
                                            xblnr   = input-refdoc
                                            blart   = 'SA'
                                            enttype = input-entrytype
                                            bldat   = input-docdate
                                            budat   = input-posdate
                                            workctr = input-workctr
                                            fundctr = lv_costcenter
                                            amount  = input-amount
                                            plant   = lv_plant
                                            fromgl  = l_debit_gl-saknr
                                            togl    = l_credit_gl-saknr
                                            erdat   = sy-datum
                                            ernam   = sy-uname
                                            erzet   = sy-uzeit
                                            belnr   = belnr
                                            gjahr   = fisyear
                                            type    = 'S'
                                            message = msg ).
        MODIFY zfi_farm_ent_log FROM log.
      ELSE.
        log = VALUE zfi_farm_ent_log( mandt   = sy-mandt
                                      bukrs   = input-compcode
                                      xblnr   = input-refdoc
                                      enttype = input-entrytype
                                      bldat   = input-docdate
                                      budat   = input-posdate
                                      workctr = input-workctr
                                      fundctr = lv_costcenter
                                      amount  = input-amount
                                      fromgl  = l_debit_gl-saknr
                                      togl    = l_credit_gl-saknr
                                      plant   = lv_plant
                                      erdat   = sy-datum
                                      ernam   = sy-uname
                                      erzet   = sy-uzeit
                                      type    = 'E'
                                      message = msg ).
        MODIFY zfi_farm_ent_log FROM log.
      ENDIF.
    ELSE.
      RAISE no_input.
    ENDIF.
  ENDMETHOD.


  METHOD create_vendor_invoice.

    TYPES: BEGIN OF ty_withtx,
             lifnr     TYPE lfbw-lifnr,
             witht     TYPE lfbw-witht,
             wt_withcd TYPE lfbw-wt_withcd,
             qproz     TYPE t059z-qproz,
             qsatz     TYPE t059z-qsatz,
             konth     TYPE t030-konth,
           END OF ty_withtx.
    DATA: gt_withtax TYPE TABLE OF ty_withtx.

    DATA: lv_fisyear TYPE bapi0002_4-fiscal_year,
          lv_month   TYPE bapi0002_4-fiscal_period,
          l_return   TYPE bapireturn1,
          lv_fund    TYPE char10.

    DATA: lt_payable   TYPE TABLE OF bapiacap09,
          lt_accountgl TYPE TABLE OF bapiacgl09,
          lt_currency  TYPE TABLE OF bapiaccr09.

    DATA: lt_withtax TYPE TABLE OF  bapiacwt09,
          ls_withtax TYPE bapiacwt09.
    DATA: lt_return TYPE bapiret2_t.

    DATA: lv_objtyp TYPE bapiache09-obj_type.
    DATA: lv_objkey TYPE bapiache09-obj_key.
    DATA: lv_objsys TYPE bapiache09-obj_sys.

    DATA: lv_msg TYPE string,
          l_msg  TYPE string.

    CLEAR: lv_fisyear,lv_month,l_return.
    CALL FUNCTION 'BAPI_COMPANYCODE_GET_PERIOD'
      EXPORTING
        companycodeid = input-compcode
        posting_date  = input-posdate
      IMPORTING
        fiscal_year   = lv_fisyear
        fiscal_period = lv_month
        return        = l_return.

    DATA(ls_header) = VALUE bapiache09( bus_act    = 'RFBU'
                                        username   = sy-uname
                                        comp_code  = input-compcode
                                        doc_date   = input-docdate
                                        pstng_date = input-posdate
                                        fisc_year  = lv_fisyear
                                        doc_type   = 'KR'
                                        ref_doc_no = input-refdoc ).

    REFRESH: lt_payable,lt_accountgl,lt_currency.
**** Vendor Line items to be passed ***
    lt_payable = VALUE #(
                 ( itemno_acc  = '1'
                   vendor_no   = input-vendor
                   bline_date  = input-docdate
                   item_text   = input-itemtxt ) ).
    APPEND VALUE #( itemno_acc = '1'
                    currency   = 'INR'
                    amt_doccur = input-amount * -1
                    amt_base   = input-amount * -1  ) TO lt_currency."Vendor Line item

    CLEAR lv_fund.
    CALL FUNCTION 'ZFUND_GET'
      EXPORTING
        date       = input-posdate
      IMPORTING
        lv_fund    = lv_fund
      EXCEPTIONS
        enter_date = 1
        OTHERS     = 2.
    IF sy-subrc <> 0.

    ENDIF.

    SELECT SINGLE * FROM zfarm_glacc_mast INTO @DATA(l_glacc)
      WHERE enttype = @input-entrytype AND bschl = '40'.
    IF sy-subrc = 0.
      DATA(lv_glacc) = |{ l_glacc-saknr ALPHA = IN }|.
      SHIFT lv_glacc LEFT DELETING LEADING '0'.
    ENDIF.

    SELECT SINGLE objid FROM crhd INTO @DATA(lv_objid)
      WHERE objty = 'A' AND arbpl = @input-workctr.
    IF sy-subrc = 0.
      SELECT SINGLE kostl FROM crco INTO @DATA(lv_costcenter)
        WHERE objty = 'A' AND objid = @lv_objid.
      fundctr = lv_costcenter.
    ENDIF.

    APPEND VALUE #( itemno_acc  = '2'
                    gl_account  = l_glacc-saknr
                    fund        = lv_fund
                    item_text   = input-itemtxt
                    cmmt_item   = lv_glacc
                    costcenter  = lv_costcenter ) TO lt_accountgl.

    APPEND VALUE #( itemno_acc  = '2'
                    currency    = 'INR'
                    amt_doccur  = input-amount
                    amt_base    = input-amount
                  ) TO lt_currency .

    REFRESH: gt_withtax.
*TDS Calculation for Vendors
    SELECT a~lifnr
           b~witht
           b~wt_withcd
           b~qproz
           b~qsatz
           c~konth
       INTO CORRESPONDING FIELDS OF TABLE gt_withtax
       FROM lfbw AS a
       INNER JOIN t059z AS b ON b~witht = a~witht
                             AND b~wt_withcd = a~wt_withcd
       INNER JOIN t030 AS c ON  c~bwmod  = b~witht
                             AND c~komok = b~wt_withcd
       WHERE a~lifnr EQ input-vendor AND
             a~bukrs EQ input-compcode AND
             a~wt_subjct = 'X' AND
             b~land1 = 'IN' AND
             c~ktopl = 'YAIN' AND
             c~ktosl = 'WIT'.
    SORT gt_withtax[] BY lifnr.

    IF gt_withtax[] IS NOT INITIAL.
      REFRESH: lt_withtax.
      CLEAR ls_withtax.
      ls_withtax-itemno_acc = '1'.
      ls_withtax-wt_type = VALUE #( gt_withtax[ lifnr = input-vendor ]-witht OPTIONAL ).
      ls_withtax-wt_code = VALUE #( gt_withtax[ lifnr = input-vendor ]-wt_withcd OPTIONAL ).
      ls_withtax-bas_amt_tc = input-amount.
      APPEND ls_withtax TO lt_withtax.
    ENDIF.

    REFRESH: lt_return.
*** Document Check Before posting ***
    CALL FUNCTION 'BAPI_ACC_DOCUMENT_CHECK'
      EXPORTING
        documentheader = ls_header
      TABLES
        accountgl      = lt_accountgl
        accountpayable = lt_payable
        currencyamount = lt_currency
        return         = lt_return
        accountwt      = lt_withtax.
    LOOP AT lt_return INTO DATA(ls_ret) WHERE ( type = 'E' OR type = 'A' ).
      IF ls_ret-id <> 'RW' AND ls_ret-number <> '609'.
        CLEAR l_msg.
        l_msg = |{ ls_ret-message } { ls_ret-message_v3 }|.
        msg = |{ msg },{ l_msg }|.
      ENDIF.
    ENDLOOP.
    IF sy-subrc NE 0.
      REFRESH: lt_return.
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
          accountgl      = lt_accountgl
          accountpayable = lt_payable
          currencyamount = lt_currency
          return         = lt_return
          accountwt      = lt_withtax.
      IF sy-subrc = 0.
        COMMIT WORK AND WAIT.
        belnr = lv_objkey+0(10).
        fisyear = lv_objkey+14(4).
        msg = |Document Posted Successfully|.
        DATA(log) = VALUE zfi_farm_ent_log( mandt   = sy-mandt
                                                      bukrs   = input-compcode
                                                      xblnr   = input-refdoc
                                                      blart   = 'KR'
                                                      enttype = input-entrytype
                                                      vendor  = input-vendor
                                                      bldat   = input-docdate
                                                      budat   = input-posdate
                                                      workctr = input-workctr
                                                      amount  = input-amount
                                                      fundctr = lv_costcenter
                                                      fromgl  = l_glacc-saknr
                                                      belnr   = belnr
                                                      gjahr   = fisyear
                                                      erdat   = sy-datum
                                                      ernam   = sy-uname
                                                      erzet   = sy-uzeit
                                                      type    = 'S'
                                                      message = lv_msg ).
        MODIFY zfi_farm_ent_log FROM log.
      ELSE.
        log = VALUE zfi_farm_ent_log( mandt   = sy-mandt
                                      bukrs   = input-compcode
                                      xblnr   = input-refdoc
                                      blart   = 'KR'
                                      enttype = input-entrytype
                                      vendor  = input-vendor
                                      bldat   = input-docdate
                                      budat   = input-posdate
                                      workctr = input-workctr
                                      amount  = input-amount
                                      fundctr = lv_costcenter
                                      fromgl  = l_glacc
                                      erdat   = sy-datum
                                      ernam   = sy-uname
                                      erzet   = sy-uzeit
                                      type    = 'E'
                                      message = msg ).
        MODIFY zfi_farm_ent_log FROM log.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
