class ZCL_COMMON_CHECK definition
  public
  final
  create public .

public section.

  methods FI_PERIOD_CHECK
    importing
      !POSTING_DATE type BUDAT
      !ACCT_TYPE type T001B-MKOAR
      !COM_CODE type BUKRS
    exporting
      !TYPE type CHAR1
      !MESSAGE type CHAR100
    exceptions
      MANDATORY .
  methods MM_PERIOD_CHECK
    importing
      !COM_CODE type BUKRS
      !DATE type BUDAT
    exporting
      !TYPE type CHAR1
      !MESSAGE type CHAR100
    exceptions
      MANDATORY .
  methods FI_BUDGET_CHECK
    importing
      !COM_CODE type BUKRS
      !GEN_LEDG type SAKNR
      !COSTCENTER type KOSTL
      !DATE type BUDAT
      !AMOUNT type WRBTR
    exporting
      !TYPE type CHAR1
      !MESSAGE type CHAR100
    exceptions
      MANDATORY .
  methods CUSTOMER_BLOCK_CHECK
    importing
      !BUKRS type BUKRS
      !VKORG type VKORG
    changing
      !CUST_TAB type ZSD_TT_CUST_BLOCK
    exceptions
      KUNNR_NOT_FILLED_IN_TABLE .
  methods VENDOR_BLOCK_CHECK
    importing
      !COM_CODE type BUKRS
      !VEND_NO type LIFNR
    exporting
      !TYPE type CHAR1
      !MESSAGE type CHAR100
    exceptions
      MANDATORY .
  methods SD_INV_NO_RANGE_CHECK
    importing
      value(AUART) type AUART
      !VKBUR type VKBUR
      !WERKS type WERKS_D
    exporting
      !NUMKI type NUMKI
      !RETURN type TEXT100 .
  methods GROUP_FEATURE_FILL
    importing
      !SO_KUNNR type /ACCGO/CAK_TT_CUSTOMER_RANGE optional
    exporting
      !LT_GRPFEATURE type ZTT_GRP_FEATURE .
  methods DMS_PROCESS_STOP
    importing
      !KUNNR type KUNNR
      !PROCESS_TYPE type ZHTYPE
    exporting
      !TYPE type BAPI_MTYPE
      !MESSAGE type STRING .
protected section.
private section.
ENDCLASS.



CLASS ZCL_COMMON_CHECK IMPLEMENTATION.


  METHOD customer_block_check.        "customer is blocked or Not
    IF cust_tab IS INITIAL.
      SELECT b~bukrs
             c~vkorg
             a~kunnr
             INTO CORRESPONDING FIELDS OF TABLE cust_tab
             FROM kna1 AS a INNER JOIN knb1 AS b ON a~kunnr = b~kunnr
                            INNER JOIN knvv	AS c ON a~kunnr = c~kunnr
       WHERE b~bukrs = bukrs
         AND c~vkorg = vkorg
         AND c~vtweg = '20'
         AND c~spart = '10'.
    ELSE.
      READ TABLE cust_tab TRANSPORTING NO FIELDS WITH KEY kunnr = ''.
      IF sy-subrc = 0.
        RAISE kunnr_not_filled_in_table.
      ENDIF.
    ENDIF.
    SELECT b~bukrs,
           c~vkorg,
           a~kunnr,
           a~aufsd,
           a~faksd,
           a~lifsd,
           a~loevm,
           a~sperr,
           a~cassd,
           a~nodel,
           b~nodel AS nodel1,
           b~loevm AS loevm1,
           c~loevm AS loevm2
           FROM kna1 AS a INNER JOIN knb1 AS b ON a~kunnr = b~kunnr
                          INNER JOIN knvv	AS c ON a~kunnr = c~kunnr
     INTO TABLE @DATA(lt_kunnr)
     FOR ALL ENTRIES IN @cust_tab
     WHERE a~kunnr = @cust_tab-kunnr
       AND b~bukrs = @cust_tab-bukrs
       AND c~vkorg = @cust_tab-vkorg
       AND c~vtweg = '20'
       AND c~spart = '10'.
    DATA lw_data LIKE LINE OF lt_kunnr.
    LOOP AT cust_tab ASSIGNING FIELD-SYMBOL(<fs>).
*      DATA(lw_data) = lt_kunnr[ bukrs = <fs>-bukrs vkorg = <fs>-vkorg kunnr = <fs>-kunnr ].
      READ TABLE lt_kunnr INTO lw_data WITH KEY bukrs = <fs>-bukrs vkorg = <fs>-vkorg kunnr = <fs>-kunnr.
      IF sy-subrc = 0.
        IF lw_data-aufsd  IS NOT INITIAL. <fs>-block = 'X'. CONTINUE. ENDIF.
        IF lw_data-faksd  IS NOT INITIAL. <fs>-block = 'X'. CONTINUE. ENDIF.
        IF lw_data-lifsd  IS NOT INITIAL. <fs>-block = 'X'. CONTINUE. ENDIF.
        IF lw_data-loevm  IS NOT INITIAL. <fs>-block = 'X'. CONTINUE. ENDIF.
        IF lw_data-sperr  IS NOT INITIAL. <fs>-block = 'X'. CONTINUE. ENDIF.
        IF lw_data-cassd  IS NOT INITIAL. <fs>-block = 'X'. CONTINUE. ENDIF.
        IF lw_data-nodel  IS NOT INITIAL. <fs>-block = 'X'. CONTINUE. ENDIF.
        IF lw_data-nodel1 IS NOT INITIAL. <fs>-block = 'X'. CONTINUE. ENDIF.
        IF lw_data-loevm1 IS NOT INITIAL. <fs>-block = 'X'. CONTINUE. ENDIF.
        IF lw_data-loevm2 IS NOT INITIAL. <fs>-block = 'X'. CONTINUE. ENDIF.
        CLEAR lw_data.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD fi_budget_check.

    DATA : lv_fyear   TYPE  bapi0002_4-fiscal_year,
           lv_fperiod TYPE  bapi0002_4-fiscal_period,
           l_return   TYPE  bapireturn1,
           lv_e_oper  TYPE  t001b-frpe1,
           lt_budget  TYPE TABLE OF  zstr_budget.

    IF com_code IS NOT INITIAL AND gen_ledg IS NOT INITIAL AND costcenter IS NOT INITIAL

      AND date IS NOT INITIAL AND amount IS NOT INITIAL.

      "Function Module to get Fiscal Year based On Date
      CLEAR: lv_fyear,lv_fperiod,l_return.
      CALL FUNCTION 'BAPI_COMPANYCODE_GET_PERIOD'
        EXPORTING
          companycodeid = com_code
          posting_date  = date
        IMPORTING
          fiscal_year   = lv_fyear
          fiscal_period = lv_fperiod
          return        = l_return.

      IF l_return IS INITIAL.

"Function Module to get BUDGET_CALCULATION_MIS

        CALL FUNCTION 'ZBAPI_BUDGET_CALCULATION_MIS'
          EXPORTING
            company_code = com_code
            fiscal_year  = lv_fyear
            month        = lv_fperiod
          TABLES
            lt_budget    = lt_budget.

      ENDIF.

      SORT : lt_budget BY gl_account_name costcenter.

      READ TABLE lt_budget INTO DATA(ls_budget) WITH KEY gl_account_no = gen_ledg
                                                    costcenter = costcenter BINARY SEARCH.

      IF sy-subrc = 0.

        IF ls_budget-remaining_budget LT amount.

          type = 'E'.

          message = |Budget is not sufficient|.

        ELSE.

          type = 'S'.

          message = |Budget is sufficient|.

        ENDIF.

      ELSE.

        type = 'E'.

        message = |Budget is not allotted|.

      ENDIF.


    ELSE.

      RAISE mandatory.

    ENDIF.






  ENDMETHOD.


  METHOD FI_PERIOD_CHECK.

    IF acct_type IS NOT INITIAL  AND posting_date IS NOT INITIAL AND com_code IS NOT INITIAL .

      DATA : lv_fyear   TYPE  bapi0002_4-fiscal_year,
             lv_fperiod TYPE  bapi0002_4-fiscal_period,
             l_return   TYPE  bapireturn1,
             lv_e_oper  TYPE  t001b-frpe1.

      "Function Module to get Fiscal Year based On Date
      CLEAR: lv_fyear,lv_fperiod,l_return.
      CALL FUNCTION 'BAPI_COMPANYCODE_GET_PERIOD'
        EXPORTING
          companycodeid = com_code
          posting_date  = posting_date
        IMPORTING
          fiscal_year   = lv_fyear
          fiscal_period = lv_fperiod
          return        = l_return.
      IF l_return IS INITIAL.
*Function Module to Check FI Posting Period is Open or Not
        DATA(lv_fisperiod) = CONV t001b-frpe1( lv_fperiod ).
        CALL FUNCTION 'FI_PERIOD_CHECK'
          EXPORTING
            i_bukrs          = com_code
            i_gjahr          = lv_fyear
            i_koart          = acct_type
            i_monat          = lv_fisperiod
          EXCEPTIONS
            error_period     = 1
            error_period_acc = 2
            invalid_input    = 3
            OTHERS           = 4.
        IF sy-subrc <> 0.
          type = 'E'.
*Function Module to build the error Messages
          CALL FUNCTION 'MESSAGE_TEXT_BUILD'
            EXPORTING
              msgid               = sy-msgid
              msgnr               = sy-msgno
              msgv1               = sy-msgv1
              msgv2               = sy-msgv1
              msgv3               = sy-msgv1
              msgv4               = sy-msgv1
            IMPORTING
              message_text_output = message.
        ELSE.
          type = 'S'.
          message = |Posting Period is Open|.
        ENDIF.
      ENDIF.
    ELSE.
      RAISE mandatory.
    ENDIF.




  ENDMETHOD.


  METHOD group_feature_fill.

    SELECT a~customer,
           a~businesspartner,
           b~group_feature,
           c~group_feature_na
      FROM ibupacustomer AS a
      INNER JOIN bp001 AS b ON
      b~partner = a~businesspartner
      INNER JOIN tp24t AS c ON
      c~group_feature = b~group_feature
      INTO TABLE @lt_grpfeature
      FOR ALL ENTRIES IN @so_kunnr
      WHERE customer  EQ @so_kunnr-low AND spras = 'EN'.

  ENDMETHOD.


  METHOD mm_period_check.

    IF com_code IS NOT INITIAL  AND date IS NOT INITIAL.

      CALL FUNCTION 'MM_PERIOD_CHECK'
        EXPORTING
          i_bukrs           = com_code
          i_budat           = date
*         I_KZRFB           =
*       IMPORTING
*         E_MONAT           =
*         E_GJAHR           =
        EXCEPTIONS
          invalid_mm_period = 1
          marv_no_entry     = 2
          fi_period_error   = 3
          OTHERS            = 4.
      IF sy-subrc <> 0.

        type = 'E'.

*Function Module to build the error Messages
        CALL FUNCTION 'MESSAGE_TEXT_BUILD'
          EXPORTING
            msgid               = sy-msgid
            msgnr               = sy-msgno
            msgv1               = sy-msgv1
            msgv2               = sy-msgv1
            msgv3               = sy-msgv1
            msgv4               = sy-msgv1
          IMPORTING
            message_text_output = message.
      ELSE.
        type = 'S'.
        message = |Posting Period is Open|.
      ENDIF.
    ELSE.
      RAISE mandatory.
    ENDIF.



  ENDMETHOD.


  METHOD sd_inv_no_range_check.
    CASE auart.
      WHEN 'ZHSS' OR 'IG' OR 'IGS'.
        return =  |Number range is not maintained for Billing Type-{ auart }|. " Tcode: ZSD_INV_NROBJ
      WHEN OTHERS.
        SELECT SINGLE numki
          INTO @DATA(us_range_intern)
          FROM zsd_inv_nr_obj
          WHERE fkart = @auart AND
                vkbur = @vkbur OR
                werks = @werks.
        IF sy-subrc <> 0.
          SELECT SINGLE numki
            INTO us_range_intern
            FROM zsd_inv_nr_obj
            WHERE vkbur = vkbur OR
                  werks = werks.
          IF sy-subrc <> 0.
            return = |Number range is not maintained for Billing Type-{ auart } Sales Office-{ vkbur } |. " Tcode: ZSD_INV_NROBJ
          ENDIF.
        ENDIF.
    ENDCASE.
  ENDMETHOD.


  METHOD vendor_block_check.      "Check mm vendor is blocked or Not

    IF com_code IS NOT INITIAL AND vend_no IS NOT INITIAL.

*      DATA : lv_blocked TYPE  cvp_xblck,
*             lv_subrc   TYPE  sy-subrc.


      SELECT SINGLE lifnr,nodel,sperr,loevm FROM lfa1 INTO @DATA(ls_lfa1)
                                                  WHERE lifnr = @vend_no.

      IF ls_lfa1-nodel IS NOT INITIAL OR ls_lfa1-loevm IS NOT INITIAL.

        type = 'E'.

        message = |This Vendor is Blocked|.

      ELSEIF ls_lfa1-sperr IS NOT INITIAL.

        type = 'E'.

        message = |This Vendor Posting is Blocked|.

      ELSE.

        SELECT SINGLE lifnr,nodel,sperr,loevm FROM lfb1 INTO @DATA(ls_lfb1)
                              WHERE lifnr = @vend_no AND bukrs = @com_code.

        IF ls_lfb1-nodel IS NOT INITIAL OR ls_lfb1-loevm IS NOT INITIAL.

          type = 'E'.

          message = |This Vendor is Blocked|.

        ELSEIF ls_lfb1-sperr IS NOT INITIAL.

          type = 'E'.

          message = |This Vendor Posting is Blocked|.

        ELSE.

          type = 'S'.

          message = |This Vendor is Not Blocked|.

        ENDIF.
      ENDIF.

**      Function Module to Check mm vendor is block or Not
*
*      CALL FUNCTION 'OIJ08_TRIP_CHECK_VENDOR_BLOCK'
*        EXPORTING
*          iv_supplier = vend_no
*          iv_bukrs    = com_code
*        IMPORTING
*          ev_blocked  = lv_blocked
*          ev_subrc    = lv_subrc.
*
*      IF lv_subrc = 0.
*
*        type = 'E'.
*
*        message = |This Vendor Is Blocked|.
*
*      ELSE.
*
*        type = 'S'.
*
*        message = |This Vendor Is Not Blocked|.
*
*      ENDIF.


    ELSE.

      RAISE mandatory.

    ENDIF.



  ENDMETHOD.


  METHOD dms_process_stop.
    DATA: idd07v TYPE TABLE OF  dd07v.
*************fetch from the customer process blocking table***********
    SELECT SINGLE * FROM zcust_procs_stp INTO @DATA(ls_custstp) WHERE kunnr = @kunnr
                                                                AND   htype = @process_type.
    IF sy-subrc = 0.
*******************get the fixed values***********
      CALL FUNCTION 'DD_DOMVALUES_GET'
        EXPORTING
          domname        = 'ZHTYPE'
          text           = 'X'
          langu          = sy-langu
        TABLES
          dd07v_tab      = idd07v
        EXCEPTIONS
          wrong_textflag = 1
          OTHERS         = 2.

      SORT : idd07v BY domvalue_l.
      READ TABLE idd07v INTO DATA(ls_idd07v) WITH KEY domvalue_l = type BINARY SEARCH.

      type    = 'E'.
      message = |Customer { ls_idd07v-ddtext } Process currently Blocked|.
    ELSE.
      type    = 'S'.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
