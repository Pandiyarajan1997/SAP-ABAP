DATA : lv_kunnr TYPE kna1-kunnr.
DATA : lv_reg TYPE kna1-regio.
DATA : lv_days TYPE zpayterm_region-region_code .
IF sy-tcode = 'VF01' .
  LOOP AT xaccit WHERE kunnr NE ' ' .
    SELECT SINGLE regio FROM kna1 INTO lv_reg WHERE kunnr = xaccit-kunnr.
    SELECT SINGLE no_of_days FROM zpayterm_region INTO lv_days WHERE region_code = lv_reg AND payment_terms = xaccit-zterm.
    xaccit-zbd1t = xaccit-zbd1t + lv_days .
    MODIFY xaccit.
  ENDLOOP.
ENDIF.

IF sy-tcode = 'VF01' .

  DATA : lv_mail TYPE adr6-smtp_addr .
  DATA : lv_num TYPE vbrk-kunrg .

  DATA : lv_pla    TYPE vbrp-vkbur,
         lv_inv    TYPE vbrk-vbeln,
         lv_ind    TYPE  vbrk-fkdat,
         lv_ind1   TYPE char20,
         lv_nam    TYPE t001w-name1,
         lv_vbeln  TYPE vbrk-vbeln,
         lv_ret    TYPE char40,
         lv_vbeln1 TYPE vbrk-vbeln,
         lv_netwr  TYPE vbrk-netwr,
         lv_mwsbk  TYPE vbrk-mwsbk,
         lv_netwr2 TYPE vbrk-netwr,
         lv_netwr1 TYPE string.

  DATA : lv_tot TYPE string.
  DATA : lv_adr TYPE kna1-adrnr.

  DATA : gv_soli_tab      TYPE soli_tab, " Email Body
         gv_soli_tab_line TYPE LINE OF soli_tab.

  IF cvbrk-vkorg EQ '4000' .

    lv_pla = cvbrp-vkbur .
    lv_vbeln = cvbrk-vbeln .

    IF cvbrk-fkart EQ 'YBBR' .

      lv_ind = cvbrk-fkdat .
      CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT'
        EXPORTING
          input  = lv_ind
        IMPORTING
          output = lv_ind1.

      SELECT SINGLE vkbur INTO lv_pla FROM vbrp WHERE vbeln = cvbrk-vbeln  .

      SELECT SINGLE name1 INTO lv_nam FROM t001w WHERE werks = lv_pla .

      SELECT SINGLE adrnr INTO lv_adr FROM kna1 WHERE kunnr = cvbrk-kunag .

      SELECT SINGLE smtp_addr INTO lv_mail FROM adr6 WHERE addrnumber = lv_adr .

      SELECT SINGLE netwr INTO lv_netwr FROM vbrk WHERE vbeln = cvbrk-vbeln.

      SELECT SINGLE mwsbk INTO lv_mwsbk FROM vbrk WHERE vbeln = cvbrk-mwsbk.

      lv_netwr = cvbrk-netwr .
      lv_mwsbk = cvbrk-mwsbk .

      lv_netwr2 = lv_netwr + lv_mwsbk .

      lv_netwr1 = lv_netwr2 .

      CONCATENATE 'JENSON & NICHOLSON Invoice No :' lv_vbeln 'Date:' lv_ind1 'Value :' lv_netwr1 INTO lv_tot SEPARATED BY space .

      gv_soli_tab_line = 'Dear Customer,' .
      APPEND gv_soli_tab_line TO gv_soli_tab.

      gv_soli_tab_line = '           ' .
      APPEND gv_soli_tab_line TO gv_soli_tab.

      gv_soli_tab_line = '          With reference to your order, the above invoice has been generated at our end.'.
      APPEND gv_soli_tab_line TO gv_soli_tab.

      gv_soli_tab_line = '                                  ' .
      APPEND gv_soli_tab_line TO gv_soli_tab.

      gv_soli_tab_line = 'If you have any queries / doubts, kindly contact us on  8300030505 (or) send us the mail to CCC@jnpl.in'.
      APPEND gv_soli_tab_line TO gv_soli_tab.

      gv_soli_tab_line = '                                                                                          ' .
      APPEND gv_soli_tab_line TO gv_soli_tab.

      gv_soli_tab_line = '                                                                                          ' .
      APPEND gv_soli_tab_line TO gv_soli_tab.

      gv_soli_tab_line = 'Thanks & Best Regards,' .
      APPEND gv_soli_tab_line TO gv_soli_tab.

      gv_soli_tab_line = 'Customer Care (Jenson and Nicholson)' .
      APPEND gv_soli_tab_line TO gv_soli_tab.

      CALL FUNCTION 'EFG_GEN_SEND_EMAIL'
        EXPORTING
          i_title     = lv_tot
          i_sender    = 'CCC@JNPL.IN'
          i_recipient = lv_mail
*         I_FLG_COMMIT                 = 'X'
*         I_FLG_SEND_IMMEDIATELY       = 'X'
*         I_FLG_SENDER_IS_UNAME        =
        TABLES
          i_tab_lines = gv_soli_tab
*         I_TAB_RECIPIENTS             =
* EXCEPTIONS
*         NOT_QUALIFIED                = 1
*         FAILED      = 2
*         OTHERS      = 3
        .
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

    ENDIF.

*IF CVBRK-FKART EQ 'YBRE' .
*
**DATA : LV_MAIL TYPE ADR6-SMTP_ADDR .
**DATA : LV_NUM TYPE VBRK-KUNRG .
**
**DATA : LV_PLA TYPE VBRP-VKBUR ,
**       LV_INV TYPE VBRK-VBELN ,
**       LV_IND TYPE  VBRK-FKDAT ,
**       LV_IND1 TYPE CHAR20 ,
**       LV_NAM TYPE T001W-NAME1 ,
**       LV_VBELN TYPE VBRK-VBELN ,
**       LV_RET TYPE CHAR40 ,
**       LV_VBELN1 TYPE VBRK-VBELN .
**
**DATA : LV_TOT TYPE STRING.
**
**DATA : LV_ADR TYPE KNA1-ADRNR.
*
*
*LV_PLA = CVBRP-VKBUR .
*LV_VBELN = CVBRK-VBELN .
*
*LV_IND = CVBRK-FKDAT .
*CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT'
*  EXPORTING
*    INPUT         = LV_IND
* IMPORTING
*   OUTPUT        =  LV_IND1.
*
*
*
*SELECT SINGLE VKBUR INTO LV_PLA FROM VBRP WHERE VBELN = CVBRK-VBELN  .
*
*SELECT SINGLE NAME1 INTO LV_NAM FROM T001W WHERE WERKS = LV_PLA .
*
*SELECT SINGLE ADRNR INTO LV_ADR FROM KNA1 WHERE KUNNR = CVBRK-KUNAG .
*
*SELECT SINGLE SMTP_ADDR INTO LV_MAIL FROM ADR6 WHERE ADDRNUMBER = LV_ADR .
*
*
*CONCATENATE LV_PLA','LV_NAM','LV_VBELN ',' LV_IND1 INTO LV_TOT SEPARATED BY SPACE .
*
*
*
*
*gv_soli_tab_line = 'Dear CFA Agent,' .
*APPEND gv_soli_tab_line TO gv_soli_tab.
*
*gv_soli_tab_line = '           ' .
*APPEND gv_soli_tab_line TO gv_soli_tab.
*
*gv_soli_tab_line = '          Kindly note that we updated above Sales or Material Return  in the system. Your system stock is added with these return materials.' .
*APPEND gv_soli_tab_line TO gv_soli_tab.
*
*
*gv_soli_tab_line = '                                                                                          ' .
*APPEND gv_soli_tab_line TO gv_soli_tab.
*
*gv_soli_tab_line = ' If you have any dispute on the Sales or Material Return, kindly contact us within 48 hours with details of the variation, at CCC@jnpl.in. If not reported within the time line, no future claims would be entertained in this regard.' .
*APPEND gv_soli_tab_line TO gv_soli_tab.
*
*gv_soli_tab_line = '                                                                                          ' .
*APPEND gv_soli_tab_line TO gv_soli_tab.
*
*gv_soli_tab_line = '                                                                                          ' .
*APPEND gv_soli_tab_line TO gv_soli_tab.
*
*
*gv_soli_tab_line = 'Thanks & Best Regards,' .
*APPEND gv_soli_tab_line TO gv_soli_tab.
*
*gv_soli_tab_line = 'JNPL Customer Care team' .
*APPEND gv_soli_tab_line TO gv_soli_tab.
*
*CALL FUNCTION 'EFG_GEN_SEND_EMAIL'
*  EXPORTING
*    I_TITLE                      = LV_TOT
*    I_SENDER                     = 'CCC@JNPL.IN'
*    I_RECIPIENT                  = LV_MAIL
**   I_FLG_COMMIT                 = 'X'
**   I_FLG_SEND_IMMEDIATELY       = 'X'
**   I_FLG_SENDER_IS_UNAME        =
*  TABLES
*    I_TAB_LINES                  = gv_soli_tab
**   I_TAB_RECIPIENTS             =
** EXCEPTIONS
**   NOT_QUALIFIED                = 1
**   FAILED                       = 2
**   OTHERS                       = 3
*          .
*IF SY-SUBRC <> 0.
** Implement suitable error handling here
*ENDIF.
*
*ENDIF.

  ENDIF.

  IF cvbrk-vkorg EQ '1000' .

    lv_pla = cvbrp-vkbur .
    lv_vbeln = cvbrk-vbeln .

    IF cvbrk-fkart EQ 'YBBR' OR cvbrk-fkart EQ 'YBDP' OR cvbrk-fkart EQ 'YRF2' OR cvbrk-fkart EQ 'YBTE' OR cvbrk-fkart EQ 'YBFS' .

      lv_ind = cvbrk-fkdat .
      CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT'
        EXPORTING
          input  = lv_ind
        IMPORTING
          output = lv_ind1.

      SELECT SINGLE vkbur INTO lv_pla FROM vbrp WHERE vbeln = cvbrk-vbeln  .

      SELECT SINGLE name1 INTO lv_nam FROM t001w WHERE werks = lv_pla .

      SELECT SINGLE adrnr INTO lv_adr FROM kna1 WHERE kunnr = cvbrk-kunag .

** Added on 10.10.2022
      IF sy-sysid = 'DEV' OR sy-sysid = 'QAS'.
        CLEAR lv_mail.
        SELECT SINGLE low FROM tvarvc INTO @DATA(ls_tvarvc)
                                      WHERE name = 'ZRLB_INVOICE'
                                      AND type = 'P'.
        lv_mail = ls_tvarvc.
      ELSE.
        SELECT SINGLE smtp_addr INTO lv_mail FROM adr6 WHERE addrnumber = lv_adr .
      ENDIF.
** End of changes on 10.10.2022

      SELECT SINGLE netwr INTO lv_netwr FROM vbrk WHERE vbeln = cvbrk-vbeln.

      SELECT SINGLE mwsbk INTO lv_mwsbk FROM vbrk WHERE vbeln = cvbrk-mwsbk.

      lv_netwr = cvbrk-netwr .
      lv_mwsbk = cvbrk-mwsbk .

      lv_netwr2 = lv_netwr + lv_mwsbk .

      lv_netwr1 = lv_netwr2 .

      CONCATENATE 'Sheenlac Paints Limited Invoice No :' lv_vbeln 'Date:' lv_ind1 'Value :' lv_netwr1 INTO lv_tot SEPARATED BY space .

      gv_soli_tab_line = 'Dear Customer,' .
      APPEND gv_soli_tab_line TO gv_soli_tab.

      gv_soli_tab_line = '           ' .
      APPEND gv_soli_tab_line TO gv_soli_tab.

      gv_soli_tab_line = '          With reference to your order, the above invoice has been generated at our end.'.
      APPEND gv_soli_tab_line TO gv_soli_tab.

      gv_soli_tab_line = '                                  ' .
      APPEND gv_soli_tab_line TO gv_soli_tab.

      gv_soli_tab_line = 'If you have any queries / doubts, kindly contact us on 8300030404 (or) send us the mail to ccchennai@sheenlac.in'.
      APPEND gv_soli_tab_line TO gv_soli_tab.

      gv_soli_tab_line = '                                                                                          ' .
      APPEND gv_soli_tab_line TO gv_soli_tab.

      gv_soli_tab_line = '                                                                                          ' .
      APPEND gv_soli_tab_line TO gv_soli_tab.

      gv_soli_tab_line = 'Thanks & Best Regards,' .
      APPEND gv_soli_tab_line TO gv_soli_tab.

      gv_soli_tab_line = 'Customer Care (Sheenlac Paints Ltd)' .
      APPEND gv_soli_tab_line TO gv_soli_tab.

      CALL FUNCTION 'EFG_GEN_SEND_EMAIL'
        EXPORTING
          i_title     = lv_tot
          i_sender    = 'ccchennai@sheenlac.in'
          i_recipient = lv_mail
*         I_FLG_COMMIT                 = 'X'
*         I_FLG_SEND_IMMEDIATELY       = 'X'
*         I_FLG_SENDER_IS_UNAME        =
        TABLES
          i_tab_lines = gv_soli_tab
*         I_TAB_RECIPIENTS             =
* EXCEPTIONS
*         NOT_QUALIFIED                = 1
*         FAILED      = 2
*         OTHERS      = 3
        .
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

    ENDIF.

*Added by: Samsudeen M
*Added on: 05.09.2023
*Purpose: Customer Financing Entries Capture
    DATA: lo_cust_fin TYPE REF TO zcl_scm_sales_to_invoice.
    CREATE OBJECT lo_cust_fin.
    DATA: lv_fisyear TYPE bapi0002_4-fiscal_year,
          lv_month   TYPE bapi0002_4-fiscal_period,
          l_return   TYPE bapireturn1.
    IF cvbrk-fkdat IS NOT INITIAL.
      CLEAR: lv_fisyear,lv_month,l_return.
      CALL FUNCTION 'BAPI_COMPANYCODE_GET_PERIOD'
        EXPORTING
          companycodeid = cvbrk-bukrs
          posting_date  = cvbrk-fkdat
        IMPORTING
          fiscal_year   = cvbrk-gjahr
          fiscal_period = lv_month
          return        = l_return.
    ENDIF.
    CALL METHOD lo_cust_fin->financing
      EXPORTING
        ls_vbrk = cvbrk.
  ENDIF.

ENDIF.
