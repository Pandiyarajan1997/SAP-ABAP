FUNCTION zbapi_glacc_expenses.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(COMP_CODE) TYPE  BUKRS
*"     VALUE(GL_ACCOUNT) TYPE  BKK_R_HKONT_BCA OPTIONAL
*"     VALUE(FISC_YEAR) TYPE  GJAHR
*"     VALUE(PERIOD) TYPE  MONAT
*"  TABLES
*"      LT_GL_EXPENSE STRUCTURE  ZSTR_GLACC_EXP
*"----------------------------------------------------------------------


  TYPES: BEGIN OF ty_bsis,
           bukrs TYPE bukrs,
           hkont TYPE hkont,
           gjahr TYPE gjahr,
           belnr TYPE belnr_d,
           budat TYPE budat,
           bldat TYPE bldat,
           blart TYPE blart,
           waers TYPE waers,
           monat TYPE monat,
           shkzg TYPE shkzg,
           dmbtr TYPE dmbtr,
           werks TYPE werks_d,
           kostl TYPE kostl,
           sgtxt TYPE sgtxt,
           prctr TYPE prctr,
           geber TYPE bp_geber,
         END OF ty_bsis.

  DATA: lt_bsis TYPE TABLE OF ty_bsis,
        ls_bsis TYPE ty_bsis.

  DATA: lt_cost_txt     TYPE STANDARD TABLE OF cskt,
        ls_cost_txt     TYPE cskt,
        lt_accounts_txt TYPE STANDARD TABLE OF skat,
        ls_accounts_txt TYPE skat.

  RANGES r_hkont FOR bsis-hkont.

  DATA: ls_gl_expense TYPE zstr_glacc_exp.

  REFRESH : lt_bsis.


  IF comp_code IS NOT INITIAL  AND gl_account IS INITIAL
     AND fisc_year IS NOT INITIAL AND period IS NOT INITIAL .

*** Fetching all data from bsis ***
    SELECT bukrs
           hkont
           gjahr
           belnr
           budat
           bldat
           blart
           waers
           monat
           shkzg
           dmbtr
           werks
           kostl
           sgtxt
           prctr
           geber FROM bsis
                 INTO TABLE lt_bsis
                 WHERE bukrs = comp_code
                 AND gjahr = fisc_year
                 AND monat = period.
    IF sy-subrc EQ 0.
      SORT lt_bsis[] BY hkont.
    ENDIF.

  ELSEIF comp_code IS NOT INITIAL AND gl_account IS NOT INITIAL
         AND fisc_year IS NOT INITIAL AND period IS NOT INITIAL .

** Fetching all data from bsis ***
    SELECT bukrs
           hkont
           gjahr
           belnr
           budat
           bldat
           blart
           waers
           monat
           shkzg
           dmbtr
           werks
           kostl
           sgtxt
           prctr
           geber FROM bsis
                 INTO TABLE lt_bsis
                 WHERE bukrs = comp_code
                 AND hkont IN gl_account
                 AND gjahr = fisc_year
                 AND monat = period.
    IF sy-subrc EQ 0.
      SORT lt_bsis[] BY hkont.
    ENDIF.
  ENDIF.

  IF lt_bsis[] IS NOT INITIAL.

    SELECT * FROM cskt
             INTO TABLE lt_cost_txt
             FOR ALL ENTRIES IN lt_bsis
             WHERE kokrs = lt_bsis-bukrs
             AND kostl = lt_bsis-kostl
             AND spras EQ sy-langu.


    SELECT * FROM skat
             INTO TABLE lt_accounts_txt
             FOR ALL ENTRIES IN lt_bsis
             WHERE saknr = lt_bsis-hkont
             AND ktopl EQ 'YAIN'
             AND spras EQ sy-langu.

    SORT lt_cost_txt[] BY kostl.
    SORT lt_accounts_txt[] BY saknr.
  ENDIF.


  LOOP AT lt_bsis INTO ls_bsis.
    CLEAR ls_gl_expense.
    ls_gl_expense-bukrs = ls_bsis-bukrs.
    ls_gl_expense-plant = ls_bsis-werks.
    ls_gl_expense-gl_account = ls_bsis-hkont.

    CLEAR ls_accounts_txt.
    READ TABLE lt_accounts_txt INTO ls_accounts_txt WITH KEY saknr = ls_bsis-hkont.
    IF sy-subrc EQ 0.
      ls_gl_expense-glacc_des = ls_accounts_txt-txt50.
    ENDIF.

    ls_gl_expense-fisc_year = ls_bsis-gjahr.
    ls_gl_expense-fisc_period = ls_bsis-monat.
    ls_gl_expense-doc_number = ls_bsis-belnr.
    ls_gl_expense-doc_date = ls_bsis-bldat.
    ls_gl_expense-posting_date = ls_bsis-budat.
    ls_gl_expense-costcenter = ls_bsis-kostl.

    CLEAR ls_cost_txt.
    READ TABLE lt_cost_txt INTO ls_cost_txt WITH KEY kostl = ls_bsis-kostl.
    IF sy-subrc EQ 0.
      ls_gl_expense-costcenter_des = ls_cost_txt-ltext.
    ENDIF.

    ls_gl_expense-amount = ls_bsis-dmbtr.
    ls_gl_expense-currency = ls_bsis-waers.
    ls_gl_expense-profit_cntr = ls_bsis-prctr.
    ls_gl_expense-sgtxt = ls_bsis-sgtxt.
    ls_gl_expense-geber = ls_bsis-geber.
    ls_gl_expense-doc_type = ls_bsis-blart.
    ls_gl_expense-dc_indicator = ls_bsis-shkzg.

    APPEND ls_gl_expense TO lt_gl_expense.

    CLEAR ls_bsis.
  ENDLOOP.

ENDFUNCTION.
