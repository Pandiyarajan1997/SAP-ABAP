FUNCTION zbapi_budget_calculation_mis.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(COMPANY_CODE) TYPE  BUKRS
*"     VALUE(FISCAL_YEAR) TYPE  GNJHR
*"     VALUE(MONTH) TYPE  ZMONTHS
*"  TABLES
*"      LT_BUDGET STRUCTURE  ZSTR_BUDGET
*"----------------------------------------------------------------------
  DATA:lt_annual_totals	TYPE fmavc_t_aco_annual_totals,
       lS_annual_totals	TYPE fmavc_s_aco_annual_totals,
       p_fikrs          TYPE fikrs,
       p_ledg           TYPE buavc_aldnr,
       ls_fiscyear_sel  TYPE gusl_s_range,  "GUSL_S_RANGE
       lt_fiscyear_sel  TYPE gusl_t_range,
       ld_i_aldnr       TYPE buavc_aldnr,
       ls_empty_sel     TYPE gusl_s_selection, "GUSL_S_SELECTION
       lt_empty_sel     TYPE gusl_t_selection. "GUSL_S_SELECTION

  DATA: ls_range TYPE gusl_s_range.
  DATA: lt_cskt TYPE STANDARD TABLE OF cskt,
        ls_cskt TYPE cskt,
        lt_skat TYPE STANDARD TABLE OF skat,
        ls_skat TYPE skat.

  DATA: lt_cost_txt     TYPE STANDARD TABLE OF cskt,
        ls_cost_txt     TYPE cskt,
        lt_accounts_txt TYPE STANDARD TABLE OF skat,
        ls_accounts_txt TYPE skat.

  DATA: ls_budget TYPE zstr_budget.

  p_fikrs = company_code  .
  p_ledg = '9H'.

  ls_fiscyear_sel-sign = 'I'.
  ls_fiscyear_sel-option = 'EQ'.
  ls_fiscyear_sel-low = fiscal_year.
  APPEND ls_fiscyear_sel TO lt_fiscyear_sel.


  ls_empty_sel-fieldname = 'RFUND'.
  CLEAR ls_range.
  ls_range-sign = 'I'.
  ls_range-option = 'EQ'.
  CASE month .
    WHEN '01'.
      ls_range-low = 'FUNDI'.
    WHEN '02'.
      ls_range-low = 'FUNDII'.
    WHEN '03'.
      ls_range-low = 'FUNDIII'.
    WHEN '04'.
      ls_range-low = 'FUNDIV'.
    WHEN '05'.
      ls_range-low = 'FUNDV'.
    WHEN '06'.
      ls_range-low = 'FUNDVI'.
    WHEN '07'.
      ls_range-low = 'FUNDVII'.
    WHEN '08'.
      ls_range-low = 'FUNDVIII'.
    WHEN '09'.
      ls_range-low = 'FUNDIX'.
    WHEN '10'.
      ls_range-low = 'FUNDX'.
    WHEN '11'.
      ls_range-low = 'FUNDXI'.
    WHEN '12'.
      ls_range-low = 'FUNDXII'.
    WHEN OTHERS.
  ENDCASE.
  APPEND ls_range TO ls_empty_sel-t_range.
  APPEND ls_empty_sel TO lt_empty_sel.


  CALL FUNCTION 'FMAVC_SELECT_MULTAN_TOTALS_ACO'
    EXPORTING
      i_fm_area         = p_fikrs
      i_fiscyear_sel    = lt_fiscyear_sel
      i_aldnr           = p_ledg
      i_t_address_sel   = lt_empty_sel
    IMPORTING
      e_t_annual_totals = lt_annual_totals
    EXCEPTIONS
      access_failure    = 1
      OTHERS            = 2.


  IF LT_annual_totals[] IS NOT INITIAL.

    SELECT * FROM cskt
      INTO TABLE lt_cost_txt
     WHERE kokrs = '1000'
       AND spras EQ sy-langu.

    SELECT * FROM skat
      INTO TABLE lt_accounts_txt
     WHERE ktopl EQ 'YAIN'
       AND spras EQ sy-langu.


    LOOP AT LT_annual_totals INTO lS_annual_totals.
      CLEAR ls_budget.

      ls_budget-zyear = fiscal_year.
      ls_budget-zmonth = month.
      ls_budget-company_code = company_code.
      ls_budget-rfund = ls_range-low.
      ls_budget-costcenter = ls_annual_totals-address-rfundsctr.

      READ TABLE lt_cost_txt INTO ls_cost_txt WITH KEY kostl = ls_budget-costcenter.
      IF sy-subrc EQ 0.
        ls_budget-costcenter_text = ls_cost_txt-ltext.
      ENDIF.

      ls_budget-gl_account_no = ls_annual_totals-address-rcmmtitem.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input         = ls_budget-gl_account_no
       IMPORTING
         OUTPUT        = ls_budget-gl_account_no
                .

      READ TABLE lt_accounts_txt INTO ls_accounts_txt WITH KEY saknr = ls_budget-gl_account_no.
      IF sy-subrc EQ 0.
        ls_budget-gl_account_name = ls_accounts_txt-txt50.
      ENDIF.

      ls_budget-assigned_budget = ls_annual_totals-consumable_bdgt_posted.
      ls_budget-used_buget = ls_annual_totals-consumed_amount_posted.
      ls_budget-remaining_budget = ls_budget-assigned_budget - ls_budget-used_buget.

      APPEND ls_budget TO lt_budget.

    ENDLOOP.

  ENDIF.

ENDFUNCTION.
