FUNCTION zfund_get.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(DATE) TYPE  DATUM
*"  EXPORTING
*"     REFERENCE(LV_FUND) TYPE  CHAR10
*"  EXCEPTIONS
*"      ENTER_DATE
*"----------------------------------------------------------------------
  IF date IS NOT INITIAL.
    CLEAR lv_fund.
    CASE date+4(2).
      WHEN '01'.
        lv_fund = 'FUNDX'.
      WHEN '02'.
        lv_fund = 'FUNDXI'.
      WHEN '03'.
        lv_fund = 'FUNDXII'.
      WHEN '04'.
        lv_fund = 'FUNDI'.
      WHEN '05'.
        lv_fund = 'FUNDII'.
      WHEN '06'.
        lv_fund = 'FUNDIII'.
      WHEN '07'.
        lv_fund = 'FUNDIV'.
      WHEN '08'.
        lv_fund = 'FUNDV'.
      WHEN '09'.
        lv_fund = 'FUNDVI'.
      WHEN '10'.
        lv_fund = 'FUNDVII'.
      WHEN '11'.
        lv_fund = 'FUNDVIII'.
      WHEN '12'.
        lv_fund = 'FUNDIX'.
    ENDCASE.
  ELSE.
    RAISE enter_date.
  ENDIF.




ENDFUNCTION.
