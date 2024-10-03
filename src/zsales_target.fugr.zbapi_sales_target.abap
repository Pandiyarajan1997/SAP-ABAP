FUNCTION zbapi_sales_target.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IT_MON) TYPE  ZMONT
*"     VALUE(IT_YEAR) TYPE  ZYEA
*"  TABLES
*"      IT_SALES_TARGET STRUCTURE  ZSALES_MONTH_TG
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IT_MON) TYPE  ZMONT
*"     VALUE(IT_YEAR) TYPE  ZYEA
*"  TABLES
*"      IT_SALES_TARGET STRUCTURE  ZSALES_MONTH_TG
*CHANGED BY - KPABAP (SAMSUDEEN)
*CHANGED_ON - 02.05.2022
*REFERENCE - RAMAKRISHNAN.
* DESCRIPTION - Adding one new field in output structure (DMI_TARGET)
* TR-NO :  DEVK931666
*"----------------------------------------------------------------------

  TYPES: BEGIN OF ty_zsales_month_tgt,
           employ_code   TYPE zsales_month_tgt-employ_code,
           emp_name      TYPE  zsales_month_tgt-emp_name,
           mon           TYPE  zsales_month_tgt-mon,
           yr            TYPE zsales_month_tgt-yr,
           cx            TYPE  zsales_month_tgt-cx,
           cy            TYPE  zsales_month_tgt-cy,
           deco          TYPE zsales_month_tgt-deco,
           snc           TYPE zsales_month_tgt-snc,
           project       TYPE zsales_month_tgt-project,
           dmi_target    TYPE zsales_month_tgt-dmi_target,
           cky           TYPE zsales_month_tgt-cky,
           active_dealer TYPE zsales_month_tgt-active_dealer,
           ppi           TYPE zsales_month_tgt-ppi,
           focus_prdt1   TYPE zsales_month_tgt-focus_prdt1,
           focus_prdt2   TYPE zsales_month_tgt-focus_prdt2,
           dmi_target1   TYPE wertv8,
           dmi_target2   TYPE wertv8,
           dmi_target3   TYPE wertv8,
           dmi_target4   TYPE wertv8,
         END OF ty_zsales_month_tgt.

  DATA: it_sales_tg TYPE TABLE OF ty_zsales_month_tgt,
        wa_sales_tg TYPE ty_zsales_month_tgt.

  SELECT
      employ_code
      emp_name
      mon
      yr
      cx
      cy
      deco
      snc
      project
      dmi_target
      cky
      active_dealer
      ppi
      focus_prdt1
      focus_prdt1
      dmi_target1
      dmi_target2
      dmi_target3
      dmi_target4 INTO TABLE it_sales_tg
                  FROM zsales_month_tgt WHERE mon EQ it_mon AND yr EQ it_year.

  LOOP AT it_sales_tg INTO wa_sales_tg.
    it_sales_target-employ_code = wa_sales_tg-employ_code.
    it_sales_target-emp_name    = wa_sales_tg-emp_name.
    it_sales_target-mon         = wa_sales_tg-mon     .
    it_sales_target-yr          = wa_sales_tg-yr      .
    it_sales_target-cx          = wa_sales_tg-cx      .
    it_sales_target-cy          = wa_sales_tg-cy      .
    it_sales_target-deco        = wa_sales_tg-deco    .
    it_sales_target-snc         = wa_sales_tg-snc     .
    it_sales_target-project     = wa_sales_tg-project .
    it_sales_target-dmi_target = wa_sales_tg-dmi_target.
    it_sales_target-cky         = wa_sales_tg-cky    .
    it_sales_target-active_dealer = wa_sales_tg-active_dealer.
    it_sales_target-ppi = wa_sales_tg-ppi.
    it_sales_target-focus_prdt1 =  wa_sales_tg-focus_prdt1.
    it_sales_target-focus_prdt2 = wa_sales_tg-focus_prdt2.
    it_sales_target-dmi_target1 = wa_sales_tg-dmi_target1.
    it_sales_target-dmi_target2 = wa_sales_tg-dmi_target2.
    it_sales_target-dmi_target3 = wa_sales_tg-dmi_target3.
    it_sales_target-dmi_target4 = wa_sales_tg-dmi_target4.
    APPEND it_sales_target.
  ENDLOOP.





ENDFUNCTION.
