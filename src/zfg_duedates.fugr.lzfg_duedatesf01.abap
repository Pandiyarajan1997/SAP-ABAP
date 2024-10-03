*----------------------------------------------------------------------*
***INCLUDE LZFG_DUEDATESF01.
*----------------------------------------------------------------------*
FORM new_entry.

  DATA: lt_date_attr TYPE TABLE OF casdayattr.

  IF zfi_due_dates-hdate IS NOT INITIAL.
    REFRESH: lt_date_attr.
    CALL FUNCTION 'DAY_ATTRIBUTES_GET'
      EXPORTING
        date_from                  = zfi_due_dates-hdate
        date_to                    = zfi_due_dates-hdate
        language                   = sy-langu
      TABLES
        day_attributes             = lt_date_attr
      EXCEPTIONS
        factory_calendar_not_found = 1
        holiday_calendar_not_found = 2
        date_has_invalid_format    = 3
        date_inconsistency         = 4
        OTHERS                     = 5.
    IF sy-subrc = 0.
      DATA(lv_days) = VALUE #( lt_date_attr[ 1 ]-weekday_l OPTIONAL ).
      TRANSLATE lv_days TO UPPER CASE.
      zfi_due_dates-hday = lv_days.
    ENDIF.
    zfi_due_dates-ernam = sy-uname.
    zfi_due_dates-erdat = sy-datum.
  ENDIF.
ENDFORM.
