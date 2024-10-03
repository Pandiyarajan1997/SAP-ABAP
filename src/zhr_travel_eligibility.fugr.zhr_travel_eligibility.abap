FUNCTION zhr_travel_eligibility.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      IT_ELIGIBILITY STRUCTURE  ZTRAVEL_ELIGIBILITY
*"----------------------------------------------------------------------

  DATA: it_ztravel_elig TYPE TABLE OF ztravel_elig,
        wa_ZTRAVEL_ELIG TYPE ztravel_elig.


  DATA: wa_travel_elig TYPE ztravel_eligibility.

  DATA: it_DD07V_tmode TYPE TABLE OF dd07v,
        it_DD07V_tclas TYPE TABLE OF dd07v,
        wa_dd07v       TYPE dd07v.

*Transport Mode
  CALL FUNCTION 'DD_DOMVALUES_GET'
    EXPORTING
      domname        = 'ZTRANS_MODE'
      text           = 'X'
      langu          = sy-langu
*     BYPASS_BUFFER  = ' '
* IMPORTING
*     RC             =
    TABLES
      dd07v_tab      = it_DD07V_tmode
    EXCEPTIONS
      wrong_textflag = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

*Transport Class
  CALL FUNCTION 'DD_DOMVALUES_GET'
    EXPORTING
      domname        = 'ZTRANS_CLASS'
      text           = 'X'
      langu          = sy-langu
*     BYPASS_BUFFER  = ' '
* IMPORTING
*     RC             =
    TABLES
      dd07v_tab      = it_DD07V_tclas
    EXCEPTIONS
      wrong_textflag = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.


* Selecting all records from Travel eligibility table
  SELECT *
    FROM ztravel_elig
    INTO TABLE it_ztravel_elig.

  IF sy-subrc = 0.
    MOVE-CORRESPONDING it_ztravel_elig[] TO it_eligibility[].

    LOOP AT it_eligibility INTO wa_travel_elig.
      IF wa_travel_elig-transport_mode IS NOT INITIAL.
        READ TABLE it_DD07V_tmode INTO wa_dd07v WITH KEY domvalue_l = wa_travel_elig-transport_mode.
        IF sy-subrc = 0.
          wa_travel_elig-TRANSPORT_MODE_TXT = wa_dd07v-ddtext.
        ENDIF.
      ENDIF.

      IF wa_travel_elig-transport_class IS NOT INITIAL.
        READ TABLE it_DD07V_tclas INTO wa_dd07v WITH KEY domvalue_l = wa_travel_elig-transport_class.
        IF sy-subrc = 0.
          wa_travel_elig-TRANSPORT_class_TXT = wa_dd07v-ddtext.
        ENDIF.
      ENDIF.

      MODIFY it_eligibility FROM wa_travel_elig.

    ENDLOOP.
  ENDIF.

ENDFUNCTION.
