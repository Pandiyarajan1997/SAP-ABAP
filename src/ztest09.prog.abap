*&---------------------------------------------------------------------*
*& Report ZTEST09
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztest09.

SELECT * FROM zmm_ge_asn INTO TABLE @DATA(lt_tab).

CALL FUNCTION 'ZMM_POPUP_ALV'
  EXPORTING
    i_popup = 'X'
  TABLES
    it_alv  = lt_tab.
