*&---------------------------------------------------------------------*
*& Report ZYMARD_NSAP_UPD_DMSSTK
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zymard_nsap_upd_dmsstk.
** Data Declaration for structures **
INCLUDE zymard_nsap_upd_dmsstk_top.
** Selection Screen Input **
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_kunnr FOR lv_kunnr.
  PARAMETERS: p_date TYPE hrp1001-begda.
SELECTION-SCREEN END OF BLOCK b1.

INCLUDE zymard_nsap_upd_dmsstk_forms.

INITIALIZATION.
  IF p_date IS INITIAL.
    p_date = sy-datum.
  ENDIF.

START-OF-SELECTION.
  "get bearer token from MIS
  CLEAR: gv_token,gv_msg.
  PERFORM f_get_bearer_token CHANGING gv_token gv_msg.

  IF gv_token IS NOT INITIAL.
** API Call **
    PERFORM f_dms_apicall.
  ELSE.
    WRITE: / 'Error Occurs as', gv_msg.
  ENDIF.

** Insert API data to YMARD_NSAP table **
  PERFORM f_insert_data.

END-OF-SELECTION.
