*&---------------------------------------------------------------------*
*& Include          ZSD_SF_CUST_INV_E01
*&---------------------------------------------------------------------*
INITIALIZATION.

  DATA lobj_cust TYPE REF TO lcl_cust.
  CREATE OBJECT lobj_cust. "create the object for local class

AT SELECTION-SCREEN.
*  l_syucomm = sy-ucomm.

AT SELECTION-SCREEN OUTPUT.
*  CASE l_syucomm.
*    WHEN 'UC1'.
      LOOP AT SCREEN.
        IF screen-name = 'P_STATUS'.
          IF p_appr = abap_true.
            screen-input = 0.
*            p_status = '99'.
          ELSE.
            screen-input = 1.
            p_status = ''.
          ENDIF.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
*  ENDCASE.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_layo.      "layout option for alv

  lobj_cust->select_alv_variant(
    CHANGING
      cv_layout = p_layo ).

START-OF-SELECTION.
  l_syucomm = 'SAVE'.
  APPEND l_syucomm TO fcode.
  l_syucomm = 'GRN'.
  APPEND l_syucomm TO fcode.
*  IF p_appr = abap_true AND p_status NE '99'.
*    MESSAGE 'Please Select status for Pre Approval' TYPE 'S' DISPLAY LIKE 'E'.
*    DATA(l_err) = 'X'.
*  ENDIF.
*  IF l_err IS INITIAL.
    lobj_cust->fetch( ).       "call method for fetching the data
    IF lobj_cust->lt_final IS NOT INITIAL.
      CALL SCREEN 9001.          "call the alv screen
    ELSE.
      MESSAGE TEXT-003 TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
*  ENDIF.
