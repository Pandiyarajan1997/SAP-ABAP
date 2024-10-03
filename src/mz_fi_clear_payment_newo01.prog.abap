*&---------------------------------------------------------------------*
*&  Include           MZ_FI_FB05_INCOMING_PAYMENTO01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
  FREE gt_list[].
  IF gt_message[] IS NOT INITIAL.
    gs_list-fcode = '&POST1'.
    APPEND gs_list TO gt_list.
    gs_list-fcode = '&SAVE1'.
    APPEND gs_list TO gt_list.
    SET PF-STATUS '9000' EXCLUDING gt_list.
  ELSE.
    SET PF-STATUS '9000' .
  ENDIF.
  SET TITLEBAR '9000'.
ENDMODULE.                    "status_9000 OUTPUT

*&SPWIZARD: OUTPUT MODULE FOR TC 'TC_9000'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE tc_9000_change_tc_attr OUTPUT.
  IF tc_9000-lines IS INITIAL.
    PERFORM add_lines.
  ENDIF.
  DESCRIBE TABLE gt_tab LINES tc_9000-lines.
ENDMODULE.                    "tc_9000_change_tc_attr OUTPUT

*&SPWIZARD: OUTPUT MODULE FOR TC 'TC_9000'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GET LINES OF TABLECONTROL
MODULE tc_9000_get_lines OUTPUT.
  g_tc_9000_lines = sy-loopc.
  IF gt_message[] IS NOT INITIAL.
    LOOP AT  SCREEN.
      screen-input = 0.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.
ENDMODULE.                    "tc_9000_get_lines OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PBO_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_9000 OUTPUT.

  gs_header-blart = 'DZ'.
  IF gs_header-budat EQ 0.
    gs_header-budat = sy-datum.
  ENDIF.
  CLEAR: gv_class.
  SELECT SINGLE class FROM usr02 INTO gv_class
                      WHERE bname = sy-uname.
  IF gt_message[] IS INITIAL.
    LOOP AT SCREEN.
      IF screen-name = 'B3'.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN.
      screen-input = 0.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.

ENDMODULE.                    "pbo_9000 OUTPUT
