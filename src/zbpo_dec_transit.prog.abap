*&---------------------------------------------------------------------*
*&  Include           ZBPO_DEC_TRANSIT
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_9193  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9193 OUTPUT.
  SET PF-STATUS 'ZGUI'.
*  SET TITLEBAR 'xxx'.

*  PERFORM SELECT_QUERY_T001W."



  DATA: LT_VALUE TYPE TABLE OF VRM_VALUE,
        LS_VALUE LIKE LINE OF LT_VALUE.




  DATA : P_KEY TYPE CHAR3 .

*  clear p_bukrs.

  LS_VALUE-KEY  = '01'.
  LS_VALUE-TEXT  = 'SELECT ALL'.
  APPEND LS_VALUE TO LT_VALUE.
  CLEAR: LS_VALUE.

  LS_VALUE-KEY  = '02'.
  LS_VALUE-TEXT  = 'DESELECT ALL'.
  APPEND LS_VALUE TO LT_VALUE.
  CLEAR: LS_VALUE.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      ID     = 'P_KEY'
      VALUES = LT_VALUE.
  REFRESH LT_VALUE.


ENDMODULE.                 " STATUS_9193  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SCREEN_MODIFY_TAB_CONTROL  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SCREEN_MODIFY_TAB_CONTROL OUTPUT.
  LOOP AT SCREEN.
    IF SCREEN-NAME EQ 'WA_FINAL-WERKS'.
      SCREEN-INPUT = 0.
      MODIFY SCREEN.
    ELSEIF SCREEN-NAME EQ 'WA_FINAL-NAME1'.
      SCREEN-INPUT = 0.
      MODIFY SCREEN.
    ELSEIF SCREEN-NAME EQ 'WA_FINAL-ST_DAYS'.
      SCREEN-INPUT = 0.
      MODIFY SCREEN.
    ELSEIF SCREEN-NAME EQ 'WA_FINAL-STATUS'.
      SCREEN-INPUT = 0.
      MODIFY SCREEN.

    ENDIF.
  ENDLOOP.

ENDMODULE.                 " SCREEN_MODIFY_TAB_CONTROL  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STEP_LOOP  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STEP_LOOP OUTPUT.

  TAB-LINES = '1000'.
ENDMODULE.                 " STEP_LOOP  OUTPUT
