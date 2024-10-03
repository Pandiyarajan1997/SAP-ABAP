

*&---------------------------------------------------------------------*
*&      Module  MODIFY  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MODIFY OUTPUT.

  LOOP AT SCREEN.
    IF SCREEN-NAME EQ 'WA_FINAL-LIFNR'.
      SCREEN-INPUT = 0.
      MODIFY SCREEN.
    ELSEIF SCREEN-NAME EQ 'WA_FINAL-NAME1'.
      SCREEN-INPUT = 0.
      MODIFY SCREEN.
    ELSEIF SCREEN-NAME EQ 'WA_FINAL-ORT01'.
      SCREEN-INPUT = 0.
      MODIFY SCREEN.
*    ELSEIF SCREEN-NAME EQ 'WA_FINAL-BASED_LIMIT'.
**      SCREEN-INPUT = 0.
*      MODIFY SCREEN.
    ELSEIF SCREEN-NAME EQ 'WA_FINAL-AMOUNT'.
      SCREEN-INPUT = 0.
      MODIFY SCREEN.
    ELSEIF SCREEN-NAME EQ 'WA_FINAL-LV_DAYS1'.
      SCREEN-INPUT = 0.
      MODIFY SCREEN.
    ELSEIF SCREEN-NAME EQ 'WA_FINAL-BALANCE'.
      SCREEN-INPUT = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.


ENDMODULE.                 " MODIFY  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_5009  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_5009 OUTPUT.


  SET PF-STATUS 'ZVEN_GUI'.
*  SET TITLEBAR 'xxx'.

  CLEAR OK_CODE.
  CLEAR  LV_FLAG.
  CLEAR LV_FLAG1.
  CLEAR  S_LIFNR.


ENDMODULE.                 " STATUS_5009  OUTPUT

*&---------------------------------------------------------------------*
*&  Include           ZPBO_DEC_VEN_DET
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_5000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_5000 OUTPUT.
  SET PF-STATUS 'ZGUI_STATUS'.
*  SET TITLEBAR 'TITLE'.



  DATA : LT_DATE TYPE SY-DATUM.
  LT_DATE = SY-DATUM.


  DATA : LV_CRE TYPE BSIK-DMBTR .
  DATA : LV_DEB TYPE BSIK-DMBTR .


  PERFORM SELECT_QUERY.


ENDMODULE.                 " STATUS_5000  OUTPUT
