*&---------------------------------------------------------------------*
*& Report  ZBILLITEM
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ZBILLITEM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0001 OUTPUT.
*  SET PF-STATUS 'xxxxxxxx'.
*  SET TITLEBAR 'xxx'.

  IF SY-TCODE = 'VF03'.
    LOOP AT SCREEN.
      SCREEN-INPUT = '0'.
      MODIFY SCREEN.
      ENDLOOP.
      ENDIF.

ENDMODULE.                 " STATUS_0001  OUTPUT
