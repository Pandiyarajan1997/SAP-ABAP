*----------------------------------------------------------------------*
***INCLUDE ZHSBC_PAYMENT_FORM1.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_ALL  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_101 OUTPUT.
  SET PF-STATUS 'HSBC_ALL'.
  SET TITLEBAR 'HSBC_101'.

ENDMODULE.                 " STATUS_ALL  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0101 INPUT.

CASE sy-ucomm.
  WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
    CALL SCREEN '100'.
  WHEN 'EXE'.
    v = 0.
    back_screen = '101'.
    call screen '120'.
ENDCASE.


ENDMODULE.                 " USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*&      Module  CLEAR  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CLEAR OUTPUT.

PERFORM CLEARING.

*CLEAR : P_BUKRS,
*        P_HBKID,
*        P_HKTID,
*        P_GJAHR,
*        S_BELNR ,
*        S_BUDAT.
*
*clear : S_BUDAT.
*
*REFRESH : S_BUDAT.

clear BACK_SCREEN.

ENDMODULE.                 " CLEAR  OUTPUT
