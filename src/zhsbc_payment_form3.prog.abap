*&---------------------------------------------------------------------*
*&  Include           ZHSBC_PAYMENT_FORM3
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0103  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0103 OUTPUT.
SET PF-STATUS 'HSBC_ALL'.
SET TITLEBAR 'HSBC_103'.

ENDMODULE.                 " STATUS_0103  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0103 INPUT.

  CASE sy-ucomm.
  WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
    CALL SCREEN '100'.
  WHEN 'EXE'.
        IF P_HBKID is INITIAL.

      message 'Enter House Bank.....' TYPE 'S'.
      call screen '103'.

    ENDIF.

    IF P_HKTID is INITIAL.

      message 'Enter Account ID' TYPE 'S'.
      call screen '103'.

    ENDIF.

    IF P_HBKID is NOT INITIAL and P_HKTID is NOT INITIAL.

    BACK_SCREEN = '103'.
    call SCREEN '120'.

    ENDIF.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0103  INPUT
