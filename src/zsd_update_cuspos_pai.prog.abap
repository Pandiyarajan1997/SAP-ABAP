*&---------------------------------------------------------------------*
*& Include          ZSD_UPDATE_CUSPOS_PAI
*&---------------------------------------------------------------------*

MODULE status_9001 OUTPUT.
  SET PF-STATUS 'ZSD_UPD'.
  SET TITLEBAR 'ZSD_UPD'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9001 INPUT.

  CASE sy-ucomm.

    WHEN 'BACK'.

      LEAVE TO SCREEN 0.

    WHEN 'EXIT' OR 'CANCEL'.

      LEAVE PROGRAM.

  ENDCASE.

ENDMODULE.
