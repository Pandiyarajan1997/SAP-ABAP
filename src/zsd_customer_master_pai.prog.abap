*&---------------------------------------------------------------------*
*& Include          ZSD_CUSTOMER_MASTER_PAI
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
      "Buttons in ALV
    WHEN 'BACK' OR 'CANC'.
      LEAVE TO SCREEN 0.
      "Customer Extension Process
    WHEN 'EXTEND'.
      PERFORM dms_customer_cc_sa_extension.

      CALL METHOD gr_grid->refresh_table_display( ).
  ENDCASE.
ENDMODULE.
