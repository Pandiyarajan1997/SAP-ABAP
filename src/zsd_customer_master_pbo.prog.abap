*&---------------------------------------------------------------------*
*& Include          ZSD_CUSTOMER_MASTER_PBO
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'ZDMS_EXTN'.
  SET TITLEBAR 'TITLE1'.

  PERFORM create_custom_container.

ENDMODULE.
