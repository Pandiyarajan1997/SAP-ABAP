*&---------------------------------------------------------------------*
*& Include          ZXFKOU01
*&---------------------------------------------------------------------*
IF sy-tcode EQ 'FB65'.
  IF sy-ucomm EQ 'BU'.

  ENDIF.
MESSAGE 'Mail Sent successfully' TYPE 'S'.
ENDIF.
