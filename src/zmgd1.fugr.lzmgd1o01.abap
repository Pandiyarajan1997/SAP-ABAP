*----------------------------------------------------------------------*
***INCLUDE LZMGD1O01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module MODIFY_SCREEN OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE modify_screen OUTPUT.
  IF sy-tcode CS '3'.

    LOOP AT SCREEN.
      IF screen-group1 = 'MA1'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module GET_DATA OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE get_data OUTPUT.

  CALL FUNCTION 'MARC_GET_SUB'
    IMPORTING
      wmarc = marc
      xmarc = *marc
      ymarc = lmarc.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  MODIFY_DATA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_data INPUT.
  DATA: lv_quan TYPE marc-supply_quantity,
        lv_uom  TYPE marc-suplly_uom.

  CLEAR: lv_quan,lv_uom.

  lv_quan = marc-supply_quantity.
  lv_uom = marc-suplly_uom.

  CALL FUNCTION 'MARC_GET_SUB'
    IMPORTING
      wmarc = marc
      xmarc = *marc
      ymarc = lmarc.

  marc-supply_quantity = lv_quan.
  marc-suplly_uom = lv_uom.

  CALL FUNCTION 'MARC_SET_SUB'
    EXPORTING
      wmarc = marc.




ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  VALIDATION  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE validation INPUT.
  IF marc-supply_quantity IS NOT INITIAL AND marc-suplly_uom IS INITIAL.
    MESSAGE 'Please provide supply_UOM for given Quantity' TYPE 'E'.
  ENDIF.

ENDMODULE.
