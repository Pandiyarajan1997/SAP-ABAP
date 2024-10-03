*----------------------------------------------------------------------*
***INCLUDE LZMGD2O01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module MODIFY_SCREEN OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE modify_screen OUTPUT.

  IF sy-tcode CS '3'.
    LOOP AT SCREEN.
      IF screen-group1 EQ 'P1'.
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

  CALL FUNCTION 'MARA_GET_SUB'
    IMPORTING
      wmara = mara
      xmara = *mara
      ymara = lmara.

  IF mara-zcust_grp1 IS NOT INITIAL.
    SELECT SINGLE bezei FROM tvv1t  INTO zcust_des WHERE kvgr1 = mara-zcust_grp1.
    IF sy-subrc = 0.

    ENDIF.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  MODIFY_DATA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_data INPUT.

  DATA: lv_mrp     TYPE mara-zmrp_percen,
        lv_custgrp TYPE mara-zcust_grp1.
  lv_mrp = mara-zmrp_percen.
  lv_custgrp = mara-zcust_grp1.

  CALL FUNCTION 'MARA_GET_SUB'
    IMPORTING
      wmara = mara
      xmara = *mara
      ymara = lmara.

  mara-zmrp_percen = lv_mrp.
  mara-zcust_grp1 = lv_custgrp.

  CALL FUNCTION 'MARA_SET_SUB'
    EXPORTING
      wmara = mara.
  IF mara-zcust_grp1 IS NOT INITIAL.
    SELECT SINGLE bezei FROM tvv1t  INTO zcust_des WHERE kvgr1 = mara-zcust_grp1.
    IF sy-subrc = 0.

    ENDIF.
  ENDIF.

ENDMODULE.
MODULE validation INPUT.
  IF mara-zcust_grp1 IS NOT INITIAL.
    SELECT SINGLE kvgr1 FROM tvv1  INTO @DATA(zcust_grp) WHERE kvgr1 = @mara-zcust_grp1.
    IF sy-subrc NE 0.
      MESSAGE 'Incorrect Customer Group' TYPE 'E'.
    ENDIF.
  ENDIF.
ENDMODULE.
