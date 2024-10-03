*----------------------------------------------------------------------*
***INCLUDE LZMGD3O01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module MODIFY_SCREEN OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
MODULE modify_screen OUTPUT.
  IF sy-tcode CS '3'.
    LOOP AT SCREEN.
      IF screen-group1 = 'Z1'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
    LOOP AT SCREEN.
      IF screen-group1 = 'T01'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
MODULE get_data OUTPUT.

  CALL FUNCTION 'MARA_GET_SUB'
    IMPORTING
      wmara = mara
      xmara = *mara
      ymara = lmara.

  IF mara-ecom_name IS NOT INITIAL.
    SELECT SINGLE shtext FROM ztbl_sh INTO shtext
                                      WHERE shcode = mara-ecom_name.
  ENDIF.

  IF mara-ecom_category IS NOT INITIAL.
    SELECT SINGLE pctext FROM ztbl_pc INTO shtext1
                                       WHERE pccode = mara-ecom_category.
  ENDIF.

ENDMODULE.

*----------------------------------------------------------------------*
MODULE modify_data INPUT.
  DATA: lv_foc      TYPE mara-is_foc,
        lv_refmat   TYPE mara-reference_material,
        lv_stat     TYPE mara-status,
        lv_ecom     TYPE mara-ecom_name,
        lv_ecom1    TYPE mara-ecom_category,
        lv_ecom_mat TYPE mara-ecom_mat_name.
  DATA: gs_shades TYPE ztbl_sh.
  CLEAR: lv_foc, lv_refmat, lv_stat , lv_ecom,lv_ecom1,lv_ecom_mat.
  lv_foc = mara-is_foc.
  lv_refmat = mara-reference_material.
  lv_stat = mara-status.
  lv_ecom = mara-ecom_name.
  lv_ecom1 = mara-ecom_category.
  lv_ecom_mat = mara-ecom_mat_name.

  CALL FUNCTION 'MARA_GET_SUB'
    IMPORTING
      wmara = mara
      xmara = *mara
      ymara = lmara.

  mara-is_foc = lv_foc.
  mara-reference_material = lv_refmat.
  mara-status = lv_stat.
  mara-ecom_name = lv_ecom.
  mara-ecom_category = lv_ecom1.
  mara-ecom_mat_name = lv_ecom_mat.

  CALL FUNCTION 'MARA_SET_SUB'
    EXPORTING
      wmara = mara.

  IF mara-ecom_category IS NOT INITIAL.
    SELECT SINGLE shtext FROM ztbl_sh INTO shtext1
                                      WHERE shcode = mara-ecom_name.
  ENDIF.

  IF mara-ecom_category IS NOT INITIAL.
    SELECT SINGLE pctext FROM ztbl_pc INTO shtext1
                                      WHERE pccode = mara-ecom_category.

  ENDIF.

ENDMODULE.

MODULE validation INPUT.

  TYPES: BEGIN OF ty_mara,
           matnr              TYPE matnr,
           reference_material TYPE matnr,
         END OF ty_mara.

  DATA: lt_mara TYPE TABLE OF ty_mara,
        ls_mara TYPE ty_mara.

  DATA: lv_matnr  TYPE matnr,
        lv_matnr1 TYPE matnr.

  DATA: lv_msg(150) TYPE c.

  IF ( mara-is_foc IS NOT INITIAL AND mara-reference_material IS INITIAL ).
    MESSAGE 'For FOC Material Provide the Reference Material' TYPE  'E'.
  ENDIF.
  IF mara-reference_material IS NOT INITIAL.
    CLEAR lv_matnr.
    SELECT SINGLE matnr FROM mara INTO lv_matnr WHERE matnr = mara-reference_material.
    IF sy-subrc NE 0.
      MESSAGE 'Please enter valid material Number or Select from help Provided' TYPE 'E'.
    ENDIF.
  ENDIF.

  IF ( mara-matnr IS NOT INITIAL AND mara-reference_material IS NOT INITIAL ).
    SELECT matnr
           reference_material FROM mara
                              INTO TABLE lt_mara WHERE reference_material = mara-reference_material.
    IF sy-subrc = 0.
      SORT lt_mara BY reference_material.
      LOOP AT lt_mara INTO ls_mara WHERE reference_material = mara-reference_material AND matnr NE mara-matnr.
        EXIT.
      ENDLOOP.
      IF sy-subrc = 0.
        CLEAR: lv_msg,lv_matnr1.
        lv_matnr1 = ls_mara-matnr.
        SHIFT lv_matnr1 LEFT DELETING LEADING '0'.
        CONDENSE lv_matnr1 NO-GAPS.
        CONCATENATE 'Reference Material Already Mapped To Material'  lv_matnr1 INTO lv_msg.
        CONDENSE lv_msg.
        MESSAGE lv_msg  TYPE 'E'.
      ENDIF.
    ENDIF.
  ENDIF.

ENDMODULE.

*----------------------------------------------------------------------*
MODULE validation1 INPUT.
  DATA: lv_shcode TYPE ztbl_sh-shcode. .
****************ECOMMERCE Checks For material Number**********************
  IF mara-ecom_name IS NOT INITIAL.
    CLEAR lv_shcode.
    SELECT SINGLE shcode FROM ztbl_sh INTO lv_shcode WHERE shcode = mara-ecom_name.
    IF sy-subrc NE 0.
      MESSAGE 'Please enter valid ecom_name' TYPE 'E'.
    ENDIF.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  VALIDATION2  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE validation2 INPUT.
  DATA: lv_pccode TYPE ztbl_pc-pccode.
  IF mara-ecom_category IS NOT INITIAL.
    CLEAR lv_pccode.
    SELECT SINGLE pccode FROM ztbl_pc INTO lv_pccode WHERE pccode = mara-ecom_category.
    IF sy-subrc NE 0.
      MESSAGE 'Please enter valid ecom_category' TYPE 'E'.
    ENDIF.
  ENDIF.
ENDMODULE.
