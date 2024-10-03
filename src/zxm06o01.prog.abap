*&---------------------------------------------------------------------*
*& Include          ZXM06O01
*&---------------------------------------------------------------------*
MODULE modify OUTPUT.
  IF sy-tcode EQ 'ME23N' OR sy-tcode EQ 'ME22N' OR sy-tcode EQ 'ME29N'.

    IF gw_aktyp NE 'A'.
      LOOP AT SCREEN.
        IF screen-group1 = 'B1'.
          screen-input = 1.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ELSE.
      LOOP AT SCREEN.
        IF screen-group1 = 'B1'.
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  F4  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4 INPUT.

*  DATA: lt_shlp TYPE STANDARD TABLE OF zpo_remarks,
*        ls_shlp TYPE zpo_remarks.

  DATA: lt_ret TYPE TABLE OF ddshretval,
        ls_ret TYPE ddshretval.


  SELECT zid, zremarks FROM zpo_remarks INTO TABLE @DATA(lt_shlp).

  IF lt_shlp[] IS NOT INITIAL.
    IF sy-tcode EQ 'ME21N' OR sy-tcode EQ 'ME22N' OR sy-tcode EQ 'ME23N' OR sy-tcode EQ 'ME29N'.
      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
*         DDIC_STRUCTURE         = ' '
          retfield    = 'ZREMARKS'
          dynpprog    = sy-cprog
          dynpnr      = sy-dynnr
          dynprofield = 'ZREMARKS'
          value_org   = 'S'
        TABLES
          value_tab   = lt_shlp
          return_tab  = lt_ret.
    ENDIF.

  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  VALIDATION  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE validation INPUT.
  DATA: gt_shlp TYPE STANDARD TABLE OF zpo_remarks,
        gs_shlp TYPE zpo_remarks.

  REFRESH gt_shlp.
  SELECT * FROM zpo_remarks INTO TABLE gt_shlp.
  IF zremarks IS NOT INITIAL.
    READ TABLE gt_shlp INTO gs_shlp WITH KEY zremarks = zremarks.
    IF sy-subrc NE 0.
      MESSAGE 'Please provide Remarks in Search help' TYPE 'E'.
    ENDIF.
  ENDIF.
ENDMODULE.
