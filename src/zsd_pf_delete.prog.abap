*&---------------------------------------------------------------------*
*& Report ZSD_PF_DELETE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsd_pf_delete.

DATA: lt_knvp TYPE STANDARD TABLE OF knvp,
      ls_knvp TYPE knvp.

*PARAMETERS: p_prf TYPE tpar-parvw.

SELECT * FROM knvp INTO TABLE lt_knvp
                   WHERE parvw EQ 'AS'.
IF sy-subrc EQ 0.
  LOOP AT lt_knvp INTO ls_knvp.
    DELETE FROM knvp WHERE kunnr = ls_knvp-kunnr
                     AND vkorg = ls_knvp-vkorg
                     AND vtweg = ls_knvp-vtweg
                     AND spart = ls_knvp-spart
                     AND parvw = ls_knvp-parvw.
  ENDLOOP.
ENDIF.
