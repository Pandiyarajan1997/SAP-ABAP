*----------------------------------------------------------------------*
***INCLUDE LZSD_SALTYPEF01.
*----------------------------------------------------------------------*
FORM f_update_details.
** -- Created On & Created By
  zsd_saltyp-erdat = sy-datum.
  zsd_saltyp-ernam = sy-uname.

** -- ChangedOn & changed By
  zsd_saltyp-aenam = sy-uname.
  zsd_saltyp-aedat = sy-datum.
ENDFORM.

FORM f_modify_details.

  FIELD-SYMBOLS: <fs_field> TYPE any .
  LOOP AT total.
    CHECK <action> EQ aendern.
** -- Updated By
    ASSIGN COMPONENT 'AENAM' OF STRUCTURE <vim_total_struc> TO <fs_field>.
    IF sy-subrc EQ 0.
      <fs_field> = sy-uname.
    ENDIF.
** -- Updated On
    ASSIGN COMPONENT 'AEDAT' OF STRUCTURE <vim_total_struc> TO <fs_field>.
    IF sy-subrc EQ 0.
      <fs_field> = sy-datum.
    ENDIF.
    READ TABLE extract WITH KEY <vim_xtotal_key>.
    IF sy-subrc EQ 0.
      extract = total.
      MODIFY extract INDEX sy-tabix.
    ENDIF.
    IF total IS NOT INITIAL.
      MODIFY total.
    ENDIF.
  ENDLOOP.
ENDFORM.
