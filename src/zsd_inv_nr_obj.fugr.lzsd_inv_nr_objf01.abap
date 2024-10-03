*----------------------------------------------------------------------*
***INCLUDE LZSD_INV_NR_OBJF01.
*----------------------------------------------------------------------*

FORM new_entry.
  SELECT SINGLE bukrs FROM t001k INTO @DATA(l_bukrs) WHERE bwkey = @zsd_inv_nr_obj-werks.
  zsd_inv_nr_obj-bukrs = l_bukrs.
  IF l_bukrs = 'DMS1'.
    IF zsd_inv_nr_obj-prefix IS NOT INITIAL .
      SELECT SINGLE fkart,werks
        FROM zsd_inv_nr_obj INTO @DATA(l_data)
        WHERE prefix = @zsd_inv_nr_obj-prefix AND bukrs = 'DMS1'.
      IF sy-subrc = 0.
        DATA(l_txt) = |{ zsd_inv_nr_obj-prefix }-Prefix is not allowed already mapped to Plant-{ l_data-werks }|.
        MESSAGE l_txt TYPE 'E' DISPLAY LIKE 'E'.
      ENDIF.
    ENDIF.
    SELECT SINGLE fkart,werks
      FROM zsd_inv_nr_obj INTO @l_data
      WHERE numki = @zsd_inv_nr_obj-numki AND bukrs = 'DMS1'.
    IF sy-subrc = 0.
      l_txt = |{ zsd_inv_nr_obj-numki }-No Range is not allowed already mapped to Plant-{ l_data-werks }|.
      MESSAGE l_txt TYPE 'E' DISPLAY LIKE 'E'.
    ENDIF.
  ENDIF.
ENDFORM.
